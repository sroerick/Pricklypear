(** Pricklypear Image — password hashing.

    CHAMPION: argon2id (t_cost=2, m_cost=65536 KiB, parallelism=1,
    PHC-encoded), the auth.borg endpoint. Legacy rows in the interim
    format

      sha256$<salt_hex>$<digest_hex>

    still VERIFY, and callers rehash-on-success (auth_store.verify_password
    → [Auth_store.transparent_rehash]) so a deployed principal migrates to
    argon2id on its next successful login with no downtime.

    Upgrade path deliberately avoids a flag day: we never dual-enter a row
    and never reject a legacy principal — the digest comparison is cheap
    constant-time, and the entire class of "verify passes with the old
    scheme" is bounded by untouched rows only. *)

let hex_encode (b : string) : string =
  let len = String.length b in
  let buf = Bytes.create (len * 2) in
  let hex = "0123456789abcdef" in
  for i = 0 to len - 1 do
    let c = Char.code b.[i] in
    Bytes.set buf (i * 2) hex.[c lsr 4];
    Bytes.set buf (i * 2 + 1) hex.[c land 0xf]
  done;
  Bytes.unsafe_to_string buf

let random_bytes (n : int) : string =
  Mirage_crypto_rng.generate n

let sha256_hex (s : string) : string =
  Digestif.SHA256.(digest_string s |> to_hex)

(* Argon2id parameters — auth.borg "interim" note and the OWASP cheat
   sheet agree on m_cost=65536 KiB / t_cost=2 / parallelism=1 as the
   interactive-login sweet spot. hash_len=32, PHC encoding. *)
let argon_t_cost = 2
let argon_m_cost = 65536
let argon_parallelism = 1
let argon_hash_len = 32



let hash_argonid ~(salt : string) (password : string) : string =
  (* Top-level [Argon2.hash] — its [encoded] is concrete string, unlike
     the functor-instantiated ID.hash_encoded (abstract [ID.encoded]). *)
  match
    Argon2.hash ~t_cost:argon_t_cost ~m_cost:argon_m_cost
      ~parallelism:argon_parallelism ~pwd:password ~salt ~kind:Argon2.ID
      ~hash_len:argon_hash_len
      ~encoded_len:(Argon2.encoded_len ~t_cost:argon_t_cost
                      ~m_cost:argon_m_cost ~parallelism:argon_parallelism
                      ~salt_len:(String.length salt)
                      ~hash_len:argon_hash_len ~kind:Argon2.ID)
      ~version:Argon2.VERSION_NUMBER
  with
  | Ok (_raw, encoded) -> encoded
  | Error e ->
      invalid_arg ("argon2id hash failed: " ^ Argon2.ErrorCodes.message e)

(** Constant-time string compare to avoid password/hash timing leaks. *)
let secure_equal (a : string) (b : string) : bool =
  let la = String.length a and lb = String.length b in
  let len = max la lb in
  let acc = ref (la lxor lb) in
  for i = 0 to len - 1 do
    let ca = if i < la then Char.code a.[i] else 0 in
    let cb = if i < lb then Char.code b.[i] else 0 in
    acc := !acc lor (ca lxor cb)
  done;
  !acc = 0

(** Result of a verify: did the password match, and is the stored hash in
    the CURRENT scheme (so no upgrade needed)? Callers with DB access use
    [needs_upgrade] to trigger the transparent rehash. *)
type verify_outcome = {
  ok : bool;
  needs_upgrade : bool;
}

let verify_legacy ~(stored : string) (password : string) : bool =
  match String.split_on_char '$' stored with
  | ["sha256"; salt_hex; digest] when salt_hex <> "" && digest <> "" ->
      let candidate = sha256_hex (salt_hex ^ ":" ^ password) in
      secure_equal candidate digest
  | _ -> false

let verify_current ~(stored : string) (password : string) : bool =
  (* PHC-encoded argon2id starts "$argon2id$". *)
  if String.starts_with ~prefix:"$argon2id$" stored then
    (match Argon2.verify ~encoded:stored ~pwd:password ~kind:Argon2.ID with
     | Ok b -> b
     | Error _ -> false)
  else false

let verify_full ~(stored : string) (password : string) : verify_outcome =
  if String.starts_with ~prefix:"$argon2id$" stored then
    { ok = verify_current ~stored password; needs_upgrade = false }
  else
    { ok = verify_legacy ~stored password; needs_upgrade = true }

let verify_password ~(stored : string) (password : string) : bool =
  (verify_full ~stored password).ok

let hash_password ?(salt_len = 16) (password : string) : string =
  let salt = random_bytes salt_len in
  hash_argonid ~salt password

(** Fixed dummy argon2id hash for unknown-username verifies (timing
    flatten, i-1787970741): a lookup for a nonexistent account burns the
    same argon2 work as one for a real row, so usernames are not
    distinguishable by response latency. The salt is a fixed public
    constant — this hash is never compared for success, only spent, so
    no RNG dependency at force time either. *)
let dummy_hash = lazy (hash_argonid ~salt:"pp-unknown-user-dummy" "throttle-dummy")

(** True when [stored] is in the legacy interim scheme and a successful
    verify should rewrite it to argon2id. *)
let needs_upgrade (stored : string) : bool =
  not (String.starts_with ~prefix:"$argon2id$" stored)
