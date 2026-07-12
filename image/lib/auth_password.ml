(** Pricklypear Image — interim password hashing.

    Complete system wants argon2id (auth.borg). Until argon2 is on the
    poohstack switch we use salted SHA-256:

      sha256$<salt_hex>$<digest_hex>

    Compatible with the dream-product-cut interim format. Upgrade path:
    rehash on successful login once argon2 is available. *)

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

let hash_password ?(salt_len = 16) (password : string) : string =
  let salt = random_bytes salt_len in
  let salt_hex = hex_encode salt in
  let digest = sha256_hex (salt_hex ^ ":" ^ password) in
  Printf.sprintf "sha256$%s$%s" salt_hex digest

let verify_password ~(stored : string) (password : string) : bool =
  match String.split_on_char '$' stored with
  | ["sha256"; salt_hex; digest] when salt_hex <> "" && digest <> "" ->
      let candidate = sha256_hex (salt_hex ^ ":" ^ password) in
      secure_equal candidate digest
  | _ -> false
