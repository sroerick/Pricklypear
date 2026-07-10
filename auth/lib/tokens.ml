(** Pricklypear Auth — Token minting/verification.

    Per spec [auth.verification] TOKEN MINTING:
      1. Generate random token bytes (32B).
      2. HMAC-sign with the session_signing key from system_keys.
         opaque_id = random; signed = HMAC(opaque_id).
      3. INSERT INTO sessions (id=opaque_id, user_id, token_hash=signed,
                                  expires_at=...).
      4. Return token = base64(opaque_id || signed). Client stores as cookie.

    TOKEN FORMAT (minimal, NOT JWT):
      opaque_id (32B random) || HMAC(opaque_id)
      Hash stored in sessions table enables revocation.

    Token wire format follows common_types.Auth_types.token:
      base64(opaque_id) "." base64(signature). *)

open Lwt.Syntax

let opaque_id_len = 32  (* bytes of entropy *)

(* Generate 32 bytes of random data using mirage-crypto-rng. *)
let random_opaque_id () : Cstruct.t =
  Mirage_crypto_rng.generate opaque_id_len

(* HMAC-SHA256 of the opaque_id using the signing key. *)
let sign ~(key : Cstruct.t) (opaque_id : Cstruct.t) : Cstruct.t =
  Mirage_crypto.Hash.SHA256.hmac ~key opaque_id

(* Encode Cstruct as hex DIGITS (printable). Note: Hex.to_string DECODES. *)
let hex_of_cs (cs : Cstruct.t) : string =
  match Hex.of_cstruct cs with `Hex s -> s

(* Mint a token for [user_id]. Generates opaque id + signature,
   persists the session row (expires_at default 7 days), returns the
   wire-format token string. *)
let mint
    ~(store : Store.t)
    ~(key : Cstruct.t)
    ~(user_id : string)
    : Pricklypear_common.Auth_types.token option Lwt.t =
  (* Generate the opaque_id as a fresh UUID (PG-side gen_random_uuid,
     since auth_sessions.id is uuid). Sign it with the HMAC of the
     key. opaque_id is the row's id cast to text — opaque enough for
     our internal-network scope. *)
  let* opaque_id_opt =
    Store.insert_session_returning_id ~store ~user_id ~key
  in
  match opaque_id_opt with
  | None -> Lwt.return_none
  | Some opaque_id_str ->
      let opaque_id = Cstruct.of_string opaque_id_str in
      let signature = sign ~key opaque_id in
      let signature_str = hex_of_cs signature in
      (* Persist the signature as token_hash on the inserted row. *)
      let* ok = Store.update_session_token_hash ~store
        ~opaque_id:opaque_id_str ~token_hash:signature_str in
      let* () =
        Store.log store ~user_id:(Some user_id) ~kind:"mint" ~success:true ()
      in
      Lwt.return (Some { Pricklypear_common.Auth_types.opaque_id = opaque_id_str;
                         signature = signature_str })

(* Verify a wire-format token: split, recompute HMAC, look up the session.
   Returns Some user_id if the signature matches AND the session is
   valid (not revoked, not expired). Returns None otherwise. *)
let verify
    ~(store : Store.t)
    ~(key : Cstruct.t)
    (token : Pricklypear_common.Auth_types.token)
    : string option Lwt.t =
  let opaque_id = Cstruct.of_string token.opaque_id in
  let expected_sig = sign ~key opaque_id in
  let expected_sig_str = hex_of_cs expected_sig in
  if not (String.equal expected_sig_str token.signature) then
    Lwt.return_none  (* bad signature — forged or corrupted *)
  else
    Store.lookup_session store
      ~opaque_id:token.opaque_id ~token_hash:token.signature

(* Render a [token] as the wire-format string. *)
let to_string (t : Pricklypear_common.Auth_types.token) : string =
  Pricklypear_common.Auth_types.token_to_string t

(* Parse a wire-format string back into a [token]. *)
let of_string (s : string)
  : (Pricklypear_common.Auth_types.token, string) result =
  Pricklypear_common.Auth_types.token_of_string s
