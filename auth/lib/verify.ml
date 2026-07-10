(** Pricklypear Auth — Verification functions.

    Per spec [auth.verification]: the four pure functions of the
    credential store. This module implements verify_ssh_key (used by
    supervisor's SSH front door) and verify_token (used by the
    supervisor's routing layer for web sessions). verify_password is
    stubbed (requires argon2id, not on poohstack yet).

    Every verify call writes to auth_log (per spec [auth.audit-log]). *)
open Lwt.Syntax

let signing_key = ref None

(* Set the signing key. Called on boot after fetching from store. *)
let set_signing_key (k : Cstruct.t) : unit =
  signing_key := Some k

let get_signing_key () : Cstruct.t =
  match !signing_key with
  | Some k -> k
  | None -> failwith "auth.verify: signing key not loaded"

(* Verify a presented SSH public key against the credentials table.
   On match, returns the user_id of the matching credential. Writes
   an auth_log row. *)
let verify_ssh_key
    ~(store : Store.t) ?(source_ip : string = "")
    (req : Pricklypear_common.Auth_types.verify_ssh_key_req)
    : Pricklypear_common.Auth_types.verify_ssh_key_resp Lwt.t =
  let src_ip = if source_ip = "" then None else Some source_ip in
  let* uid_opt = Store.lookup_credential_ssh_key store req.key in
  let* () =
    match uid_opt with
    | Some uid ->
        Store.log store ~user_id:(Some uid) ~kind:"ssh_key" ~success:true
          ~source_ip:src_ip ()
    | None ->
        Store.log store ~kind:"ssh_key" ~success:false ~source_ip:src_ip ()
  in
  match uid_opt with
  | Some uid -> Lwt.return (Pricklypear_common.Auth_types.Ok uid)
  | None -> Lwt.return Pricklypear_common.Auth_types.Denied

(* Verify a presented session token: recompute HMAC, lookup session,
   check not revoked, not expired. *)
let verify_token
    ~(store : Store.t) ?(source_ip : string = "")
    (token_str : string)
    : string option Lwt.t =
  let src_ip = if source_ip = "" then None else Some source_ip in
  let* token_res = Lwt.return (Tokens.of_string token_str) in
  match token_res with
  | Error _ ->
      let* () =
        Store.log store ~kind:"verify_token" ~success:false ~source_ip:src_ip ()
      in
      Lwt.return_none
  | Ok token ->
      let key = get_signing_key () in
      let* uid_opt = Tokens.verify ~store ~key token in
      let* () =
        match uid_opt with
        | Some uid ->
            Store.log store ~user_id:(Some uid) ~kind:"verify_token"
              ~success:true ~source_ip:src_ip ()
        | None ->
            Store.log store ~kind:"verify_token" ~success:false ~source_ip:src_ip ()
      in
      Lwt.return uid_opt

(* Mint a session token for [user_id]. The supervisor calls this after
   a successful login (password verify TODO, since argon2 isn't on the
   poohstack switch yet). Returns the wire-format token string. *)
let mint_token ~(store : Store.t) (user_id : string)
    : string option Lwt.t =
  let key = get_signing_key () in
  let* tok_opt = Tokens.mint ~store ~key ~user_id in
  match tok_opt with
  | None -> Lwt.return_none
  | Some t ->
      let* () =
        Store.log store ~user_id:(Some user_id) ~kind:"mint" ~success:true ()
      in
      Lwt.return (Some (Tokens.to_string t))

(* Verify a username/password against credentials.kind='password'.
   INTERIM hash scheme: sha256$salt$digest via Mirage_crypto (see
   Store.hash_password). Not argon2id — accepted for personal/small-team
   v1; swap when argon2 lands on poohstack. *)
let verify_password
    ~(store : Store.t)
    ~username ~password
    ?(source_ip = "")
    ()
    : Pricklypear_common.Auth_types.verify_ssh_key_resp Lwt.t =
  let src_ip = if source_ip = "" then None else Some source_ip in
  let* found = Store.lookup_password_hash store ~username in
  match found with
  | None ->
      let* () =
        Store.log store ~kind:"password" ~success:false ~source_ip:src_ip ()
      in
      Lwt.return Pricklypear_common.Auth_types.Denied
  | Some (uid, hash) ->
      if Store.verify_password_hash ~stored:hash ~password then
        let* () =
          Store.log store ~user_id:(Some uid) ~kind:"password"
            ~success:true ~source_ip:src_ip ()
        in
        Lwt.return (Pricklypear_common.Auth_types.Ok uid)
      else
        let* () =
          Store.log store ~user_id:(Some uid) ~kind:"password"
            ~success:false ~source_ip:src_ip ()
        in
        Lwt.return Pricklypear_common.Auth_types.Denied
