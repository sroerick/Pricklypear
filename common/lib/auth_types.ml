(** Pricklypear Common — Auth types.

    Shared between the supervisor (which calls auth via RPC) and the auth
    unikernel (which serves the RPC). Per spec [auth]: auth is the only
    thing that verifies credentials and mints tokens; the supervisor never
    sees raw credentials past the verify call. *)
open Yojson.Safe

type user_id = string
type credential_id = string

type user = {
  id : user_id;
  username : string;
  is_admin : bool;
  created_at : string;
}

type credential_kind =
  | K_ssh_key
  | K_password
  | K_web_token
  | K_api_key

let credential_kind_to_string = function
  | K_ssh_key -> "ssh_key"
  | K_password -> "password"
  | K_web_token -> "web_token"
  | K_api_key -> "api_key"

let credential_kind_of_string = function
  | "ssh_key" -> Some K_ssh_key
  | "password" -> Some K_password
  | "web_token" -> Some K_web_token
  | "api_key" -> Some K_api_key
  | _ -> None

(* A signed session token: opaque_id || HMAC(opaque_id).
   The opaque_id is the sessions.id; the HMAC is checked by auth. *)
type token = {
  opaque_id : string;     (* 32-byte random, base64 *)
  signature : string;     (* HMAC, base64 *)
}

let token_to_string (t : token) : string =
  (* joined with "." so we can split — base64 is url-safe (no .) *)
  t.opaque_id ^ "." ^ t.signature

let token_of_string (s : string) : (token, string) result =
  match String.index_opt s '.' with
  | None -> Error "malformed token (no separator)"
  | Some i ->
      let opaque_id = String.sub s 0 i in
      let signature = String.sub s (i + 1) (String.length s - i - 1) in
      if opaque_id = "" || signature = "" then
        Error "malformed token (empty part)"
      else Ok { opaque_id; signature }

(* RPC request/response types *)
type verify_ssh_key_req = { key : string }
type verify_ssh_key_resp = Ok of user_id | Denied

let verify_ssh_key_resp_to_json = function
  | Ok uid -> `Assoc [ ("ok", `Bool true); ("user_id", `String uid) ]
  | Denied -> `Assoc [ ("ok", `Bool false) ]

let verify_ssh_key_resp_of_json (j : Yojson.Safe.t) : (verify_ssh_key_resp, string) result =
  try
    let assoc = match j with `Assoc a -> a | _ -> failwith "not assoc" in
    match List.assoc "ok" assoc with
    | `Bool true ->
        (match List.assoc "user_id" assoc with
         | `String uid -> Ok (Ok uid)
         | _ -> failwith "user_id")
    | `Bool false -> Ok Denied
    | _ -> failwith "ok"
  with Failure msg -> Error msg
