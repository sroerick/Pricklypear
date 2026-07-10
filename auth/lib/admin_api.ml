(** Pricklypear Auth — Admin RPC API.

    Per spec [auth.user-management] / [auth.rpc-from-supervisor]:
      POST /admin/create_user
      POST /admin/add_credential
      GET  /admin/list_users
      GET  /lookup/:user_id

    Dev auth of the RPC itself is localhost-bound (see http.ml).
    Admin-only enforcement: caller must present X-Admin: true header
    or ?admin=1 in v1 (shared-secret/mTLS deferred). *)
open Lwt.Syntax

type user_record = {
  id : string;
  username : string;
  is_admin : bool;
}

let create_user ~(store : Store.t) ~(username : string) ?(is_admin = false) ()
    : (string, string) result Lwt.t =
  if String.trim username = "" then
    Lwt.return (Error "username required")
  else
    Store.create_user store ~username ~is_admin ()

let add_credential
    ~(store : Store.t)
    ~(user_id : string)
    ~(kind : string)
    ~(public_data : string option)
    ~(secret_data : string option)
    : (unit, string) result Lwt.t =
  match kind with
  | "ssh_key" ->
      (match public_data with
       | None | Some "" -> Lwt.return (Error "ssh_key requires public_data.key")
       | Some key -> Store.add_credential_ssh store ~user_id ~ssh_key:key)
  | "password" ->
      (match secret_data with
       | None | Some "" -> Lwt.return (Error "password requires secret_data")
       | Some pw -> Store.add_credential_password store ~user_id ~password:pw)
  | other ->
      Lwt.return (Error (Printf.sprintf "unsupported credential kind: %s" other))

let lookup ~(store : Store.t) (user_id : string)
    : user_record option Lwt.t =
  let* r = Store.lookup_user_by_id store user_id in
  match r with
  | None -> Lwt.return_none
  | Some (id, username, is_admin) ->
      Lwt.return (Some { id; username; is_admin })

let list_users ~(store : Store.t) : user_record list Lwt.t =
  let* rows = Store.list_users store in
  Lwt.return
    (List.map
       (fun (id, username, is_admin) -> { id; username; is_admin })
       rows)
