(** Pricklypear Image — in-process auth store (single-image Option B).

    Operates on migrations/002_auth.sql tables from the image process.
    No separate authd. Auth holds verify + mint only; session identity
    is attached per HTTP request (auth.multi-user-product / web-login). *)
open Lwt.Syntax

type user = {
  id : string;
  username : string;
  is_admin : bool;
}

type session_row = {
  id : string;
  user_id : string;
  username : string;
  is_admin : bool;
  expires_at : string;
}

let env_or name dft =
  match Sys.getenv_opt name with Some s -> s | None -> dft

let pg_conn (store : Store.t) : Store.Pg.t option = Store.current_conn store

let cell_string row i =
  match List.nth_opt row i with
  | Some v -> Pgx.Value.to_string v
  | None -> None

let cell_bool row i =
  match cell_string row i with
  | Some ("t" | "true" | "TRUE" | "1") -> true
  | Some ("f" | "false" | "FALSE" | "0") -> false
  | _ -> false

let log_auth (store : Store.t) ?user_id ~kind ~success ?source_ip ()
  : unit Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_unit
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "INSERT INTO auth_log (user_id, kind, success, source_ip) " ^
            "VALUES ($1, $2, $3, $4)"
          in
          let* _ =
            Store.Pg.execute conn
              ~params:
                [ (match user_id with
                   | Some u -> Pgx.Value.of_string u
                   | None -> Pgx.Value.null);
                  Pgx.Value.of_string kind;
                  Pgx.Value.of_bool success;
                  (match source_ip with
                   | Some ip -> Pgx.Value.of_string ip
                   | None -> Pgx.Value.null) ]
              sql
          in
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[auth_store] log_auth: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_unit)

let count_users (store : Store.t) : int Lwt.t =
  match pg_conn store with
  | None -> Lwt.return 0
  | Some conn ->
      Lwt.catch
        (fun () ->
          let* rows =
            Store.Pg.execute conn "SELECT count(*)::text FROM users"
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0 with
               | Some s ->
                   Lwt.return
                     (int_of_string_opt s |> Option.value ~default:0)
               | None -> Lwt.return 0)
          | [] -> Lwt.return 0)
        (fun exn ->
          Printf.eprintf "[auth_store] count_users: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return 0)

let get_user_by_username (store : Store.t) (username : string)
  : user option Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT id::text, username, is_admin::text FROM users " ^
            "WHERE username = $1 LIMIT 1"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string username ] sql
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0, cell_string row 1 with
               | Some id, Some uname ->
                   Lwt.return_some
                     { id; username = uname; is_admin = cell_bool row 2 }
               | _ -> Lwt.return_none)
          | [] -> Lwt.return_none)
        (fun exn ->
          Printf.eprintf "[auth_store] get_user_by_username: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_none)

let get_user_by_id (store : Store.t) (user_id : string) : user option Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT id::text, username, is_admin::text FROM users " ^
            "WHERE id = $1::uuid LIMIT 1"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0, cell_string row 1 with
               | Some id, Some uname ->
                   Lwt.return_some
                     { id; username = uname; is_admin = cell_bool row 2 }
               | _ -> Lwt.return_none)
          | [] -> Lwt.return_none)
        (fun exn ->
          Printf.eprintf "[auth_store] get_user_by_id: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_none)

let create_user (store : Store.t) ~(username : string) ~(is_admin : bool)
  : (user, string) result Lwt.t =
  match pg_conn store with
  | None -> Lwt.return (Error "no database")
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "INSERT INTO users (username, is_admin) VALUES ($1, $2) " ^
            "RETURNING id::text, username, is_admin::text"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:
                [ Pgx.Value.of_string username;
                  Pgx.Value.of_bool is_admin ]
              sql
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0, cell_string row 1 with
               | Some id, Some uname ->
                   Lwt.return
                     (Ok
                        { id; username = uname;
                          is_admin = cell_bool row 2 })
               | _ -> Lwt.return (Error "create_user: bad row"))
          | [] -> Lwt.return (Error "create_user: no row"))
        (fun exn ->
          let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] create_user: %s\n%!" msg;
          Lwt.return (Error msg))

let set_password (store : Store.t) ~(user_id : string) ~(password : string)
  : (unit, string) result Lwt.t =
  match pg_conn store with
  | None -> Lwt.return (Error "no database")
  | Some conn ->
      Lwt.catch
        (fun () ->
          let hash = Auth_password.hash_password password in
          let secret =
            Yojson.Safe.to_string (`Assoc [ ("hash", `String hash) ])
          in
          (* One password credential per user: replace existing. *)
          let* _ =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ]
              "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'password'"
          in
          let sql =
            "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
            "VALUES ($1::uuid, 'password', NULL, $2::jsonb)"
          in
          let* _ =
            Store.Pg.execute conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string secret ]
              sql
          in
          Lwt.return (Ok ()))
        (fun exn ->
          let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] set_password: %s\n%!" msg;
          Lwt.return (Error msg))

let password_hash_for_user (store : Store.t) ~(username : string)
  : (user * string) option Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT u.id::text, u.username, u.is_admin::text, " ^
            "       c.secret_data::text " ^
            "FROM users u " ^
            "JOIN credentials c ON c.user_id = u.id " ^
            "WHERE u.username = $1 AND c.kind = 'password' " ^
            "ORDER BY c.created_at DESC LIMIT 1"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string username ] sql
          in
          match rows with
          | row :: _ ->
              (match
                 cell_string row 0, cell_string row 1, cell_string row 3
               with
               | Some id, Some uname, Some secret_json ->
                   let hash_opt =
                     try
                       match Yojson.Safe.from_string secret_json with
                       | `Assoc a ->
                           (match List.assoc_opt "hash" a with
                            | Some (`String h) -> Some h
                            | _ -> None)
                       | `String h -> Some h
                       | _ -> None
                     with _ -> None
                   in
                   (match hash_opt with
                    | Some h ->
                        Lwt.return_some
                          ( { id; username = uname;
                              is_admin = cell_bool row 2 },
                            h )
                    | None -> Lwt.return_none)
               | _ -> Lwt.return_none)
          | [] -> Lwt.return_none)
        (fun exn ->
          Printf.eprintf "[auth_store] password_hash_for_user: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_none)

let verify_password (store : Store.t) ~(username : string)
    ~(password : string) ?source_ip () : user option Lwt.t =
  let* found = password_hash_for_user store ~username in
  match found with
  | None ->
      let* () =
        log_auth store ~kind:"password" ~success:false ?source_ip ()
      in
      Lwt.return_none
  | Some (user, stored) ->
      if Auth_password.verify_password ~stored password then
        let* () =
          log_auth store ~user_id:user.id ~kind:"password" ~success:true
            ?source_ip ()
        in
        (* Touch last_used_at *)
        (match pg_conn store with
         | None -> Lwt.return_some user
         | Some conn ->
             let* _ =
               Lwt.catch
                 (fun () ->
                   Store.Pg.execute conn
                     ~params:[ Pgx.Value.of_string user.id ]
                     "UPDATE credentials SET last_used_at = now() \
                      WHERE user_id = $1::uuid AND kind = 'password'")
                 (fun _ -> Lwt.return [])
             in
             Lwt.return_some user)
      else
        let* () =
          log_auth store ~user_id:user.id ~kind:"password" ~success:false
            ?source_ip ()
        in
        Lwt.return_none

(** Session token: opaque random id. Cookie carries raw token; DB stores
    only SHA-256 hex of the token (auth.web-login). *)
let token_hash (raw_token : string) : string =
  Auth_password.sha256_hex raw_token

let mint_session
    (store : Store.t)
    ~(user_id : string)
    ?(ttl_seconds = 60 * 60 * 24 * 14)
    ?user_agent
    ?source_ip
    () : (string, string) result Lwt.t =
  match pg_conn store with
  | None -> Lwt.return (Error "no database")
  | Some conn ->
      Lwt.catch
        (fun () ->
          let raw =
            Base64.encode_string ~pad:false
              (Auth_password.random_bytes 32)
          in
          let th = token_hash raw in
          let sql =
            "INSERT INTO auth_sessions \
             (user_id, token_hash, expires_at) \
             VALUES ($1::uuid, $2, now() + ($3::text || ' seconds')::interval) \
             RETURNING id::text"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string th;
                  Pgx.Value.of_string (string_of_int ttl_seconds) ]
              sql
          in
          let* () =
            log_auth store ~user_id ~kind:"mint" ~success:true ?source_ip ()
          in
          ignore user_agent;
          match rows with
          | _ :: _ -> Lwt.return (Ok raw)
          | [] -> Lwt.return (Error "mint_session: no row"))
        (fun exn ->
          let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] mint_session: %s\n%!" msg;
          Lwt.return (Error msg))

let lookup_session (store : Store.t) ~(raw_token : string)
  : session_row option Lwt.t =
  if raw_token = "" then Lwt.return_none
  else
    match pg_conn store with
    | None -> Lwt.return_none
    | Some conn ->
        Lwt.catch
          (fun () ->
            let th = token_hash raw_token in
            let sql =
              "SELECT s.id::text, s.user_id::text, u.username, \
                      u.is_admin::text, s.expires_at::text \
               FROM auth_sessions s \
               JOIN users u ON u.id = s.user_id \
               WHERE s.token_hash = $1 \
                 AND s.revoked_at IS NULL \
                 AND s.expires_at > now() \
               LIMIT 1"
            in
            let* rows =
              Store.Pg.execute conn
                ~params:[ Pgx.Value.of_string th ] sql
            in
            match rows with
            | row :: _ ->
                (match
                   cell_string row 0,
                   cell_string row 1,
                   cell_string row 2,
                   cell_string row 4
                 with
                 | Some id, Some uid, Some uname, Some exp ->
                     Lwt.return_some
                       { id; user_id = uid; username = uname;
                         is_admin = cell_bool row 3;
                         expires_at = exp }
                 | _ -> Lwt.return_none)
            | [] -> Lwt.return_none)
          (fun exn ->
            Printf.eprintf "[auth_store] lookup_session: %s\n%!"
              (Printexc.to_string exn);
            Lwt.return_none)

let revoke_session (store : Store.t) ~(raw_token : string) ?source_ip ()
  : unit Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_unit
  | Some conn ->
      Lwt.catch
        (fun () ->
          let th = token_hash raw_token in
          let sql =
            "UPDATE auth_sessions SET revoked_at = now() \
             WHERE token_hash = $1 AND revoked_at IS NULL \
             RETURNING user_id::text"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string th ] sql
          in
          match rows with
          | row :: _ ->
              let uid = cell_string row 0 in
              log_auth store ?user_id:uid ~kind:"logout" ~success:true
                ?source_ip ()
          | [] -> Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[auth_store] revoke_session: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_unit)

let revoke_all_sessions (store : Store.t) ~(user_id : string) : int Lwt.t =
  match pg_conn store with
  | None -> Lwt.return 0
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "UPDATE auth_sessions SET revoked_at = now() \
             WHERE user_id = $1::uuid AND revoked_at IS NULL \
             RETURNING id"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          let* () =
            log_auth store ~user_id ~kind:"revoke_all" ~success:true ()
          in
          Lwt.return (List.length rows))
        (fun exn ->
          Printf.eprintf "[auth_store] revoke_all_sessions: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return 0)

let list_users (store : Store.t) : user list Lwt.t =
  match pg_conn store with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let* rows =
            Store.Pg.execute conn
              "SELECT id::text, username, is_admin::text \
               FROM users ORDER BY username"
          in
          let users =
            List.filter_map
              (fun row ->
                match cell_string row 0, cell_string row 1 with
                | Some id, Some uname ->
                    Some
                      { id; username = uname;
                        is_admin = cell_bool row 2 }
                | _ -> None)
              rows
          in
          Lwt.return users)
        (fun exn ->
          Printf.eprintf "[auth_store] list_users: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return [])

(** First boot: if users table empty, create admin from env.
    PP_BOOTSTRAP_USER (default admin), PP_BOOTSTRAP_PASSWORD (default admin).
    If admin already exists but has no password credential, set one from env
    (dev recovery for pre-auth DBs that only had ssh_key). *)
let bootstrap_admin_if_empty (store : Store.t) : user option Lwt.t =
  let username = env_or "PP_BOOTSTRAP_USER" "admin" in
  let password = env_or "PP_BOOTSTRAP_PASSWORD" "admin" in
  let* n = count_users store in
  if n > 0 then
    let* admin = get_user_by_username store username in
    match admin with
    | None -> Lwt.return_none
    | Some user ->
        let* has_pw = password_hash_for_user store ~username in
        (match has_pw with
         | Some _ -> Lwt.return_none
         | None ->
             let* pw = set_password store ~user_id:user.id ~password in
             (match pw with
              | Error e ->
                  Printf.eprintf
                    "[auth_store] bootstrap password for existing %s: %s\n%!"
                    username e;
                  Lwt.return_none
              | Ok () ->
                  Printf.eprintf
                    "[auth_store] set password for existing admin %s\n%!"
                    username;
                  Lwt.return_some user))
  else
    let* created = create_user store ~username ~is_admin:true in
    match created with
    | Error e ->
        Printf.eprintf "[auth_store] bootstrap failed: %s\n%!" e;
        Lwt.return_none
    | Ok user ->
        let* pw = set_password store ~user_id:user.id ~password in
        (match pw with
         | Error e ->
             Printf.eprintf "[auth_store] bootstrap password: %s\n%!" e;
             Lwt.return_none
         | Ok () ->
             Printf.eprintf
               "[auth_store] bootstrapped admin user %s (set PP_BOOTSTRAP_PASSWORD)\n%!"
               username;
             let* () =
               log_auth store ~user_id:user.id ~kind:"bootstrap"
                 ~success:true ()
             in
             Lwt.return_some user)
