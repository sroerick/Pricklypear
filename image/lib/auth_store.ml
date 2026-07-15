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

let username_dns_ok (name : string) : bool =
  let name = String.lowercase_ascii (String.trim name) in
  let n = String.length name in
  n > 0 && n <= 63
  && (match name.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
  && (match name.[n - 1] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
  &&
  let rec loop i =
    if i >= n then true
    else
      match name.[i] with
      | 'a' .. 'z' | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> false
  in
  loop 0

let create_user (store : Store.t) ~(username : string) ~(is_admin : bool)
  : (user, string) result Lwt.t =
  let username = String.lowercase_ascii (String.trim username) in
  if not (username_dns_ok username) then
    Lwt.return
      (Error
         "username must be a DNS label: [a-z0-9]([a-z0-9-]*[a-z0-9])?")
  else
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

(** Per-user LLM provider key (auth.user-llm-keys). Not api_key (auth INTO PP). *)
type llm_key_meta = {
  last4 : string;
  label : string option;
  base : string option;
  model : string option;
}

let last4_of_key (key : string) : string =
  let n = String.length key in
  if n <= 4 then key else String.sub key (n - 4) 4

let set_llm_key (store : Store.t) ~(user_id : string) ~(api_key : string)
    ?(base : string option = None) ?(model : string option = None)
    ?(label : string option = None) () : (llm_key_meta, string) result Lwt.t =
  if String.trim api_key = "" then
    Lwt.return (Error "empty api_key")
  else
    match pg_conn store with
    | None -> Lwt.return (Error "no database")
    | Some conn ->
        Lwt.catch
          (fun () ->
            let last4 = last4_of_key api_key in
            let public_fields =
              [ ("last4", `String last4) ]
              @ (match label with Some l when l <> "" -> [ ("label", `String l) ] | _ -> [])
              @ (match base with Some b when b <> "" -> [ ("base", `String b) ] | _ -> [])
              @ (match model with Some m when m <> "" -> [ ("model", `String m) ] | _ -> [])
            in
            let public_json =
              Yojson.Safe.to_string (`Assoc public_fields)
            in
            let secret_json =
              Yojson.Safe.to_string
                (`Assoc [ ("api_key", `String api_key) ])
            in
            let* _ =
              Store.Pg.execute conn
                ~params:[ Pgx.Value.of_string user_id ]
                "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'llm_key'"
            in
            let sql =
              "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
              "VALUES ($1::uuid, 'llm_key', $2::jsonb, $3::jsonb)"
            in
            let* _ =
              Store.Pg.execute conn
                ~params:
                  [ Pgx.Value.of_string user_id;
                    Pgx.Value.of_string public_json;
                    Pgx.Value.of_string secret_json ]
                sql
            in
            Lwt.return
              (Ok
                 { last4;
                   label;
                   base;
                   model }))
          (fun exn ->
            let msg = Printexc.to_string exn in
            Printf.eprintf "[auth_store] set_llm_key: %s\n%!" msg;
            Lwt.return (Error msg))

let clear_llm_key (store : Store.t) ~(user_id : string)
  : (unit, string) result Lwt.t =
  match pg_conn store with
  | None -> Lwt.return (Error "no database")
  | Some conn ->
      Lwt.catch
        (fun () ->
          let* _ =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ]
              "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'llm_key'"
          in
          Lwt.return (Ok ()))
        (fun exn ->
          let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] clear_llm_key: %s\n%!" msg;
          Lwt.return (Error msg))

let get_llm_key_meta (store : Store.t) ~(user_id : string)
  : llm_key_meta option Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT public_data::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'llm_key' " ^
            "ORDER BY created_at DESC LIMIT 1"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0 with
               | None -> Lwt.return_none
               | Some js ->
                   (try
                      match Yojson.Safe.from_string js with
                      | `Assoc a ->
                          let last4 =
                            match List.assoc_opt "last4" a with
                            | Some (`String s) -> s
                            | _ -> "????"
                          in
                          let opt_str k =
                            match List.assoc_opt k a with
                            | Some (`String s) when s <> "" -> Some s
                            | _ -> None
                          in
                          Lwt.return_some
                            { last4;
                              label = opt_str "label";
                              base = opt_str "base";
                              model = opt_str "model" }
                      | _ -> Lwt.return_none
                    with _ -> Lwt.return_none))
          | [] -> Lwt.return_none)
        (fun exn ->
          Printf.eprintf "[auth_store] get_llm_key_meta: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_none)

let get_llm_key_secret (store : Store.t) ~(user_id : string)
  : (string * llm_key_meta) option Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT secret_data::text, public_data::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'llm_key' " ^
            "ORDER BY created_at DESC LIMIT 1"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0, cell_string row 1 with
               | Some secret_js, public_js_opt ->
                   let key_opt =
                     try
                       match Yojson.Safe.from_string secret_js with
                       | `Assoc a ->
                           (match List.assoc_opt "api_key" a with
                            | Some (`String k) when k <> "" -> Some k
                            | _ -> None)
                       | _ -> None
                     with _ -> None
                   in
                   (match key_opt with
                    | None -> Lwt.return_none
                    | Some key ->
                        let meta =
                          match public_js_opt with
                          | Some js ->
                              (try
                                 match Yojson.Safe.from_string js with
                                 | `Assoc a ->
                                     let last4 =
                                       match List.assoc_opt "last4" a with
                                       | Some (`String s) -> s
                                       | _ -> last4_of_key key
                                     in
                                     let opt_str k =
                                       match List.assoc_opt k a with
                                       | Some (`String s) when s <> "" -> Some s
                                       | _ -> None
                                     in
                                     { last4;
                                       label = opt_str "label";
                                       base = opt_str "base";
                                       model = opt_str "model" }
                                 | _ ->
                                     { last4 = last4_of_key key;
                                       label = None;
                                       base = None;
                                       model = None }
                               with _ ->
                                 { last4 = last4_of_key key;
                                   label = None;
                                   base = None;
                                   model = None })
                          | None ->
                              { last4 = last4_of_key key;
                                label = None;
                                base = None;
                                model = None }
                        in
                        Lwt.return_some (key, meta))
               | _ -> Lwt.return_none)
          | [] -> Lwt.return_none)
        (fun exn ->
          Printf.eprintf "[auth_store] get_llm_key_secret: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_none)

(** SSH public keys for REPL door (auth.user-ssh-repl phase 1). *)
type ssh_key_meta = {
  id : string;
  fingerprint : string;
  label : string option;
  openssh : string;
}

let ssh_fingerprint (openssh : string) : string =
  let open Digestif.SHA256 in
  let dig = digest_string (String.trim openssh) |> to_raw_string in
  let b64 = Base64.encode_string dig in
  "SHA256:" ^ b64

let parse_openssh_pubkey (s : string) : (string, string) result =
  let s = String.trim s in
  if s = "" then Error "empty key"
  else
    let parts =
      String.split_on_char ' ' s |> List.filter (fun p -> p <> "")
    in
    match parts with
    | typ :: _blob :: _ when String.starts_with ~prefix:"ssh-" typ ->
        if typ = "ssh-ed25519" || typ = "ssh-rsa" || typ = "ecdsa-sha2-nistp256"
        then Ok s
        else Error ("unsupported key type: " ^ typ)
    | _ -> Error "expected OpenSSH public key (ssh-ed25519 AAAA… [comment])"

let add_ssh_key (store : Store.t) ~(user_id : string) ~(openssh : string)
    ?(label : string option = None) () : (ssh_key_meta, string) result Lwt.t =
  match parse_openssh_pubkey openssh with
  | Error e -> Lwt.return (Error e)
  | Ok openssh ->
      match pg_conn store with
      | None -> Lwt.return (Error "no database")
      | Some conn ->
          Lwt.catch
            (fun () ->
              let fp = ssh_fingerprint openssh in
              let public_fields =
                [ ("openssh", `String openssh)
                ; ("fingerprint", `String fp)
                ]
                @ (match label with
                   | Some l when l <> "" -> [ ("label", `String l) ]
                   | _ -> [])
              in
              let public_json =
                Yojson.Safe.to_string (`Assoc public_fields)
              in
              let sql =
                "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
                "VALUES ($1::uuid, 'ssh_key', $2::jsonb, NULL) " ^
                "RETURNING id::text"
              in
              let* rows =
                Store.Pg.execute conn
                  ~params:
                    [ Pgx.Value.of_string user_id;
                      Pgx.Value.of_string public_json ]
                  sql
              in
              match rows with
              | row :: _ ->
                  (match cell_string row 0 with
                   | Some id ->
                       Lwt.return
                         (Ok
                            { id;
                              fingerprint = fp;
                              label;
                              openssh })
                   | None -> Lwt.return (Error "add_ssh_key: no id"))
              | [] -> Lwt.return (Error "add_ssh_key: no row"))
            (fun exn ->
              let msg = Printexc.to_string exn in
              Printf.eprintf "[auth_store] add_ssh_key: %s\n%!" msg;
              Lwt.return (Error msg))

let list_ssh_keys (store : Store.t) ~(user_id : string)
  : ssh_key_meta list Lwt.t =
  match pg_conn store with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT id::text, public_data::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'ssh_key' " ^
            "ORDER BY created_at"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          let metas =
            List.filter_map
              (fun row ->
                match cell_string row 0, cell_string row 1 with
                | Some id, Some js ->
                    (try
                       match Yojson.Safe.from_string js with
                       | `Assoc a ->
                           let openssh =
                             match List.assoc_opt "openssh" a with
                             | Some (`String s) -> s
                             | _ -> ""
                           in
                           let fingerprint =
                             match List.assoc_opt "fingerprint" a with
                             | Some (`String s) -> s
                             | _ -> ssh_fingerprint openssh
                           in
                           let label =
                             match List.assoc_opt "label" a with
                             | Some (`String s) when s <> "" -> Some s
                             | _ -> None
                           in
                           if openssh = "" then None
                           else Some { id; fingerprint; label; openssh }
                       | _ -> None
                     with _ -> None)
                | _ -> None)
              rows
          in
          Lwt.return metas)
        (fun exn ->
          Printf.eprintf "[auth_store] list_ssh_keys: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return [])

let revoke_ssh_key (store : Store.t) ~(user_id : string)
    ~(id_or_fingerprint : string) : (unit, string) result Lwt.t =
  match pg_conn store with
  | None -> Lwt.return (Error "no database")
  | Some conn ->
      Lwt.catch
        (fun () ->
          (* Delete by credential id OR fingerprint match in public_data *)
          let sql =
            "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'ssh_key' " ^
            "AND (id::text = $2 OR public_data->>'fingerprint' = $2 " ^
            "     OR public_data->>'openssh' = $2)"
          in
          let* _ =
            Store.Pg.execute conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string (String.trim id_or_fingerprint) ]
              sql
          in
          Lwt.return (Ok ()))
        (fun exn ->
          let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] revoke_ssh_key: %s\n%!" msg;
          Lwt.return (Error msg))

let verify_ssh_key (store : Store.t) ~(openssh_or_blob : string)
    ?source_ip () : user option Lwt.t =
  match pg_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let presented = String.trim openssh_or_blob in
          let fp = ssh_fingerprint presented in
          let sql =
            "SELECT u.id::text, u.username, u.is_admin::text, c.id::text " ^
            "FROM credentials c JOIN users u ON u.id = c.user_id " ^
            "WHERE c.kind = 'ssh_key' " ^
            "AND (c.public_data->>'openssh' = $1 " ^
            "     OR c.public_data->>'fingerprint' = $2) " ^
            "LIMIT 1"
          in
          let* rows =
            Store.Pg.execute conn
              ~params:
                [ Pgx.Value.of_string presented;
                  Pgx.Value.of_string fp ]
              sql
          in
          match rows with
          | row :: _ ->
              (match cell_string row 0, cell_string row 1 with
               | Some id, Some uname ->
                   let user =
                     { id; username = uname; is_admin = cell_bool row 2 }
                   in
                   let* () =
                     log_auth store ~user_id:id ~kind:"ssh_key" ~success:true
                       ?source_ip ()
                   in
                   (match cell_string row 3 with
                    | Some cid ->
                        let* _ =
                          Store.Pg.execute conn
                            ~params:[ Pgx.Value.of_string cid ]
                            "UPDATE credentials SET last_used_at = now() \
                             WHERE id = $1::uuid"
                        in
                        Lwt.return_some user
                    | None -> Lwt.return_some user)
               | _ ->
                   let* () =
                     log_auth store ~kind:"ssh_key" ~success:false ?source_ip ()
                   in
                   Lwt.return_none)
          | [] ->
              let* () =
                log_auth store ~kind:"ssh_key" ~success:false ?source_ip ()
              in
              Lwt.return_none)
        (fun exn ->
          Printf.eprintf "[auth_store] verify_ssh_key: %s\n%!"
            (Printexc.to_string exn);
          Lwt.return_none)

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
