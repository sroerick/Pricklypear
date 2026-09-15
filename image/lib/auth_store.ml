(** Pricklypear Image — in-process auth store (single-image Option B).

    Operates on migrations/002_auth.sql tables from the image process.
    No separate authd. Auth holds verify + mint only; session identity
    is attached per HTTP request (auth.multi-user-product / web-login).

    ERROR POLICY (i-1788671060-2): auth LOOKUPS no longer swallow DB
    errors into None/0/true — an outage must not present as "bad
    credentials", an empty user table (which would re-trigger admin
    bootstrap), or a fail-open allow_agent_promote default. Catch-arms
    log-and-propagate via [Store_db.reraise]; the HTTP boundary renders
    500. Write actions keep their honest (unit, string) result Error
    contracts (callers surface the failure). Genuine probes (log_auth,
    the last_used_at touch) keep a swallow and say so. *)
(* H2.4: DB execute via Store_db.exec *)

type user = {
  id : string;
  username : string;
  is_admin : bool;
  user_type : string;           (* "human" | "agent" *)
  parent_user_id : string option;  (* the human an agent belongs to; None for humans *)
}

type session_row = {
  id : string;
  user_id : string;
  username : string;
  is_admin : bool;
  expires_at : string;
}

(* row-cell accessors live in Row (was a local copy here). *)

(* ---- tmp/eval-fast-path: positive password-verify cache ----

   i-1787300749: every Basic-auth request pays a full argon2id verify
   (~0.2-1s CPU) plus 3-4 DB statements, so agent tooling pays ~0.5s per
   eval. Cache POSITIVE verifies keyed (username, sha256(password)) —
   the password itself is never stored, only its digest inside the key.
   Negative results are never cached (throttle + timing-flatten path
   stay live). Entries expire after [verify_cache_ttl] seconds and the
   whole cache is dropped whenever any password is written
   (set_password), so a password change invalidates immediately.

   Cached identity fields (is_admin/user_type) may lag admin edits by up
   to the TTL — acceptable for the same reason session cookies are
   long-lived; revoke-sessions does not (and did not) affect Basic. *)

let verify_cache_ttl = 300.0
let verify_cache_max = 64
let verify_cache : (string, user * float) Hashtbl.t = Hashtbl.create 16
let verify_cache_mu = Mutex.create ()

let verify_cache_key ~username ~password =
  let open Digestif.SHA256 in
  let hex = to_hex (digest_string (username ^ "\x00" ^ password)) in
  username ^ "|" ^ hex

let verify_cache_get key =
  Mutex.lock verify_cache_mu;
  let r =
    match Hashtbl.find_opt verify_cache key with
    | Some (u, t) when Unix.gettimeofday () -. t < verify_cache_ttl ->
        Some u
    | _ ->
        (match Hashtbl.find_opt verify_cache key with
         | Some _ -> Hashtbl.remove verify_cache key
         | None -> ());
        None
  in
  Mutex.unlock verify_cache_mu;
  r

let verify_cache_put key user =
  Mutex.lock verify_cache_mu;
  (if Hashtbl.length verify_cache >= verify_cache_max then
     Hashtbl.reset verify_cache);
  Hashtbl.replace verify_cache key (user, Unix.gettimeofday ());
  Mutex.unlock verify_cache_mu

(* Drop every cached verify: called on any credential rewrite. *)
let clear_verify_cache () =
  Mutex.lock verify_cache_mu;
  Hashtbl.reset verify_cache;
  Mutex.unlock verify_cache_mu

let last4_of_key (key : string) : string =
  let n = String.length key in
  if n <= 4 then key else String.sub key (n - 4) 4

let env_or name dft =
  match Sys.getenv_opt name with Some s -> s | None -> dft

let pg_conn (store : Store.t) : Io_db.Db.conn option = Io_db.Db.current_conn store

let log_auth (store : Store.t) ?user_id ~kind ~success ?source_ip ()
  : unit =
  (* PROBE (i-1788671060-2 keep): the audit trail must never block or
     fail the auth decision it records — a failed INSERT logs to stderr
     and returns, like any best-effort telemetry emit. *)
  match pg_conn store with
  | None -> ()
  | Some conn ->
      (try
      let sql =
            "INSERT INTO auth_log (user_id, kind, success, source_ip) " ^
            "VALUES ($1, $2, $3, $4)"
          in
          let _ =
            Store_db.exec conn
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
          ()
    with exn ->
      Printf.eprintf "[auth_store] log_auth: %s\n%!"
            (Printexc.to_string exn);
          ())

let count_users (store : Store.t) : int =
  match pg_conn store with
  | None -> 0
  | Some conn ->
      (try
      let rows =
            Store_db.exec conn "SELECT count(*)::text FROM users"
          in
          match rows with
          | row :: _ ->
              (match Row.cell_string row 0 with
               | Some s ->
                   (int_of_string_opt s |> Option.value ~default:0)
               | None -> 0)
          | [] -> 0
    with exn -> Store_db.reraise ~op:"count_users" exn)

let user_cols = "id::text, username, is_admin::text, user_type, parent_user_id::text"

let user_of_row row =
  match Row.cell_string row 0, Row.cell_string row 1, Row.cell_string row 3 with
  | Some id, Some uname, Some utype ->
      Some { id; username = uname;
             is_admin = Row.cell_bool row 2;
             user_type = utype;
             parent_user_id = Row.cell_uuid_opt row 4 }
  | _ -> None

let get_user_by_username (store : Store.t) (username : string)
  : user option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT " ^ user_cols ^ " FROM users WHERE username = $1 LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string username ] sql
          in
          match rows with
          | row :: _ ->
              (match user_of_row row with
               | Some u -> Some u
               | None -> None)
          | [] -> None
    with exn -> Store_db.reraise ~op:"get_user_by_username" exn)

let get_user_by_id (store : Store.t) (user_id : string) : user option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT " ^ user_cols ^ " FROM users WHERE id = $1::uuid LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          match rows with
          | row :: _ ->
              (match user_of_row row with
               | Some u -> Some u
               | None -> None)
          | [] -> None
    with exn -> Store_db.reraise ~op:"get_user_by_id" exn)

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
  : (user, string) result =
  let username = String.lowercase_ascii (String.trim username) in
  if not (username_dns_ok username) then
    (Error
         "username must be a DNS label: [a-z0-9]([a-z0-9-]*[a-z0-9])?")
  else
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let sql =
            "INSERT INTO users (username, is_admin) VALUES ($1, $2) " ^
            "RETURNING " ^ user_cols
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string username;
                  Pgx.Value.of_bool is_admin ]
              sql
          in
          match rows with
          | row :: _ ->
              (match user_of_row row with
               | Some u -> (Ok u)
               | None -> (Error "create_user: bad row"))
          | [] -> (Error "create_user: no row")
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] create_user: %s\n%!" msg;
          (Error msg))

(** Create an agent user attached to a parent human.
    username = "<parent>/agent", user_type='agent', parent_user_id=parent.
    is_admin=false (agents never admin). Idempotent: returns existing if any. *)
let create_agent (store : Store.t) ~(parent_username : string)
  : (user, string) result =
  let parent_username = String.lowercase_ascii (String.trim parent_username) in
  let agent_username = parent_username ^ "/agent" in
  (* Idempotent: return existing agent if one exists for this parent. *)
  let existing = get_user_by_username store agent_username in
  match existing with
  | Some u when u.user_type = "agent" -> (Ok u)
  | _ ->
      (match pg_conn store with
       | None -> (Error "no database")
       | Some conn ->
           let parent = get_user_by_username store parent_username in
           (match parent with
            | None ->
                (Error ("no such parent user: " ^ parent_username))
            | Some p when p.user_type = "agent" ->
                (Error "agent cannot be a parent of another agent")
            | Some p ->
                (try
      let sql =
                      "INSERT INTO users " ^
                      "(username, is_admin, user_type, parent_user_id) " ^
                      "VALUES ($1, false, 'agent', $2::uuid) " ^
                      "RETURNING " ^ user_cols
                    in
                    let rows =
                      Store_db.exec conn
                        ~params:
                          [ Pgx.Value.of_string agent_username;
                            Pgx.Value.of_string p.id ]
                        sql
                    in
                    match rows with
                    | row :: _ ->
                        (match user_of_row row with
                         | Some u -> (Ok u)
                         | None -> (Error "create_agent: bad row"))
                    | [] -> (Error "create_agent: no row")
    with exn ->
      let msg = Printexc.to_string exn in
                    Printf.eprintf "[auth_store] create_agent: %s\n%!" msg;
                    (Error msg))))

let set_password (store : Store.t) ~(user_id : string) ~(password : string)
  : (unit, string) result =
  (* tmp/eval-fast-path: any credential rewrite invalidates cached
     verifies for everyone (simplest correct policy). *)
  let () = clear_verify_cache () in
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let hash = Auth_password.hash_password password in
          let secret =
            Yojson.Safe.to_string (`Assoc [ ("hash", `String hash) ])
          in
          (* One password credential per user: replace existing. *)
          let _ =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ]
              "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'password'"
          in
          let sql =
            "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
            "VALUES ($1::uuid, 'password', NULL, $2::jsonb)"
          in
          let _ =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string secret ]
              sql
          in
          (Ok ())
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] set_password: %s\n%!" msg;
          (Error msg))

let password_hash_for_user (store : Store.t) ~(username : string)
  : (user * string) option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT u.id::text, u.username, u.is_admin::text, u.user_type, " ^
            "       u.parent_user_id::text, c.secret_data::text " ^
            "FROM users u " ^
            "JOIN credentials c ON c.user_id = u.id " ^
            "WHERE u.username = $1 AND c.kind = 'password' " ^
            "ORDER BY c.created_at DESC LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string username ] sql
          in
          match rows with
          | row :: _ ->
              (match
                 Row.cell_string row 0, Row.cell_string row 1, Row.cell_string row 5
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
                        Some
                          ( { id; username = uname;
                              is_admin = Row.cell_bool row 2;
                              user_type =
                                (match Row.cell_string row 3 with
                                 | Some s -> s | None -> "human");
                              parent_user_id = Row.cell_uuid_opt row 4 },
                            h )
                    | None -> None)
               | _ -> None)
          | [] -> None
    with exn -> Store_db.reraise ~op:"password_hash_for_user" exn)

let verify_password (store : Store.t) ~(username : string)
    ~(password : string) ?source_ip () : user option =
  (* Login throttle (i-1787970741): keyed by lowercase username so case
     variants share one bucket, checked BEFORE the DB lookup — admit=false
     returns None immediately with no DB work. Only ever engages after
     recorded failures, so ordinary correct logins are never blocked. *)
  let throttle_key = String.lowercase_ascii username in
  if not (Auth_throttle.admit throttle_key (Auth_throttle.now ())) then None
  else
    (* tmp/eval-fast-path: cached positive verify (i-1787300749) — no
       argon2, no DB, no log on hit. *)
    let key = verify_cache_key ~username ~password in
    match verify_cache_get key with
    | Some user -> Some user
    | None ->
    let found = password_hash_for_user store ~username in
    match found with
    | None ->
        (* Timing flatten: unknown usernames still pay one argon2 verify
           against a fixed dummy hash, so known/unknown accounts cost the
           same and are not distinguishable by latency. Counts as a
           throttle failure. *)
        let _ =
          Auth_password.verify_password
            ~stored:(Lazy.force Auth_password.dummy_hash) password
        in
        let () =
          Auth_throttle.record_failure throttle_key (Auth_throttle.now ());
          log_auth store ~kind:"password" ~success:false ?source_ip ()
        in
        None
    | Some (user, stored) ->
        if Auth_password.verify_password ~stored password then
          let () =
            Auth_throttle.clear throttle_key;
            verify_cache_put key user;
            log_auth store ~user_id:user.id ~kind:"password" ~success:true
              ?source_ip ()
          in
          (* Transparent argon2id upgrade (BORGE-1785545259): a successful
             legacy-scheme verify rewrites the stored hash to argon2id, so
             principals migrate without a flag day. *)
          if Auth_password.needs_upgrade stored then
            (match set_password store ~user_id:user.id ~password with
             | Ok () ->
                 Printf.eprintf
                   "[auth] %s password upgraded interim -> argon2id\n%!"
                   user.username
             | Error e ->
                 Printf.eprintf "[auth] %s argon2id upgrade failed: %s\n%!"
                   user.username e);
          (* Touch last_used_at — PROBE (i-1788671060-2 keep): telemetry
             only; login must succeed even if the touch fails. *)
          (match pg_conn store with
           | None -> Some user
           | Some conn ->
               let _ =
                 (try
      Store_db.exec conn
                       ~params:[ Pgx.Value.of_string user.id ]
                       "UPDATE credentials SET last_used_at = now() \
                        WHERE user_id = $1::uuid AND kind = 'password'"
    with _ ->
      [])
               in
               Some user)
        else
          let () =
            Auth_throttle.record_failure throttle_key (Auth_throttle.now ());
            log_auth store ~user_id:user.id ~kind:"password" ~success:false
              ?source_ip ()
          in
          None

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
    () : (string, string) result =
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
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
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string th;
                  Pgx.Value.of_string (string_of_int ttl_seconds) ]
              sql
          in
          let () =
            log_auth store ~user_id ~kind:"mint" ~success:true ?source_ip ()
          in
          ignore user_agent;
          match rows with
          | _ :: _ -> (Ok raw)
          | [] -> (Error "mint_session: no row")
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] mint_session: %s\n%!" msg;
          (Error msg))

let lookup_session (store : Store.t) ~(raw_token : string)
  : session_row option =
  if raw_token = "" then None
  else
    match pg_conn store with
    | None -> None
    | Some conn ->
        (try
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
            let rows =
              Store_db.exec conn
                ~params:[ Pgx.Value.of_string th ] sql
            in
            match rows with
            | row :: _ ->
                (match
                   Row.cell_string row 0,
                   Row.cell_string row 1,
                   Row.cell_string row 2,
                   Row.cell_string row 4
                 with
                 | Some id, Some uid, Some uname, Some exp ->
                     Some
                       { id; user_id = uid; username = uname;
                         is_admin = Row.cell_bool row 3;
                         expires_at = exp }
                 | _ -> None)
            | [] -> None
    with exn -> Store_db.reraise ~op:"lookup_session" exn)

(* tmp/eval-fast-path: cookie identity in ONE query (was lookup_session
   + get_user_by_id = 2 round trips per request). Returns the full user
   for attach when the session row is live (unrevoked, unexpired). *)
let lookup_session_user (store : Store.t) ~(raw_token : string)
  : user option =
  if raw_token = "" then None
  else
    match pg_conn store with
    | None -> None
    | Some conn ->
        (try
          let th = token_hash raw_token in
          (* user_cols is unqualified (single-table sites); here it sits inside a
             JOIN of auth_sessions s x users u, both owning "id", so the bare
             columns are ambiguous (42702). Qualify explicitly. i-1788690774. *)
          let sql =
            "SELECT u.id::text, u.username, u.is_admin::text, u.user_type, "
            ^ "u.parent_user_id::text "
            ^ "FROM auth_sessions s "
            ^ "JOIN users u ON u.id = s.user_id "
            ^ "WHERE s.token_hash = $1 "
            ^ "AND s.revoked_at IS NULL "
            ^ "AND s.expires_at > now() "
            ^ "LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string th ] sql
          in
          match rows with
          | row :: _ -> user_of_row row
          | [] -> None
        with exn -> Store_db.reraise ~op:"lookup_session_user" exn)

let revoke_session (store : Store.t) ~(raw_token : string) ?source_ip ()
  : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
      (try
      let th = token_hash raw_token in
          let sql =
            "UPDATE auth_sessions SET revoked_at = now() \
             WHERE token_hash = $1 AND revoked_at IS NULL \
             RETURNING user_id::text"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string th ] sql
          in
          match rows with
          | row :: _ ->
              let uid = Row.cell_string row 0 in
              log_auth store ?user_id:uid ~kind:"logout" ~success:true
                ?source_ip ()
          | [] -> ()
    with exn -> Store_db.reraise ~op:"revoke_session" exn)

let revoke_all_sessions (store : Store.t) ~(user_id : string) : int =
  match pg_conn store with
  | None -> 0
  | Some conn ->
      (try
      let sql =
            "UPDATE auth_sessions SET revoked_at = now() \
             WHERE user_id = $1::uuid AND revoked_at IS NULL \
             RETURNING id"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          let () =
            log_auth store ~user_id ~kind:"revoke_all" ~success:true ()
          in
          (List.length rows)
    with exn -> Store_db.reraise ~op:"revoke_all_sessions" exn)

(** API token (Bearer) credentials — auth INTO the OS for agents/pp_eval.
    kind='api_key' rows. Raw token returned ONCE at issue time; only the
    SHA-256 hash is stored. Per-user; no global tokens. *)
let issue_token (store : Store.t) ~(user_id : string)
    ?(label : string option = None) () : (string, string) result =
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let raw =
            Base64.encode_string ~pad:false
              (Auth_password.random_bytes 32)
          in
          let th = token_hash raw in
          let public_fields =
            [ ("token_last4", `String (last4_of_key raw)) ]
            @ (match label with
               | Some l when l <> "" -> [ ("label", `String l) ]
               | _ -> [])
          in
          let public_json = Yojson.Safe.to_string (`Assoc public_fields) in
          let secret_json =
            Yojson.Safe.to_string (`Assoc [ ("token_hash", `String th) ])
          in
          let sql =
            "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
            "VALUES ($1::uuid, 'api_key', $2::jsonb, $3::jsonb) " ^
            "RETURNING id::text"
          in
          let _ =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string public_json;
                  Pgx.Value.of_string secret_json ]
              sql
          in
          let () =
            log_auth store ~user_id ~kind:"issue_token" ~success:true ()
          in
          (Ok raw)
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] issue_token: %s\n%!" msg;
          (Error msg))

let verify_token (store : Store.t) ~(raw_token : string) ?source_ip ()
  : user option =
  if raw_token = "" then None
  else
    match pg_conn store with
    | None -> None
    | Some conn ->
        (try
      let th = token_hash raw_token in
            let sql =
              "SELECT " ^
              "  u.id::text, u.username, u.is_admin::text, u.user_type, " ^
              "  u.parent_user_id::text, c.id::text " ^
              "FROM credentials c JOIN users u ON u.id = c.user_id " ^
              "WHERE c.kind = 'api_key' " ^
              "  AND c.secret_data->>'token_hash' = $1 " ^
              "LIMIT 1"
            in
            let rows =
              Store_db.exec conn
                ~params:[ Pgx.Value.of_string th ] sql
            in
            match rows with
            | row :: _ ->
                (match user_of_row row with
                 | Some user ->
                     (match Row.cell_string row 5 with
                      | Some cid ->
                          let _ =
                            Store_db.exec conn
                              ~params:[ Pgx.Value.of_string cid ]
                              ("UPDATE credentials SET last_used_at = now() " ^
                               "WHERE id = $1::uuid")
                          in
                          let () =
                            log_auth store ~user_id:user.id ~kind:"api_key"
                              ~success:true ?source_ip ()
                          in
                          Some user
                      | None ->
                          let () =
                            log_auth store ~user_id:user.id ~kind:"api_key"
                              ~success:true ?source_ip ()
                          in
                          Some user)
                 | None ->
                     let () =
                       log_auth store ~kind:"api_key" ~success:false ?source_ip ()
                     in
                     None)
            | [] ->
                let () =
                  log_auth store ~kind:"api_key" ~success:false ?source_ip ()
                in
                None
    with exn -> Store_db.reraise ~op:"verify_token" exn)

(** Delegated tokens (auth.delegation-tokens, spec_delegation producer):
    kind='delegated_token' credentials. user_id is the OWNER the bearer
    acts for; public_data.actor names the carrying agent. Raw token
    returned once; only the sha256 hash stored. Data-only: the session
    built from a hit is delegated (no code persistence, no admin). *)
let mint_delegated_token (store : Store.t) ~(owner_user_id : string)
    ~(actor : string) ?(label : string option = None) ()
  : (string * string, string) result =
  (* returns (raw_token, credential_id) *)
  if String.trim actor = "" then
    (Error "mint-delegated-token: actor must be a non-empty username")
  else
    match pg_conn store with
    | None -> (Error "no database")
    | Some conn ->
        (try
      let raw =
            Base64.encode_string ~pad:false
              (Auth_password.random_bytes 32)
          in
          let th = token_hash raw in
          let public_fields =
            [ ("actor", `String (String.trim actor));
              ("token_last4", `String (last4_of_key raw)) ]
            @ (match label with
               | Some l when l <> "" -> [ ("label", `String l) ]
               | _ -> [])
          in
          let public_json = Yojson.Safe.to_string (`Assoc public_fields) in
          let secret_json =
            Yojson.Safe.to_string (`Assoc [ ("token_hash", `String th) ])
          in
          let sql =
            "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
            "VALUES ($1::uuid, 'delegated_token', $2::jsonb, $3::jsonb) " ^
            "RETURNING id::text"
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string owner_user_id;
                  Pgx.Value.of_string public_json;
                  Pgx.Value.of_string secret_json ]
              sql
          in
          let () =
            log_auth store ~user_id:owner_user_id
              ~kind:"mint_delegated" ~success:true ()
          in
          (match rows with
           | row :: _ ->
               (match Row.cell_string row 0 with
                | Some cid -> (Ok (raw, cid))
                | None -> (Error "mint-delegated-token: no credential id returned"))
           | [] -> (Error "mint-delegated-token: insert returned no row"))
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] mint_delegated_token: %s\n%!" msg;
          (Error msg))

(** Resolve a delegated Bearer token to the OWNER user + actor username.
    Same token_hash domain as api_key; on a hit, last_used_at is updated
    and auth_log kind='delegated'. Returns None on miss / revoked /
    malformed actor. *)
let verify_delegated_token (store : Store.t) ~(raw_token : string)
    ?source_ip () : (user * string) option =
  if raw_token = "" then None
  else
    match pg_conn store with
    | None -> None
    | Some conn ->
        (try
      let th = token_hash raw_token in
          let sql =
            "SELECT " ^
            "  u.id::text, u.username, u.is_admin::text, u.user_type, " ^
            "  u.parent_user_id::text, c.id::text, " ^
            "  c.public_data->>'actor' " ^
            "FROM credentials c JOIN users u ON u.id = c.user_id " ^
            "WHERE c.kind = 'delegated_token' " ^
            "  AND c.secret_data->>'token_hash' = $1 " ^
            "LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string th ] sql
          in
          match rows with
          | row :: _ ->
              (match user_of_row row, Row.cell_string row 6 with
               | Some owner, Some actor when String.trim actor <> "" ->
                   (* Provenance must be real, not aspirational: the actor
                      has to name an extant user (auth.delegation-tokens). *)
                   (match get_user_by_username store (String.trim actor) with
                    | None ->
                        let () =
                          log_auth store ~user_id:owner.id ~kind:"delegated"
                            ~success:false ?source_ip ()
                        in
                        None
                    | Some _ ->
                        (match Row.cell_string row 5 with
                         | Some cid ->
                             let _ =
                               Store_db.exec conn
                                 ~params:[ Pgx.Value.of_string cid ]
                                 ("UPDATE credentials SET last_used_at = now() " ^
                                  "WHERE id = $1::uuid")
                             in
                             let () =
                               log_auth store ~user_id:owner.id ~kind:"delegated"
                                 ~success:true ?source_ip ()
                             in
                             Some (owner, String.trim actor)
                         | None ->
                             let () =
                               log_auth store ~user_id:owner.id ~kind:"delegated"
                                 ~success:true ?source_ip ()
                             in
                             Some (owner, String.trim actor)))
               | _ ->
                   (* owner row unresolvable or actor provenance missing —
                      deny closed rather than mis-attribute a write *)
                   let () =
                     log_auth store ~kind:"delegated" ~success:false ?source_ip ()
                   in
                   None)
          | [] ->
              let () =
                log_auth store ~kind:"delegated" ~success:false ?source_ip ()
              in
              None
    with exn -> Store_db.reraise ~op:"verify_delegated_token" exn)

type delegated_token_meta = {
  did : string;
  dactor : string;
  dlabel : string option;
  dlast4 : string;
  dlast_used_at : string option;
}

let list_delegated_tokens (store : Store.t) ~(user_id : string)
  : delegated_token_meta list =
  match pg_conn store with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT id::text, public_data->>'actor', " ^
            "       public_data->>'label', public_data->>'token_last4', " ^
            "       last_used_at::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'delegated_token' " ^
            "ORDER BY created_at"
          in
          let rows = Store_db.exec conn ~params:[ Pgx.Value.of_string user_id ] sql in
          List.filter_map
            (fun row ->
               match Row.cell_string row 0, Row.cell_string row 1 with
               | Some cid, Some actor when cid <> "" ->
                   Some
                     { did = cid;
                       dactor = actor;
                       dlabel = Row.cell_string row 2;
                       dlast4 = (match Row.cell_string row 3 with Some s -> s | None -> "");
                       dlast_used_at = Row.cell_string row 4 }
               | _ -> None)
            rows
    with exn -> Store_db.reraise ~op:"list_delegated_tokens" exn)

(** Revoke (delete) a delegated credential row. Owner may revoke own
    rows; the is-admin check lives at the prim layer. Returns true when
    a row was deleted. *)
let revoke_delegated_token (store : Store.t) ~(user_id : string)
    ~(admin : bool) ~(credential_id : string) : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
      (try
      let sql =
            "DELETE FROM credentials " ^
            "WHERE id = $1::uuid AND kind = 'delegated_token' " ^
            "  AND (user_id = $2::uuid OR $3::bool) " ^
            "RETURNING id::text"
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string credential_id;
                  Pgx.Value.of_string user_id;
                  Pgx.Value.of_bool admin ]
              sql
          in
          let () =
            log_auth store ~user_id ~kind:"revoke_delegated"
              ~success:(rows <> []) ()
          in
          rows <> []
    with exn -> Store_db.reraise ~op:"revoke_delegated_token" exn)

type token_meta = {
  id : string;
  label : string option;
  token_last4 : string;
  created_at : string;
  last_used_at : string option;
}

let list_tokens (store : Store.t) ~(user_id : string) : token_meta list =
  match pg_conn store with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT id::text, public_data::text, created_at::text, " ^
            "       last_used_at::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'api_key' " ^
            "ORDER BY created_at"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          let metas =
            List.filter_map
              (fun row ->
                match Row.cell_string row 0, Row.cell_string row 1, Row.cell_string row 2 with
                | Some id, Some js, Some created ->
                    let label, token_last4 =
                      match Yojson.Safe.from_string js with
                      | `Assoc a ->
                          let opt_str k =
                            match List.assoc_opt k a with
                            | Some (`String s) when s <> "" -> Some s
                            | _ -> None
                          in
                          let tl4 =
                            match List.assoc_opt "token_last4" a with
                            | Some (`String s) -> s
                            | _ -> "????"
                          in
                          (opt_str "label", tl4)
                      | _ -> (None, "????")
                    in
                    Some { id; label; token_last4; created_at = created;
                           last_used_at = Row.cell_string row 3 }
                | _ -> None)
              rows
          in
          metas
    with exn -> Store_db.reraise ~op:"list_tokens" exn)

let revoke_token (store : Store.t) ~(credential_id : string)
  : (unit, string) result =
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let _ =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string credential_id ]
              "DELETE FROM credentials WHERE id = $1::uuid AND kind = 'api_key'"
          in
          (Ok ())
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] revoke_token: %s\n%!" msg;
          (Error msg))

(** rotate_token: revoke old + issue new. Returns the new raw token. *)
let rotate_token (store : Store.t) ~(credential_id : string)
    ~(user_id : string) ?label () : (string, string) result =
  let rev = revoke_token store ~credential_id in
  match rev with
  | Error e -> (Error e)
  | Ok () -> issue_token store ~user_id ~label ()

(** allow_agent_promote toggle (on the human's user row). *)
let get_allow_agent_promote (store : Store.t) ~(user_id : string)
  : bool =
  match pg_conn store with
  | None -> true
  | Some conn ->
      (try
      let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ]
              "SELECT allow_agent_promote::text FROM users WHERE id = $1::uuid"
          in
          match rows with
          | row :: _ -> (Row.cell_bool row 0)
          | [] -> true
    with exn -> Store_db.reraise ~op:"get_allow_agent_promote" exn)

(** Set a user's type + parent (auth.user-management admin surface;
    the missing type flip the agent-native chapter recorded as pending).
    user_type "agent" requires parent_user_id = Some <human id> (the
    owning human — an agent can never parent another agent, matching
    create_agent); "human" clears the parent. Unknown user -> Error.
    Does not touch is_admin or credentials. *)
let set_user_type (store : Store.t) ~(user_id : string)
    ~(user_type : string) ~(parent_user_id : string option)
  : (unit, string) result =
  if user_type <> "human" && user_type <> "agent" then
    (Error "user_type must be 'human' or 'agent'")
  else
  match get_user_by_id store user_id with
  | None -> (Error "no such user")
  | Some _u ->
  let parent =
    match (user_type, parent_user_id) with
    | "agent", Some pid ->
        (match get_user_by_id store pid with
         | None -> (Error "parent user not found")
         | Some p when p.user_type <> "human" ->
             (Error "agent cannot be a parent of another agent")
         | Some _ -> Ok (Some pid))
    | "agent", None -> (Error "agent requires parent_user_id")
    | "human", _ -> Ok None
    | _ -> (Error "user_type must be 'human' or 'agent'")
  in
  match parent with
  | Error e -> (Error e)
  | Ok parent_id ->
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let _ =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string user_type;
                  (match parent_id with
                   | Some pid -> Pgx.Value.of_string pid
                   | None -> Pgx.Value.null);
                  Pgx.Value.of_string user_id ]
              "UPDATE users SET user_type = $1, parent_user_id = $2::uuid \
               WHERE id = $3::uuid"
          in
          (Ok ())
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] set_user_type: %s\n%!" msg;
          (Error msg))

let set_allow_agent_promote (store : Store.t) ~(user_id : string)
    ~(value : bool) : (unit, string) result =
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let _ =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_bool value;
                  Pgx.Value.of_string user_id ]
              "UPDATE users SET allow_agent_promote = $1 WHERE id = $2::uuid"
          in
          (Ok ())
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] set_allow_agent_promote: %s\n%!" msg;
          (Error msg))

(** Per-user LLM provider key (auth.user-llm-keys). Not api_key (auth INTO PP). *)
type llm_key_meta = {
  last4 : string;
  label : string option;
  base : string option;
  model : string option;
}

let set_llm_key (store : Store.t) ~(user_id : string) ~(api_key : string)
    ?(base : string option = None) ?(model : string option = None)
    ?(label : string option = None) () : (llm_key_meta, string) result =
  if String.trim api_key = "" then
    (Error "empty api_key")
  else
    match pg_conn store with
    | None -> (Error "no database")
    | Some conn ->
        (try
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
              (* i-1788670058: seal under the envelope when a master key
                 is configured; degraded mode keeps plaintext but LOUD
                 (a silently "sealed" secret would be worse than an
                 honest plaintext row nobody knows about). *)
              match Envelope.encrypt ~user_id api_key with
              | Ok blob -> blob
              | Error e ->
                  Printf.eprintf
                    "[auth_store] set_llm_key: STORING PLAINTEXT (%s); set PP_LLM_ENVELOPE_KEY\n%!"
                    e;
                  Yojson.Safe.to_string
                    (`Assoc [ ("api_key", `String api_key) ])
            in
            let _ =
              Store_db.exec conn
                ~params:[ Pgx.Value.of_string user_id ]
                "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'llm_key'"
            in
            let sql =
              "INSERT INTO credentials (user_id, kind, public_data, secret_data) " ^
              "VALUES ($1::uuid, 'llm_key', $2::jsonb, $3::jsonb)"
            in
            let _ =
              Store_db.exec conn
                ~params:
                  [ Pgx.Value.of_string user_id;
                    Pgx.Value.of_string public_json;
                    Pgx.Value.of_string secret_json ]
                sql
            in
            (Ok
                 { last4;
                   label;
                   base;
                   model })
    with exn ->
      let msg = Printexc.to_string exn in
            Printf.eprintf "[auth_store] set_llm_key: %s\n%!" msg;
            (Error msg))

let clear_llm_key (store : Store.t) ~(user_id : string)
  : (unit, string) result =
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      let _ =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ]
              "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'llm_key'"
          in
          (Ok ())
      with exn ->
        let msg = Printexc.to_string exn in
            Printf.eprintf "[auth_store] clear_llm_key: %s\n%!" msg;
            (Error msg))


(* ---- llm_key credential parsing (one home; was duplicated between
   get_llm_key_meta_direct and get_llm_key_secret_direct, ~79 lines) ---- *)

(* Parse a public_data JSON blob into the display meta. [default_last4] fills
   in when the blob is absent, unparseable, not an object, or lacks last4.
   Returns None only for absent/unparseable/non-object blobs — callers decide
   whether that means None or a fully-default record. *)
let llm_key_meta_of_public ?(default_last4 = "????") (js : string option)
  : llm_key_meta option =
  match js with
  | None -> None
  | Some js ->
      (try
         match Yojson.Safe.from_string js with
         | `Assoc a ->
             let last4 =
               match List.assoc_opt "last4" a with
               | Some (`String s) -> s
               | _ -> default_last4
             in
             let opt_str k =
               match List.assoc_opt k a with
               | Some (`String s) when s <> "" -> Some s
               | _ -> None
             in
             Some
               { last4;
                 label = opt_str "label";
                 base = opt_str "base";
                 model = opt_str "model" }
         | _ -> None
       with _ -> None)

(* Extract the api_key from a secret_data JSON blob. Envelope blobs
   (i-1788670058) are opened bound to [user_id]; legacy plaintext rows
   still read. A wrong-identity or tampered envelope blob is None and
   LOUD - a silent None would look like "no key configured". *)
let llm_key_of_secret ~(user_id : string) (js : string) : string option =
  if Envelope.is_envelope_blob js then
    (match Envelope.decrypt ~user_id js with
     | Some k when k <> "" -> Some k
     | _ ->
         Printf.eprintf
           "[auth_store] llm_key_of_secret: envelope open FAILED for %s (wrong identity, tampered blob, or missing/mismatched PP_LLM_ENVELOPE_KEY)\n%!"
           user_id;
         None)
  else
    try
      match Yojson.Safe.from_string js with
      | `Assoc a ->
          (match List.assoc_opt "api_key" a with
           | Some (`String k) when k <> "" -> Some k
           | _ -> None)
      | _ -> None
    with _ -> None

let default_llm_key_meta (key : string) : llm_key_meta =
  { last4 = last4_of_key key; label = None; base = None; model = None }

(** i-1788670058 boot migration: re-seal legacy plaintext llm_key rows
    under the envelope. Idempotent: envelope blobs lack the legacy
    'api_key' member and are skipped by both the SELECT guard and the
    UPDATE guard. No master key configured -> no-op (rows stay
    plaintext; boot prints the loud warning instead). Returns the
    number of rows re-sealed. *)
let migrate_llm_keys (store : Store.t) : int =
  if not (Envelope.master_key_present ()) then 0
  else
    match pg_conn store with
    | None -> 0
    | Some conn ->
        (try
           let sel =
             "SELECT user_id::text, secret_data::text FROM credentials "
             ^ "WHERE kind = 'llm_key' AND secret_data ? 'api_key'"
           in
           let rows = Store_db.exec conn ~params:[] sel in
           let n = ref 0 in
           List.iter
             (fun row ->
                match (Row.cell_string row 0, Row.cell_string row 1) with
                | Some uid, Some secret_js ->
                    (match llm_key_of_secret ~user_id:uid secret_js with
                     | Some key ->
                         (match Envelope.encrypt ~user_id:uid key with
                          | Ok blob ->
                              let _ =
                                Store_db.exec conn
                                  ~params:
                                    [ Pgx.Value.of_string uid;
                                      Pgx.Value.of_string blob ]
                                  ("UPDATE credentials SET secret_data = $2::jsonb "
                                   ^ "WHERE user_id = $1::uuid AND kind = 'llm_key' "
                                   ^ "AND secret_data ? 'api_key'")
                              in
                              incr n
                          | Error e ->
                              Printf.eprintf
                                "[auth_store] migrate_llm_keys: seal failed for %s: %s\n%!"
                                uid e)
                     | None -> ())
                | _ -> ())
             rows;
           if !n > 0 then
             Printf.eprintf "[auth_store] llm-key envelope migration: re-sealed %d rows\n%!" !n;
           !n
         with exn ->
           Printf.eprintf "[auth_store] migrate_llm_keys: %s\n%!"
             (Printexc.to_string exn);
           0)
let get_llm_key_meta_direct (store : Store.t) ~(user_id : string)
  : llm_key_meta option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT public_data::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'llm_key' " ^
            "ORDER BY created_at DESC LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          match rows with
          | row :: _ -> llm_key_meta_of_public (Row.cell_string row 0)
          | [] -> None
    with exn -> Store_db.reraise ~op:"get_llm_key_meta_direct" exn)

(** get_llm_key_meta with agent fallback to parent's key. *)
let get_llm_key_meta (store : Store.t) ~(user_id : string)
  : llm_key_meta option =
  let direct = get_llm_key_meta_direct store ~user_id in
  match direct with
  | Some _ -> direct
  | None ->
      let user_opt = get_user_by_id store user_id in
      (match user_opt with
       | Some u when u.user_type = "agent" ->
           (match u.parent_user_id with
            | Some pid -> get_llm_key_meta_direct store ~user_id:pid
            | None -> None)
       | _ -> None)

let get_llm_key_secret_direct (store : Store.t) ~(user_id : string)
  : (string * llm_key_meta) option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT secret_data::text, public_data::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'llm_key' " ^
            "ORDER BY created_at DESC LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          match rows with
          | row :: _ ->
              (match Row.cell_string row 0 with
               | Some secret_js ->
                     (match llm_key_of_secret ~user_id secret_js with
                    | None -> None
                    | Some key ->
                        let meta =
                          match
                            llm_key_meta_of_public
                              ~default_last4:(last4_of_key key)
                              (Row.cell_string row 1)
                          with
                          | Some m -> m
                          | None -> default_llm_key_meta key
                        in
                        Some (key, meta))
               | None -> None)
          | [] -> None
    with exn -> Store_db.reraise ~op:"get_llm_key_secret_direct" exn)

(** get_llm_key_secret with agent fallback: if the user has no own llm_key
    and is an agent (user_type='agent', parent_user_id set), look up the
    parent human's llm_key. *)
let get_llm_key_secret (store : Store.t) ~(user_id : string)
  : (string * llm_key_meta) option =
  let direct = get_llm_key_secret_direct store ~user_id in
  match direct with
  | Some _ -> direct
  | None ->
      (* Fall back to parent only if this user is an agent. *)
      let user_opt = get_user_by_id store user_id in
      (match user_opt with
       | Some u when u.user_type = "agent" ->
           (match u.parent_user_id with
            | Some pid -> get_llm_key_secret_direct store ~user_id:pid
            | None -> None)
       | _ -> None)

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
    ?(label : string option = None) () : (ssh_key_meta, string) result =
  match parse_openssh_pubkey openssh with
  | Error e -> (Error e)
  | Ok openssh ->
      match pg_conn store with
      | None -> (Error "no database")
      | Some conn ->
          (try
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
              let rows =
                Store_db.exec conn
                  ~params:
                    [ Pgx.Value.of_string user_id;
                      Pgx.Value.of_string public_json ]
                  sql
              in
              match rows with
              | row :: _ ->
                  (match Row.cell_string row 0 with
                   | Some id ->
                       (Ok
                            { id;
                              fingerprint = fp;
                              label;
                              openssh })
                   | None -> (Error "add_ssh_key: no id"))
              | [] -> (Error "add_ssh_key: no row")
    with exn ->
      let msg = Printexc.to_string exn in
              Printf.eprintf "[auth_store] add_ssh_key: %s\n%!" msg;
              (Error msg))

let list_ssh_keys (store : Store.t) ~(user_id : string)
  : ssh_key_meta list =
  match pg_conn store with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT id::text, public_data::text FROM credentials " ^
            "WHERE user_id = $1::uuid AND kind = 'ssh_key' " ^
            "ORDER BY created_at"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string user_id ] sql
          in
          let metas =
            List.filter_map
              (fun row ->
                match Row.cell_string row 0, Row.cell_string row 1 with
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
          metas
    with exn -> Store_db.reraise ~op:"list_ssh_keys" exn)

let revoke_ssh_key (store : Store.t) ~(user_id : string)
    ~(id_or_fingerprint : string) : (unit, string) result =
  match pg_conn store with
  | None -> (Error "no database")
  | Some conn ->
      (try
      (* Delete by credential id OR fingerprint match in public_data *)
          let sql =
            "DELETE FROM credentials WHERE user_id = $1::uuid AND kind = 'ssh_key' " ^
            "AND (id::text = $2 OR public_data->>'fingerprint' = $2 " ^
            "     OR public_data->>'openssh' = $2)"
          in
          let _ =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string user_id;
                  Pgx.Value.of_string (String.trim id_or_fingerprint) ]
              sql
          in
          (Ok ())
    with exn ->
      let msg = Printexc.to_string exn in
          Printf.eprintf "[auth_store] revoke_ssh_key: %s\n%!" msg;
          (Error msg))

let verify_ssh_key (store : Store.t) ~(openssh_or_blob : string)
    ?source_ip () : user option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let presented = String.trim openssh_or_blob in
          let fp = ssh_fingerprint presented in
          let sql =
            "SELECT u.id::text, u.username, u.is_admin::text, u.user_type, " ^
            "       u.parent_user_id::text, c.id::text " ^
            "FROM credentials c JOIN users u ON u.id = c.user_id " ^
            "WHERE c.kind = 'ssh_key' " ^
            "AND (c.public_data->>'openssh' = $1 " ^
            "     OR c.public_data->>'fingerprint' = $2) " ^
            "LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string presented;
                  Pgx.Value.of_string fp ]
              sql
          in
          match rows with
          | row :: _ ->
              (match user_of_row row with
               | Some user ->
                   let () =
                     log_auth store ~user_id:user.id ~kind:"ssh_key" ~success:true
                       ?source_ip ()
                   in
                   (match Row.cell_string row 5 with
                    | Some cid ->
                        let _ =
                          Store_db.exec conn
                            ~params:[ Pgx.Value.of_string cid ]
                            "UPDATE credentials SET last_used_at = now() \
                             WHERE id = $1::uuid"
                        in
                        Some user
                    | None -> Some user)
               | None ->
                   let () =
                     log_auth store ~kind:"ssh_key" ~success:false ?source_ip ()
                   in
                   None)
          | [] ->
              let () =
                log_auth store ~kind:"ssh_key" ~success:false ?source_ip ()
              in
              None
    with exn -> Store_db.reraise ~op:"verify_ssh_key" exn)

let list_users (store : Store.t) : user list =
  match pg_conn store with
  | None -> []
  | Some conn ->
      (try
      let rows =
            Store_db.exec conn
              ("SELECT " ^ user_cols ^ " FROM users ORDER BY username")
          in
          let users = List.filter_map user_of_row rows in
          users
    with exn -> Store_db.reraise ~op:"list_users" exn)

(** First boot: if users table empty, create admin from env.
    PP_BOOTSTRAP_USER (default admin), PP_BOOTSTRAP_PASSWORD (default admin).
    If admin already exists but has no password credential, set one from env
    (dev recovery for pre-auth DBs that only had ssh_key). *)
let bootstrap_admin_if_empty (store : Store.t) : user option =
  let username = env_or "PP_BOOTSTRAP_USER" "admin" in
  let password = env_or "PP_BOOTSTRAP_PASSWORD" "admin" in
  (* i-1787971357: never boot a default admin/admin without explicit
     opt-in. When the password is the default AND PP_ALLOW_INSECURE_BOOTSTRAP
     is unset, mint a random password (printed once to stderr). Lazy so no
     password is generated when the admin already has one / isn't created. *)
  let password =
    lazy
      (if password = "admin"
          && Sys.getenv_opt "PP_ALLOW_INSECURE_BOOTSTRAP" = None
       then
         let pw = Base64.encode_string (Mirage_crypto_rng.generate 18) in
         Printf.eprintf
           "[auth] generated admin password: %s — set PP_BOOTSTRAP_PASSWORD to control; PP_ALLOW_INSECURE_BOOTSTRAP=1 keeps admin/admin\n%!"
           pw;
         pw
       else password)
  in
  let n = count_users store in
  if n > 0 then
    let admin = get_user_by_username store username in
    match admin with
    | None -> None
    | Some user ->
        let has_pw = password_hash_for_user store ~username in
        (match has_pw with
         | Some _ -> None
         | None ->
             let pw = set_password store ~user_id:user.id ~password:(Lazy.force password) in
             (match pw with
              | Error e ->
                  Printf.eprintf
                    "[auth_store] bootstrap password for existing %s: %s\n%!"
                    username e;
                  None
              | Ok () ->
                  Printf.eprintf
                    "[auth_store] set password for existing admin %s\n%!"
                    username;
                  Some user))
  else
    let created = create_user store ~username ~is_admin:true in
    match created with
    | Error e ->
        Printf.eprintf "[auth_store] bootstrap failed: %s\n%!" e;
        None
    | Ok user ->
        let pw = set_password store ~user_id:user.id ~password:(Lazy.force password) in
        (match pw with
         | Error e ->
             Printf.eprintf "[auth_store] bootstrap password: %s\n%!" e;
             None
         | Ok () ->
             Printf.eprintf
               "[auth_store] bootstrapped admin user %s (set PP_BOOTSTRAP_PASSWORD)\n%!"
               username;
             let () =
               log_auth store ~user_id:user.id ~kind:"bootstrap"
                 ~success:true ()
             in
             Some user)
