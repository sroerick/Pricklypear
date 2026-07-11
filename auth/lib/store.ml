(** Pricklypear Auth — Storage layer.

    Connects with the dedicated [pricklypear_auth] pg role (NOT
    postgres/supervisor). Per spec [auth.credentials-store]:
    system_keys is role-restricted so ONLY auth sees the signing key.

    Four functions of the credential store (spec [auth.verification]):
      verify(cred) -> May<user_id>
      lookup(user_id) -> May<User>
      mint(user_id) -> token
      verify_token(token) -> May<user_id>

    These are implemented in verify.ml (lookup/verify_ssh_key/
    verify_token) and tokens.ml (mint), but all DB calls go through
    this store's [Pg] connection. *)
open Lwt.Syntax

module Io_unix = struct
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  type sockaddr =
    | Unix of string
    | Inet of string * int

  let output_char = Lwt_io.write_char
  let output_string oc s = Lwt_io.write oc s
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let really_input ic buf pos len = Lwt_io.read_into_exactly ic buf pos len
  let close_in = Lwt_io.close
  let getlogin () = Lwt.return "pricklypear-auth"
  let open_connection = function
    | Unix path ->
        let sock = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
        let* () = Lwt_unix.connect sock (Lwt_unix.ADDR_UNIX path) in
        Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Input sock,
                    Lwt_io.of_fd ~mode:Lwt_io.Output sock)
    | Inet (host, port) ->
        let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
        let* () = Lwt_unix.connect sock addr in
        Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Input sock,
                    Lwt_io.of_fd ~mode:Lwt_io.Output sock)
end

module Pg = Pgx_lwt.Make (Io_unix)

type config = {
  database : string;
  host : string;
  port : int;
  user : string;
  password : string;
}

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft

(* Auth connects as the pricklypear_auth role. Per spec: this role is
   granted SELECT/INSERT/UPDATE on auth tables ONLY. It does NOT own
   the schema (the supervisor does — it runs migrations as 'postgres').
   The role password is read from env (PP_AUTH_DB_PASS), defaulting to
   the dev placeholder set in migrations/002_auth.sql. *)
let config_from_env () : config =
  {
    database = env_or "PP_DB_NAME" (env_or "PGDATABASE" "pricklypear");
    host = env_or "PP_DB_HOST" (env_or "PGHOST" "127.0.0.1");
    port = int_of_string (env_or "PP_DB_PORT" (env_or "PGPORT" "5433"));
    user = env_or "PP_DB_USER" "pricklypear_auth";
    password = env_or "PP_DB_PASS" "change-me-on-prod";
  }

type t = {
  cfg : config;
  mutable conn : Pg.t option;
}

let make ~cfg () : t = { cfg; conn = None }

let connect (t : t) : bool Lwt.t =
  match t.conn with
  | Some _ -> Lwt.return_true
  | None ->
      Lwt.catch
        (fun () ->
          let* conn =
            Pg.connect
              ~host:t.cfg.host ~port:t.cfg.port
              ~user:t.cfg.user ~password:t.cfg.password
              ~database:t.cfg.database ()
          in
          t.conn <- Some conn;
          Printf.eprintf "[auth.store] connected to PG %s:%d/%s as %s\n"
            t.cfg.host t.cfg.port t.cfg.database t.cfg.user;
          Lwt.return_true)
        (fun exn ->
          Printf.eprintf "[auth.store] PG connect failed: %s\n"
            (Printexc.to_string exn);
          Lwt.return_false)

let with_conn (t : t) (f : Pg.t -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> Lwt.fail (Failure "auth.store: PG not connected")
  | Some conn -> f conn

(* --- User lookups --- *)

(* Find a user by username; returns the user_id (uuid string) on success. *)
let lookup_user_by_username (t : t) (username : string)
    : string option Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT id::text FROM users WHERE username = $1"
        ~params:[ Pgx.Value.of_string username ]
    in
    match rows with
    | [ [ cell ] ] -> Pgx.Value.to_string cell
    | _ -> None)

(* Find a user by id; returns (id, username, is_admin) tuple. *)
let lookup_user_by_id (t : t) (user_id : string)
    : (string * string * bool) option Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT id::text, username, is_admin FROM users WHERE id::text = $1"
        ~params:[ Pgx.Value.of_string user_id ]
    in
    match rows with
    | [ [ id_cell; uname_cell; admin_cell ] ] ->
        (match Pgx.Value.to_string id_cell,
                Pgx.Value.to_string uname_cell,
                Pgx.Value.to_bool admin_cell with
         | Some id, Some u, Some a -> Some (id, u, a)
         | _ -> None)
    | _ -> None)

(* --- SSH-key credential lookup --- *)

(* Find a user_id by exact SSH key string in credentials.public_data.
   public_data is jsonb: {"key": "..."}; matches where public_data->>'key'
   equals the presented key. *)
let lookup_credential_ssh_key (t : t) (ssh_key : string)
    : string option Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT user_id::text FROM credentials \
         WHERE kind = 'ssh_key' AND public_data->>'key' = $1"
        ~params:[ Pgx.Value.of_string ssh_key ]
    in
    match rows with
    | [ [ cell ] ] -> Pgx.Value.to_string cell
    | _ -> None)

(* List all (username, ssh_key) pairs for users that have an ssh_key
   credential. Used by the supervisor at boot to build the Awa.Auth.db
   for awa-ssh 0.3.1's static-db auth API (see BORGE-1783439434).
   This is a snapshot — runtime key rotation requires supervisor
   restart until awa-ssh 0.5.x is installed. *)
let list_ssh_credentials (t : t) : (string * string) list Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT u.username, c.public_data->>'key' \
         FROM credentials c \
         JOIN users u ON c.user_id = u.id \
         WHERE c.kind = 'ssh_key' AND c.public_data->>'key' IS NOT NULL"
    in
    List.filter_map
      (fun row ->
        match row with
        | [ uname_cell; key_cell ] ->
            (match Pgx.Value.to_string uname_cell,
                    Pgx.Value.to_string key_cell with
             | Some u, Some k when k <> "" -> Some (u, k)
             | _ -> None)
        | _ -> None)
      rows)

(* Generate a new session row PG-side. The opaque_id IS the row's
   uuid (auth_sessions.id) cast to text. We INSERT with gen_random_uuid
   and RETURN the generated id as text so the token contains it.
   token_hash is set later via update_session_token_hash once we've
   signed the opaque_id. *)
let insert_session_returning_id
    ~(store : t) ~(user_id : string) ~(key : Cstruct.t)
    : string option Lwt.t =
  let _ = key in  (* signing key is used by caller; we just need the row id *)
  Lwt.catch
    (fun () ->
      with_conn store (fun conn ->
        let+ rows =
          Pg.execute conn
            "INSERT INTO auth_sessions (id, user_id, token_hash, expires_at) \
             VALUES (gen_random_uuid(), $1, '', now() + interval '7 days') \
             RETURNING id::text"
            ~params:[ Pgx.Value.of_string user_id ]
        in
        match rows with
        | [ [ cell ] ] -> Pgx.Value.to_string cell
        | _ -> None))
    (fun exn ->
      Printf.eprintf "[auth.store] insert_session_returning_id failed: %s\n"
        (Printexc.to_string exn);
      Lwt.return_none)

(* Update the token_hash on an existing session row. *)
let update_session_token_hash
    ~(store : t) ~(opaque_id : string) ~(token_hash : string)
    : bool Lwt.t =
  Lwt.catch
    (fun () ->
      with_conn store (fun conn ->
        let+ _ =
          Pg.execute_unit conn
          "UPDATE auth_sessions SET token_hash = $1 WHERE id::text = $2"
          ~params:[ Pgx.Value.of_string token_hash;
                    Pgx.Value.of_string opaque_id ]
        in
        true))
    (fun exn ->
      Printf.eprintf "[auth.store] update_session_token_hash failed: %s\n"
        (Printexc.to_string exn);
      Lwt.return_false)

(* Find a session by opaque_id; check token_hash matches and
   not revoked and not expired. *)
let lookup_session
    (t : t) ~(opaque_id : string) ~(token_hash : string)
    : string option Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT user_id::text FROM auth_sessions \
         WHERE id::text = $1 AND token_hash = $2 \
           AND revoked_at IS NULL AND expires_at > now()"
        ~params:[ Pgx.Value.of_string opaque_id;
                  Pgx.Value.of_string token_hash ]
    in
    match rows with
    | [ [ cell ] ] -> Pgx.Value.to_string cell
    | _ -> None)

(* --- system_keys (the signing key) --- *)

(* Fetch the session_signing key from system_keys. Returns the raw bytes
   (Cstruct.t) on success, or None if the key isn't seeded yet. *)
let get_signing_key (t : t) : Cstruct.t option Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT key_data FROM system_keys WHERE id = 'session_signing'"
    in
    match rows with
    | [ [ cell ] ] ->
        (match Pgx.Value.to_binary cell with
         | Some s -> Some (Cstruct.of_string s)
         | None -> None)
    | _ -> None)

(* Seed the signing key if not present. Used by boot.ml when no key
   exists yet (first boot generates a fresh random 32-byte key). *)
let seed_signing_key_if_missing (t : t) (key : Cstruct.t) : bool Lwt.t =
  Lwt.catch
    (fun () ->
      let+ _ =
        with_conn t (fun conn ->
          Pg.execute_unit conn
            "INSERT INTO system_keys (id, key_data) \
             VALUES ('session_signing', $1) \
             ON CONFLICT (id) DO NOTHING"
            ~params:[ Pgx.Value.of_binary (Cstruct.to_string key) ])
      in
      true)
    (fun exn ->
      Printf.eprintf "[auth.store] seed_signing_key failed: %s\n"
        (Printexc.to_string exn);
      Lwt.return_false)

(* --- Audit log --- *)

(* Append a row to auth_log. Cheap, append-only, no UPDATE/DELETE. *)
let log
    (t : t) ?(user_id : string option = None) ~(kind : string)
    ~(success : bool) ?(source_ip : string option = None) () : unit Lwt.t =
  Lwt.catch
    (fun () ->
      with_conn t (fun conn ->
        let+ _ =
          Pg.execute_unit conn
            "INSERT INTO auth_log (id, user_id, kind, success, source_ip) \
             VALUES (gen_random_uuid(), $1, $2, $3, $4)"
            ~params:[
              (match user_id with Some s -> Pgx.Value.of_string s | None -> Pgx.Value.null);
              Pgx.Value.of_string kind;
              Pgx.Value.of_bool success;
              (match source_ip with Some s -> Pgx.Value.of_string s | None -> Pgx.Value.null);
            ]
        in
        ()))
    (fun exn ->
      Printf.eprintf "[auth.store] audit log write failed: %s\n"
        (Printexc.to_string exn);
      Lwt.return_unit)

(* --- Bootstrap admin --- *)

(* Used by boot.ml: insert the first admin user + their ssh_key credential
   IF the env arg is set AND users table is empty. Per spec [auth.bootstrap-admin]. *)
let bootstrap_admin
    (t : t) ~(username : string) ~(ssh_key : string) : bool Lwt.t =
  Lwt.catch
    (fun () ->
      let* () =
        with_conn t (fun conn ->
          let+ _ =
            Pg.execute_unit conn
              "INSERT INTO users (id, username, is_admin) \
               VALUES (gen_random_uuid(), $1::text, true)"
              ~params:[ Pgx.Value.of_string username ]
          in
          ())
      in
      let+ _ =
        with_conn t (fun conn ->
          let+ _ =
            Pg.execute_unit conn
              "INSERT INTO credentials (id, user_id, kind, public_data) \
               VALUES (gen_random_uuid(), \
                       (SELECT id::uuid FROM users WHERE username = $1::text), \
                       'ssh_key', \
                       jsonb_build_object('key', $2::text))"
              ~params:[ Pgx.Value.of_string username;
                        Pgx.Value.of_string ssh_key ]
          in
          ())
      in
      true)
    (fun exn ->
      Printf.eprintf "[auth.store] bootstrap_admin failed: %s\n"
        (Printexc.to_string exn);
      Lwt.return_false)

(* Is the users table empty? (Used by boot to decide whether bootstrap
   should run.) *)
let users_empty (t : t) : bool Lwt.t =
  with_conn t (fun conn ->
    let+ rows = Pg.execute conn "SELECT count(*)::int FROM users" in
    match rows with
    | [ [ cell ] ] ->
        (match Pgx.Value.to_int cell with
         | Some 0 -> true
         | _ -> false)
    | _ -> true)

(* --- User management (auth.user-management) ---

   create_user / add_credential_ssh / add_credential_password.
   Password scheme is INTERIM: sha256$<salt_hex>$<hash_hex> using
   Mirage_crypto Hash.SHA256. Not argon2id — document for v1.5 swap. *)

let create_user (t : t) ~(username : string) ?(is_admin = false) ()
    : (string, string) result Lwt.t =
  Lwt.catch
    (fun () ->
      with_conn t (fun conn ->
        let+ rows =
          Pg.execute conn
            "INSERT INTO users (id, username, is_admin) \
             VALUES (gen_random_uuid(), $1::text, $2) \
             RETURNING id::text"
            ~params:[ Pgx.Value.of_string username;
                      Pgx.Value.of_bool is_admin ]
        in
        match rows with
        | [ [ cell ] ] ->
            (match Pgx.Value.to_string cell with
             | Some id -> Ok id
             | None -> Error "create_user: no id returned")
        | _ -> Error "create_user: insert failed"))
    (fun exn ->
      let msg = Printexc.to_string exn in
      Printf.eprintf "[auth.store] create_user failed: %s\n" msg;
      Lwt.return (Error msg))

let add_credential_ssh (t : t) ~(user_id : string) ~(ssh_key : string)
    : (unit, string) result Lwt.t =
  Lwt.catch
    (fun () ->
      with_conn t (fun conn ->
        let+ _ =
          Pg.execute_unit conn
            "INSERT INTO credentials (id, user_id, kind, public_data) \
             VALUES (gen_random_uuid(), $1::uuid, 'ssh_key', \
                     jsonb_build_object('key', $2::text))"
            ~params:[ Pgx.Value.of_string user_id;
                      Pgx.Value.of_string ssh_key ]
        in
        Ok ()))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

(* Hex DIGITS (not binary). Hex.to_string decodes; we need of_cstruct's payload. *)
let hex_encode (cs : Cstruct.t) : string =
  match Hex.of_cstruct cs with `Hex s -> s

let hex_decode (s : string) : Cstruct.t =
  Hex.to_cstruct (`Hex s)

let sha256_raw (s : string) : string =
  Digestif.SHA256.(to_raw_string (digest_string s))

(* Interim password hash: sha256$salt_hex$digest_hex
   mirage-crypto 1.x: RNG returns string; hashes via Digestif. *)
let hash_password (password : string) : string =
  let salt = Cstruct.of_string (Mirage_crypto_rng.generate 16) in
  let salt_hex = hex_encode salt in
  let material = Cstruct.to_string salt ^ password in
  let digest = Cstruct.of_string (sha256_raw material) in
  let digest_hex = hex_encode digest in
  Printf.sprintf "sha256$%s$%s" salt_hex digest_hex

let verify_password_hash ~(stored : string) ~(password : string) : bool =
  match String.split_on_char '$' stored with
  | [ "sha256"; salt_hex; digest_hex ] ->
      (try
         let salt = hex_decode salt_hex in
         let material = Cstruct.to_string salt ^ password in
         let digest = Cstruct.of_string (sha256_raw material) in
         let got = hex_encode digest in
         String.equal got digest_hex
       with _ -> false)
  | _ -> false

let add_credential_password (t : t) ~(user_id : string) ~(password : string)
    : (unit, string) result Lwt.t =
  let hash = hash_password password in
  Lwt.catch
    (fun () ->
      with_conn t (fun conn ->
        let+ _ =
          Pg.execute_unit conn
            "INSERT INTO credentials (id, user_id, kind, secret_data) \
             VALUES (gen_random_uuid(), $1::uuid, 'password', \
                     jsonb_build_object('hash', $2::text))"
            ~params:[ Pgx.Value.of_string user_id;
                      Pgx.Value.of_string hash ]
        in
        Ok ()))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

(* Lookup password hash for username; None if no password credential. *)
let lookup_password_hash (t : t) ~(username : string) : (string * string) option Lwt.t =
  (* returns (user_id, hash) *)
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT u.id::text, c.secret_data->>'hash' \
         FROM users u \
         JOIN credentials c ON c.user_id = u.id \
         WHERE u.username = $1 AND c.kind = 'password' \
         ORDER BY c.created_at DESC LIMIT 1"
        ~params:[ Pgx.Value.of_string username ]
    in
    match rows with
    | [ [ id_cell; hash_cell ] ] ->
        (match Pgx.Value.to_string id_cell, Pgx.Value.to_string hash_cell with
         | Some id, Some h when h <> "" -> Some (id, h)
         | _ -> None)
    | _ -> None)

let list_users (t : t) : (string * string * bool) list Lwt.t =
  with_conn t (fun conn ->
    let+ rows =
      Pg.execute conn
        "SELECT id::text, username, is_admin FROM users ORDER BY username"
    in
    List.filter_map
      (fun row ->
        match row with
        | [ id_c; u_c; a_c ] ->
            (match Pgx.Value.to_string id_c,
                    Pgx.Value.to_string u_c,
                    Pgx.Value.to_bool a_c with
             | Some id, Some u, Some a -> Some (id, u, a)
             | _ -> None)
        | _ -> None)
      rows)
