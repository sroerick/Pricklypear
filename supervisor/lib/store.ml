(** Pricklypear Supervisor — Storage + migration runner.

    Per spec [migrations.applied-by-supervisor]: the supervisor owns
    schema state because it owns image lifecycle. On supervisor boot:

      1. Read applied versions from schema_migrations.
      2. Apply any pending migrations in order.
      3. Only then: start serving image-boot requests.

    A migration in flight blocks new image boots until it completes (or
    fails). Migration failure: supervisor refuses to boot images, alerts
    admin, requires manual intervention. No half-applied state.

    The supervisor connects with its OWN pg role (NOT pricklypear_auth;
    that one's reserved for the auth image). In dev the role defaults
    to "postgres" because that's what the throwaway dev cluster uses. *)
open Lwt.Syntax
open Lwt.Infix

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
  let getlogin () = Lwt.return "pricklypear-supervisor"
  let open_connection = function
    | Unix path ->
        let sock =
          Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
        let* () = Lwt_unix.connect sock (Lwt_unix.ADDR_UNIX path) in
        Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Input sock,
                    Lwt_io.of_fd ~mode:Lwt_io.Output sock)
    | Inet (host, port) ->
        let sock =
          Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
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

let config_from_env () : config =
  {
    database = env_or "PP_DB_NAME" (env_or "PGDATABASE" "pricklypear");
    host = env_or "PP_DB_HOST" (env_or "PGHOST" "127.0.0.1");
    port = int_of_string (env_or "PP_DB_PORT" (env_or "PGPORT" "5433"));
    user = env_or "PP_DB_USER" (env_or "PGUSER" "postgres");
    password = env_or "PP_DB_PASS" "";
  }

type t = {
  cfg : config;
  mutable conn : Pg.t option;
}

let make ~cfg () : t = { cfg; conn = None }

(* Connect synchronously. Returns true on success. Supervisor boot
   blocks on this — if PG is down, the supervisor logs an error and
   runs migrations on next reconnect (TODO: retry loop). *)
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
          Printf.eprintf "[supervisor.store] connected to PG %s:%d/%s as %s\n"
            t.cfg.host t.cfg.port t.cfg.database t.cfg.user;
          Lwt.return_true)
        (fun exn ->
          Printf.eprintf "[supervisor.store] PG connect failed: %s\n"
            (Printexc.to_string exn);
          Lwt.return_false)

(* Run a single SQL migration script. The script may contain multiple
   statements separated by semicolons. pgx uses the extended (prepared)
   query protocol, which does NOT allow multiple statements in one
   Pg.execute_unit call. So we split the file into individual statements
   respecting dollar-quoted string bodies ($$...$$ can contain ;s) and
   apply each one separately. The script itself is idempotent (CREATE
   TABLE IF NOT EXISTS, INSERT ... WHERE NOT EXISTS, etc.) so applying
   twice is safe even outside schema_migrations tracking. *)

(* Split a SQL script into individual statements. Respects
   dollar-quoted strings ($tag$ ... $tag$) so semicolons inside DO $$ ;;
   blocks aren't treated as statement separators. Single-quoted string
   literals ('...') are also handled. Returns statements with whitespace
   trimmed; empty statements are skipped. *)
let split_sql_statements (content : string) : string list =
  let n = String.length content in
  let buf = Buffer.create 256 in
  let stmts = ref [] in
  let i = ref 0 in
  let emit () =
    let s = String.trim (Buffer.contents buf) in
    (* Skip empty statements. Skip comment-only statements (a statement
       that contains only `-- ...` lines), because PO sends those back
       as EmptyQueryResponse and pgx rejects them. *)
    let is_comment_only =
      let lines = String.split_on_char '\n' s in
      List.for_all
        (fun l ->
          let lt = String.trim l in
          lt = ""
          || (String.length lt >= 2 && String.sub lt 0 2 = "--"))
        lines
    in
    if s <> "" && not is_comment_only then
      stmts := s :: !stmts;
    Buffer.clear buf
  in
  let dollar_tag = ref None in
  let in_squote = ref false in
  let in_line_comment = ref false in
  while !i < n do
    let c = content.[!i] in
    if !in_line_comment then begin
      Buffer.add_char buf c;
      if c = '\n' then in_line_comment := false
    end else if !in_squote then begin
      Buffer.add_char buf c;
      if c = '\\' && !i + 1 < n then begin
        incr i;
        Buffer.add_char buf content.[!i]
      end else if c = '\'' then
        in_squote := false
    end else begin
      match !dollar_tag with
      | Some tag ->
          let tlen = String.length tag in
          if !i + tlen <= n && String.sub content !i tlen = tag then begin
            Buffer.add_string buf tag;
            i := !i + tlen - 1;
            dollar_tag := None
          end else
            Buffer.add_char buf c
      | None ->
          if c = '\'' then begin
            in_squote := true;
            Buffer.add_char buf c
          end else if c = '-' && !i + 1 < n && content.[!i + 1] = '-' then begin
            (* -- ... end-of-line comment: copy chars to buf but don't
               treat `;` inside the comment as a separator. *)
            in_line_comment := true;
            Buffer.add_char buf c;
            Buffer.add_char buf content.[!i + 1];
            incr i
          end else if c = '$' && !i + 1 < n
                    && content.[!i + 1] = '$' then begin
            Buffer.add_string buf "$$";
            dollar_tag := Some "$$";
            incr i
          end else if c = '$' then begin
            let j = ref (!i + 1) in
            while !j < n && (let ch = content.[!j] in
                  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
                  || (ch >= '0' && ch <= '9') || ch = '_') do
              incr j
            done;
            if !j < n && content.[!j] = '$' && !j > !i + 1 then begin
              let tag = String.sub content !i (!j - !i + 1) in
              Buffer.add_string buf tag;
              i := !j;
              dollar_tag := Some tag
            end else
              Buffer.add_char buf c
          end else if c = ';' then
            emit ()
          else
            Buffer.add_char buf c
    end;
    incr i
  done;
  emit ();
  List.rev !stmts

let apply_migration_file (t : t) ~path : (unit, string) result Lwt.t =
  match t.conn with
  | None -> Lwt.return_error "PG not connected"
  | Some conn ->
      let* content =
        Lwt_io.(with_file ~mode:Input path read) >>= fun s -> Lwt.return s
      in
      let stmts = split_sql_statements content in
      let* result =
        Lwt_list.fold_left_s
          (fun acc stmt ->
            match acc with
            | Error _ as e -> Lwt.return e
            | Ok () ->
                Lwt.catch
                  (fun () ->
                    let+ _ = Pg.execute_unit conn stmt in
                    Ok ())
                  (fun exn ->
                    Lwt.return_error
                      (Printf.sprintf "stmt failed: %s\n-- stmt: %s"
                         (Printexc.to_string exn)
                         (if String.length stmt > 200
                          then String.sub stmt 0 200 ^ "..."
                          else stmt))))
          (Ok ())
          stmts
      in
      (match result with
       | Ok () ->
           Printf.eprintf "[supervisor.store] applied migration %s (%d statements)\n"
             path (List.length stmts);
           flush_all ();
           Lwt.return_ok ()
       | Error msg as e ->
           Printf.eprintf "[supervisor.store] migration %s FAILED: %s\n" path msg;
           flush_all ();
           Lwt.return e)

(* List of known migrations, in order. TODO: scan migrations/ dir
   dynamically. For now, declared statically. *)
let migration_files () : (int * string * string) list =
  (* (version, description, path) *)
  [
    (1, "initial branches/code-store schema", "migrations/001_branches.sql");
    (2, "auth tables", "migrations/002_auth.sql");
    (3, "generic data row storage (shared across branches)", "migrations/003_data.sql");
    (4, "bible-verse ref unique index", "migrations/004_bible_ref_index.sql");
  ]

(* Run all pending migrations. Reads schema_migrations to know what's
   applied; applies the rest in order. Returns Ok on success, Error msg
   on failure (caller should refuse to boot images). *)
let run_pending_migrations (t : t) : (unit, string) result Lwt.t =
  match t.conn with
  | None -> Lwt.return_error "PG not connected"
  | Some conn ->
      (* Ensure schema_migrations exists (chicken-and-egg; the very
         first run hasn't applied 001 yet). *)
      let ensure_schema_migrations =
        "CREATE TABLE IF NOT EXISTS schema_migrations ( \
         version INTEGER PRIMARY KEY, \
         applied_at timestamptz NOT NULL DEFAULT now(), \
         description text)"
      in
      let* _ = Lwt.catch
        (fun () -> Pg.execute_unit conn ensure_schema_migrations)
        (fun _ -> Lwt.return_unit)
      in
      (* Read applied. *)
      let* applied =
        Lwt.catch
          (fun () ->
            let+ rows = Pg.execute conn "SELECT version FROM schema_migrations" in
            List.filter_map
              (fun r ->
                match r with
                | [ cell ] -> Pgx.Value.to_int cell
                | _ -> None)
              rows)
          (fun _ -> Lwt.return [])
      in
      let pending =
        List.filter (fun (v, _, _) -> not (List.mem v applied))
          (migration_files ())
      in
      let* results =
        Lwt_list.map_s
          (fun (v, desc, path) ->
            let* r = apply_migration_file t ~path in
            match r with
            | Ok () ->
                let _ = Lwt.catch
                  (fun () ->
                    Pg.execute_unit conn
                      (Printf.sprintf
                         "INSERT INTO schema_migrations (version, description) \
                          VALUES (%d, '%s')"
                         v (String.escaped desc)))
                  (fun _ -> Lwt.return_unit)
                in
                Lwt.return (v, Ok ())
            | Error e -> Lwt.return (v, Error e))
          pending
      in
      let first_err =
        List.find_map
          (fun (v, r) ->
             match r with
             | Ok () -> None
             | Error e -> Some (v, e))
          results
      in
      match first_err with
      | Some (v, e) -> Lwt.return_error (Printf.sprintf "migration %d: %s" v e)
      | None -> Lwt.return_ok ()
