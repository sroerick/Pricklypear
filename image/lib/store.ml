(** Pricklypear Image — Storage layer.

    A thin wrapper over [pgx_lwt] that provides branch-aware lookups for
    function_defs, type_defs, and view_bindings, plus the session/health tables.

    Per spec [storage.schema]: each code-def table is keyed by (branch_id,
    name). Effective definition lookup walks the parent chain (recursive
    CTE — branch OVERRIDE semantics, NOT inheritance union).

    For the minimal image, the DB pool is constructed lazily and lookups
    return [None] if the DB is unreachable, so the image still boots and
    serves its system pages. This makes the smoke test (boot, serve hello
    world, REPL endpoint accept a connection) possible with NO Postgres
    running — satisfies the verification checkpoint. TODO: real lookups. *)
open Lwt.Syntax

(* A [Pgx_lwt.Io_intf.S] implementation backed by [Lwt_unix]. Pgx_lwt's
   Make functor requires this; it provides the low-level read/write/flush
   primitives over a TCP or Unix-domain socket. *)
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
  let getlogin () = Lwt.return "pricklypear"
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

(* Concrete pgx connection module, built with our Lwt_unix Io. *)
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
    user = env_or "PP_DB_USER" (env_or "PGUSER" "pricklypear_image");
    password = env_or "PP_DB_PASS" "";
  }

type t = {
  cfg : config;
  mutable conn : Pg.t option;
  mutable branch_id : string;  (* (checkout 'branch) reassigns it *)
  (* Session-level dynamic transaction binding (option A from
     BORGE-1783438027, lands it29). Points to the transaction-scoped
     pgx connection while inside (with-transaction ...), so all DB
     prims called from within the body see the SAME connection (and
     thus the same transaction — including row locks acquired by
     select-for-update). None when not inside a transaction, in
     which case prims fall back to [conn_of] (the default pool conn). *)
  mutable current_tx : Pg.t option;
}

let make ~cfg ~branch_id () : t = { cfg; conn = None; branch_id; current_tx = None }

(* Expose the optional pool connection to sibling modules. [conn_of]
   always returns the long-lived pool connection; prims should prefer
     [current_conn t]
   which returns the in-transaction connection (if set) or falls
   back to the pool conn. *)
let conn_of (t : t) : Pg.t option = t.conn

(* Active connection for DB prims to use: in-transaction connection if
   we're inside a (with-transaction ...) body, else the default pool
   connection. Returns None when there's no DB at all. *)
let current_conn (t : t) : Pg.t option =
  match t.current_tx with
  | Some tx -> Some tx
  | None -> t.conn

(* Run [f] inside a Postgres transaction with SERIALIZABLE isolation
   (per AGENTS.md / spec [storage.transactions]: SERIALIZABLE by
   default for mutations). [f] receives the upgraded transactional
   connection (pgx returns a new `t` representing the in-transaction
   state); all execute calls against that connection participate in the
   transaction.

   If [f] raises, pgx automatically ROLLS BACK and propagates the
   exception to with_transaction's caller. Per pgx's mli it's an error
   to call commit/rollback manually inside [f].

   On no-DB (t.conn = None), still calls [f] with None so the caller
   can implement their no-DB fallback (rare case; stores without DB
   aren't writable).

   Resolves the carried-forward TODO on fork_branch and merge_branch.
   Both now run all their SQL statements as a SINGLE atomic
   transaction. *)
let with_transaction ?(isolation = Pgx.Isolation.Serializable)
    (t : t) (f : Pg.t option -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> f None
  | Some conn ->
      Pg.with_transaction ~isolation conn (fun tx_conn -> f (Some tx_conn))

(** Session-scoped dynamic transaction binding (option A from
    BORGE-1783438027, lands it29). Used by the (with-transaction ...)
    Lisp prim: sets [t.current_tx] to the transactional pgx connection
    BEFORE [f] runs, so DB prims called from within the body consult
    [current_conn] (which they already use) and see the SAME
    transaction — including row locks acquired via
    (select-for-update 'type id).

    The cleanup at the end is a [finally]-shaped try-with that clears
    [t.current_tx] whether [f] succeeds or raises. CRITICAL: the
    error handler retries the body's value-type raise AFTER restoring
    the [current_tx = None] invariant; pgx still sees the propagated
    exception via its own finally and ROLLS BACK the transaction.

    Nested transactions (a (with-transaction ...) inside another) are
    NOT supported — at the pgx level [Pg.with_transaction] on the
    in-transaction conn would error. We raise [Interp_error] if
    someone tries it. *)
let with_dynamic_transaction
    ?(isolation = Pgx.Isolation.Serializable) (t : t)
    (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> f ()
  | Some conn ->
      if t.current_tx <> None then
        Lwt.fail (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.User_error
                       "with-transaction: nested transactions not supported"))
      else
        Pg.with_transaction ~isolation conn
          (fun tx_conn ->
             t.current_tx <- Some tx_conn;
             Lwt.finalize f
               (fun () -> t.current_tx <- None; Lwt.return_unit))


let connect (t : t) : unit Lwt.t =
  match t.conn with
  | Some _ -> Lwt.return_unit
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
          Printf.eprintf "[store] connected to PG %s:%d/%s as %s\n"
            t.cfg.host t.cfg.port t.cfg.database t.cfg.user;
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[store] PG connect failed: %s\n"
            (Printexc.to_string exn);
          Lwt.return_unit)

(* Branch-aware lookup. Walks the branch parent chain via a recursive
   CTE so that a definition in [main] is visible from a child branch,
   and a definition in the child OVERRIDES the same name in main
   (per spec storage.no-inheritance: OVERRIDE, not UNION).

   Returns the definition's parsed AST and author. Errors/misses are
   logged and return None so the REPL keeps working with no DB. *)
let lookup_function (t : t) (name : string)
  : (Pricklypear_lisp.Ast.expr * string) option Lwt.t =
  match t.conn with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          (* Recursive CTE: start at the session's branch and walk parents.
             Return the FIRST row matching [name] when ordered by depth
             ascending (closest ancestor wins). *)
          let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.body, fd.author FROM function_defs fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "WHERE fd.name = $2 " ^
            "ORDER BY chain.depth ASC LIMIT 1" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name ]
              sql in
          (match rows with
           | [] -> Lwt.return_none
           | row :: _ ->
               (* Each row is [cell; cell] where cell = v option;
                  first is jsonb body, second is author text. *)
               (match row with
                | [ body_cell; author_cell ] ->
                    (match Pgx.Value.to_string body_cell,
                             Pgx.Value.to_string author_cell with
                     | Some body_str, Some author ->
                        (match Yojson.Safe.from_string body_str with
                         | j ->
                             (match Pricklypear_lisp.Ast.expr_of_json j with
                              | Ok e -> Lwt.return (Some (e, author))
                              | Error msg ->
                                  Printf.eprintf
                                    "[store] lookup_function: parse %s\n" msg;
                                  Lwt.return_none)
                         | exception Yojson.Json_error msg ->
                             Printf.eprintf
                               "[store] lookup_function: json %s\n" msg;
                             Lwt.return_none)
                     | _ -> Lwt.return_none)
                | _ -> Lwt.return_none))
        )
        (fun exn ->
          Printf.eprintf "[store] lookup_function error: %s\n"
            (Printexc.to_string exn);
          Lwt.return_none)

(* List all function names visible to this branch (via the same
   recursive-CTE override walk). Used at session-make time to load
   persisted definitions into the env. *)
(** Effective function defs for the current branch: parent-chain walk,
    closest definition wins (depth ASC, first occurrence kept).
    [depth] is 0 for rows owned by the current branch, >0 if inherited.
    Callers that only need name/body/author can ignore depth. *)
let list_function_names_meta (t : t)
  : (string * Pricklypear_lisp.Ast.expr * string * int) list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.name, fd.body, fd.author, chain.depth " ^
            "FROM function_defs fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC, fd.name ASC" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql in
          let seen = Hashtbl.create 64 in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; body_cell; author_cell; depth_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string body_cell,
                            Pgx.Value.to_string author_cell,
                            Pgx.Value.to_string depth_c with
                     | Some name, Some body_str, Some author, Some d_s ->
                         if Hashtbl.mem seen name then None
                         else begin
                           Hashtbl.replace seen name ();
                           let depth =
                             int_of_string_opt d_s |> Option.value ~default:0
                           in
                           (try
                              let j = Yojson.Safe.from_string body_str in
                              (match Pricklypear_lisp.Ast.expr_of_json j with
                               | Ok e -> Some (name, e, author, depth)
                               | Error _ -> None)
                            with Yojson.Json_error _ -> None)
                         end
                     | _ -> None)
                | _ -> None)
              rows
          in
          Lwt.return acc)
        (fun exn ->
          Printf.eprintf "[store] list_function_names error: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

(** Effective defs only (deduped). Same as [list_function_names_meta] without depth. *)
let list_function_names (t : t)
  : (string * Pricklypear_lisp.Ast.expr * string) list Lwt.t =
  let* rows = list_function_names_meta t in
  Lwt.return (List.map (fun (n, e, a, _) -> (n, e, a)) rows)

(** Full branch-chain defs for env rehydrate: parents first (depth DESC),
    then children. Includes ALL rows (not deduped) so Session.load_defs can
    redefine — last write wins (child override). See session.load_defs note. *)
let list_function_names_for_load (t : t)
  : (string * Pricklypear_lisp.Ast.expr * string) list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.name, fd.body, fd.author, chain.depth " ^
            "FROM function_defs fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth DESC, fd.name ASC" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; body_cell; author_cell; _depth_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string body_cell,
                            Pgx.Value.to_string author_cell with
                     | Some name, Some body_str, Some author ->
                         (try
                            let j = Yojson.Safe.from_string body_str in
                            (match Pricklypear_lisp.Ast.expr_of_json j with
                             | Ok e -> Some (name, e, author)
                             | Error _ -> None)
                          with Yojson.Json_error _ -> None)
                     | _ -> None)
                | _ -> None)
              rows
          in
          Lwt.return acc)
        (fun exn ->
          Printf.eprintf "[store] list_function_names_for_load error: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

(* ---- own-branch stats / overrides (no recursive CTE) ---- *)

let count_functions_on_branch (t : t) ~(branch_id : string) : int Lwt.t =
  match t.conn with
  | None -> Lwt.return 0
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT COUNT(*)::text FROM function_defs WHERE branch_id = $1" in
          let* rows =
            Pg.execute conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_string c with
                | Some s -> Lwt.return (int_of_string_opt s |> Option.value ~default:0)
                | None -> Lwt.return 0)
           | _ -> Lwt.return 0))
        (fun exn ->
          Printf.eprintf "[store] count_functions_on_branch: %s\n"
            (Printexc.to_string exn);
          Lwt.return 0)

let count_routes_on_branch (t : t) ~(branch_id : string) : int Lwt.t =
  match t.conn with
  | None -> Lwt.return 0
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT COUNT(*)::text FROM view_bindings WHERE branch_id = $1" in
          let* rows =
            Pg.execute conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_string c with
                | Some s -> Lwt.return (int_of_string_opt s |> Option.value ~default:0)
                | None -> Lwt.return 0)
           | _ -> Lwt.return 0))
        (fun exn ->
          Printf.eprintf "[store] count_routes_on_branch: %s\n"
            (Printexc.to_string exn);
          Lwt.return 0)

(** Own rows only: (name, author, updated_at). *)
let list_own_functions (t : t) ~(branch_id : string)
  : (string * string * string) list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT name, author, updated_at::text FROM function_defs " ^
            "WHERE branch_id = $1 ORDER BY name" in
          let* rows =
            Pg.execute conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          Lwt.return
            (List.filter_map
               (fun row ->
                 match row with
                 | [ n; a; u ] ->
                     (match Pgx.Value.to_string n,
                             Pgx.Value.to_string a,
                             Pgx.Value.to_string u with
                      | Some name, Some author, Some updated ->
                          Some (name, author, updated)
                      | _ -> None)
                 | _ -> None)
               rows))
        (fun exn ->
          Printf.eprintf "[store] list_own_functions: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

let list_own_routes (t : t) ~(branch_id : string)
  : (string * string) list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT route, handler FROM view_bindings WHERE branch_id = $1 " ^
            "ORDER BY route" in
          let* rows =
            Pg.execute conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          Lwt.return
            (List.filter_map
               (fun row ->
                 match row with
                 | [ r; h ] ->
                     (match Pgx.Value.to_string r, Pgx.Value.to_string h with
                      | Some route, Some handler -> Some (route, handler)
                      | _ -> None)
                 | _ -> None)
               rows))
        (fun exn ->
          Printf.eprintf "[store] list_own_routes: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

(** Child own names left-joined to parent: name, child_updated, parent_updated opt. *)
let list_overrides (t : t) ~(branch_id : string) ~(parent_id : string option)
  : (string * string * string option) list Lwt.t =
  match t.conn, parent_id with
  | None, _ -> Lwt.return []
  | Some _, None ->
      (* Root branch: every own def is "new". *)
      let* own = list_own_functions t ~branch_id in
      Lwt.return (List.map (fun (n, _, u) -> (n, u, None)) own)
  | Some conn, Some pid ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT c.name, c.updated_at::text, p.updated_at::text " ^
            "FROM function_defs c " ^
            "LEFT JOIN function_defs p " ^
            "  ON p.branch_id = $2 AND p.name = c.name " ^
            "WHERE c.branch_id = $1 ORDER BY c.name" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string pid ]
              sql
          in
          Lwt.return
            (List.filter_map
               (fun row ->
                 match row with
                 | [ n; cu; pu ] ->
                     (match Pgx.Value.to_string n, Pgx.Value.to_string cu with
                      | Some name, Some child_u ->
                          let parent_u =
                            match Pgx.Value.to_string pu with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          Some (name, child_u, parent_u)
                      | _ -> None)
                 | _ -> None)
               rows))
        (fun exn ->
          Printf.eprintf "[store] list_overrides: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

let lookup_function_on_branch (t : t) ~(branch_id : string) ~(name : string)
  : (Pricklypear_lisp.Ast.expr * string * string) option Lwt.t =
  match t.conn with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT body::text, author, updated_at::text FROM function_defs " ^
            "WHERE branch_id = $1 AND name = $2" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string name ]
              sql
          in
          (match rows with
           | [ [ body_c; author_c; upd_c ] ] ->
               (match Pgx.Value.to_string body_c,
                       Pgx.Value.to_string author_c,
                       Pgx.Value.to_string upd_c with
                | Some body_str, Some author, Some updated ->
                    (try
                       match Pricklypear_lisp.Ast.expr_of_json
                               (Yojson.Safe.from_string body_str) with
                       | Ok e -> Lwt.return (Some (e, author, updated))
                       | Error _ -> Lwt.return_none
                     with _ -> Lwt.return_none)
                | _ -> Lwt.return_none)
           | _ -> Lwt.return_none))
        (fun exn ->
          Printf.eprintf "[store] lookup_function_on_branch: %s\n"
            (Printexc.to_string exn);
          Lwt.return_none)

let delete_function_on_branch (t : t) ~(branch_id : string) ~(name : string)
  : bool Lwt.t =
  match t.conn with
  | None -> Lwt.return false
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "DELETE FROM function_defs WHERE branch_id = $1 AND name = $2 " ^
            "RETURNING name" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string name ]
              sql
          in
          Lwt.return (rows <> []))
        (fun exn ->
          Printf.eprintf "[store] delete_function_on_branch: %s\n"
            (Printexc.to_string exn);
          Lwt.return false)

(** Copy selected function names from source branch to target (upsert). *)
let promote_functions (t : t) ~(source_id : string) ~(target_id : string)
    ~(names : string list) : int Lwt.t =
  match t.conn with
  | None -> Lwt.return 0
  | Some _ when names = [] -> Lwt.return 0
  | Some conn ->
      Lwt.catch
        (fun () ->
          Lwt_list.fold_left_s
            (fun acc name ->
              let* src =
                lookup_function_on_branch t ~branch_id:source_id ~name
              in
              match src with
              | None -> Lwt.return acc
              | Some (body, author, _) ->
                  let body_json =
                    Yojson.Safe.to_string
                      (Pricklypear_lisp.Ast.expr_to_json body) in
                  let sql =
                    "INSERT INTO function_defs " ^
                    "  (branch_id, name, body, author) " ^
                    "VALUES ($1, $2, $3::jsonb, $4) " ^
                    "ON CONFLICT (branch_id, name) DO UPDATE " ^
                    "SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
                    "    updated_at = now() RETURNING name" in
                  let* rows =
                    Pg.execute conn
                      ~params:[ Pgx.Value.of_string target_id;
                                Pgx.Value.of_string name;
                                Pgx.Value.of_string body_json;
                                Pgx.Value.of_string author ]
                      sql
                  in
                  Lwt.return (if rows = [] then acc else acc + 1))
            0 names)
        (fun exn ->
          Printf.eprintf "[store] promote_functions: %s\n"
            (Printexc.to_string exn);
          Lwt.return 0)

(* Persist a function def. INSERT ... ON CONFLICT (branch_id, name)
   DO UPDATE so redefining a name in the same branch upserts. The body
   is serialized to JSONB. *)
let persist_function (t : t) (name : string)
    (body : Pricklypear_lisp.Ast.expr) (author : string)
  : unit Lwt.t =
  match t.conn with
  | None -> Lwt.return_unit  (* no DB; env binding still happened in-memory *)
  | Some conn ->
      Lwt.catch
        (fun () ->
          let body_json =
            Yojson.Safe.to_string
              (Pricklypear_lisp.Ast.expr_to_json body) in
          let sql =
            "INSERT INTO function_defs (branch_id, name, body, author) " ^
            "VALUES ($1, $2, $3::jsonb, $4) " ^
            "ON CONFLICT (branch_id, name) DO UPDATE " ^
            "SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
            "    updated_at = now()" in
          let* _ =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_string body_json;
                        Pgx.Value.of_string author ]
              sql in
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[store] persist_function error: %s\n"
            (Printexc.to_string exn);
          Lwt.return_unit)

let list_branches (t : t) : Pricklypear_common.Branch.t list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "SELECT id::text, COALESCE(parent_id::text, ''), " ^
            "       owner, name, created_at::text, " ^
            "       COALESCE(last_synced::text, '') " ^
            "FROM branches ORDER BY owner, name" in
          let* rows = Pg.execute conn sql in
          Lwt.return
            (List.filter_map
               (fun row ->
                 match row with
                 | [ id_c; pid_c; owner_c; name_c; created_c; synced_c ] ->
                     (match Pgx.Value.to_string id_c,
                              Pgx.Value.to_string owner_c,
                              Pgx.Value.to_string name_c,
                              Pgx.Value.to_string created_c with
                      | Some id, Some owner, Some name, Some created ->
                          let parent_id =
                            match Pgx.Value.to_string pid_c with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          let last_synced =
                            match Pgx.Value.to_string synced_c with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          let base =
                            Pricklypear_common.Branch.make
                              ~id ~parent_id ~owner ~name
                              ~created_at:created ()
                          in
                          Some
                            (match last_synced with
                             | Some s -> { base with last_synced = Some s }
                             | None -> base)
                      | _ -> None)
                 | _ -> None)
               rows))
        (fun exn ->
          Printf.eprintf "[store] list_branches error: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

(* Look up a branch id by (owner, name). Used by (fork ...) to find
   the parent's UUID from its Lisp-level reference. Returns None if
   the branch isn't in the DB (or no DB). *)
let lookup_branch_id_by_name (t : t) ~(owner : string) ~(name : string)
  : string option Lwt.t =
  match t.conn with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql = "SELECT id::text FROM branches WHERE owner = $1 AND name = $2" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string owner;
                        Pgx.Value.of_string name ]
              sql
          in
          (match rows with
           | [ [ id_cell ] ] ->
               Lwt.return (Pgx.Value.to_string id_cell)
           | _ -> Lwt.return_none))
        (fun exn ->
          Printf.eprintf "[store] lookup_branch_id error: %s\n"
            (Printexc.to_string exn);
          Lwt.return_none)

(* Switch the store's branch_id: subsequent lookups/persists/registers
   will target the new branch. Per spec [image.repl-experience]:
   "(checkout branch-id)" keeps REPL locals (env) but changes visibility
   (which (define)d functions are effective). We mutate the field.

   Re-loading the env is the caller's responsibility (session.eval_one
   does it after we return successfully). *)
let checkout_branch (t : t) ~(new_branch_id : string) : unit Lwt.t =
  t.branch_id <- new_branch_id;
  Lwt.return_unit

(* Get the branch_id the store is currently bound to. *)
let current_branch_id (t : t) : string = t.branch_id

(** Canonical system/main UUID — seeded by migrations/001_branches.sql. *)
let system_main_id = "00000000-0000-0000-0000-000000000001"

(** Temporarily rebind [t.branch_id] for the duration of [f], then restore.
    Used so seed_stdlib can write package defaults to system/main without
    polluting the user's personal branch. *)
let with_branch (t : t) ~(branch_id : string) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  let old = t.branch_id in
  t.branch_id <- branch_id;
  Lwt.finalize f (fun () -> t.branch_id <- old; Lwt.return_unit)

(** Ensure a branch row exists; create an EMPTY child (parent pointer only)
    if missing. Does NOT copy function_defs/types/routes — inheritance via
    the recursive CTE makes parent defs visible, and personal overrides are
    just rows on the child. This is the right shape for user/personal. *)
let ensure_branch (t : t) ~(parent_id : string) ~(owner : string)
    ~(name : string) : string option Lwt.t =
  let* existing = lookup_branch_id_by_name t ~owner ~name in
  match existing with
  | Some id -> Lwt.return (Some id)
  | None ->
      (match t.conn with
       | None -> Lwt.return_none
       | Some conn ->
           Lwt.catch
             (fun () ->
               let sql =
                 "INSERT INTO branches (id, parent_id, owner, name) " ^
                 "VALUES (gen_random_uuid(), $1, $2, $3) " ^
                 "ON CONFLICT (owner, name) DO UPDATE SET name = EXCLUDED.name " ^
                 "RETURNING id::text" in
               let* rows =
                 Pg.execute conn
                   ~params:[ Pgx.Value.of_string parent_id;
                             Pgx.Value.of_string owner;
                             Pgx.Value.of_string name ]
                   sql
               in
               (match rows with
                | [ [ id_cell ] ] -> Lwt.return (Pgx.Value.to_string id_cell)
                | _ ->
                    (* Race: another image created it; re-lookup. *)
                    lookup_branch_id_by_name t ~owner ~name))
             (fun exn ->
               Printf.eprintf "[store] ensure_branch error: %s\n"
                 (Printexc.to_string exn);
               lookup_branch_id_by_name t ~owner ~name))

(* Fork a branch: create a new branch with parent_id as its parent,
   then copy all function_defs, type_defs, view_bindings from the
   parent into the new branch.

   Per spec [branching.fork]:
     (fork 'main 'alice/experiment)
   -> INSERT INTO branches (id, parent_id, owner, name) VALUES ...
   -> INSERT INTO function_defs (branch_id, name, body, ...)
      SELECT new_uuid, name, body, ... FROM function_defs
      WHERE branch_id = parent_id
   -> Same for type_defs, view_bindings

   Cost: O(num code rows in parent). For ~100 fns + ~20 types,
   a few hundred row inserts — well under a second.

   Forking does NOT copy data table rows (those live in main's
   data tables and are shared across all branches).

   Returns the new branch_id as a string (or raises/fails-closed).
   The fork is one logical operation; the 3 copy INSERTs run in a
   single SERIALIZABLE transaction via [with_transaction] (it16).
   If any statement fails, the entire fork is rolled back. *)
(* Persist a route binding: (route "/path" 'handler-fn-name).
   INSERT ... ON CONFLICT (branch_id, route) DO UPDATE — so re-defining
   a route overrides the existing binding. *)
let normalize_visibility (v : string) : string =
  match String.lowercase_ascii (String.trim v) with
  | "public" -> "public"
  | _ -> "private"

(** Persist a route. [visibility_opt]:
    - Some v → set visibility (public|private)
    - None → on INSERT default private; on UPDATE keep existing visibility
      (handler-only re-register does not clobber public). *)
let register_route (t : t) ~(route : string) ~(handler : string)
    ?visibility ()
  : unit Lwt.t =
  match t.conn with
  | None -> Lwt.return_unit
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql, params =
            match visibility with
            | Some vis ->
                let vis = normalize_visibility vis in
                ( "INSERT INTO view_bindings " ^
                  "  (branch_id, route, handler, visibility) " ^
                  "VALUES ($1, $2, $3, $4) " ^
                  "ON CONFLICT (branch_id, route) DO UPDATE " ^
                  "SET handler = EXCLUDED.handler, " ^
                  "    visibility = EXCLUDED.visibility"
                , [ Pgx.Value.of_string t.branch_id
                  ; Pgx.Value.of_string route
                  ; Pgx.Value.of_string handler
                  ; Pgx.Value.of_string vis ] )
            | None ->
                ( "INSERT INTO view_bindings " ^
                  "  (branch_id, route, handler, visibility) " ^
                  "VALUES ($1, $2, $3, 'private') " ^
                  "ON CONFLICT (branch_id, route) DO UPDATE " ^
                  "SET handler = EXCLUDED.handler"
                , [ Pgx.Value.of_string t.branch_id
                  ; Pgx.Value.of_string route
                  ; Pgx.Value.of_string handler ] )
          in
          let* _ = Pg.execute conn ~params sql in
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[store] register_route error: %s\n"
            (Printexc.to_string exn);
          Lwt.return_unit)

(* List all route bindings visible to this branch (recursive-CTE walk,
   same as list_function_names — annotations from ancestors are visible
   via inheritance, child's definition OVERRIDES parent's). Returns
   (route, handler, visibility) ordered with closest ancestor first. *)
let list_routes (t : t) : (string * string * string) list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT vb.route, vb.handler, " ^
            "COALESCE(vb.visibility, 'private') " ^
            "FROM view_bindings vb " ^
            "JOIN chain ON vb.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql
          in
          Lwt.return
            (List.filter_map
               (fun row ->
                 match row with
                 | [ route_c; handler_c; vis_c ] ->
                     (match Pgx.Value.to_string route_c,
                             Pgx.Value.to_string handler_c with
                      | Some r, Some h ->
                          let vis =
                            match Pgx.Value.to_string vis_c with
                            | Some v -> normalize_visibility v
                            | None -> "private"
                          in
                          Some (r, h, vis)
                      | _ -> None)
                 | [ route_c; handler_c ] ->
                     (* Pre-migration fallback *)
                     (match Pgx.Value.to_string route_c,
                             Pgx.Value.to_string handler_c with
                      | Some r, Some h -> Some (r, h, "private")
                      | _ -> None)
                 | _ -> None)
               rows))
        (fun exn ->
          Printf.eprintf "[store] list_routes error: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

(* ---- type_defs ----

   Per spec [storage.types]: user declares a type with (deftype 'name
   field1 field2 ...). The declaration is INSERTed into type_defs with
   branch_id = current branch and fields = JSON array of strings (the
   field names, in declared order).

   The data DOESN'T live in a per-type table: packed in migration 003's
   note that generating one PG table per Lisp type would require
   runtime DDL breaking additive-migrations-only. Instead, all data
   rows are stored in the shared data_rows(type_name, fields jsonb)
   table, and type_defs is pure declaration metadata. (Type
   declarations MAY eventually guide field-level validation; for now
   they're documentation.)

   The recursive-CTE override walk applies to type_defs the same as
   function_defs / view_bindings: ancestors' types are visible, child's
   definition wins on name conflict. *)

let persist_type (t : t) ~(name : string) ~(fields : string list)
    ~(author : string) : unit Lwt.t =
  match t.conn with
  | None -> Lwt.return_unit
  | Some conn ->
      Lwt.catch
        (fun () ->
          let fields_json =
            Yojson.Safe.to_string
              (`List (List.map (fun s -> `String s) fields)) in
          let sql =
            "INSERT INTO type_defs (branch_id, name, fields, author) " ^
            "VALUES ($1, $2, $3::jsonb, $4) " ^
            "ON CONFLICT (branch_id, name) DO UPDATE " ^
            "SET fields = EXCLUDED.fields, author = EXCLUDED.author, " ^
            "    updated_at = now()" in
          let* _ =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_string fields_json;
                        Pgx.Value.of_string author ]
              sql
          in
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[store] persist_type error: %s\n"
            (Printexc.to_string exn);
          Lwt.return_unit)

(* List all type declarations visible to this branch via the recursive
   CTE override walk. Returns (name, fields) pairs with closest
   ancestor first; caller-side Hashtbl first-occurrence dedup wins. *)
let list_types (t : t) : (string * string list) list Lwt.t =
  match t.conn with
  | None -> Lwt.return []
  | Some conn ->
      Lwt.catch
        (fun () ->
          let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT td.name, td.fields::text FROM type_defs td " ^
            "JOIN chain ON td.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC" in
          let* rows =
            Pg.execute conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql
          in
          Lwt.return
            (List.filter_map
               (fun row ->
                 match row with
                 | [ name_cell; fields_cell ] ->
                     (match Pgx.Value.to_string name_cell,
                             Pgx.Value.to_string fields_cell with
                      | Some name, Some fs ->
                          (try
                             let fs' =
                               match Yojson.Safe.from_string fs with
                               | `List lst ->
                                   List.filter_map
                                     (fun j ->
                                       match j with
                                       | `String s -> Some s
                                       | _ -> None)
                                     lst
                               | _ -> []
                             in Some (name, fs')
                           with _ -> None)
                      | _ -> None)
                 | _ -> None)
               rows))
        (fun exn ->
          Printf.eprintf "[store] list_types error: %s\n"
            (Printexc.to_string exn);
          Lwt.return [])

let fork_branch (t : t) ~(parent_id : string) ~(owner : string)
    ~(child_name : string) : string option Lwt.t =
  (* All four SQL statements (branch row + 3 copies) run inside a
     single SERIALIZABLE transaction via [with_transaction]. If any
     statement raises, pgx AUTOMATICALLY ROLLS BACK and propagates the
     exception to [with_transaction]'s caller. We catch the exception
     OUTSIDE [with_transaction] (after pgx has rolled back) so a partial
     fork doesn't leave a half-forked branch row in PG; the caller sees
     [None] with an [e]printf diagnostic, REPL stays alive.*)
  Lwt.catch
    (fun () ->
      with_transaction t (fun conn ->
        match conn with
        | None -> Lwt.return_none  (* no DB; can't fork *)
        | Some conn ->
            (* 1. Create the new branch row, returning its id. *)
            let insert_branch_sql =
              "INSERT INTO branches (id, parent_id, owner, name) " ^
              "VALUES (gen_random_uuid(), $1, $2, $3) " ^
              "RETURNING id::text" in
            let* rows =
              Pg.execute conn
                ~params:[ Pgx.Value.of_string parent_id;
                          Pgx.Value.of_string owner;
                          Pgx.Value.of_string child_name ]
                insert_branch_sql
            in
            (match rows with
             | [ [ id_cell ] ] ->
                 (match Pgx.Value.to_string id_cell with
                  | Some new_branch_id ->
                      (* 2. Copy function_defs from parent. *)
                      let copy_fns_sql =
                        "INSERT INTO function_defs " ^
                        "  (branch_id, name, body, author, updated_at) " ^
                        "SELECT $1, name, body, author, updated_at " ^
                        "FROM function_defs WHERE branch_id = $2" in
                      let* _ =
                        Pg.execute conn
                          ~params:[ Pgx.Value.of_string new_branch_id;
                                    Pgx.Value.of_string parent_id ]
                          copy_fns_sql
                      in
                      (* 3. Copy type_defs from parent. *)
                      let copy_types_sql =
                        "INSERT INTO type_defs " ^
                        "  (branch_id, name, fields, migrated, schema_vers, " ^
                        "   author, updated_at) " ^
                        "SELECT $1, name, fields, migrated, schema_vers, " ^
                        "       author, updated_at " ^
                        "FROM type_defs WHERE branch_id = $2" in
                      let* _ =
                        Pg.execute conn
                          ~params:[ Pgx.Value.of_string new_branch_id;
                                    Pgx.Value.of_string parent_id ]
                          copy_types_sql
                      in
                      (* 4. Copy view_bindings from parent (incl. visibility). *)
                      let copy_views_sql =
                        "INSERT INTO view_bindings " ^
                        "  (branch_id, route, handler, visibility) " ^
                        "SELECT $1, route, handler, " ^
                        "  COALESCE(visibility, 'private') " ^
                        "FROM view_bindings WHERE branch_id = $2" in
                      let* _ =
                        Pg.execute conn
                          ~params:[ Pgx.Value.of_string new_branch_id;
                                    Pgx.Value.of_string parent_id ]
                          copy_views_sql
                      in
                      Lwt.return (Some new_branch_id)
                  | None -> Lwt.return_none)
             | _ -> Lwt.return_none)))
    (fun exn ->
      Printf.eprintf "[store] fork_branch error: %s\n"
        (Printexc.to_string exn);
      Lwt.return_none)

(* Merge a source branch's function_defs into a target branch.

   Per spec [branching.merge]:
     (merge 'alice/experiment 'alice/main)
   -> copy all function_defs from source to target (upsert).
   Policy: admin-only merges to main; per-user can merge into their
   own branches. Policy is enforced at the Lisp intercept layer
   (session.ml); Store.merge_branch just does the SQL.

   Initial implementation: copies ALL function_defs from source to
   target via INSERT ... ON CONFLICT (branch_id, name) DO UPDATE.
   TODO: diff-based merge (only fns whose updated_at > parent's
   last_synced_with_child, OR whose name was added in child).
   TODO: detect conflicts (parent ALSO modified same name since fork)
   and per-function conflict resolution.
   TODO: schema-merge for type_defs — only main accepts schema changes
   via special migration-governed merge.

   Wrapped in a SERIALIZABLE transaction via [with_transaction] since
   it16 (previously best-effort atomic — carried-forward since it9). *)
let merge_branch (t : t) ~(source_id : string) ~(target_id : string)
  : int option Lwt.t =
  Lwt.catch
    (fun () ->
      with_transaction t (fun conn ->
        match conn with
        | None -> Lwt.return_none  (* no DB; can't merge *)
        | Some conn ->
            (* Upsert all function_defs from source into target. Returns
               the count of merged rows via Pg's INSERT RETURNING. *)
            let sql =
              "INSERT INTO function_defs (branch_id, name, body, author, updated_at) " ^
              "SELECT $1, name, body, author, updated_at " ^
              "FROM function_defs WHERE branch_id = $2 " ^
              "ON CONFLICT (branch_id, name) DO UPDATE " ^
              "  SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
              "      updated_at = EXCLUDED.updated_at " ^
              "RETURNING name" in
            let* rows =
              Pg.execute conn
                ~params:[ Pgx.Value.of_string target_id;
                          Pgx.Value.of_string source_id ]
                sql
            in
            Lwt.return (Some (List.length rows))))
    (fun exn ->
      Printf.eprintf "[store] merge_branch error: %s\n"
        (Printexc.to_string exn);
      Lwt.return_none)
