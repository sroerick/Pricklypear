(** Pricklypear Image — CRUD primitives over the one-tree datum namespace.

    get/create/update/delete/rows are normal prims returning real DB
    results. They compose:
      (define u (get 'user 42))
      (str u)

    Per spec [one-tree.branching-interaction] /data DECISION (user-owned
    data) + [one-tree.migration-plan] (user-scoping):
      - Datum nodes live in tree_nodes; reads project through the
        tree_data_rows view. The scaffolding data_rows table is the
        reverse-mirror kept warm for residual readers / fork-copy.
      - Data is NOT branch-versioned (no branch_id); fork/merge copies
        code, not data.
      - Visibility is USER-SCOPED: a session sees rows it authored plus
        the shared seed corpus (bible-*, strongs-entry). Prim SQL
        enforces this; RLS on tree_nodes is the floor (migration 013).
      - `fields` is JSON; Lisp sees it as a string, with dict-* prims
        for field access.
      - No ORM; parameterized SQL via pgx. Multi-step atomicity uses
        Store.with_dynamic_transaction / (with-transaction …).

    Uses [Io_lwt.Db.current_conn] so in-tx calls see the bound connection
    (read prims run inside with_transaction so set_local_pp_user keeps the
    RLS floor active; see i-rls-nontx).  Misses return VNil so Lisp can
    branch on `(if (get 'user id) ...)`. *)
open Pricklypear_lisp.Ast

(* Reads project through tree_data_rows (the tree_nodes datum view),
   UNCONDITIONALLY, with an explicit author/seed confinement predicate
   (primary control) plus the RLS floor on tree_nodes (migration 013).
   tree_data_rows is not behind the legacy PP_TREE_READS flag — the tree
   view is the read source of truth. Writes go through Tree_store into
   tree_nodes directly; the reverse trigger mirrors tree_nodes back into
   the data_rows scaffolding table (residual readers / fork-copy). *)
let data_rel () = "tree_data_rows"

(** Session username for confinement. Prefer Request_context snapshot
    (cotenant-safe); fall back to anonymous when unbound (scripts). *)
let session_user () : string =
  match Request_context.get () with
  | Some c -> (Request_context.manifest c).username
  | None -> "anonymous"

(** RLS floor for the datum prims (i-1788670872-3).

    The CRUD tree/edge/data-lens prims read or write tree_nodes rows —
    including kind='datum' rows that migration 013's datum_user_isolation
    policy confines to the session identity WHEN pp.user is set. The floor
    only exists inside a transaction (set_config('pp.user', …, true) is
    LOCAL); on the bare pool conn the policy passes everything, so tree
    prims and http_tree reads could touch another identity's datums while
    the CRUD get/find-* prims (i-rls-nontx) confined.

    with_tree_tx routes each prim through a READ COMMITTED transaction so
    set_local_pp_user arms the floor and multi-statement sequences (link,
    tree/move, tree/delete + edge count) are atomic. READ COMMITTED, not
    the SERIALIZABLE default: these prims ran per-statement autocommit
    before, so low isolation preserves their concurrency behavior exactly
    (no new 40001 abort class).

    Inside a caller's transaction ((with-transaction …) Lisp bodies, the
    job runner) the floor is already armed by the outer tx and a nested
    BEGIN would abort (pgx begin_work) — so the callback rides the
    current conn unchanged. *)
let with_tree_tx (store : Store.t) ~(none : unit -> 'a)
    (f : Store_db.conn -> 'a) : 'a =
  if Io_lwt.Db.in_transaction store then
    (match Io_lwt.Db.current_conn store with
     | None -> none () (* unreachable: in_transaction ⇒ tx conn bound *)
     | Some conn -> f conn)
  else
    Io_lwt.Db.with_transaction_low store
      (function None -> none () | Some conn -> f conn)

(** SQL fragment: row is visible to $N (author match OR seed type OR
    member space). Seed membership derives from the authoritative SQL
    predicate tree_is_seed_type (migrations/016_seed_single_source.sql)
    rather than a duplicated IN-list; space membership derives from
    tree_path_in_member_space (migrations/029_data_spaces.sql) against
    the view's exposed path column, so CRUD reads never drift from the
    RLS floor. Per spec [branching.shared-data-spaces]: space visibility
    checks the CALLER's membership (pp.user / session user), never the
    authoring function's — author-runs-as-author does not apply to the
    space compartment. *)
let visible_sql ~(user_param : int) : string =
  Printf.sprintf
    "(author = $%d OR tree_is_seed_type(type_name) OR tree_path_in_member_space(path, $%d))"
    user_param user_param

(** If [type_name] is declared on the store's branch, validate [fields_json]
    against its field list. Undeclared types are free-form (no schema).
    Returns None on ok, Some error_message on reject. *)
let check_type_fields (store : Store.t) ~(type_name : string)
    ~(fields_json : string) : string option =
  let declared = Store.lookup_type_fields store ~name:type_name in
  match declared with
  | None -> None
  | Some fields ->
      (match Tree_store.validate_fields ~declared:fields fields_json with
       | Ok () -> None
       | Error msg ->
           Some
             (Printf.sprintf "create/update %s: %s" type_name msg))

let value_of_id cell : value =
  (* Pgx.Value.to_string returns string option; normalize to VStr/VNil *)
  match Pgx.Value.to_string cell with
  | Some s -> VStr s
  | None -> VNil

(** Safe ORDER BY parser for the [sort] argument of (rows …) / (find-rows …).

    i-1788669685: [sort] used to be appended to the SQL verbatim
    (" ORDER BY " ^ s), and PostgreSQL accepts arbitrary expressions in
    ORDER BY — a blind/error-based read oracle over every table the image
    role can read (credentials, auth_sessions, …; RLS floors only datum
    trees). Live call-site audit (2026-09-10, 37 find-rows + rows callers
    via find-code/function-source): NO caller passes a sort today, so a
    strict grammar is compatible.

    Accepted grammar — 1 or 2 whitespace-separated tokens:
      column            := created_at | updated_at | id
      field accessor    := fields->>'name'   (name: [A-Za-z0-9_-]{1,64})
      direction (opt)   := ASC | DESC       (case-insensitive)
    The SQL fragment is built only from the validated tokens; the raw
    string never reaches the query. Anything else raises a clean prim
    error naming the accepted forms (loud, not a silent default). *)
let safe_sort_sql ~(prim : string) ~(sort : string) : string =
  let bad () =
    raise
      (Interp_error
         (Prim_error
            ( prim,
              "sort must be \"created_at|updated_at|id\" or "
              ^ "\"fields->>'field'\", optionally followed by ASC|DESC",
              "bad-args" )))
  in
  let is_field_char c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    || c = '_' || c = '-'
  in
  let s = String.trim sort in
  let toks = List.filter (fun t -> t <> "") (String.split_on_char ' ' s) in
  let finish token dir =
    let dir_sql =
      match dir with
      | None -> "ASC"
      | Some d ->
          (match String.lowercase_ascii d with
           | "asc" -> "ASC"
           | "desc" -> "DESC"
           | _ -> bad ())
    in
    let expr =
      (match token with
       | "created_at" | "updated_at" | "id" -> token
       | _ -> (
           (* fields->>'name' — exact shape, identifier-strict name. *)
           let prefix = "fields->>'" in
           let plen = String.length prefix in
           let len = String.length token in
           if len >= plen + 2 && String.sub token 0 plen = prefix
              && token.[len - 1] = '\''
           then
             let name = String.sub token plen (len - plen - 1) in
             if name = "" || String.length name > 64
                || not (String.for_all is_field_char name)
             then bad ()
             else Printf.sprintf "fields->>'%s'" name
           else bad ()))
    in
    Printf.sprintf " ORDER BY %s %s" expr dir_sql
  in
  match toks with
  | [ token ] -> finish token None
  | [ token; dir ] -> finish token (Some dir)
  | _ -> bad ()

(** (deftype 'name field1 field2 ...) -> VStr "defined type <name>".
    INSERTs a row into type_defs with branch_id = session's branch and
    fields = JSON array ["field1","field2",...] (the field names in
    declared order). Per spec [storage.types]: type declarations are
    metadata; the actual data lives in datum nodes (tree_nodes, projected
    via tree_data_rows), user-scoped + shared seed (migration 013)
    keyed by (type_name, id). Type declarations may
    eventually drive field-level validation; for now they're documentation.

    The first arg is the type name, then zero or more field names —
    each as a quoted symbol (e.g. (deftype 'user 'name 'email 'age))
    so they arrive as VStr (quote of a bare symbol evaluates to VStr).
    Bare symbols would be looked up as variables and fail. *)
let deftype (store : Store.t) ~(args : value list)
    ~(author : string) : value =
  let field_strs =
    List.filter_map
      (fun v -> match v with VStr s -> Some s | _ -> None)
      args
  in
  (match field_strs with
   | [] ->
       raise (Pricklypear_lisp.Ast.Interp_error
                (Pricklypear_lisp.Ast.Prim_error
                   ("deftype", "need at least a type name", "bad-args")))
   | name :: fields ->
       let _ = Store.persist_type store ~name ~fields ~author in
       (VStr (Printf.sprintf "defined type %s" name)))

(** (get 'type id) -> VStr fields-json | VNil.
    Reads ONE row by (type_name, id). The id is a UUID string.
    Returns the row's `fields` JSON as a VStr so Lisp can pass it
    onward; or VNil if not found. *)
let get (store : Store.t) ~(type_name : string) ~(id : string) : value =
  (* Route through with_transaction so set_local_pp_user runs and the RLS
     floor (tree_data_rows / tree_nodes policy, migration 013) is active.
     The callback conn is the tx conn (set pp.user is LOCAL to it). *)
  Io_lwt.Db.with_transaction store (function
    | None -> VNil
    | Some conn ->
        let user = session_user () in
        let sql =
          "SELECT fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND id = $2::uuid AND " ^
          visible_sql ~user_param:3 in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name;
                      Pgx.Value.of_string id;
                      Pgx.Value.of_string user ]
            sql
        in
        (match rows with
         | [] -> VNil
         | row :: _ ->
             (match row with
              | [ fields_cell ] ->
                  (match Pgx.Value.to_string fields_cell with
                   | Some s -> (VStr s)
                   | None -> VNil)
              | _ -> VNil)))

(** (select-for-update 'type id) -> VStr fields-json | VNil.
    Reads ONE row by (type_name, id) AND locks it with SELECT ... FOR
    UPDATE for the duration of the surrounding transaction.

    Per spec [storage.transactions]: "Explicit row locks:
    (select-for-update 'type id) Exposed as a primitive for finer
    control."

    MUST be called inside (with-transaction ...). Outside a dynamic
    transaction this raises a clean Interp_error (User_error) instead of
    issuing FOR UPDATE on the pool conn — which previously could poison
    Eio mutexes and cascade into auth failures (observed 2026-07-22). *)
let select_for_update
    (store : Store.t) ~(type_name : string) ~(id : string) : value =
  if not (Io_lwt.Db.in_transaction store) then
    raise
      (Interp_error
         (User_error
            ("select-for-update requires (with-transaction ...); \
             refusing FOR UPDATE outside a transaction", "bad-args")))
  else
    match Io_lwt.Db.current_conn store with
    | None -> VNil
    | Some conn ->
        let user = session_user () in
        let sql =
          "SELECT fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND id = $2::uuid AND " ^
          visible_sql ~user_param:3 ^ " " ^
          "FOR UPDATE" in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name;
                      Pgx.Value.of_string id;
                      Pgx.Value.of_string user ]
            sql
        in
        (match rows with
         | [] -> VNil
         | row :: _ ->
             (match row with
              | [ fields_cell ] ->
                  (match Pgx.Value.to_string fields_cell with
                   | Some s -> (VStr s)
                   | None -> VNil)
              | _ -> VNil))

(** (create 'type "{fields-json}") -> VStr new-id; raises Prim_error
    ("create", "failed", "failed") when the insert is rejected.
    INSERTs a row with the given JSON fields, returning the new UUID.
    The author is the session's current username.

    Why take fields as a JSON STRING (not a Lisp value):
    - The Lisp layer doesn't yet have a dict/assoc value type.
    - Caller can build the JSON via (string-append ...) or a future
      (json ...) prim; for now both caller and DB speak JSON strings.
    - The DB validates it's well-formed JSON (jsonb column). *)
(* ---- shared data spaces (branching.shared-data-spaces) ----

   S1 (migrations/029_data_spaces.sql) built the storage floor: the
   data.s.<space>.<type>.<id> path compartment, membership tables, the
   tree_datum_path routing rule (admitted type -> space), and the
   member-space disjunct in tree_data_rows + RLS. S2 (this block) is the
   PRIM surface: space management fns, the non-member write guard, and
   node-path parity. Caller-membership is the check — author-runs-as-
   author does NOT apply to the space compartment (the caller's identity
   is threaded explicitly; the author field stays attribution). *)

let space_prim_error prim msg =
  raise
    (Pricklypear_lisp.Ast.Interp_error
       (Pricklypear_lisp.Ast.User_error
          (Printf.sprintf "%s: %s" prim msg, "denied")))

(** Type admission: the space [type_name] writes route to, if any. *)
let space_of_type (conn : Store_db.conn) ~(type_name : string) : string option =
  match
    Store_db.exec conn ~params:[ Pgx.Value.of_string type_name ]
      "SELECT space FROM data_space_types WHERE type_name = $1"
  with
  | [ [ c ] ] -> Pgx.Value.to_string c
  | _ -> None

(** Membership mirror of the SQL tree_path_in_member_space
    (migrations/029 + 030): explicit member rows, or ANY authenticated
    identity for an open space ('anonymous' explicitly excluded — keep
    the two mirrors' anonymous handling identical). *)
let is_space_member (conn : Store_db.conn) ~(space : string)
    ~(username : string) : bool =
  match
    Store_db.exec conn
      ~params:[ Pgx.Value.of_string space; Pgx.Value.of_string username ]
      "SELECT (EXISTS (SELECT 1 FROM data_space_members \
                WHERE space = $1 AND username = $2) \
              OR ($2 <> '' AND $2 <> 'anonymous' \
                  AND EXISTS (SELECT 1 FROM data_spaces \
                              WHERE name = $1 AND open)))  "
  with
  | [ [ c ] ] -> Pgx.Value.to_bool c |> Option.value ~default:false
  | _ -> false

(** Space of the space row bearing datum [id] (the row may live in a space
    whose type has since been un-admitted — the PATH, not current
    admission, is what the row owes membership checks to). *)
let space_of_datum (conn : Store_db.conn) ~(id : string) : string option =
  match
    Store_db.exec conn ~params:[ Pgx.Value.of_string id ]
      "SELECT tree_dec_label(subltree(path, 2, 3)::text) FROM tree_nodes \
       WHERE kind = 'datum' AND path <@ 'data.s'::ltree \
       AND tree_datum_id(path) = $1::uuid"
  with
  | [ [ c ] ] -> Pgx.Value.to_string c
  | _ -> None

(** The S2 non-member write guard (spec: "a removed member's subsequent
    create errors with a membership error, not a silent per-user write").
    Two checks: (1) writing a space-ADMITTED type requires membership;
    (2) touching an existing space ROW requires membership in the row's
    space (survives un-admit; covers update/delete-by-id). The RLS floor
    (WITH CHECK) is the belt; this is the descriptive error. Callers that
    do not thread an identity get [~caller] defaulted to "unknown"
    (optional arg so internal stragglers keep compiling): touching an
    admitted type or a space row then errors LOUDLY, which is the
    intended straggler behavior — thread the real identity to pass. *)
let guard_space_write (conn : Store_db.conn) ~(prim : string)
    ~(type_name : string) ~(caller : string) ~(id : string option) : unit =
  (match space_of_type conn ~type_name with
   | Some sp when not (is_space_member conn ~space:sp ~username:caller) ->
       space_prim_error prim
         (Printf.sprintf "type %s is admitted to space %s; %s is not a member"
            type_name sp caller)
   | _ -> ());
  match id with
  | None -> ()
  | Some id ->
      (match space_of_datum conn ~id with
       | Some sp when not (is_space_member conn ~space:sp ~username:caller) ->
           space_prim_error prim
             (Printf.sprintf "datum lives in space %s; %s is not a member"
                sp caller)
       | _ -> ())

(** {1 Space management} (the S2 prim bodies; prims_db threads the
    caller + kernel-admin bit). Gates: any non-anon identity may create;
    member management + type admission require the space owner (owner
    column or role='owner' member) or kernel admin; listing is membership-
    scoped (kernel admin sees all). *)

let space_exists (conn : Store_db.conn) ~(space : string) : bool =
  match
    Store_db.exec conn ~params:[ Pgx.Value.of_string space ]
      "SELECT EXISTS (SELECT 1 FROM data_spaces WHERE name = $1)  "
  with
  | [ [ c ] ] -> Pgx.Value.to_bool c |> Option.value ~default:false
  | _ -> false

let space_owner_ok (conn : Store_db.conn) ~(space : string)
    ~(caller : string) ~(kernel_admin : bool) : bool =
  if kernel_admin then true
  else
    match
      Store_db.exec conn
        ~params:[ Pgx.Value.of_string space; Pgx.Value.of_string caller ]
        "SELECT EXISTS (SELECT 1 FROM data_spaces WHERE name = $1 AND owner = $2 \
         UNION SELECT 1 FROM data_space_members \
         WHERE space = $1 AND username = $2 AND role = 'owner')  "
    with
    | [ [ c ] ] -> Pgx.Value.to_bool c |> Option.value ~default:false
    | _ -> false

(** The space-exists? prim body (lazy ensure paths in Nopales packages
    create-or-skip without parsing the duplicate-creation error). *)
let space_exists_prim (store : Store.t) ~(name : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VBool false
  | Some conn -> VBool (space_exists conn ~space:name)

let validate_space_name name =
  (* Space names become single ltree labels; the encoder is the validator. *)
  try ignore (Tree_store.enc_label name) with
  | Invalid_argument msg -> space_prim_error "space-create" msg

let space_create (store : Store.t) ~(caller : string) ~(name : string)
    ?(open_ : bool = false) () : value =
  validate_space_name name;
  match Io_lwt.Db.current_conn store with
  | None -> space_prim_error "space-create" "failed: no DB"
  | Some conn ->
      if space_exists conn ~space:name then
        space_prim_error "space-create"
          (Printf.sprintf "space %s already exists" name);
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string name; Pgx.Value.of_string caller;
                    Pgx.Value.of_bool open_ ]
          "INSERT INTO data_spaces (name, owner, open) \
           VALUES ($1, $2, $3) RETURNING name"
      in
      (match rows with
       | [ [ _ ] ] ->
           ignore
             (Store_db.exec conn
                ~params:[ Pgx.Value.of_string name;
                          Pgx.Value.of_string caller ]
                "INSERT INTO data_space_members (space, username, role) \
                 VALUES ($1, $2, 'owner') ON CONFLICT DO NOTHING");
           VStr
             (Printf.sprintf "created %sspace %s (owner %s)"
                (if open_ then "open " else "") name caller)
       | _ -> space_prim_error "space-create" "failed")

let space_add_member (store : Store.t) ~(caller : string)
    ~(kernel_admin : bool) ~(space : string) ~(username : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> space_prim_error "space-add-member" "failed: no DB"
  | Some conn ->
      if not (space_exists conn ~space) then
        space_prim_error "space-add-member"
          (Printf.sprintf "no such space %s" space);
      if not (space_owner_ok conn ~space ~caller ~kernel_admin) then
        space_prim_error "space-add-member"
          (Printf.sprintf "%s is not an owner of space %s" caller space);
      ignore
        (Store_db.exec conn
           ~params:[ Pgx.Value.of_string space;
                     Pgx.Value.of_string username ]
           "INSERT INTO data_space_members (space, username) \
            VALUES ($1, $2) ON CONFLICT DO NOTHING");
      VStr (Printf.sprintf "%s added to space %s" username space)

let space_remove_member (store : Store.t) ~(caller : string)
    ~(kernel_admin : bool) ~(space : string) ~(username : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> space_prim_error "space-remove-member" "failed: no DB"
  | Some conn ->
      if not (space_owner_ok conn ~space ~caller ~kernel_admin) then
        space_prim_error "space-remove-member"
          (Printf.sprintf "%s is not an owner of space %s" caller space);
      ignore
        (Store_db.exec conn
           ~params:[ Pgx.Value.of_string space;
                     Pgx.Value.of_string username ]
           "DELETE FROM data_space_members WHERE space = $1 AND username = $2");
      VStr (Printf.sprintf "%s removed from space %s" username space)

let space_admit (store : Store.t) ~(caller : string) ~(kernel_admin : bool)
    ~(space : string) ~(type_name : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> space_prim_error "space-admit" "failed: no DB"
  | Some conn ->
      if not (space_exists conn ~space) then
        space_prim_error "space-admit" (Printf.sprintf "no such space %s" space);
      if not (space_owner_ok conn ~space ~caller ~kernel_admin) then
        space_prim_error "space-admit"
          (Printf.sprintf "%s is not an owner of space %s" caller space);
      ignore
        (Store_db.exec conn
           ~params:[ Pgx.Value.of_string type_name;
                     Pgx.Value.of_string space;
                     Pgx.Value.of_string caller ]
           "INSERT INTO data_space_types (type_name, space, admitted_by) \
            VALUES ($1, $2, $3) \
            ON CONFLICT (type_name) DO UPDATE \
            SET space = EXCLUDED.space, admitted_by = EXCLUDED.admitted_by");
      VStr (Printf.sprintf "type %s admitted to space %s" type_name space)

let space_list (store : Store.t) ~(caller : string) ~(kernel_admin : bool)
    : value =
  match Io_lwt.Db.current_conn store with
  | None -> VList []
  | Some conn ->
      let sql, params =
        if kernel_admin then
          ("SELECT name FROM data_spaces ORDER BY name", [])
        else
          ( "SELECT space FROM ( \
               SELECT space FROM data_space_members WHERE username = $1 \
               UNION \
               SELECT name FROM data_spaces WHERE open \
                 AND $1 <> 'anonymous') t \
             ORDER BY space"
          , [ Pgx.Value.of_string caller ] )
      in
      let rows = Store_db.exec conn ~params sql in
      VList
        (List.filter_map
           (fun row ->
             match row with
             | [ c ] -> (
                 match Pgx.Value.to_string c with
                 | Some s -> Some (VStr s)
                 | None -> None)
             | _ -> None)
           rows)

let space_members (store : Store.t) ~(caller : string)
    ~(kernel_admin : bool) ~(space : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VList []
  | Some conn ->
      (if (not kernel_admin) && not (is_space_member conn ~space ~username:caller)
       then
         space_prim_error "space-members"
           (Printf.sprintf "%s is not a member of space %s" caller space));
      let rows =
        Store_db.exec conn ~params:[ Pgx.Value.of_string space ]
          "SELECT username || ' (' || role || ')' FROM data_space_members \
           WHERE space = $1 ORDER BY username"
      in
      VList
        (List.filter_map
           (fun row ->
             match row with
             | [ c ] -> (
                 match Pgx.Value.to_string c with
                 | Some s -> Some (VStr s)
                 | None -> None)
             | _ -> None)
           rows)

(** {1 Data history} (branching.shared-data-spaces S3: the log consumer.)

    (data-history 'type id) -> VStr JSON list, NEWEST-FIRST:
      [{gen, author, created_at, delete, body}]
    Storage was already free: tree-time's tree_versions (018-021)
    versions kind=datum on every destructive write, path-keyed, so any
    compartment's rows get before-images per write. This is only the
    consumer: resolve the row's CURRENT path through the visibility-
    confined view (~viewer mirrors ~caller — the space compartment checks
    the caller's membership, not the authoring function's), then read
    Tree_store.versioned_history for that path.
    LIVE ROWS ONLY in v1: a deleted row's path is no longer resolvable
    through the view, so its history (incl. the tombstone version) is
    unreachable from this prim until a history-by-id surface lands. *)

let data_history_json (store : Store.t) ~(viewer : string)
    ~(type_name : string) ~(id : string) : Yojson.Safe.t list =
  match Io_lwt.Db.current_conn store with
  | None -> []
  | Some conn ->
      let path_opt =
        let sql =
          "SELECT path::text FROM " ^ data_rel () ^
          " WHERE type_name = $1 AND id = $2::uuid AND " ^
          visible_sql ~user_param:3 in
        (match
           Store_db.exec conn
             ~params:[ Pgx.Value.of_string type_name;
                       Pgx.Value.of_string id;
                       Pgx.Value.of_string viewer ]
             sql
         with
         | [ [ p ] ] -> Pgx.Value.to_string p
         | _ -> None)
      in
      (match path_opt with
       | None -> []
       | Some path ->
           let vs = Tree_store.versioned_history conn ~path () in
           (* ASC on gen -> reverse to newest-first *)
           List.rev_map
             (fun (v : Tree_store.version) ->
               let body =
                 match v.Tree_store.v_body with
                 | `Assoc a ->
                     (match List.assoc_opt "body" a with
                      | Some b -> b
                      | None -> `Null)
                 | _ -> `Null
               in
               `Assoc
                 [ ("gen",
                    (match v.Tree_store.v_gen with
                     | Some g -> `Intlit (Int64.to_string g)
                     | None -> `Null))
                 ; ("author",
                    (match v.Tree_store.v_author with
                     | Some a -> `String a
                     | None -> `Null))
                 ; ("created_at",
                    (match v.Tree_store.v_created_at with
                     | Some t -> `String t
                     | None -> `Null))
                 ; ("delete", `Bool v.Tree_store.v_tombstone)
                 ; ("body", body)
                 ])
             vs)

let data_history (store : Store.t) ~(viewer : string)
    ~(type_name : string) ~(id : string) : value =
  match data_history_json store ~viewer ~type_name ~id with
  | [] -> VNil
  | xs -> VStr (Yojson.Safe.to_string (`List xs))

(** Uniform no-DB contract for WRITE prims (i-1788670475): a write with
    no DB connection raises Interp_error — it must never masquerade as an
    empty result (VNil = "no row matched"), a benign error string, or a
    fabricated success shape (tree/delete used to emit
    {"deleted":0,...}). Read prims keep their empty defaults
    (DB-down-as-empty is the separate swallow-and-default class).
    Same (prim, message, kind) shape create already raised. *)
let no_db (prim : string) : 'a =
  raise (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.Prim_error (prim, "failed: no DB", "failed")))

(** Typed write outcomes for OCaml callers (HTTP edge, internal writers)
    — i-1788670909: the prims keep their VStr value contract, but callers
    must never sniff shapes (a 36-char rejection string is not a uuid;
    a validation rejection is not a missing row). The value-returning
    create/update below wrap these. *)
type datum_write =
  | W_ok of string        (* success: the id echo *)
  | W_rejected of string  (* typed-boundary validation rejection *)
  | W_no_row              (* update matched no row *)

let create_outcome
    (store : Store.t) ~(type_name : string) ~(fields_json : string)
    ~(author : string) : datum_write =
  let reject = check_type_fields store ~type_name ~fields_json in
  match reject with
  | Some msg -> (W_rejected msg)
  | None ->
  with_tree_tx store ~none:(fun () -> no_db "create") (fun conn ->
    guard_space_write conn ~prim:"create" ~type_name ~caller:author ~id:None;
    (* tree is truth; reverse trigger mirrors to data_rows *)
    let id = Tree_store.insert_datum conn ~type_name ~fields_json ~author in
    (match id with
     | Ok s -> W_ok s
     | Error _ ->
         raise (Pricklypear_lisp.Ast.Interp_error
                  (Pricklypear_lisp.Ast.Prim_error
                     ("create", "failed", "failed")))))

let create
    (store : Store.t) ~(type_name : string) ~(fields_json : string)
    ~(author : string)
    : value =
  match create_outcome store ~type_name ~fields_json ~author with
  | W_ok s -> VStr s
  | W_rejected msg -> VStr msg
  | W_no_row -> VNil (* unreachable for create; shape parity with update *)

(** (update 'type id "{fields-json}") -> VStr id | VNil.
    UPDATE fields for ONE row by (type_name, id). Returns the id
    (echo of the row updated) on success, or VNil if no row matched. *)
(* Identity-driven surfaces (prims, /data browser) call the _guarded
   variants, threading the session identity; internal bookkeeping
   writers keep the master signatures, which route through the guard
   with caller "unknown" (an admitted-type update by an unthreaded
   internal caller errors loudly — the intended straggler behavior). *)
let update_outcome
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(fields_json : string) ~(caller : string) : datum_write =
  let reject = check_type_fields store ~type_name ~fields_json in
  match reject with
  | Some msg -> (W_rejected msg)
  | None ->
  with_tree_tx store ~none:(fun () -> no_db "update") (fun conn ->
    guard_space_write conn ~prim:"update" ~type_name ~caller
      ~id:(Some id);
    let id' = Tree_store.update_datum conn ~type_name ~id ~fields_json in
    (match id' with Ok s -> W_ok s | Error _ -> W_no_row))

let update_impl
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(fields_json : string) ~(caller : string)
    : value =
  match update_outcome store ~type_name ~id ~fields_json ~caller with
  | W_ok s -> VStr s
  | W_rejected msg -> VStr msg
  | W_no_row -> VNil

let update store ~type_name ~id ~fields_json =
  update_impl store ~type_name ~id ~fields_json ~caller:"unknown"

let update_guarded store ~type_name ~id ~fields_json ~caller =
  update_impl store ~type_name ~id ~fields_json ~caller

(** (update-field 'type id "field" "value") -> VStr id | VNil.
    Ergonomic shorthand for the v1 dict-set + update round-trip:
      (define u (get 'type id))
      (update 'type id (dict-set u "field" "value"))
    -> (update-field 'type id "field" "value")
    Composition is pure-OCaml (one get, one dict-set, one update inside
    this prim). Lwt-typed; returns VStr id on success, VNil if no row
    matched or no DB. Value is JSON-coerced (numeric strings emit as JSON
    numbers; non-parseable values emit as JSON strings — same rules as
    dict-set).

    Runs in ONE transaction: the row is read with SELECT ... FOR UPDATE,
    the field merged in OCaml, and the UPDATE issued in the same tx. The
    FOR UPDATE lock serializes concurrent update-field calls on the same
    row, so two writers updating different fields both land (no
    lost-update race). The tx conn (with pp.user LOCAL set) is used
    throughout, so the RLS floor stays active. *)
let update_field_impl
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(field : string) ~(value : string) ~(caller : string)
    : value =
  (* Preserve type-field validation (update_field used to route through
     update -> check_type_fields). Build a throwaway single-field JSON to
     reuse the same rejection path. *)
  let sample_json = Printf.sprintf "{\"%s\":\"%s\"}" field value in
  let reject = check_type_fields store ~type_name ~fields_json:sample_json in
  match reject with
  | Some msg -> VStr msg
  | None ->
  Io_lwt.Db.with_transaction store (function
    | None -> no_db "update-field"
    | Some conn ->
        guard_space_write conn ~prim:"update-field" ~type_name ~caller
          ~id:(Some id);
        let user = session_user () in
        let sql =
          "SELECT fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND id = $2::uuid AND " ^
          visible_sql ~user_param:3 ^ " FOR UPDATE" in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name;
                      Pgx.Value.of_string id;
                      Pgx.Value.of_string user ]
            sql
        in
        (match rows with
         | [] -> VNil  (* no such row / not visible *)
         | row :: _ ->
             let json_str =
               match row with
               | [ fields_cell ] ->
                   (match Pgx.Value.to_string fields_cell with
                    | Some s -> s
                    | None -> "{}")
               | _ -> "{}"
             in
             (* Compose with the SAME logic as Primitives.prim_dict_set so
                numeric-auto-detect and JSON passthrough behave identically. *)
             let base =
               try
                 (match Yojson.Safe.from_string json_str with
                  | `Assoc kvs -> kvs
                  | _ -> [])
               with _ -> []
             in
             let v_json =
               try Yojson.Safe.from_string value
               with _ -> `String value
             in
             let updated =
               (field, v_json) ::
               (List.filter (fun (k, _) -> k <> field) base)
             in
             let updated_json = Yojson.Safe.to_string (`Assoc updated) in
             let id' =
               Tree_store.update_datum conn ~type_name ~id ~fields_json:updated_json
             in
             (match id' with Ok s -> VStr s | Error _ -> VNil)))

let update_field store ~type_name ~id ~field ~value =
  update_field_impl store ~type_name ~id ~field ~value ~caller:"unknown"

let update_field_guarded store ~type_name ~id ~field ~value ~caller =
  update_field_impl store ~type_name ~id ~field ~value ~caller

(** (delete 'type id) -> VStr id | VNil.
    DELETEs one row by (type_name, id). Returns the id on success
    (echo), VNil if no row matched. *)
let delete_impl (store : Store.t) ~(type_name : string) ~(id : string)
    ~(caller : string) : value =
  with_tree_tx store ~none:(fun () -> no_db "delete") (fun conn ->
    (* Closes the delete-by-id hole at the space boundary (loud error;
       the RLS floor armed by this tx rejects cross-user deletes). *)
    guard_space_write conn ~prim:"delete" ~type_name ~caller ~id:(Some id);
    let id' = Tree_store.delete_datum conn ~type_name ~id in
    (match id' with Ok s -> VStr s | Error _ -> VNil))

let delete store ~type_name ~id =
  delete_impl store ~type_name ~id ~caller:"unknown"

let delete_guarded store ~type_name ~id ~caller =
  delete_impl store ~type_name ~id ~caller

(** (list 'type [limit] [sort]) -> VList of (VStr id, VStr fields) pairs.
    With no limit, defaults to 100 (avoid OOM on a huge table). Optional
    sort is parsed by safe_sort_sql (columns created_at|updated_at|id or
    fields->>'field', optional ASC|DESC); default ORDER BY is created_at
    ASC. Each row is a 2-element VList `[id fields]` so callers can
    `(map (lambda (row) (car row)) rows)` to get ids, etc. *)
let list
    (store : Store.t) ~(type_name : string) ?(limit : int = 100)
    ?(sort : string option = None) ()
    : value =
  Io_lwt.Db.with_transaction store (function
    | None -> (VList [])
    | Some conn ->
        let user = session_user () in
        let order_by =
          match sort with
          | Some s -> safe_sort_sql ~prim:"rows" ~sort:s
          | None -> " ORDER BY created_at ASC"
        in
        let sql =
          "SELECT id::text, fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND " ^ visible_sql ~user_param:3 ^
          order_by ^ " LIMIT $2" in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name;
                      Pgx.Value.of_int limit;
                      Pgx.Value.of_string user ]
            sql
        in
        let lisp_rows =
          List.map
            (fun row ->
              match row with
              | [ id_cell; fields_cell ] ->
                  VPair (value_of_id id_cell, value_of_id fields_cell)
              | _ -> VNil)
            rows
        in
        (VList lisp_rows))

(** (find-row 'type "field" "value") -> VStr fields-json | VNil.
    First row where fields->>field = value (text equality). *)
let find_row
    ?(latest : bool = false)
    (store : Store.t) ~(type_name : string) ~(field : string) ~(value : string)
    : value =
  Io_lwt.Db.with_transaction store (function
    | None -> VNil
    | Some conn ->
        let user = session_user () in
        let sql =
          "SELECT fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND fields->>$2 = $3 AND " ^
          visible_sql ~user_param:4 ^ " " ^
          (* latest: newest row wins (function_stat "last run" widget).
             Default stays oldest-first (first-row-wins) for existing
             callers (issues register etc.). *)
          (if latest then "ORDER BY created_at DESC LIMIT 1"
           else "ORDER BY created_at ASC LIMIT 1")
        in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name
                    ; Pgx.Value.of_string field
                    ; Pgx.Value.of_string value
                    ; Pgx.Value.of_string user ]
            sql
        in
        (match rows with
         | [] -> VNil
         | row :: _ ->
             (match row with
              | [ fields_cell ] -> (value_of_id fields_cell)
              | _ -> VNil)))

(** (find-rows 'type "f1" "v1" "f2" "v2" ... [limit] [sort])
    -> VList of (id . fields) pairs.
    All field filters AND together (fields->>k = v text). Optional
    trailing VInt limit (default 500) and an optional trailing VStr sort
    (parsed by safe_sort_sql: created_at|updated_at|id or fields->>'field',
    optional ASC|DESC — never raw SQL). Default ORDER BY: created_at ASC.
    Callers needing domain-specific ordering (e.g. verses by number) pass
    e.g. "fields->>'verse' ASC" explicitly. *)
let find_rows
    (store : Store.t) ~(type_name : string)
    ~(filters : (string * string) list) ?(limit : int = 500)
    ?(sort : string option = None) ()
    : value =
  Io_lwt.Db.with_transaction store (function
    | None -> (VList [])
    | Some conn ->
        if filters = [] then list store ~type_name ~limit ()
        else
          let user = session_user () in
          let buf = Buffer.create 128 in
          Buffer.add_string buf
            ("SELECT id::text, fields::text FROM " ^ data_rel () ^ " WHERE type_name = $1");
          let params = ref [ Pgx.Value.of_string type_name ] in
          let i = ref 2 in
          List.iter
            (fun (field, value) ->
              Buffer.add_string buf
                (Printf.sprintf " AND fields->>$%d = $%d" !i (!i + 1));
              params := !params @ [ Pgx.Value.of_string field
                                 ; Pgx.Value.of_string value ];
              i := !i + 2)
            filters;
          Buffer.add_string buf
            (Printf.sprintf " AND %s" (visible_sql ~user_param:!i));
          params := !params @ [ Pgx.Value.of_string user ];
          i := !i + 1;
            let order_by =
              match sort with
              | Some s -> safe_sort_sql ~prim:"find-rows" ~sort:s
              | None -> " ORDER BY created_at ASC"
            in
          Buffer.add_string buf (Printf.sprintf "%s LIMIT $%d" order_by !i);
          params := !params @ [ Pgx.Value.of_int limit ];
          let rows = Store_db.exec conn ~params:!params (Buffer.contents buf) in
          let lisp_rows =
            List.map
              (fun row ->
                match row with
                | [ id_cell; fields_cell ] ->
                    VPair (value_of_id id_cell, value_of_id fields_cell)
                | _ -> VNil)
              rows
          in
          (VList lisp_rows))

(** (rows-author 'type [limit] [sort]) -> VList of [id; author; fields]
    triples. Same RLS visibility and ordering as [list], but each row also
    carries its data_rows.author, so callers can enforce identity policy
    (e.g. glochid call ACL) against the REAL writer instead of a
    caller-written "from" field. Additive: plain [rows]/[find-rows] shapes
    are unchanged. *)
let list_with_author
    (store : Store.t) ~(type_name : string) ?(limit : int = 100)
    ?(sort : string option = None) ()
    : value =
  Io_lwt.Db.with_transaction store (function
    | None -> (VList [])
    | Some conn ->
        let user = session_user () in
        (* Merge-integration: rows-author postdates i-1788669685 but kept
           the raw concat this file's other readers route through
           safe_sort_sql — same strict grammar, no raw SQL. *)
        let order_by =
          match sort with
          | Some s -> safe_sort_sql ~prim:"rows-author" ~sort:s
          | None -> " ORDER BY created_at ASC"
        in
        let sql =
          "SELECT id::text, author, fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND " ^ visible_sql ~user_param:3 ^
          order_by ^ " LIMIT $2" in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name;
                      Pgx.Value.of_int limit;
                      Pgx.Value.of_string user ]
            sql
        in
        let lisp_rows =
          List.map
            (fun row ->
              match row with
              | [ id_cell; author_cell; fields_cell ] ->
                  VList [ value_of_id id_cell;
                          value_of_id author_cell;
                          value_of_id fields_cell ]
              | _ -> VNil)
            rows
        in
        (VList lisp_rows))

(** (find-rows-author 'type "f1" "v1" ... [limit] [sort]) -> VList of
    [id; author; fields] triples. [find_rows] with the author column
    surfaced; see [list_with_author]. *)
let find_rows_with_author
    (store : Store.t) ~(type_name : string)
    ~(filters : (string * string) list) ?(limit : int = 500)
    ?(sort : string option = None) ()
    : value =
  Io_lwt.Db.with_transaction store (function
    | None -> (VList [])
    | Some conn ->
        if filters = [] then list_with_author store ~type_name ~limit ()
        else
          let user = session_user () in
          let buf = Buffer.create 128 in
          Buffer.add_string buf
            ("SELECT id::text, author, fields::text FROM " ^ data_rel () ^
             " WHERE type_name = $1");
          let params = ref [ Pgx.Value.of_string type_name ] in
          let i = ref 2 in
          List.iter
            (fun (field, value) ->
              Buffer.add_string buf
                (Printf.sprintf " AND fields->>$%d = $%d" !i (!i + 1));
              params := !params @ [ Pgx.Value.of_string field
                                  ; Pgx.Value.of_string value ];
              i := !i + 2)
            filters;
          Buffer.add_string buf
            (Printf.sprintf " AND %s" (visible_sql ~user_param:!i));
          params := !params @ [ Pgx.Value.of_string user ];
          i := !i + 1;
          let order_by =
            match sort with
            | Some s -> safe_sort_sql ~prim:"find-rows-author" ~sort:s
            | None -> " ORDER BY created_at ASC"
          in
          Buffer.add_string buf (Printf.sprintf "%s LIMIT $%d" order_by !i);
          params := !params @ [ Pgx.Value.of_int limit ];
          let rows = Store_db.exec conn ~params:!params (Buffer.contents buf) in
          let lisp_rows =
            List.map
              (fun row ->
                match row with
                | [ id_cell; author_cell; fields_cell ] ->
                    VList [ value_of_id id_cell;
                            value_of_id author_cell;
                            value_of_id fields_cell ]
                | _ -> VNil)
              rows
          in
          (VList lisp_rows))

(** (find-random 'type) -> (id . fields) pair | VNil.
    One random row of the given type (for bible/random etc.). *)
let find_random (store : Store.t) ~(type_name : string) : value =
  Io_lwt.Db.with_transaction store (function
    | None -> VNil
    | Some conn ->
        let user = session_user () in
        let sql =
          "SELECT id::text, fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND " ^ visible_sql ~user_param:2 ^ " " ^
          "ORDER BY random() LIMIT 1"
        in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name;
                      Pgx.Value.of_string user ]
            sql
        in
        (match rows with
         | [] -> VNil
         | row :: _ ->
             (match row with
              | [ id_cell; fields_cell ] ->
                  (VPair (value_of_id id_cell, value_of_id fields_cell))
              | _ -> VNil)))

(** (find-text 'type "field" "needle" [limit]) -> VList of (id . fields).
    Case-insensitive substring match on fields->>field. *)
let find_text
    (store : Store.t) ~(type_name : string) ~(field : string)
    ~(needle : string) ?(limit : int = 50) ()
    : value =
  Io_lwt.Db.with_transaction store (function
    | None -> (VList [])
    | Some conn ->
        let user = session_user () in
        let sql =
          "SELECT id::text, fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND fields->>$2 ILIKE $3 AND " ^
          visible_sql ~user_param:5 ^ " " ^
          "ORDER BY created_at ASC LIMIT $4"
        in
        let pat = "%" ^ needle ^ "%" in
        let rows =
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string type_name
                    ; Pgx.Value.of_string field
                    ; Pgx.Value.of_string pat
                    ; Pgx.Value.of_int limit
                    ; Pgx.Value.of_string user ]
            sql
        in
        let lisp_rows =
          List.map
            (fun row ->
              match row with
              | [ id_cell; fields_cell ] ->
                  VPair (value_of_id id_cell, value_of_id fields_cell)
              | _ -> VNil)
            rows
        in
        (VList lisp_rows))

(* ---- OCaml UI helpers (image.data-browser) — not Lisp prims ---- *)

type row_record = {
  id : string;
  fields_json : string;
  author : string option;
  created_at : string option;
  updated_at : string option;
}

let count_by_type (store : Store.t) ~(type_name : string) : int =
  match Io_lwt.Db.current_conn store with
  | None -> 0
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT COUNT(*)::int FROM " ^ data_rel () ^
        " WHERE type_name = $1 AND " ^ visible_sql ~user_param:2
      in
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string user ]
          sql
      in
      (match rows with
       | [ cell :: _ ] ->
           (match Pgx.Value.to_int cell with
            | Some n -> n
            | None -> 0)
       | _ -> 0)

(* function_stat telemetry (migration 031): the last measured run for
   [fn], newest first — the /binding "last run" widget. Plain table, no
   user scoping (server-wide perf telemetry; author is recorded). *)
let function_stat_last (store : Store.t) ~(fn : string) : string option =
  match Io_lwt.Db.current_conn store with
  | None -> None
  | Some conn ->
      let sql =
        "SELECT fields::text FROM function_stat " ^
        "WHERE fn = $1 ORDER BY created_at DESC LIMIT 1"
      in
      (try
         (match
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string fn ]
              sql
          with
          | [ [ fields ] ] -> Pgx.Value.to_string fields
          | _ -> None)
       with _ -> None)

(* ONE grouped count over the datum view instead of count_by_type per
   type (log/issues "/data 49s"): the index page used to issue one
   sequential-scan COUNT per type (~40 of them, each with the ltree
   visibility predicates); one GROUP BY scan feeds the whole table. *)
let counts_by_type (store : Store.t) : (string * int) list =
  match Io_lwt.Db.current_conn store with
  | None -> []
 | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT type_name, COUNT(*)::int FROM " ^ data_rel () ^
        " WHERE " ^ visible_sql ~user_param:1 ^
        " GROUP BY type_name ORDER BY type_name ASC"
      in
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string user ]
          sql
      in
      List.filter_map
        (fun row ->
          match row with
          | [ name_cell; count_cell ] ->
              (match Pgx.Value.to_string name_cell, Pgx.Value.to_int count_cell with
               | Some name, Some n -> Some (name, n)
               | _ -> None)
          | _ -> None)
        rows

let list_data_type_names (store : Store.t) : string list =
  match Io_lwt.Db.current_conn store with
  | None -> []
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT DISTINCT type_name FROM " ^ data_rel () ^
        " WHERE " ^ visible_sql ~user_param:1 ^
        " ORDER BY type_name ASC"
      in
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string user ]
          sql
      in
      (List.filter_map
           (fun row ->
             match row with
             | [ cell ] -> Pgx.Value.to_string cell
             | _ -> None)
           rows)

let list_page
    (store : Store.t) ~(type_name : string)
    ?(limit : int = 50) ?(offset : int = 0) ()
    : row_record list =
  match Io_lwt.Db.current_conn store with
  | None -> []
  | Some conn ->
      let lim = max 1 (min limit 200) in
      let off = max 0 offset in
      let user = session_user () in
      let sql =
        "SELECT id::text, fields::text, author, " ^
        "created_at::text, updated_at::text " ^
        "FROM " ^ data_rel () ^ " WHERE type_name = $1 AND " ^
        visible_sql ~user_param:4 ^ " " ^
        "ORDER BY created_at ASC LIMIT $2 OFFSET $3"
      in
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string type_name
                  ; Pgx.Value.of_int lim
                  ; Pgx.Value.of_int off
                  ; Pgx.Value.of_string user ]
          sql
      in
      (List.filter_map
           (fun row ->
             match row with
             | [ id_c; fields_c; author_c; created_c; updated_c ] ->
                 (match Pgx.Value.to_string id_c,
                         Pgx.Value.to_string fields_c with
                  | Some id, Some fields_json ->
                      Some {
                        id;
                        fields_json;
                        author = Pgx.Value.to_string author_c;
                        created_at = Pgx.Value.to_string created_c;
                        updated_at = Pgx.Value.to_string updated_c;
                      }
                  | _ -> None)
             | _ -> None)
           rows)

let get_row
    (store : Store.t) ~(type_name : string) ~(id : string)
    : row_record option =
  match Io_lwt.Db.current_conn store with
  | None -> None
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT id::text, fields::text, author, " ^
        "created_at::text, updated_at::text " ^
        "FROM " ^ data_rel () ^ " WHERE type_name = $1 AND id = $2::uuid AND " ^
        visible_sql ~user_param:3
      in
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string type_name
                  ; Pgx.Value.of_string id
                  ; Pgx.Value.of_string user ]
          sql
      in
      (match rows with
       | [ id_c; fields_c; author_c; created_c; updated_c ] :: _ ->
           (match Pgx.Value.to_string id_c, Pgx.Value.to_string fields_c with
            | Some id, Some fields_json ->
                (Some {
                     id;
                     fields_json;
                     author = Pgx.Value.to_string author_c;
                     created_at = Pgx.Value.to_string created_c;
                     updated_at = Pgx.Value.to_string updated_c;
                   })
            | _ -> None)
       | _ -> None)


(** (node-path 'type id) -> VStr path. Space-aware path constructor:
    an admitted type reports its true data.s.<space>.* home (looked up in
    data_space_types — the pure Tree_store.datum_path mirror cannot see
    dynamic admission), everything else routes exactly as the SQL
    tree_datum_path does (seed -> shared; else user). *)
let node_path (store : Store.t) ~(type_name : string) ~(id : string) : value =
  let user = session_user () in
  (match Io_lwt.Db.current_conn store with
   | Some conn ->
       (match space_of_type conn ~type_name with
        | Some sp ->
            VStr (Tree_store.datum_path ~space:(Some sp) ~type_name ~id ())
        | None -> VStr (Tree_store.datum_path ~user ~type_name ~id ()))
   | None -> VStr (Tree_store.datum_path ~user ~type_name ~id ()))

(** (type-fields 'type) -> VList of VStr field names | VNil if undeclared. *)
let type_fields (store : Store.t) ~(type_name : string) : value =
  let fs = Store.lookup_type_fields store ~name:type_name in
  match fs with
  | None -> VNil
  | Some fields -> (VList (List.map (fun f -> VStr f) fields))

let tree_prim_error name msg =
  raise
    (Pricklypear_lisp.Ast.Interp_error
       (Pricklypear_lisp.Ast.Prim_error (name, msg, "bad-args")))

let wrap_path_exn name (f : unit -> value) : value =
  try f () with Invalid_argument msg -> tree_prim_error name msg


(* ---- hierarchical tree prims (notes.tree-cutover K0) ---- *)

let tree_get (store : Store.t) ~(path : string) ?(at_gen : int64 option) () : value =
  with_tree_tx store ~none:(fun () -> VNil) (fun conn ->
      let n =
        match at_gen with
        | None -> Tree_store.get_by_path conn ~path
        | Some g ->
            (* reads-as-of-gen: optional :gen N resolves to the VERSIONED leaf
               at that gen (tree-time.reads-as-of-gen — gets a fallback
               coordinate, never a required one). See tree_gen_read for the
               body-only surface; here we keep tree/get's node JSON shape. *)
            (match Tree_store.versioned_head conn ~path ~at_gen:g () with
             | None -> None
             | Some v -> Tree_store.version_of_leaf v)
      in
      (match n with
       | None -> VNil
       | Some node -> (VStr (Tree_store.node_to_json node))))

let tree_list (store : Store.t) ~(prefix : string) ?(depth : int option)
    ?(limit : int = 50000) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes =
        match depth with
        | Some 1 -> Tree_store.list_children conn ~prefix ~limit ()
        | _ -> Tree_store.list_subtree conn ~prefix ~limit ()
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

(* (tree/children-of path [limit]) -> VList of direct-child node JSON.
   Level-predicate read (nlevel(path)+1, + outline covering at +2 under
   literal label h), never an anti-join — one-tree.projections
   CHILDREN-BY-LEVEL. Same surface as tree/list depth 1; explicit name. *)
let tree_children_of (store : Store.t) ~(path : string)
    ?(limit : int = 50000) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes = Tree_store.list_children conn ~prefix:path ~limit () in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

(* (tree/subtree path [limit]) -> VList of all-descendant node JSON under
   path, capped at limit. Backed by list_subtree; same surface as
   (tree/list path) with no depth. Explicit bounded-snapshot name. *)
let tree_subtree (store : Store.t) ~(path : string)
    ?(limit : int = 50000) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes = Tree_store.list_subtree conn ~prefix:path ~limit () in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

(* ---- lens-aware read prims (one-tree.tree-read-prims, BORGE-1785516993) ----
   Resolve a RELATIVE logical path (e.g. "fn.notes/doc/get",
   "pkg.todo", "route./issues") against the session's branch lens chain
   (self -> parent -> ... -> system/main, closest-wins), the same rule
   function_defs use. A session on <user>/personal sees the EFFECTIVE
   override chain for tree_nodes, not just its own branch. *)

let lens_chain (store : Store.t) : string list =
  Store.branch_chain store ~branch_id:(Store.current_branch_id store)

let tree_lens_get (store : Store.t) ~(suffix : string) : value =
  with_tree_tx store ~none:(fun () -> VNil) (fun conn ->
      let n = Tree_store.lens_get_in_chain conn ~suffix (lens_chain store) in
      (match n with
       | None -> VNil
       | Some node -> (VStr (Tree_store.node_to_json node))))

let tree_lens_children (store : Store.t) ~(suffix : string)
    ?(limit : int = 50000) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes =
        Tree_store.lens_children_in_chain conn ~suffix ~limit
          (lens_chain store)
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

let tree_lens_subtree (store : Store.t) ~(suffix : string)
    ?(limit : int = 50000) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes =
        Tree_store.lens_subtree_in_chain conn ~suffix ~limit
          (lens_chain store)
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

(* ---- local-first prims (borge/local-first.borg lens-model +
   replication-units, campaign 8.1). A lens is a NAMED CHILD BRANCH of the
   checked-out branch (fork-on-clone, one-tree P5 EMPTY-LENS fork —
   copy-on-fork, parent pointer only); the server branch stays the parent.
   These prims make the two pinned contracts concrete: lens/fork names +
   creates the fork per the <owner>/lens-<device>[-<date>] contract, and
   replication/unit materializes the exact authorized-read subtree the
   identity would replicate (data.u.<user>.* + data.shared.* + the forked
   code lens + the branch ancestry it resolves through). *)

(* Current branch (owner, name) resolved from the branches registry. *)
let current_branch_ref (store : Store.t) : (string * string) option =
  let bid = Store.current_branch_id store in
  List.find_opt
    (fun (b : Pricklypear_common.Branch.t) -> b.id = bid)
    (Store.list_branches store)
  |> Option.map (fun (b : Pricklypear_common.Branch.t) -> (b.owner, b.name))

(* (lens/fork device-name) -> VStr "ok owner/lens-<device>[-<date>] (id)".
   lens-model: create a lens as a NAMED CHILD of the checked-out branch, per
   the pinned contract. parent_id = the current branch (server stays the
   parent); owner = the parent owner; child_name = lens-<device>[-<date>].
   Single-writer-per-lens is preserved by construction (server never writes
   into a lens except via sync/merge). *)
let lens_fork (store : Store.t) ~(device : string) ~(date : string option) () : value =
  match current_branch_ref store with
  | None -> VStr "lens/fork: no current branch"
  | Some (owner, _name) ->
      let suffix =
        match date with
        | Some d -> device ^ "-" ^ d
        | None -> device
      in
      let child_name = "lens-" ^ suffix in
      let parent_id = Store.current_branch_id store in
      (match Store.fork_branch store ~parent_id ~owner ~child_name with
       | Some id -> VStr (Printf.sprintf "ok %s/%s (%s)" owner child_name id)
       | None -> VStr "lens/fork: fork failed")

(* (replication/unit) -> VStr json-manifest. replication-units: materialize
   the exact authorized-read subtree the current identity would replicate —
   data.u.<user>.* + data.shared.* (the data, READ-mostly seed for shared), + the
   forked code lens + the branch ancestry it resolves through. Path-prefix RLS
   confinement (one-tree RLS floor) is the replication boundary; this prim
   never enumerates anything outside the identity-readable set. Ancestry uses
   Store.branch_chain (self-first, closest-wins). *)
let replication_unit (store : Store.t) () : value =
  let user = session_user () in
  (* data.u.<user>.* paths store the user label-ENCODED (tree_store datum_path),
     so the replication prefix and the count must use the encoded form — a raw
     username prefix would match nothing and under-count the unit. *) 
  let user_enc = Tree_store.enc_label user in
  let user_prefix = "data.u." ^ user_enc in
  let bid = Store.current_branch_id store in
  let chain = Store.branch_chain store ~branch_id:bid in
  let count prefix =
    match Io_lwt.Db.current_conn store with
    | None -> 0
    | Some conn ->
        List.length (Tree_store.list_subtree conn ~prefix ~limit:100000 ())
  in
  let owner, bname =
    match current_branch_ref store with
    | Some (o, n) -> (o, n)
    | None -> ("", "")
  in
  let branch_ref =
    if owner = "" then bid else owner ^ "/" ^ bname
  in
  let manifest =
    `Assoc
      [ ("owner", `String user)
      ; ("branch", `String branch_ref)
      ; ("branch_id", `String bid)
      ; ( "lens"
        , `String (owner ^ "/lens-<device>[-<date>] (forked as empty child)")
        )
      ; ("ancestry", `List (List.map (fun id -> `String id) chain))
      ; ( "data"
        , `Assoc
            [ ("user", `String user_prefix)
            ; ("user_count", `Int (count user_prefix))
            ; ("shared", `String "data.shared")
            ; ("shared_count", `Int (count "data.shared"))
            ]
        )
      ; ( "scope"
        , `String
            "data.u.<user>.* + data.shared.* + b.<lens>.* + branch ancestry; path-prefix RLS confinement = replication boundary"
        )
      ]
  in
  VStr (Yojson.Safe.to_string manifest)

(* ---- reads-as-of-gen prim surface (tree-time.reads-as-of-gen) ----
   The versioned read foundation (versioned_head / versioned_history / the
   `version` row type / gen-now) landed in the 5.1 close-out. These crud
   functions expose the user-facing gen coordinate: gen-read (node body as
   of a gen, crossing the branch lens rules head reads cross), gen-diff
   (node-granular diff between two gens — hashes first, walk on mismatch,
   the same algorithm family the three-way merge + local-first sync use),
   and undo (restore-by-write: read at head-n, write a NEW versioned row at
   head+1, so the restore itself is versioned and redoable). *)

(* Is [path] an absolute tree path (a b-prefixed code node or a data-prefixed
   datum node), or a RELATIVE logical suffix (kind.name) that must be resolved
   across the branch lens chain? *)
let absolute_tree_path (path : string) : bool =
  String.starts_with ~prefix:"b." path || String.starts_with ~prefix:"data." path

let tree_gen_read (store : Store.t) ~(path : string) ~(gen : int64) : value =
  with_tree_tx store ~none:(fun () -> VNil) (fun conn ->
      let v =
        if absolute_tree_path path then
          if gen <= 0L then
            (* head *)
            Tree_store.versioned_head conn ~path ()
          else
            Tree_store.versioned_head conn ~path ~at_gen:gen ()
        else
          (* relative logical suffix -> resolve effective path across the
             branch lens chain (closest-wins), then read as-of-gen. *)
          let at_gen_opt = if gen <= 0L then None else Some gen in
          let mk_path bid =
            match Tree_store.lens_suffix_path ~branch_id:bid path with
            | Some p -> p
            | None -> ""
          in
          Tree_store.lens_versioned_in_chain conn ~at_gen:at_gen_opt ~mk_path
            (lens_chain store)
      in
      (match v with
       | None -> VNil
       | Some v -> (VStr (Tree_store.version_to_json v))))

let body_of_version (v : Tree_store.version) : (string * Yojson.Safe.t) list =
  match v.Tree_store.v_body with
  | `Assoc kv ->
      (match List.assoc_opt "body" kv with
       | Some (`Assoc b) -> b
       | _ -> [])
  | _ -> []

(* (gen-diff path gen-a gen-b) -> node-granular diff. Hashes first in the
   store (versioned_head is one index range scan per gen); when both leaves
   hash-identical -> "same". Otherwise walk a shallow key-level difference of
   the two bodies and return a JSON object {path, a, b, same, delta}. *)
let tree_gen_diff (store : Store.t) ~(path : string)
    ~(gen_a : int64) ~(gen_b : int64) : value =
  with_tree_tx store ~none:(fun () -> VNil) (fun conn ->
      let a = Tree_store.versioned_head conn ~path ~at_gen:gen_a () in
      let b = Tree_store.versioned_head conn ~path ~at_gen:gen_b () in
      (match a, b with
       | Some va, Some vb
         when va.Tree_store.v_hash = vb.Tree_store.v_hash
              && va.Tree_store.v_rank = vb.Tree_store.v_rank ->
           (* sibling-order: a rank-only change (reorder) is a real
              difference even when the body hash is unchanged *)
           (VStr "same")
       | Some va, Some vb ->
           let ba = body_of_version va in
           let bb = body_of_version vb in
           let keys =
             List.sort_uniq String.compare (List.map fst ba @ List.map fst bb)
           in
           let delta_pairs =
             List.filter_map
               (fun k ->
                 let xa = List.assoc_opt k ba in
                 let xb = List.assoc_opt k bb in
                 if xa = xb then None
                 else
                   Some
                     (k,
                      `Assoc
                        [ ("a",
                           (match xa with Some x -> x | None -> `Null))
                        ; ("b",
                           (match xb with Some x -> x | None -> `Null))
                        ]))
               keys
           in
           let delta_pairs =
             if va.Tree_store.v_rank <> vb.Tree_store.v_rank then
               delta_pairs
               @ [ ( "rank"
                   , `Assoc
                       [ ( "a"
                         , (match va.Tree_store.v_rank with
                            | Some r -> `String r
                            | None -> `Null) )
                       ; ( "b"
                         , (match vb.Tree_store.v_rank with
                            | Some r -> `String r
                            | None -> `Null) )
                       ] ) ]
             else delta_pairs
           in
           let g_of v =
             match v.Tree_store.v_gen with
             | Some g -> `String (Int64.to_string g)
             | None -> `Null
           in
           (VStr
              (Yojson.Safe.to_string
                 (`Assoc
                    [ ("path", `String path)
                    ; ("a", g_of va)
                    ; ("b", g_of vb)
                    ; ("same", `Bool false)
                    ; ("delta", `Assoc delta_pairs)
                    ])))
       | _ -> VNil))

(* (undo path [n]) -> restore the value at head-n as a NEW write at head+1.
   Restore-by-write (not head-pointer manipulation): the restore is itself a
   versioned append, so undo is lossless and redoable ((undo path) again on
   the restored state climbs back down the gen chain). Supports the datum
   path class (the first documented consumer is notes undo over datum
   nodes). For code paths restore-by-write is deferred to the code-restore
   consumer (consumers subsection) — this prim reports that cleanly. Returns
   the written path or an error string. *)
let tree_undo (store : Store.t) ~(path : string) ?(n : int64 = 1L) () : value =
  with_tree_tx store ~none:(fun () -> no_db "undo") (fun conn ->
      if not (absolute_tree_path path) then
        (VStr "undo: path must be an absolute b.* or data.* path")
      else
        let hist = Tree_store.versioned_history conn ~path () in
        if hist = [] then (VStr "undo: no history for path")
        else
          let newest_gen =
            List.fold_left
              (fun acc v ->
                match v.Tree_store.v_gen with
                | Some g -> Int64.max acc g
                | None -> acc)
              0L hist
          in
          let target = Int64.sub newest_gen n in
          (match
             Tree_store.versioned_head conn ~path ~at_gen:target ()
           with
           | None ->
               (VStr
                  (Printf.sprintf "undo: no value at gen %Ld" target))
           | Some v ->
               let body_json =
                 match body_of_version v with
                 | [] -> "{}"
                 | kv -> Yojson.Safe.to_string (`Assoc kv)
               in
               let type_name =
                 match v.Tree_store.v_type_name with
                 | Some t -> t
                 | None -> "" (* datum rows carry type_name in the version row *)
               in
               let author = v.Tree_store.v_author in
               let author =
                 match author with Some a -> a | None -> session_user ()
               in
               if String.starts_with ~prefix:"b." path then
                 (VStr
                    "undo: code-path restore-by-write deferred (code-restore \
                     consumer); use gen-read to inspect, then define")
               else
                 (* tree-time.consumers: kind-aware restore. Datum rows
                    (kind=datum) re-write via upsert_datum; hierarchical
                    nodes (note/folder/heading) must go back through
                    upsert_hier so the payload envelope keeps its note kind
                    (an upsert_datum restore silently rewrites the payload
                    marker to "datum" — the kind-column/payload mismatch
                    the notes-undo consumer exposes). The version row
                    captures the ORIGINAL kind, so the restore defeats to it;
                    unknown/empty kind falls back to datum behaviour. *)
                 let hier_kind =
                   match v.Tree_store.v_kind with
                   | k when k <> "" && Tree_store.is_hier_kind k -> Some k
                   | _ -> None
                 in
                 (match hier_kind with
                  | Some k ->
                      (match
                         Tree_store.upsert_hier conn ~path ~kind:k ~type_name
                           ~body_json ~author ~rank:None
                       with
                       | Ok p ->
                           (* sibling-order: the restore is EXACT — the
                              rank in force at the target gen (NULL clears
                              a later reorder), not a COALESCE-keep *)
                           let _ =
                             Tree_store.set_rank conn ~path:p ~rank:v.Tree_store.v_rank
                           in
                           (VStr p)
                       | Error e -> (VStr e))
                  | None ->
                      (match
                         Tree_store.upsert_datum conn ~path ~type_name
                           ~body_json ~author ~rank:None
                       with
                       | Ok p ->
                           let _ =
                             Tree_store.set_rank conn ~path:p ~rank:v.Tree_store.v_rank
                           in
                           (VStr p)
                       | Error e -> (VStr e)))))

(* html escaping: one string-level implementation lives in the kernel
   (Prims_pure.html_escape_str); html_escape_text was a fourth image copy —
   the tree fragment renderer just calls the kernel escaper now. *)

(* Budget-safe HTML for notes tree children under [prefix].
   Mirrors notes/ui/tree-children-html (libs/notes/ui) but runs in OCaml so
   folders with thousands of notes do not blow the Nopales eval budget.
   Presentation lives HERE (the prim/eval boundary next to tree/children-html),
   not in Tree_store: the storage layer is HTML-free and only returns
   structured children (list_children / parents_with_hier_children) plus the
   display-title/slug accessors the fragment needs. *)
let render_children_html (conn : Store_db.conn) ~(prefix : string)
    ?(limit : int = 50000) ~(garden_root : string) () : string =
  if not (Tree_store.valid_path prefix) then
    "<p class=\"muted\">(empty)</p>"
  else
    let kids = Tree_store.list_children conn ~prefix ~limit () in
    let kids =
      List.filter
        (fun n -> n.Tree_store.kind = "folder" || n.Tree_store.kind = "note")
        kids
    in
    if kids = [] then "<p class=\"muted\">(empty)</p>"
    else
      let expandable =
        let set = Hashtbl.create 64 in
        List.iter (fun p -> Hashtbl.replace set p true)
          (Tree_store.parents_with_hier_children conn ~prefix);
        set
      in
      let garden_path = garden_root ^ ".garden" in
      let buf = Buffer.create 4096 in
      let emit_folder_details ~path ~label ~open_slug =
        Buffer.add_string buf "<details class=\"tree-folder\" hx-get=\"";
        Buffer.add_string buf "/partials/notes-tree?path=";
        Buffer.add_string buf (Prims_pure.html_escape_str path);
        Buffer.add_string buf
          "\" hx-trigger=\"toggle once\" hx-target=\"find .tree-children\" ";
        Buffer.add_string buf "hx-swap=\"innerHTML\"><summary class=\"tree-folder-header\">";
        Buffer.add_string buf
          "<span class=\"tree-folder-icon\">&#x1F4C1; </span>";
        (match open_slug with
         | Some slug ->
             Buffer.add_string buf "<a href=\"/notes?slug=";
             Buffer.add_string buf (Prims_pure.html_escape_str slug);
             Buffer.add_string buf "\" onclick=\"event.stopPropagation()\">";
             Buffer.add_string buf (Prims_pure.html_escape_str label);
             Buffer.add_string buf "</a>"
         | None -> Buffer.add_string buf (Prims_pure.html_escape_str label));
        Buffer.add_string buf
          "</summary><div class=\"tree-children\"><span class=\"muted\">";
        Buffer.add_string buf "open to load…</span></div></details>"
      in
      let emit_note_li n =
        let slug = Tree_store.node_display_slug n in
        let title = Tree_store.node_display_title n in
        Buffer.add_string buf "<li class=\"tree-item\"><a href=\"/notes?slug=";
        Buffer.add_string buf (Prims_pure.html_escape_str slug);
        Buffer.add_string buf "\">";
        Buffer.add_string buf (Prims_pure.html_escape_str title);
        Buffer.add_string buf "</a></li>"
      in
      let rec emit_inlined path =
        let nested = Tree_store.list_children conn ~prefix:path ~limit () in
        let nested_expand =
          let set = Hashtbl.create 64 in
          List.iter (fun p -> Hashtbl.replace set p true)
            (Tree_store.parents_with_hier_children conn ~prefix:path);
          set
        in
        List.iter
          (fun n ->
            if
              n.Tree_store.kind = "folder" || n.Tree_store.kind = "note"
            then
              emit_node ~expandable:nested_expand n)
          nested
      and emit_node ~expandable n =
        if n.Tree_store.kind = "folder" then
          if n.Tree_store.path = garden_path then emit_inlined n.Tree_store.path
          else
            emit_folder_details ~path:n.Tree_store.path
              ~label:(Tree_store.node_display_title n)
              ~open_slug:None
        else if n.Tree_store.kind = "note" then
          if Hashtbl.mem expandable n.Tree_store.path then
            emit_folder_details ~path:n.Tree_store.path
              ~label:(Tree_store.node_display_title n)
              ~open_slug:(Some (Tree_store.node_display_slug n))
          else emit_note_li n
        else ()
      in
      List.iter (emit_node ~expandable) kids;
      Buffer.contents buf

let tree_children_html (store : Store.t) ~(prefix : string) ~(garden_root : string)
    () : value =
  with_tree_tx store ~none:(fun () -> VStr "<p class=\"muted\">(empty)</p>")
    (fun conn ->
       let html = render_children_html conn ~prefix ~garden_root () in
       (VStr html))

let tree_upsert_outcome (store : Store.t) ~(path : string) ~(kind : string)
    ~(type_name : string) ~(body_json : string) ~(author : string)
    : (string, string) result =
  if kind = "datum" then
    (* Datum node: run the same typed-boundary validation as create/update,
       then write via Tree_store.upsert_datum (path-explicit; generates an
       id when the path's id label is a placeholder). one-tree.tree-datum-prims. *)
    let reject = check_type_fields store ~type_name ~fields_json:body_json in
    (match reject with
     | Some msg -> (Error msg)
     | None ->
     with_tree_tx store ~none:(fun () -> no_db "tree/upsert") (fun conn ->
       Tree_store.upsert_datum conn ~path ~type_name ~body_json ~author
         ~rank:None))
  else
    with_tree_tx store ~none:(fun () -> no_db "tree/upsert") (fun conn ->
      Tree_store.upsert_hier conn ~path ~kind ~type_name ~body_json ~author
        ~rank:None)

let tree_upsert (store : Store.t) ~(path : string) ~(kind : string)
    ~(type_name : string) ~(body_json : string) ~(author : string) : value =
  match
    tree_upsert_outcome store ~path ~kind ~type_name ~body_json ~author
  with
  | Ok p -> (VStr p)
  | Error e -> (VStr e)

let tree_delete (store : Store.t) ~(path : string) ~(recursive : bool)
    : value =
  (* One tx: the incoming-edge count and the delete see the same state,
     and the delete cannot race a concurrent link creation. *)
  with_tree_tx store ~none:(fun () ->
      (* i-1788670475: was VStr "{\"deleted\":0,...}" — a fabricated
         success shape that made destructive flows look idempotent with
         the DB down. A write with no DB raises, loudly. *)
      no_db "tree/delete")
    (fun conn ->
      (* tree-edges: report incoming-edge count (warn, not forbid — the
         delete succeeds; edges pointing at the deleted path dangle and
         are repairable). Exact-path count only (recursive deletes do
         not aggregate descendant inbound counts in v1). *)
      let inbound = Tree_store.incoming_edge_count conn ~path in
      let n = Tree_store.delete_path conn ~path ~recursive in
      (VStr
         (Yojson.Safe.to_string
            (`Assoc
               [ ("deleted", `Int n)
               ; ("inbound_edges", `Int inbound)
               ]))))

(* ---- datum type-change validation for tree/move (one-tree.tree-datum-prims) ----
   Moving a datum node to a path under a DIFFERENT <type_name> is a type
   change and must re-run typed-boundary validation against the new type
   (exactly the same rule create/update enforce). When both paths are datum
   paths and the type segment differs, read the node's body at [old_path]
   and validate the declared fields of the NEW type; a missing required
   field is a clean rejection naming the field, identical in spirit to
   create/update. Same-type moves, hierarchical moves, and any path that is
   not a datum path skip validation (no type semantics to check). *)

(** The type_name encoded in a datum path, decoded. Datum path grammar:
      data.u.<user>.<type>.<id>
    | data.shared.<type>.<id>
    | data.l.<lens>.<user>.<type>.<id>     (versioned data lens, 2026-08-23)
    The type segment is base64-label-encoded (Tree_store.enc_label).
    None when [path] is not a datum path. *)
let datum_path_type (path : string) : string option =
  let parts = String.split_on_char '.' path in
  let type_label =
    match parts with
    | "data" :: "u" :: _user :: ty :: _id :: _ -> Some ty
    | "data" :: "shared" :: ty :: _id :: _ -> Some ty
    | "data" :: "l" :: _lens :: _user :: ty :: _id :: _ -> Some ty
    | _ -> None
  in
  match type_label with
  | None -> None
  | Some ty -> Tree_store.dec_label ty

(** If [old_path] -> [new_path] is a datum move that changes type, validate
    the moved node's body against the NEW type's declared fields. Returns
    Some rejection-message (naming the missing fields) when it fails; None
    when there is nothing to check (not a datum move, same type, undeclared
    new type, node missing, or free-form body). *)
let datum_move_type_reject (store : Store.t) ~(old_path : string)
    ~(new_path : string) : string option =
  match datum_path_type old_path, datum_path_type new_path with
  | Some old_ty, Some new_ty when old_ty <> new_ty ->
      (match Io_lwt.Db.current_conn store with
       | None -> None
       | Some conn ->
           (match Tree_store.get_by_path conn ~path:old_path with
            | None -> None
            | Some node ->
                let body_json =
                  match node.body with
                  | `Assoc _ -> Yojson.Safe.to_string node.body
                  | _ -> "{}"
                in
                let declared =
                  match Store.lookup_type_fields store ~name:new_ty with
                  | Some f -> f
                  | None -> []
                in
                (match
                   Tree_store.validate_fields ~declared body_json
                 with
                 | Ok () -> None
                 | Error msg ->
                     Some
                       (Printf.sprintf "type change %s -> %s: %s" old_ty
                          new_ty msg))))
  | _ -> None

let tree_move (store : Store.t) ~(old_path : string) ~(new_path : string)
    : value =
  (* One tx: the type-change validation read, the inbound-edge count and
     the subtree move are atomic (the move cannot land on a node that
     changed validation state mid-flight). datum_move_type_reject rides
     this same conn via current_conn. *)
  with_tree_tx store ~none:(fun () -> no_db "tree/move") (fun conn ->
      (* tree-edges: moving the TARGET of inbound edges leaves them
         dangling (to is stored verbatim, never rewritten) — report the
         count, warn-not-forbid. Moving a SOURCE carries its edges (the
         subtree move rewrites their paths). *)
      let inbound = Tree_store.incoming_edge_count conn ~path:old_path in
      (* Validate BEFORE moving: a type-change rejection must leave the
         tree untouched (the tx would otherwise commit the move). *)
      (match datum_move_type_reject store ~old_path ~new_path with
       | Some msg -> (VStr ("tree/move: " ^ msg))
       | None ->
           let r = Tree_store.move_subtree conn ~old_path ~new_path in
           (match r with
            | Ok n ->
                (VStr
                   (Yojson.Safe.to_string
                      (`Assoc
                         [ ("moved", `Int n)
                         ; ("inbound_edges", `Int inbound)
                         ])))
            | Error e -> (VStr e))))

(* ---- tree-edges prims (one-tree.tree-edges) ---- *)

(* (link from to [kind [label]]) — create an edge node (kind=link) at
   <from>.lnk.<uuid>. from must resolve (get_by_path); to is stored
   verbatim as branch-qualified path text and MAY dangle; resolution is
   a read-time act against the reader's lens. Edge kind defaults to
   "ref". Returns the edge node JSON. *)
let link_edge (store : Store.t) ~(from_path : string) ~(to_path : string)
    ~(edge_kind : string) ~(label : string option) ~(author : string) : value =
  if not (Tree_store.valid_path to_path) then
    VStr "link: to must be a well-formed absolute path (it may dangle)"
  else
    (* One tx: resolve-from + uuid + upsert + read-back are atomic; the
       from-node read rides the same pp.user floor. *)
    with_tree_tx store ~none:(fun () -> no_db "link") (fun conn ->
        match Tree_store.get_by_path conn ~path:from_path with
        | None -> VStr ("link: from does not resolve: " ^ from_path)
        | Some _ ->
            let body =
              let base =
                [ ("to", `String to_path); ("kind", `String edge_kind) ]
              in
              let base =
                match label with
                | Some l when l <> "" -> base @ [ ("label", `String l) ]
                | _ -> base
              in
              Yojson.Safe.to_string (`Assoc base)
            in
            (* edge path = <from>.lnk.<uuid> — the uuid comes from PG *)
            let sql_uuid =
              "WITH nu AS (SELECT gen_random_uuid() AS id) "
              ^ "SELECT tree_uuid_label(nu.id) FROM nu"
            in
            (match Store_db.exec conn sql_uuid with
             | [ [ u ] ] -> (
                 match Pgx.Value.to_string u with
                 | Some uuid ->
                     let edge_path =
                       Tree_store.path_child from_path "lnk" ^ "." ^ uuid
                     in
                     (match
                        Tree_store.upsert_link conn ~path:edge_path
                          ~body_json:body ~author
                      with
                      | Ok p -> (
                          match Tree_store.get_by_path conn ~path:p with
                          | Some n -> VStr (Tree_store.node_to_json n)
                          | None -> VStr p)
                      | Error e -> VStr ("link: " ^ e))
                 | None -> VStr "link: uuid generation failed")
             | _ -> VStr "link: uuid generation failed"))

(* (unlink edge-path) — tombstone the edge: the same delete/versioning
   path as any node (the tree-time trigger appends a tombstone; undo of
   unlink restores the edge). Only kind=link nodes may be unlinked. *)
let unlink_edge (store : Store.t) ~(edge_path : string) : value =
  with_tree_tx store ~none:(fun () -> no_db "unlink") (fun conn ->
      match Tree_store.get_by_path conn ~path:edge_path with
      | None -> VStr ("unlink: no node at " ^ edge_path)
      | Some n ->
          if not (Tree_store.is_link_kind n.kind) then
            VStr ("unlink: not an edge (kind " ^ n.kind ^ ")")
          else
            let deleted = Tree_store.delete_path conn ~path:edge_path ~recursive:false in
            if deleted = 1 then VStr edge_path
            else VStr "unlink: delete failed")

(* (out-links path) — the node's outgoing edges with resolution status:
   each item is JSON {path,to,kind,label,status,target}; status is
   "resolved" (target summary from the reader's lens) or "dangling".
   An edge list is a children-by-level index read, never a scan. *)
let out_links (store : Store.t) ~(path : string) ?(limit : int = 1000) () :
    value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let edges = Tree_store.list_out_links conn ~path ~limit () in
      let item (e : Tree_store.node) =
        let to_path =
          match Tree_store.body_string_field e.body "to" with
          | Some t -> t
          | None -> ""
        in
        let status, target =
          if to_path = "" then ("dangling", `Null)
          else
            match Tree_store.get_by_path conn ~path:to_path with
            | Some t ->
                ( "resolved"
                , `Assoc
                    [ ("path", `String t.path)
                    ; ("kind", `String t.kind)
                    ; ( "type"
                      , match t.type_name with
                        | Some ty -> `String ty
                        | None -> `Null )
                    ; ("title", `String (Tree_store.node_display_title t))
                    ] )
            | None -> ("dangling", `Null)
        in
        VStr
          (Yojson.Safe.to_string
             (`Assoc
                [ ("path", `String e.path)
                ; ("to", `String to_path)
                ; ( "kind"
                  , match Tree_store.body_string_field e.body "kind" with
                    | Some k -> `String k
                    | None -> `Null )
                ; ( "label"
                  , match Tree_store.body_string_field e.body "label" with
                    | Some l -> `String l
                    | None -> `Null )
                ; ("status", `String status)
                ; ("target", target)
                ]))
      in
      VList (List.map item edges))

(* Backlinks upgrade (one-tree.tree-edges): edges whose to = needle —
   backed by the migration-026 partial expression index — UNION the
   legacy body-string scan (datum bodies holding the needle as a literal
   value). The legacy half is DEPRECATED: logged once per process, kept
   only until all body-valued references become edges. Same signature as
   the old backlinks: a list of paths. *)
let backlinks_deprecation_logged = ref false

let backlinks (store : Store.t) ~(needle : string) ?(limit : int = 200) ()
    : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let edge_paths =
        Tree_store.edges_to conn ~needle ~limit ()
        |> List.map (fun (e : Tree_store.node) -> e.path)
      in
      let legacy_paths =
        if List.length edge_paths >= limit then []
        else begin
          if not !backlinks_deprecation_logged then begin
            backlinks_deprecation_logged := true;
            Printf.eprintf
              "[backlinks] DEPRECATED: legacy body-string backlinks are a scan; materialize references as edges ((link from to)).\n%!"
          end;
          Tree_store.backlinks conn ~needle ~limit ()
        end
      in
      (* edges first, then legacy paths not already present (dedup) *)
      let seen = List.fold_left (fun acc p -> p :: acc) [] edge_paths in
      let merged =
        edge_paths
        @ List.filter (fun p -> not (List.mem p seen)) legacy_paths
      in
      VList (List.map (fun p -> VStr p) merged))

(* ---- sibling-order prims (one-tree.sibling-order) ----

   Rank allocation is midpoint-between-neighbours over canonical base-36
   ranks (Tree_store.rank helpers); when a generated rank would exceed
   max_rank_len the whole sibling set is rebalanced onto fresh fixed-
   width ranks and the placement recomputed (always fits after a
   rebalance). Mutations deny_anon at the prim layer. Ranks live in the
   rank column (migration 025), are versioned by the tree-time trigger,
   and reads order rank COLLATE "C" NULLS LAST, path ASC. *)

(** Candidate rank for placing a node immediately before (before=true)
   or after sibling [anchor] under [parent]. Rebalances the sibling set
   (once) when the midpoint would exceed max_rank_len, then recomputes. *)
let place_candidate (conn : Store_db.conn) ~(parent : string)
    ~(anchor : string) ~(before : bool) : (string, string) result =
  let rec attempt retried =
    let children = Tree_store.list_children conn ~prefix:parent () in
    (match
       List.find_opt (fun (n : Tree_store.node) -> n.path = anchor) children
     with
     | None -> Error ("anchor not found under " ^ parent)
     | Some a -> (
         match a.rank with
         | None -> Error "anchor has no rank (rank it first: tree/reorder)"
         | Some ar ->
             let ranked =
               List.filter_map
                 (fun (n : Tree_store.node) ->
                   match n.rank with Some r -> Some (n.path, r) | None -> None)
                 children
             in
             (* [ranked] is in rank order (SQL: ranked first, rank ASC).
                The neighbour is the ranked sibling immediately before /
                after the anchor. *)
             let neighbor : (string * string) option =
               if before then
                 List.fold_left
                   (fun acc (p, r) ->
                     if String.compare r ar < 0 then Some (p, r) else acc)
                   None ranked
               else
                 List.find_map
                   (fun (p, r) ->
                     if String.compare r ar > 0 then Some (p, r) else None)
                   ranked
             in
             let candidate =
               match neighbor with
               | Some (_p, r) ->
                   if before then Tree_store.rank_midpoint r ar
                   else Tree_store.rank_midpoint ar r
               | None -> (
                   if before then
                     match Tree_store.rank_before ar with
                     | Some r -> r
                     | None -> "" (* floor exhaustion -> rebalance *)
                   else Tree_store.rank_after ar)
             in
             if
               candidate = ""
               || String.length candidate > Tree_store.max_rank_len
             then
               if retried then Error "rank rebalance failed"
               else begin
                 let _ = Tree_store.rebalance_siblings conn ~parent in
                 attempt true
               end
             else Ok candidate))
  in
  attempt false

let tree_move_before (store : Store.t) ~(path : string) ~(anchor : string)
    : value =
  wrap_path_exn "tree/move-before" (fun () ->
      match Tree_store.path_parent path, Tree_store.path_parent anchor with
      | Some parent, Some ap when parent = ap && path <> anchor -> (
          with_tree_tx store ~none:(fun () -> no_db "tree/move-before")
            (fun conn ->
               match place_candidate conn ~parent ~anchor ~before:true with
               | Error e -> VStr ("tree/move-before: " ^ e)
               | Ok rank ->
                   if Tree_store.set_rank conn ~path ~rank:(Some rank) = 1 then
                     VStr rank
                   else VStr ("tree/move-before: no node at " ^ path)))
      | Some _, Some _ when path = anchor ->
          VStr (Tree_store.path_leaf path) (* no-op: already there *)
      | _ ->
          VStr "tree/move-before: path and anchor must be siblings (same parent)")

let tree_move_after (store : Store.t) ~(path : string) ~(anchor : string)
    : value =
  wrap_path_exn "tree/move-after" (fun () ->
      match Tree_store.path_parent path, Tree_store.path_parent anchor with
      | Some parent, Some ap when parent = ap && path <> anchor -> (
          with_tree_tx store ~none:(fun () -> no_db "tree/move-after")
            (fun conn ->
               match place_candidate conn ~parent ~anchor ~before:false with
               | Error e -> VStr ("tree/move-after: " ^ e)
               | Ok rank ->
                   if Tree_store.set_rank conn ~path ~rank:(Some rank) = 1 then
                     VStr rank
                   else VStr ("tree/move-after: no node at " ^ path)))
      | Some _, Some _ when path = anchor ->
          VStr (Tree_store.path_leaf path) (* no-op: already there *)
      | _ ->
          VStr "tree/move-after: path and anchor must be siblings (same parent)")

(* (tree/reorder path rank) — set the rank directly. "" clears (back to
   unordered). Rank must be base-36 ([0-9a-z]+) so midpoint allocation
   stays well-defined. Returns node JSON. *)
let tree_reorder (store : Store.t) ~(path : string) ~(rank : string) : value =
  with_tree_tx store ~none:(fun () -> no_db "tree/reorder") (fun conn ->
      if rank <> "" && not (Tree_store.rank_valid rank) then
        VStr "tree/reorder: rank must be base-36 ([0-9a-z]+)"
      else if
        Tree_store.set_rank conn ~path
          ~rank:(if rank = "" then None else Some rank)
        = 0
      then VStr ("tree/reorder: no node at " ^ path)
      else
        (match Tree_store.get_by_path conn ~path with
         | Some n -> VStr (Tree_store.node_to_json n)
         | None -> VStr "ok"))

(* (tree/siblings path) — ordered children of path's parent (the node
   itself included). Root paths have no siblings. *)
let tree_siblings (store : Store.t) ~(path : string) : value =
  wrap_path_exn "tree/siblings" (fun () ->
      match Tree_store.path_parent path with
      | None -> VList [] (* root: no parent, no siblings *)
      | Some parent -> (
          with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
              let nodes = Tree_store.list_children conn ~prefix:parent () in
              VList
                (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))))

(* (tree/next path) / (tree/prev path) — the adjacent sibling in the
   total order (ranked first by rank, then unranked by path). Nil at
   either end, or when the path is not a child of anything. *)
let tree_neighbor (store : Store.t) ~(path : string) ~(dir : int) : value =
  wrap_path_exn "tree/neighbor" (fun () ->
      match Tree_store.path_parent path with
      | None -> VNil
      | Some parent -> (
          with_tree_tx store ~none:(fun () -> VNil) (fun conn ->
              let nodes = Tree_store.list_children conn ~prefix:parent () in
              let idx =
                List.find_index (fun (n : Tree_store.node) -> n.path = path) nodes
              in
              (match idx with
               | None -> VNil (* not a child of its parent: nothing adjacent *)
               | Some i ->
                   let j = i + dir in
                   if j >= 0 && j < List.length nodes then
                     (match List.nth_opt nodes j with
                      | Some n -> VStr (Tree_store.node_to_json n)
                      | None -> VNil)
                   else VNil))))

let tree_next (store : Store.t) ~(path : string) : value =
  tree_neighbor store ~path ~dir:1

let tree_prev (store : Store.t) ~(path : string) : value =
  tree_neighbor store ~path ~dir:(-1)

(* (tree/insert-child parent kind type body [:after anchor]) — create a
   child of [parent] with a rank allocated after [anchor] (or appended
   after the last ranked sibling). Hier kinds only: a kind=datum node
   must keep its data.<type>.<id> path grammar, so datum ordering stays
   on tree/upsert + tree/reorder (borge issue filed). The leaf label
   comes from the body's slug (or title), path_label-encoded. *)
let tree_insert_child (store : Store.t) ~(parent : string) ~(kind : string)
    ~(type_name : string) ~(body_json : string) ~(anchor : string option)
    ~(author : string) : value =
  if not (Tree_store.valid_path parent) then
    VStr "tree/insert-child: invalid parent path"
  else if not (Tree_store.is_hier_kind kind) then
    VStr
      "tree/insert-child: kind must be folder|note|heading (datum ordering: \
       tree/upsert + tree/reorder)"
  else
    (* One tx: label parse + rank placement + upsert + read-back atomic. *)
    with_tree_tx store ~none:(fun () -> no_db "tree/insert-child") (fun conn ->
        let label =
          match Tree_store.parse_body_json body_json with
          | Ok body -> (
              match Tree_store.body_string_field body "slug" with
              | Some s -> Some (Tree_store.path_label s)
              | None -> (
                  match Tree_store.body_string_field body "title" with
                  | Some t when String.trim t <> "" ->
                      Some (Tree_store.path_label t)
                  | _ -> None))
          | Error _ -> None
        in
        match label with
        | None -> VStr "tree/insert-child: body needs a slug or title"
        | Some lab ->
            let path = Tree_store.path_child parent lab in
            let rank_res =
              match anchor with
              | Some a -> (
                  match Tree_store.path_parent a with
                  | Some ap when ap = parent ->
                      place_candidate conn ~parent ~anchor:a ~before:false
                  | _ -> Error "anchor must be a ranked child of parent")
              | None ->
                  let last_ranked =
                    List.fold_left
                      (fun acc (n : Tree_store.node) ->
                        match n.rank with Some r -> Some r | None -> acc)
                      None
                      (Tree_store.list_children conn ~prefix:parent ())
                  in
                  Ok
                    (Tree_store.rank_after
                       (match last_ranked with Some r -> r | None -> ""))
            in
            (match rank_res with
             | Error e -> VStr ("tree/insert-child: " ^ e)
             | Ok rank -> (
                 match
                   Tree_store.upsert_hier conn ~path ~kind ~type_name
                     ~body_json ~author ~rank:(Some rank)
                 with
                 | Ok p -> (
                     match Tree_store.get_by_path conn ~path:p with
                     | Some n -> VStr (Tree_store.node_to_json n)
                     | None -> VStr p)
                 | Error e -> VStr ("tree/insert-child: " ^ e))))

let tree_get_uuid (store : Store.t) ~(uuid : string) : value =
  with_tree_tx store ~none:(fun () -> VNil) (fun conn ->
      let n = Tree_store.get_by_uuid conn ~uuid in
      (match n with
       | None -> VNil
       | Some node -> (VStr (Tree_store.node_to_json node))))

let tree_find_by (store : Store.t) ~(kind : string option) ~(field : string)
    ~(value : string) ?(prefix : string = "") ?(limit : int = 50) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes =
        Tree_store.find_by_body_field conn ~kind ~field ~value ~prefix ~limit ()
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

let tree_list_kind (store : Store.t) ~(prefix : string) ~(kind : string)
    ?(limit : int = 2000) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let nodes =
        Tree_store.list_by_kind conn ~prefix ~kind ~limit ()
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes)))

let tree_note_index (store : Store.t) ~(prefix : string) ?(limit : int = 2000)
    ?(with_outline : bool = false)
    () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let rows =
        Tree_store.list_note_index conn ~prefix ~limit ~with_outline ()
      in
      (VList
           (List.map
              (fun (id, fields) -> VPair (VStr id, VStr fields))
              rows)))

let tree_folder_index (store : Store.t) ~(prefix : string) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let folders = Tree_store.list_folder_index conn ~prefix () in
      (VList (List.map (fun f -> VStr f) folders)))

(** Substring search over tree notes+headings. Returns list of hit JSON. *)
let tree_search_text (store : Store.t) ~(prefix : string) ~(needle : string)
    ?(limit : int = 20) () : value =
  with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
      let hits =
        Tree_store.search_text conn ~prefix ~needle ~limit ()
      in
      (VList (List.map (fun j -> VStr j) hits)))

let tree_path_label (s : string) : value = VStr (Tree_store.path_label s)

(* ---- pure path-algebra prims (one-tree.tree-path-prims) ----
   No DB, no auth. The kernel raises Invalid_argument on an invalid
   path; converted here to a Prim_error so Nopales sees a clean raise
   rather than a guess. Root paths (single label) have nil parent and
   no strict ancestors. *)
let tree_parent (path : string) : value =
  wrap_path_exn "tree/parent" (fun () ->
      match Tree_store.path_parent path with
      | Some p -> VStr p
      | None -> VNil (* root path has no parent *))

let tree_leaf (path : string) : value =
  wrap_path_exn "tree/leaf" (fun () -> VStr (Tree_store.path_leaf path))

let tree_segments (path : string) : value =
  wrap_path_exn "tree/segments" (fun () ->
      VList (List.map (fun s -> VStr s) (Tree_store.path_segments path)))

let tree_child ~(path : string) ~(label : string) : value =
  wrap_path_exn "tree/child" (fun () ->
      VStr (Tree_store.path_child path label))

let tree_ancestors (path : string) : value =
  wrap_path_exn "tree/ancestors" (fun () ->
      VList
        (List.map (fun s -> VStr s) (Tree_store.path_ancestors path)))

let tree_garden_root ~(user : string) : value =
  VStr (Tree_store.garden_root ~user)

(* ---- tree-time.gc (5.3): retention worker surface ------------------------
   retention-policy-set / retention-policies / retention-delete-policy manage
   policy AS HABITAT DATA (tree_retention_policy, migration 022); retention-run
   drives the worker (rollup + DROP PARTITION) for each path class that has an
   explicit whole-class policy. Retention is deferred until a class outgrows its
   welcome — the default (no whole-class policy) is keep-everything, never
   ad-hoc deletion. *) 

let retention_set_policy (store : Store.t) ~(path_class : string)
    ~(type_name : string option) ~(keep_windows : int64) () : value =
  VStr (Store.retention_set_policy store ~path_class ~type_name ~keep_windows)

let retention_delete_policy (store : Store.t) ~(path_class : string)
    ~(type_name : string option) () : value =
  VStr
    (Store.retention_delete_policy store ~path_class ~type_name)

let retention_policies (store : Store.t) () : value =
  let ps = Store.retention_list_policies store in
  let js =
    List.map
      (fun (pc, tn, kw, ca) ->
        Yojson.Safe.to_string
          (`Assoc
            [ ("path_class", `String pc);
              ("type_name",
                match tn with None -> `Null | Some s -> `String s);
              ("keep_windows", `Intlit (Int64.to_string kw));
              ("created_at", `String ca) ]))
      ps
  in
  VList (List.map (fun j -> VStr j) js)

let retention_run (store : Store.t) ~(path_class : string option) () : value =
  let rs = Store.retention_run store ~path_class () in
  let js =
    List.map
      (fun (pc, d, c) ->
        Yojson.Safe.to_string
          (`Assoc
            [ ("path_class", `String pc);
              ("dropped_windows", `Intlit (Int64.to_string d));
              ("checkpointed_paths", `Intlit (Int64.to_string c)) ]))
      rs
  in
  VList (List.map (fun j -> VStr j) js)


(* ---- data lens operations (one-tree.borg branching-interaction,
   2026-08-23 make: versioned user-keyed data). A data lens is a
   deliberate fork of the user's live data.u.<user> subtree into
   data.l.<lens>.<user>. The lens can diverge (edits land in the lens,
   not live data), merge back (lens -> trunk, LWW per node v0), or be
   abandoned (drop the lens subtree). The default data.u stays
   single-head and branch-free; data.l is the explicit opt-in version
   axis. *)

(* (fork-data lens-name [type-name]) -> VStr.
   Copy the user's live datum nodes (data.u.<user>) into a fresh data
   lens (data.l.<lens>.<user>). When ~type_name is set, only that type
   is forked; otherwise all the user's non-seed types. Each source node's
   path is rewritten: data.u.<user>.<type>.<id> ->
   data.l.<lens>.<user>.<type>.<id>. The node payload (body, hash, author)
   is copied; the path field in the payload is updated. Returns
   "ok <lens> (<n> nodes)" or an error. *)
let fork_data (store : Store.t) ~(lens : string)
    ?(type_name : string option = None) () : value =
  let user = session_user () in
  let user_enc = Tree_store.enc_label user in
  let lens_enc = Tree_store.enc_label lens in
  let live_prefix = Printf.sprintf "data.u.%s" user_enc in
  let lens_prefix =
    Printf.sprintf "data.l.%s.%s" lens_enc user_enc
  in
  (with_tree_tx store
     ~none:(fun () -> VStr "fork-data: no database connection")
     (fun conn ->
       (* Check the lens doesn't already exist (avoid silent double-fork). *)
       let existing =
         Tree_store.list_subtree conn ~prefix:lens_prefix ~limit:1 ()
       in
       if existing <> [] then
         VStr (Printf.sprintf "fork-data: lens %s already exists" lens)
       else
         (* Copy datum nodes from live -> lens by repathing.
            The SQL constructs the new path by replacing the data.u.<user>
            prefix with data.l.<lens>.<user>, preserving the type + id
            segments. Payload path is updated to match. *)
         let type_clause =
           match type_name with
           | None -> ""
           | Some tn ->
               Printf.sprintf " AND type_name = $3::text"
         in
         let params =
           match type_name with
           | None ->
               [ Pgx.Value.of_string live_prefix
               ; Pgx.Value.of_string lens_prefix ]
           | Some tn ->
               [ Pgx.Value.of_string live_prefix
               ; Pgx.Value.of_string lens_prefix
               ; Pgx.Value.of_string tn ]
         in
          let sql =
            "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) "
            ^ "SELECT "
            ^ "  ($2::text || subltree(path, 3, nlevel(path)))::ltree, "
            ^ "  'datum', type_name, "
            ^ "  jsonb_set(payload, '{path}', "
            ^ "    to_jsonb(($2::text || subltree(path, 3, nlevel(path)))::text)), "
            ^ "  hash, author, now() "
            ^ "FROM tree_nodes "
              ^ "WHERE kind = 'datum' AND path <@ $1::ltree"
              ^ type_clause
              ^ " ON CONFLICT (path) DO NOTHING RETURNING path::text"
          in
         let rows =
           Store_db.exec conn ~params sql
         in
         let n = List.length rows in
         VStr (Printf.sprintf "ok %s (%d nodes forked)" lens n)))

(* (abandon-data lens-name) -> VStr.
   Drop the data lens subtree entirely. The user's live data.u is
   untouched. Returns "ok <lens> (<n> nodes dropped)". *)
let abandon_data_lens (store : Store.t) ~(lens : string) () : value =
  let user = session_user () in
  let user_enc = Tree_store.enc_label user in
  let lens_enc = Tree_store.enc_label lens in
  let lens_prefix =
    Printf.sprintf "data.l.%s.%s" lens_enc user_enc
  in
  (with_tree_tx store
     ~none:(fun () -> VStr "abandon-data: no database connection")
     (fun conn ->
       let n = Tree_store.delete_path conn ~path:lens_prefix ~recursive:true in
       VStr (Printf.sprintf "ok %s (%d nodes abandoned)" lens n)))

(* (merge-data lens-name) -> VStr.
   Merge a data lens back into the user's live data.u.<user>. v0 policy:
   LWW per node -- the lens version overwrites the live version for every
   node in the lens. This is the simple, honest v0: no three-way diff,
   no conflict surfacing (the user deliberately forked, edited, and chose
   to merge; the lens is the intended new state). Conflict surfacing and
   three-way merge are the v1 ambition (one-tree.borg branching-interaction).

   For each lens node (data.l.<lens>.<user>.<type>.<id>), upsert the body
   into the live path (data.u.<user>.<type>.<id>), then CONSUME the lens:
   drop the data.l.<lens>.<user> subtree so the lens is gone after a
   successful merge (v0 lifecycle: fork -> diverge -> merge | abandon).
   The drop is explicit here, not left to the reverse mirror trigger:
   migration 023's tree_mirror_data_rows purge now skips data.l.* (so a
   lens survives ordinary live writes as a restore point), so merge owns
   the consume. Returns a JSON report {lens, merged, skipped, dropped}. *)
let merge_data_lens (store : Store.t) ~(lens : string) () : value =
  let user = session_user () in
  let user_enc = Tree_store.enc_label user in
  let lens_enc = Tree_store.enc_label lens in
  let lens_prefix =
    Printf.sprintf "data.l.%s.%s" lens_enc user_enc
  in
  (with_tree_tx store
     ~none:(fun () -> VStr "merge-data: no database connection")
     (fun conn ->
       (* For each lens datum, upsert into the corresponding live path.
          The live path is constructed by replacing the data.l.<lens> prefix
          with data.u — i.e. dropping the lens segment. *)
       let lens_nodes =
         Tree_store.list_subtree conn ~prefix:lens_prefix ~limit:100000 ()
       in
       let merged = ref 0 in
       let skipped = ref 0 in
       List.iter
         (fun (n : Tree_store.node) ->
           (* Reconstruct the live path: data.l.<lens>.<user>.<type>.<id>
              -> data.u.<user>.<type>.<id>. Drop the first two segments
              (data.l) and the lens segment, prepend data.u.<user>. *)
           let live_path =
             let parts = String.split_on_char '.' n.path in
             match parts with
             | "data" :: "l" :: _lens :: rest ->
                 "data.u." ^ String.concat "." rest
             | _ -> n.path
           in
           let body_json =
             match n.body with
             | `Assoc _ -> Yojson.Safe.to_string n.body
             | _ -> "{}"
           in
           let result =
             Tree_store.upsert_datum conn
               ~path:live_path
               ~type_name:
                 (match n.type_name with
                  | Some tn -> tn
                  | None -> "")
               ~body_json ~author:user ~rank:None
           in
           (match result with
            | Ok _ -> incr merged
            | Error _ -> incr skipped))
         lens_nodes;
       (* Consume the lens: drop the now-integrated data.l.<lens>.<user>
          subtree. *)
       let dropped =
         Tree_store.delete_path conn ~path:lens_prefix ~recursive:true
       in
       let report =
         `Assoc
           [ ("lens", `String lens)
           ; ("merged", `Int !merged)
           ; ("skipped", `Int !skipped)
           ; ("dropped", `Int dropped)
           ]
       in
       VStr (Yojson.Safe.to_string report)))

(* (list-data-lenses) -> VList of VStr lens names.
   Enumerate the user's data lenses by scanning for distinct data.l.<lens>.<user>
   prefixes in tree_nodes. *)
let list_data_lenses (store : Store.t) () : value =
  let user = session_user () in
  let user_enc = Tree_store.enc_label user in
  (with_tree_tx store ~none:(fun () -> VList []) (fun conn ->
        let sql =
          "SELECT DISTINCT tree_dec_label(subltree(path, 2, 3)::text) "
          ^ "FROM tree_nodes "
          ^ "WHERE kind = 'datum' AND path <@ 'data.l'::ltree "
          ^ "AND subltree(path, 3, 4) = '" ^ user_enc ^ "'::ltree "
          ^ "ORDER BY 1"
        in
       let rows = Store_db.exec conn ~params:[] sql in
       let lenses =
         List.filter_map
           (fun row ->
             match row with
             | [ v ] ->
                 (match Pgx.Value.to_string v with
                  | Some s -> Some (VStr s)
                  | None -> None)
             | _ -> None)
           rows
       in
       VList lenses))
