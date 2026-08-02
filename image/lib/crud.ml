(** Pricklypear Image — CRUD primitives over the `data_rows` table.

    get/create/update/delete/rows are normal prims returning real DB
    results. They compose:
      (define u (get 'user 42))
      (str u)

    Per spec [storage.types] / migration 003 + [one-tree.branching-interaction]
    /data DECISION (user-owned data):
      - `data_rows(id uuid PK, type_name text, fields jsonb, author text, ...)`
      - Data rows are NOT branch-versioned (no branch_id); fork/merge
        copies code, not data.
      - Visibility is USER-SCOPED: a session sees rows it authored plus
        the shared seed corpus (bible-*, strongs-entry). Prim SQL
        enforces this; RLS on tree_nodes is the floor (migration 013).
      - `fields` is JSON; Lisp sees it as a string, with dict-* prims
        for field access.
      - No ORM; parameterized SQL via pgx. Multi-step atomicity uses
        Store.with_dynamic_transaction / (with-transaction …).

    Uses [Io_lwt.Db.current_conn] so in-tx calls see the bound connection.
    Misses return VNil so Lisp can branch on `(if (get 'user id) ...)`. *)
open Pricklypear_lisp.Ast

(* Reads go through data_rows with an explicit author/seed confinement
   predicate (primary control). tree_data_rows remains available when
   PP_TREE_READS=1; the view also confines when pp.user is set. Writes
   always hit data_rows (forward mirror keeps the tree in lockstep). *)
let data_rel () = if Store.tree_reads () then "tree_data_rows" else "data_rows"

(** Session username for confinement. Prefer Request_context snapshot
    (cotenant-safe); fall back to anonymous when unbound (scripts). *)
let session_user () : string =
  match Request_context.get () with
  | Some c -> (Request_context.manifest c).username
  | None -> "anonymous"

(** SQL fragment: row is visible to $N (author match OR seed type).
    Seed list MUST match Tree_store.seed_types / tree_is_seed_type. *)
let visible_sql ~(user_param : int) : string =
  Printf.sprintf
    "(author = $%d OR type_name IN ('bible-verse','bible-book','bible-interlinear','strongs-entry'))"
    user_param

(** SQL fragment: row is writable by $N (own rows only — not seed). *)
let owned_sql ~(user_param : int) : string =
  Printf.sprintf "(author = $%d)" user_param

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

(** (deftype 'name field1 field2 ...) -> VStr "defined type <name>".
    INSERTs a row into type_defs with branch_id = session's branch and
    fields = JSON array ["field1","field2",...] (the field names in
    declared order). Per spec [storage.types]: type declarations are
    metadata; the actual data lives in data_rows (user-scoped + shared seed)
    (migration 003) keyed by (type_name, id). Type declarations may
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
   | [] -> (VStr "deftype: need at least a type name")
   | name :: fields ->
       let _ = Store.persist_type store ~name ~fields ~author in
       (VStr (Printf.sprintf "defined type %s" name)))

(** (get 'type id) -> VStr fields-json | VNil.
    Reads ONE row by (type_name, id). The id is a UUID string.
    Returns the row's `fields` JSON as a VStr so Lisp can pass it
    onward; or VNil if not found. *)
let get (store : Store.t) ~(type_name : string) ~(id : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT fields::text FROM " ^ data_rel () ^ " " ^
        "WHERE type_name = $1 AND id = $2::uuid AND " ^
        visible_sql ~user_param:3 in
      let rows =
        Io_lwt.Db.execute conn
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
            "select-for-update requires (with-transaction ...); \
             refusing FOR UPDATE outside a transaction"))
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
          Io_lwt.Db.execute conn
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

(** (create 'type "{fields-json}") -> VStr new-id | VStr "create failed".
    INSERTs a row with the given JSON fields, returning the new UUID.
    The author is the session's current username.

    Why take fields as a JSON STRING (not a Lisp value):
    - The Lisp layer doesn't yet have a dict/assoc value type.
    - Caller can build the JSON via (string-append ...) or a future
      (json ...) prim; for now both caller and DB speak JSON strings.
    - The DB validates it's well-formed JSON (jsonb column). *)
let create
    (store : Store.t) ~(type_name : string) ~(fields_json : string)
    ~(author : string)
    : value =
  let reject = check_type_fields store ~type_name ~fields_json in
  match reject with
  | Some msg -> (VStr msg)
  | None ->
  match Io_lwt.Db.current_conn store with
  | None -> (VStr "create failed: no DB")
  | Some conn ->
  if Tree_store.tree_writes () then begin
    (* one-tree P4: write the tree; reverse trigger mirrors to data_rows *)
    let id = Tree_store.insert_datum conn ~type_name ~fields_json ~author in
    (match id with Some s -> VStr s | None -> VStr "create failed")
  end else begin
      let sql =
        "INSERT INTO data_rows (type_name, fields, author) " ^
        "VALUES ($1, $2::jsonb, $3) RETURNING id::text" in
      let rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string fields_json;
                    Pgx.Value.of_string author ]
          sql
      in
      (match rows with
       | [] -> (VStr "create failed")
       | row :: _ ->
           (match row with
            | [ id_cell ] -> (value_of_id id_cell)
            | _ -> (VStr "create failed")))
  end

(** (update 'type id "{fields-json}") -> VStr id | VNil.
    UPDATE fields for ONE row by (type_name, id). Returns the id
    (echo of the row updated) on success, or VNil if no row matched. *)
let update
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(fields_json : string)
    : value =
  let reject = check_type_fields store ~type_name ~fields_json in
  match reject with
  | Some msg -> (VStr msg)
  | None ->
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
  if Tree_store.tree_writes () then begin
    let id' = Tree_store.update_datum conn ~type_name ~id ~fields_json in
    (match id' with Some s -> VStr s | None -> VNil)
  end else begin
      let user = session_user () in
      let sql =
        "UPDATE data_rows SET fields = $3::jsonb, updated_at = now() " ^
        "WHERE type_name = $1 AND id = $2::uuid AND " ^
        owned_sql ~user_param:4 ^ " " ^
        "RETURNING id::text" in
      let rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string id;
                    Pgx.Value.of_string fields_json;
                    Pgx.Value.of_string user ]
          sql
      in
      (match rows with
       | [] -> VNil
       | row :: _ ->
           (match row with
            | [ id_cell ] -> (value_of_id id_cell)
            | _ -> VNil))
  end

(** (update-field 'type id "field" "value") -> VStr id | VNil.
    Ergonomic shorthand for the v1 dict-set + update round-trip:
      (define u (get 'type id))
      (update 'type id (dict-set u "field" "value"))
    -> (update-field 'type id "field" "value")
    Composition is pure-OCaml (one get, one dict-set, one update inside
    this prim). Lwt-typed; returns VStr id on success, VNil if no row
    matched or no DB. Value is JSON-coerced (numeric strings emit as JSON
    numbers; non-parseable values emit as JSON strings — same rules as
    dict-set). NO transactional scope beyond the implicit per-statement
    pgx tx — see ``select-for-update`` caveat for why that matters for
    concurrent writers; the row is fetched and updated in two separate
    pgx statements. *)
let update_field
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(field : string) ~(value : string)
    : value =
  let current = get store ~type_name ~id in
  (match current with
   | VNil -> VNil  (* no such row *)
   | _ ->
       let json_str =
         match current with VStr s -> s | _ -> "{}"
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
       update store ~type_name ~id ~fields_json:updated_json)

(** (delete 'type id) -> VStr id | VNil.
    DELETEs one row by (type_name, id). Returns the id on success
    (echo), VNil if no row matched. *)
let delete (store : Store.t) ~(type_name : string) ~(id : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
  if Tree_store.tree_writes () then begin
    let id' = Tree_store.delete_datum conn ~type_name ~id in
    (match id' with Some s -> VStr s | None -> VNil)
  end else begin
      let user = session_user () in
      let sql =
        "DELETE FROM data_rows " ^
        "WHERE type_name = $1 AND id = $2::uuid AND " ^
        owned_sql ~user_param:3 ^ " " ^
        "RETURNING id::text" in
      let rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string id;
                    Pgx.Value.of_string user ]
          sql
      in
      (match rows with
       | [] -> VNil
       | row :: _ ->
           (match row with
            | [ id_cell ] -> (value_of_id id_cell)
            | _ -> VNil))
  end

(** (list 'type [limit]) -> VList of (VStr id, VStr fields) pairs.
    With no limit, defaults to 100 (avoid OOM on a huge table).
    Each row is a 2-element VList `[id fields]` so callers can
    `(mapcar (lambda (row) (car row)) rows)` to get ids, etc. *)
let list
    (store : Store.t) ~(type_name : string) ?(limit : int = 100) ()
    : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT id::text, fields::text FROM " ^ data_rel () ^ " " ^
        "WHERE type_name = $1 AND " ^ visible_sql ~user_param:3 ^ " " ^
        "ORDER BY created_at ASC LIMIT $2" in
      let rows =
        Io_lwt.Db.execute conn
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
      (VList lisp_rows)

(** (find-row 'type "field" "value") -> VStr fields-json | VNil.
    First row where fields->>field = value (text equality). *)
let find_row
    (store : Store.t) ~(type_name : string) ~(field : string) ~(value : string)
    : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT fields::text FROM " ^ data_rel () ^ " " ^
        "WHERE type_name = $1 AND fields->>$2 = $3 AND " ^
        visible_sql ~user_param:4 ^ " " ^
        "ORDER BY created_at ASC LIMIT 1"
      in
      let rows =
        Io_lwt.Db.execute conn
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
            | _ -> VNil))

(** (find-rows 'type "f1" "v1" "f2" "v2" ... [limit])
    -> VList of (id . fields) pairs.
    All field filters AND together (fields->>k = v text). Optional
    trailing VInt limit (default 500). Orders by created_at ASC. *)
let find_rows
    (store : Store.t) ~(type_name : string)
    ~(filters : (string * string) list) ?(limit : int = 500) ()
    : value =
  match Io_lwt.Db.current_conn store with
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
        Buffer.add_string buf
          (Printf.sprintf
             " ORDER BY COALESCE((fields->>'verse')::int, 0) ASC, created_at ASC LIMIT $%d"
             !i);
        params := !params @ [ Pgx.Value.of_int limit ];
        let rows = Io_lwt.Db.execute conn ~params:!params (Buffer.contents buf) in
        let lisp_rows =
          List.map
            (fun row ->
              match row with
              | [ id_cell; fields_cell ] ->
                  VPair (value_of_id id_cell, value_of_id fields_cell)
              | _ -> VNil)
            rows
        in
        (VList lisp_rows)

(** (find-random 'type) -> (id . fields) pair | VNil.
    One random row of the given type (for bible/random etc.). *)
let find_random (store : Store.t) ~(type_name : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let user = session_user () in
      let sql =
        "SELECT id::text, fields::text FROM " ^ data_rel () ^ " " ^
        "WHERE type_name = $1 AND " ^ visible_sql ~user_param:2 ^ " " ^
        "ORDER BY random() LIMIT 1"
      in
      let rows =
        Io_lwt.Db.execute conn
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
            | _ -> VNil))

(** (find-text 'type "field" "needle" [limit]) -> VList of (id . fields).
    Case-insensitive substring match on fields->>field. *)
let find_text
    (store : Store.t) ~(type_name : string) ~(field : string)
    ~(needle : string) ?(limit : int = 50) ()
    : value =
  match Io_lwt.Db.current_conn store with
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
        Io_lwt.Db.execute conn
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
      (VList lisp_rows)

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
        Io_lwt.Db.execute conn
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
        Io_lwt.Db.execute conn
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
        Io_lwt.Db.execute conn
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
        Io_lwt.Db.execute conn
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


(** (node-path 'type id) -> VStr path. Pure path constructor. *)
let node_path ~(type_name : string) ~(id : string) : value =
  let user = session_user () in
  VStr (Tree_store.datum_path ~user ~type_name ~id ())

(** (backlinks id-or-path [limit]) -> VList of VStr paths. *)
let backlinks (store : Store.t) ~(needle : string) ?(limit : int = 200) ()
  : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let paths = Tree_store.backlinks conn ~needle ~limit () in
      (VList (List.map (fun p -> VStr p) paths))

(** (type-fields 'type) -> VList of VStr field names | VNil if undeclared. *)
let type_fields (store : Store.t) ~(type_name : string) : value =
  let fs = Store.lookup_type_fields store ~name:type_name in
  match fs with
  | None -> VNil
  | Some fields -> (VList (List.map (fun f -> VStr f) fields))

(* ---- hierarchical tree prims (notes.tree-cutover K0) ---- *)

let tree_get (store : Store.t) ~(path : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let n = Tree_store.get_by_path conn ~path in
      (match n with
       | None -> VNil
       | Some node -> (VStr (Tree_store.node_to_json node)))

let tree_list (store : Store.t) ~(prefix : string) ?(depth : int option)
    ?(limit : int = 50000) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes =
        match depth with
        | Some 1 -> Tree_store.list_children conn ~prefix ~limit ()
        | _ -> Tree_store.list_subtree conn ~prefix ~limit ()
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

let tree_children_html (store : Store.t) ~(prefix : string) ~(garden_root : string)
    () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VStr "<p class=\"muted\">(empty)</p>")
  | Some conn ->
      let html =
        Tree_store.render_children_html conn ~prefix ~garden_root ()
      in
      (VStr html)

let tree_upsert (store : Store.t) ~(path : string) ~(kind : string)
    ~(type_name : string) ~(body_json : string) ~(author : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VStr "tree/upsert: no DB")
  | Some conn ->
      let r =
        Tree_store.upsert_hier conn ~path ~kind ~type_name ~body_json ~author
      in
      (match r with
       | Ok p -> (VStr p)
       | Error e -> (VStr e))

let tree_delete (store : Store.t) ~(path : string) ~(recursive : bool)
    : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VInt 0L)
  | Some conn ->
      let n = Tree_store.delete_path conn ~path ~recursive in
      (VInt (Int64.of_int n))

let tree_move (store : Store.t) ~(old_path : string) ~(new_path : string)
    : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VStr "tree/move: no DB")
  | Some conn ->
      let r = Tree_store.move_subtree conn ~old_path ~new_path in
      (match r with
       | Ok n -> (VInt (Int64.of_int n))
       | Error e -> (VStr e))

let tree_get_uuid (store : Store.t) ~(uuid : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let n = Tree_store.get_by_uuid conn ~uuid in
      (match n with
       | None -> VNil
       | Some node -> (VStr (Tree_store.node_to_json node)))

let tree_find_by (store : Store.t) ~(kind : string option) ~(field : string)
    ~(value : string) ?(limit : int = 50) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes =
        Tree_store.find_by_body_field conn ~kind ~field ~value ~limit ()
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

let tree_list_kind (store : Store.t) ~(prefix : string) ~(kind : string)
    ?(limit : int = 2000) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes =
        Tree_store.list_by_kind conn ~prefix ~kind ~limit ()
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

let tree_note_index (store : Store.t) ~(prefix : string) ?(limit : int = 2000)
    () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let rows = Tree_store.list_note_index conn ~prefix ~limit () in
      (VList
           (List.map
              (fun (id, fields) -> VPair (VStr id, VStr fields))
              rows))

let tree_folder_index (store : Store.t) ~(prefix : string) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let folders = Tree_store.list_folder_index conn ~prefix () in
      (VList (List.map (fun f -> VStr f) folders))

(** Substring search over tree notes+headings. Returns list of hit JSON. *)
let tree_search_text (store : Store.t) ~(prefix : string) ~(needle : string)
    ?(limit : int = 20) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let hits =
        Tree_store.search_text conn ~prefix ~needle ~limit ()
      in
      (VList (List.map (fun j -> VStr j) hits))

let tree_path_label (s : string) : value = VStr (Tree_store.path_label s)

let tree_garden_root ~(user : string) : value =
  VStr (Tree_store.garden_root ~user)
