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

(** SQL fragment: row is visible to $N (author match OR seed type).
    Seed membership derives from the authoritative SQL predicate
    tree_is_seed_type (migrations/016_seed_single_source.sql) rather than a
    duplicated IN-list, so CRUD reads never drift from the RLS floor. *)
let visible_sql ~(user_param : int) : string =
  Printf.sprintf "(author = $%d OR tree_is_seed_type(type_name))" user_param

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
  | None ->
      raise (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.Prim_error
                  ("create", "failed: no DB", "failed")))
  | Some conn ->
    (* tree is truth; reverse trigger mirrors to data_rows *)
    let id = Tree_store.insert_datum conn ~type_name ~fields_json ~author in
    (match id with
     | Some s -> VStr s
     | None ->
         raise (Pricklypear_lisp.Ast.Interp_error
                  (Pricklypear_lisp.Ast.Prim_error
                     ("create", "failed", "failed"))))

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
    let id' = Tree_store.update_datum conn ~type_name ~id ~fields_json in
    (match id' with Some s -> VStr s | None -> VNil)

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
let update_field
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(field : string) ~(value : string)
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
    | None -> VNil
    | Some conn ->
        let user = session_user () in
        let sql =
          "SELECT fields::text FROM " ^ data_rel () ^ " " ^
          "WHERE type_name = $1 AND id = $2::uuid AND " ^
          visible_sql ~user_param:3 ^ " FOR UPDATE" in
        let rows =
          Io_lwt.Db.execute conn
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
             (match id' with Some s -> VStr s | None -> VNil)))

(** (delete 'type id) -> VStr id | VNil.
    DELETEs one row by (type_name, id). Returns the id on success
    (echo), VNil if no row matched. *)
let delete (store : Store.t) ~(type_name : string) ~(id : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
    let id' = Tree_store.delete_datum conn ~type_name ~id in
    (match id' with Some s -> VStr s | None -> VNil)

(** (list 'type [limit]) -> VList of (VStr id, VStr fields) pairs.
    With no limit, defaults to 100 (avoid OOM on a huge table).
    Each row is a 2-element VList `[id fields]` so callers can
    `(mapcar (lambda (row) (car row)) rows)` to get ids, etc. *)
let list
    (store : Store.t) ~(type_name : string) ?(limit : int = 100) ()
    : value =
  Io_lwt.Db.with_transaction store (function
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
        (VList lisp_rows))

(** (find-row 'type "field" "value") -> VStr fields-json | VNil.
    First row where fields->>field = value (text equality). *)
let find_row
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
              | _ -> VNil)))

(** (find-rows 'type "f1" "v1" "f2" "v2" ... [limit] [sort])
    -> VList of (id . fields) pairs.
    All field filters AND together (fields->>k = v text). Optional
    trailing VInt limit (default 500) and an optional trailing VStr sort
    (a raw SQL ORDER BY clause, appended verbatim). Default ORDER BY:
    created_at ASC. Callers needing domain-specific ordering (e.g. bible
    verses by verse number) must pass the sort explicitly. *)
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
            | Some s -> " ORDER BY " ^ s ^ " LIMIT "
            | None -> " ORDER BY created_at ASC LIMIT "
          in
          Buffer.add_string buf (Printf.sprintf "%s$%d" order_by !i);
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

let tree_get (store : Store.t) ~(path : string) ?(at_gen : int64 option) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
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

(* (tree/children-of path [limit]) -> VList of direct-child node JSON.
   Level-predicate read (nlevel(path)+1, + outline covering at +2 under
   literal label h), never an anti-join — one-tree.projections
   CHILDREN-BY-LEVEL. Same surface as tree/list depth 1; explicit name. *)
let tree_children_of (store : Store.t) ~(path : string)
    ?(limit : int = 50000) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes = Tree_store.list_children conn ~prefix:path ~limit () in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

(* (tree/subtree path [limit]) -> VList of all-descendant node JSON under
   path, capped at limit. Backed by list_subtree; same surface as
   (tree/list path) with no depth. Explicit bounded-snapshot name. *)
let tree_subtree (store : Store.t) ~(path : string)
    ?(limit : int = 50000) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes = Tree_store.list_subtree conn ~prefix:path ~limit () in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

(* ---- lens-aware read prims (one-tree.tree-read-prims, BORGE-1785516993) ----
   Resolve a RELATIVE logical path (e.g. "fn.notes/doc/get",
   "pkg.todo", "route./issues") against the session's branch lens chain
   (self -> parent -> ... -> system/main, closest-wins), the same rule
   function_defs use. A session on <user>/personal sees the EFFECTIVE
   override chain for tree_nodes, not just its own branch. *)

let lens_chain (store : Store.t) : string list =
  Store.branch_chain store ~branch_id:(Store.current_branch_id store)

let tree_lens_get (store : Store.t) ~(suffix : string) : value =
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let n = Tree_store.lens_get_in_chain conn ~suffix (lens_chain store) in
      (match n with
       | None -> VNil
       | Some node -> (VStr (Tree_store.node_to_json node)))

let tree_lens_children (store : Store.t) ~(suffix : string)
    ?(limit : int = 50000) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes =
        Tree_store.lens_children_in_chain conn ~suffix ~limit
          (lens_chain store)
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

let tree_lens_subtree (store : Store.t) ~(suffix : string)
    ?(limit : int = 50000) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VList [])
  | Some conn ->
      let nodes =
        Tree_store.lens_subtree_in_chain conn ~suffix ~limit
          (lens_chain store)
      in
      (VList (List.map (fun n -> VStr (Tree_store.node_to_json n)) nodes))

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
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
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
       | Some v -> (VStr (Tree_store.version_to_json v)))

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
  match Io_lwt.Db.current_conn store with
  | None -> VNil
  | Some conn ->
      let a = Tree_store.versioned_head conn ~path ~at_gen:gen_a () in
      let b = Tree_store.versioned_head conn ~path ~at_gen:gen_b () in
      (match a, b with
       | Some va, Some vb when va.Tree_store.v_hash = vb.Tree_store.v_hash ->
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
       | _ -> VNil)

(* (undo path [n]) -> restore the value at head-n as a NEW write at head+1.
   Restore-by-write (not head-pointer manipulation): the restore is itself a
   versioned append, so undo is lossless and redoable ((undo path) again on
   the restored state climbs back down the gen chain). Supports the datum
   path class (the first documented consumer is notes undo over datum
   nodes). For code paths restore-by-write is deferred to the code-restore
   consumer (consumers subsection) — this prim reports that cleanly. Returns
   the written path or an error string. *)
let tree_undo (store : Store.t) ~(path : string) ?(n : int64 = 1L) () : value =
  match Io_lwt.Db.current_conn store with
  | None -> (VStr "undo: no DB")
  | Some conn ->
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
                 (match
                    Tree_store.upsert_datum conn ~path ~type_name ~body_json
                      ~author
                  with
                  | Ok p -> (VStr p)
                  | Error e -> (VStr e)))

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
  if kind = "datum" then
    (* Datum node: run the same typed-boundary validation as create/update,
       then write via Tree_store.upsert_datum (path-explicit; generates an
       id when the path's id label is a placeholder). one-tree.tree-datum-prims. *)
    let reject = check_type_fields store ~type_name ~fields_json:body_json in
    (match reject with
     | Some msg -> (VStr msg)
     | None ->
     match Io_lwt.Db.current_conn store with
     | None -> (VStr "tree/upsert: no DB")
     | Some conn ->
       let r =
         Tree_store.upsert_datum conn ~path ~type_name ~body_json ~author
       in
       (match r with Ok p -> (VStr p) | Error e -> (VStr e)))
  else
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
    data.u.<user>.<type>.<id> | data.shared.<type>.<id>; the type segment
    is base64-label-encoded (Tree_store.enc_label). None when [path] is not
    a datum path. *)
let datum_path_type (path : string) : string option =
  let parts = String.split_on_char '.' path in
  let type_label =
    match parts with
    | "data" :: "u" :: _user :: ty :: _id :: _ -> Some ty
    | "data" :: "shared" :: ty :: _id :: _ -> Some ty
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
  match datum_move_type_reject store ~old_path ~new_path with
  | Some msg -> (VStr ("tree/move: " ^ msg))
  | None ->
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
