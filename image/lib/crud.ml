(** Pricklypear Image — CRUD primitives over the `data_rows` table.

    Each of get/create/update/delete/list is a NORMAL PRIMITIVE on the
    Lwt-typed evaluator ABI (post-iteration-12 refactor): prim_fn is
    `value list -> value Lwt.t`, so these prims return their real DB
    result as a promise. They compose normally:
      (define u (get 'user 42))
      (str u)
    `(get 'note id)` returning the note as its value is what unblocked
    CRUD on the new ABI.

    Per spec [storage.types] / migration 003:
      - `data_rows(id uuid PK, type_name text, fields jsonb, author text, ...)`
      - Data rows are SHARED across branches (no branch_id column);
        fork/merge only copies code, not data. All branches see the
        same data_rows.
      - The `fields` payload is a JSON document; the Lisp layer sees
        it as a string for now. A future iteration can add a parser
        that converts to/from Lisp assoc lists or a dict value type.
      - No ORM; plain parameterized SQL via pgx. SERIALIZABLE
        transactions by default for mutations — once our pgx wrapper
        exposes BEGIN/COMMIT, wrap create/update/delete in a
        transaction. For now, each op is one statement (atomic by
        itself, multi-step ops would need wrapping).

    The store handle is shared with [Store.t]; we reuse its [Pg.t]
    via [Store.conn_of]. Returns VNil with no DB or no rows so the
    REPL prints `()` and Lisp code can do `(if (get 'user id) ...)`. *)
open Lwt.Syntax
open Pricklypear_lisp.Ast

module Pg = Store.Pg

let value_of_id cell : value =
  (* Pgx.Value.to_string returns string option; normalize to VStr/VNil *)
  match Pgx.Value.to_string cell with
  | Some s -> VStr s
  | None -> VNil

(** (deftype 'name field1 field2 ...) -> VStr "defined type <name>".
    INSERTs a row into type_defs with branch_id = session's branch and
    fields = JSON array ["field1","field2",...] (the field names in
    declared order). Per spec [storage.types]: type declarations are
    metadata; the actual data lives in the shared data_rows table
    (migration 003) keyed by (type_name, id). Type declarations may
    eventually drive field-level validation; for now they're documentation.

    The first arg is the type name, then zero or more field names —
    each as a quoted symbol (e.g. (deftype 'user 'name 'email 'age))
    so they arrive as VStr (quote of a bare symbol evaluates to VStr).
    Bare symbols would be looked up as variables and fail. *)
let deftype (store : Store.t) ~(args : value list)
    ~(author : string) : value Lwt.t =
  let field_strs =
    List.filter_map
      (fun v -> match v with VStr s -> Some s | _ -> None)
      args
  in
  (match field_strs with
   | [] -> Lwt.return (VStr "deftype: need at least a type name")
   | name :: fields ->
       let* _ = Store.persist_type store ~name ~fields ~author in
       Lwt.return (VStr (Printf.sprintf "defined type %s" name)))

(** (get 'type id) -> VStr fields-json | VNil.
    Reads ONE row by (type_name, id). The id is a UUID string.
    Returns the row's `fields` JSON as a VStr so Lisp can pass it
    onward; or VNil if not found. *)
let get (store : Store.t) ~(type_name : string) ~(id : string) : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return VNil
  | Some conn ->
      let sql =
        "SELECT fields::text FROM data_rows " ^
        "WHERE type_name = $1 AND id = $2::uuid" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string id ]
          sql
      in
      (match rows with
       | [] -> Lwt.return VNil
       | row :: _ ->
           (match row with
            | [ fields_cell ] ->
                (match Pgx.Value.to_string fields_cell with
                 | Some s -> Lwt.return (VStr s)
                 | None -> Lwt.return VNil)
            | _ -> Lwt.return VNil))

(** (select-for-update 'type id) -> VStr fields-json | VNil.
    Reads ONE row by (type_name, id) AND locks it with SELECT ... FOR
    UPDATE for the duration of the surrounding transaction.

    Per spec [storage.transactions]: "Explicit row locks:
    (select-for-update 'type id) Exposed as a primitive for finer
    control."

    IMPORTANT: This MUST be called inside a (with-transaction ...)
    block (or the prim-level equivalent) for the lock to have
    transactional scope. If called outside any transaction, pgx will
    wrap the single statement in an implicit transaction; the lock
    is released as soon as the statement returns (i.e. useless).
    Usage pattern (v1, pre-with-transaction prim — i.e. today):
      (define u (select-for-update 'user id))  ; lock acquired
      (define u2 (dict-set u "active" "false"))
      (update 'user id u2)                       ; update within tx
    When the transaction commits/rolls back, the lock is released.

    In v1 (no with-transaction prim YET), this is essentially a
    hinted-get. Improper use outside a transaction is non-fatal but
    provides no concurrency protection. A future (with-transaction ...)
    prim will give this real meaning. *)
let select_for_update
    (store : Store.t) ~(type_name : string) ~(id : string) : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return VNil
  | Some conn ->
      let sql =
        "SELECT fields::text FROM data_rows " ^
        "WHERE type_name = $1 AND id = $2::uuid " ^
        "FOR UPDATE" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string id ]
          sql
      in
      (match rows with
       | [] -> Lwt.return VNil
       | row :: _ ->
           (match row with
            | [ fields_cell ] ->
                (match Pgx.Value.to_string fields_cell with
                 | Some s -> Lwt.return (VStr s)
                 | None -> Lwt.return VNil)
            | _ -> Lwt.return VNil))

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
    : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return (VStr "create failed: no DB")
  | Some conn ->
      let sql =
        "INSERT INTO data_rows (type_name, fields, author) " ^
        "VALUES ($1, $2::jsonb, $3) RETURNING id::text" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string fields_json;
                    Pgx.Value.of_string author ]
          sql
      in
      (match rows with
       | [] -> Lwt.return (VStr "create failed")
       | row :: _ ->
           (match row with
            | [ id_cell ] -> Lwt.return (value_of_id id_cell)
            | _ -> Lwt.return (VStr "create failed")))

(** (update 'type id "{fields-json}") -> VStr id | VNil.
    UPDATE fields for ONE row by (type_name, id). Returns the id
    (echo of the row updated) on success, or VNil if no row matched. *)
let update
    (store : Store.t) ~(type_name : string) ~(id : string)
    ~(fields_json : string)
    : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return VNil
  | Some conn ->
      let sql =
        "UPDATE data_rows SET fields = $3::jsonb, updated_at = now() " ^
        "WHERE type_name = $1 AND id = $2::uuid " ^
        "RETURNING id::text" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string id;
                    Pgx.Value.of_string fields_json ]
          sql
      in
      (match rows with
       | [] -> Lwt.return VNil
       | row :: _ ->
           (match row with
            | [ id_cell ] -> Lwt.return (value_of_id id_cell)
            | _ -> Lwt.return VNil))

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
    : value Lwt.t =
  let* current = get store ~type_name ~id in
  (match current with
   | VNil -> Lwt.return VNil  (* no such row *)
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
let delete (store : Store.t) ~(type_name : string) ~(id : string) : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return VNil
  | Some conn ->
      let sql =
        "DELETE FROM data_rows " ^
        "WHERE type_name = $1 AND id = $2::uuid " ^
        "RETURNING id::text" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_string id ]
          sql
      in
      (match rows with
       | [] -> Lwt.return VNil
       | row :: _ ->
           (match row with
            | [ id_cell ] -> Lwt.return (value_of_id id_cell)
            | _ -> Lwt.return VNil))

(** (list 'type [limit]) -> VList of (VStr id, VStr fields) pairs.
    With no limit, defaults to 100 (avoid OOM on a huge table).
    Each row is a 2-element VList `[id fields]` so callers can
    `(mapcar (lambda (row) (car row)) rows)` to get ids, etc. *)
let list
    (store : Store.t) ~(type_name : string) ?(limit : int = 100) ()
    : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return (VList [])
  | Some conn ->
      let sql =
        "SELECT id::text, fields::text FROM data_rows " ^
        "WHERE type_name = $1 ORDER BY created_at ASC LIMIT $2" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name;
                    Pgx.Value.of_int limit ]
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
      Lwt.return (VList lisp_rows)

(** (find-row 'type "field" "value") -> VStr fields-json | VNil.
    First row where fields->>field = value (text equality). *)
let find_row
    (store : Store.t) ~(type_name : string) ~(field : string) ~(value : string)
    : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return VNil
  | Some conn ->
      let sql =
        "SELECT fields::text FROM data_rows " ^
        "WHERE type_name = $1 AND fields->>$2 = $3 " ^
        "ORDER BY created_at ASC LIMIT 1"
      in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name
                  ; Pgx.Value.of_string field
                  ; Pgx.Value.of_string value ]
          sql
      in
      (match rows with
       | [] -> Lwt.return VNil
       | row :: _ ->
           (match row with
            | [ fields_cell ] -> Lwt.return (value_of_id fields_cell)
            | _ -> Lwt.return VNil))

(** (find-rows 'type "f1" "v1" "f2" "v2" ... [limit])
    -> VList of (id . fields) pairs.
    All field filters AND together (fields->>k = v text). Optional
    trailing VInt limit (default 500). Orders by created_at ASC. *)
let find_rows
    (store : Store.t) ~(type_name : string)
    ~(filters : (string * string) list) ?(limit : int = 500) ()
    : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return (VList [])
  | Some conn ->
      if filters = [] then list store ~type_name ~limit ()
      else
        let buf = Buffer.create 128 in
        Buffer.add_string buf
          "SELECT id::text, fields::text FROM data_rows WHERE type_name = $1";
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
          (Printf.sprintf
             " ORDER BY COALESCE((fields->>'verse')::int, 0) ASC, created_at ASC LIMIT $%d"
             !i);
        params := !params @ [ Pgx.Value.of_int limit ];
        let* rows = Pg.execute conn ~params:!params (Buffer.contents buf) in
        let lisp_rows =
          List.map
            (fun row ->
              match row with
              | [ id_cell; fields_cell ] ->
                  VPair (value_of_id id_cell, value_of_id fields_cell)
              | _ -> VNil)
            rows
        in
        Lwt.return (VList lisp_rows)

(** (find-random 'type) -> (id . fields) pair | VNil.
    One random row of the given type (for bible/random etc.). *)
let find_random (store : Store.t) ~(type_name : string) : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return VNil
  | Some conn ->
      let sql =
        "SELECT id::text, fields::text FROM data_rows " ^
        "WHERE type_name = $1 ORDER BY random() LIMIT 1"
      in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name ]
          sql
      in
      (match rows with
       | [] -> Lwt.return VNil
       | row :: _ ->
           (match row with
            | [ id_cell; fields_cell ] ->
                Lwt.return
                  (VPair (value_of_id id_cell, value_of_id fields_cell))
            | _ -> Lwt.return VNil))

(** (find-text 'type "field" "needle" [limit]) -> VList of (id . fields).
    Case-insensitive substring match on fields->>field. *)
let find_text
    (store : Store.t) ~(type_name : string) ~(field : string)
    ~(needle : string) ?(limit : int = 50) ()
    : value Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return (VList [])
  | Some conn ->
      let sql =
        "SELECT id::text, fields::text FROM data_rows " ^
        "WHERE type_name = $1 AND fields->>$2 ILIKE $3 " ^
        "ORDER BY created_at ASC LIMIT $4"
      in
      let pat = "%" ^ needle ^ "%" in
      let* rows =
        Pg.execute conn
          ~params:[ Pgx.Value.of_string type_name
                  ; Pgx.Value.of_string field
                  ; Pgx.Value.of_string pat
                  ; Pgx.Value.of_int limit ]
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
      Lwt.return (VList lisp_rows)
