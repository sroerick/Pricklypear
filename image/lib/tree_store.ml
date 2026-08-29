(** Pricklypear Image — ONE TREE store (spec [one-tree]).

    The tree is the substrate that will replace the relational
    scaffolding (function_defs / type_defs / view_bindings / data_rows)
    behind the UNCHANGED Nopales prim ABI. This module owns:

      - the PATH GRAMMAR (ltree labels, dots not slashes),
      - the reversible label codec that MUST match migrations/009_tree.sql
        [tree_enc_label] / [tree_uuid_label] exactly,
      - the node ENVELOPE (jsonb) constructors/parsers,
      - content hashing (sha256 over the canonical body).

    P1: pure helpers + DB read/verify helpers. No behavior change yet —
    the tree is written by the SQL shadow-mirror triggers (migration 009)
    and read here for validation. P3 flips prim/store reads to this
    module; P4 makes the tree the write-truth.

    Branch inheritance is NOT baked into the path. The override-walk is
    realized by trying a name under each branch prefix from self up the
    parent chain (first hit wins). Datum paths are USER-OWNED (not branch
    lenses): data.u.<user>.<type>.<id> + data.shared.<type>.<id> per
    [one-tree.branching-interaction] /data DECISION. *)


(* ---- label codec (MUST match 009_tree.sql) ---- *)

(** ltree labels admit [A-Za-z0-9_] only. Encode arbitrary text as
    base64url-ish of the raw bytes: '+'->'_', '/'->'-', strip '=', append
    the pad count (0/1/2). Empty -> "x". Mirrors tree_enc_label in SQL. *)
let enc_label (s : string) : string =
  if s = "" then "x"
  else begin
    let b64 = Base64.encode_string ~pad:true s in
    let len = String.length b64 in
    let pad =
      if len >= 2 && String.sub b64 (len - 2) 2 = "==" then 2
      else if len >= 1 && b64.[len - 1] = '=' then 1
      else 0
    in
    let core = if pad > 0 then String.sub b64 0 (len - pad) else b64 in
    let buf = Bytes.of_string core in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | '+' -> Bytes.set buf i '_'
      | '/' -> Bytes.set buf i '-'
      | c -> ignore c
    done;
    Bytes.unsafe_to_string buf ^ string_of_int pad
  end

(** Inverse of [enc_label]. Returns None on malformed input. *)
let dec_label (s : string) : string option =
  if s = "x" then Some ""
  else begin
    let len = String.length s in
    if len < 1 then None
    else begin
      let pad_digit = s.[len - 1] in
      if pad_digit < '0' || pad_digit > '2' then None
      else begin
        let pad = Char.code pad_digit - Char.code '0' in
        let core = String.sub s 0 (len - 1) in
        let buf = Bytes.of_string core in
        for i = 0 to Bytes.length buf - 1 do
          match Bytes.get buf i with
          | '_' -> Bytes.set buf i '+'
          | '-' -> Bytes.set buf i '/'
          | c -> ignore c
        done;
        let b64 = Bytes.unsafe_to_string buf ^ String.make pad '=' in
        match Base64.decode ~pad:true b64 with
        | Ok raw -> Some raw
        | Error _ -> None
      end
    end
  end

(** UUID -> ltree-safe label (strip dashes). Mirrors tree_uuid_label. *)
let uuid_label (u : string) : string =
  String.concat "" (String.split_on_char '-' u)

(* ---- path constructors ---- *)

let fn_path ~branch_id ~name =
  Printf.sprintf "b.%s.fn.%s" (uuid_label branch_id) (enc_label name)

let type_path ~branch_id ~name =
  Printf.sprintf "b.%s.type.%s" (uuid_label branch_id) (enc_label name)

let route_path ~branch_id ~route =
  Printf.sprintf "b.%s.route.%s" (uuid_label branch_id) (enc_label route)

let pkg_path ~branch_id ~name =
  Printf.sprintf "b.%s.pkg.%s" (uuid_label branch_id) (enc_label name)

(** Seed corpus types — visible to every session under data.shared.*.
    The SERVER-side authority is the tree_is_seed_type / tree_seed_types
    SQL in migrations/016_seed_single_source.sql (the single place the
    list is defined on the DB). This OCaml array is the pure mirror needed
    by the DB-free `node-path` prim; verify_seed_types checks it against
    the SQL authority at boot so drift is caught automatically. *)
let seed_types = [ "bible-verse"; "bible-book"; "bible-interlinear"; "strongs-entry"; "guestbook_entry" ]

let is_seed_type (type_name : string) : bool =
  List.mem type_name seed_types

(** Read the authoritative seed list from SQL (tree_seed_types). *)
let db_seed_types (conn : Store_db.conn) : string list =
  try
    let rows =
      Store_db.exec conn "SELECT t FROM unnest(tree_seed_types()) AS u(t)"
    in
    List.filter_map
      (function [ c ] -> Pgx.Value.to_string c | _ -> None)
      rows
  with exn ->
    Printf.eprintf "[store] tree_seed_types() query failed: %s\n%!"
      (Printexc.to_string exn);
    []

(** Verify the OCaml mirror matches the SQL authority. Non-fatal: logs a
    warning so an out-of-date Tree_store.seed_types is surfaced without
    taking the image down. Call once at boot (Store.connect succeeded). *)
let verify_seed_types (conn : Store_db.conn) : unit =
  let sorted = List.sort String.compare in
  let db = sorted (db_seed_types conn) in
  let oml = sorted seed_types in
  if db <> oml then
    Printf.eprintf
      "[store] WARNING: OCaml seed_types [%s] != DB tree_seed_types [%s];\n\
       %s    update image/lib/tree_store.ml seed_types to match migrations/016.\n%!"
      (String.concat "," oml)
      (String.concat "," db)
      (if db = [] then "      (DB returned nothing — migrations not applied?)\n    " else "")

(** Datum path. Prefer [~user] for per-user rows; seed types always land
    under data.shared regardless of user. Without ~user, non-seed types
    use data.u.unknown — writers must pass ~user.

    [~lens] opts into the versioned data lens namespace: the path becomes
    data.l.<lens>.<user>.<type>.<id> instead of data.u.<user>.<type>.<id>.
    A data lens is a deliberate fork of the user's live data that can
    diverge and merge back (one-tree.borg branching-interaction, 2026-08-23
    make). Seed types are never lensed — data.shared is branch-free. *)
let datum_path ?user ?(lens : string option = None) ~type_name ~id () =
  let id_lab = uuid_label id in
  let ty_lab = enc_label type_name in
  if is_seed_type type_name then
    Printf.sprintf "data.shared.%s.%s" ty_lab id_lab
  else
    let u =
      match user with
      | Some s when s <> "" -> s
      | _ -> "unknown"
    in
    match lens with
    | Some l when l <> "" ->
        Printf.sprintf "data.l.%s.%s.%s.%s" (enc_label l) (enc_label u) ty_lab id_lab
    | _ ->
        Printf.sprintf "data.u.%s.%s.%s" (enc_label u) ty_lab id_lab

(* ---- content hashing ---- *)

let hash_body (canonical_body_json : string) : string =
  Auth_password.sha256_hex canonical_body_json

(* ---- node envelope ---- *)

type node = {
  id : string;
  path : string;
  kind : string;             (* fn | type | route | datum | folder | note | heading | link *)
  type_name : string option; (* datum: the Nopales type *)
  body : Yojson.Safe.t;      (* canonical body (AST / fields / route) *)
  author : string;
  doc : string option;       (* fn only *)
  hash : string;
  created_at : string;       (* row column; source of truth (not in payload) *)
  updated_at : string;       (* row column; advanced on every upsert *)
  rank : string option;      (* sibling-order: NULL = unordered (sorts last) *)
}

let envelope ~kind ~path ~type_ ~body ~author ~doc : Yojson.Safe.t =
  `Assoc [
    ("v", `Int 1);
    ("kind", `String kind);
    ("path", `String path);
    ("type", `String type_);
    ("body", body);
    ("author", `String author);
    ("doc", (match doc with Some d -> `String d | None -> `Null));
    ("hash", `String (hash_body (Yojson.Safe.to_string body)));
  ]

let node_of_row (row : Pgx.row) : node option =
  (* Expected column order: id, path, kind, type_name, payload, hash,
     author, created_at, updated_at, rank (see select_cols) *)
  match row with
  | [ id_c; path_c; kind_c; type_c; payload_c; hash_c; author_c;
      created_c; updated_c; rank_c ] ->
      (match Pgx.Value.to_string id_c,
             Pgx.Value.to_string path_c,
             Pgx.Value.to_string kind_c,
             Pgx.Value.to_string payload_c,
             Pgx.Value.to_string hash_c,
             Pgx.Value.to_string author_c,
             Pgx.Value.to_string created_c,
             Pgx.Value.to_string updated_c with
       | Some id, Some path, Some kind, Some payload_s, Some hash, Some author,
         Some created_at, Some updated_at ->
           (try
              let payload = Yojson.Safe.from_string payload_s in
              let body =
                match payload with
                | `Assoc kvs ->
                    (match List.assoc_opt "body" kvs with
                     | Some b -> b
                     | None -> `Null)
                | _ -> `Null
              in
              let doc =
                match payload with
                | `Assoc kvs ->
                    (match List.assoc_opt "doc" kvs with
                     | Some (`String d) -> Some d
                     | _ -> None)
                | _ -> None
              in
              Some { id; path; kind;
                     type_name = Pgx.Value.to_string type_c;
                     body; author; doc; hash;
                     created_at; updated_at;
                     rank = Pgx.Value.to_string rank_c }
            with Yojson.Json_error _ -> None)
       | _ -> None)
  | _ -> None

(* ---- write helpers (upsert/delete tree_nodes; the reverse trigger mirrors
   to the legacy tables in the same tx) ---- *)

let upsert_fn (conn : Store_db.conn) ~(branch_id : string) ~(name : string)
    ~(body_json : string) ~(author : string) ~(doc : string option)
    : unit =
  (* conn: Store_db.conn (tx-aware) *)
        let path = fn_path ~branch_id ~name in
      let hash = hash_body body_json in
      let sql =
        "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) " ^
        "VALUES ($1::ltree, 'fn', NULL, " ^
        "  jsonb_build_object('v',1,'kind','fn','path',$1::text,'type','fn','body',$2::jsonb, " ^
        "    'author',$3::text,'doc',$4::text,'hash',$5::text), $5::text, $3::text, now()) " ^
        "ON CONFLICT (path) DO UPDATE SET payload=EXCLUDED.payload, hash=EXCLUDED.hash, " ^
        "  author=EXCLUDED.author, updated_at=now()" in
      let doc_param = match doc with Some d -> Pgx.Value.of_string d | None -> Pgx.Value.null in
      let _ =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string body_json;
                    Pgx.Value.of_string author; doc_param; Pgx.Value.of_string hash ]
          sql
      in
      ()

let upsert_type (conn : Store_db.conn) ~(branch_id : string) ~(name : string)
    ~(fields_json : string) ~(author : string) : unit =
  (* conn: Store_db.conn (tx-aware) *)
        let path = type_path ~branch_id ~name in
      let hash = hash_body fields_json in
      let sql =
        "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) " ^
        "VALUES ($1::ltree, 'type', $2::text, " ^
        "  jsonb_build_object('v',1,'kind','type','path',$1::text,'type',$2::text,'body',$3::jsonb, " ^
        "    'author',$4::text,'doc',NULL,'hash',$5::text), $5::text, $4::text, now()) " ^
        "ON CONFLICT (path) DO UPDATE SET payload=EXCLUDED.payload, hash=EXCLUDED.hash, " ^
        "  author=EXCLUDED.author, updated_at=now()" in
      let _ =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string name;
                    Pgx.Value.of_string fields_json; Pgx.Value.of_string author;
                    Pgx.Value.of_string hash ]
          sql
      in
      ()

let upsert_route (conn : Store_db.conn) ~(branch_id : string) ~(route : string)
    ~(handler : string) ~(visibility : string) : unit =
  (* conn: Store_db.conn (tx-aware) *)
        let path = route_path ~branch_id ~route in
      let body_json =
        Yojson.Safe.to_string
          (`Assoc [ ("route", `String route); ("handler", `String handler);
                    ("visibility", `String visibility) ]) in
      let hash = hash_body body_json in
      let sql =
        "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) " ^
        "VALUES ($1::ltree, 'route', NULL, " ^
        "  jsonb_build_object('v',1,'kind','route','path',$1::text,'type','route','body',$2::jsonb, " ^
        "    'author','system','doc',NULL,'hash',$3::text), $3::text, 'system', now()) " ^
        "ON CONFLICT (path) DO UPDATE SET payload=EXCLUDED.payload, hash=EXCLUDED.hash, " ^
        "  updated_at=now()" in
      let _ =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string body_json;
                    Pgx.Value.of_string hash ]
          sql
      in
      ()

let delete_fn (conn : Store_db.conn) ~(branch_id : string) ~(name : string) : bool =
  (* conn: Store_db.conn (tx-aware) *)
        let sql = "DELETE FROM tree_nodes WHERE path = $1::ltree RETURNING path::text" in
      let rows =
        Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string (fn_path ~branch_id ~name) ] sql
      in
      (rows <> [])

let insert_datum (conn : Store_db.conn) ~(type_name : string) ~(fields_json : string)
    ~(author : string) : string option =
  (* id generated by PG; path via tree_datum_path (013) so seed vs user
     roots stay consistent with the SQL mirror. *)
  let hash = hash_body fields_json in
  let sql =
    "WITH nid AS (SELECT gen_random_uuid() AS id) " ^
    "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) " ^
"SELECT tree_datum_path($1::text, nid.id, $3::text, ''), 'datum', $1::text, " ^
    "  jsonb_build_object('v',1,'kind','datum', " ^
"    'path', tree_datum_path($1::text, nid.id, $3::text, '')::text, " ^
    "    'type',$1::text,'body',$2::jsonb,'author',$3::text,'doc',NULL,'hash',$4::text), " ^
    "  $4::text, $3::text, now() " ^
    "FROM nid RETURNING tree_datum_id(path)::text" in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string type_name; Pgx.Value.of_string fields_json;
                Pgx.Value.of_string author; Pgx.Value.of_string hash ]
      sql
  in
  (match rows with
   | [ [ id_c ] ] -> Pgx.Value.to_string id_c
   | _ -> None)

let update_datum (conn : Store_db.conn) ~(type_name : string) ~(id : string)
    ~(fields_json : string) : string option =
  (* Match by datum id (last path label) so old and new path shapes both work,
       but constrain to LIVE datum paths (exclude the data.l namespace). A data lens fork
     shadows the same (type,id) under data.l.<lens>.<user>; without this guard
     the UPDATE hits both live and lens nodes, RETURNING yields 2 rows, the
     single-row match returns None (prim reports failure), and the forward
     mirror trigger (tree_mirror_data_rows) purges the lens copy. With the
     guard the lens keeps its snapshot body and survives until merge/abandon.
     Take the first RETURNING row as a safety net against stale live paths. *)
  let hash = hash_body fields_json in
  let sql =
    "UPDATE tree_nodes SET " ^
    "  payload = jsonb_set(jsonb_set(payload,'{body}',$2::jsonb),'{hash}',to_jsonb($3::text)), " ^
    "  hash = $3::text, updated_at = now() " ^
    "WHERE kind='datum' AND tree_datum_id(path) = $1::uuid " ^
    "  AND (type_name = $4 OR type_name IS NULL) " ^
    "  AND NOT (path <@ 'data.l'::ltree) " ^
    "RETURNING tree_datum_id(path)::text" in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string id; Pgx.Value.of_string fields_json;
                Pgx.Value.of_string hash; Pgx.Value.of_string type_name ]
      sql
  in
  (match rows with
   | [ id_c ] :: _ -> Pgx.Value.to_string id_c
   | _ -> None)

let delete_datum (conn : Store_db.conn) ~(type_name : string) ~(id : string) : string option =
  (* Constrain to LIVE datum paths (exclude the data.l namespace) for the same reason as
     update_datum: don't delete a lens copy that shadows the live row. *)
  let sql =
    "DELETE FROM tree_nodes WHERE kind='datum' AND tree_datum_id(path) = $1::uuid " ^
    "  AND (type_name = $2 OR type_name IS NULL) " ^
    "  AND NOT (path <@ 'data.l'::ltree) " ^
    "RETURNING tree_datum_id(path)::text" in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string id; Pgx.Value.of_string type_name ]
      sql
  in
  (match rows with
   | [ id_c ] :: _ -> Pgx.Value.to_string id_c
   | _ -> None)

(** Upsert a datum node at [path] (kind="datum"). When the path's id
    label is a placeholder (empty, "_", or "new"), generate a fresh uuid
    via gen_random_uuid() and build the path with tree_datum_path the
    way insert_datum does — so a caller can create a new datum node by
    path without pre-allocating an id. Otherwise the caller-supplied
    full path is used (data.u.<user>.<type>.<id> or data.shared.<type>.<id>).
    Typed-boundary validation runs in the caller (crud.ml check_type_fields);
    this function is the SQL write. ON CONFLICT (path) DO UPDATE the
    body/hash/author so the same prim upserts and updates. The reverse
    mirror (tree_from_nodes) syncs data_rows. Returns Ok path | Error msg. *)
let upsert_datum (conn : Store_db.conn) ~(path : string) ~(type_name : string)
    ~(body_json : string) ~(author : string) ~(rank : string option)
    : (string, string) result =
  let last_label =
    let parts = String.split_on_char '.' path in
    match List.rev parts with [] -> "" | l :: _ -> l
  in
  let is_placeholder =
    last_label = "" || last_label = "_" || last_label = "new"
  in
  let hash = hash_body body_json in
  let run_sql ~params sql =
    try
      let rows = Io_lwt.Db.execute conn ~params sql in
      (match rows with
       | [ [ p ] ] ->
           (match Pgx.Value.to_string p with
            | Some s -> Ok s
            | None -> Error "upsert_datum: no path returned")
       | _ -> Error "upsert_datum: failed")
    with exn -> Error ("upsert_datum: " ^ Printexc.to_string exn)
  in
  if is_placeholder then
    (* Generate uuid + path via tree_datum_path(type_name, id, author).
       The generated path is always ltree-safe; no valid_path check on
       the caller's placeholder path. *)
    if type_name = "" then Error "upsert_datum: empty type_name"
    else
      let sql =
        "WITH nid AS (SELECT gen_random_uuid() AS id) " ^
        "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, rank, updated_at) " ^
"SELECT tree_datum_path($1::text, nid.id, $3::text, ''), 'datum', $1::text, " ^
        "  jsonb_build_object('v',1,'kind','datum', " ^
"    'path', tree_datum_path($1::text, nid.id, $3::text, '')::text, " ^
        "    'type',$1::text,'body',$2::jsonb,'author',$3::text,'doc',NULL,'hash',$4::text), " ^
        "  $4::text, $3::text, $5::text, now() " ^
        "FROM nid ON CONFLICT (path) DO UPDATE SET " ^
        "  payload = EXCLUDED.payload, hash = EXCLUDED.hash, " ^
        "  author = EXCLUDED.author, " ^
        "  rank = COALESCE(EXCLUDED.rank, tree_nodes.rank), " ^
        "  updated_at = now() " ^
        "RETURNING path::text"
      in
      run_sql ~params:
        [ Pgx.Value.of_string type_name; Pgx.Value.of_string body_json;
          Pgx.Value.of_string author; Pgx.Value.of_string hash;
          (match rank with Some r -> Pgx.Value.of_string r | None -> Pgx.Value.null) ]
        sql
  else if type_name = "" then Error "upsert_datum: empty type_name"
  else
    let sql =
      "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, rank, updated_at) " ^
      "VALUES ($1::ltree, 'datum', $2::text, " ^
      "  jsonb_build_object('v',1,'kind','datum','path',$1::text,'type',$2::text, " ^
      "    'body',$3::jsonb,'author',$4::text,'doc',NULL,'hash',$5::text), " ^
      "  $5::text, $4::text, $6::text, now()) " ^
      "ON CONFLICT (path) DO UPDATE SET " ^
      "  payload = EXCLUDED.payload, hash = EXCLUDED.hash, " ^
      "  author = EXCLUDED.author, " ^
      "  rank = COALESCE(EXCLUDED.rank, tree_nodes.rank), " ^
      "  updated_at = now() " ^
      "RETURNING path::text"
    in
    run_sql ~params:
      [ Pgx.Value.of_string path; Pgx.Value.of_string type_name;
        Pgx.Value.of_string body_json; Pgx.Value.of_string author;
        Pgx.Value.of_string hash;
        (match rank with Some r -> Pgx.Value.of_string r | None -> Pgx.Value.null) ]
      sql

(** Validate [fields_json] against a declared type's field names.
    Rules (one-tree.typed-nodes / object-model.type-system v1):
      - fields_json must be a JSON object
      - every declared field must be present as a key (value may be null/"")
      - extra keys ARE allowed (additive tolerance; branching.schema-authority)
    Returns Ok () or Error message. Pure — no DB. *)
let validate_fields ~(declared : string list) (fields_json : string)
  : (unit, string) result =
  if declared = [] then Ok ()
  else
    try
      match Yojson.Safe.from_string fields_json with
      | `Assoc kvs ->
          let missing =
            List.filter (fun f -> not (List.mem_assoc f kvs)) declared
          in
          if missing = [] then Ok ()
          else
            Error
              ("missing fields: " ^ String.concat ", " missing
               ^ " (declared: " ^ String.concat ", " declared ^ ")")
      | _ -> Error "fields must be a JSON object"
    with Yojson.Json_error msg -> Error ("invalid JSON: " ^ msg)

(** Backlinks: datum nodes whose fields JSON has any value equal to [needle]
    (typically a uuid id or a tree path). Uses jsonb_each_text over body.
    Caps at [limit] (default 200). *)
let backlinks (conn : Store_db.conn) ~(needle : string) ?(limit : int = 200)
    () : string list =
  let sql =
    "SELECT n.path::text FROM tree_nodes n, " ^
    "LATERAL jsonb_each_text(COALESCE(n.payload->'body', '{}'::jsonb)) e " ^
    "WHERE n.kind = 'datum' AND e.value = $1::text " ^
    "ORDER BY n.path::text ASC LIMIT $2" in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string needle; Pgx.Value.of_int limit ]
      sql
  in
  (List.filter_map
       (function
         | [ c ] -> Pgx.Value.to_string c
         | _ -> None)
       rows)

(** Recompute tree_branches.head_hash for [branch_id] as the sha256 of the
    sorted concatenation of own-node hashes under b.<uuidhex>. Empty lens →
    empty string. Used after fork (empty) and after promote/merge batches. *)
let recompute_head_hash (conn : Store_db.conn) ~(branch_id : string) : unit =
  let root = "b." ^ uuid_label branch_id in
  let sql =
    "UPDATE tree_branches SET head_hash = COALESCE(( " ^
    "  SELECT encode(digest(string_agg(hash, '' ORDER BY path::text), 'sha256'), 'hex') " ^
    "  FROM tree_nodes WHERE path <@ $1::ltree AND nlevel(path) > 1 " ^
    "), '') WHERE branch_id = $2::uuid" in
  let _ =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string root; Pgx.Value.of_string branch_id ]
      sql
  in
  ()

(* ---- DB helpers (read/verify; mirror writes happen in SQL triggers) ---- *)

let select_cols =
  "id::text, path::text, kind, type_name, payload::text, hash, author, "
  ^ "created_at::text, updated_at::text, rank"

(** Fetch one node by exact path. *)
let get_by_path (conn : Store_db.conn) ~(path : string) : node option =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes WHERE path = $1::ltree" in
      let rows =
        Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string path ] sql
      in
      (match rows with
       | row :: _ -> (node_of_row row)
       | [] -> None)

(** A versioned-leaf row (tree-time: one version row per (path, gen) write).
    Field names are v_-prefixed to avoid clashing with the `node` record. *)
type version = {
  v_gen : int64 option;
  v_path : string;
  v_kind : string;
  v_type_name : string option;
  v_body : Yojson.Safe.t;
  v_hash : string option;
  v_author : string option;
  v_created_at : string option;
  v_tombstone : bool;
  v_rank : string option;      (* sibling-order: rank in force at that gen *)
}

let version_of_row (row : Pgx.row) : version option =
  match row with
  | [ gen_c; path_c; kind_c; type_c; body_c; hash_c; author_c;
      created_c; tomb_c; rank_c ] ->
      let body =
        match Pgx.Value.to_string body_c with
        | Some s -> (try Some (Yojson.Safe.from_string s) with _ -> None)
        | None -> None
      in
      let tomb =
        match Pgx.Value.to_string tomb_c with
        | Some t -> t = "true"
        | None -> false
      in
      (match body with
       | None -> None
       | Some body ->
           Some
             { v_gen = (match Pgx.Value.to_int64 gen_c with Some g -> Some g | None -> None);
               v_path = (match Pgx.Value.to_string path_c with Some p -> p | None -> "");
               v_kind = (match Pgx.Value.to_string kind_c with Some k -> k | None -> "");
               v_type_name = Pgx.Value.to_string type_c;
               v_body = body;
               v_hash = Pgx.Value.to_string hash_c;
               v_author = Pgx.Value.to_string author_c;
               v_created_at = Pgx.Value.to_string created_c;
               v_tombstone = tomb;
               v_rank = Pgx.Value.to_string rank_c })
  | _ -> None

(** Newest versioned row at head (or as-of [at_gen]) for [path], matching
    migration 018's tree_versions_head semantics. None = path never versioned.
    [path] encodes the branch for code paths (b + branch + suffix) and is
    globally unique, so a path-only lookup serves both code and data classes
    without a branch_id argument. This is the versioned read surface 5.1's
    shadow test compares against the live tree_nodes head, and 5.2
    reads-as-of-gen drives the gen filter through. *)
let versioned_head (conn : Store_db.conn) ~(path : string)
    ?(at_gen : int64 option) () : version option =
  let sql =
    "SELECT v.gen, v.path::text, v.kind, v.type_name, v.body::text, v.hash, "
    ^ "v.author, v.created_at::text, v.body->>'_tombstone'::text, v.rank "
    ^ "FROM tree_versions v WHERE v.path = $1::ltree "
    ^ "AND ($2::bigint IS NULL OR v.gen <= $2::bigint) "
    ^ "ORDER BY v.gen DESC LIMIT 1"
  in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string path
               ; (match at_gen with Some g -> Pgx.Value.of_int64 g | None -> Pgx.Value.null) ]
      sql
  in
  (match rows with
   | row :: _ -> (version_of_row row)
   | [] -> None)

(** All version rows for [path], oldest gen first (append chain). *)
let versioned_history (conn : Store_db.conn) ~(path : string) () : version list =
  let sql =
    "SELECT v.gen, v.path::text, v.kind, v.type_name, v.body::text, v.hash, "
    ^ "v.author, v.created_at::text, v.body->>'_tombstone'::text, v.rank "
    ^ "FROM tree_versions v WHERE v.path = $1::ltree ORDER BY v.gen ASC"
  in
  let rows =
    Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string path ] sql
  in
  (List.filter_map version_of_row rows)


(** Subtree listing: all nodes under a path prefix (ltree descendant). *)
let list_subtree (conn : Store_db.conn) ~(prefix : string)
    ?(limit : int = 1000) () : node list =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes "
        ^ "WHERE path <@ $1::ltree "
        (* sibling-order: ranked siblings first (byte-order rank), then
           unranked, path-ordered among themselves. NULL rank = unordered. *)
        ^ "ORDER BY rank COLLATE \"C\" NULLS LAST, path::text ASC LIMIT $2" in
      let rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ] sql
      in
      (List.filter_map node_of_row rows)

(** All datum nodes of a Nopales type (the (rows 't) backing in P3). *)
let list_data_by_type (conn : Store_db.conn) ~(type_name : string)
    ?(limit : int = 100) () : node list =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes " ^
        "WHERE kind = 'datum' AND type_name = $1 " ^
        "ORDER BY created_at ASC LIMIT $2" in
      let rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string type_name; Pgx.Value.of_int limit ] sql
      in
      (List.filter_map node_of_row rows)

(** Branch-chain resolution: try [name] under self, then each ancestor.
    Returns the first hit (closest-ancestor-wins = the override rule).
    [branch_chain] is ordered self-first (depth ASC), as store.branch
    chain produces. *)
let rec resolve_in_chain (conn : Store_db.conn) ~(mk_path : string -> string)
    (chain : string list) : node option =
  match chain with
  | [] -> None
  | branch_id :: rest ->
      let n = get_by_path conn ~path:(mk_path branch_id) in
      (match n with
       | Some _ -> n
       | None -> resolve_in_chain conn ~mk_path rest)

(** Lens prefixing for a RELATIVE logical path. The store-tree path grammar
    uses b.<branch_id>.<rest> for branch-lensed code nodes (fn/type/route/
    pkg). A lens-aware read takes a logical suffix of the form
    <kind>.<name> (e.g. "fn.notes/doc/get", "route./issues", "pkg.todo")
    and materializes the full path under a given branch id by ENCODING the
    name segment with the label codec (same as fn_path/type_path/route_path/
    pkg_path) — the kind label itself is already ltree-safe. Returns None
    for an empty/malformed suffix (no kind). *)
let lens_suffix_path ~(branch_id : string) (suffix : string) : string option =
  if suffix = "" then None
  else
    let kind, nl =
      match String.index_opt suffix '.' with
      | Some i when i > 0 ->
          (String.sub suffix 0 i, String.sub suffix (i + 1) (String.length suffix - i - 1))
      | _ -> (suffix, "")
    in
    if nl = "" then None
    else Some ("b." ^ uuid_label branch_id ^ "." ^ kind ^ "." ^ enc_label nl)

(** Lens-aware read of one node by a RELATIVE logical path, resolved across
    [chain] (self-first / closest-wins — the override rule). Tries the
    suffix under each branch id in order; first hit wins. Backed by
    get_by_path so created_at/updated_at are present via node_to_json.
    Returns the closest node or None if no branch in the chain has it. *)
let lens_get_in_chain (conn : Store_db.conn) ~(suffix : string)
    (chain : string list) : node option =
  let mk_path branch_id =
    match lens_suffix_path ~branch_id suffix with
    | Some p -> p
    | None -> "" (* empty suffix can never match a real ltree node *)
  in
  resolve_in_chain conn ~mk_path chain

(** Reads-as-of-gen across the branch lens chain: like lens_get_in_chain, but
    returns the VERSIONED leaf (node body at head or as-of [at_gen]) for the
    EFFECTIVE path — the closest-ancestor-wins branch that carries the
    suffix. Spec tree-time.reads-as-of-gen: a gen-as-of read "crosses the
    same branch lens rules as head reads". None = no branch in the chain
    carries the suffix at all; the returned version may still be a TOMBSTONE
    (existed-then-deleted), which the caller distinguishes via v_tombstone. *)
let rec lens_versioned_in_chain (conn : Store_db.conn)
    ~(at_gen : int64 option) ~(mk_path : string -> string)
    (chain : string list) : version option =
  let read_opt path =
    match at_gen with
    | Some g -> versioned_head conn ~path ~at_gen:g ()
    | None -> versioned_head conn ~path ()
  in
  match chain with
  | [] -> None
  | branch_id :: rest ->
      let p = mk_path branch_id in
      if p = "" then lens_versioned_in_chain conn ~at_gen ~mk_path rest
      else
        (match read_opt p with
         | Some v -> Some v
         | None -> lens_versioned_in_chain conn ~at_gen ~mk_path rest)

(** Consistency check against the SQL tree_verify() function.
    Returns (kind, table_count, tree_count, in_sync) rows. *)
let verify (conn : Store_db.conn) : (string * int * int * bool) list =
  (* conn: Store_db.conn (tx-aware) *)
        let rows = Io_lwt.Db.execute conn "SELECT * FROM tree_verify()" in
      (List.filter_map
           (fun row ->
             match row with
             | [ k; tc; xc; sy ] ->
                 (match Pgx.Value.to_string k,
                        Pgx.Value.to_int tc,
                        Pgx.Value.to_int xc,
                        Pgx.Value.to_bool sy with
                  | Some k, Some tc, Some xc, Some sy -> Some (k, tc, xc, sy)
                  | _ -> None)
             | _ -> None)
           rows)

(* ---- hierarchical notes helpers (notes.storage / notes.tree-cutover K0) ----

   folder | note | heading are first-class tree kinds. They are NOT kind=datum
   and must never use data.<type>.<uuid> paths — reverse-mirror only handles
   fn/type/route/datum. Path labels are ltree-safe: [A-Za-z0-9_] only.
   Readable slug policy: path_label lowercases and maps non [a-z0-9] to _.
   Original slug (with hyphens) lives in payload body when needed. *)

(** Sanitize one path segment for ltree. Empty -> "x". *)
let path_label (s : string) : string =
  let b = Buffer.create (String.length s) in
  let last_us = ref false in
  String.iter
    (fun c ->
      let c =
        if c >= 'A' && c <= 'Z' then Char.chr (Char.code c + 32) else c
      in
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then begin
        Buffer.add_char b c;
        last_us := false
      end else if not !last_us then begin
        Buffer.add_char b '_';
        last_us := true
      end)
    s;
  let out = Buffer.contents b in
  let out =
    let len = String.length out in
    let start = if len > 0 && out.[0] = '_' then 1 else 0 in
    let stop =
      if len > start && out.[len - 1] = '_' then len - 1 else len
    in
    if stop <= start then "" else String.sub out start (stop - start)
  in
  if out = "" then "x" else out

let allowed_hier_kinds = [ "folder"; "note"; "heading" ]

let is_hier_kind (k : string) : bool =
  List.mem k allowed_hier_kinds

(** Validate a full dotted path: every label must be ltree-safe. *)
let valid_path (path : string) : bool =
  path <> ""
  &&
  List.for_all
    (fun lab ->
      lab <> ""
      && String.for_all
           (fun c ->
             (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
             || (c >= '0' && c <= '9') || c = '_')
           lab)
    (String.split_on_char '.' path)

let garden_root ~user =
  Printf.sprintf "data.note.u.%s" (path_label user)

(* ---- path algebra (one-tree.tree-path-prims; pure, no DB) ----

   Kernel path constructors/accessors over the ltree textual form — the
   ONE canonical set, replacing per-package string hacks (notes/tree/*,
   todo/tree/*, tree/breadcrumb-parts). Label semantics for APPEND are
   identical to [path_label] for labels that need sanitizing; labels that
   are ALREADY valid ltree labels pass through untouched, so
   (tree/child (tree/parent p) (tree/leaf p)) = p for every stored path
   (including uppercase base64url-encoded fn/type/route labels, which
   [path_label] alone would lowercase-mangle). Invalid paths raise
   Invalid_argument — the prim layer converts that to a Prim_error;
   nothing here guesses. *)

(** True when [s] is already an ltree-safe label ([A-Za-z0-9_]+). *)
let is_ltree_label (s : string) : bool =
  s <> ""
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9') || c = '_')
       s

(** Labels of a path; raises Invalid_argument on an invalid path. *)
let path_labels_exn (path : string) : string list =
  if not (valid_path path) then invalid_arg ("invalid path: " ^ path)
  else String.split_on_char '.' path

(** Strict parent path; None at a root (single-label) path. *)
let path_parent (path : string) : string option =
  match path_labels_exn path with
  | [ _ ] -> None
  | labels ->
      Some (String.concat "." (List.rev (List.tl (List.rev labels))))

(** Final label. *)
let path_leaf (path : string) : string =
  match List.rev (path_labels_exn path) with
  | last :: _ -> last
  | [] -> invalid_arg "invalid path: empty" (* unreachable *)

(** Every label, root-first. *)
let path_segments (path : string) : string list = path_labels_exn path

(** Append [label] to a valid path. A label that is already an ltree
    label is used verbatim; anything else is sanitized via [path_label]
    (identical encoding to tree/path-label). *)
let path_child (path : string) (label : string) : string =
  let labels = path_labels_exn path in
  let lab = if is_ltree_label label then label else path_label label in
  String.concat "." (labels @ [ lab ])

(** Every STRICT prefix, root-first: "a.b.c" -> ["a"; "a.b"]. A root
    path has no strict prefixes ([]). *)
let path_ancestors (path : string) : string list =
  let labels = path_labels_exn path in
  let rec go acc prefix = function
    | [] -> List.rev acc
    | [ _ ] -> List.rev acc (* full path excluded: strict prefixes only *)
    | x :: rest ->
        let prefix = if prefix = "" then x else prefix ^ "." ^ x in
        go (prefix :: acc) prefix rest
  in
  go [] "" labels

(* ---- sibling-order rank (one-tree.sibling-order) ----

   Fractional indexing over base-36 [0-9a-z], compared as raw bytes
   (ORDER BY rank COLLATE "C") — the same alphabet byte order in every
   locale. A rank is any nonempty [0-9a-z]+ string (the spec's examples
   "a0", "a1", "a1V" included); allocated ranks additionally never end
   in '0', which keeps byte order == the fractional-value order for
   everything the allocator produces. Allocation is midpoint-between-
   neighbours; when a generated rank would exceed [max_rank_len] — or a
   degenerate value-aliased pair leaves no room (b = a ^ "0"*k) — the
   whole sibling set is rebalanced onto fresh fixed-width ranks.
   Write-path detail — not visible in the prim ABI. *)

let max_rank_len = 12

let rank_alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"

(** Rank check: nonempty [0-9a-z]+. *)
let rank_valid (s : string) : bool =
  let n = String.length s in
  n > 0
  && String.for_all
       (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z'))
       s

let rank_digit_of (c : char) : int =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'z' then Char.code c - Char.code 'a' + 10
  else invalid_arg ("rank: digit out of alphabet: " ^ String.make 1 c)

let rank_char_of (d : int) : char = rank_alphabet.[d]

(** Strict midpoint between distinct ranks a < b (byte order). The result
   is strictly between them (and canonical). Returns "" only for the
   degenerate value-aliased pair (b = a ^ "0"*k): no rank exists between,
   the caller must rebalance the sibling set. *)
let rank_midpoint (a : string) (b : string) : string =
  if not (rank_valid a && rank_valid b) then
    invalid_arg "rank_midpoint: non-canonical rank";
  if String.compare a b >= 0 then invalid_arg "rank_midpoint: need a < b";
  let len_a = String.length a and len_b = String.length b in
  let exception Degenerate in
  let buf = Buffer.create (max len_a len_b + 3) in
  let rec go i =
    if i >= len_a then begin
      (* a is a byte-prefix of b: emit a canonical Y strictly below b's
         remainder R. R = leading '0's then a digit d > '0', so: Y = those
         '0's followed by pred(d) — or "01" when d = 1 (pred '0' may not
         terminate a rank; "0"+anything is still < "1..."). When R is ALL
         zeros (b = a ^ "0"*k, value-aliased with a) no rank exists
         between the two: signal the caller to rebalance. *)
      let rec zeros j = if j < len_b && b.[j] = '0' then zeros (j + 1) else j in
      let j = zeros i in
      if j >= len_b then raise Degenerate;
      let d = rank_digit_of b.[j] in
      for _k = i to j - 1 do Buffer.add_char buf '0' done;
      if d = 1 then Buffer.add_string buf "01"
      else Buffer.add_char buf (rank_char_of (d - 1))
    end
    else if i >= len_b then
      (* b is a byte-prefix of a -> byte order a > b: excluded above. *)
      invalid_arg "rank_midpoint: b prefix of a"
    else begin
      let ca = a.[i] and cb = b.[i] in
      if ca = cb then begin
        Buffer.add_char buf ca;
        go (i + 1)
      end
      else if rank_digit_of cb - rank_digit_of ca >= 2 then
        (* room at the first differing position: one midpoint digit *)
        Buffer.add_char buf
          (rank_char_of
             (rank_digit_of ca + (rank_digit_of cb - rank_digit_of ca) / 2))
      else begin
        (* adjacent digits: take ca, then any suffix past a's remainder
           ('h' terminates the result canonically) *)
        Buffer.add_char buf ca;
        Buffer.add_string buf (String.sub a (i + 1) (len_a - i - 1));
        Buffer.add_char buf 'h'
      end
    end
  in
  match go 0 with
  | exception Degenerate -> ""
  | () -> Buffer.contents buf

(** Rank strictly below every existing rank (before-first). [first] is
   the smallest ranked sibling's rank ("" = none ranked yet). None means
   the caller must rebalance the sibling set first. *)
let rank_before (first : string) : string option =
  if first = "" then Some "a"
  else if String.compare first "0" <= 0 then None (* "0" is the floor *)
  else Some (rank_midpoint "0" first)

(** Rank strictly above every existing rank (append / after-last).
   [last] is the largest ranked sibling's rank ("" = none ranked yet). *)
let rank_after (last : string) : string =
  if last = "" then "a"
  else
    (try rank_midpoint last "zzzzzzzzzzzz"
     with Invalid_argument _ -> last ^ "h")

(** Fresh fixed-width ranks for a sibling set of [n]: base-36 (i+1)
   zero-filled to a common width (fixed width orders correctly as plain
   text). Starts at 1 so the "0" floor sentinel is never allocated. *)
let rank_rebalance (n : int) : string list =
  let rec base36 x acc =
    if x = 0 then acc
    else base36 (x / 36) (rank_char_of (x mod 36) :: acc)
  in
  let digits_of x =
    match base36 x [] with [] -> "0" | l -> String.concat "" (List.map (String.make 1) l)
  in
  let width = max 1 (String.length (digits_of (max 1 n))) in
  let zfill i =
    let ds = digits_of i in
    let pad = String.make (width - String.length ds) '0' in
    pad ^ ds
  in
  List.init (max 0 n) (fun i -> zfill (i + 1))


let node_to_json (n : node) : string =
  let body = n.body in
  Yojson.Safe.to_string
    (`Assoc
       [ ("id", `String n.id)
       ; ("path", `String n.path)
       ; ("kind", `String n.kind)
       ; ( "type"
         , match n.type_name with Some t -> `String t | None -> `String n.kind )
       ; ("body", body)
       ; ("author", `String n.author)
       ; ("hash", `String n.hash)
       ; ("created_at", `String n.created_at)
       ; ("updated_at", `String n.updated_at)
       ; ("rank", match n.rank with Some r -> `String r | None -> `Null)
       ])

(** Render a [version] row as the JSON the reads-as-of-gen prims return.
    Mirrors node_to_json's field vocabulary (id/path/kind/type/body/author/
    hash/created_at) so a gen-as-of read has the SAME surface as a head
    tree/get, plus the [gen] coordinate the read resolved at and a
    [tombstone] flag. id/path/kind/type/author/hash are pulled from the
    version envelope body when present (it is the 019 trigger's
    {v,kind,path,type,body,author,doc,hash} node envelope); created_at.
    None values become JSON null. A tombstone row is still returned (its
    body is the reserved marker) so callers can distinguish
    "never existed" (None, prim returns nil) from "existed but deleted by
    gen G" (present row, tombstone=true). *)
let version_to_json (v : version) : string =
  let assoc = match v.v_body with `Assoc a -> a | _ -> [] in
  let get k (d : Yojson.Safe.t) =
    match List.assoc_opt k assoc with Some x -> x | None -> d
  in
  let os k d =
    match List.assoc_opt k assoc with
    | Some (`String s) -> `String s | Some x -> x | None -> d
  in
  let gen =
    match v.v_gen with Some g -> `String (Int64.to_string g) | None -> `Null
  in
  Yojson.Safe.to_string
    (`Assoc
       [ ("id", os "id" gen)
       ; ("path", os "path" (`String v.v_path))
       ; ("kind", os "kind" (`String v.v_kind))
       ; ("type", os "type" (`Null))
       ; ("body", get "body" (`Null))
       ; ("author", os "author" (`Null))
       ; ("hash", os "hash" (`Null))
       ; ("created_at",
          match v.v_created_at with Some c -> `String c | None -> `Null)
       ; ("gen", gen)
       ; ("tombstone", `Bool v.v_tombstone)
       ; ("rank",
          match v.v_rank with Some r -> `String r | None -> `Null)
       ])

(** Convert a versioned-leaf row into a [node] so the gen-as-of surface can
    reuse tree/get's node JSON shape when the caller passes an explicit :gen.
    The envelope body carries {v,kind,path,type,body,author,doc,hash}; rows
    that predate the version-era or are TOMBSTONEs have no live body and map
    to None (so the read surfaces as „missing at that gen“ the same way a
    head get of an absent path does). *) 
let version_of_leaf (v : version) : node option =
  let assoc = match v.v_body with `Assoc a -> a | _ -> [] in
  let os k d =
    match List.assoc_opt k assoc with
    | Some (`String s) -> s | Some _ -> d | None -> d
  in
  let body =
    match List.assoc_opt "body" assoc with
    | Some (`Assoc _ as b) -> b
    | Some b -> b
    | None -> `Assoc []
  in
  if v.v_tombstone then None
  else if body = `Assoc [] then None
  else
    Some
      { id = os "id" v.v_path
      ; path = os "path" v.v_path
      ; kind = os "kind" v.v_kind
      ; type_name = v.v_type_name
      ; body
      ; author = os "author" ""
      ; doc = None
      ; hash = os "hash" (match v.v_hash with Some h -> h | None -> "")
      ; created_at =
          (match v.v_created_at with Some c -> c | None -> os "created_at" "")
      ; updated_at =
          (match v.v_created_at with Some c -> c | None -> os "created_at" "")
      ; rank = v.v_rank
      }

let parse_body_json (fields_json : string) : (Yojson.Safe.t, string) result =
  try Ok (Yojson.Safe.from_string fields_json)
  with Yojson.Json_error e -> Error ("bad body json: " ^ e)

let body_string_field (body : Yojson.Safe.t) (field : string) : string option =
  match body with
  | `Assoc kvs ->
      (match List.assoc_opt field kvs with
       | Some (`String s) when s <> "" -> Some s
       | Some (`Int i) -> Some (string_of_int i)
       | _ -> None)
  | _ -> None

let string_contains hay needle =
  let hlen = String.length hay and nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub hay i nlen = needle then true
    else loop (i + 1)
  in
  nlen = 0 || loop 0

(** Lookup by payload body.uuid (notes identity). *)
let get_by_uuid (conn : Store_db.conn) ~(uuid : string) : node option =
  let sql =
    "SELECT " ^ select_cols ^ " FROM tree_nodes "
    ^ "WHERE payload #>> '{body,uuid}' = $1 LIMIT 1"
  in
  let rows =
    Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string uuid ] sql
  in
  match rows with
  | row :: _ -> (node_of_row row)
  | [] -> None

(** Find hierarchical nodes where body->>field = value (e.g. slug). *)
let find_by_body_field (conn : Store_db.conn) ~(kind : string option)
    ~(field : string) ~(value : string) ?(limit : int = 50) () : node list =
  let sql =
    match kind with
    | Some k ->
        "SELECT " ^ select_cols ^ " FROM tree_nodes "
        ^ "WHERE kind = $1 AND payload #>> ARRAY['body',$2] = $3 "
        ^ "ORDER BY path::text ASC LIMIT $4"
    | None ->
        "SELECT " ^ select_cols ^ " FROM tree_nodes "
        ^ "WHERE payload #>> ARRAY['body',$1] = $2 "
        ^ "ORDER BY path::text ASC LIMIT $3"
  in
  let rows =
    match kind with
    | Some k ->
        Io_lwt.Db.execute conn
          ~params:
            [ Pgx.Value.of_string k
            ; Pgx.Value.of_string field
            ; Pgx.Value.of_string value
            ; Pgx.Value.of_int limit
            ]
          sql
    | None ->
        Io_lwt.Db.execute conn
          ~params:
            [ Pgx.Value.of_string field
            ; Pgx.Value.of_string value
            ; Pgx.Value.of_int limit
            ]
          sql
  in
  (List.filter_map node_of_row rows)

(** Upsert a hierarchical node at [path]. [body_json] is the payload body
    object (fields). [kind] must be folder|note|heading.
    K1: notes enforce unique (author, slug); folder+note unique body.uuid. *)
let upsert_hier (conn : Store_db.conn) ~(path : string) ~(kind : string)
    ~(type_name : string) ~(body_json : string) ~(author : string)
    ~(rank : string option)
    : (string, string) result =
  if not (is_hier_kind kind) then
    (Error ("upsert_hier: kind must be folder|note|heading, got " ^ kind))
  else if not (valid_path path) then
    (Error ("upsert_hier: invalid path labels: " ^ path))
  else
    match parse_body_json body_json with
    | Error e -> (Error e)
    | Ok body ->
        let slug_clash =
          if kind <> "note" then None
          else
            match body_string_field body "slug" with
            | None -> None
            | Some slug ->
                let hits =
                  find_by_body_field conn ~kind:(Some "note") ~field:"slug"
                    ~value:slug ~limit:5 ()
                in
                let other =
                  List.find_opt
                    (fun n -> n.path <> path && n.author = author)
                    hits
                in
                (match other with
                   | Some n ->
                       Some
                         (Printf.sprintf
                            "upsert_hier: slug %S already used at %s" slug n.path)
                   | None -> None)
        in
        (match slug_clash with
         | Some err -> (Error err)
         | None ->
             let uuid_clash =
               if kind <> "folder" && kind <> "note" then None
               else
                 match body_string_field body "uuid" with
                 | None -> None
                 | Some uuid ->
                     let existing = get_by_uuid conn ~uuid in
                     (match existing with
                        | Some n when n.path <> path ->
                            Some
                              (Printf.sprintf
                                 "upsert_hier: uuid %s already at %s" uuid n.path)
                        | _ -> None)
             in
             (match uuid_clash with
              | Some err -> (Error err)
              | None ->
                  let hash = hash_body (Yojson.Safe.to_string body) in
                  (* sibling-order: an explicit rank lands on INSERT and on
                     CONFLICT UPDATE (COALESCE keeps the existing rank when
                     the caller passes none — tree/upsert never clobbers
                     order that tree/reorder established). *)
                  let sql =
                    "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, rank, updated_at) "
                    ^ "VALUES ($1::ltree, $2::text, $3::text, "
                    ^ "  jsonb_build_object('v',1,'kind',$2::text,'path',$1::text,'type',$3::text, "
                    ^ "    'body',$4::jsonb,'author',$5::text,'doc',NULL,'hash',$6::text), "
                    ^ "  $6::text, $5::text, $7::text, now()) "
                    ^ "ON CONFLICT (path) DO UPDATE SET "
                    ^ "  kind = EXCLUDED.kind, type_name = EXCLUDED.type_name, "
                    ^ "  payload = EXCLUDED.payload, hash = EXCLUDED.hash, "
                    ^ "  author = EXCLUDED.author, "
                    ^ "  rank = COALESCE(EXCLUDED.rank, tree_nodes.rank), "
                    ^ "  updated_at = now() "
                    ^ "RETURNING path::text"
                  in
                  (try
      let rows =
                        Io_lwt.Db.execute conn
                          ~params:
                            [ Pgx.Value.of_string path
                            ; Pgx.Value.of_string kind
                            ; Pgx.Value.of_string type_name
                            ; Pgx.Value.of_string body_json
                            ; Pgx.Value.of_string author
                            ; Pgx.Value.of_string hash
                            ; (match rank with
                               | Some r -> Pgx.Value.of_string r
                               | None -> Pgx.Value.null)
                            ]
                          sql
                      in
                      match rows with
                      | [ [ p ] ] ->
                          (match Pgx.Value.to_string p with
                           | Some s -> (Ok s)
                           | None ->
                               (Error "upsert_hier: no path returned"))
                      | _ -> (Error "upsert_hier: failed")
    with exn ->
      let msg = Printexc.to_string exn in
                      if
                        string_contains msg "tree_nodes_note_slug"
                        || string_contains msg "tree_nodes_hier_uuid"
                        || string_contains msg "unique"
                      then
                        (Error ("upsert_hier: unique constraint: " ^ msg))
                      else raise exn)))

(** Delete one node, or the whole subtree when [recursive] is true. *)
let delete_path (conn : Store_db.conn) ~(path : string) ~(recursive : bool)
    : int =
  if not (valid_path path) then 0
  else
    let sql =
      if recursive then
        "DELETE FROM tree_nodes WHERE path <@ $1::ltree RETURNING path::text"
      else
        "DELETE FROM tree_nodes WHERE path = $1::ltree RETURNING path::text"
    in
    let rows =
      Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string path ] sql
    in
    (List.length rows)

(** Move subtree rooted at [old_path] to [new_path]. Rewrites every descendant
    path and payload.path. Fails if new_path already exists or labels invalid. *)
let move_subtree (conn : Store_db.conn) ~(old_path : string) ~(new_path : string)
    : (int, string) result =
  if not (valid_path old_path && valid_path new_path) then
    (Error "move_subtree: invalid path labels")
  else if old_path = new_path then (Ok 0)
  else
    (* Refuse moving under itself. *)
    let prefix = old_path ^ "." in
    if
      String.length new_path > String.length old_path
      && String.sub new_path 0 (String.length prefix) = prefix
    then (Error "move_subtree: cannot move under self")
    else
      let existing = get_by_path conn ~path:new_path in
      match existing with
      | Some _ -> (Error ("move_subtree: target exists: " ^ new_path))
      | None ->
          let sql =
            "WITH moved AS ( "
            ^ "  UPDATE tree_nodes SET "
            ^ "    path = CASE WHEN path = $1::ltree THEN $2::ltree "
            ^ "                ELSE ($2::ltree || subpath(path, nlevel($1::ltree))) END, "
            ^ "    payload = jsonb_set(payload, '{path}', to_jsonb( "
            ^ "      (CASE WHEN path = $1::ltree THEN $2::ltree "
            ^ "            ELSE ($2::ltree || subpath(path, nlevel($1::ltree))) END)::text)), "
            ^ "    updated_at = now() "
            ^ "  WHERE path <@ $1::ltree "
            ^ "  RETURNING path::text "
            ^ ") SELECT count(*)::int FROM moved"
          in
          let rows =
            Io_lwt.Db.execute conn
              ~params:
                [ Pgx.Value.of_string old_path; Pgx.Value.of_string new_path ]
              sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_int c with
                | Some n -> (Ok n)
                | None -> (Error "move_subtree: bad count"))
           | _ -> (Error "move_subtree: failed"))

(** Lightweight note index rows: (uuid, fields_json) for kind=note under prefix.
    fields_json has slug/title/tags/kind/day/folder/_path — no outline. *)
let list_note_index (conn : Store_db.conn) ~(prefix : string) ?(limit : int = 2000)
    () : (string * string) list =
  if not (valid_path prefix) then []
  else
    let sql =
      "SELECT path::text, "
      ^ "coalesce(payload #>> '{body,uuid}', ''), "
      ^ "coalesce(payload #>> '{body,slug}', ''), "
      ^ "coalesce(payload #>> '{body,title}', ''), "
      ^ "coalesce(payload #>> '{body,tags}', ''), "
      ^ "coalesce(payload #>> '{body,kind}', 'note'), "
      ^ "coalesce(payload #>> '{body,day}', '') "
      ^ "FROM tree_nodes WHERE path <@ $1::ltree AND kind = 'note' "
      ^ "ORDER BY path::text ASC LIMIT $2"
    in
    let rows =
      Io_lwt.Db.execute conn
        ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ]
        sql
    in
    let root = prefix ^ "." in
    let folder_of path =
      if not (String.length path > String.length root && String.sub path 0 (String.length root) = root)
      then ""
      else
        let rest =
          String.sub path (String.length root)
            (String.length path - String.length root)
        in
        let parts = String.split_on_char '.' rest in
        (match List.rev parts with
         | _slug :: rev_dirs -> String.concat "/" (List.rev rev_dirs)
         | [] -> "")
    in
    (List.filter_map
         (function
           | [ path_c; uuid_c; slug_c; title_c; tags_c; kind_c; day_c ] ->
               (match
                  Pgx.Value.to_string path_c,
                  Pgx.Value.to_string uuid_c,
                  Pgx.Value.to_string slug_c,
                  Pgx.Value.to_string title_c,
                  Pgx.Value.to_string tags_c,
                  Pgx.Value.to_string kind_c,
                  Pgx.Value.to_string day_c
                with
                | Some path, Some uuid, Some slug, Some title, Some tags, Some kind, Some day
                  ->
                    let folder = folder_of path in
                    let id = if uuid = "" then path else uuid in
                    let fields =
                      Yojson.Safe.to_string
                        (`Assoc
                           [ ("slug", `String slug)
                           ; ("title", `String title)
                           ; ("tags", `String tags)
                           ; ("kind", `String kind)
                           ; ("day", `String day)
                           ; ("folder", `String folder)
                           ; ("outline", `String "[]")
                           ; ("_path", `String path)
                           ])
                    in
                    Some (id, fields)
                | _ -> None)
           | _ -> None)
         rows)

(** Distinct folder paths under [prefix], computed in SQL.
    Derives folder from path structure: strip prefix, drop last segment,
    rejoin remaining with '/'. Returns sorted list of folder strings. *)
let list_folder_index (conn : Store_db.conn) ~(prefix : string) () : string list =
  if not (valid_path prefix) then []
  else
    (* Mirror list_note_index.folder_of: strip "prefix.", drop last label,
       rejoin remaining labels with '/'. Pure SQL so notes/doc/folder-names
       does not materialize thousands of rows in Nopales. *)
    let root = prefix ^ "." in
    let from_pos = string_of_int (String.length root + 1) in
    let sql =
      "SELECT DISTINCT "
      ^ "CASE "
      ^ "  WHEN position('.' in substring(path::text from " ^ from_pos ^ ")) = 0 "
      ^ "    THEN '' "
      ^ "  ELSE "
      ^ "    replace( "
      ^ "      left( "
      ^ "        substring(path::text from " ^ from_pos ^ "), "
      ^ "        length(substring(path::text from " ^ from_pos ^ ")) "
      ^ "          - position('.' in reverse(substring(path::text from " ^ from_pos ^ "))) "
      ^ "      ), "
      ^ "      '.', '/' "
      ^ "    ) "
      ^ "END AS folder "
      ^ "FROM tree_nodes "
      ^ "WHERE path <@ $1::ltree AND kind = 'note' "
      ^ "ORDER BY 1"
    in
    let rows =
      Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string prefix ] sql
    in
    List.filter_map (function [ c ] -> Pgx.Value.to_string c | _ -> None) rows

(** Substring search over note + heading bodies under [prefix].
    Returns lightweight hit dicts as JSON strings for Nopales dict-get:
      slug, title, node_id, header_path, matched, rank, kind, tags
    Doc-level hits have empty node_id; heading hits include breadcrumb. *)
let search_text (conn : Store_db.conn) ~(prefix : string) ~(needle : string)
    ?(limit : int = 20) () : string list =
  if not (valid_path prefix) then []
  else
    let needle = String.trim needle in
    if needle = "" then []
    else
      let pat = "%" ^ needle ^ "%" in
      let needle_l = String.lowercase_ascii needle in
      let note_sql =
        "SELECT "
        ^ "coalesce(payload #>> '{body,slug}', ''), "
        ^ "coalesce(payload #>> '{body,title}', ''), "
        ^ "coalesce(payload #>> '{body,tags}', ''), "
        ^ "coalesce(payload #>> '{body,kind}', 'note') "
        ^ "FROM tree_nodes WHERE path <@ $1::ltree AND kind = 'note' "
        ^ "AND (payload #>> '{body,title}' ILIKE $2 "
        ^ "  OR payload #>> '{body,tags}' ILIKE $2 "
        ^ "  OR payload #>> '{body,slug}' ILIKE $2) "
        ^ "ORDER BY 2 ASC LIMIT $3"
      in
      let heading_sql =
        "SELECT "
        ^ "coalesce(n.payload #>> '{body,slug}', ''), "
        ^ "coalesce(n.payload #>> '{body,title}', ''), "
        ^ "coalesce(n.payload #>> '{body,tags}', ''), "
        ^ "coalesce(n.payload #>> '{body,kind}', 'note'), "
        ^ "coalesce(h.payload #>> '{body,id}', ''), "
        ^ "coalesce(h.payload #>> '{body,title}', ''), "
        ^ "coalesce(h.payload #>> '{body,body}', ''), "
        ^ "(SELECT string_agg(coalesce(a.payload #>> '{body,title}', ''), ' › ' "
        ^ "ORDER BY nlevel(a.path)) "
        ^ "FROM tree_nodes a "
        ^ "WHERE a.kind = 'heading' AND a.path <@ n.path AND h.path <@ a.path) AS header_path "
        ^ "FROM tree_nodes h "
        ^ "JOIN LATERAL ( "
        ^ "  SELECT path, payload FROM tree_nodes nn "
        ^ "  WHERE nn.kind = 'note' AND h.path <@ nn.path AND nn.path <@ $1::ltree "
        ^ "  ORDER BY nlevel(nn.path) DESC LIMIT 1 "
        ^ ") n ON true "
        ^ "WHERE h.kind = 'heading' AND h.path <@ $1::ltree "
        ^ "AND (h.payload #>> '{body,title}' ILIKE $2 "
        ^ "  OR h.payload #>> '{body,body}' ILIKE $2) "
        ^ "ORDER BY 6 ASC LIMIT $3"
      in
      let params =
        [ Pgx.Value.of_string prefix
        ; Pgx.Value.of_string pat
        ; Pgx.Value.of_int limit
        ]
      in
      let note_rows = Io_lwt.Db.execute conn ~params note_sql in
      let heading_rows = Io_lwt.Db.execute conn ~params heading_sql in
      let hit ~slug ~title ~tags ~kind ~node_id ~header_path ~matched ~rank =
        Yojson.Safe.to_string
          (`Assoc
             [ ("slug", `String slug)
             ; ("title", `String title)
             ; ("tags", `String tags)
             ; ("kind", `String kind)
             ; ("node_id", `String node_id)
             ; ("header_path", `String header_path)
             ; ("matched", `String matched)
             ; ("rank", `Int rank)
             ])
      in
      let contains_ci hay =
        string_contains (String.lowercase_ascii hay) needle_l
      in
      let parse_note = function
        | [ slug_c; title_c; tags_c; kind_c ] ->
            (match
               Pgx.Value.to_string slug_c,
               Pgx.Value.to_string title_c,
               Pgx.Value.to_string tags_c,
               Pgx.Value.to_string kind_c
             with
             | Some slug, Some title, Some tags, Some kind ->
                 let matched, rank =
                   if String.lowercase_ascii slug = needle_l then ("slug", 0)
                   else if contains_ci title then ("title", 1)
                   else if contains_ci tags then ("tags", 3)
                   else if contains_ci slug then ("slug", 1)
                   else ("title", 4)
                 in
                 Some (rank, slug, "", hit ~slug ~title ~tags ~kind ~node_id:""
                         ~header_path:"" ~matched ~rank)
             | _ -> None)
        | _ -> None
      in
      let parse_heading = function
        | [ slug_c; title_c; tags_c; kind_c; nid_c; htitle_c; hbody_c; path_c ]
          ->
            (match
               Pgx.Value.to_string slug_c,
               Pgx.Value.to_string title_c,
               Pgx.Value.to_string tags_c,
               Pgx.Value.to_string kind_c,
               Pgx.Value.to_string nid_c,
               Pgx.Value.to_string htitle_c,
               Pgx.Value.to_string hbody_c,
               Pgx.Value.to_string path_c
             with
             | Some slug, Some title, Some tags, Some kind, Some nid,
               Some htitle, Some hbody, Some header_path ->
                 let matched, rank =
                   if contains_ci htitle then ("header", 2)
                   else if contains_ci hbody then ("body", 4)
                   else ("header", 4)
                 in
                 Some
                   ( rank
                   , slug
                   , nid
                   , hit ~slug ~title ~tags ~kind ~node_id:nid ~header_path
                       ~matched ~rank )
             | _ -> None)
        | _ -> None
      in
      let notes = List.filter_map parse_note note_rows in
      let heads = List.filter_map parse_heading heading_rows in
      let merged =
        List.sort
          (fun (r1, s1, n1, _) (r2, s2, n2, _) ->
            let c = compare r1 r2 in
            if c <> 0 then c
            else
              let c2 = String.compare s1 s2 in
              if c2 <> 0 then c2 else String.compare n1 n2)
          (notes @ heads)
      in
      (* Dedupe (slug, node_id); keep best rank. *)
      let seen = Hashtbl.create 32 in
      let acc = ref [] in
      List.iter
        (fun (_r, slug, nid, json) ->
          let key = slug ^ "\x00" ^ nid in
          if not (Hashtbl.mem seen key) then begin
            Hashtbl.add seen key ();
            acc := json :: !acc
          end)
        merged;
      let out = List.rev !acc in
      if List.length out <= limit then out else
        let rec take n xs =
          if n <= 0 then []
          else match xs with [] -> [] | x :: rest -> x :: take (n - 1) rest
        in
        take limit out

(** List nodes of a given kind under path prefix (e.g. all notes in a garden). *)
let list_by_kind (conn : Store_db.conn) ~(prefix : string) ~(kind : string)
    ?(limit : int = 2000) () : node list =
  if not (valid_path prefix) then []
  else
    let sql =
      "SELECT " ^ select_cols ^ " FROM tree_nodes "
      ^ "WHERE path <@ $1::ltree AND kind = $2 "
      ^ "ORDER BY path::text ASC LIMIT $3"
    in
    let rows =
      Io_lwt.Db.execute conn
        ~params:
          [ Pgx.Value.of_string prefix
          ; Pgx.Value.of_string kind
          ; Pgx.Value.of_int limit
          ]
        sql
    in
    (List.filter_map node_of_row rows)

(** Direct children under [prefix], index-friendly (one-tree.projections
    CHILDREN-BY-LEVEL). Expressed as an nlevel predicate the ltree GiST
    path index can drive — never a correlated NOT EXISTS anti-join over
    the whole subtree (that form is O(subtree^2) and blocked /notes at
    ~3000 notes).

    - one-label children: nlevel = nlevel(prefix) + 1 (folders, notes)
    - outline covering: nlevel = nlevel(prefix) + 2 under the literal
      label [h] (note outline roots at prefix.h.<nid>, path grammar in
      one-tree.tree-model). No intermediate node is required at
      prefix.h. *)
let list_children (conn : Store_db.conn) ~(prefix : string) ?(limit : int = 50000)
    () : node list =
  if not (valid_path prefix) then []
  else
    let sql =
      "SELECT " ^ select_cols ^ " FROM tree_nodes n "
      ^ "WHERE n.path <@ $1::ltree AND n.path <> $1::ltree "
      ^ "AND ( "
      ^ "  nlevel(n.path) = nlevel($1::ltree) + 1 "
      ^ "  OR ( "
      ^ "    nlevel(n.path) = nlevel($1::ltree) + 2 "
      ^ "    AND subpath(n.path, nlevel($1::ltree), 1)::text = 'h' "
      ^ "  ) "
      ^ ") "
      (* sibling-order: ranked siblings first (byte-order rank — locale-
         independent), then unranked, path-ordered among themselves. *)
      ^ "ORDER BY n.rank COLLATE \"C\" NULLS LAST, n.path::text ASC LIMIT $2"
    in
    let rows =
      Io_lwt.Db.execute conn
        ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ]
        sql
    in
    (List.filter_map node_of_row rows)

(** Lens-aware children-of: DIRECT-child nodes of the RELATIVE logical path,
    resolved against [chain] (self-first / closest-wins). Uses the effective
    branch where the logical path first resolves, then list_children on that
    materialized path — so direct children are read under the branch that
    actually owns/overtides the node. Returns [] when the path resolves
    nowhere. Shared with lens_get_in_chain so the override rule is single. *)
let lens_children_in_chain (conn : Store_db.conn) ~(suffix : string)
    ?(limit : int = 50000) (chain : string list) : node list =
  let mk_path branch_id =
    match lens_suffix_path ~branch_id suffix with
    | Some p -> p
    | None -> ""
  in
  match resolve_in_chain conn ~mk_path chain with
  | None -> []
  | Some n -> list_children conn ~prefix:n.path ~limit ()

(** Lens-aware subtree: all descendants of the RELATIVE logical path,
    resolved against [chain] (closest-wins), read under the effective
    branch, capped at limit. Backed by list_subtree. Returns [] when the
    path resolves nowhere. *)
let lens_subtree_in_chain (conn : Store_db.conn) ~(suffix : string)
    ?(limit : int = 50000) (chain : string list) : node list =
  let mk_path branch_id =
    match lens_suffix_path ~branch_id suffix with
    | Some p -> p
    | None -> ""
  in
  match resolve_in_chain conn ~mk_path chain with
  | None -> []
  | Some n -> list_subtree conn ~prefix:n.path ~limit ()

(** Set the rank of one node (None clears it — back to unordered).
   Returns the number of rows updated (0 = no such node). A rank change
   is an ordinary versioned write (migration 025's trigger echoes the
   rank column; a same-rank UPDATE is a suppressed mirror echo). *)
let set_rank (conn : Store_db.conn) ~(path : string)
    ~(rank : string option) : int =
  if not (valid_path path) then 0
  else
    let sql =
      "UPDATE tree_nodes SET rank = $2::text, updated_at = now() "
      ^ "WHERE path = $1::ltree RETURNING path::text"
    in
    let rows =
      Io_lwt.Db.execute conn
        ~params:
          [ Pgx.Value.of_string path
          ; (match rank with Some r -> Pgx.Value.of_string r | None -> Pgx.Value.null)
          ]
        sql
    in
    List.length rows

(** Re-assign fresh fixed-width ranks to every RANKED sibling under
   [parent] (rank-ordered), keeping their relative order. Unranked
   siblings stay NULL (they sort after the ranked block). Returns the
   number reassigned. Caller wraps in a transaction if atomicity matters. *)
let rebalance_siblings (conn : Store_db.conn) ~(parent : string) : int =
  let ranked =
    list_children conn ~prefix:parent ()
    |> List.filter_map (fun (n : node) ->
           match n.rank with Some r -> Some (n.path, r) | None -> None)
  in
  let fresh = rank_rebalance (List.length ranked) in
  List.fold_left2
    (fun acc (path, _old) new_rank ->
      acc + set_rank conn ~path ~rank:(Some new_rank))
    0 ranked fresh

let html_escape_text (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let path_label_of (path : string) : string =
  match List.rev (String.split_on_char '.' path) with
  | [] -> path
  | last :: _ -> last

let node_display_title (n : node) : string =
  match body_string_field n.body "title" with
  | Some t when String.trim t <> "" -> t
  | _ ->
      (match body_string_field n.body "slug" with
       | Some s when String.trim s <> "" -> s
       | _ -> path_label_of n.path)

let node_display_slug (n : node) : string =
  match body_string_field n.body "slug" with
  | Some s when String.trim s <> "" -> s
  | _ -> path_label_of n.path

(** Direct children of [prefix] that themselves own a folder/note child.
    One query: depth+2 folder/note nodes → their parent path at depth+1.
    Lets notes like garden.bible expand to show characters/bsb. *)
let parents_with_hier_children (conn : Store_db.conn) ~(prefix : string)
    : string list =
  if not (valid_path prefix) then []
  else
    let sql =
      "SELECT DISTINCT subpath(path, 0, nlevel($1::ltree) + 1)::text "
      ^ "FROM tree_nodes "
      ^ "WHERE path <@ $1::ltree "
      ^ "AND nlevel(path) = nlevel($1::ltree) + 2 "
      ^ "AND kind IN ('folder', 'note')"
    in
    let rows =
      Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string prefix ] sql
    in
    List.filter_map (function [ c ] -> Pgx.Value.to_string c | _ -> None) rows

(** Budget-safe HTML for notes tree children under [prefix].
    Mirrors notes/ui/tree-children-html but runs in OCaml so folders with
    thousands of notes do not blow the Nopales eval budget. *)
let render_children_html (conn : Store_db.conn) ~(prefix : string)
    ?(limit : int = 50000) ~(garden_root : string) () : string =
  if not (valid_path prefix) then
    "<p class=\"muted\">(empty)</p>"
  else
    let kids = list_children conn ~prefix ~limit () in
    let kids =
      List.filter (fun n -> n.kind = "folder" || n.kind = "note") kids
    in
    if kids = [] then "<p class=\"muted\">(empty)</p>"
    else
      let expandable =
        let set = Hashtbl.create 64 in
        List.iter (fun p -> Hashtbl.replace set p true)
          (parents_with_hier_children conn ~prefix);
        set
      in
      let garden_path = garden_root ^ ".garden" in
      let buf = Buffer.create 4096 in
      let emit_folder_details ~path ~label ~open_slug =
        Buffer.add_string buf "<details class=\"tree-folder\" hx-get=\"";
        Buffer.add_string buf "/partials/notes-tree?path=";
        Buffer.add_string buf (html_escape_text path);
        Buffer.add_string buf
          "\" hx-trigger=\"toggle once\" hx-target=\"find .tree-children\" ";
        Buffer.add_string buf "hx-swap=\"innerHTML\"><summary class=\"tree-folder-header\">";
        Buffer.add_string buf
          "<span class=\"tree-folder-icon\">&#x1F4C1; </span>";
        (match open_slug with
         | Some slug ->
             Buffer.add_string buf "<a href=\"/notes?slug=";
             Buffer.add_string buf (html_escape_text slug);
             Buffer.add_string buf "\" onclick=\"event.stopPropagation()\">";
             Buffer.add_string buf (html_escape_text label);
             Buffer.add_string buf "</a>"
         | None -> Buffer.add_string buf (html_escape_text label));
        Buffer.add_string buf
          "</summary><div class=\"tree-children\"><span class=\"muted\">";
        Buffer.add_string buf "open to load…</span></div></details>"
      in
      let emit_note_li n =
        let slug = node_display_slug n in
        let title = node_display_title n in
        Buffer.add_string buf "<li class=\"tree-item\"><a href=\"/notes?slug=";
        Buffer.add_string buf (html_escape_text slug);
        Buffer.add_string buf "\">";
        Buffer.add_string buf (html_escape_text title);
        Buffer.add_string buf "</a></li>"
      in
      let rec emit_inlined path =
        let nested = list_children conn ~prefix:path ~limit () in
        let nested_expand =
          let set = Hashtbl.create 64 in
          List.iter (fun p -> Hashtbl.replace set p true)
            (parents_with_hier_children conn ~prefix:path);
          set
        in
        List.iter
          (fun n ->
            if n.kind = "folder" || n.kind = "note" then
              emit_node ~expandable:nested_expand n)
          nested
      and emit_node ~expandable n =
        if n.kind = "folder" then
          if n.path = garden_path then emit_inlined n.path
          else
            emit_folder_details ~path:n.path ~label:(node_display_title n)
              ~open_slug:None
        else if n.kind = "note" then
          if Hashtbl.mem expandable n.path then
            emit_folder_details ~path:n.path ~label:(node_display_title n)
              ~open_slug:(Some (node_display_slug n))
          else emit_note_li n
        else ()
      in
      List.iter (emit_node ~expandable) kids;
      Buffer.contents buf
