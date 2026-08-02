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

(** Seed corpus types — visible to every session under data.shared.*
    (must match tree_is_seed_type in migrations/013_data_user_scoping.sql). *)
let seed_types = [ "bible-verse"; "bible-book"; "bible-interlinear"; "strongs-entry" ]

let is_seed_type (type_name : string) : bool =
  List.mem type_name seed_types

(** Datum path. Prefer [~user] for per-user rows; seed types always land
    under data.shared regardless of user. Without ~user, non-seed types
    use data.u.unknown — writers must pass ~user. *)
let datum_path ?user ~type_name ~id () =
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
    Printf.sprintf "data.u.%s.%s.%s" (enc_label u) ty_lab id_lab

(* ---- content hashing ---- *)

let hash_body (canonical_body_json : string) : string =
  Auth_password.sha256_hex canonical_body_json

(* ---- node envelope ---- *)

type node = {
  id : string;
  path : string;
  kind : string;             (* fn | type | route | datum | folder | note | heading *)
  type_name : string option; (* datum: the Nopales type *)
  body : Yojson.Safe.t;      (* canonical body (AST / fields / route) *)
  author : string;
  doc : string option;       (* fn only *)
  hash : string;
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
  (* Expected column order: id, path, kind, type_name, payload, hash, author *)
  match row with
  | [ id_c; path_c; kind_c; type_c; payload_c; hash_c; author_c ] ->
      (match Pgx.Value.to_string id_c,
             Pgx.Value.to_string path_c,
             Pgx.Value.to_string kind_c,
             Pgx.Value.to_string payload_c,
             Pgx.Value.to_string hash_c,
             Pgx.Value.to_string author_c with
       | Some id, Some path, Some kind, Some payload_s, Some hash, Some author ->
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
                     body; author; doc; hash }
            with Yojson.Json_error _ -> None)
       | _ -> None)
  | _ -> None

(* ---- one-tree P4: write-to-tree flag ----
   Default ON: the image writes tree_nodes as TRUTH; the SQL reverse-mirror
   trigger (009 tree_from_nodes) keeps the legacy tables in lockstep for
   residual readers / fork-copy paths. Set PP_TREE_WRITES=0 to write tables
   (forward trigger still mirrors to tree). Independent of PP_TREE_READS. *)
let tree_writes () : bool =
  (* Default OFF until tree upsert path is proven on all write sites.
     Tables + forward deferred mirror still keep tree_nodes in lockstep.
     Set PP_TREE_WRITES=1 to exercise direct tree writes. *)
  match Sys.getenv_opt "PP_TREE_WRITES" with
  | Some "1" | Some "true" | Some "yes" | Some "on" -> true
  | _ -> false

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
    "SELECT tree_datum_path($1::text, nid.id, $3::text), 'datum', $1::text, " ^
    "  jsonb_build_object('v',1,'kind','datum', " ^
    "    'path', tree_datum_path($1::text, nid.id, $3::text)::text, " ^
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
  (* Match by datum id (last path label) so old and new path shapes both work. *)
  let hash = hash_body fields_json in
  let sql =
    "UPDATE tree_nodes SET " ^
    "  payload = jsonb_set(jsonb_set(payload,'{body}',$2::jsonb),'{hash}',to_jsonb($3::text)), " ^
    "  hash = $3::text, updated_at = now() " ^
    "WHERE kind='datum' AND tree_datum_id(path) = $1::uuid " ^
    "  AND (type_name = $4 OR type_name IS NULL) " ^
    "RETURNING tree_datum_id(path)::text" in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string id; Pgx.Value.of_string fields_json;
                Pgx.Value.of_string hash; Pgx.Value.of_string type_name ]
      sql
  in
  (match rows with
   | [ [ id_c ] ] -> Pgx.Value.to_string id_c
   | _ -> None)

let delete_datum (conn : Store_db.conn) ~(type_name : string) ~(id : string) : string option =
  let sql =
    "DELETE FROM tree_nodes WHERE kind='datum' AND tree_datum_id(path) = $1::uuid " ^
    "  AND (type_name = $2 OR type_name IS NULL) " ^
    "RETURNING tree_datum_id(path)::text" in
  let rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string id; Pgx.Value.of_string type_name ]
      sql
  in
  (match rows with
   | [ [ id_c ] ] -> Pgx.Value.to_string id_c
   | _ -> None)

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

let select_cols = "id::text, path::text, kind, type_name, payload::text, hash, author"

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

(** Subtree listing: all nodes under a path prefix (ltree descendant). *)
let list_subtree (conn : Store_db.conn) ~(prefix : string)
    ?(limit : int = 1000) () : node list =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes " ^
        "WHERE path <@ $1::ltree ORDER BY path::text ASC LIMIT $2" in
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
       ])

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
                  let sql =
                    "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) "
                    ^ "VALUES ($1::ltree, $2::text, $3::text, "
                    ^ "  jsonb_build_object('v',1,'kind',$2::text,'path',$1::text,'type',$3::text, "
                    ^ "    'body',$4::jsonb,'author',$5::text,'doc',NULL,'hash',$6::text), "
                    ^ "  $6::text, $5::text, now()) "
                    ^ "ON CONFLICT (path) DO UPDATE SET "
                    ^ "  kind = EXCLUDED.kind, type_name = EXCLUDED.type_name, "
                    ^ "  payload = EXCLUDED.payload, hash = EXCLUDED.hash, "
                    ^ "  author = EXCLUDED.author, updated_at = now() "
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
      ^ "ORDER BY n.path::text ASC LIMIT $2"
    in
    let rows =
      Io_lwt.Db.execute conn
        ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ]
        sql
    in
    (List.filter_map node_of_row rows)

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
