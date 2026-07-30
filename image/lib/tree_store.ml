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
    parent chain (first hit wins). /data and /meta are shared subtrees
    (branch-free) per [branching.data]. *)

open Lwt.Syntax

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

let datum_path ~type_name ~id =
  Printf.sprintf "data.%s.%s" (enc_label type_name) (uuid_label id)

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
    : unit Lwt.t =
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
      let* _ =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string body_json;
                    Pgx.Value.of_string author; doc_param; Pgx.Value.of_string hash ]
          sql
      in
      Lwt.return_unit

let upsert_type (conn : Store_db.conn) ~(branch_id : string) ~(name : string)
    ~(fields_json : string) ~(author : string) : unit Lwt.t =
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
      let* _ =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string name;
                    Pgx.Value.of_string fields_json; Pgx.Value.of_string author;
                    Pgx.Value.of_string hash ]
          sql
      in
      Lwt.return_unit

let upsert_route (conn : Store_db.conn) ~(branch_id : string) ~(route : string)
    ~(handler : string) ~(visibility : string) : unit Lwt.t =
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
      let* _ =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string body_json;
                    Pgx.Value.of_string hash ]
          sql
      in
      Lwt.return_unit

let delete_fn (conn : Store_db.conn) ~(branch_id : string) ~(name : string) : bool Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let sql = "DELETE FROM tree_nodes WHERE path = $1::ltree RETURNING path::text" in
      let* rows =
        Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string (fn_path ~branch_id ~name) ] sql
      in
      Lwt.return (rows <> [])

let insert_datum (conn : Store_db.conn) ~(type_name : string) ~(fields_json : string)
    ~(author : string) : string option Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        (* id generated by PG; path derived from it in one statement *)
      let hash = hash_body fields_json in
      let sql =
        "WITH nid AS (SELECT gen_random_uuid() AS id) " ^
        "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at) " ^
        "SELECT ('data.' || $4::text || '.' || replace(nid.id::text,'-',''))::ltree, 'datum', $1::text, " ^
        "  jsonb_build_object('v',1,'kind','datum', " ^
        "    'path', ('data.' || $4::text || '.' || replace(nid.id::text,'-','')), " ^
        "    'type',$1::text,'body',$2::jsonb,'author',$3::text,'doc',NULL,'hash',$5::text), " ^
        "  $5::text, $3::text, now() " ^
        "FROM nid RETURNING (string_to_array(path::text,'.'))[3]" in
      let* rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string type_name; Pgx.Value.of_string fields_json;
                    Pgx.Value.of_string author; Pgx.Value.of_string (enc_label type_name);
                    Pgx.Value.of_string hash ]
          sql
      in
      (match rows with
       | [ [ idhex ] ] ->
           (match Pgx.Value.to_string idhex with
            | Some h when String.length h = 32 ->
                (* re-dash the 32-hex into a uuid string *)
                let d = Printf.sprintf "%s-%s-%s-%s-%s"
                  (String.sub h 0 8) (String.sub h 8 4) (String.sub h 12 4)
                  (String.sub h 16 4) (String.sub h 20 12) in
                Lwt.return (Some d)
            | _ -> Lwt.return_none)
       | _ -> Lwt.return_none)

let update_datum (conn : Store_db.conn) ~(type_name : string) ~(id : string)
    ~(fields_json : string) : string option Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let path = datum_path ~type_name ~id in
      let hash = hash_body fields_json in
      let sql =
        "UPDATE tree_nodes SET " ^
        "  payload = jsonb_set(jsonb_set(payload,'{body}',$2::jsonb),'{hash}',to_jsonb($3::text)), " ^
        "  hash = $3::text, updated_at = now() " ^
        "WHERE path = $1::ltree AND kind='datum' RETURNING (string_to_array(path::text,'.'))[3]" in
      let* rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string path; Pgx.Value.of_string fields_json;
                    Pgx.Value.of_string hash ]
          sql
      in
      (match rows with
       | [ [ idhex ] ] ->
           (match Pgx.Value.to_string idhex with
            | Some h when String.length h = 32 ->
                let d = Printf.sprintf "%s-%s-%s-%s-%s"
                  (String.sub h 0 8) (String.sub h 8 4) (String.sub h 12 4)
                  (String.sub h 16 4) (String.sub h 20 12) in
                Lwt.return (Some d)
            | _ -> Lwt.return_none)
       | _ -> Lwt.return_none)

let delete_datum (conn : Store_db.conn) ~(type_name : string) ~(id : string) : string option Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "DELETE FROM tree_nodes WHERE path = $1::ltree AND kind='datum' " ^
        "RETURNING (string_to_array(path::text,'.'))[3]" in
      let* rows =
        Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string (datum_path ~type_name ~id) ] sql
      in
      (match rows with
       | [ [ idhex ] ] ->
           (match Pgx.Value.to_string idhex with
            | Some h when String.length h = 32 ->
                let d = Printf.sprintf "%s-%s-%s-%s-%s"
                  (String.sub h 0 8) (String.sub h 8 4) (String.sub h 12 4)
                  (String.sub h 16 4) (String.sub h 20 12) in
                Lwt.return (Some d)
            | _ -> Lwt.return_none)
       | _ -> Lwt.return_none)

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
    () : string list Lwt.t =
  let sql =
    "SELECT n.path::text FROM tree_nodes n, " ^
    "LATERAL jsonb_each_text(COALESCE(n.payload->'body', '{}'::jsonb)) e " ^
    "WHERE n.kind = 'datum' AND e.value = $1::text " ^
    "ORDER BY n.path::text ASC LIMIT $2" in
  let* rows =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string needle; Pgx.Value.of_int limit ]
      sql
  in
  Lwt.return
    (List.filter_map
       (function
         | [ c ] -> Pgx.Value.to_string c
         | _ -> None)
       rows)

(** Recompute tree_branches.head_hash for [branch_id] as the sha256 of the
    sorted concatenation of own-node hashes under b.<uuidhex>. Empty lens →
    empty string. Used after fork (empty) and after promote/merge batches. *)
let recompute_head_hash (conn : Store_db.conn) ~(branch_id : string) : unit Lwt.t =
  let root = "b." ^ uuid_label branch_id in
  let sql =
    "UPDATE tree_branches SET head_hash = COALESCE(( " ^
    "  SELECT encode(digest(string_agg(hash, '' ORDER BY path::text), 'sha256'), 'hex') " ^
    "  FROM tree_nodes WHERE path <@ $1::ltree AND nlevel(path) > 1 " ^
    "), '') WHERE branch_id = $2::uuid" in
  let* _ =
    Io_lwt.Db.execute conn
      ~params:[ Pgx.Value.of_string root; Pgx.Value.of_string branch_id ]
      sql
  in
  Lwt.return_unit

(* ---- DB helpers (read/verify; mirror writes happen in SQL triggers) ---- *)

let select_cols = "id::text, path::text, kind, type_name, payload::text, hash, author"

(** Fetch one node by exact path. *)
let get_by_path (conn : Store_db.conn) ~(path : string) : node option Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes WHERE path = $1::ltree" in
      let* rows =
        Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string path ] sql
      in
      (match rows with
       | row :: _ -> Lwt.return (node_of_row row)
       | [] -> Lwt.return_none)

(** Subtree listing: all nodes under a path prefix (ltree descendant). *)
let list_subtree (conn : Store_db.conn) ~(prefix : string)
    ?(limit : int = 1000) () : node list Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes " ^
        "WHERE path <@ $1::ltree ORDER BY path::text ASC LIMIT $2" in
      let* rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ] sql
      in
      Lwt.return (List.filter_map node_of_row rows)

(** All datum nodes of a Nopales type (the (rows 't) backing in P3). *)
let list_data_by_type (conn : Store_db.conn) ~(type_name : string)
    ?(limit : int = 100) () : node list Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes " ^
        "WHERE kind = 'datum' AND type_name = $1 " ^
        "ORDER BY created_at ASC LIMIT $2" in
      let* rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string type_name; Pgx.Value.of_int limit ] sql
      in
      Lwt.return (List.filter_map node_of_row rows)

(** Branch-chain resolution: try [name] under self, then each ancestor.
    Returns the first hit (closest-ancestor-wins = the override rule).
    [branch_chain] is ordered self-first (depth ASC), as store.branch
    chain produces. *)
let rec resolve_in_chain (conn : Store_db.conn) ~(mk_path : string -> string)
    (chain : string list) : node option Lwt.t =
  match chain with
  | [] -> Lwt.return_none
  | branch_id :: rest ->
      let* n = get_by_path conn ~path:(mk_path branch_id) in
      (match n with
       | Some _ -> Lwt.return n
       | None -> resolve_in_chain conn ~mk_path rest)

(** Consistency check against the SQL tree_verify() function.
    Returns (kind, table_count, tree_count, in_sync) rows. *)
let verify (conn : Store_db.conn) : (string * int * int * bool) list Lwt.t =
  (* conn: Store_db.conn (tx-aware) *)
        let* rows = Io_lwt.Db.execute conn "SELECT * FROM tree_verify()" in
      Lwt.return
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
let get_by_uuid (conn : Store_db.conn) ~(uuid : string) : node option Lwt.t =
  let sql =
    "SELECT " ^ select_cols ^ " FROM tree_nodes "
    ^ "WHERE payload #>> '{body,uuid}' = $1 LIMIT 1"
  in
  let* rows =
    Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string uuid ] sql
  in
  match rows with
  | row :: _ -> Lwt.return (node_of_row row)
  | [] -> Lwt.return_none

(** Find hierarchical nodes where body->>field = value (e.g. slug). *)
let find_by_body_field (conn : Store_db.conn) ~(kind : string option)
    ~(field : string) ~(value : string) ?(limit : int = 50) () : node list Lwt.t =
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
  let* rows =
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
  Lwt.return (List.filter_map node_of_row rows)

(** Upsert a hierarchical node at [path]. [body_json] is the payload body
    object (fields). [kind] must be folder|note|heading.
    K1: notes enforce unique (author, slug); folder+note unique body.uuid. *)
let upsert_hier (conn : Store_db.conn) ~(path : string) ~(kind : string)
    ~(type_name : string) ~(body_json : string) ~(author : string)
    : (string, string) result Lwt.t =
  if not (is_hier_kind kind) then
    Lwt.return (Error ("upsert_hier: kind must be folder|note|heading, got " ^ kind))
  else if not (valid_path path) then
    Lwt.return (Error ("upsert_hier: invalid path labels: " ^ path))
  else
    match parse_body_json body_json with
    | Error e -> Lwt.return (Error e)
    | Ok body ->
        let* slug_clash =
          if kind <> "note" then Lwt.return_none
          else
            match body_string_field body "slug" with
            | None -> Lwt.return_none
            | Some slug ->
                let* hits =
                  find_by_body_field conn ~kind:(Some "note") ~field:"slug"
                    ~value:slug ~limit:5 ()
                in
                let other =
                  List.find_opt
                    (fun n -> n.path <> path && n.author = author)
                    hits
                in
                Lwt.return
                  (match other with
                   | Some n ->
                       Some
                         (Printf.sprintf
                            "upsert_hier: slug %S already used at %s" slug n.path)
                   | None -> None)
        in
        (match slug_clash with
         | Some err -> Lwt.return (Error err)
         | None ->
             let* uuid_clash =
               if kind <> "folder" && kind <> "note" then Lwt.return_none
               else
                 match body_string_field body "uuid" with
                 | None -> Lwt.return_none
                 | Some uuid ->
                     let* existing = get_by_uuid conn ~uuid in
                     Lwt.return
                       (match existing with
                        | Some n when n.path <> path ->
                            Some
                              (Printf.sprintf
                                 "upsert_hier: uuid %s already at %s" uuid n.path)
                        | _ -> None)
             in
             (match uuid_clash with
              | Some err -> Lwt.return (Error err)
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
                  Lwt.catch
                    (fun () ->
                      let* rows =
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
                           | Some s -> Lwt.return (Ok s)
                           | None ->
                               Lwt.return (Error "upsert_hier: no path returned"))
                      | _ -> Lwt.return (Error "upsert_hier: failed"))
                    (fun exn ->
                      let msg = Printexc.to_string exn in
                      if
                        string_contains msg "tree_nodes_note_slug"
                        || string_contains msg "tree_nodes_hier_uuid"
                        || string_contains msg "unique"
                      then
                        Lwt.return
                          (Error ("upsert_hier: unique constraint: " ^ msg))
                      else Lwt.fail exn)))

(** Delete one node, or the whole subtree when [recursive] is true. *)
let delete_path (conn : Store_db.conn) ~(path : string) ~(recursive : bool)
    : int Lwt.t =
  if not (valid_path path) then Lwt.return 0
  else
    let sql =
      if recursive then
        "DELETE FROM tree_nodes WHERE path <@ $1::ltree RETURNING path::text"
      else
        "DELETE FROM tree_nodes WHERE path = $1::ltree RETURNING path::text"
    in
    let* rows =
      Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string path ] sql
    in
    Lwt.return (List.length rows)

(** Move subtree rooted at [old_path] to [new_path]. Rewrites every descendant
    path and payload.path. Fails if new_path already exists or labels invalid. *)
let move_subtree (conn : Store_db.conn) ~(old_path : string) ~(new_path : string)
    : (int, string) result Lwt.t =
  if not (valid_path old_path && valid_path new_path) then
    Lwt.return (Error "move_subtree: invalid path labels")
  else if old_path = new_path then Lwt.return (Ok 0)
  else
    (* Refuse moving under itself. *)
    let prefix = old_path ^ "." in
    if
      String.length new_path > String.length old_path
      && String.sub new_path 0 (String.length prefix) = prefix
    then Lwt.return (Error "move_subtree: cannot move under self")
    else
      let* existing = get_by_path conn ~path:new_path in
      match existing with
      | Some _ -> Lwt.return (Error ("move_subtree: target exists: " ^ new_path))
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
          let* rows =
            Io_lwt.Db.execute conn
              ~params:
                [ Pgx.Value.of_string old_path; Pgx.Value.of_string new_path ]
              sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_int c with
                | Some n -> Lwt.return (Ok n)
                | None -> Lwt.return (Error "move_subtree: bad count"))
           | _ -> Lwt.return (Error "move_subtree: failed"))

(** Covering children under [prefix]: nodes under the prefix with no other
    tree_nodes between them and the prefix. Handles both one-label children
    (folder/note) and note outline roots at prefix.h.<nid> (two labels). *)
let list_children (conn : Store_db.conn) ~(prefix : string) ?(limit : int = 500)
    () : node list Lwt.t =
  if not (valid_path prefix) then Lwt.return []
  else
    let sql =
      "SELECT " ^ select_cols ^ " FROM tree_nodes n "
      ^ "WHERE n.path <@ $1::ltree AND n.path <> $1::ltree "
      ^ "AND NOT EXISTS ( "
      ^ "  SELECT 1 FROM tree_nodes mid "
      ^ "  WHERE mid.path <@ $1::ltree AND mid.path <> $1::ltree "
      ^ "    AND n.path <@ mid.path AND n.path <> mid.path "
      ^ ") "
      ^ "ORDER BY n.path::text ASC LIMIT $2"
    in
    let* rows =
      Io_lwt.Db.execute conn
        ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ]
        sql
    in
    Lwt.return (List.filter_map node_of_row rows)
