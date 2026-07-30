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
  kind : string;             (* fn | type | route | datum *)
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
  match Sys.getenv_opt "PP_TREE_WRITES" with
  | Some "0" | Some "false" | Some "no" | Some "off" -> false
  | _ -> true

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
