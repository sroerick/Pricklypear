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

(* ---- DB helpers (read/verify; mirror writes happen in SQL triggers) ---- *)

let select_cols = "id::text, path::text, kind, type_name, payload::text, hash, author"

(** Fetch one node by exact path. *)
let get_by_path (store : Store.t) ~(path : string) : node option Lwt.t =
  match Io_lwt.Db.current_conn store with
  | None -> Lwt.return_none
  | Some conn ->
      let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes WHERE path = $1::ltree" in
      let* rows =
        Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string path ] sql
      in
      (match rows with
       | row :: _ -> Lwt.return (node_of_row row)
       | [] -> Lwt.return_none)

(** Subtree listing: all nodes under a path prefix (ltree descendant). *)
let list_subtree (store : Store.t) ~(prefix : string)
    ?(limit : int = 1000) () : node list Lwt.t =
  match Io_lwt.Db.current_conn store with
  | None -> Lwt.return []
  | Some conn ->
      let sql =
        "SELECT " ^ select_cols ^ " FROM tree_nodes " ^
        "WHERE path <@ $1::ltree ORDER BY path::text ASC LIMIT $2" in
      let* rows =
        Io_lwt.Db.execute conn
          ~params:[ Pgx.Value.of_string prefix; Pgx.Value.of_int limit ] sql
      in
      Lwt.return (List.filter_map node_of_row rows)

(** All datum nodes of a Nopales type (the (rows 't) backing in P3). *)
let list_data_by_type (store : Store.t) ~(type_name : string)
    ?(limit : int = 100) () : node list Lwt.t =
  match Io_lwt.Db.current_conn store with
  | None -> Lwt.return []
  | Some conn ->
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
let rec resolve_in_chain (store : Store.t) ~(mk_path : string -> string)
    (chain : string list) : node option Lwt.t =
  match chain with
  | [] -> Lwt.return_none
  | branch_id :: rest ->
      let* n = get_by_path store ~path:(mk_path branch_id) in
      (match n with
       | Some _ -> Lwt.return n
       | None -> resolve_in_chain store ~mk_path rest)

(** Consistency check against the SQL tree_verify() function.
    Returns (kind, table_count, tree_count, in_sync) rows. *)
let verify (store : Store.t) : (string * int * int * bool) list Lwt.t =
  match Io_lwt.Db.current_conn store with
  | None -> Lwt.return []
  | Some conn ->
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
