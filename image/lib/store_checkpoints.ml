(** Store CHECKPOINTS — named points on the gen stream (branching.checkpoints,
    plan phase 2026-10-12, roerick-ratified). A checkpoint is a NAME pinned to
    the branch's OWN head gen at capture time; the append-only tree_versions
    trail makes every earlier state a READ, so cp-diff is a windowed per-pin
    hash classification — the branch_status decision discipline (hash equality
    with ABSENT as its own value) over a WINDOWED coordinate, never a second
    classifier — and cp-restore lands NEW WRITES through the tree write
    helpers (tree is truth: upserts fire the version trail at fresh gens, the
    reverse mirrors, and code_history before-images; the gen counter only
    moves forward; base_gen is never touched — restore is not a merge).

    GEN CAPTURE (spec): branch_generations bumps are trigger-side (038), so
    the capture reads the branch's OWN counter and inserts in ONE atomic
    INSERT..SELECT — never inside a caller's write transaction, and never the
    ancestry-walk max (current_branch_generation), whose ancestor moves would
    skew the window. Rides store_history's family per the spec (7th family;
    scripts/verify_store_split.sh pins the composition). Opaque jsonb only:
    no Lisp interpreter references (store-split contract). *)

type cp_row = {
  cp_branch_id : string;
  cp_name : string;
  cp_gen : Int64.t;
  cp_message : string option;
  cp_author : string;
  cp_created_at : string;
}

(* The windowed decision table: compare each pin's head hash against its
   captured hash with ABSENT as its own value. Unchanged pins never enter the
   diff. added-since = live now, absent (or tombstoned) at the cp gen;
   removed-since = present at the cp gen, physically gone now (delete_fn
   removes the tree row; only the version trail keeps the tombstone). *)
type cp_diff_op = CpChanged | CpAdded | CpRemoved

let cp_diff_op_s = function
  | CpChanged -> "changed"
  | CpAdded -> "added-since"
  | CpRemoved -> "removed-since"

type cp_diff_row = {
  d_kind : string;           (* "fn" | "type" | "route" *)
  d_name : string;           (* decoded def name / route path *)
  d_op : cp_diff_op;
  d_head_hash : string option;
  d_cp_hash : string option;
  d_path : string;           (* live tree path ("" for removed pins) *)
  d_cp_body : string option; (* captured node payload jsonb (restore source) *)
}

type cp_log_row = {
  l_kind : string;
  l_name : string;
  l_op : string; (* create | update | delete *)
  l_author : string;
  l_gen : Int64.t;
  l_changed_at : string;
  l_origin : string;        (* eval | pick | file (042) *)
  l_origin_detail : string; (* "" when unset *)
}

type cp_restore_outcome = {
  ro_kind : string;
  ro_name : string;
  ro_op : string;
  ro_error : string option; (* None = restored *)
}

let cp_name_ok (n : string) : bool =
  String.length n >= 1
  && String.length n <= 64
  &&
  String.for_all
    (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-')
    n

let row_of ~branch_id name gen message author created =
  {
    cp_branch_id = branch_id;
    cp_name = name;
    cp_gen = gen;
    cp_message = message;
    cp_author = author;
    cp_created_at = created;
  }

(* ONE atomic statement: capture the branch's OWN head counter and insert, so
   no write can slip between read and insert. A duplicate (branch_id, name)
   refuses (DO NOTHING + empty RETURNING) — pick a new name; checkpoints are
   immutable anchors, not moving tags. *)
let create_checkpoint (t : Store_db.t) ~(branch_id : string) ~(name : string)
    ~(message : string option) ~(author : string) : (cp_row, string) result =
  match t.conn with
  | None -> Error "no database"
  | Some conn ->
      if not (cp_name_ok name) then
        Error "checkpoint name must match [a-z0-9-]+ (1..64 chars)"
      else
        (try
           let sql =
             "INSERT INTO branch_checkpoints (branch_id, name, gen, message, author) "^
             "SELECT $1::uuid, $2, COALESCE((SELECT generation FROM branch_generations "^
             "  WHERE branch_id = $1::uuid), 0), $3, $4 "^
             "ON CONFLICT (branch_id, name) DO NOTHING "^
             "RETURNING gen::text, created_at::text" in
           let msg_p =
             match message with
             | Some m -> Pgx.Value.of_string m
             | None -> Pgx.Value.null in
           let rows =
             Store_db.exec conn
               ~params:[
                 Pgx.Value.of_string branch_id;
                 Pgx.Value.of_string name;
                 msg_p;
                 Pgx.Value.of_string author ]
               sql in
           (match rows with
            | [ [ gen_c; at_c ] ] ->
                let gen =
                  match Pgx.Value.to_int64 gen_c with Some g -> g | None -> 0L in
                let at =
                  match Pgx.Value.to_string at_c with Some s -> s | None -> "" in
                Ok (row_of ~branch_id name gen message author at)
            | _ -> Error ("checkpoint name already in use: " ^ name))
         with exn -> Store_db.reraise ~op:"create_checkpoint" exn)

let list_checkpoints (t : Store_db.t) ~(branch_id : string) : cp_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT name, gen::text, message, author, created_at::text "^
           "FROM branch_checkpoints WHERE branch_id = $1::uuid ORDER BY id DESC" in
         let rows =
           Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql in
         List.filter_map
           (fun row ->
              match row with
              | [ name_c; gen_c; msg_c; author_c; at_c ] ->
                  (match
                     Pgx.Value.to_string name_c, Pgx.Value.to_int64 gen_c,
                     Pgx.Value.to_string author_c
                   with
                   | Some n, Some g, Some a ->
                       Some
                         (row_of ~branch_id n g (Pgx.Value.to_string msg_c) a
                            (match Pgx.Value.to_string at_c with
                             | Some s -> s
                             | None -> ""))
                   | _ -> None)
              | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"list_checkpoints" exn)

let lookup_checkpoint (t : Store_db.t) ~(branch_id : string) ~(name : string) :
    cp_row option =
  List.find_opt
    (fun (c : cp_row) -> String.equal c.cp_name name)
    (list_checkpoints t ~branch_id)

(* Windowed per-pin classify: live tree rows vs the latest version row at
   gen <= cp_gen. tree_dec_label decodes the encoded path label back to the
   def name / route path (036). A cp-state row that is itself a tombstone
   means the pin did not exist at the checkpoint. *)
let checkpoint_diff (t : Store_db.t) ~(branch_id : string) ~(cp_gen : Int64.t) :
    cp_diff_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "WITH own AS MATERIALIZED ( "^
           "  SELECT path::text AS path, "^
           "         (string_to_array(path::text,'.'))[3] AS kind, "^
           "         (string_to_array(path::text,'.'))[4] AS key, "^
           "         hash AS head_hash "^
           "  FROM tree_nodes "^
           "  WHERE path <@ ('b.' || replace($1::text, '-', ''))::ltree "^
           "    AND (string_to_array(path::text,'.'))[3] IN ('fn','type','route') "^
           "), cp_state AS MATERIALIZED ( "^
           "  SELECT DISTINCT ON (path) path::text AS path, hash AS cp_hash, "^
           "         body AS cp_body "^
           "  FROM tree_versions "^
           "  WHERE branch_id = $1::uuid AND gen <= $2::bigint "^
           "    AND kind IN ('fn','type','route') "^
           "  ORDER BY path, gen DESC "^
           ") "^
           "SELECT COALESCE(o.kind, (string_to_array(c.path,'.'))[3]) AS kind, "^
           "  tree_dec_label(COALESCE(o.key, (string_to_array(c.path,'.'))[4])) AS name, "^
           "  COALESCE(o.path, '') AS head_path, "^
           "  o.head_hash, c.cp_hash, COALESCE(c.cp_body::text, '') AS cp_body, "^
           "  (c.cp_body->>'_tombstone') AS tomb "^
           "FROM own o FULL OUTER JOIN cp_state c ON c.path = o.path" in
         let rows =
           Store_db.exec conn
             ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_int64 cp_gen ]
             sql in
         List.filter_map
           (fun row ->
              match row with
              | [ kind_c; name_c; head_path_c; head_h_c; cp_h_c; cp_body_c; tomb_c ]
                ->
                  let kind =
                    match Pgx.Value.to_string kind_c with
                    | Some k -> k
                    | None -> "" in
                  let name =
                    match Pgx.Value.to_string name_c with
                    | Some n -> n
                    | None -> "" in
                  let head_path =
                    match Pgx.Value.to_string head_path_c with
                    | Some s -> s
                    | None -> "" in
                  let head_hash = Pgx.Value.to_string head_h_c in
                  let cp_hash = Pgx.Value.to_string cp_h_c in
                  let cp_body =
                    match Pgx.Value.to_string cp_body_c with
                    | Some "" | None -> None
                    | Some s -> Some s in
                  let tomb = Pgx.Value.to_string tomb_c in
                  let op =
                    match (head_hash, cp_hash) with
                    | Some _, None -> Some CpAdded
                    | Some _, Some _ ->
                        if tomb = Some "true" then Some CpAdded
                        else if head_hash <> cp_hash then Some CpChanged
                        else None (* unchanged: filtered *)
                    | None, Some _ ->
                        if tomb = Some "true" then None (* deleted pre-cp *)
                        else Some CpRemoved
                    | None, None -> None in
                  (match op with
                   | None -> None
                   | Some op ->
                       Some
                         {
                           d_kind = kind;
                           d_name = name;
                           d_op = op;
                           d_head_hash = head_hash;
                           d_cp_hash = cp_hash;
                           d_path = head_path;
                           d_cp_body = cp_body;
                         })
              | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"checkpoint_diff" exn)

(* ---- op-transplant source resolution (branching.op-transplant,
   2026-10-16) ---- The same windowed version-trail read cp-diff uses, minus
   the classification: for branch/pick the source branch's pin bodies resolve
   at a coordinate (head / cp gen / explicit gen) as opaque jsonb payloads.
   Opaque bodies only — no Lisp interpreter references (store-split
   contract); the AST codec + closure live in branch_pick.ml at the
   eval-boundary layer. *)

type pin_row = {
  p_kind : string;    (* "fn" | "type" | "route" *)
  p_name : string;    (* decoded def name / route path *)
  p_hash : string;    (* payload hash — comparable across write paths *)
  p_payload : string; (* full node payload jsonb *)
}

(* The branch's OWN live pins: tree_nodes rows under the branch prefix (the
   `own` CTE of checkpoint_diff). *)
let pins_at_head (t : Store_db.t) ~(branch_id : string) : pin_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT (string_to_array(path::text,'.'))[3], "^
           "  tree_dec_label((string_to_array(path::text,'.'))[4]), "^
           "  hash, payload::text "^
           "FROM tree_nodes "^
           "WHERE path <@ ('b.' || replace($1::text, '-', ''))::ltree "^
           "  AND (string_to_array(path::text,'.'))[3] IN ('fn','type','route')" in
         let rows =
           Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql in
         List.filter_map
           (fun row ->
              match row with
              | [ kind_c; name_c; hash_c; payload_c ] ->
                  (match
                     Pgx.Value.to_string kind_c, Pgx.Value.to_string name_c,
                     Pgx.Value.to_string hash_c, Pgx.Value.to_string payload_c
                   with
                  | Some k, Some n, Some h, Some p ->
                      Some { p_kind = k; p_name = n; p_hash = h; p_payload = p }
                  | _ -> None)
              | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"pins_at_head" exn)

(* Latest non-tombstone version row per pin at gen <= cutoff (the cp_state
   discipline of checkpoint_diff). A pin absent — or tombstoned — at the
   cutoff simply has no row here: ABSENT is a value, not an error. *)
let pins_at_gen (t : Store_db.t) ~(branch_id : string) ~(cutoff : Int64.t) :
    pin_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT (string_to_array(path::text,'.'))[3], "^
           "  tree_dec_label((string_to_array(path::text,'.'))[4]), "^
           "  hash, body::text "^
           "FROM ( "^
           "  SELECT DISTINCT ON (path) path, hash, body "^
           "  FROM tree_versions "^
           "  WHERE branch_id = $1::uuid AND gen <= $2::bigint "^
           "    AND kind IN ('fn','type','route') "^
           "  ORDER BY path, gen DESC "^
           ") s WHERE (body->>'_tombstone') IS NULL" in
         let rows =
           Store_db.exec conn
             ~params:[
               Pgx.Value.of_string branch_id;
               Pgx.Value.of_int64 cutoff ]
             sql in
         List.filter_map
           (fun row ->
              match row with
              | [ kind_c; name_c; hash_c; payload_c ] ->
                  (match
                     Pgx.Value.to_string kind_c, Pgx.Value.to_string name_c,
                     Pgx.Value.to_string hash_c, Pgx.Value.to_string payload_c
                   with
                  | Some k, Some n, Some h, Some p ->
                      Some { p_kind = k; p_name = n; p_hash = h; p_payload = p }
                  | _ -> None)
              | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"pins_at_gen" exn)
(* JSON payload helpers. The captured tree payload is the FULL node envelope
   ('v','kind','path','type','body','author','doc','hash'); 'body' is the
   kind's natural content: raw fn body jsonb, type fields jsonb, or the route
   {route,handler,visibility} object. *)
let payload_fields (payload : string) : string option * string option =
  match Yojson.Safe.from_string payload with
  | `Assoc kv ->
      let body =
        match List.assoc_opt "body" kv with
        | Some `Null | None -> None
        | Some b -> Some (Yojson.Safe.to_string b) in
      let doc =
        match List.assoc_opt "doc" kv with
        | Some (`String d) -> Some d
        | _ -> None in
      (body, doc)
  | _ -> (None, None)

let route_fields (payload : string) : (string * string) option =
  match Yojson.Safe.from_string payload with
  | `Assoc kv -> (
      match List.assoc_opt "body" kv with
      | Some (`Assoc bv) ->
          let s k =
            match List.assoc_opt k bv with
            | Some (`String v) -> Some v
            | _ -> None in
          (match (s "handler", s "visibility") with
           | Some h, Some v -> Some (h, v)
           | _ -> None)
      | _ -> None)
  | _ -> None

(* Restore selected rows as NEW WRITES through the tree write helpers (tree is
   truth): upserts fire the version trail at fresh gens, the reverse mirrors
   update the def tables, and code_history before-images fire on the
   def-table UPDATEs. A pin whose captured body is missing (added-since) is
   skipped — there is nothing to restore it TO; a removed pin IS restorable
   (the captured body re-lands as an insert). base_gen is never touched. *)
let restore_checkpoint (t : Store_db.t) ~(branch_id : string)
    ~(rows : cp_diff_row list) ~(author : string) : cp_restore_outcome list =
  let outcome (r : cp_diff_row) err =
    {
      ro_kind = r.d_kind;
      ro_name = r.d_name;
      ro_op = cp_diff_op_s r.d_op;
      ro_error = err;
    } in
  match t.conn with
  | None -> List.map (fun r -> outcome r (Some "no database")) rows
  | Some conn ->
      List.map
        (fun (r : cp_diff_row) ->
           match r.d_cp_body with
           | None -> outcome r (Some "no captured body (added-since)")
           | Some payload -> (
               let res =
                 match r.d_kind with
                 | "fn" ->
                     let body, doc = payload_fields payload in
                     (match body with
                      | None -> Error "captured payload has no body"
                      | Some b ->
                          Tree_store.upsert_fn conn ~branch_id ~name:r.d_name
                            ~body_json:b ~author ~doc)
                 | "type" ->
                     let body, _ = payload_fields payload in
                     (match body with
                      | None -> Error "captured payload has no fields"
                      | Some f ->
                          Tree_store.upsert_type conn ~branch_id ~name:r.d_name
                            ~fields_json:f ~author)
                 | "route" -> (
                     match route_fields payload with
                     | None -> Error "captured payload has no handler/visibility"
                     | Some (handler, visibility) ->
                         Tree_store.upsert_route conn ~branch_id ~route:r.d_name
                           ~handler ~visibility)
                 | other -> Error ("unknown kind: " ^ other)
               in
               match res with
               | Ok _ -> outcome r None
               | Error why -> outcome r (Some why)))
        rows

(* The oplog: newest-first write timeline from the version trail (every def
   write fires it via the reverse mirror; code_history is the recovery
   substrate the restore path consumes). op classification runs over the FULL
   trail (row_number over the path's whole history) BEFORE the window is
   applied, so a cp-windowed log still classifies correctly. *)
let checkpoint_log (t : Store_db.t) ~(branch_id : string)
    ~(since_gen : Int64.t option) : cp_log_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT kind, name, op, author, gen, changed_at, origin, "^
           "  origin_detail FROM ( "^
           "  SELECT kind, "^
           "    tree_dec_label((string_to_array(path::text,'.'))[4]) AS name, "^
           "    CASE WHEN body->>'_tombstone' IS NOT NULL THEN 'delete' "^
           "         WHEN ROW_NUMBER() OVER (PARTITION BY path ORDER BY gen) = 1 "^
           "         THEN 'create' ELSE 'update' END AS op, "^
           "    author, gen, created_at AS changed_at, "^
           "    origin, COALESCE(origin_detail, '') AS origin_detail "^
           "  FROM tree_versions "^
           "  WHERE branch_id = $1::uuid AND kind IN ('fn','type','route') "^
           ") v WHERE ($2::bigint IS NULL OR gen > $2::bigint) "^
           "ORDER BY gen DESC LIMIT 64" in
         let p2 =
           match since_gen with
           | Some g -> Pgx.Value.of_int64 g
           | None -> Pgx.Value.null in
         let rows =
           Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id; p2 ] sql
         in
         List.filter_map
           (fun row ->
              match row with
              | [ kind_c; name_c; op_c; author_c; gen_c; at_c; origin_c; detail_c ] ->
                  (match
                     Pgx.Value.to_string kind_c, Pgx.Value.to_string name_c,
                     Pgx.Value.to_string op_c, Pgx.Value.to_string author_c,
                     Pgx.Value.to_int64 gen_c
                   with
                  | Some k, Some n, Some o, Some a, Some g ->
                      Some
                        {
                          l_kind = k;
                          l_name = n;
                          l_op = o;
                          l_author = a;
                          l_gen = g;
                          l_changed_at =
                            (match Pgx.Value.to_string at_c with
                             | Some s -> s
                             | None -> "");
                          l_origin =
                            (match Pgx.Value.to_string origin_c with
                             | Some s -> s
                             | None -> "eval");
                          l_origin_detail =
                            (match Pgx.Value.to_string detail_c with
                             | Some s -> s
                             | None -> "");
                        }
                  | _ -> None)
              | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"checkpoint_log" exn)
