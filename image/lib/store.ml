(** Pricklypear Image — Storage layer (branch-aware queries + fork/merge).
    Connection/tx substrate is [Store_db], included below.
    H3: SQL via [exec] on dual conn (lwt|eio from PP_IO). *)
include Store_db

(* ---- one-tree: reads source from the tree-backed views ----
   tree_function_defs / tree_type_defs / tree_view_bindings / tree_data_rows
   are projections over tree_nodes. The scaffolding tables (function_defs,
   type_defs, view_bindings, data_rows) are derived via the reverse mirror
   and kept warm for residual readers / fork-copy paths. *)
let fn_rel () = "tree_function_defs"
let ty_rel () = "tree_type_defs"
let rt_rel () = "tree_view_bindings"
let pkg_rel () = "tree_package_registry"

(** Cheap visibility signature for staleness checks
    (image.per-identity-sessions): count + max(updated_at) of
    function/type/route defs over the current branch chain, one round
    trip. A parked session compares this against the value from its
    last load; mismatch => rehydrate. *)
let visible_stamp (t : t) : string =
  match t.conn with
  | None -> ""
  | Some conn ->
      (try
         let agg rel =
           "(SELECT count(*)::text || '|' || COALESCE(max(updated_at)::text, '-') "
           ^ "FROM " ^ rel ^ " d JOIN chain ON d.branch_id = chain.id)" in
         let sql =
           "WITH RECURSIVE chain AS ( "
           ^ "  SELECT id, parent_id FROM branches WHERE id = $1 "
           ^ "  UNION ALL "
           ^ "  SELECT b.id, b.parent_id FROM branches b JOIN chain c ON b.id = c.parent_id "
           ^ ") SELECT " ^ agg (fn_rel ()) ^ " || '/' || " ^ agg (ty_rel ())
           ^ " || '/' || " ^ agg (rt_rel ()) in
         match exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql with
         | [ [ c ] ] -> (match Pgx.Value.to_string c with Some s -> s | None -> "")
         | _ -> ""
       with _ -> "")

(* Branch-aware lookup. Walks the branch parent chain via a recursive
   CTE so that a definition in [main] is visible from a child branch,
   and a definition in the child OVERRIDES the same name in main
   (per spec storage.no-inheritance: OVERRIDE, not UNION).

   Returns (body, author, doc option). Errors/misses are logged and
   return None so the REPL keeps working with no DB. *)
let lookup_function (t : t) (name : string)
  : (Pricklypear_lisp.Ast.expr * string * string option) option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      (* Recursive CTE: start at the session's branch and walk parents.
             Return the FIRST row matching [name] when ordered by depth
             ascending (closest ancestor wins). *)
          let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.body, fd.author, fd.doc FROM " ^ fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "WHERE fd.name = $2 " ^
            "ORDER BY chain.depth ASC LIMIT 1" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name ]
              sql in
          (match rows with
           | [] -> None
           | row :: _ ->
               (* body jsonb, author text, doc text nullable. *)
               (match row with
                | [ body_cell; author_cell; doc_cell ] ->
                    (match Pgx.Value.to_string body_cell,
                             Pgx.Value.to_string author_cell with
                     | Some body_str, Some author ->
                        let doc = Pgx.Value.to_string doc_cell in
                        (match Yojson.Safe.from_string body_str with
                         | j ->
                             (match Pricklypear_lisp.Ast.expr_of_json j with
                              | Ok e -> (Some (e, author, doc))
                              | Error msg ->
                                  Printf.eprintf
                                    "[store] lookup_function: parse %s\n" msg;
                                  None)
                         | exception Yojson.Json_error msg ->
                             Printf.eprintf
                               "[store] lookup_function: json %s\n" msg;
                             None)
                     | _ -> None)
                | _ -> None))
    with exn ->
      Printf.eprintf "[store] lookup_function error: %s\n"
            (Printexc.to_string exn);
          None)

(* List all function names visible to this branch (via the same
   recursive-CTE override walk). Used at session-make time to load
   persisted definitions into the env. *)
(** Effective function defs for the current branch: parent-chain walk,
    closest definition wins (depth ASC, first occurrence kept).
    [depth] is 0 for rows owned by the current branch, >0 if inherited.
    Callers that only need name/body/author can ignore depth. *)
let list_function_names_meta (t : t)
  : (string * Pricklypear_lisp.Ast.expr * string * int) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.name, fd.body, fd.author, chain.depth, fd.package " ^
            "FROM " ^ fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC, fd.name ASC" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql in
          let seen = Hashtbl.create 64 in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; body_cell; author_cell; depth_c; _pkg_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string body_cell,
                            Pgx.Value.to_string author_cell,
                            Pgx.Value.to_string depth_c with
                     | Some name, Some body_str, Some author, Some d_s ->
                         if Hashtbl.mem seen name then None
                         else begin
                           Hashtbl.replace seen name ();
                           let depth =
                             int_of_string_opt d_s |> Option.value ~default:0
                           in
                           (try
                              let j = Yojson.Safe.from_string body_str in
                              (match Pricklypear_lisp.Ast.expr_of_json j with
                               | Ok e -> Some (name, e, author, depth)
                               | Error _ -> None)
                            with Yojson.Json_error _ -> None)
                         end
                     | _ -> None)
                | _ -> None)
              rows
          in
          acc
    with exn ->
      Printf.eprintf "[store] list_function_names error: %s\n"
            (Printexc.to_string exn);
          [])

(** Effective defs only (deduped). Same as [list_function_names_meta] without depth. *)
let list_function_names (t : t)
  : (string * Pricklypear_lisp.Ast.expr * string) list =
  let rows = list_function_names_meta t in
  (List.map (fun (n, e, a, _) -> (n, e, a)) rows)

(** Substring search over effective function bodies (JSONB ::text) on the
    current branch inheritance chain. Closest definition wins (depth ASC
    dedup). [needle] is matched with SQL ILIKE '%needle%' (no user-supplied
    wildcards expanded beyond that). Returns names only, ordered by name. *)
let search_function_bodies (t : t) ~(needle : string) ~(limit : int)
  : string list =
  let needle = String.trim needle in
  let limit = if limit <= 0 then 200 else min limit 2000 in
  if needle = "" then []
  else
    match t.conn with
    | None -> []
    | Some conn ->
        (try
           let sql =
             "WITH RECURSIVE chain AS ( " ^
             "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
             "  UNION ALL " ^
             "  SELECT b.id, b.parent_id, c.depth + 1 " ^
             "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
             "), ranked AS ( " ^
             "  SELECT fd.name, chain.depth, " ^
             "         row_number() OVER (PARTITION BY fd.name ORDER BY chain.depth ASC) AS rn " ^
             "  FROM " ^ fn_rel () ^ " fd " ^
             "  JOIN chain ON fd.branch_id = chain.id " ^
             "  WHERE fd.body::text ILIKE '%' || $2 || '%' " ^
             ") SELECT name FROM ranked WHERE rn = 1 ORDER BY name ASC LIMIT $3"
           in
           let rows =
             exec conn
               ~params:
                 [ Pgx.Value.of_string t.branch_id
                 ; Pgx.Value.of_string needle
                 ; Pgx.Value.of_string (string_of_int limit)
                 ]
               sql
           in
           List.filter_map
             (fun row ->
               match row with
               | [ name_c ] -> Pgx.Value.to_string name_c
               | _ -> None)
             rows
         with exn ->
           Printf.eprintf "[store] search_function_bodies error: %s\n"
             (Printexc.to_string exn);
           [])

(** Full branch-chain defs for env rehydrate: parents first (depth DESC),
    then children. Includes ALL rows (not deduped) so Session.load_defs can
    redefine — last write wins (child override). 4th = package provenance
    (None = untagged / always load). See session.load_defs note. *)
let list_function_names_for_load (t : t)
  : (string * Pricklypear_lisp.Ast.expr * string * string option) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.name, fd.body, fd.author, chain.depth, fd.package " ^
            "FROM " ^ fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth DESC, fd.name ASC" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; body_cell; author_cell; _depth_c; pkg_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string body_cell,
                            Pgx.Value.to_string author_cell with
                     | Some name, Some body_str, Some author ->
                         let pkg = Pgx.Value.to_string pkg_c in
                         (try
                            let j = Yojson.Safe.from_string body_str in
                            (match Pricklypear_lisp.Ast.expr_of_json j with
                             | Ok e -> Some (name, e, author, pkg)
                             | Error _ -> None)
                          with Yojson.Json_error _ -> None)
                     | _ -> None)
                | _ -> None)
              rows
          in
          acc
    with exn ->
      Printf.eprintf "[store] list_function_names_for_load error: %s\n"
            (Printexc.to_string exn);
          [])

(* ---- own-branch stats / overrides (no recursive CTE) ---- *)

let count_functions_on_branch (t : t) ~(branch_id : string) : int =
  match t.conn with
  | None -> 0
  | Some conn ->
      (try
      let sql =
            "SELECT COUNT(*)::text FROM function_defs WHERE branch_id = $1" in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_string c with
                | Some s -> (int_of_string_opt s |> Option.value ~default:0)
                | None -> 0)
           | _ -> 0)
    with exn ->
      Printf.eprintf "[store] count_functions_on_branch: %s\n"
            (Printexc.to_string exn);
          0)

let count_routes_on_branch (t : t) ~(branch_id : string) : int =
  match t.conn with
  | None -> 0
  | Some conn ->
      (try
      let sql =
            "SELECT COUNT(*)::text FROM view_bindings WHERE branch_id = $1" in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_string c with
                | Some s -> (int_of_string_opt s |> Option.value ~default:0)
                | None -> 0)
           | _ -> 0)
    with exn ->
      Printf.eprintf "[store] count_routes_on_branch: %s\n"
            (Printexc.to_string exn);
          0)

(** Parent branch id for [branch_id], if any. *)
let branch_parent_id (t : t) ~(branch_id : string) : string option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT COALESCE(parent_id::text, '') FROM branches WHERE id = $1"
          in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          match rows with
          | [ [ pid ] ] ->
              (match Pgx.Value.to_string pid with
               | Some "" | None -> None
               | Some s -> (Some s))
          | _ -> None
    with _ ->
      None)

(** Branch LENS CHAIN for [branch_id]: self-first (closest-wins) walk up
    the parent chain to system/main (inclusive), oldest ancestor last.
    Used by the lens-aware tree read prims (one-tree.tree-read-prims) so a
    session on <user>/personal sees the effective override chain for
    tree_nodes under b.<id>.* — same closest-ancestor-wins rule function_
    defs use. A branch with no reachable parent just resolves to itself. *)
let branch_chain (t : t) ~(branch_id : string) : string list =
  let rec walk acc id =
    match branch_parent_id t ~branch_id:id with
    | Some parent when parent <> id ->
        walk (id :: acc) parent
    | _ -> (id :: acc)
  in
  (* walk builds oldest-first; reverse to self-first *)
  List.rev (walk [] branch_id)

(** Own rows only: (name, author, updated_at). *)
let list_own_functions (t : t) ~(branch_id : string)
  : (string * string * string) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT name, author, updated_at::text FROM function_defs " ^
            "WHERE branch_id = $1 ORDER BY name" in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ n; a; u ] ->
                     (match Pgx.Value.to_string n,
                             Pgx.Value.to_string a,
                             Pgx.Value.to_string u with
                      | Some name, Some author, Some updated ->
                          Some (name, author, updated)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn ->
      Printf.eprintf "[store] list_own_functions: %s\n"
            (Printexc.to_string exn);
          [])

let list_own_routes (t : t) ~(branch_id : string)
  : (string * string) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT route, handler FROM view_bindings WHERE branch_id = $1 " ^
            "ORDER BY route" in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ r; h ] ->
                     (match Pgx.Value.to_string r, Pgx.Value.to_string h with
                      | Some route, Some handler -> Some (route, handler)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn ->
      Printf.eprintf "[store] list_own_routes: %s\n"
            (Printexc.to_string exn);
          [])

(** Child own names left-joined to parent: name, child_updated, parent_updated opt. *)
(* ---- branch status (branching.branch-status) ----

   Git-style per-name status of a branch against its parent chain, as ONE
   derived read of the tree-time substrate (tree_versions, migrations
   018-021) keyed on the base_gen merge-base coordinate (migration 024).
   Hash-first per tree-time: equal hash = prune; bodies are never fetched.

   Per name (fn | type | route), with values resolved closest-ancestor-wins
   (a tombstone or missing newest version on one branch means that branch
   is dead for the name and the walk continues up, matching the
   scaffolding/unbind semantics):
     b = branch value at own head
     p = parent-chain value at parent head
     a = parent-chain value at base_gen (the fork-time base)
     own = a live scaffolding row exists on the branch (the pin)

   HASH DOMAIN (make-phase lesson, 2026-08-28): the STORED
   tree_versions.hash column is NOT comparable across write paths —
   tree-first writes (Tree_store.hash_body over compact OCaml JSON) and
   mirror writes (009 trigger digest over PG jsonb::text, which spaces
   after colons) produce DIFFERENT digests for identical bodies (020
   itself calls the stored hash "a consistency artifact, not content").
   Comparing stored hashes mislabeled identical pins as diverged. Status
   therefore computes digest((body->'body')::text) AT QUERY TIME — one
   canonical domain (PG jsonb::text) regardless of which path wrote the
   row. The stored column stays for retention/GC bookkeeping and
   same-path gen-diff; a global rehash is tree-time's scope, not ours.

   Tombstones are the reserved 019/020 marker (body->_tombstone). Deletion
   on a branch REVEALS the parent binding (unbind semantics), so there is
   no "locally deleted" status — a delete is a pin removal, not a divergent
   state.

   APPROXIMATION (documented in the spec): base_gen bounds ONLY the direct
   parent's versions; ancestors above the parent contribute their full
   head. Exact for the dominant topology (empty child of system/main);
   for deeper chains, post-fork writes on grandparents fold into the base.
   Accepted v1; revisit with per-ancestor bounds if it ever misleads. *)

type diff_kind = DiffFn | DiffType | DiffRoute

type st_status = SClean | SAheadNew | SAhead | SBehind | SDiverged | SSynced

let st_status_s = function
  | SClean -> "clean"
  | SAheadNew -> "ahead-new"
  | SAhead -> "ahead"
  | SBehind -> "behind"
  | SDiverged -> "diverged"
  | SSynced -> "synced"

type branch_status_row = {
  st_kind : diff_kind;              (* DiffFn | DiffType | DiffRoute *)
  st_name : string;                 (* fn/type name, or the route path *)
  st_status : st_status;
  st_own : bool;                    (* live pin on the branch *)
  st_branch_hash : string option;   (* b *)
  st_parent_hash : string option;   (* p *)
  st_base_hash : string option;     (* a *)
}

let hash_eq (x : string option) (y : string option) : bool =
  match x, y with
  | None, None -> true
  | Some a, Some b -> String.equal a b
  | _ -> false

(* The spec's decision table, as code. Comparisons are hash equality with
   ABSENT (None) as its own value. *)
let classify_status ~(own : bool) ~(b : string option) ~(p : string option)
    ~(a : string option) : st_status =
  if not own then (if hash_eq p a then SClean else SBehind)
  else if hash_eq b p then SSynced
  else if hash_eq p a then
    (match a with None -> SAheadNew | Some _ -> SAhead)
  else if hash_eq b a then SBehind   (* pin holds the base; parent moved *)
  else SDiverged

(* One round trip per call: chain walk + version resolution for all three
   kinds (fn/type/route) + live-pin existence from the scaffolding tables.
   Read-only (plain exec on the given conn — callers inside a transaction
   pass the tx conn so the read shares its snapshot). *)
let branch_status_conn (conn : conn) ~(branch_id : string)
  : branch_status_row list =
  (try
        let sql =
          "WITH RECURSIVE chain AS ( "^
          "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 "^
          "  UNION ALL "^
          "  SELECT b.id, b.parent_id, c.depth + 1 "^
          "  FROM branches b JOIN chain c ON b.id = c.parent_id "^
          "), vers AS ( "^
          "  SELECT v.kind, "^
          "         (string_to_array(v.path::text, '.'))[4] AS key, "^
          "         c.depth, v.gen, v.hash, v.body "^
          "  FROM tree_versions v "^
          "  JOIN chain c ON c.id = v.branch_id "^
          "  WHERE v.kind IN ('fn','type','route') "^
          "), heads AS ( "^
          "  SELECT DISTINCT ON (kind, key, depth) kind, key, depth, "^
          "         encode(digest((body->'body')::text,'sha256'),'hex') "^
          "           AS chash, body "^
          "  FROM vers ORDER BY kind, key, depth, gen DESC "^
          "), b_res AS ( "^
          "  SELECT DISTINCT ON (kind, key) kind, key, chash AS b_hash "^
          "  FROM heads WHERE (body->>'_tombstone') IS NULL "^
          "  ORDER BY kind, key, depth ASC "^
          "), p_res AS ( "^
          "  SELECT DISTINCT ON (kind, key) kind, key, chash AS p_hash "^
          "  FROM heads WHERE depth >= 1 AND (body->>'_tombstone') IS NULL "^
          "  ORDER BY kind, key, depth ASC "^
          "), vers_base AS ( "^
          "  SELECT v.kind, "^
          "         (string_to_array(v.path::text, '.'))[4] AS key, "^
          "         c.depth, v.gen, v.hash, v.body "^
          "  FROM tree_versions v "^
          "  JOIN chain c ON c.id = v.branch_id "^
          "  WHERE v.kind IN ('fn','type','route') "^
          "    AND c.depth >= 1 "^
          "    AND (c.depth > 1 OR v.gen <= (SELECT COALESCE(base_gen, 0) "^
          "                                   FROM branches WHERE id = $1)) "^
          "), heads_base AS ( "^
          "  SELECT DISTINCT ON (kind, key, depth) kind, key, depth, "^
          "         encode(digest((body->'body')::text,'sha256'),'hex') "^
          "           AS chash, body "^
          "  FROM vers_base ORDER BY kind, key, depth, gen DESC "^
          "), a_res AS ( "^
          "  SELECT DISTINCT ON (kind, key) kind, key, chash AS a_hash "^
          "  FROM heads_base WHERE depth >= 1 AND (body->>'_tombstone') IS NULL "^
          "  ORDER BY kind, key, depth ASC "^
          "), own_fn AS ( "^
          "  SELECT tree_enc_label(name) AS key FROM function_defs "^
          "  WHERE branch_id = $1 "^
          "), own_ty AS ( "^
          "  SELECT tree_enc_label(name) AS key FROM type_defs "^
          "  WHERE branch_id = $1 "^
          "), own_rt AS ( "^
          "  SELECT tree_enc_label(route) AS key FROM view_bindings "^
          "  WHERE branch_id = $1 "^
          "), keys AS ( "^
          "  SELECT kind, key FROM heads "^
          "  UNION SELECT 'fn', key FROM own_fn "^
          "  UNION SELECT 'type', key FROM own_ty "^
          "  UNION SELECT 'route', key FROM own_rt "^
          ") SELECT k.kind, tree_dec_label(k.key) AS name, "^
          "       b.b_hash, p.p_hash, a.a_hash, "^
          "       (o1.key IS NOT NULL) AS own_fn, "^
          "       (o2.key IS NOT NULL) AS own_ty, "^
          "       (o3.key IS NOT NULL) AS own_rt "^
          "FROM keys k "^
          "LEFT JOIN b_res b ON b.kind = k.kind AND b.key = k.key "^
          "LEFT JOIN p_res p ON p.kind = k.kind AND p.key = k.key "^
          "LEFT JOIN a_res a ON a.kind = k.kind AND a.key = k.key "^
          "LEFT JOIN own_fn o1 ON o1.key = k.key "^
          "LEFT JOIN own_ty o2 ON o2.key = k.key "^
          "LEFT JOIN own_rt o3 ON o3.key = k.key "^
          "ORDER BY k.kind, name"
        in
        let rows = exec conn ~params:[ Pgx.Value.of_string branch_id ] sql in
        List.filter_map
          (fun row ->
            match row with
            | [ kind_c; name_c; bh_c; ph_c; ah_c; ofn_c; oty_c; ort_c ] ->
                let kind =
                  match Pgx.Value.to_string kind_c with
                  | Some "type" -> DiffType
                  | Some "route" -> DiffRoute
                  | _ -> DiffFn
                in
                let name = Pgx.Value.to_string name_c in
                let h c = Pgx.Value.to_string c in
                let b = h bh_c and p = h ph_c and a = h ah_c in
                let own =
                  match kind with
                  | DiffFn -> Pgx.Value.to_bool ofn_c = Some true
                  | DiffType -> Pgx.Value.to_bool oty_c = Some true
                  | DiffRoute -> Pgx.Value.to_bool ort_c = Some true
                in
                (match name with
                 | None -> None
                 | Some name ->
                     (* Nothing live anywhere (tombstoned on every head):
                        both sides agree it is gone - nothing to report. *)
                     if (not own) && b = None then None
                     else
                       Some
                         { st_kind = kind;
                           st_name = name;
                           st_status =
                             classify_status ~own ~b ~p ~a;
                           st_own = own;
                           st_branch_hash = b;
                           st_parent_hash = p;
                           st_base_hash = a })
            | _ -> None)
          rows
        with exn ->
        Printf.eprintf "[store] branch_status error: %s\n"
          (Printexc.to_string exn);
        [])

let branch_status (t : t) ~(branch_id : string) : branch_status_row list =
  match t.conn with
  | None -> []
  | Some conn -> branch_status_conn conn ~branch_id

let lookup_function_on_branch (t : t) ~(branch_id : string) ~(name : string)
  : (Pricklypear_lisp.Ast.expr * string * string * string option) option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT body::text, author, updated_at::text, doc FROM function_defs " ^
            "WHERE branch_id = $1 AND name = $2" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string name ]
              sql
          in
          (match rows with
           | [ [ body_c; author_c; upd_c; doc_c ] ] ->
               (match Pgx.Value.to_string body_c,
                       Pgx.Value.to_string author_c,
                       Pgx.Value.to_string upd_c with
                | Some body_str, Some author, Some updated ->
                    let doc = Pgx.Value.to_string doc_c in
                    (try
                       match Pricklypear_lisp.Ast.expr_of_json
                               (Yojson.Safe.from_string body_str) with
                       | Ok e -> (Some (e, author, updated, doc))
                       | Error _ -> None
                     with _ -> None)
                | _ -> None)
           | _ -> None)
    with exn ->
      Printf.eprintf "[store] lookup_function_on_branch: %s\n"
            (Printexc.to_string exn);
          None)

let delete_function_on_branch (t : t) ~(branch_id : string) ~(name : string)
  : bool =
  match t.conn with
  | None -> false
  | Some conn ->
      (try
            Tree_store.delete_fn conn ~branch_id ~name
    with exn ->
      Printf.eprintf "[store] delete_function_on_branch: %s\n"
            (Printexc.to_string exn);
          false)

(** Copy selected function names from source branch to target (upsert). *)
let promote_functions (t : t) ~(source_id : string) ~(target_id : string)
    ~(names : string list) : int =
  match t.conn with
  | None -> 0
  | Some _ when names = [] -> 0
  | Some conn ->
      (try
      let n =
            List.fold_left
              (fun acc name ->
                let src =
                  lookup_function_on_branch t ~branch_id:source_id ~name
                in
                match src with
                | None -> acc
                | Some _ ->
                    let sql =
                      "INSERT INTO function_defs " ^
                      "  (branch_id, name, body, author, doc, package) " ^
                      "SELECT $1, name, body, author, doc, package " ^
                      "FROM function_defs " ^
                      "WHERE branch_id = $2 AND name = $3 " ^
                      "ON CONFLICT (branch_id, name) DO UPDATE " ^
                      "SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
                      "    doc = EXCLUDED.doc, package = EXCLUDED.package, " ^
                      "    updated_at = now() RETURNING name" in
                    let rows =
                      exec conn
                        ~params:[ Pgx.Value.of_string target_id;
                                  Pgx.Value.of_string source_id;
                                  Pgx.Value.of_string name ]
                        sql
                    in
                    (if rows = [] then acc else acc + 1))
              0 names
          in
          (* branching.promote ROUTES TRAVEL WITH THEIR HANDLER:
             upsert source view_bindings whose handler is a promoted name;
             preserve visibility. Non-promoted handlers' routes stay put. *)
          let () =
            List.iter
              (fun name ->
                let sql =
                  "INSERT INTO view_bindings " ^
                  "  (branch_id, route, handler, visibility) " ^
                  "SELECT $1, route, handler, " ^
                  "       COALESCE(visibility, 'private') " ^
                  "FROM view_bindings " ^
                  "WHERE branch_id = $2 AND handler = $3 " ^
                  "ON CONFLICT (branch_id, route) DO UPDATE " ^
                  "SET handler = EXCLUDED.handler, " ^
                  "    visibility = EXCLUDED.visibility"
                in
                let _ =
                  exec conn
                    ~params:
                      [ Pgx.Value.of_string target_id
                      ; Pgx.Value.of_string source_id
                      ; Pgx.Value.of_string name ]
                    sql
                in
                ())
              names
          in
          let () = Tree_store.recompute_head_hash conn ~branch_id:target_id in
          n
    with exn ->
      Printf.eprintf "[store] promote_functions: %s\n"
            (Printexc.to_string exn);
          0)

(* Persist a function def. INSERT ... ON CONFLICT (branch_id, name)
   DO UPDATE so redefining a name in the same branch upserts. The body
   is serialized to JSONB. [doc] writes function_defs.doc; None clears
   (NULL) so stale docs do not survive a docstring-free redefine. *)
let persist_function (t : t) (name : string)
    (body : Pricklypear_lisp.Ast.expr)
    ?(doc : string option = None)
    ?(package : string option = None)
    (author : string)
  : unit =
  match t.conn with
  | None -> ()  (* no DB; env binding still happened in-memory *)
  | Some conn ->
      (try
      let body_json =
            Yojson.Safe.to_string
              (Pricklypear_lisp.Ast.expr_to_json body) in
          (* tree is truth; reverse trigger mirrors to table. Package
             provenance is scaffolding-only — stamped on function_defs
             directly since the tree node envelope has no package field. *)
            let () =
              Tree_store.upsert_fn conn ~branch_id:t.branch_id ~name
                ~body_json ~author ~doc
            in
            (match package with
             | None -> ()
             | Some pkg ->
                 let sql =
                   "UPDATE function_defs SET package = $3 " ^
                   "WHERE branch_id = $1 AND name = $2" in
                 let _ =
                   exec conn
                     ~params:[ Pgx.Value.of_string t.branch_id;
                               Pgx.Value.of_string name;
                               Pgx.Value.of_string pkg ]
                     sql in
                 ())
    with exn ->
      Printf.eprintf "[store] persist_function error: %s\n"
            (Printexc.to_string exn);
          ())

(** image.boot-seed-batch: preload the set of function names and route
    paths already present on the current branch (system/main during boot
    seed) so persist_function_if_absent / register_route_if_absent skip
    the per-name get_by_path SELECT. Call inside the seed with_branch on
    system/main; pair with [end_seed_names] in a finally. *)
let begin_seed_names (t : t) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      let set = Hashtbl.create 1024 in
      (try
         let sql = "SELECT name FROM " ^ fn_rel () ^ " WHERE branch_id = $1" in
         let rows = exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql in
         List.iter (fun row ->
             match row with
             | [ name_c ] ->
                 (match Pgx.Value.to_string name_c with
                  | Some n -> Hashtbl.replace set n ()
                  | None -> ())
             | _ -> ()) rows
       with exn ->
         Printf.eprintf "[store] begin_seed_names (fns) error: %s\n"
           (Printexc.to_string exn));
      (try
         let sql = "SELECT route FROM " ^ rt_rel () ^ " WHERE branch_id = $1" in
         let rows = exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql in
         List.iter (fun row ->
             match row with
             | [ route_c ] ->
                 (match Pgx.Value.to_string route_c with
                  | Some r -> Hashtbl.replace set r ()
                  | None -> ())
             | _ -> ()) rows
       with exn ->
         Printf.eprintf "[store] begin_seed_names (routes) error: %s\n"
           (Printexc.to_string exn));
      t.seed_names <- Some set

let end_seed_names (t : t) : unit =
  t.seed_names <- None

(** branching.seed-policy: insert only if (branch_id, name) absent.
    Existing main (or any branch) rows are left untouched. Used by boot
    seed so deploy cannot clobber live promote. *)
let persist_function_if_absent (t : t) (name : string)
    (body : Pricklypear_lisp.Ast.expr)
    ?(doc : string option = None)
    ?(package : string option = None)
    (author : string)
  : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (* image.boot-seed-batch: during the additive boot seed pass, consult
         the preloaded name set instead of issuing a get_by_path SELECT per
         define. Additive seed never overwrites existing rows, so an
         existing name is a full skip. A genuinely new name upserts and
         records itself so later duplicate defines skip too. *)
      (match t.seed_names with
       | Some set when Hashtbl.mem set name -> ()
       | _ ->
      (try
      let body_json =
            Yojson.Safe.to_string
              (Pricklypear_lisp.Ast.expr_to_json body) in
          (* seed-policy: never overwrite body; fill package provenance
             on the existing scaffolding row when untagged. *)
            let exists =
              Tree_store.get_by_path conn
                ~path:(Tree_store.fn_path ~branch_id:t.branch_id ~name) in
            let stamp_pkg_if_null () =
              match package with
              | None -> ()
              | Some pkg ->
                  let sql =
                    "UPDATE function_defs SET package = $3 " ^
                    "WHERE branch_id = $1 AND name = $2 AND package IS NULL" in
                  let _ =
                    exec conn
                      ~params:[ Pgx.Value.of_string t.branch_id;
                                Pgx.Value.of_string name;
                                Pgx.Value.of_string pkg ]
                      sql in
                  ()
            in
            (match exists with
             | Some _ -> stamp_pkg_if_null ()
             | None ->
                 let () =
                   Tree_store.upsert_fn conn ~branch_id:t.branch_id ~name
                     ~body_json ~author ~doc
                 in
                 stamp_pkg_if_null ());
            (match t.seed_names with
             | Some set -> Hashtbl.replace set name ()
             | None -> ())
      with exn ->
      Printf.eprintf "[store] persist_function_if_absent error: %s\n"
            (Printexc.to_string exn);
          ()))

(** Visible docstring for [name], or None. *)
let lookup_function_doc (t : t) (name : string) : string option =
  let found = lookup_function t name in
  match found with
  | Some (_, _, Some d) when String.trim d <> "" -> Some d
  | _ -> None

let list_branches (t : t) : Pricklypear_common.Branch.t list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT id::text, COALESCE(parent_id::text, ''), " ^
            "       owner, name, created_at::text, " ^
            "       COALESCE(last_synced::text, ''), " ^
            "       COALESCE(base_gen, 0)::bigint " ^
            "FROM branches ORDER BY owner, name" in
          let rows = exec conn sql in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ id_c; pid_c; owner_c; name_c; created_c; synced_c; bg_c ] ->
                     (match Pgx.Value.to_string id_c,
                              Pgx.Value.to_string owner_c,
                              Pgx.Value.to_string name_c,
                              Pgx.Value.to_string created_c with
                      | Some id, Some owner, Some name, Some created ->
                          let parent_id =
                            match Pgx.Value.to_string pid_c with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          let last_synced =
                            match Pgx.Value.to_string synced_c with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          let base_gen =
                            match Pgx.Value.to_int64 bg_c with
                            | Some g -> g
                            | None -> 0L
                          in
                          let base =
                            Pricklypear_common.Branch.make
                               ~id ~parent_id ~owner ~name
                               ~created_at:created ~base_gen ()
                          in
                          Some
                            (match last_synced with
                             | Some s -> { base with last_synced = Some s }
                             | None -> base)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn ->
      Printf.eprintf "[store] list_branches error: %s\n"
            (Printexc.to_string exn);
          [])

(* Look up a branch id by (owner, name). Used by (fork ...) to find
   the parent's UUID from its Lisp-level reference. Returns None if
   the branch isn't in the DB (or no DB). *)
let lookup_branch_id_by_name (t : t) ~(owner : string) ~(name : string)
  : string option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      let sql = "SELECT id::text FROM branches WHERE owner = $1 AND name = $2" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string owner;
                        Pgx.Value.of_string name ]
              sql
          in
          (match rows with
           | [ [ id_cell ] ] ->
               (Pgx.Value.to_string id_cell)
           | _ -> None)
    with exn ->
      Printf.eprintf "[store] lookup_branch_id error: %s\n"
            (Printexc.to_string exn);
          None)

(* Switch the store's branch_id: subsequent lookups/persists/registers
   will target the new branch. Per spec [image.repl-experience]:
   "(checkout branch-id)" keeps REPL locals (env) but changes visibility
   (which (define)d functions are effective). We mutate the field.

   Re-loading the env is the caller's responsibility (session.eval_one
   does it after we return successfully). *)
let checkout_branch (t : t) ~(new_branch_id : string) : unit =
  t.branch_id <- new_branch_id;
  ()

(* Get the branch_id the store is currently bound to. *)
let current_branch_id (t : t) : string = t.branch_id

(** Current generation counter for [branch_id] (migration 017). The SQL
    helper [current_branch_generation] walks the branch's ancestry chain
    and returns the max generation across it, so a write to an ancestor
    (e.g. main) is automatically visible to every descendant. The remote
    client stores defs under a [(branch, generation)] key and does a
    single cheap round trip against this to decide whether its local
    def cache is stale. Returns None when the DB is unreachable / the
    helper is missing (pre-017), letting callers treat an unknown
    generation as "reload". *)
let current_generation (t : t) ~(branch_id : string) : int64 option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
         let sql =
           "SELECT COALESCE(current_branch_generation($1::uuid), 0)::bigint"
         in
         let rows =
           exec conn
             ~params:[ Pgx.Value.of_string branch_id ]
             sql
         in
         match rows with
         | [ [ cell ] ] ->
             Option.bind (Pgx.Value.to_int64 cell) (fun g -> Some g)
         | _ -> None
       with exn ->
         Printf.eprintf "[store] current_generation error: %s\n"
           (Printexc.to_string exn);
         None)

(** Canonical system/main UUID — seeded by migrations/001_branches.sql. *)
let system_main_id = "00000000-0000-0000-0000-000000000001"

(** Delete the (branch_id, name) row from function_defs. Used by
    (unbind name): removes the CURRENT branch's override so the name
    resolves via inheritance to the shadowed parent binding. Refuses
    to delete on system/main (the inheritance root). Returns true if
    a row was deleted, false if no branch-local row existed. *)
let delete_function (t : t) (name : string) : bool =
  if t.branch_id = system_main_id then
    raise (Failure "unbind: cannot unbind on system/main")
  else
    match t.conn with
    | None -> false
    | Some conn ->
        (try
          (* tree is truth; reverse mirror deletes the function_defs row *)
          Tree_store.delete_fn conn ~branch_id:t.branch_id ~name
        with exn ->
          Printf.eprintf "[store] delete_function error: %s\n"
            (Printexc.to_string exn);
          false)

(** Temporarily rebind [t.branch_id] for the duration of [f], then restore.
    Used so seed_stdlib can write package defaults to system/main without
    polluting the user's personal branch. *)
let with_branch (t : t) ~(branch_id : string) (f : unit -> 'a) : 'a =
  let old = t.branch_id in
  t.branch_id <- branch_id;
  Fun.protect ~finally:(fun () -> t.branch_id <- old) f

(** Ensure a branch row exists; create an EMPTY child (parent pointer only)
    if missing. Does NOT copy function_defs/types/routes — inheritance via
    the recursive CTE makes parent defs visible, and personal overrides are
    just rows on the child. This is the right shape for user/personal. *)
let ensure_branch (t : t) ~(parent_id : string) ~(owner : string)
    ~(name : string) : string option =
  let existing = lookup_branch_id_by_name t ~owner ~name in
  match existing with
  | Some id -> (Some id)
  | None ->
      (match t.conn with
       | None -> None
       | Some conn ->
           (try
      let sql =
            "INSERT INTO branches (id, parent_id, owner, name, base_gen) " ^
            "VALUES (gen_random_uuid(), $1, $2, $3, " ^
            "        COALESCE((SELECT generation FROM branch_generations " ^
            "                  WHERE branch_id = $1), 0)) " ^
            "ON CONFLICT (owner, name) DO UPDATE SET name = EXCLUDED.name " ^
            "RETURNING id::text" in
               let rows =
                 exec conn
                   ~params:[ Pgx.Value.of_string parent_id;
                             Pgx.Value.of_string owner;
                             Pgx.Value.of_string name ]
                   sql
               in
               (match rows with
                | [ [ id_cell ] ] -> (Pgx.Value.to_string id_cell)
                | _ ->
                    (* Race: another image created it; re-lookup. *)
                    lookup_branch_id_by_name t ~owner ~name)
    with exn ->
      Printf.eprintf "[store] ensure_branch error: %s\n"
                 (Printexc.to_string exn);
               lookup_branch_id_by_name t ~owner ~name))

(* Fork a branch: create an EMPTY lens with parent_id as its parent.
   one-tree P5 / branching.personal-workspace: NO copy of code rows.
   Inheritance is the recursive-CTE / path-prefix walk; child pins are
   rows written later on the child. Data stays shared (branching.data).
   Returns the new branch_id. Single SERIALIZABLE insert. *)
(* Persist a route binding: (route "/path" 'handler-fn-name).
   INSERT ... ON CONFLICT (branch_id, route) DO UPDATE — so re-defining
   a route overrides the existing binding. *)
let normalize_visibility (v : string) : string =
  match String.lowercase_ascii (String.trim v) with
  | "public" -> "public"
  | _ -> "private"

(** Persist a route. [visibility_opt]:
    - Some v → set visibility (public|private)
    - None → on INSERT default private; on UPDATE keep existing visibility
      (handler-only re-register does not clobber public). *)
let register_route (t : t) ~(route : string) ~(handler : string)
    ?visibility ()
  : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let vis =
            match visibility with
            | Some v -> normalize_visibility v
            | None -> "private"
          in
          Tree_store.upsert_route conn ~branch_id:t.branch_id ~route ~handler
            ~visibility:vis
    with exn ->
      Printf.eprintf "[store] register_route error: %s\n"
            (Printexc.to_string exn);
          ())

(** branching.seed-policy: insert route only if absent on this branch. *)
let register_route_if_absent (t : t) ~(route : string) ~(handler : string)
    ?visibility ()
  : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (* image.boot-seed-batch: skip the per-route get_by_path SELECT during
         additive boot seed when the route is already in the preloaded set. *)
      (match t.seed_names with
       | Some set when Hashtbl.mem set route -> ()
       | _ ->
      (try
      let vis =
            match visibility with
            | Some v -> normalize_visibility v
            | None -> "private"
          in
          (* seed-policy: insert route only if the tree node is absent *)
            let exists =
              Tree_store.get_by_path conn
                ~path:(Tree_store.route_path ~branch_id:t.branch_id ~route) in
            (match exists with
             | Some _ -> ()
             | None ->
                 Tree_store.upsert_route conn ~branch_id:t.branch_id ~route
                   ~handler ~visibility:vis);
            (match t.seed_names with
             | Some set -> Hashtbl.replace set route ()
             | None -> ())
      with exn ->
      Printf.eprintf "[store] register_route_if_absent error: %s\n"
            (Printexc.to_string exn);
          ()))

(* List all route bindings visible to this branch (recursive-CTE walk,
   same as list_function_names — annotations from ancestors are visible
   via inheritance, child's definition OVERRIDES parent's). Returns
   (route, handler, visibility) ordered with closest ancestor first. *)
let list_routes (t : t) : (string * string * string) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT vb.route, vb.handler, " ^
            "COALESCE(vb.visibility, 'private') " ^
            "FROM " ^ rt_rel () ^ " vb " ^
            "JOIN chain ON vb.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ route_c; handler_c; vis_c ] ->
                     (match Pgx.Value.to_string route_c,
                             Pgx.Value.to_string handler_c with
                      | Some r, Some h ->
                          let vis =
                            match Pgx.Value.to_string vis_c with
                            | Some v -> normalize_visibility v
                            | None -> "private"
                          in
                          Some (r, h, vis)
                      | _ -> None)
                 | [ route_c; handler_c ] ->
                     (* Pre-migration fallback *)
                     (match Pgx.Value.to_string route_c,
                             Pgx.Value.to_string handler_c with
                      | Some r, Some h -> Some (r, h, "private")
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn ->
      Printf.eprintf "[store] list_routes error: %s\n"
            (Printexc.to_string exn);
          [])

(* ---- type_defs ----

   Per spec [storage.types]: user declares a type with (deftype 'name
   field1 field2 ...). The declaration is INSERTed into type_defs with
   branch_id = current branch and fields = JSON array of strings (the
   field names, in declared order).

   The data DOESN'T live in a per-type table: packed in migration 003's
   note that generating one PG table per Lisp type would require
   runtime DDL breaking additive-migrations-only. Instead, all data
   rows are stored in the shared data_rows(type_name, fields jsonb)
   table, and type_defs is pure declaration metadata. (Type
   declarations MAY eventually guide field-level validation; for now
   they're documentation.)

   The recursive-CTE override walk applies to type_defs the same as
   function_defs / view_bindings: ancestors' types are visible, child's
   definition wins on name conflict. *)

let persist_type (t : t) ~(name : string) ~(fields : string list)
    ~(author : string) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let fields_json =
            Yojson.Safe.to_string
              (`List (List.map (fun s -> `String s) fields)) in
          Tree_store.upsert_type conn ~branch_id:t.branch_id ~name
            ~fields_json ~author
    with exn ->
      Printf.eprintf "[store] persist_type error: %s\n"
            (Printexc.to_string exn);
          ())

(* List all type declarations visible to this branch via the recursive
   CTE override walk. Returns (name, fields) pairs with closest
   ancestor first; caller-side Hashtbl first-occurrence dedup wins. *)
let list_types (t : t) : (string * string list) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT td.name, td.fields::text FROM " ^ ty_rel () ^ " td " ^
            "JOIN chain ON td.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ name_cell; fields_cell ] ->
                     (match Pgx.Value.to_string name_cell,
                             Pgx.Value.to_string fields_cell with
                      | Some name, Some fs ->
                          (try
                             let fs' =
                               match Yojson.Safe.from_string fs with
                               | `List lst ->
                                   List.filter_map
                                     (fun j ->
                                       match j with
                                       | `String s -> Some s
                                       | _ -> None)
                                     lst
                               | _ -> []
                             in Some (name, fs')
                           with _ -> None)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn ->
      Printf.eprintf "[store] list_types error: %s\n"
            (Printexc.to_string exn);
          [])

(** Field names for a type visible on the current branch (CTE walk).
    None if the type is not declared. *)
let lookup_type_fields (t : t) ~(name : string) : string list option =
  let types = list_types t in
  match List.find_opt (fun (n, _) -> n = name) types with
  | Some (_, fields) -> Some fields
  | None -> None

let fork_branch (t : t) ~(parent_id : string) ~(owner : string)
    ~(child_name : string) : string option =
  (* one-tree P5: EMPTY LENS fork.
     Create a branch row with parent_id only — NO copy of function_defs /
     type_defs / view_bindings. Inheritance is the recursive-CTE / path-
     prefix walk (child shadows parent). Matches ensure_branch and
     branching.personal-workspace ("empty child of main"). Copy-on-fork
     froze inheritance and contradicted the lens model.
     tree_branches lens registry is maintained by the branches→tree
     deferred trigger (009). head_hash is recomputed (empty → null/empty).
     Single SERIALIZABLE tx; rollback-safe. *)
  (try
      with_transaction t (fun conn ->
        match conn with
        | None -> None
        | Some conn ->
            let insert_branch_sql =
              "INSERT INTO branches (id, parent_id, owner, name, base_gen) " ^
              "VALUES (gen_random_uuid(), $1, $2, $3, " ^
              "        COALESCE((SELECT generation FROM branch_generations " ^
              "                  WHERE branch_id = $1), 0)) " ^
              "RETURNING id::text" in
            let rows =
              exec conn
                ~params:[ Pgx.Value.of_string parent_id;
                          Pgx.Value.of_string owner;
                          Pgx.Value.of_string child_name ]
                insert_branch_sql
            in
            (match rows with
             | [ [ id_cell ] ] ->
                 (match Pgx.Value.to_string id_cell with
                  | Some new_branch_id ->
                      (* Refresh head_hash on the new empty lens (0 own nodes). *)
                      let () =
                        Tree_store.recompute_head_hash conn
                          ~branch_id:new_branch_id
                      in
                      (Some new_branch_id)
                  | None -> None)
             | _ -> None))
    with exn ->
      Printf.eprintf "[store] fork_branch error: %s\n"
        (Printexc.to_string exn);
      None)

(* Merge a source branch's function_defs into a target branch.

   Per spec [branching.merge]:
     (merge 'alice/experiment 'alice/main)
   -> copy all function_defs from source to target (upsert).
   Policy: admin-only merges to main; per-user can merge into their
   own branches. Policy is enforced at the Lisp intercept layer
   (session.ml); Store.merge_branch just does the SQL.

   branching.branch-status S3: status-driven merge. Into the source's
   PARENT: upsert ONLY names the source moved since base_gen (ahead-new /
   ahead / diverged); behind names would clobber the parent's newer work
   and are skipped; synced/clean names are already identical. Conflict
   report = the diverged set (both sides moved since the fork base) —
   genuine conflict detection; resolution stays LWW (report-only).
   Any other target (no shared base): head-vs-head fallback — upsert
   source fns whose body differs from the target's own row, no report.
   TODO: schema-merge for type_defs — only main accepts schema changes
   via special migration-governed merge.

   Wrapped in a SERIALIZABLE transaction via [with_transaction] since
   it16 (previously best-effort atomic — carried-forward since it9). *)
type merge_result = {
  count : int;
  conflicts : string list;
}

(* ---- branch diff (branching.diff) ---- *)

type diff_status = DiffAdded | DiffRemoved | DiffModified

(** One divergent row across two branches' effective views. [from_body]/
    [to_body] are [Reader.pp_expr] strings (for types: the fields JSON
    text; for routes: the handler). [from_expr]/[to_expr] carry the parsed
    AST for Modified fn rows so the /diff renderer can run Ast_diff.diff
    without re-parsing. [from_updated]/[to_updated] are ISO timestamps
    (None when the side is absent).

    Scope = merge scope: function_defs + type_defs + view_bindings;
    NEVER data_rows (shared, branch-free). *) 
type branch_diff_row = {
  diff_name : string;
  diff_kind : diff_kind;
  diff_status : diff_status;
  from_body : string option;
  to_body : string option;
  from_updated : string option;
  to_updated : string option;
  from_expr : Pricklypear_lisp.Ast.expr option;
  to_expr : Pricklypear_lisp.Ast.expr option;
}

(** Parse a serialized-AST jsonb string into [Ast.expr]. None on any
    parse failure (Malformed rows are skipped for AST purposes but still
    reported as modified with their raw [Reader.pp_expr] bodies). *)
let parse_expr_opt (body_str : string) : Pricklypear_lisp.Ast.expr option =
  match Yojson.Safe.from_string body_str with
  | j -> (
      match Pricklypear_lisp.Ast.expr_of_json j with
      | Ok e -> Some e
      | Error _ -> None)
  | exception Yojson.Json_error _ -> None

(** Format merge result for the (merge ...) prim / UI. *)
let format_merge_result (r : merge_result) : string =
  match r.conflicts with
  | [] -> Printf.sprintf "merged %d rows" r.count
  | cs ->
      Printf.sprintf "merged %d rows (%d conflicts: %s)"
        r.count (List.length cs) (String.concat ", " cs)

let merge_branch (t : t) ~(source_id : string) ~(target_id : string)
  : merge_result option =
  (* Is this a merge into the source's direct parent? Read BEFORE the tx
     (the value is stable); the base advance runs AFTER the tx commits,
     because the deferred gen-bump triggers (017/019) fire at COMMIT —
     reading branch_generations inside the tx sees the PRE-merge counter. *)
  let src_parent =
    match t.conn with
    | None -> None
    | Some conn -> (
        match
          exec conn
            ~params:[ Pgx.Value.of_string source_id ]
            "SELECT parent_id::text FROM branches WHERE id = $1"
        with
        | [ [ c ] ] -> Pgx.Value.to_string c
        | _ -> None)
  in
  let is_parent_merge = src_parent = Some target_id in
  let merged =
    try
      with_transaction t (fun conn ->
        match conn with
        | None -> None  (* no DB; can't merge *)
        | Some conn ->
            (* branching.branch-status S3: status-driven merge.
               [is_parent_merge] was resolved before the tx (see above). *)
            let names, conflicts =
              if is_parent_merge then begin
                let rows = branch_status_conn conn ~branch_id:source_id in
                let fns =
                  List.filter (fun r -> r.st_kind = DiffFn) rows in
                let ups =
                  List.filter_map
                    (fun r ->
                      match r.st_status with
                      | SAheadNew | SAhead | SDiverged -> Some r.st_name
                      | _ -> None)
                    fns
                in
                let conf =
                  List.filter_map
                    (fun r ->
                      if r.st_status = SDiverged then Some r.st_name
                      else None)
                    fns
                in
                (ups, conf)
              end
              else begin
                let sql =
                  "SELECT s.name FROM function_defs s " ^
                  "WHERE s.branch_id = $1 AND NOT EXISTS ( " ^
                  "  SELECT 1 FROM function_defs x " ^
                  "  WHERE x.branch_id = $2 AND x.name = s.name " ^
                  "  AND x.body IS NOT DISTINCT FROM s.body) " ^
                  "ORDER BY s.name" in
                let rows =
                  exec conn
                    ~params:[ Pgx.Value.of_string source_id;
                              Pgx.Value.of_string target_id ]
                    sql
                in
                (List.filter_map
                   (function [ c ] -> Pgx.Value.to_string c | _ -> None)
                   rows,
                 [] (* no shared base: no conflict report *))
              end
            in
            (* Upsert only the selected names (per-name loop keeps binding
               simple; merge sets are branch-scale, tens of rows). *)
            let count =
              List.fold_left
                (fun acc name ->
                  let sql =
                    "INSERT INTO function_defs " ^
                    "  (branch_id, name, body, author, doc, package, updated_at) " ^
                    "SELECT $1, name, body, author, doc, package, updated_at " ^
                    "FROM function_defs " ^
                    "WHERE branch_id = $2 AND name = $3 " ^
                    "ON CONFLICT (branch_id, name) DO UPDATE " ^
                    "  SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
                    "      doc = EXCLUDED.doc, package = EXCLUDED.package, " ^
                    "      updated_at = EXCLUDED.updated_at " ^
                    "RETURNING name" in
                  let rows =
                    exec conn
                      ~params:[ Pgx.Value.of_string target_id;
                                Pgx.Value.of_string source_id;
                                Pgx.Value.of_string name ]
                      sql
                  in
                  if rows = [] then acc else acc + 1)
                0 names
            in
            (* Advance the source's merge base AFTER commit (see above):
               post-commit, branch_generations carries the merge's own
               bump, so base_gen lands on the parent's true head — git's
               moving merge-base. Only meaningful for parent-merges;
               promote deliberately does NOT advance. *)
            let () = Tree_store.recompute_head_hash conn ~branch_id:target_id in
            (Some { count; conflicts }))
    with exn ->
      Printf.eprintf "[store] merge_branch error: %s\n"
        (Printexc.to_string exn);
      None
  in
  let () =
    if is_parent_merge then
      match merged with
      | Some _ -> (
          match t.conn with
          | None -> ()
          | Some conn ->
              (try
                 let _ =
                   exec conn
                     ~params:[ Pgx.Value.of_string source_id;
                               Pgx.Value.of_string target_id ]
                     ("UPDATE branches SET base_gen = COALESCE( " ^
                      "  (SELECT generation FROM branch_generations " ^
                      "    WHERE branch_id = $2), 0) WHERE id = $1")
                 in
                 ()
               with exn ->
                 Printf.eprintf "[store] merge base advance: %s\n"
                   (Printexc.to_string exn)))
      | None -> ()
  in
  merged

(** Branch diff (branching.diff). Compares the EFFECTIVE views of two
    branches (closest-ancestor-wins walk per branch, deduped) across
    function_defs + type_defs + view_bindings, and classifies each
    divergent name as added / removed / modified.

    Read-only (NO writes) — matches the read-path style of
    [list_function_names_meta] (plain [exec], no transaction). Scope
    mirrors [merge_branch]: never data_rows. *) 
let diff_branches (t : t) ~(from_id : string) ~(to_id : string)
  : branch_diff_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
          (* chain_from / chain_to: parent walks for each branch.  Each
             side's *effective* rows are deduped (row_number depth-ASC,
             keep first = closest wins), then FULL OUTER JOINed on the
             key so added/removed/modified fall out naturally.  The body
             payload differs per table (fn=body, type=fields, route=
             handler) so the query is parameterized per kind. *)
          let read_kind (kind : diff_kind) : branch_diff_row list =
            let rel, key_sql, body_expr, is_route =
              match kind with
              | DiffFn ->
                  (fn_rel (), "name", "body", false)
              | DiffType ->
                  (ty_rel (), "name", "fields", false)
              | DiffRoute ->
                  (rt_rel (), "route", "handler", true)
            in
            let sql =
              String.concat ""
                [ "WITH RECURSIVE chain_from AS ( "
                ; "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 "
                ; "  UNION ALL "
                ; "  SELECT b.id, b.parent_id, c.depth + 1 "
                ; "  FROM branches b JOIN chain_from c ON b.id = c.parent_id "
                ; "), chain_to AS ( "
                ; "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $2 "
                ; "  UNION ALL "
                ; "  SELECT b.id, b.parent_id, c.depth + 1 "
                ; "  FROM branches b JOIN chain_to c ON b.id = c.parent_id "
                ; "), fromd AS ( "
                ; "  SELECT " ^ key_sql ^ " AS key, " ^ body_expr ^ " AS body, "
                  ^ "updated_at::text AS up, "
                ; "         row_number() OVER (PARTITION BY " ^ key_sql
                  ^ " ORDER BY chain_from.depth ASC) AS rn "
                ; "  FROM " ^ rel ^ " fd JOIN chain_from ON fd.branch_id = chain_from.id "
                ; "), tod AS ( "
                ; "  SELECT " ^ key_sql ^ " AS key, " ^ body_expr ^ " AS body, "
                  ^ "updated_at::text AS up, "
                ; "         row_number() OVER (PARTITION BY " ^ key_sql
                  ^ " ORDER BY chain_to.depth ASC) AS rn "
                ; "  FROM " ^ rel ^ " fd JOIN chain_to ON fd.branch_id = chain_to.id "
                ; ") SELECT f.key, f.body, f.up::text, t.key, t.body, t.up::text "
                ; "FROM (SELECT * FROM fromd WHERE rn = 1) f "
                ; "FULL OUTER JOIN (SELECT * FROM tod WHERE rn = 1) t ON t.key = f.key "
                ; "WHERE f.key IS NULL OR t.key IS NULL "
                ; "      OR f.body IS DISTINCT FROM t.body "
                ; "ORDER BY COALESCE(f.key, t.key)" ]
            in
            let rows =
              exec conn
                ~params:
                  [ Pgx.Value.of_string from_id
                  ; Pgx.Value.of_string to_id ]
                sql
            in
            List.filter_map
              (fun row ->
                match row with
                | [ fk_c; fb_c; fu_c; tk_c; tb_c; tu_c ] ->
                    let from_key = Pgx.Value.to_string fk_c in
                    let to_key = Pgx.Value.to_string tk_c in
                    let name =
                      match from_key, to_key with
                      | Some k, _ | _, Some k -> k
                      | None, None -> ""
                    in
                    let has_from = from_key <> None in
                    let has_to = to_key <> None in
                    if name = "" then None
                    else
                      let status =
                        if has_from && not has_to then DiffRemoved
                        else if has_to && not has_from then DiffAdded
                        else DiffModified
                      in
                      let from_body = if has_from then Pgx.Value.to_string fb_c else None in
                      let to_body = if has_to then Pgx.Value.to_string tb_c else None in
                      let from_updated = if has_from then Pgx.Value.to_string fu_c else None in
                      let to_updated = if has_to then Pgx.Value.to_string tu_c else None in
                      (* Marshal body strings into presentable text + parsed
                         AST (functions/types only; routes have no body AST). *)
                      let pp (b : string option) : string option =
                        Option.map (fun bs ->
                            if is_route then bs
                            else if kind = DiffType then bs
                            else
                              match parse_expr_opt bs with
                              | Some e -> Pricklypear_lisp.Reader.pp_expr e
                              | None -> bs) b
                      in
                      let ex (b : string option) : Pricklypear_lisp.Ast.expr option =
                        if is_route || kind <> DiffFn then None
                        else Option.bind b parse_expr_opt
                      in
                      Some {
                        diff_name = name;
                        diff_kind = kind;
                        diff_status = status;
                        from_body = pp from_body;
                        to_body = pp to_body;
                        from_updated;
                        to_updated;
                        from_expr = ex from_body;
                        to_expr = ex to_body;
                      }
                | _ -> None)
              rows
          in
          List.concat
            [ read_kind DiffFn
            ; read_kind DiffType
            ; read_kind DiffRoute ]
    with exn ->
      Printf.eprintf "[store] diff_branches error: %s\n"
        (Printexc.to_string exn);
      [])

(* ---- package-surface: package_registry (branch-lensed) ---- *)

type package_row = {
  name : string;
  enabled : bool;
  exports : string list;
  author : string;
}

let json_string_list (j : Yojson.Safe.t) : string list =
  match j with
  | `List xs ->
      List.filter_map
        (function `String s -> Some s | _ -> None)
        xs
  | _ -> []

let string_list_to_json (xs : string list) : string =
  Yojson.Safe.to_string
    (`List (List.map (fun s -> `String s) xs))

(** Effective package registry rows for the current branch (closest wins). *)
let list_packages (t : t) : package_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT pr.name, pr.enabled::text, pr.exports::text, pr.author, " ^
            "       chain.depth " ^
            "FROM " ^ pkg_rel () ^ " pr " ^
            "JOIN chain ON pr.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC, pr.name ASC" in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql
          in
          let seen = Hashtbl.create 32 in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; en_c; exp_c; author_c; _depth_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string en_c,
                            Pgx.Value.to_string exp_c,
                            Pgx.Value.to_string author_c with
                     | Some name, Some en_s, Some exp_s, Some author ->
                         if Hashtbl.mem seen name then None
                         else begin
                           Hashtbl.replace seen name ();
                           let enabled =
                             en_s = "t" || en_s = "true" || en_s = "1"
                           in
                           let exports =
                             try json_string_list (Yojson.Safe.from_string exp_s)
                             with _ -> []
                           in
                           Some { name; enabled; exports; author }
                         end
                     | _ -> None)
                | _ -> None)
              rows
          in
          acc
    with exn ->
      Printf.eprintf "[store] list_packages error: %s\n"
            (Printexc.to_string exn);
          [])

(** Set of package names disabled on the current branch (effective). *)
let disabled_package_names (t : t) : string list =
  let rows = list_packages t in
  (List.filter_map
       (fun (r : package_row) -> if r.enabled then None else Some r.name)
       rows)

let lookup_package (t : t) (name : string) : package_row option =
  let rows = list_packages t in
  (List.find_opt (fun (r : package_row) -> r.name = name) rows)

(** Upsert registry row. [enabled_preserve]: if true and a row already exists
    on this branch, keep its enabled bit (header reload must not re-enable).
    Exports always replaced when provided. *)
let upsert_package_registry (t : t) ~(name : string)
    ~(exports : string list) ~(author : string)
    ?(enabled : bool = true) ?(enabled_preserve : bool = true)
    () : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let exp_json = string_list_to_json exports in
          let sql =
            if enabled_preserve then
              "INSERT INTO package_registry " ^
              "  (branch_id, name, enabled, exports, author) " ^
              "VALUES ($1, $2, $3, $4::jsonb, $5) " ^
              "ON CONFLICT (branch_id, name) DO UPDATE " ^
              "SET exports = EXCLUDED.exports, author = EXCLUDED.author, " ^
              "    updated_at = now()"
            else
              "INSERT INTO package_registry " ^
              "  (branch_id, name, enabled, exports, author) " ^
              "VALUES ($1, $2, $3, $4::jsonb, $5) " ^
              "ON CONFLICT (branch_id, name) DO UPDATE " ^
              "SET enabled = EXCLUDED.enabled, exports = EXCLUDED.exports, " ^
              "    author = EXCLUDED.author, updated_at = now()"
          in
          let _ =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_bool enabled;
                        Pgx.Value.of_string exp_json;
                        Pgx.Value.of_string author ]
              sql
          in
          ()
    with exn ->
      Printf.eprintf "[store] upsert_package_registry error: %s\n"
            (Printexc.to_string exn);
          ())

(** Write enabled bit on the *current* branch (creates a shadowing row if
    the package only exists on an ancestor). Copies exports from effective
    row when inserting a new shadow. *)
let set_package_enabled (t : t) ~(name : string) ~(enabled : bool)
    ~(author : string) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let existing = lookup_package t name in
          let exports =
            match existing with
            | Some r -> r.exports
            | None -> []
          in
          let exp_json = string_list_to_json exports in
          let sql =
            "INSERT INTO package_registry " ^
            "  (branch_id, name, enabled, exports, author) " ^
            "VALUES ($1, $2, $3, $4::jsonb, $5) " ^
            "ON CONFLICT (branch_id, name) DO UPDATE " ^
            "SET enabled = EXCLUDED.enabled, author = EXCLUDED.author, " ^
            "    updated_at = now()"
          in
          let _ =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_bool enabled;
                        Pgx.Value.of_string exp_json;
                        Pgx.Value.of_string author ]
              sql
          in
          ()
    with exn ->
      Printf.eprintf "[store] set_package_enabled error: %s\n"
            (Printexc.to_string exn);
          ())

(** Stamp untagged function_defs on the current branch with [package].
    Matches export list names and [package]/ prefix. Used when a
    (package …) header is seen so provenance lands even if later defines
    in the same load abort (e.g. std kernel special-form fallbacks). *)
let stamp_package_bindings (t : t) ~(package : string)
    ~(exports : string list) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let exp_json = string_list_to_json exports in
          let sql =
            "UPDATE function_defs SET package = $2 " ^
            "WHERE branch_id = $1 AND package IS NULL AND ( " ^
            "  name = $2 " ^
            "  OR name LIKE $2 || '/%' " ^
            "  OR name = ANY (SELECT jsonb_array_elements_text($3::jsonb)) " ^
            ")" in
          let _ =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string package;
                        Pgx.Value.of_string exp_json ]
              sql
          in
          ()
    with exn ->
      Printf.eprintf "[store] stamp_package_bindings error: %s\n"
            (Printexc.to_string exn);
          ())

(** Map name -> package provenance for effective function_defs (closest wins). *)
let function_package_map (t : t) : (string, string) Hashtbl.t =
  match t.conn with
  | None -> (Hashtbl.create 1)
  | Some conn ->
      (try
      let sql =
            "WITH RECURSIVE chain AS ( " ^
            "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1 " ^
            "  UNION ALL " ^
            "  SELECT b.id, b.parent_id, c.depth + 1 " ^
            "  FROM branches b JOIN chain c ON b.id = c.parent_id " ^
            ") SELECT fd.name, fd.package, chain.depth " ^
            "FROM " ^ fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "WHERE fd.package IS NOT NULL " ^
            "ORDER BY chain.depth ASC, fd.name ASC" in
          let rows =
            exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql
          in
          let tbl = Hashtbl.create 128 in
          List.iter
            (fun row ->
              match row with
              | [ name_c; pkg_c; _depth_c ] ->
                  (match Pgx.Value.to_string name_c,
                          Pgx.Value.to_string pkg_c with
                   | Some name, Some pkg when not (Hashtbl.mem tbl name) ->
                       Hashtbl.replace tbl name pkg
                   | _ -> ())
              | _ -> ())
            rows;
          tbl
    with exn ->
      Printf.eprintf "[store] function_package_map error: %s\n"
            (Printexc.to_string exn);
          (Hashtbl.create 1))

(* ---- code-history (branching.code-history) ---- *)

type code_history_entry = {
  rel : string;
  op : string;
  author : string option;
  doc : string option;
  package : string option;
  changed_at : string;
  body : string option; (* jsonb text *)
}

let code_history_entry_to_yojson (e : code_history_entry) : Yojson.Safe.t =
  let opt_str = function None -> `Null | Some s -> `String s in
  let body_json =
    match e.body with
    | None -> `Null
    | Some s ->
        (try Yojson.Safe.from_string s with _ -> `String s)
  in
  `Assoc
    [ ("rel", `String e.rel)
    ; ("op", `String e.op)
    ; ("author", opt_str e.author)
    ; ("doc", opt_str e.doc)
    ; ("package", opt_str e.package)
    ; ("changed_at", `String e.changed_at)
    ; ("body", body_json)
    ]

(** 012-table before-image history for [name] on [branch_id]. Kept as the
    pre-shadow fallback source (tree-time.projections Phase 3): the versioned
    tree only captures writes after migration 019, so pre-019 historical
    depth still lives here until the coexistence window closes. *)
let list_code_history_012 (t : t) ~(branch_id : string) ~(name : string)
  : code_history_entry list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT rel, op, author, doc, package, changed_at::text, " ^
           "       body::text " ^
           "FROM code_history " ^
           "WHERE branch_id = $1 AND name = $2 " ^
           "ORDER BY changed_at DESC, id DESC"
         in
         let rows =
           exec conn
             ~params:
               [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
             sql
         in
         List.filter_map
           (fun row ->
             match row with
             | [ rel_c; op_c; author_c; doc_c; pkg_c; at_c; body_c ] ->
                 (match
                    ( Pgx.Value.to_string rel_c
                    , Pgx.Value.to_string op_c
                    , Pgx.Value.to_string at_c )
                  with
                  | Some rel, Some op, Some changed_at ->
                      Some
                        { rel
                        ; op
                        ; author = Pgx.Value.to_string author_c
                        ; doc = Pgx.Value.to_string doc_c
                        ; package = Pgx.Value.to_string pkg_c
                        ; changed_at
                        ; body = Pgx.Value.to_string body_c
                        }
                  | _ -> None)
             | _ -> None)
           rows
       with exn ->
         Printf.eprintf "[store] list_code_history_012: %s\n"
           (Printexc.to_string exn);
         [])

(* ---- code-history over tree_versions (tree-time.projections, Phase 3) ----
   Re-implements branching.code-history / code-diff over the versioned tree
   (tree_versions) instead of the 012 before-image table, KEEPING SIGNATURES
   UNCHANGED.

   SEMANTIC NOTE (recorded in tree-time.coexistence-plan Phase 3): 012 stored
   the OLD before-image per UPDATE/DELETE (never on the first INSERT);
   tree_versions stores the post-write head payload at each gen. A tree-backed
   history read therefore reports ONLY the rows STRICTLY BELOW HEAD — the
   previous states, newest-first — which the projections doc phrases as "read
   the version row at gen-1". The current head row (the live state) is never
   history, so an insert-only fn has no history until it is rewritten or
   deleted, exactly like the 012 table. Delete-then-recreate leaves a
   middle TOMBSTONE row (op=DELETE) below head, which is preserved.

   FALLBACK: when the code path has NO version rows yet (a pre-019 write —
   the 019 shadow only captures writes after installation), fall back to the
   012 table so no pre-shadow historical depth is lost during the coexistence
   window. Post-shadow history is complete in tree_versions. *)

(* The inner definition payload of a code version row. The tree payload for a
   fn/type/route node carries the code under a 'body' key (fn: {body, params};
   type: the fields list; route: the handler JSON) — the same shape 012 stored
   in function_defs.body / type_defs.fields / view_bindings handler. *)
let code_body_of_version (v : Tree_store.version) : string option =
  match v.Tree_store.v_body with
  | `Assoc kv -> (
      match List.assoc_opt "body" kv with
      | Some b -> Some (Yojson.Safe.to_string b)
      | None -> None )
  | _ -> None

let code_doc_of_version (v : Tree_store.version) : string option =
  match v.Tree_store.v_body with
  | `Assoc kv -> (
      match List.assoc_opt "doc" kv with
      | Some (`String s) -> Some s
      | _ -> None )
  | _ -> None

(* Resolve a logical code [name] on [branch_id] to its tree path + rel.
   Tries fn, then type, then route (same priority live_code_body_text uses).
   A candidate path "exists" if it has a version chain (covers the live AND
   the tombstoned/deleted case) OR a live tree node (the pre-019 case that
   falls back to the 012 table). *)
let code_path_for_name (t : t) ~(branch_id : string) ~(name : string)
  : (string * string) option =
  match t.conn with
  | None -> None
  | Some conn ->
      let candidates =
        [ (Tree_store.fn_path ~branch_id ~name, "function_defs")
        ; (Tree_store.type_path ~branch_id ~name, "type_defs")
        ; (Tree_store.route_path ~branch_id ~route:name, "view_bindings")
        ]
      in
      List.find_map
        (fun (p, rel) ->
          let has_ver =
            Tree_store.versioned_history conn ~path:p () <> []
          in
          let live = Tree_store.get_by_path conn ~path:p <> None in
          if has_ver || live then Some (p, rel) else None)
        candidates

(* Tree-backed code history for [name] on [branch_id], newest-first.
   Only rows STRICTLY BELOW HEAD are history — the 012 table captured
   before-images on UPDATE/DELETE only (never on the first INSERT), and the
   projections doc defines a history read as "the version row at gen-1".
   So the current head row (the live state) is EXCLUDED; the newest history
   entry is the state just before the latest change. Empty when the name has
   no code path OR only a first-insert head row (no superseded state yet) —
   caller falls back to the 012 table so pre-shadow depth is never lost. *)
let list_code_history_tree (t : t) ~(branch_id : string) ~(name : string)
  : code_history_entry list =
  match code_path_for_name t ~branch_id ~name with
  | None -> []
  | Some (path, rel) -> (
      match t.conn with
      | None -> []
      | Some conn ->
          let vers = Tree_store.versioned_history conn ~path () in
          (* newest gen = the head (excluded); anything below it is history *)
          let head_gen =
            List.fold_left
              (fun acc v ->
                match v.Tree_store.v_gen with
                | Some g -> Int64.max acc g
                | None -> acc)
              0L vers
          in
          let below_head =
            List.filter
              (fun v ->
                match v.Tree_store.v_gen with
                | Some g -> Int64.compare g head_gen < 0
                | None -> true)
              vers
          in
          List.rev_map
            (fun v ->
              { rel
              ; op =
                  (if v.Tree_store.v_tombstone then "DELETE" else "UPDATE")
              ; author = v.Tree_store.v_author
              ; doc = code_doc_of_version v
              ; package = None
              ; changed_at =
                  (match v.Tree_store.v_created_at with
                   | Some c -> c
                   | None -> "")
              ; body =
                  (if v.Tree_store.v_tombstone then None
                   else code_body_of_version v)
              })
            below_head)

(** Newest-first history rows for [name] on [branch_id]. Tree-backed (Phase 3)
    with an 012 fallback for pre-shadow names. *)
let list_code_history (t : t) ~(branch_id : string) ~(name : string)
  : code_history_entry list =
  let tree_hist = list_code_history_tree t ~branch_id ~name in
  if tree_hist <> [] then tree_hist
  else list_code_history_012 t ~branch_id ~name

let list_code_history_json (t : t) ~(branch_id : string) ~(name : string)
  : string =
  let entries = list_code_history t ~branch_id ~name in
  Yojson.Safe.to_string
    (`List (List.map code_history_entry_to_yojson entries))

(** Live body text for a name on a branch: function body preferred, else
    type fields, else view_bindings handler JSON. *)
let live_code_body_text (t : t) ~(branch_id : string) ~(name : string)
  : string option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
         let try_fn () =
           let sql =
             "SELECT body::text FROM function_defs " ^
             "WHERE branch_id = $1 AND name = $2"
           in
           match
             exec conn
               ~params:
                 [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
               sql
           with
           | [ [ c ] ] -> Pgx.Value.to_string c
           | _ -> None
         in
         let try_ty () =
           let sql =
             "SELECT fields::text FROM type_defs " ^
             "WHERE branch_id = $1 AND name = $2"
           in
           match
             exec conn
               ~params:
                 [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
               sql
           with
           | [ [ c ] ] -> Pgx.Value.to_string c
           | _ -> None
         in
         let try_rt () =
           let sql =
             "SELECT jsonb_build_object('handler', handler, " ^
             "  'visibility', COALESCE(visibility, 'private'))::text " ^
             "FROM view_bindings WHERE branch_id = $1 AND route = $2"
           in
           match
             exec conn
               ~params:
                 [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
               sql
           with
           | [ [ c ] ] -> Pgx.Value.to_string c
           | _ -> None
         in
         (match try_fn () with
          | Some s -> Some s
          | None ->
              (match try_ty () with
               | Some s -> Some s
               | None -> try_rt ()))
       with exn ->
         Printf.eprintf "[store] live_code_body_text: %s\n"
           (Printexc.to_string exn);
         None)

(** Minimal line-oriented text diff: lines only in [a], only in [b], or
    shared. Not a full LCS unified diff — good enough for prim smoke. *)
let simple_text_diff ~(a : string) ~(b : string) : string =
  let lines s =
    if s = "" then []
    else String.split_on_char '\n' s
  in
  let la = lines a in
  let lb = lines b in
  let module S = Set.Make (String) in
  let sa = List.fold_left (fun acc x -> S.add x acc) S.empty la in
  let sb = List.fold_left (fun acc x -> S.add x acc) S.empty lb in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "--- history\n+++ current\n";
  List.iter
    (fun line ->
      if not (S.mem line sb) then begin
        Buffer.add_char buf '-';
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end)
    la;
  List.iter
    (fun line ->
      if not (S.mem line sa) then begin
        Buffer.add_char buf '+';
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end)
    lb;
  Buffer.contents buf

(** Current live body vs the nth-newest history body (1-based). *)
let code_diff (t : t) ~(branch_id : string) ~(name : string) ~(n : int)
  : (string, string) result =
  if n < 1 then Error "code-diff: n must be >= 1"
  else
    let hist = list_code_history t ~branch_id ~name in
    match List.nth_opt hist (n - 1) with
    | None -> Error (Printf.sprintf "code-diff: no history entry #%d" n)
    | Some entry ->
        let old_body = Option.value entry.body ~default:"" in
        let live =
          Option.value
            (live_code_body_text t ~branch_id ~name)
            ~default:""
        in
        Ok (simple_text_diff ~a:old_body ~b:live)

(* The handler + visibility from a route node's inner body. The route tree
   payload body is {route, handler, visibility} (Tree_store.upsert_route);
   code-restore needs the two writable fields to re-write the route. *)
let route_parts_of_body (body_js : string) : (string * string) option =
  match Yojson.Safe.from_string body_js with
  | `Assoc kv -> (
      match List.assoc_opt "handler" kv with
      | Some (`String h) ->
          let vis =
            match List.assoc_opt "visibility" kv with
            | Some (`String v) -> v
            | _ -> "private"
          in
          Some (h, vis)
      | _ -> None )
  | _ -> None

(** (code-restore name [n] [owner/branch]) — restore a code node (fn/type/
    route) at head-n as a NEW write at head+1 (restore-by-write). This is the
    code-path analogue of (undo) (which is datum-path only): gen-read + write
    the same value back through the code write surface (upsert_fn /
    upsert_type / upsert_route), so the reverse mirror re-populates
    function_defs / type_defs / view_bindings and the restore itself is
    versioned + redoable (the restored value becomes the new head; calling it
    again restores the state before that). The below-head semantic matches
    code-history's "read the version row at gen-1": n=1 restores the state
    just before the latest change; n=2 the state before that. Out-of-range n,
    no history, no body, or an unsupported rel are clean error strings (never
    a crash). Consumers: tree-time.consumers — code-restore = gen-read +
    define, the lossless-redefine pattern the 012 design documented by hand,
    now a real prim. [author] is the session user, the fallback only when the
    source version row carries no author. *)
let code_restore (t : t) ~(branch_id : string) ~(name : string)
    ~(author : string) ?(n : int = 1) () : string =
  match code_path_for_name t ~branch_id ~name with
  | None -> Printf.sprintf "code-restore: no such code path: %s" name
  | Some (path, rel) -> (
      match t.conn with
      | None -> "code-restore: no DB"
      | Some conn ->
          let vers = Tree_store.versioned_history conn ~path () in
          if vers = [] then
            Printf.sprintf "code-restore: no version history for %s" name
          else
            (* newest-first; index 0 is the live head (the current state, never
               a restore target), index n is the state n steps back. Row-index,
               NOT gen arithmetic: the branch_generations counter is shared
               across branches and bumped by tombstones, so gens are not
               guaranteed consecutive for a single path — counting version rows
               is the correct "n steps back". *) 
            let newest_first = List.rev vers in
            if n < 1 then Printf.sprintf "code-restore: n must be >= 1"
            else if n >= List.length newest_first then
              Printf.sprintf
                "code-restore: %s has no state %d steps back" name n
            else
              let v = List.nth newest_first n in
              if v.Tree_store.v_tombstone then
                Printf.sprintf
                  "code-restore: %s at %d steps back is a deletion (tombstone); use tree/delete"
                  name n
              else
                let rest_author =
                  match v.Tree_store.v_author with
                  | Some a -> a
                  | None -> author
                in
                (match code_body_of_version v with
                 | None ->
                     Printf.sprintf
                       "code-restore: no body %d steps back for %s" n name
                 | Some body_js -> (
                     match rel with
                     | "function_defs" ->
                         let doc = code_doc_of_version v in
                         Tree_store.upsert_fn conn ~branch_id ~name
                           ~body_json:body_js ~author:rest_author ~doc;
                         Printf.sprintf
                           "restored fn %s (%d steps back)" name n
                     | "type_defs" ->
                         Tree_store.upsert_type conn ~branch_id ~name
                           ~fields_json:body_js ~author:rest_author;
                         Printf.sprintf
                           "restored type %s (%d steps back)" name n
                     | "view_bindings" -> (
                         match route_parts_of_body body_js with
                         | Some (handler, visibility) ->
                             Tree_store.upsert_route conn ~branch_id
                               ~route:name ~handler ~visibility;
                             Printf.sprintf
                               "restored route %s (%d steps back)" name n
                         | None ->
                             Printf.sprintf
                               "code-restore: route body malformed for %s"
                               name )
                     | _ ->
                         Printf.sprintf "code-restore: unsupported rel %s" rel)))

(* ---- tree-time.gc (5.3): retention worker -------------------------------
   Retention is habitat data + the jobs runner, composed on the existing
   gen-window partitions:
     - policy rows (tree_retention_policy, migration 022) declare per-path-
       class retention (code keeps all; data per-type, default keep-everything
       until a policy exists).
     - tree_versions_retain(path_class, keep_windows) (SQL, 022) computes the
       retention floor from the live head gen, ROLLS UP the surviving state of
       fully-retired paths to CHECKPOINT rows at the floor boundary (so as-of
       reads of ancient gens still resolve), then DROP PARTITIONs every lazy
       gen-window entirely below the floor (never the base window). Idempotent
       + conservative; on the real habitat it is gated by a scheduled job on
       the jobs runner (migration 015), not ad-hoc deletion.
   The first long pole is code paths (tiny); retention is a TODO until a
   path class outgrows its welcome — measured via /timing-style storage
   accounting, not vibes. *) 

let retention_set_policy (t : t) ~(path_class : string)
    ~(type_name : string option) ~(keep_windows : int64) : string =
  if path_class <> "code" && path_class <> "data" then
    "retention-policy-set: path_class must be code or data"
  else
    match current_conn t with
    | None -> "retention-policy-set: no DB"
    | Some conn -> (
        try
          let tn =
            match type_name with
            | None -> Pgx.Value.null
            | Some s -> Pgx.Value.of_string s
          in
          ignore
            (exec conn
               ~params:
                 [ Pgx.Value.of_string path_class; tn;
                   Pgx.Value.of_int64 keep_windows ]
               "INSERT INTO tree_retention_policy (path_class, type_name, keep_windows)
                VALUES ($1, $2, $3)
                ON CONFLICT (path_class, type_name)
                  DO UPDATE SET keep_windows = EXCLUDED.keep_windows"
              : Pgx.row list);
          match type_name with
          | None -> Printf.sprintf "retention policy %s: keep %Ld windows" path_class keep_windows
          | Some tnm ->
              Printf.sprintf "retention policy %s/%s: keep %Ld windows" path_class tnm keep_windows
        with ex ->
          Printf.sprintf "retention-policy-set: %s" (Printexc.to_string ex))

let retention_delete_policy (t : t) ~(path_class : string)
    ~(type_name : string option) : string =
  match current_conn t with
  | None -> "retention-delete-policy: no DB"
  | Some conn -> (
      try
        let tn =
          match type_name with
          | None -> Pgx.Value.null
          | Some s -> Pgx.Value.of_string s
        in
        ignore
          (exec conn
             ~params:[ Pgx.Value.of_string path_class; tn ]
             "DELETE FROM tree_retention_policy
              WHERE path_class = $1 AND type_name IS NOT DISTINCT FROM $2"
            : Pgx.row list);
        "retention policy deleted"
      with ex ->
        Printf.sprintf "retention-delete-policy: %s" (Printexc.to_string ex))

let retention_list_policies (t : t)
    : (string * string option * int64 * string) list =
  match current_conn t with
  | None -> []
  | Some conn -> (
      try
        exec conn
          "SELECT path_class, type_name, keep_windows,
                  created_at::text FROM tree_retention_policy
           ORDER BY path_class, type_name NULLS FIRST"
        |> List.map (fun row ->
               let cell i = List.nth row i in
               let pc =
                 match Pgx.Value.to_string (cell 0) with
                 | Some s -> s
                 | None -> ""
               in
               let tn =
                 match Pgx.Value.to_string (cell 1) with
                 | Some s -> Some s
                 | None -> None
               in
               let kw =
                 match Pgx.Value.to_int64 (cell 2) with
                 | Some k -> k
                 | None -> 0L
               in
               let ca =
                 match Pgx.Value.to_string (cell 3) with
                 | Some s -> s
                 | None -> ""
               in
               (pc, tn, kw, ca))
      with _ -> [])

(* Run the retention worker for each path class that has an explicit
   whole-class policy (type_name IS NULL). Whole-class policies are the v1
   gate: retention is a TODO until a path class outgrows its welcome, and
   the global gen-window partitions mean the floor is whole-class regardless
   of any per-type rows (per-type rows are recorded for future finer-grained
   work but do not change the shared floor). keep_everything until a
   whole-class policy exists — the default — is a no-op, never ad-hoc
   deletion. Returns per-class summary rows (dropped, checkpointed). *) 
let retention_run (t : t) ~(path_class : string option) ()
    : (string * int64 * int64) list =
  let all =
    match path_class with
    | Some pc -> [ pc ]
    | None -> [ "code"; "data" ]
  in
  match current_conn t with
  | None -> []
  | Some conn -> (
      try
        List.concat_map
          (fun pc ->
            (* whole-class policy for this path class? *) 
            let pol =
              exec conn
                ~params:[ Pgx.Value.of_string pc ]
                "SELECT keep_windows FROM tree_retention_policy
                 WHERE path_class = $1 AND type_name IS NULL"
            in
            match pol with
            | [] -> [] (* keep everything: no whole-class policy exists *) 
            | [ row ] -> (
                match Pgx.Value.to_int64 (List.nth row 0) with
                | None -> []
                | Some kw ->
                    let rows =
                      exec conn
                        ~params:
                          [ Pgx.Value.of_string pc;
                            Pgx.Value.of_int64 kw ]
                        "SELECT dropped_windows, checkpointed_paths
                         FROM tree_versions_retain($1::text, $2::bigint)"
                    in
                    (match rows with
                     | [ r ] ->
                         let d =
                           match Pgx.Value.to_int64 (List.nth r 0) with
                           | Some x -> x
                           | None -> 0L
                         in
                         let c =
                           match Pgx.Value.to_int64 (List.nth r 1) with
                           | Some x -> x
                           | None -> 0L
                         in
                         [ (pc, d, c) ]
                     | _ -> []))
            | _ -> [])
          all
      with _ -> [])

