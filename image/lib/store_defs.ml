(** Pricklypear Image — Store: definition lookups (i-1788671083-c split).
    Effective fn/route/type reads over the branch chain, counts, and the
    incremental-rehydrate fingerprints. Bodies cross this boundary as
    OPAQUE jsonb text (the Lisp-AST codec lives at the interpreter
    boundary: [expr_of_body_json] on Pricklypear_lisp's Ast module). *)
(* Branch-aware lookup. Walks the branch parent chain via a recursive
   CTE so that a definition in [main] is visible from a child branch,
   and a definition in the child OVERRIDES the same name in main
   (per spec storage.no-inheritance: OVERRIDE, not UNION).

   Returns (body, author, doc option). A miss (no such name) is None; a
   FAILED query propagates (Store_db.reraise, i-1788671060-2) — an
   outage must surface as an error, never as "unbound variable". With
   no DB configured this still returns None so the REPL keeps
   working (the no-DB arm, a distinct degraded mode). *)
(* Opaque-body contract (i-1788671083-c): returns the RAW jsonb body text
   as stored; the caller parses via [Ast.expr_of_body_json]. A malformed
   body row now comes back instead of masquerading as a miss — the
   boundary owns the skip decision. *)
let lookup_function (t : Store_db.t) (name : string)
  : (string * string * string option) option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      (* Recursive CTE: start at the session's branch and walk parents.
             Return the FIRST row matching [name] when ordered by depth
             ascending (closest ancestor wins). *)
          let sql =
            Store_common.chain_sql ("SELECT fd.body, fd.author, fd.doc FROM " ^ Store_common.fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "WHERE fd.name = $2 " ^
            "ORDER BY chain.depth ASC LIMIT 1") in
          let rows =
            Store_db.exec conn
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
                        Some (body_str, author, doc)
                     | _ -> None)
                | _ -> None))
    with exn -> Store_db.reraise ~op:"lookup_function" exn)

(* List all function names visible to this branch (via the same
   recursive-CTE override walk). Used at session-make time to load
   persisted definitions into the env. *)
(** Effective function defs for the current branch: parent-chain walk,
    closest definition wins (depth ASC, first occurrence kept).
    [depth] is 0 for rows owned by the current branch, >0 if inherited.
    Callers that only need name/body/author can ignore depth. *)
let list_function_names_meta (t : Store_db.t)
  : (string * string * string * int) list =  (* name, RAW body jsonb, author, depth *)
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT fd.name, fd.body, fd.author, chain.depth, fd.package " ^
            "FROM " ^ Store_common.fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC, fd.name ASC") in
          let rows =
            Store_db.exec conn
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
                           Some (name, body_str, author, depth)
                         end
                     | _ -> None)
                | _ -> None)
              rows
          in
          acc
    with exn -> Store_db.reraise ~op:"list_function_names" exn)

(** Effective defs only (deduped). Same as [list_function_names_meta] without depth. *)
let list_function_names (t : Store_db.t)
  : (string * string * string) list =  (* name, RAW body jsonb, author *)
  let rows = list_function_names_meta t in
  (List.map (fun (n, e, a, _) -> (n, e, a)) rows)

(** Substring search over effective function bodies (JSONB ::text) on the
    current branch inheritance chain. Closest definition wins (depth ASC
    dedup). [needle] is matched with SQL ILIKE '%needle%' (no user-supplied
    wildcards expanded beyond that). Returns names only, ordered by name. *)
let search_function_bodies (t : Store_db.t) ~(needle : string) ~(limit : int)
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
             Store_common.chain_sql (", ranked AS ( " ^
             "  SELECT fd.name, chain.depth, " ^
             "         row_number() OVER (PARTITION BY fd.name ORDER BY chain.depth ASC) AS rn " ^
             "  FROM " ^ Store_common.fn_rel () ^ " fd " ^
             "  JOIN chain ON fd.branch_id = chain.id " ^
             "  WHERE fd.body::text ILIKE '%' || $2 || '%' " ^
             ") SELECT name FROM ranked WHERE rn = 1 ORDER BY name ASC LIMIT $3")
           in
           let rows =
             Store_db.exec conn
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
         with exn -> Store_db.reraise ~op:"search_function_bodies" exn)

(** Full branch-chain defs for env rehydrate: parents first (depth DESC),
    then children. Includes ALL rows (not deduped) so Session.load_defs can
    redefine — last write wins (child override). 4th = package provenance
    (None = untagged / always load). See session.load_defs note. *)
let list_function_names_for_load (t : Store_db.t)
  : (string * string * string * string option
     * string * string) list =  (* name, RAW body jsonb, author, pkg, branch, updated *)
  (* tmp/eval-fast-path: rows also carry (branch_id, updated_at) so
     load_defs can record per-def fingerprints for incremental
     rehydrate. *)
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT fd.name, fd.body, fd.author, chain.depth, fd.package, " ^
            "fd.branch_id::text, fd.updated_at::text " ^
            "FROM " ^ Store_common.fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth DESC, fd.name ASC") in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; body_cell; author_cell; _depth_c; pkg_c; branch_c; updated_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string body_cell,
                            Pgx.Value.to_string author_cell,
                            Pgx.Value.to_string branch_c,
                            Pgx.Value.to_string updated_c with
                     | Some name, Some body_str, Some author, Some branch, Some updated ->
                         let pkg = Pgx.Value.to_string pkg_c in
                         Some (name, body_str, author, pkg, branch, updated)
                     | _ -> None)
                | _ -> None)
              rows
          in
          acc
    with exn -> Store_db.reraise ~op:"list_function_names_for_load" exn)

(** tmp/eval-fast-path: cheap per-def fingerprints for incremental
    rehydrate — (name, branch_id, depth, updated_at, package) for every
    def row on the branch chain, NO bodies. Same ordering contract as
    [list_function_names_for_load] (depth DESC = ancestors first) so the
    caller can resolve the effective (closest-branch) row per name.
    Empty on error / no DB (caller falls back to a full load). *)
let list_def_fingerprints (t : Store_db.t)
  : (string * string * int * string * string option) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT fd.name, fd.branch_id::text, chain.depth, " ^
            "fd.updated_at::text, fd.package " ^
            "FROM " ^ Store_common.fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "ORDER BY chain.depth DESC, fd.name ASC") in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql in
          List.filter_map
            (fun row ->
              match row with
              | [ name_c; branch_c; depth_c; updated_c; pkg_c ] ->
                  (match Pgx.Value.to_string name_c,
                          Pgx.Value.to_string branch_c,
                          Pgx.Value.to_string updated_c with
                   | Some name, Some branch, Some updated ->
                       let depth =
                         match Pgx.Value.to_int depth_c with
                         | Some d -> d
                         | None -> 0 in
                       let pkg = Pgx.Value.to_string pkg_c in
                       Some (name, branch, depth, updated, pkg)
                   | _ -> None)
              | _ -> None)
            rows
    with exn -> Store_db.reraise ~op:"list_def_fingerprints" exn)

(** tmp/eval-fast-path: bodies for SPECIFIC (branch_id, name) pairs —
    the incremental-rehydrate delta fetch. Same row shape as
    [list_function_names_for_load] (name, body, author, package),
    restricted to the given pairs. Empty list on error. *)
let list_function_bodies_for (t : Store_db.t)
    ~pairs:(branch_name_pairs : (string * string) list)
  : (string * string * string * string option) list =  (* name, RAW body jsonb, author, pkg *)
  match t.conn with
  | None -> []
  | _ when branch_name_pairs = [] -> []
  | Some conn ->
      (try
        let placeholders =
          String.concat ", "
            (List.mapi
               (fun i _ -> Printf.sprintf "($%d::text, $%d::text)" (2 * i + 1) (2 * i + 2))
               branch_name_pairs)
        in
        let sql =
          "SELECT fd.name, fd.body, fd.author, fd.package FROM " ^ Store_common.fn_rel ()
          ^ " fd WHERE (fd.branch_id::text, fd.name) IN ("
          ^ placeholders ^ ")" in
        let params =
          List.flatten
            (List.map
               (fun (b, n) ->
                 [ Pgx.Value.of_string b; Pgx.Value.of_string n ])
               branch_name_pairs)
        in
        let rows = Store_db.exec conn ~params sql in
        List.filter_map
          (fun row ->
            match row with
            | [ name_c; body_cell; author_cell; pkg_c ] ->
                (match Pgx.Value.to_string name_c,
                        Pgx.Value.to_string body_cell,
                        Pgx.Value.to_string author_cell with
                 | Some name, Some body_str, Some author ->
                     let pkg = Pgx.Value.to_string pkg_c in
                     Some (name, body_str, author, pkg)
                 | _ -> None)
            | _ -> None)
          rows
      with exn -> Store_db.reraise ~op:"list_function_bodies_for" exn)

(** image.load-library-skip (2026-09-03): OWN-branch defs only (no
    recursive CTE). load-library fingerprints these via body + doc to
    skip re-evaluating unchanged defines; inherited names must stay OFF
    the skip map so persist_function's no-diff-no-pin unpin stays
    reachable for re-defines of parent bodies. Same row shape as
    [list_function_names_for_load] plus the doc column. *)
let list_own_function_defs (t : Store_db.t)
  : (string * string * string * string option
     * string option) list =  (* name, RAW body jsonb, author, pkg, doc *)
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT fd.name, fd.body, fd.author, fd.package, fd.doc " ^
           "FROM " ^ Store_common.fn_rel () ^ " fd " ^
           "WHERE fd.branch_id = $1 ORDER BY fd.name ASC" in
         let rows =
           Store_db.exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql in
         List.filter_map
           (fun row ->
             match row with
             | [ name_c; body_cell; author_cell; pkg_c; doc_c ] ->
                 (match Pgx.Value.to_string name_c,
                         Pgx.Value.to_string body_cell,
                         Pgx.Value.to_string author_cell with
                  | Some name, Some body_str, Some author ->
                      let pkg = Pgx.Value.to_string pkg_c in
                      let doc = Pgx.Value.to_string doc_c in
                      Some (name, body_str, author, pkg, doc)
                  | _ -> None)
             | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"list_own_function_defs" exn)

(* ---- own-branch stats / overrides (no recursive CTE) ---- *)

let count_functions_on_branch (t : Store_db.t) ~(branch_id : string) : int =
  match t.conn with
  | None -> 0
  | Some conn ->
      (try
      let sql =
            "SELECT COUNT(*)::text FROM function_defs WHERE branch_id = $1" in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_string c with
                | Some s -> (int_of_string_opt s |> Option.value ~default:0)
                | None -> 0)
           | _ -> 0)
    with exn -> Store_db.reraise ~op:"count_functions_on_branch" exn)

let count_routes_on_branch (t : Store_db.t) ~(branch_id : string) : int =
  match t.conn with
  | None -> 0
  | Some conn ->
      (try
      let sql =
            "SELECT COUNT(*)::text FROM view_bindings WHERE branch_id = $1" in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          (match rows with
           | [ [ c ] ] ->
               (match Pgx.Value.to_string c with
                | Some s -> (int_of_string_opt s |> Option.value ~default:0)
                | None -> 0)
           | _ -> 0)
    with exn -> Store_db.reraise ~op:"count_routes_on_branch" exn)

(** Parent branch id for [branch_id], if any. *)
let branch_parent_id (t : Store_db.t) ~(branch_id : string) : string option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT COALESCE(parent_id::text, '') FROM branches WHERE id = $1"
          in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
          in
          match rows with
          | [ [ pid ] ] ->
              (match Pgx.Value.to_string pid with
               | Some "" | None -> None
               | Some s -> (Some s))
          | _ -> None
    with exn -> Store_db.reraise ~op:"branch_parent_id" exn)

(** Branch LENS CHAIN for [branch_id]: self-first (closest-wins) walk up
    the parent chain to system/main (inclusive), oldest ancestor last.
    Used by the lens-aware tree read prims (one-tree.tree-read-prims) so a
    session on <user>/personal sees the effective override chain for
    tree_nodes under b.<id>.* — same closest-ancestor-wins rule function_
    defs use. A branch with no reachable parent just resolves to itself. *)
let branch_chain (t : Store_db.t) ~(branch_id : string) : string list =
  let rec walk acc id =
    match branch_parent_id t ~branch_id:id with
    | Some parent when parent <> id ->
        walk (id :: acc) parent
    | _ -> (id :: acc)
  in
  (* walk builds oldest-first; reverse to self-first *)
  List.rev (walk [] branch_id)

(** Own rows only: (name, author, updated_at). *)
let list_own_functions (t : Store_db.t) ~(branch_id : string)
  : (string * string * string) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT name, author, updated_at::text FROM function_defs " ^
            "WHERE branch_id = $1 ORDER BY name" in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
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
    with exn -> Store_db.reraise ~op:"list_own_functions" exn)

let list_own_routes (t : Store_db.t) ~(branch_id : string)
  : (string * string) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT route, handler FROM view_bindings WHERE branch_id = $1 " ^
            "ORDER BY route" in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql
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
    with exn -> Store_db.reraise ~op:"list_own_routes" exn)

(** Child own names left-joined to parent: name, child_updated, parent_updated opt. *)
