(** Pricklypear Image — Storage layer (branch-aware queries + fork/merge).
    Connection/tx substrate is [Store_db], included below.
    H3: SQL via [exec] on dual conn (lwt|eio from PP_IO). *)
include Store_db

(* ---- one-tree P3/P4: read-from-tree flag (spec one-tree.migration-plan) ----
   Default ON: branch-aware reads source from the tree-backed views
   (tree_function_defs / tree_type_defs / tree_view_bindings) and data
   reads from tree_data_rows. Set PP_TREE_READS=0 to fall back to the
   scaffolding tables. SQL shape + OCaml result mapping unchanged —
   only the relation name differs — so the prim ABI is preserved. *)
let tree_reads () : bool =
  (* Default OFF while tree write path is opt-in. Forward mirror keeps
     tree_nodes updated for inspection; prim reads use scaffolding tables
     until PP_TREE_READS=1 is explicitly enabled after verify_one_tree. *)
  match Sys.getenv_opt "PP_TREE_READS" with
  | Some "1" | Some "true" | Some "yes" | Some "on" -> true
  | _ -> false

let fn_rel () = if tree_reads () then "tree_function_defs" else "function_defs"
let ty_rel () = if tree_reads () then "tree_type_defs" else "type_defs"
let rt_rel () = if tree_reads () then "tree_view_bindings" else "view_bindings"
let pkg_rel () = if tree_reads () then "tree_package_registry" else "package_registry"

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
let list_overrides (t : t) ~(branch_id : string) ~(parent_id : string option)
  : (string * string * string option) list =
  match t.conn, parent_id with
  | None, _ -> []
  | Some _, None ->
      (* Root branch: every own def is "new". *)
      let own = list_own_functions t ~branch_id in
      (List.map (fun (n, _, u) -> (n, u, None)) own)
  | Some conn, Some pid ->
      (try
      let sql =
            "SELECT c.name, c.updated_at::text, p.updated_at::text " ^
            "FROM function_defs c " ^
            "LEFT JOIN function_defs p " ^
            "  ON p.branch_id = $2 AND p.name = c.name " ^
            "WHERE c.branch_id = $1 ORDER BY c.name" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string pid ]
              sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ n; cu; pu ] ->
                     (match Pgx.Value.to_string n, Pgx.Value.to_string cu with
                      | Some name, Some child_u ->
                          let parent_u =
                            match Pgx.Value.to_string pu with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          Some (name, child_u, parent_u)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn ->
      Printf.eprintf "[store] list_overrides: %s\n"
            (Printexc.to_string exn);
          [])

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
      if Tree_store.tree_writes () then
            Tree_store.delete_fn conn ~branch_id ~name
          else begin
          let sql =
            "DELETE FROM function_defs WHERE branch_id = $1 AND name = $2 " ^
            "RETURNING name" in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string name ]
              sql
          in
          (rows <> [])
          end
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
          if Tree_store.tree_writes () then begin
            (* one-tree P4: write the tree; reverse trigger mirrors to table *)
            let () =
              Tree_store.upsert_fn conn ~branch_id:t.branch_id ~name
                ~body_json ~author ~doc
            in
            (* package provenance is scaffolding-only for Phase 1 *)
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
          end else begin
          let sql =
            "INSERT INTO function_defs " ^
            "  (branch_id, name, body, author, doc, package) " ^
            "VALUES ($1, $2, $3::jsonb, $4, $5, $6) " ^
            "ON CONFLICT (branch_id, name) DO UPDATE " ^
            "SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
            "    doc = EXCLUDED.doc, updated_at = now(), " ^
            "    package = COALESCE(EXCLUDED.package, function_defs.package)" in
          let doc_param =
            match doc with
            | Some s -> Pgx.Value.of_string s
            | None -> Pgx.Value.null
          in
          let pkg_param =
            match package with
            | Some s -> Pgx.Value.of_string s
            | None -> Pgx.Value.null
          in
          let _ =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_string body_json;
                        Pgx.Value.of_string author;
                        doc_param;
                        pkg_param ]
              sql in
          ()
          end
    with exn ->
      Printf.eprintf "[store] persist_function error: %s\n"
            (Printexc.to_string exn);
          ())

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
      (try
      let body_json =
            Yojson.Safe.to_string
              (Pricklypear_lisp.Ast.expr_to_json body) in
          if Tree_store.tree_writes () then begin
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
             | Some _ ->
                 (* seed-policy: never overwrite body; still fill provenance *)
                 stamp_pkg_if_null ()
             | None ->
                 let () =
                   Tree_store.upsert_fn conn ~branch_id:t.branch_id ~name
                     ~body_json ~author ~doc
                 in
                 stamp_pkg_if_null ())
          end else begin
          let sql =
            "INSERT INTO function_defs " ^
            "  (branch_id, name, body, author, doc, package) " ^
            "VALUES ($1, $2, $3::jsonb, $4, $5, $6) " ^
            "ON CONFLICT (branch_id, name) DO NOTHING" in
          let doc_param =
            match doc with
            | Some s -> Pgx.Value.of_string s
            | None -> Pgx.Value.null
          in
          let pkg_param =
            match package with
            | Some s -> Pgx.Value.of_string s
            | None -> Pgx.Value.null
          in
          let _ =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_string body_json;
                        Pgx.Value.of_string author;
                        doc_param;
                        pkg_param ]
              sql in
          (* Seed-policy never overwrites body, but still fill package
             provenance when the existing row is untagged. *)
          (match package with
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
               ())
          end
    with exn ->
      Printf.eprintf "[store] persist_function_if_absent error: %s\n"
            (Printexc.to_string exn);
          ())

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
            "       COALESCE(last_synced::text, '') " ^
            "FROM branches ORDER BY owner, name" in
          let rows = exec conn sql in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ id_c; pid_c; owner_c; name_c; created_c; synced_c ] ->
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
                          let base =
                            Pricklypear_common.Branch.make
                              ~id ~parent_id ~owner ~name
                              ~created_at:created ()
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
          let sql =
            "DELETE FROM function_defs WHERE branch_id = $1 AND name = $2 " ^
            "RETURNING name"
          in
          let rows =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name ]
              sql
          in
          (* Tree_store mirror: delete the fn node if tree writes are on *)
          (if Tree_store.tree_writes () then
             ignore (Tree_store.delete_fn conn ~branch_id:t.branch_id ~name));
          rows <> []
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
                 "INSERT INTO branches (id, parent_id, owner, name) " ^
                 "VALUES (gen_random_uuid(), $1, $2, $3) " ^
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
      if Tree_store.tree_writes () then begin
            let vis =
              match visibility with
              | Some v -> normalize_visibility v
              | None -> "private"
            in
            Tree_store.upsert_route conn ~branch_id:t.branch_id ~route ~handler
              ~visibility:vis
          end else begin
          let sql, params =
            match visibility with
            | Some vis ->
                let vis = normalize_visibility vis in
                ( "INSERT INTO view_bindings " ^
                  "  (branch_id, route, handler, visibility) " ^
                  "VALUES ($1, $2, $3, $4) " ^
                  "ON CONFLICT (branch_id, route) DO UPDATE " ^
                  "SET handler = EXCLUDED.handler, " ^
                  "    visibility = EXCLUDED.visibility"
                , [ Pgx.Value.of_string t.branch_id
                  ; Pgx.Value.of_string route
                  ; Pgx.Value.of_string handler
                  ; Pgx.Value.of_string vis ] )
            | None ->
                ( "INSERT INTO view_bindings " ^
                  "  (branch_id, route, handler, visibility) " ^
                  "VALUES ($1, $2, $3, 'private') " ^
                  "ON CONFLICT (branch_id, route) DO UPDATE " ^
                  "SET handler = EXCLUDED.handler"
                , [ Pgx.Value.of_string t.branch_id
                  ; Pgx.Value.of_string route
                  ; Pgx.Value.of_string handler ] )
          in
          let _ = exec conn ~params sql in
          ()
          end
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
      (try
      let vis =
            match visibility with
            | Some v -> normalize_visibility v
            | None -> "private"
          in
          if Tree_store.tree_writes () then begin
            let exists =
              Tree_store.get_by_path conn
                ~path:(Tree_store.route_path ~branch_id:t.branch_id ~route) in
            (match exists with
             | Some _ -> ()
             | None ->
                 Tree_store.upsert_route conn ~branch_id:t.branch_id ~route
                   ~handler ~visibility:vis)
          end else begin
          let sql =
            "INSERT INTO view_bindings " ^
            "  (branch_id, route, handler, visibility) " ^
            "VALUES ($1, $2, $3, $4) " ^
            "ON CONFLICT (branch_id, route) DO NOTHING"
          in
          let _ =
            exec conn
              ~params:
                [ Pgx.Value.of_string t.branch_id
                ; Pgx.Value.of_string route
                ; Pgx.Value.of_string handler
                ; Pgx.Value.of_string vis ]
              sql
          in
          ()
          end
    with exn ->
      Printf.eprintf "[store] register_route_if_absent error: %s\n"
            (Printexc.to_string exn);
          ())

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
          if Tree_store.tree_writes () then begin
            Tree_store.upsert_type conn ~branch_id:t.branch_id ~name
              ~fields_json ~author
          end else begin
          let sql =
            "INSERT INTO type_defs (branch_id, name, fields, author) " ^
            "VALUES ($1, $2, $3::jsonb, $4) " ^
            "ON CONFLICT (branch_id, name) DO UPDATE " ^
            "SET fields = EXCLUDED.fields, author = EXCLUDED.author, " ^
            "    updated_at = now()" in
          let _ =
            exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_string fields_json;
                        Pgx.Value.of_string author ]
              sql
          in
          ()
          end
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
              "INSERT INTO branches (id, parent_id, owner, name) " ^
              "VALUES (gen_random_uuid(), $1, $2, $3) " ^
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

   Initial implementation: copies ALL function_defs from source to
   target via INSERT ... ON CONFLICT (branch_id, name) DO UPDATE.
   TODO: diff-based merge (only fns whose updated_at > parent's
   last_synced_with_child, OR whose name was added in child).
   TODO: detect conflicts (parent ALSO modified same name since fork)
   and per-function conflict resolution.
   TODO: schema-merge for type_defs — only main accepts schema changes
   via special migration-governed merge.

   Wrapped in a SERIALIZABLE transaction via [with_transaction] since
   it16 (previously best-effort atomic — carried-forward since it9). *)
type merge_result = {
  count : int;
  conflicts : string list;
}

(** Format merge result for the (merge ...) prim / UI. *)
let format_merge_result (r : merge_result) : string =
  match r.conflicts with
  | [] -> Printf.sprintf "merged %d rows" r.count
  | cs ->
      Printf.sprintf "merged %d rows (%d conflicts: %s)"
        r.count (List.length cs) (String.concat ", " cs)

let merge_branch (t : t) ~(source_id : string) ~(target_id : string)
  : merge_result option =
  (try
      with_transaction t (fun conn ->
        match conn with
        | None -> None  (* no DB; can't merge *)
        | Some conn ->
            (* CONFLICT REPORT (branching.merge): names both sides own where
               target.updated_at > source.updated_at. Report-only; LWW still. *)
            let conflict_sql =
              "SELECT s.name FROM function_defs s " ^
              "JOIN function_defs t ON t.name = s.name AND t.branch_id = $1 " ^
              "WHERE s.branch_id = $2 AND t.updated_at > s.updated_at " ^
              "ORDER BY s.name"
            in
            let conflict_rows =
              exec conn
                ~params:
                  [ Pgx.Value.of_string target_id
                  ; Pgx.Value.of_string source_id ]
                conflict_sql
            in
            let conflicts =
              List.filter_map
                (function
                  | [ c ] -> Pgx.Value.to_string c
                  | _ -> None)
                conflict_rows
            in
            (* SCOPE: function_defs only — never type_defs / view_bindings. *)
            let sql =
              "INSERT INTO function_defs " ^
              "  (branch_id, name, body, author, doc, package, updated_at) " ^
              "SELECT $1, name, body, author, doc, package, updated_at " ^
              "FROM function_defs WHERE branch_id = $2 " ^
              "ON CONFLICT (branch_id, name) DO UPDATE " ^
              "  SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
              "      doc = EXCLUDED.doc, package = EXCLUDED.package, " ^
              "      updated_at = EXCLUDED.updated_at " ^
              "RETURNING name" in
            let rows =
              exec conn
                ~params:[ Pgx.Value.of_string target_id;
                          Pgx.Value.of_string source_id ]
                sql
            in
            let () = Tree_store.recompute_head_hash conn ~branch_id:target_id in
            (Some { count = List.length rows; conflicts }))
    with exn ->
      Printf.eprintf "[store] merge_branch error: %s\n"
        (Printexc.to_string exn);
      None)

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

(** Newest-first history rows for [name] on [branch_id].
    Searches function_defs / type_defs / view_bindings history under the
    same logical name (fn/type name or route path). *)
let list_code_history (t : t) ~(branch_id : string) ~(name : string)
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
         Printf.eprintf "[store] list_code_history: %s\n"
           (Printexc.to_string exn);
         [])

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
