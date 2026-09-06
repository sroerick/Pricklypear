(** Pricklypear Image — Store shared SQL substrate (i-1788671083-c split).
    One-tree rel names, the ONE branch-chain recursive CTE, and the
    staleness stamp. Domain families (store_defs/branch/packages) call
    these via [Store_common.*]; the [Store] facade re-exports only what
    external callers use. *)
(* ---- one-tree: reads source from the tree-backed views ----
   tree_function_defs / tree_type_defs / tree_view_bindings / tree_data_rows
   are projections over tree_nodes. The scaffolding tables (function_defs,
   type_defs, view_bindings, data_rows) are derived via the reverse mirror
   and kept warm for residual readers / fork-copy paths. *)
let fn_rel () = "tree_function_defs"
let ty_rel () = "tree_type_defs"
let rt_rel () = "tree_view_bindings"
let pkg_rel () = "tree_package_registry"

(** The branch-chain recursive CTE — the closest-ancestor-wins
    inheritance rule, defined ONCE (was copy-pasted across 12 store.ml
    queries in slightly divergent spellings, i-1788671262-4). Any
    semantic change to the walk (cycle guard, depth cap) now happens
    here. [cte] names the chain (diff_branches walks two); [anchor] is
    the SQL expression the walk starts at — default the $1 param, the
    parent-excluding walk passes a subselect. [body] is everything
    after the chain CTE: the final SELECT/ORDER/LIMIT, or further
    CTEs (WITH RECURSIVE marks the whole WITH list, so a second
    recursive chain may follow inside [body]). *)
let chain_sql ?(cte = "chain") ?(anchor = "$1") body =
  "WITH RECURSIVE " ^ cte ^ " AS ( "
  ^ "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = " ^ anchor ^ " "
  ^ "  UNION ALL "
  ^ "  SELECT b.id, b.parent_id, c.depth + 1 "
  ^ "  FROM branches b JOIN " ^ cte ^ " c ON b.id = c.parent_id "
  ^ ") " ^ body

(** Cheap visibility signature for staleness checks
    (image.per-identity-sessions): a value that changes iff any
    fn/type/route def on the session's branch chain changed, one round
    trip. A parked session compares this against the value from its
    last load; mismatch => rehydrate.

    tmp/eval-fast-path: the legacy query (count+max(updated_at) over the
    three def views) seq-scans tree_nodes with a tree_path_branch call
    per row, x3 — ~21ms on the dev DB and worse on prod
    (i-1788359831), paid on EVERY request. Fast path reads the
    trigger-maintained monotonic def_gen on tree_branches (migration
    028; bumped only for fn/type/route tree writes so data writes never
    invalidate def sessions). Falls back to the legacy query when the
    column is missing (pre-migration deploy order) or no tree_branches
    rows exist. See borge/image.borg STALENESS. *)
let visible_stamp (t : Store_db.t) : string =
  match t.conn with
  | None -> ""
  | Some conn ->
      (try
         let fast_sql =
           chain_sql
             "SELECT max(d.def_gen)::text || '|' || count(*)::text "
             ^ "FROM tree_branches d JOIN chain ON d.branch_id = chain.id" in
         let legacy_sql =
           let agg rel =
             "(SELECT count(*)::text || '|' || COALESCE(max(updated_at)::text, '-') "
             ^ "FROM " ^ rel ^ " d JOIN chain ON d.branch_id = chain.id)" in
           chain_sql
             ("SELECT " ^ agg (fn_rel ()) ^ " || '/' || " ^ agg (ty_rel ())
             ^ " || '/' || " ^ agg (rt_rel ())) in
         let run sql =
           match Store_db.exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql with
           | [ [ c ] ] -> (match Pgx.Value.to_string c with Some s -> s | None -> "")
           | _ -> "" in
         (* fast failure (pre-migration column) must fall through to the
            legacy query, not to the outer catch-all. *)
         let fast = try run fast_sql with _ -> "" in
         (* "<gen>|<nrow>" with a real gen => use it; NULL gen (no
            tree_branches rows) => legacy. *)
         if fast <> "" && not (String.starts_with ~prefix:"|" fast)
         then "g" ^ fast
         else run legacy_sql
       with _ ->
         (* PROBE (i-1788671060-2 keep): a failed stamp must not break
            attach — ensure_fresh treats "" as "stay on whatever is in
            memory" (session.ml), so during a DB blip sessions keep
            serving their loaded env instead of 500-ing every request;
            freshness detection resumes when the DB returns. *)
         "")

