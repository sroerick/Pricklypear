# 1104 — keep the RLS floor, make the hot tree_nodes reads index-friendly

Decision (roerick, 2026-11-04): option B. RLS stays the enforcement floor.
The hot read shapes reach the indexes through SECURITY DEFINER wrappers;
enforcement does not move into the app.

## Why every tree_nodes read seq-scans

`tree_nodes` (259MB) carries two permissive policies, `datum_user_isolation`
(029) and `note_user_isolation` (041). Their quals are added to every read as
security-barrier quals, and the ltree operators (`=`, `<@`) are not
LEAKPROOF, so a non-leakproof qual in the caller's query cannot be used as an
index qual on a relation carrying security quals. Measured on prod: point read
1851ms / 14323 buffers, containment 1495ms, `VACUUM (ANALYZE)` no change,
`enable_seqscan=off` finds no index candidate — the indexes exist (btree 048,
GiST 009) and the planner will not use them.

Inside a function that runs with RLS bypassed there are no security quals, so
the same predicates are ordinary index quals again.

## What ships

`migrations/049_node_visible.sql`:

* `pp_node_visible(p_path ltree, p_kind text DEFAULT 'datum')` — one predicate
  carrying the exact policy semantics. Both policies are rewritten to
  `USING (pp_node_visible(path, kind))` / same `WITH CHECK`. Policy and
  wrappers call one function, so they cannot drift.
  The note policy reduces to `kind IN ('note','folder','heading') OR
  pp.user=''`, already covered by the datum policy's `kind <> 'datum'`; a
  permissive policy set ORs, so the effective floor is that one expression.
* Three wrappers, each `SECURITY DEFINER`, `SET search_path = pg_catalog,
  public`, applying `pp_node_visible` to every returned row and capping in
  SQL after the check:

| wrapper | shape | index |
|---|---|---|
| `pp_node_get(p_path ltree)` | path equality | btree `tree_nodes_path_btree` |
| `pp_node_list(p_prefix, p_kind, p_cap, p_depth)` | containment `<@`, plus the direct-children shape (`p_depth = 1`) that `tree/list` uses with depth 1 | GiST `tree_nodes_path_gist` |
| `pp_node_find_by(p_prefix, p_field, p_value, p_kind, p_viewer, p_garden, p_cap)` | body field = value under a prefix | 010 partial expression indexes |

Store call sites go through them behind `PP_RLS_WRAPPERS=0`, which restores
the pre-1104 SQL on the same build:

* `Tree_store.get_by_path` (tree/get) - routed.
* `Tree_store.list_subtree` / `cached_list_subtree` (tree/list, tree/subtree)
  - routed.
* `Tree_store.find_by_body_field` (tree/find-by) - routed through
  `pp_node_find_by`, carrying the caller's viewer/garden vis floor.
* `Tree_store.list_children` (tree/list depth 1, tree/children-of) - routed
  through `pp_node_list(..., p_depth = 1)`: same level predicate (including
  the +2 'h' outline roots), sibling order and cap as the pre-1104 SQL.

## Expected EXPLAIN before / after

Before (today, every shape):

```
Seq Scan on tree_nodes  (cost=0.00..14323.00 rows=1 width=...)
  Filter: ((kind <> 'datum'::text) OR ... pp.user ... OR (path <@ ...) OR ...)
Buffers: shared hit=14319 read=4   ->  ~1500-1850 ms
```

After, inside the wrapper (RLS bypassed, so no security quals):

```
-- pp_node_get
Index Scan using tree_nodes_path_btree on tree_nodes n
  Index Cond: (path = p_path)
  Filter: pp_node_visible(path, kind)

-- pp_node_list (containment)
Index Scan using tree_nodes_path_gist on tree_nodes n
  Index Cond: (path <@ p_prefix)
  Filter: ((p_depth IS DISTINCT FROM 1) OR (children predicate))
          AND pp_node_visible(path, kind)
  ->  Sort (rank COLLATE "C" NULLS LAST, path)

-- pp_node_find_by
Index Scan using tree_nodes_body_*_idx on tree_nodes n
  Index Cond: ((payload #>> '{body,<field>}') = p_value)
  Filter: scope AND vis floor AND pp_node_visible(path, kind)
```

## What still scans

* Anything outside the wrappers: ad-hoc SQL, `tree_data_rows`, the lens
  prims, `list_data_lenses`, `tree_versions` reads, the mirror triggers.
  They keep the floor and keep the seq scan.
* Unranked sorts: the wrapper sorts the index rows (`rank COLLATE "C" NULLS
  LAST, path`), so a wide prefix still pays a sort over the matched set.

## Ownership is part of the fix

`013` sets `FORCE ROW LEVEL SECURITY`, so the table owner is subject to the
policies too. A SECURITY DEFINER function owned by the ordinary app role is
still filtered: correct, still a seq scan. The wrappers must be owned by a
role that bypasses RLS (`pp_rls_definer`, NOLOGIN BYPASSRLS).
`tests/sql/rls_definer_role.sql` is the superuser step that creates it, grants
it SELECT on `tree_nodes`, and hands the wrappers over. Without it the
migration warns and the wrappers stay correct but slow.

State check on any database:

```
psql -t -A -c 'SELECT pp_rls_wrappers_mode()'
-- force_rls=t wrapper_owner=pp_rls_definer bypassrls=t can_read=t index_seek=t
```

## Equivalence test (the floor, ships with the branch)

`tests/sql/rls_equivalence.sql` asserts, for 3 users x the fixed path set (own
garden, other garden, `data.shared`, a closed member space, an encrypted note
garden, a lens, a code path), that the wrapper output equals the same query
with RLS enforced as that user (run as `pp_rls_probe`, no BYPASSRLS). Shapes:
path equality, containment list, depth-1 children, find-by.

One command, from the repo root, against a throwaway database:

```
ksh tests/sql/run-rls-equivalence.sh
```

It creates `pp_rls_equiv_<pid>`, applies every migration in order, provisions
the two roles, runs the assertion, prints `pp_rls_wrappers_mode()`, and drops
the database (`KEEP=1` keeps it). Connection comes from `PGHOST`/`PGPORT`/
`PGUSER`; it needs a role that can create databases and roles (postgres).

Against an existing database:

```
psql -v ON_ERROR_STOP=1 -f tests/sql/rls_definer_role.sql
psql -v ON_ERROR_STOP=1 -f tests/sql/rls_equivalence.sql
```

## The prim-shape rule the wrappers depend on

1. A new `tree_nodes` query shape gets a wrapper, or it stays on the RLS path
   and stays slow. That is a performance cliff, not a correctness one: the
   slow path is still the floor, so it fails safe.
2. Nopales libs never see SQL and never see visibility. New data needs arrive
   as a new prim, not as a query escape hatch.
3. Make the plural the primitive and the singular the sugar: `(rows-for-items
   ids)` is the real prim and `(rows-for-item id)` is it with one id; same for
   counts, comments, deps. The one-statement shape should be the shape a lib
   author reaches for by default, so performance does not depend on the author
   knowing a rule.
4. Batch siblings for the per-item prims are follow-on work, not part of this
   branch.

## Measuring (same build, one env var)

```
PP_RLS_WRAPPERS=0   # pre-1104 SQL: direct tree_nodes reads under RLS
PP_RLS_WRAPPERS=1   # wrappers (default)
```

Flip it, restart the image, hit /todos, compare X-Timing-Ms db vs total.

## Not done in this branch

* Nothing routing-side: every store shape that reads tree_nodes for a page is
  on a wrapper (get, subtree list, children depth 1, find-by).
* No EXPLAIN was captured: this box has no Postgres. The plans above are the
  expected shapes, to be confirmed by the first run on mainframe.

## Prim shape rule (roerick, 11-04)

* A new tree_nodes query shape gets a wrapper, or it stays on the RLS path and
  stays slow. That is a performance cliff, not a correctness one: it fails safe.
* Nopales libs never see SQL and never see visibility. A new data need arrives
  as a new prim, not as a query escape hatch.
* Make the plural the primitive and the singular the sugar: `(rows-for-items ids)`
  is the real prim and `(rows-for-item id)` is it with one id; same for counts,
  comments and deps. The one-statement shape should be the one a lib author
  reaches for by default, so performance does not depend on the author knowing
  a rule.
* Batch siblings for the per-item prims are follow-on work, not part of 1104.
