-- 044_tree_def_name_index.sql — make the one-tree def views index-driven under
-- FORCED RLS (the prod page-latency fix, 2026-10-18).
--
-- SYMPTOM (measured on prod 2026-10-18): every launched HTTP request pays a
-- ~2s attach tax; /welcome (0 DB hits in its handler) took 2-8s wall while the
-- same handler renders in 4-7ms on a laptop. A single Postgres backend sat at
-- 70-85% CPU on the 1-vCPU box, and the image log showed ~2.5 statements/sec
-- over 20ms, dominated by two shapes:
--
--   SELECT id::text, path::text, ... FROM tree_nodes WHERE path = $1::ltree
--   WITH RECURSIVE chain AS (...) SELECT fd.body, fd.author, fd.doc
--     FROM tree_function_defs fd JOIN chain ... WHERE fd.name = $2
--
-- ROOT CAUSE: the def views derive `name` in the SELECT list --
--
--   tree_dec_label((string_to_array(path::text,'.'))[4]) AS name
--
-- tree_nodes has relforcerowsecurity = true and a non-LEAKPROOF expression
-- (`tree_dec_label`, IMMUTABLE but NOT leakproof) in the predicate, so the
-- planner may not push the derived-name equality below the RLS barrier. It
-- therefore drives from tree_nodes_kind_type_idx on `kind='fn'` and filters
-- after the policy — scanning ALL 4293 fn rows (259MB table) to return 1.
-- Measured as the app role on prod:
--
--   view lookup (tree_dec_label in the predicate)   48.7 ms  4292 rows removed
--   same shape against function_defs (btree index)   1.1 ms
--
-- An expression index on tree_dec_label(...) does NOT help: it is equally
-- unusable across the RLS barrier (verified: still a Bitmap Index Scan on
-- kind with "Rows Removed by Filter: 4292"). Same for the composite
-- (tree_path_branch, tree_dec_label) form.
--
-- FIX: materialize the label as a STORED GENERATED column. A plain column
-- reference carries no leak-proofing question, so the planner uses the index
-- normally under forced RLS. The views select the column instead of
-- recomputing it. Verified on dev as a forced-RLS role: 48.7ms -> 3.8ms
-- (point lookup 0.45ms).
--
-- The column is a pure function of (path, kind) — the same inputs the views
-- already used — so this is a storage/form change, not a semantic one. It is
-- NULL for non-def kinds (notes/datum/folder/heading), and the partial index
-- is scoped to the three def kinds. Both mirrors keep writing through
-- tree_from_nodes()/tree_from_defs() unchanged: generated columns are
-- maintained by Postgres and MUST NOT appear in an INSERT column list.
--
-- Additive-only per repo convention. Backfill is implicit in ADD COLUMN
-- GENERATED ... STORED (Postgres rewrites the table once at DDL time).

-- 1. The stored label. Generated, so it can never drift from path/kind.
ALTER TABLE tree_nodes
  ADD COLUMN IF NOT EXISTS def_name text
  GENERATED ALWAYS AS (
    CASE
      WHEN kind IN ('fn', 'type', 'route')
      THEN tree_dec_label((string_to_array(path::text, '.'))[4])
    END
  ) STORED;

-- 2. The index. Leading def_name matches the equality predicate the views
--    and every lookup_function* / list_* query carry; kind keeps the partial
--    predicate usable as an index condition too.
CREATE INDEX IF NOT EXISTS tree_nodes_def_name_idx
  ON tree_nodes (def_name, kind)
  WHERE kind IN ('fn', 'type', 'route');

-- 3. Rebuild the three def views to select the stored column.
--    DROP is required for tree_function_defs: CREATE OR REPLACE cannot change
--    a view's column list reordering (and the 011 definition added `package`).
DROP VIEW IF EXISTS tree_function_defs;
CREATE VIEW tree_function_defs AS
SELECT tree_path_branch(n.path) AS branch_id,
       n.def_name AS name,
       n.payload->'body' AS body,
       n.payload->>'author' AS author,
       n.payload->>'doc' AS doc,
       fd.package AS package,
       n.created_at, n.updated_at
FROM tree_nodes n
LEFT JOIN function_defs fd
  ON fd.branch_id = tree_path_branch(n.path)
 AND fd.name = n.def_name
WHERE n.kind = 'fn';

DROP VIEW IF EXISTS tree_type_defs;
CREATE VIEW tree_type_defs AS
SELECT tree_path_branch(n.path) AS branch_id,
       n.def_name AS name,
       n.payload->'body' AS fields,
       n.payload->>'author' AS author,
       n.created_at, n.updated_at
FROM tree_nodes n
WHERE n.kind = 'type';

DROP VIEW IF EXISTS tree_view_bindings;
CREATE VIEW tree_view_bindings AS
SELECT tree_path_branch(n.path) AS branch_id,
       n.payload->'body'->>'route' AS route,
       n.payload->'body'->>'handler' AS handler,
       COALESCE(n.payload->'body'->>'visibility', 'private') AS visibility,
       n.created_at, n.updated_at
FROM tree_nodes n
WHERE n.kind = 'route';

-- 4. Keep statistics fresh so the planner trusts the new index immediately.
ANALYZE tree_nodes;

INSERT INTO schema_migrations (version, description)
SELECT 44, 'tree_nodes.def_name stored generated column + index; def views index-driven under forced RLS'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 44);
