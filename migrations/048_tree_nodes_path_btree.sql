-- 048_tree_nodes_path_btree.sql — give tree_nodes a path btree and fresh stats
-- (prod /todos page-latency fix, 2026-11-02).
--
-- SYMPTOM (measured on prod 2026-11-02, one request, now-ms deltas):
--   (tree/get "data.zzz.a") miss, three in a row ... 1138 / 1121 / 995 ms
--   (tree/list <type-prefix> 1 n) 4 rows ......... 1229-1648 ms
--   (tree/list <type-prefix> 1 n) 198 rows ....... 1160 ms
--   (tree/list "data.s" 3 5000 <type>) 4128 scan ..  80 ms  (server-side type filter)
--   (tree/find-by ...) indexed body read .........  88 ms
--   (space-exists?) on data_spaces ...............   1-2 ms
-- Constant ~1s per tree_nodes statement whatever it returns, on a 1-vCPU
-- box with a ~259MB tree_nodes (044 measured the same size). A null path
-- answers in 3ms only because valid_path() rejects it before PG is asked.
--
-- READING: point reads are `WHERE path = $1::ltree` (Tree_store.get_by_path).
-- tree_nodes has ONLY a GiST index on path (009_tree.sql:37); GiST serves
-- containment (<@), not equality, so every point read seq-scans. Same shape
-- as 044's "scan ALL rows to return 1" — the missing piece there was an
-- index the planner could use.
--
-- FIX: btree on path (equality + ordered prefix), then ANALYZE so the
-- planner has current stats to choose it with. Additive and idempotent.
--
-- NOT INCLUDED, deliberately: VACUUM (bloat on tree_nodes is plausible and
-- would help further) is a manual operator step — it cannot run inside a
-- migration transaction, and a failed migration aborts a deploy.
--
-- FOLLOW-UP (admin lane): if reads are still slow after this lands, the next
-- step is EXPLAIN (ANALYZE, BUFFERS) on
--   SELECT id::text, path::text, kind, type_name, payload::text, hash, author,
--          created_at::text, updated_at::text, rank
--     FROM tree_nodes WHERE path = $1::ltree;
-- and on Tree_store.list_children's containment query, to see whether the
-- policy's non-leakproof tree_enc_label() keeps the GiST index off the
-- containment qual (the 044 barrier), plus VACUUM (ANALYZE) tree_nodes.

CREATE INDEX IF NOT EXISTS tree_nodes_path_btree ON tree_nodes (path);

ANALYZE tree_nodes;

INSERT INTO schema_migrations (version, description)
SELECT 48, 'tree_nodes: btree path index + ANALYZE (prod tree read latency)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 48);
