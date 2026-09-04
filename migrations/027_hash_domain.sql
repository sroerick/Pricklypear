-- Pricklypear migration 027: ONE hash domain (one-tree.hash-domain).
--
-- Per the 2026-08-28 make-phase lesson recorded in branching.branch-status:
-- tree_nodes.hash / tree_versions.hash were NOT comparable across write
-- paths — tree-first writes (the old OCaml-side Tree_store.hash_body over
-- COMPACT Yojson text) and the 009 mirror triggers (digest over PG's
-- CANONICAL jsonb::text: sorted keys, ": " / ", " separators) produced
-- DIFFERENT digests for identical bodies. branch_status therefore had to
-- re-digest every body at query time, and sync subtree manifests mixed
-- domains for mirror-written (promoted) rows.
--
-- Since 2026-09 every writer computes the hash DB-SIDE from the body
-- jsonb — encode(digest($body::jsonb::text,'sha256'),'hex') — in
-- Tree_store upserts AND the 009 mirror alike. One canonicalization, one
-- implementation, writer-independent. This backfill converts rows written
-- by the old OCaml path so the stored column is trustworthy:
--
--   1. tree_nodes: rehash every node from its own payload body.
--   2. tree_versions: rehash real rows from their captured body;
--      tombstone rows (body ? '_tombstone') KEEP the reserved
--      __pp_tombstone__ digest — the marker is the body key, not the
--      hash, but there is no reason to disturb it.
--   3. tree_branches.head_hash: stale mixed-domain aggregates — recompute
--      per branch from the now-uniform node hashes (same shape as
--      Tree_store.recompute_head_hash: sha256 over string_agg of node
--      hashes ordered by path, own subtree only, nlevel > 1).
--
-- Strictly ADDITIVE + idempotent + image-applied (registered in
-- schema_migrations so the runner will not re-apply). Re-running is safe:
-- the recomputation is a pure function of body content.

-- 1) tree_nodes: one domain for every node kind (fn/type/route/datum/
--    note/folder/link — all carry a real body).
UPDATE tree_nodes
   SET hash = encode(digest((payload->'body')::text, 'sha256'), 'hex');

-- 2) tree_versions: real rows rehash from their captured envelope body;
--    tombstones keep the reserved digest.
UPDATE tree_versions
   SET hash = encode(digest((body->'body')::text, 'sha256'), 'hex')
 WHERE (body->>'_tombstone') IS NULL;

-- 3) head_hash aggregates: recompute for every branch from the uniform
--    column (mirrors Tree_store.recompute_head_hash exactly).
UPDATE tree_branches tb
   SET head_hash = COALESCE((
         SELECT encode(digest(string_agg(tn.hash, '' ORDER BY tn.path::text), 'sha256'), 'hex')
         FROM tree_nodes tn
        WHERE tn.path <@ tb.root_path AND nlevel(tn.path) > 1
       ), '');

INSERT INTO schema_migrations (version, description)
SELECT 27, 'one-tree hash-domain unification: rehash tree_nodes/tree_versions to digest(jsonb canonical body); recompute head_hash aggregates'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 27);
