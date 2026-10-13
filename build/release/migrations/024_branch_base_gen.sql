-- Pricklypear migration 024: branch base_gen — the merge-base coordinate.
--
-- Per spec [branching.branch-status] (plan pass pinned; tree-time consumer).
-- A branch records the parent's head generation (branch_generations,
-- migration 017/019) at fork time. Combined with the append-only
-- tree_versions history (018-021), "value of path P at base_gen" is a
-- READ, so git-style clean / ahead / behind / diverged / synced status is
-- a derived hash comparison, not a name-presence join.
--
-- base_gen ADVANCES when a merge lands the branch into its parent (git's
-- moving merge-base). promote deliberately does NOT advance it: selective
-- pins stay visible as `synced` (the unbind-candidate class) until
-- unbound or merged.
--
-- Backfill: existing branches predate the column, so the true fork point
-- is unrecoverable. Approximation (documented in branching.branch-status):
-- treat the fork as having happened at the parent's CURRENT generation —
-- inherited paths then read clean/behind correctly, and existing own pins
-- classify against today's parent content. Empty children (the dominant
-- shape) are exact under this backfill; full-fork copies whose parent
-- moved since the real fork read `synced` (they are content-identical
-- pins — which is the actionable truth for the unbind flow anyway).
--
-- Strictly ADDITIVE + idempotent + image-applied (registered in
-- schema_migrations so the runner will not re-apply).

ALTER TABLE branches ADD COLUMN IF NOT EXISTS base_gen bigint NOT NULL DEFAULT 0;

UPDATE branches b
SET base_gen = COALESCE(
  (SELECT bg.generation FROM branch_generations bg WHERE bg.branch_id = b.parent_id),
  0)
WHERE b.parent_id IS NOT NULL;

-- Root branch(es) (system/main): no parent, base_gen stays 0 (unused).

INSERT INTO schema_migrations (version, description)
SELECT 24, 'branch base_gen merge-base coordinate (branching.branch-status / tree-time consumer)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 24);
