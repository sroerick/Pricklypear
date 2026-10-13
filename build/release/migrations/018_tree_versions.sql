-- Pricklypear migration 018: tree-time versioned-leaf table.
--
-- Per spec [tree-time.migration-018] (campaign 5.0 plan pass, pinned
-- schema contract). This ADDS the append-only history table for the
-- generation axis on the one tree: (branch_id, path, gen) -> node. It is
-- pure Phase 1 additive foundation — nothing reads it yet (the scaffolding
-- mirrors + tree_nodes stay the source of truth through Phase 2's
-- flag-guarded read switch), and the write path only appends version rows
-- once tree_store.ml (5.1) grows its dual-write shadow.
--
-- Path grammar matches tree_nodes (migration 009):
--   fn     b.<branch_id>.fn.<name>
--   type   b.<branch_id>.type.<name>
--   route  b.<branch_id>.route.<route_b64url>
--   datum  data.<type_name>.<id>          (branch-free)
--
-- This file is strictly ADDITIVE + idempotent + image-applied (registered
-- in schema_migrations so the runner will not re-apply).

-- tree_generations: the TREE-WIDE data head counter (branch-free paths).
-- code paths (b.*) use branch_generations (017) as their head instead, so
-- this row serves only branch-free data.* writes. One monotonic bigint;
-- the 017 root-row bump discipline generalized to one row (the
-- local-first.sync merge clock).
CREATE TABLE IF NOT EXISTS tree_generations (
  generation bigint NOT NULL DEFAULT 0
);
INSERT INTO tree_generations (generation)
SELECT 0 WHERE NOT EXISTS (SELECT 1 FROM tree_generations);

-- ---------------------------------------------------------------------------
-- tree_versions — append-only history, one row per (branch_id, path, gen)
-- write. update/delete never rewrite: they APPEND a new gen row. Delete
-- appends a TOMBSTONE row (a row, never a DELETE) so undo-of-delete /
-- audit-by-state-diff are free.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS tree_versions (
  branch_id  uuid         NULL,     -- NULL for branch-free data.* paths
  path       ltree        NOT NULL, -- same encoding as tree_nodes
  gen        bigint       NOT NULL,
  kind       text         NOT NULL, -- fn | type | route | datum
  type_name  text,                  -- datum only
  body       jsonb        NOT NULL, -- node envelope; TOMBSTONE = reserved delete marker
  hash       text         NOT NULL, -- sha256 over canonical body (diff-first prune key)
  author     text         NOT NULL,
  created_at timestamptz  NOT NULL DEFAULT now(),  -- write time; immutable per version row
  -- Append-only uniqueness = one row per (path, gen). `path` ALREADY
  -- encodes the branch (b.<branch hex>.* for code paths, globally-unique
  -- data.<type>.<id> for data), so (path, gen) is unique across every
  -- branch — and it permits branch_id to be NULL on branch-free data.*
  -- rows (a composite PK including branch_id would force branch_id NOT
  -- NULL, which the data plane forbids). branch_id is a nullable
  -- denormalized copy for code-branch reads only.
  PRIMARY KEY (path, gen),
  -- code paths (path under the `b` root label) MUST carry their owning
  -- branch; data.* paths are branch-free by the one-tree /data DECISION.
  -- (ltree: path <@ 'b' is true when path is a descendant of the root `b`.)
  CONSTRAINT tree_versions_code_owns_branch CHECK (
    NOT (path <@ 'b'::ltree) OR branch_id IS NOT NULL
  )
) PARTITION BY RANGE (gen);

-- Partitioning note: the spec's migration-018 pin says "RANGE PARTITIONED
-- BY list(gen/1000)". PostgreSQL forbids a PRIMARY KEY (or any unique
-- index) on a partitioned table whose partition key is an EXPRESSION, so we
-- partition by RANGE (gen) directly with 1000-gen windows instead — identical
-- retention semantics (DROP PARTITION per gen window) and it permits the
-- (branch_id, path, gen) primary key. The 1000-gen window is the retention
-- granularity; the base window covers gens below 1000, and the retention
-- worker (tree-time gc) creates further windows lazily.
CREATE TABLE IF NOT EXISTS tree_versions_gen_0 PARTITION OF tree_versions
  FOR VALUES FROM (MINVALUE) TO (1000);

-- Covering index for one-pass head/projection reads: an index range scan
-- returns the newest gen per path with no join. Note: the pk index is
-- (branch_id, path, gen) — this dedicated b-tree adds (branch_id, path,
-- gen DESC) so a DESC scan hits the newest row first.
CREATE INDEX IF NOT EXISTS tree_versions_head_idx
  ON tree_versions (branch_id, path, gen DESC);

-- ---------------------------------------------------------------------------
-- Head + tombstone helpers. A path is ALIVE at head when its newest row at
-- or below head is NOT the TOMBSTONE marker. These are the SQL-level reads
-- Phase 2 wiring (tree_store/crud) will drive through the gen filter.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_versions_tombstone() RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE marker text := '{"_tombstone":true}';
BEGIN
  RETURN marker::jsonb;
END $$;

-- The newest gen for (branch, path) at or below `at_gen` (inclusive).
-- NULL row = path never written; returns the tombstone marker body if the
-- newest resolved row is a delete.
CREATE OR REPLACE FUNCTION tree_versions_head(
  p_branch uuid, p_path ltree, at_gen bigint DEFAULT NULL
) RETURNS TABLE(gen bigint, body jsonb, hash text, author text, created_at timestamptz)
LANGUAGE sql STABLE AS $$
  SELECT v.gen, v.body, v.hash, v.author, v.created_at
  FROM tree_versions v
  WHERE v.branch_id IS NOT DISTINCT FROM p_branch
    AND v.path = p_path
    AND (at_gen IS NULL OR v.gen <= at_gen)
  ORDER BY v.gen DESC
  LIMIT 1
$$;

-- All leaves under p_path alive at head (or at at_gen), newest row per
-- path first (projection reads). `gen <= at_gen` when given; otherwise head.
CREATE OR REPLACE FUNCTION tree_versions_leaves(
  p_branch uuid, p_path ltree, at_gen bigint DEFAULT NULL
) RETURNS TABLE(path ltree, gen bigint, body jsonb, hash text, author text, created_at timestamptz)
LANGUAGE sql STABLE AS $$
  SELECT DISTINCT ON (v.path) v.path, v.gen, v.body, v.hash, v.author, v.created_at
  FROM tree_versions v
  WHERE v.branch_id IS NOT DISTINCT FROM p_branch
    AND v.path <@ p_path
    AND (at_gen IS NULL OR v.gen <= at_gen)
  ORDER BY v.path, v.gen DESC
$$;

INSERT INTO schema_migrations (version, description)
SELECT 18, 'tree_versions append-only versioned-leaf table + tree_generations head counter (tree-time)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 18);
