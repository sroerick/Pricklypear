-- Pricklypear migration 019: tree-time Phase 1 dual-write shadow.
--
-- tree-time.versioned-leaves (5.1): the write path appends a version row
-- to tree_versions (migration 018) IN ADDITION to every tree_nodes write.
-- The dual-write is implemented as an AFTER trigger on tree_nodes — not as
-- per-function appends inside tree_store.ml — for ONE decisive reason:
-- MIRROR PRESERVATION (one-tree.bl-blast-radius / this chapter's
-- versioned-leaves details doc). A scaffolding-table writer (promote/merge
-- writing function_defs/type_defs/view_bindings/data_rows) reaches the tree
-- through the 009_tree.sql tree_mirror_* triggers; a trigger on tree_nodes
-- shadows EVERY such write source (tree_store.ml direct writes AND mirror
-- writes AND reverse-mirror writes) with a single, uniform mechanism. A
-- tree_store.ml-only dual-write would have to be re-wired through every
-- mirror trigger to keep the scaffolding-writer and tree-writer histories in
-- lockstep; the node trigger gets that for free.
--
-- Semantics (per tree-time.migration-018):
--   - gen source per path class: b.<branch>.* code paths bump
--     branch_generations (017, per-branch); branch-free data.* paths bump
--     the tree-wide tree_generations row (018). The append takes gen+1 and
--     bumps the counter — the counter never moves backwards (verify script
--     check #4).
--   - delete never DELETEs: it appends a TOMBSTONE row. The tombstone body
--     uses the SAME reserved marker tree_versions_tombstone() returns
--     ({"_tombstone":true}) so head helpers and readers agree on one marker.
--   - gen-window partitions (>=1000) are created lazily via
--     tree_versions_prepare(); the base window tree_versions_gen_0 comes
--     from migration 018.
--
-- This migration is STRICTLY ADDITIVE + idempotent + image-applied
-- (registered in schema_migrations so the runner will not re-apply).
-- It is the shadow only; nothing reads tree_versions until Phase 2
-- (reads-as-of-gen, tree-time.reads-as-of-gen) flips the PP_* flag.

-- Lazily create the gen-window partition for [gen] (windows >= 1000 only;
-- the base window gen_0 exists from 018). The retention worker (gc) also
-- uses this to pre-create windows. Skipping wn=0 avoids an overlap with
-- tree_versions_gen_0 (partition-of = tree_versions FOR VALUES FROM
-- MINVALUE TO (1000); a partition FOR VALUES (0,1000) would collide).
CREATE OR REPLACE FUNCTION tree_versions_prepare(gen bigint)
RETURNS void
LANGUAGE plpgsql
AS $function$
DECLARE
  wn bigint := gen / 1000;
  lo bigint := wn * 1000;
  hi bigint := lo + 1000;
BEGIN
  IF wn < 1 THEN RETURN; END IF;
  IF to_regclass(format('tree_versions_p%s', wn)) IS NOT NULL THEN RETURN; END IF;
  BEGIN
    EXECUTE format(
      'CREATE TABLE tree_versions_p%s PARTITION OF tree_versions FOR VALUES FROM (%s) TO (%s)',
      wn, lo, hi);
  EXCEPTION WHEN duplicate_table THEN NULL;  -- lost the race; window exists
  END;
END $function$;

-- The node-level AFTER trigger body: derive the owning branch, bump the
-- right head counter, auto-create the gen-window, and append a version row
-- (TOMBSTONE on delete). Runs per row on INSERT/UPDATE/DELETE of tree_nodes.
CREATE OR REPLACE FUNCTION tree_version_append()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
DECLARE
  bid  uuid;
  ngen bigint;
BEGIN
  -- Owning branch for code paths; NULL for branch-free data.*.
  bid := tree_path_branch(COALESCE(NEW.path, OLD.path));

  -- Allocate the next gen atomically, per path class.
  IF bid IS NOT NULL THEN
    INSERT INTO branch_generations (branch_id, generation)
    VALUES (bid, 1)
    ON CONFLICT (branch_id)
      DO UPDATE SET generation = branch_generations.generation + 1
    RETURNING generation INTO ngen;
  ELSE
    UPDATE tree_generations SET generation = generation + 1
    RETURNING generation INTO ngen;
    IF ngen IS NULL THEN
      INSERT INTO tree_generations (generation) VALUES (1)
      RETURNING generation INTO ngen;
    END IF;
  END IF;

  PERFORM tree_versions_prepare(ngen);

  IF TG_OP = 'DELETE' THEN
    -- TOMBSTONE: reserved marker body (same `_tombstone` key
    -- tree_versions_tombstone() returns), never a DELETE.
    INSERT INTO tree_versions
      (branch_id, path, gen, kind, type_name, body, hash, author, created_at)
    VALUES
      ( bid, OLD.path, ngen, OLD.kind, OLD.type_name,
        jsonb_build_object(
          'v', 1, 'kind', OLD.kind, 'path', OLD.path::text,
          'type', OLD.type_name, 'body', JSON 'null',
          'author', OLD.author, 'doc', JSON 'null',
          'hash', encode(digest('__pp_tombstone__', 'sha256'), 'hex'),
          '_tombstone', true),
        encode(digest('__pp_tombstone__', 'sha256'), 'hex'),
        OLD.author, now() );
    RETURN OLD;
  END IF;

  -- INSERT / UPDATE: record the new head payload at gen. An
  -- INSERT .. ON CONFLICT (path) DO UPDATE fires this once as an UPDATE
  -- (with NEW) — a single version row per logical write, no dupes.
  INSERT INTO tree_versions
    (branch_id, path, gen, kind, type_name, body, hash, author, created_at)
  VALUES
    ( bid, NEW.path, ngen, NEW.kind, NEW.type_name,
      NEW.payload, NEW.hash, NEW.author, now() );
  RETURN NEW;
END $function$;

-- Dual-write shadow: AFTER each tree_nodes write, append its version row.
DROP TRIGGER IF EXISTS tree_version_append_trg ON tree_nodes;
CREATE TRIGGER tree_version_append_trg
  AFTER INSERT OR DELETE OR UPDATE ON tree_nodes
  FOR EACH ROW EXECUTE FUNCTION tree_version_append();

INSERT INTO schema_migrations (version, description)
SELECT 19, 'tree-time Phase 1 dual-write shadow: tree_version_append trigger on tree_nodes'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 19);
