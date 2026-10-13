-- Pricklypear migration 021: tree-time head reconciliation tool.
--
-- tree-time.phase-1 (5.1/5.2): the dual-write shadow (019 trigger) writes
-- a tree_versions row per tree_nodes write, including a TOMBSTONE on delete.
-- A delete-then-recreate cycle whose re-create did NOT re-shadow (historical
-- trigger-iteration residue, or a writer that inserted tree_nodes without the
-- append trigger attached) leaves a LIVE tree_nodes row beneath a TOMBSTONE
-- newest version row: the versioned head says ABSENT while the live row says
-- PRESENT. Per tree-time.projections, that is NOT head-equivalent and the
-- Phase-2 read switch must not run until it is.
--
-- This migration ADDS an idempotent reconciliation function (no data is
-- destroyed, nothing is rewritten in place) that heals exactly that class:
-- for every shadowed LIVE tree_nodes row whose newest version row is a
-- TOMBSTONE, it appends a FRESH non-tombstone version row holding the LIVE
-- tree_nodes content (the live row is the authority), allocating the head
-- gen exactly as the 019 trigger does (branch_generations for b.* code paths,
-- tree_generations for branch-free data.* paths, gen-window partition lazily
-- created). The live tree_nodes row is untouched. Idempotent: a healed row no
-- longer matches the cursor, so rerunning it heals zero.
--
-- It ALSO repairs head COUNTERS (
--branch_generations / tree_generations): a branch whose
-- branch_generations.generation is below the max live code-gen of its own
-- live code version rows is a head-coordinate regression (the next write
-- would collide or regress); the counter is raised to that max. This is the
-- same restore-by-repair discipline, and is what makes the Phase-1 verify
-- gate's head-counter check pass after a reconcile.
--
-- Run it, then the Phase-1 verify gate
-- (scripts/verify_tree_version_shadow.sh) must PASS.
--
-- Additive (CREATE OR REPLACE FUNCTION only) + idempotent + image-applied.

CREATE OR REPLACE FUNCTION tree_versions_reconcile_heads()
RETURNS bigint                              -- number of paths healed
LANGUAGE plpgsql
AS $function$
DECLARE
  healed bigint := 0;
  rec record;
  bid  uuid;
  ngen bigint;
BEGIN
  -- (1) Heal tombstone-head-but-live rows: append live content as the new head.
  FOR rec IN
    SELECT tn.path, tn.kind, tn.type_name, tn.payload, tn.hash, tn.author
    FROM tree_nodes tn
    WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
      AND (SELECT (body->>'_tombstone')::text FROM tree_versions v
           WHERE v.path = tn.path ORDER BY v.gen DESC LIMIT 1) = 'true'
  LOOP
    -- Allocate the next head gen per path class, exactly like tree_version_append().
    bid := tree_path_branch(rec.path);
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

    -- Append the LIVE content as a fresh non-tombstone head row (restore-by-
    -- append, the same lossless pattern undo uses). The live row is authority.
    INSERT INTO tree_versions
      (branch_id, path, gen, kind, type_name, body, hash, author, created_at)
    VALUES
      ( bid, rec.path, ngen, rec.kind, rec.type_name,
        rec.payload, rec.hash, rec.author, now() );
    healed := healed + 1;
  END LOOP;

  -- (2) Repair head counters: a branch_generations counter below the max live
  --     code-gen of its own live code version rows, or a tree_generations
  --     counter below the max data-gen, is a head-coordinate regression. Raise
  --     to the max. Idempotent (GREATEST with the current value).
  UPDATE branch_generations bg
  SET generation = GREATEST(bg.generation, sub.max_gen)
  FROM (SELECT v.branch_id, max(v.gen) AS max_gen
        FROM tree_versions v
        WHERE v.branch_id IS NOT NULL
          AND EXISTS (SELECT 1 FROM tree_nodes tn WHERE tn.path = v.path)
        GROUP BY v.branch_id) sub
  WHERE sub.branch_id = bg.branch_id
    AND bg.generation < sub.max_gen;

  UPDATE tree_generations tg
  SET generation = GREATEST(tg.generation, sub.max_gen)
  FROM (SELECT max(v.gen) AS max_gen
        FROM tree_versions v
        WHERE v.branch_id IS NULL
          AND EXISTS (SELECT 1 FROM tree_nodes tn WHERE tn.path = v.path)) sub;

  RETURN healed;
END $function$;

INSERT INTO schema_migrations (version, description)
SELECT 21, 'tree-time head reconciliation: tree_versions_reconcile_heads() heals tombstone-head-but-live rows'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 21);
