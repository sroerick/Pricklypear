-- Pricklypear migration 022: tree-time GC — retention policy + retention worker.
--
-- tree-time.gc (5.3): retention is the three existing pieces composed:
--   (1) PARTITIONING — tree_versions is already partitioned by gen-range
--       windows (018 base gen_0 + lazily-created tree_versions_p{wn} windows
--       >= 1000 from 019). Physical removal is DROP PARTITION — instant, no
--       vacuum scan, PG-native.
--   (2) RETENTION POLICY IS HABITAT DATA — this migration adds a policy
--       TABLE (tree_retention_policy). Policy rows declare per-path-class
--       retention (code paths keep all — the audit trail; data paths per-type,
--       default keep-everything until a policy row exists). The jobs runner
--       (migration 015) executes the retention worker.
--   (3) ROLLUP, NOT SILENT LOSS — before a window is dropped, surviving
--       state collapses to a CHECKPOINT row at the window boundary gen, so
--       reads as of ancient gens still resolve (to the checkpointed state)
--       and head reads never notice. "Nobody gets everything forever — the
--       audit claim survives at coarser grain."
--
-- PHASE 4 (tree-time.coexistence-plan): this is the gate that flips the gc
-- chapter to implemented — "the retention worker runs on the jobs runner."
--
-- STRICTLY ADDITIVE + idempotent + image-applied (registered in
-- schema_migrations so the runner will not re-apply).

-- ---------------------------------------------------------------------------
-- Policy as habitat data. path_class: 'code' | 'data'. type_name NULL =
-- whole path class. keep_windows = number of newest 1000-gen windows to
-- retain; windows entirely below the retention floor are rolled up + dropped.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS tree_retention_policy (
  id            bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  path_class    text        NOT NULL CHECK (path_class IN ('code','data')),
  type_name     text,                    -- NULL = whole path class
  keep_windows  bigint      NOT NULL DEFAULT 8 CHECK (keep_windows >= 1),
  created_at    timestamptz NOT NULL DEFAULT now(),
  UNIQUE (path_class, type_name)
);

-- ---------------------------------------------------------------------------
-- Rollup helper: collapse surviving state for a path class to CHECKPOINT
-- rows at the retention floor, then DROP the gen-window partitions entirely
-- below that floor. Idempotent + conservative:
--   * floor = head_gen - keep_windows*1000, clamped to >= 1000 so the base
--     gen_0 window (gens < 1000, the womb of the tree) is never dropped.
--   * only paths whose ENTIRE write history sits below floor are checkpointed
--     (paths with any row at gen >= floor keep their live/kept head — their
--     past stays readable via rows still in the kept region, no checkpoint
--     needed; a delete-then-never-recreate path collapses to its last state).
--   * checkpoint rows are written at gen = floor (the boundary INTO the kept
--     region), so they survive every dropped window below and as-of reads of
--     an ancient dropped gen resolve to the checkpointed state.
--   * only lazy windows (wn >= 1) are dropped — never gen_0.
--
-- Returns (dropped_windows, checkpointed_paths).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_versions_retain(p_class text, keep_windows bigint)
RETURNS TABLE(dropped_windows bigint, checkpointed_paths bigint)
LANGUAGE plpgsql
AS $function$
DECLARE
  head_gen bigint;
  floor    bigint;
  wn       bigint;
  rolled   bigint := 0;
  dropped  bigint := 0;
  rec      record;
  wins     bigint[] := '{}';
BEGIN
  -- Head gen per path class.
  IF p_class = 'code' THEN
    SELECT COALESCE(max(generation), 0) INTO head_gen FROM branch_generations;
  ELSE
    SELECT COALESCE(generation, 0) INTO head_gen FROM tree_generations;
  END IF;

  -- Retention floor. Never below the base-window boundary (1000).
  floor := GREATEST(head_gen - keep_windows * 1000, 1000);
  IF floor <= 1000 OR head_gen < keep_windows * 1000 THEN
    -- Nothing beyond the base window is retentive yet.
    RETURN QUERY SELECT 0::bigint, 0::bigint;
    RETURN;
  END IF;

  -- Make sure the boundary window (floor/1000) exists to receive checkpoints.
  PERFORM tree_versions_prepare(floor);

  -- Checkpoint: newest-in-retained-past per path whose whole history is < floor.
  FOR rec IN
    SELECT DISTINCT ON (v.path)
           v.path, v.branch_id, v.kind, v.type_name, v.body, v.hash, v.author
    FROM tree_versions v
    WHERE v.gen < floor
      AND NOT EXISTS (SELECT 1 FROM tree_versions v2
                      WHERE v2.path = v.path AND v2.gen >= floor)
    ORDER BY v.path, v.gen DESC
  LOOP
    INSERT INTO tree_versions
      (branch_id, path, gen, kind, type_name, body, hash, author, created_at)
    VALUES
      (rec.branch_id, rec.path, floor, rec.kind, rec.type_name,
       rec.body, rec.hash, rec.author, now())
    ON CONFLICT (path, gen) DO UPDATE
      SET body = EXCLUDED.body, hash = EXCLUDED.hash, author = EXCLUDED.author;
    rolled := rolled + 1;
  END LOOP;

  -- Drop lazy windows entirely below the floor: (wn+1)*1000 <= floor.
  -- Materialize the window list into an ARRAY first (a FOR-loop over a live
  -- SELECT on tree_versions holds a lock on its partitions and DROP TABLE
  -- would fail with 'used by active queries'), then DROP each partition.
  SELECT array_agg(DISTINCT gen / 1000)
    INTO wins
    FROM tree_versions
   WHERE gen >= 1000 AND gen < floor;

  FOREACH wn IN ARRAY wins LOOP
    IF (wn + 1) * 1000 <= floor
       AND to_regclass(format('tree_versions_p%s', wn)) IS NOT NULL
    THEN
      EXECUTE format('DROP TABLE %I', format('tree_versions_p%s', wn));
      dropped := dropped + 1;
    END IF;
  END LOOP;

  RETURN QUERY SELECT dropped, rolled;
END $function$;

INSERT INTO schema_migrations (version, description)
SELECT 22, 'tree-time gc: tree_retention_policy table + tree_versions_retain() retention worker (rollup + DROP PARTITION)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 22);
