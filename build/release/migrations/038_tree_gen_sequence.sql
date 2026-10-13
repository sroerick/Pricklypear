-- Pricklypear migration 038: de-contend the data.* head counter.
--
-- BORGE-1790987438 (2026-10-02 prod lock pileup): the tree-wide
-- tree_generations SINGLETON row (migration 018) is the head counter for
-- every branch-free data.* path. tree_version_append() and the sibling-order
-- trigger allocate a gen with
--
--     UPDATE tree_generations SET generation = generation + 1 RETURNING ...
--
-- which takes a TRANSACTION-SCOPED ROW LOCK on the one row and holds it
-- until COMMIT. Every datum write tree-wide — notes, todos, chat, any
-- package data row — contends on that single row. Measured on dev PG 18.6:
-- one session holding a tx that wrote a datum (row lock on
-- tree_generations) blocks 4+ concurrent DISJOINT-path datum writers on
-- the SAME single row (pg_locks shows one granted RowExclusiveLock + a
-- chain of un-granted ExclusiveLocks on tree_generations; pg_stat_activity
-- wait_event=tuple). That is the amplifier that turned one leaked
-- idle-in-transaction session into a tree-wide write freeze.
--
-- FIX (chosen from the issue's options, after measuring): replace the
-- singleton row as the gen SOURCE for data.* paths with a Postgres SEQUENCE
-- (tree_generations_seq). `nextval` is NON-TRANSACTIONAL: it allocates the
-- next value without taking any row lock and holds nothing across the
-- transaction, so concurrent disjoint-path datum writes no longer serialize
-- on one row.
--
--   - WHY NOT a per-type shard (the other option): tree_versions is
--     PARTITIONED BY RANGE (gen) and the retention worker (022) drops whole
--     gen windows off a GLOBAL floor (head_gen - keep_windows*1000). A
--     sharded counter gives each shard its own low/high range, so a cold
--     shard's NEWEST rows would sit below the global floor and be rolled
--     up + dropped as if ancient — subtle data loss. The sequence keeps ONE
--     GLOBAL gen axis: retention windows, as-of reads, and the (path, gen)
--     uniqueness semantics are unchanged.
--   - GAPS ARE HARMLESS (sequence values are never reclaimed): every reader
--     of tree_versions is either window-based (022 retention), ORDER BY gen
--     DESC LIMIT 1 (tree_versions_head), or gen <= at_gen (as-of reads);
--     undo restores-by-write (crud.ml tree_undo writes a NEW version row
--     through the trigger — it never computes gen+1 numerically). No
--     contiguity assumption anywhere. A rolled-back datum write consumes a
--     gen number that no row uses — invisible to every consumer.
--   - SAME-PATH ordering preserved: for a fixed path, nextval is strictly
--     increasing, so per-path gen order (the only order that matters for
--     undo / head / as-of) is unchanged.
--   - DB-side (CREATE OR REPLACE on the trigger functions), so OLD image
--     binaries — whose write path goes through the DB triggers and the
--     store_retention SQL call — inherit the fix with no code deploy.
--
-- The tree_generations TABLE remains (additive contract: nothing is
-- dropped), but it is FROZEN at its last value — writers no longer touch
-- it, so it never becomes a lock point. The sequence is the authority;
-- 021's reconcile heals the sequence seed (a sequence cannot regress, the
-- arm is belt-and-braces), 022's retention reads last_value.
--
-- Idempotent: CREATE SEQUENCE IF NOT EXISTS + CREATE OR REPLACE FUNCTION
-- (safe to re-apply; the 019/020/021/022/025 files are untouched historical
-- records). Registered in schema_migrations at the end.

-- ---------------------------------------------------------------------------
-- The sequence. Seeded UNCONDITIONALLY but idempotently (see below): the
-- seed is the GREATEST of the current last_value, the old singleton counter
-- and the max data gen ever written, so new gens never collide with an
-- existing (path, gen) row, never regress, and a re-run never clamps a live
-- sequence.
-- ---------------------------------------------------------------------------
CREATE SEQUENCE IF NOT EXISTS tree_generations_seq START 1;

-- Seed the sequence UNCONDITIONALLY, but idempotently: the new value is
-- GREATEST(current last_value, old singleton counter, max data gen ever
-- written, 1), so a re-run never LOWERS a live sequence (a running image
-- may have advanced it) and never starts below an existing (path, gen)
-- row. On a fresh DB (no rows) this lands the sequence at 1.
-- NOTE: do NOT guard the setval with WHERE NOT EXISTS on pg_sequences —
-- the just-created sequence is already visible there in the same session,
-- so the guard would skip the seed exactly when it is needed most (an
-- existing DB whose data gens already exceed the sequence START).
SELECT setval('tree_generations_seq',
       GREATEST(
         COALESCE((SELECT last_value FROM tree_generations_seq), 0),
         COALESCE((SELECT generation FROM tree_generations LIMIT 1), 0),
         COALESCE((SELECT max(gen) FROM tree_versions WHERE branch_id IS NULL), 0),
         1),
       true);

-- ---------------------------------------------------------------------------
-- tree_version_append() — the node-level AFTER trigger (019, refined by 020's
-- mirror-echo guard + 025's sibling-order rank). Data.* gen allocation moves
-- from the singleton row UPDATE (row lock held till commit) to nextval.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_version_append()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
DECLARE
  bid  uuid;
  ngen bigint;
BEGIN
  -- MIRROR NO-OP WRITE-BACK guard (020), rank-aware (025): identical
  -- kind/type/author/body AND identical rank is an echo; a rank delta is
  -- an authoritative reorder and MUST be versioned.
  IF TG_OP = 'UPDATE' AND NEW.kind = OLD.kind
     AND NEW.type_name IS NOT DISTINCT FROM OLD.type_name
     AND NEW.author = OLD.author
     AND NEW.rank IS NOT DISTINCT FROM OLD.rank
     AND NEW.payload->'body' = OLD.payload->'body' THEN
    RETURN NEW;
  END IF;

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
    -- data.* head = tree_generations_seq (038). nextval holds NO row lock,
    -- so datum writes no longer serialize on a shared counter (the
    -- BORGE-1790987438 amplifier). Gaps from rolled-back writers are
    -- harmless (readers are window / DESC-head / <= at_gen; undo
    -- restores-by-write). Per-path order is still strictly increasing.
    ngen := nextval('tree_generations_seq');
  END IF;

  PERFORM tree_versions_prepare(ngen);

  IF TG_OP = 'DELETE' THEN
    -- TOMBSTONE: reserved marker body (same `_tombstone` key
    -- tree_versions_tombstone() returns), never a DELETE.
    INSERT INTO tree_versions
      (branch_id, path, gen, kind, type_name, rank, body, hash, author,
       created_at)
    VALUES
      ( bid, OLD.path, ngen, OLD.kind, OLD.type_name, OLD.rank,
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

  -- INSERT / UPDATE: record the new head payload at gen (with its rank).
  INSERT INTO tree_versions
    (branch_id, path, gen, kind, type_name, rank, body, hash, author,
     created_at)
  VALUES
    ( bid, NEW.path, ngen, NEW.kind, NEW.type_name, NEW.rank,
      NEW.payload, NEW.hash, NEW.author, now() );
  RETURN NEW;
END $function$;

-- ---------------------------------------------------------------------------
-- tree_versions_reconcile_heads() (021): the tombstone-head heal loop uses
-- the same nextval allocation; the data counter-heal arm now heals the
-- SEQUENCE seed (belt-and-braces — a sequence cannot regress).
-- ---------------------------------------------------------------------------
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
      -- data.* head = tree_generations_seq (038); nextval, no row lock.
      ngen := nextval('tree_generations_seq');
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
  --     code-gen of its own live code version rows is a head-coordinate
  --     regression. Raise to the max. Idempotent (GREATEST with the current
  --     value). The data head is tree_generations_seq (038) — a sequence
  --     cannot regress, but heal the seed anyway (belt-and-braces) if it is
  --     somehow below the max live data-gen.
  UPDATE branch_generations bg
  SET generation = GREATEST(bg.generation, sub.max_gen)
  FROM (SELECT v.branch_id, max(v.gen) AS max_gen
        FROM tree_versions v
        WHERE v.branch_id IS NOT NULL
          AND EXISTS (SELECT 1 FROM tree_nodes tn WHERE tn.path = v.path)
        GROUP BY v.branch_id) sub
  WHERE sub.branch_id = bg.branch_id
    AND bg.generation < sub.max_gen;

  IF (SELECT last_value FROM tree_generations_seq)
     < (SELECT COALESCE(max(v.gen), 0) FROM tree_versions v
        WHERE v.branch_id IS NULL
          AND EXISTS (SELECT 1 FROM tree_nodes tn WHERE tn.path = v.path))
  THEN
    PERFORM setval('tree_generations_seq',
      (SELECT max(v.gen) FROM tree_versions v
       WHERE v.branch_id IS NULL
         AND EXISTS (SELECT 1 FROM tree_nodes tn WHERE tn.path = v.path)),
      true);
  END IF;

  RETURN healed;
END $function$;

-- ---------------------------------------------------------------------------
-- tree_versions_retain() (022): the data head anchor for the retention floor
-- moves from the frozen tree_generations row to the sequence's last_value.
-- last_value >= max written data gen, so the floor is at worst one partial
-- window HIGHER than before — the SAFE direction (never drops a window that
-- could still hold a live head). Old binaries calling this via
-- store_retention.ml inherit the fix.
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
    SELECT COALESCE(last_value, 0) INTO head_gen FROM tree_generations_seq;
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
SELECT 38, 'de-contend data.* head counter: tree_generations_seq nextval (no row lock on a shared row); tree_generations row frozen (BORGE-1790987438)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 38);
