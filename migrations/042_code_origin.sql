-- Pricklypear migration 042: version-trail origin (branching.op-transplant +
-- branching.file-bridge, plan phase 2026-10-16, spec commit 3f4d6d1).
--
-- The spec: ONE additive migration serving both subsections — the version
-- trail gains an origin column (default 'eval'; 'pick', 'file' as those
-- paths land) + a detail text (source ref / file name). branch/log renders
-- it.
--
-- MECHANISM: tree_versions rows are written by the tree_version_append()
-- AFTER trigger (019, refined by 020's mirror-echo guard + 025's
-- sibling-order rank + 038's sequence gen allocation); the OCaml write
-- helpers only touch tree_nodes. So origin flows through a
-- TRANSACTION-SCOPED custom GUC: the branch/pick and file-bridge ingest
-- paths run their landing writes inside ONE transaction after
-- set_config('pp.origin' / 'pp.origin_detail'), and the trigger reads them
-- with missing_ok — unset => 'eval' / NULL, so EVERY existing write path
-- (eval defines, boot seed, cp-restore, merges, data writes) is untouched.
-- set_config(..., is_local => true) is transaction-scoped, so a pick that
-- rides an ambient transaction (store_db with_transaction's join rule)
-- cannot leak its origin past the commit; the landing body still resets the
-- GUCs in a finally for the rider case where the ambient tx continues.
--
-- Same DB-side pattern as 038: CREATE OR REPLACE on the trigger function,
-- so OLD image binaries inherit the columns (NOT NULL DEFAULT 'eval') with
-- no code deploy. ADD COLUMN on the partitioned parent propagates to every
-- gen partition. STRICTLY ADDITIVE + idempotent + psql-applied
-- (schema_migrations gate).

ALTER TABLE tree_versions
  ADD COLUMN IF NOT EXISTS origin text NOT NULL DEFAULT 'eval';

ALTER TABLE tree_versions
  ADD COLUMN IF NOT EXISTS origin_detail text;

-- tree_version_append() — the node-level AFTER trigger (019, refined by
-- 020's mirror-echo guard + 025's sibling-order rank; data.* gen allocation
-- via tree_generations_seq, 038). 042 adds the origin provenance columns.
CREATE OR REPLACE FUNCTION tree_version_append()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
DECLARE
  bid  uuid;
  ngen bigint;
  v_origin text;
  v_detail text;
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

  -- Origin provenance (042): transaction-scoped GUCs, missing_ok. Unset =>
  -- 'eval' / NULL — every existing write path records origin='eval'. The
  -- branch/pick (origin='pick', detail=<source-branch>@<coordinate>) and
  -- file-bridge ingest (origin='file', detail=<file name>) paths set these
  -- inside their ONE landing transaction and reset them in a finally.
  v_origin := COALESCE(current_setting('pp.origin', true), 'eval');
  v_detail := NULLIF(current_setting('pp.origin_detail', true), '');

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
       created_at, origin, origin_detail)
    VALUES
      ( bid, OLD.path, ngen, OLD.kind, OLD.type_name, OLD.rank,
        jsonb_build_object(
          'v', 1, 'kind', OLD.kind, 'path', OLD.path::text,
          'type', OLD.type_name, 'body', JSON 'null',
          'author', OLD.author, 'doc', JSON 'null',
          'hash', encode(digest('__pp_tombstone__', 'sha256'), 'hex'),
          '_tombstone', true),
        encode(digest('__pp_tombstone__', 'sha256'), 'hex'),
        OLD.author, now(), v_origin, v_detail );
    RETURN OLD;
  END IF;

  -- INSERT / UPDATE: record the new head payload at gen (with its rank).
  INSERT INTO tree_versions
    (branch_id, path, gen, kind, type_name, rank, body, hash, author,
     created_at, origin, origin_detail)
  VALUES
    ( bid, NEW.path, ngen, NEW.kind, NEW.type_name, NEW.rank,
      NEW.payload, NEW.hash, NEW.author, now(), v_origin, v_detail );
  RETURN NEW;
END $function$;

INSERT INTO schema_migrations (version, description)
SELECT 42, 'version-trail origin column: eval/pick/file provenance via pp.origin GUC (branching.op-transplant + branching.file-bridge)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 42);
