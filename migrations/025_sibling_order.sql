-- Pricklypear migration 025: sibling-order (one-tree.sibling-order).
--
-- Children get a USER-CONTROLLED ORDER: a nullable rank TEXT column on
-- tree_nodes (fractional-indexing string, Figma/Linear style), carried
-- through the tree-time version shadow so ordering is versioned,
-- diffable, undoable like every other node property.
--
--   - NOT fractional path labels: the path is WHERE a node is; the rank
--     is WHAT SLOT it is in. Rank is mutable without touching identity.
--   - NULL rank means unordered: reads order by
--       rank COLLATE "C" NULLS LAST, path::text ASC
--     so unranked siblings sort after ranked ones, path-ordered among
--     themselves. Existing rows need no backfill; ordering is opt-in
--     per sibling set.
--   - Rank strings are base-36 fractional strings over [0-9a-z] compared
--     as raw bytes (COLLATE "C", with implicit right-padding '0') — the
--     same alphabet byte order everywhere, locale-independent. Midpoint
--     allocation + rebalance-on-exhaustion live in image/lib/tree_store.ml
--     (write-path detail, not visible in the prim ABI).
--   - tree_versions gains the same rank column and tree_version_append
--     echoes it, so gen-read/undo carry order (reorder is an ordinary
--     versioned write; a rank-only UPDATE is NOT a mirror echo).
--
-- This file is strictly ADDITIVE + idempotent + image-applied (registered
-- in schema_migrations so the runner will not re-apply).

ALTER TABLE tree_nodes ADD COLUMN IF NOT EXISTS rank text;
ALTER TABLE tree_versions ADD COLUMN IF NOT EXISTS rank text;

-- Read-shape index for ranked sibling sets. The children read is a LEVEL
-- predicate over the ltree GiST path index (CHILDREN-BY-LEVEL invariant —
-- never an anti-join); this partial btree only covers the ranked subset so
-- rank-presence checks and rank-ordered scans of a small set stay cheap.
CREATE INDEX IF NOT EXISTS tree_nodes_rank_idx
  ON tree_nodes (rank COLLATE "C")
  WHERE rank IS NOT NULL;

-- The node-level AFTER trigger body (migration 019, refined by 020's
-- mirror-echo guard), now carrying RANK:
--   - the echo guard must treat a RANK-ONLY change as a REAL write
--     (a reorder is content), so `rank IS NOT DISTINCT FROM` joins the
--     no-op condition;
--   - both version INSERTs gain the rank column (NEW.rank / OLD.rank),
--     so gen-read / gen-diff / undo see the order that was in force at
--     that generation.
CREATE OR REPLACE FUNCTION tree_version_append()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
DECLARE
  bid  uuid;
  ngen bigint;
BEGIN
  -- MIRROR NO-OP WRITE-BACK guard (020), now rank-aware: identical
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

INSERT INTO schema_migrations (version, description)
SELECT 25, 'sibling-order: tree_nodes/tree_versions rank column + rank-aware version trigger'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 25);
