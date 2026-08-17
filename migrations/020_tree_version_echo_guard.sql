-- Pricklypear migration 020: tree-time mirror-echo guard for tree_version_append.
--
-- tree-time.versioned-leaves (5.1/5.2): the 019 AFTER-trigger on tree_nodes
-- shadows every tree_nodes write into tree_versions. But the one-tree mirror
-- arrival means a single LOGICAL write can touch tree_nodes TWICE:
--
--   tree-first writer -> tree_nodes (authoritative, gen N)
--     -> tree_from_nodes (reverse mirror, deferred) -> scaffolding table (*_defs /
--        data_rows) -> tree_mirror_* (forward mirror, constraint) -> tree_nodes echo
--        (same content, recomputed hash; pg.tree_mirror guard stops the ping-pong)
--
-- The echo carries the same content body/author/kind/type_name (only the derived
-- `hash` — sha256 over a differently-normalized body text — is recomputed by the
-- mirror). Without a guard, 019 double-counts every logical write as two (path, gen)
-- rows and bumps the head counter 2x.
--
-- This migration REFINES tree_version_append(): on UPDATE, when the new row's
-- content (kind, type_name IS NOT DISTINCT, author, payload->'body') is unchanged
-- from OLD, it is a no-op mirror echo — return NEW BEFORE allocating a gen or
-- inserting a version row. This is the "mirror head-gen filter" of
-- tree-time.projections (every appended gen is a real new head; echoes are not
-- versions). Authoritative writes — tree-first (store writes tree_nodes directly)
-- AND table-first (a scaffolding write mirrored INTO tree_nodes with changed
-- content) — always change body/author/kind/type and land exactly one version row.
--
-- Additive: CREATE OR REPLACE on the 019 function; no table changes. Idempotent via
-- the 019 trigger (unaffected) — re-applying this only re-replaces the function.

-- The node-level AFTER trigger body, with the mirror-echo no-op guard.
CREATE OR REPLACE FUNCTION tree_version_append()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
DECLARE
  bid  uuid;
  ngen bigint;
BEGIN
  -- A MIRROR NO-OP WRITE-BACK must not allocate a gen or a version row:
  -- the reverse mirror (tree_from_nodes) synced this tree node into a
  -- scaffolding table whose forward mirror (tree_mirror_* with the
  -- pp.tree_mirror guard) wrote the SAME CONTENT back to tree_nodes. The
  -- mirror recomputes the derived `hash` field (a consistency artifact, not
  -- content), so we compare the CONTENT BODY + author + kind + type, NOT
  -- the hash: a spurious hash delta is still a mirror echo, not a new
  -- write. Content is unchanged, so versioning it would double-count every
  -- logical write as two (path, gen) rows and bump gens 2x. Authoritative
  -- writes -- tree-first (store writes tree_nodes directly) AND table-first
  -- (a scaffolding write mirrored INTO tree_nodes with changed content) --
  -- always change the body/author and land exactly one version row here.
  -- Only the no-op mirror echo drops, BEFORE gen allocation so the head
  -- counter stays aligned with the rows it actually owns.
  IF TG_OP = 'UPDATE' AND NEW.kind = OLD.kind
     AND NEW.type_name IS NOT DISTINCT FROM OLD.type_name
     AND NEW.author = OLD.author
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
  -- (with NEW) when it changes content: a single version row per logical
  -- write, no dupes.
  INSERT INTO tree_versions
    (branch_id, path, gen, kind, type_name, body, hash, author, created_at)
  VALUES
    ( bid, NEW.path, ngen, NEW.kind, NEW.type_name,
      NEW.payload, NEW.hash, NEW.author, now() );
  RETURN NEW;
END $function$;

INSERT INTO schema_migrations (version, description)
SELECT 20, 'tree-time mirror-echo guard: tree_version_append suppresses no-op mirror echoes'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 20);
