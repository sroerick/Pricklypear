-- Pricklypear migration 017: branch generation counter for definition-cache
-- invalidation (remote-eval.borg caching subsection).
--
-- The remote Nopales client caches function/type/route defs locally under
-- a (branch_id, generation) key. A write anywhere in a branch's ancestry
-- must bump a monotonic generation counter so the client's next cheap
-- cache-check round trip observes the bump and reloads. This is
-- materialized-view invalidation in small costume: ONE counter per branch,
-- not a hash per def.
--
-- We use a small branch_generations table keyed by branch_id (kept apart
-- from the core branches row and the one-tree lens registry). Triggers on
-- the branch-scoped code tables (function_defs / type_defs /
-- view_bindings) bump the generation of the branch actually written. The
-- reverse one-tree mirror (tree_nodes -> scaffolding tables, migration 009)
-- shares these tables, so a tree-as-truth write still flows through these
-- triggers. promote/merge write function_defs/view_bindings, so those
-- bumps are covered by the same triggers (no separate promote/merge hook).
--
-- data_rows are BRANCH-FREE / shared across every branch (migration 003
-- branching.data). A data write is therefore visible to every branch's
-- ancestry, so we bump the ROOT branch(es) (parent_id IS NULL); the
-- ancestry-walking current_generation query then surfaces that bump to
-- every descendant. (The client does not cache hot data_rows — placement
-- keeps those remote — but the counter remains truthful for the shared
-- data plane.)

CREATE TABLE IF NOT EXISTS branch_generations (
  branch_id  uuid PRIMARY KEY REFERENCES branches(id) ON DELETE CASCADE,
  generation bigint NOT NULL DEFAULT 0
);

-- Seed a row for every existing branch so lookups are always non-null.
INSERT INTO branch_generations (branch_id, generation)
SELECT id, 0 FROM branches
ON CONFLICT (branch_id) DO NOTHING;

-- Bump one branch's counter (upsert to 1 then increment).
CREATE OR REPLACE FUNCTION branch_gen_bump(bid uuid) RETURNS void
LANGUAGE plpgsql AS $$
BEGIN
  IF bid IS NULL THEN RETURN; END IF;
  INSERT INTO branch_generations (branch_id, generation)
  VALUES (bid, 1)
  ON CONFLICT (branch_id)
    DO UPDATE SET generation = branch_generations.generation + 1;
END $$;

-- Bump the root branch(es) for the shared (branch-free) data plane, so a
-- data write is visible to every branch's ancestry via the CTE walk.
CREATE OR REPLACE FUNCTION branch_gen_bump_root() RETURNS void
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM branch_gen_bump(b.id)
  FROM branches b WHERE b.parent_id IS NULL;
END $$;

-- Trigger-compatible wrapper (STATEMENT triggers must return trigger).
CREATE OR REPLACE FUNCTION branch_gen_bump_root_trg() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM branch_gen_bump_root();
  RETURN NULL;
END $$;

-- ---------------------------------------------------------------------------
-- Per-statement settlement: AFTER ROW triggers accumulate the affected
-- branch_ids into branch_gen_pending; the AFTER STATEMENT trigger bumps
-- each distinct one once and clears the accumulation. This keeps a
-- multi-row statement (merge / promote) to a single bump, and works
-- whether the row trigger sees NEW (insert/update) or OLD (delete).
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS branch_gen_pending (
  pending_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  branch_id  uuid NOT NULL
);

CREATE OR REPLACE FUNCTION branch_gen_accumulate() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    INSERT INTO branch_gen_pending(branch_id) VALUES (OLD.branch_id);
  ELSE
    INSERT INTO branch_gen_pending(branch_id) VALUES (NEW.branch_id);
  END IF;
  RETURN COALESCE(NEW, OLD);
END $$;

CREATE OR REPLACE FUNCTION branch_gen_settle() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM branch_gen_bump(branch_id)
  FROM (SELECT DISTINCT branch_id FROM branch_gen_pending) p;
  DELETE FROM branch_gen_pending;
  RETURN NULL;
END $$;

-- function_defs (branch-scoped code writes incl. promote/merge/reverse-mirror)
DROP TRIGGER IF EXISTS branch_gen_function_accum_trg ON function_defs;
CREATE TRIGGER branch_gen_function_accum_trg
  AFTER INSERT OR UPDATE OR DELETE ON function_defs
  FOR EACH ROW EXECUTE FUNCTION branch_gen_accumulate();
DROP TRIGGER IF EXISTS branch_gen_function_settle_trg ON function_defs;
CREATE TRIGGER branch_gen_function_settle_trg
  AFTER INSERT OR UPDATE OR DELETE ON function_defs
  FOR EACH STATEMENT EXECUTE FUNCTION branch_gen_settle();

-- type_defs
DROP TRIGGER IF EXISTS branch_gen_type_accum_trg ON type_defs;
CREATE TRIGGER branch_gen_type_accum_trg
  AFTER INSERT OR UPDATE OR DELETE ON type_defs
  FOR EACH ROW EXECUTE FUNCTION branch_gen_accumulate();
DROP TRIGGER IF EXISTS branch_gen_type_settle_trg ON type_defs;
CREATE TRIGGER branch_gen_type_settle_trg
  AFTER INSERT OR UPDATE OR DELETE ON type_defs
  FOR EACH STATEMENT EXECUTE FUNCTION branch_gen_settle();

-- view_bindings (routes)
DROP TRIGGER IF EXISTS branch_gen_route_accum_trg ON view_bindings;
CREATE TRIGGER branch_gen_route_accum_trg
  AFTER INSERT OR UPDATE OR DELETE ON view_bindings
  FOR EACH ROW EXECUTE FUNCTION branch_gen_accumulate();
DROP TRIGGER IF EXISTS branch_gen_route_settle_trg ON view_bindings;
CREATE TRIGGER branch_gen_route_settle_trg
  AFTER INSERT OR UPDATE OR DELETE ON view_bindings
  FOR EACH STATEMENT EXECUTE FUNCTION branch_gen_settle();

-- data_rows (shared data plane -> bump the ROOT branch(es) so every
-- branch's ancestry sees the change).
DROP TRIGGER IF EXISTS branch_gen_data_row_trg ON data_rows;
CREATE TRIGGER branch_gen_data_row_trg
  AFTER INSERT OR UPDATE OR DELETE ON data_rows
  FOR EACH STATEMENT EXECUTE FUNCTION branch_gen_bump_root_trg();

-- ---------------------------------------------------------------------------
-- Current-generation query used by the decorated inheritance CTE: the max
-- generation across the branch's ancestry chain. Bumping an ancestor (e.g.
-- main) is automatically visible to every descendant.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION current_branch_generation(bid uuid) RETURNS bigint
LANGUAGE sql STABLE AS $$
  WITH RECURSIVE chain AS (
    SELECT id, parent_id FROM branches WHERE id = bid
    UNION ALL
    SELECT b.id, b.parent_id FROM branches b
    JOIN chain c ON b.id = c.parent_id
  )
  SELECT COALESCE(MAX(bg.generation), 0)
    FROM chain c
    LEFT JOIN branch_generations bg ON bg.branch_id = c.id
$$;

INSERT INTO schema_migrations (version, description)
SELECT 17, 'branch generation counter + triggers for def-cache invalidation'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 17);
