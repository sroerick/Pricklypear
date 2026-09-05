-- 028_branch_def_gen.sql — image.per-identity-sessions staleness fast path.
--
-- PROBLEM: Store.visible_stamp runs count(*)+max(updated_at) over
-- tree_function_defs / tree_type_defs / tree_view_bindings on the
-- session's branch chain — a seq scan over tree_nodes with a
-- tree_path_branch() call PER ROW, x3 relations, on EVERY request
-- (attach). Measured ~21ms on the dev DB (2.3k fn nodes); worse on prod
-- (i-1788359831: ltree scans 0.7-1.0s there). The borge contract
-- (borge/image.borg STALENESS: "one cheap query per session ... ~ms")
-- is violated by the implementation.
--
-- FIX: monotonic per-branch def_gen counter on tree_branches, bumped by
-- a row-level trigger on tree_nodes for fn/type/route writes ONLY (data
-- writes must not invalidate def sessions). NO tree_in_mirror() guard:
-- merge/fork write the scaffolding tables directly and reach tree_nodes
-- through the deferred mirror — those writes MUST bump too.
--
-- visible_stamp reads max(def_gen) over the branch chain (tiny branches
-- table, index lookups) and falls back to the legacy count+max query
-- when the column/rows are missing (pre-migration safety).
-- Additive-only per repo convention. See tmp/eval-fast-path work.

ALTER TABLE tree_branches
  ADD COLUMN IF NOT EXISTS def_gen bigint NOT NULL DEFAULT 0;

CREATE OR REPLACE FUNCTION tree_bump_def_gen() RETURNS trigger AS $$
DECLARE
  bid uuid;
  k text;
BEGIN
  k := CASE WHEN TG_OP = 'DELETE' THEN OLD.kind ELSE NEW.kind END;
  IF k NOT IN ('fn', 'type', 'route') THEN
    RETURN NULL;
  END IF;
  bid := CASE WHEN TG_OP = 'DELETE' THEN tree_path_branch(OLD.path)
              ELSE tree_path_branch(NEW.path) END;
  IF bid IS NULL THEN RETURN NULL; END IF;
  UPDATE tree_branches SET def_gen = def_gen + 1 WHERE branch_id = bid;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS tree_def_gen_bump ON tree_nodes;
CREATE TRIGGER tree_def_gen_bump
AFTER INSERT OR UPDATE OR DELETE ON tree_nodes
FOR EACH ROW EXECUTE FUNCTION tree_bump_def_gen();
