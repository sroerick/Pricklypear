-- Pricklypear migration 011: package registry + function provenance.
--
-- lisp-interpreter.package-surface Phase 1:
--   - package_registry is branch-lensed (enable/disable + export list)
--   - path family b.<branch>.pkg.<name> via forward-only tree mirror
--   - function_defs.package stamps which package defined a binding
--
-- Disable semantics = HIDE (enabled=false), never DELETE rows.
-- std is hard-protected in OCaml; not special-cased in SQL.

ALTER TABLE function_defs
  ADD COLUMN IF NOT EXISTS package text NULL;

CREATE INDEX IF NOT EXISTS function_defs_package_idx
  ON function_defs (branch_id, package)
  WHERE package IS NOT NULL;

CREATE TABLE IF NOT EXISTS package_registry (
  branch_id   uuid NOT NULL REFERENCES branches(id) ON DELETE CASCADE,
  name        text NOT NULL,
  enabled     boolean NOT NULL DEFAULT true,
  exports     jsonb NOT NULL DEFAULT '[]'::jsonb,
  author      text NOT NULL DEFAULT 'system',
  updated_at  timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (branch_id, name)
);

CREATE INDEX IF NOT EXISTS package_registry_enabled_idx
  ON package_registry (branch_id, enabled);

-- Forward mirror: package_registry -> tree_nodes kind=pkg
-- Pattern matches 009_tree.sql tree_mirror_function_defs (pp.tree_mirror guard).
CREATE OR REPLACE FUNCTION tree_mirror_package_registry() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE path = ('b.' || tree_uuid_label(OLD.branch_id) || '.pkg.'
                   || tree_enc_label(OLD.name))::ltree;
    RETURN OLD;
  END IF;
  p := ('b.' || tree_uuid_label(NEW.branch_id) || '.pkg.'
        || tree_enc_label(NEW.name))::ltree;
  body := jsonb_build_object(
    'name', NEW.name,
    'enabled', NEW.enabled,
    'exports', COALESCE(NEW.exports, '[]'::jsonb));
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'pkg', 'path', p::text, 'type', 'pkg',
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h);
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'pkg', NULL, env, h, NEW.author, now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, updated_at = now();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS tree_mirror_package_registry_trg ON package_registry;
CREATE CONSTRAINT TRIGGER tree_mirror_package_registry_trg
  AFTER INSERT OR UPDATE OR DELETE ON package_registry
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_mirror_package_registry();

-- Expose package provenance on the tree-backed fn view when PP_TREE_READS=1.
-- package lives on the scaffolding row (not in historic tree payloads);
-- LEFT JOIN keeps tree-only rows visible with package NULL.
-- DROP required: CREATE OR REPLACE cannot reorder/rename view columns.
DROP VIEW IF EXISTS tree_function_defs;
CREATE VIEW tree_function_defs AS
SELECT tree_path_branch(n.path) AS branch_id,
       tree_dec_label((string_to_array(n.path::text, '.'))[4]) AS name,
       n.payload->'body' AS body,
       n.payload->>'author' AS author,
       n.payload->>'doc' AS doc,
       fd.package AS package,
       n.created_at, n.updated_at
FROM tree_nodes n
LEFT JOIN function_defs fd
  ON fd.branch_id = tree_path_branch(n.path)
 AND fd.name = tree_dec_label((string_to_array(n.path::text, '.'))[4])
WHERE n.kind = 'fn';

CREATE OR REPLACE VIEW tree_package_registry AS
SELECT tree_path_branch(path) AS branch_id,
       tree_dec_label((string_to_array(path::text, '.'))[4]) AS name,
       COALESCE((payload->'body'->>'enabled')::boolean, true) AS enabled,
       COALESCE(payload->'body'->'exports', '[]'::jsonb) AS exports,
       payload->>'author' AS author,
       created_at, updated_at
FROM tree_nodes WHERE kind = 'pkg';

INSERT INTO schema_migrations (version, description)
SELECT 11, 'package registry + function_defs.package provenance'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 11);
