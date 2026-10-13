-- Pricklypear migration 012: append-only before-image history for code rows.
-- Spec: branching.code-history
--
-- Captures OLD on BEFORE UPDATE OR DELETE for function_defs, type_defs,
-- and view_bindings. INSERT does not fire. Triggers run in the writer
-- transaction so rollbacks leave no history rows.

CREATE TABLE IF NOT EXISTS code_history (
  id         bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  rel        text NOT NULL,          -- 'function_defs' | 'type_defs' | 'view_bindings'
  branch_id  uuid NOT NULL,
  name       text NOT NULL,          -- fn/type name or route path
  body       jsonb,                  -- superseded before-image
  author     text,
  doc        text,
  package    text,
  op         text NOT NULL,          -- 'UPDATE' | 'DELETE'
  changed_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS code_history_lookup_idx
  ON code_history (rel, branch_id, name, changed_at DESC);

CREATE OR REPLACE FUNCTION code_history_capture() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF TG_TABLE_NAME = 'function_defs' THEN
    INSERT INTO code_history
      (rel, branch_id, name, body, author, doc, package, op)
    VALUES
      ('function_defs', OLD.branch_id, OLD.name, OLD.body,
       OLD.author, OLD.doc, OLD.package, TG_OP);
  ELSIF TG_TABLE_NAME = 'type_defs' THEN
    INSERT INTO code_history
      (rel, branch_id, name, body, author, doc, package, op)
    VALUES
      ('type_defs', OLD.branch_id, OLD.name, OLD.fields,
       OLD.author, NULL, NULL, TG_OP);
  ELSIF TG_TABLE_NAME = 'view_bindings' THEN
    INSERT INTO code_history
      (rel, branch_id, name, body, author, doc, package, op)
    VALUES
      ('view_bindings', OLD.branch_id, OLD.route,
       jsonb_build_object(
         'handler', OLD.handler,
         'visibility', COALESCE(OLD.visibility, 'private')),
       NULL, NULL, NULL, TG_OP);
  END IF;

  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS code_history_function_defs_trg ON function_defs;
CREATE TRIGGER code_history_function_defs_trg
  BEFORE UPDATE OR DELETE ON function_defs
  FOR EACH ROW EXECUTE FUNCTION code_history_capture();

DROP TRIGGER IF EXISTS code_history_type_defs_trg ON type_defs;
CREATE TRIGGER code_history_type_defs_trg
  BEFORE UPDATE OR DELETE ON type_defs
  FOR EACH ROW EXECUTE FUNCTION code_history_capture();

DROP TRIGGER IF EXISTS code_history_view_bindings_trg ON view_bindings;
CREATE TRIGGER code_history_view_bindings_trg
  BEFORE UPDATE OR DELETE ON view_bindings
  FOR EACH ROW EXECUTE FUNCTION code_history_capture();

INSERT INTO schema_migrations (version, description)
SELECT 12, 'code_history table + BEFORE UPDATE/DELETE triggers on code rows'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 12);
