-- Pricklypear migration 003: generic data row storage.
--
-- Per spec [storage.types]: user-declared types in Lisp (registered via
-- (deftype 'name fields...)) are stored as type_defs (migration 001).
-- Actual data rows of those types live HERE in `data_rows`.
--
-- Data rows are shared across branches (per spec [branching.data]):
-- fork/merge only copies CODE (functions/types/views), not data.
-- All branches see the SAME data rows. Writes go straight to main;
-- reads don't need the recursive-CTE branch walk.
--
-- Design choice for v1: rows are stored as a single jsonb {fields}
-- document per row, keyed by (type_name, id). This avoids generating
-- one PG table per Lisp-declared type (which would require DDL at
-- runtime and break the additive-migrations_only rule). Future work:
-- if a type is hot enough to warrant its own table, a migration can
-- hoist it (the Lisp layer wouldn't change).
--
-- Per spec [workflow.conventions]: SERIALIZABLE transactions by
-- default for mutations. Additive-only migration. No ORM.

CREATE TABLE IF NOT EXISTS data_rows (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  type_name   text NOT NULL,         -- Lisp-level type, e.g. 'user', 'post'
  fields      jsonb NOT NULL,        -- {field_name: value, ...} as JSON
  author      text NOT NULL,         -- session username that wrote it
  created_at  timestamptz NOT NULL DEFAULT now(),
  updated_at  timestamptz NOT NULL DEFAULT now()
);

-- Branchable lookups: by type, by (type, field) value.
CREATE INDEX IF NOT EXISTS data_rows_type_idx ON data_rows (type_name);
CREATE INDEX IF NOT EXISTS data_rows_fields_gin ON data_rows USING gin (fields);

-- A "type registry": lightweight bookkeeping for the Lisp-level type
-- defs that have been INSTANTIATED (so (list 'user) knows the type
-- exists). type_defs in 001 is the source of truth for type DECLARATION;
-- this is just a fast (name -> exists) cache, same data.
-- We don't add a separate table; type_defs already has it.

INSERT INTO schema_migrations (version, description)
SELECT 3, 'generic data row storage (shared across branches)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 3);
