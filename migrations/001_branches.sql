-- Pricklypear migration 001: the code store.
--
-- These tables hold BRANCHED code (functions, types, view bindings) and
-- session/supervisor bookkeeping. They are applied by the supervisor on
-- boot, before any image is allowed to start. Tracked in schema_migrations.
--
-- Per spec [storage.schema]. NOTE: NO Postgres INHERITANCE anywhere
-- (see storage.no-inheritance — branches are an OVERRIDE primitive, not
-- a UNION primitive; we use plain tables with branch_id FK and resolve
-- overrides in queries).

CREATE TABLE IF NOT EXISTS branches (
  id          uuid PRIMARY KEY,
  parent_id   uuid REFERENCES branches(id),
  owner       text NOT NULL,
  name        text NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now(),
  last_synced timestamptz,
  UNIQUE (owner, name)
);

-- The 'main' branch is always present; reseed idempotently.
INSERT INTO branches (id, parent_id, owner, name, created_at)
SELECT '00000000-0000-0000-0000-000000000001', NULL, 'system', 'main', now()
WHERE NOT EXISTS (
  SELECT 1 FROM branches WHERE owner = 'system' AND name = 'main'
);

CREATE TABLE IF NOT EXISTS function_defs (
  branch_id   uuid NOT NULL REFERENCES branches(id) ON DELETE CASCADE,
  name        text NOT NULL,
  body        jsonb NOT NULL,  -- serialized Lisp AST (see lisp/lib/ast.ml)
  doc         text,
  author      text NOT NULL,
  updated_at  timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (branch_id, name)
);

CREATE TABLE IF NOT EXISTS type_defs (
  branch_id   uuid NOT NULL REFERENCES branches(id) ON DELETE CASCADE,
  name        text NOT NULL,
  fields      jsonb NOT NULL,  -- field declarations
  migrated    boolean NOT NULL DEFAULT false,
  schema_vers integer NOT NULL DEFAULT 1,
  author      text NOT NULL,
  updated_at  timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (branch_id, name)
);

CREATE TABLE IF NOT EXISTS view_bindings (
  branch_id   uuid NOT NULL REFERENCES branches(id) ON DELETE CASCADE,
  route       text NOT NULL,    -- e.g. "/inspect/:id"
  handler     text NOT NULL,    -- function name in function_defs
  PRIMARY KEY (branch_id, route)
);

-- sessions = supervisor's fleet tracking of per-user image instances.
-- (Auth sessions live in auth_sessions in 002_auth.sql.)
CREATE TABLE IF NOT EXISTS sessions (
  id          uuid PRIMARY KEY,
  user_id     text NOT NULL,
  branch_id   uuid NOT NULL REFERENCES branches(id),
  pid         integer,             -- image instance pid (mirage/unix dev)
  boot_mode   text NOT NULL,       -- 'on-connect' | 'always-on'
  health      text NOT NULL DEFAULT 'unknown',  -- see Protocol.health_status
  started_at  timestamptz NOT NULL DEFAULT now(),
  last_seen   timestamptz
);

CREATE INDEX IF NOT EXISTS sessions_user_idx ON sessions (user_id);
CREATE INDEX IF NOT EXISTS sessions_health_idx ON sessions (health);

CREATE TABLE IF NOT EXISTS schema_migrations (
  version     integer PRIMARY KEY,
  applied_at timestamptz NOT NULL DEFAULT now(),
  description text
);

INSERT INTO schema_migrations (version, description)
SELECT 1, 'initial branches/code-store schema'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 1);
