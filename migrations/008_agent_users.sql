-- Pricklypear migration 008: agent users + token auth + promote toggle.
--
-- Additive ALTER to the users table (migrations.borg additive-only rule):
--   user_type           'human' (default) | 'agent'
--   parent_user_id      the human an agent belongs to; NULL for humans
--   allow_agent_promote the human's toggle for whether their agent may
--                       promote into their personal branch (default true)
--
-- No schema change to credentials: kind='api_key' rows already fit the
-- existing (user_id, kind, public_data, secret_data) shape — secret_data
-- holds {"token_hash": "<sha256 hex>"} and public_data holds {"label": ...}.
-- A new index on (kind, secret_data->>'token_hash') makes Bearer lookups fast.

ALTER TABLE users
  ADD COLUMN IF NOT EXISTS user_type text NOT NULL DEFAULT 'human';

ALTER TABLE users
  ADD COLUMN IF NOT EXISTS parent_user_id uuid NULL REFERENCES users(id);

ALTER TABLE users
  ADD COLUMN IF NOT EXISTS allow_agent_promote boolean NOT NULL DEFAULT true;

-- Fast lookup for Bearer token verification: WHERE kind='api_key'
-- AND secret_data->>'token_hash' = $1.
CREATE INDEX IF NOT EXISTS credentials_api_key_hash_idx
  ON credentials (kind, (secret_data->>'token_hash'))
  WHERE kind = 'api_key';

INSERT INTO schema_migrations (version, description)
SELECT 8, 'agent users (user_type, parent_user_id, allow_agent_promote) + api_key token index'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 8);
