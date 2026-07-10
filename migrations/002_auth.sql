-- Pricklypear migration 002: the auth tables.
--
-- Applied by the supervisor (which owns schema state for the whole
-- system) but the [system_keys] table is granted ONLY to a dedicated
-- pg role [pricklypear_auth] that ONLY the auth unikernel connects as.
-- A DB dump from any other role doesn't include the signing key
-- (defense-in-depth, free; see auth.credentials-store).
--
-- KNOWN RISK (per spec): a DB dump from auth's role, or an auth-image
-- compromise, leaks the signing key -> all web sessions forgeable.
-- Accepted for personal/small-team scope. HARDENING GOAL (when this
-- becomes stable): move the signing key to auth-image memory only.

CREATE TABLE IF NOT EXISTS users (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  username    text NOT NULL UNIQUE,
  is_admin    boolean NOT NULL DEFAULT false,
  created_at  timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS credentials (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id     uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  kind        text NOT NULL,        -- 'ssh_key' | 'password' | 'web_token' | 'api_key'
  public_data jsonb,                -- SSH key material for verification; null for password
  secret_data jsonb,                -- argon2 hash for password; reference for api_key
  created_at  timestamptz NOT NULL DEFAULT now(),
  last_used_at timestamptz
);

CREATE INDEX IF NOT EXISTS credentials_user_idx ON credentials (user_id);
CREATE INDEX IF NOT EXISTS credentials_kind_idx ON credentials (kind);

-- auth_sessions: NOT to be confused with the supervisor's `sessions` table
-- (which tracks image instances). auth_sessions are login sessions (web
-- tokens). Named to avoid the clash.
CREATE TABLE IF NOT EXISTS auth_sessions (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),           -- the token's opaque id
  user_id     uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  token_hash  text NOT NULL,              -- HMAC of the token; lookup target
  expires_at  timestamptz NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now(),
  revoked_at  timestamptz
);

CREATE INDEX IF NOT EXISTS auth_sessions_user_idx ON auth_sessions (user_id);
CREATE INDEX IF NOT EXISTS auth_sessions_hash_idx ON auth_sessions (token_hash);

CREATE TABLE IF NOT EXISTS auth_log (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id     text,                       -- NULL if unknown/failed
  kind        text NOT NULL,               -- 'ssh_key' | 'password' | 'mint' | 'verify_token'
  success     boolean NOT NULL,
  source_ip   text,
  happened_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS auth_log_user_idx ON auth_log (user_id);
CREATE INDEX IF NOT EXISTS auth_log_kind_idx ON auth_log (kind);

CREATE TABLE IF NOT EXISTS system_keys (
  id          text PRIMARY KEY,           -- 'session_signing'
  key_data    bytea NOT NULL,              -- the HMAC signing key, raw
  created_at  timestamptz NOT NULL DEFAULT now()
);

-- Role isolation: only the auth unikernel connects as pricklypear_auth.
-- It gets its own role (separate from the supervisor's role) with
-- SELECT/INSERT/UPDATE on auth tables only. system_keys is restricted
-- further: only pricklypear_auth can SELECT it.
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pricklypear_auth') THEN
    CREATE ROLE pricklypear_auth LOGIN PASSWORD 'change-me-on-prod';
  END IF;
END $$;

GRANT SELECT, INSERT, UPDATE ON users, credentials, auth_sessions,
                              auth_log, system_keys TO pricklypear_auth;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO pricklypear_auth;

INSERT INTO schema_migrations (version, description)
SELECT 2, 'auth tables (users, credentials, auth_sessions, auth_log, system_keys)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 2);
