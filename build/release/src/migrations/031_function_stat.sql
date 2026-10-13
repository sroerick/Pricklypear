-- Pricklypear migration 031: function_stat telemetry leaves the object tree.
--
-- function-stat-ring (borge/image.borg) stored perf stats as tree DATUMS:
-- every row paid the full tree write (mirrors, RLS, tree_versions) and
-- every retention delete paid it AGAIN per row — at 76k rows a delete was
-- a 25-minute transaction that took prod down (incident 2026-09-08, twice:
-- the unchunked first pass, then the per-row cost even in 2000-row
-- chunks). Telemetry is not content. Dedicated table: plain INSERT on
-- flush, plain DELETE for retention (milliseconds, no triggers).
--
-- The legacy datum rows (type_name = 'function_stat') are NOT migrated:
-- they are perf samples, not ledger. The one-time prod drain (replica-
-- mode DELETE keeping the newest 1000) already shrank them; the residual
-- rows are inert — the reader switches to this table in the same release.
-- Operators may drop them at will:
--   DELETE FROM tree_nodes WHERE kind='datum' AND type_name='function_stat';
-- (replica-mode + matching data_rows cleanup recommended; see the drain
-- runbook in the incident notes.)
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

CREATE TABLE IF NOT EXISTS function_stat (
  id         bigserial PRIMARY KEY,
  fn         text NOT NULL,
  fields     jsonb NOT NULL,
  author     text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS function_stat_fn_created_idx
  ON function_stat (fn, created_at DESC);

INSERT INTO schema_migrations (version, description)
SELECT 31, 'function_stat telemetry: dedicated table (out of the object tree)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 31);
