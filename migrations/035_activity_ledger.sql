-- Pricklypear migration 035: activity ledger (product-experience.borg
-- §activity-ledger, security.borg §the-watched-machine).
--
-- The SMF-shaped, identity-attributed, structure-only event stream. The
-- 031 rule holds verbatim: TELEMETRY IS NOT CONTENT. This lives in a
-- DEDICATED table — never tree datums — so the request path can push to
-- an in-process ring (drop-oldest), a ~30s fiber flushes with one plain
-- INSERT per row, and retention is a set-based DELETE over plain rows
-- (no mirrors, no RLS triggers, no tree_versions). A 25-minute retention
-- delete over tree datums took prod down twice (2026-09-08); this table
-- exists so the ledger can never repeat that.
--
-- Two tables:
--   activity_ledger      — raw events, bounded retention (newest ~50k)
--   activity_rollup_day  — per (day, actor, class) sums, kept forever
--                          (chargeback / budgets-as-security substrate)
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

CREATE TABLE IF NOT EXISTS activity_ledger (
  id         bigserial PRIMARY KEY,
  at         timestamptz NOT NULL DEFAULT now(),
  actor      text NOT NULL,
  session_id text,
  branch_id  text,
  class      text NOT NULL,
  target     text,
  outcome    text NOT NULL,
  cost       jsonb,
  source_ip  inet
);

CREATE INDEX IF NOT EXISTS activity_ledger_actor_at_idx
  ON activity_ledger (actor, at DESC);
CREATE INDEX IF NOT EXISTS activity_ledger_class_at_idx
  ON activity_ledger (class, at DESC);
CREATE INDEX IF NOT EXISTS activity_ledger_at_idx
  ON activity_ledger (at DESC);

-- Day rollups: kept forever, set-based upsert from the flushed raw rows.
-- (day, actor, class) is the chargeback key; steps/db_hits/db_ms sums plus
-- an event count. No content, no target — class counts only, so the
-- trifecta computation and cost medians read from this table alone.
CREATE TABLE IF NOT EXISTS activity_rollup_day (
  day     date NOT NULL,
  actor   text NOT NULL,
  class   text NOT NULL,
  events  bigint NOT NULL DEFAULT 0,
  steps   bigint NOT NULL DEFAULT 0,
  prim_calls bigint NOT NULL DEFAULT 0,
  db_hits bigint NOT NULL DEFAULT 0,
  db_ms   bigint NOT NULL DEFAULT 0,
  PRIMARY KEY (day, actor, class)
);

INSERT INTO schema_migrations (version, description)
SELECT 35, 'activity ledger: dedicated raw table + day rollups (telemetry, not content)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 35);
