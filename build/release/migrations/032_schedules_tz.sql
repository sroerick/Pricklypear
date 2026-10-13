-- Pricklypear migration 032: schedules learn a timezone (roerick 09-11,
-- "match cron with mountain time").
--
-- tz is per-schedule, not global: "utc" (the historical meaning, every
-- existing row) or "local" (cron fields matched against the image
-- process zone -- prod runs America/Denver via the 5cec9fe TZ knob).
-- A local cron like "0 15 * * *" then means 15:00 Denver, DST included,
-- which retires the one-shot dst-flip schedule class (b17824c9) and the
-- hand-flip the work-digest needs before DST ends 2026-11-01.
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

ALTER TABLE schedules ADD COLUMN IF NOT EXISTS tz TEXT NOT NULL DEFAULT 'utc';

INSERT INTO schema_migrations (version, description)
SELECT 32, 'schedules tz column: utc (default) or local process zone'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 32);
