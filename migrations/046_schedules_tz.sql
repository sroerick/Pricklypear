-- Pricklypear migration 046: schedules learn a timezone (roerick 09-11,
-- "match cron with mountain time").
--
-- RENUMBERED 2026-10-25 from 032_schedules_tz.sql. Two migrations claimed
-- version 32 (this one and 032_sync_bases.sql), and the promote gate keys
-- applied-ness on the version integer alone (scripts/openbsd/pricklypear-
-- promote.ksh: `ver=$(( 10#${base%%_*} ))`, skipped when already in
-- schema_migrations). sync_bases landed first and recorded version 32, so
-- `ls | sort` put 032_schedules_tz.sql first, found 32 applied, and skipped
-- it forever -- the `tz` column was never created on prod while the gate
-- read "applied". Symptoms: every schedule read/write raised 42703, and
-- 7 enabled schedules sat 13 days overdue (fixed only when boot.ml started
-- logging the swallow, card 6ab0722c). Never reuse a version number.
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
SELECT 46, 'schedules tz column: utc (default) or local process zone'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 46);
