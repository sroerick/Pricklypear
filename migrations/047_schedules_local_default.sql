-- Pricklypear migration 047: human schedules keep their local meaning.
--
-- 11-01 (roerick: "let's do a proper pass"): work-digest fired at 15:00 UTC
-- = 08:00 Denver because its row carried tz='utc' from before the tz column
-- existed. 046 wrote down the hand-flip the work-digest needed and it never
-- happened, so the class stayed broken.
--
-- CODE: jobs.ml schedule_upsert now defaults tz to "local" (image process
-- zone, America/Denver in prod, DST included). Machine jobs whose cadence
-- must not move with human clocks pass "utc" explicitly; every-ms schedules
-- ignore tz entirely.
--
-- DATA: work-digest is the fixed-hour human row. Flipping its tz leaves the
-- stored next_run at the old UTC instant, so it fires once at that time and
-- advance_schedule reschedules it on the local calendar from then on.
-- Re-upserting the schedule recomputes next_run immediately if the early
-- fire should be skipped.
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

UPDATE schedules SET tz = 'local'
 WHERE name = 'work-digest' AND branch = 'admin/personal' AND tz = 'utc';

INSERT INTO schema_migrations (version, description)
SELECT 47, 'schedules: work-digest on the local calendar; local is the new default'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 47);
