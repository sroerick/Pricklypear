-- 045_jobs_lease.sql — replace runner_id-only dead-runner recovery with a
-- lease + heartbeat (borge/jobs.borg runner; the fix that lets more than one
-- process claim jobs).
--
-- WHY. 015's reaper recovered stale 'running' rows by comparing runner_id:
--
--   UPDATE jobs SET ... WHERE status='running' AND runner_id <> $1
--
-- That is only correct when exactly ONE process ever claims jobs: boot.ml
-- calls the reaper on every runner tick (including the 1s idle poll), so any
-- row claimed by a DIFFERENT live process was declared dead within a second,
-- and requeued underneath the worker still evaluating it. The SKIP LOCKED
-- claim protocol was already multi-worker-correct; only the reaper was not.
--
-- SHAPE (copied from glochid/lib/supervisor.ml's ratified claim TTL, F5):
-- a claim stamps lease_deadline = now + ttl; the runner heartbeats it while
-- the job evaluates; recovery is "the deadline passed", never "the owner is
-- someone else". A live lease is never stolen. runner_id stays the lease
-- OWNER (it is what the claim already stamps and what jobs_status_running_idx
-- indexes); the lease columns are the liveness proof.
--
-- PRE-MIGRATION ROWS: lease_deadline defaults to 0, i.e. already expired, so
-- any 'running' row left over from an older image is recovered by the normal
-- attempts/backoff rule on the first reap after upgrade. No backfill needed.

ALTER TABLE jobs ADD COLUMN IF NOT EXISTS lease_heartbeat_at bigint NOT NULL DEFAULT 0;
ALTER TABLE jobs ADD COLUMN IF NOT EXISTS lease_deadline     bigint NOT NULL DEFAULT 0;

-- Expiry scan: running rows whose lease lapsed, oldest deadline first.
CREATE INDEX IF NOT EXISTS jobs_lease_idx
  ON jobs (lease_deadline) WHERE status = 'running';

-- Host-scoped boot preempt (Jobs.preempt_dead_host_leases): find this host's
-- lease owners without scanning the whole running set.
CREATE INDEX IF NOT EXISTS jobs_runner_prefix_idx
  ON jobs (runner_id text_pattern_ops) WHERE status = 'running';

INSERT INTO schema_migrations (version, description)
SELECT 45, 'jobs lease + heartbeat (lease_heartbeat_at, lease_deadline); expiry-only reaper'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 45);
