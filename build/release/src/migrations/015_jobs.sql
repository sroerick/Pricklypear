-- 015_jobs.sql — durable background jobs (borge/jobs.borg)
-- Dedicated table (not data_rows): the runner's SKIP LOCKED claim protocol
-- needs real status/run_at columns. Jobs live outside branch lenses (shared).

CREATE TABLE IF NOT EXISTS jobs (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  fn          text NOT NULL,
  args        jsonb NOT NULL DEFAULT '{}'::jsonb,
  branch      text NOT NULL,
  submitted_by text NOT NULL,
  status      text NOT NULL DEFAULT 'pending',  -- pending | running | done | failed | dead
  run_at      bigint NOT NULL DEFAULT 0,         -- epoch-ms earliest start
  attempts    int  NOT NULL DEFAULT 0,
  max_attempts int NOT NULL DEFAULT 3,
  result      jsonb,
  error       text NOT NULL DEFAULT '',
  runner_id   text NOT NULL DEFAULT '',
  created_at  timestamptz NOT NULL DEFAULT now()
);

-- Claim index: due pending jobs, oldest first.
CREATE INDEX IF NOT EXISTS jobs_claim_idx
  ON jobs (run_at, id) WHERE status = 'pending';

CREATE INDEX IF NOT EXISTS jobs_submitted_by_idx
  ON jobs (submitted_by, created_at DESC);

CREATE INDEX IF NOT EXISTS jobs_status_running_idx
  ON jobs (runner_id) WHERE status = 'running';

-- job_steps: checkpoint rows for (step) memoization (borge/jobs.borg step-checkpoints).
CREATE TABLE IF NOT EXISTS job_steps (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  job_id      uuid NOT NULL REFERENCES jobs(id) ON DELETE CASCADE,
  step_name   text NOT NULL,
  value       jsonb NOT NULL DEFAULT 'null'::jsonb,
  finished_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (job_id, step_name)
);

-- schedules: cron / every-ms (borge/jobs.borg schedules).
CREATE TABLE IF NOT EXISTS schedules (
  name        text PRIMARY KEY,
  fn          text NOT NULL,
  args        jsonb NOT NULL DEFAULT '{}'::jsonb,
  branch      text NOT NULL,
  submitted_by text NOT NULL,
  cron        text,           -- 5-field cron, or NULL
  every_ms    bigint,         -- interval, or NULL
  next_run    bigint NOT NULL DEFAULT 0,  -- epoch-ms
  enabled     boolean NOT NULL DEFAULT true
);

INSERT INTO schema_migrations (version, description)
SELECT 15, 'durable background jobs (jobs, job_steps, schedules)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 15);
