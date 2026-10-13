-- Pricklypear migration 007: binary media blob storage.
--
-- Per spec [storage.media]: agent/browser image (and small binary)
-- uploads live in media_blobs, NOT in data_rows jsonb. Bytea keeps
-- payloads off the GIN-indexed JSON path and off the 64 KiB /api/eval
-- body cap. Ownership is the author column; rows are shared across
-- branches (same rule as data_rows — fork/merge copies code, not media).
--
-- Additive-only. Idempotent.

CREATE TABLE IF NOT EXISTS media_blobs (
  id            uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  author        text NOT NULL,
  content_type  text NOT NULL,
  filename      text,
  byte_size     int  NOT NULL,
  sha256        text NOT NULL,
  data          bytea NOT NULL,
  meta          jsonb NOT NULL DEFAULT '{}'::jsonb,
  created_at    timestamptz NOT NULL DEFAULT now(),
  updated_at    timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS media_blobs_author_idx
  ON media_blobs (author);
CREATE INDEX IF NOT EXISTS media_blobs_created_idx
  ON media_blobs (created_at DESC);
CREATE INDEX IF NOT EXISTS media_blobs_sha256_idx
  ON media_blobs (sha256);

INSERT INTO schema_migrations (version, description)
SELECT 7, 'media_blobs binary object store (agent image upload)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 7);
