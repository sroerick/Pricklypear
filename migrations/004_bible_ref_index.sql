-- Pricklypear migration 004: unique ref index for bible-verse rows.
--
-- Supports idempotent bulk seed of BSB (fields.ref = 'BSB:John:3:16')
-- and fast (find-row 'bible-verse "ref" …). Additive only.

CREATE UNIQUE INDEX IF NOT EXISTS data_rows_bible_ref_uidx
  ON data_rows ((fields->>'ref'))
  WHERE type_name = 'bible-verse';

CREATE INDEX IF NOT EXISTS data_rows_bible_abbrev_chapter_idx
  ON data_rows ((fields->>'abbrev'), (fields->>'chapter'))
  WHERE type_name = 'bible-verse';

INSERT INTO schema_migrations (version, description)
SELECT 4, 'bible-verse fields.ref unique index + abbrev/chapter index'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 4);
