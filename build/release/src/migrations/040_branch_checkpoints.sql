-- Pricklypear migration 040: branch checkpoints — named points on the gen stream.
--
-- Spec [branching.checkpoints] (plan phase 2026-10-12, roerick-ratified:
-- "checkpoints yes"). The substrate already records every write (tree_versions
-- append-only + content-hashed + gen-stamped, 038; per-branch
-- branch_generations counter, 017/019; code_history before-images, 012). The
-- only missing primitive was NAMING a point. jj names an implicitly-
-- snapshotted working copy; DeltaDB sells addressable deltas — both reduce to
-- NAMES PINNED TO GENS here.
--
-- A checkpoint row is (branch_id, name) -> gen captured at cp time:
--   (branch/cp "name" [message])        capture on the CURRENT branch
--   (branch/cp-list [owner/branch])     newest first, readable ref
--   (branch/cp-diff "name")             windowed per-pin classify
--   (branch/cp-restore "name" [names])  restore as NEW writes (never a time
--                                       machine: code_history before-images
--                                       fire, gen only moves forward,
--                                       base_gen untouched)
--   (branch/log [ref] ["cp-name"])      the oplog
-- SEMANTICS (branching.checkpoints): CODE ONLY (fn/type/route pins; data
-- never rolls back); restore never moves base_gen; recovery lives in
-- code_history exactly like a merge clobber.
--
-- GEN CAPTURE discipline: branch_generations bumps are trigger-side (the
-- tree_version_append AFTER trigger, 038), so the capture must read the
-- branch's counter AFTER the prior writes commit. The store captures gen and
-- inserts the row in ONE atomic INSERT..SELECT statement.
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

CREATE TABLE IF NOT EXISTS branch_checkpoints (
  id          bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  branch_id   uuid NOT NULL REFERENCES branches(id) ON DELETE CASCADE,
  name        text NOT NULL,   -- [a-z0-9-]+ (CHECK + store-side grammar)
  gen         bigint NOT NULL, -- branch head gen at cp time
  message     text,
  author      text NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now(),
  UNIQUE (branch_id, name),
  CONSTRAINT branch_checkpoints_name_shape CHECK (name ~ '^[a-z0-9-]+$')
);

CREATE INDEX IF NOT EXISTS branch_checkpoints_branch_idx
  ON branch_checkpoints (branch_id, id DESC);

INSERT INTO schema_migrations (version, description)
SELECT 40, 'branch checkpoints: named points on the gen stream (branching.checkpoints)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 40);
