-- Pricklypear migration 032: the sync merge index — per-client base manifests.
--
-- borge/local-first.borg sync, the chunked/delta exchange (2026-09-11).
-- The v1 sync/push exchange carries the client's FULL (path, hash)
-- manifest every round — O(subtree) on the wire both ways. Past nginx's
-- ~1MB request cap that 413s (issue i-1789178161-87d0742f: 2124 rows was
-- fine at ~290KB manifest, but the envelope bodies blew it; with bodies
-- fixed, the manifest itself is the next cliff at ~7k rows), and the
-- server cannot interpret a CHUNKED (subset) manifest: absent paths
-- would classify as ServerOnly and inflate the pull list.
--
-- The fix is the spec's own words: "The server keeps the corpus, the
-- data plane authority, and THE MERGE INDEX." The merge index is the
-- per-client last-synced (merged) manifest — git's remote-tracking ref
-- in miniature. One row per (user, lens): the server stores the merged
-- manifest it produced at the client's last commit (sync/commit2), and
-- later rounds negotiate DELTAS against it: the client sends only what
-- changed vs its local base (changes + deletions); paths absent from
-- the delta mean "client unchanged = base", so the server can classify
-- a subset manifest honestly and the request becomes O(changes).
--
-- Not content: never a tree datum, never in a manifest, never a
-- pp-sync snapshot (same discipline as function_stat 031). One row per
-- (user, lens) — bounded by device count, upserted in place.
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

CREATE TABLE IF NOT EXISTS sync_bases (
  user_id    uuid        NOT NULL,
  lens       text        NOT NULL,
  branch     text        NOT NULL,
  generation bigint      NOT NULL,
  manifest   jsonb       NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (user_id, lens)
);

INSERT INTO schema_migrations (version, description)
SELECT 32, 'sync merge index: per-client base manifests (delta exchange)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 32);
