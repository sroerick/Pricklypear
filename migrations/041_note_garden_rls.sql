-- Migration 041: note-garden RLS (82871ca1). Belt guard at the SQL layer for
-- note/folder/heading rows in the tree_nodes data tree.
--
-- The existing datum_user_isolation policy (013) EXEMPTS note/folder/heading
-- kinds as "safe by path-keying"; the kernel gate (7a6567c) covers the prims.
-- The DB belt: non-datum rows under data.note.u.* belong to their owning
-- user only, unless pp.user is unset (the system/mirror lane), or the path
-- is elsewhere (data.s.* etc. stay open per the shared-space contract).
--
-- STRICTLY ADDITIVE + idempotent.

DROP POLICY IF EXISTS note_user_isolation ON tree_nodes;
CREATE POLICY note_user_isolation ON tree_nodes
  USING (
    (kind IN ('note','folder','heading')
     AND NOT (path <@ ('data.note.u.' || tree_enc_label(COALESCE(
               current_setting('pp.user', true), '')))::ltree))
    OR (kind IN ('note','folder','heading')
        AND path <@ ('data.note.u.' || tree_enc_label(
             COALESCE(current_setting('pp.user', true), '')))::ltree)
    OR COALESCE(current_setting('pp.user', true), '') = ''
  );

INSERT INTO schema_migrations (version, description)
SELECT 41, 'note garden RLS: user-scoped non-datum paths (82871ca1)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 41);
