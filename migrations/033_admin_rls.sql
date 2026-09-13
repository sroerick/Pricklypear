-- Pricklypear migration 033: admin disjunct in the datum RLS floor.
--
-- Per 2026-09-13 roerick decision: gregor maintains owner-side data
-- (flashcard resets, drain repair) that per-user RLS rightly hides.
-- is_admin already exists on users and gates the outbound/shell paths;
-- this extends the SAME flag to the data plane as one more disjunct in
-- the datum isolation policy and the app-level visible_sql mirror
-- (image/lib/crud.ml). Both layers change together so CRUD reads never
-- drift from the RLS floor.
--
-- Semantics:
--   * an admin reads/updates/deletes any datum row;
--   * update never rebinds data_rows.author (update_datum carries no
--     author), so admin maintenance keeps the row's attribution;
--   * (create) still binds author = caller: an admin writing creates
--     AS the admin, never as the row's subject;
--   * the disjunct intentionally ignores the space compartment: admin
--     is not member-scoped. Grant is_admin by hand in users only.
--
-- STABLE, not IMMUTABLE: reads users.
CREATE OR REPLACE FUNCTION tree_user_is_admin(username text)
RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT COALESCE(
    (SELECT is_admin FROM users u WHERE u.username = username),
    false)
$$;

-- tree_data_rows: re-issue the 029 shape + the admin disjunct.
CREATE OR REPLACE VIEW tree_data_rows AS
SELECT tree_datum_id(path) AS id,
       type_name,
       payload->'body' AS fields,
       payload->>'author' AS author,
       created_at,
       updated_at,
       path
FROM tree_nodes
WHERE kind = 'datum'
  AND NOT (path <@ 'data.l'::ltree)
  AND (
    COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
    OR tree_path_in_member_space(path, current_setting('pp.user', true))
    OR tree_user_is_admin(current_setting('pp.user', true))
  );

-- RLS floor: datum_user_isolation gains the admin disjunct
-- (USING + WITH CHECK, mirroring the view; drop+create per 023/029
-- precedent).
DROP POLICY IF EXISTS datum_user_isolation ON tree_nodes;
CREATE POLICY datum_user_isolation ON tree_nodes
  USING (
    kind <> 'datum'
    OR COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
    OR (
      path <@ 'data.l'::ltree
      AND subltree(path, 3, 4) = tree_enc_label(current_setting('pp.user', true))::ltree
    )
    OR tree_path_in_member_space(path, current_setting('pp.user', true))
    OR tree_user_is_admin(current_setting('pp.user', true))
  )
  WITH CHECK (
    kind <> 'datum'
    OR COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
    OR (
      path <@ 'data.l'::ltree
        AND subltree(path, 3, 4) = tree_enc_label(current_setting('pp.user', true))::ltree
    )
    OR tree_path_in_member_space(path, current_setting('pp.user', true))
    OR tree_user_is_admin(current_setting('pp.user', true))
  );

INSERT INTO schema_migrations (version, description)
SELECT 33, 'admin disjunct in datum RLS floor + tree_data_rows view'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 33);
