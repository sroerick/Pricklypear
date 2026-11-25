-- 051_data_grants.sql — sharing-grants rollout step 2: data_grants and the
-- read disjunct, beside the existing spaces.
--
-- Spec: borge/branching.borg (subsection sharing-grants). A share is a GRANT
-- on an ltree path prefix. Rows never move; a grant only widens who may READ
-- what already lives in its author's compartment.
--
-- WHAT THIS STEP DOES
--   1. data_grants, as the book specifies it, plus a GiST index on prefix.
--   2. Attenuation at the table: a grant with a parent must name a live
--      parent that holds 'reshare', a prefix inside the parent's prefix and
--      a subset of the parent's rights. Revoking a grant revokes its live
--      descendants (trigger), so the read predicate checks one row.
--   3. tree_path_granted(path, user): the one grant predicate. Grantee is a
--      username, 'authenticated' (any signed-in identity) or 'anonymous'
--      (everyone). Only the 'read' right confers visibility.
--   4. The predicate joins the READ side only, in both SQL mirrors:
--        - pp_vis_readable (050), which both tree_nodes policies' USING and
--          the 049 read wrappers already call;
--        - the tree_data_rows view.
--      pp_node_visible, and so WITH CHECK, is untouched: a grant never
--      opens a write. crud.ml's read-side mirror is Crud.visible_read_sql.
--   5. data_grants_generation, bumped per statement on data_grants, which
--      the in-image subtree cache keys data.* entries on.
--
-- NOT IN THIS STEP (book ORDER and KNOWN LEAK SURFACES): groups as
-- grantees, agent owners and their default grant (step 3), any prim or UI
-- that creates grants (step 4), backlinks/search, the replication unit,
-- and the RLS cost
-- measurement against the production budget. Until a prim exists, only a
-- superuser session writes this table.
--
-- tree_path_granted is invoker-rights, like tree_path_in_member_space: a
-- role that reads tree_nodes needs SELECT on data_grants.
--
-- Additive and idempotent.

CREATE TABLE IF NOT EXISTS data_grants (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  prefix      ltree NOT NULL,
  grantee     text NOT NULL,
  rights      text[] NOT NULL,
  granted_by  text NOT NULL,
  parent      uuid REFERENCES data_grants(id),
  revoked_at  timestamptz,
  created_at  timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT data_grants_rights_known
    CHECK (cardinality(rights) > 0
           AND rights <@ ARRAY['read','comment','write','reshare']::text[]),
  CONSTRAINT data_grants_grantee_nonempty CHECK (grantee <> '')
);

CREATE INDEX IF NOT EXISTS data_grants_prefix_live_gist
  ON data_grants USING gist (prefix) WHERE revoked_at IS NULL;
CREATE INDEX IF NOT EXISTS data_grants_parent_idx ON data_grants (parent);

-- Attenuation: a re-share may only narrow.
CREATE OR REPLACE FUNCTION data_grants_attenuate() RETURNS trigger
LANGUAGE plpgsql AS $fn$
DECLARE
  p data_grants%ROWTYPE;
BEGIN
  IF NEW.parent IS NULL THEN
    RETURN NEW;
  END IF;
  SELECT * INTO p FROM data_grants WHERE id = NEW.parent;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'data_grants: parent % does not exist', NEW.parent;
  END IF;
  IF p.revoked_at IS NOT NULL THEN
    RAISE EXCEPTION 'data_grants: parent % is revoked', NEW.parent;
  END IF;
  IF NOT ('reshare' = ANY (p.rights)) THEN
    RAISE EXCEPTION 'data_grants: parent % does not hold reshare', NEW.parent;
  END IF;
  IF NOT (NEW.prefix <@ p.prefix) THEN
    RAISE EXCEPTION 'data_grants: re-share prefix % is not inside parent prefix %',
      NEW.prefix, p.prefix;
  END IF;
  IF NOT (NEW.rights <@ p.rights) THEN
    RAISE EXCEPTION 'data_grants: re-share rights % exceed parent rights %',
      NEW.rights, p.rights;
  END IF;
  RETURN NEW;
END
$fn$;

DROP TRIGGER IF EXISTS data_grants_attenuate ON data_grants;
CREATE TRIGGER data_grants_attenuate
  BEFORE INSERT OR UPDATE OF parent, prefix, rights ON data_grants
  FOR EACH ROW EXECUTE FUNCTION data_grants_attenuate();

-- Revocation kills descendants. Each child update fires this trigger again,
-- so the whole lineage below the revoked grant is marked.
CREATE OR REPLACE FUNCTION data_grants_revoke_cascade() RETURNS trigger
LANGUAGE plpgsql AS $fn$
BEGIN
  IF NEW.revoked_at IS NOT NULL AND OLD.revoked_at IS NULL THEN
    UPDATE data_grants SET revoked_at = NEW.revoked_at
     WHERE parent = NEW.id AND revoked_at IS NULL;
  END IF;
  RETURN NULL;
END
$fn$;

DROP TRIGGER IF EXISTS data_grants_revoke_cascade ON data_grants;
CREATE TRIGGER data_grants_revoke_cascade
  AFTER UPDATE OF revoked_at ON data_grants
  FOR EACH ROW EXECUTE FUNCTION data_grants_revoke_cascade();

-- Grant generation: one counter bumped by every statement that changes
-- data_grants (cascaded revokes included, since they are statements too).
-- The in-image subtree cache (Tree_store.cached_list_subtree) stores it with
-- each data.* entry, so a revoke is seen on the next read instead of after
-- the cache TTL (book: KNOWN LEAK SURFACES, cached subtree reads).
CREATE TABLE IF NOT EXISTS data_grants_generation (
  id   boolean PRIMARY KEY DEFAULT true CHECK (id),
  gen  bigint NOT NULL DEFAULT 0
);
INSERT INTO data_grants_generation (id, gen) VALUES (true, 0)
  ON CONFLICT (id) DO NOTHING;

CREATE OR REPLACE FUNCTION data_grants_bump_generation() RETURNS trigger
LANGUAGE plpgsql AS $fn$
BEGIN
  UPDATE data_grants_generation SET gen = gen + 1 WHERE id;
  RETURN NULL;
END
$fn$;

DROP TRIGGER IF EXISTS data_grants_bump_generation ON data_grants;
CREATE TRIGGER data_grants_bump_generation
  AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON data_grants
  FOR EACH STATEMENT EXECUTE FUNCTION data_grants_bump_generation();

-- The one grant predicate.
CREATE OR REPLACE FUNCTION tree_path_granted(p_path ltree, p_user text)
RETURNS boolean
LANGUAGE sql
STABLE
AS $fn$
  SELECT EXISTS (
    SELECT 1 FROM data_grants g
     WHERE g.revoked_at IS NULL
       AND g.prefix @> p_path
       AND 'read' = ANY (g.rights)
       AND (g.grantee = 'anonymous'
            OR (g.grantee = 'authenticated'
                AND COALESCE(p_user, '') NOT IN ('', 'anonymous'))
            OR (g.grantee = COALESCE(p_user, '')
                AND COALESCE(p_user, '') NOT IN ('', 'anonymous',
                                                 'authenticated'))))
$fn$;

COMMENT ON FUNCTION tree_path_granted(ltree, text) IS
  'sharing-grants: a live grant with the read right covers p_path for p_user (username, authenticated, anonymous). Read side only (051).';

-- Read side, mirror 1: pp_vis_readable (050) gains the grant disjunct.
CREATE OR REPLACE FUNCTION pp_vis_readable(p_path ltree, p_payload jsonb)
RETURNS boolean
LANGUAGE sql
STABLE
AS $fn$
  SELECT COALESCE(p_payload #>> '{body,vis}', '') = 'public'
      OR (COALESCE(p_payload #>> '{body,vis}', '') = 'users'
          AND COALESCE(current_setting('pp.user', true), '') NOT IN ('', 'anonymous'))
      OR (COALESCE(p_payload #>> '{body,vis}', '') = 'private'
          AND tree_path_in_member_space(p_path, current_setting('pp.user', true)))
      OR tree_path_granted(p_path, current_setting('pp.user', true))
$fn$;

COMMENT ON FUNCTION pp_vis_readable(ltree, jsonb) IS
  'Read-side widening: public = anyone, users = signed-in, private = member space, plus a live read grant (051). Absent vis widens nothing. No root bypass.';

-- Read side, mirror 2: the view keeps the 033 shape and gains the disjunct.
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
    OR tree_path_granted(path, current_setting('pp.user', true))
  );

-- The 049 read wrappers are SECURITY DEFINER and, where provisioned, owned
-- by pp_rls_definer; they call pp_vis_readable and so tree_path_granted as
-- that role. Give it (and the equivalence test's probe) the same read on
-- data_grants that tests/sql/rls_definer_role.sql gives them on the space
-- tables, or every wrapper read would fail with "permission denied".
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_definer') THEN
    GRANT SELECT ON data_grants TO pp_rls_definer;
    GRANT EXECUTE ON FUNCTION tree_path_granted(ltree, text) TO pp_rls_definer;
  END IF;
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_probe') THEN
    GRANT SELECT ON data_grants TO pp_rls_probe;
    GRANT EXECUTE ON FUNCTION tree_path_granted(ltree, text) TO pp_rls_probe;
  END IF;
END $$;

INSERT INTO schema_migrations (version, description)
SELECT 51, 'sharing-grants step 2: data_grants + attenuation/revocation triggers + tree_path_granted on the read side (pp_vis_readable, tree_data_rows)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 51);
