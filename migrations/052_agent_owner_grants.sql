-- 052_agent_owner_grants.sql — sharing-grants rollout step 3: agent owners
-- and their default grant.
--
-- Spec: borge/branching.borg (subsection sharing-grants, AGENT PRINCIPALS):
-- "Agent identities (008_agent_users) gain an owner. By default an owner's
-- agents hold a read+write grant on the owner's data.u subtree, and no other
-- identity's agents do."
--
-- WHAT THIS STEP DOES
--   1. The owner is users.parent_user_id (008), the relation create_agent,
--      set_user_type and delete_user already maintain. No new column.
--   2. The default grant is an ordinary data_grants row:
--        (prefix data.u.<owner>, grantee <agent>, rights {read,write},
--         granted_by 'system:agent-owner')
--      so the owner revokes it like any grant, the grant generation sees it,
--      and the policies read data_grants only (never users).
--      A trigger on users keeps it in step with the relation: an agent that
--      gains or changes its owner gets the new default and loses the old
--      one; an agent that loses its owner (or stops being an agent, or is
--      deleted) loses it. A revoked default is not re-created unless the
--      relation itself changes.
--   3. The 'write' right now opens the write side, beside spaces:
--      tree_path_write_granted joins pp_node_visible (so both policies'
--      USING and WITH CHECK) and crud.ml's Crud.visible_sql. A write grant
--      must also carry 'read' (constraint), so the read side needs no change.
--   4. Backfill: every existing parented agent gets its default unless one
--      (live or revoked) already exists, so a replay never undoes a revoke.
--
-- Additive and idempotent.

-- 3. A write grant carries read.
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint
                  WHERE conname = 'data_grants_write_needs_read') THEN
    ALTER TABLE data_grants ADD CONSTRAINT data_grants_write_needs_read
      CHECK (NOT ('write' = ANY (rights)) OR 'read' = ANY (rights));
  END IF;
END $$;

CREATE OR REPLACE FUNCTION tree_path_write_granted(p_path ltree, p_user text)
RETURNS boolean
LANGUAGE sql
STABLE
AS $fn$
  SELECT EXISTS (
    SELECT 1 FROM data_grants g
     WHERE g.revoked_at IS NULL
       AND g.prefix @> p_path
       AND 'write' = ANY (g.rights)
       AND (g.grantee = 'anonymous'
            OR (g.grantee = 'authenticated'
                AND COALESCE(p_user, '') NOT IN ('', 'anonymous'))
            OR (g.grantee = COALESCE(p_user, '')
                AND COALESCE(p_user, '') NOT IN ('', 'anonymous',
                                                 'authenticated'))))
$fn$;

COMMENT ON FUNCTION tree_path_write_granted(ltree, text) IS
  'sharing-grants: a live grant with the write right covers p_path for p_user. Joins the write floor (pp_node_visible) and Crud.visible_sql (052).';

-- The 049 floor, restated with the write-grant disjunct last.
CREATE OR REPLACE FUNCTION pp_node_visible(
  p_path ltree, p_kind text DEFAULT 'datum')
RETURNS boolean
LANGUAGE sql
STABLE
AS $fn$
  SELECT p_kind <> 'datum'
      OR COALESCE(current_setting('pp.user', true), '') = ''
      OR p_path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
      OR p_path <@ 'data.shared'::ltree
      OR (p_path <@ 'data.l'::ltree
          AND subltree(p_path, 3, 4) = tree_enc_label(current_setting('pp.user', true))::ltree)
      OR tree_path_in_member_space(p_path, current_setting('pp.user', true))
      OR tree_path_write_granted(p_path, current_setting('pp.user', true))
$fn$;

COMMENT ON FUNCTION pp_node_visible(ltree, text) IS
  'tree_nodes floor: may the current pp.user see and write this row. Called by both RLS policies (USING and WITH CHECK) and by the read wrappers. 052 adds live write grants.';

-- 2. The default grant, kept in step with users.parent_user_id.
CREATE OR REPLACE FUNCTION agent_owner_default_prefix(p_agent_id uuid)
RETURNS ltree
LANGUAGE sql
STABLE
AS $fn$
  SELECT ('data.u.' || tree_enc_label(o.username))::ltree
    FROM users a JOIN users o ON o.id = a.parent_user_id
   WHERE a.id = p_agent_id AND a.user_type = 'agent'
$fn$;

CREATE OR REPLACE FUNCTION agent_owner_grant_sync() RETURNS trigger
LANGUAGE plpgsql AS $fn$
DECLARE
  want ltree;
BEGIN
  IF TG_OP = 'DELETE' THEN
    UPDATE data_grants SET revoked_at = now()
     WHERE granted_by = 'system:agent-owner' AND grantee = OLD.username
       AND revoked_at IS NULL;
    RETURN NULL;
  END IF;

  IF TG_OP = 'UPDATE'
     AND NEW.username IS NOT DISTINCT FROM OLD.username
     AND NEW.user_type IS NOT DISTINCT FROM OLD.user_type
     AND NEW.parent_user_id IS NOT DISTINCT FROM OLD.parent_user_id THEN
    RETURN NULL;
  END IF;

  want := agent_owner_default_prefix(NEW.id);

  -- Retire defaults that no longer match the relation (old name, old owner,
  -- or no owner at all).
  UPDATE data_grants SET revoked_at = now()
   WHERE granted_by = 'system:agent-owner'
     AND grantee IN (NEW.username,
                     CASE WHEN TG_OP = 'UPDATE' THEN OLD.username END)
     AND revoked_at IS NULL
     AND (want IS NULL OR grantee <> NEW.username OR prefix <> want);

  IF want IS NOT NULL AND NOT EXISTS (
       SELECT 1 FROM data_grants
        WHERE granted_by = 'system:agent-owner' AND grantee = NEW.username
          AND prefix = want AND revoked_at IS NULL) THEN
    INSERT INTO data_grants (prefix, grantee, rights, granted_by)
    VALUES (want, NEW.username, ARRAY['read','write'], 'system:agent-owner');
  END IF;
  RETURN NULL;
END
$fn$;

DROP TRIGGER IF EXISTS agent_owner_grant_sync ON users;
CREATE TRIGGER agent_owner_grant_sync
  AFTER INSERT OR UPDATE OF username, user_type, parent_user_id OR DELETE
  ON users
  FOR EACH ROW EXECUTE FUNCTION agent_owner_grant_sync();

-- 4. Backfill, never resurrecting a revoked default.
INSERT INTO data_grants (prefix, grantee, rights, granted_by)
SELECT agent_owner_default_prefix(a.id), a.username,
       ARRAY['read','write'], 'system:agent-owner'
  FROM users a
 WHERE a.user_type = 'agent' AND a.parent_user_id IS NOT NULL
   AND agent_owner_default_prefix(a.id) IS NOT NULL
   AND NOT EXISTS (
     SELECT 1 FROM data_grants g
      WHERE g.granted_by = 'system:agent-owner' AND g.grantee = a.username
        AND g.prefix = agent_owner_default_prefix(a.id));

-- The read wrappers run pp_node_visible as pp_rls_definer; the equivalence
-- probe evaluates it too (see 051 and tests/sql/rls_definer_role.sql).
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_definer') THEN
    GRANT EXECUTE ON FUNCTION tree_path_write_granted(ltree, text) TO pp_rls_definer;
  END IF;
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_probe') THEN
    GRANT EXECUTE ON FUNCTION tree_path_write_granted(ltree, text) TO pp_rls_probe;
  END IF;
END $$;

INSERT INTO schema_migrations (version, description)
SELECT 52, 'sharing-grants step 3: agent owner default grant (read+write on data.u.<owner>, trigger on users) + write grants on the write floor'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 52);
