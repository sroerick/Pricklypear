-- tests/sql/rls_definer_role.sql — superuser provisioning for the perf-1104
-- read wrappers, and for the equivalence test's RLS probe.
--
-- WHY: migration 013 sets FORCE ROW LEVEL SECURITY on tree_nodes, so the
-- table owner is subject to the policies too (superusers and BYPASSRLS roles
-- are the only exceptions). A SECURITY DEFINER wrapper owned by the ordinary
-- app role is therefore still filtered: correct, still a seq scan. The
-- wrappers need an owner that bypasses RLS.
--
-- Run once per database as a superuser (or a role with CREATEROLE + BYPASSRLS
-- rights). Idempotent. The migration's ownership block is what actually hands
-- the functions over; this file creates the role and its grants first.
--
--   psql -v ON_ERROR_STOP=1 -f tests/sql/rls_definer_role.sql

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_definer') THEN
    CREATE ROLE pp_rls_definer NOLOGIN BYPASSRLS;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_probe') THEN
    -- No BYPASSRLS on purpose: this is the role the equivalence test runs the
    -- RLS-enforced leg as.
    CREATE ROLE pp_rls_probe NOLOGIN;
  END IF;
END $$;

GRANT USAGE ON SCHEMA public TO pp_rls_definer, pp_rls_probe;

-- The wrappers read tree_nodes and call these two helpers.
GRANT SELECT ON tree_nodes TO pp_rls_definer, pp_rls_probe;
-- The probe leg of the equivalence test evaluates the POLICY, not just reads
-- tree_nodes: pp_node_visible calls tree_path_in_member_space, which reads
-- data_space_members. Granting the probe membership metadata does not touch the
-- floor (that is tree_nodes, and the probe has no BYPASSRLS); without it the
-- reference query cannot compute its own expectations and the test proves
-- nothing.
GRANT SELECT ON data_spaces, data_space_members TO pp_rls_definer, pp_rls_probe;
GRANT EXECUTE ON FUNCTION tree_enc_label(text),
                        tree_path_in_member_space(ltree, text)
  TO pp_rls_definer, pp_rls_probe;

-- Hand the wrappers over (the migration does this itself, this is the
-- re-run path for a database that applied 049 before the role existed).
DO $$
BEGIN
  IF to_regprocedure('pp_node_get(ltree)') IS NOT NULL THEN
    ALTER FUNCTION pp_node_get(ltree) OWNER TO pp_rls_definer;
    ALTER FUNCTION pp_node_list(ltree, text, int, int) OWNER TO pp_rls_definer;
    ALTER FUNCTION pp_node_find_by(ltree, text, text, text, text, ltree, int)
      OWNER TO pp_rls_definer;
    RAISE NOTICE 'read wrappers owned by pp_rls_definer';
  ELSE
    RAISE NOTICE 'pp_node_get absent: run migration 049 first, then re-run this file';
  END IF;
END $$;
