-- tests/sql/rls_factory_role.sql — the factory seat's database scope.
--
-- WHAT THIS IS FOR
--   Three things in the perf-1104 loop are checks, not decisions:
--     * the pre-flight (what is actually applied, owned and enforced here)
--     * tests/sql/rls_equivalence.sql (the security floor; rolled back)
--     * SELECT pp_rls_wrappers_mode() after a migration
--   A check that costs the reviewing seat a wake every time it runs is a
--   check that stops being run. This file gives the executing seat those
--   three and stops there on purpose.
--
-- WHO: role `deploy`, LOGIN without a password. The local pg_hba peer/ident
--   line maps the OS account of the same name onto it, so the seat connects
--   with `psql -d <app db>` and no credential exists anywhere to leak. If the
--   seat's OS account is named differently, change the 'deploy' literals
--   below to match: psql does not substitute variables inside dollar-quoted
--   bodies, so the name is a literal on purpose.
--
-- RUN AS a superuser, once, against the app database:
--   psql -v ON_ERROR_STOP=1 -d <app db> -f tests/sql/rls_factory_role.sql
--   Idempotent: re-running re-asserts the same scope. Run it again after 049
--   to pick up EXECUTE on the wrappers; the grant block skips functions that
--   do not exist yet and says so.
--
-- THE WRITE SURFACE IS MEASURED, NOT ASSUMED
--   Section 3 was written by running the fixtures, not by reading them. An
--   INSERT into tree_nodes fires three triggers — tree_from_nodes (009),
--   tree_version_append (019/038), tree_bump_def_gen (028) — which write the
--   mirror tables, the version shadow, the two generation counters and one
--   sequence. On a fresh database the fixture dies at the first of those with
--   `permission denied for table tree_versions` unless the trigger's own
--   tables are in scope. So "INSERT on tree_nodes" never meant one table.
--   What is granted is the tables a tree write itself touches: no MORE power
--   than the tree_nodes grant already implies, and stated here rather than
--   discovered by the reviewer.
--
-- WHAT IT DELIBERATELY DOES NOT GRANT
--   * users, credentials, auth_sessions, auth_log, system_keys, and the
--     delegation/grant tables: not granted, not reachable.
--   * No DDL. 049 rewrites both policies on tree_nodes; CREATE POLICY needs
--     table ownership and the ownership block needs pp_rls_definer
--     (BYPASSRLS). Neither is a narrow grant, so the policy rewrite stays with
--     the seat that reviews it. One command per migration, and the one step
--     where a second pair of eyes is the point.
--   * No DELETE, no TRUNCATE anywhere. Version rows are append-only by design
--     (019 appends tombstones), and nothing in the checks removes rows: the
--     fixtures are rolled back.
--   * No membership in pp_rls_definer. SET ROLE there would bypass RLS on
--     tree_nodes, i.e. the whole store at once, which is exactly the boundary
--     this seat exists to stay inside.
--
-- READ CAVEAT, stated rather than implied: the RLS floor admits every row
--   while pp.user is unset ('' is the system/mirror lane; see pp_node_visible).
--   SELECT on tree_nodes is therefore a cross-garden SQL read for whoever
--   holds it. That is the reach this seat already has through the delegated
--   app token, and it does not extend to the identity tables above.
--
-- IF THE SEAT SHOULD ONLY VERIFY, NOT ITERATE: drop section 3. Pre-flight,
--   pp_rls_wrappers_mode() and the wrapper-vs-RLS comparison over rows that
--   already exist keep working; the fixture block in rls_equivalence.sql will
--   fail with permission denied, which is the correct answer for that seat.

\set ON_ERROR_STOP on

-- ---------------------------------------------------------------------------
-- 0. Guard rails. Provisioning needs a superuser (role creation, and ADMIN
--    OPTION on the probe role for the membership grant).
-- ---------------------------------------------------------------------------
DO $$
BEGIN
  IF NOT (SELECT rolsuper FROM pg_roles WHERE rolname = current_user) THEN
    RAISE EXCEPTION 'rls_factory_role.sql: run as a superuser (current_user=%)', current_user;
  END IF;
  IF to_regclass('public.tree_nodes') IS NULL THEN
    RAISE EXCEPTION 'rls_factory_role.sql: tree_nodes is not in this database; point -d at the app database';
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 1. The login. Nothing is widened here: every attribute postgres leaves off
--    a fresh role is restated, so a role that drifted cannot ride along.
-- ---------------------------------------------------------------------------
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'deploy') THEN
    CREATE ROLE deploy LOGIN;
    RAISE NOTICE 'created role: deploy (LOGIN, peer auth, no password)';
  ELSE
    RAISE NOTICE 'role deploy exists; re-asserting scope only';
  END IF;
END $$;

ALTER ROLE deploy NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;

-- ---------------------------------------------------------------------------
-- 2. Reads and verification: pre-flight, the equivalence reference legs, the
--    mode function.
-- ---------------------------------------------------------------------------
GRANT USAGE ON SCHEMA public TO deploy;

GRANT SELECT ON tree_nodes, data_spaces, data_space_members, schema_migrations
  TO deploy;

-- Helpers the fixtures and the policies call, plus the three read wrappers and
-- the state function. Conditional: 049 may not be applied yet on this database.
DO $$
DECLARE
  v_sig   text;
  v_skips text := '';
BEGIN
  FOREACH v_sig IN ARRAY ARRAY[
    'tree_enc_label(text)',
    'tree_path_in_member_space(ltree,text)',
    'pp_node_visible(ltree,text)',
    'pp_node_get(ltree)',
    'pp_node_list(ltree,text,int,int)',
    'pp_node_find_by(ltree,text,text,text,text,ltree,int)',
    'pp_rls_wrappers_mode()'
  ] LOOP
    IF to_regprocedure(v_sig) IS NULL THEN
      v_skips := v_skips || ' ' || v_sig;
      CONTINUE;
    END IF;
    EXECUTE format('GRANT EXECUTE ON FUNCTION %s TO deploy', v_sig);
  END LOOP;
  IF v_skips <> '' THEN
    RAISE NOTICE 'EXECUTE skipped (function absent, apply 049 then re-run):%', v_skips;
  END IF;
END $$;

-- The reference leg of rls_equivalence.sql runs SET LOCAL ROLE pp_rls_probe.
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_probe') THEN
    RAISE WARNING 'pp_rls_probe is absent: run tests/sql/rls_definer_role.sql first, then re-run this file';
  ELSE
    GRANT pp_rls_probe TO deploy;
    IF NOT has_table_privilege('pp_rls_probe', 'tree_nodes', 'SELECT') THEN
      RAISE WARNING 'pp_rls_probe cannot SELECT tree_nodes; the equivalence reference leg will fail. Re-run tests/sql/rls_definer_role.sql';
    END IF;
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 3. Fixture writes: exactly the tables a tree_nodes INSERT reaches through
--    its triggers, plus the sequence the data head is allocated from (038).
--    INSERT is the only verb the fixtures need; UPDATE appears only where the
--    production write path does ON CONFLICT DO UPDATE on a counter or a
--    mirror row. Conditional on existence so the file can run before 049.
-- ---------------------------------------------------------------------------
DO $$
DECLARE
  v_tbl   text;
  v_skips text := '';
BEGIN
  FOREACH v_tbl IN ARRAY ARRAY[
    'tree_nodes',         -- the fixture rows themselves
    'data_spaces',        -- fixture: the closed member space row
    'data_space_members', -- fixture: alice's membership row
    'tree_versions',      -- 019/038 version shadow (append-only)
    'function_defs',      -- 009 tree_from_nodes: kind fn
    'type_defs',          -- 009 tree_from_nodes: kind type
    'view_bindings',      -- 009 tree_from_nodes: kind route
    'data_rows',          -- 009/013 tree_from_nodes: kind datum
    'tree_generations',   -- 019/038 data.* head counter
    'branch_generations'  -- 019/038 per-branch head counter
  ] LOOP
    IF to_regclass(format('public.%I', v_tbl)) IS NULL THEN
      v_skips := v_skips || ' ' || v_tbl;
      CONTINUE;
    END IF;
    EXECUTE format('GRANT INSERT ON public.%I TO deploy', v_tbl);
  END LOOP;

  -- Read-modify-write: UPDATE reads the columns it touches, and ON CONFLICT
  -- DO UPDATE reads the stored row's expressions, so the same tables need
  -- SELECT. They are already reachable through tree_nodes (the seat reads it),
  -- so this adds no reach; it is what the trigger does to itself.
  FOREACH v_tbl IN ARRAY ARRAY[
    'tree_branches', 'tree_generations', 'branch_generations',
    'function_defs', 'type_defs', 'view_bindings', 'data_rows'
  ] LOOP
    CONTINUE WHEN to_regclass(format('public.%I', v_tbl)) IS NULL;
    EXECUTE format('GRANT SELECT ON public.%I TO deploy', v_tbl);
  END LOOP;

  -- ON CONFLICT DO UPDATE on the counters and mirrors (009, 019, 038) needs
  -- UPDATE; the definition bump (028) updates tree_branches.def_gen.
  FOREACH v_tbl IN ARRAY ARRAY[
    'tree_generations', 'branch_generations', 'tree_branches',
    'function_defs', 'type_defs', 'view_bindings', 'data_rows'
  ] LOOP
    CONTINUE WHEN to_regclass(format('public.%I', v_tbl)) IS NULL;
    EXECUTE format('GRANT UPDATE ON public.%I TO deploy', v_tbl);
  END LOOP;

  IF to_regclass('public.tree_generations_seq') IS NULL THEN
    v_skips := v_skips || ' tree_generations_seq';
  ELSE
    EXECUTE 'GRANT SELECT, USAGE ON SEQUENCE tree_generations_seq TO deploy';
  END IF;

  IF v_skips <> '' THEN
    RAISE NOTICE 'fixture-write skipped (absent on this database):%', v_skips;
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 4. Assert the parts of the scope this file decides. Anything the file does
--    not decide is reported in section 5, not enforced: a provisioning script
--    that refuses to complete because of a pre-existing database-wide grant
--    is a script nobody can run.
-- ---------------------------------------------------------------------------
DO $$
DECLARE
  r record;
BEGIN
  SELECT rolsuper, rolbypassrls, rolcreaterole, rolcreatedb, rolcanlogin
    INTO r FROM pg_roles WHERE rolname = 'deploy';
  IF r.rolsuper OR r.rolbypassrls OR r.rolcreaterole OR r.rolcreatedb THEN
    RAISE EXCEPTION 'deploy holds a role attribute outside the scope (super=% bypassrls=% createrole=% createdb=%)',
      r.rolsuper, r.rolbypassrls, r.rolcreaterole, r.rolcreatedb;
  END IF;
  IF NOT r.rolcanlogin THEN
    RAISE EXCEPTION 'deploy cannot log in; the seat connects as this role';
  END IF;
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_definer')
     AND pg_has_role('deploy', 'pp_rls_definer', 'MEMBER') THEN
    RAISE EXCEPTION 'deploy is a member of pp_rls_definer: that is BYPASSRLS, the whole store at once';
  END IF;
  RAISE NOTICE 'scope asserted: login, no superuser/createdb/createrole/bypassrls, not a member of pp_rls_definer';
END $$;

-- ---------------------------------------------------------------------------
-- 5. State report. This is the output the reviewer reads.
-- ---------------------------------------------------------------------------
SELECT format('role deploy: super=%s createdb=%s createrole=%s bypassrls=%s login=%s',
              rolsuper, rolcreatedb, rolcreaterole, rolbypassrls, rolcanlogin) AS role_state
  FROM pg_roles WHERE rolname = 'deploy';

-- Every table in public the seat can touch. The first four are reads; the
-- rest are the trigger-fed write path from section 3. A row that is neither is
-- a boundary question for the reviewer.
SELECT c.relname AS table_name,
       concat_ws(',',
         CASE WHEN has_table_privilege('deploy', c.oid, 'SELECT') THEN 'SELECT' END,
         CASE WHEN has_table_privilege('deploy', c.oid, 'INSERT') THEN 'INSERT' END,
         CASE WHEN has_table_privilege('deploy', c.oid, 'UPDATE') THEN 'UPDATE' END,
         CASE WHEN has_table_privilege('deploy', c.oid, 'DELETE') THEN 'DELETE' END,
         CASE WHEN has_table_privilege('deploy', c.oid, 'TRUNCATE') THEN 'TRUNCATE' END) AS privs,
       CASE WHEN c.oid IN ('tree_nodes'::regclass, 'data_spaces'::regclass,
                           'data_space_members'::regclass, 'schema_migrations'::regclass)
              THEN 'read'
            ELSE 'tree-write-path' END AS section
  FROM pg_class c
  JOIN pg_namespace n ON n.oid = c.relnamespace
 WHERE n.nspname = 'public'
   AND c.relkind IN ('r','p','v','m','f')
   AND has_table_privilege('deploy', c.oid, 'SELECT,INSERT,UPDATE,DELETE,TRUNCATE')
 ORDER BY 3, 1;

SELECT format('public schema CREATE for deploy: %s (no DDL in this scope)',
              has_schema_privilege('deploy', 'public', 'CREATE')) AS ddl_state;

SELECT format('pp_rls_probe membership for deploy: %s',
              EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_probe')
              AND pg_has_role('deploy', 'pp_rls_probe', 'MEMBER')) AS probe_state;

-- ---------------------------------------------------------------------------
-- Teardown, if the seat is ever retired (uncomment; not part of provisioning):
--   REVOKE ALL ON ALL TABLES IN SCHEMA public FROM deploy;
--   REVOKE ALL ON ALL SEQUENCES IN SCHEMA public FROM deploy;
--   REVOKE ALL ON SCHEMA public FROM deploy;
--   REVOKE pp_rls_probe FROM deploy;
--   DROP OWNED BY deploy;
--   DROP ROLE deploy;
-- ---------------------------------------------------------------------------
