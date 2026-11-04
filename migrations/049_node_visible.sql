-- 049_node_visible.sql — ONE visibility predicate, both tree_nodes policies
-- rewritten to call it, and three SECURITY DEFINER read wrappers that reach
-- the indexes the RLS floor hides today (perf 1104, roerick's option B: keep
-- the database floor, make the hot reads index-friendly).
--
-- WHY (measured 2026-11-04, prod): tree_nodes is 259MB with two permissive
-- RLS policies (datum_user_isolation 029, note_user_isolation 041). Every
-- read seq-scans: point read 1851ms / 14323 buffers, containment 1495ms.
-- VACUUM (ANALYZE) changed nothing and enable_seqscan=off found no index
-- candidate, because the policy quals are security-barrier quals and the
-- ltree operators are not LEAKPROOF: a non-leakproof qual cannot be used as
-- an index qual while the relation carries security quals. Inside a function
-- that runs with RLS bypassed there are no security quals, so `path = $1`
-- (btree, 048) and `path <@ $1` (GiST, 009) are ordinary index quals again.
--
-- THE FLOOR IS UNCHANGED. The policies keep exactly their old meaning and
-- every wrapper re-applies the SAME predicate to every row it returns, so a
-- caller cannot see a row the policy would have hidden. Policy and wrappers
-- call one function; they cannot drift.
--
-- OWNERSHIP (needed for the speed, not for correctness): 013 sets FORCE ROW
-- LEVEL SECURITY, so the table owner is subject to the policies too. A
-- SECURITY DEFINER function owned by the ordinary app role is therefore
-- still filtered: correct, still a seq scan. The wrappers must be owned by a
-- role that bypasses RLS. This file uses pp_rls_definer when that role
-- exists AND can read tree_nodes; otherwise it leaves the wrappers owned by
-- whoever applied the migration and says so. tests/sql/rls_definer_role.sql
-- is the superuser step that provisions the role and the ownership.
--
-- STRICTLY ADDITIVE except the two policy expressions, which are replaced by
-- an equivalent call. Idempotent.

-- ---------------------------------------------------------------------------
-- 0. The wrappers set search_path = pg_catalog, public (so the pg_catalog
--    functions they use cannot be shadowed). Everything they read must
--    therefore be in public; fail loudly here rather than at runtime.
-- ---------------------------------------------------------------------------
DO $$
BEGIN
  IF to_regclass('public.tree_nodes') IS NULL THEN
    RAISE EXCEPTION
      '1104: tree_nodes is not in schema public; the wrapper search_path (pg_catalog, public) has to be adjusted for this database';
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 1. The predicate.
--
--    Exact union of the two permissive policies as they stand after 029+041:
--
--    datum_user_isolation (029): kind <> 'datum' OR pp.user = '' OR garden
--      OR data.shared OR (data.l with the caller's label) OR member space.
--    note_user_isolation (041): after reduction its two note-kind disjuncts
--      are (A AND NOT B) OR (A AND B) = A, so it is
--      kind IN ('note','folder','heading') OR pp.user = '' — already covered
--      by the datum policy's kind <> 'datum'. A permissive policy set ORs its
--      policies, so the effective floor is the datum expression alone.
--
--    p_kind defaults to 'datum' (strictest case), so pp_node_visible(path)
--    answers "may the caller see a datum row at this path".
--
--    No SET search_path here on purpose: a SQL function with a SET clause is
--    not inlined into the calling query, and this one is also called from the
--    policy on every row of the queries that still take the RLS path.
-- ---------------------------------------------------------------------------
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
$fn$;

COMMENT ON FUNCTION pp_node_visible(ltree, text) IS
  'tree_nodes read floor: may the current pp.user see this row. Called by both RLS policies and by the read wrappers so they cannot drift.';

-- ---------------------------------------------------------------------------
-- 2. Both policies call the predicate. Same expression, so the union is the
--    same expression the policies had before.
-- ---------------------------------------------------------------------------
DROP POLICY IF EXISTS datum_user_isolation ON tree_nodes;
CREATE POLICY datum_user_isolation ON tree_nodes
  USING (pp_node_visible(path, kind))
  WITH CHECK (pp_node_visible(path, kind));

DROP POLICY IF EXISTS note_user_isolation ON tree_nodes;
CREATE POLICY note_user_isolation ON tree_nodes
  USING (pp_node_visible(path, kind))
  WITH CHECK (pp_node_visible(path, kind));

-- ---------------------------------------------------------------------------
-- 3. Read wrappers. Each one is the shape the store already asks:
--
--    pp_node_get     path equality          -> btree tree_nodes_path_btree
--    pp_node_list    containment (subtree)  -> GiST tree_nodes_path_gist
--                    + direct children (p_depth = 1), the shape tree/list
--                    uses with depth 1 (todo/, inbox/, standup/)
--    pp_node_find_by body field + value     -> 010 partial expression index
--
--    The visibility check runs on the rows the index returned, and LIMIT is
--    in SQL (after the check, so a cap counts visible rows, exactly like the
--    RLS-enforced query it replaces).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION pp_node_get(p_path ltree)
RETURNS SETOF tree_nodes
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = pg_catalog, public
AS $fn$
  SELECT n.* FROM tree_nodes n
   WHERE n.path = p_path
     AND pp_node_visible(n.path, n.kind)
$fn$;

CREATE OR REPLACE FUNCTION pp_node_list(
  p_prefix ltree, p_kind text DEFAULT NULL, p_cap int DEFAULT 1000,
  p_depth int DEFAULT NULL)
RETURNS SETOF tree_nodes
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = pg_catalog, public
AS $fn$
  SELECT n.* FROM tree_nodes n
   WHERE n.path <@ p_prefix
     AND (p_depth IS DISTINCT FROM 1
          OR (n.path <> p_prefix
              AND (nlevel(n.path) = nlevel(p_prefix) + 1
                   OR (nlevel(n.path) = nlevel(p_prefix) + 2
                       AND subpath(n.path, nlevel(p_prefix), 1)::text = 'h'))))
     AND (p_kind IS NULL OR n.kind = p_kind)
     AND pp_node_visible(n.path, n.kind)
   ORDER BY n.rank COLLATE "C" NULLS LAST, n.path::text ASC
   LIMIT COALESCE(p_cap, 1000)
$fn$;

CREATE OR REPLACE FUNCTION pp_node_find_by(
  p_prefix ltree, p_field text, p_value text,
  p_kind text DEFAULT NULL, p_viewer text DEFAULT NULL,
  p_garden ltree DEFAULT NULL, p_cap int DEFAULT 50)
RETURNS SETOF tree_nodes
LANGUAGE plpgsql
STABLE
SECURITY DEFINER
SET search_path = pg_catalog, public
AS $fn$
DECLARE
  v_sql text;
BEGIN
  -- The body field is spliced into the payload path literal, not bound as a
  -- parameter: a parameterized key defeats the migration-010 partial
  -- expression indexes (card 872d8066). Identifier shape only, the same rule
  -- the image-side find-by enforces.
  IF p_field IS NULL OR p_field !~ '^[A-Za-z0-9_]+$' THEN
    RETURN;
  END IF;
  -- p_viewer / p_garden carry the note vis floor the pre-1104 find-by applied
  -- in its own SQL (rows outside the viewer's garden must carry body vis
  -- users or public). It is not part of the RLS floor; it is repeated here so
  -- the wrapped read returns the same rows, cap included.
  v_sql :=
    'SELECT n.* FROM tree_nodes n WHERE n.payload #>> '
    || quote_literal('{body,' || p_field || '}') || ' = $1 '
    || 'AND ($2::ltree IS NULL OR n.path <@ $2::ltree) '
    || 'AND ($3::text IS NULL OR n.kind = $3::text) '
    || 'AND ($4::text IS NULL '
    || '     OR COALESCE(n.path <@ $5::ltree, false) '
    || '     OR COALESCE(n.payload #>> ''{body,vis}'', ''private'') = ''public'' '
    || '     OR (COALESCE(n.payload #>> ''{body,vis}'', ''private'') = ''users'' '
    || '         AND $4::text <> ''anonymous'')) '
    || 'AND pp_node_visible(n.path, n.kind) '
    || 'ORDER BY n.path::text ASC LIMIT COALESCE($6::int, 50)';
  RETURN QUERY EXECUTE v_sql
    USING p_value, p_prefix, p_kind, p_viewer, p_garden, p_cap;
END
$fn$;

COMMENT ON FUNCTION pp_node_find_by(ltree, text, text, text, text, ltree, int) IS
  'tree/find-by read: body field = value under a prefix, RLS floor + optional note vis floor applied to returned rows, LIMIT in SQL.';

-- ---------------------------------------------------------------------------
-- 4. Ownership. FORCE RLS (013) makes the table owner subject too, so the
--    wrappers are only faster when owned by a role that bypasses RLS.
--    Provisioned by tests/sql/rls_definer_role.sql (superuser).
-- ---------------------------------------------------------------------------
DO $$
DECLARE
  v_role text := 'pp_rls_definer';
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = v_role)
     AND has_table_privilege(v_role, 'tree_nodes', 'SELECT') THEN
    BEGIN
      EXECUTE format('ALTER FUNCTION pp_node_get(ltree) OWNER TO %I', v_role);
      EXECUTE format('ALTER FUNCTION pp_node_list(ltree,text,int,int) OWNER TO %I', v_role);
      EXECUTE format('ALTER FUNCTION pp_node_find_by(ltree,text,text,text,text,ltree,int) OWNER TO %I', v_role);
      RAISE NOTICE '1104: read wrappers owned by % (RLS bypassed inside, pp_node_visible applied per row)', v_role;
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING '1104: could not hand the read wrappers to % (%); they stay owned by % and remain RLS-filtered', v_role, SQLERRM, CURRENT_USER;
    END;
  ELSE
    RAISE WARNING '1104: % missing or without SELECT on tree_nodes. Wrappers stay owned by % and stay RLS-filtered: correct, still a seq scan. Run tests/sql/rls_definer_role.sql as superuser, then re-run this ownership block.', v_role, CURRENT_USER;
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 5. Operator-facing state. One select answers "are the wrappers actually
--    fast on this database".
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION pp_rls_wrappers_mode()
RETURNS text
LANGUAGE sql
STABLE
AS $fn$
  SELECT format(
           'force_rls=%s wrapper_owner=%s bypassrls=%s can_read=%s index_seek=%s',
           c.relforcerowsecurity,
           r.rolname,
           r.rolbypassrls OR r.rolsuper,
           has_table_privilege(r.rolname, 'tree_nodes', 'SELECT'),
           (r.rolbypassrls OR r.rolsuper)
             AND has_table_privilege(r.rolname, 'tree_nodes', 'SELECT'))
    FROM pg_class c
    JOIN pg_roles r
      ON r.rolname = (SELECT pg_get_userbyid(p.proowner)
                        FROM pg_proc p
                       WHERE p.oid = 'pp_node_get(ltree)'::regprocedure)
   WHERE c.oid = 'tree_nodes'::regclass
$fn$;

INSERT INTO schema_migrations (version, description)
SELECT 49, 'node visibility predicate pp_node_visible + RLS policies call it + SECURITY DEFINER read wrappers pp_node_get/list/find_by (perf 1104)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 49);
