-- tests/sql/rls_equivalence.sql — the security floor of perf 1104.
--
-- For every user x path case it asserts that the SECURITY DEFINER read
-- wrapper returns exactly what the SAME query returns with RLS enforced as
-- that user. If the wrappers ever see more than the policy allows, this
-- fails. Run it against a scratch database:
--
--   ksh tests/sql/run-rls-equivalence.sh          (creates + drops the DB)
--   psql -v ON_ERROR_STOP=1 -f tests/sql/rls_equivalence.sql   (existing DB)
--
-- Fixtures cover: own garden, other garden, data.shared, a closed member
-- space (alice only), an encrypted note garden, a lens, and a code path
-- (kind <> 'datum', which the policy leaves open by design).

\set ON_ERROR_STOP on
BEGIN;

-- ---------------------------------------------------------------------------
-- Fixtures. Inserted as the migration role with pp.user UNSET: the policy's
-- `pp.user = ''` disjunct is the system/mirror lane and accepts them.
-- ---------------------------------------------------------------------------
SELECT set_config('pp.user', '', true);

DO $$
DECLARE
  alice text := tree_enc_label('alice');
  bob   text := tree_enc_label('bob');
  sp    text := tree_enc_label('closed-space');
  lens  text := tree_enc_label('lens-one');
BEGIN
  INSERT INTO data_spaces (name, open, created_by)
    VALUES ('closed-space', false, 'alice')
    ON CONFLICT (name) DO NOTHING;
  INSERT INTO data_space_members (space, username)
    VALUES ('closed-space', 'alice')
    ON CONFLICT DO NOTHING;

  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author) VALUES
    (('data.u.' || alice || '.todo-item.a1')::ltree, 'datum', 'todo-item',
     jsonb_build_object('body', jsonb_build_object('title','alice own')), 'h1', 'alice'),
    (('data.u.' || bob || '.todo-item.b1')::ltree, 'datum', 'todo-item',
     jsonb_build_object('body', jsonb_build_object('title','bob own')), 'h2', 'bob'),
    ('data.shared.todo-item.s1'::ltree, 'datum', 'todo-item',
     jsonb_build_object('body', jsonb_build_object('title','shared')), 'h3', 'alice'),
    (('data.s.' || sp || '.todo-item.p1')::ltree, 'datum', 'todo-item',
     jsonb_build_object('body', jsonb_build_object('title','member space')), 'h4', 'alice'),
    (('data.note.u.' || alice || '.note.n1')::ltree, 'note', 'note',
     jsonb_build_object('body', jsonb_build_object('slug','enc-note')), 'h5', 'alice'),
    (('data.l.' || lens || '.' || alice || '.todo-item.l1')::ltree, 'datum', 'todo-item',
     jsonb_build_object('body', jsonb_build_object('title','lens')), 'h6', 'alice'),
    ('b.0123456789abcdef.fn.demo'::ltree, 'fn', 'fn',
     jsonb_build_object('body', jsonb_build_object('name','demo')), 'h7', 'alice')
  ON CONFLICT (path) DO NOTHING;
END $$;

-- ---------------------------------------------------------------------------
-- Equivalence. Ran as pp_rls_probe: no BYPASSRLS, so the reference queries
-- below are filtered by the real policies.
-- ---------------------------------------------------------------------------
SET LOCAL ROLE pp_rls_probe;

DO $$
DECLARE
  users  text[] := ARRAY['alice', 'bob', 'carol'];
  u      text;
  enc    text;
  exp    text[];
  act    text[];
  cases  int := 0;
  bad    int := 0;
  msg    text := '';
  paths  text[];
  p      text;
  prefixes text[];
  pf     text;
BEGIN
  FOREACH u IN ARRAY users LOOP
    PERFORM set_config('pp.user', u, true);
    enc := tree_enc_label(u);

    -- 1. path equality: own garden, other garden, shared, member space,
    --    note, lens, code path.
    paths := ARRAY[
      'data.u.' || enc || '.todo-item.a1',
      'data.u.' || enc || '.todo-item.b1',
      'data.shared.todo-item.s1',
      'data.s.' || tree_enc_label('closed-space') || '.todo-item.p1',
      'data.note.u.' || enc || '.note.n1',
      'data.l.' || tree_enc_label('lens-one') || '.' || enc || '.todo-item.l1',
      'b.0123456789abcdef.fn.demo',
      'data.u.' || tree_enc_label('carol') || '.todo-item.none' ];
    FOREACH p IN ARRAY paths LOOP
      exp := ARRAY(SELECT path::text FROM tree_nodes
                    WHERE path = p::ltree AND pp_node_visible(path, kind)
                    ORDER BY 1);
      act := ARRAY(SELECT path::text FROM pp_node_get(p::ltree) ORDER BY 1);
      cases := cases + 1;
      IF exp IS DISTINCT FROM act THEN
        bad := bad + 1;
        msg := msg || format(E'\n  get user=%s path=%s rls=%s wrapper=%s',
                             u, p, exp, act);
      END IF;
    END LOOP;

    -- 2. containment list (and the depth=1 children shape).
    prefixes := ARRAY['data.u', 'data.shared', 'data.s', 'data.note.u',
                      'data.l', 'b'];
    FOREACH pf IN ARRAY prefixes LOOP
      exp := ARRAY(SELECT path::text FROM tree_nodes
                    WHERE path <@ pf::ltree AND pp_node_visible(path, kind)
                    ORDER BY rank COLLATE "C" NULLS LAST, path::text LIMIT 1000);
      act := ARRAY(SELECT path::text FROM pp_node_list(pf::ltree, NULL, 1000));
      cases := cases + 1;
      IF exp IS DISTINCT FROM act THEN
        bad := bad + 1;
        msg := msg || format(E'\n  list user=%s prefix=%s rls=%s wrapper=%s',
                             u, pf, exp, act);
      END IF;
      exp := ARRAY(SELECT path::text FROM tree_nodes n
                    WHERE n.path <@ pf::ltree AND n.path <> pf::ltree
                      AND (nlevel(n.path) = nlevel(pf::ltree) + 1
                           OR (nlevel(n.path) = nlevel(pf::ltree) + 2
                               AND subpath(n.path, nlevel(pf::ltree), 1)::text = 'h'))
                      AND pp_node_visible(n.path, n.kind)
                    ORDER BY n.rank COLLATE "C" NULLS LAST, n.path::text LIMIT 1000);
      act := ARRAY(SELECT path::text FROM pp_node_list(pf::ltree, NULL, 1000, 1));
      cases := cases + 1;
      IF exp IS DISTINCT FROM act THEN
        bad := bad + 1;
        msg := msg || format(E'\n  children user=%s prefix=%s rls=%s wrapper=%s',
                             u, pf, exp, act);
      END IF;
    END LOOP;

    -- 3. find-by on a body field (RLS floor only: no viewer, so the wrapper
    --    applies no note vis floor either).
    exp := ARRAY(SELECT path::text FROM tree_nodes
                  WHERE kind = 'note' AND payload #>> '{body,slug}' = 'enc-note'
                    AND path <@ ('data.note.u.' || enc)::ltree
                    AND pp_node_visible(path, kind)
                  ORDER BY path::text LIMIT 50);
    act := ARRAY(SELECT path::text
                   FROM pp_node_find_by(('data.note.u.' || enc)::ltree,
                                        'slug', 'enc-note', 'note',
                                        NULL, NULL, 50)
                  ORDER BY 1);
    cases := cases + 1;
    IF exp IS DISTINCT FROM act THEN
      bad := bad + 1;
      msg := msg || format(E'\n  find_by user=%s rls=%s wrapper=%s', u, exp, act);
    END IF;
  END LOOP;

  IF bad > 0 THEN
    RAISE EXCEPTION 'RLS EQUIVALENCE FAILED: % of % cases differ%s', bad, cases, msg;
  END IF;
  RAISE NOTICE 'RLS EQUIVALENCE OK: % cases, % users', cases, array_length(users, 1);
END $$;

RESET ROLE;
ROLLBACK;
