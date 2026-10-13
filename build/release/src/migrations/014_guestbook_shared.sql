-- Pricklypear migration 014: guestbook_entry is shared corpus.
--
-- Follow-up to 013 / BORGE-1785695889. Guestbook is a public wall: every
-- visitor must see every entry. Under user-scoped data, rows authored by
-- with-public-write (author='public') landed under data.u.public.* and
-- were invisible to other sessions. Treat guestbook_entry like bible seed:
-- path data.shared.guestbook_entry.<id>, visible via tree_is_seed_type /
-- CRUD seed allowlist.

CREATE OR REPLACE FUNCTION tree_is_seed_type(t text) RETURNS boolean
LANGUAGE sql IMMUTABLE AS $$
  SELECT t IN (
    'bible-verse',
    'bible-book',
    'bible-interlinear',
    'strongs-entry',
    'guestbook_entry'
  )
$$;

-- Move any existing guestbook datum nodes onto data.shared.*
DO $$
DECLARE
  r RECORD;
  new_p ltree;
  n int := 0;
BEGIN
  PERFORM set_config('pp.tree_mirror', '1', true);
  FOR r IN
    SELECT path, type_name, author, tree_datum_id(path) AS id
      FROM tree_nodes
     WHERE kind = 'datum' AND type_name = 'guestbook_entry'
  LOOP
    IF r.id IS NULL THEN CONTINUE; END IF;
    new_p := tree_datum_path(r.type_name, r.id, r.author);
    IF r.path = new_p THEN CONTINUE; END IF;
    DELETE FROM tree_nodes WHERE path = new_p AND path <> r.path;
    UPDATE tree_nodes
       SET path = new_p,
           payload = jsonb_set(COALESCE(payload, '{}'::jsonb), '{path}',
                               to_jsonb(new_p::text)),
           updated_at = now()
     WHERE path = r.path;
    n := n + 1;
  END LOOP;
  RAISE NOTICE '014 guestbook shared backfill: moved=%', n;
END $$;

-- Repair: ensure every data_rows guestbook row has a shared tree node.
DO $$
DECLARE
  r RECORD;
  p ltree;
  body jsonb;
  h text;
  env jsonb;
  n int := 0;
BEGIN
  PERFORM set_config('pp.tree_mirror', '1', true);
  FOR r IN
    SELECT id, type_name, fields, author
      FROM data_rows
     WHERE type_name = 'guestbook_entry'
  LOOP
    p := tree_datum_path(r.type_name, r.id, r.author);
    DELETE FROM tree_nodes
     WHERE kind = 'datum' AND tree_datum_id(path) = r.id AND path <> p;
    IF EXISTS (SELECT 1 FROM tree_nodes WHERE path = p) THEN
      CONTINUE;
    END IF;
    body := r.fields;
    h := encode(digest(body::text, 'sha256'), 'hex');
    env := jsonb_build_object(
      'v', 1, 'kind', 'datum', 'path', p::text, 'type', r.type_name,
      'body', body, 'author', r.author, 'doc', NULL, 'hash', h);
    INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
    VALUES (p, 'datum', r.type_name, env, h, r.author, now())
    ON CONFLICT (path) DO UPDATE
      SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
          author = EXCLUDED.author, type_name = EXCLUDED.type_name,
          updated_at = now();
    n := n + 1;
  END LOOP;
  RAISE NOTICE '014 guestbook repair: inserted=%', n;
END $$;

INSERT INTO schema_migrations (version, description)
SELECT 14, 'guestbook_entry is shared seed corpus (public wall)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 14);
