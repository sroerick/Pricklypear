-- Pricklypear migration 034: row-level space routing + the shared Board.
--
-- Spec: borge/workstreams.borg shared-register (2026-10-02 spec batch).
--
-- Type-level admission (029) is too coarse for the Board: admitting
-- todo-item would move EVERY todo item on the box. This migration adds
-- ROW-LEVEL routing: a datum whose body carries "space":"<name>" lands in
-- data.s.<name>.<type>.<id> regardless of type admission. Reads/writes stay
-- governed by the existing member/open-space predicate (030), so an OPEN
-- space is readable/writable by every authenticated identity and denied to
-- anonymous.
--
-- What this migration does:
--   1. Ensures an OPEN 'board' space (owner 'system'). Open = implicit
--      membership for every authenticated identity (030), registered nor not.
--   2. tree_datum_path gains an optional space_name arg. Lookup order:
--      seed -> lens -> explicit row-level space -> admitted-type space -> user.
--      The 4-arg signature is dropped and recreated as 5-arg-with-default,
--      exactly as 023 did for the lens arg, so every existing 4-arg caller
--      recompiles against the new body.
--   3. tree_mirror_data_rows routes through the row's "space" body field,
--      falling back to the row's EXISTING space when the body omits it, so a
--      body-edit that forgets the field cannot silently move a row back to a
--      garden.
--   4. MOVES the existing 'Board' todo-list + its todo-items from
--      data.u.<owner>.* into data.s.board.*, PRESERVING ids (the last path
--      label is the id) and stamping "space":"board" into their bodies.
--
-- STRICTLY ADDITIVE + idempotent + psql-applied (schema_migrations gate).

-- ---------------------------------------------------------------------------
-- 1. The open Board space.
-- ---------------------------------------------------------------------------
INSERT INTO data_spaces (name, owner, open)
VALUES ('board', 'system', true)
ON CONFLICT (name) DO UPDATE SET open = true;

-- ---------------------------------------------------------------------------
-- 2. tree_datum_path: add optional space_name (row-level routing).
-- Drop the 4-arg signature first: CREATE OR REPLACE with a different arg
-- count creates an overload, and a defaulted 5-arg + explicit 4-arg would be
-- ambiguous. All existing 4-arg callers resolve to the 5-arg default.
-- ---------------------------------------------------------------------------
DO $$ BEGIN
  DROP FUNCTION tree_datum_path(text, uuid, text, text);
EXCEPTION WHEN undefined_function THEN NULL;
END $$;
CREATE OR REPLACE FUNCTION tree_datum_path(type_name text, id uuid, author text,
                                            lens_name text,
                                            space_name text DEFAULT '')
RETURNS ltree
LANGUAGE plpgsql STABLE AS $$
DECLARE
  a text;
  sp text;
BEGIN
  IF tree_is_seed_type(type_name) THEN
    RETURN ('data.shared.' || tree_enc_label(type_name) || '.' ||
            tree_uuid_label(id))::ltree;
  END IF;
  a := COALESCE(NULLIF(author, ''), 'unknown');
  IF lens_name IS NOT NULL AND lens_name <> '' THEN
    RETURN ('data.l.' || tree_enc_label(lens_name) || '.' ||
            tree_enc_label(a) || '.' ||
            tree_enc_label(type_name) || '.' ||
            tree_uuid_label(id))::ltree;
  END IF;
  IF space_name IS NOT NULL AND space_name <> '' THEN
    RETURN ('data.s.' || tree_enc_label(space_name) || '.' ||
            tree_enc_label(type_name) || '.' ||
            tree_uuid_label(id))::ltree;
  END IF;
  SELECT t.space INTO sp FROM data_space_types t
   WHERE t.type_name = tree_datum_path.type_name;
  IF sp IS NOT NULL THEN
    RETURN ('data.s.' || tree_enc_label(sp) || '.' ||
            tree_enc_label(type_name) || '.' ||
            tree_uuid_label(id))::ltree;
  END IF;
  RETURN ('data.u.' || tree_enc_label(a) || '.' ||
          tree_enc_label(type_name) || '.' ||
          tree_uuid_label(id))::ltree;
END $$;

-- ---------------------------------------------------------------------------
-- 3. tree_mirror_data_rows: honor the row's "space" body field, with a
-- fallback to the row's existing space so a body-edit that drops the field
-- cannot silently relocate a shared row into a garden (2026-10-02 vanish).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_mirror_data_rows() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb; sp text;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE kind = 'datum'
       AND tree_datum_id(path) = OLD.id;
    RETURN OLD;
  END IF;
  sp := COALESCE(NULLIF(NEW.fields->>'space',''),
                 (SELECT tree_dec_label(subltree(n.path,2,3)::text)
                    FROM tree_nodes n
                   WHERE n.kind = 'datum'
                     AND tree_datum_id(n.path) = NEW.id
                     AND n.path <@ 'data.s'::ltree
                   LIMIT 1),
                 '');
  p := tree_datum_path(NEW.type_name, NEW.id, NEW.author, '', sp);
  body := NEW.fields;
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'datum', 'path', p::text, 'type', NEW.type_name,
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h);
    -- Purge stale same-id datum nodes diverged from the live path p, but NEVER
    -- data.l.* lens copies: a live write must not delete the user's forked
    -- snapshot (merge/abandon own the lens lifecycle).
    DELETE FROM tree_nodes
     WHERE kind = 'datum'
       AND tree_datum_id(path) = NEW.id
       AND path <> p
       AND NOT (path <@ 'data.l'::ltree);
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'datum', NEW.type_name, env, h, NEW.author, now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, type_name = EXCLUDED.type_name,
        updated_at = now();
  RETURN NEW;
END
$$;

-- ---------------------------------------------------------------------------
-- 4. Move the existing Board list + its cards into data.s.board, ids kept.
-- pp.tree_mirror suppresses the reverse mirror while we rewrite paths; the
-- data_rows fields are stamped explicitly so the row-level route stays put.
-- ---------------------------------------------------------------------------
DO $$
DECLARE
  r RECORD;
  b_id uuid;
  b_id_txt text;
  new_p ltree;
  moved_list int := 0;
  moved_cards int := 0;
BEGIN
  PERFORM set_config('pp.tree_mirror', '1', true);
  SELECT tree_datum_id(path) INTO b_id
    FROM tree_nodes
   WHERE kind = 'datum' AND type_name = 'todo-list'
     AND payload->'body'->>'title' = 'Board'
     AND path <@ 'data.u'::ltree
   ORDER BY updated_at DESC
   LIMIT 1;
  IF b_id IS NULL THEN
    RAISE NOTICE '034: no garden Board list found; nothing to move';
    RETURN;
  END IF;
  b_id_txt := b_id::text;

  FOR r IN
    SELECT path, type_name FROM tree_nodes
     WHERE kind = 'datum' AND tree_datum_id(path) = b_id
  LOOP
    new_p := ('data.s.board.' || tree_enc_label(r.type_name) || '.' ||
              tree_uuid_label(b_id))::ltree;
    IF r.path <> new_p THEN
      DELETE FROM tree_nodes WHERE path = new_p AND path <> r.path;
      UPDATE tree_nodes
         SET path = new_p,
             payload = jsonb_set(
               jsonb_set(payload, '{path}', to_jsonb(new_p::text)),
               '{body,space}', to_jsonb('board'::text)),
             updated_at = now()
       WHERE path = r.path;
      moved_list := moved_list + 1;
    END IF;
  END LOOP;

  FOR r IN
    SELECT path, type_name FROM tree_nodes
     WHERE kind = 'datum' AND type_name = 'todo-item'
       AND payload->'body'->>'list_id' = b_id_txt
       AND path <@ 'data.u'::ltree
  LOOP
    new_p := ('data.s.board.' || tree_enc_label(r.type_name) || '.' ||
              tree_uuid_label(tree_datum_id(r.path)))::ltree;
    DELETE FROM tree_nodes WHERE path = new_p AND path <> r.path;
    UPDATE tree_nodes
       SET path = new_p,
           payload = jsonb_set(
             jsonb_set(payload, '{path}', to_jsonb(new_p::text)),
             '{body,space}', to_jsonb('board'::text)),
           updated_at = now()
     WHERE path = r.path;
    moved_cards := moved_cards + 1;
  END LOOP;

  UPDATE data_rows
     SET fields = jsonb_set(fields, '{space}', to_jsonb('board'::text)),
         updated_at = now()
   WHERE id = b_id
      OR (type_name = 'todo-item' AND fields->>'list_id' = b_id_txt);

  RAISE NOTICE '034: moved Board list % (% cards) into data.s.board',
    b_id, moved_cards;
END $$;

INSERT INTO schema_migrations (version, description)
SELECT 34, 'row-level space routing (body "space") + shared open Board space; Board moved with ids'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 34);
