-- Pricklypear migration 013: user-scoped datum paths + RLS floor.
--
-- Per spec [one-tree.branching-interaction] /data DECISION (2026-08-02)
-- and [one-tree.migration-plan] Phase 2b. Closes BORGE-1785693032.
--
-- Datum paths become:
--   per-user  data.u.<user_enc>.<type_enc>.<id_hex>
--   shared    data.shared.<type_enc>.<id_hex>
-- A session sees exactly its own data.u.<user>.* plus data.shared.*.
-- No branch id in datum paths. data_rows stays physically (additive-only);
-- tree_data_rows view + CRUD prims confine; RLS on tree_nodes is the floor.
--
-- Seed types (visible to every session via data.shared):
--   bible-verse, bible-book, bible-interlinear, strongs-entry
-- All other existing rows backfill under data.u.<author_enc>.* keyed off
-- data_rows.author / tree_nodes.author. Unknown/empty author -> data.u.unknown.
--
-- Notes hierarchy (kind folder|note|heading under data.note.u.<user>.*) is
-- NOT kind=datum and is exempted by the RLS kind <> 'datum' clause.

-- ---------------------------------------------------------------------------
-- Helpers: datum id is ALWAYS the last ltree label (uuid hex).
-- Works for data.<type>.<id>, data.u.<user>.<type>.<id>, data.shared.<type>.<id>.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_datum_id(p ltree) RETURNS uuid
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE
  labs text[];
  h text;
BEGIN
  labs := string_to_array(p::text, '.');
  IF labs IS NULL OR array_length(labs, 1) IS NULL THEN
    RETURN NULL;
  END IF;
  h := labs[array_length(labs, 1)];
  IF h IS NULL OR length(h) <> 32 THEN
    RETURN NULL;
  END IF;
  RETURN (substr(h, 1, 8) || '-' || substr(h, 9, 4) || '-' ||
          substr(h, 13, 4) || '-' || substr(h, 17, 4) || '-' ||
          substr(h, 21, 12))::uuid;
END $$;

-- Seed-type predicate (shared corpus). Keep list short and explicit.
CREATE OR REPLACE FUNCTION tree_is_seed_type(t text) RETURNS boolean
LANGUAGE sql IMMUTABLE AS $$
  SELECT t IN (
    'bible-verse',
    'bible-book',
    'bible-interlinear',
    'strongs-entry'
  )
$$;

-- Build the canonical datum path for (type, id, author).
CREATE OR REPLACE FUNCTION tree_datum_path(type_name text, id uuid, author text)
RETURNS ltree
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE
  a text;
BEGIN
  IF tree_is_seed_type(type_name) THEN
    RETURN ('data.shared.' || tree_enc_label(type_name) || '.' ||
            tree_uuid_label(id))::ltree;
  END IF;
  a := COALESCE(NULLIF(author, ''), 'unknown');
  RETURN ('data.u.' || tree_enc_label(a) || '.' ||
          tree_enc_label(type_name) || '.' ||
          tree_uuid_label(id))::ltree;
END $$;

-- ---------------------------------------------------------------------------
-- Forward mirror: data_rows -> tree_nodes (user/shared path roots).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_mirror_data_rows() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);
  IF (TG_OP = 'DELETE') THEN
    -- Delete by id via either old or new path shape (id is last label).
    DELETE FROM tree_nodes
     WHERE kind = 'datum'
       AND tree_datum_id(path) = OLD.id;
    RETURN OLD;
  END IF;
  p := tree_datum_path(NEW.type_name, NEW.id, NEW.author);
  body := NEW.fields;
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'datum', 'path', p::text, 'type', NEW.type_name,
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h);
  -- If the row moved path (author/type change), drop the old node first.
  DELETE FROM tree_nodes
   WHERE kind = 'datum'
     AND tree_datum_id(path) = NEW.id
     AND path <> p;
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'datum', NEW.type_name, env, h, NEW.author, now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, type_name = EXCLUDED.type_name,
        updated_at = now();
  RETURN NEW;
END $$;

-- ---------------------------------------------------------------------------
-- Reverse mirror: tree_nodes -> data_rows (id = last path label).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_from_nodes() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE bid uuid; lbl text; docv text; did uuid;
BEGIN
  IF tree_in_mirror() THEN
    IF TG_OP = 'DELETE' THEN RETURN OLD; ELSE RETURN NEW; END IF;
  END IF;
  IF (TG_OP = 'DELETE') THEN
    bid := tree_path_branch(OLD.path);
    IF OLD.kind = 'fn' THEN
      lbl := (string_to_array(OLD.path::text,'.'))[4];
      DELETE FROM function_defs WHERE branch_id=bid AND name=tree_dec_label(lbl);
    ELSIF OLD.kind = 'type' THEN
      lbl := (string_to_array(OLD.path::text,'.'))[4];
      DELETE FROM type_defs WHERE branch_id=bid AND name=tree_dec_label(lbl);
    ELSIF OLD.kind = 'route' THEN
      DELETE FROM view_bindings WHERE branch_id=bid AND route=OLD.payload->'body'->>'route';
    ELSIF OLD.kind = 'datum' THEN
      did := tree_datum_id(OLD.path);
      IF did IS NOT NULL THEN
        DELETE FROM data_rows WHERE id = did;
      END IF;
    END IF;
    RETURN OLD;
  END IF;
  docv := NEW.payload->>'doc';
  IF NEW.kind = 'fn' THEN
    bid := tree_path_branch(NEW.path);
    lbl := (string_to_array(NEW.path::text,'.'))[4];
    INSERT INTO function_defs (branch_id,name,body,author,doc)
    VALUES (bid, tree_dec_label(lbl), NEW.payload->'body',
            NEW.payload->>'author', docv)
    ON CONFLICT (branch_id,name) DO UPDATE
      SET body=EXCLUDED.body, author=EXCLUDED.author, doc=EXCLUDED.doc,
          updated_at=now();
  ELSIF NEW.kind = 'type' THEN
    bid := tree_path_branch(NEW.path);
    lbl := (string_to_array(NEW.path::text,'.'))[4];
    INSERT INTO type_defs (branch_id,name,fields,author)
    VALUES (bid, tree_dec_label(lbl), NEW.payload->'body', NEW.payload->>'author')
    ON CONFLICT (branch_id,name) DO UPDATE
      SET fields=EXCLUDED.fields, author=EXCLUDED.author, updated_at=now();
  ELSIF NEW.kind = 'route' THEN
    bid := tree_path_branch(NEW.path);
    INSERT INTO view_bindings (branch_id,route,handler,visibility)
    VALUES (bid, NEW.payload->'body'->>'route', NEW.payload->'body'->>'handler',
            COALESCE(NEW.payload->'body'->>'visibility','private'))
    ON CONFLICT (branch_id,route) DO UPDATE
      SET handler=EXCLUDED.handler, visibility=EXCLUDED.visibility;
  ELSIF NEW.kind = 'datum' THEN
    did := tree_datum_id(NEW.path);
    IF did IS NOT NULL THEN
      INSERT INTO data_rows (id,type_name,fields,author)
      VALUES (did, NEW.type_name, NEW.payload->'body', NEW.payload->>'author')
      ON CONFLICT (id) DO UPDATE
        SET fields=EXCLUDED.fields, author=EXCLUDED.author,
            type_name=EXCLUDED.type_name, updated_at=now();
    END IF;
  END IF;
  RETURN NEW;
END $$;

-- ---------------------------------------------------------------------------
-- Backfill existing datum nodes from old data.<type>.<id> shape.
-- Guard with pp.tree_mirror so reverse trigger does not thrash data_rows.
-- ---------------------------------------------------------------------------
DO $$
DECLARE
  r RECORD;
  new_p ltree;
  moved int := 0;
  shared_n int := 0;
  user_n int := 0;
BEGIN
  PERFORM set_config('pp.tree_mirror', '1', true);
  FOR r IN
    SELECT n.path, n.type_name, n.author, n.payload, n.hash, tree_datum_id(n.path) AS id
      FROM tree_nodes n
     WHERE n.kind = 'datum'
  LOOP
    IF r.id IS NULL THEN
      CONTINUE;
    END IF;
    new_p := tree_datum_path(r.type_name, r.id, r.author);
    IF r.path = new_p THEN
      CONTINUE;
    END IF;
    -- Drop any pre-existing target (shouldn't happen) then move.
    DELETE FROM tree_nodes WHERE path = new_p AND path <> r.path;
    UPDATE tree_nodes
       SET path = new_p,
           payload = jsonb_set(COALESCE(payload, '{}'::jsonb), '{path}',
                               to_jsonb(new_p::text)),
           updated_at = now()
     WHERE path = r.path;
    moved := moved + 1;
    IF tree_is_seed_type(r.type_name) THEN
      shared_n := shared_n + 1;
    ELSE
      user_n := user_n + 1;
    END IF;
  END LOOP;
  RAISE NOTICE '013 backfill: moved=% shared=% user=%', moved, shared_n, user_n;
END $$;

-- Ensure every data_rows row has a tree node at the new path (repair gaps).
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
  FOR r IN SELECT id, type_name, fields, author FROM data_rows LOOP
    p := tree_datum_path(r.type_name, r.id, r.author);
    IF EXISTS (SELECT 1 FROM tree_nodes WHERE path = p) THEN
      CONTINUE;
    END IF;
    -- Remove any stale datum node for this id under an old path.
    DELETE FROM tree_nodes
     WHERE kind = 'datum' AND tree_datum_id(path) = r.id AND path <> p;
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
  RAISE NOTICE '013 repair: inserted/updated % missing datum nodes', n;
END $$;

-- ---------------------------------------------------------------------------
-- tree_data_rows view: id from last label; optional pp.user confinement.
-- When pp.user is unset/empty the view returns ALL datums (scripts/admin).
-- When set, only data.u.<user>.* ∪ data.shared.* (RLS-aligned).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE VIEW tree_data_rows AS
SELECT tree_datum_id(path) AS id,
       type_name,
       payload->'body' AS fields,
       payload->>'author' AS author,
       created_at, updated_at
FROM tree_nodes
WHERE kind = 'datum'
  AND (
    COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
  );

-- ---------------------------------------------------------------------------
-- RLS floor on tree_nodes. Non-datum kinds unrestricted here (branch lens
-- is the code isolation axis; notes already path-keyed under data.note.u.*).
-- App role is typically not the table owner; FORCE covers owner connections.
-- ---------------------------------------------------------------------------
ALTER TABLE tree_nodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE tree_nodes FORCE ROW LEVEL SECURITY;

DROP POLICY IF EXISTS datum_user_isolation ON tree_nodes;
CREATE POLICY datum_user_isolation ON tree_nodes
  USING (
    kind <> 'datum'
    OR COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
  )
  WITH CHECK (
    kind <> 'datum'
    OR COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
  );

-- Bypass for the mirror guard path is unnecessary: triggers run as table
-- owner and FORCE applies, but pp.user='' (unset) opens the policy. Mirror
-- functions set pp.tree_mirror, not pp.user — unset pp.user keeps mirrors
-- working. Prim code sets pp.user inside request-scoped transactions.

INSERT INTO schema_migrations (version, description)
SELECT 13, 'user-scoped datum paths data.u.<user>.* + data.shared.* + RLS floor'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 13);
