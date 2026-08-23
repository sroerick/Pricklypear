-- Pricklypear migration 023: versioned user-keyed data — data lens namespace.
--
-- Spec: one-tree.borg branching-interaction (2026-08-23 plan-phase proposal,
--       now make). Adds an OPT-IN data.l.<lens>.<user>.<type>.<id> path for
--       deliberate data forks that diverge and merge back. The default
--       data.u.<user>.* stays single-head and branch-free; data.l.* is the
--       explicit version axis.
--
-- What this migration does:
--   1. tree_datum_path gains a ~lens_name parameter; when set, the path is
--      data.l.<lens>.<user>.<type>.<id> instead of data.u.<user>.<type>.<id>.
--   2. tree_data_rows view + RLS policy allow data.l.<lens>.<user>.* reads/writes
--      for the owning user (same path-prefix confinement as data.u.*).
--
-- What this migration does NOT do:
--   - No data migration of existing rows (new path shape is opt-in only).
--   - No notes path migration (that is a separate make task; see notes.borg).
--   - No tree-time generation tracking for data.l.* (follows in tree-time make).

-- ---------------------------------------------------------------------------
-- tree_datum_path: add optional lens_name parameter.
-- When lens_name is non-empty, path is data.l.<lens>.<user>.<type>.<id>.
-- The DEFAULT '' preserves the existing data.u.<user>.<type>.<id> shape for
-- all existing callers that don't pass lens_name.
-- DROP the old 3-arg signature first to avoid a non-unique overload
-- (CREATE OR REPLACE with a different arg count creates a new function,
-- not a replacement). DO block for PG portability (no IF EXISTS on DROP).
DO $$ BEGIN
  DROP FUNCTION tree_datum_path(text, uuid, text);
EXCEPTION WHEN undefined_function THEN NULL;
END $$;
CREATE OR REPLACE FUNCTION tree_datum_path(type_name text, id uuid, author text,
                                            lens_name text)
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
  IF lens_name IS NOT NULL AND lens_name <> '' THEN
    RETURN ('data.l.' || tree_enc_label(lens_name) || '.' ||
            tree_enc_label(a) || '.' ||
            tree_enc_label(type_name) || '.' ||
            tree_uuid_label(id))::ltree;
  END IF;
  RETURN ('data.u.' || tree_enc_label(a) || '.' ||
          tree_enc_label(type_name) || '.' ||
          tree_uuid_label(id))::ltree;
END $$;

-- ---------------------------------------------------------------------------
-- tree_data_rows view: allow data.l.<lens>.<user>.* visibility.
-- A datum is visible when:
--   - pp.user is unset (admin/scripts), OR
--   - path is under data.u.<user> (own live data), OR
--   - path is under data.shared (seed), OR
--   - path is under data.l.<lens>.<user> for ANY lens (own data lens forks).
--     subquery: path <@ 'data.l' AND the 4th label (position 3) = enc(user).
--     Path layout: data.l.<lens>.<user>.<type>.<id>
--     Positions:   0    1    2      3      4      5
--     So the user is at subltree(path, 3, 4), NOT subltree(path, 2, 3)
--     which is the lens name.
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
    OR (
      path <@ 'data.l'::ltree
      AND subltree(path, 3, 4) = tree_enc_label(current_setting('pp.user', true))::ltree
    )
  );

-- ---------------------------------------------------------------------------
-- RLS policy: extend datum_user_isolation to allow data.l.* for the owner.
-- Same path-prefix confinement: data.l.<lens>.<user>.* is visible to <user>.
-- ---------------------------------------------------------------------------
-- Drop the old policy (PG < 9.6 doesn't support DROP POLICY IF EXISTS;
-- wrap in a DO block for portability).
DO $$ BEGIN
  DROP POLICY datum_user_isolation ON tree_nodes;
EXCEPTION WHEN undefined_object THEN NULL;
END $$;
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
  );


-- ---------------------------------------------------------------------------
-- Update tree_mirror_data_rows to use the 4-arg tree_datum_path explicitly.
-- The original (migration 013) calls tree_datum_path(type, id, author) with 3
-- args. After this migration drops the 3-arg overload, that call must resolve
-- to the 4-arg function with DEFAULT ''. Updating the function body here
-- ensures PL/pgSQL recompiles against the new signature so the 3-arg overload
-- is not silently recreated by the planner caching the old resolution.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_mirror_data_rows() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE kind = 'datum'
       AND tree_datum_id(path) = OLD.id;
    RETURN OLD;
  END IF;
  p := tree_datum_path(NEW.type_name, NEW.id, NEW.author, '');
  body := NEW.fields;
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'datum', 'path', p::text, 'type', NEW.type_name,
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h);
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
END
$$;
-- ---------------------------------------------------------------------------
-- Guard the reverse-mirror trigger (tree_from_nodes) against data.l.* paths.
-- The reverse mirror syncs tree_nodes -> scaffolding tables (function_defs,
-- type_defs, view_bindings, data_rows). Data lens nodes (data.l.*) are
-- version-only copies that have no scaffolding-table counterpart; mirroring them
-- would corrupt the live data_rows (e.g. DELETE FROM data_rows WHERE id = did
-- would delete the live row). The trigger must SKIP data.l.* datum nodes.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_from_nodes() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE bid uuid; lbl text; docv text; did uuid;
BEGIN
  IF tree_in_mirror() THEN
    IF TG_OP = 'DELETE' THEN RETURN OLD; ELSE RETURN NEW; END IF;
  END IF;
  -- Skip data.l.* nodes entirely: they are lens-only copies with no
  -- scaffolding-table counterpart. Mirroring them would corrupt live rows.
  IF (TG_OP = 'DELETE' AND OLD.path <@ 'data.l'::ltree)
     OR (TG_OP <> 'DELETE' AND NEW.path <@ 'data.l'::ltree) THEN
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
END
$$;

INSERT INTO schema_migrations (version, description)
SELECT 23, 'data lens namespace data.l.<lens>.<user>.* + RLS visibility + trigger guard'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 23);
