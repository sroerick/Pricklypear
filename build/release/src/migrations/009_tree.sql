-- Pricklypear migration 009: ONE TREE substrate (shadow phase).
--
-- Per spec [one-tree.tree-model / migration-plan]. This is the ADDITIVE
-- physical layout for the one-big-tree store, plus the P2 SHADOW MIRROR
-- that keeps the tree in lockstep with the scaffolding tables
-- (function_defs, type_defs, view_bindings, data_rows) via deferred
-- triggers. Nothing reads from the tree yet (that is P3+); this is
-- pure additive shadow — zero behavior change, fully reversible by
-- dropping the triggers (the additive-only rule applies to the tree's
-- OWN tables going forward; these triggers are the mirror mechanism).
--
-- Path grammar (ltree, dots not slashes — ltree labels):
--   fn     b.<branch_id>.fn.<name>
--   type   b.<branch_id>.type.<name>
--   route  b.<branch_id>.route.<route_b64url>   (routes contain '/:+'
--                                               which are not ltree-safe)
--   datum  data.<type_name>.<id>
-- Node payload is a jsonb envelope {v,kind,path,type,body,author,doc,hash}.
-- hash = sha256 over the CANONICAL body (independent of path/author) so
-- content corruption is detectable. data_nodes are SHARED (branch-free)
-- per [branching.data]; code/type/route nodes live under a branch prefix.

CREATE EXTENSION IF NOT EXISTS ltree;
CREATE EXTENSION IF NOT EXISTS pgcrypto;  -- digest() for content hashes

CREATE TABLE IF NOT EXISTS tree_nodes (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  path        ltree NOT NULL UNIQUE,
  kind        text NOT NULL,          -- fn | type | route | datum
  type_name   text,                   -- datum only: the Nopales type
  payload     jsonb NOT NULL,         -- envelope (see header)
  hash        text NOT NULL,          -- sha256 over canonical body
  author      text NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now(),
  updated_at  timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS tree_nodes_path_gist ON tree_nodes USING gist (path);
CREATE INDEX IF NOT EXISTS tree_nodes_kind_type_idx ON tree_nodes (kind, type_name);
CREATE INDEX IF NOT EXISTS tree_nodes_payload_gin ON tree_nodes USING gin (payload);

-- The lens registry. In v1 the branches table still owns branch_id +
-- parent chain; tree_branches adds the lens root pointer + head hash.
-- True content-addressed pointer-moves (fork/promote) land in P5.
CREATE TABLE IF NOT EXISTS tree_branches (
  branch_id   uuid PRIMARY KEY REFERENCES branches(id) ON DELETE CASCADE,
  root_path   ltree NOT NULL,          -- b.<branch_id>
  parent_id   uuid,                    -- mirror of branches.parent_id (walk)
  head_hash   text                     -- subtree root hash (P5)
);

-- ---------------------------------------------------------------------------
-- Label encoding. ltree labels: [A-Za-z0-9_], no dots/dashes/slashes.
-- UUIDs -> hex (strip dashes). Names/routes/types -> base64url-ish of the
-- raw bytes using _ for +, - for /, and trailing digits for pad count.
-- Deterministic + reversible in OCaml (tree_store.ml decodes).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_enc_label(s text) RETURNS text
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE
  b64 text;
  pad int;
BEGIN
  IF s IS NULL OR s = '' THEN RETURN 'x'; END IF;
  b64 := encode(s::bytea, 'base64');
  pad := (length(b64) - length(rtrim(b64, '=')));  -- 0,1,2 trailing '='
  b64 := rtrim(b64, '=');
  b64 := replace(b64, '+', '_');
  b64 := replace(b64, '/', '-');
  RETURN b64 || pad::text;   -- pad count disambiguates; ''->'x' handled above
END $$;

CREATE OR REPLACE FUNCTION tree_uuid_label(u uuid) RETURNS text
LANGUAGE sql IMMUTABLE AS $$ SELECT replace(u::text, '-', '') $$;

-- ---------------------------------------------------------------------------
-- Mirror trigger functions. Each maps one scaffolding row -> one tree node
-- (upsert), and removes the node on row delete. CONSTRAINT TRIGGER ... DEFERRABLE
-- INITIALLY DEFERRED so the mirror runs at COMMIT (after the statement),
-- keeping it in the same transaction as the write.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_mirror_function_defs() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);  -- guard: our tree write must not bounce back
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE path = ('b.' || tree_uuid_label(OLD.branch_id) || '.fn.'
                   || tree_enc_label(OLD.name))::ltree;
    RETURN OLD;
  END IF;
  p := ('b.' || tree_uuid_label(NEW.branch_id) || '.fn.'
        || tree_enc_label(NEW.name))::ltree;
  body := NEW.body;  -- canonical fn body IS the AST jsonb
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'fn', 'path', p::text, 'type', 'fn',
    'body', body, 'author', NEW.author, 'doc', NEW.doc, 'hash', h);
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'fn', NULL, env, h, NEW.author, now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, updated_at = now();
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION tree_mirror_type_defs() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);  -- guard: our tree write must not bounce back
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE path = ('b.' || tree_uuid_label(OLD.branch_id) || '.type.'
                   || tree_enc_label(OLD.name))::ltree;
    RETURN OLD;
  END IF;
  p := ('b.' || tree_uuid_label(NEW.branch_id) || '.type.'
        || tree_enc_label(NEW.name))::ltree;
  body := NEW.fields;  -- canonical type body IS the fields jsonb array
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'type', 'path', p::text, 'type', NEW.name,
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h);
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'type', NEW.name, env, h, NEW.author, now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, updated_at = now();
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION tree_mirror_view_bindings() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb; vis text;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);  -- guard: our tree write must not bounce back
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE path = ('b.' || tree_uuid_label(OLD.branch_id) || '.route.'
                   || tree_enc_label(OLD.route))::ltree;
    RETURN OLD;
  END IF;
  p := ('b.' || tree_uuid_label(NEW.branch_id) || '.route.'
        || tree_enc_label(NEW.route))::ltree;
  vis := COALESCE(NEW.visibility, 'private');
  body := jsonb_build_object('route', NEW.route, 'handler', NEW.handler,
                             'visibility', vis);
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'route', 'path', p::text, 'type', 'route',
    'body', body, 'author', 'system', 'doc', NULL, 'hash', h);
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'route', NULL, env, h, 'system', now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, updated_at = now();
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION tree_mirror_data_rows() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p ltree; body jsonb; h text; env jsonb;
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);  -- guard: our tree write must not bounce back
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_nodes
     WHERE path = ('data.' || tree_enc_label(OLD.type_name) || '.'
                   || tree_uuid_label(OLD.id))::ltree;
    RETURN OLD;
  END IF;
  p := ('data.' || tree_enc_label(NEW.type_name) || '.'
        || tree_uuid_label(NEW.id))::ltree;
  body := NEW.fields;  -- canonical datum body IS the fields jsonb
  h := encode(digest(body::text, 'sha256'), 'hex');
  env := jsonb_build_object(
    'v', 1, 'kind', 'datum', 'path', p::text, 'type', NEW.type_name,
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h);
  INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author, updated_at)
  VALUES (p, 'datum', NEW.type_name, env, h, NEW.author, now())
  ON CONFLICT (path) DO UPDATE
    SET payload = EXCLUDED.payload, hash = EXCLUDED.hash,
        author = EXCLUDED.author, updated_at = now();
  RETURN NEW;
END $$;

-- Deferred mirror triggers (fire at commit, in the write's transaction).
DROP TRIGGER IF EXISTS tree_mirror_function_defs_trg ON function_defs;
CREATE CONSTRAINT TRIGGER tree_mirror_function_defs_trg
  AFTER INSERT OR UPDATE OR DELETE ON function_defs
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_mirror_function_defs();

DROP TRIGGER IF EXISTS tree_mirror_type_defs_trg ON type_defs;
CREATE CONSTRAINT TRIGGER tree_mirror_type_defs_trg
  AFTER INSERT OR UPDATE OR DELETE ON type_defs
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_mirror_type_defs();

DROP TRIGGER IF EXISTS tree_mirror_view_bindings_trg ON view_bindings;
CREATE CONSTRAINT TRIGGER tree_mirror_view_bindings_trg
  AFTER INSERT OR UPDATE OR DELETE ON view_bindings
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_mirror_view_bindings();

DROP TRIGGER IF EXISTS tree_mirror_data_rows_trg ON data_rows;
CREATE CONSTRAINT TRIGGER tree_mirror_data_rows_trg
  AFTER INSERT OR UPDATE OR DELETE ON data_rows
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_mirror_data_rows();

-- Keep tree_branches in step with branches (lens registry).
CREATE OR REPLACE FUNCTION tree_mirror_branches() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM tree_branches WHERE branch_id = OLD.id;
    RETURN OLD;
  END IF;
  INSERT INTO tree_branches (branch_id, root_path, parent_id)
  VALUES (NEW.id, ('b.' || tree_uuid_label(NEW.id))::ltree, NEW.parent_id)
  ON CONFLICT (branch_id) DO UPDATE
    SET root_path = EXCLUDED.root_path, parent_id = EXCLUDED.parent_id;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS tree_mirror_branches_trg ON branches;
CREATE CONSTRAINT TRIGGER tree_mirror_branches_trg
  AFTER INSERT OR UPDATE OR DELETE ON branches
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_mirror_branches();

-- ---------------------------------------------------------------------------
-- BACKFILL existing rows into the tree (idempotent upserts; the ON CONFLICT
-- in the mirror functions makes re-runs safe).
-- ---------------------------------------------------------------------------
INSERT INTO tree_branches (branch_id, root_path, parent_id)
SELECT id, ('b.' || tree_uuid_label(id))::ltree, parent_id FROM branches
ON CONFLICT (branch_id) DO NOTHING;

INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author)
SELECT
  ('b.' || tree_uuid_label(branch_id) || '.fn.' || tree_enc_label(name))::ltree,
  'fn', NULL,
  jsonb_build_object('v',1,'kind','fn',
    'path', ('b.' || tree_uuid_label(branch_id) || '.fn.' || tree_enc_label(name)),
    'type','fn','body', body, 'author', author, 'doc', doc,
    'hash', encode(digest(body::text,'sha256'),'hex')),
  encode(digest(body::text,'sha256'),'hex'),
  author
FROM function_defs
ON CONFLICT (path) DO NOTHING;

INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author)
SELECT
  ('b.' || tree_uuid_label(branch_id) || '.type.' || tree_enc_label(name))::ltree,
  'type', name,
  jsonb_build_object('v',1,'kind','type',
    'path', ('b.' || tree_uuid_label(branch_id) || '.type.' || tree_enc_label(name)),
    'type', name, 'body', fields, 'author', author, 'doc', NULL,
    'hash', encode(digest(fields::text,'sha256'),'hex')),
  encode(digest(fields::text,'sha256'),'hex'),
  author
FROM type_defs
ON CONFLICT (path) DO NOTHING;

INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author)
SELECT
  ('b.' || tree_uuid_label(branch_id) || '.route.' || tree_enc_label(route))::ltree,
  'route', NULL,
  jsonb_build_object('v',1,'kind','route',
    'path', ('b.' || tree_uuid_label(branch_id) || '.route.' || tree_enc_label(route)),
    'type','route',
    'body', jsonb_build_object('route', route, 'handler', handler,
                               'visibility', COALESCE(visibility,'private')),
    'author','system','doc', NULL,
    'hash', encode(digest(jsonb_build_object('route', route,'handler',handler,
             'visibility', COALESCE(visibility,'private'))::text,'sha256'),'hex')),
  encode(digest(jsonb_build_object('route', route,'handler',handler,
           'visibility', COALESCE(visibility,'private'))::text,'sha256'),'hex'),
  'system'
FROM view_bindings
ON CONFLICT (path) DO NOTHING;

INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author)
SELECT
  ('data.' || tree_enc_label(type_name) || '.' || tree_uuid_label(id))::ltree,
  'datum', type_name,
  jsonb_build_object('v',1,'kind','datum',
    'path', ('data.' || tree_enc_label(type_name) || '.' || tree_uuid_label(id)),
    'type', type_name, 'body', fields, 'author', author, 'doc', NULL,
    'hash', encode(digest(fields::text,'sha256'),'hex')),
  encode(digest(fields::text,'sha256'),'hex'),
  author
FROM data_rows
ON CONFLICT (path) DO NOTHING;

-- ---------------------------------------------------------------------------
-- Consistency check: counts of mirrored rows vs tree nodes per kind.
-- Returns rows (kind, table_count, tree_count, in_sync). Used by smoke/tests.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_verify()
  RETURNS TABLE(kind text, table_count bigint, tree_count bigint, in_sync boolean)
LANGUAGE sql AS $$
  SELECT 'fn', (SELECT count(*) FROM function_defs),
         (SELECT count(*) FROM tree_nodes WHERE kind='fn'),
         (SELECT count(*) FROM function_defs) =
           (SELECT count(*) FROM tree_nodes WHERE kind='fn')
  UNION ALL
  SELECT 'type', (SELECT count(*) FROM type_defs),
         (SELECT count(*) FROM tree_nodes WHERE kind='type'),
         (SELECT count(*) FROM type_defs) =
           (SELECT count(*) FROM tree_nodes WHERE kind='type')
  UNION ALL
  SELECT 'route', (SELECT count(*) FROM view_bindings),
         (SELECT count(*) FROM tree_nodes WHERE kind='route'),
         (SELECT count(*) FROM view_bindings) =
           (SELECT count(*) FROM tree_nodes WHERE kind='route')
  UNION ALL
  SELECT 'datum', (SELECT count(*) FROM data_rows),
         (SELECT count(*) FROM tree_nodes WHERE kind='datum'),
         (SELECT count(*) FROM data_rows) =
           (SELECT count(*) FROM tree_nodes WHERE kind='datum')
$$;

-- ---------------------------------------------------------------------------
-- P4 REVERSE MIRROR: tree_nodes -> scaffolding tables. Lets the image write
-- the TREE as truth while the legacy tables (and every tool/read path that
-- still hits them) stay in lockstep. Recursion-guarded via a transaction-
-- scoped GUC: table->tree triggers set it, tree->table triggers skip when
-- set, so a table write mirrors to tree and STOPS (no ping-pong).
-- Deferred to commit, same-transaction => identical durability semantics.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_in_mirror() RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT COALESCE(current_setting('pp.tree_mirror', true), '') = '1'
$$;

CREATE OR REPLACE FUNCTION tree_from_nodes() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE bid uuid; lbl text; docv text;
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
      DELETE FROM data_rows WHERE id=((string_to_array(OLD.path::text,'.'))[3])::uuid;
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
    INSERT INTO data_rows (id,type_name,fields,author)
    VALUES (((string_to_array(NEW.path::text,'.'))[3])::uuid,
            NEW.type_name, NEW.payload->'body', NEW.payload->>'author')
    ON CONFLICT (id) DO UPDATE
      SET fields=EXCLUDED.fields, author=EXCLUDED.author, updated_at=now();
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS tree_from_nodes_trg ON tree_nodes;
CREATE CONSTRAINT TRIGGER tree_from_nodes_trg
  AFTER INSERT OR UPDATE OR DELETE ON tree_nodes
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION tree_from_nodes();

-- Re-point the forward (table->tree) triggers to set the guard so the
-- tree write they cause does NOT bounce back into the table.
CREATE OR REPLACE FUNCTION tree_mirror_guard_set() RETURNS void
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM set_config('pp.tree_mirror','1', true);
END $$;

-- ---------------------------------------------------------------------------
-- P3 READ VIEWS: tree-backed relations that reproduce the exact column shape
-- of the scaffolding tables, so store.ml / crud.ml can read from the tree
-- behind the unchanged prim ABI by swapping the relation (flag-gated via
-- PP_TREE_READS). branch_id is recovered from the path's 2nd label (the
-- uuid-hex); names/routes are decoded from their labels.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_dec_label(lbl text) RETURNS text
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE pad int; core text; b64 text;
BEGIN
  IF lbl IS NULL THEN RETURN NULL; END IF;
  IF lbl = 'x' THEN RETURN ''; END IF;
  pad := substr(lbl, length(lbl), 1)::int;   -- trailing pad count 0/1/2
  core := substr(lbl, 1, length(lbl) - 1);
  b64 := replace(replace(core, '_', '+'), '-', '/');
  b64 := b64 || repeat('=', pad);
  RETURN convert_from(decode(b64, 'base64'), 'UTF8');
END $$;

-- branch_id (uuid) from a b.<uuidhex>.... path. Shared subtrees
-- (data.*, meta.*) have NO branch prefix -> return NULL (caller checks).
CREATE OR REPLACE FUNCTION tree_path_branch(p ltree) RETURNS uuid
LANGUAGE plpgsql IMMUTABLE AS $$
BEGIN
  IF p::text NOT LIKE 'b.%' THEN RETURN NULL; END IF;
  RETURN (substr(p::text, 3, 8) || '-' || substr(p::text, 11, 4) || '-' ||
          substr(p::text, 15, 4) || '-' || substr(p::text, 19, 4) || '-' ||
          substr(p::text, 23, 12))::uuid;
END $$;

CREATE OR REPLACE VIEW tree_function_defs AS
SELECT tree_path_branch(path) AS branch_id,
       tree_dec_label((string_to_array(path::text, '.'))[4]) AS name,
       payload->'body' AS body,
       payload->>'author' AS author,
       payload->>'doc' AS doc,
       created_at, updated_at
FROM tree_nodes WHERE kind = 'fn';

CREATE OR REPLACE VIEW tree_type_defs AS
SELECT tree_path_branch(path) AS branch_id,
       tree_dec_label((string_to_array(path::text, '.'))[4]) AS name,
       payload->'body' AS fields,
       payload->>'author' AS author,
       created_at, updated_at
FROM tree_nodes WHERE kind = 'type';

CREATE OR REPLACE VIEW tree_view_bindings AS
SELECT tree_path_branch(path) AS branch_id,
       payload->'body'->>'route' AS route,
       payload->'body'->>'handler' AS handler,
       COALESCE(payload->'body'->>'visibility', 'private') AS visibility,
       created_at, updated_at
FROM tree_nodes WHERE kind = 'route';

CREATE OR REPLACE VIEW tree_data_rows AS
SELECT ((string_to_array(path::text, '.'))[3])::uuid AS id,
       type_name,
       payload->'body' AS fields,
       payload->>'author' AS author,
       created_at, updated_at
FROM tree_nodes WHERE kind = 'datum';

INSERT INTO schema_migrations (version, description)
SELECT 9, 'one-tree substrate: tree_nodes + tree_branches + deferred shadow mirror + read views'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 9);
