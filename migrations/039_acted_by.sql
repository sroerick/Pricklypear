-- Pricklypear migration 039: acted_by (auth.delegation-acted-by).
--
-- FED3's first named prerequisite (disposition recorded 2026-10-02 in the
-- tuna federation chapter): delegated-token writes authenticate as the
-- OWNER (author = manifest.username) but nothing recorded WHOSE
-- credential performed the write, so a downstream consumer (the FED3
-- bridge among them) would attribute agent work to the owner.
-- Pre-registered failure 3 ("a bridge write attributed to any identity
-- other than the true actor") makes that state unshippable.
--
-- Adds the responsibility column and the mirror stamps:
--
--   data_rows.acted_by text -- NULL when a plain owner/identity write
--                             produced the row; the agent's username when
--                             a delegated token did.
--   tree_nodes payload env -- the canonical document gains an 'acted_by'
--                             env key, so tree_versions (per-write
--                             history, migration 019) records it per gen
--                             for free (the version body IS the env).
--
-- Attribution semantics (PINNED; keep code aligned with this):
--   create: author = the session's username (the OWNER on delegated
--           tokens; the standing law), acted_by = the session's actor
--           or NULL.
--   update: delegated updates stamp acted_by = the actor; non-delegated
--           (owner / bookkeeping) writes PRESERVE the existing column
--           (COALESCE). acted_by answers "has an agent's credential ever
--           acted on this row"; author + tree_versions history carry
--           per-write identity.
--   delete: the row vanishes; the tombstone version keeps the pre-delete
--           payload including the env 'acted_by' key.
--
-- STRICTLY ADDITIVE + image-applied + idempotent (registered in
-- schema_migrations so the runner will not re-apply).

ALTER TABLE data_rows ADD COLUMN IF NOT EXISTS acted_by text;

-- ---------------------------------------------------------------------------
-- Forward mirror: data_rows -> tree_nodes. Copy of migration 034's body with
-- ONE delta: the env gains 'acted_by' so the tree document stays
-- self-describing for rows written through the legacy data_rows end.
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
    'body', body, 'author', NEW.author, 'doc', NULL, 'hash', h,
    'acted_by', NEW.acted_by);
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
-- Reverse mirror: tree_nodes -> data_rows. Copy of migration 023's body with
-- the datum branch gaining acted_by: read from the payload's 'acted_by' env
-- key, PRESERVED on update (COALESCE) so writes that carry no stamp
-- (restore / sync / bookkeeping upserts) never silently clear attribution.
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
      INSERT INTO data_rows (id,type_name,fields,author,acted_by)
      VALUES (did, NEW.type_name, NEW.payload->'body', NEW.payload->>'author',
              NEW.payload->>'acted_by')
      ON CONFLICT (id) DO UPDATE
        SET fields=EXCLUDED.fields, author=EXCLUDED.author,
            type_name=EXCLUDED.type_name, updated_at=now(),
            acted_by = COALESCE(EXCLUDED.acted_by, data_rows.acted_by);
    END IF;
  END IF;
  RETURN NEW;
END
$$;

INSERT INTO schema_migrations (version, description)
SELECT 39, 'acted_by: delegated-agent attribution on data_rows + payload env stamp'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 39);
