-- Pricklypear migration 029: shared data spaces — the data.s.<space>.* compartment.
--
-- Spec: borge/branching.borg shared-data-spaces (2026-09-04 plan pass; S1 make).
-- Multi-identity collaboration (a human + their agent users) on shared
-- projects gets a THIRD datum compartment, member-gated:
--   per-user  data.u.<user>.<type>.<id>      (owner, single-head)
--   seed      data.shared.<type>.<id>        (all sessions, read-mostly)
--   space     data.s.<space>.<type>.<id>     (members only, multi-writer)
-- NOT a branch: no fork/merge/inheritance/base_gen. Single-head rows,
-- multi-writer on the server, serialized by the one image. Local-first:
-- spaces replicate under the data.shared pull+propose policy.
--
-- What this migration does (S1 = the storage floor):
--   1. data_spaces + data_space_members (membership).
--   2. data_space_types: per-type admission (type-level routing v1);
--      the space-admit prim is S2 — S1 tests admit directly via SQL.
--   3. tree_datum_path routes admitted (non-seed, non-lens) types to
--      data.s.<space>.*. Lookup order: seed -> lens -> space -> user.
--      A data-lens copy of a space-admitted type stays user-keyed in the
--      lens (data.l.<lens>.<user>.*): the live home is the space, the
--      lens is a per-user versioned copy.
--   4. tree_path_in_member_space(p, u): the one membership predicate.
--   5. tree_data_rows view + datum_user_isolation RLS gain the member-
--      space disjunct; the view also EXPOSES path (appended column) so
--      the OCaml confinement (crud.ml visible_sql) can apply the same
--      predicate mirror-side.
--
-- KNOWN S1 EDGES (S2 owns): (a) admission/un-admission moves a type's
-- HOME for path recomputation; an un-admitted type's existing space rows
-- keep their paths until next write-touch, when the forward mirror
-- recomputes them to per-user (the spec's "stranded rows stay" stance —
-- S2's migrator decision rounds this out); (b) the OCaml pure datum_path
-- mirror (tree_store.ml) cannot see admissions (they are dynamic) —
-- node-path style pure callers report per-user paths for admitted types
-- until S2 routes them through the SQL function; (c) data_rows gains no
-- space column: the forward mirror recomputes space from type admission.

CREATE TABLE IF NOT EXISTS data_spaces (
  name       text PRIMARY KEY,
  owner      text NOT NULL,          -- session username at create time
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS data_space_members (
  space    text NOT NULL REFERENCES data_spaces(name) ON DELETE CASCADE,
  username text NOT NULL,
  role     text NOT NULL DEFAULT 'member',   -- 'member' | 'owner'
  PRIMARY KEY (space, username)
);

CREATE TABLE IF NOT EXISTS data_space_types (
  type_name   text PRIMARY KEY,      -- one space per type (v1)
  space       text NOT NULL REFERENCES data_spaces(name) ON DELETE CASCADE,
  admitted_by text NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now()
);

-- ---------------------------------------------------------------------------
-- The one membership predicate (mirrors tree_is_seed_type's role for shared).
-- STABLE, not IMMUTABLE: reads data_space_members.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION tree_path_in_member_space(p ltree, username text)
RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1 FROM data_space_members m
     WHERE m.username = username
       AND p <@ ('data.s.' || tree_enc_label(m.space))::ltree)
$$;

-- ---------------------------------------------------------------------------
-- tree_datum_path: admitted (non-seed, non-lens) types route to the space.
-- Signature UNCHANGED (type, id, author, lens) — the space is derived from
-- type admission, so every existing caller (the 013/023 mirror triggers,
-- tree_store.ml insert_datum) routes through the new rule with no call-site
-- change. Drop + recreate following the 023 precedent (CREATE OR REPLACE
-- cannot change... we keep the same args, but drop+create forces PL/pgSQL
-- recompilation in callers, same as 023 did).
-- ---------------------------------------------------------------------------
DO $$ BEGIN
  DROP FUNCTION tree_datum_path(text, uuid, text, text);
EXCEPTION WHEN undefined_function THEN NULL;
END $$;
CREATE FUNCTION tree_datum_path(type_name text, id uuid, author text,
                                lens_name text)
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
-- tree_data_rows view: gain the member-space disjunct + expose path
-- (appended column; CREATE OR REPLACE VIEW permits appending columns).
-- ---------------------------------------------------------------------------
CREATE OR REPLACE VIEW tree_data_rows AS
SELECT tree_datum_id(path) AS id,
       type_name,
       payload->'body' AS fields,
       payload->>'author' AS author,
       created_at,
       updated_at,
       path
FROM tree_nodes
WHERE kind = 'datum'
  AND NOT (path <@ 'data.l'::ltree)
  AND (
    COALESCE(current_setting('pp.user', true), '') = ''
    OR path <@ ('data.u.' || tree_enc_label(current_setting('pp.user', true)))::ltree
    OR path <@ 'data.shared'::ltree
    OR tree_path_in_member_space(path, current_setting('pp.user', true))
  );

-- ---------------------------------------------------------------------------
-- RLS floor: datum_user_isolation gains the member-space disjunct
-- (USING + WITH CHECK — members write space paths, non-members cannot).
-- ---------------------------------------------------------------------------
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
    OR tree_path_in_member_space(path, current_setting('pp.user', true))
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
    OR tree_path_in_member_space(path, current_setting('pp.user', true))
  );

INSERT INTO schema_migrations (version, description)
SELECT 29, 'shared data spaces: data.s.<space>.* member-gated compartment (S1 storage floor)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 29);
