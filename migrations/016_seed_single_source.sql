-- Pricklypear migration 016: single source of truth for the seed-type list.
--
-- Seed corpus types (shared under data.shared.*, visible to every session)
-- were previously listed in three places and drifted (see log/issues/
-- i-seed-list-3copies-2026-08-12): the SQL predicate tree_is_seed_type, the
-- crud.ml visible_sql IN-list, and the OCaml Tree_store.seed_types array.
-- Migration 013/014 already moved these into one SQL predicate; this step
-- makes THAT predicate derive from a single authoritative array function,
-- tree_seed_types(). The OCaml Tree_store.seed_types constant stays as the
-- pure mirror needed by the DB-free `node-path` prim; a boot-time check
-- (Tree_store.verify_seed_types) flags any divergence from this authority.

-- The ONE place the type-name list lives on the server side:
CREATE OR REPLACE FUNCTION tree_seed_types() RETURNS text[]
LANGUAGE sql IMMUTABLE AS $$
  SELECT ARRAY[
    'bible-verse',
    'bible-book',
    'bible-interlinear',
    'strongs-entry',
    'guestbook_entry'
  ]
$$;

-- Predicate now derives from the single authority above.
CREATE OR REPLACE FUNCTION tree_is_seed_type(t text) RETURNS boolean
LANGUAGE sql IMMUTABLE AS $$
  SELECT t = ANY(tree_seed_types())
$$;

INSERT INTO schema_migrations (version, description)
SELECT 16, 'single source of truth for seed-type list (tree_seed_types)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 16);
