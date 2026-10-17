-- 043_tree_path_member_arg.sql — fix tree_path_in_member_space parameter
-- shadowing (non-members passed the membership check).
--
-- BUG: the membership half read `WHERE m.username = username`. In a SQL
-- function, a parameter whose name collides with a column in scope is
-- SHADOWED BY THE COLUMN (PostgreSQL: column names take precedence over
-- SQL-function parameter names). `m.username = username` therefore evaluated
-- as `m.username = m.username` — always true — so the membership EXISTS
-- passed for ANY username whenever ANY member row existed for an ancestor
-- space. Concretely: a non-member (carol) could read member-space rows
-- through tree_data_rows and through the datum_user_isolation RLS policy,
-- both of which call this predicate. Caught by the data_spaces_test carol
-- (non-member) visibility cases on dev PG (card 13d1e71e):
--   tree_path_in_member_space('data.s.<enc sp>.abc', 'nobody-here') = t
--
-- Fix: reference the username parameter by ordinal ($2), which the column
-- cannot shadow. CREATE OR REPLACE only — the view tree_data_rows and the
-- RLS policy datum_user_isolation depend on this function, so 043 must not
-- drop or rename it.

CREATE OR REPLACE FUNCTION tree_path_in_member_space(p ltree, username text)
RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1 FROM data_space_members m
     WHERE m.username = $2
       AND p <@ ('data.s.' || tree_enc_label(m.space))::ltree)
  OR (
    $2 IS NOT NULL
    AND $2 <> ''
    AND $2 <> 'anonymous'
    AND EXISTS (
      SELECT 1 FROM data_spaces s
       WHERE s.open
         AND p <@ ('data.s.' || tree_enc_label(s.name))::ltree))
$$;

INSERT INTO schema_migrations (version, description)
SELECT 43, 'tree_path_in_member_space: use $2 for the username param — unqualified username was shadowed by the column, non-members passed membership'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 43);
