-- Pricklypear migration 030: OPEN spaces — data_spaces.open.
--
-- Spec: borge/branching.borg shared-data-spaces (2026-09-04, open-space
-- decision landed with the chat-lobby first consumer; multi-user.borg
-- chat repair). A space flagged open relaxes "members only" to "every
-- authenticated identity is implicitly a member": no membership rows to
-- manage, no signup churn. The 'anonymous' sentinel is explicitly NOT
-- implicit — guests keep their own data.u.anonymous.* per 013.
--
-- What this migration does:
--   1. data_spaces gains open boolean NOT NULL DEFAULT false.
--   2. tree_path_in_member_space gains the open disjunct:
--      (username <> 'anonymous' AND the row's path descends from an
--      open space). The view tree_data_rows and the RLS policy
--      datum_user_isolation call THIS function, so replacing the body
--      extends both. The OCaml mirror (crud.ml is_space_member, used by
--      the S2 write guard) is updated in code, not here.
--
-- DM caveat (recorded in the stanza): chat rides ONE open lobby space
-- for both lobby and DM messages, restoring the pre-013 handler-filter
-- model for DMs (routes filter room membership; prim-level rows of an
-- admitted type are readable by any authenticated identity). Honest
-- pair-space DMs need row-level space routing — v2.

ALTER TABLE data_spaces ADD COLUMN IF NOT EXISTS open boolean NOT NULL DEFAULT false;

CREATE OR REPLACE FUNCTION tree_path_in_member_space(p ltree, username text)
RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1 FROM data_space_members m
     WHERE m.username = username
       AND p <@ ('data.s.' || tree_enc_label(m.space))::ltree)
  OR (
    username IS NOT NULL
    AND username <> ''
    AND username <> 'anonymous'
    AND EXISTS (
      SELECT 1 FROM data_spaces s
       WHERE s.open
         AND p <@ ('data.s.' || tree_enc_label(s.name))::ltree))
$$;

INSERT INTO schema_migrations (version, description)
SELECT 30, 'open spaces: data_spaces.open — implicit membership for authenticated identities'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 30);
