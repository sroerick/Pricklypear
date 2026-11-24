-- 050_vis_private_default.sql — a row with no body vis reads PRIVATE.
--
-- Owner decision (roerick, 2026-11-23): everything is private unless it is
-- marked public. This is the book's rule (notes.borg note-visibility: "Absent
-- reads private"; row-type-public-visibility decision 6: public is declared,
-- never a policy default).
--
-- WHAT THIS UNDOES: card 5a30a045 (e95dfbd2 / e8d7af53, 2026-11-18) edited
-- 049 in place so pp_vis_readable treated an ABSENT body vis as 'public' and
-- OR'd it into both tree_nodes policies and the read wrappers. Every datum
-- without a vis field, including private data.u rows, became readable by any
-- identity, anonymous included. It also added a pp.user = 'root' clause, a
-- sovereign read inside the floor that 049's own header records as rejected.
--
-- WHAT STAYS: the policies and wrappers keep calling pp_vis_readable, so only
-- the function changes here. vis only ever WIDENS the pp_node_visible floor:
--   public  = anyone (explicit flag only)
--   users   = any signed-in identity
--   private = members of the space containing the path (already covered by
--             pp_node_visible; restated so the flag table reads whole)
--   absent / anything else = no widening.
--
-- REPLAY NOTE: dev and deploy re-apply all migrations in sorted order, so
-- 049's CREATE OR REPLACE runs first and this one wins. A checkout without
-- 050 rewinds the function to 049's shape on its next start.
--
-- Additive and idempotent.

CREATE OR REPLACE FUNCTION pp_vis_readable(p_path ltree, p_payload jsonb)
RETURNS boolean
LANGUAGE sql
STABLE
AS $fn$
  SELECT COALESCE(p_payload #>> '{body,vis}', '') = 'public'
      OR (COALESCE(p_payload #>> '{body,vis}', '') = 'users'
          AND COALESCE(current_setting('pp.user', true), '') NOT IN ('', 'anonymous'))
      OR (COALESCE(p_payload #>> '{body,vis}', '') = 'private'
          AND tree_path_in_member_space(p_path, current_setting('pp.user', true)))
$fn$;

COMMENT ON FUNCTION pp_vis_readable(ltree, jsonb) IS
  'Read-side vis widening: public = anyone, users = signed-in, private = member space. Absent vis widens nothing (private by default, 050). No root bypass.';

INSERT INTO schema_migrations (version, description)
SELECT 50, 'pp_vis_readable: absent body vis reads private, drop the root bypass (undoes 5a30a045 public-by-default)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 50);
