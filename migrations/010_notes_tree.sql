-- Pricklypear migration 010: hierarchical notes indexes (notes.tree-cutover K1).
--
-- folder/note/heading live in tree_nodes (not kind=datum). Enforce:
--   - unique body.uuid among folder+note (stable identity / get-uuid)
--   - unique (author, body.slug) among notes (per-user global slug)
-- Additive only; safe to re-run (IF NOT EXISTS).

CREATE UNIQUE INDEX IF NOT EXISTS tree_nodes_hier_uuid_uidx
  ON tree_nodes ((payload #>> '{body,uuid}'))
  WHERE kind IN ('folder', 'note')
    AND coalesce(payload #>> '{body,uuid}', '') <> '';

CREATE UNIQUE INDEX IF NOT EXISTS tree_nodes_note_slug_author_uidx
  ON tree_nodes (author, (payload #>> '{body,slug}'))
  WHERE kind = 'note'
    AND coalesce(payload #>> '{body,slug}', '') <> '';

CREATE INDEX IF NOT EXISTS tree_nodes_body_slug_idx
  ON tree_nodes ((payload #>> '{body,slug}'))
  WHERE kind = 'note';

CREATE INDEX IF NOT EXISTS tree_nodes_body_uuid_idx
  ON tree_nodes ((payload #>> '{body,uuid}'))
  WHERE kind IN ('folder', 'note', 'heading')
    AND coalesce(payload #>> '{body,uuid}', '') <> '';
