-- Pricklypear migration 026: tree-edges index (one-tree.tree-edges).
--
-- An edge IS a node: kind=link at <source-path>.lnk.<uuid>, body
-- {to, kind, label?}. "from" is positional (the parent path), never in
-- the body. This migration adds the index that makes edge queries
-- index-driven instead of scans:
--
--   - out-links(path): a children-by-level read (path <@ source AND
--     nlevel = nlevel(source)+1 AND kind='link') — already covered by
--     the ltree GiST path index.
--   - backlinks/edges-to(needle): payload->'body'->>'to' = needle over
--     kind='link' — covered by the partial btree below.
--
-- Edges dangle BY DESIGN (branching-interaction): to is stored verbatim
-- and resolved at read time against the reader's lens; an unresolved
-- edge is dangling data, never an error state. No foreign keys.

CREATE INDEX IF NOT EXISTS tree_nodes_link_to_idx
  ON tree_nodes ((payload->'body'->>'to'))
  WHERE kind = 'link';

INSERT INTO schema_migrations (version, description)
SELECT 26, 'tree-edges: partial index on link edge targets (payload body to, kind=link)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 26);
