-- 006_route_visibility.sql
-- Additive: public|private flag on user routes (auth.route-visibility).
-- Existing rows stay private via DEFAULT.

ALTER TABLE view_bindings
  ADD COLUMN IF NOT EXISTS visibility text NOT NULL DEFAULT 'private';
