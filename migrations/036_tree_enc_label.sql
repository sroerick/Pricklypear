-- 036_tree_enc_label.sql — one-tree label codec: strip the MIME newline that
-- PostgreSQL's encode(...,'base64') inserts every 76 output characters.
--
-- BUG fixed here: tree_enc_label (009) returned a label containing a literal
-- newline for any input whose base64 is long enough to wrap — PG's base64 is
-- MIME, wrapping at 76 chars and adding a trailing newline at exactly 76. That
-- is any raw input of >= 57 bytes. ltree labels admit [A-Za-z0-9_] only, so the
-- '::ltree' casts in the 009 mirror triggers (and every caller below) raised
-- for names/routes/types >= 57 bytes: a define with such a name aborted at
-- COMMIT (the mirror is a deferred constraint trigger), the "marcus create
-- crash". The OCaml codec (image/lib/tree_store.ml enc_label) never wrapped, so
-- this restores the SQL side to the behavior the two-sided codec contract
-- requires. tree_dec_label already needs no change: PG decode(...,'base64') is
-- MIME-tolerant and ignores whitespace.
--
-- Additive + idempotent: CREATE OR REPLACE only, no data rewrite. No
-- newline-bearing label could ever have been stored — the ltree cast rejected
-- it before the row landed.

CREATE OR REPLACE FUNCTION tree_enc_label(s text) RETURNS text
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE
  b64 text;
  pad int;
BEGIN
  IF s IS NULL OR s = '' THEN RETURN 'x'; END IF;
  -- Defeat PG base64 MIME wrapping (LF every 76 chars) before anything else.
  b64 := translate(encode(s::bytea, 'base64'), E'\n\r', '');
  pad := (length(b64) - length(rtrim(b64, '=')));  -- 0,1,2 trailing '='
  b64 := rtrim(b64, '=');
  b64 := replace(b64, '+', '_');
  b64 := replace(b64, '/', '-');
  RETURN b64 || pad::text;   -- pad count disambiguates; ''->'x' handled above
END $$;

INSERT INTO schema_migrations (version, description)
SELECT 36, 'tree_enc_label: strip PG base64 MIME newline (>=57-byte names broke ltree paths)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 36);
