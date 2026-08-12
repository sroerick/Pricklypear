# i-rootbody-1

## BUG: notes/tree/save-outline drops root body

notes/tree/save-outline only writes children via write-heading; the root node body is never persisted. Any note with content only in the root body loses it on upsert. Workaround: put content in child nodes. Found 2026-08-04 during platform-ref refile. Affects tree-mode notes only (notes/tree/enabled? = true).

## Resolution

Root outline body is now persisted and reloaded so tree-mode notes with body
only at the root round-trip through `notes/doc/upsert` without loss.

**What changed:**
- `save-outline` now calls a new `notes/tree/write-root-body` (after
  `clear-headings`) which stores `(outline/body outline)` as a `"body"` key on
  the note node's own content (`tree/upsert note-path "note" "note" …`,
  merging into the existing note via `tree/get`). Writes headers via
  `write-heading` for children exactly as before (nil no-op if the note node
  is missing).
- `load-outline` reads the root body back with a new `notes/tree/root-body`
  helper (`tree/get note-path` → parse → `dict-get … "body"`), instead of the
  previously hardcoded `""`. Child headings loaded unchanged via
  `heading-kids`.
- Applied identically to both copies of the notes/tree code:
  `libs/notes/tree/lib.pp` (root) and `libs/notes/lib.pp` (second copy).

**Files touched:** `libs/notes/tree/lib.pp`, `libs/notes/lib.pp`.

**Verification:** `dune build` and `dune runtest` green; paren balance /
structure intact; `borge drift` shows only pre-existing unrelated items.
This is a Nopales seed-data change (not a `.borg`-declared OCaml module).

