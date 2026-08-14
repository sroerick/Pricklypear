# Fix-Loop: log/issues (7 items, pulled from habitat note `log/issues`)

You are fixing 7 issues filed in the habitat `log/issues` notes doc. Each is
mirrored as a `.md` in this directory; `INDEX.md` lists them. Work them in
priority order below. **Bugs first**, then smells/cleanup. One issue per
iteration unless a fix naturally bundles (e.g. the three `crud.ml` cleanup
items can share an edit pass).

## Project setup (do this FIRST, every fresh session)

```bash
cd ~/dev/wyo.tech/pricklypear
eval $(opam env --switch=poohstack --set-switch)        # REQUIRED before any build
set -a; . ~/.config/pricklypear/env; set +a            # PP_URL, PP_EVAL, PP creds
export PP_EVAL="${PP_EVAL:-$PWD/_build/default/cli/bin/pp_eval.exe}"
```

- Build: `dune build` at the root. Test: `dune runtest`.
- `lisp/` + `common/` tests need no services. `image/` tests need the switch.
- Dev stack (only if you need a live PG): `scripts/dev.sh start`
  (PG on 5433, image on 18080, REPL on 17878). Logs in `/tmp/pp-dev/`.
- Default prod habitat is `https://pricklypear.rocks` via the env file —
  prefer eval there for inspection; do NOT mutate prod without explicit ask.
  Local dev is `scripts/dev.sh` PG.
- **Workflow rules** (from `AGENTS.md`): work on a branch (`<user>/personal`
  or `<user>/dev` on the habitat, git branch in repo) — never edit `system/main`
  / `main` directly. Obituary rule: before replacing an impl, write what/why.
  After touching any `.borg`-declared module: `borge drift`. File (don't fix)
  anything out-of-scope → add a node to `log/issues` on the habitat.

## Key source files

- `image/lib/crud.ml` — CRUD prims: `get`, `list`, `find_row`, `find_rows`,
  `find_random`, `find_text`, `update_field`, `create`, `update`, `delete`.
  `visible_sql` is the inline SQL visibility filter (line ~40).
- `image/lib/tree_store.ml` — `seed_types` OCaml list (line ~98).
- `image/lib/store_db.ml` — `set_local_pp_user` (line 107); only called inside
  `with_transaction` / `with_dynamic_transaction` (lines 122, 139).
- `migrations/013_data_user_scoping.sql` — `tree_is_seed_type(t text)` SQL fn.
- `migrations/014_guestbook_shared.sql` — re-declares `tree_is_seed_type`.
- `image/lib/agent_tools.ml` — api_eval JSON emit (around line 690; `pp_value`
  is `Pricklypear_lisp.Reader.pp_value`).
- `image/lib/http_core.ml` — also uses `pp_value` for eval rendering (lines 231,
  1444, 1460).
- `libs/notes/tree/lib.pp` — `notes/tree/save-outline` (line 403),
  `notes/tree/write-heading` (line 449).

---

## Priority 1 — BUGS

### 1. `i-rls-nontx-2026-08-12` — RLS floor inactive on non-tx CRUD reads
**File:** `image/lib/crud.ml`, `image/lib/store_db.ml`
**Problem:** `set_local_pp_user` is only called inside `with_transaction` /
`with_dynamic_transaction` (store_db.ml 122, 139). All six CRUD read prims
(`get`, `list`, `find_row`, `find_rows`, `find_random`, `find_text`) use
`Io_lwt.Db.execute` directly — no tx, no `pp.user` set. So the `tree_data_rows`
view confinement and the RLS policy on `tree_nodes` are both **no-ops** on
reads. `visible_sql` (the inline SQL WHERE) is the SOLE enforcement; if it's
broken/removed, users read everyone else's data.
**Fix:** Ensure `pp.user` is set on every connection use, not only inside tx.
Cleanest: run reads through a lightweight tx too, OR set
`set_local_pp_user` on the connection before every read (and reset after).
Prefer routing reads through `Store.with_transaction` / a read-only tx helper
so the existing `set_local_pp_user` call applies uniformly — that also gives
SERIALIZABLE isolation and makes the fix single-site. Verify the `pp.user`
GUC actually lands (check `store_db.ml:107` `set_local_pp_user`).
**Acceptance:**
- A read with `visible_sql` neutered (temporarily) still returns only the
  caller's rows + shared seed types — i.e. RLS/tree confinement now enforces
  it, not `visible_sql`.
- `dune runtest` green.
- `scripts/verify_one_tree.sh` still green if PG dev is up.

### 2. `i-updatefield-race-2026-08-12` — update-field lost-update race
**File:** `image/lib/crud.ml` (`update_field`, line 233)
**Problem:** `update_field` does `get` → `dict-set` → `update` as **two
separate SQL statements** with no transaction. Two concurrent `update-field`
calls on the same row race: both read same base, both write, second clobbers
first. The comment in the function acknowledges this.
**Fix:** Wrap the read-modify-write in `Store.with_transaction` and add
`SELECT ... FOR UPDATE` on the row inside the tx so the lock spans the body.
Use the existing `with-transaction` Nopales prim / `Store.with_transaction`
OCaml helper. Make sure the `set_local_pp_user` from issue #1 still applies
(once #1 routes reads through tx, this composes).
**Acceptance:**
- Concurrent `update-field` calls on different fields of the same row both
  land (no lost update). Add an alcotest or a script if feasible.
- `dune runtest` green.

### 3. `i-rootbody-1` — notes/tree/save-outline drops root body
**File:** `libs/notes/tree/lib.pp` (`notes/tree/save-outline` line 403,
`notes/tree/write-heading` line 449)
**Problem:** `save-outline` only writes children via `write-heading`; the
**root node body is never persisted**. Any note with content only in the root
body loses it on upsert. Affects tree-mode notes only
(`notes/tree/enabled? = true`). Found 2026-08-04.
**Fix:** Persist the root node's body in `save-outline` (in addition to
iterating children). Look at how `write-heading` writes a child node's body
and apply the same path to the root node. Verify with a tree-mode note that
has only root-body content — round-trip it through upsert and confirm the
body survives.
**Acceptance:**
- A tree-mode note with body only at root round-trips through
  `notes/doc/upsert` without losing the root body.
- `scripts/verify_notes_tree.sh` green (if PG dev up).

---

## Priority 2 — DESIGN / SMELL

### 4. `i-fields-as-string-2026-08-12` — CRUD returns fields as VStr of JSON text (triple-escaping in api/eval)
**Files:** `image/lib/agent_tools.ml` (api_eval JSON emit ~line 690),
`image/lib/crud.ml` (CRUD read prims), `image/lib/http_core.ml` (pp_value use)
**Problem:** All CRUD read prims do `SELECT fields::text` and wrap in VStr.
That JSON string gets Lisp-printed with backslash escaping (`pp_value`), then
JSON-encoded again by api_eval → **triple-escaped** output for every quote in
field data. Also: `create` docstring says the Lisp layer has no dict/assoc
value type, but `dict-set`/`dict-get`/`json-parse`/`json-emit` all exist now.
**Fix (two parts):**
1. `api_eval` should return the value as **native JSON**, not a `pp_value`
   string — i.e. serialize the Lisp value to JSON directly instead of
   Lisp-printing then JSON-encoding the printed string. Touch
   `agent_tools.ml` ~line 690 (and check `http_core.ml` 231/1444/1460 for
   consistency — those are the browser REPL render path, may intentionally
   stay `pp_value` for human display; only the JSON api path needs native JSON).
2. CRUD prims should `json-parse` `fields` into structured Lisp values at the
   DB boundary so downstream code gets a dict, not an opaque JSON string.
   Coordinate with the existing `dict-get`/`dict-set`/`json-parse` prims —
   decide whether to return a VDict (new) or a parsed JSON-backed structure.
   This is a DESIGN change — **write a short DECISIONS note in the habitat
   `spec/crud` (or `spec/notes`) notes doc** per the obituary rule before
   changing the return shape, since callers depend on the current VStr shape.
**Acceptance:**
- A CRUD read through `POST /api/eval` returns field data with quotes **not**
  triple-escaped (round-trips as valid JSON).
- Existing callers that did `(dict-get row "field")` still work (or are
  migrated in the same change).
- `dune runtest` green; agent smoke `(get 'todo <id>)` returns clean JSON.

### 5. `i-findrows-orderby-2026-08-12` — bible-specific ORDER BY in generic find-rows
**File:** `image/lib/crud.ml` (`find_rows`, line 341)
**Problem:** `find_rows` hardcodes
`ORDER BY COALESCE((fields->>verse)::int, 0) ASC, created_at ASC`. The
`verse` sort is domain knowledge about bible-verse data baked into the
generic CRUD layer. Works for other types (degrades to `created_at`) but
couples CRUD to the bible schema.
**Fix:** Add an optional sort parameter to `find_rows` (e.g.
`(find-rows 'type "f" "v" ... [limit] [sort])`), OR move verse-ordering into
the bible/chapter package. Prefer an optional sort param so the generic
layer stays domain-free. Default to `created_at ASC` when no sort given.
**Acceptance:**
- `find_rows` no longer mentions `verse` in `crud.ml`.
- Bible verse listing still sorts by verse (via the new sort param, set by
  the bible/chapter caller).
- `dune runtest` green.

### 6. `i-seed-list-3copies-2026-08-12` — seed type list in 3 places
**Files:** `image/lib/crud.ml` (`visible_sql` ~line 40),
`image/lib/tree_store.ml` (`seed_types` line 98),
`migrations/013_data_user_scoping.sql` (`tree_is_seed_type`)
**Problem:** Seed types hardcoded in 3 places, all must agree. Migration 014
already showed drift risk (`guestbook_entry` was missing from the SQL fn
after 013 shipped). Adding a shared type means updating all three.
**Fix:** Derive `visible_sql` from `tree_is_seed_type` (call the SQL fn) OR
from a single OCaml constant (`Tree_store.seed_types`) — collapse to ONE
source of truth. Prefer the SQL fn as authority (it's what RLS uses) and have
`Tree_store.seed_types` query/derive from it, with `visible_sql` calling
`tree_is_seed_type(type_name)` in its WHERE. Verify all three currently list
the same 5 types before refactoring: `bible-verse`, `bible-book`,
`bible-interlinear`, `strongs-entry`, `guestbook_entry`.
**Acceptance:**
- Only one place holds the seed-type list (the others derive from it).
- A grep across the repo shows no other hardcoded seed-type literal arrays.
- `dune runtest` + `scripts/verify_one_tree.sh` green.

### 7. `i-stale-comments-2026-08-12` — stale comments in crud.ml
**File:** `image/lib/crud.ml` (`data_rel` + module header)
**Problem:** `data_rel` comment says reads go through `data_rows` but the
fn returns the `tree_data_rows` view. References `PP_TREE_READS=1` as
conditional but code unconditionally returns the view. Says writes always
hit `data_rows` but writes go through `Tree_store` to `tree_nodes` (reverse
trigger mirrors to `data_rows` — opposite direction). Module header has
similar stale text. Mid-refactor leftovers.
**Fix:** Update the `data_rel` comment and module header to match current
reality: reads = `tree_data_rows` view (unconditional), writes =
`Tree_store` → `tree_nodes` (trigger mirrors to `data_rows`). Drop the
`PP_TREE_READS` conditional language. Pure comment fix — no behavior change.
**Acceptance:**
- Comments match the code. No behavior change. `dune build` green.

---

## Per-iteration checklist (do for EACH issue)

1. `eval $(opam env --switch=poohstack --set-switch)` — MANDATORY.
2. Re-read the issue `.md` in `log/issues/` and the cited source lines.
3. Make the edit. Keep changes minimal and surgical.
4. `dune build` — must pass.
5. `dune runtest` — must pass (run the subset relevant if full is slow, but
   prefer full).
6. If the issue touches a `.borg`-declared module: `borge drift`.
7. For DESIGN changes (issue #4, #6): write a DECISIONS node to the habitat
   `spec/<pkg>` notes doc per the obituary rule BEFORE changing shape.
8. Commit with a clear message referencing the issue id
   (e.g. `fix(crud): set pp.user on non-tx reads (i-rls-nontx-2026-08-12)`).
9. Update the issue's `.md` here with a `## Resolution` section (what changed,
   files touched, verification). Do NOT delete the issue file.
10. If you discover a NEW problem mid-fix: add a node to the habitat
    `log/issues` note (one line) — file, don't fix.

## Done gate (after all 7)

- `dune build && dune runtest` green.
- `borge drift` clean (if any `.borg` module touched).
- All 7 `.md` files in `log/issues/` have a `## Resolution` section.
- Write a 3-line handoff to the habitat `log/sessions` note summarizing what
  was fixed and what (if anything) was filed as new.

## Notes / gotchas

- **`opam env` is the #1 failure** — without the poohstack switch the build
  silently uses the wrong packages. Always run it first.
- Issue #1 and #2 compose — fixing #1 (route reads through tx) makes #2's
  `SELECT FOR UPDATE` trivial since a tx is already open. Consider doing #1
  first, then #2 in the same pass.
- Issue #4 is the biggest/riskiest (changes the api_eval return shape). If
  time-boxed, do #1/#2/#3/#5/#6/#7 (the safe ones) first and leave #4 for a
  dedicated pass — it's a DESIGN change, not a quick fix.
- Do NOT mutate the prod habitat (`https://pricklypear.rocks`) without an
  explicit ask. Use local `scripts/dev.sh` PG for verification.
- Do NOT dump `(list-bindings)` into context — use `describe` / `apropos-docs`.
