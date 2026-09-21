# One-tree userland cutover — loop outline

Purpose: migrate every product package's Nopales source off the old
`data_rows` projection prims (`get create update update-field delete rows
find-row find-rows find-text find-random select-for-update`) onto the
tree-native prims (`tree/…` + `node-path`/`type-fields`), one package per
agent iteration, until a global grep finds zero old-family call sites and
the translation layer (`crud.ml` old family + scaffolding mirrors) can be
retired as a follow-up.

Baseline audit: 2026-08-09 (snapshots of live `system/main` +
`admin/personal`; method at bottom). Tree cutover itself landed
2026-07-26 — old prims still work, they write *through* to `tree_nodes`
via the reverse mirror. Nothing here is broken; this is convergence work.

## How the loop works

- Do **one checkbox per pass**, top to bottom. Do not batch packages.
- Everything happens on the **live habitat** via eval; git is only the
  vault you sync at the end.

```bash
set -a; . ~/.config/pricklypear/env; set +a
PP=_build/default/cli/bin/pp_eval.exe
"$PP" -j '(checkout "system/main")'      # sanity: where am i
```

### Per-package recipe

1. **Branch**: work on `admin/dev` (create with `(fork "admin/dev")` if
   missing, then `(checkout "admin/dev")`). Never define/route directly
   on `system/main`.
2. **Probe shapes before rewriting** (return shapes differ — see
   gotchas). Run each once and record what you see:
   ```lisp
   (load-library "<pkg>")
   (tree/list "data.<type>" 1 5)            ; list-children shape
   (tree/get (node-path '<type> "<an-existing-id>"))
   (tree/find-by "datum" "<field>" "<value>" 5)
   ```
3. **Rewrite** call sites live (redefine the function on your branch;
   the prim-call body changes, signatures/routes stay). Read current
   source first: `(function-doc "<pkg>/<fn>")` or pull the package as in
   step 6 and edit against that.
4. **Smoke on-branch**: exercise the app's real paths — list, view one,
   create, update/toggle, delete — via eval of the package's fns or its
   routes with curl. Confirm data parity:
   `(count (rows '<type> 5000))` before == after (old prim still reads
   the projection; both should agree).
5. **Obituary rule**: before replacing an implementation, add one line
   to the `spec/<pkg>` notes doc DECISIONS section — what/why.
6. **Promote** to `system/main` per the habitat-workflow ritual
   (`pp_sync land --from admin/dev`, or current promote UX), then vault:
   `_build/default/cli/bin/pp_sync.exe pull <pkg>` and commit the
   `libs/<pkg>/` change.
7. **Out-of-scope discoveries** → one line to habitat `log/issues`.
   Do not fix them mid-loop.
8. End-of-pass: grep the package again (live snapshot or eval); old
   count must be 0 (unless explicitly exempted below), then hand off
   ~3 lines to habitat `log/sessions`.

### Grep (authoritative check against live)

```bash
_build/default/cli/bin/pp_sync.exe snapshot --branch system/main --out /tmp/pp-audit
cd /tmp/pp-audit && grep -rEno '\((get|create|update|update-field|delete|rows|find-row|find-rows|find-text|find-random|select-for-update)[\s\"\x27)]' <pkg>/
```

### Gotchas (learned from the kernel, verify once per loop)

- **Return-shape change**: old prims return the *row body* JSON;
  `tree/get`, `tree/find-by`, `tree/list-kind` return the full *node*
  JSON (`{"v":1,"kind":"datum","path":…,"type":…,"body":{…},…}`).
  Consumers need one extra `(dict-get node "body")` hop.
- **`tree/upsert` returns a path string on success, an error string on
  failure** (both plain VStr). Detect errors by content, not type.
- **Create placeholder**: last path label `""`, `"_"`, or `"new"`
  triggers uuid generation — `(tree/upsert "data.todo-item.new" "datum"
  "todo-item" fields-json)` is the `(create …)` swap.
- **Typed boundary**: declared types require ALL fields on every
  upsert, same as `create`/`update` today — no partial updates; use
  `(get …)` + merge + upsert to emulate `update-field`.
- `tree/list prefix [depth] [limit]`; `tree/find-by kind field value
  [limit] [prefix]` — for flat data, kind is `"datum"`. find-by is
  garden-scoped by default (i-1788348645); an explicit `prefix` opts out
  to a wider subtree, `""` = whole store.
- Old-family call sites in grep output include false positives: local
  vars/params named `rows`, docstrings, comments. Check each site;
  the maps below mark known ones.

## Tasks

- [x] **1. todo** — DONE 2026-08-12. Actual count was **40 real sites**
  (the doc's "24" undercounted: it missed multi-line `create`/`update`/
  `update-field` calls). All migrated to the `todo/tree/*` wrappers
  already on `system/main` (`tree/get`/`rows`/`create`/`save`/`del`/
  `set-field` + `node-path`). `todo/tree/del` and `todo/tree/save`
  redefined to return the row **id** (preserving old prim return shapes:
  old `delete`/`update`/`update-field` returned id; `tree/delete` returns
  `1`, `tree/upsert` returns a path). Typed-boundary fix: added
  `"done" "false"` to the 3 `create-item` field lists (`todo-item` is a
  declared 7-field type: list_id title notes state due position done).
  `dict-set*` already emits native JSON types (`false`/`0`), so no data
  format change. Promoted to `system/main`; repo `libs/todo/` vaulted.
  Smoke green: list/get/create/cycle/toggle/save/delete + list CRUD +
  ensure-inbox. Data parity: tree == projection (8 items / 95 lists).
  **Pre-existing stray `todo/db/*` adapters** (11 defs, tree-native, from
  a killed prior iteration's partial work — already committed in the
  2026-08-12 repo HEAD and present on live `system/main`) remain on main;
  they are unused (call sites now use `todo/tree/*`) and tree-native so
  they don't trip the old-family grep, but should be removed in a
  follow-up (filed to habitat `log/issues`). **Blocks kanban.**

- [ ] **2. notes doc CRUD** — 8 live sites excluding `notes/migrate`
  (which is intentional archaeology tooling — **leave it alone**).
  Core slug path: `find-rows 'note "slug"` at `notes/lib.pp:795,
  :2450`, `notes/doc/lib.pp:200`; doc upsert `create`/`update 'note`
  at `notes/lib.pp:952-3, :4721-2`, `notes/doc/lib.pp:357-8`; deletes
  at `notes/lib.pp:513, :636, :1588`, `notes/doc/lib.pp:41`.
  `notes/tree`, `notes/ui` are already tree-native. Swap to
  `tree/find-by`/slug lookup + `tree/upsert` at the existing
  `data.note…` paths; do NOT move note bodies in this pass (garden
  hierarchy redesign is separate). Smoke: `/notes` list,
  `/notes?slug=agents` load, edit+save a doc, add+delete a scratch
  note. High-traffic path — smoke carefully.

- [ ] **3. edit** — 19 sites, all in `edit/lib.pp`. Types `edit-asset`,
  `edit-cue`, `edit-edl-entry`, `edit-line`, `edit-project`,
  `edit-shot`, `edit-shot-cast`, `edit-shot-ref`, `edit-take`. Mostly
  `(rows 't N)` index pages + `(get 't (as-str id))` detail pages.
  Mechanical.

- [ ] **4. forge** — 14 real sites; already uses `node-path` in 2 spots
  (:499, :669 — note :669 `(node-path)` no-arg call, verify arity).
  **Also rewrite the docstrings at `lib.pp:367-393` and :1466 — the
  forge primer teaches fresh agents the OLD prim API; it must teach
  tree prims or this migration regrows.** Types: `forge-flag`,
  `forge-graph`, `forge-node`.

- [ ] **5. bi** — 10 sites (`bi/lib.pp` 7, `bi/metric` 3). Types
  `bi-expense`, `bi-revenue`, `bi-metric-cache`. One `find-row` on the
  cache (:11). `(rows)` at lib.pp:372 is likely a var — check.

- [ ] **6. 9-call cluster** — one package per pass, mechanically
  similar: **workout** (`workout-exercise`, `workout-session`; sites
  at :172, :285, :298, :1749, :1954, :2052, :2169, :2403 — :1547 is a
  local var) · **weight** (`weight-log`; :10, :59, :158 — other
  `rows`es are lambda params/vars) · **flash** (`flash-card`,
  `flash-deck`; :57, :102, :110, :262, :314, :367, :557, :576, :815 —
  note personal branch has 3 MORE old sites than main; reconcile pins
  first) · **bible** (`bible-book`, `bible-verse`; uses `find-row`,
  `find-random`, `find-text` — `tree/find-by` covers find-row;
  find-text → `tree/search-text`; find-random has no tree analog yet —
  file to `log/issues` and keep one old call w/ note, or implement
  via list+random pick).

- [ ] **7. rb** — 5 sites: `(get 'recipe id)` :616, `(rows 'recipe)`
  :663/:1672, `(rows 'order)` :1311; `type-fields` at :1037/:1050 are
  already tree-flavored (keep). :2187 is a lambda param.

- [ ] **8. Long tail (3–4 calls)** — **wiki** (`wiki-event` rows/delete
  :92/:269 + `update-field 'note` :399-400 — wiki writes NOTE fields,
  coordinate with task 2) · **contacts** (:22/:35/:117) · **chat**
  (`chat-message`, `chat-room`; :5/:262/:306) · **calendar**
  (`calendar-event`; :146/:451 — :87 is a var).

- [ ] **9. Long tail (1–2 calls)** — **roam** (1 site on main, 4 on
  `admin/personal` — do this one ON personal and promote; note-link
  cleanup) · **bookmarks** (:5/:82/:85) · **signup** (:138 create) ·
  **plan** (:8, var-named-rows suspected :8 `(rows 'user-plan …)` is
  real) · **welcome** (:113 guestbook) · **data** (:7 generic
  `(get type-name id)` wrapper for the /data browser — decide: tree-ify
  or retire with the browser page; flag to `#roerick` judgment) ·
  **spec** (:188 — check if var) · **verify** (:288 `find-rows
  'note-link` — test suite; keep ONE deliberate old-family test until
  the translation layer retires, mark it, rewrite the rest).

- [ ] **10. kanban** — 7 sites, all `(get 'todo-list'/'todo-item id)`
  reads of todo's storage (:241, :271, :486, :720, :743, :766, :771) +
  5 `node-path` uses already correct. **Do after todo (task 1) lands**
  so the shape contract both sides agree on is tree-native.

- [ ] **11. _root promotion** — 3 tree calls exist on `admin/personal`
  only. Just promote; no rewrite.

- [ ] **12. Global verify + closeout** — fresh snapshot of
  `system/main`; old-family grep returns only `notes/migrate` +
  the marked `verify` retention + `bible` find-random (unless fixed);
  `borge drift`; update `borge report` status; post `log/sessions`;
  open the follow-up issue "retire old-family prim translation layer +
  drop scaffolding mirrors" (NOT in scope here).

## Done definition (package level)

Old-family grep = 0 (or exempted with note), smoke paths green, spec
DECISIONS entry written, promoted to `system/main`, `libs/<pkg>/`
vaulted and committed, `log/sessions` handoff posted.

## Baseline audit method (2026-08-09, for re-running)

`pp_sync.exe snapshot --branch system/main --out /tmp/pp-audit-main`
(and `admin/personal`), then per package count
`\((get|create|update|update-field|delete|rows|find-row|find-rows|find-text|find-random|select-for-update)[\s"')]`
vs `\(tree/[a-z-]+`. Prim registry of record:
`image/lib/prims_db.ml` (`register_crud …`).
