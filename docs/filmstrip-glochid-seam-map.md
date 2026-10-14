# Filmstrip / glochid seam map

**Filed:** 2026-10-14 (hollis, at roerick's direction).
**Model locked by roerick, supersedes the "two residences" framing of 2026-10-01:**

> FILMSTRIP = the desktop UI framework. Rail, panes, pane-aware routes,
> workspace rows, OOB swaps. Built in htmx. A desktop that apps load into.
> GLOCHID = the agent harness. An APP which loads inside filmstrip.
>
> The fog exists because both were grown simultaneously inside lens
> (the local-first evaluator), so framework internals got hand-copied
> into the harness's embedded source instead of loaded once.

**Target end-state:** `libs/filmstrip` is the ONE authored filmstrip.
Glochid loads it and consumes named seams. No filmstrip/* definition
inside glochid's embedded source. Both the habitat (`/filmstrip`) and
the harness `GET /` render from the same body.

---

## 1. The fork, quantified

`glochid/lib/surface.ml` embeds `glochid_source` (1926 lines, 92 defines).
Of the 11 verb redefinitions in `glochid/lib/surface.ml`, **8 are redefs to delete** (3 are legitimate app extension points):

| redefined define | verdict | fate |
|---|---|---|
| `filmstrip/apps` | **legit** | becomes seam: `filmstrip/register-app!` |
| `filmstrip/browse` | **legit** | dispatched by the framework, app supplies handlers |
| `filmstrip/tree-roots` | **legit** | app content, framework calls it |
| `filmstrip/ws-node` | NO | dies into the storage seam (read leg) |
| `filmstrip/ws-save` | NO | dies into the storage seam (write leg) |
| `filmstrip/save-all` | NO | dies into the storage seam (write leg) |
| `filmstrip/key-sentinels` | NO | dies into `filmstrip/register-keys!` |
| `filmstrip/shelf-oob` | NO | framework body + app-supplied content |
| `filmstrip/winbox-oob` | NO | framework body + app-supplied content |
| `filmstrip/pane-stamp` | NO | framework body (host hook already exists) |
| `filmstrip/oob-content-strip` | NO | framework body (region-move answer) |

(Counts the ledger the way it consumes: **11 rows, 8 redefs to delete**, 3 promoted to seams — gregor recount 2026-10-14, matches `surface.ml` define grep: every row above is a `(define …)`; no other filmstrip/ verb is overridden.)

And the reverse direction: **desktops were born in the app, not the
framework.** `glochid/deskt`, `glochid/desk-sentinel`,
`glochid/first-on-desk`, `glochid/first-region-on-desk`,
`glochid/desktop-set`, `glochid/heal-focus`, `glochid/windows-tree`,
`glochid/windows-pane-node`, `glochid/keep-nodes` exist ONLY in glochid.
When the fork is retired, these migrate BACK into `libs/filmstrip`
(desktops are a desktop-framework feature).

## 2. What glochid legitimately owns at app level

All of these stay in glochid, consuming framework seams instead of
replacing framework bodies:

- harness surface: `glochid/shell*`, `glochid/content-area`,
  `glochid/panes-region`, `glochid/rail-box`, `glochid/shell-doc`
- harness apps/trees: `glochid/code-tree`, `glochid/fs-*`,
  `glochid/prims-tree`, `glochid/ui-tree`, `glochid/new`, `glochid/resume`
- harness chat templates: `glochid/base-css` (theme), `glochid/tg`,
  `glochid/css-text`, the `ui/*` + `lookbook` declarations
- keyboard CONTENT: `glochid/shortcuts` rows (verbs stay framework)

## 3. Proposed framework seams (what libs/filmstrip must grow to be loadable
by both hosts)

1. **Storage seam** — a read+write **pair**, not a single knob (a single
   `row->json` is half a knob: `ws-node` is a READ, `ws-save`/`save-all`
   are writes; the shapes are carried by two pods or one two-leg seam):
   `(filmstrip/store-backend (lambda (read-row) (lambda (save!row upsert!row))) )`
   habitat = tree prims/Postgres; glochid = fs prims/file.
   `ws-node` calls the read leg; `ws-save`/`save-all` call the write legs.
2. **App registry verb** — `(filmstrip/register-app! name browse-fn)`
   replacing the apps/browse wholesale redefinition (idempotent; idem
   re-load safe).
3. **Keyboard registry verb** — `(filmstrip/register-keys! rows)`
   appending sentinels instead of copying the registry.
4. **OOB hook table** — shelf/winbox/content-strip re-renders become
   framework internals with app-supplied *content*, not redefined bodies.
5. **Local-prim purge** — `dict-set*` and `request-param` shims copied
   inside the embedded source sit in the shared library floor
   (b164c1f already did this for the vault; same move for glochid).

## 3a. Two seams reread by gregor 2026-10-14 (checklist before anyone cuts)

These two belong kernel/boot side and MUST be in place before or
with the redef deletion — a bare delete is not sufficient:

- **Load-order seam.** Deleting the embedded redefs is necessary but not
  sufficient unless the runtime guarantees framework-before-app at boot.
  Cheapest structural guarantee: `filmstrip/register-app!` (seam 2)
  **refuses** to run until every framework verb it knows is bound, then
  idempotently binds. A shadow-return becomes structurally impossible
  once `surface.ml` regenerates, instead of depending on boot order
  staying correct by hand.
- **Kernel-datum-type checklist line** (the `filmstrip-ws` class, live
  on prod twice now): when the desktops feature back-migrates into
  `libs/filmstrip`, **declare the desktops datum types kernel-side
  before the first migrated write** — same class as the undeclared
  `agent-memory`/`agent-agenda` on prod today (typed-boundary reject,
  silent discard), and the `ws-init!` guarded-boot-seed (2c9e65d) is
  the precedent to copy for each desktops datum type.

## 3b. Test re-home (rides the bodies)

The framework test groups living in `glochid/test/glochid_test.ml`
surfaces 0-8 — rail-trees, move-to-desktop, lookbook, habitat-panes,
keyboard-shortcuts, bindings-browser — migrate WITH the bodies as each
redef retires. That also clears the 10-failure glochid baseline
(pre-existing at HEAD 2026-10-14); do not silently absorb those
failures into the migration.

## 4. Proof the fork bites twice

hollis's live-filmstrip repair (2026-10-12..14): the year-column entry
shape walked bare-string → dotted-pair → 2-list before the runtime
accepted it. FOUR commits on habitat. Glochid's embedded copy still
carries the old shape — this class of fix must now re-land there by
hand, or is silently wrong there. One source makes the bug class
structurally impossible.

## 5. Suggested divide of labor

- gregor (already in glochid/): grow the seams in `libs/filmstrip`,
  migrate glochid to consume them, delete the 8 redefs, pull the
  desktops feature back into the framework.
- hollis: keep this map current as the refactor lands; sweep the board
  cards whose bodies cite the old "two residences" framing; watch
  glochid's copy of the year-entry shape when surface.ml regenerates;
  **per-verb habitat e2e on each redef retirement** (main-strip-v5
  gate style — verbs enumerated in section 1's table, one pass per
  retirement, recorded in the journal like 2026-10-13's regate).
