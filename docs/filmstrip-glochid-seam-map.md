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
Of those, **10 redefine framework verbs**:

| redefined define | what it is | legit? |
|---|---|---|
| `filmstrip/apps` | app registry | **YES** — the app extension point |
| `filmstrip/browse` | cascade dispatch to apps | **YES** — the app extension point |
| `filmstrip/tree-roots` | rail content roots | **YES** — app content |
| `filmstrip/ws-node` | workspace row read | NO — storage seam copy |
| `filmstrip/ws-save` | workspace row write | NO — storage seam copy |
| `filmstrip/save-all` | workspace row upsert | NO — storage seam copy |
| `filmstrip/key-sentinels` | keyboard registry | NO — registry wholesale-copied |
| `filmstrip/shelf-oob` | shelf OOB re-render | NO — framework internals |
| `filmstrip/winbox-oob` | windows-tree OOB | NO — framework internals |
| `filmstrip/pane-stamp` | pane DOM stamping | NO — framework internals |
| `filmstrip/oob-content-strip` | content-strip OOB | NO — framework internals |

(11 rows; apps/browse/tree-roots are the 3 legitimate.)

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

1. **Storage seam** — one knob, not three copied verbs:
   `(filmstrip/store-backend (lambda (row->json) ...))` habitat = tree
   prims/Postgres; glochid = fs prims/file. `ws-node`/`ws-save`/
   `save-all` call THROUGH it.
2. **App registry verb** — `(filmstrip/register-app! name browse-fn)`
   replacing the apps/browse wholesale redefinition (idempotent; idem
   re-load safe).
3. **Keyboard registry verb** — `(filmstrip/register-keys! rows)`
   appending sentinels instead of copying the registry.
4. **OOB hook table** — shelf/winbox/content-strip re-renders become
   framework internals with app-supplied *content*, not redefined bodies.
5. **Local-prim purge** — `dict-set*` and `request-param` shims copied
   inside the embedded source belong in the shared library floor
   (b164c1f already did this for the vault; same move for glochid).

## 4. Proof the fork bites twice

hollis's live-filmstrip repair (2026-10-12..14): the year-column entry
shape walked bare-string → dotted-pair → 2-list before the runtime
accepted it. FOUR commits on habitat. Glochid's embedded copy still
carries the old shape — this class of fix must now re-land there by
hand, or is silently wrong there. One source makes the bug class
structurally impossible.

## 5. Suggested divide of labor

- gregor (already in glochid/): grow the seams in `libs/filmstrip`,
  migrate glochid to consume them, delete the 10 redefinitions, pull
  the desktops feature back into the framework.
- hollis: keep this map current as the refactor lands; sweep the board
  cards whose bodies cite the old "two residences" framing; watch
  glochid's copy of the year-entry shape when surface.ml regenerates.
