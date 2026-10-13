# rail-trees-v4 — the rail becomes N trees over ONE renderer

Spec: `borge/filmstrip.borg` `subsection rail-trees-v4` (locked at make
start). Companion to window-controlbox-v3 (region/pin landed) — v4
re-forms the rail inventory as trees over one control.

## The contract

- **Tree = geometry rule over controlboxes.** Row grammar (acme split):
  leading fold arrow (own hitbox) · label word (open-or-focus, safe) ·
  trailing verb words (mutation POSTs). No third interaction — ever.
- **HATEOAS levels:** `GET /filmstrip/tree?root=<id>&node=<path>`
  answers ONE level as a `<li>` fragment. Splice into the open
  `<details>`' `.fs-tr-kids` — never re-render a level. First render of
  a box = head + level 0 only.
- **State lives in the DOM:** folds are native `<details>`; `hx-trigger
  ="toggle once" hx-target="find .fs-tr-kids" hx-swap="innerHTML"` on
  the dir details = fetch exactly once, re-fold never refetches. Zero
  JS beyond htmx attrs, zero server-side expansion state. Mode flips,
  desktop flips, shelving preserve it (sentinel `:has` hiding +
  hx-preserve).
- **Row contract (node dict, host-provided):**
  `label` · `kind` (dir|leaf) · `node` (path key) · `open` (route or "")
  · `verbs` = list of `(word route vals-json)`.
- **Roots contract:** `filmstrip/tree-roots` = list of
  `(id title levelfn)`; `levelfn: path -> list of node dicts`. Lib
  default: empty (features are host-provided, like browse/apps).

## Tasks

1. **lib** (`libs/filmstrip/lib.pp`): url-enc/url-dec (same six
   escapees), `filmstrip/tree-roots` default empty,
   `filmstrip/tree-verb`, `filmstrip/tree-node-li` (details/summary
   row + lazy-kids ul + eager actions fold for verbs),
   `filmstrip/tree-level`, `filmstrip/tree-box`, `POST /filmstrip/tree`
   (level fragment) + `POST /filmstrip/tree/collapse` (OOB box swap),
   tree CSS rules appended to `filmstrip/css`.
2. **glochid** (`agent/lib/surface.ml`): three roots —
   `windows` (targeting: desktops 0..2 dirs from the ws row + `shelf`
   subtree; labels POST desktop-switch; pane rows verbs
   focus/pin/width/close, shelf rows restore/destroy) ·
   `code` (the fs walk as a tree; leaf labels open /code?path panes) ·
   `prims` (bindings bridge: packages -> functions; leaf labels open
   /bindings detail panes). shelving the windows TREE pane keeps DOM
   via the ordinary pane shelf (hx-preserve).
3. **rail mode tabs + sentinel**: `#fs-railtabs` words (one per root)
   POST `/filmstrip/railmode`; persisted `railmode` in ws.fields;
   `#fs-railmode` sentinel class `rm-<id>`; `:has` shows only the
   active `.fs-rmode[data-rmode=…]` box. Windows tree REPLACES
   `#fs-winbox` + `#fs-shelf` (shelf = subtree): glochid's
   winbox-oob/shelf-oob redefinitions re-render `#fs-tree-windows` OOB
   so close/restore/destroy keep refreshing it; deskbox stays.
4. **probe**: `probe_shell_sem.mjs` batch C (T1 tabs swap visibly +
   preserve expansion · T2 lazy exactly-once, refold never refetches ·
   T3 label open-or-focus + verbs POST, no navigation · T4 collapse-all
   restores head state with one POST · T5 shelve → row under shelf
   subtree after re-expand · T6 zero inline styles) + re-point the
   v2-era winRows/shelfRows assertions to the tree boxes.
5. **tests**: agent_test — windows tree from a seeded ws (declared
   state), level endpoint fragment, lazy attrs, tree-route dispatch.
6. close: `dune build` + runtest + probe + `smoke_agent_surface.sh`,
   `borge drift`, stanza -> implemented + session note + REPORT here.

## Open seam (spec left it to make)

Outline-vs-drill selection: v1 ships OUTLINE only (splice), the drill
projection is a later garnish — the endpoint is already
projection-agnostic.

## Report (2026-09-13, CLOSE)

ALL TASKS SHIPPED. probe_shell_sem.mjs: ALL GREEN, 104 assertions
(I1-I8 + T0-T6); dune runtest --force every suite green (lisp 170,
agent 22 incl `rail-trees`, image suites); agent surface smoke updated
to the v4 contract and passing; `borge drift` structural: none;
stanza -> implemented (80.6%).

Mid-make discoveries (recorded in the stanza note):
1. **Substring is (s START LEN).** The per-char url-enc walk read
   growing slices; fixed to 1/2-char windows. (Named-define recursion,
   NOT letrec -- cleaner here anyway.)
2. **OOB-swapping the whole tree box div made htmx drop listeners** for
   subsequently spliced levels (0 XHRs on verb clicks after one box
   swap). The mutation refresh seam is now `hx-swap-oob="innerHTML"` on
   the level-0 UL (`fs-tree-<id>-root`): head never changes, and
   expansion state above mutated rows SURVIVES mutations -- an upgrade
   over the first draft, which reset expansion on every mutation.
3. **acme Get cross-desktop bug (latent v2/v3):** unshelving a pane
   whose row lives on another desktop re-showed it at its OLD desktop
   -> 0x0 focused pane (I1 violation reachable via the tree's global
   rows). open-apply's unshelve branch now passes through pane-stamp --
   same current-desktop stamp as fresh panes.
4. dict-set* refuses non-string values: node verbs ride JSON strings
   (json-emit at the host, json-parse in the renderer).
5. Pre-existing chat model <select> carried an inline style; replaced
   with `.fs-modelsel` (T6 at-rest zero-inline-styles now TRUE).

Deferred: app-contributed rail modes from pinned panes (the pinned
pane's own tree as a tab) -- the seam (tree-roots redefinition) is the
mechanism, the chat-contacts/calendar-month trees are content work,
not renderer work. Drill-down projection likewise deferred; outline
projection shipped.


## Postscript (2026-09-16, pane-trees amendment)

The open seam landed as the "kids" amendment rather than a drill
projection: `/code` now renders the SAME node rows the rail rides
(filmstrip/tree-node-li over the glochid/code-tree levelfn) -- one
template, two faces. Node dicts MAY carry `kids` (JSON string of a
pre-walked child level; eager dir, zero XHRs below the host threshold
-- glochid: 12 visible entries, 1 level, calibrated to the 400k step
request budget). Boot/head rail renders strip kids (filmstrip/
tree-lazy); splices and pane faces honor them. Latent v4 bug fixed
en route: label-word hx-vals were url-enc'd, which broke
filmstrip/open's split-query route matching for every query-bearing
route (fs leaves could never open from tree labels); vals are raw
now. code/rows + code/link deleted (obituary in borge/glochid.borg
filmstrip-shell, amendment note in borge/filmstrip.borg
rail-trees-v4). Full battery green: runtest, smoke, probe 115 OK.

## Postscript (2026-09-20, default-open desks + the move verb)

Operator note drove an amendment (stanza note in
borge/filmstrip.borg): node dicts MAY carry `expand` -- "true" renders
the dir's <details> already open; it RIDES kids (a parsed-open details
never fires its toggle-once pull). tree-lazy preserves eager kids on
expand-marked nodes (head + mutation OOB render them; budget stays
host policy); tree-collapse answers a NEW tree-closed projection so
collapse-all still folds everything. glochid's windows tree pre-walks
desk/0..2 kids + the expand seed (shelf stays lazy). Cross-stanza
window-controlbox addition: POST /filmstrip/move-to (`id` + optional
`d`; no `d` = the pane-head ⇢ glyph -- cycle the filmstrip/desk-count
ring) re-desks a pane record with close-heal focus discipline; tree
rows carry explicit to0/to1/to2 words (current desk skipped, rail
panes none/refused). Gates: agent_test rail-trees extended + new
move-to-desktop case; probe_shell_sem.mjs batch M (168 OK -- and the
I3 driver now rides the page's htmx, since an eager desk never
refetches its level); runtest + smoke green; borge drift none.
