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

### 1a. What the embedded copy has that the habitat copy does not
(gregor visual confirmation 2026-10-14; greps re-verified by hollis on
master — this drives the desktops/rail-mode/rail-tree/omarchy-theme
migration back into `libs/filmstrip` framework slots)

1. **Theme: omarchy dark** (`surface.ml` ~38-54, "HOST SEAM 3" comment
   at ~721). Near-black navy `#0f1117` background, panels `#12141c` /
   `#1b1f2a`, text `#d8dee9`, muted `#6b7080`, accent `#7aa2f7`, status
   green `#2f9e44` with orange `#e8590c` / red `#c92a2a` alerts. Palette
   resolves live from the omarchy theme's `colors.toml` via `glochid/tg`
   with a pre-theme fallback for omarchy-less boxes. NOT in
   `libs/filmstrip`.
2. **Desktops**: a single `#ws-desk` sentinel node with classes
   `dsk-0`..`dsk-9` carries the current desktop; panes carry
   `data-desktop`; hiding is pure CSS `:has` (surface.ml ~129-140:
   `.fs-shell:has(#ws-desk.dsk-N) .fs-pane[data-desktop]:not(...)`) —
   open windows tree per desktop, active-desktop switching, rail docked
   on every desktop, no JS anywhere.
3. **Rail modes**: `#fs-railmode` sentinel (surface.ml ~198-201: the
   SAME `:has` hiding trick as desktops) over `data-rmode`
   windows/code/prims/ui, with `rm-*` axis classes on `fs-sh-axis`;
   rail panes carry `data-region="rail"`, dock on every desktop, and
   the focused rail pane keeps the RING, not a lifted window.

**Grep markers for porters:** grep `#fs-railmode` and `#ws-desk` when
these land in `libs/filmstrip`. Do NOT grep `fs-tree-windows`: it
exists only as a term inside TWO `surface.ml` COMMENTS (~1389, ~2060)
documenting the lib's own mutation-hook OOB targets — there is no
rendered `fs-tree-windows` markup in the embedded shell.

### 1b. Boot-coupling guard (gregor caveat 2026-10-14, reproed by hollis)

The standalone glochid bin built from master **crashes at boot**:
`boot-load: unbound: type-fields`, fatal Interp_error — reproduced
here (`dune build glochid/bin/main.exe` then `--serve` on an empty
cwd).

Root-cause chain (owned — 2c9e65d is mine): the app embeds CURRENT
`libs/*/lib.pp` as boot source (llm/embed_rules.dune.inc;
surface.ml ~2739-2748 boot-loads html, css, hyper, FILMSTRIP, lookbook,
then glochid layer "whose redefs must win over the filmstrip defaults
via env late binding", then the surface). The filmstrip lib's
TOP-LEVEL `(filmstrip/ws-init!)` (libs/filmstrip ~54 — the 2c9e65d
guarded zero-field seed) calls the KERNEL prims `type-fields` /
`deftype`, which the app prim set does not ship — so the app boot dies
on filmstrip load. On the HABITAT it passes because the kernel ships
those prims.

So: **any lib top-level calling a kernel-only prim breaks every
non-kernel host that embeds the lib as boot source.** Checklist with
the desktops/omarchy migration (the §3a register-app! load-order guard
is the verb-level twin of this line):

- **Framework lib code must be boot-loadable under an app prim set.**
  Both of gregor's named remedies are live: peel the runtime seed
  (`filmstrip/ws-init!` toplevel call) out of `lib.pp` and invoke it
  from the kernel boot/reseed where habitat boot seeds already run, or
  gate the call on prim availability (per-lib presence-guard that
  no-ops "host cannot seed"). Until one lands the app cannot boot from
  master and nobody has a live vNext surface to eyeball.
- Per-type datum declarations (the desktops types post-migration) ride
  the §3a kernel-datum-type line: declared kernel-side BEFORE the first
  migrated write — never as a lib top-level side effect.

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

## 3. Proposed framework seams (status after phase-1 cut-over, fe6df36)

1. **Storage seam — LANDED (4a0e0d5, deployed fe6df363ac99).** A
   read+write **pair**, not a single knob: `filmstrip/store-backend`
   carries both legs; habitat = tree prims/Postgres; glochid = fs
   prims/file. `ws-node` calls the read leg; `ws-save` calls the write
   leg. **Parked divergent: the embedded `save-all`** — its copy carries
   railmode/boxrows/axis/desktop carry-forward the framework save-all
   lacks, so it stays in place and rides the storage write leg in
   phase 2 (the copy's extras must be reconciled, not silently dropped).
2. **App registry verb** — `(filmstrip/register-app! name browse-fn)`
   replacing the apps/browse wholesale redefinition (idempotent; idem
   re-load safe). PENDING — rides phase 3 with the §3a refusal guard.
3. **Keyboard registry verb — LANDED (4a0e0d5).**
   `(filmstrip/register-keys! rows)` appends sentinels; the embedded
   `key-sentinels` copy is gone and glochid appends its own shortcut
   rows at boot. Basic confirmed: glochid runtest holds the pre-existing
   baseline of 10 (the 3b re-home clears them, not p1).
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

---

## 6. Deprecation gate (the fork is dead when)

"Deprecate the embedded copy" is DONE when all five hold; until then it
is in managed retreat, not deprecated:

1. GREP GATE: zero `(define (filmstrip/...` in glochid's embedded
   source (surface.ml + the embedded boot source), except the three
   promoted seams expressed as app-supplied handlers (register-app!,
   browse content, tree-roots content).
2. REFUSAL GUARD LIVE (§3a): filmstrip/register-app! refuses until
   every framework verb it knows is bound, so a shadowing redef is
   structurally impossible rather than merely deleted.
3. SAME-BODY RENDER: habitat /filmstrip and harness GET / render from
   the SAME embedded libs/filmstrip/lib.pp at the same git rev. The
   build-time embed (llm/embed_rules.dune.inc) is the version pin:
   framework updates propagate through pull/build/sync, never through
   hand re-landings. Divergent code is replaced by a pinned version.
4. BOOT GATE (§1b closed): the standalone glochid bin boots from
   master with no kernel-only prim in the filmstrip lib path — the
   ws-init! toplevel peeled to kernel boot or presence-gated. THIS IS
   THE PREREQUISITE: until it lands, nobody can run the shared
   framework on the laptop at all.
5. §4 BUG CLASS DEAD: a habitat filmstrip fix requires exactly zero
   edits in glochid/ (the 2026-10-12..14 year-shape re-land is the
   standing regression test for this claim).

With this gate, board cards citing "two residences" or "migrate pp chat
in (chat UI surface lives in glochid)" retire: the framework owns the
shared UI components (ui/bubble, ui/turn-open, ui/tool-open — already
in its ui tree); the app owns the TRANSPORT (agent loop, keys, lens
sync), never the interface.
