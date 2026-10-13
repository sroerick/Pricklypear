# Glochid render analysis: call-tree profiling + lazy-eval audit

> 2026-09-20 · instrument: `PP_PROFILE_RENDER` (flat per-fn tables, exact)
> + the new debug `PP_PROFILE_TREE=1` call-tree capture (brackets ride the
> flat `current_fn` attribution — exact per-name, approximate nesting; see
> instrument note at the bottom). Subject: the shell doc render (`/`),
> the filmstrip level endpoints, and the smaller faces, at the 400k-step
> budget. Related: BORGE-1789950734 (filed).

## One-paragraph verdict

The shell doc render rides the 400k budget ceiling (~370k cold with a
4-pane workspace; an earlier 5-pane state blew it). The work is not the
panes and not the theme — it is **eager tree materialization**: ~60% of
the budget builds 148 `filmstrip/tree-node-li` rows (~900 self-steps
each; includes a second level of eager `code` kids) for FOUR rail boxes
of which the operator SEES ONE. The laziness discipline itself is solid —
the css sheet, env loading, tree expansion pulls, and shell mutations are
all already lazy/memoized — the remaining waste is concentrated in the
v4 rail-trees eager box roots, per-pane verb reassembly, and a JSON
row-contract round trip per row.

## Where the steps go (shell, cold, 4-pane workspace)

| fn | calls | self-steps | share |
|---|---|---|---|
| `filmstrip/tree-node-li` | 148 | 134,237 | 36% |
| `dict-set*` | 1598 | 38,741 | 10% |
| `html/attr` | 2177 | 34,832 | 9% |
| `filmstrip/act-glyph-lookup` (nested) | 672 | 12,621 | 3% |
| `html/attrs` / `html/tag*` / `html/join` | ~2900 | ~65,000 | 17% |
| `fn` plumbing (map-indexed bodies) | 284 | 28,504 | 8% |
| nested pane pages (`route/dispatch` ×4, in-budget) | 4 | ~2,600 | 1% |
| settings/chat head fragments | few | ~1,600 | <1% |
| residual (sheet render of rule list, url-enc, …

) | — | ~50,000 | ~14% |

Isolated per-LEVEL tree costs (via the `/filmstrip/tree?root=X` level
endpoints, which render exactly one level — cheap, as designed):

- `windows` root (3 desks + shelf, desk-0 eager pane rows): **12k** steps
- `ui` root (13 lookbook elements): **21k** steps
- `code` root (12 dirs + eager depth-1 kids): extrapolates to **~115k**
  of the shell doc — the single biggest eager block
- `prims` root (12 packages): analogous to `code`, minus kids

The windows-tree pane rows also rebuild their verb rows every render
(`glochid/windows-pane-node`: focus/pin/width/single/close + `to<N>`
moves per live pane + restore/destroy per shelved pane, each a
string-fold JSON build + `act-glyph` lookup) — ~9 verbs × pane × every
shell render, never memoized against the pane record.

## Laziness ledger

### Already lazy / memoized (healthy)

1. **the css sheet**: `glochid/css-text` is `(delay (css/render …))` —
   the filmstrip sheet renders once per session (~200 steps at steady
   state); the omarchy theme tokens resolve once with it.
2. **env loading**: per-identity sessions bind defines as recipes
   (lazy-env-thunks); bodies force once on first touch.
3. **tree expansion**: only ROOT levels are rendered eagerly; deeper
   nodes are hx-get-on-fold pulls (`filmstrip/tree-lazy`, toggle-once).
4. **shell mutations**: railmode/desk/pin/close/restore act via OOB
   swaps against the sentinel CSS — no doc re-render.
5. **lookbook eval**: each variant call + card fetch runs in a FRESH
   budget (`lookbook/eval` prim) — a runaway seed cannot eat a route.
6. **pane content**: nested `route/dispatch` renders only live panes of
   the current desktop (shelved/others skipped).
7. **the ui rail root**: inventories 13 names via `lookbook/elements`
   (one def), rows are cheap leaves.

### Eager misses (ranked by measured cost) — ✅ items 1-2 SHIPPED 2026-09-20

1. **HIDDEN RAIL BOXES rendered on every shell doc** (~135k, 36%+).
   All four `.fs-rmode` boxes are materialized at root (code includes
   its eager kids) while `:has(#fs-railmode.rm-*)` displays one.
   → **SHIPPED (BORGE-1789950734)**: `glochid/tree-boxes` renders rows
   only for the active railmode + MRU-2 `boxrows` (ws field); hidden
   boxes answer `glochid/tree-stub` (chrome + empty UL); the flip
   route OOBs the level rows on FIRST visit only — revisits keep
   expansion DOM. Measured after: cold shell **~165k steps (was ~370k)**.
2. **code box eager depth-1 kids** (~115k of the doc render by itself).
   Now paid ONCE on first reveal of the code box (the flip OOB runs
   inside a dedicated 400k budget), never per doc render. The MRU cap
   means a cold boot re-renders at most the active + last box.
3. **per-pane verb reassembly** (glochid layer, ~5-10% of render with
   4+ panes; grows linearly): the pane node dicts are rebuilt and
   re-serialized every render. A glochid-layer memo keyed on the pane
   record (workspace fields are only written through `filmstrip/save-all`
   mutations) would be safe and local. *Open — deferred: after items
   1-2 landed the shell sits at ~165k/400k; revisit if pane counts grow.*
4. **row dict JSON round trip** (~10%): tree rows are JSON dict STRINGS
   (`json-emit` at build, re-parse at render) — the shared-node
   CONTRACT (rail-trees-v4, bind twins); a direct-dict path is a spec
   conversation, filed as a companion note on 1789950734.

### Carry-forward bug found while shipping item 1

`filmstrip/save-all` rebuilt the ws dict WITHOUT `railmode` (and would
have wiped `boxrows`): every pane mutation silently reset the rail mode
to the windows default. Both fields now read-modify-write through
save-all — the docstring's own carry rule, previously unenforced.

### Verdict on "are we maximizing lazy eval"

The architecture already uses lazy eval where the specs call for it
(sheet, env, pulls, OOB, budgets). The one *systematic* gap is the
shell doc render evaluating hidden state — a display-vs-evaluation
conflation the v4 stanza does not acknowledge. Everything else is
per-call-level tuning, not a laziness failure.

## Instrument notes (for the next run)

- Flat tables: `PP_PROFILE_RENDER=1` on the glochid unit; per-fn
  self-steps + call counts, printed at `Host_seam.apply_value_raw`
  (one entry per handler application — nested pane dispatches
  profile SEPARATELY, each with its span's budget).
- `PP_PROFILE_TREE=1` (new, inert without the flag) captures a
  parent-linked tree per application and prints an indented dump —
  useful for concentration-by-segment; brackets ride the flat
  `current_fn` discipline, which is exact per-name but approximate
  across trampoline/thunk-force boundaries (a leaked node shows
  `incl ≈ root` — read flat totals first). Same-name sibling runs
  collapse into one line.
- The render sits within ~30k of the limit at 4 panes — keep
  `.agent/workspace.json` lean when profiling, and bisect by moving it
  aside (env resets to the default shell).
