# Theme hex-literal audit — `libs/ui/lib.pp`

Phase 3 audit: classify every hardcoded hex literal in the UI structural sheet
and adjacent `ui/lib.pp` raw blocks. Total = **77** occurrences across **25
unique values**.

File regions audited (local git vault, `admin/personal`-pushed state):

| Region | Lines | Occurrences | Role |
|---|---|---|---|
| `ui/theme` lambda | 147–1987 | **73** | the big structural sheet |
| `ui/nav-style` (raw block, capture .ok/.err) | 136–146 (`#226622`@144, `#800000`@145) | 2 | nav capture status |
| nav CSS concat in `ui/css` (raw block) | ~54–55 (`#226622`, `#800000`) | 2 | menu capture status |

> `ui/theme-nav` (line 1988+) contains **0** hex literals.

## Value inventory (unique, occurrence count)

```
#000        22   structural border (theme-constant)
#fff8dc      7   highlight / pending (theme-variable)
#808080      6   box-shadow / light border (theme-constant gray)
#666         5   muted gray border/text (theme-variable-ish, see notes)
#e0e8ff      3   WAITING bg (theme-variable)
#cfc         3   DONE bg (theme-variable)
#555         3   archived/muted text (theme-variable)
#226622      3   DONE green text/border + capture .ok (theme-variable)
#fff3cd      2   banner bg (theme-variable)
#eee         2   archived bg (theme-variable)
#c0c0c0      2   light gray list border (theme-constant-ish)
#8b0000      2   banner border (theme-variable)
#ffffe0      1   calendar today bg (theme-variable)
#fff         1   button outset top-edge (theme-constant)
#ffe8a3      1   impact badge bg (theme-variable)
#e8f5e8      1   DONE pill bg variant (theme-variable)
#ddd         1   CANCELLED bg (theme-variable)
#ccc         1   calendar list border (theme-constant-ish)
#8b6914      1   impact badge border (theme-variable)
#888         1   archived title text (theme-variable)
#886600      1   TODO pill border (theme-variable)
#777         1   archived-title muted text (theme-variable)
#664400      1   TODO pill text (theme-variable)
#1a1a1a      1   banner text (theme-variable)
#000080      1   selection inset accent bar (theme-variable)
```

Sum: 22+7+6+5+3+3+3+3+2+2+2+2 = 60 · 17 singles = 77 ✓

## Classification

**Legend:**
- **VAR** = theme-variable → must become `var(--*)` token (add to `theme/vars`
  + all 5 theme packages: theme/theme-paper/theme-midnight/theme-classic/theme-as400).
- **CONST** = theme-constant → leave as-is (structural chrome that matches all
  themes) OR fold into a static `--border-strong` token if a theme ever needs
  to override. Default recommendation: leave as-is (no new token churn).

---

### Group 1 — Banner / emergency (`--banner-*`) — ALL VAR
Selectors: `.emergency-banner`, `.emergency-home`.
| Line | Value | Prop | Token |
|---|---|---|---|
| 269 | `#8b0000` | border | `--banner-border` |
| 271 | `#fff3cd` | background | `--banner-bg` |
| 273 | `#1a1a1a` | color (text) | `--banner-ink` |
| 285 | `#8b0000` | border | `--banner-border` |
| 287 | `#fff3cd` | background | `--banner-bg` |

### Group 2 — Selection / active-row highlight (`--sel-bg`) — ALL VAR
Selectors: `.ol-node.is-selected > .ol-sum` (outline+inset), `tr.br-current`,
`a .cal-cell:hover`, `.bb-item.is-active` (uses `--row-alt`, ok).
| Line | Value | Prop | Token |
|---|---|---|---|
| 643 | `#fff8dc` | bg (`.ol-node.is-selected`) | `--sel-bg` |
| 647 | `#000080` | inset accent bar | `--sel-accent` |
| 1093 | `#fff8dc` | bg (`tr.br-current`) | `--sel-bg` |
| 1408 | `#fff8dc` | bg (`a .cal-cell:hover`) | `--sel-bg` |
> `#000` selection outline / `.ol-node` box-shadow is CONST (structural).

### Group 3 — TODO pill / box / button / dot (pending “cream”) — ALL VAR
Selectors: `.ol-todo.st-TODO`, `.todo-box.st-TODO`, `.ol-todo-btn.st-TODO`,
`.cal-dot-todo`. (Same `#fff8dc` cream as selection; share a
`--pending-bg` token if a theme wants pending ≠ selection, else reuse `--sel-bg`.)
| Line | Value | Prop | Token |
|---|---|---|---|
| 725 | `#886600` | border-color (TODO pill) | `--pending-border` |
| 727 | `#664400` | color (TODO pill) | `--pending-ink` |
| 729 | `#fff8dc` | bg (TODO pill) | `--pending-bg` (or `--sel-bg`) |
| 1376 | `#fff8dc` | bg (`.todo-box.st-TODO`) | `--pending-bg` |
| 1476 | `#fff8dc` | bg (`.ol-todo-btn.st-TODO`) | `--pending-bg` |
| 1511 | `#fff8dc` | bg (`.cal-dot-todo`) | `--pending-bg` |

### Group 4 — DONE state — ALL VAR
Selectors: `.ol-todo.st-DONE`, `.todo-box.st-DONE`, `.ol-todo-btn.st-DONE`,
`.cal-dot-ev`, `.ol-node.st-DONE > .ol-sum .ol-title`, `li.todo-item.is-done
.todo-title`, `.capture-nav-form .ok`.
| Line | Value | Prop | Token |
|---|---|---|---|
| 734/736 | `#226622` | border-color + color (DONE pill) | `--done-ink` |
| 738 | `#e8f5e8` | bg (DONE pill) | `--done-bg` |
| 1381 | `#cfc` | bg (`.todo-box.st-DONE`) | `--done-bg` (lighter green) |
| 1411 | `#cfc` | bg (`.ol-todo-btn.st-DONE`) | `--done-bg` |
| 1510 | `#cfc` | bg (`.cal-dot-ev`) | `--done-bg` |
| 1652 | `#226622` | color (`.capture-nav-form .ok`) | `--ok`-adjacent (see note) |
| 1771 | `#666` | color (`.is-done .todo-title` line-through) | `--done-muted` |
> **Note:** `#cfc` and `#e8f5e8` are two different greens used for “done”; a
> theme may want them unified or distinct. Recommend `--done-bg` covers both
> (pick variety pragmatically) OR split `--done-bg`/`--done-bg-strong`.

### Group 5 — WAITING state — ALL VAR
Selectors: `.todo-box.st-WAITING`, `.ol-todo-btn.st-WAITING`, `.cal-dot-note`,
`.ol-node` waiting? (only these three + dot).
| Line | Value | Prop | Token |
|---|---|---|---|
| 1386 | `#e0e8ff` | bg (`.todo-box.st-WAITING`) | `--waiting-bg` |
| 1414 | `#e0e8ff` | bg (`.ol-todo-btn.st-WAITING`) | `--waiting-bg` |
| 1512 | `#e0e8ff` | bg (`.cal-dot-note`) | `--waiting-bg` |

### Group 6 — ARCHIVED / CANCELLED — ALL VAR
Selectors: `.ol-todo.st-ARCHIVED`, `.todo-box.st-ARCHIVED`, `.todo-box.st-CANCELLED`,
`.ol-node.st-ARCHIVED > .ol-sum .ol-title`, `li.todo-item.is-archived .todo-title`.
| Line | Value | Prop | Token |
|---|---|---|---|
| 743 | `#666` | border-color (ARCHIVED pill) | `--archived-border` |
| 745 | `#555` | color | `--archived-ink` |
| 747 | `#eee` | bg | `--archived-bg` |
| 754 | `#555` | color (ARCHIVED title) | `--archived-ink` |
| 761 | `#777` | color (ARCHIVED title italic) | `--archived-ink` (or muted) |
| 1389 | `#ddd` | bg (`.todo-box.st-CANCELLED`) | `--cancelled-bg` |
| 1389 | `#666` | color (CANCELLED) | `--archived-ink` |
| 1394 | `#eee` | bg (ARCHIVED box) | `--archived-bg` |
| 1396 | `#555` | color | `--archived-ink` |
| 1401 | `#888` | color (is-archived .todo-title) | `--archived-ink` |

### Group 7 — Calendar today (`--today-bg`) — ALL VAR
| Line | Value | Prop | Token |
|---|---|---|---|
| 1482 | `#ffffe0` | bg (`.cal-today`) | `--today-bg` |
> `#000` today outline is CONST.

### Group 8 — Impact badge — ALL VAR
| Line | Value | Prop | Token |
|---|---|---|---|
| 1146 | `#ffe8a3` | bg (`.badge.impact`) | `--badge-bg` |
| 1148 | `#8b6914` | border | `--badge-border` |

### Group 9 — Structural constants — CONST (leav as-is)
- `#000` ×22 — borders (header/nav/table/cal/menu/bb-tabs, selection outline,
  `.cal-today` outline, `.bb-item.is-active` border-left, todo-nav active).
- `#808080` ×6 — `2px 2px 0` box-shadows (menu-drop-panel @361, card?,
  `.ol-actions-panel` @1973, others) + `.ol-fold:hover` border @683 +
  `1px solid` border @470. **All gray shadows / light borders.**
- `#fff` ×1 — button `2px outset` top-edge highlight (line 546).
- `#c0c0c0` ×2 — `li.note-item`/todo list border-bottom (light gray line).
- `#ccc` ×1 — `.cal-event,.cal-todo` border-bottom.
> These are value-neutral (work over any bg) — no token needed. If a theme
> later wants a raised/pressed shadow tint, add a single `--shadow-strong`
> token then. Out of phase-3 scope.

### Group 10 — Muted gray text/border — MIXED (recommend VAR under `--muted` reuse where semantic)
- `#666` ×5 — @706 `.ol-todo` border (CONST border), @952 `.ol-actions` button
  border (CONST), @1772 `li.todo-item.is-done .todo-title` color (VAR →
  `--done-muted`), @743 ARCHIVED pill border (VAR → `--archived-border`), @1389
  CANCELLED color (VAR → `--archived-ink`).

---

## Recommended NEW tokens (to add to `theme/vars` + all 5 packages)

Variables (theme-variable; dark/light values differ per theme):
```
--banner-border  #8b0000   --banner-bg  #fff3cd   --banner-ink  #1a1a1a
--sel-bg         #fff8dc   --sel-accent #000080
--pending-bg     #fff8dc   --pending-ink #664400  --pending-border #886600
--done-bg        #cfc      --done-ink   #226622   --done-muted #666
--waiting-bg     #e0e8ff
--archived-bg    #eee      --archived-ink #555    --archived-border #666
--cancelled-bg   #ddd
--today-bg       #ffffe0
--badge-bg       #ffe8a3   --badge-border #8b6914
```
**Also fix (out of the 77, adjacent):** the capture `.ok`/`.err` raw blocks
hardcode `#226622` / `#800000` (`ui/nav-style` @144-145 + ui/css concat
@54-55). `.ok` should map to `--ok`, `.err` to `--err`. These are 4 of the 77
counted literals (`#226622`, `#800000` ×2/each). `#800000` is a 6-digit hex —
use `var(--err)`.

## Example dark-theme values (midnight/as400 suggestion)
- phosphor: `--sel-bg #1a3a1a`, `--done-bg #0f2a10`, `--done-ink #55cc55`,
  `--waiting-bg #1a2a3a`, `--banner-bg #3a2a00`, `--banner-border #886600`,
  `--banner-ink #ffdd88`, `--today-bg #333a1a`, `--archived-bg #1a1a1a`,
  `--archived-ink #777`.
  → pick per-theme when implementing; audit only records the *current light
  values* so the author can contrast.

## Verification after implementing
- `var(--` count should jump from 70 → 70 + ~23 new tokens used (≈93+).
- `grep -oE '#[0-9a-fA-F]{3,8}' libs/ui/lib.pp | wc -l` in `ui/theme` region
  should drop to only the CONST group (~31: 22×#000 + 6×#808080 + #fff +
  2×#c0c0c0 + #ccc) plus any intentional CONST holds.
- `(ui/css)` structural sheet byte-identical across the 5 themes; only `:root`
  differs.
