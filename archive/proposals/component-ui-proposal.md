# Plan proposal: Component-driven UI (co-located styles)

Status: **plan** (discussion / spec edit). Not a borge issue — this
is the spec-expansion track. Parent section: `image.product-theme`
in `borge/image.borg`. This doc is the scratch space; the authoritative
text lands in the `.borg` file when we agree.

## Problem

`ui/theme` is a 231-rule monolith with ~46 **global, un-prefixed
selectors** (`body`, `header`, `nav`, `h1`, `a`, `button`, `table`,
`pre`, `li.note-item`, `li.todo-item`, …). It is the same shape of bug
we just fixed in `notes/tree-css`: a sheet that styles elements by tag
name across the whole app, so every package's HTML is implicitly
coupled to it. We deleted that pattern from notes; it still lives in
`ui/theme`.

Consequences:
- **Coupling by tag name.** `todo`'s `<li class="todo-item">` only
  looks right because `ui/theme` knows about `li.todo-item`. The
  component (`todo`) and its styles (`ui/theme`) live in different
  packages. Change one, the other silently drifts.
- **No reuse.** There is no `(card …)` or `(nav …)` primitive that
  bundles markup + styles. `layout/card` exists but is unused by the
  app chrome; packages hand-roll `<div class="card">` and trust
  `ui/theme` to style it.
- **Theme leaks repeat.** The phosphor bleed we just fixed was
  possible *because* global element selectors make clobber cheap. A
  component-scoped architecture makes that structurally harder.
- **Discovery is bad.** "What styles apply to a todo?" → grep
  `ui/theme` for `todo`, which also matches `todo-nav-list`, etc.
  No co-location.

## Proposal: components own their styles

A **component** is a Nopales binding that returns HTML *and* is
paired with a css/sheet scoped to a class prefix it owns. The
component and its sheet live in the same package.

### Shape

```lisp
;; in libs/todo/lib.pp
(define todo/item-html       ; markup builder (already exists)
  (lambda (item) …))

(define todo/item-css        ; NEW: styles for THIS component
  (css/sheet
    (list
      (css/rule ".todo-item" (list "display" "flex" …))
      (css/rule ".todo-item.is-done .todo-title"
        (list "text-decoration" "line-through")))))
```

### The one global sheet that stays

`ui/theme` shrinks to **element defaults + the `:root` token block
only**: `body`, `a`, `html`, `* { box-sizing }`, maybe `h1`/`h2` base
size. No `.card`, no `nav`, no `li.todo-item`, no `.ol-node`. Those
move to the component that owns them:

| Current `ui/theme` selector(s) | Moves to |
|---|---|
| `nav`, `nav a`, `.menu-drop*` | `ui/theme-nav` (already partial) |
| `header`, `header .brand`, `header .meta` | `ui/theme-header` (new) or `ui/nav` |
| `table`, `th`, `td`, `tr:nth-child` | `ui/theme-table` (new) |
| `.card` | `layout/card` (already has the markup) |
| `.ol-*`, `details.ol-node`, `.ol-todo*` | `outline` package |
| `li.note-item`, `.note-del` | `notes` (already partially there) |
| `li.todo-item`, `.todo-*` | `todo` package |
| `.cal-*`, calendar | `calendar` package |
| `pre,textarea,input[*]` | `ui/theme-form` (new) or a `form` component |
| `button,.btn` | `ui/theme-button` (new) |
| `.emergency-*` | kernel (OCaml) — already special |

`ui/css` becomes a **composer** that splices component sheets:

```lisp
(define ui/css
  (lambda ()
    (css/render
      (css/sheet
        (list
          (css/use (theme/vars))
          (css/use (ui/theme))         ; element defaults + tokens
          (css/use (ui/theme-nav))
          (css/use (ui/theme-button))
          (css/use (ui/theme-form))
          (css/use (ui/theme-table))
          (css/use layout/card-css)
          (css/use notes/tree-css)
          (css/use outline/css)
          (css/use todo/css)
          (css/use calendar/css)
          (css/use (theme/extra)))))))
```

### Rules

1. **Class-prefix ownership.** A component owns a prefix
   (`.todo-`, `.ol-`, `.cal-`, `.note-item`, `.nc-`). Only that
   package writes rules for that prefix. No package writes a bare
   `li.` or `button` rule.
2. **Tag selectors only in `ui/theme`.** `body`, `a`, `html`, `*`,
   and base `h1`-`h6` live in the element-defaults sheet. Everything
   else is class-scoped.
3. **Colors via `var(--*)`.** No hardcoded hex in component sheets
   (same rule we just enforced in `notes/tree-css`). The midnight
   magenta probe catches leaks.
4. **Composition, not inheritance.** `ui/css` lists component sheets
   explicitly. A new package opts in by adding its `(css/use pkg/css)`
   line. This is the dependency-tracked read-set the cache already
   records (borge/image.borg § Session cache) — adding a component
   sheet adds it to the read-set, so the cache busts correctly.
5. **No structural sheets in themes.** (Already done.) Themes stay
   token-only: colors + `--font-sans`/`--font-mono`.
6. **`css/raw` stays the deliberate guest.** Rare, for things the
   css/* DSL can't express (e.g. `@font-face`). Not a component's
   primary style mechanism.

### Migration

This is **additive and incremental** — no big-bang. Each step is
independently shippable:

1. **Extract `ui/theme-button`, `ui/theme-form`, `ui/theme-table`,
   `ui/theme-header`** from `ui/theme`. Pure move; rendered `ui/css`
   is byte-identical. Verify with a diff of `(ui/css)` before/after.
2. **Move `.ol-*` rules to `outline/css`, `.todo-*` to `todo/css`,
   `.cal-*` to `calendar/css`.** Each package gains a `<pkg>/css`
   binding. `ui/css` adds the `(css/use …)` lines. Still byte-identical
   rendered output.
3. **Promote `layout/card` to the real card component** — packages
   call `(layout/card …)` instead of hand-rolling `<div class="card">`.
   `layout/card-css` owns `.card`. (Optional; markup change, not just
   CSS.)
4. **Shrink `ui/theme` to element defaults.** Remove the moved rules.
   Now `ui/theme` is ~15 rules, not 231.

Each step is a `pp-sync push` of the affected packages + verify
`(ui/css)` renders clean. No main reseed needed for the theme-*
packages (filesystem, release-managed). The CSS packages (`ui`,
`notes`, `todo`, `outline`, `calendar`) go to Postgres main via push.

### What this does NOT change

- The `css/*` compiler, `ui/css` adapter, session cache, and
  dependency-tracked invalidation — all unchanged. This is purely
  about *where rules live*, not the rendering or caching machinery.
- The theme token system (`theme/vars`, `theme/*` packages) —
  unchanged. Components consume `var(--*)`; themes supply them.
- `html/*` primitives — unchanged. Components are Nopales bindings
  that return HTML via `html/*`, paired with a css/sheet.

### Open questions (resolve in plan before make)

1. **Sheet-per-package vs sheet-per-component?** A package with
   several components (todo has item, list, nav) — one `todo/css` or
   `todo/item-css` + `todo/list-css`? Lean: one `todo/css` per
   package, components share a prefix. Revisit if a package grows.
2. **Does `layout/` become the home for shared atoms** (`card`,
   `button`, `form-field`)? Or do those stay in `ui/theme-*`? Lean:
   `ui/theme-*` for element defaults that *every* page needs (button,
   form, table); `layout/*` for optional structured components (card,
   hero, cta) that packages opt into.
3. **htmx fragments** — do partials need component CSS too, or do
   they inherit the full-page sheet? Current spec says fragments
   don't re-inject `<style>`. That still holds: fragments rely on
   the page already having `ui/css`. So component CSS must be in the
   page-level `ui/css` composition, not emitted per-fragment.
4. **The `notes/tree-css` precedent.** We just made notes-tree
   component-scoped. This proposal generalizes that to every
   package. The notes fix is the first migration step, already
   done — use it as the reference shape.

## Relationship to the theming fix we just shipped

The theme fix (token-only themes, no structural sheets) is the
**first half** of this. It made themes honest. This proposal is the
**second half**: make components honest. Together they give the
layering the spec already wants (`image.product-theme`):

```
theme/*  → tokens (colors + fonts)        ← done
ui/theme → element defaults + :root vars  ← shrink (this proposal)
<pkg>/css → component-scoped styles       ← new (this proposal)
ui/css   → composer (css/use list)        ← grows
```
