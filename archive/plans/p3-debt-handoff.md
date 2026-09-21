# P3 debt handoff — package-surface, session god-module, crud Lwt

**Written:** 2026-07-31  
**Context:** After backlog burn-down + security trio. Product P1/P2 closed.
These three remain open on purpose (structural / multi-session).

**Borge issues (local `.borge-bugs/`, gitignored):**
- `BORGE-1785211142` — package-surface
- `BORGE-1785211159` — session.ml god-module
- `BORGE-1785397493` — crud.ml Lwt → effects seam

**Related closed this arc:**
- `29af04e` security (prim shadow, macroexpand gate, admin deftype)
- `408d208` admin agent/token UI
- `e62127a` now/now-ms/sleep + I/O prim effect tables
- `408c893` boot `discover_seed_packages` (closed 1785211333)

**Habitat note (if live):** slug `p3-debt-handoff` — agents may upsert via notes.

---

# 1. Package-surface (`BORGE-1785211142`) — explore first

## 1.1 One-line pitch

v1 needs **authoritative export lists + discovery that respects them**, not a
Racket-style module system. Internals stay callable (Lisp-1); agents/docs
prefer exports.

## 1.2 Three meanings of “package” (do not conflate)

| Layer | Meaning | Status |
|--------|---------|--------|
| **Name prefix** | `todo/list` — `/` organizes bindings | **Shipped** (`image.packages`, verified) |
| **Git/libs unit** | `libs/todo/lib.pp` via `load-library` | **Shipped** (+ boot directory scan) |
| **Blessed API** | What gardens/agents may depend on | **Planned** (`lisp-interpreter.package-surface`) |

## 1.3 Spec already decided (`borge/lisp-interpreter.borg` § package-surface)

Status: **planned**.

```lisp
(package pkg-name
  (export name1 name2 …))
```

- **If absent:** all defines public (status quo — no forced migration).
- **If present:** non-exported = **internal**: still callable; documented unstable;
  optional lint when *other* packages reference them.
- **Non-goals:** import forms, per-file namespaces, in-package versions,
  sealing (internal uncallable).
- **Success line:** std + ui + layout have export lists; concepts/docs explain
  public vs internal; agents stop defaulting to internals.

Stability tiers: kernel prims > exported package > internal > personal garden.

## 1.4 What already exists (half-built)

| Prim / tool | Behavior today | Gap |
|-------------|----------------|-----|
| `(list-packages)` | Top-level prefixes from `function_defs` | OK |
| `(package-bindings "todo")` | All `todo` / `todo/…` any depth | OK |
| `(package-exports "todo")` | **Heuristic:** shallow only (≤1 slash after pkg) | Not author intent |
| `(package-exports "std")` | **Hardcoded** list in `session.ml` | Drifts from freeze/code |
| `(describe …)` | Progressive disclosure | Should prefer real exports |
| `(export-package …)` | Dump lib.pp text from store | Should emit `(package …)` header |
| `image/lib/packages.ml` | prefix split/join, grouping for UI | No export set |
| Boot seed | preferred order + scan `libs/*/lib.pp` | No export awareness |

**Heuristic problem:** depth ≠ privacy.
- `notes/doc/get` (public) is “deep” → bindings-only under heuristic.
- `todo/item-state` (helper) is shallow → looks public.

Only an **author export list** fixes notes/todo-scale packages.

## 1.5 Recommended phases

### Phase 1 — Declaration + discovery (do this first)

1. **Directive** in loader (`load-string` / `load-library`): recognize leading
   `(package NAME (export …))` — not a normal user value define.
2. **Persist exports** so they survive rehydrate + branch inheritance
   (Postgres row or equivalent — not RAM-only).
3. **`(package-exports pkg)`**:
   - declared set if present
   - else std/kernel frozen table (one source of truth)
   - else current shallow heuristic
4. **Seed libs:** `std`, `ui`, `layout` get real headers (book success line).
   Optional: `todo` as app example.
5. **UX:** `describe` / `/bindings` badge internal; agent.txt already says prefer exports.
6. **`export-package`:** round-trip the `(package …)` form into vault text.

**Not in Phase 1:** sealing, multi-file, requires graph, promote filter, call-site lint.

### Phase 2 — Law with teeth

- Lint: package A AST references package B internal → warn
- Promote default = exports only (`--force` for internals)
- AI propose/install prompts bias to exports

### Phase 3 — Scale (optional)

- Multi-file `libs/<pkg>/*.pp` or manifest
- `(requires outline)` in header to replace hand priority seed list over time
- Still no Racket modules

## 1.6 Design calls (decide before make)

1. **Export names full or local?** Prefer full `todo/add`, or bare `add` auto-prefixed with package name.
2. **Storage:** table vs meta on branch; must inherit parent → child like function_defs.
3. **std freeze = package-exports "std"** — merge tables; stop dual maintenance in session.ml hardcode.
4. **Routes / deftype / macros:** out of export list for v1 (functions only).
5. **Personal shadow:** user may still redefine exported names on personal; document it.

## 1.7 Implementation touchpoints

- Spec: `borge/lisp-interpreter.borg` § `package-surface` (planned → implement)
- Loader: `image/lib/loader.ml`
- Prefix helpers: `image/lib/packages.ml` (extend, don’t reinvent)
- Discovery: `image/lib/agent_tools.ml` (`package_export_names`, `describe`)
- Prim registration: `image/lib/session.ml` (thin; avoid growing god-module — peel if needed)
- Libs: `libs/std/lib.pp`, `libs/ui/lib.pp`, `libs/layout/lib.pp`
- Related: `std-library` freeze subsection; boot `discover_seed_packages`

## 1.8 Effort / risk

| Phase | Effort | Risk |
|-------|--------|------|
| 1 | ~1–2 days | Low if soft-only; watch branch inheritance of export rows |
| 2 | medium | Lint false positives on deep graphs |
| 3 | medium | Load-order bugs |

## 1.9 Example declaration

```lisp
(package todo
  (export
    todo/add
    todo/active-items
    todo/page))

(define todo/item-state …)   ; internal, still callable
(define todo/add …)          ; public
```

---

# 2. session.ml god-module (`BORGE-1785211159`)

## 2.1 Pitch

~3300 LOC owns Session.t, rehydrate, checkout, eval_form, boot, and ~80 prim
registrations. Not a v1 correctness bug — a **velocity / merge-pain** tax.

## 2.2 Natural seams (prefer peel when already touching area)

```
prims_auth.ml   — create-user, tokens, is-admin?, create-agent, promote
prims_net.ml    — http-get, sleep (register_net_prims already a function)
prims_db.ml     — thin; crud.ml already exists
boot.ml         — seed_stdlib, discover_seed_packages, agents note
session.ml      — t, rehydrate, checkout, eval_one/form, make
```

Package-surface Phase 1 should land in **loader + packages + store**, not more
inline session blobs.

## 2.3 Guidance

- Do **not** schedule a pure “split session” epic unless blocked.
- Next auth/net/package work: extract that slice as you go.
- Update issue LOC when peeling.

---

# 3. crud.ml Lwt saturation (`BORGE-1785397493`)

## 3.1 Pitch

`crud.ml` is `open Lwt.Syntax` + `value Lwt.t` throughout (~700 LOC). Spec /
`docs/runtime-effects-transition.md` / one-tree invariant: **Lwt stays at the
rim**; core should `Control.perform_prim` / `Io.DB`.

## 3.2 Guidance

- Works today — not a product blocker.
- Do **not** rewrite crud in isolation.
- Fold into next effects-rim pass; new tree/CRUD code should not deepen Lwt in core.
- Pair with H1.x / Control_rim work, not package-surface.

---

# 4. Suggested next-session order

1. **Package-surface Phase 1** (highest product leverage for agents + libs growth)
2. Opportunistic **session peel** only if Phase 1 forces new registration surface
3. **crud Lwt** only with effects transition

## Commands (laptop)

```bash
cd ~/dev/wyo.tech/pricklypear
eval $(opam env --switch=poohstack --set-switch)
borge issue show BORGE-1785211142
# spec
#   borge/lisp-interpreter.borg  → subsection package-surface
#   borge/image.borg             → subsection packages (prefix model, verified)
rg -n "package-exports|package_export" image/lib
```

## Live habitat (optional)

```bash
set -a; . ~/.config/pricklypear/env; set +a
export PP_EVAL="${PP_EVAL:-$HOME/dev/wyo.tech/pricklypear/_build/default/cli/bin/pp_eval.exe}"
"$PP_EVAL" -j '(load-library "notes")'
"$PP_EVAL" -j '(notes/doc/get "p3-debt-handoff")'
# or agents living note
"$PP_EVAL" -j '(notes/doc/get "agents")'
```

---

# 5. Issue tracker hygiene

Open triage after burn-down (intentional):

| ID | Title |
|----|--------|
| 1785211142 | package-surface |
| 1785211159 | session.ml god-module |
| 1785397493 | crud.ml Lwt seam |

When starting Phase 1: `borg_task` or plan `package-surface` section; close or
update 1142 with evidence; keep 1159/5397493 open unless peeled as side effect.
