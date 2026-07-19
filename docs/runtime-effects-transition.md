# Runtime transition: monads → OCaml 5 effects (slow path)

**Status:** living plan (2026-07-17). Opportunistic — advance when
touching related code or when a bug’s cleanest fix is effects-shaped.
Not a freeze on product work.

**Mandate (compressed):**

1. **Guest Nopales** = capability effects (Pure / ReadDb / …) — language.
2. **Core (`lisp/`)** = direct-style; no monadic types; I/O via algebraic effects.
3. **Image logic** = direct-style; no `Lwt.t` leaking across modules.
4. **Rim** = one I/O backend module; Lwt now; Eio (or other) last.
5. **Unikernel** does not choose horizons 1–2; backend is replaceable.
6. **Slow is correct.** Dual-run and flags beat big-bang.

This is **not** “migrate to Eio.” Eio is a probable Horizon-3 Unix rim.
The path is **effects from the language core outward**.

---

## Three layers (do not collapse these)

| Layer | Question | Mechanism | User-visible? |
|-------|----------|-----------|----------------|
| **A. Guest capabilities** | What powers does this Lisp need? | `lisp/lib/effect.ml` lattice, infer, gates | badges, AI policy, route policy |
| **B. Host control effects** | How does eval do I/O without monads? | OCaml 5 `Effect.perform` + handlers | no (implementation) |
| **C. I/O runtime** | Who schedules sockets/DB/fibers? | Lwt today → maybe Eio/Miou later | maybe perf only |

- Finishing **A** is designing effects into Nopales.
- Doing **B** removes monadic coloring from the core you read.
- **C** is the power company; pick it last.

Guest Lisp never sees `perform`, Lwt, or Eio.

### H0.5 — Concepts (capabilities ≠ async ≠ host effects)

Keep these three stories separate in docs and UI copy:

1. **Capabilities (layer A)** — labels on *what a Nopales expression needs*:
   `pure`, `read-db`, `write-db`, `branch`, `publish`, `ai`, `html`, `txn`.
   Shown as badges on `/bindings`, enforced by AI install gate and public
   route policy. Soft `txn` lint warns about multi-write / `select-for-update`
   without `with-transaction`. This is the **language product**.

2. **Async / promises (historical plumbing)** — today’s `value Lwt.t` in
   eval is an implementation accident from it12 composition, **not** a
   guest concept. Users do not write async/await. H1 deletes this coloring
   from `lisp/` without changing Lisp syntax.

3. **Host control effects (layer B)** — OCaml 5 `Effect.perform` used
   *inside* the image so eval can be direct-style. Never exposed to
   Nopales. Orthogonal to capability badges.

4. **Scheduler / rim (layer C)** — Lwt or Eio at the process edge only.
   Choosing Eio does not change capabilities or guest syntax.

**One-liner for welcome/about:** “Effects badges are capabilities (what
your code may touch), not threads or promises.”

---

## Non-goals

- Big-bang delete of Lwt in one PR.
- Exposing algebraic effects to Nopales users.
- Blocking product features on this plan.
- Choosing Mirage/Solo5 stack before Horizon 2 exit.
- Replacing capability inference with host effects (orthogonal).
- Formal verification blocked on runtime choice.

---

## Invariants (every horizon)

1. **Composition stays:** `(define u (get …))` then use `u` still works.
2. **`with-transaction` stays meaningful** across nested prims in one body.
3. **Budget / cancel story** does not regress (one bad eval cannot wedge the image).
4. **Tests:** pure `lisp/` tests stay service-free; image smokes stay green.
5. **Dual-run until boring:** old path remains until new path is default and quiet.
6. **No `Lwt.` outside the allowed rim** once a horizon claims that (CI/lint later).
7. **Capability module keeps working** — host control rewrite must not break badges/AI gate.

---

## Horizon map

```
H0  Capability productization          [in progress / ongoing]
H1  Effect-control eval (lisp clean)   [next real runtime step]
H2  Direct-style image + Io port       [after H1 default]
H3  Rim backend choice (Eio likely)    [opportunistic / last]
```

Each horizon has **entry**, **work units**, **exit**, **opportunistic triggers**.

---

## H0 — Guest capability effects (language)

**Why first:** this *is* Nopales effects for users and agents. Independent of Lwt.

### Already true
- `lisp/lib/effect.ml` — lattice, `infer`, `policy_allows`, prim tables
- UI badges on `/bindings` / inspect
- AI install gate (`agent_tools.ml`)
- `lisp/test/effect_test.ml`

### Remaining work units (do anytime)

| ID | Work | Touch |
|----|------|--------|
| H0.1 | Audit every registered prim vs `effects_for_prim` | `effect.ml`, prim registration sites |
| H0.2 | `known_fn` resolution from session bindings (derive-on-read) | `session.ml`, badge + AI call sites |
| H0.3 | Route publish policy (target fn effects) | route prim / `session.ml` |
| H0.4 | Soft Txn lint on inspect (multi-write / select-for-update) | `http_pages.ml` |
| H0.5 | Concepts copy: capabilities ≠ async ≠ host effects | welcome/about / docs |
| H0.6 | Optional sugar later: `define-pure` / `define-page` | eval + session |

### Exit
- User-visible pure/db/ai/page is trustworthy across user→user calls.
- AI + route gates use the same lattice.
- Docs don’t conflate A/B/C.

### Opportunistic triggers
- Adding a prim → H0.1 row for that prim.
- Touching AI install → H0.2 if still bare `infer`.
- Touching `(route …)` → H0.3.
- “What does this function do?” UX → H0.4 / H0.5.

---

## H1 — Host control effects: uncook `lisp/` (core aesthetic + design win)

**Why:** removes `'a Lwt.t` from the interpreter. Teaches/designs effects **in this codebase**.

### Target shape

```ocaml
(* lisp — no lwt dependency *)
val eval : env -> expr -> value

(* control effects — names illustrative *)
type _ Effect.t +=
  | Prim_call : string * value list -> value Effect.t
  (* or finer-grained: Get, Create, … — start coarse, refine later *)

(* image session rim *)
Effect.Deep.match_with
  (fun () -> Eval.eval env expr)
  { retc; exnc; effc = … (* → Store / LLM, may still use Lwt inside *) }
```

Pure prims stay sync. DB/AI/session prims `perform`. Handler is the only place that knows the backend.

### Work units

| ID | Work | Notes |
|----|------|--------|
| H1.0 | Design note: effect constructors, nesting, tx, budget | 1 short doc section or issue; no code required first |
| H1.1 | Dual eval: keep `eval_lwt`, add `eval_eff` (or flag) | no behavior change default |
| H1.2 | Spike: `get` + `create` + one pure path via perform | prove composition + tests |
| H1.3 | `with-transaction` under handlers (dynamic binding or effect) | must match today’s semantics |
| H1.4 | Budget integration (tick still aborts one call) | |
| H1.5 | Grow prim coverage until parity with `eval_lwt` | opportunistic by module |
| H1.6 | Default flip when smokes green for a soak period | |
| H1.7 | Drop `lwt` from `lisp/` dune; delete `eval_lwt` | exit |

### Exit
- `lisp/` has no `Lwt` / no `lwt` library dep.
- Default eval is direct-style + handlers at session boundary.
- All existing lisp tests + image smokes pass on default.
- Capability inference unchanged (still AST-level).

### Opportunistic triggers
- Bug where monadic plumbing obscures control flow → prefer H1 slice over more `let*`.
- New DB prim → implement on eff path if dual-run exists; keep lwt shim thin.
- Touching `eval.ml` special forms → keep both paths in sync until H1.7.
- Refactor fatigue in `session.ml` eval_one → good time for H1.1–H1.2.

### Rules while dual-running
- One golden smoke set; both backends or default+periodic other.
- Feature flags: e.g. `PP_EVAL_CONTROL=lwt|eff` (name flexible).
- Do not change guest syntax.

---

## H2 — Direct-style image + Io port

**Why:** stop `Lwt.t` leaking through session/http/crud types; isolate rim.

### Target shape

```ocaml
module type IO = sig
  (* Minimal — grow from real call sites, don’t speculative-abstract *)
  val run : (unit -> 'a) -> 'a   (* or boot-installed context *)
  module Db : sig
    type conn
    val query : conn -> string -> params -> row list
    val with_transaction : conn -> (unit -> 'a) -> 'a
  end
  (* Net/HTTP only when something moves off httpaf-lwt internals *)
end

module Io_lwt : sig
  (* only file allowed to call Lwt / pgx_lwt / httpaf-lwt-unix directly *)
end
```

Image logic calls `Io` or runs under the H1 handler; not `Lwt.bind` in business code.

### Work units

| ID | Work | Notes |
|----|------|--------|
| H2.1 | Inventory: modules that open Lwt (count already high in session/store/auth) | scout, no rewrite |
| H2.2 | Introduce `Io` / `Io_lwt` with **Db** only | Store/CRUD first |
| H2.3 | Move Store + crud behind Io | |
| H2.4 | Auth store behind Io | |
| H2.5 | Collapse remaining `let*` in session to direct style under handler | |
| H2.6 | Lint: `Lwt` only under `image/lib/io_*.ml` + boot | dune/CI optional |
| H2.7 | HTTP loop still may be Lwt internally until H3 | OK — contained |

### Exit
- Reading `session.ml` / `crud.ml` does not require understanding Lwt.
- `rg 'Lwt\.' image/lib` ≈ io + boot + maybe http server adapter only.
- H1 still default.

### Opportunistic triggers
- Editing `store.ml` / `crud.ml` → H2.2–H2.3 slice.
- Auth bugs → H2.4.
- New PG access → must go through Io once H2.2 exists (no new raw pgx).

---

## H3 — Rim backend (Eio probable, not mandatory)

**Why:** only if we want zero Lwt in the dependency tree / Unix multicore I/O / ecosystem.

### Preconditions
- H1 exited (lisp clean).
- H2 exited or nearly (Io port exists and is the only DB path).

### Work units

| ID | Work | Notes |
|----|------|--------|
| H3.1 | `Io_eio` spike: health + one query | side path / flag |
| H3.2 | HTTP adapter (httpun-eio / gluten-eio / etc.) | ecosystem pick at the time |
| H3.3 | Parity smokes under `PP_IO=eio` | |
| H3.4 | Default flip after soak | |
| H3.5 | Remove Lwt backend | optional forever if D preferred |

### Alternative H3 endings
- **Stay on `Io_lwt`:** core already direct-style; monads only under carpet. Valid long-term.
- **`Io_miou`:** if unikernel path becomes real.
- Never delete Lwt until a second backend is boring.

### Opportunistic triggers
- Pain from Lwt (bugs, missing multicore, maintainer energy).
- Explicit product decision “Unix image is Eio.”
- Not: “we added a feature.”

---

## Decision rules (for agents and humans)

When touching code, ask in order:

1. **Is this a new prim?** → register capabilities (H0.1). Prefer eff-path if H1 dual-run exists.
2. **Is the cleanest fix about “what can this code do?”** → H0 (gates/infer), not scheduler.
3. **Is the cleanest fix about monadic control flow / eval composition?** → H1 slice.
4. **Is the cleanest fix about PG/HTTP plumbing isolation?** → H2 slice.
5. **Is the cleanest fix “we need a different OS scheduler”?** → H3 only with preconditions.

**Prefer the smallest horizon that removes the pain.**

When unstable:
- Flag back to previous control/backend.
- File a bug; do not widen the rewrite.
- Capability layer should keep working even if control path reverts.

---

## Suggested first concrete steps (when next opportunity appears)

Not a schedule — a preferred order for the *next* dedicated slices:

1. **H0.1** prim↔effect audit (small, product-safe).
2. **H1.0** write effect constructor + tx/budget sketch in this doc’s appendix or a borge issue.
3. **H1.1–H1.2** dual eval + get/create spike behind flag.
4. Keep shipping product; advance H0.2–H0.3 when near those files.
5. **H2** only after H1 default or when Store is already open for big edits.

---

## File ownership (cheat sheet)

| Concern | Primary files today |
|---------|---------------------|
| Guest capabilities | `lisp/lib/effect.ml`, `lisp/test/effect_test.ml` |
| Eval / monads | `lisp/lib/eval.ml`, `lisp/lib/dune` |
| Prim registration | `image/lib/primitives.ml`, `prims_pure.ml`, `session.ml` |
| DB | `store.ml`, `store_db.ml`, `crud.ml` |
| Handler rim (future) | `session.ml` (`eval_one`), eventually `image/lib/io_*.ml` |
| HTTP rim | `http.ml`, `http_core.ml`, `boot.ml` |
| AI policy | `agent_tools.ml` |
| Badges | `http_pages.ml` |

---

## Success criteria (whole mandate)

- [ ] Nopales has a trustworthy capability story (H0).
- [ ] Printed `eval.ml` has no promise plumbing (H1).
- [ ] Image domain code reads direct-style (H2).
- [ ] Backend is one module we could replace (H2/H3).
- [ ] Guest language unchanged; composition and transactions hold.
- [ ] Eio is a choice we *can* make, not a religion we already made.

---

## Appendix A — Glossary

| Term | Meaning |
|------|---------|
| Capability effects | Guest labels: pure, read-db, write-db, … |
| Control effects | OCaml 5 `Effect` used to implement eval/prims |
| Rim | Process edge: HTTP server, PG driver, `Lwt_main`/`Eio_main` |
| Io port | Module type hiding the rim from domain code |
| Dual-run | Old and new implementation selectable until soak |

## Appendix B — Relationship to existing specs

- `borge/lisp-interpreter.borg` §effects — guest lattice; async vs capabilities vs host effects.
- it12 Lwt-in-eval — historical unblock for DB composition; **superseded as end state** by H1, not as product regression.
- `architecture.borg` multi-process/Mirage — retired topology; does not dictate H1–H2.
- AGENTS.md “Eio not inside the image” — holds until H3 explicitly starts; H1–H2 do not require Eio.

## Appendix C — Progress log

| Date | Note |
|------|------|
| 2026-07-17 | Plan written. H0 partially done. H1–H3 not started. Default runtime still Lwt end-to-end. |
| 2026-07-18 | Overnight ralph `runtime-effects-overnight` started. Baseline: phase-1 `Request_context` (manifest snapshot + `Lwt.with_value` after identity gate); plan doc committed. |
| 2026-07-18 | **H0.1** prim↔effect audit: all 112 registered image prims classified in `lisp/lib/effect.ml` buckets; effect tests extended. |
| 2026-07-18 | **H0.2/H0.3** `known_fn_of_bindings` (memo + cycle break); badges + AI install/rewrite/seed use store bindings; public route gate on handler lattice (allow pure|read|write|html|txn). |
| 2026-07-18 | **H0.4/H0.5** soft `txn_lint` on binding inspect + concepts section (capabilities ≠ async ≠ host effects). |

When a horizon unit ships, add a row here (or point at commit).
