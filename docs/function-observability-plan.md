Now I have all the context needed. Let me compose the full implementation plan.

**Note on output path conflict:** The task instructions explicitly state the repo is write-locked (idle phase) and to deliver the plan as inline text only. The runtime output path override says to write to `…/2987034c/plan.md`. Per the review-only/no-edit precedence rule, I will not write any files and deliver the entire plan inline.

---

# Implementation Plan

## Goal

Add function observability to Pricklypear in six independently shippable phases: recursion highlighting on `/binding/<name>`, eval step counter with per-function attribution, DB cost annotations, heuristic big-O classifier, LLM complexity analysis on demand, and an inside-out explainer.

---

## Architectural constraint (applies to all phases)

`lisp/` is **pure** — depends only on `yojson`, no `pure-html`, no Lwt, no DB (`lisp/lib/dune:9`). All static AST analysis (recursion detection, tail-position classification, big-O classification, sexp peel) must live in `lisp/` so it is testable with alcotest and no services. All HTML rendering (Pure_html node trees) and DB/LLM integration must live in `image/` (`image/lib/dune` lists `pure-html`, `pgx`, `lwt`, etc.). The `budget` type (`lisp/lib/eval.ml:14`) is already mutable and accessed via the module-level `current_budget` ref (`eval.ml:28`) — image-layer prims already read it (e.g., `budget-remaining`). This dynamic-binding pattern is the seam for all instrumentation.

---

## Phase 1 — Recursion highlighting on `/binding/<name>`

### Tasks

1. **Create `lisp/lib/recursion.ml`** — pure AST analysis module.
   - `type tail_ctx = Tail | Non_tail` — tail-position context.
   - `type recursion_info = { is_recursive : bool; tail_recursive : bool; self_ref_sites : (int list) list; mutual_refs : string list }` where `self_ref_sites` are paths (list of arg indices) into the expr where `App(Var fn_name, ...)` occurs.
   - `analyze_self : fn_name:string -> expr -> recursion_info` — walks the expr recursively, tracking tail context. An `App(Var fn_name, args)` at any position is a self-reference; if it's in `Tail` context, it's a tail-recursive site. `tail_recursive = all self_ref_sites are in Tail context`.
   - `classify_tail_positions : expr -> (int list -> bool) * (int list -> bool)` — returns two predicates: is a given path in tail position? is it in non-tail position? Derived by walking the expr with a tail-context flag:
     - `If` — then/else branches are Tail; test is Non_tail.
     - `Seq` — second element is Tail; first is Non_tail.
     - `Let`/`Letrec` — body is Tail; bindings are Non_tail.
     - `Lambda` — body is Non_tail (the lambda body is a new call frame, not tail of the caller).
     - `App`/`Prim` — all args are Non_tail; the whole App inherits the caller's context for tail purposes (an App in Tail context IS the tail call).
   - `acceptance`: alcotest checks on factorial, map, fib, mutual even/odd.

2. **Add `recursion` to `lisp/lib/dune`** (`modules` stanza, line 3).
   - File: `lisp/lib/dune:3`
   - Change: add `recursion` to the modules list.

3. **Extend `image/lib/call_graph.ml`** — add mutual recursion cycle detection.
   - File: `image/lib/call_graph.ml` (after `build_snapshot`, ~line 61+)
   - Add: `mutual_cycles : snapshot -> (string * string list) list` — Tarjan's SCC or simple DFS cycle detection on `callers_of`/`callees_of` Hashtbls. Returns groups of functions that are mutually recursive.
   - Acceptance: unit-checkable via image tests (needs Store, so alcotest in image/ or verify script).

4. **Create `image/lib/ast_html.ml`** — Pure_html node tree renderer for ASTs.
   - `pp_expr_html : ?recursion:recursion_info -> ?mutual:(string list) -> expr -> Pure_html.node`
   - Mirrors the structure of `pp_expr_at` (`lisp/lib/reader.ml:426`) but emits `HTML.span [HTML.class_ "..."] [ ... ]` nodes instead of strings.
   - Self-reference sites get `class_ "ast-self-ref"`.
   - Tail-position sites get `class_ "ast-tail"`.
   - Non-tail recursion sites get `class_ "ast-non-tail"`.
   - Mutual recursion call sites (when the callee is in the `mutual` set) get `class_ "ast-mutual"`.
   - Falls back to `txt "%s" (Reader.pp_expr expr)` inside `HTML.pre` if no recursion info is passed (backward compat).
   - Uses `Pure_html` (available in image/, per `image/lib/dune:24`).
   - Acceptance: renders without exception on sample ASTs; grep `/binding/factorial` for `ast-self-ref`.

5. **Add `ast_html` to `image/lib/dune`** (`modules` stanza, line 6).
   - File: `image/lib/dune:6`

6. **Modify `image/lib/http_pages.ml`** — use `pp_expr_html` on binding pages.
   - File: `image/lib/http_pages.ml:8` — `show_binding_page` — replace `HTML.pre [] [txt "%s" pp]` with `pp_expr_html ~recursion:(Recursion.analyze_self ~fn_name:name expr) expr`.
   - File: `image/lib/http_pages.ml` — `list_bindings_page` code_pane (the `Some pkg, Some local` match, ~line 200+) — replace `HTML.pre [HTML.class_ "bb-source"] [txt "%s" pp]` with `pp_expr_html` call.
   - File: `image/lib/http_pages.ml` — `inspect_binding_page` (~line 380+) — replace `HTML.pre [] [txt "%s" pp]` in the "pretty" card with `pp_expr_html`.
   - Acceptance: `wget -qO- /binding/factorial | grep ast-self-ref` returns matches.

7. **Add CSS rules** — either in `libs/css/lib.pp` (Nopales CSS lib) or inline `<style>` in chrome template.
   - File: `image/lib/http_core.ml` (chrome template, ~line 118+) — add `<style>` block with `.ast-self-ref`, `.ast-tail`, `.ast-non-tail`, `.ast-mutual` rules (background highlight, border-left, etc.).
   - Acceptance: visual verification via browser screenshot.

8. **Create `lisp/test/recursion_test.ml`** — alcotest suite.
   - Test `analyze_self` on: factorial (tail-recursive if written with accumulator, non-tail if naive), map (non-tail via `cons`), fib (non-tail, exponential branching), mutual even/odd (mutual recursion).
   - Test `classify_tail_positions` on `if`/`seq`/`let`/`lambda` bodies.
   - File: `lisp/test/dune` — add `recursion_test` module + test stanza.

### Files to Modify
- `lisp/lib/recursion.ml` (new)
- `lisp/lib/dune` (add module)
- `image/lib/call_graph.ml` (add `mutual_cycles`)
- `image/lib/ast_html.ml` (new)
- `image/lib/dune` (add module)
- `image/lib/http_pages.ml` (3 call sites: show_binding_page, list_bindings_page code_pane, inspect_binding_page)
- `image/lib/http_core.ml` (CSS in chrome template)
- `lisp/test/recursion_test.ml` (new)
- `lisp/test/dune` (add test)

### Spec impact
- New subsection in `borge/lisp-interpreter.borg` after `effects` subsection (~line 1207): `observability.recursion-highlight`. File `borge issue new "recursion highlighting on /binding pages"` during plan mode. Code changes land in make mode.

### Risks
- `pp_expr_html` must handle ALL expr variants (Quote, Define, Prim, Letrec, etc.) — missing a variant would crash the page. The flat `pp_expr_at` fallback ensures backward compat.
- Mutual recursion detection requires loading all function_defs — already done by `Call_graph.build_snapshot` which loads via `Store.list_function_names`. No new DB queries needed.
- `classify_tail_positions` must match the evaluator's actual tail-position semantics. The trampoline in `eval` (eval.ml:428) treats `If` then/else, `Seq` second, `Let`/`Letrec` body as tail; `App` args and `If` test as non-tail. The classifier must mirror this exactly. Cross-check against `eval_step` (eval.ml:345+).

---

## Phase 2 — Eval step counter

### Tasks

1. **Modify `lisp/lib/eval.ml`** — add instrumentation fields to `budget` and hook the eval loop.
   - File: `lisp/lib/eval.ml:14` — extend `budget`:
     ```ocaml
     type budget = {
       mutable steps : int;
       limit : int;
       mutable current_fn : string option;      (* NEW *)
       mutable prim_calls : int;                 (* NEW *)
       mutable db_hits : int;                    (* NEW *)
       fn_step_counts : (string, int) Hashtbl.t; (* NEW *)
     }
     ```
   - File: `lisp/lib/eval.ml:21` — update `make_budget` to initialize new fields (`current_fn = None`, `prim_calls = 0`, `db_hits = 0`, `fn_step_counts = Hashtbl.create 16`).
   - File: `lisp/lib/eval.ml:49` — modify `tick` to also attribute the step to the current function:
     ```ocaml
     let tick (b : budget) : unit =
       b.steps <- b.steps + 1;
       (match b.current_fn with
        | Some fn ->
            let n = try Hashtbl.find b.fn_step_counts fn with Not_found -> 0 in
            Hashtbl.replace b.fn_step_counts fn (n + 1)
        | None -> ());
       if b.steps > b.limit then raise (Interp_error Budget_exceeded)
     ```
   - File: `lisp/lib/eval.ml:428` — modify `eval` trampoline to save/restore `current_fn`:
     ```ocaml
     and eval ~budget ~prims env e =
       let prev_fn = budget.current_fn in
       let rec loop env e =
         match eval_step ~budget ~prims env e with
         | Done v -> budget.current_fn <- prev_fn; v
         | Cont (env', e') -> loop env' e'
       in
       loop env e
     ```
     This ensures non-tail recursive `eval` calls restore the caller's `current_fn` when they return, and tail calls (which loop in the trampoline) inherit the new `current_fn` set by the App branch.
   - File: `lisp/lib/eval.ml:405` — modify `eval_step` App/Var branch to set `current_fn` when calling a VLambda:
     ```ocaml
     | Var name ->
         (match Env.lookup env name with
         | Some (VMacro ...) -> ...
         | _ ->
             let f = eval ~budget ~prims env head in
             let arg_vals = List.map (eval ~budget ~prims env) args in
             let result = apply_step ~budget ~prims f arg_vals in
             (match f, result with
              | VLambda _, Cont _ -> budget.current_fn <- Some name
              | _ -> ());
             result)
     ```
     Also handle the non-Var head case (anonymous lambda): leave `current_fn` unchanged (or set to `None`).
   - File: `lisp/lib/eval.ml:1020` — increment `prim_calls` in `apply_step` VPrim branch:
     ```ocaml
     | VPrim name ->
         budget.prim_calls <- budget.prim_calls + 1;
         Done (Control.perform_prim name args)
     ```
   - Also increment `prim_calls` in `eval_step` Prim branch (eval.ml:399):
     ```ocaml
     | Prim (name, args) ->
         budget.prim_calls <- budget.prim_calls + 1;
         ...
     ```
   - Acceptance: alcotest — eval `(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)` and check `budget.fn_step_counts` has `"fact"` with > 0 steps; `prim_calls` > 0.

2. **Modify `image/lib/prims_db.ml`** — wrap CRUD prims to count DB hits.
   - File: `image/lib/prims_db.ml:65` — modify `register_crud` to wrap the fn:
     ```ocaml
     let register_crud name fn =
       Primitives.register tbl name (fun args ->
         (match Pricklypear_lisp.Eval.current_budget () with
          | Some b -> b.db_hits <- b.db_hits + 1
          | None -> ());
         fn (coerce_sym_args args))
     ```
     Wait — `current_budget` is a `ref`, not a function. Use `!Pricklypear_lisp.Eval.current_budget`. Need to expose it (it's already `let current_budget : budget option ref = ref None` at eval.ml:28, public).
   - Acceptance: eval `(get 'user "some-id")` and check `budget.db_hits > 0`.

3. **Modify `image/lib/session.ml`** — persist function stats after route handler eval.
   - File: `image/lib/session.ml:1555-1575` — in the route handler `run ()` function, after `eval_string_rim t ("(" ^ handler_name ^ ")")`, read the budget stats and persist:
     - The budget is created inside `eval_string_direct` (eval.ml:1042: `let budget = make_budget ()`). It's not accessible from the session layer.
     - **Design decision**: expose the last-used budget. Add `Eval.last_budget : budget option ref` (module-level, like `current_budget`). Set it at the end of `eval_string_direct`. Then session.ml reads `Eval.last_budget` after the eval.
     - File: `lisp/lib/eval.ml:1042` — add `last_budget := Some budget` before `eval ...` (or after).
     - File: `image/lib/session.ml:1570` — after successful route handler eval:
       ```ocaml
       (match !Pricklypear_lisp.Eval.last_budget with
        | Some b ->
            (* Persist function_stat row: {fn: handler_name, steps: b.steps,
               prim_calls: b.prim_calls, db_hits: b.db_hits, timestamp: now} *)
            let _ = Crud.update_or_create t.store
              ~type_name:"function_stat"
              ~id:(handler_name ^ ":" ^ (string_of_int b.steps))  (* or UUID *)
              ~fields_json:(Yojson.Safe.to_string (`Assoc [
                ("fn", `String handler_name);
                ("steps", `Int b.steps);
                ("prim_calls", `Int b.prim_calls);
                ("db_hits", `Int b.db_hits);
                ("when", `String (Prims_pure.prim_now [] |> ...));
              ])) ~author:(t.manifest.username) in
            ()
        | None -> ());
       ```
     - Actually, dogfooding via `data_rows` requires a `(deftype 'function-stat ...)` first. This can be a one-time `deftype` in the boot seed (`libs/` or system/main). Or just use raw `Crud.create` without a declared type (the data browser works without type declaration — `data_rows` is schemaless jsonb).
     - Acceptance: after hitting a route, `rows 'function_stat` returns a row with the handler name and step count.

4. **Modify `image/lib/http_pages.ml`** — display "last run" stats on `/binding/<name>`.
   - File: `image/lib/http_pages.ml:8` — `show_binding_page` — after the `HTML.pre` (or `pp_expr_html`), add:
     ```ocaml
     let stats = Crud.find_rows (Session.store session) ~type_name:"function_stat"
       ~filters:[("fn", name)] ~limit:1 () in
     (* render "last run: N steps, M prim calls, K DB hits" *)
     ```
   - Also add to the `list_bindings_page` code_pane meta column.
   - Acceptance: `wget -qO- /binding/some-function | grep "last run"`.

5. **Extend `lisp/test/eval_test.ml`** — add step-counter tests.
   - Test that `budget.fn_step_counts` accumulates per-function steps.
   - Test that `budget.prim_calls` counts prim invocations.
   - Test that `budget.current_fn` is correctly set/restored through tail and non-tail calls.
   - Acceptance: `dune runtest` in lisp/ passes.

### Files to Modify
- `lisp/lib/eval.ml` (budget type, make_budget, tick, eval, eval_step App/Prim branches, apply_step VPrim, last_budget ref)
- `image/lib/prims_db.ml` (wrap register_crud with db_hits counter)
- `image/lib/session.ml` (persist function_stat after route handler eval)
- `image/lib/http_pages.ml` (display last-run stats)
- `lisp/test/eval_test.ml` (add step-counter tests)

### Spec impact
- Extend `borge/lisp-interpreter.borg` BUDGET section (~line 1500) to document `current_fn`, `fn_step_counts`, `prim_calls`, `db_hits`. File `borge issue new "eval step counter and per-function attribution"` during plan mode.

### Risks
- **Tail-call attribution correctness**: The save/restore in `eval` relies on the OCaml call stack for non-tail calls and the trampoline for tail calls. Must verify that deeply nested non-tail calls (e.g., `(* n (fact (- n 1)))`) correctly attribute the inner `fact` call's steps to `"fact"` and the outer `*` steps to the caller. Test with both naive factorial (non-tail) and accumulator factorial (tail).
- **Budget sharing**: The budget is shared across all nested `eval` calls (passed by ref). `last_budget` must be set per top-level `eval_string_direct` call, not per nested `eval`. Set it in `eval_string_direct` and `eval_expr_rim`, not in `eval`.
- **Performance**: Adding a Hashtbl lookup per tick adds O(1) overhead. Should be negligible (Hashtbl is fast), but benchmark a tight loop.
- **`current_budget` exposure**: `Eval.current_budget` is a `ref` (eval.ml:28). Image-layer code reads it via `!Pricklypear_lisp.Eval.current_budget`. Verify it's accessible (it's a public `let` binding).

---

## Phase 3 — DB cost annotations

### Tasks

1. **Create `image/lib/db_cost.ml`** — static and live DB cost model.
   - `type cost_tier = Constant | Linear | Index_seek | Seq_scan | Unknown`
   - `static_cost : string -> cost_tier` — maps DB prim names to cost tiers:
     - `get` → `Index_seek` (PK lookup by UUID)
     - `create` → `Constant` (INSERT)
     - `update` → `Constant` (UPDATE by PK)
     - `delete` → `Constant` (DELETE by PK)
     - `rows` → `Seq_scan` (full type scan, no WHERE)
     - `find-row` → `Index_seek` (filtered lookup)
     - `find-rows` → `Linear` (filtered scan with GIN)
     - `find-text` → `Linear` (substring scan)
   - `sql_template : string -> string option` — returns the SQL template for each DB prim (extracted from `Crud.*` implementations). This requires reading the actual SQL in `image/lib/crud.ml` and documenting it.
   - `explain_cost : Store.t -> string -> (string, string) result` — runs `EXPLAIN` on the SQL template with placeholder substitution. Cached per prim name (EXPLAIN result doesn't change between calls unless schema changes).
   - Acceptance: `db_cost.static_cost "get"` returns `Index_seek`.

2. **Modify `image/lib/prims_db.ml`** — wrap CRUD prims with live timing.
   - File: `image/lib/prims_db.ml:65` — extend `register_crud` wrapper to also time the call:
     ```ocaml
     let register_crud name fn =
       Primitives.register tbl name (fun args ->
         let b = !Pricklypear_lisp.Eval.current_budget in
         (match b with Some b -> b.db_hits <- b.db_hits + 1 | None -> ());
         let t0 = Mtime_clock.elapsed () in  (* or Unix.gettimeofday *)
         let result = fn (coerce_sym_args args) in
         let elapsed = Mtime_clock.elapsed () - t0 in
         (* record in function_stat or a db_timing Hashtbl *)
         result)
     ```
   - Timing requires a clock. `Unix.gettimeofday` is available (image/ depends on `unix`). Or use `Mtime` if available on poohstack — check `opam list`. If not, use `Unix.gettimeofday` for seconds precision, or `Sys.time ()` for OCaml CPU time.
   - Persist timing into `function_stat` rows (extend the schema from Phase 2 with `db_time_ms` field).

3. **Modify `image/lib/http_pages.ml`** — display DB cost annotations.
   - On `/binding/<name>`, after the "last run" stats, add a "DB cost" card:
     - Static tier badge for each DB prim used in the function body (derived from `Effect.infer` + `db_cost.static_cost`).
     - Live timing from the most recent `function_stat` row.
   - Acceptance: `wget -qO- /binding/some-db-function | grep "DB cost"`.

4. **Modify `image/lib/crud.ml`** — expose SQL templates for documentation.
   - Add a `sql_of : string -> string option` function that returns the SQL for each operation name, or add comments annotating each SQL string with the prim name.
   - This is a documentation/extraction step, not a behavior change.

### Files to Modify
- `image/lib/db_cost.ml` (new)
- `image/lib/dune` (add module)
- `image/lib/prims_db.ml` (wrap with timing)
- `image/lib/http_pages.ml` (display DB cost)
- `image/lib/crud.ml` (expose SQL templates)

### Spec impact
- New subsection `observability.db-cost` in `borge/lisp-interpreter.borg` (or `borge/storage.borg` since it's about DB interaction costs). File `borge issue new "DB cost annotations on /binding pages"`.

### Risks
- **EXPLAIN requires DB access** — the static tier (cost classification) is pure, but `explain_cost` requires a live PG connection. Must be lazy (only called when the user requests it, not on every page load).
- **Timing precision**: `Unix.gettimeofday` has millisecond precision. DB calls are typically < 1ms for PK lookups. Consider accumulating total DB time across a function call rather than per-call timing.
- **Clock dependency**: Verify `Mtime` is on the poohstack switch. If not, use `Unix.gettimeofday`. Don't add a new dependency without justification.

---

## Phase 4 — Heuristic big-O classifier

### Tasks

1. **Create `lisp/lib/complexity.ml`** — pure sexp pattern matcher for big-O classification.
   - `type complexity = O_constant | O_log_n | O_linear | O_n_log_n | O_quadratic | O_exponential | Unknown`
   - `classify : expr -> complexity` — pattern matches common shapes:
     - **O(1)**: no recursion, no loops, no `rows`/`list/map`/`list/foldl`.
     - **O(n)**: single `cdr`-recursion (self-call with `(cdr x)` as an arg), or `list/map`/`list/filter`/`list/foldl` over a single list.
     - **O(n log n)**: divide-and-conquer (self-call with `(halve x)` or `(/ x 2)` pattern, two recursive branches).
     - **O(n²)**: `rows` inside `list/map` (N DB calls × M rows), or nested `list/map` over the same list.
     - **O(2^n)**: branching recursion without memoization (two+ self-calls with `(- n 1)` or `(- n 2)` args, like naive fib).
     - **Unknown**: default when no pattern matches. Must be honest — never guess.
   - `classify_with_known : known_fn:(string, expr) Hashtbl.t -> fn_name:string -> expr -> complexity` — resolves cross-function calls (e.g., if `foo` calls `bar` which is O(n), `foo` is at least O(n)).
   - `badge : complexity -> string` — `"O(1)"`, `"O(n)"`, `"O(n log n)"`, `"O(n²)"`, `"O(2^n)"`, `"?"`.
   - Acceptance: alcotest — classify factorial (O(n)), map (O(n)), merge-sort (O(n log n)), naive fib (O(2^n)), rows-in-map (O(n²)), pure arithmetic (O(1)).

2. **Add `complexity` to `lisp/lib/dune`** modules list.

3. **Create `lisp/test/complexity_test.ml`** — alcotest suite.
   - Test each pattern shape with canonical examples.
   - Test cross-function classification (foo calls bar).
   - Test `Unknown` default for unstructured code.

4. **Modify `image/lib/http_pages.ml`** — display big-O badge on `/binding/<name>`.
   - File: `image/lib/http_pages.ml:8` — `show_binding_page` — add a complexity badge next to the effects badge:
     ```ocaml
     let cx = Complexity.classify_with_known
       ~known_fn:(Effect.known_fn_of_bindings ...) ~fn_name:name expr in
     HTML.span [HTML.class_ "badge complexity"] [txt "%s" (Complexity.badge cx)]
     ```
   - Also add to `list_bindings_page` meta column (next to effects badge).
   - Acceptance: `wget -qO- /binding/factorial | grep "O(n)"`.

5. **Optional: empirical curve-fit cross-check** (depends on Phase 2).
   - When `function_stat` rows exist for a function (from Phase 2), fit `steps(n)` at n=10/100/1000 (if the function can be called with different input sizes).
   - This requires a benchmarking prim: `(bench 'fn [10 100 1000])` that calls the function with synthetic inputs and records step counts. This is a separate sub-phase.
   - Mark as optional/stretch — the heuristic classifier is the primary deliverable.

### Files to Modify
- `lisp/lib/complexity.ml` (new)
- `lisp/lib/dune` (add module)
- `lisp/test/complexity_test.ml` (new)
- `lisp/test/dune` (add test)
- `image/lib/http_pages.ml` (display badge)

### Spec impact
- New subsection `observability.complexity` in `borge/lisp-interpreter.borg`. File `borge issue new "heuristic big-O complexity classifier"`.

### Risks
- **False positives**: The pattern matcher may misclassify uncommon shapes. The `Unknown` default mitigates this — always be honest. Document the patterns it recognizes.
- **Cross-function complexity**: `classify_with_known` requires all called functions' ASTs. If a callee is a prim (VPrim), its complexity is unknown (prims are opaque). The classifier should assume the worst (Unknown) or a configurable default.
- **`list/map` and `list/foldl` are special forms** (eval.ml:73-74) — they loop in OCaml, not in Nopales recursion. The classifier must recognize them as O(n) iteration, not recursive calls.

---

## Phase 5 — LLM complexity analysis on demand

### Tasks

1. **Create `image/lib/analyze.ml`** — the `(analyze 'fn)` prim implementation.
   - `prim_analyze : Session.t -> value list -> value` — takes a function name (VStr or VSymbol), looks up the AST, calls LLM, caches result.
   - Steps:
     1. Look up the function AST via `Store.lookup_function`.
     2. Stringify via `Reader.pp_expr`.
     3. Compute cache key: `sha1 (pp_expr expr)` — reuse the `sha1` prim or `Digestif` (image/ depends on `digestif`, per `image/lib/dune:26`).
     4. Check cache: `Crud.find_row store ~type_name:"function_analysis" ~field:"cache_hash" ~value:sha1_hex`.
     5. If cached, return the cached JSON.
     6. If not, call `Llm_client.call` with a structured prompt:
        ```
        System: "You are a code complexity analyzer. Respond as JSON: {complexity, confidence, rationale}"
        User: "Analyze this Lisp function for time complexity:\n\n{pp_expr}\n\nRespond as JSON."
        ```
     7. Parse the LLM response as JSON (`Yojson.Safe.from_string`). If parse fails, return an error VStr.
     8. Cache in `data_rows` as `function_analysis` type: `{cache_hash, fn_name, complexity, confidence, rationale, measured_steps (from function_stat if available)}`.
     9. Return the JSON as VStr.
   - Per-user LLM key: retrieve via `Auth_store.get_llm_key store ~user_id:(...)`, wrap with `Llm_client.with_creds`. Fall back to env `LLM_API_KEY`.
   - Acceptance: `(analyze "factorial")` returns a VStr JSON with complexity field.

2. **Register the `analyze` prim** — in `image/lib/session.ml` or `image/lib/prims_db.ml`.
   - File: `image/lib/session.ml` — in `register_session_control_prims` (line 179) or a new `register_observability_prims` function:
     ```ocaml
     Primitives.register tbl "analyze"
       (fun args ->
         match args with
         | [VStr name | VSymbol name] -> Analyze.prim_analyze t name
         | _ -> VStr "analyze: bad args (need function name)")
     ```
   - Add `analyze` to `prim_summary` in `primitives.ml` (~line 251+): `"analyze" -> "LLM complexity analysis of a function (cached)"`.

3. **Create `image/lib/http_analyze.ml`** — HTTP handler for the htmx button.
   - `POST /api/analyze/:name` — calls `prim_analyze`, returns an HTML fragment with the result.
   - Register the route in `image/lib/http.ml` (or wherever routes are wired).
   - The fragment: a `<div id="analyze-result">` with complexity, confidence, rationale, and a "LLM estimate" label.
   - If `function_stat` rows exist (Phase 2), include "measured: N steps" as cross-check.
   - Acceptance: `curl -X POST /api/analyze/factorial` returns HTML with complexity.

4. **Modify `image/lib/http_pages.ml`** — add htmx button on `/binding/<name>`.
   - File: `image/lib/http_pages.ml:8` — `show_binding_page` — after the AST display, add:
     ```ocaml
     HTML.button [
       HTML.class_ "btn";
       Hx.post (Printf.sprintf "/api/analyze/%s" name);
       Hx.target "#analyze-result";
       Hx.swap "innerHTML";
     ] [txt "Analyze (LLM)"];
     HTML.div [HTML.id "analyze-result"] [txt ""];
     ```
   - File: `image/lib/http_pages.ml` — `list_bindings_page` code_pane — same button.
   - Use `Pure_html` htmx attrs (`Hx.post`, `Hx.target`, `Hx.swap`) — NOT raw string tuples (poohstack rule).

5. **Add `/api/analyze/:name` route** — in `image/lib/http.ml`.
   - File: `image/lib/http.ml` — add route handler:
     ```ocaml
     | `POST, "/api/analyze/" :: rest -> http_analyze session reqd rest
     ```
   - Must be hard-private (kernel path, requires auth).

### Files to Modify
- `image/lib/analyze.ml` (new)
- `image/lib/http_analyze.ml` (new)
- `image/lib/dune` (add modules)
- `image/lib/session.ml` (register `analyze` prim)
- `image/lib/primitives.ml` (add `analyze` to prim_summary)
- `image/lib/http_pages.ml` (htmx button)
- `image/lib/http.ml` (route for `/api/analyze/:name`)

### Spec impact
- New subsection `observability.llm-analysis` in `borge/lisp-interpreter.borg` (or `borge/ai-authoring.borg` since it uses LLM). File `borge issue new "LLM complexity analysis prim and /binding button"`.

### Risks
- **LLM availability**: If no LLM key is configured (neither per-user nor env), the prim must return a clear error, not hang. `Llm_client.call` already returns `Error "no LLM key configured..."` (llm_client.ml:91).
- **Response parsing**: LLMs may not return clean JSON. Must handle parse failures gracefully — return `Error` with the raw text, or attempt to extract JSON from markdown code blocks.
- **Cache invalidation**: The cache key is `sha1(pp_expr)`. When the function is edited, `pp_expr` changes, so the hash changes, and the old cache is orphaned (not deleted — additive-only data_rows). This is acceptable (orphaned rows are small). Alternatively, `find-row` by `cache_hash` and `update` if found.
- **Cost**: Each LLM call costs money. The button is manual (on demand), not automatic. The cache prevents repeat calls. Label clearly as "LLM estimate" to distinguish from the heuristic classifier (Phase 4).
- **Per-user key retrieval**: `Auth_store.get_llm_key` (auth_store.ml:740+) returns `llm_key_meta` (last4, label, base, model) — but the actual key is stored in a `secret_json` field. Need to retrieve the full key, not just the meta. Check `Auth_store` for a `get_llm_key_secret` or similar function. If it doesn't exist, it must be added (but carefully — secrets must not leak).

---

## Phase 6 — Inside-out explainer

### Tasks

1. **Create `lisp/lib/explain.ml`** — mechanical sexp peel from innermost outward.
   - `type layer = { expr : expr; depth : int; kind : string; annotation : string }`
   - `peel : expr -> layer list` — produces layers from innermost to outermost:
     - Find the innermost sub-expression (deepest nesting).
     - Emit it as layer 0 with an annotation (e.g., "base case", "leaf value", "prim call").
     - Peel it (replace with a placeholder), emit the next layer.
     - Continue until the outermost expression.
   - `annotate_layer : known_fn:(string, expr) Hashtbl.t -> layer -> layer` — adds inferred info:
     - If the sub-expr is a `Prim` call, annotate with the prim name and its effect (from `Effect.effects_for_prim`).
     - If it's a `Var` bound to a known function, annotate with that function's complexity (from Phase 4, if available).
     - If it's in tail position (from Phase 1's `classify_tail_positions`), annotate "tail".
     - If it's a self-reference, annotate "recursive call".
   - `explain : known_fn:... -> fn_name:string -> expr -> layer list` — full pipeline: peel + annotate.
   - Acceptance: alcotest — explain factorial produces layers from `(- n 1)` (innermost) to `(if ...)` (outermost), each annotated.

2. **Add `explain` to `lisp/lib/dune`** modules list.

3. **Create `lisp/test/explain_test.ml`** — alcotest suite.
   - Test that `peel` produces correct depth ordering.
   - Test that `annotate_layer` adds correct annotations for prims, self-refs, tail positions.

4. **Register the `(explain 'fn)` prim** — in `image/lib/session.ml` or `image/lib/analyze.ml`.
   - `prim_explain : Session.t -> value list -> value` — calls `Explain.explain`, formats layers as a readable string or JSON, returns VStr.
   - Optional: for each layer, call `Llm_client.call` with a short prompt asking for a prose gloss of that single sub-expression. Cache keyed by `sha1(pp_expr sub_expr)`. Mark as optional — the mechanical peel + free annotations are the primary deliverable.

5. **Modify `image/lib/http_pages.ml`** — display the explainer on `/binding/<name>`.
   - Add an "Explain" section (collapsible `<details>` element) showing the layers:
     - Each layer as a `<div class="explain-layer">` with depth, kind, annotation, and the sub-expr (via `pp_expr_html` from Phase 1).
     - Optional LLM gloss per layer (if cached).
   - Acceptance: `wget -qO- /binding/factorial | grep "explain-layer"`.

### Files to Modify
- `lisp/lib/explain.ml` (new)
- `lisp/lib/dune` (add module)
- `lisp/test/explain_test.ml` (new)
- `lisp/test/dune` (add test)
- `image/lib/session.ml` or `image/lib/analyze.ml` (register `explain` prim)
- `image/lib/http_pages.ml` (display explainer)

### Spec impact
- New subsection `observability.explainer` in `borge/lisp-interpreter.borg`. File `borge issue new "inside-out explainer and (explain 'fn) prim"`.

### Risks
- **Peel correctness**: "Innermost" is ambiguous for non-tree expressions. Define innermost as "deepest nesting depth" (max distance from root). For siblings at the same depth, order left-to-right.
- **Placeholder replacement**: When peeling, the innermost sub-expr is replaced with a placeholder (e.g., `Var "_"`). This changes the expr structure. The peel must produce a list of (original sub-expr, context) pairs, not modify the original expr.
- **LLM gloss cost**: If the optional LLM gloss is enabled, each layer triggers an LLM call. For a 10-layer function, that's 10 calls. Cache aggressively. Make LLM gloss opt-in (a separate button or flag).

---

## Dependencies

```
Phase 1 (recursion highlighting) — no deps, standalone
Phase 2 (step counter) — no deps, standalone
Phase 3 (DB cost) — depends on Phase 2 (function_stat rows for live timing)
Phase 4 (big-O classifier) — no deps, standalone; benefits from Phase 1 (tail position data)
Phase 5 (LLM analysis) — depends on Phase 2 (measured steps cross-check), Phase 4 (heuristic cross-check); uses `llm_client.ml` (existing)
Phase 6 (explainer) — depends on Phase 1 (pp_expr_html, tail-position classification), Phase 4 (complexity annotations); optional LLM gloss uses `llm_client.ml`
```

Recommended implementation order: 1 → 2 → 4 → 3 → 5 → 6 (maximizes independence: 1, 2, 4 can be done in parallel).

---

## Borge discipline (all phases)

The repo is currently in **idle phase** (write-locked by borge). Per `AGENTS.md`:
- Spec changes go through **plan mode** (file `borge issue new "..."` for each phase's spec gap).
- Code changes land in **make mode** (after plan resolves issues).
- After every code change touching a `.borg`-declared module: run `borge drift` to verify spec/code alignment.
- The `lisp-interpreter.borg` chapter declares `eval`, `reader`, `effect`, `control`, `special_forms` modules. New modules (`recursion.ml`, `complexity.ml`, `explain.ml`) are NOT yet declared — they need borge entries.
- `image/lib/http_pages.ml` is declared in `borge/product-experience.borg` (or `borge/image.borg`). Changes to it require borge drift check.
- `image/lib/call_graph.ml` is declared in `borge/image.borg` (inspector section). Changes require borge drift.

---

## Test approach summary

| Phase | Pure tests (lisp/, no services) | Image tests (need switch, maybe PG) | Verify scripts (need dev stack) |
|---|---|---|---|
| 1 | `lisp/test/recursion_test.ml` — `analyze_self`, `classify_tail_positions` | — | `scripts/verify_recursion_highlight.sh` — grep `/binding/name` for CSS classes |
| 2 | `lisp/test/eval_test.ml` — step counts, current_fn, prim_calls | `image/test/` — db_hits counting via prims_db | `scripts/verify_step_counter.sh` — eval a route, check `function_stat` row |
| 3 | — | `image/test/` — `db_cost.static_cost` | `scripts/verify_db_cost.sh` — grep `/binding/name` for DB cost badge |
| 4 | `lisp/test/complexity_test.ml` — classify patterns | — | `scripts/verify_complexity.sh` — grep `/binding/name` for O(*) badge |
| 5 | — | `image/test/` — `prim_analyze` mock (no real LLM) | `scripts/verify_analyze.sh` — POST `/api/analyze/name` (mock LLM or skip if no key) |
| 6 | `lisp/test/explain_test.ml` — `peel`, `annotate_layer` | — | `scripts/verify_explain.sh` — grep `/binding/name` for explain-layer |

Build commands: `eval $(opam env --switch=poohstack --set-switch)` → `dune build` → `dune runtest` (lisp/ tests need no services; image/ tests need the switch).

---

## Open questions (need clarification before implementation)

1. **Phase 2 — `last_budget` exposure**: Is adding a module-level `last_budget` ref to `lisp/lib/eval.ml` acceptable? It's a mutable ref in the pure package, but `current_budget` already sets the precedent. Alternatively, thread the budget through `eval_string_rim` return value (more invasive).

2. **Phase 3 — Timing clock**: Is `Mtime` available on the poohstack switch? If not, is `Unix.gettimeofday` (millisecond precision) sufficient for DB call timing? DB calls may be sub-millisecond.

3. **Phase 5 — LLM key retrieval**: `Auth_store.get_llm_key` returns `llm_key_meta` (last4, label, base, model) — not the actual secret. Is there a function to retrieve the full API key for use with `Llm_client.with_creds`? If not, one must be added to `auth_store.ml` (carefully — the key is a secret).

4. **Phase 5 — `function_analysis` cache schema**: Should the cache be a `data_rows` type (dogfooding, schemaless) or a dedicated `function_analysis` table (migration)? `data_rows` is consistent with the existing pattern but orphans stale rows. A dedicated table with `ON DELETE CASCADE` referencing `function_defs` would auto-invalidate. This is a design decision.

5. **Phase 1 — `pp_expr_html` vs `pp_expr_at` modification**: Should `pp_expr_html` in `image/lib/ast_html.ml` be a fresh implementation mirroring `pp_expr_at`'s logic, or should `pp_expr_at` in `lisp/lib/reader.ml` be refactored to emit a structured tree (variant) that both the string renderer and the HTML renderer consume? The latter is cleaner but touches the pure package's public API.

---

acceptance-report