(** Eval budget accounting — split out of eval.ml (i-1788671219 walker
    split) so the god module sheds its accounting concern. Pure: no I/O,
    no interpreter dependency; eval.ml re-exports the names via aliases
    so the [Eval.budget] surface (image/lib/session.ml, remote, agent,
    cli, tests) is unchanged.

    A budget tracks a per-call step limit. Counted down at each eval
    step. When zero, raises [Interp_error Budget_exceeded]. *)
open Ast
module Effect = Stdlib.Effect

type budget = {
  mutable steps : int;
  limit : int;
  mutable current_fn : string option;
  mutable prim_calls : int;
  mutable db_hits : int;
  mutable db_ms : float;
  fn_step_counts : (string, int) Hashtbl.t;
}

(** Default per-call step limit for [make_budget]. Raised 100k -> 400k
    2026-09-03 (budget-exceeded loops): DB time does not consume steps,
    but the ui shell re-render alone costs ~71k steps (ui/css 56k,
    ui/nav 4k, ui/home 11k) whenever a page session refreshes it, so
    real pages like todo/page could not fit 100k alongside the render.
    400k steps ~= 0.25s of pure eval CPU; wall time is still bounded by
    the request gate. *)
let default_limit = 400_000

(** Allocate a fresh [budget] with the given [limit] (default
    [default_limit]); counters start at zero. *)
let make_budget ?(limit = default_limit) () : budget =
  {
    steps = 0;
    limit;
    current_fn = None;
    prim_calls = 0;
    db_hits = 0;
    db_ms = 0.;
    fn_step_counts = Hashtbl.create 16;
  }

(** {2 Concurrency contract (i-1788670103-5)}

    The server forks a fiber per request and DB prims yield (Eio await
    inside the prim body), so a plain mutable global is clobbered
    mid-prim: while request A's DB prim awaits, request B's eval steps
    publish their own budget, and A's post-await accounting read would
    charge B. The authoritative read is therefore FIBER-SCOPED dynamic
    binding: eval entry points ([eval_string_direct],
    [read_eval_all_direct], image [eval_expr_rim], the thunk-force
    closures, [Example_eval]) establish the scope with [with_scope], and
    [current ()] performs the [Scope] effect — the innermost handler on
    the CALLING fiber answers, so a suspended fiber's budget is
    unreachable by other fibers no matter how they interleave.

    [current_budget] remains only as a FALLBACK publish channel for
    evals driven outside any scope (kept fresh by the per-step publish
    in [Eval.eval_step]); scope-less reads are best-effort, exactly as
    before this fix. Reads that happen synchronously after a publish
    (pure prims like [budget-remaining], thunk force at lookup time)
    were already safe; post-yield reads inside prim bodies (DB
    accounting) are the case [with_scope] closes.

    [last_budget] is a different channel: written once at eval entry
    for single-threaded consumers (cli, tests, explain's stepping
    trace). Production cross-eval consumers must NOT read it after a
    yield point: function_stat persistence receives the budget object
    explicitly from the session eval path, and /timing reads a
    fiber-keyed slot (image/lib/timing.ml) — both race-free by
    construction. *)

(** Fallback publish channel for the budget of the eval currently on
    the stack (see the concurrency contract above). Lets OCaml prims
    like [budget-remaining] and nested apply helpers share the caller's
    step counter without threading budget through the prim ABI. *)
let current_budget : budget option ref = ref None

(** Last top-level budget from [eval_string_direct] / rim entry — for
    image-layer function_stat persistence. Single-threaded channel
    only; see the concurrency contract above. *)
let last_budget : budget option ref = ref None

(** {3 Fiber-scoped dynamic binding} *)

(** Query the innermost fiber-scoped budget. Performed by [current];
    answered by the innermost [with_scope] handler on the calling
    fiber. *)
type _ Effect.t += Scope : budget option Effect.t

(** Establish [b] as the current budget for the dynamic extent of [f].
    Fiber-safe under arbitrary interleaving: each suspended fiber
    resumes inside its OWN handler frame, so concurrent evals never see
    each other's cell; nesting shadows (innermost wins) and the outer
    binding becomes visible again when the inner extent exits. *)
let with_scope (b : budget option) (f : unit -> 'a) : 'a =
  let cell = ref b in
  let open Effect.Deep in
  match_with f ()
    { retc = Fun.id
    ; exnc = raise
    ; effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Scope -> Some (fun (k : (a, _) continuation) -> continue k !cell)
          | _ -> None)
    }

(** The budget of the eval on the calling fiber: the innermost
    [with_scope] scope, falling back to the [current_budget] publish
    ref when no scope is installed (evals driven outside an entry
    point). The fallback catch is precise — only the unhandled-scope
    case degrades to the ref; exceptions raised by the resumed
    computation (e.g. Budget_exceeded) propagate normally. *)
let current () : budget option =
  match Effect.perform Scope with
  | b -> b
  | exception Effect.Unhandled _ -> !current_budget

(** Like [current], but always returns a usable budget (a fresh one
    when no scope/ref is active) — the thunk-force entry contract:
    "the request that forces pays". *)
let current_or_fresh () : budget =
  match current () with Some b -> b | None -> make_budget ()

(** Remaining steps on the current budget (0 if none active). *)
let budget_remaining_steps () : int =
  match current () with
  | None -> 0
  | Some b -> max 0 (b.limit - b.steps)

(** Absolute step count on the current budget (0 if none active). Used by
    the remote-eval stepping trace (borge/remote-eval.borg stepping) to
    attribute the span of local pure steps between two wire crossings: the
    trace samples [current_steps] right before a prim forwards over the
    wire, so the difference counts the local steps since the last sample.
    Additive; harmless when no stepping controller is active. *)
let current_steps () : int =
  match current () with
  | None -> 0
  | Some b -> b.steps

(** Record one DB hit on the current budget, if any. Resolves through
    the fiber scope, so post-yield calls inside prim bodies charge the
    CALLING eval's budget, not whichever request published last. *)
let note_db_hit () : unit =
  match current () with
  | Some b -> b.db_hits <- b.db_hits + 1
  | None -> ()

(** Add [ms] to the current budget's accumulated DB time, if any. *)
let note_db_ms (ms : float) : unit =
  match current () with
  | Some b -> b.db_ms <- b.db_ms +. ms
  | None -> ()

(** Advance [b]'s step count (tracking per-function counts) and raise
    [Interp_error Budget_exceeded] once the limit is crossed. *)
let tick (b : budget) : unit =
  b.steps <- b.steps + 1;
  (match b.current_fn with
   | Some fn ->
       let n = try Hashtbl.find b.fn_step_counts fn with Not_found -> 0 in
       Hashtbl.replace b.fn_step_counts fn (n + 1)
   | None -> ());
  if b.steps > b.limit then raise (Interp_error Budget_exceeded)
