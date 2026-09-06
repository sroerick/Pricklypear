(** Eval budget accounting — split out of eval.ml (i-1788671219 walker
    split) so the god module sheds its accounting concern. Pure: no I/O,
    no interpreter dependency; eval.ml re-exports the names via aliases
    so the [Eval.budget] surface (image/lib/session.ml, remote, agent,
    cli, tests) is unchanged.

    A budget tracks a per-call step limit. Counted down at each eval
    step. When zero, raises [Interp_error Budget_exceeded]. *)
open Ast

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

(** Dynamic binding for the budget of the eval currently on the stack.
    Lets OCaml prims like [budget-remaining] and nested apply helpers
    share the caller's step counter without threading budget through
    the prim ABI. *)
let current_budget : budget option ref = ref None

(** Last top-level budget from [eval_string_direct] / rim entry — for
    image-layer function_stat persistence. *)
let last_budget : budget option ref = ref None

(** Remaining steps on the current budget (0 if none active). *)
let budget_remaining_steps () : int =
  match !current_budget with
  | None -> 0
  | Some b -> max 0 (b.limit - b.steps)

(** Absolute step count on the current budget (0 if none active). Used by
    the remote-eval stepping trace (borge/remote-eval.borg stepping) to
    attribute the span of local pure steps between two wire crossings: the
    trace samples [current_steps] right before a prim forwards over the
    wire, so the difference counts the local steps since the last sample.
    Additive; harmless when no stepping controller is active. *)
let current_steps () : int =
  match !current_budget with
  | None -> 0
  | Some b -> b.steps

(** Record one DB hit on the current budget, if any. *)
let note_db_hit () : unit =
  match !current_budget with
  | Some b -> b.db_hits <- b.db_hits + 1
  | None -> ()

(** Add [ms] to the current budget's accumulated DB time, if any. *)
let note_db_ms (ms : float) : unit =
  match !current_budget with
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
