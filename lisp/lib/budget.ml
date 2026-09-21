(** debug call-tree node: open while its body steps (d_raw = -1),
    closed with the INCLUSIVE step total at scope exit; self-steps are
    derived at dump time (inclusive minus children). Pure accounting —
    no interpreter dependency, like the rest of this module. *)
type dbg_node = {
  d_name : string;
  d_parent : int option;
  mutable d_entry : int;
  mutable d_raw : int;
  mutable d_calls : int;
}

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
  (* application COUNT per fn — the self-steps table's companion. The
     step table shows WHERE steps go; this shows HOW MANY invocations
     paid for them, so cheap-but-hot fns (call-count bugs like the
     html/block-member? walk) stand out against heavy-body fns with the
     same self-step total. One Hashtbl update per VLambda application
     (eval.ml), NOT per step. *)
  fn_call_counts : (string, int) Hashtbl.t;
  (* debug call-tree capture (PP_PROFILE_TREE): one node per VLambda
     invocation SITE-INSTANCE, parent-linked by the same nesting the
     flat current_fn attribute approximates. Greedy memory per eval,
     so it exists only when the env flag is set — dbg_tree is false
     (and the structures stay empty) in every production request. *)
  dbg_tree : bool;
  mutable dbg_serial : int;
  dbg_nodes : (int, dbg_node) Hashtbl.t;
  mutable dbg_cur : int option;
  (* image.db-panel: per DB-prim breakdown — prim name → (calls, db ms).
     Filled at the register_crud choke point (note_prim_db); read by
     Timing.measure through the fiber-keyed slot so /timing can show
     WHICH prims an endpoint ran. One Hashtbl update per DB prim call —
     negligible next to the query it wraps. *)
  prim_db : (string, int * float) Hashtbl.t;
}

(** debug call-tree nodes live in [dbg_nodes]; see [dbg_node] above. *)

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
    fn_call_counts = Hashtbl.create 16;
    dbg_tree = Sys.getenv_opt "PP_PROFILE_TREE" <> None;
    dbg_serial = 0;
    dbg_nodes = Hashtbl.create 64;
    dbg_cur = None;
    prim_db = Hashtbl.create 8;
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

(** Bill one DB-prim call under [name] with [ms] of DB wall time onto
    the per-prim breakdown (image.db-panel). No-op outside an eval. *)
let note_prim_db (name : string) (ms : float) : unit =
  match current () with
  | Some b ->
    let calls, tot = try Hashtbl.find b.prim_db name with Not_found -> (0, 0.) in
    Hashtbl.replace b.prim_db name (calls + 1, tot +. ms)
  | None -> ()

(** One VLambda application of [name] (eval.ml; NOT per step). No-op
    outside an eval like the other note_* entry points. *)
let note_fn_call (name : string) : unit =
  match current () with
  | Some b ->
    let n = try Hashtbl.find b.fn_call_counts name with Not_found -> 0 in
    Hashtbl.replace b.fn_call_counts name (n + 1)
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

(** {3 debug call tree (PP_PROFILE_TREE)} — nodes are the [dbg_node]
    record declared above. *)

let dbg_reconcile (b : budget) (name : string) : unit =
  if b.dbg_tree then
    (* pop frames until the open top IS this fn — the flat [current_fn]
       attribute is approximate at trampoline boundaries, so ANY fn
       transition (set or restore) reconciles the frame stack. *)
    let rec pop () =
      match b.dbg_cur with
      | None -> ()
      | Some id -> (
          match Hashtbl.find_opt b.dbg_nodes id with
          | Some n when n.d_name <> name ->
              n.d_raw <- b.steps - n.d_entry;
              b.dbg_cur <- n.d_parent;
              pop ()
          | _ -> ())
    in
    pop ()

let dbg_enter (b : budget) (name : string) : unit =
  if b.dbg_tree then begin
    dbg_reconcile b name;
    b.dbg_serial <- b.dbg_serial + 1;
    let id = b.dbg_serial in
    Hashtbl.replace b.dbg_nodes id
      { d_name = name;
        d_parent = b.dbg_cur;
        d_entry = b.steps;
        d_raw = -1;
        d_calls = 1 };
    b.dbg_cur <- Some id
  end

(** Indented depth-first tree in creation order; repeated sibling runs
    collapse into one line (calls summed, incl summed) — the raw count
    of attribute-helper applications is in the thousands per render.
    Single request, single fiber. *)
let dbg_dump (b : budget) : string =
  if not b.dbg_tree then ""
  else
    let byparent : (int option, int list ref) Hashtbl.t =
      Hashtbl.create 16 in
    Hashtbl.iter
      (fun id (n : dbg_node) ->
        let slot =
          match Hashtbl.find_opt byparent n.d_parent with
          | Some r -> r
          | None ->
              let r = ref [] in
              Hashtbl.replace byparent n.d_parent r;
              r
        in
        slot := !slot @ [ id])
      b.dbg_nodes;
    let buf = Buffer.create 4096 in
    let raw_of (k : dbg_node) : int =
      if k.d_raw < 0 then b.steps - k.d_entry else k.d_raw
    in
    let line ~prefix ~name ~calls ~incl =
      Buffer.add_string buf
        (Printf.sprintf "%s%s calls=%d incl=%d\n" prefix name calls incl)
    in
    let rec emit ~id ~prefix =
      match Hashtbl.find_opt b.dbg_nodes id with
      | None -> ()
      | Some (n : dbg_node) ->
          let kids =
            match Hashtbl.find_opt byparent (Some id) with
            | Some r -> !r
            | None -> []
          in
          line ~prefix ~name:n.d_name ~calls:n.d_calls ~incl:(raw_of n);
          let rec walk = function
            | [] -> ()
            | kid :: rest -> (
                match Hashtbl.find_opt b.dbg_nodes kid with
                | None -> walk rest
                | Some (k : dbg_node) ->
                    let rec gather acc = function
                      | next :: more -> (
                          match Hashtbl.find_opt b.dbg_nodes next with
                          | Some (kn : dbg_node) when kn.d_name = k.d_name ->
                              gather (next :: acc) more
                          | _ -> gather acc (next :: more))
                      | [] -> (List.rev acc, [])
                    in
                    let run, rest2 = gather [ kid ] rest in
                    let calls =
                      List.fold_left
                        (fun acc n2 ->
                          match Hashtbl.find_opt b.dbg_nodes n2 with
                          | Some kn -> acc + kn.d_calls
                          | None -> acc)
                        0 run
                    in
                    let incl =
                      List.fold_left
                        (fun acc n2 ->
                          match Hashtbl.find_opt b.dbg_nodes n2 with
                          | Some kn -> acc + raw_of kn
                          | None -> acc)
                        0 run
                    in
                    let collapsed = List.length run > 1 in
                    line
                      ~prefix:(if collapsed then prefix ^ "  x" else prefix ^ "  ")
                      ~name:k.d_name ~calls ~incl;
                    (match List.rev run with
                     | [ only ] -> emit ~id:only ~prefix:(prefix ^ "  ")
                     | last :: _ ->
                         (* descend into the run's LAST instance only *)
                         emit ~id:last ~prefix:(prefix ^ "    ")
                     | [] -> ());
                    walk rest2)
          in
          walk kids
    in
    (match Hashtbl.find_opt byparent None with
     | Some roots -> List.iter (fun r -> emit ~id:r ~prefix:"") !roots
     | None -> ());
    Buffer.contents buf
