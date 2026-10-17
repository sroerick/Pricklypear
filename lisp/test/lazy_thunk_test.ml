(** Tests for lisp-interpreter.lazy-env-thunks: VThunk cells, forcing at
    Env.lookup (single observation point), memoization of value AND
    failure, cycle detection, and the explicit (delay)/(force) forms.

    Pure tests — no services. Per spec borge/lisp-interpreter.borg. *)
open Pricklypear_lisp
open Ast

let eval_str env s = Eval.eval_string ~prims:Eval.empty_prims env s

(* A counting evaluator: forces via Eval like the kernel's delay does,
   but records how many times it ran. *)
let counting_eval (count : int ref) (env : env) (expr : expr) : value =
  incr count;
  let budget = Eval.make_budget () in
  Eval.eval ~budget ~prims:Eval.empty_prims env expr

let delay_force_roundtrip () =
  let env = Env.create () in
  let v = eval_str env "(force (delay ((lambda (x) x) 3)))" in
  Alcotest.(check string) "force(delay)" "3" (Reader.pp_value v)

let force_non_thunk_is_identity () =
  let env = Env.create () in
  let v = eval_str env "(force 41)" in
  Alcotest.(check string) "force non-thunk" "41" (Reader.pp_value v)

let delay_does_not_eval_at_definition () =
  let env = Env.create () in
  (* If delay were eager, the error in the body would raise here. *)
  let v = eval_str env "(delay (error \"boom\" \"test\"))" in
  Alcotest.(check string) "delay is lazy" "#<thunk>" (Reader.pp_value v)

let force_runs_body_once_memoizes () =
  let env = Env.create () in
  let count = ref 0 in
  (* (define t (delay ...)) keeps define EAGER but the thunk lazy.
     Use side-effect-free counting via the OCaml evaluator instead. *)
  Env.define_thunk env "recipe" ~force:(counting_eval count)
    (App (Lambda ([ "x" ], Var "x"), [ Lit (VInt 42L) ]));
  ignore (eval_str env "recipe");
  ignore (eval_str env "recipe");
  let v = eval_str env "recipe" in
  Alcotest.(check string) "forced value" "42" (Reader.pp_value v);
  Alcotest.(check int) "body ran once" 1 !count

let failure_is_memoized () =
  let env = Env.create () in
  let count = ref 0 in
  Env.define_thunk env "bad" ~force:(counting_eval count)
    (App (Var "error", [ Lit (VStr "test"); Lit (VStr "nope") ]));
  let raised_first =
    try ignore (eval_str env "bad"); false
    with Interp_error (User_error ("nope", _)) -> true
  in
  let raised_second =
    try ignore (eval_str env "bad"); false
    with Interp_error (User_error ("nope", _)) -> true
  in
  Alcotest.(check bool) "first force raised" true raised_first;
  Alcotest.(check bool) "second touch replays failure" true raised_second;
  Alcotest.(check int) "body ran once despite failure" 1 !count

(* image.audit-2026-10-05: a force that fails with Budget_exceeded must
   NOT memoize the failure — budget exhaustion is context-dependent (the
   request that first touched the binding happened to be nearly spent),
   and replaying it poisoned the binding for the whole session (every
   later /notes render 500'd at full budget until rehydrate). The cell
   must stay Unforced so a later lookup RETRIES the body under its own
   (fresh) budget. *)
let budget_failure_is_retryable () =
  let env = Env.create () in
  let count = ref 0 in
  let mode = ref `fail in
  let gated_eval (_env : env) (_expr : expr) : value =
    incr count;
    match !mode with
    | `fail -> raise (Interp_error Budget_exceeded)
    | `ok -> VInt 7L
  in
  Env.define_thunk env "recipe" ~force:gated_eval
    (App (Lambda ([ "x" ], Var "x"), [ Lit (VInt 7L) ]));
  let raised_first =
    try ignore (eval_str env "recipe"); false
    with Interp_error Budget_exceeded -> true
  in
  Alcotest.(check bool) "first force raised budget" true raised_first;
  mode := `ok;
  let v = eval_str env "recipe" in
  Alcotest.(check string) "second touch retries and succeeds" "7"
    (Reader.pp_value v);
  ignore (eval_str env "recipe");
  Alcotest.(check int) "success memoized (no third run)" 2 !count

(* card 4c6d3e9f (2026-10-15): the budget carve-out generalizes. A
   Prim_error "denied" is SEAT-DEPENDENT (delegation tier, ws writes) -
   the same body forces fine as admin or after a tier change, so
   memoizing it pinned the filmstrip library for delegated seats
   (2026-10-14 load-library abort). Must stay retryable. *)
let denied_failure_is_retryable () =
  let env = Env.create () in
  let count = ref 0 in
  let mode = ref `fail in
  let gated_eval (_env : env) (_expr : expr) : value =
    incr count;
    match !mode with
    | `fail ->
        raise
          (Interp_error
             (Prim_error ("reseed-package", "denied for delegated sessions", "denied")))
    | `ok -> VStr "seeded"
  in
  Env.define_thunk env "seed" ~force:gated_eval
    (App (Lambda ([ "x" ], Var "x"), [ Lit (VInt 1L) ]));
  let raised_first =
    try ignore (eval_str env "seed"); false
    with Interp_error (Prim_error (_, _, "denied")) -> true
  in
  Alcotest.(check bool) "first force raised denied" true raised_first;
  mode := `ok;
  let v = eval_str env "seed" in
  Alcotest.(check string) "second touch retries and succeeds" "\"seeded\""
    (Reader.pp_value v);
  ignore (eval_str env "seed");
  Alcotest.(check int) "success memoized (no third run)" 2 !count

(* card 4c6d3e9f: a thunk referencing a name that registers LATER (a
   package loading in order) forces Unbound_var when first touched
   mid-load; memoizing that poisons the binding even after the load
   completes. Must stay retryable - after the name registers, the next
   touch succeeds and memoizes. *)
let unbound_failure_is_retryable () =
  let env = Env.create () in
  let count = ref 0 in
  let registered = ref false in
  let gated_eval (e : env) (expr : expr) : value =
    incr count;
    if not !registered then raise (Interp_error (Unbound_var "later-form"))
    else
      Eval.eval ~budget:(Eval.make_budget ()) ~prims:Eval.empty_prims e expr
  in
  Env.define_thunk env "late-ref" ~force:gated_eval
    (App (Var "later-form", [ Lit (VInt 1L) ]));
  let raised_first =
    try ignore (eval_str env "late-ref"); false
    with Interp_error (Unbound_var "later-form") -> true
  in
  Alcotest.(check bool) "first force raised unbound" true raised_first;
  (* the package finishes loading: the name registers *)
  ignore (eval_str env "(define (later-form x) x)");
  registered := true;
  let v = eval_str env "late-ref" in
  Alcotest.(check string) "second touch retries and succeeds" "1"
    (Reader.pp_value v);
  ignore (eval_str env "late-ref");
  Alcotest.(check int) "success memoized (no third run)" 2 !count

let cyclic_force_raises () =
  let env = Env.create () in
  (* self refers to itself: forcing marks Forcing, the inner lookup
     hits Forcing -> cyclic error (not divergence). *)
  Env.define_thunk env "self" ~force:(counting_eval (ref 0)) (Var "self");
  let raised =
    try ignore (eval_str env "self"); false
    with Interp_error (User_error (_, "cyclic-thunk")) -> true
  in
  Alcotest.(check bool) "cycle detected" true raised

let forcing_resolves_through_fully_linked_env () =
  let env = Env.create () in
  (* recipe is bound BEFORE recipe-fn is defined, and its body calls
     recipe-fn. Strict load would fail (unbound); lazy force after the
     env is fully linked works. This is the rb/*-before-recipe case. *)
  let count = ref 0 in
  Env.define_thunk env "rb/cola" ~force:(counting_eval count)
    (App (Var "recipe", [ Lit (VStr "Cola") ]));
  ignore (eval_str env "(define (recipe name) name)");
  let v = eval_str env "rb/cola" in
  Alcotest.(check string) "resolved via linked env"
    "\"Cola\"" (Reader.pp_value v);
  Alcotest.(check int) "body ran once" 1 !count

let tests = [
  ("delay/force round-trip", `Quick, delay_force_roundtrip);
  ("force on non-thunk is identity", `Quick, force_non_thunk_is_identity);
  ("delay does not eval at definition", `Quick, delay_does_not_eval_at_definition);
  ("force runs body once, memoizes", `Quick, force_runs_body_once_memoizes);
  ("failure is memoized", `Quick, failure_is_memoized);
  ("budget failure is retryable (not memoized)", `Quick, budget_failure_is_retryable);
  ("denied failure is retryable (not memoized)", `Quick, denied_failure_is_retryable);
  ("unbound failure is retryable (not memoized)", `Quick, unbound_failure_is_retryable);
  ("cyclic force raises", `Quick, cyclic_force_raises);
  ("forcing sees fully linked env", `Quick, forcing_resolves_through_fully_linked_env);
]

let () = Alcotest.run "lazy_thunk" [ ("lazy-env-thunks", tests) ]
