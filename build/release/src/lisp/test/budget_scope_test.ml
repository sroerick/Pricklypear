(** Fiber-scoped eval budget (i-1788670103-5).

    The server forks a fiber per request and DB prims yield (Eio await
    inside the prim body), so the old plain [current_budget] ref was
    clobbered mid-prim: while request A's DB prim awaited, request B's
    eval steps published their own budget, and A's post-await
    accounting read charged B. The fix is an effect-scoped dynamic
    binding ([Budget.with_scope] / [Budget.current]) established at
    eval entry points; these tests pin the scoping semantics AND the
    concurrency bug class with a deterministic single-fiber
    simulation: a prim body that first drives ANOTHER eval (publishing
    the ref — exactly what a concurrent request fiber does) and then
    performs the accounting read. Pure tests — no services. *)
open Pricklypear_lisp

(* Physical equality on budget objects (same-object identity checks). *)
let same_budget (x : Eval.budget option) (y : Eval.budget option) =
  match (x, y) with Some x, Some y -> x == y | None, None -> true | _ -> false

(* Prims are VPrim-name values resolved at apply time against the rim's
   table (image binds the kernel catalog into the env as VPrim; pure
   tests bind by hand). "+" is a real adder so nested evals parse+eval. *)
let add (args : Ast.value list) : Ast.value =
  match args with
  | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.add a b)
  | _ -> raise (Ast.Interp_error (Ast.User_error ("+ needs two ints", "bad-args")))

let bind_prims (env : Env.env) (prims : Eval.prim_table) names =
  List.iter
    (fun name ->
      Env.define env name (Ast.VPrim name);
      Eval.register_prim prims name add)
    names

(* Parse + eval one string under an explicit budget/prim table, the way
   hosts drive nested evals. *)
let eval_str ~(budget : Eval.budget) ~(prims : Eval.prim_table)
    (env : Env.env) (s : string) : Ast.value =
  Eval.eval ~budget ~prims env (Reader.read_one s)

let scope_tests =
  [
    ( "scope: innermost wins",
      `Quick,
      fun () ->
        let a = Eval.make_budget () and b = Eval.make_budget () in
        Eval.with_scope (Some a) (fun () ->
            Alcotest.(check bool) "outer" true
              (same_budget (Eval.current ()) (Some a));
            Eval.with_scope (Some b) (fun () ->
                Alcotest.(check bool) "inner shadows outer" true
                  (same_budget (Eval.current ()) (Some b)));
            (* the outer binding is visible again on inner exit *)
            Alcotest.(check bool) "outer re-emerges" true
              (same_budget (Eval.current ()) (Some a))) );
    ( "scope: fallback ref when no scope is installed",
      `Quick,
      fun () ->
        let prev = !Eval.current_budget in
        let b = Eval.make_budget () in
        Eval.current_budget := Some b;
        let got = Eval.current () in
        Eval.current_budget := prev;
        Alcotest.(check bool) "ref fallback" true
          (same_budget got (Some b)) );
    ( "scope: current_or_fresh falls back to a fresh budget",
      `Quick,
      fun () ->
        let prev = !Eval.current_budget in
        Eval.current_budget := None;
        let b = Eval.current_or_fresh () in
        Eval.current_budget := prev;
        Alcotest.(check int) "fresh budget starts at 0 steps" 0 b.steps );
  ]

let db_accounting_tests =
  [
    ( "db accounting survives ref pollution mid-prim (THE fix)",
      `Quick,
      fun () ->
        (* Request A's eval, fiber-scoped. Its DB prim simulates a
           yield: before charging db_ms, a concurrent request's eval
           runs (unscoped — its steps publish the global ref, the
           exact interference a suspended fiber suffers), then the
           accounting read happens. The charge must land on A's
           budget via the scope, never on the polluter. *)
        let a = Eval.make_budget () in
        let polluter = Eval.make_budget () in
        let prims = Eval.make_prim_table () in
        let env = Env.create () in
        bind_prims env prims [ "+" ];
        Eval.register_prim prims "db-prim" (fun _ ->
            (* concurrent request B evaluates, publishing the ref *)
            ignore
              (eval_str ~budget:polluter ~prims:Eval.empty_prims env
                 "(+ 1 2)");
            if not (same_budget !Eval.current_budget (Some polluter)) then
              Alcotest.fail "setup: pollution did not publish the ref";
            (* post-yield accounting read (inside the prim body) *)
            Eval.note_db_ms 5.0;
            Eval.note_db_hit ();
            Ast.VNil);
        Env.define env "db-prim" (Ast.VPrim "db-prim");
        Eval.with_scope (Some a) (fun () ->
            ignore
              (Control.run_with_rim ~prims (fun () ->
                   eval_str ~budget:a ~prims env "(db-prim)")));
        Alcotest.(check bool) "charge landed on the scoped budget" true
          (abs_float (a.db_ms -. 5.0) < 1e-9);
        Alcotest.(check int) "db hit on the scoped budget" 1 a.db_hits;
        Alcotest.(check bool) "polluter uncharged" true (polluter.db_ms = 0.0);
        Alcotest.(check int) "polluter db hits untouched" 0 polluter.db_hits
    );
    ( "budget-remaining reads the scoped budget after ref pollution",
      `Quick,
      fun () ->
        (* (budget-remaining) is a pure prim — its read is synchronous
           today — but the pin forbids regressions to ref reads: after
           simulated pollution the answer must still be the calling
           eval's remaining steps. *)
        let a = Eval.make_budget ~limit:1000 () in
        let polluter = Eval.make_budget ~limit:77 () in
        let prims = Eval.make_prim_table () in
        let env = Env.create () in
        bind_prims env prims [ "+" ];
        let peeked = ref (-1) in
        Eval.register_prim prims "pollute-and-peek" (fun _ ->
            ignore
              (eval_str ~budget:polluter ~prims:Eval.empty_prims env
                 "(+ 1 2)");
            peeked := Eval.budget_remaining_steps ();
            Ast.VNil);
        Env.define env "pollute-and-peek" (Ast.VPrim "pollute-and-peek");
        Eval.with_scope (Some a) (fun () ->
            ignore
              (Control.run_with_rim ~prims (fun () ->
                   eval_str ~budget:a ~prims env "(pollute-and-peek)")));
        (* the polluter's 77-step limit must NOT leak into the answer:
           a's remaining = 1000 - a's own steps for this eval (a few). *)
        Alcotest.(check bool) "remaining tracks the scoped budget" true
          (!peeked > 900);
        Alcotest.(check bool) "polluter steps did not leak" true
          (polluter.steps < !peeked) );
    ( "thunk force charges the forcing eval under ref pollution",
      `Quick,
      fun () ->
        (* (delay …) binds under scope A; the ref is polluted by a
           concurrent eval; forcing under scope B must charge B ("the
           request that forces pays") — not the polluter the old ref
           read would have found. *)
        let a = Eval.make_budget ~limit:100_000 () in
        let b = Eval.make_budget ~limit:100_000 () in
        let polluter = Eval.make_budget () in
        let polluter_self = ref 0 in
        let prims = Eval.make_prim_table () in
        let env = Env.create () in
        bind_prims env prims [ "+" ];
        Eval.with_scope (Some a) (fun () ->
            ignore
              (Control.run_with_rim ~prims (fun () ->
                   eval_str ~budget:a ~prims env
                     "(define t (delay (+ 1 2)))")));
        (* the delay form itself cost A a couple of steps (define +
           delay), but the PAYLOAD was not evaluated: nothing charged
           the polluter. *)
        Alcotest.(check bool) "payload not evaluated at delay time" true
          (polluter.steps = 0);
        let a_steps_after_define = a.steps in
        Eval.register_prim prims "pollute" (fun _ ->
            ignore
              (eval_str ~budget:polluter ~prims:Eval.empty_prims env
                 "(+ 1 2)");
            (* the polluter's OWN eval legitimately charges its own
               budget (that's the publish); record the self-charge so
               the post-force assertion can prove the PAYLOAD added
               nothing. *)
            polluter_self := polluter.steps;
            Ast.VNil);
        Env.define env "pollute" (Ast.VPrim "pollute");
        Eval.with_scope (Some b) (fun () ->
            ignore
              (Control.run_with_rim ~prims (fun () ->
                   eval_str ~budget:b ~prims env "(seq (pollute) t)")));
        (* the payload ran under B: the polluter holds ONLY its own
           self-charge (under the old ref read, the force closure would
           have billed the payload to the polluter), A is frozen at its
           define-time count, and the forcing eval paid. *)
        Alcotest.(check int) "polluter billed nothing beyond its own eval"
          !polluter_self polluter.steps;
        Alcotest.(check int) "A frozen after define" a_steps_after_define
          a.steps;
        Alcotest.(check bool) "forcing eval paid" true (b.steps > 0) );
  ]

let () =
  let open Alcotest in
  run "budget_scope"
    [ ("scope", scope_tests); ("db-accounting", db_accounting_tests) ]
