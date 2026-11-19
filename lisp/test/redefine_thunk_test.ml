(** Card 8660796a: redefining a binding on system/main used to 500 every
    anonymous request through the redefined name until restart with
    "cyclic thunk force (binding refers to itself)".

    Shape of the bug: an anonymous session holds a COPY of the binding
    table's [VThunk cell] ref (a parked snapshot env). The force marks the
    cell [Forcing] and, if the owner dies / a define refreshes the binding
    mid-flight, the cell stays in flight forever. Any later force through
    that same ref then looked self-referential.

    The fix: [define_thunk] REFRESHES the existing cell in place (so every
    env holding a copy sees the new body), and [force_value] distinguishes
    a genuine cycle (owner on OUR stack) from an abandoned in-flight cell,
    which it settles and retries instead of raising.

    Pure tests - no services. Per spec borge/lisp-interpreter.borg. *)
open Pricklypear_lisp
open Ast

let eval_str env s = Eval.eval_string ~prims:Eval.empty_prims env s

let counting_eval (count : int ref) (env : env) (expr : expr) : value =
  incr count;
  let budget = Eval.make_budget () in
  Eval.eval ~budget ~prims:Eval.empty_prims env expr

(* R1/R3: redefine a name, force it twice. The second definition must be
   the one observed, and no "cyclic thunk force" may appear. *)
let redefine_refreshes_cell () =
  let env = Env.create () in
  let count = ref 0 in
  Env.define_thunk env "thl/first" ~force:(counting_eval count)
    (Lambda ([ "x" ], Var "x"));
  Env.define_thunk env "thl/first" ~force:(counting_eval count)
    (App (Lambda ([ "y" ], Var "y"), [ Lit (VInt 9L) ]));
  let v1 = eval_str env "thl/first" in
  Alcotest.(check string) "redefinition wins" "9" (Reader.pp_value v1);
  let v2 = eval_str env "thl/first" in
  Alcotest.(check string) "second force is memoized, no cycle" "9"
    (Reader.pp_value v2)

(* The real repro: a second env holds a COPY of the same cell ref (the
   per-request snapshot env card 6b8edbe4 copies cells into). Redefining
   in the live env must refresh that shared cell, not install a new ref. *)
let redefine_refreshes_shared_cell () =
  let live = Env.create () in
  let count = ref 0 in
  Env.define_thunk live "thl/index-page" ~force:(counting_eval count)
    (App (Lambda ([ "x" ], Var "x"), [ Lit (VStr "old") ]));
  let snapshot = Env.create () in
  (match Env.find_raw live "thl/index-page" with
   | Some v -> Env.define snapshot "thl/index-page" v
   | None -> Alcotest.fail "binding missing before copy");
  Env.define_thunk live "thl/index-page" ~force:(counting_eval count)
    (App (Lambda ([ "x" ], Var "x"), [ Lit (VStr "new") ]));
  let v1 = eval_str snapshot "thl/index-page" in
  Alcotest.(check string) "snapshot sees the new definition" "\"new\""
    (Reader.pp_value v1);
  let v2 = eval_str live "thl/index-page" in
  Alcotest.(check string) "live env sees the new definition" "\"new\""
    (Reader.pp_value v2)

(* An abandoned in-flight cell (owner died or a redefine landed mid-force)
   is settled and retried instead of raising "cyclic thunk force". *)
let abandoned_in_flight_cell_is_settled () =
  let env = Env.create () in
  let count = ref 0 in
  let force = counting_eval count in
  let payload = { thunk_body = Lit (VInt 1L); thunk_env = env; thunk_force = force } in
  let cell = ref (Forcing payload) in
  Env.define env "parked" (VThunk cell);
  let v = eval_str env "parked" in
  Alcotest.(check string) "abandoned cell settled" "1" (Reader.pp_value v)

(* A genuine self-reference still raises the cyclic error: the owner IS on
   this call stack. Cycle detection is not weakened. *)
let genuine_cycle_still_raises () =
  let env = Env.create () in
  Env.define_thunk env "self" ~force:(counting_eval (ref 0)) (Var "self");
  let raised =
    try ignore (eval_str env "self"); false
    with Interp_error (User_error (_, "cyclic-thunk")) -> true
  in
  Alcotest.(check bool) "cycle still detected" true raised

let tests = [
  ("redefine refreshes the cell", `Quick, redefine_refreshes_cell);
  ("redefine refreshes a shared cell copy", `Quick, redefine_refreshes_shared_cell);
  ("abandoned in-flight cell is settled", `Quick, abandoned_in_flight_cell_is_settled);
  ("genuine cycle still raises", `Quick, genuine_cycle_still_raises);
]

let () = Alcotest.run "redefine_thunk" [ ("redefine-thunk", tests) ]
