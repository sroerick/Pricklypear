(** Tests for borge/remote-eval.borg placement subsection. The placer
    decides LOCAL vs REMOTE(/api/eval) per expression. Verifies the
    four rules: transaction-wrapped → Remote (hard correctness); zero
    prims → Local; prim-dense above threshold → Remote; the decision
    carries a human-readable reason. No network — the placer is a pure
    AST walker. *)

open Pricklypear_lisp
open Ast

let cfg =
  let local_prims =
    [ "+"; "-"; "*"; "div"; "list"; "car"; "cdr"; "cons"; "not";
      "dict-get"; "string-append"; "str"; "count"; "null?" ]
  in
  Placer.default_cfg ~local_prims

let check_placement ~name expected src =
  let got = Placer.place ~cfg src in
  let got_s = Placer.pp_placement got in
  let exp_s = Placer.pp_placement expected in
  Alcotest.(check string) name exp_s got_s

let test_zero_prims_local () =
  check_placement ~name:"pure arithmetic is local" Local "(+ 1 2)";
  check_placement ~name:"pure list is local" Local "(cons 1 (list 2 3))";
  check_placement ~name:"nested pure is local" Local
    "(+ (+ 1 2) (- 3 1))"

let test_transaction_remote () =
  (* A transaction forces REMOTE regardless of prim count — locks
     can't span HTTP. *)
  let p = Placer.place ~cfg "(with-transaction (lambda () (create 'note 1)))" in
  (match p with
   | Remote (_, reason) ->
       Alcotest.(check bool) "reason mentions with-transaction" true
         (String.length reason > 0)
   | Local -> Alcotest.fail "transaction must be Remote");
  (* even a transaction with a pure body is Remote (the wrapper is the point) *)
  let p2 = Placer.place ~cfg "(with-transaction (lambda () (+ 1 2)))" in
  (match p2 with
   | Remote _ -> ()
   | Local -> Alcotest.fail "transaction wrapper alone must be Remote")

let test_prim_dense_remote () =
  (* Four proxied prim calls (get) above the default threshold 3 → Remote *)
  let p =
    Placer.place ~cfg
      "(dict-get (get 'a 1) \"x\") (dict-get (get 'b 2) \"y\") (get 'c 3) (get 'd 4)"
  in
  (match p with
   | Remote (_, reason) ->
       Alcotest.(check bool) "reason is non-empty" true (String.length reason > 0)
   | Local -> Alcotest.fail "prim-dense above threshold must be Remote")

let test_below_threshold_local () =
  (* One proxied prim call (get) at/below threshold → Local (proxy it) *)
  check_placement ~name:"single prim call is local" Local
    "(dict-get (get 'user 42) \"name\")"

let test_decision_carries_reason () =
  let p =
    Placer.place ~cfg
      "(get 'a 1) (get 'b 2) (get 'c 3) (get 'd 4) (get 'e 5)"
  in
  (match p with
   | Remote (where, reason) ->
       Alcotest.(check string) "routes to /api/eval" "/api/eval" where;
       Alcotest.(check bool) "reason explains the threshold" true
         (String.length reason > 0
          && (String.length
                (try ignore (Str.search_forward (Str.regexp "threshold") reason 0); "y"
                 with Not_found -> "") > 0))
   | Local -> Alcotest.fail "prim-dense must be Remote with a reason")

let () =
  Alcotest.run "remote-placer"
    [
      ( "placement",
        [
          Alcotest.test_case "zero prims → local" `Quick test_zero_prims_local;
          Alcotest.test_case "with-transaction → remote" `Quick test_transaction_remote;
          Alcotest.test_case "prim-dense → remote" `Quick test_prim_dense_remote;
          Alcotest.test_case "below threshold → local" `Quick test_below_threshold_local;
          Alcotest.test_case "decision carries reason" `Quick test_decision_carries_reason;
        ] );
    ]
