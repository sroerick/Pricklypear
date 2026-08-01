open Pricklypear_lisp

let parse s = Reader.read_one s

let fact_non_tail =
  parse
    "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"

let fact_tail =
  parse
    "(define fact-t (lambda (n acc) (if (= n 0) acc (fact-t (- n 1) (* n acc)))))"

let fib =
  parse
    "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))"

let pure_add = parse "(define add1 (lambda (x) (+ x 1)))"

let test_self_recursive () =
  let info = Recursion.analyze_self ~fn_name:"fact" fact_non_tail in
  Alcotest.(check bool) "recursive" true info.is_recursive;
  Alcotest.(check bool) "non-tail" false info.tail_recursive

let test_tail_recursive () =
  let info = Recursion.analyze_self ~fn_name:"fact-t" fact_tail in
  Alcotest.(check bool) "recursive" true info.is_recursive;
  Alcotest.(check bool) "tail" true info.tail_recursive

let test_fib_branching () =
  let info = Recursion.analyze_self ~fn_name:"fib" fib in
  Alcotest.(check bool) "recursive" true info.is_recursive;
  Alcotest.(check int) "two sites" 2 (List.length info.self_sites);
  Alcotest.(check bool) "non-tail" false info.tail_recursive

let test_non_recursive () =
  let info = Recursion.analyze_self ~fn_name:"add1" pure_add in
  Alcotest.(check bool) "not recursive" false info.is_recursive

let test_complexity () =
  Alcotest.(check string) "fact O(n)" "O(n)"
    (Complexity.badge (Complexity.classify ~fn_name:"fact" fact_non_tail));
  Alcotest.(check string) "fib exp" "O(2ⁿ)"
    (Complexity.badge (Complexity.classify ~fn_name:"fib" fib));
  Alcotest.(check string) "add1 O(1)" "O(1)"
    (Complexity.badge (Complexity.classify ~fn_name:"add1" pure_add))

let test_explain_peel () =
  let layers = Explain.peel ~fn_name:"fact" fact_non_tail in
  Alcotest.(check bool) "non-empty" true (layers <> []);
  let kinds = List.map (fun (l : Explain.layer) -> l.kind) layers in
  Alcotest.(check bool) "has call or if" true
    (List.exists
       (fun k ->
         k = "if" || String.starts_with ~prefix:"if" k
         || String.starts_with ~prefix:"call:" k)
       kinds)

let test_step_attribution () =
  let env = Env.create () in
  let prims = Eval.make_prim_table () in
  let open Ast in
  let bin name f =
    Eval.register_prim prims name (function
      | [ VInt a; VInt b ] -> VInt (f a b)
      | _ -> raise (Interp_error (User_error (name ^ " bad args"))))
  in
  bin "+" Int64.add;
  bin "-" Int64.sub;
  bin "*" Int64.mul;
  Eval.register_prim prims "=" (function
    | [ VInt a; VInt b ] -> VBool (Int64.equal a b)
    | _ -> VBool false);
  Env.define env "+" (VPrim "+");
  Env.define env "-" (VPrim "-");
  Env.define env "*" (VPrim "*");
  Env.define env "=" (VPrim "=");
  ignore (Eval.eval_string ~prims env
            "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))");
  ignore (Eval.eval_string ~prims env "(fact 5)");
  match !Eval.last_budget with
  | None -> Alcotest.fail "no last_budget"
  | Some b ->
      Alcotest.(check bool) "steps>0" true (b.steps > 0);
      Alcotest.(check bool) "prim_calls>0" true (b.prim_calls > 0);
      let fact_steps =
        try Hashtbl.find b.fn_step_counts "fact" with Not_found -> 0
      in
      Alcotest.(check bool) "fact attributed" true (fact_steps > 0)

let () =
  Alcotest.run "recursion-complexity"
    [
      ( "recursion",
        [
          ("self non-tail", `Quick, test_self_recursive);
          ("tail", `Quick, test_tail_recursive);
          ("fib", `Quick, test_fib_branching);
          ("non-rec", `Quick, test_non_recursive);
        ] );
      ( "complexity",
        [ ("classify", `Quick, test_complexity) ] );
      ( "explain",
        [ ("peel", `Quick, test_explain_peel) ] );
      ( "budget",
        [ ("attribution", `Quick, test_step_attribution) ] );
    ]
