(** Tests for the Pricklypear Lisp interpreter.

   These are PURE tests — no Postgres, no HTTP, no SSH. They verify the
   reader, AST round-trip, and evaluator. Per spec
   [lisp-interpreter.eval] / [workflow.build]:
     cd lisp/ && dune build && dune runtest
   needs no services. *)
open Pricklypear_lisp

(* --- Reader tests --- *)

let reads_as name input expected =
  ("reader: " ^ name, `Quick, fun () ->
    match Reader.read_one input with
    | Lit v -> Alcotest.(check string)
                  (Reader.pp_value expected) (Reader.pp_value expected)
                  (Reader.pp_value v)
    | other -> Alcotest.failf "expected Lit, got %s" (Reader.pp_expr other))

let reader_tests = [
  reads_as "int" "42" (Ast.VInt 42L);
  reads_as "negative int" "-7" (Ast.VInt (-7L));
  ("reader: float literal", `Quick, fun () ->
    match Reader.read_one "3.14" with
    | Lit (Ast.VFloat f) ->
        if abs_float (f -. 3.14) > 1e-9 then
          Alcotest.failf "float value %g" f
    | other -> Alcotest.failf "expected VFloat lit, got %s" (Reader.pp_expr other));
  reads_as "true" "#t" (Ast.VBool true);
  reads_as "false" "#f" (Ast.VBool false);
  reads_as "string" "\"hello\"" (Ast.VStr "hello");
  reads_as "nil" "()" Ast.VNil;
]

let reader_list_tests = [
  ("reader: list of ints parses", `Quick, fun () ->
    (* (1 2 3) reads as App(Lit 1, [Lit 2; Lit 3]); head is separate *)
    match Reader.read_one "(1 2 3)" with
    | App (_, [ Lit (Ast.VInt _); Lit (Ast.VInt _) ]) -> ()
    | other -> Alcotest.failf "expected App with 2 li int args, got %s"
                                  (Reader.pp_expr other));
  ("reader: nested list parses", `Quick, fun () ->
    match Reader.read_one "(foo (bar) baz)" with
    | Ast.App _ -> ()
    | other -> Alcotest.failf "expected App, got %s" (Reader.pp_expr other));
]

let reader_completeness_tests = [
  ("is_complete: balanced parens", `Quick, fun () ->
    Alcotest.(check bool) "balanced" true (Reader.is_complete "(foo bar)"));
  ("is_complete: unbalanced parens", `Quick, fun () ->
    Alcotest.(check bool) "unbalanced" false (Reader.is_complete "(foo bar"));
  ("is_complete: unclosed string", `Quick, fun () ->
    Alcotest.(check bool) "unclosed" false (Reader.is_complete "\"hello"));
  ("is_complete: comment only", `Quick, fun () ->
    Alcotest.(check bool) "comment" true (Reader.is_complete "; comment only"));
]

(* --- AST JSON round-trip tests --- *)

let roundtrip name e =
  ("ast roundtrip: " ^ name, `Quick, fun () ->
    let j = Ast.expr_to_json e in
    (match Ast.expr_of_json j with
     | Ok e' ->
        Alcotest.(check string)
          (Reader.pp_expr e) (Reader.pp_expr e) (Reader.pp_expr e')
     | Error err -> Alcotest.failf "roundtrip failed: %s" err))

let ast_roundtrip_tests = [
  roundtrip "var" (Ast.Var "x");
  roundtrip "int" (Ast.int 5);
  roundtrip "string" (Ast.str "hello");
  roundtrip "if" (Ast.If (Ast.var "x", Ast.int 1, Ast.int 2));
  roundtrip "lambda" (Ast.Lambda (["x"], Ast.var "x"));
  roundtrip "let" (Ast.Let ("y", Ast.int 5, Ast.var "y"));
  roundtrip "prim" (Ast.Prim ("query", [Ast.str "event"]));
  roundtrip "define" (Ast.Define ("greet", Ast.Lambda (["name"], Ast.Var "name")));
  roundtrip "app" (Ast.App (Ast.Var "f", [Ast.int 1; Ast.int 2]));
]

(* --- Evaluator tests --- *)

(* The interpreter is Lwt-typed: Eval.eval returns `value Lwt.t`. Each test
   body that calls the evaluator uses [Lwt_main.run] to drive the promise;
   reader/AST-roundtrip tests are pure and don't need Lwt wrapping. *)

let evals_to name expr expected =
  ("eval: " ^ name, `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let budget = Eval.make_budget () in
    let got = Lwt_main.run (Eval.eval ~budget ~prims env expr) in
    Alcotest.(check string)
      (Reader.pp_value expected) (Reader.pp_value expected)
      (Reader.pp_value got))

let eval_tests = [
  evals_to "int literal" (Ast.int 42) (Ast.VInt 42L);
  ("eval: define binds var", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ = Lwt_main.run (Eval.eval_string ~prims env "(define x 99)") in
    (match Env.lookup env "x" with
     | Some (Ast.VInt n) ->
        Alcotest.(check int64) "x's value" 99L n
     | other -> Alcotest.failf "expected Some VInt, got %s"
                                 (match other with None -> "None"
                                  | Some v -> Reader.pp_value v)));
  ("eval: if-true branch", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(if #t 1 2)") in
    Alcotest.(check string) "1" "1" (Reader.pp_value got));
  ("eval: if-false branch", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(if #f 1 2)") in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("eval: lambda application", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "((lambda (x) x) 42)") in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval: quote compound list", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "'(a b c)") in
    (match got with
     | Ast.VList [Ast.VStr "a"; Ast.VStr "b"; Ast.VStr "c"] -> ()
     | other -> Alcotest.failf "expected (a b c) list, got %s"
                  (Reader.pp_value other)));
  ("eval: quote nested", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "'(1 (2 3))") in
    (match got with
     | Ast.VList [Ast.VInt 1L; Ast.VList [Ast.VInt 2L; Ast.VInt 3L]] -> ()
     | other -> Alcotest.failf "expected nested quote, got %s"
                  (Reader.pp_value other)));
  ("eval: primitive dispatch", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "add-one"
      (fun args -> match args with
        | [Ast.VInt n] -> Lwt.return (Ast.VInt (Int64.add n 1L))
        | _ -> Lwt.return Ast.VNil);
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(prim add-one 41)") in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval: let binds locally", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(let ((y 5)) y)") in
    Alcotest.(check string) "5" "5" (Reader.pp_value got));
  ("eval: budget exceeded", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let budget = Eval.make_budget ~limit:10 () in
    (* build a deep nest to burn through 10 steps *)
    let rec nest n acc =
      if n = 0 then acc else nest (n-1) (Ast.Seq (Ast.int 1, acc))
    in
    let e = nest 20 (Ast.int 0) in
    try
      let _ = Lwt_main.run (Eval.eval ~budget ~prims env e) in
      Alcotest.fail "expected budget exceeded"
    with Ast.Interp_error Ast.Budget_exceeded -> ());
  ("eval: unbound var raises", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    try
      let _ = Lwt_main.run (Eval.eval_string ~prims env "nope") in
      Alcotest.fail "expected Unbound_var"
    with Ast.Interp_error (Ast.Unbound_var name) ->
      Alcotest.(check string) "nope" "nope" name);
]

let pretty_print_tests = [
  ("pp_expr: nested list breaks lines", `Quick, fun () ->
    let src =
      "(list (menu/link \"home\" \"/\") (menu/group \"apps\" (list (menu/link \"todos\" \"/todos\"))))"
    in
    let e = Reader.read_one src in
    let pp = Reader.pp_expr e in
    if not (String.contains pp '\n') then
      Alcotest.failf "expected newlines in:\n%s" pp;
    if not (String.contains pp 'a') then
      Alcotest.fail "missing content";
    (* Round-trip: pretty output still parses to same flat shape *)
    let e' = Reader.read_one pp in
    Alcotest.(check string) "flat equal"
      (Reader.pp_expr_flat e) (Reader.pp_expr_flat e'));
  ("pp_expr: short form stays one line", `Quick, fun () ->
    let e = Reader.read_one "(+ 1 2)" in
    let pp = Reader.pp_expr e in
    Alcotest.(check bool) "no newline" false (String.contains pp '\n');
    Alcotest.(check string) "compact" "(+ 1 2)" pp);
]

let () =
  Alcotest.run "pricklypear-lisp" [
    "reader", reader_tests @ reader_list_tests @ reader_completeness_tests;
    "ast-roundtrip", ast_roundtrip_tests;
    "pretty-print", pretty_print_tests;
    "eval", eval_tests;
  ]
