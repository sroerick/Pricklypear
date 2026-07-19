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
  ("eval: seq empty returns nil", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(seq)") in
    Alcotest.(check string) "nil" "()" (Reader.pp_value got));
  ("eval: seq single returns body", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(seq 7)") in
    Alcotest.(check string) "7" "7" (Reader.pp_value got));
  ("eval: seq binary returns last", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(seq 1 2)") in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("eval: seq multi returns last", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(seq 1 2 3)") in
    Alcotest.(check string) "3" "3" (Reader.pp_value got));
  ("eval: seq nested multi", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env
                              "(seq 1 (seq 2 3) 4)") in
    Alcotest.(check string) "4" "4" (Reader.pp_value got));
  ("eval: seq side effects left-to-right", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env
      "(seq (define a 1) (define b 2) (define c 3) c)") in
    Alcotest.(check string) "c" "3" (Reader.pp_value got);
    (match Env.lookup env "a", Env.lookup env "b" with
     | Some (Ast.VInt 1L), Some (Ast.VInt 2L) -> ()
     | _ -> Alcotest.fail "expected a=1 b=2 after seq defines"));
  ("eval: seq in lambda body multi", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env
      "((lambda () (seq (define x 10) (define y 20) y)))") in
    Alcotest.(check string) "20" "20" (Reader.pp_value got));
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

let macro_tests = [
  ("value_to_expr roundtrip reify list", `Quick, fun () ->
    let e = Reader.read_one "(a b c)" in
    let v = Eval.reify_expr e in
    let e' = Eval.value_to_expr v in
    Alcotest.(check string) "pp"
      (Reader.pp_expr_flat e) (Reader.pp_expr_flat e'));
  ("value_to_expr keeps string lits", `Quick, fun () ->
    let e = Reader.read_one "(html/attr \"class\" \"muted\")" in
    let v = Eval.reify_expr e in
    let e' = Eval.value_to_expr v in
    Alcotest.(check string) "pp"
      (Reader.pp_expr_flat e) (Reader.pp_expr_flat e'));
  ("quasiquote unquote", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+" (function
      | [Ast.VInt a; Ast.VInt b] -> Lwt.return (Ast.VInt (Int64.add a b))
      | _ -> raise (Ast.Interp_error (Ast.User_error "+")));
    let env = Env.create () in
    Env.define env "+" (Ast.VPrim "+");
    let got = Lwt_main.run (Eval.eval_string ~prims env "`(a ,(+ 1 2) c)") in
    match got with
    | Ast.VList [Ast.VStr "a"; Ast.VInt 3L; Ast.VStr "c"] -> ()
    | other -> Alcotest.failf "qq got %s" (Reader.pp_value other));
  ("quasiquote splice", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "`(a ,@'(b c) d)") in
    match got with
    | Ast.VList [Ast.VStr "a"; Ast.VStr "b"; Ast.VStr "c"; Ast.VStr "d"] -> ()
    | other -> Alcotest.failf "splice got %s" (Reader.pp_value other));
  ("defmacro when", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ =
      Lwt_main.run
        (Eval.eval_string ~prims env
           "(defmacro when (t b) `(if ,t ,b ()))")
    in
    let got =
      Lwt_main.run (Eval.eval_string ~prims env "(when #t 42)")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got);
    let got2 =
      Lwt_main.run (Eval.eval_string ~prims env "(when #f 42)")
    in
    Alcotest.(check string) "nil" "()" (Reader.pp_value got2));
  ("defmacro expands to let (qq)", `Quick, fun () ->
    (* Inbox c7-2611: macros expanding to let crashed with opaque
       Interp_error. Must evaluate to the bound value. *)
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ =
      Lwt_main.run
        (Eval.eval_string ~prims env
           "(defmacro let1 (name val body) `(let ((,name ,val)) ,body))")
    in
    let got =
      Lwt_main.run (Eval.eval_string ~prims env "(let1 a 1 a)")
    in
    Alcotest.(check string) "let1 => 1" "1" (Reader.pp_value got));
  ("defmacro expands to let (list)", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "list" (fun args -> Lwt.return (Ast.VList args));
    let env = Env.create () in
    (* Image binds registered prims into env as VPrim; pure tests must too. *)
    Env.define env "list" (Ast.VPrim "list");
    let _ =
      Lwt_main.run
        (Eval.eval_string ~prims env
           "(defmacro let1 (name val body)\
             (list (quote let) (list (list name val)) body))")
    in
    let got =
      try Lwt_main.run (Eval.eval_string ~prims env "(let1 a 1 a)")
      with Ast.Interp_error e ->
        Alcotest.failf "let1 list failed: %s" (Ast.pp_interp_error e)
    in
    Alcotest.(check string) "let1 list => 1" "1" (Reader.pp_value got));
  ("defmacro rejects dotted rest params", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    match
      try
        ignore
          (Lwt_main.run
             (Eval.eval_string ~prims env
                "(defmacro bad (a . b) a)"));
        `Ok
      with Ast.Interp_error (Ast.User_error m) -> `Err m
         | Ast.Interp_error e -> `Err (Ast.pp_interp_error e)
    with
    | `Err m ->
        if not (String.starts_with ~prefix:"dotted rest" m
                || String.length m >= 6
                   && String.sub m 0 6 = "dotted") then
          Alcotest.failf "expected dotted rest error, got: %s" m
    | `Ok -> Alcotest.fail "expected dotted rest rejection");
  ("gensym unique", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let a = Lwt_main.run (Eval.eval_string ~prims env "(gensym)") in
    let b = Lwt_main.run (Eval.eval_string ~prims env "(gensym)") in
    match a, b with
    | Ast.VStr sa, Ast.VStr sb when sa <> sb -> ()
    | _ -> Alcotest.fail "gensym not unique");
  ("cond first match", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Lwt_main.run
        (Eval.eval_string ~prims env
           "(cond (#f 1) (#t 2) (#t 3))")
    in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("cond else", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Lwt_main.run
        (Eval.eval_string ~prims env "(cond (#f 1) (else 9))")
    in
    Alcotest.(check string) "9" "9" (Reader.pp_value got));
  ("cond empty", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Lwt_main.run (Eval.eval_string ~prims env "(cond)") in
    Alcotest.(check string) "nil" "()" (Reader.pp_value got));
  ("and short-circuit", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Lwt_main.run (Eval.eval_string ~prims env "(and #t 1 2)")
    in
    Alcotest.(check string) "2" "2" (Reader.pp_value got);
    let got2 =
      Lwt_main.run (Eval.eval_string ~prims env "(and #t #f 3)")
    in
    Alcotest.(check string) "false" "#f" (Reader.pp_value got2);
    let got3 = Lwt_main.run (Eval.eval_string ~prims env "(and)") in
    Alcotest.(check string) "true" "#t" (Reader.pp_value got3));
  ("or short-circuit", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Lwt_main.run (Eval.eval_string ~prims env "(or #f 0 1)")
    in
    Alcotest.(check string) "0" "0" (Reader.pp_value got);
    let got2 = Lwt_main.run (Eval.eval_string ~prims env "(or)") in
    Alcotest.(check string) "false" "#f" (Reader.pp_value got2));
]

(* H1 dual-path: eval_eff + Control.run_lwt (PP_EVAL_CONTROL=eff shape). *)
let eval_eff_tests = [
  ("eval_eff: int literal", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Lwt_main.run (Eval.eval_string_eff_lwt ~prims env "42")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval_eff: define + lookup", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ = Lwt_main.run (Eval.eval_string_eff_lwt ~prims env "(define x 99)") in
    let got = Lwt_main.run (Eval.eval_string_eff_lwt ~prims env "x") in
    Alcotest.(check string) "99" "99" (Reader.pp_value got));
  ("eval_eff: if + lambda app", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Lwt_main.run
        (Eval.eval_string_eff_lwt ~prims env "((lambda (x) (if #t x 0)) 7)")
    in
    Alcotest.(check string) "7" "7" (Reader.pp_value got));
  ("eval_eff: prim via Prim_call", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "add-one"
      (fun args ->
        match args with
        | [ Ast.VInt n ] -> Lwt.return (Ast.VInt (Int64.add n 1L))
        | _ -> Lwt.fail_with "bad args");
    let env = Env.create () in
    let got =
      Lwt_main.run (Eval.eval_string_eff_lwt ~prims env "(prim add-one 41)")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval_eff: composition bind then prim", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "get"
      (fun args ->
        match args with
        | [ Ast.VStr _; Ast.VInt id ] ->
            Lwt.return (Ast.VStr ("row-" ^ Int64.to_string id))
        | _ -> Lwt.fail_with "bad get");
    let env = Env.create () in
    Env.define env "get" (Ast.VPrim "get");
    let got =
      Lwt_main.run
        (Eval.eval_string_eff_lwt ~prims env
           "(let ((u (get \"user\" 1))) u)")
    in
    Alcotest.(check string) "row" "\"row-1\"" (Reader.pp_value got));
  ("eval_eff: budget still aborts", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let budget = Eval.make_budget ~limit:3 () in
    let e = Reader.read_one "(seq 1 2 3 4 5 6 7 8 9 10)" in
    (try
       let _ =
         Lwt_main.run (Eval.eval_eff_lwt ~budget ~prims env e)
       in
       Alcotest.fail "expected Budget_exceeded"
     with
     | Ast.Interp_error Ast.Budget_exceeded -> ()
     | exn ->
         Alcotest.failf "wrong exn: %s" (Printexc.to_string exn)));
  (* H1.3: with-transaction → __with_transaction__ via Prim_call; nested
     apply_eff under run_lwt so body prims still perform. *)
  ("eval_eff: with-transaction nests prims", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let seen = ref [] in
    Eval.register_prim prims "create"
      (fun args ->
        seen := "create" :: !seen;
        match args with
        | [ Ast.VStr t; Ast.VStr _ ] -> Lwt.return (Ast.VStr ("id-" ^ t))
        | _ -> Lwt.fail_with "bad create");
    Eval.register_prim prims "__with_transaction__"
      (fun args ->
        seen := "tx-enter" :: !seen;
        match args with
        | [ lam ] ->
            let budget = Eval.make_budget () in
            (* image-shaped: nested run_lwt + apply_eff *)
            Control.run_lwt ~prims (fun () ->
                Eval.apply_eff ~budget ~prims lam [])
        | _ -> Lwt.fail_with "bad tx");
    let env = Env.create () in
    Env.define env "create" (Ast.VPrim "create");
    let got =
      Lwt_main.run
        (Eval.eval_string_eff_lwt ~prims env
           "(with-transaction (lambda () (create \"note\" \"{}\")))")
    in
    Alcotest.(check string) "id" "\"id-note\"" (Reader.pp_value got);
    Alcotest.(check bool) "tx ran" true (List.mem "tx-enter" !seen);
    Alcotest.(check bool) "create nested" true (List.mem "create" !seen));
]

let () =
  Alcotest.run "pricklypear-lisp" [
    "reader", reader_tests @ reader_list_tests @ reader_completeness_tests;
    "ast-roundtrip", ast_roundtrip_tests;
    "pretty-print", pretty_print_tests;
    "eval", eval_tests;
    "macros", macro_tests;
    "eval-eff", eval_eff_tests;
  ]
