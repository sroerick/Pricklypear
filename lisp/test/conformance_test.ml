(** Nopales core conformance suite.

    Real Nopales programs paired with expected eval results. Service-free
    (pure lisp/ only). Per spec [lisp-interpreter.conformance]: this is the
    semantic gate of core-freeze — a reddened case blocks silent core breaks.

    Run: dune runtest lisp  (included in default lisp tests) *)
open Pricklypear_lisp

let setup_arith () =
  let prims = Eval.make_prim_table () in
  let binop name op =
    Eval.register_prim prims name (function
      | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (op a b)
      | _ -> raise (Ast.Interp_error (Ast.User_error name)))
  in
  binop "+" Int64.add;
  binop "-" Int64.sub;
  binop "*" Int64.mul;
  Eval.register_prim prims "=" (function
    | [ Ast.VInt a; Ast.VInt b ] -> Ast.VBool (Int64.equal a b)
    | [ a; b ] -> Ast.VBool (a = b)
    | _ -> raise (Ast.Interp_error (Ast.User_error "=")));
  Eval.register_prim prims "<" (function
    | [ Ast.VInt a; Ast.VInt b ] -> Ast.VBool (a < b)
    | _ -> raise (Ast.Interp_error (Ast.User_error "<")));
  Eval.register_prim prims "not" (function
    | [ Ast.VBool b ] -> Ast.VBool (not b)
    | [ Ast.VNil ] -> Ast.VBool true
    | [ _ ] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "not")));
  Eval.register_prim prims "list" (fun xs -> Ast.VList xs);
  Eval.register_prim prims "car" (function
    | [ Ast.VList (h :: _) ] -> h
    | [ Ast.VPair (a, _) ] -> a
    | _ -> raise (Ast.Interp_error (Ast.User_error "car")));
  Eval.register_prim prims "cdr" (function
    | [ Ast.VList (_ :: t) ] -> Ast.VList t
    | [ Ast.VPair (_, b) ] -> b
    | _ -> raise (Ast.Interp_error (Ast.User_error "cdr")));
  Eval.register_prim prims "cons" (function
    | [ h; Ast.VList t ] -> Ast.VList (h :: t)
    | [ h; t ] -> Ast.VPair (h, t)
    | _ -> raise (Ast.Interp_error (Ast.User_error "cons")));
  Eval.register_prim prims "null?" (function
    | [ Ast.VNil ] | [ Ast.VList [] ] -> Ast.VBool true
    | [ _ ] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "null?")));
  Eval.register_prim prims "symbol?" (function
    | [ Ast.VSymbol _ ] -> Ast.VBool true
    | [ _ ] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "symbol?")));
  Eval.register_prim prims "string?" (function
    | [ Ast.VStr _ ] -> Ast.VBool true
    | [ _ ] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "string?")));
  Eval.register_prim prims "symbol=?" (function
    | [ Ast.VSymbol a; Ast.VSymbol b ] -> Ast.VBool (String.equal a b)
    | [ _; _ ] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "symbol=?")));
  Eval.register_prim prims "type-of" (function
    | [ Ast.VSymbol _ ] -> Ast.VStr "symbol"
    | [ Ast.VStr _ ] -> Ast.VStr "str"
    | [ Ast.VInt _ ] -> Ast.VStr "int"
    | [ Ast.VBool _ ] -> Ast.VStr "bool"
    | [ Ast.VNil ] -> Ast.VStr "nil"
    | [ Ast.VList _ ] -> Ast.VStr "list"
    | _ -> Ast.VStr "other");
  let env = Env.create () in
  List.iter
    (fun n -> Env.define env n (Ast.VPrim n))
    [ "+"; "-"; "*"; "="; "<"; "not"; "list"; "car"; "cdr"; "cons"; "null?"
    ; "symbol?"; "string?"; "symbol=?"; "type-of" ];
  (prims, env)

let eval_pp ~prims env src =
  Reader.pp_value (Eval.eval_string ~prims env src)

let case name src expect =
  ( name,
    `Quick,
    fun () ->
      let prims, env = setup_arith () in
      let got = eval_pp ~prims env src in
      Alcotest.(check string) name expect got )

(** Multi-form: run [setup] forms for effect, then assert last [src]. *)
let case_setup name setups src expect =
  ( name,
    `Quick,
    fun () ->
      let prims, env = setup_arith () in
      List.iter (fun s -> ignore (Eval.eval_string ~prims env s)) setups;
      let got = eval_pp ~prims env src in
      Alcotest.(check string) name expect got )

let reader_cases =
  [ case "keyword self-evaluates" ":target" ":target"
  ; case "int literal" "42" "42"
  ; case "string literal" "\"hi\"" "\"hi\""
  ; case "bool true" "#t" "#t"
  ; case "bool false" "#f" "#f"
  ; case "nil" "()" "()"
  ; case "quote symbol" "'hello" "hello"
  ; case "quote string stays string" "'\"hello\"" "\"hello\""
  ; case "quote list" "'(a b c)" "(a b c)"
  ]

let value_cases =
  [ case "symbol vs string type-of symbol" "(type-of 'a)" "\"symbol\""
  ; case "symbol vs string type-of str" "(type-of \"a\")" "\"str\""
  ; case "symbol?" "(symbol? 'lambda)" "#t"
  ; case "string? rejects symbol" "(string? 'lambda)" "#f"
  ; case "string? accepts str" "(string? \"lambda\")" "#t"
  ; case "car of quoted lambda is symbol"
      "(symbol=? (car '(lambda (x) x)) 'lambda)" "#t"
  ; case "list constructor" "(list 1 2 3)" "(1 2 3)"
  ]

let special_form_cases =
  [ case "if true" "(if #t 1 2)" "1"
  ; case "if false" "(if #f 1 2)" "2"
  ; case "begin last" "(begin 1 2 3)" "3"
  ; case "let" "(let ((x 10)) (+ x 1))" "11"
  ; case "lambda app" "((lambda (x) (+ x 1)) 41)" "42"
  ; case "and short-circuit true" "(and #t #t 7)" "7"
  ; case "and short-circuit false" "(and #t #f 7)" "#f"
  ; case "or short-circuit" "(or #f 3 9)" "3"
  ; case "cond else" "(cond (#f 1) (else 2))" "2"
  ; case "match list head"
      "(match '(lambda (x) y) ((lambda params body) params) (_ 0))" "(x)"
  ; case "match fallthrough underscore"
      "(match 42 ((list x) 1) (_ 7))" "7"
  ; case "match binds var"
      "(match 5 (x (+ x 1)))" "6"
  ; case "guard recovers" "(guard (lambda (e) 99) (error \"boom\"))" "99"
  ; case_setup "define sugar"
      [ "(define (add1 x) (+ x 1))" ]
      "(add1 41)" "42"
  ; case_setup "define multi-body"
      [ "(define (f x) (+ x 0) (+ x 1))" ]
      "(f 2)" "3"
  ; case_setup "defmacro when"
      [ "(defmacro when (test body) `(if ,test ,body #f))" ]
      "(when #t 42)" "42"
  ]

let list_hof_cases =
  [ case "list/map"
      "(list/map (lambda (x) (* x 2)) (list 1 2 3))" "(2 4 6)"
  ; case "list/filter"
      "(list/filter (lambda (x) (= x 2)) (list 1 2 3 2))" "(2 2)"
  ; case "list/foldl"
      "(list/foldl (lambda (acc x) (+ acc x)) 0 (list 1 2 3 4))" "10"
  ; case "list/foldr"
      "(list/foldr (lambda (x acc) (cons x acc)) (list) (list 1 2 3))"
      "(1 2 3)"
  ; case "list/map empty" "(list/map (lambda (x) x) (list))" "()"
  ]

let arithmetic_cases =
  [ case "add" "(+ 20 22)" "42"
  ; case "nested arith" "(* (+ 1 2) (- 10 4))" "18"
  ]

let error_cases =
  [ ( "unbound has kind unbound_var",
      `Quick,
      fun () ->
        let prims, env = setup_arith () in
        match
          try
            ignore (Eval.eval_string ~prims env "no-such-binding");
            None
          with Ast.Interp_error e -> Some e
        with
        | Some (Ast.Unbound_var "no-such-binding" as e) ->
            Alcotest.(check string) "kind" "unbound_var"
              (Ast.interp_error_kind e)
        | Some e -> Alcotest.failf "wrong: %s" (Ast.pp_interp_error e)
        | None -> Alcotest.fail "expected unbound" )
  ; ( "budget not caught by guard",
      `Quick,
      fun () ->
        let prims, env = setup_arith () in
        ignore (Eval.eval_string ~prims env "(define loop (lambda () (loop)))");
        let raised =
          try
            let budget = Eval.make_budget ~limit:40 () in
            ignore
              (Control.run_sync ~prims (fun () ->
                   Eval.eval ~budget ~prims:Eval.empty_prims env
                     (Reader.read_one "(guard (lambda (e) 0) (loop))")));
            false
          with
          | Ast.Interp_error Ast.Budget_exceeded -> true
          | _ -> false
        in
        Alcotest.(check bool) "budget escapes" true raised )
  ; ( "parse error has line col",
      `Quick,
      fun () ->
        match
          try
            ignore (Reader.read_one "(foo");
            None
          with Reader.Reader_error (_, line, col) -> Some (line, col)
        with
        | Some (line, col) ->
            Alcotest.(check bool) "line" true (line >= 1);
            Alcotest.(check bool) "col" true (col >= 1)
        | None -> Alcotest.fail "expected parse error" )
  ]

let () =
  Alcotest.run "nopales-conformance"
    [ "reader", reader_cases
    ; "values", value_cases
    ; "special-forms", special_form_cases
    ; "list-hof", list_hof_cases
    ; "arith", arithmetic_cases
    ; "errors", error_cases
    ]
