(** Nopales core conformance case data — single source of truth.

    Shared between the alcotest runner ([lisp/test/conformance_test.ml])
    and the js_of_ocaml browser-target runner
    ([client/conformance/main.ml]). Spec [lisp-interpreter.conformance] +
    [structural-editing.client-eval]: client/server agreement is a tested
    invariant, not a hope. *)
open Pricklypear_lisp

type case =
  { group : string
  ; name : string
  ; setups : string list
  ; src : string
  ; expected : string
  }

(* Minimal pure prim table for the core gate. The full product prim table
   lives in pure/prims_pure.ml; conformance pins the EVALUATOR semantics
   with a tiny arith/list surface so the gate is service-free. *)
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

let mk ?(setups = []) group name src expected =
  { group; name; setups; src; expected }

let cases = [
  (* reader *)
  mk "reader" "keyword self-evaluates" ":target" ":target";
  mk "reader" "int literal" "42" "42";
  mk "reader" "string literal" "\"hi\"" "\"hi\"";
  mk "reader" "bool true" "#t" "#t";
  mk "reader" "bool false" "#f" "#f";
  mk "reader" "nil" "()" "()";
  mk "reader" "quote symbol" "'hello" "hello";
  mk "reader" "quote string stays string" "'\"hello\"" "\"hello\"";
  mk "reader" "quote list" "'(a b c)" "(a b c)";
  mk "reader" "unicode \\uXXXX overline" "\"\\u203e\"" "\"\xE2\x80\xBE\"";
  mk "reader" "unicode \\uXXXX degree" "\"71\\u00B0F\"" "\"71\xC2\xB0F\"";
  (* values *)
  mk "values" "symbol vs string type-of symbol" "(type-of 'a)" "\"symbol\"";
  mk "values" "symbol vs string type-of str" "(type-of \"a\")" "\"str\"";
  mk "values" "symbol?" "(symbol? 'lambda)" "#t";
  mk "values" "string? rejects symbol" "(string? 'lambda)" "#f";
  mk "values" "string? accepts str" "(string? \"lambda\")" "#t";
  mk "values" "car of quoted lambda is symbol"
    "(symbol=? (car '(lambda (x) x)) 'lambda)" "#t";
  mk "values" "list constructor" "(list 1 2 3)" "(1 2 3)";
  (* special-forms *)
  mk "special-forms" "if true" "(if #t 1 2)" "1";
  mk "special-forms" "if false" "(if #f 1 2)" "2";
  mk "special-forms" "begin last" "(begin 1 2 3)" "3";
  mk "special-forms" "let" "(let ((x 10)) (+ x 1))" "11";
  mk "special-forms" "let* sequential" "(let* ((x 1) (y (+ x 1))) y)" "2";
  mk "special-forms" "let* empty" "(let* () 42)" "42";
  mk "special-forms" "let parallel no leak"
    "(let ((a 1) (b 2)) (+ a b))" "3";
  mk "special-forms" "lambda app" "((lambda (x) (+ x 1)) 41)" "42";
  mk "special-forms" "and short-circuit true" "(and #t #t 7)" "7";
  mk "special-forms" "and short-circuit false" "(and #t #f 7)" "#f";
  mk "special-forms" "or short-circuit" "(or #f 3 9)" "3";
  mk "special-forms" "cond else" "(cond (#f 1) (else 2))" "2";
  mk "special-forms" "match list head"
    "(match '(lambda (x) y) ((lambda params body) params) (_ 0))" "(x)";
  mk "special-forms" "match fallthrough underscore"
    "(match 42 ((list x) 1) (_ 7))" "7";
  mk "special-forms" "match binds var" "(match 5 (x (+ x 1)))" "6";
  mk "special-forms" "guard recovers" "(guard (lambda (e) 99) (error \"boom\"))" "99";
  mk "special-forms" "define sugar"
    ~setups:[ "(define (add1 x) (+ x 1))" ] "(add1 41)" "42";
  mk "special-forms" "define multi-body"
    ~setups:[ "(define (f x) (+ x 0) (+ x 1))" ] "(f 2)" "3";
  mk "special-forms" "defmacro when"
    ~setups:[ "(defmacro when (test body) `(if ,test ,body #f))" ]
    "(when #t 42)" "42";
  (* list-hof *)
  mk "list-hof" "list/map"
    "(list/map (lambda (x) (* x 2)) (list 1 2 3))" "(2 4 6)";
  mk "list-hof" "list/filter"
    "(list/filter (lambda (x) (= x 2)) (list 1 2 3 2))" "(2 2)";
  mk "list-hof" "list/foldl"
    "(list/foldl (lambda (acc x) (+ acc x)) 0 (list 1 2 3 4))" "10";
  mk "list-hof" "list/foldr"
    "(list/foldr (lambda (x acc) (cons x acc)) (list) (list 1 2 3))" "(1 2 3)";
  mk "list-hof" "list/map empty" "(list/map (lambda (x) x) (list))" "()";
  (* arith *)
  mk "arith" "add" "(+ 20 22)" "42";
  mk "arith" "nested arith" "(* (+ 1 2) (- 10 4))" "18";
]
