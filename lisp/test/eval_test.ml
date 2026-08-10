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
  (* UTF-8 degree sign as raw bytes must round-trip via pp_value (not
     OCaml String.escaped \\194\\176, which the old reader turned into
     literal digits "194176" and broke the weather widget). *)
  ("reader: utf8 string round-trip", `Quick, fun () ->
    let deg = "\xC2\xB0" in
    let original = "71" ^ deg ^ "F" in
    let printed = Reader.pp_value (Ast.VStr original) in
    (* printer keeps UTF-8 bytes; must not emit \\194\\176 *)
    Alcotest.(check string) "escape_string keeps utf8"
      original (Reader.escape_string original);
    Alcotest.(check bool) "pp has no backslash-digit for degree"
      false (String.contains printed '\\' &&
             let i = try String.index printed '\\' with Not_found -> -1 in
             i >= 0 && i + 1 < String.length printed &&
             printed.[i + 1] >= '0' && printed.[i + 1] <= '9');
    match Reader.read_one printed with
    | Lit (Ast.VStr s) ->
        Alcotest.(check string) "utf8 round-trip" original s
    | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
  ("reader: decimal \\ddd byte escape", `Quick, fun () ->
    (* Legacy OCaml String.escaped form still loads: \\194\\176 → UTF-8 ° *)
    match Reader.read_one "\"\\194\\176F\"" with
    | Lit (Ast.VStr s) ->
        Alcotest.(check string) "ddd escape" "\xC2\xB0F" s
    | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
  ("reader: named escapes", `Quick, fun () ->
    match Reader.read_one "\"a\\nb\\tc\\\"d\"" with
    | Lit (Ast.VStr s) ->
        Alcotest.(check string) "named" "a\nb\tc\"d" s
    | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
  ("reader: \\uXXXX unicode escape", `Quick, fun () ->
    (* The menu-caret bug: "\u203e" must become the overline ‾ (U+203E,
       UTF-8 E2 80 BE), not the literal 4 chars "u203e". *)
    (match Reader.read_one "\"a\\u203eb\"" with
     | Lit (Ast.VStr s) ->
         Alcotest.(check string) "u203e overline" "a\xE2\x80\xBEb" s
     | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
    (* degree sign U+00B0 → C2 B0 (the weather \u00B0 case) *)
    (match Reader.read_one "\"71\\u00B0F\"" with
     | Lit (Ast.VStr s) ->
         Alcotest.(check string) "u00B0 degree" "71\xC2\xB0F" s
     | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
    (* case-insensitive hex *)
    (match Reader.read_one "\"\\u00b0\"" with
     | Lit (Ast.VStr s) -> Alcotest.(check string) "lower hex" "\xC2\xB0" s
     | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
    (* BMP 3-byte scalar U+203E already covered; check a 1-byte ASCII *)
    (match Reader.read_one "\"\\u0041\"" with
     | Lit (Ast.VStr s) -> Alcotest.(check string) "u0041 = A" "A" s
     | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other)));
  ("reader: \\uXXXX lone surrogate rejected", `Quick, fun () ->
    let raised = ref false in
    (try ignore (Reader.read_one "\"\\uD800\"")
     with Reader.Reader_error _ -> raised := true);
    Alcotest.(check bool) "lone surrogate D800 raises" true !raised);
  ("reader: \\uXXXX needs 4 hex digits", `Quick, fun () ->
    let raised = ref false in
    (try ignore (Reader.read_one "\"\\u203\""   (* only 3 hex digits *))
     with Reader.Reader_error _ -> raised := true);
    Alcotest.(check bool) "short \\u raises" true !raised);
  ("reader: \\uXXXX round-trips through pp_value", `Quick, fun () ->
    (* escape_string keeps the UTF-8 bytes, so reading back the printed
       form preserves the overline — no \\u needed on the print side. *)
    let s = "x\xE2\x80\xBEy" in
    match Reader.read_one (Reader.pp_value (Ast.VStr s)) with
    | Lit (Ast.VStr s') -> Alcotest.(check string) "roundtrip" s s'
    | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
  ("reader: forgiving fallback still drops backslash for non-u", `Quick, fun () ->
    (* Unrelated unknown escape still hits the forgiving rule. *)
    match Reader.read_one "\"a\\qb\"" with
    | Lit (Ast.VStr s) -> Alcotest.(check string) "forgiving \\q -> q" "aqb" s
    | other -> Alcotest.failf "expected string lit, got %s" (Reader.pp_expr other));
  (* Line comments: `;` must skip to end of line, not just consume the
     `;` itself. Regression for the bug where `;; package structed ...`
     headers lexed as the symbol `package` (every consumer worked around
     it by pre-stripping comments in image/lib/loader.ml). *)
  ("reader: trailing line comment is skipped", `Quick, fun () ->
    match Reader.read_one "42 ; this is a comment" with
    | Lit (Ast.VInt n) -> Alcotest.(check int64) "42" 42L n
    | other -> Alcotest.failf "expected 42, got %s" (Reader.pp_expr other));
  ("reader: leading line comment then form", `Quick, fun () ->
    match Reader.read_one ";; package structed\n(define x 99)" with
    | App (Var "define", _) -> ()
    | other -> Alcotest.failf "expected define, got %s" (Reader.pp_expr other));
  ("reader: double-semicolon header comment", `Quick, fun () ->
    (* The exact shape of a libs/*.pp header — the original bug left
       `package` as a bare symbol after consuming only the two `;;`. *)
    match Reader.read_all ";; package structed/list -- exported from system/main\n\n(define x 99)" with
    | [ App (Var "define", _) ] -> ()
    | other -> Alcotest.failf "expected single define form, got %d forms"
                 (List.length other));
  ("reader: comment-only input reads as no forms", `Quick, fun () ->
    match Reader.read_all ";; just a comment\n; another\n" with
    | [] -> ()
    | other -> Alcotest.failf "expected 0 forms, got %d" (List.length other));
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

(* Dotted-pair notation is rejected at read time (improper lists are not
   supported). A lone '.' must raise a clean Reader_error instead of
   silently becoming a symbol named "." that corrupts list length. *)
let reader_dotted_tests = [
  ("reader: dotted pair (1 . 2) raises", `Quick, fun () ->
    let raised =
      try
        let _ = Reader.read_one "(1 . 2)" in
        false
      with Reader.Reader_error _ -> true
    in
    Alcotest.(check bool) "dotted pair raises" true raised);
  ("reader: dotted rest (1 2 . 3) raises", `Quick, fun () ->
    let raised =
      try
        let _ = Reader.read_one "(1 2 . 3)" in
        false
      with Reader.Reader_error _ -> true
    in
    Alcotest.(check bool) "dotted rest raises" true raised);
  ("reader: bare dot raises", `Quick, fun () ->
    let raised =
      try
        let _ = Reader.read_one "." in
        false
      with Reader.Reader_error _ -> true
    in
    Alcotest.(check bool) "bare dot raises" true raised);
  ("reader: float .5 still parses", `Quick, fun () ->
    match Reader.read_one ".5" with
    | Lit (Ast.VFloat f) ->
        if abs_float (f -. 0.5) > 1e-9 then
          Alcotest.failf "expected 0.5, got %g" f
    | other ->
        Alcotest.failf "expected VFloat lit, got %s" (Reader.pp_expr other));
  ("reader: ellipsis ... still a symbol", `Quick, fun () ->
    match Reader.read_one "..." with
    | Ast.Var "..." -> ()
    | other ->
        Alcotest.failf "expected Var ..., got %s" (Reader.pp_expr other));
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

(* H1.7: interpreter is direct-style; [Eval.eval_string] runs under
   [Control.run_sync]. Pure tests — no services. *)

let evals_to name expr expected =
  ("eval: " ^ name, `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let budget = Eval.make_budget () in
    let got = (Eval.eval ~budget ~prims env expr) in
    Alcotest.(check string)
      (Reader.pp_value expected) (Reader.pp_value expected)
      (Reader.pp_value got))

let eval_tests = [
  evals_to "int literal" (Ast.int 42) (Ast.VInt 42L);
  ("eval: define binds var", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ = (Eval.eval_string ~prims env "(define x 99)") in
    (match Env.lookup env "x" with
     | Some (Ast.VInt n) ->
        Alcotest.(check int64) "x's value" 99L n
     | other -> Alcotest.failf "expected Some VInt, got %s"
                                 (match other with None -> "None"
                                  | Some v -> Reader.pp_value v)));
  ("eval: if-true branch", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(if #t 1 2)") in
    Alcotest.(check string) "1" "1" (Reader.pp_value got));
  ("eval: if-false branch", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(if #f 1 2)") in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("eval: lambda application", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "((lambda (x) x) 42)") in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval: quote compound list", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "'(a b c)") in
    (match got with
     | Ast.VList [Ast.VSymbol "a"; Ast.VSymbol "b"; Ast.VSymbol "c"] -> ()
     | other -> Alcotest.failf "expected (a b c) list, got %s"
                  (Reader.pp_value other)));
  ("eval: quote nested", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "'(1 (2 3))") in
    (match got with
     | Ast.VList [Ast.VInt 1L; Ast.VList [Ast.VInt 2L; Ast.VInt 3L]] -> ()
     | other -> Alcotest.failf "expected nested quote, got %s"
                  (Reader.pp_value other)));
  ("eval: primitive dispatch", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "add-one"
      (fun args -> match args with
        | [Ast.VInt n] -> (Ast.VInt (Int64.add n 1L))
        | _ -> Ast.VNil);
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(prim add-one 41)") in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval: let binds locally", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(let ((y 5)) y)") in
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
      let _ = (Eval.eval ~budget ~prims env e) in
      Alcotest.fail "expected budget exceeded"
    with Ast.Interp_error Ast.Budget_exceeded -> ());
  ("eval: unbound var raises", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    try
      let _ = (Eval.eval_string ~prims env "nope") in
      Alcotest.fail "expected Unbound_var"
    with Ast.Interp_error (Ast.Unbound_var name) ->
      Alcotest.(check string) "nope" "nope" name);
  ("eval: seq empty returns nil", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(seq)") in
    Alcotest.(check string) "nil" "()" (Reader.pp_value got));
  ("eval: seq single returns body", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(seq 7)") in
    Alcotest.(check string) "7" "7" (Reader.pp_value got));
  ("eval: seq binary returns last", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(seq 1 2)") in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("eval: seq multi returns last", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(seq 1 2 3)") in
    Alcotest.(check string) "3" "3" (Reader.pp_value got));
  ("eval: seq nested multi", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
                              "(seq 1 (seq 2 3) 4)") in
    Alcotest.(check string) "4" "4" (Reader.pp_value got));
  ("eval: seq side effects left-to-right", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "(seq (define a 1) (define b 2) (define c 3) c)") in
    Alcotest.(check string) "c" "3" (Reader.pp_value got);
    (match Env.lookup env "a", Env.lookup env "b" with
     | Some (Ast.VInt 1L), Some (Ast.VInt 2L) -> ()
     | _ -> Alcotest.fail "expected a=1 b=2 after seq defines"));
  ("eval: seq in lambda body multi", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "((lambda () (seq (define x 10) (define y 20) y)))") in
    Alcotest.(check string) "20" "20" (Reader.pp_value got));
  ("eval: letrec simple lambda", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "(letrec ((f (lambda (n) n))) (f 5))") in
    Alcotest.(check string) "5" "5" (Reader.pp_value got));
  ("eval: letrec self-recursion", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    (* loop calls itself once: #t -> recurse on #f -> base case 7.
       Prim-free: exercises the self-binding backpatch. *)
    let got = (Eval.eval_string ~prims env
      "(letrec ((loop (lambda (x) (if x (loop #f) 7)))) (loop #t))") in
    Alcotest.(check string) "7" "7" (Reader.pp_value got));
  ("eval: letrec mutual recursion", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    (* a's body calls b; b's body is a constant. Prim-free proof that
       each binding closes over env' and can reach its sibling. *)
    let got = (Eval.eval_string ~prims env
      "(letrec ((a (lambda () (b)))\
               (b (lambda () 42)))\
        (a))") in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval: letrec empty bindings", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(letrec () 42)") in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval: apply special form on lambda", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "(apply (lambda (a b) b) '(1 2))") in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("eval: apply special form on prim", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "add2"
      (fun args -> match args with
       | [Ast.VInt a; Ast.VInt b] -> Ast.VInt (Int64.add a b)
       | _ -> Ast.VNil);
    let env = Env.create () in
    Env.define env "add2" (Ast.VPrim "add2");
    let got = (Eval.eval_string ~prims env
      "(apply add2 '(1 2))") in
    Alcotest.(check string) "3" "3" (Reader.pp_value got));
  ("eval: apply errors on wrong arity", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let raised =
      try
      let _ = (Eval.eval_string ~prims env
        "(apply (lambda (a b) a) '(1))") in
      false
      with Ast.Interp_error _ -> true
    in
    Alcotest.(check bool) "arity mismatch raises" true raised);
  ("eval: optional param omitted", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "((lambda (a &optional b) b) 1)") in
    Alcotest.(check string) "empty for omitted optional" "\"\""
      (Reader.pp_value got));
  ("eval: optional param supplied", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "((lambda (a &optional b) b) 1 2)") in
    Alcotest.(check string) "2nd arg" "2" (Reader.pp_value got));
  ("eval: rest param collects extras", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "((lambda (a &rest rest) rest) 1 2 3)") in
    Alcotest.(check string) "rest list" "(2 3)" (Reader.pp_value got));
  ("eval: rest param all variadic", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "((lambda (&rest all) all) 1 2 3)") in
    Alcotest.(check string) "all args" "(1 2 3)" (Reader.pp_value got));
  ("eval: optional + rest together", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env
      "((lambda (a &optional b &rest rest) rest) 1 2 3 4)") in
    Alcotest.(check string) "rest after optional" "(3 4)"
      (Reader.pp_value got));
  ("eval: fixed arity still enforced", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let raised =
      try
      let _ = (Eval.eval_string ~prims env "((lambda (a b) a) 1)") in
      false
      with Ast.Interp_error _ -> true
    in
    Alcotest.(check bool) "fixed arity mismatch raises" true raised);
  ("eval: optional arity too many raises", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let raised =
      try
      let _ = (Eval.eval_string ~prims env
        "((lambda (a &optional b) a) 1 2 3)") in
      false
      with Ast.Interp_error _ -> true
    in
    Alcotest.(check bool) "too many args raises" true raised);
  ("eval: required arity too few raises", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let raised =
      try
      let _ = (Eval.eval_string ~prims env
        "((lambda (a b &optional c) a) 1)") in
      false
      with Ast.Interp_error _ -> true
    in
    Alcotest.(check bool) "too few required raises" true raised);
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
      | [Ast.VInt a; Ast.VInt b] -> (Ast.VInt (Int64.add a b))
      | _ -> raise (Ast.Interp_error (Ast.User_error "+")));
    let env = Env.create () in
    Env.define env "+" (Ast.VPrim "+");
    let got = (Eval.eval_string ~prims env "`(a ,(+ 1 2) c)") in
    match got with
    | Ast.VList [Ast.VSymbol "a"; Ast.VInt 3L; Ast.VSymbol "c"] -> ()
    | other -> Alcotest.failf "qq got %s" (Reader.pp_value other));
  ("quasiquote splice", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "`(a ,@'(b c) d)") in
    match got with
    | Ast.VList [Ast.VSymbol "a"; Ast.VSymbol "b"; Ast.VSymbol "c"; Ast.VSymbol "d"] -> ()
    | other -> Alcotest.failf "splice got %s" (Reader.pp_value other));
  ("defmacro when", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ =
      (Eval.eval_string ~prims env
           "(defmacro when (t b) `(if ,t ,b ()))")
    in
    let got =
      (Eval.eval_string ~prims env "(when #t 42)")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got);
    let got2 =
      (Eval.eval_string ~prims env "(when #f 42)")
    in
    Alcotest.(check string) "nil" "()" (Reader.pp_value got2));
  ("defmacro expands to let (qq)", `Quick, fun () ->
    (* Inbox c7-2611: macros expanding to let crashed with opaque
       Interp_error. Must evaluate to the bound value. *)
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ =
      (Eval.eval_string ~prims env
           "(defmacro let1 (name val body) `(let ((,name ,val)) ,body))")
    in
    let got =
      (Eval.eval_string ~prims env "(let1 a 1 a)")
    in
    Alcotest.(check string) "let1 => 1" "1" (Reader.pp_value got));
  ("defmacro expands to let (list)", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "list" (fun args -> (Ast.VList args));
    let env = Env.create () in
    (* Image binds registered prims into env as VPrim; pure tests must too. *)
    Env.define env "list" (Ast.VPrim "list");
    let _ =
      (Eval.eval_string ~prims env
           "(defmacro let1 (name val body)\
             (list (quote let) (list (list name val)) body))")
    in
    let got =
      try (Eval.eval_string ~prims env "(let1 a 1 a)")
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
          ((Eval.eval_string ~prims env
                "(defmacro bad (a . b) a)"));
        `Ok
      with Ast.Interp_error (Ast.User_error m) -> `Err m
         | Ast.Interp_error e -> `Err (Ast.pp_interp_error e)
         | Reader.Reader_error (m, _, _) -> `Err m
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
    let a = (Eval.eval_string ~prims env "(gensym)") in
    let b = (Eval.eval_string ~prims env "(gensym)") in
    match a, b with
    | Ast.VSymbol sa, Ast.VSymbol sb when sa <> sb -> ()
    | _ -> Alcotest.fail "gensym not unique");
  ("cond first match", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env
           "(cond (#f 1) (#t 2) (#t 3))")
    in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("cond else", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env "(cond (#f 1) (else 9))")
    in
    Alcotest.(check string) "9" "9" (Reader.pp_value got));
  ("cond empty", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = (Eval.eval_string ~prims env "(cond)") in
    Alcotest.(check string) "nil" "()" (Reader.pp_value got));
  ("and short-circuit", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env "(and #t 1 2)")
    in
    Alcotest.(check string) "2" "2" (Reader.pp_value got);
    let got2 =
      (Eval.eval_string ~prims env "(and #t #f 3)")
    in
    Alcotest.(check string) "false" "#f" (Reader.pp_value got2);
    let got3 = (Eval.eval_string ~prims env "(and)") in
    Alcotest.(check string) "true" "#t" (Reader.pp_value got3));
  ("or short-circuit", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env "(or #f 0 1)")
    in
    Alcotest.(check string) "0" "0" (Reader.pp_value got);
    let got2 = (Eval.eval_string ~prims env "(or)") in
    Alcotest.(check string) "false" "#f" (Reader.pp_value got2));
]

(* H1 dual-path: eval_eff + Control.run_lwt (PP_EVAL_CONTROL=eff shape). *)
let eval_eff_tests = [
  ("eval_eff: int literal", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env "42")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval_eff: define + lookup", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ = (Eval.eval_string ~prims env "(define x 99)") in
    let got = (Eval.eval_string ~prims env "x") in
    Alcotest.(check string) "99" "99" (Reader.pp_value got));
  ("eval_eff: if + lambda app", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env "((lambda (x) (if #t x 0)) 7)")
    in
    Alcotest.(check string) "7" "7" (Reader.pp_value got));
  ("eval_eff: prim via Prim_call", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "add-one"
      (fun args ->
        match args with
        | [ Ast.VInt n ] -> (Ast.VInt (Int64.add n 1L))
        | _ -> failwith "bad args");
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env "(prim add-one 41)")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("eval_eff: composition bind then prim", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "get"
      (fun args ->
        match args with
        | [ Ast.VStr _; Ast.VInt id ] ->
            (Ast.VStr ("row-" ^ Int64.to_string id))
        | _ -> failwith "bad get");
    let env = Env.create () in
    Env.define env "get" (Ast.VPrim "get");
    let got =
      (Eval.eval_string ~prims env
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
         Control.run_sync ~prims (fun () -> Eval.eval ~budget ~prims env e)
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
        | [ Ast.VStr t; Ast.VStr _ ] -> (Ast.VStr ("id-" ^ t))
        | _ -> failwith "bad create");
    Eval.register_prim prims "__with_transaction__"
      (fun args ->
        seen := "tx-enter" :: !seen;
        match args with
        | [ lam ] ->
            let budget = Eval.make_budget () in
            (* image-shaped: nested run_lwt + apply_eff *)
            Control.run_sync ~prims (fun () ->
                Eval.apply ~budget ~prims lam [])
        | _ -> failwith "bad tx");
    let env = Env.create () in
    Env.define env "create" (Ast.VPrim "create");
    let got =
      (Eval.eval_string ~prims env
           "(with-transaction (lambda () (create \"note\" \"{}\")))")
    in
    Alcotest.(check string) "id" "\"id-note\"" (Reader.pp_value got);
    Alcotest.(check bool) "tx ran" true (List.mem "tx-enter" !seen);
    Alcotest.(check bool) "create nested" true (List.mem "create" !seen));
  (* H1.5 special-form parity *)
  ("eval_eff: cond else", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      (Eval.eval_string ~prims env
           "(cond (#f 1) (else 2))")
    in
    Alcotest.(check string) "2" "2" (Reader.pp_value got));
  ("eval_eff: quasiquote unquote", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> (Ast.VInt (Int64.add a b))
        | _ -> failwith "+");
    let env = Env.create () in
    Env.define env "+" (Ast.VPrim "+");
    let got =
      (Eval.eval_string ~prims env "`(a ,(+ 1 2) c)")
    in
    match got with
    | Ast.VList [ Ast.VSymbol "a"; Ast.VInt 3L; Ast.VSymbol "c" ] -> ()
    | other -> Alcotest.failf "qq got %s" (Reader.pp_value other));
  ("eval_eff: defmacro when", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let _ =
      (Eval.eval_string ~prims env
           "(defmacro when (test body) `(if ,test ,body ()))")
    in
    let got =
      (Eval.eval_string ~prims env "(when #t 42)")
    in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
]

(* Macro string args: Lit VStr bound as (__str__ s) so expansion + unquote
   round-trip to Lit VStr (inbox c11-4013). *)
let macro_str_tests =
  let setup () =
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "string-append"
      (fun args ->
        let buf = Buffer.create 16 in
        List.iter
          (function
            | Ast.VStr s -> Buffer.add_string buf s
            | Ast.VList [ Ast.VStr "__str__"; Ast.VStr s ] ->
                Buffer.add_string buf s
            | _ -> failwith "string-append")
          args;
        Ast.VStr (Buffer.contents buf));
    let env = Env.create () in
    Env.define env "string-append" (Ast.VPrim "string-append");
    (prims, env)
  in
  [ ("macro string lit via quasiquote unquote", `Quick, fun () ->
      let prims, env = setup () in
      ignore
        (Eval.eval_string ~prims env
           "(defmacro m-str (x) `(string-append ,x \"-a\"))");
      let got = Eval.eval_string ~prims env "(m-str \"foo\")" in
      Alcotest.(check string) "foo-a" "\"foo-a\"" (Reader.pp_value got));
    ("macro string lit builds lambda body literal", `Quick, fun () ->
      let prims, env = setup () in
      ignore
        (Eval.eval_string ~prims env
           "(defmacro m-lam (x) `(lambda () ,x))");
      let got = Eval.eval_string ~prims env "((m-lam \"A\"))" in
      Alcotest.(check string) "A" "\"A\"" (Reader.pp_value got));
  ]

(* list/map|filter|foldl are kernel special forms (OCaml loops + apply). *)
let list_hof_tests =
  let setup () =
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.add a b)
        | _ -> failwith "+");
    Eval.register_prim prims "*"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.mul a b)
        | _ -> failwith "*");
    Eval.register_prim prims "="
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VBool (Int64.equal a b)
        | _ -> Ast.VBool false);
    Eval.register_prim prims "list" (fun args -> Ast.VList args);
    Eval.register_prim prims "null?"
      (function
        | [ Ast.VNil ] | [ Ast.VList [] ] -> Ast.VBool true
        | _ -> Ast.VBool false);
    Eval.register_prim prims "car"
      (function
        | [ Ast.VList (h :: _) ] -> h
        | _ -> failwith "car");
    Eval.register_prim prims "cdr"
      (function
        | [ Ast.VList (_ :: t) ] -> Ast.VList t
        | _ -> failwith "cdr");
    Eval.register_prim prims "cons"
      (function
        | [ a; Ast.VList xs ] -> Ast.VList (a :: xs)
        | [ a; b ] -> Ast.VPair (a, b)
        | _ -> failwith "cons");
    let env = Env.create () in
    List.iter
      (fun n -> Env.define env n (Ast.VPrim n))
      [ "+"; "*"; "="; "list"; "null?"; "car"; "cdr"; "cons" ];
    (prims, env)
  in
  [ ("list/map doubles", `Quick, fun () ->
      let prims, env = setup () in
      let got =
        Eval.eval_string ~prims env
          "(list/map (lambda (x) (* x 2)) (list 1 2 3))"
      in
      Alcotest.(check string) "map" "(2 4 6)" (Reader.pp_value got));
    ("list/filter evens", `Quick, fun () ->
      let prims, env = setup () in
      let got =
        Eval.eval_string ~prims env
          "(list/filter (lambda (x) (= x 2)) (list 1 2 3 2))"
      in
      Alcotest.(check string) "filter" "(2 2)" (Reader.pp_value got));
    ("list/foldl sum", `Quick, fun () ->
      let prims, env = setup () in
      let got =
        Eval.eval_string ~prims env
          "(list/foldl (lambda (a x) (+ a x)) 0 (list 1 2 3 4))"
      in
      Alcotest.(check string) "fold" "10" (Reader.pp_value got));
    ("list/map empty", `Quick, fun () ->
      let prims, env = setup () in
      let got =
        Eval.eval_string ~prims env
          "(list/map (lambda (x) x) (list))"
      in
      Alcotest.(check string) "empty" "()" (Reader.pp_value got));
  ]

(* Tail-call optimization: trampoline keeps OCaml stack flat. *)
let tco_tests =
  let setup () =
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.add a b)
        | _ -> failwith "+");
    Eval.register_prim prims "-"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.sub a b)
        | [ Ast.VInt a ] -> Ast.VInt (Int64.neg a)
        | _ -> failwith "-");
    Eval.register_prim prims "*"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VInt (Int64.mul a b)
        | _ -> failwith "*");
    Eval.register_prim prims "="
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VBool (Int64.equal a b)
        | _ -> Ast.VBool false);
    Eval.register_prim prims "<"
      (fun args ->
        match args with
        | [ Ast.VInt a; Ast.VInt b ] -> Ast.VBool (a < b)
        | _ -> Ast.VBool false);
    let env = Env.create () in
    List.iter
      (fun n -> Env.define env n (Ast.VPrim n))
      [ "+"; "-"; "*"; "="; "<" ];
    (prims, env)
  in
  [
    ("tail sum does not stack-overflow", `Quick, fun () ->
      let prims, env = setup () in
      (* Self-tail recursion: depth far above typical OCaml stack. *)
      ignore
        (Eval.eval_string ~prims env
           "(define sumt (lambda (n acc)\n\
               (if (= n 0) acc (sumt (- n 1) (+ acc n)))))");
      let budget = Eval.make_budget ~limit:2_000_000 () in
      let got =
        Control.run_sync ~prims (fun () ->
            Eval.eval ~budget ~prims:Eval.empty_prims env
              (Reader.read_one "(sumt 100000 0)"))
      in
      (* 100000*100001/2 = 5_000_050_000 *)
      Alcotest.(check string) "sum" "5000050000" (Reader.pp_value got));
    ("non-tail factorial still works", `Quick, fun () ->
      let prims, env = setup () in
      ignore
        (Eval.eval_string ~prims env
           "(define fac (lambda (n)\n\
               (if (= n 0) 1 (* n (fac (- n 1))))))");
      let got = Eval.eval_string ~prims env "(fac 10)" in
      Alcotest.(check string) "10!" "3628800" (Reader.pp_value got));
    ("infinite tail loop hits budget", `Quick, fun () ->
      let prims, env = setup () in
      ignore
        (Eval.eval_string ~prims env
           "(define loop (lambda () (loop)))");
      let raised =
        try
          ignore (Eval.eval_string ~prims env "(loop)");
          false
        with
        | Ast.Interp_error Ast.Budget_exceeded -> true
        | _ -> false
      in
      Alcotest.(check bool) "budget" true raised);
    ("mutual tail via if", `Quick, fun () ->
      let prims, env = setup () in
      ignore
        (Eval.eval_string ~prims env
           "(define even? (lambda (n)\n\
               (if (= n 0) #t (odd? (- n 1)))))");
      ignore
        (Eval.eval_string ~prims env
           "(define odd? (lambda (n)\n\
               (if (= n 0) #f (even? (- n 1)))))");
      let budget = Eval.make_budget ~limit:2_000_000 () in
      let got =
        Control.run_sync ~prims (fun () ->
            Eval.eval ~budget ~prims:Eval.empty_prims env
              (Reader.read_one "(even? 100000)"))
      in
      Alcotest.(check string) "even" "#t" (Reader.pp_value got));
  ]

(** Minimal pure stand-ins for image symbol/string prims so dialect tests
    stay service-free while covering the symbols contract. *)
let register_symbol_prims (prims : Eval.prim_table) : unit =
  Eval.register_prim prims "symbol?" (function
    | [Ast.VSymbol _] -> Ast.VBool true
    | [_] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "symbol?")));
  Eval.register_prim prims "string?" (function
    | [Ast.VStr _] -> Ast.VBool true
    | [_] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "string?")));
  Eval.register_prim prims "symbol=?" (function
    | [Ast.VSymbol a; Ast.VSymbol b] -> Ast.VBool (String.equal a b)
    | [_; _] -> Ast.VBool false
    | _ -> raise (Ast.Interp_error (Ast.User_error "symbol=?")));
  Eval.register_prim prims "symbol->string" (function
    | [Ast.VSymbol s] -> Ast.VStr s
    | _ -> raise (Ast.Interp_error (Ast.User_error "symbol->string")));
  Eval.register_prim prims "string->symbol" (function
    | [Ast.VStr s] -> Ast.VSymbol s
    | _ -> raise (Ast.Interp_error (Ast.User_error "string->symbol")));
  Eval.register_prim prims "type-of" (function
    | [Ast.VSymbol _] -> Ast.VStr "symbol"
    | [Ast.VStr _] -> Ast.VStr "str"
    | [Ast.VInt _] -> Ast.VStr "int"
    | [Ast.VBool _] -> Ast.VStr "bool"
    | [Ast.VNil] -> Ast.VStr "nil"
    | [Ast.VList _] -> Ast.VStr "list"
    | _ -> Ast.VStr "other")

let dialect_tests = [
  ("symbols: quote distinct from string", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let a = Eval.eval_string ~prims env "'hello" in
    let b = Eval.eval_string ~prims env "\"hello\"" in
    (match a, b with
     | Ast.VSymbol "hello", Ast.VStr "hello" -> ()
     | _ -> Alcotest.failf "got %s and %s"
              (Reader.pp_value a) (Reader.pp_value b)));
  ("symbols: quote list heads are symbols", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let v = Eval.eval_string ~prims env "'(lambda (x) x)" in
    (match v with
     | Ast.VList (Ast.VSymbol "lambda" :: _) -> ()
     | other -> Alcotest.failf "head: %s" (Reader.pp_value other)));
  ("symbols: value_to_expr preserves Var vs Lit VStr", `Quick, fun () ->
    let e = Reader.read_one "(lambda (x) \"hi\")" in
    let v = Eval.quote_to_value e in
    (match v with
     | Ast.VList [Ast.VSymbol "lambda"; Ast.VList [Ast.VSymbol "x"]; Ast.VStr "hi"] -> ()
     | other -> Alcotest.failf "quote_to_value: %s" (Reader.pp_value other));
    let e' = Eval.value_to_expr v in
    (match e' with
     | Ast.Lambda (["x"], Ast.Lit (Ast.VStr "hi")) -> ()
     | other -> Alcotest.failf "value_to_expr: %s" (Reader.pp_expr other)));
  ("symbols: gensym returns VSymbol", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let a = Eval.eval_string ~prims env "(gensym)" in
    let b = Eval.eval_string ~prims env "(gensym)" in
    (match a, b with
     | Ast.VSymbol sa, Ast.VSymbol sb when sa <> sb -> ()
     | _ -> Alcotest.failf "gensym: %s %s"
              (Reader.pp_value a) (Reader.pp_value b)));
  ("symbols: prims symbol? string? symbol=? conversions type-of", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    register_symbol_prims prims;
    let env = Env.create () in
    List.iter (fun n -> Env.define env n (Ast.VPrim n))
      ["symbol?"; "string?"; "symbol=?"; "symbol->string"; "string->symbol"; "type-of"];
    let check expr expect =
      let got = Eval.eval_string ~prims env expr in
      Alcotest.(check string) expr expect (Reader.pp_value got)
    in
    check "(symbol? 'lambda)" "#t";
    check "(symbol? \"lambda\")" "#f";
    check "(string? \"lambda\")" "#t";
    check "(string? 'lambda)" "#f";
    check "(symbol=? 'a 'a)" "#t";
    check "(symbol=? 'a 'b)" "#f";
    check "(symbol=? 'a \"a\")" "#f";
    check "(symbol->string 'hello)" "\"hello\"";
    check "(string->symbol \"hello\")" "hello";
    check "(type-of 'x)" "\"symbol\"";
    check "(type-of \"x\")" "\"str\"");
  ("define sugar", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+" (function
      | [Ast.VInt a; Ast.VInt b] -> Ast.VInt (Int64.add a b)
      | _ -> raise (Ast.Interp_error (Ast.User_error "+")));
    let env = Env.create () in
    Env.define env "+" (Ast.VPrim "+");
    ignore (Eval.eval_string ~prims env "(define (add1 x) (+ x 1))");
    let got = Eval.eval_string ~prims env "(add1 41)" in
    Alcotest.(check string) "42" "42" (Reader.pp_value got));
  ("define docstring trailing", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let v = Eval.eval_string ~prims env "(define d1 1 \"one\")" in
    Alcotest.(check string) "bind" "1" (Reader.pp_value v);
    let got = Eval.eval_string ~prims env "d1" in
    Alcotest.(check string) "lookup" "1" (Reader.pp_value got);
    (match Eval.parse_top_define (Reader.read_one "(define d1 1 \"one\")") with
     | Some { name = "d1"; doc = Some "one"; _ } -> ()
     | _ -> Alcotest.fail "parse_top_define trailing"));
  ("define sugar leading docstring", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+" (function
      | [Ast.VInt a; Ast.VInt b] -> Ast.VInt (Int64.add a b)
      | _ -> raise (Ast.Interp_error (Ast.User_error "+")));
    let env = Env.create () in
    Env.define env "+" (Ast.VPrim "+");
    ignore (Eval.eval_string ~prims env
              "(define (inc x) \"add one\" (+ x 1))");
    let got = Eval.eval_string ~prims env "(inc 2)" in
    Alcotest.(check string) "3" "3" (Reader.pp_value got);
    (match
       Eval.parse_top_define
         (Reader.read_one "(define (inc x) \"add one\" (+ x 1))")
     with
     | Some { name = "inc"; doc = Some "add one"; body = Ast.Lambda (["x"], _) } ->
         ()
     | _ -> Alcotest.fail "parse sugar docstring"));
  ("define lambda leading docstring", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    Eval.register_prim prims "+" (function
      | [Ast.VInt a; Ast.VInt b] -> Ast.VInt (Int64.add a b)
      | _ -> raise (Ast.Interp_error (Ast.User_error "+")));
    let env = Env.create () in
    Env.define env "+" (Ast.VPrim "+");
    ignore (Eval.eval_string ~prims env
              "(define f (lambda (x) \"lam\" (+ x 1)))");
    let got = Eval.eval_string ~prims env "(f 10)" in
    Alcotest.(check string) "11" "11" (Reader.pp_value got);
    (match
       Eval.parse_top_define
         (Reader.read_one "(define f (lambda (x) \"lam\" (+ x 1)))")
     with
     | Some { doc = Some "lam"; body = Ast.App (Ast.Var "lambda", _); _ } ->
         ()
     | other ->
         Alcotest.failf "lambda doc peel: %s"
           (match other with None -> "none" | Some _ -> "bad shape")));
  ("define lone string body is value not doc", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define (hi) \"hello\")");
    let got = Eval.eval_string ~prims env "(hi)" in
    Alcotest.(check string) "hello" "\"hello\"" (Reader.pp_value got);
    (match Eval.parse_top_define (Reader.read_one "(define (hi) \"hello\")") with
     | Some { doc = None; _ } -> ()
     | _ -> Alcotest.fail "lone string must not be docstring"));
  ("begin multi-body", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got = Eval.eval_string ~prims env "(begin 1 2 3)" in
    Alcotest.(check string) "3" "3" (Reader.pp_value got));
  ("match list lambda", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Eval.eval_string ~prims env
        "(match '(lambda (x) y)\n\
          ((lambda params body) params)\n\
          (_ 0))"
    in
    match got with
    | Ast.VList [Ast.VSymbol "x"] -> ()
    | other -> Alcotest.failf "match got %s" (Reader.pp_value other));
  ("guard catches user error", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Eval.eval_string ~prims env
        "(guard (lambda (e) 99) (error \"boom\"))"
    in
    Alcotest.(check string) "99" "99" (Reader.pp_value got));
  ("guard handler receives error tagged list", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let got =
      Eval.eval_string ~prims env
        "(guard (lambda (e) e) (error \"boom\"))"
    in
    match got with
    | Ast.VList [Ast.VSymbol "error"; Ast.VStr msg; Ast.VSymbol "user_error"] ->
        Alcotest.(check string) "msg" "boom" msg
    | other -> Alcotest.failf "guard err shape: %s" (Reader.pp_value other));
  ("guard does not catch Budget_exceeded", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore
      (Eval.eval_string ~prims env
         "(define loop (lambda () (loop)))");
    let raised =
      try
        let budget = Eval.make_budget ~limit:50 () in
        ignore
          (Control.run_sync ~prims (fun () ->
              Eval.eval ~budget ~prims:Eval.empty_prims env
                (Reader.read_one
                   "(guard (lambda (e) 0) (loop))")));
        false
      with
      | Ast.Interp_error Ast.Budget_exceeded -> true
      | _ -> false
    in
    Alcotest.(check bool) "budget escapes guard" true raised);
  ("parse error includes line:col", `Quick, fun () ->
    (match
       try
         ignore (Reader.read_one "(foo");
         None
       with
       | Reader.Reader_error (msg, line, col) -> Some (msg, line, col)
       | Ast.Interp_error (Ast.Parse_error (msg, line, col)) ->
           Some (msg, line, col)
       | _ -> None
     with
     | Some (_msg, line, col) ->
         Alcotest.(check bool) "line positive" true (line >= 1);
         Alcotest.(check bool) "col positive" true (col >= 1);
         (match Ast.interp_error_loc (Ast.Parse_error ("x", line, col)) with
          | Some (l, c) ->
              Alcotest.(check int) "loc line" line l;
              Alcotest.(check int) "loc col" col c
          | None -> Alcotest.fail "interp_error_loc missing")
     | None -> Alcotest.fail "expected parse error with loc"));
  ("unbound var kind and name", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    (match
       try
         ignore (Eval.eval_string ~prims env "missing-name");
         None
       with Ast.Interp_error e -> Some e
     with
     | Some (Ast.Unbound_var name as e) ->
         Alcotest.(check string) "name" "missing-name" name;
         Alcotest.(check string) "kind" "unbound_var" (Ast.interp_error_kind e)
     | Some e ->
         Alcotest.failf "wrong error: %s" (Ast.pp_interp_error e)
     | None -> Alcotest.fail "expected Unbound_var"));
  ("error kinds are stable snake_case tags", `Quick, fun () ->
    Alcotest.(check string) "budget" "budget_exceeded"
      (Ast.interp_error_kind Ast.Budget_exceeded);
    Alcotest.(check string) "user" "user_error"
      (Ast.interp_error_kind (Ast.User_error "x"));
    Alcotest.(check string) "arity" "arity_mismatch"
      (Ast.interp_error_kind (Ast.Arity_mismatch ("f", 1, 2)));
    Alcotest.(check string) "parse" "parse_error"
      (Ast.interp_error_kind (Ast.Parse_error ("bad", 1, 2)));
    Alcotest.(check string) "prim" "prim_error"
      (Ast.interp_error_kind (Ast.Prim_error ("p", "m"))));
]

(* --- public-route gate (Route_gate) ---
   Pure fail-closed gate: infer a handler's effects from the in-memory
   env and check against the public-route allowlist. Unknown / non-function
   handlers are DENIED. No DB, no Lwt — tests build an env with lambda
   handlers whose bodies mention prims (the bodies are not evaluated at
   define time, so no prim table is needed). *)
let gate_ok env handler =
  match Route_gate.check_public_handler ~env handler with
  | Ok () -> true
  | Error _ -> false

let gate_tests = [
  ("gate: pure handler allowed", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define h (lambda () 1))");
    Alcotest.(check bool) "pure allowed" true (gate_ok env "h"));
  ("gate: write-db handler denied", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define h (lambda () (create)))");
    Alcotest.(check bool) "write denied" false (gate_ok env "h"));
  ("gate: read-db handler allowed", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define h (lambda () (get)))");
    Alcotest.(check bool) "read allowed" true (gate_ok env "h"));
  ("gate: branch handler denied", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define h (lambda () (fork)))");
    Alcotest.(check bool) "branch denied" false (gate_ok env "h"));
  ("gate: unknown handler denied (fail-closed)", `Quick, fun () ->
    let env = Env.create () in
    Alcotest.(check bool) "unknown denied" false (gate_ok env "nope"));
  ("gate: non-function binding denied", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define x 5)");
    Alcotest.(check bool) "non-fn denied" false (gate_ok env "x"));
  ("gate: handler calling a write-db sibling denied (env known_fn)", `Quick, fun () ->
    (* Cross-reference via the in-memory env: h calls bad; bad writes.
       The known_fn built from env lambdas must resolve bad → WriteDb. *)
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define bad (lambda () (create)))");
    ignore (Eval.eval_string ~prims env "(define h (lambda () (bad)))");
    Alcotest.(check bool) "sibling write denied" false (gate_ok env "h"));
  ("gate: handler calling a read-db sibling allowed", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define reader (lambda () (get)))");
    ignore (Eval.eval_string ~prims env "(define h (lambda () (reader)))");
    Alcotest.(check bool) "sibling read allowed" true (gate_ok env "h"));
  ("gate: http-get handler denied (Net effect)", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env "(define h (lambda () (http-get)))");
    Alcotest.(check bool) "net denied by default" false (gate_ok env "h"));
  ("gate: macro hiding create is denied after expand", `Quick, fun () ->
    (* BORGE-1785212882: (wq …) expands to (create …); gate must see WriteDb. *)
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env
      "(defmacro wq (t) `(create ,t))");
    ignore (Eval.eval_string ~prims env
      "(define h (lambda () (wq note)))");
    Alcotest.(check bool) "macro write denied" false (gate_ok env "h"));
  ("gate: pure macro still allowed", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    ignore (Eval.eval_string ~prims env
      "(defmacro twice (x) `(+ ,x ,x))");
    ignore (Eval.eval_string ~prims env
      "(define h (lambda () (twice 3)))");
    Alcotest.(check bool) "pure macro allowed" true (gate_ok env "h"));
]

let contains_sub ~sub s =
  let n = String.length s and m = String.length sub in
  let rec find i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else find (i + 1)
  in
  m = 0 || find 0

let shadow_tests = [
  ("define cannot shadow VPrim", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    Env.define env "create" (Ast.VPrim "create");
    (match
       try
         ignore (Eval.eval_string ~prims env "(define create (lambda () 1))");
         None
       with Ast.Interp_error (Ast.User_error m) -> Some m
          | exn -> Alcotest.failf "unexpected: %s" (Printexc.to_string exn)
     with
     | Some m ->
         Alcotest.(check bool) "mentions primitive" true
           (contains_sub ~sub:"primitive" m)
     | None -> Alcotest.fail "expected define to reject prim shadow"));
  ("define cannot shadow special form name", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    (match
       try
         ignore (Eval.eval_string ~prims env "(define if (lambda () 1))");
         None
       with Ast.Interp_error (Ast.User_error m) -> Some m
          | exn -> Alcotest.failf "unexpected: %s" (Printexc.to_string exn)
     with
     | Some m ->
         Alcotest.(check bool) "mentions special form" true
           (contains_sub ~sub:"special form" m)
     | None -> Alcotest.fail "expected define to reject special form"));
  ("define of ordinary name still works", `Quick, fun () ->
    let prims = Eval.make_prim_table () in
    let env = Env.create () in
    let v = Eval.eval_string ~prims env "(define ok (lambda () 42))" in
    match v with
    | Ast.VLambda _ -> ()
    | other -> Alcotest.failf "got %s" (Reader.pp_value other));
]


let () =
  Alcotest.run "pricklypear-lisp" [
    "reader", reader_tests @ reader_list_tests @ reader_completeness_tests
               @ reader_dotted_tests;
    "ast-roundtrip", ast_roundtrip_tests;
    "pretty-print", pretty_print_tests;
    "eval", eval_tests;
    "macros", macro_tests;
    "macro-str", macro_str_tests;
    "eval-eff", eval_eff_tests;
    "list-hof", list_hof_tests;
    "tco", tco_tests;
    "dialect", dialect_tests;
    "gate", gate_tests;
    "shadow", shadow_tests;
  ]
