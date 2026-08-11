(** Pricklypear Image — primitive tests (no DB).

    Tests the PURE primitives in image/lib/primitives.ml by evaluating
    Lisp forms against a session prim_table + env built the same way
    Session.make builds them. We don't actually call Session (which
    needs manifest + store); instead we build the table directly.

    H1.7: lisp eval is direct-style; pure image prims still register as
    Lwt via [register_sync], serviced by [Control_rim.run_lwt]. *)
open Pricklypear_lisp.Ast

(* Build a prim_table + an env with all prims auto-defined as VPrim,
   so `(+)` etc. resolve. Mirrors image/lib/session.ml's setup. *)
let make_test_env () =
  let prim_table = Pricklypear_image_lib.Primitives.make_with_defaults () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ ->
      Pricklypear_lisp.Env.define env name (VPrim name))
    prim_table;
  Pricklypear_lisp.Env.define env "pi" (VFloat Float.pi);
  (env, prim_table)

(* Helper: eval a Lisp form string against the test env under the effect rim. *)
let eval s =
  let env, prims = make_test_env () in
  Pricklypear_image_lib.Control_rim.run ~prims (fun () ->
      Pricklypear_lisp.Eval.eval_string_direct
        ~prims:Pricklypear_lisp.Eval.empty_prims env s)

(* Helper: eval to a string-pretty form. *)
let eval_str s =
  Pricklypear_lisp.Reader.pp_value (eval s)

(* --- arithmetic --- *)
let arithmetic_tests = [
  ("+ variadic sums", `Quick, fun () ->
    Alcotest.(check string) "sum" "42" (eval_str "(+ 1 2 39)"));
  ("+ identity", `Quick, fun () ->
    Alcotest.(check string) "0-arg add" "0" (eval_str "(+)"));
  ("- binary", `Quick, fun () ->
    Alcotest.(check string) "sub" "5" (eval_str "(- 10 5)"));
  ("- unary negates", `Quick, fun () ->
    Alcotest.(check string) "neg" "-7" (eval_str "(- 7)"));
  ("* variadic products", `Quick, fun () ->
    Alcotest.(check string) "prod" "24" (eval_str "(* 2 3 4)"));
  ("* identity", `Quick, fun () ->
    Alcotest.(check string) "1 mul" "1" (eval_str "(*)"));
  ("div integer", `Quick, fun () ->
    Alcotest.(check string) "div" "5" (eval_str "(div 10 2)"));
  ("/ alias of div", `Quick, fun () ->
    Alcotest.(check string) "/" "5" (eval_str "(/ 10 2)"));
  ("/ truncates ints", `Quick, fun () ->
    Alcotest.(check string) "/ trunc" "2" (eval_str "(/ 5 2)"));
  ("= numeric equality true", `Quick, fun () ->
    Alcotest.(check string) "eq true" "#t" (eval_str "(= 5 5)"));
  ("= numeric equality false", `Quick, fun () ->
    Alcotest.(check string) "eq false" "#f" (eval_str "(= 5 6)"));
  ("<  less-than true", `Quick, fun () ->
    Alcotest.(check string) "lt true" "#t" (eval_str "(< 3 4)"));
  (">  greater-than false", `Quick, fun () ->
    Alcotest.(check string) "gt" "#f" (eval_str "(> 3 4)"));
  ("<= true on equal", `Quick, fun () ->
    Alcotest.(check string) "le eq" "#t" (eval_str "(<= 4 4)"));
  (">= false on less", `Quick, fun () ->
    Alcotest.(check string) "ge" "#f" (eval_str "(>= 3 5)"));
  ("not flips bool", `Quick, fun () ->
    Alcotest.(check string) "not true" "#f" (eval_str "(not #t)"));
  ("not nil -> true", `Quick, fun () ->
    Alcotest.(check string) "not nil" "#t" (eval_str "(not ())"));
]

(* --- lisp-interpreter.math --- *)
let math_tests = [
  ("expt int-in int-out", `Quick, fun () ->
    Alcotest.(check string) "2^10" "1024" (eval_str "(expt 2 10)"));
  ("expt 0 exp", `Quick, fun () ->
    Alcotest.(check string) "n^0" "1" (eval_str "(expt 9 0)"));
  ("expt float path", `Quick, fun () ->
    let v = eval "(expt 2.0 0.5)" in
    match v with
    | VFloat f ->
        Alcotest.(check bool) "~sqrt2" true
          (abs_float (f -. sqrt 2.) < 1e-9)
    | _ -> Alcotest.fail "expected float");
  ("sqrt 9", `Quick, fun () ->
    Alcotest.(check string) "sqrt" "3.0" (eval_str "(sqrt 9)"));
  ("sqrt negative errors", `Quick, fun () ->
    match try ignore (eval "(sqrt -1)"); None with
          | Interp_error (User_error (m, _)) -> Some m
          | _ -> Some "other"
    with
    | Some m -> Alcotest.(check bool) "msg" true (String.length m > 0)
    | None -> Alcotest.fail "expected error");
  ("exp 0", `Quick, fun () ->
    Alcotest.(check string) "e0" "1.0" (eval_str "(exp 0)"));
  ("log e ~ 1", `Quick, fun () ->
    let v = eval "(log (exp 1))" in
    match v with
    | VFloat f -> Alcotest.(check bool) "~1" true (abs_float (f -. 1.) < 1e-9)
    | _ -> Alcotest.fail "float");
  ("log base 10 of 1000", `Quick, fun () ->
    let v = eval "(log 1000 10)" in
    match v with
    | VFloat f -> Alcotest.(check bool) "~3" true (abs_float (f -. 3.) < 1e-9)
    | _ -> Alcotest.fail "float");
  ("log non-positive errors", `Quick, fun () ->
    match try ignore (eval "(log 0)"); None with
          | Interp_error (User_error (_, _)) -> Some ()
          | _ -> None
    with
    | Some () -> ()
    | None -> Alcotest.fail "expected error");
  ("sin 0", `Quick, fun () ->
    Alcotest.(check string) "sin0" "0.0" (eval_str "(sin 0)"));
  ("cos 0", `Quick, fun () ->
    Alcotest.(check string) "cos0" "1.0" (eval_str "(cos 0)"));
  ("tan 0", `Quick, fun () ->
    Alcotest.(check string) "tan0" "0.0" (eval_str "(tan 0)"));
  ("asin 0", `Quick, fun () ->
    Alcotest.(check string) "asin0" "0.0" (eval_str "(asin 0)"));
  ("acos 1", `Quick, fun () ->
    Alcotest.(check string) "acos1" "0.0" (eval_str "(acos 1)"));
  ("atan 0", `Quick, fun () ->
    Alcotest.(check string) "atan0" "0.0" (eval_str "(atan 0)"));
  ("atan2 quadrant", `Quick, fun () ->
    let v = eval "(atan 1.0 1.0)" in
    match v with
    | VFloat f ->
        Alcotest.(check bool) "pi/4" true
          (abs_float (f -. (Float.pi /. 4.)) < 1e-9)
    | _ -> Alcotest.fail "float");
  ("asin domain error", `Quick, fun () ->
    match try ignore (eval "(asin 2)"); None with
          | Interp_error (User_error (_, _)) -> Some ()
          | _ -> None
    with
    | Some () -> ()
    | None -> Alcotest.fail "expected error");
  ("pi bound value", `Quick, fun () ->
    let v = eval "pi" in
    match v with
    | VFloat f ->
        Alcotest.(check bool) "pi" true (abs_float (f -. Float.pi) < 1e-15)
    | _ -> Alcotest.fail "pi not float");
  ("number? int", `Quick, fun () ->
    Alcotest.(check string) "n?" "#t" (eval_str "(number? 3)"));
  ("number? float", `Quick, fun () ->
    Alcotest.(check string) "n?f" "#t" (eval_str "(number? 3.0)"));
  ("number? str", `Quick, fun () ->
    Alcotest.(check string) "n?s" "#f" (eval_str "(number? \"3\")"));
  ("integer? int", `Quick, fun () ->
    Alcotest.(check string) "i?" "#t" (eval_str "(integer? 3)"));
  ("integer? float tag-based", `Quick, fun () ->
    Alcotest.(check string) "i?f" "#f" (eval_str "(integer? 3.0)"));
  ("zero?", `Quick, fun () ->
    Alcotest.(check string) "z" "#t" (eval_str "(zero? 0)"));
  ("positive?", `Quick, fun () ->
    Alcotest.(check string) "p" "#t" (eval_str "(positive? 1)"));
  ("negative?", `Quick, fun () ->
    Alcotest.(check string) "neg" "#t" (eval_str "(negative? -1)"));
  ("even?", `Quick, fun () ->
    Alcotest.(check string) "even" "#t" (eval_str "(even? 4)"));
  ("odd?", `Quick, fun () ->
    Alcotest.(check string) "odd" "#t" (eval_str "(odd? 3)"));
  ("even? non-integral float errors", `Quick, fun () ->
    match try ignore (eval "(even? 1.5)"); None with
          | Interp_error (User_error (_, _)) -> Some ()
          | _ -> None
    with
    | Some () -> ()
    | None -> Alcotest.fail "expected error");
  ("quotient", `Quick, fun () ->
    Alcotest.(check string) "q" "3" (eval_str "(quotient 10 3)"));
  ("remainder", `Quick, fun () ->
    Alcotest.(check string) "r" "1" (eval_str "(remainder 10 3)"));
  ("quotient negative dividend", `Quick, fun () ->
    (* Int64.div truncates toward zero *)
    Alcotest.(check string) "qneg" "-3" (eval_str "(quotient -10 3)"));
  ("remainder negative dividend", `Quick, fun () ->
    Alcotest.(check string) "rneg" "-1" (eval_str "(remainder -10 3)"));
  ("quotient by zero errors", `Quick, fun () ->
    match try ignore (eval "(quotient 1 0)"); None with
          | Interp_error (User_error (_, _)) -> Some ()
          | _ -> None
    with
    | Some () -> ()
    | None -> Alcotest.fail "expected error");
  ("quotient rejects float", `Quick, fun () ->
    match try ignore (eval "(quotient 1.0 2.0)"); None with
          | Interp_error (User_error (_, _)) -> Some ()
          | _ -> None
    with
    | Some () -> ()
    | None -> Alcotest.fail "expected error");
  ("div by zero errors", `Quick, fun () ->
    match try ignore (eval "(div 1 0)"); None with
          | Interp_error (User_error (_, _)) -> Some ()
          | _ -> None
    with
    | Some () -> ()
    | None -> Alcotest.fail "expected error");
  ("random n in range", `Quick, fun () ->
    ignore (eval "(random-seed 42)");
    let v = eval "(random 10)" in
    match v with
    | VInt n ->
        Alcotest.(check bool) "range" true (n >= 0L && n < 10L)
    | _ -> Alcotest.fail "expected int");
  ("random zero-arg float", `Quick, fun () ->
    ignore (eval "(random-seed 1)");
    let v = eval "(random)" in
    match v with
    | VFloat f ->
        Alcotest.(check bool) "[0,1)" true (f >= 0. && f < 1.)
    | _ -> Alcotest.fail "expected float");
  ("random-seed deterministic", `Quick, fun () ->
    ignore (eval "(random-seed 99)");
    let a = eval_str "(random 1000)" in
    ignore (eval "(random-seed 99)");
    let b = eval_str "(random 1000)" in
    Alcotest.(check string) "same" a b);
]

(* --- list/pair operations --- *)
let list_tests = [
  ("list variadic", `Quick, fun () ->
    Alcotest.(check string) "list" "(1 2 3)" (eval_str "(list 1 2 3)"));
  ("list zero-arg is nil", `Quick, fun () ->
    Alcotest.(check string) "empty list" "()" (eval_str "(list)"));
  ("car of list", `Quick, fun () ->
    Alcotest.(check string) "car head" "1" (eval_str "(car (list 1 2 3))"));
  ("cdr of list", `Quick, fun () ->
    Alcotest.(check string) "cdr tail" "(2 3)" (eval_str "(cdr (list 1 2 3))"));
  ("car of pair", `Quick, fun () ->
    Alcotest.(check string) "car pair" "1" (eval_str "(car (cons 1 2))"));
  ("cdr of pair", `Quick, fun () ->
    Alcotest.(check string) "cdr pair" "2" (eval_str "(cdr (cons 1 2))"));
  ("cons onto list", `Quick, fun () ->
    Alcotest.(check string) "cons" "(0 1 2)" (eval_str "(cons 0 (list 1 2))"));
  ("null? empty", `Quick, fun () ->
    Alcotest.(check string) "null?()" "#t" (eval_str "(null? ())"));
  ("null? on non-empty", `Quick, fun () ->
    Alcotest.(check string) "null? non-empty" "#f"
      (eval_str "(null? (list 1 2))"));
  ("pair? on pair", `Quick, fun () ->
    Alcotest.(check string) "pair? pair" "#t"
      (eval_str "(pair? (cons 1 2))"));
  ("pair? on empty list", `Quick, fun () ->
    Alcotest.(check string) "pair? empty" "#f"
      (eval_str "(pair? (list))"));
  ("count on list", `Quick, fun () ->
    Alcotest.(check string) "count" "3" (eval_str "(count (list 1 2 3))"));
  ("count on nil", `Quick, fun () ->
    Alcotest.(check string) "count nil" "0" (eval_str "(count ())"));
]

(* --- string / reflection --- *)
let string_tests = [
  ("string-append", `Quick, fun () ->
    Alcotest.(check string) "concat" "\"foobarbaz\""
      (eval_str "(string-append \"foo\" \"bar\" \"baz\")"));
  ("str of int", `Quick, fun () ->
    Alcotest.(check string) "str int" "\"42\""
      (eval_str "(str 42)"));
  ("str of bool", `Quick, fun () ->
    Alcotest.(check string) "str bool" "\"#t\""
      (eval_str "(str #t)"));
  ("type-of int", `Quick, fun () ->
    Alcotest.(check string) "type int" "\"int\""
      (eval_str "(type-of 5)"));
  ("type-of str", `Quick, fun () ->
    Alcotest.(check string) "type str" "\"str\""
      (eval_str "(type-of \"hello\")"));
  ("type-of list", `Quick, fun () ->
    Alcotest.(check string) "type list" "\"list\""
      (eval_str "(type-of (list 1 2))"));
  ("type-of bool", `Quick, fun () ->
    Alcotest.(check string) "type bool" "\"bool\""
      (eval_str "(type-of #f)"));
  ("string<? lexicographic", `Quick, fun () ->
    Alcotest.(check string) "lt" "#t"
      (eval_str "(string<? \"abc\" \"abd\")"));
  ("string<? false on equal", `Quick, fun () ->
    Alcotest.(check string) "eq" "#f"
      (eval_str "(string<? \"abc\" \"abc\")"));
  ("string<? orders ISO dates", `Quick, fun () ->
    Alcotest.(check string) "dates" "#t"
      (eval_str "(string<? \"2026-07-16\" \"2026-07-23\")"));
  ("string<? is lexicographic not numeric", `Quick, fun () ->
    Alcotest.(check string) "coerced ints compare as strings" "#t"
      (eval_str "(string<? 10 9)"));
  ("string<=? true on equal", `Quick, fun () ->
    Alcotest.(check string) "le eq" "#t"
      (eval_str "(string<=? \"abc\" \"abc\")"));
  ("string>? lexicographic", `Quick, fun () ->
    Alcotest.(check string) "gt" "#t"
      (eval_str "(string>? \"abd\" \"abc\")"));
  ("string>=? true on equal", `Quick, fun () ->
    Alcotest.(check string) "ge eq" "#t"
      (eval_str "(string>=? \"abc\" \"abc\")"));
]

(* --- composition: define + call + nested arithmetic --- *)
let composition_tests = [
  ("let-bind persists through lambda", `Quick, fun () ->
    Alcotest.(check string) "let persists"
      "25"
      (eval_str "(let ((square (lambda (x) (* x x)))) (square 5))"));
  ("let + arithmetic", `Quick, fun () ->
    Alcotest.(check string) "let nested" "30"
      (eval_str "(let ((x 5) (y 6)) (* x y))"));
  ("if true branch", `Quick, fun () ->
    Alcotest.(check string) "if true" "\"yes\""
      (eval_str "(if #t \"yes\" \"no\")"));
  ("if false via nil", `Quick, fun () ->
    Alcotest.(check string) "if nil" "\"no\""
      (eval_str "(if () \"yes\" \"no\")"));
  ("nested arithmetic inside lambda body", `Quick, fun () ->
    Alcotest.(check string) "nested"
      "12"
      (eval_str "((lambda (a b) (+ (* a 2) (* b 3))) 3 2)"));
]

(* === IT13 PROBE: reader parses JSON-form strings with escaped quotes ===
   This is a real test for a real bug found while building CRUD primitives:
   the reader's escape handler was reading the '\\' twice (peeks '\\',
   then advances which returns '\\', then matches '\\' as the escape
   letter — so `\"` added '\\' to buf and the following '"' closed the
   string). After the fix, `"\"name\":\"alice\""` should read back as
   the string `{"name":"alice"}`. *)
let json_string_probe () =
  let json_str = "{\"name\":\"alice\",\"age\":30}" in   (* the JSON data *)
  let lisp_src = "\"" ^ String.escaped json_str ^ "\"" in  (* lisp string literal *)
  let e = Pricklypear_lisp.Reader.read_one lisp_src in
  (match e with
   | Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VStr s) ->
       Alcotest.(check string)
         "reader parses JSON-form string (escapes work)"
         json_str s
   | _ ->
       Alcotest.failf "expected Lit VStr, got %s"
         (Pricklypear_lisp.Reader.pp_expr e))

let probe_tests = [
  ("reader parses JSON string with \\\" escapes", `Quick, json_string_probe);
]

(* Helper: read key back from JSON-string via Yojson. Used by the
   dict-set test to verify the new key round-trips through a parse. *)
let dict_get_back (json : string) (key : string) : string option =
  try
    (match Yojson.Safe.from_string json with
     | `Assoc kvs ->
         (match List.assoc_opt key kvs with
          | Some (`String s) -> Some s
          | Some (`Int n) -> Some (string_of_int n)
          | _ -> None)
     | _ -> None)
  with _ -> None

(* --- dict primitives (operate on VStr JSON; pure, no DB) --- *)
let dict_tests = [
  ("dict-get string value", `Quick, fun () ->
    Alcotest.(check string) "get name"
      "\"alice\"" (* pp_value of VStr adds outer quotes *)
      (eval_str "(dict-get \"{\\\"name\\\":\\\"alice\\\",\\\"age\\\":30}\" \"name\")"));
  ("dict-get int value stringified", `Quick, fun () ->
    Alcotest.(check string) "get age"
      "\"30\"" (* pp_value of VStr adds outer quotes *)
      (eval_str "(dict-get \"{\\\"name\\\":\\\"alice\\\",\\\"age\\\":30}\" \"age\")"));
  ("dict-get missing key is nil", `Quick, fun () ->
    Alcotest.(check string) "missing key"
      "()"
      (eval_str "(dict-get \"{\\\"a\\\":1}\" \"nope\")"));
  ("dict-set adds new key", `Quick, fun () ->
    (* The exact key order may vary; just check the chosen key is present and decodes back. *)
    let v = eval "(dict-set \"{\\\"a\\\":1}\" \"b\" \"2\")" in
    (match v with
     | VStr s ->
         let r = dict_get_back s "b" in
         Alcotest.(check (option string)) "b present" (Some "2") r
     | _ ->
         Alcotest.failf "expected VStr, got %s" (Pricklypear_lisp.Reader.pp_value v)));
  ("dict-keys lists 2 keys", `Quick, fun () ->
    let v = eval "(dict-keys \"{\\\"name\\\":\\\"alice\\\",\\\"age\\\":30}\")" in
    (match v with
     | VList [ VStr _; VStr _ ] -> ()
     | _ ->
         Alcotest.failf "expected list of 2 keys, got %s"
           (Pricklypear_lisp.Reader.pp_value v)));
]

(* --- render-html primitive (tagged VStr) --- *)
let render_html_tests = [
  ("render-html tags the string", `Quick, fun () ->
    let v = eval "(render-html \"<b>hi</b>\")" in
    Alcotest.(check bool) "is raw html struck"
      true
      (Pricklypear_image_lib.Primitives.is_raw_html v <> None));
  ("render-html raw content extractable", `Quick, fun () ->
    let v = eval "(render-html \"<h2>title</h2>\")" in
    Alcotest.(check (option string)) "content"
      (Some "<h2>title</h2>")
      (Pricklypear_image_lib.Primitives.is_raw_html v));
  ("plain VStr not raw html", `Quick, fun () ->
    Alcotest.(check (option string)) "not raw"
      None
      (Pricklypear_image_lib.Primitives.is_raw_html (VStr "plain")));
]

(* --- extract-links (OCaml wiki scanner) --- *)
let extract_links_tests = [
  ("three wiki links", `Quick, fun () ->
    let v =
      eval
        "(extract-links \"see [[a]] and [[b|Bee]] plus [[c]]\")"
    in
    match v with
    | VList
        [ VList [ VStr "a"; VStr "a" ]
        ; VList [ VStr "b"; VStr "Bee" ]
        ; VList [ VStr "c"; VStr "c" ] ] -> ()
    | other ->
        Alcotest.failf "links got %s" (Pricklypear_lisp.Reader.pp_value other));
  ("zero links", `Quick, fun () ->
    Alcotest.(check string) "empty" "()"
      (eval_str "(extract-links \"no links here\")"));
  ("unclosed bracket no hang", `Quick, fun () ->
    Alcotest.(check string) "unclosed" "()"
      (eval_str "(extract-links \"text [[oops\")"));
  ("empty brackets skipped", `Quick, fun () ->
    Alcotest.(check string) "empty pair" "()"
      (eval_str "(extract-links \"[[]]\")"));
  ("media syntax included", `Quick, fun () ->
    let v =
      eval
        "(extract-links \"[[media:uuid-1|alt text]]\")"
    in
    match v with
    | VList [ VList [ VStr "media:uuid-1"; VStr "alt text" ] ] -> ()
    | other ->
        Alcotest.failf "media got %s" (Pricklypear_lisp.Reader.pp_value other));
  ("budget-remaining positive mid-eval", `Quick, fun () ->
    let v = eval "(budget-remaining)" in
    match v with
    | VInt n when n > 0L -> ()
    | other ->
        Alcotest.failf "budget-remaining got %s"
          (Pricklypear_lisp.Reader.pp_value other));
  ("render-links wiki + media", `Quick, fun () ->
    let contains hay needle =
      let n = String.length needle in
      let rec loop i =
        if i + n > String.length hay then false
        else if String.sub hay i n = needle then true
        else loop (i + 1)
      in
      loop 0
    in
    let v =
      eval
        "(render-links \"hi [[a|A]] [[media:uuid-1|alt]]\")"
    in
    match v with
    | VStr s ->
        if not (contains s "/notes?slug=a") then
          Alcotest.failf "missing wiki href: %s" s;
        if not (contains s "note-media") then
          Alcotest.failf "missing media class: %s" s;
        if not (contains s ">A</a>") then
          Alcotest.failf "missing label: %s" s
    | other ->
        Alcotest.failf "render-links got %s"
          (Pricklypear_lisp.Reader.pp_value other));
  ("render-links bible sexp", `Quick, fun () ->
    let contains hay needle =
      let n = String.length needle in
      let rec loop i =
        if i + n > String.length hay then false
        else if String.sub hay i n = needle then true
        else loop (i + 1)
      in
      loop 0
    in
    let v =
      eval
        "(render-links \"see (bible John 3 16) and [[x]]\")"
    in
    match v with
    | VStr s ->
        if not (contains s "/dabar?book=John") then
          Alcotest.failf "missing dabar href: %s" s;
        if not (contains s "chapter=3") then
          Alcotest.failf "missing chapter: %s" s;
        if not (contains s "verse=16") then
          Alcotest.failf "missing verse: %s" s;
        if not (contains s "/notes?slug=x") then
          Alcotest.failf "wiki still works: %s" s
    | other ->
        Alcotest.failf "render-links bible got %s"
          (Pricklypear_lisp.Reader.pp_value other));
]

let equality_tests = [
  ("equal? lists same", `Quick, fun () ->
    Alcotest.(check string) "eq" "#t"
      (eval_str "(equal? (list 1 2 3) (list 1 2 3))"));
  ("equal? lists differ", `Quick, fun () ->
    Alcotest.(check string) "ne" "#f"
      (eval_str "(equal? (list 1 2) (list 1 2 3))"));
  ("equal? strings", `Quick, fun () ->
    Alcotest.(check string) "str eq" "#t"
      (eval_str "(equal? \"a\" \"a\")"));
  ("equal? int vs float distinct", `Quick, fun () ->
    Alcotest.(check string) "int!=float" "#f"
      (eval_str "(equal? 1 1.0)"));
  ("equal? cons-chain equals list", `Quick, fun () ->
    Alcotest.(check string) "cons==list" "#t"
      (eval_str "(equal? (list 1 2 3) (cons 1 (cons 2 (cons 3 ()))))"));
  ("equal? dotted pair not equal to proper list", `Quick, fun () ->
    Alcotest.(check string) "dotted!=proper" "#f"
      (eval_str "(equal? (cons 1 (cons 2 3)) (list 1 2 3))"));
  ("equal? symbols", `Quick, fun () ->
    Alcotest.(check string) "sym eq" "#t"
      (eval_str "(equal? 'a 'a)"));
  ("equal? nil and empty list", `Quick, fun () ->
    Alcotest.(check string) "nil==empty" "#t"
      (eval_str "(equal? () (list))"));
]

let cadr_tests = [
  ("cadr", `Quick, fun () ->
    Alcotest.(check string) "cadr" "2"
      (eval_str "(cadr (list 1 2 3))"));
  ("caddr", `Quick, fun () ->
    Alcotest.(check string) "caddr" "3"
      (eval_str "(caddr (list 1 2 3))"));
  ("caar", `Quick, fun () ->
    Alcotest.(check string) "caar" "1"
      (eval_str "(caar (list (list 1 2) 3))"));
  ("cddr", `Quick, fun () ->
    Alcotest.(check string) "cddr" "(3)"
      (eval_str "(cddr (list 1 2 3))"));
]

let assoc_member_tests = [
  ("assoc hit", `Quick, fun () ->
    Alcotest.(check string) "assoc hit" "(b 2)"
      (eval_str "(assoc 'b (list (list 'a 1) (list 'b 2)))"));
  ("assoc miss is nil", `Quick, fun () ->
    Alcotest.(check string) "assoc miss" "()"
      (eval_str "(assoc 'z (list (list 'a 1)))"));
  ("assoc on pair entries", `Quick, fun () ->
    Alcotest.(check string) "assoc pair" "(c . 3)"
      (eval_str "(assoc 'c (list (cons 'a 1) (cons 'c 3)))"));
  ("member hit returns tail", `Quick, fun () ->
    Alcotest.(check string) "member hit" "(2 3)"
      (eval_str "(member 2 (list 1 2 3))"));
  ("member miss is #f", `Quick, fun () ->
    Alcotest.(check string) "member miss" "#f"
      (eval_str "(member 9 (list 1 2 3))"));
]

let apply_tests = [
  ("apply prim with list", `Quick, fun () ->
    Alcotest.(check string) "apply +" "3"
      (eval_str "(apply + (list 1 2))"));
  ("apply prim with quoted list", `Quick, fun () ->
    Alcotest.(check string) "apply + quote" "10"
      (eval_str "(apply + '(2 8))"));
  ("apply lambda", `Quick, fun () ->
    Alcotest.(check string) "apply lambda" "12"
      (eval_str "(apply (lambda (a b) (* a b)) (list 3 4))"));
]

let std_freeze_tests = [
  ("length on list", `Quick, fun () ->
    Alcotest.(check string) "length" "3"
      (eval_str "(length (list 1 2 3))"));
  ("length on nil", `Quick, fun () ->
    Alcotest.(check string) "length nil" "0"
      (eval_str "(length ())"));
  ("length on cons chain", `Quick, fun () ->
    Alcotest.(check string) "length cons" "3"
      (eval_str "(length (cons 1 (cons 2 (cons 3 ()))))"));
  ("count still works", `Quick, fun () ->
    Alcotest.(check string) "count" "2"
      (eval_str "(count (list 1 2))"));
  ("list/foldr right fold", `Quick, fun () ->
    (* foldr cons 0 (1 2 3) = (1 (2 (3 0))) — nested cons/list *)
    Alcotest.(check string) "foldr nest" "(1 (2 (3 0)))"
      (eval_str "(list/foldr (lambda (x acc) (cons x (list acc))) 0 (list 1 2 3))"));
  ("list/foldr sum right", `Quick, fun () ->
    (* foldr + 0 (1 2 3 4) = 10 *)
    Alcotest.(check string) "foldr sum" "10"
      (eval_str "(list/foldr + 0 (list 1 2 3 4))"));
  ("list/foldr on empty is init", `Quick, fun () ->
    Alcotest.(check string) "foldr empty" "42"
      (eval_str "(list/foldr + 42 (list))"));
  ("list/foldl still works", `Quick, fun () ->
    Alcotest.(check string) "foldl sum" "10"
      (eval_str "(list/foldl + 0 (list 1 2 3 4))"));
]

(* --- kernel I/O primitives (from notes inbox kernel-io-primitives) --- *)
(* pp_value wraps VStr in outer quotes; q builds the expected form so we
   avoid nested-quote escaping pain in the test source. *)
let q s = "\"" ^ s ^ "\""
let io_prim_tests = [
  ("string-replace", `Quick, fun () ->
    Alcotest.(check string) "replace" (q "xxx")
      (eval_str "(string-replace \"axa\" \"a\" \"x\")"));
  ("string-replace empty pat no-op", `Quick, fun () ->
    Alcotest.(check string) "empty pat" (q "abc")
      (eval_str "(string-replace \"abc\" \"\" \"X\")"));
  ("string-downcase", `Quick, fun () ->
    Alcotest.(check string) "down" (q "abc")
      (eval_str "(string-downcase \"ABC\")"));
  ("string-upcase", `Quick, fun () ->
    Alcotest.(check string) "up" (q "ABC")
      (eval_str "(string-upcase \"abc\")"));
  ("string-starts-with?", `Quick, fun () ->
    Alcotest.(check string) "starts" "#t"
      (eval_str "(string-starts-with? \"hello world\" \"hello\")"));
  ("string-ends-with?", `Quick, fun () ->
    Alcotest.(check string) "ends" "#t"
      (eval_str "(string-ends-with? \"hello.txt\" \".txt\")"));
  ("url-encode", `Quick, fun () ->
    Alcotest.(check string) "enc" (q "a%20b%2Cc")
      (eval_str "(url-encode \"a b,c\")"));
  ("url-decode", `Quick, fun () ->
    Alcotest.(check string) "dec" (q "a b,c")
      (eval_str "(url-decode \"a%20b%2Cc\")"));
  ("url roundtrip", `Quick, fun () ->
    Alcotest.(check string) "roundtrip" (q "hello & world?")
      (eval_str "(url-decode (url-encode \"hello & world?\"))"));
  ("sha1", `Quick, fun () ->
    Alcotest.(check string) "sha1 abc"
      (q "a9993e364706816aba3e25717850c26c9cd0d89d")
      (eval_str "(sha1 \"abc\")"));
  ("html-decode named", `Quick, fun () ->
    Alcotest.(check string) "decode named" (q "Hi & bye")
      (eval_str "(html-decode \"Hi &amp; bye\")"));
  ("html-decode numeric", `Quick, fun () ->
    let v = eval "(html-decode \"&#169;\")" in
    (match v with
     | Pricklypear_lisp.Ast.VStr s when String.length s = 2
          && Char.code s.[0] = 0xC2 && Char.code s.[1] = 0xA9 -> ()
     | other ->
         Alcotest.failf "decode num got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("html-decode leaves unknown", `Quick, fun () ->
    Alcotest.(check string) "decode unknown" (q "&nope;")
      (eval_str "(html-decode \"&nope;\")"));
  ("parse-date ISO", `Quick, fun () ->
    let v = eval "(parse-date \"2026-07-18T07:57:43Z\")" in
    (match v with
     | VInt n when n > 0L -> ()
     | other ->
         Alcotest.failf "parse-date ISO got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("parse-date RFC-822", `Quick, fun () ->
    let v = eval "(parse-date \"Tue, 28 Jul 2026 02:56:21 GMT\")" in
    (match v with
     | VInt n when n > 0L -> ()
     | other ->
         Alcotest.failf "parse-date RFC822 got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("parse-date bare", `Quick, fun () ->
    let v = eval "(parse-date \"2026-07-18\")" in
    (match v with
     | VInt n when n > 0L -> ()
     | other ->
         Alcotest.failf "parse-date bare got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("parse-date garbage -> 0", `Quick, fun () ->
    Alcotest.(check string) "garbage" "0"
      (eval_str "(parse-date \"not a date\")"));
  ("xml-parse simple", `Quick, fun () ->
    let v = eval "(xml-parse \"<rss><channel><item><title>Hi &amp; bye</title></item></channel></rss>\")" in
    (match v with
     | VList [ VStr "rss"; VList []; body ] ->
         (match body with
          | VList [ VStr "channel"; _; item ] ->
              (match item with
               | VList [ VStr "item"; _; title ] ->
                   (match title with
                    | VList [ VStr "title"; _; VStr "Hi & bye" ] -> ()
                    | _ -> Alcotest.fail "title body")
               | _ -> Alcotest.fail "item")
          | _ -> Alcotest.fail "channel")
     | other ->
         Alcotest.failf "xml got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("xml-parse attrs", `Quick, fun () ->
    let v = eval "(xml-parse \"<a href=\\\"x\\\">t</a>\")" in
    (match v with
     | VList [ VStr "a"; VList [ VPair (VStr "href", VStr "x") ]; VStr "t" ] -> ()
     | other ->
         Alcotest.failf "xml attrs got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("now returns positive epoch", `Quick, fun () ->
    let v = eval "(now)" in
    (match v with
     | VInt n when n > 1_700_000_000L -> ()
     | other ->
         Alcotest.failf "now got %s" (Pricklypear_lisp.Reader.pp_value other)));
  ("now-ms larger than now", `Quick, fun () ->
    let a = eval "(now)" in
    let b = eval "(now-ms)" in
    (match a, b with
     | VInt s, VInt ms when ms >= Int64.mul s 1000L -> ()
     | other ->
         Alcotest.failf "now-ms relation bad"));
  ("xml-parse error on garbage", `Quick, fun () ->

    (* Lenient parser may recover (VStr text) or return a tree/error VList;
       just check it returns something without raising. *)
    let _v = eval "(xml-parse \"<<<\")" in
    ());
]

let multipart_tests = [
  ("part builds tagged list", `Quick, fun () ->
    let v =
      try eval "(part \"<b>hi</b>\" :target \"#t\" :swap \"beforeend\" :id \"1\")"
      with Pricklypear_lisp.Ast.Interp_error e ->
        Alcotest.failf "interp: %s" (Pricklypear_lisp.Ast.pp_interp_error e)
         | exn -> Alcotest.failf "exn: %s" (Printexc.to_string exn)
    in
    match Pricklypear_image_lib.Primitives.is_part v with
    | Some p ->
        Alcotest.(check string) "body" "<b>hi</b>" p.body;
        Alcotest.(check (option string)) "target" (Some "#t") p.target;
        Alcotest.(check (option string)) "swap" (Some "beforeend") p.swap;
        Alcotest.(check (option string)) "id" (Some "1") p.id
    | None -> Alcotest.failf "not a part: %s" (Pricklypear_lisp.Reader.pp_value v));
  ("multipart bundles parts", `Quick, fun () ->
    let v =
      eval
        "(multipart :target \"#feed\" :swap \"beforeend\"
            (part \"<b>one</b>\") (part \"<i>two</i>\" :id \"2\"))"
    in
    match Pricklypear_image_lib.Primitives.is_multipart v with
    | Some mp ->
        Alcotest.(check (option string)) "env target" (Some "#feed")
          mp.envelope_target;
        Alcotest.(check int) "n parts" 2 (List.length mp.parts);
        (match mp.parts with
         | [a; b] ->
             Alcotest.(check string) "a" "<b>one</b>" a.body;
             Alcotest.(check string) "b" "<i>two</i>" b.body;
             Alcotest.(check (option string)) "b id" (Some "2") b.id
         | _ -> Alcotest.fail "part count")
    | None -> Alcotest.failf "not multipart: %s" (Pricklypear_lisp.Reader.pp_value v));
  ("json-part content-type", `Quick, fun () ->
    let v = eval "(json-part \"{\\\"ok\\\":true}\" :id \"j\")" in
    match Pricklypear_image_lib.Primitives.is_part v with
    | Some p ->
        Alcotest.(check bool) "json ct" true
          (String.starts_with ~prefix:"application/json" p.content_type);
        Alcotest.(check (option string)) "id" (Some "j") p.id
    | None -> Alcotest.fail "not part");
  ("keyword :target self-evaluates in image", `Quick, fun () ->
    Alcotest.(check string) "kw" ":target" (eval_str ":target"));
  ("multipart accepts list of parts", `Quick, fun () ->
    let v =
      eval
        "(multipart :target \"#t\" (list (part \"a\") (part \"b\")))"
    in
    match Pricklypear_image_lib.Primitives.is_multipart v with
    | Some mp -> Alcotest.(check int) "2" 2 (List.length mp.parts)
    | None -> Alcotest.fail "not multipart");
]

let () =
  Alcotest.run "pricklypear-image-primitives" [
    "arithmetic", arithmetic_tests;
    "math", math_tests;
    "list", list_tests;
    "string", string_tests;
    "probe", probe_tests;
    "dict", dict_tests;
    "render-html", render_html_tests;
    "composition", composition_tests;
    "extract-links", extract_links_tests;
    "equality", equality_tests;
    "cadr", cadr_tests;
    "assoc-member", assoc_member_tests;
    "apply", apply_tests;
    "std-freeze", std_freeze_tests;
    "io-prims", io_prim_tests;
    "multipart", multipart_tests;
  ]
