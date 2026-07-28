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
  (env, prim_table)

(* Helper: eval a Lisp form string against the test env under the Lwt rim. *)
let eval s =
  let (env, prims) = make_test_env () in
  Lwt_main.run
    (Pricklypear_image_lib.Control_rim.run_lwt ~prims (fun () ->
        Pricklypear_lisp.Eval.eval_string_direct
          ~prims:Pricklypear_lisp.Eval.empty_prims env s))

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

let () =
  Alcotest.run "pricklypear-image-primitives" [
    "arithmetic", arithmetic_tests;
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
  ]
