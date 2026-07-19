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

let () =
  Alcotest.run "pricklypear-image-primitives" [
    "arithmetic", arithmetic_tests;
    "list", list_tests;
    "string", string_tests;
    "probe", probe_tests;
    "dict", dict_tests;
    "render-html", render_html_tests;
    "composition", composition_tests;
  ]
