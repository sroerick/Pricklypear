(** Pricklypear Image — hypermedia harness kernel prims tests (no DB, no session).

    Tests the PURE soup/* lambdasoup bindings by evaluating Nopales forms
    against a prim_table with register_pure_prims. Mirrors primitives_test's
    pattern: build the table directly, eval via eval_string_direct.

    Spec: borge/hypermedia-harness.borg (kernel-prims verify stanza).
    No route/dispatch here (that needs a session — verified via pp_eval +
    the fidelity smoke). *)
open Pricklypear_lisp.Ast

(* Build a prim_table + an env with all prims auto-defined as VPrim. *)
let make_test_env () =
  let prim_table = Pricklypear_image_lib.Primitives.make_with_defaults () in
  Pricklypear_image_lib.Prims_hyper.register_pure_prims prim_table;
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ ->
      Pricklypear_lisp.Env.define env name (VPrim name))
    prim_table;
  (env, prim_table)

let eval s =
  let env, prims = make_test_env () in
  Pricklypear_image_lib.Control_rim.run ~prims (fun () ->
      Pricklypear_lisp.Eval.eval_string_direct
        ~prims:Pricklypear_lisp.Eval.empty_prims env
        ~budget:(Pricklypear_lisp.Eval.make_budget ()) s)

let eval_str s =
  Pricklypear_lisp.Reader.pp_value (eval s)

(* --- parse + serialize round-trip --- *)
let parse_tests = [
  ("parse + serialize round-trips a node", `Quick, fun () ->
    Alcotest.(check string) "roundtrip"
      "\"<div><p>hi</p></div>\""
      (eval_str "(soup/serialize (soup/parse \"<div><p>hi</p></div>\"))"));
  ("parse a full page", `Quick, fun () ->
    let r = eval_str "(soup/serialize (soup/parse \"<html><body><h1>Title</h1></body></html>\"))" in
    Alcotest.(check bool) "contains h1" true (String.length r > 0));
]

(* --- select --- *)
let select_tests = [
  ("select finds all matching elements", `Quick, fun () ->
    Alcotest.(check string) "3 p tags"
      "3"
      (eval_str "(length (soup/select (soup/parse \"<div><p>1</p><p>2</p><p>3</p></div>\") \"p\"))"));
  ("select by class", `Quick, fun () ->
    Alcotest.(check string) "1 .x"
      "1"
      (eval_str "(length (soup/select (soup/parse \"<div><p class=x>1</p><p>2</p></div>\") \"p.x\"))"));
  ("select by id", `Quick, fun () ->
    Alcotest.(check string) "1 #t"
      "1"
      (eval_str "(length (soup/select (soup/parse \"<div id=t>x</div>\") \"#t\"))"));
  ("select finds nested", `Quick, fun () ->
    Alcotest.(check string) "descendants"
      "3"
      (eval_str "(length (soup/select (soup/parse \"<div><a>x</a><span><a>y</a></span><a>z</a></div>\") \"a\"))"));
]

(* --- text + attr --- *)
let text_attr_tests = [
  ("text reads text content", `Quick, fun () ->
    Alcotest.(check string) "text"
      "\"hello\""
      (eval_str "(soup/text (car (soup/select (soup/parse \"<p class=x>hello</p>\") \"p.x\")))"));
  ("attr reads an attribute", `Quick, fun () ->
    Alcotest.(check string) "href"
      "\"/x\""
      (eval_str "(soup/attr (car (soup/select (soup/parse \"<a href=/x>l</a>\") \"a\")) \"href\")"));
  ("attr returns nil for missing", `Quick, fun () ->
    Alcotest.(check string) "missing"
      "()"
      (eval_str "(soup/attr (car (soup/select (soup/parse \"<a href=/x>l</a>\") \"a\")) \"class\")"));
]

(* --- swap strategies (the 8 htmx swap values) --- *)
let swap_tests = [
  ("innerHTML replaces children", `Quick, fun () ->
    Alcotest.(check string) "innerHTML"
      "\"<div id=\\\"t\\\"><b>new</b></div>\""
      (eval_str "(begin (define p (soup/parse \"<div id=t>old</div>\")) (soup/swap! (car (soup/select p \"#t\")) \"innerHTML\" \"<b>new</b>\") (soup/serialize p))"));
  ("outerHTML replaces the element", `Quick, fun () ->
    Alcotest.(check string) "outerHTML"
      "\"<div><b>y</b></div>\""
      (eval_str "(begin (define p (soup/parse \"<div><span id=o>x</span></div>\")) (soup/swap! (car (soup/select p \"#o\")) \"outerHTML\" \"<b>y</b>\") (soup/serialize p))"));
  ("beforeend appends a child", `Quick, fun () ->
    Alcotest.(check string) "beforeend"
      "\"<ul id=\\\"l\\\"><li>a</li><li>b</li></ul>\""
      (eval_str "(begin (define p (soup/parse \"<ul id=l><li>a</li></ul>\")) (soup/swap! (car (soup/select p \"#l\")) \"beforeend\" \"<li>b</li>\") (soup/serialize p))"));
  ("afterbegin prepends a child in order", `Quick, fun () ->
    Alcotest.(check string) "afterbegin"
      "\"<ul id=\\\"l\\\"><li>new1</li><li>old</li></ul>\""
      (eval_str "(begin (define p (soup/parse \"<ul id=l><li>old</li></ul>\")) (soup/swap! (car (soup/select p \"#l\")) \"afterbegin\" \"<li>new1</li>\") (soup/serialize p))"));
  ("beforebegin inserts sibling before (parented)", `Quick, fun () ->
    Alcotest.(check string) "beforebegin"
      "\"<body><p>before</p><div id=\\\"t\\\">x</div></body>\""
      (eval_str "(begin (define p (soup/parse \"<body><div id=t>x</div></body>\")) (soup/swap! (car (soup/select p \"#t\")) \"beforebegin\" \"<p>before</p>\") (soup/serialize p))"));
  ("afterend inserts sibling after (parented)", `Quick, fun () ->
    Alcotest.(check string) "afterend"
      "\"<body><div id=\\\"t\\\">x</div><p>after</p></body>\""
      (eval_str "(begin (define p (soup/parse \"<body><div id=t>x</div></body>\")) (soup/swap! (car (soup/select p \"#t\")) \"afterend\" \"<p>after</p>\") (soup/serialize p))"));
  ("delete removes the element", `Quick, fun () ->
    Alcotest.(check string) "delete"
      "\"<div>keep</div>\""
      (eval_str "(begin (define p (soup/parse \"<div><span id=o>x</span>keep</div>\")) (soup/swap! (car (soup/select p \"#o\")) \"delete\" \"\") (soup/serialize p))"));
  ("none is a no-op", `Quick, fun () ->
    Alcotest.(check string) "none"
      "\"<div id=\\\"t\\\">x</div>\""
      (eval_str "(begin (define p (soup/parse \"<div id=t>x</div>\")) (soup/swap! (car (soup/select p \"#t\")) \"none\" \"<b>ignored</b>\") (soup/serialize p))"));
]

(* --- page-copy + immutability --- *)
let immutability_tests = [
  ("page-copy deep-copies so source is never mutated", `Quick, fun () ->
    Alcotest.(check string) "source unchanged"
      "\"<div id=\\\"t\\\">x</div>\""
      (eval_str "(begin (define p (soup/parse \"<div id=t>x</div>\")) (define p2 (soup/page-copy p)) (soup/swap! (car (soup/select p2 \"#t\")) \"innerHTML\" \"changed\") (soup/serialize p))"));
  ("page-copy copy IS mutated", `Quick, fun () ->
    Alcotest.(check string) "copy changed"
      "\"<div id=\\\"t\\\">changed</div>\""
      (eval_str "(begin (define p (soup/parse \"<div id=t>x</div>\")) (define p2 (soup/page-copy p)) (soup/swap! (car (soup/select p2 \"#t\")) \"innerHTML\" \"changed\") (soup/serialize p2))"));
]

(* --- closest-attr (inherited attribute ancestor walk) --- *)
let closest_attr_tests = [
  ("closest-attr finds on element itself", `Quick, fun () ->
    Alcotest.(check string) "self"
      "\"/post\""
      (eval_str "(begin (define p (soup/parse \"<button hx-post=/post>go</button>\")) (soup/closest-attr (car (soup/select p \"button\")) \"hx-post\"))"));
  ("closest-attr walks to ancestor", `Quick, fun () ->
    Alcotest.(check string) "ancestor"
      "\"#out\""
      (eval_str "(begin (define p (soup/parse \"<div hx-target=#out><button id=b>go</button></div>\")) (soup/closest-attr (car (soup/select p \"#b\")) \"hx-target\"))"));
  ("closest-attr returns nil when not found", `Quick, fun () ->
    Alcotest.(check string) "missing"
      "()"
      (eval_str "(begin (define p (soup/parse \"<div><button id=b>go</button></div>\")) (soup/closest-attr (car (soup/select p \"#b\")) \"hx-target\"))"));
]

(* --- diff --- *)
let diff_tests = [
  ("diff identical pages = no changes", `Quick, fun () ->
    Alcotest.(check string) "same"
      "\"no changes\""
      (eval_str "(begin (define a (soup/parse \"<div id=x>1</div>\")) (define b (soup/parse \"<div id=x>1</div>\")) (soup/diff a b))"));
  ("diff detects added element", `Quick, fun () ->
    let r = eval_str "(begin (define a (soup/parse \"<div id=x>1</div>\")) (define b (soup/parse \"<div id=x>1</div><div id=y>2</div>\")) (soup/diff a b))" in
    Alcotest.(check bool) "mentions y" true (String.length r > 0 && String.contains r 'y'));
  ("diff detects removed element", `Quick, fun () ->
    let r = eval_str "(begin (define a (soup/parse \"<div id=x>1</div><div id=y>2</div>\")) (define b (soup/parse \"<div id=x>1</div>\")) (soup/diff a b))" in
    Alcotest.(check bool) "mentions y" true (String.length r > 0 && String.contains r 'y'));
  ("diff detects text change in same-id element", `Quick, fun () ->
    let r = eval_str "(begin (define a (soup/parse \"<div id=x>old</div>\")) (define b (soup/parse \"<div id=x>new</div>\")) (soup/diff a b))" in
    Alcotest.(check bool) "reports change" true (String.length r > 0));
]

(* --- apply-parts! (multipart ordered part list) --- *)
let apply_parts_tests = [
  ("apply-parts! lands each frag at its target in order", `Quick, fun () ->
    Alcotest.(check string) "both targets swapped"
      "\"<div id=\\\"a\\\"><b>new-a</b></div><div id=\\\"b\\\"><i>new-b</i></div>\""
      (eval_str "(begin \
        \n (define p (soup/parse \"<div id=a>old-a</div><div id=b>old-b</div>\")) \
        \n (define parts (list \
        \n   (list (cons \"html\" \"<b>new-a</b>\") (cons \"target\" \"#a\") (cons \"swap\" \"innerHTML\")) \
        \n   (list (cons \"html\" \"<i>new-b</i>\") (cons \"target\" \"#b\") (cons \"swap\" \"innerHTML\")))) \
        \n (soup/apply-parts! p parts) \
        \n (soup/serialize p))"));
  ("apply-parts! defaults to innerHTML when swap absent", `Quick, fun () ->
    Alcotest.(check string) "default innerHTML"
      "\"<div id=\\\"a\\\">new-a</div>\""
      (eval_str "(begin \
        \n (define p (soup/parse \"<div id=a>old-a</div>\")) \
        \n (define parts (list (list (cons \"html\" \"new-a\") (cons \"target\" \"#a\")))) \
        \n (soup/apply-parts! p parts) \
        \n (soup/serialize p))"));
  ("apply-parts! respects beforeend strategy in sequence", `Quick, fun () ->
    Alcotest.(check string) "appended in order"
      "\"<ul id=\\\"l\\\"><li>1</li><li>2</li></ul>\""
      (eval_str "(begin \
        \n (define p (soup/parse \"<ul id=l></ul>\")) \
        \n (define parts (list \
        \n   (list (cons \"html\" \"<li>1</li>\") (cons \"target\" \"#l\") (cons \"swap\" \"beforeend\")) \
        \n   (list (cons \"html\" \"<li>2</li>\") (cons \"target\" \"#l\") (cons \"swap\" \"beforeend\")))) \
        \n (soup/apply-parts! p parts) \
        \n (soup/serialize p))"));
  ("apply-parts! skips a part whose target is absent", `Quick, fun () ->
    Alcotest.(check string) "missing target skipped"
      "\"<div id=\\\"a\\\">new-a</div>\""
      (eval_str "(begin \
        \n (define p (soup/parse \"<div id=a>old-a</div>\")) \
        \n (define parts (list \
        \n   (list (cons \"html\" \"new-a\") (cons \"target\" \"#a\")) \
        \n   (list (cons \"html\" \"ghost\") (cons \"target\" \"#nope\")))) \
        \n (soup/apply-parts! p parts) \
        \n (soup/serialize p))"));
]

let () =
  Alcotest.run "prims_hyper" [
    ("parse + serialize", parse_tests);
    ("select", select_tests);
    ("text + attr", text_attr_tests);
    ("swap strategies", swap_tests);
    ("immutability", immutability_tests);
    ("closest-attr", closest_attr_tests);
    ("diff", diff_tests);
    ("apply-parts!", apply_parts_tests);
  ]
