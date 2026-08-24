(** Tests for remote/lib/free_vars.ml — free-variable analysis over a
    Nopales AST. Pure (no I/O, no env); mirrors the worker's purity.

    The reader produces special forms as [App(Var "lambda", …)] (eval
    desugars later), so the walker must dispatch on those App forms —
    binding lambda params / let names so they aren't mistaken for deps,
    treating quote bodies as data, and skipping kernel special-form
    names. These tests pin that behavior against the reader's actual
    output (not hand-built AST). *)

open Pricklypear_lisp

(** Free vars of a source string, sorted for order-independent compares. *)
let fv (src : string) : string list =
  List.sort compare (Free_vars.free_vars_of_source src)

(** All distinct free names in [src], as a sorted list. *)
let check label expected src =
  Alcotest.(check (list string)) label expected (fv src)

let () =
  Alcotest.run "free_vars"
  [ ( "basics"
    , [ Alcotest.test_case "plain var" `Quick (fun () ->
          check "plain var" [ "x" ] "x")
      ; Alcotest.test_case "lambda binds params" `Quick (fun () ->
          (* (lambda (a b) a) — a,b bound; no free vars *)
          check "lambda binds params" [] "(lambda (a b) a)")
      ; Alcotest.test_case "lambda with free var" `Quick (fun () ->
          (* (lambda (a) b) — a bound, b free *)
          check "lambda free b" [ "b" ] "(lambda (a) b)")
      ; Alcotest.test_case "nested call" `Quick (fun () ->
          (* (foo (bar x)) — foo, bar, x all free *)
          check "nested call" [ "bar"; "foo"; "x" ] "(foo (bar x))")
      ; Alcotest.test_case "pure prim shows as dep (conservative)" `Quick (fun () ->
          (* (+ 1 2) — + is a PRIM, not a kernel special form, so it
             shows as free. The puller asks function-source, gets nil,
             skips. This is the intended conservative behavior. *)
          check "+ is free" [ "+" ] "(+ 1 2)")
      ; Alcotest.test_case "stdlib call is a dep" `Quick (fun () ->
          (* (cons 1 xs) — cons is a local pure prim (not kernel special),
             but xs is free. cons IS registered as a pure prim on the
             client, but free_vars only skips kernel special-form names,
             not pure prims — so cons shows as free. That's fine: the
             puller will ask function-source for "cons", get nil (it's a
             prim), and skip it. Pin the actual behavior. *)
          check "cons + xs" [ "cons"; "xs" ] "(cons 1 xs)")
      ] )
  ; ( "special-forms"
    , [ Alcotest.test_case "if branches" `Quick (fun () ->
          (* (if t a b) — t, a, b free; if is kernel *)
          check "if" [ "a"; "b"; "t" ] "(if t a b)")
      ; Alcotest.test_case "let binds" `Quick (fun () ->
          (* (let ((a 1) (b 2)) (+ a b)) — a,b bound; + is a prim (not a
             kernel special form) so it shows free; puller skips. *)
          check "let binds" [ "+" ] "(let ((a 1) (b 2)) (+ a b))")
      ; Alcotest.test_case "let val free" `Quick (fun () ->
          (* (let ((a x)) a) — a bound, x free in the val *)
          check "let val free" [ "x" ] "(let ((a x)) a)")
      ; Alcotest.test_case "define binds name + params" `Quick (fun () ->
          (* (define (f x) (g x)) — f, x bound; g free *)
          check "define binds" [ "g" ] "(define (f x) (g x))")
      ; Alcotest.test_case "define bare lambda" `Quick (fun () ->
          (* (define f (lambda (x) (g x))) — f bound, x bound, g free *)
          check "define bare lambda" [ "g" ] "(define f (lambda (x) (g x)))")
      ; Alcotest.test_case "quote is data" `Quick (fun () ->
          (* '(a b c) — quoted, not code; no free vars *)
          check "quote data" [] "'(a b c)")
      ; Alcotest.test_case "quasiquote is data" `Quick (fun () ->
          (* `(a b) — quasiquoted; no free vars (unquote handled too) *)
          check "quasiquote data" [] "`(a b)")
      ; Alcotest.test_case "seq/cond" `Quick (fun () ->
          (* (seq (f) (g)) — f, g free; seq kernel *)
          check "seq" [ "f"; "g" ] "(seq (f) (g))")
      ; Alcotest.test_case "cond branches" `Quick (fun () ->
          (* (cond (a b) (else c)) — cond kernel; a,b,c free; `else` is
             a cond keyword but not a kernel special-form name, so it
             shows as free (conservative — puller gets nil, skips). *)
          check "cond" [ "a"; "b"; "c"; "else" ] "(cond (a b) (else c))")
      ; Alcotest.test_case "lambda no spurious 'lambda' dep" `Quick (fun () ->
          (* The bug that started these tests: the reader produces
             App(Var "lambda", …), so a naive walker treated "lambda"
             as a free var. Pin that it does NOT. *)
          check "no lambda dep" [ "g" ] "(lambda (x) (g x))")
      ; Alcotest.test_case "and/or" `Quick (fun () ->
          check "and/or" [ "a"; "b" ] "(and a b)")
      ] )
  ; ( "transitive"
    , [ Alcotest.test_case "3-deep chain" `Quick (fun () ->
          (* (define (top n) (- (mid n) 1)) — top,n bound; mid free; - is
             a prim (shows free, puller skips). *)
          check "top" [ "-"; "mid" ] "(define (top n) (- (mid n) 1))")
      ; Alcotest.test_case "higher-order list ops" `Quick (fun () ->
          (* (list/map f xs) — list/map kernel; f, xs free *)
          check "list/map" [ "f"; "xs" ] "(list/map f xs)")
      ; Alcotest.test_case "with-transaction body" `Quick (fun () ->
          (* (with-transaction (lambda () (create 'note 'n1))) —
             with-transaction kernel; create free (it's a proxy prim,
             shows as a dep — the puller will ask function-source, get
             nil, skip). Pin actual behavior. *)
          check "with-tx" [ "create" ]
            "(with-transaction (lambda () (create 'note 'n1)))")
      ] )
  ]
