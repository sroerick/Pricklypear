(** Public-route gate hardening: a sibling define whose macroexpansion
    raises must be SKIPPED by [env_lambda_bindings], not 500 every gated
    public route.

    2026-09-13 prod outage: a DB-only define used the 3-arg
    (let* binds body extra) form while std's let* macro is fixed-arity 2;
    expand_macro_once raised Interp_error mid-fold and check_public_handler
    denied every gated public route with "failed the effect walk: macro
    let* arity 2 vs 3" — /welcome et al 500'd for ~13h.

    Regression contract (mirrors the force-guard in the same function):
    - the broken sibling is skipped (not in known_fn),
    - an unrelated handler publishes clean,
    - a handler that actually applies the broken sibling DENIES
      (unresolved-name; fail-closed),
    - a handler whose OWN body macroexpand fails denies (strict path). *)

open Pricklypear_lisp
open Ast

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec find i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else find (i + 1)
  in
  m = 0 || find 0

(* Session-like env: the shared single-body let* macro plus sibling
   defines, one of them malformed for that macro (3-arg let* call). *)
let setup_env () =
  let env = Env.create () in
  let eval_one s = ignore (Eval.eval_string ~prims:Eval.empty_prims env s) in
  eval_one
    "(defmacro let* (bindings body) \
     (if (null? bindings) body \
     (quasiquote (let ((unquote (car bindings))) \
     (let* (unquote (cdr bindings)) (unquote body))))))";
  eval_one "(define (good-helper) (* 6 7))";
  eval_one "(define (broken-sibling) (let* ((x 1)) x (redirect \"/y\")))";
  eval_one "(define (welcome) (good-helper))";
  eval_one "(define (uses-broken) (broken-sibling))";
  eval_one "(define (self-broken) (let* ((x 1)) x (redirect \"/y\")))";
  env

let checks =
  [ ( "sibling macroexpand failure is skipped, not fatal",
      `Quick,
      fun () ->
        let env = setup_env () in
        let bindings = Route_gate.env_lambda_bindings env in
        Alcotest.(check bool) "good sibling present"
          true
          (List.mem_assoc "good-helper" bindings);
        Alcotest.(check bool) "broken sibling skipped"
          false
          (List.mem_assoc "broken-sibling" bindings) );
    ( "unrelated handler publishes despite broken sibling",
      `Quick,
      fun () ->
        let env = setup_env () in
        match Route_gate.check_public_handler ~env "welcome" with
        | Ok () -> ()
        | Error m -> Alcotest.fail m );
    ( "handler applying the broken sibling is denied",
      `Quick,
      fun () ->
        let env = setup_env () in
        match Route_gate.check_public_handler ~env "uses-broken" with
        | Ok () -> Alcotest.fail "applying a skipped sibling must deny"
        | Error _ -> () );
    ( "handler's own bad macro call denies without crashing",
      `Quick,
      fun () ->
        let env = setup_env () in
        match Route_gate.check_public_handler ~env "self-broken" with
        | Ok () -> Alcotest.fail "own-body macro failure must deny"
        | Error m ->
            Alcotest.(check bool) "walk denial" true
              (contains ~sub:"failed the effect walk" m) );
    ( "handler_body extracts a VLambda body",
      `Quick,
      fun () ->
        let env = setup_env () in
        match Route_gate.handler_body env "welcome" with
        | Some _ -> ()
        | None -> Alcotest.fail "expected a body for a defined lambda" );
    ( "handler_body is None for an unbound name",
      `Quick,
      fun () ->
        let env = setup_env () in
        Alcotest.(check bool) "unbound -> None" true
          (Route_gate.handler_body env "missing/mystery" = None) );
    ( "route classifier sees the same body the gate reads",
      `Quick,
      fun () ->
        let env = setup_env () in
        (match Route_gate.handler_body env "broken-sibling" with
         | Some body ->
             Alcotest.(check string) "redirect-bearing sibling is html-ish"
               "fragment"
               (Effect.route_shape_string (Effect.classify_route_shape body))
         | None -> ());
        let env2 = Env.create () in
        let ev s = ignore (Eval.eval_string ~prims:Eval.empty_prims env2 s) in
        ev "(define (page) (layout/page (list 1)))";
        (match Route_gate.handler_body env2 "page" with
         | Some body ->
             Alcotest.(check string) "layout/page body is a shell" "shell"
               (Effect.route_shape_string
                  (Effect.classify_route_shape body))
         | None -> Alcotest.fail "page handler should have a body" ));
  ]

let () = Alcotest.run "route_gate" [ "sibling-hardening", checks ]
