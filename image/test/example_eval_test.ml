(* Pricklypear Image — example-eval restriction tests (no DB).

   habitat-book.eval-examples: (example-eval src) parses + evals
   Nopales source in a RESTRICTED environment. These tests pin the
   restriction surface against a synthetic prim table + env built the
   same way primitives_test builds them (no Session/PG):

     - whitelist surface: pure + read-only DB + supplement present;
       mutation families and escape hatches ABSENT (spec verify
       smoke: "define/route/create prims are absent from the example
       table");
     - static gate: mutation special forms refused, quoted data inert,
       capability-carrying calls refused via known_fn;
     - runtime boundary: prims absent from the restricted table fail
       closed — including through macro expansion and function bodies;
     - env semantics: parent bindings resolve, thunks force, nothing
       leaks back (child env);
     - caps: step budget kills runaway nesting — CONVERTED to Refused
       (budget -> refusal) so (book/verify)'s guard can hole it.

   The eval path runs under Example_eval's own nested rim, so no outer
   Control_rim is needed in the test (bench precedent). *)
open Pricklypear_lisp.Ast

let pp = Pricklypear_lisp.Reader.pp_value

(* Synthetic full prim table + env, mirroring Session.make's shape:
   every registered prim name auto-binds as VPrim in the env. *)
let make_full () =
  let prim_table = Pricklypear_image_lib.Primitives.make_with_defaults () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ -> Pricklypear_lisp.Env.define env name (VPrim name))
    prim_table;
  Pricklypear_lisp.Env.define env "pi" (VFloat Float.pi);
  (env, prim_table)

(* Bind a VPrim name without registering a body (simulates the session
   auto-bind for CRUD prims that only the image registers). *)
let bind_prim env name = Pricklypear_lisp.Env.define env name (VPrim name)

(* Eval src through the restricted engine; returns the rendered value
   or an "REFUSED: …" / "ERR: …" marker string instead of raising. *)
let run ?(limit = Pricklypear_image_lib.Example_eval.default_step_limit) ?known_fn (src : string) :
    string =
  let env, full = make_full () in
  let known_fn = match known_fn with Some f -> f | None -> fun _ -> None in
  try
    let v =
      Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full ~known_fn ~limit src
    in
    pp v
  with
  | Pricklypear_image_lib.Example_eval.Refused reason -> "REFUSED: " ^ reason
  | Interp_error e -> "ERR: " ^ Pricklypear_lisp.Ast.interp_error_kind e
  | exn -> "ERR: " ^ Printexc.to_string exn

(* --- whitelist surface --- *)

let whitelist_tests =
  [ ("pure prims whitelisted", `Quick, fun () ->
      Alcotest.(check bool) "+" true (Pricklypear_image_lib.Example_eval.whitelisted "+");
      Alcotest.(check bool) "string-append" true
        (Pricklypear_image_lib.Example_eval.whitelisted "string-append");
      Alcotest.(check bool) "read-string" true
        (Pricklypear_image_lib.Example_eval.whitelisted "read-string"));
    ("read-only DB prims whitelisted", `Quick, fun () ->
      List.iter
        (fun n -> Alcotest.(check bool) n true (Pricklypear_image_lib.Example_eval.whitelisted n))
        [ "get"; "rows"; "find-rows"; "whoami"; "current-branch";
          "list-bindings"; "function-doc"; "current-author" ]);
    ("read-only supplement whitelisted", `Quick, fun () ->
      List.iter
        (fun n -> Alcotest.(check bool) n true (Pricklypear_image_lib.Example_eval.whitelisted n))
        [ "string-split"; "prose-tokens"; "render-tokens"; "tree/get";
          "tree/children-of"; "tree/subtree"; "tree/parent";
          "backlinks"; "out-links"; "list-packages"; "describe";
          "callees"; "holes"; "budget-remaining" ]);
    (* spec eval-examples verify smoke: mutation families absent *)
    ("mutation prims NOT whitelisted", `Quick, fun () ->
      List.iter
        (fun n -> Alcotest.(check bool) n false (Pricklypear_image_lib.Example_eval.whitelisted n))
        [ "create"; "update"; "update-field"; "delete"; "deftype";
          "route"; "public-route"; "checkout"; "fork"; "merge";
          "create-user"; "load-string"; "load-library"; "reseed-package";
          "tree/upsert"; "tree/delete"; "with-public-write";
          "submit-job"; "set-llm-key" ]);
    ("escape-hatch prims NOT whitelisted", `Quick, fun () ->
      List.iter
        (fun n -> Alcotest.(check bool) n false (Pricklypear_image_lib.Example_eval.whitelisted n))
        [ "eval-string"; "bench"; "example-eval"; "http-get"; "http-post";
          "tcp-connect"; "sleep"; "agent"; "llm-chat"; "propose-function";
          "install-function" ]);
    ("restricted table holds only whitelisted names", `Quick, fun () ->
      let _, full = make_full () in
      let r = Pricklypear_image_lib.Example_eval.restricted_prims full in
      let bad =
        Hashtbl.fold
          (fun name _ acc ->
            if Pricklypear_image_lib.Example_eval.whitelisted name then acc else name :: acc)
          r []
      in
      Alcotest.(check (list string)) "no non-whitelisted entries" [] bad);
  ]

(* --- happy paths --- *)

let eval_tests =
  [ ("pure arithmetic evals", `Quick, fun () ->
      Alcotest.(check string) "1+2" "3" (run "(+ 1 2)"));
    ("multiple forms return last", `Quick, fun () ->
      Alcotest.(check string) "last" "12" (run "(+ 1 2) (* 3 4)"));
    ("let/lambda/quote usable", `Quick, fun () ->
      Alcotest.(check string) "let" "7" (run "(let ((x 6)) (+ x 1))");
      Alcotest.(check string) "lambda" "9" (run "((lambda (x) (* x 3)) 3)");
      Alcotest.(check string) "quote is data" "(define x 1)"
        (run "(quote (define x 1))"));
    ("parent bindings resolve", `Quick, fun () ->
      let env, full = make_full () in
      Pricklypear_lisp.Env.define env "answer" (VInt 40L);
      let v =
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(+ answer 2)"
      in
      Alcotest.(check string) "binding" "42" (pp v));
    ("thunks force through the parent env", `Quick, fun () ->
      let env, full = make_full () in
      Pricklypear_lisp.Env.define_thunk env "lazy-42"
        ~force:(fun _e _b -> VInt 42L) (Var "unused");
      let v =
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(+ lazy-42 0)"
      in
      Alcotest.(check string) "thunk" "42" (pp v));
    ("nothing leaks into the session env", `Quick, fun () ->
      (* define is refused statically; prove isolation via shadowing:
         the example's child env dies, the parent keeps its own value. *)
      let env, full = make_full () in
      Pricklypear_lisp.Env.define env "x" (VInt 1L);
      let _ =
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(let ((x 99)) x)"
      in
      let v =
        Pricklypear_lisp.Eval.eval
          ~budget:(Pricklypear_lisp.Eval.make_budget ())
          ~prims:Pricklypear_lisp.Eval.empty_prims env (Var "x")
      in
      Alcotest.(check string) "parent unchanged" "1" (pp v));
  ]

(* --- static gate refusals --- *)

let refused msg = String.starts_with ~prefix:"REFUSED:" msg

let gate_tests =
  [ ("define refused", `Quick, fun () ->
      Alcotest.(check string) "define"
        "REFUSED: special form 'define' is not allowed in examples"
        (run "(define x 1)"));
    ("nested define refused", `Quick, fun () ->
      Alcotest.(check bool) "nested" (refused (run "(let ((y 1)) (define z 2) z)"))
        true);
    ("defmacro refused", `Quick, fun () ->
      Alcotest.(check bool) "defmacro" (refused (run "(defmacro m () 1)")) true);
    ("with-transaction refused", `Quick, fun () ->
      Alcotest.(check bool) "txn" (refused (run "(with-transaction 1)")) true);
    ("prim escape hatch refused", `Quick, fun () ->
      Alcotest.(check bool) "prim form" (refused (run "(prim \"list\" 1)")) true);
    ("quoted define is inert data", `Quick, fun () ->
      Alcotest.(check bool) "quoted data"
        (not (refused (run "'(define x 1)")))
        true);
    ("quoted write-db call is inert data too", `Quick, fun () ->
      (* Reader sugar 'x parses as App (Var "quote") — eval returns the
         datum, so the gate must not infer the quoted form's effects
         (desugar_quote). A quoted (create …) neither executes nor
         refuses. *)
      Alcotest.(check string) "quoted create returns datum"
        "(create (quote user) (quote x) (quote y))" (run "'(create 'user 'x 'y)"));
    ("write capability refused via known_fn", `Quick, fun () ->
      let known_fn = function
        | "boom" -> Some (Pricklypear_lisp.Effect.EffSet.singleton
                            Pricklypear_lisp.Effect.WriteDb)
        | _ -> None
      in
      let out = run ~known_fn "(boom)" in
      Alcotest.(check bool) "db-write fn refused" (refused out) true;
      Alcotest.(check bool) "names the capability"
        (String.contains out 'w') true);
    ("net capability refused via known_fn", `Quick, fun () ->
      let known_fn = function
        | "fetch" -> Some (Pricklypear_lisp.Effect.EffSet.singleton
                            Pricklypear_lisp.Effect.Net)
        | _ -> None
      in
      Alcotest.(check bool) "net fn" (refused (run ~known_fn "(fetch)")) true);
  ]

(* --- runtime boundary (restricted rim) --- *)

let runtime_tests =
  [ ("unknown prim fails closed", `Quick, fun () ->
      (* "do-evil" is registered + auto-bound (full-table shape) but
         unknown to the effect lists (static gate passes it) and absent
         from the whitelist → the restricted rim refuses at dispatch. *)
      let env, full = make_full () in
      Pricklypear_image_lib.Primitives.register full "do-evil"
        (fun _ -> VStr "pwned");
      bind_prim env "do-evil";
      match
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(do-evil)"
      with
      | v -> Alcotest.fail ("should have refused, got " ^ pp v)
      | exception Interp_error (Prim_error (name, msg, _)) ->
          Alcotest.(check string) "refused name" "do-evil" name;
          Alcotest.(check bool) "fail-closed msg"
            (String.contains msg 'u') true);
    ("write prim through a macro fails closed", `Quick, fun () ->
      (* A pre-existing VMacro whose expansion calls create: the static
         gate cannot see it; the restricted rim must. *)
      let env, full = make_full () in
      bind_prim env "create";
      Pricklypear_lisp.Env.define env "wiggle"
        (VMacro ([ "x" ], App (Var "create", [ Lit (VSymbol "user") ]), env));
      match
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(wiggle 1)"
      with
      | v -> Alcotest.fail ("should have refused, got " ^ pp v)
      | exception Interp_error (Prim_error (name, _, _)) ->
          Alcotest.(check string) "create absent" "create" name);
    ("write prim through a function body fails closed", `Quick, fun () ->
      let env, full = make_full () in
      bind_prim env "delete";
      Pricklypear_lisp.Env.define env "sneaky"
        (VLambda ([], App (Var "delete", [ Lit (VSymbol "user") ]), env));
      match
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(sneaky)"
      with
      | v -> Alcotest.fail ("should have refused, got " ^ pp v)
      | exception Interp_error (Prim_error (name, _, _)) ->
          Alcotest.(check string) "delete absent" "delete" name);
    ("read prims still dispatch in the restricted rim", `Quick, fun () ->
      (* Whitelisted names keep their closures: a fake "rows" runs
         under the restricted rim. Register the body AND bind the name
         (session.make does both: bulk VPrim bind over the table, then
         image-only registrations — "rows" is a CRUD prim only the
         image registers). *)
      let env, full = make_full () in
      bind_prim env "rows";
      Pricklypear_image_lib.Primitives.register full "rows"
        (fun args -> VList [ VStr (String.concat "," (List.map pp args)) ]);
      let v =
        Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
          ~known_fn:(fun _ -> None) "(rows 'user)"
      in
      Alcotest.(check string) "rows ran" "(\"user\")" (pp v));
  ]

(* --- caps --- *)

let cap_tests =
  [ ("step budget converts runaway nesting to a refusal", `Quick, fun () ->
      let deep =
        String.concat "" (List.init 2000 (fun _ -> "(+ 1 "))
        ^ "0" ^ String.concat "" (List.init 2000 (fun _ -> ")"))
      in
      let out = run ~limit:100 deep in
      (* v2 (P2.4): budget exhaustion CONVERTS to Refused — the only
         live budget here is the nested one, and guard (book/verify's
         only catch) re-raises Budget_exceeded by design, so a runaway
         example must surface as a hole-able refusal, not a page
         killer. Pre-P2.4 this surfaced as ERR: budget_exceeded. *)
      Alcotest.(check bool) "budget -> refused"
        (String.starts_with ~prefix:"REFUSED: example budget exceeded" out)
        true);
    ("self-application loop refused at the default limit", `Quick, fun () ->
      (* ((lambda (x) (x x)) (lambda (x) (x x))) — pure-lambda runaway
         (no recursion special form needed), the shape book/verify can
         actually meet in curated examples. Must come back as a
         refusal well under the wall-clock cap. *)
      let out = run "((lambda (x) (x x)) (lambda (x) (x x)))" in
      Alcotest.(check bool) "y-loop refused"
        (String.starts_with ~prefix:"REFUSED: example budget exceeded" out)
        true);
    ("outer caller budget untouched by a runaway example", `Quick, fun () ->
      (* The nested budget dies; the caller's scope re-emerges on
         scope exit (fiber-scoped, i-1788670103-5) — prove the session
         can keep evaluating after a refusal. *)
      let env, full = make_full () in
      (try
         ignore
           (Pricklypear_image_lib.Example_eval.eval_source ~env ~full_prims:full
              ~known_fn:(fun _ -> None)
              "((lambda (x) (x x)) (lambda (x) (x x)))")
       with Pricklypear_image_lib.Example_eval.Refused _ -> ());
      let v =
        Pricklypear_image_lib.Control_rim.run ~prims:full (fun () ->
            Pricklypear_lisp.Eval.eval
              ~budget:(Pricklypear_lisp.Eval.make_budget ())
              ~prims:Pricklypear_lisp.Eval.empty_prims env
              (App (Var "+", [ int 1; int 2 ])))
      in
      Alcotest.(check string) "caller alive" "3" (pp v));
    ("empty source refused", `Quick, fun () ->
      Alcotest.(check string) "empty" "REFUSED: empty source" (run ""));
    ("parse error propagates as ERR", `Quick, fun () ->
      Alcotest.(check bool) "parse err"
        (String.starts_with ~prefix:"ERR:" (run "(+ 1")) true);
  ]

let () =
  let open Alcotest in
  run "example_eval"
    [
      ("whitelist", whitelist_tests);
      ("eval", eval_tests);
      ("static gate", gate_tests);
      ("runtime boundary", runtime_tests);
      ("caps", cap_tests);
    ]
