(** Pricklypear Image — agent_tools effect-gate tests (no DB).

    Tests the macroexpand-fail path of [check_ai_body_effects] (B1):
    a failing macroexpand must NOT fall back to the unexpanded tree —
    it must surface an Error so the AI install is denied. *)
open Pricklypear_lisp

(* Build an env with a macro that raises during expansion: the macro
   body references an unbound variable, so expanding (boom) raises
   Interp_error (Unbound_var). *)
let make_env_with_bad_macro () =
  let prims = Eval.make_prim_table () in
  let env = Env.create () in
  (* (defmacro boom () nope) — body is [Var "nope"], unbound at expand *)
  let _ = Eval.eval_string ~prims env "(defmacro boom () nope)" in
  env

let tests = [
  ("macroexpand failure surfaces Error (not silent Ok)", `Quick, fun () ->
    let env = make_env_with_bad_macro () in
    let result =
      Pricklypear_image_lib.Agent_tools.check_ai_body_effects ~env "(boom)"
    in
    match result with
    | Ok () ->
        Alcotest.fail
          "expected Error on macroexpand failure, got Ok (silent fallback bug)"
    | Error msg ->
        (* the message must mention macroexpand failure *)
        let has_macroexpand =
          let needle = "macroexpand" in
          let nlen = String.length needle in
          let mlen = String.length msg in
          let rec loop i =
            if i + nlen > mlen then false
            else if String.sub msg i nlen = needle then true
            else loop (i + 1)
          in
          loop 0
        in
        Alcotest.(check bool) "mentions macroexpand" true has_macroexpand);
  ("no-env path still Ok for pure body", `Quick, fun () ->
    (* Without ~env, macroexpand is skipped; a pure body should be Ok *)
    let result =
      Pricklypear_image_lib.Agent_tools.check_ai_body_effects "(+ 1 2)"
    in
    match result with
    | Ok () -> ()
    | Error msg ->
        Alcotest.failf "expected Ok for pure body, got Error: %s" msg);
]

let m5_tests = [
  ("annotate_name renders arity + badge", `Quick, fun () ->
    Alcotest.(check string)
      "arith" "f (a b) [db-write]"
      (Pricklypear_image_lib.Agent_tools.annotate_name "f" (Some ["a"; "b"]) "db-write"));
  ("annotate_name no args just badge", `Quick, fun () ->
    Alcotest.(check string)
      "prim" "describe [db-read]"
      (Pricklypear_image_lib.Agent_tools.annotate_name "describe" None "db-read")
  );
  ("annotate_name empty param list -> no parens", `Quick, fun () ->
    Alcotest.(check string)
      "thunk" "page [html]"
      (Pricklypear_image_lib.Agent_tools.annotate_name "page" (Some []) "html"));
  ("doc_coverage_footer present when undocumented exist", `Quick, fun () ->
    match Pricklypear_image_lib.Agent_tools.doc_coverage_footer
            ~documented:12 ~total:15 ~pkg:"todo" with
    | Some f ->
        (* documented X/Y — (docs-missing "pkg"), Y > X *)
        Alcotest.(check bool) "footer mentions coverage" true (String.contains f '/')
    | None -> Alcotest.fail "expected footer for 12/15");
  ("doc_coverage_footer canonical text", `Quick, fun () ->
    match Pricklypear_image_lib.Agent_tools.doc_coverage_footer
            ~documented:12 ~total:15 ~pkg:"todo" with
    | Some f -> Alcotest.(check string)
        "text" "documented 12/15 — (docs-missing \"todo\")" f
    | None -> Alcotest.fail "expected footer for 12/15");
  ("doc_coverage_footer absent when fully documented", `Quick, fun () ->
    match Pricklypear_image_lib.Agent_tools.doc_coverage_footer
            ~documented:15 ~total:15 ~pkg:"todo" with
    | Some _ -> Alcotest.fail "expected no footer at 15/15"
    | None -> ());
  ("app_check_line fixed-width shape", `Quick, fun () ->
    let l = Pricklypear_image_lib.Agent_tools.app_check_line
        ~path:"/contacts" ~handler:"contacts/page" ~shape:"fragment" ~vis:"private"
    in
    (* path::handler::shape::[vis], shape in the shape column *)
    Alcotest.(check bool) "shape token present" true (String.contains l 'f'));
  ("lambda_params reads the constructor Lambda form", `Quick, fun () ->
    let e = Pricklypear_lisp.Ast.Lambda ([ "a"; "b" ], Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 0L)) in
    Alcotest.(check (option (list string)))
      "params" (Some [ "a"; "b" ])
      (Pricklypear_image_lib.Agent_tools.lambda_params e));
  ("lambda_params reads the reader App-lambda form (seed/load shape)", `Quick, fun () ->
    (* todo/db/get shape: App (Var "lambda", [App(Var "type",[Var "id"]); body]) *)
    let params =
      Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "type",
        [ Pricklypear_lisp.Ast.Var "id" ])
    in
    let e = Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "lambda",
      [ params; Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 0L) ])
    in
    Alcotest.(check (option (list string)))
      "params" (Some [ "type"; "id" ])
      (Pricklypear_image_lib.Agent_tools.lambda_params e));
  ("lambda_params reads empty reader-form lambda params (Lit VNil)", `Quick, fun () ->
    (* todo/page shape: App (Var "lambda", [Lit VNil; body]) *)
    let e = Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "lambda",
      [ Pricklypear_lisp.Ast.Lit Pricklypear_lisp.Ast.VNil; Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 0L) ])
    in
    Alcotest.(check (option (list string)))
      "params" (Some [])
      (Pricklypear_image_lib.Agent_tools.lambda_params e));
  ("lambda_params None on non-lambda body", `Quick, fun () ->
    let e = Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "if",
      [ Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 1L)
      ; Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 2L)
      ; Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 3L) ])
    in
    Alcotest.(check (option (list string)))
      "params" None
      (Pricklypear_image_lib.Agent_tools.lambda_params e));
]

let () =
  Alcotest.run "pricklypear-image-agent-tools" [
    "effect-gate", tests;
    "m5-discovery-surfaces", m5_tests;
  ]
