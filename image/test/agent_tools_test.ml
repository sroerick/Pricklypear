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

let () =
  Alcotest.run "pricklypear-image-agent-tools" [
    "effect-gate", tests;
  ]
