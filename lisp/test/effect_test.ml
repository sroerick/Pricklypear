(** Capability effect inference tests — pure, no services. *)
open Pricklypear_lisp

let infer s =
  let e = Reader.read_one s in
  Effect.infer e

let pp_fx e = Effect.EffSet.pp e

let checks = [
  ("pure arithmetic", `Quick, fun () ->
    let fx = infer "(* x x)" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("lambda pure", `Quick, fun () ->
    let fx = infer "(lambda (x) (* x x))" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("get is read-db", `Quick, fun () ->
    let fx = infer "(get 'note id)" in
    Alcotest.(check bool) "read" true (Effect.EffSet.mem Effect.ReadDb fx);
    Alcotest.(check bool) "not write" false (Effect.EffSet.mem Effect.WriteDb fx));
  ("create is write-db", `Quick, fun () ->
    let fx = infer "(create 'note \"{}\")" in
    Alcotest.(check bool) "write" true (Effect.EffSet.mem Effect.WriteDb fx));
  ("route is publish", `Quick, fun () ->
    let fx = infer "(route \"/x\" \"f\")" in
    Alcotest.(check bool) "pub" true (Effect.EffSet.mem Effect.Publish fx));
  ("render-html is html", `Quick, fun () ->
    let fx = infer "(render-html \"<b>x</b>\")" in
    Alcotest.(check bool) "html" true (Effect.EffSet.mem Effect.Html fx));
  ("propose-function is ai", `Quick, fun () ->
    let fx = infer "(propose-function \"f\" \"spec\")" in
    Alcotest.(check bool) "ai" true (Effect.EffSet.mem Effect.Ai fx));
  ("compose read+html", `Quick, fun () ->
    let fx = infer "(lambda () (render-html (dict-get (get 'n id) \"t\")))" in
    Alcotest.(check bool) "read" true (Effect.EffSet.mem Effect.ReadDb fx);
    Alcotest.(check bool) "html" true (Effect.EffSet.mem Effect.Html fx));
  ("badge pure", `Quick, fun () ->
    Alcotest.(check string) "b" "pure" (Effect.EffSet.badge Effect.EffSet.empty));
  ("policy allows pure", `Quick, fun () ->
    Alcotest.(check bool) "ok" true
      (Effect.policy_allows ~allow:Effect.ai_default_allow Effect.EffSet.empty));
  ("policy denies write", `Quick, fun () ->
    let got = Effect.EffSet.singleton Effect.WriteDb in
    Alcotest.(check bool) "deny" false
      (Effect.policy_allows ~allow:Effect.ai_default_allow got));
  ("policy allows read+html", `Quick, fun () ->
    let got = Effect.EffSet.normalize [ Effect.ReadDb; Effect.Html ] in
    Alcotest.(check bool) "ok" true
      (Effect.policy_allows ~allow:Effect.ai_default_allow got));
]

let () =
  Alcotest.run "pricklypear-lisp-effects" [ "effects", checks ]
