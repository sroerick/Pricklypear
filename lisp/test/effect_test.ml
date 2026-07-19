(** Capability effect inference tests — pure, no services. *)
open Pricklypear_lisp

let infer s =
  let e = Reader.read_one s in
  Effect.infer e

let pp_fx e = Effect.EffSet.pp e

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec find i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else find (i + 1)
  in
  m = 0 || find 0

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
  ("policy allows write-db for app scaffolding", `Quick, fun () ->
    let got = Effect.EffSet.singleton Effect.WriteDb in
    Alcotest.(check bool) "allow write" true
      (Effect.policy_allows ~allow:Effect.ai_default_allow got));
  ("policy denies publish without elevate", `Quick, fun () ->
    let got = Effect.EffSet.singleton Effect.Publish in
    Alcotest.(check bool) "deny publish" false
      (Effect.policy_allows ~allow:Effect.ai_default_allow got));
  ("policy allows read+write+html", `Quick, fun () ->
    let got =
      Effect.EffSet.normalize
        [ Effect.ReadDb; Effect.WriteDb; Effect.Html ]
    in
    Alcotest.(check bool) "ok" true
      (Effect.policy_allows ~allow:Effect.ai_default_allow got));
  (* H0.1 audit coverage for previously unlisted registered prims *)
  ("div/abs pure", `Quick, fun () ->
    let fx = infer "(div (abs x) 2)" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("public-route is publish", `Quick, fun () ->
    let fx = Effect.effects_for_prim "public-route" in
    Alcotest.(check bool) "pub" true (Effect.EffSet.mem Effect.Publish fx));
  ("load-library is write-db", `Quick, fun () ->
    let fx = Effect.effects_for_prim "load-library" in
    Alcotest.(check bool) "write" true (Effect.EffSet.mem Effect.WriteDb fx));
  ("agent-say is ai+write", `Quick, fun () ->
    let fx = Effect.effects_for_prim "agent-say" in
    Alcotest.(check bool) "ai" true (Effect.EffSet.mem Effect.Ai fx);
    Alcotest.(check bool) "write" true (Effect.EffSet.mem Effect.WriteDb fx));
  ("redirect is html", `Quick, fun () ->
    let fx = Effect.effects_for_prim "redirect" in
    Alcotest.(check bool) "html" true (Effect.EffSet.mem Effect.Html fx));
  (* H0.2 known_fn derive-on-read *)
  ("known_fn pulls callee write-db", `Quick, fun () ->
    let save =
      Reader.read_one "(lambda (j) (create 'note j))"
    in
    let page =
      Reader.read_one "(lambda () (save \"{}\"))"
    in
    let known =
      Effect.known_fn_of_bindings [("save", save)]
    in
    let fx = Effect.infer ~known_fn:known page in
    Alcotest.(check bool) "write via callee" true
      (Effect.EffSet.mem Effect.WriteDb fx));
  ("known_fn cycle does not diverge", `Quick, fun () ->
    let a = Reader.read_one "(lambda () (b))" in
    let b = Reader.read_one "(lambda () (a))" in
    let known = Effect.known_fn_of_bindings [("a", a); ("b", b)] in
    let fx = Effect.infer ~known_fn:known (Reader.read_one "(a)") in
    Alcotest.(check bool) "pure cycle" true (Effect.EffSet.is_pure fx));
  (* H0.4 soft txn lint *)
  ("txn_lint multi-write without txn", `Quick, fun () ->
    let e =
      Reader.read_one
        "(lambda () (create 'n \"{}\") (delete 'n id))"
    in
    let hs = Effect.txn_lint e in
    Alcotest.(check bool) "has hint" true (hs <> []);
    Alcotest.(check bool) "mentions write-db sites" true
      (List.exists (contains ~sub:"write-db sites") hs));
  ("txn_lint quiet with with-transaction", `Quick, fun () ->
    let e =
      Reader.read_one
        "(with-transaction (lambda () (create 'n \"{}\") (delete 'n id)))"
    in
    let hs = Effect.txn_lint e in
    Alcotest.(check bool) "no multi-write hint" true
      (not (List.exists (contains ~sub:"write-db sites") hs)));
  ("txn_lint select-for-update bare", `Quick, fun () ->
    let e = Reader.read_one "(lambda () (select-for-update 'n id))" in
    let hs = Effect.txn_lint e in
    Alcotest.(check bool) "s4u hint" true
      (List.exists (contains ~sub:"select-for-update") hs));
]

let () =
  Alcotest.run "pricklypear-lisp-effects" [ "effects", checks ]
