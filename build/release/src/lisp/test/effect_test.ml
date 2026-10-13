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
  ("public_route_allow default has no write-db", `Quick, fun () ->
    Unix.putenv "PP_PUBLIC_ALLOW_WRITE" "0";
    let allow = Effect.public_route_allow () in
    Alcotest.(check bool) "no write" false
      (Effect.EffSet.mem Effect.WriteDb allow);
    Alcotest.(check bool) "has read" true
      (Effect.EffSet.mem Effect.ReadDb allow);
    let got = Effect.EffSet.singleton Effect.WriteDb in
    Alcotest.(check bool) "deny write" false
      (Effect.policy_allows ~allow got));
  ("public_route_allow write opt-in", `Quick, fun () ->
    Unix.putenv "PP_PUBLIC_ALLOW_WRITE" "1";
    let allow = Effect.public_route_allow () in
    Alcotest.(check bool) "write" true
      (Effect.EffSet.mem Effect.WriteDb allow);
    Unix.putenv "PP_PUBLIC_ALLOW_WRITE" "0");
  (* H0.1 audit coverage for previously unlisted registered prims *)
  ("div/abs pure", `Quick, fun () ->
    let fx = infer "(div (abs x) 2)" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("/ alias pure (no drift vs div)", `Quick, fun () ->
    let fx = infer "(/ 10 4)" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("quotient/remainder pure", `Quick, fun () ->
    let fx = infer "(remainder (quotient x y) z)" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("math tower pure", `Quick, fun () ->
    let fx = infer "(sqrt (expt (sin x) 2))" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("number? pure", `Quick, fun () ->
    let fx = Effect.effects_for_prim "number?" in
    Alcotest.(check bool) "pure" true (Effect.EffSet.is_pure fx));
  ("random pure", `Quick, fun () ->
    let fx = Effect.effects_for_prim "random" in
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
  (* bumpers M2 (borge/bumpers.borg notices-envelope): define-notices *)
  ("define_notices pure quiet define", `Quick, fun () ->
    (* well-formed, doc'd, pure: exactly ONE notice (the define kind) *)
    let ns =
      Effect.define_notices ~name:"add" ~doc:(Some "adds two") (Reader.read_one "(lambda (a b) (+ a b))")
    in
    Alcotest.(check int) "one notice" 1 (List.length ns);
    (match ns with
     | [ n ] ->
         Alcotest.(check string) "kind" "define" n.kind;
         Alcotest.(check string) "name" "add" n.name;
         Alcotest.(check string) "effects" "pure"
           (Option.value n.effects ~default:"");
         Alcotest.(check bool) "no bench on a quiet define" false
           (contains ~sub:"bench" n.text)
     | _ -> Alcotest.fail "expected exactly one notice"));
  ("define_notices effects badge text", `Quick, fun () ->
    let ns =
      Effect.define_notices ~name:"save" ~doc:(Some "d") (Reader.read_one "(lambda (j) (create 'note j))")
    in
    (match ns with
     | [ n ] ->
         Alcotest.(check string) "effects badge" "db-write"
           (Option.value n.effects ~default:"") ;
         Alcotest.(check bool) "bench pointer rides the define kind" true
           (contains ~sub:"bench" n.text);
         Alcotest.(check bool) "timing pointer" true
           (contains ~sub:"/timing" n.text)
     | _ -> Alcotest.fail "expected one define notice"));
  ("define_notices missing docstring tip", `Quick, fun () ->
    let ns =
      Effect.define_notices ~name:"save" ~doc:None (Reader.read_one "(lambda (j) (+ 1 j))")
    in
    Alcotest.(check int) "two notices" 2 (List.length ns);
    (match ns with
     | [ d; t ] ->
         Alcotest.(check string) "define kind" "define" d.kind;
         Alcotest.(check string) "tip kind" "tip" t.kind;
         Alcotest.(check bool) "doc nudge mentions function-doc" true
           (contains ~sub:"function-doc" t.text);
         Alcotest.(check bool) "doc nudge names the function" true
           (contains ~sub:"(define save" t.text)
     | _ -> Alcotest.fail "expected define + tip");
    (* budget: a define never exceeds the per-response budget of 2 *)
    Alcotest.(check bool) "within budget" true
      (List.length (Effect.cap_notices ns) = 2));
  ("taper_tips once per session", `Quick, fun () ->
    let mk () =
      Effect.define_notices ~name:"f" ~doc:None (Reader.read_one "(lambda () 1)")
    in
    let first, fired = Effect.taper_tips ~fired:[] (mk ()) in
    Alcotest.(check bool) "first define keeps the tip" true
      (List.exists (fun (n : Effect.define_notice) -> n.kind = "tip") first);
    let second, fired' = Effect.taper_tips ~fired (mk ()) in
    Alcotest.(check bool) "tip dropped on replay" false
      (List.exists (fun (n : Effect.define_notice) -> n.kind = "tip") second);
    Alcotest.(check bool) "define kind still fires on replay" true
      (List.exists (fun (n : Effect.define_notice) -> n.kind = "define") second);
    Alcotest.(check int) "fired set grows once" 1 (List.length fired');
    Alcotest.(check bool) "fired set monotone" true
      (List.length fired' >= List.length fired));
  (* ---- bumpers M3: route-notices (fragment-by-default classifier) ---- *)
  ("classify_route_shape full shell (layout/page)", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(layout/page (list (html/div (list (html/text \"hi\")))))")
    in
    Alcotest.(check string) "shell" "shell" (Effect.route_shape_string shape));
  ("classify_route_shape layout/page-with-css is shell", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(layout/page-with-css (list (html/p (list (html/text \"x\")))))")
    in
    Alcotest.(check string) "shell" "shell" (Effect.route_shape_string shape));
  ("classify_route_shape html/page is shell", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(html/page (list (html/h1 (list (html/text \"y\")))))")
    in
    Alcotest.(check string) "shell" "shell" (Effect.route_shape_string shape));
  ("classify_route_shape shell nested inside let", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(let (parts (list (html/div (list)))) \
                          (layout/page (list parts)))")
    in
    Alcotest.(check string) "shell" "shell" (Effect.route_shape_string shape));
  ("classify_route_shape fragment (html/div only)", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(html/div (html/attr \"class\" \"x\") (html/text \"hi\"))")
    in
    Alcotest.(check string) "fragment" "fragment" (Effect.route_shape_string shape));
  ("classify_route_shape fragment via render-html (no full shell)", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(render-html (html/join (list (html/p (list (html/text \"x\"))))))")
    in
    Alcotest.(check string) "fragment" "fragment" (Effect.route_shape_string shape));
  ("classify_route_shape not-html (json endpoint)", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape
        (Reader.read_one "(json-emit (dict-set (dict-set (dict-keys (list)) \"a\" 1) \"b\" 2))")
    in
    Alcotest.(check string) "not-html" "not-html" (Effect.route_shape_string shape));
  ("classify_route_shape not-html (plain string)", `Quick, fun () ->
    let shape =
      Effect.classify_route_shape (Reader.read_one "\"hello world\"")
    in
    Alcotest.(check string) "not-html" "not-html" (Effect.route_shape_string shape));
  ("route_notice full shell carries remark + harness pointer", `Quick, fun () ->
    let ns =
      Effect.route_notice
        { Effect.path = "/pages"
        ; effects = Some Effect.EffSet.empty
        ; shape = Effect.Shell
        ; standalone = false }
    in
    (match ns with
     | [ n ] ->
         Alcotest.(check string) "kind" "route" n.kind;
         Alcotest.(check string) "name" "/pages" n.name;
         Alcotest.(check bool) "full shell remark" true
           (contains ~sub:"full shell" n.text);
         Alcotest.(check bool) "harness pointer" true
           (contains ~sub:"verify tiered" n.text);
         Alcotest.(check bool) "standalone hint" true
           (contains ~sub:"standalone page? declare it" n.text)
     | _ -> Alcotest.fail "expected exactly one route notice"));
  ("route_notice fragment has harness pointer, no shell remark", `Quick, fun () ->
    let ns =
      Effect.route_notice
        { Effect.path = "/widget"
        ; effects = Some Effect.EffSet.empty
        ; shape = Effect.Fragment
        ; standalone = false }
    in
    (match ns with
     | [ n ] ->
         Alcotest.(check bool) "harness pointer" true
           (contains ~sub:"verify tiered" n.text);
         Alcotest.(check bool) "no shell remark" false
           (contains ~sub:"full shell" n.text)
     | _ -> Alcotest.fail "expected exactly one route notice"));
  ("route_notice not-html carries no harness pointer", `Quick, fun () ->
    let ns =
      Effect.route_notice
        { Effect.path = "/api/data"
        ; effects = Some Effect.EffSet.empty
        ; shape = Effect.NotHtml
        ; standalone = false }
    in
    (match ns with
     | [ n ] ->
         Alcotest.(check bool) "just published" true
           (contains ~sub:"published" n.text);
         Alcotest.(check bool) "no harness pointer" false
           (contains ~sub:"verify tiered" n.text)
     | _ -> Alcotest.fail "expected exactly one route notice"));
  ("route_notice standalone silences fragment family", `Quick, fun () ->
    let ns =
      Effect.route_notice
        { Effect.path = "/pages"
        ; effects = Some Effect.EffSet.empty
        ; shape = Effect.Shell
        ; standalone = true }
    in
    (match ns with
     | [ n ] ->
         Alcotest.(check bool) "silenced" true
           (contains ~sub:"standalone" n.text);
         Alcotest.(check bool) "no full shell remark" false
           (contains ~sub:"full shell" n.text)
     | _ -> Alcotest.fail "expected exactly one route notice"));
  ("route_notice unbound handler (no body) notes not-yet-defined", `Quick, fun () ->
    let ns =
      Effect.route_notice
        { Effect.path = "/later"
        ; effects = None
        ; shape = Effect.NotHtml
        ; standalone = false }
    in
    (match ns with
     | [ n ] ->
         Alcotest.(check bool) "effects omitted" true (n.effects = None);
         Alcotest.(check bool) "not-yet-defined text" true
           (contains ~sub:"not yet defined" n.text)
     | _ -> Alcotest.fail "expected exactly one route notice"));
  (* bumpers M4 (borge/bumpers.borg verify-route): static constant-rebuild
     warnings -- the first named verify-route warning pattern. Conservative:
     only literal constants warn; request-dependent builds never can. *)
  ("verify_route_warnings large css string literal", `Quick, fun () ->
    (* one big inline CSS string (>= 1024 chars) -> warn *) 
    let big = String.make 1500 'x' in
    let ws = Effect.verify_route_warnings (Reader.read_one (Printf.sprintf "(html/style (html/text \"%s\"))" big)) in
    Alcotest.(check int) "one warning" 1 (List.length ws);
    (match ws with
     | [ w ] ->
         Alcotest.(check bool) "delay nudge" true (contains ~sub:"delay" w)
     | _ -> Alcotest.fail "expected the constant-rebuild warning"));
  ("verify_route_warnings big literal list (layout incident shape)", `Quick, fun () ->
    (* a (list ...) of 30 literal CSS strings fed to html/join — the actual
       prod 2026-12-30 layout stylesheet shape -> warn *) 
    let parts =
      List.init 30 (fun i -> Printf.sprintf ".cl%d{color:#%06x}" i (i * 0x111111))
    in
    let src = "(html/join (list " ^ String.concat " " (List.map (fun s -> Printf.sprintf "\"%s\"" s) parts) ^ "))" in
    let ws = Effect.verify_route_warnings (Reader.read_one src) in
    Alcotest.(check int) "one warning" 1 (List.length ws);
    Alcotest.(check bool) "delay nudge" true
      (List.exists (contains ~sub:"delay") ws));
  ("verify_route_warnings small constants never warn", `Quick, fun () ->
    let ws =
      Effect.verify_route_warnings
        (Reader.read_one "(html/p (html/attr \"class\" \"x\") (html/text \"hello world\"))")
    in
    Alcotest.(check int) "no warnings" 0 (List.length ws));
  ("verify_route_warnings request-dependent build never warns", `Quick, fun () ->
    (* builds HTML from a request param + rows — no AST literal payload -> no warn *) 
    let ws =
      Effect.verify_route_warnings
        (Reader.read_one "(html/div (html/attr \"class\" (string-append \"doc-\" (as-str (request-param \"slug\")))) (html/join (rows 'note \"{}\")))")
    in
    Alcotest.(check int) "no warnings" 0 (List.length ws));
  ("verify_route_warnings many small strings within 24 stay quiet", `Quick, fun () ->
    (* 10 short literal strings < 24 items and < 1024 chars -> no warn *) 
    let parts = List.init 10 (fun i -> Printf.sprintf "\"short-%d\"" i) in
    let src = "(list " ^ String.concat " " parts ^ ")" in
    let ws = Effect.verify_route_warnings (Reader.read_one src) in
    Alcotest.(check int) "no warnings" 0 (List.length ws));
]

let () =
  Alcotest.run "pricklypear-lisp-effects" [ "effects", checks ]
