(** Package-manifest parser: the (package …) form is metadata, not a call. *)

open Pricklypear_lisp

let parse_src src =
  match Package_manifest.parse_source src with
  | Ok m -> m
  | Error e -> Alcotest.fail ("parse failed: " ^ e)

let check_name expected m =
  match m with
  | None -> Alcotest.fail "expected a manifest"
  | Some (m : Package_manifest.t) ->
      Alcotest.(check string) "name" expected m.name

let test_full () =
  let m =
    parse_src
      {|;; header comment
(package notes
  (version "0.3.0")
  (requires std html (outline ">=0.1"))
  (provides notes/doc notes/page)
  (contributes
    (nav notes/capture-nav)
    (css notes/css notes/tree-css)
    (home notes/home))
  (export notes/doc/get notes/doc/upsert))
(define notes/doc/get (lambda (slug) slug))
|}
  in
  check_name "notes" m;
  match m with
  | None -> ()
  | Some (m : Package_manifest.t) ->
      Alcotest.(check (option string)) "version" (Some "0.3.0") m.version;
      Alcotest.(check (list string)) "requires"
        [ "std"; "html"; "outline" ] m.requires;
      Alcotest.(check (list string)) "provides"
        [ "notes/doc"; "notes/page" ] m.provides;
      Alcotest.(check (list string)) "exports"
        [ "notes/doc/get"; "notes/doc/upsert" ] m.exports;
      Alcotest.(check int) "contributions" 4
        (List.length m.contributions);
      (match m.contributions with
       | [ Package_manifest.Nav "notes/capture-nav"
         ; Package_manifest.Css "notes/css"
         ; Package_manifest.Css "notes/tree-css"
         ; Package_manifest.Home "notes/home" ] -> ()
       | _ -> Alcotest.fail "contribution order/kinds did not round-trip")

let test_minimal () =
  match parse_src "(package p (export p/a p/b))" with
  | None -> Alcotest.fail "expected a manifest"
  | Some (m : Package_manifest.t) ->
      Alcotest.(check string) "name" "p" m.name;
      Alcotest.(check (option string)) "no version" None m.version;
      Alcotest.(check (list string)) "no requires" [] m.requires;
      Alcotest.(check (list string)) "exports" [ "p/a"; "p/b" ] m.exports

let test_none () =
  match Package_manifest.parse_source "(define x 1)\n" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "a file without (package …) must have no manifest"
  | Error e -> Alcotest.fail e

let test_dedupe () =
  match
    parse_src
      "(package p (requires a a b) (export p/a p/a))"
  with
  | Some (m : Package_manifest.t) ->
      Alcotest.(check (list string)) "requires deduped" [ "a"; "b" ] m.requires;
      Alcotest.(check (list string)) "exports deduped" [ "p/a" ] m.exports
  | None -> Alcotest.fail "expected a manifest"

let test_contribution_app () =
  match
    parse_src
      "(package p (contributes (app \"x\" \"X\" \"one line\" \"/x\")))"
  with
  | Some (m : Package_manifest.t) ->
      (match m.contributions with
       | [ Package_manifest.App_tile { glyph; name; summary; href } ] ->
           Alcotest.(check string) "glyph" "x" glyph;
           Alcotest.(check string) "name" "X" name;
           Alcotest.(check string) "summary" "one line" summary;
           Alcotest.(check string) "href" "/x" href
       | _ -> Alcotest.fail "app contribution did not parse")
  | None -> Alcotest.fail "expected a manifest"

let test_register_contributions () =
  let open Package_manifest in
  let items =
    register_contributions ~package:"a" []
      [ Css "a/css"; Nav "a/nav" ]
  in
  let items =
    register_contributions ~package:"b" items
      [ Css "b/css"; Home "b/home" ]
  in
  Alcotest.(check (list string)) "css order"
    [ "a/css"; "b/css" ] (functions_of_kind "css" items);
  Alcotest.(check (list string)) "nav order"
    [ "a/nav" ] (functions_of_kind "nav" items);
  Alcotest.(check (list string)) "home order"
    [ "b/home" ] (functions_of_kind "home" items);
  (* A re-load replaces the same slot, it does not append a duplicate. *)
  let items =
    register_contributions ~package:"a" items [ Css "a/css" ]
  in
  Alcotest.(check (list string)) "re-load stable"
    [ "a/css"; "b/css" ] (functions_of_kind "css" items);
  Alcotest.(check (list string)) "re-load drops a/nav"
    [] (functions_of_kind "nav" items);
  Alcotest.(check int) "one a/css" 1
    (List.length
       (List.filter (fun (_, c) -> c = Css "a/css") items))

let test_invalid () =
  let bad src =
    match Package_manifest.parse_source src with
    | Error _ -> true
    | Ok _ -> false
  in
  Alcotest.(check bool) "invalid name" true
    (bad "(package Bad.Name (export x))");
  Alcotest.(check bool) "reserved kernel" true (bad "(package kernel)");
  Alcotest.(check bool) "empty version clause" true
    (bad "(package p (version))");
  Alcotest.(check bool) "missing name" true (bad "(package)")

let test_clause () =
  let clause src =
    match Reader.read_one src with
    | e -> Package_manifest.is_clause e
    | exception _ -> false
  in
  List.iter
    (fun s ->
       Alcotest.(check bool) s true (clause s))
    [ "(version \"1\")"; "(requires a)"; "(provides a)"; "(export a)";
      "(contributes (nav x))"; "(doc \"x\")" ];
  Alcotest.(check bool) "define is not a clause" false
    (clause "(define x 1)")

let () =
  Alcotest.run "nopales-package-manifest"
    [ ( "manifest"
      , [ ("full", `Quick, test_full)
        ; ("minimal", `Quick, test_minimal)
        ; ("none", `Quick, test_none)
        ; ("dedupe", `Quick, test_dedupe)
        ; ("contribution-app", `Quick, test_contribution_app)
        ; ("register-contributions", `Quick, test_register_contributions)
        ; ("invalid", `Quick, test_invalid)
        ; ("clause", `Quick, test_clause) ] ) ]
