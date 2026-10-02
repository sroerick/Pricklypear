(** pp_sync_diff — canonical body-level vault drift (card 8dff7c9e).

    No live server: pure reader + AST comparison. These pin the
    "fix committed in git, live main unchanged" signal that the old
    name-only diff could not see. *)

open Pp_sync_diff

let check_list msg expected actual =
  Alcotest.(check (list string)) msg expected actual

let test_parse_bodies_canonical () =
  (* Same AST, different formatting + docstring => equal canonical body. *)
  let a = "(define f (lambda (x) (+ x 1)))\n\"doc A\"" in
  let b = "(define f\n  (lambda\n    (x)\n    (+ x 1)))\n\"doc B\"" in
  match (parse_bodies a, parse_bodies b) with
  | Some [(na, ba)], Some [(nb, bb)] ->
      Alcotest.(check string) "same name" na nb;
      Alcotest.(check string) "canonical body ignores whitespace + doc" ba bb
  | _ -> Alcotest.fail "expected one define each"

let test_parse_bodies_unparseable () =
  Alcotest.(check bool) "unbalanced source => None" true
    (parse_bodies "(define f (lambda (x)" = None)

let test_divergence_stale_body () =
  (* The load-bearing case: name on both sides, body differs. *)
  let local = [ ("pkg/f", "{\"app\":[{\"var\":\"lambda\"}]}") ] in
  let live = [ ("pkg/f", "{\"app\":[{\"var\":\"old\"}]}") ] in
  let changed, only_local, only_live =
    divergence ~local ~live
  in
  Alcotest.(check int) "one changed" 1 (List.length changed);
  Alcotest.(check (list string)) "changed name" [ "pkg/f" ]
    (List.map (fun (n, _, _) -> n) changed);
  check_list "no only-local" [] only_local;
  check_list "no only-live" [] only_live

let test_divergence_sets () =
  let local = [ ("a", "1"); ("b", "2") ] in
  let live = [ ("b", "2"); ("c", "3") ] in
  let changed, only_local, only_live =
    divergence ~local ~live
  in
  Alcotest.(check int) "no changed" 0 (List.length changed);
  check_list "a only local" [ "a" ] only_local;
  check_list "c only live" [ "c" ] only_live

let test_divergence_equal_no_drift () =
  let same = [ ("a", "1"); ("b", "2") ] in
  let changed, ol, ov = divergence ~local:same ~live:same in
  Alcotest.(check int) "changed" 0 (List.length changed);
  check_list "only_local" [] ol;
  check_list "only_live" [] ov

let test_pp_body_one_line () =
  (* Canonical body JSON for (lambda (x) (+ x 1)). *)
  let src = "(define f (lambda (x) (+ x 1)))" in
  match parse_bodies src with
  | Some [ (_, body) ] ->
      let pp = pp_body body in
      Alcotest.(check bool) "pretty print has no newline" false
        (String.contains pp '\n');
      Alcotest.(check bool) "pretty print is source-shaped" true
        (String.length pp > 0 && pp.[0] = '(')
  | _ -> Alcotest.fail "expected one define"

let test_unwrap_quoted_names () =
  (* prims print their lists as JSON now; the old parser turned "[]"
     into a bogus element named "[]" (status showed "only-live: 1"
     with an empty list). lisp-printed + bare-symbol forms still parse. *)
  check_list "JSON empty" [] (unwrap_quoted_names "[]");
  check_list "JSON array" [ "todo/a"; "todo/b" ]
    (unwrap_quoted_names "[\"todo/a\",\"todo/b\"]");
  check_list "lisp quoted" [ "a"; "b" ] (unwrap_quoted_names "\"a\" \"b\"");
  check_list "lisp plain" [ "a"; "b" ] (unwrap_quoted_names "(a b)");
  check_list "empty string" [] (unwrap_quoted_names "")

let test_export_provenance () =
  (* the header the image stamps on every export-package text *)
  let main_export =
    ";; package std -- exported from live branch system/main\n" in
  Alcotest.(check (option string)) "parses the branch" (Some "system/main")
    (export_provenance main_export);
  Alcotest.(check (option string)) "garden branch identified" (Some "admin/personal")
    (export_provenance
       ";; package err -- exported from live branch admin/personal\n");
  Alcotest.(check (option string)) "no header -> None" None
    (export_provenance "(define x (lambda () 1))")

let () =
  Alcotest.run "pp_sync_diff"
    [ ( "parse",
        [ Alcotest.test_case "canonical bodies ignore formatting" `Quick
            test_parse_bodies_canonical
        ; Alcotest.test_case "unparseable source => None" `Quick
            test_parse_bodies_unparseable
        ] )
    ; ( "divergence",
        [ Alcotest.test_case "stale body detected" `Quick
            test_divergence_stale_body
        ; Alcotest.test_case "only-local / only-live sets" `Quick
            test_divergence_sets
        ; Alcotest.test_case "equal => no drift" `Quick
            test_divergence_equal_no_drift
        ] )
    ; ( "pretty",
        [ Alcotest.test_case "one-line source shape" `Quick
            test_pp_body_one_line
        ] )
    ; ( "unwrap",
        [ Alcotest.test_case "quoted-name list shapes (JSON + lisp)" `Quick
            test_unwrap_quoted_names ] )
    ; ( "provenance",
        [ Alcotest.test_case "export header branch self-report" `Quick
            test_export_provenance ] )
    ]
