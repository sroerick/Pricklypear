(* Vault-shape rules (i-1788671331-d): one file per package family.
   The old discovery inferred sub-packages (rindex over binding names) so
   pull-all exported notes/capture, todo/item, … as stale nested dumps the
   boot seed never reads — ~230 double-bookkept defines, already drifted.
   These tests pin the family collapse + the refusal contract. *)

open Pp_sync_vault

let contains sub s =
  let ln = String.length sub and ls = String.length s in
  let rec go i = if i + ln > ls then false else String.sub s i ln = sub || go (i + 1) in
  go 0

let test_package_family () =
  Alcotest.(check string) "top-level stays" "notes" (package_family "notes");
  Alcotest.(check string) "two segments" "notes"
    (package_family "notes/capture");
  Alcotest.(check string) "three segments collapse to FIRST segment" "notes"
    (package_family "notes/capture/add");
  Alcotest.(check string) "deep" "a" (package_family "a/b/c/d");
  Alcotest.(check string) "empty" "" (package_family "")

let test_packages_of () =
  let dirs = [ "notes"; "todo"; "std" ] in
  (* live binding names incl. the sub-prefix shapes that used to become
     phantom pull targets; bare kernel-prim names carry NO package
     evidence (the old inference ignored them too) *)
  let bindings =
    [ "notes/capture/add"
    ; "notes/tree/get"
    ; "notes/page"
    ; "todo/item/assign"
    ; "todo/state/next"
    ; "std/map"
    ; "capture/stray"
    ; "car"
    ; "+"
    ; "(root)" ] in
  Alcotest.(check (list string)) "families sorted + deduped"
    [ "capture"; "notes"; "std"; "todo" ]
    (packages_of ~dirs ~bindings);
  Alcotest.(check (list string)) "no phantom sub-packages"
    []
    (List.filter
       (fun p -> String.contains p '/')
       (packages_of ~dirs ~bindings));
  Alcotest.(check (list string)) "bare prim names are not packages" []
    (packages_of ~dirs:[] ~bindings:[ "car"; "+"; "(root)" ]);
  Alcotest.(check (list string)) "dirs alone" [ "notes" ]
    (packages_of ~dirs:[ "notes" ] ~bindings:[]);
  Alcotest.(check (list string)) "bindings alone" [ "todo" ]
    (packages_of ~dirs:[] ~bindings:[ "todo/item/add" ]);
  (* main binds a stray define literally named "/" (REPL debris): its
     family is EMPTY — discovery must not invent family ""→_root
     (2026-10-02 dry-run found libs/_root would be written) *)
  Alcotest.(check (list string)) "stray '/' cannot invent _root"
    [] (packages_of ~dirs:[] ~bindings:[ "/"; "/x/" ])

let test_vault_refusal () =
  let refused p =
    match vault_refusal p with Some _ -> true | None -> false in
  Alcotest.(check bool) "root refused" true (refused "(root)");
  Alcotest.(check bool) "root bare refused" true (refused "root");
  Alcotest.(check bool) "sub-prefix refused" true (refused "notes/capture");
  Alcotest.(check bool) "deep sub-prefix refused" true
    (refused "todo/item/assign");
  Alcotest.(check bool) "family passes" false (refused "notes");
  Alcotest.(check bool) "plain package passes" false (refused "todo");
  (* banned families refuse loudly on explicit verbs (their live rows
     are known duplicates: libs/list was deleted — 6ed8656, 628416f) *)
  Alcotest.(check bool) "banned 'list' refused" true (refused "list");
  (match vault_refusal "notes/capture" with
   | Some msg ->
       Alcotest.(check bool) "refusal points at the family file" true
         (contains "libs/notes/lib.pp" msg)
   | None -> Alcotest.fail "expected refusal")

let () =
  let open Alcotest in
  run "sync_vault"
    [ ( "family-collapse"
      , [ test_case "package_family" `Quick test_package_family
        ; test_case "packages_of" `Quick test_packages_of ] )
    ; ( "refusals"
      , [ test_case "vault_refusal" `Quick test_vault_refusal ] ) ]
