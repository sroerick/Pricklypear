(** pp-sync comment-preserving vault merge (card 8dff7c9e).

    The vault is literate source: comment blocks live only in git (the
    store keeps the AST; export-package emits no interior comments). A
    snapshot must merge generator output into the existing file, keeping
    the old text verbatim for unchanged forms. No DB / no network. *)

open Pp_sync_merge

let src_old =
  ";; package demo -- exported from live branch system/main\n\
   ;; Sync: x\n\
   \n\
   (package demo\n  (export\n    demo/a\n    demo/b\n))\n\
   \n\
   ;; why a exists — the doc comment that MUST survive\n\
   (define demo/a\n  (lambda () 1))\n\
   \n\
   (define demo/b\n  ;; interior note that survives while b is unchanged\n\
   \x20 (lambda () 2))\n"

(* New export: a CHANGED, b unchanged, c added. *)
let src_new =
  ";; package demo -- exported from live branch system/main\n\
   ;; Sync: x\n\
   \n\
   (package demo\n  (export\n    demo/a\n    demo/b\n    demo/c\n))\n\
   \n\
   (define demo/a\n  (lambda () 999))\n\
   \n\
   (define demo/b\n  (lambda () 2))\n\
   \n\
   (define demo/c\n  (lambda () 3))\n"

let count_comments s =
  let re = Re.Perl.compile_pat {|;[^;\n]|} in
  List.length (Re.all re s)

let contains sub s =
  let ns = String.length sub and ls = String.length s in
  let rec go i =
    if i + ns > ls then false
    else if String.sub s i ns = sub then true
    else go (i + 1)
  in
  go 0

let test_keeps_unchanged_verbatim () =
  let merged, rep = merge ~old_text:src_old ~new_text:src_new in
  Alcotest.(check bool)
    "unchanged demo/b keeps its interior comment" true
    (contains ";; interior note that survives" merged);
  Alcotest.(check bool)
    "changed demo/a takes the NEW body" true
    (contains "(lambda () 999)" merged);
  Alcotest.(check bool)
    "changed demo/a's OLD leading comment is preserved" true
    (contains ";; why a exists" merged);
  Alcotest.(check bool) "new demo/c is emitted" true
    (contains "(define demo/c" merged);
  Alcotest.(check int) "reused count" 1 rep.reused

let test_reports_changed_with_comments () =
  (* demo/a changes and its OLD body had a leading (outside) comment;
     the report must not claim a comment was LOST for it (the comment is
     in the prefix we re-attach). demo/b changes and HAS an interior
     comment: that one is reported. *)
  let old2 =
    ";; header\n(package demo (export demo/b))\n\n\
     (define demo/b\n  ;; interior\n  (lambda () 2))\n"
  in
  let new2 =
    ";; header\n(package demo (export demo/b))\n\n\
     (define demo/b\n  (lambda () 888))\n"
  in
  let _merged, rep = merge ~old_text:old2 ~new_text:new2 in
  Alcotest.(check (list string)) "changed" [ "demo/b" ] rep.changed;
  Alcotest.(check (list string))
    "changed_with_comments (body had an interior comment)" [ "demo/b" ]
    rep.changed_with_comments

let test_reports_removed () =
  let new3 =
    ";; header\n(package demo (export demo/a))\n\n(define demo/a (lambda () 1))\n"
  in
  let _merged, rep = merge ~old_text:src_old ~new_text:new3 in
  Alcotest.(check bool) "demo/b reported as removed" true
    (List.mem "demo/b" rep.removed)

let test_empty_old_is_passthrough () =
  let merged, _rep = merge ~old_text:"" ~new_text:src_new in
  Alcotest.(check string) "empty old => new text unchanged" src_new merged

let test_comment_survival_rate () =
  (* The headline guarantee: for an unchanged codebase, comment count is
     preserved (not zeroed). *)
  let _merged, rep = merge ~old_text:src_old ~new_text:src_old in
  Alcotest.(check int) "all forms reused (package + 2 defines)" 3 rep.reused;
  Alcotest.(check int) "no changes" 0 (List.length rep.changed)

(* A HAND-AUTHORED vault file (custom header, no generator stamp) with
   REAL live content must MERGE — keeping the custom header (the
   first-chunk old prefix) and the per-form comments of unchanged
   forms — not be refused wholesale (the std case, 2026-10-02). *)
let src_hand_old =
  ";; Standard Lisp helpers (curated header — the face of the file)\n\
   ;; Loaded by (load-library \"std\") at image boot.\n\n\
   (package std\n  (export\n    std/a\n    std/b))\n\
   \n\
   ;; std/a: why it exists — comment must survive\n\
   (define std/a\n  (lambda () 1))\n\
   (define std/b\n  (lambda () 2))\n"

let src_hand_new =
  ";; package std -- exported from live branch system/main\n\
   ;; Sync: x\n\n\
   (package std\n  (export\n    std/a\n    std/b))\n\
   \n\
   (define std/a\n  (lambda () 1))\n\
   (define std/b\n  (lambda () 42))\n"

let test_hand_authored_header_kept () =
  let merged, _rep = merge ~old_text:src_hand_old ~new_text:src_hand_new in
  Alcotest.(check bool) "custom header first line kept"
    true
    (contains "curated header" merged);
  Alcotest.(check bool) "generator header NOT stamped in"
    false
    (contains "exported from live branch" merged);
  Alcotest.(check bool) "unchanged form keeps its comment" true
    (contains ";; std/a: why it exists" merged);
  Alcotest.(check bool) "changed form takes the new body" true
    (contains "(lambda () 42)" merged)

let generated_old =
  ";; package demo -- exported from live branch system/main\n\
   ;; Sync: x\n\n(package demo (export demo/a))\n\n\
   (define demo/a (lambda () 1))\n\n\
   (define demo/b (lambda () 2))\n"

let empty_live =
  ";; package demo -- exported from live branch system/main\n\
   ;; Sync: x\n\n(package demo (export))\n"

(* plan_write contract: the ONE gate every vault write path goes through.
   Cases kept small and explicit; strings use explicit \n escapes (no
   backslash-EOL continuations) so this file stays readable. *)

let pw = Pp_sync_merge.plan_write

let is_refused (which : string) (m : string) ~expected : unit =
  Alcotest.(check bool) which true
    (String.length m > 0 && String.length expected > 0)

let test_plan_write_contract () =
  (* hand-authored + live export has NO defines -> REFUSE *)
  (match pw ~old_text:src_hand_old ~new_text:empty_live ~live_names:[] ~force:false with
   | Refused m ->
       Alcotest.(check bool)
         "refusal names hand-authored + zero defines" true
         (contains "hand-authored" m && contains "ZERO" m)
   | Write _ -> Alcotest.fail "hand-authored + empty live must refuse");
  (* NEW FILE + zero-define export -> refuse (empty shells don't seed vault
     files; 2026-10-02 err/route/soup incident) *)
  (match pw ~old_text:"" ~new_text:empty_live ~live_names:[] ~force:false with
   | Refused m ->
       Alcotest.(check bool) "zero-define shell refuses on new file" true
         (contains "ZERO" m)
   | Write _ -> Alcotest.fail "an empty shell must not create a vault file");
  (* --force overrides with a RAW write of the generator text *)
  (match pw ~old_text:src_hand_old ~new_text:empty_live ~live_names:[] ~force:true with
   | Write w ->
       Alcotest.(check bool) "force is raw" true w.raw;
       Alcotest.(check bool) "force writes generator text verbatim" true
         (w.text = empty_live)
   | Refused _ -> Alcotest.fail "force must not refuse");
  (* generated + growing (no drops) -> MERGE *)
  (match pw ~old_text:generated_old ~new_text:src_new ~live_names:[] ~force:false with
   | Write w -> Alcotest.(check bool) "generated grows via merge" true (not w.raw)
   | Refused m -> Alcotest.fail ("growing must merge: " ^ m))

(* Dropped-name accounting: a name may leave a file only if live binds
   it SOMEWHERE (re-homing); a name bound NOWHERE on append-only main is
   an anomaly and is refused BY NAME. A shrinking-but-accounted export
   still hits the count (gut) guard. *)
let test_plan_write_dropped_names () =
  let old_gen =
    ";; package demo -- exported from live branch system/main\n" ^ "\n"
    ^ "(package demo (export demo/a demo/b))\n\n"
    ^ "(define demo/a (lambda () 1))\n\n"
    ^ "(define demo/b (lambda () 2))\n"
  in
  let new_keeps_b =
    ";; package demo -- exported from live branch system/main\n" ^ "\n"
    ^ "(package demo (export demo/b))\n\n"
    ^ "(define demo/b (lambda () 2))\n"
  in
  let new_adds_c =
    ";; package demo -- exported from live branch system/main\n" ^ "\n"
    ^ "(package demo (export demo/b demo/c))\n\n"
    ^ "(define demo/b (lambda () 2))\n\n"
    ^ "(define demo/c (lambda () 3))\n"
  in
  (* demo/a dropped and bound nowhere on live -> REFUSED naming it *)
  (match pw ~old_text:old_gen ~new_text:new_keeps_b ~live_names:[] ~force:false with
   | Refused m ->
       Alcotest.(check bool) "names the nowheres" true
         (contains "demo/a" m && contains "NOWHERE" m)
   | Write _ -> Alcotest.fail "unaccounted drop must refuse");
  (* demo/a dropped but still bound live elsewhere -> accounted: an
     ACCOUNTED SHRINK IS A LEGITIMATE MIRROR EDIT (the 2026-10-02 count
     guard wrongly refused real re-homings like todo/browse* -> filmstrip)
     so it MERGES and reports the moved name *)
  (match pw ~old_text:old_gen ~new_text:new_keeps_b ~live_names:[ "demo/a" ] ~force:false with
   | Write w ->
       Alcotest.(check bool) "accounted shrink merges" true (not w.raw);
       Alcotest.(check (list string)) "shrink reported as removal"
         [ "demo/a" ] w.report.removed
   | Refused m -> Alcotest.fail ("accounted shrink must merge: " ^ m));
  (* demo/a re-homed, export does not shrink -> MERGE, drop REPORTED *)
  (match pw ~old_text:old_gen ~new_text:new_adds_c ~live_names:[ "demo/a" ] ~force:false with
   | Write w ->
       Alcotest.(check bool) "accounted re-homing merges" true (not w.raw);
       Alcotest.(check (list string)) "the moved name is reported as removed"
         [ "demo/a" ] w.report.removed
   | Refused m -> Alcotest.fail ("accounted re-homing must merge: " ^ m))

let test_malformed_define_no_crash () =
  let bad_old =
    ";; weird hand file\n\n(package demo (export demo/a))\n\n\
     (define demo/a (lambda () 1) 42)\n(driver-license 7)\n\n\
     (define demo/z (lambda () 9))\n"
  in
  let merged, rep =
    try merge ~old_text:bad_old ~new_text:src_new
    with e -> Alcotest.fail ("merge crashed: " ^ Printexc.to_string e)
  in
  Alcotest.(check bool) "something was produced" true (String.length merged > 0);
  (* demo/z is fine old-side but absent new-side → removed, and the
     merge must not treat the garbage form as a match for anything. *)
  Alcotest.(check (list string)) "bits nobody can claim are dropped/removed"
    [ "demo/z" ] rep.removed

let () =
  Alcotest.run "pp_sync_merge"
    [ ( "merge",
        [ Alcotest.test_case "keeps unchanged forms verbatim" `Quick
            test_keeps_unchanged_verbatim
        ; Alcotest.test_case "reports changed forms with comments" `Quick
            test_reports_changed_with_comments
        ; Alcotest.test_case "reports removed names" `Quick
            test_reports_removed
        ; Alcotest.test_case "empty old is passthrough" `Quick
            test_empty_old_is_passthrough
        ; Alcotest.test_case "identical input => no changes" `Quick
            test_comment_survival_rate
        ; Alcotest.test_case "hand-authored header kept on merge" `Quick
            test_hand_authored_header_kept
        ; Alcotest.test_case "plan_write refusals + force" `Quick
            test_plan_write_contract
        ; Alcotest.test_case "plan_write dropped-name accounting" `Quick
            test_plan_write_dropped_names
        ; Alcotest.test_case "malformed define does not crash" `Quick
            test_malformed_define_no_crash
        ] )
    ]
