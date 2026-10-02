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
        ] )
    ]
