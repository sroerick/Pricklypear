(** Pricklypear Image — Http_code file-serving tests (i-1788670545-5).

    Pins the typed read-error contract for the hard-public faces
    (/code/raw, /assets, /book.pdf, /src.tgz): read failures classify as
    Not_found | Too_large | Io_error, and Io_error's raw OS detail is
    carried for the ops log only (never interpolated into response
    bodies). Also pins the walk_tree TTL memo: same root inside the TTL
    returns the cached tree, a fresh root or an expired TTL re-walks.
    No DB / no live server. *)

open Pricklypear_image_lib

let pp_error fmt = function
  | Http_code.Not_found -> Format.fprintf fmt "not_found"
  | Http_code.Too_large -> Format.fprintf fmt "too_large"
  | Http_code.Io_error _ -> Format.fprintf fmt "io_error"

let error_eq a b =
  match (a, b) with
  | Http_code.Not_found, Http_code.Not_found -> true
  | Http_code.Too_large, Http_code.Too_large -> true
  | Http_code.Io_error _, Http_code.Io_error _ -> true
  | _ -> false

let error_t : Http_code.file_error Alcotest.testable =
  Alcotest.testable pp_error error_eq

let read_result_t = Alcotest.result Alcotest.string error_t

let scratch name =
  Filename.concat (Filename.get_temp_dir_name ()) ("pp_http_code_test_" ^ name)

let write path bytes =
  let oc = open_out path in
  output_string oc bytes;
  close_out oc

let test_read_file_within_typed_errors () =
  (* a regular file reads back whole *)
  let okp = scratch "ok.txt" in
  write okp "hello";
  Alcotest.(check read_result_t) "regular file reads" (Ok "hello")
    (Http_code.read_file_within ~max:100 okp);
  (* missing path -> Not_found (was Error (Printexc.to_string exn)) *)
  Alcotest.(check read_result_t) "missing file is Not_found"
    (Error Http_code.Not_found)
    (Http_code.read_file_within ~max:100 (scratch "definitely-missing"));
  (* non-regular file (directory) -> Not_found, no "not a file" string *)
  let dir = scratch "dir" in
  if Sys.file_exists dir then Unix.rmdir dir;
  Unix.mkdir dir 0o755;
  Alcotest.(check read_result_t) "directory is Not_found"
    (Error Http_code.Not_found) (Http_code.read_file_within ~max:100 dir);
  (* over the cap -> Too_large, distinct from Not_found *)
  let big = scratch "big.bin" in
  write big (String.make 2048 'x');
  Alcotest.(check read_result_t) "oversized file is Too_large"
    (Error Http_code.Too_large) (Http_code.read_file_within ~max:100 big);
  (* exactly at the cap reads fine *)
  let cap = scratch "cap.bin" in
  write cap (String.make 100 'x');
  Alcotest.(check read_result_t) "at-cap file reads" (Ok (String.make 100 'x'))
    (Http_code.read_file_within ~max:100 cap);
  (* unreadable (EACCES on open) -> Io_error; the detail string exists for
     the ops log and is nonempty, but the constructor — not its payload —
     is what response bodies key on. Skipped for root, which reads
     regardless of mode. *)
  if Unix.geteuid () <> 0 then begin
    let locked = scratch "locked.txt" in
    write locked "secret";
    Unix.chmod locked 0o000;
    (match Http_code.read_file_within ~max:100 locked with
     | Error (Http_code.Io_error detail) ->
         Alcotest.(check bool) "io detail nonempty" true (String.length detail > 0)
     | other ->
         Alcotest.failf "expected Io_error, got %s"
           (match other with
            | Error e -> pp_error Format.str_formatter e;
              Format.flush_str_formatter ()
            | Ok _ -> "ok"));
    Unix.chmod locked 0o644;
    Sys.remove locked
  end;
  Sys.remove okp;
  Sys.remove big;
  Sys.remove cap;
  Unix.rmdir dir

let rec file_rels (nodes : Http_code.tree_node list) : string list =
  List.concat_map
    (function
      | Http_code.Dir (_, kids) -> file_rels kids
      | Http_code.File (_, rel) -> [ rel ])
    nodes

let test_walk_tree_memo () =
  let root = scratch "tree" in
  if Sys.file_exists root then Unix.rmdir root;
  Unix.mkdir root 0o755;
  write (Filename.concat root "a.ml") "let a = 1\n";
  (* first call at t=100 walks and caches *)
  let t0 = Http_code.walk_tree_cached ~root ~now:100.0 ~ttl:60.0 in
  Alcotest.(check (list string)) "fresh walk sees a.ml" [ "a.ml" ] (file_rels t0);
  (* a new file within the TTL is not visible yet: memo hit *)
  write (Filename.concat root "b.ml") "let b = 2\n";
  let t1 = Http_code.walk_tree_cached ~root ~now:110.0 ~ttl:60.0 in
  Alcotest.(check (list string)) "memo hit inside ttl" [ "a.ml" ] (file_rels t1);
  (* after the TTL expires the walk re-runs and sees both *)
  let t2 = Http_code.walk_tree_cached ~root ~now:161.0 ~ttl:60.0 in
  Alcotest.(check (list string)) "expired ttl re-walks" [ "a.ml"; "b.ml" ]
    (file_rels t2);
  (* a different root never serves another root's cache *)
  let other = scratch "other-tree" in
  if Sys.file_exists other then Unix.rmdir other;
  Unix.mkdir other 0o755;
  let t3 = Http_code.walk_tree_cached ~root:other ~now:100.0 ~ttl:60.0 in
  Alcotest.(check (list string)) "distinct root gets its own walk" []
    (file_rels t3);
  Sys.remove (Filename.concat root "a.ml");
  Sys.remove (Filename.concat root "b.ml");
  Unix.rmdir root;
  Unix.rmdir other

let () =
  let open Alcotest in
  run "http_code"
    [ ("file-errors", [ test_case "typed read errors" `Quick
                          test_read_file_within_typed_errors ])
    ; ("tree-memo", [ test_case "walk_tree ttl memo" `Quick
                        test_walk_tree_memo ])
    ]
