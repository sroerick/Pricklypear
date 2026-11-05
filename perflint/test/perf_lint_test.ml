(** pp-perf-lint R1 tests: RED, AMBER, both negatives, and exit codes.

    Fixtures are the shapes the rule exists for:
      red-adopt-stranded.pp  todo/adopt-stranded: tree/get + tree/move per
                             datum of a 4000-row list
      amber-helper.pp        prim in a helper the loop lambda calls
      negative-collection.pp the prim IS the folded collection (runs once)
      negative-fetched-rows.pp a loop over rows already in hand *)

let fixture (name : string) : string = Filename.concat "fixtures" name

let describe (f : Perf_lint.finding) : string =
  Printf.sprintf "%d:%s:%s:%s" f.Perf_lint.line
    (Perf_lint.severity_name f.severity)
    f.prim
    (match f.via with None -> "-" | Some v -> v)

let findings (name : string) : string list =
  List.map describe (Perf_lint.scan_file (fixture name))

let only (name : string) (p : Perf_lint.finding -> bool) : Perf_lint.finding list =
  List.filter p (Perf_lint.scan_file (fixture name))

let test_red_adopt_stranded () =
  Alcotest.(check (list string))
    "tree/get and tree/move inside the foldl lambda, tree/list-kind in the \
     outer type loop"
    [ "31:RED:tree/get:-"; "35:RED:tree/move:-"; "40:RED:tree/list-kind:-" ]
    (findings "red-adopt-stranded.pp");
  Alcotest.(check (list string))
    "RED findings name the enclosing define"
    [ "todo/adopt-stranded"; "todo/adopt-stranded"; "todo/adopt-stranded" ]
    (List.map
       (fun (f : Perf_lint.finding) -> f.define_name)
       (only "red-adopt-stranded.pp" (fun f -> f.severity = Perf_lint.Red)))

let test_amber_helpers () =
  Alcotest.(check (list string))
    "same-file define, let-bound lambda, and a package-local HOF wrapper"
    [ "9:AMBER:find-row:amber/load";
      "13:AMBER:tree/get:amber/peek";
      "36:AMBER:get:title-of" ]
    (findings "amber-helper.pp")

let test_negative_collection_is_not_a_loop () =
  Alcotest.(check (list string))
    "the prim is the collection, not the loop body"
    [] (findings "negative-collection.pp")

let test_negative_fetched_rows () =
  Alcotest.(check (list string))
    "loop over already-fetched rows calls no prim"
    [] (findings "negative-fetched-rows.pp")

let test_exit_code () =
  let red = Perf_lint.scan_file (fixture "red-adopt-stranded.pp") in
  let clean = Perf_lint.scan_file (fixture "negative-fetched-rows.pp") in
  Alcotest.(check int) "RED exits non-zero" 1 (Perf_lint.exit_code red);
  Alcotest.(check int) "clean exits zero" 0 (Perf_lint.exit_code clean)

let test_json_summary () =
  let fs =
    Perf_lint.scan_file (fixture "red-adopt-stranded.pp")
    @ Perf_lint.scan_file (fixture "amber-helper.pp")
  in
  let j = Perf_lint.report_json ~files:2 fs in
  let open Yojson.Safe.Util in
  let summary = member "summary" j in
  Alcotest.(check int) "red count" 3 (member "red" summary |> to_int);
  Alcotest.(check int) "amber count" 3 (member "amber" summary |> to_int);
  Alcotest.(check int) "findings count" 6 (member "findings" summary |> to_int)


(* The lint's DB prim set must stay the tier table plus the two extras;
   this also pins the table refactor in db_cost.ml to its old answers. *)
let test_db_prim_set () =
  let module D = Pricklypear_image_lib.Db_cost in
  Alcotest.(check bool) "tier table has no duplicates" true
    (List.length D.known_prims
     = List.length (List.sort_uniq compare D.known_prims));
  Alcotest.(check bool) "tree/list is a seq scan" true
    (D.static_cost "tree/list" = D.Seq_scan);
  Alcotest.(check bool) "tree/get is an index seek" true
    (D.static_cost "tree/get" = D.Index_seek);
  Alcotest.(check bool) "html/div is not a DB prim" true
    (D.static_cost "html/div" = D.Unknown);
  Alcotest.(check bool) "extras are in the lint set, non-prims are not" true
    (Perf_lint.is_db_prim "tree/list-kind"
    && Perf_lint.is_db_prim "datum/rows-for-item"
    && not (Perf_lint.is_db_prim "html/div"))
let () =
  Alcotest.run "pp_perf_lint"
    [
      ( "r1",
        [
          Alcotest.test_case "red adopt-stranded" `Quick
            test_red_adopt_stranded;
          Alcotest.test_case "amber helpers" `Quick test_amber_helpers;
          Alcotest.test_case "negative: collection prim" `Quick
            test_negative_collection_is_not_a_loop;
          Alcotest.test_case "negative: fetched rows" `Quick
            test_negative_fetched_rows;
          Alcotest.test_case "exit code" `Quick test_exit_code;
          Alcotest.test_case "json summary" `Quick test_json_summary;
          Alcotest.test_case "db prim set" `Quick test_db_prim_set;
        ] );
    ]
