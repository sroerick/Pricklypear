(** Packages prefix helpers *)
module P = Pricklypear_image_lib.Packages

let test_package_of_name () =
  Alcotest.(check string) "todo/list" "todo" (P.package_of_name "todo/list");
  Alcotest.(check string) "nested" "todo/ui" (P.package_of_name "todo/ui/row");
  Alcotest.(check string) "root" "" (P.package_of_name "hi-page")

let test_local_name () =
  Alcotest.(check string) "list" "list" (P.local_name "todo/list");
  Alcotest.(check string) "root local" "hi-page" (P.local_name "hi-page")

let test_join () =
  Alcotest.(check string) "join" "todo/list"
    (P.join_name ~package:"todo" ~local:"list");
  Alcotest.(check string) "root join" "greet"
    (P.join_name ~package:"" ~local:"greet");
  Alcotest.(check string) "strip slash" "todo/list"
    (P.join_name ~package:"todo/" ~local:"list")

let test_encode () =
  Alcotest.(check string) "encode" "todo%2Flist" (P.name_encode "todo/list");
  Alcotest.(check string) "decode" "todo/list" (P.name_decode "todo%2Flist")

let test_group () =
  let defs =
    [ ("hi-page", (), "a")
    ; ("todo/list", (), "a")
    ; ("todo/add", (), "a")
    ; ("calendar/today", (), "b")
    ]
  in
  let g = P.group_by_package defs in
  let pkgs = List.map fst g in
  Alcotest.(check (list string)) "pkg order" [""; "calendar"; "todo"] pkgs;
  let todo = List.assoc "todo" g in
  Alcotest.(check int) "todo count" 2 (List.length todo)

let () =
  Alcotest.run "pricklypear-packages"
    [ ( "packages"
      , [ ("package_of_name", `Quick, test_package_of_name)
        ; ("local_name", `Quick, test_local_name)
        ; ("join", `Quick, test_join)
        ; ("encode", `Quick, test_encode)
        ; ("group", `Quick, test_group)
        ] )
    ]
