(** i-1788670799-f parity gate: the browser boot prim table — the EXACT
    module client/bin links (Pp_client_table.build) — covers the shared
    pure catalog, and the 8 prims the old hand enumeration had dropped
    are present and callable (browser eval of any package reaching
    libs/std died on string-split before the fix). *)

let table () = Pp_client_table.build ()

let test_covers_catalog () =
  let t = table () in
  List.iter
    (fun name ->
       Alcotest.(check bool) ("registered: " ^ name) true
         (Hashtbl.mem t name))
    Prims_pure.catalog_names

(* The 8 prims the pre-fix client hand enumeration had dropped
   (i-1788670799-f). *)
let formerly_missing =
  [ "sort"; "string-join"; "string-split"; "prose-tokens"
  ; "render-tokens"; "err/shape"; "hx-get"; "hx-post" ]

let test_formerly_missing_present () =
  let t = table () in
  List.iter
    (fun name ->
       Alcotest.(check bool) ("present: " ^ name) true
         (Hashtbl.mem t name))
    formerly_missing

let test_string_split_works () =
  let t = table () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ ->
       Pricklypear_lisp.Env.define env name
         (Pricklypear_lisp.Ast.VPrim name))
    t;
  let v =
    Pricklypear_lisp.Eval.eval_string ~prims:t env
      "(string-split \"a,b,c\" \",\")"
  in
  Alcotest.(check string) "string-split" "(\"a\" \"b\" \"c\")"
    (Pricklypear_lisp.Reader.pp_value v)

let () =
  Alcotest.run "pp_client_table"
    [ ("parity",
       [ Alcotest.test_case "covers-catalog" `Quick test_covers_catalog
       ; Alcotest.test_case "formerly-missing-present" `Quick
           test_formerly_missing_present
       ; Alcotest.test_case "string-split-works" `Quick
           test_string_split_works ]) ]
