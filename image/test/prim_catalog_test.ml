(** i-1788670799-f parity: the image registry covers the shared pure
    catalog (pure/prims_pure.ml [Prims_pure.catalog_names]) and keeps
    its image-only extras layered on top. No DB. *)

let table () = Pricklypear_image_lib.Primitives.make_with_defaults ()

let test_covers_catalog () =
  let t = table () in
  List.iter
    (fun name ->
       Alcotest.(check bool) ("registered: " ^ name) true
         (Hashtbl.mem t name))
    Prims_pure.catalog_names

(* Image-only extras (layered ON TOP of the shared catalog):
   wall-clock prims (prims_time.ml reads Unix), sha1 (prims_crypto.ml),
   and the Mirage_crypto random-uuid shadow (the browser client keeps
   the pure MT19937 version). *)
let extras =
  [ "date-today"; "now"; "now-ms"; "date-weekday"; "date-format"
  ; "date-add-days"; "date-diff-days"; "sha1"; "random-uuid" ]

let test_extras_on_top () =
  let t = table () in
  List.iter
    (fun name ->
       Alcotest.(check bool) ("extra: " ^ name) true (Hashtbl.mem t name))
    extras

(* The bug class the catalog closes: libs/std reaches string-split, so
   a host missing it fails every std-touching eval with unbound var.
   Prove the shared entry is callable through the image registry. *)
let test_string_split_callable () =
  let t = table () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ ->
       Pricklypear_lisp.Env.define env name
         (Pricklypear_lisp.Ast.VPrim name))
    t;
  let v =
    Pricklypear_image_lib.Control_rim.run ~prims:t (fun () ->
        Pricklypear_lisp.Eval.eval_string_direct
          ~prims:Pricklypear_lisp.Eval.empty_prims env
          ~budget:(Pricklypear_lisp.Eval.make_budget ())
          "(string-split \"a,b,c\" \",\")")
  in
  Alcotest.(check string) "string-split" "(\"a\" \"b\" \"c\")"
    (Pricklypear_lisp.Reader.pp_value v)

let () =
  Alcotest.run "prim_catalog"
    [ ("parity",
       [ Alcotest.test_case "covers-catalog" `Quick test_covers_catalog
       ; Alcotest.test_case "extras-on-top" `Quick test_extras_on_top
       ; Alcotest.test_case "string-split-callable" `Quick
           test_string_split_callable ]) ]
