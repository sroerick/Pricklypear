(** Tests for the shared value + error codecs in pure/prims_pure.ml
    (borge/remote-eval.borg substrate).
    PURE tests — no Postgres, no HTTP, no net. They verify that
    Prims_pure.value_to_json / json_to_value and the factored
    error_to_json / error_of_json round-trip losslessly over the wire
    shape both /api/eval and a proxy rim link. *)
open Pricklypear_lisp

(* --- value codec round-trip --- *)

let value_round_trips name expected =
  ( "value round-trip: " ^ name
  , `Quick
  , fun () ->
      let j = Prims_pure.value_to_json expected in
      let back = Prims_pure.json_to_value j in
      Alcotest.(check string)
        ("round-trip " ^ name)
        (Reader.pp_value expected)
        (Reader.pp_value back) )

let round_trip_tests =
  [ value_round_trips "VStr" (Ast.VStr "hello")
  ; value_round_trips "VInt" (Ast.VInt 42L)
  ; value_round_trips "negative VInt" (Ast.VInt (-7L))
  ; value_round_trips "VFloat" (Ast.VFloat 3.14)
  ; value_round_trips "integral VFloat stays VFloat" (Ast.VFloat 5.0)
  ; ( "value round-trip: VFloat identity (constructor kept)"
    , `Quick
    , fun () ->
        let j = Prims_pure.value_to_json (Ast.VFloat 3.14) in
        match Prims_pure.json_to_value j with
        | Ast.VFloat f ->
            if abs_float (f -. 3.14) > 1e-9 then
              Alcotest.failf "float value %g" f
        | other ->
            Alcotest.failf "expected VFloat, got %s" (Reader.pp_value other) )
  ; value_round_trips "VBool true" (Ast.VBool true)
  ; value_round_trips "VBool false" (Ast.VBool false)
  ; value_round_trips "VNil" Ast.VNil
  ; value_round_trips "VList" (Ast.VList [ Ast.VInt 1L; Ast.VStr "a"; Ast.VBool true ])
  ; value_round_trips "nested VList" (Ast.VList [ Ast.VList [ Ast.VInt 1L ]; Ast.VNil ])
  ; ( "value round-trip: VPair (dotted) via list wire form"
    , `Quick
    , fun () ->
        (* Pairs are normalized to their list wire form: (1 . 2) travels
           as [1,2] and comes back as VList [1;2]. Data is preserved
           losslessly even though the constructor normalizes. *)
        let expected = Ast.VPair (Ast.VInt 1L, Ast.VInt 2L) in
        let j = Prims_pure.value_to_json expected in
        let back = Prims_pure.json_to_value j in
        Alcotest.(check string)
          "pair-list wire form" (Reader.pp_value (Ast.VList [ Ast.VInt 1L; Ast.VInt 2L ]))
          (Reader.pp_value back) )
  ; ( "value round-trip: VPair proper list via list wire form"
    , `Quick
    , fun () ->
        let expected =
          Ast.VPair (Ast.VInt 1L, Ast.VPair (Ast.VInt 2L, Ast.VNil))
        in
        let j = Prims_pure.value_to_json expected in
        let back = Prims_pure.json_to_value j in
        Alcotest.(check string)
          "pair list wire form" (Reader.pp_value (Ast.VList [ Ast.VInt 1L; Ast.VInt 2L ]))
          (Reader.pp_value back) )
  ; value_round_trips "JSON-shaped VStr stays structured"
      (Ast.VStr "{\"a\":1,\"b\":[true,false]}")
  ]

(* Non-round-trippable values: VLambda/VPrim/VMacro serialize to a
   sentinel string and come back as a VStr (never the original). *)
let non_round_trip_sentinel name v =
  ( "value sentinel: " ^ name
  , `Quick
  , fun () ->
      let j = Prims_pure.value_to_json v in
      let back = Prims_pure.json_to_value j in
      (match back with
       | Ast.VStr _ -> ()
       | other ->
           Alcotest.failf
             "%s should come back as a VStr sentinel, got %s"
             name (Reader.pp_value other)) )

let sentinel_tests =
  [ non_round_trip_sentinel "VLambda" Ast.(VLambda ([ "x" ], Var "x", { bindings = Hashtbl.create 0; parent = None }))
  ; non_round_trip_sentinel "VPrim" (Ast.VPrim "+")
  ; non_round_trip_sentinel "VMacro" Ast.(VMacro ([ "x" ], Var "x", { bindings = Hashtbl.create 0; parent = None }))
  ]

(* On the wire, JSON strings map to VStr; a VSymbol handed to
   value_to_json does NOT come back as a VSymbol (no symbols on the
   wire — symbols are a read-side concern). *)
let () =
  ()

let vsymbol_wire =
  ("value symbol: no VSymbol on wire", `Quick, fun () ->
    let j = Prims_pure.value_to_json (Ast.VSymbol "foo") in
    match Prims_pure.json_to_value j with
    | Ast.VStr _ -> ()
    | other ->
        Alcotest.failf
          "VSymbol should come back as VStr, got %s" (Reader.pp_value other))

(* --- error codec round-trip --- *)

let err_round_trips name (expected : exn) =
  ( "error round-trip: " ^ name
  , `Quick
  , fun () ->
      let j = Prims_pure.error_to_json expected in
      let back = Prims_pure.error_of_json j in
      (* Re-raise/pattern-match both to compare structurally. *)
      let canon (e : exn) =
        match e with
        | Ast.Interp_error ie -> "interp:" ^ Ast.pp_interp_error ie
        | Reader.Reader_error (m, l, c) -> Printf.sprintf "reader:%s:%d:%d" m l c
        | _ -> Printexc.to_string e
      in
      Alcotest.(check string)
        ("error round-trip " ^ name)
        (canon expected) (canon back) )

let error_tests =
  [ err_round_trips "Prim_error"
      (Ast.Interp_error (Ast.Prim_error ("foo", "boom", "my-kind")))
  ; err_round_trips "Prim_error with kind arity"
      (Ast.Interp_error (Ast.Prim_error ("get", "no such row", "not-found")))
  ; err_round_trips "Unbound_var"
      (Ast.Interp_error (Ast.Unbound_var "missing"))
  ; err_round_trips "Reader_error"
      (Reader.Reader_error ("unterminated list", 3, 17))
  ; err_round_trips "Reader_error empty list"
      (Reader.Reader_error ("unexpected end of input", 0, 0))
  ; err_round_trips "Budget_exceeded"
      (Ast.Interp_error Ast.Budget_exceeded)
  ; err_round_trips "User_error"
      (Ast.Interp_error (Ast.User_error ("needs 2 strings", "bad-args")))
  ; err_round_trips "Payload error kind stays"
      (Ast.Interp_error (Ast.User_error ("boom", "server_only")))
  ]

(* wire-shape assertions: the error JSON carries ok:false and the
   kind/name/line/col fields /api/eval clients already read. *)
let wire_shape =
  ("error wire shape", `Quick, fun () ->
    let open Yojson.Safe in
    let j =
      Prims_pure.error_to_json
        (Ast.Interp_error (Ast.Prim_error ("get", "no row", "not-found")))
    in
    (match j with
     | `Assoc fields ->
         let get k = List.assoc_opt k fields in
         (match get "ok", get "kind", get "name" with
          | Some (`Bool false), Some (`String "not-found"), Some (`String "get") -> ()
          | _ -> Alcotest.failf "unexpected Prim_error wire: %s" (to_string j))
     | _ -> Alcotest.failf "expected assoc, got %s" (to_string j));
    let j2 =
      Prims_pure.error_to_json
        (Ast.Interp_error (Ast.Unbound_var "x"))
    in
    (match j2 with
     | `Assoc fields ->
         (match List.assoc_opt "name" fields, List.assoc_opt "error" fields with
          | Some (`String "x"), Some (`String "unbound: x") -> ()
          | _ -> Alcotest.failf "unexpected Unbound_var wire: %s" (to_string j2))
     | _ -> Alcotest.failf "expected assoc, got %s" (to_string j2));
    let j3 =
      Prims_pure.error_to_json (Reader.Reader_error ("bad", 1, 2))
    in
    (match j3 with
     | `Assoc fields ->
         (match List.assoc_opt "kind" fields, List.assoc_opt "line" fields,
                List.assoc_opt "col" fields with
          | Some (`String "parse_error"), Some (`Int 1), Some (`Int 2) -> ()
          | _ -> Alcotest.failf "unexpected Reader_error wire: %s" (to_string j3))
     | _ -> Alcotest.failf "expected assoc, got %s" (to_string j3)) )

let () =
  Alcotest.run "codec"
    [ ( "value", round_trip_tests @ sentinel_tests @ [ vsymbol_wire ] )
    ; ("error", error_tests @ [ wire_shape ]) ]
