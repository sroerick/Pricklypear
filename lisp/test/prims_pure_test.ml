(** Tests for pure/prims_pure.ml prim behaviors that bit us once:
    the html-decode named-entity table must emit valid UTF-8 (a
    Latin-1 nbsp/copy/reg/deg/plusmn row once injected invalid UTF-8
    into rendered HTML), and html-escape . html-decode must round-trip
    over the whole table. Also locks quotient/remainder arg policies.
    PURE tests — no Postgres, no HTTP, no net. *)
open Pricklypear_lisp

let is_valid_utf8 s =
  let n = String.length s in
  let rec loop i =
    if i >= n then true
    else
      let d = String.get_utf_8_uchar s i in
      Uchar.utf_decode_is_valid d && loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let decode_str s =
  match Prims_pure.prim_html_decode [ Ast.VStr s ] with
  | Ast.VStr out -> out
  | v -> Alcotest.failf "html-decode returned %s" (Reader.pp_value v)

let escape_str s =
  match Prims_pure.prim_html_escape [ Ast.VStr s ] with
  | Ast.VStr out -> out
  | v -> Alcotest.failf "html-escape returned %s" (Reader.pp_value v)

(* Every named entity decodes to bytes that are (a) the table's own
   replacement, (b) valid UTF-8, and (c) closed under the pair:
   escaping the decoded char then re-decoding is the identity, and
   decoding the escaped entity source ("&nbsp;" -> "&amp;nbsp;") is
   the identity too. *)
let entity_tests =
  List.map
    (fun (name, repl) ->
      ( "entity " ^ name
      , `Quick
      , fun () ->
          Alcotest.(check string)
            ("decode &" ^ name ^ ";") repl (decode_str ("&" ^ name ^ ";"));
          if not (is_valid_utf8 repl) then
            Alcotest.failf "&%s; decodes to invalid UTF-8 (%d bytes)" name
              (String.length repl);
          Alcotest.(check string)
            ("escape-decode round-trip " ^ name) repl (decode_str (escape_str repl));
          Alcotest.(check string)
            ("escaped-source round-trip " ^ name)
            ("&" ^ name ^ ";")
            (decode_str (escape_str ("&" ^ name ^ ";"))) ))
    Prims_pure.named_entities

(* The original bug: nbsp decoded to the single Latin-1 byte 0xF0 (an
   invalid UTF-8 lead byte). Lock the correct two-byte form U+00A0. *)
let nbsp_regression =
  ( "nbsp is U+00A0 (C2 A0), not lone 0xF0"
  , `Quick
  , fun () -> Alcotest.(check string) "nbsp bytes" "\194\160" (decode_str "&nbsp;") )

(* quotient / remainder: int paths, zero divisor, non-int and
   wrong-arity args all reject through the single catch-all. *)
let raises_interp_error msg f =
  match f () with
  | v ->
      Alcotest.failf "expected Interp_error (%s), got %s" msg (Reader.pp_value v)
  | exception Ast.Interp_error (Ast.User_error (m, _)) ->
      if m <> msg then Alcotest.failf "expected %s, got %s" msg m
  | exception e -> Alcotest.failf "expected Interp_error, got %s" (Printexc.to_string e)

let as_int what v =
  match v with
  | Ast.VInt n -> Int64.to_int n
  | v -> Alcotest.failf "%s: expected VInt, got %s" what (Reader.pp_value v)

let quotient_tests =
  [ ( "quotient int path"
    , `Quick
    , fun () ->
        Alcotest.(check int) "7/2" 3
          (as_int "quotient"
             (Prims_pure.prim_quotient [ Ast.VInt 7L; Ast.VInt 2L ])) )
  ; ( "remainder int path"
    , `Quick
    , fun () ->
        Alcotest.(check int) "7 rem 2" 1
          (as_int "remainder"
             (Prims_pure.prim_remainder [ Ast.VInt 7L; Ast.VInt 2L ])) )
  ; ( "quotient by zero"
    , `Quick
    , fun () ->
        raises_interp_error "quotient by zero" (fun () ->
            Prims_pure.prim_quotient [ Ast.VInt 1L; Ast.VInt 0L ]) )
  ; ( "remainder by zero"
    , `Quick
    , fun () ->
        raises_interp_error "remainder by zero" (fun () ->
            Prims_pure.prim_remainder [ Ast.VInt 1L; Ast.VInt 0L ]) )
  ; ( "quotient rejects floats"
    , `Quick
    , fun () ->
        raises_interp_error "quotient needs two int args" (fun () ->
            Prims_pure.prim_quotient [ Ast.VFloat 1.5; Ast.VInt 2L ]) )
  ; ( "quotient rejects wrong arity"
    , `Quick
    , fun () ->
        raises_interp_error "quotient needs two int args" (fun () ->
            Prims_pure.prim_quotient [ Ast.VInt 1L ]) )
  ; ( "remainder rejects floats"
    , `Quick
    , fun () ->
        raises_interp_error "remainder needs two int args" (fun () ->
            Prims_pure.prim_remainder [ Ast.VFloat 1.5; Ast.VInt 2L ]) )
  ; ( "remainder rejects wrong arity"
    , `Quick
    , fun () ->
        raises_interp_error "remainder needs two int args" (fun () ->
            Prims_pure.prim_remainder [ Ast.VInt 1L ]) ) ]

let () =
  Alcotest.run "prims_pure"
    [ ("entities", entity_tests @ [ nbsp_regression ])
    ; ("quotient-remainder", quotient_tests) ]
