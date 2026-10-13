(** Tests for the opaque store-body codec on [Ast]
    (i-1788671083-c: the storage layer no longer links AST types —
    function bodies cross the store boundary as raw jsonb text and are
    parsed HERE, at the interpreter boundary, via
    [Ast.expr_of_body_json] / [Ast.expr_to_body_json]).
    PURE tests — no Postgres, no HTTP, no net. *)
open Pricklypear_lisp

(* --- round-trip over the expression shapes the store persists --- *)

let round_trips name (e : Ast.expr) =
  ( "body round-trip: " ^ name
  , `Quick
  , fun () ->
      let body = Ast.expr_to_body_json e in
      match Ast.expr_of_body_json body with
      | Some back ->
          Alcotest.(check string)
            ("round-trip " ^ name) (Reader.pp_expr e) (Reader.pp_expr back)
      | None -> Alcotest.fail ("failed to parse own body: " ^ body) )

let round_trip_tests =
  [ round_trips "lit int" (Ast.Lit (Ast.VInt 42L))
  ; round_trips "lit string w/ escapes"
      (Ast.Lit (Ast.VStr "a\"b\\c\nd"))
  ; round_trips "var" (Ast.Var "list/take")
  ; round_trips "lambda" (Ast.Lambda ([ "x"; "y" ], Ast.Var "x"))
  ; round_trips "app"
      (Ast.App (Ast.Var "+", [ Ast.Lit (Ast.VInt 1L); Ast.Lit (Ast.VInt 2L) ]))
  ; round_trips "let" (Ast.Let ("x", Ast.Lit (Ast.VInt 1L), Ast.Var "x"))
  ; round_trips "letrec"
      (Ast.Letrec ([ ("even", Ast.Var "odd") ], Ast.Var "even"))
  ; round_trips "if"
      (Ast.If (Ast.Var "p", Ast.Lit (Ast.VInt 1L), Ast.Lit (Ast.VInt 2L)))
  ; round_trips "prim" (Ast.Prim ("not", [ Ast.Lit (Ast.VBool false) ]))
  ; round_trips "seq"
      (Ast.Seq (Ast.Lit (Ast.VInt 1L), Ast.Lit (Ast.VInt 2L)))
  ; round_trips "quote" (Ast.Quote (Ast.Var "a"))
  ; round_trips "define"
      (Ast.Define ("f", Ast.Lambda ([], Ast.Lit (Ast.VNil))))
  ]

(* --- malformed / foreign bodies must be None, never an exception --- *)

let reject_tests =
  [ ( "empty string", "" )
  ; ( "truncated json", "{\"var\":" )
  ; ( "not an expr object", "[1,2,3]" )
  ; ( "json scalar", "\"plain string\"" )
  ; ( "unknown tag", "{\"widget\":1}" )
  ; ( "bad literal payload", "{\"lit\":{\"int\":\"NaN\"}}" )
  ]
  |> List.map (fun (name, body) ->
         ( "malformed body rejected: " ^ name
         , `Quick
         , fun () ->
             match Ast.expr_of_body_json body with
             | None -> ()
             | Some e ->
                 Alcotest.failf "expected None, parsed %s" (Reader.pp_expr e) ))

(* --- serialization is the exact jsonb text the store persists --- *)

let serialization_tests =
  [ ( "var serializes to tagged json"
    , `Quick
    , fun () ->
        Alcotest.(check string) "var body" "{\"var\":\"x\"}"
          (Ast.expr_to_body_json (Ast.Var "x")) )
  ; ( "compact (no spaces) — hash-domain friendly"
    , `Quick
    , fun () ->
        let body = Ast.expr_to_body_json (Ast.Var "x") in
        Alcotest.(check bool) "no whitespace" false
          (String.contains body ' ' || String.contains body '\n') )
  ]

let () =
  Alcotest.run "ast body codec"
    [ ("round-trip", round_trip_tests)
    ; ("malformed-reject", reject_tests)
    ; ("serialization", serialization_tests)
    ]
