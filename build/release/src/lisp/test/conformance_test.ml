(** Nopales core conformance — alcotest runner.

    The case data lives in the shared [pricklypear-conformance] library
    ([lisp/conformance/conformance.ml]) so the js_of_ocaml browser-target
    runner runs the SAME cases. Spec [lisp-interpreter.conformance]:
    this is the semantic gate of core-freeze. *)
open Pricklypear_lisp

open Pricklypear_lisp
open Pricklypear_conformance
open Conformance

let run_case c =
  ( c.Conformance.name,
    `Quick,
    fun () ->
      let prims, env = Conformance.setup_arith () in
      List.iter (fun s -> ignore (Eval.eval_string ~prims env s)) c.setups;
      let got = Conformance.eval_pp ~prims env c.src in
      Alcotest.(check string) c.name c.expected got )

(* Group cases by their declared group to preserve the test layout. *)
let groups =
  let tbl = Hashtbl.create 8 in
  List.iter (fun c ->
      let l = try Hashtbl.find tbl c.group with Not_found -> [] in
      Hashtbl.replace tbl c.group (run_case c :: l))
    Conformance.cases;
  Hashtbl.fold (fun g lst acc -> (g, List.rev lst) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> compare a b)

(* Error-handling cases: alcotest-specific (exception metadata), kept
   here rather than in the shared data so the JS runner stays a clean
   pass/fail loop. *)
let error_cases =
  [ ( "unbound has kind unbound_var",
      `Quick,
      fun () ->
        let prims, env = Conformance.setup_arith () in
        match
          try
            ignore (Eval.eval_string ~prims env "no-such-binding");
            None
          with Ast.Interp_error e -> Some e
        with
        | Some (Ast.Unbound_var "no-such-binding" as e) ->
            Alcotest.(check string) "kind" "unbound_var"
              (Ast.interp_error_kind e)
        | Some e -> Alcotest.failf "wrong: %s" (Ast.pp_interp_error e)
        | None -> Alcotest.fail "expected unbound" )
  ; ( "budget not caught by guard",
      `Quick,
      fun () ->
        let prims, env = Conformance.setup_arith () in
        ignore (Eval.eval_string ~prims env "(define loop (lambda () (loop)))");
        let raised =
          try
            let budget = Eval.make_budget ~limit:40 () in
            ignore
              (Control.run_sync ~prims (fun () ->
                   Eval.eval ~budget ~prims:Eval.empty_prims env
                     (Reader.read_one "(guard (lambda (e) 0) (loop))")));
            false
          with
          | Ast.Interp_error Ast.Budget_exceeded -> true
          | _ -> false
        in
        Alcotest.(check bool) "budget escapes" true raised )
  ; ( "parse error has line col",
      `Quick,
      fun () ->
        match
          try
            ignore (Reader.read_one "(foo");
            None
          with Reader.Reader_error (_, line, col) -> Some (line, col)
        with
        | Some (line, col) ->
            Alcotest.(check bool) "line" true (line >= 1);
            Alcotest.(check bool) "col" true (col >= 1)
        | None -> Alcotest.fail "expected parse error" )
  ]

let () =
  Alcotest.run "nopales-conformance" (groups @ [ "errors", error_cases ])
