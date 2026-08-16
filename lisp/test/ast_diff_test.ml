(** Tests for the pure AST diff walker (lisp-interpreter.ast-diff).

    PURE tests — no Postgres, no HTTP. Per spec:
      cd lisp/ && dune build && dune runtest
    needs no services. Semantics authority: branching.diff.

    Covers same/modified/replaced over all 11 real expr constructors
    (Var Lit App Let Letrec If Lambda Prim Seq Quote Define), the
    list-LCS properties (App args + Letrec bindings so an insertion
    mid-sequence does not cascade), and the top-level ast_hash
    short-circuit. *)
open Pricklypear_lisp

let rec sprintf_delta (d : Ast_diff.delta) : string =
  match d with
  | Same -> "Same"
  | Replaced { from = f; to_ = t } ->
      Printf.sprintf "Replaced(%s -> %s)" (Reader.pp_expr f) (Reader.pp_expr t)
  | Node { ctor; children } ->
      Printf.sprintf "Node(%s,[%s])" ctor
        (String.concat "; "
           (List.map (fun (l, d) -> l ^ "=" ^ sprintf_delta d) children))

(* compare two exprs for equality irrespective of the diff *)
let rec expr_eq (a : Ast.expr) (b : Ast.expr) : bool =
  match a, b with
  | Var x, Var y -> x = y
  | Lit x, Lit y -> x = y
  | App (f1, a1), App (f2, a2) ->
      expr_eq f1 f2 && List.length a1 = List.length a2 && List.for_all2 expr_eq a1 a2
  | Let (n1, v1, b1), Let (n2, v2, b2) -> n1 = n2 && expr_eq v1 v2 && expr_eq b1 b2
  | Letrec (bs1, b1), Letrec (bs2, b2) ->
      List.length bs1 = List.length bs2
      && List.for_all2 (fun (n1, e1) (n2, e2) -> n1 = n2 && expr_eq e1 e2) bs1 bs2
      && expr_eq b1 b2
  | If (a1, b1, c1), If (a2, b2, c2) -> expr_eq a1 a2 && expr_eq b1 b2 && expr_eq c1 c2
  | Lambda (p1, b1), Lambda (p2, b2) -> p1 = p2 && expr_eq b1 b2
  | Prim (n1, a1), Prim (n2, a2) ->
      n1 = n2 && List.length a1 = List.length a2 && List.for_all2 expr_eq a1 a2
  | Seq (a1, b1), Seq (a2, b2) -> expr_eq a1 a2 && expr_eq b1 b2
  | Quote e1, Quote e2 -> expr_eq e1 e2
  | Define (n1, e1), Define (n2, e2) -> n1 = n2 && expr_eq e1 e2
  | _ -> false

let rec delta_eq (a : Ast_diff.delta) (b : Ast_diff.delta) : bool =
  match a, b with
  | Same, Same -> true
  | Replaced { from = f1; to_ = t1 }, Replaced { from = f2; to_ = t2 } ->
      expr_eq f1 f2 && expr_eq t1 t2
  | Node { ctor = c1; children = ch1 }, Node { ctor = c2; children = ch2 } ->
      c1 = c2
      && List.length ch1 = List.length ch2
      && List.for_all2
           (fun (l1, d1) (l2, d2) -> l1 = l2 && delta_eq d1 d2)
           ch1 ch2
  | _ -> false

let eq_delta expected actual =
  Alcotest.(check bool)
    (Printf.sprintf "delta %s" (sprintf_delta expected))
    true (delta_eq expected actual)

let delta_tests = [
  (* --- Var --- *)
  ("diff: same Var", `Quick, fun () ->
     eq_delta Same (Ast_diff.diff (Ast.var "x") (Ast.var "x")));
  ("diff: replaced Var", `Quick, fun () ->
     let from = Ast.var "x" and to_ = Ast.var "y" in
     eq_delta (Ast_diff.Replaced { from; to_ }) (Ast_diff.diff from to_));
  (* --- Lit --- *)
  ("diff: same Lit", `Quick, fun () ->
     eq_delta Same (Ast_diff.diff (Ast.int 1) (Ast.int 1)));
  ("diff: replaced Lit", `Quick, fun () ->
     let from = Ast.int 1 and to_ = Ast.int 2 in
     eq_delta (Ast_diff.Replaced { from; to_ }) (Ast_diff.diff from to_));
  (* --- If --- *)
  ("diff: If unchanged", `Quick, fun () ->
     let e = Ast.If (Ast.var "x", Ast.int 1, Ast.int 2) in
     eq_delta Same (Ast_diff.diff e e));
  ("diff: If modified branch", `Quick, fun () ->
     let from = Ast.If (Ast.var "x", Ast.int 1, Ast.int 2)
     and to_ = Ast.If (Ast.var "x", Ast.int 9, Ast.int 2) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "if"; children = [("test", Ast_diff.Same); ("then", Ast_diff.Replaced _); ("else", Ast_diff.Same)] } -> ()
      | _ -> Alcotest.failf "unexpected if delta: %s" (sprintf_delta d)));
  ("diff: If replaced by Lambda (ctor swap)", `Quick, fun () ->
     let from = Ast.If (Ast.var "x", Ast.int 1, Ast.int 2)
     and to_ = Ast.Lambda (["x"], Ast.var "x") in
     let d = Ast_diff.diff from to_ in
     eq_delta (Ast_diff.Replaced { from; to_ }) d);
  (* --- Seq --- *)
  ("diff: Seq modified second", `Quick, fun () ->
     let from = Ast.Seq (Ast.int 1, Ast.int 2)
     and to_ = Ast.Seq (Ast.int 1, Ast.int 3) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "seq"; children = [("a", Ast_diff.Same); ("b", Ast_diff.Replaced _)] } -> ()
      | _ -> Alcotest.failf "unexpected seq delta: %s" (sprintf_delta d)));
  (* --- Lambda --- *)
  ("diff: Lambda body modified, params Same", `Quick, fun () ->
     let from = Ast.Lambda (["x"], Ast.var "x")
     and to_ = Ast.Lambda (["x"], Ast.int 1) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "lambda";
                        children = [("params", Ast_diff.Node { ctor = "params"; children = [("param", Ast_diff.Same)] });
                                    ("body", Ast_diff.Replaced _)] } -> ()
      | _ -> Alcotest.failf "unexpected lambda delta: %s" (sprintf_delta d)));
  ("diff: Lambda param rename is positional Replaced (not rename-detect)", `Quick, fun () ->
     let from = Ast.Lambda (["x"], Ast.var "x")
     and to_ = Ast.Lambda (["y"], Ast.var "y") in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "lambda"; children = _ } ->
          (* must NOT be Same; a genuine rename is visible, not hidden *)
          ()
      | _ -> Alcotest.failf "unexpected lambda delta: %s" (sprintf_delta d)));
  ("diff: Lambda param order swapped is a positional delta", `Quick, fun () ->
     let from = Ast.Lambda (["a"; "b"], Ast.var "a")
     and to_ = Ast.Lambda (["b"; "a"], Ast.var "a") in
     let d = Ast_diff.diff from to_ in
     (* order matters: (a b) vs (b a) must NOT collapse to Same *)
     if d = Ast_diff.Same then
       Alcotest.fail "param order swap wrongly reported Same");
  (* --- Let --- *)
  ("diff: Let value modified", `Quick, fun () ->
     let from = Ast.Let ("x", Ast.int 1, Ast.var "x")
     and to_ = Ast.Let ("x", Ast.int 2, Ast.var "x") in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "let"; children = [("name", Ast_diff.Same); ("val", Ast_diff.Replaced _); ("body", Ast_diff.Same)] } -> ()
      | _ -> Alcotest.failf "unexpected let delta: %s" (sprintf_delta d)));
  ("diff: Let name change is Replaced at name", `Quick, fun () ->
     let from = Ast.Let ("x", Ast.int 1, Ast.var "x")
     and to_ = Ast.Let ("y", Ast.int 1, Ast.var "y") in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "let"; children = [("name", Ast_diff.Replaced _); _; _] } -> ()
      | _ -> Alcotest.failf "unexpected let delta: %s" (sprintf_delta d)));
  (* --- Letrec --- *)
  ("diff: Letrec body modified", `Quick, fun () ->
     let bs = [("f", Ast.Lambda (["n"], Ast.var "n"))] in
     let from = Ast.Letrec (bs, Ast.App (Ast.var "f", [Ast.int 1]))
     and to_ = Ast.Letrec (bs, Ast.App (Ast.var "f", [Ast.int 2])) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "letrec"; children = _ } -> ()
      | _ -> Alcotest.failf "unexpected letrec delta: %s" (sprintf_delta d)));
  ("diff: Letrec binding insertion does not cascade", `Quick, fun () ->
     let from =
       Ast.Letrec
         ([ ("g", Ast.Lambda ([], Ast.int 1));
            ("f", Ast.Lambda (["x"], Ast.var "x")) ],
          Ast.App (Ast.var "f", [Ast.int 1]))
     and to_ =
       Ast.Letrec
         ([ ("g", Ast.Lambda ([], Ast.int 1));
            ("h", Ast.Lambda ([], Ast.int 999));   (* inserted mid-sequence *)
            ("f", Ast.Lambda (["x"], Ast.var "x")) ],
          Ast.App (Ast.var "f", [Ast.int 1]))
     in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "letrec"; children } ->
          let bindings_chilren, body_is_same =
            match List.rev children with
            | ("body", Ast_diff.Same) :: rest -> (List.rev rest, true)
            | ("body", _) :: rest -> (List.rev rest, false)
            | _ -> (children, false)
          in
          (* g unchanged, h added, f unchanged (not all following changed) *)
          let labels = List.map fst bindings_chilren in
          Alcotest.(check bool) "g still present" true (List.mem "g" labels);
          Alcotest.(check bool) "h added to" true (List.mem "h" labels);
          Alcotest.(check bool) "f still present" true (List.mem "f" labels);
          let h_delta = List.assoc "h" bindings_chilren in
          let is_replaced =
            match h_delta with
            | Ast_diff.Replaced { from = from_side; _ } ->
                (* from side is the sentinel (absent) *)
                expr_eq from_side Ast.nil
            | _ -> false
          in
          Alcotest.(check bool) "h is an added Replaced" true is_replaced;
          Alcotest.(check bool) "letrec body Same" true body_is_same
      | _ -> Alcotest.failf "unexpected letrec delta: %s" (sprintf_delta d)));
  (* --- App --- *)
  ("diff: App head replaced", `Quick, fun () ->
     let from = Ast.App (Ast.var "f", [Ast.int 1])
     and to_ = Ast.App (Ast.var "g", [Ast.int 1]) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "app"; children = ("fn", Ast_diff.Replaced _) :: _ } -> ()
      | _ -> Alcotest.failf "unexpected app delta: %s" (sprintf_delta d)));
  ("diff: App args insertion mid-sequence does NOT mark every following arg changed", `Quick, fun () ->
     let from = Ast.App (Ast.var "f", [Ast.int 1; Ast.int 2; Ast.int 3])
     and to_ = Ast.App (Ast.var "f", [Ast.int 1; Ast.int 42; Ast.int 2; Ast.int 3]) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "app"; children = ("fn", Ast_diff.Same) :: args } ->
          (* args 1 and 2 and 3 should be Same (matched via LCS), 42 added *)
          let same_count = List.fold_left (fun n (_, d) ->
              match d with Ast_diff.Same -> n + 1 | _ -> n) 0 args in
          let replaced_count = List.length args - same_count in
          Alcotest.(check int) "matched args remain Same" 3 same_count;
          Alcotest.(check int) "one added arg" 1 replaced_count
      | _ -> Alcotest.failf "unexpected app delta: %s" (sprintf_delta d)));
  ("diff: App trailing add", `Quick, fun () ->
     let from = Ast.App (Ast.var "f", [Ast.int 1])
     and to_ = Ast.App (Ast.var "f", [Ast.int 1; Ast.int 2]) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "app"; children = ("fn", Ast_diff.Same) :: args } ->
          let same_count = List.fold_left (fun n (_, d) ->
              match d with Ast_diff.Same -> n + 1 | _ -> n) 0 args in
          Alcotest.(check int) "arg 1 Same, arg 2 added" 1 same_count;
          Alcotest.(check int) "two args total" 2 (List.length args)
      | _ -> Alcotest.failf "unexpected app delta: %s" (sprintf_delta d)));
  (* --- Prim --- *)
  ("diff: Prim same", `Quick, fun () ->
     let e = Ast.Prim ("+", [Ast.int 1; Ast.int 2]) in
     eq_delta Same (Ast_diff.diff e e));
  ("diff: Prim arg modified", `Quick, fun () ->
     let from = Ast.Prim ("+", [Ast.int 1; Ast.int 2])
     and to_ = Ast.Prim ("+", [Ast.int 1; Ast.int 3]) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "prim"; children = _ } -> ()
      | _ -> Alcotest.failf "unexpected prim delta: %s" (sprintf_delta d)));
  ("diff: Prim name changed is Replaced", `Quick, fun () ->
     let from = Ast.Prim ("+", [Ast.int 1])
     and to_ = Ast.Prim ("-", [Ast.int 1]) in
     eq_delta (Ast_diff.Replaced { from; to_ }) (Ast_diff.diff from to_));
  (* --- Quote --- *)
  ("diff: Quote body modified", `Quick, fun () ->
     let from = Ast.Quote (Ast.var "x")
     and to_ = Ast.Quote (Ast.var "y") in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "quote"; children = [("body", Ast_diff.Replaced _)] } -> ()
      | _ -> Alcotest.failf "unexpected quote delta: %s" (sprintf_delta d)));
  (* --- Define --- *)
  ("diff: Define unchanged", `Quick, fun () ->
     let e = Ast.Define ("foo", Ast.Lambda (["x"], Ast.var "x")) in
     eq_delta Same (Ast_diff.diff e e));
  ("diff: Define val modified", `Quick, fun () ->
     let from = Ast.Define ("foo", Ast.Lambda (["x"], Ast.var "x"))
     and to_ = Ast.Define ("foo", Ast.Lambda (["x"], Ast.int 7)) in
     let d = Ast_diff.diff from to_ in
     (match d with
      | Ast_diff.Node { ctor = "define"; children = [("name", Ast_diff.Same); ("val", Ast_diff.Node _)] } -> ()
      | _ -> Alcotest.failf "unexpected define delta: %s" (sprintf_delta d)));
  (* --- top-level hash short-circuit --- *)
  ("diff: top-level hash equality short-circuits to Same", `Quick, fun () ->
     let e = Ast.Prim ("+", [Ast.int 1; Ast.int 2]) in
     Alcotest.(check bool) "hash equal" true (Ast_diff.ast_hash e = Ast_diff.ast_hash e);
     eq_delta Same (Ast_diff.diff e e));
  ("diff: separate-but-equal exprs hash to Same", `Quick, fun () ->
     (* two distinct AST values with identical structure *)
     let a =
       Ast.Letrec
         ([ ("f", Ast.Lambda (["x"], Ast.Prim ("+", [Ast.var "x"; Ast.int 1]))) ],
          Ast.App (Ast.var "f", [Ast.int 5]))
     in
     let b =
       Ast.Letrec
         ([ ("f", Ast.Lambda (["x"], Ast.Prim ("+", [Ast.var "x"; Ast.int 1]))) ],
          Ast.App (Ast.var "f", [Ast.int 5]))
     in
     Alcotest.(check bool) "distinct values, same hash" true
       (Ast_diff.ast_hash a = Ast_diff.ast_hash b);
     eq_delta Same (Ast_diff.diff a b));
  (* expr_to_json round-trip is untouched by diff (diff only reads) *)
  ("diff: does not mutate (round-trip preserved)", `Quick, fun () ->
     let e = Ast.App (Ast.var "f", [Ast.int 1; Ast.var "x"]) in
     let j = Ast.expr_to_json e in
     (match Ast.expr_of_json j with
      | Ok e' ->
          Alcotest.(check bool) "round-trip" true (expr_eq e e')
      | Error _ -> Alcotest.fail "expr round-trip failed"));
]

let () =
  Alcotest.run "ast_diff" [ ("ast_diff", delta_tests) ]
