(* Tests for the flat path-keyed edit script (branching sexp-diff;
   ast-nodes parts 1-2).

   PURE tests — no Postgres, no HTTP:
     cd lisp/ && dune runtest
   needs no services.

   Two layers, following the branch_algebra property-test pattern
   (image/test/branch_algebra_test.ml):

   1. Generative (QCheck): round-trip property
      [apply (diff a b) a = b] over generated pairs spanning all 11
      expr constructors (Var Lit App Let Letrec If Lambda Prim Seq
      Quote Define), against the strongest possible naive reference:
      the target expr [b] itself — apply must reconstruct it exactly.
      Plus: diff a a = [] (hash short-circuit), apply [] a = a, and
      target-side path validity (every Replace/Insert path resolves
      in [b] to the payload it claims).

   2. Deterministic targeted cases: the edit SHAPES per constructor,
      the mid-sequence-insert-no-shift property (a sibling insert
      adds exactly one edit and never touches following siblings'
      paths), ctor swaps, param positionality, letrec binding edits
      by NAME, empty-list edges, and the nil-sentinel trap (a literal
      nil arg changing is Delete+Insert, never a sentinel misread). *)

open Pricklypear_lisp

let ( // ) = Ast.(
    (* local shorthand: build Var *)
    fun s -> Var s)

let vi n = Ast.Lit (Ast.VInt (Int64.of_int n))

(* ---- generators (all 11 ctors, bounded depth) ---- *)

let names = [ "x"; "y"; "f"; "g"; "h" ]

let gen_name = QCheck.Gen.oneof_list names

let gen_leaf =
  QCheck.Gen.oneof [
    QCheck.Gen.map Ast.var gen_name;
    QCheck.Gen.map (fun n -> vi n) (QCheck.Gen.int_range 0 9);
    QCheck.Gen.map (fun s -> Ast.Lit (Ast.VStr s))
      (QCheck.Gen.oneof_list [ "a"; "b"; "c" ]);
    QCheck.Gen.map (fun b -> Ast.Lit (Ast.VBool b)) QCheck.Gen.bool;
    QCheck.Gen.return (Ast.Lit Ast.VNil);
  ]

let rec gen_expr (depth : int) : Ast.expr QCheck.Gen.t =
  if depth <= 0 then gen_leaf
  else
    let sub () = gen_expr (depth - 1) in
    QCheck.Gen.oneof [
      gen_leaf;
      QCheck.Gen.map Ast.var gen_name;
      QCheck.Gen.map (fun (f, args) -> Ast.App (f, args))
        (QCheck.Gen.pair (sub ())
           (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3) (sub ())));
      QCheck.Gen.map (fun (n, v, b) -> Ast.Let (n, v, b))
        (QCheck.Gen.triple gen_name (sub ()) (sub ()));
      QCheck.Gen.map (fun (bs, b) -> Ast.Letrec (bs, b))
        (QCheck.Gen.pair
           (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3)
              (QCheck.Gen.pair gen_name (sub ())))
           (sub ()));
      QCheck.Gen.map (fun (t, th, el) -> Ast.If (t, th, el))
        (QCheck.Gen.triple (sub ()) (sub ()) (sub ()));
      QCheck.Gen.map (fun (ps, b) -> Ast.Lambda (ps, b))
        (QCheck.Gen.pair
           (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3) gen_name)
           (sub ()));
      QCheck.Gen.map (fun (n, args) -> Ast.Prim (n, args))
        (QCheck.Gen.pair
           (QCheck.Gen.oneof_list [ "+"; "-"; "list" ])
           (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3) (sub ())));
      QCheck.Gen.map (fun (a, b) -> Ast.Seq (a, b))
        (QCheck.Gen.pair (sub ()) (sub ()));
      QCheck.Gen.map (fun e -> Ast.Quote e) (sub ());
      QCheck.Gen.map (fun (n, v) -> Ast.Define (n, v))
        (QCheck.Gen.pair gen_name (sub ()));
    ]

let print_pair (a, b) =
  Reader.pp_expr_flat a ^ "  ==>  " ^ Reader.pp_expr_flat b

let roundtrip_ok (a, b) =
  let eds = Sexp_diff.diff a b in
  Sexp_diff.apply eds a = b

(* ---- generative properties ---- *)

(* 1. THE round-trip property, over generated pairs of all 11 ctors.
   The naive reference is the target itself: apply must rebuild [b]
   exactly — no normalization, no reordering, nothing lost. *)
let prop_roundtrip =
  QCheck.Test.make ~name:"round-trip: apply(diff(a,b), a) = b" ~count:600
    (QCheck.make ~print:print_pair
       (QCheck.Gen.pair (gen_expr 3) (gen_expr 3)))
    roundtrip_ok

(* 1b. round-trip on DEEPER trees (nested letrec / lambda / quote). *)
let prop_roundtrip_deep =
  QCheck.Test.make ~name:"round-trip on deeper trees" ~count:300
    (QCheck.make ~print:print_pair
       (QCheck.Gen.pair (gen_expr 5) (gen_expr 5)))
    roundtrip_ok

(* 2. identical exprs diff to the empty script (hash short-circuit at
   every level). *)
let prop_diff_self_empty =
  QCheck.Test.make ~name:"diff a a = []" ~count:300
    (QCheck.make ~print:(fun a -> Reader.pp_expr_flat a) (gen_expr 4))
    (fun a -> Sexp_diff.diff a a = [])

(* 3. the empty script is a no-op. *)
let prop_apply_empty =
  QCheck.Test.make ~name:"apply [] a = a" ~count:200
    (QCheck.make ~print:(fun a -> Reader.pp_expr_flat a) (gen_expr 3))
    (fun a -> Sexp_diff.apply [] a = a)

(* 4. target-side path validity: every Replace/Insert path resolves in
   [b] to exactly the payload the edit claims (paths are TARGET
   coordinates for everything that survives).

   ONE documented exception (the T1R review's property-#4 oracle bug,
   reworked 2026-10-07): when the last step is letrec.binding[NAME],
   the exact [select_path] check uses FIRST-match name resolution —
   but under duplicate binding names the container diffs POSITIONALLY
   (sexp_diff.ml [has_duplicate_names] degradation) and selection is
   never by name there. For such steps the oracle is the
   NAME-MULTIPLICITY match: the payload must equal the body of SOME
   binding named NAME in the target's parent letrec. When names are
   unique this is exactly equivalent to [select_path]; when they are
   not, first-match selection is simply not the address space. The
   round-trip conjunct below stays unconditional and carries the real
   correctness weight. *)
let prop_paths_resolve_in_target =
  QCheck.Test.make
    ~name:"Replace/Insert paths resolve in the target with the claimed payload"
    ~count:600
    (QCheck.make ~print:print_pair
       (QCheck.Gen.pair (gen_expr 3) (gen_expr 3)))
    (fun (a, b) ->
       let payload_ok p payload =
         match List.rev p with
         | Ast_path.LetrecBinding nm :: rest_rev -> (
             (* last step names a binding: check against the WHOLE
                target container (name-multiplicity), not first-match
                selection *)
             match Ast_path.select_path b (List.rev rest_rev) with
             | Some (Ast.Letrec (bs, _)) ->
                 List.exists (fun (n, body) -> n = nm && body = payload) bs
             | _ -> false)
         | _ -> Ast_path.select_path b p = Some payload
       in
       let eds = Sexp_diff.diff a b in
       List.for_all
         (fun ed ->
            match ed with
            | Sexp_diff.Replace (p, _, to_) -> payload_ok p to_
            | Sexp_diff.Insert (p, _, x) -> payload_ok p x
            | Sexp_diff.Delete _ -> true)
         eds
       && roundtrip_ok (a, b))

(* 5. no redundant Replace: the walk's per-subtree hash short-circuit
   means a Replace payload never has content-equal sides. *)
let prop_no_redundant_replace =
  QCheck.Test.make ~name:"no Replace with hash-equal sides" ~count:400
    (QCheck.make ~print:print_pair
       (QCheck.Gen.pair (gen_expr 3) (gen_expr 3)))
    (fun (a, b) ->
       List.for_all
         (fun ed ->
            match ed with
            | Sexp_diff.Replace (_, from, to_) ->
                Ast_diff.ast_hash from <> Ast_diff.ast_hash to_
            | _ -> true)
         (Sexp_diff.diff a b))

(* ---- deterministic targeted cases ---- *)

let failf fmt = Printf.ksprintf (fun s -> raise (Failure s)) fmt

let expect_edits ~ctx (expected : Sexp_diff.edit list)
    (actual : Sexp_diff.edit list) : unit =
  let shape (e : Sexp_diff.edit) =
    match e with
    | Sexp_diff.Replace (p, _, _) -> "Replace " ^ Ast_path.path_to_string p
    | Sexp_diff.Insert (p, i, _) -> Printf.sprintf "Insert %s@%d" (Ast_path.path_to_string p) i
    | Sexp_diff.Delete (p, i) -> Printf.sprintf "Delete %s@%d" (Ast_path.path_to_string p) i
  in
  if List.length expected <> List.length actual
  || not (List.for_all2 (fun e a -> shape e = shape a) expected actual)
  then
    failf "%s: expected %s, got %s" ctx
      (String.concat "; " (List.map shape expected))
      (Sexp_diff.edits_to_string actual)

let expect_roundtrip ~ctx a b =
  let eds = Sexp_diff.diff a b in
  if Sexp_diff.apply eds a <> b then
    failf "%s: round-trip failed: %s" ctx (Sexp_diff.edits_to_string eds)

let expect_reverse_roundtrip ~ctx a b =
  (* symmetric sanity: diffing the other way also round-trips *)
  expect_roundtrip ~ctx:(ctx ^ " (reverse)") b a

let cases : (string * (unit -> unit)) list = [
  ("if.then replace", fun () ->
     let a = Ast.If (Ast.var "x", vi 1, vi 2) in
     let b = Ast.If (Ast.var "x", vi 9, vi 2) in
     expect_edits ~ctx:"if.then"
       [ Sexp_diff.Replace ([ Ast_path.IfThen ], vi 1, vi 9) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"if.then" a b;
     expect_reverse_roundtrip ~ctx:"if.then" a b);

  ("seq.b replace", fun () ->
     let a = Ast.Seq (vi 1, vi 2) in
     let b = Ast.Seq (vi 1, vi 3) in
     expect_edits ~ctx:"seq.b"
       [ Sexp_diff.Replace ([ Ast_path.SeqB ], vi 2, vi 3) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"seq.b" a b);

  ("define.val replace", fun () ->
     let a = Ast.Define ("foo", Ast.var "x") in
     let b = Ast.Define ("foo", vi 7) in
     expect_edits ~ctx:"define.val"
       [ Sexp_diff.Replace ([ Ast_path.DefineVal ], Ast.var "x", vi 7) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"define.val" a b);

  ("quote.body replace", fun () ->
     let a = Ast.Quote (vi 1) in
     let b = Ast.Quote (vi 2) in
     expect_edits ~ctx:"quote.body"
       [ Sexp_diff.Replace ([ Ast_path.QuoteBody ], vi 1, vi 2) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"quote.body" a b);

  ("ctor swap is one root Replace", fun () ->
     let a = Ast.If (Ast.var "x", vi 1, vi 2) in
     let b = Ast.Lambda ([ "x" ], Ast.var "x") in
     expect_edits ~ctx:"ctor-swap" [ Sexp_diff.Replace ([], a, b) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"ctor-swap" a b);

  ("App mid-sequence insert does NOT shift following siblings' paths",
   fun () ->
     let a = Ast.App (Ast.var "f", [ vi 1; vi 2; vi 3 ]) in
     let b = Ast.App (Ast.var "f", [ vi 1; vi 42; vi 2; vi 3 ]) in
     (* exactly ONE edit: the insert at target slot 1. args 2 and 3 are
        matched by the list-LCS and carry NO edits — their paths
        (app.arg[2], app.arg[3] if they had any) are untouched. *)
     expect_edits ~ctx:"app-mid-insert"
       [ Sexp_diff.Insert ([ Ast_path.AppArg 1 ], 1, vi 42) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"app-mid-insert" a b);

  ("App mid-sequence delete", fun () ->
     let a = Ast.App (Ast.var "f", [ vi 1; vi 2; vi 3 ]) in
     let b = Ast.App (Ast.var "f", [ vi 1; vi 3 ]) in
     expect_edits ~ctx:"app-mid-delete"
       [ Sexp_diff.Delete ([ Ast_path.AppArg 1 ], 1) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"app-mid-delete" a b);

  ("App head replace + differing arg is fn-Replace then Delete+Insert",
   fun () ->
     (* list-LCS truth: matched args are always content-equal (the key is
        the content hash), so a changed arg is Delete+Insert, never a
        Replace at app.arg[i] *)
     let a = Ast.App (Ast.var "f", [ vi 1 ]) in
     let b = Ast.App (Ast.var "g", [ vi 2 ]) in
     expect_edits ~ctx:"app-head"
       [ Sexp_diff.Replace ([ Ast_path.AppFn ], Ast.var "f", Ast.var "g");
         Sexp_diff.Delete ([ Ast_path.AppArg 0 ], 0);
         Sexp_diff.Insert ([ Ast_path.AppArg 0 ], 0, vi 2) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"app-head" a b);

  ("empty app args: insert / delete at the boundary", fun () ->
     let e = Ast.App (Ast.var "f", []) in
     let e1 = Ast.App (Ast.var "f", [ vi 1 ]) in
     expect_edits ~ctx:"app-empty-ins"
       [ Sexp_diff.Insert ([ Ast_path.AppArg 0 ], 0, vi 1) ]
       (Sexp_diff.diff e e1);
     expect_roundtrip ~ctx:"app-empty-ins" e e1;
     expect_edits ~ctx:"app-empty-del"
       [ Sexp_diff.Delete ([ Ast_path.AppArg 0 ], 0) ]
       (Sexp_diff.diff e1 e);
     expect_roundtrip ~ctx:"app-empty-del" e1 e);

  (* the sentinel trap: a literal nil changing to a non-nil arg (or
     back) must be Delete+Insert from the LCS pairing — never a
     Replaced-with-absent-sentinel misread into a bogus Insert. *)
  ("nil-sentinel trap: nil arg replaced is Delete+Insert, not a misread",
   fun () ->
     let a = Ast.App (Ast.var "f", [ Ast.Lit Ast.VNil ]) in
     let b = Ast.App (Ast.var "f", [ vi 1 ]) in
     expect_edits ~ctx:"nil-trap-fwd"
       [ Sexp_diff.Delete ([ Ast_path.AppArg 0 ], 0);
         Sexp_diff.Insert ([ Ast_path.AppArg 0 ], 0, vi 1) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"nil-trap-fwd" a b;
     expect_edits ~ctx:"nil-trap-bwd"
       [ Sexp_diff.Delete ([ Ast_path.AppArg 0 ], 0);
         Sexp_diff.Insert ([ Ast_path.AppArg 0 ], 0, Ast.Lit Ast.VNil) ]
       (Sexp_diff.diff b a);
     expect_roundtrip ~ctx:"nil-trap-bwd" b a);

  ("Prim arg insert", fun () ->
     let a = Ast.Prim ("+", [ vi 1 ]) in
     let b = Ast.Prim ("+", [ vi 1; vi 2 ]) in
     expect_edits ~ctx:"prim-arg-ins"
       [ Sexp_diff.Insert ([ Ast_path.PrimArg 1 ], 1, vi 2) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"prim-arg-ins" a b);

  ("Prim name change is a whole-node Replace", fun () ->
     let a = Ast.Prim ("+", [ vi 1 ]) in
     let b = Ast.Prim ("-", [ vi 1 ]) in
     expect_edits ~ctx:"prim-name" [ Sexp_diff.Replace ([], a, b) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"prim-name" a b);

  ("Let val + body edits under let.val / let.body", fun () ->
     let a = Ast.Let ("x", vi 1, Ast.var "x") in
     let b = Ast.Let ("x", vi 2, Ast.var "x") in
     expect_edits ~ctx:"let-val"
       [ Sexp_diff.Replace ([ Ast_path.LetVal ], vi 1, vi 2) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"let-val" a b);

  ("Let name change replaces the name leaf", fun () ->
     let a = Ast.Let ("x", vi 1, Ast.var "x") in
     let b = Ast.Let ("y", vi 1, Ast.var "y") in
     expect_edits ~ctx:"let-name"
       [ Sexp_diff.Replace ([ Ast_path.LetName ], Ast.var "x", Ast.var "y");
         Sexp_diff.Replace ([ Ast_path.LetBody ], Ast.var "x", Ast.var "y") ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"let-name" a b);

  ("Letrec binding inserted mid-sequence does not disturb siblings",
   fun () ->
     let g = ("g", Ast.Lambda ([], vi 1)) in
     let f = ("f", Ast.Lambda ([ "x" ], Ast.var "x")) in
     let h = ("h", Ast.Lambda ([], vi 999)) in
     let body = Ast.App (Ast.var "f", [ vi 1 ]) in
     let a = Ast.Letrec ([ g; f ], body) in
     let b = Ast.Letrec ([ g; h; f ], body) in
     (* one edit, keyed by NAME; g and f carry no edits *)
     expect_edits ~ctx:"letrec-ins"
       [ Sexp_diff.Insert ([ Ast_path.LetrecBinding "h" ], 1, snd h) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"letrec-ins" a b);

  ("Letrec binding deleted", fun () ->
     let g = ("g", Ast.Lambda ([], vi 1)) in
     let f = ("f", Ast.Lambda ([ "x" ], Ast.var "x")) in
     let h = ("h", Ast.Lambda ([], vi 999)) in
     let body = Ast.App (Ast.var "f", [ vi 1 ]) in
     let a = Ast.Letrec ([ g; h; f ], body) in
     let b = Ast.Letrec ([ g; f ], body) in
     expect_edits ~ctx:"letrec-del"
       [ Sexp_diff.Delete ([ Ast_path.LetrecBinding "h" ], 1) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"letrec-del" a b);

  (* the degradation, pinned deterministically (T1R review follow-up):
     with a duplicate name on EITHER side, letrec.binding[NAME]
     addressing is ambiguous, so the whole container diffs positionally
     — every from-binding deletes at its CURRENT position (0 after each
     prior delete), every to-binding inserts at its TARGET position, all
     Deletes strictly before all Inserts, and NO edit descends into a
     binding subtree by name. The oracle bug this pins: an insert's
     payload may be the SECOND same-named binding, which first-match
     [select_path] cannot address. *)
  ("duplicate binding names: container degrades to positional Delete-then-Insert",
   fun () ->
     let body = Ast.var "x" in
     (* target-side duplicates *)
     let a = Ast.Letrec ([ ("f", vi 1) ], body) in
     let b = Ast.Letrec ([ ("g", vi 2); ("g", vi 3); ("g", vi 4) ], body) in
     expect_edits ~ctx:"letrec-dup-target"
       [ Sexp_diff.Delete ([ Ast_path.LetrecBinding "f" ], 0);
         Sexp_diff.Insert ([ Ast_path.LetrecBinding "g" ], 0, vi 2);
         Sexp_diff.Insert ([ Ast_path.LetrecBinding "g" ], 1, vi 3);
         Sexp_diff.Insert ([ Ast_path.LetrecBinding "g" ], 2, vi 4) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"letrec-dup-target" a b;
     (* source-side duplicates *)
     let a2 = Ast.Letrec ([ ("h", vi 5); ("h", vi 6) ], body) in
     let b2 = Ast.Letrec ([ ("f", vi 7) ], body) in
     expect_edits ~ctx:"letrec-dup-source"
       [ Sexp_diff.Delete ([ Ast_path.LetrecBinding "h" ], 0);
         Sexp_diff.Delete ([ Ast_path.LetrecBinding "h" ], 0);
         Sexp_diff.Insert ([ Ast_path.LetrecBinding "f" ], 0, vi 7) ]
       (Sexp_diff.diff a2 b2);
     expect_roundtrip ~ctx:"letrec-dup-source" a2 b2);

  ("Letrec binding body edit descends under the named binding", fun () ->
     let body = Ast.App (Ast.var "f", [ vi 1 ]) in
     let a = Ast.Letrec ([ ("f", Ast.Lambda ([ "n" ], Ast.var "n")) ], body) in
     let b = Ast.Letrec ([ ("f", Ast.Lambda ([ "n" ], vi 2)) ], body) in
     (* the binding is matched BY NAME, so the diff descends into it and
        the edit lands at the SUBTREE path — the granularity the campaign
        wants for conflict reports *)
     expect_edits ~ctx:"letrec-body"
       [ Sexp_diff.Replace
           ([ Ast_path.LetrecBinding "f"; Ast_path.LambdaBody ], Ast.var "n",
             vi 2) ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"letrec-body" a b);

  ("Lambda param insert / delete (positional)", fun () ->
     let a = Ast.Lambda ([ "x" ], Ast.var "x") in
     let b = Ast.Lambda ([ "x"; "y" ], Ast.var "x") in
     expect_edits ~ctx:"param-ins"
       [ Sexp_diff.Insert ([ Ast_path.LambdaParam 1 ], 1, Ast.var "y") ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"param-ins" a b;
     expect_edits ~ctx:"param-del"
       [ Sexp_diff.Delete ([ Ast_path.LambdaParam 1 ], 1) ]
       (Sexp_diff.diff b a);
     expect_roundtrip ~ctx:"param-del" b a);

  ("Lambda param rename is a positional Replace (non-goal: no rename detection)",
   fun () ->
     let a = Ast.Lambda ([ "x" ], Ast.var "x") in
     let b = Ast.Lambda ([ "y" ], Ast.var "y") in
     expect_edits ~ctx:"param-rename"
       [ Sexp_diff.Replace ([ Ast_path.LambdaParam 0 ], Ast.var "x", Ast.var "y");
         Sexp_diff.Replace ([ Ast_path.LambdaBody ], Ast.var "x", Ast.var "y") ]
       (Sexp_diff.diff a b);
     expect_roundtrip ~ctx:"param-rename" a b);

  ("nested letrec under lambda under app: full dotted path", fun () ->
     let mk fbody =
       Ast.App (Ast.Lambda ([], Ast.Letrec ([ ("f", fbody) ], Ast.var "f")), [])
     in
     let a = mk (Ast.Lambda ([ "n" ], Ast.var "n")) in
     let b = mk (Ast.Lambda ([ "n" ], vi 2)) in
     let eds = Sexp_diff.diff a b in
     (match eds with
      | [ Sexp_diff.Replace
            ([ Ast_path.AppFn; Ast_path.LambdaBody;
               Ast_path.LetrecBinding "f"; Ast_path.LambdaBody ], _, _) ] -> ()
      | _ ->
          failf "expected one deep Replace, got %s"
            (Sexp_diff.edits_to_string eds));
     expect_roundtrip ~ctx:"deep-path" a b);

  ("select_path / replace_path round-trip on every step kind", fun () ->
     let e =
       Ast.Letrec
         ([ ("f", Ast.Lambda ([ "n" ], Ast.App (Ast.var "g", [ vi 1; vi 2 ]))) ],
          Ast.If (Ast.var "p", Ast.Seq (vi 3, vi 4), Ast.Quote (vi 5)))
     in
     let check p expected =
       match Ast_path.select_path e p with
       | Some got ->
           if Ast_diff.ast_hash got <> Ast_diff.ast_hash expected then
             failf "select %s: got %s" (Ast_path.path_to_string p)
               (Reader.pp_expr_flat got)
       | None -> failf "select %s: None" (Ast_path.path_to_string p)
     in
     check [ Ast_path.LetrecBinding "f" ]
       (Ast.Lambda ([ "n" ], Ast.App (Ast.var "g", [ vi 1; vi 2 ])));
     check
       [ Ast_path.LetrecBinding "f"; Ast_path.LambdaBody; Ast_path.AppArg 1 ]
       (vi 2);
     check [ Ast_path.LetrecBinding "f"; Ast_path.LambdaParam 0 ] (Ast.var "n");
     check [ Ast_path.LetrecBody; Ast_path.IfTest ] (Ast.var "p");
     check [ Ast_path.LetrecBody; Ast_path.IfElse; Ast_path.QuoteBody ] (vi 5);
     (* replace the inner arg, then re-select *)
     let e' =
       Ast_path.replace_path
         [ Ast_path.LetrecBinding "f"; Ast_path.LambdaBody; Ast_path.AppArg 0 ]
         (vi 9) e
     in
     (match Ast_path.select_path e' [ Ast_path.LetrecBinding "f" ] with
      | Some (Ast.Lambda (_, Ast.App (_, args))) ->
          if args <> [ vi 9; vi 2 ] then
            failf "replace_path wrote %s" (Reader.pp_expr_flat (Ast.App (Ast.var "g", args)))
      | _ -> failf "replace_path broke the tree");
     (* stale paths are no-ops, never exceptions *)
     if Ast_path.replace_path [ Ast_path.IfTest ] (vi 0) (vi 1) <> vi 1 then
       failf "stale path must be a no-op";
     if Ast_path.select_path (vi 1) [ Ast_path.IfThen ] <> None then
       failf "non-applying select must be None");

  ("apply is total on a stale script", fun () ->
     (* edits referencing paths that do not exist degrade to no-ops *)
     let e = vi 1 in
     let eds =
       [ Sexp_diff.Replace ([ Ast_path.IfThen ], vi 1, vi 2);
         Sexp_diff.Insert ([ Ast_path.AppArg 3 ], 3, vi 4);
         Sexp_diff.Delete ([ Ast_path.LetrecBinding "zz" ], 0) ]
     in
     if Sexp_diff.apply eds e <> e then failf "stale script must be a no-op");
]

let () =
  (* deterministic cases first: any Failure propagates and fails the
     run before the generative sweep. *)
  List.iter
    (fun (name, run) ->
       match run () with
       | () -> Printf.printf "ok  %s\n%!" name
       | exception Failure msg ->
           Printf.eprintf "FAIL %s: %s\n%!" name msg;
           exit 1)
    cases;
  QCheck_base_runner.run_tests_main [
    prop_roundtrip;
    prop_roundtrip_deep;
    prop_diff_self_empty;
    prop_apply_empty;
    prop_paths_resolve_in_target;
    prop_no_redundant_replace;
  ]
