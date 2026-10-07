(* Tests for the pure three-way merge (branching ast-merge; ast-nodes
   part 3).

   PURE tests — no Postgres, no HTTP:
     dune runtest lisp --force
   needs no services. (Plain `dune runtest lisp` skips up-to-date test
   actions — an unforced green is vacuous.)

   Same two layers as sexp_diff_test.ml (the branch_algebra pattern;
   generators copied — one executable per file):

   1. Deterministic targeted cases, one per spec clause:
      - disjoint edits (different subtrees) both present, no conflicts;
      - both-identical kept ONCE (the duplicated edit appears a single
        time in the merged script);
      - both-different CONFLICT at the exact path, with the exact
        ours/theirs payload values, base version kept in the body;
      - delete-vs-edit (same path, one side deletes) conflicts with
        ours = None;
      - nested lambda/letrec: edits under a letrec BINDING (name-keyed
        descent) vs under the letrec body compose;
      - ancestor/descendant across sides (ours replaced the whole
        subtree at P, theirs edited inside base's P) conflicts at the
        ANCESTOR path — never composed (the classic silent-drop bug);
      - delete-of-subtree vs edit-inside conflicts with ours = None
        (value selected from the side body);
      - same-list cross-side inserts conflict at the PARENT path
        (cross-side index frames are not interleavable — conservative
        v1 behavior, pinned);
      - a list touched by one side only composes;
      - identical sides merge to themselves with zero conflicts;
      - hash re-validation: merged body hash equals the expected
        canonical-sexp hash.

   2. Generative (QCheck):
      - merge(base, base, theirs) = theirs exactly, no conflicts —
        and symmetrically merge(base, theirs, base);
      - identical sides: merge(base, b, b) = b, no conflicts;
      - compose soundness: every surviving edit's path (parent path
        for splices) resolves in the BASE tree — the composition never
        strands an edit;
      - conflicts and surviving edits never share (or nest under) a
        path;
      - side-swap symmetry: the conflict PATH SET of
        merge(base, ours, theirs) equals that of
        merge(base, theirs, ours). *)

open Pricklypear_lisp

let v s = Ast.Var s
let vi n = Ast.Lit (Ast.VInt (Int64.of_int n))

(* ---- generators (all 11 ctors, bounded depth; copied from
   sexp_diff_test.ml) ---- *)

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
      QCheck.Gen.map (fun (a, b) -> Ast.Seq (a, b)) (QCheck.Gen.pair (sub ()) (sub ()));
      QCheck.Gen.map (fun e -> Ast.Quote e) (sub ());
      QCheck.Gen.map (fun (n, v) -> Ast.Define (n, v))
        (QCheck.Gen.pair gen_name (sub ()));
    ]

let print_triple (b, o, t) =
  Printf.sprintf "base=%s ours=%s theirs=%s" (Reader.pp_expr_flat b)
    (Reader.pp_expr_flat o) (Reader.pp_expr_flat t)

(* test-local structural descendant test (mirrors Sexp_merge.is_under) *)
let rec step_eq (a : Ast_path.step) (b : Ast_path.step) : bool =
  match (a, b) with
  | Ast_path.AppArg i, Ast_path.AppArg j -> i = j
  | Ast_path.PrimArg i, Ast_path.PrimArg j -> i = j
  | Ast_path.LambdaParam i, Ast_path.LambdaParam j -> i = j
  | Ast_path.LetrecBinding x, Ast_path.LetrecBinding y -> x = y
  | a, b -> a = b

let is_under (p1 : Ast_path.path) (p2 : Ast_path.path) : bool =
  let rec go x y =
    match (x, y) with
    | [], _ -> true
    | _ :: _, [] -> false
    | a :: r1, b :: r2 -> step_eq a b && go r1 r2
  in
  go p2 p1
  &&
  let rec len = function [] -> 0 | _ :: r -> 1 + len r in
  len p1 > len p2

let edit_path (e : Sexp_diff.edit) : Ast_path.path =
  match e with
  | Sexp_diff.Replace (p, _, _) -> p
  | Sexp_diff.Insert (p, _, _) -> p
  | Sexp_diff.Delete (p, _) -> p

(* ---- deterministic cases ---- *)

let failf fmt = Printf.ksprintf (fun s -> raise (Failure s)) fmt

let expect_no_conflicts ~ctx (r : Sexp_merge.result) : unit =
  if r.Sexp_merge.conflicts <> [] then
    failf "%s: expected no conflicts, got %s" ctx
      (Sexp_merge.conflicts_to_string r.Sexp_merge.conflicts)

let expect_one_conflict ~ctx (r : Sexp_merge.result) : unit =
  match r.Sexp_merge.conflicts with
  | [ c ] -> ignore c
  | cs ->
      failf "%s: expected exactly one conflict, got %d (%s)" ctx
        (List.length cs)
        (Sexp_merge.conflicts_to_string cs)

let expect_body ~ctx (expected : Ast.expr) (r : Sexp_merge.result) : unit =
  let got = Sexp_merge.merged_body r in
  if got <> expected then
    failf "%s: merged body mismatch@ expected %s@ got      %s" ctx
      (Reader.pp_expr_flat expected) (Reader.pp_expr_flat got)

let cases : (string * (unit -> unit)) list =
  [
    ("disjoint edits both present, no conflicts", fun () ->
       let base = Ast.Let ("x", Ast.App (v "f", [ vi 1 ]), v "x") in
       let ours = Ast.Let ("x", Ast.App (v "f", [ vi 2 ]), v "x") in
       let theirs = Ast.Let ("x", Ast.App (v "f", [ vi 1 ]), vi 9) in
       let r = Sexp_merge.merge base ours theirs in
       expect_no_conflicts ~ctx:"disjoint" r;
       expect_body ~ctx:"disjoint"
         (Ast.Let ("x", Ast.App (v "f", [ vi 2 ]), vi 9))
         r;
       (* hash re-validation *)
       if
         Ast_diff.ast_hash (Sexp_merge.merged_body r)
         <> Ast_diff.ast_hash (Ast.Let ("x", Ast.App (v "f", [ vi 2 ]), vi 9))
       then failf "disjoint: merged hash mismatch");

    ("both-identical applied once", fun () ->
       let base = Ast.App (v "f", [ vi 1; vi 2 ]) in
       let agreed = Ast.App (v "f", [ vi 7; vi 2 ]) in
       let r = Sexp_merge.merge base agreed agreed in
       expect_no_conflicts ~ctx:"identical" r;
       expect_body ~ctx:"identical" agreed r;
       (* ONCE: ours' copy survives, theirs' duplicate is excluded *)
       let n =
         List.length
           (List.filter
              (fun e ->
                 match edit_path e with
                 | [ Ast_path.AppArg 0 ] -> true
                 | _ -> false)
              r.Sexp_merge.edits)
       in
       (* ours' script for the arg change is Delete+Insert = 2 edits *)
       if n <> 2 then
         failf "identical: expected the agreed edit applied once (2 splice edits), got %d total at the path" n;
       if List.length r.Sexp_merge.edits <> 2 then
         failf "identical: expected merged script of 2 edits, got %d"
           (List.length r.Sexp_merge.edits));

    ("both-different conflicts at the exact path", fun () ->
       let base = Ast.App (v "f", [ vi 1; vi 2 ]) in
       let ours = Ast.App (v "f", [ vi 3; vi 2 ]) in
       let theirs = Ast.App (v "f", [ vi 4; vi 2 ]) in
       let r = Sexp_merge.merge base ours theirs in
       expect_one_conflict ~ctx:"both-diff" r;
       let c = List.hd r.Sexp_merge.conflicts in
       if Ast_path.path_to_string c.Sexp_merge.path <> "app.arg[0]" then
         failf "both-diff: conflict at %s, expected app.arg[0]"
           (Ast_path.path_to_string c.Sexp_merge.path);
       (match (c.Sexp_merge.ours, c.Sexp_merge.theirs) with
       | Some o, Some t when o = vi 3 && t = vi 4 -> ()
       | _ ->
           let sv = function Some e -> Reader.pp_expr_flat e | None -> "<none>" in
           failf "both-diff: conflict values ours=%s theirs=%s, expected 3/4"
             (sv c.Sexp_merge.ours) (sv c.Sexp_merge.theirs));
       (* base version kept at the conflict; the untouched arg stays *)
       expect_body ~ctx:"both-diff" base r;
       if r.Sexp_merge.edits <> [] then
         failf "both-diff: no edit should survive, got %s"
           (Sexp_diff.edits_to_string r.Sexp_merge.edits));

    ("delete-vs-edit conflicts with ours=None", fun () ->
       let base = Ast.App (v "f", [ vi 1; vi 2 ]) in
       let ours = Ast.App (v "f", [ vi 2 ]) in
       let theirs = Ast.App (v "f", [ vi 9; vi 2 ]) in
       let r = Sexp_merge.merge base ours theirs in
       expect_one_conflict ~ctx:"del-vs-edit" r;
       let c = List.hd r.Sexp_merge.conflicts in
       if Ast_path.path_to_string c.Sexp_merge.path <> "app.arg[0]" then
         failf "del-vs-edit: conflict at %s" (Ast_path.path_to_string c.Sexp_merge.path);
       (match (c.Sexp_merge.ours, c.Sexp_merge.theirs) with
       | None, Some t when t = vi 9 -> ()
       | _ -> failf "del-vs-edit: expected ours=None theirs=9");
       expect_body ~ctx:"del-vs-edit" base r);

    ("nested lambda/letrec edits compose", fun () ->
       let base =
         Ast.Letrec
           ([ ("f", Ast.Lambda ([ "n" ], Ast.App (v "g", [ v "n"; vi 1 ]))) ],
            Ast.App (v "f", [ vi 5 ]))
       in
       let ours =
         Ast.Letrec
           ([ ("f", Ast.Lambda ([ "n" ], Ast.App (v "g", [ v "n"; vi 2 ]))) ],
            Ast.App (v "f", [ vi 5 ]))
       in
       let theirs =
         Ast.Letrec
           ([ ("f", Ast.Lambda ([ "n" ], Ast.App (v "g", [ v "n"; vi 1 ]))) ],
            Ast.App (v "f", [ vi 6 ]))
       in
       let r = Sexp_merge.merge base ours theirs in
       expect_no_conflicts ~ctx:"nested" r;
       expect_body ~ctx:"nested"
         (Ast.Letrec
            ([ ("f", Ast.Lambda ([ "n" ], Ast.App (v "g", [ v "n"; vi 2 ]))) ],
             Ast.App (v "f", [ vi 6 ])))
         r);

    ("ancestor/descendant across sides conflicts at the ANCESTOR", fun () ->
       (* ours replaced the whole lambda body; theirs edited inside
          base's body — composition would silently drop one side *)
       let base = Ast.Lambda ([], Ast.App (v "f", [ vi 1 ])) in
       let ours = Ast.Lambda ([], vi 9) in
       let theirs = Ast.Lambda ([], Ast.App (v "f", [ vi 2 ])) in
       let r = Sexp_merge.merge base ours theirs in
       expect_one_conflict ~ctx:"ancestor" r;
       let c = List.hd r.Sexp_merge.conflicts in
       if Ast_path.path_to_string c.Sexp_merge.path <> "lambda.body" then
         failf "ancestor: conflict at %s, expected lambda.body"
           (Ast_path.path_to_string c.Sexp_merge.path);
       (match (c.Sexp_merge.ours, c.Sexp_merge.theirs) with
       | Some o, Some t when o = vi 9 && t = Ast.App (v "f", [ vi 2 ]) -> ()
       | _ -> failf "ancestor: conflict values wrong");
       expect_body ~ctx:"ancestor" base r);

    ("delete-of-subtree vs edit-inside conflicts with ours=None", fun () ->
       (* ours deleted letrec binding f entirely; theirs (name-keyed
          descent) edited inside f's body *)
       let base = Ast.Letrec ([ ("f", Ast.App (v "g", [ vi 1 ])) ], v "f") in
       let ours = Ast.Letrec ([], v "f") in
       let theirs = Ast.Letrec ([ ("f", Ast.App (v "g", [ vi 2 ])) ], v "f") in
       let r = Sexp_merge.merge base ours theirs in
       expect_one_conflict ~ctx:"del-subtree" r;
       let c = List.hd r.Sexp_merge.conflicts in
       if Ast_path.path_to_string c.Sexp_merge.path <> "letrec.binding[f]" then
         failf "del-subtree: conflict at %s"
           (Ast_path.path_to_string c.Sexp_merge.path);
       (match (c.Sexp_merge.ours, c.Sexp_merge.theirs) with
       | None, Some t when t = Ast.App (v "g", [ vi 2 ]) -> ()
       | _ -> failf "del-subtree: expected ours=None theirs=App(g,[2])");
       expect_body ~ctx:"del-subtree" base r);

    ("same-list cross-side inserts conflict at the parent", fun () ->
       (* conservative v1: cross-side index frames are not interleaved.
         The list lives under let.val so the conflict path is the
         container's parent node (let.val), not the root. *)
       let base = Ast.Let ("x", Ast.App (v "f", [ vi 1 ]), v "x") in
       let ours = Ast.Let ("x", Ast.App (v "f", [ vi 1; vi 2 ]), v "x") in
       let theirs = Ast.Let ("x", Ast.App (v "f", [ vi 0; vi 1 ]), v "x") in
       let r = Sexp_merge.merge base ours theirs in
       expect_one_conflict ~ctx:"same-list" r;
       let c = List.hd r.Sexp_merge.conflicts in
       if Ast_path.path_to_string c.Sexp_merge.path <> "let.val" then
         failf "same-list: conflict at %s, expected let.val"
           (Ast_path.path_to_string c.Sexp_merge.path);
       (match (c.Sexp_merge.ours, c.Sexp_merge.theirs) with
       | Some o, Some t when o = Ast.App (v "f", [ vi 1; vi 2 ]) && t = Ast.App (v "f", [ vi 0; vi 1 ]) -> ()
       | _ -> failf "same-list: conflict values should be the full side subtrees");
       expect_body ~ctx:"same-list" base r);

    ("a list touched by one side only composes", fun () ->
       let base = Ast.App (v "f", [ vi 1 ]) in
       let theirs = Ast.App (v "f", [ vi 0; vi 1 ]) in
       let r = Sexp_merge.merge base base theirs in
       expect_no_conflicts ~ctx:"one-side-list" r;
       expect_body ~ctx:"one-side-list" theirs r);

    ("identical sides merge to themselves", fun () ->
       let base = Ast.Let ("x", vi 1, v "x") in
       let both = Ast.Let ("x", vi 2, Ast.App (v "x", [ vi 3 ])) in
       let r = Sexp_merge.merge base both both in
       expect_no_conflicts ~ctx:"same-sides" r;
       expect_body ~ctx:"same-sides" both r);
  ]

(* ---- generative properties ---- *)

(* THE one-side property: merge(base, base, theirs) = theirs exactly
   (and symmetrically). The empty side contributes no edits, so the
   other side's whole script survives untouched. *)
let prop_one_side_is_identity =
  QCheck.Test.make ~name:"merge(base, base, theirs) = theirs (and symmetric)"
    ~count:400
    (QCheck.make ~print:print_triple
       (QCheck.Gen.triple (gen_expr 3) (gen_expr 3) (gen_expr 3)))
    (fun (b, o, t) ->
       let r1 = Sexp_merge.merge b b t in
       let r2 = Sexp_merge.merge b t b in
       Sexp_merge.merged_body r1 = t
       && r1.Sexp_merge.conflicts = []
       && Sexp_merge.merged_body r2 = t
       && r2.Sexp_merge.conflicts = [])

(* identical sides merge to themselves (both-agreed edits kept once). *)
let prop_identical_sides =
  QCheck.Test.make ~name:"merge(base, b, b) = b, zero conflicts" ~count:300
    (QCheck.make ~print:(fun (b, x) ->
         Reader.pp_expr_flat b ^ " | " ^ Reader.pp_expr_flat x)
       (QCheck.Gen.pair (gen_expr 3) (gen_expr 3)))
    (fun (b, x) ->
       let r = Sexp_merge.merge b x x in
       Sexp_merge.merged_body r = x && r.Sexp_merge.conflicts = [])

(* compose soundness: every SURVIVING edit is base-frame-valid — a
   Replace's full path resolves in base; a splice's PARENT path
   resolves in base (cross-side ancestors were conflicted away, so no
   surviving edit is stranded). *)
let prop_surviving_edits_resolve_in_base =
  QCheck.Test.make
    ~name:"surviving edits resolve in the base tree" ~count:300
    (QCheck.make ~print:print_triple
       (QCheck.Gen.triple (gen_expr 3) (gen_expr 3) (gen_expr 3)))
    (fun (b, o, t) ->
       let r = Sexp_merge.merge b o t in
       List.for_all
         (fun e ->
            match e with
            | Sexp_diff.Replace (p, _, _) -> Ast_path.select_path b p <> None
            | Sexp_diff.Insert (p, _, _) | Sexp_diff.Delete (p, _) ->
                let parent =
                  match List.rev p with _ :: rest -> List.rev rest | [] -> []
                in
                Ast_path.select_path b parent <> None)
         r.Sexp_merge.edits)

(* conflicts and survivors never share or nest under a path. *)
let prop_conflicts_disjoint_from_edits =
  QCheck.Test.make
    ~name:"conflict paths disjoint from surviving edit paths" ~count:300
    (QCheck.make ~print:print_triple
       (QCheck.Gen.triple (gen_expr 3) (gen_expr 3) (gen_expr 3)))
    (fun (b, o, t) ->
       let r = Sexp_merge.merge b o t in
       List.for_all
         (fun e ->
            List.for_all
              (fun c ->
                 let p = edit_path e in
                 p <> c.Sexp_merge.path
                 && not (is_under p c.Sexp_merge.path)
                 && not (is_under c.Sexp_merge.path p))
              r.Sexp_merge.conflicts)
         r.Sexp_merge.edits)

(* side-swap symmetry: conflict PATH SETS are symmetric under
   swapping ours/theirs (values swap with the sides). *)
let prop_side_swap_symmetry =
  QCheck.Test.make ~name:"conflict paths symmetric under side swap"
    ~count:200
    (QCheck.make ~print:print_triple
       (QCheck.Gen.triple (gen_expr 3) (gen_expr 3) (gen_expr 3)))
    (fun (b, o, t) ->
       let paths r =
         List.sort compare
           (List.map
              (fun c -> Ast_path.path_to_string c.Sexp_merge.path)
              r.Sexp_merge.conflicts)
       in
       paths (Sexp_merge.merge b o t) = paths (Sexp_merge.merge b t o))

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
    prop_one_side_is_identity;
    prop_identical_sides;
    prop_surviving_edits_resolve_in_base;
    prop_conflicts_disjoint_from_edits;
    prop_side_swap_symmetry;
  ]
