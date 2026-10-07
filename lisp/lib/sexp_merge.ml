(** Pure three-way merge of two [Ast.expr] bodies over a common base
    (branching ast-merge; ast-nodes part 3).

    MECHANISM (exactly the spec's): ours = diff(base,ours), theirs =
    diff(base,theirs) via [Sexp_diff]; group edits by path; DISJOINT
    paths compose with no cross-order; same path edited by both:
    identical target -> keep once, different target -> CONFLICT at that
    path with ours+theirs values. Conflicts are DATA in the result
    (merged edit script + conflict list) — never silently resolved
    (obituary rule). The merged body is [apply edits base]; the caller
    (store merge, campaign P2) re-validates the canonical-sexp hash.

    WHAT "DISJOINT" MEANS HERE (the load-bearing definition — anything
    else conflicts):

    - Two edits at the SAME path are the same-path rule (keep once /
      conflict), never "composition".
    - Ancestor/descendant paths (one a strict prefix of the other, one
      per side) are NOT disjoint: ours replaced the whole subtree at P
      while theirs edited inside base's P — composing would silently
      drop one side. CONFLICT at the shorter (ancestor) path.
    - Edits into the SAME LIST CONTAINER (same parent path, both last
      steps list-child steps: app.arg[i] / prim.arg[i] /
      lambda.param[i] / letrec.binding[NAME]) on BOTH sides are NOT
      disjoint: script indices are application-time positions in each
      side's OWN final list, so cross-side index frames cannot be
      interleaved soundly without a list-level diff3 (deferred — tail
      note). CONFLICT at the parent path. A list touched by only ONE
      side composes fine (its whole script is that side's own frame,
      which round-trips by construction).

    Everything else — edits in structurally unrelated subtrees, or
    different fixed child slots of the same node (if.then vs if.else,
    let.val vs let.body) — composes. The merged script keeps each
    side's internal edit order (per-list application-time frames), and
    cross-side survivors never share a list, so [apply] stays valid.

    Conflict VALUES: same-path conflicts report each side's net value
    from its own edit group (None = that side deleted the node; a
    Delete+Insert group reports the inserted payload). Prefix/list
    conflicts report each side's subtree at the conflict path, selected
    from that side's FULL body (the chain to the path is intact on an
    editing side by the per-script no-ancestor-descendant invariant; a
    side that deleted the subtree selects [None]).

    Total: no exceptions; identical sides merge to themselves with an
    empty conflict list. Pure, no I/O — testable with
    [dune runtest lisp] alone.

    DEFERRED (v2): a list-level three-way merge (element identity via
    the LCS pairing) would compose the benign same-list cases (both
    sides append different args, delete different elements) instead of
    conflicting. Conservative conflicts are SOUND (nothing is ever
    silently mangled); they are just noisier than git. *)

type conflict = {
  path : Ast_path.path;
  ours : Ast.expr option;  (* None = that side deleted the node *)
  theirs : Ast.expr option;
}

type result = {
  base : Ast.expr;
  edits : Sexp_diff.edit list;  (* applies cleanly to [base] *)
  conflicts : conflict list;    (* never auto-resolved *)
}

(** The merged body: apply the surviving (conflict-free) script to the
    base. Conflicted subtrees keep the BASE version — never either
    side's, never silently chosen. *)
let merged_body (r : result) : Ast.expr = Sexp_diff.apply r.edits r.base

let conflict_to_string (c : conflict) : string =
  let show = function
    | Some e -> Reader.pp_expr_flat e
    | None -> "<deleted>"
  in
  Printf.sprintf "CONFLICT at %s: ours=%s theirs=%s"
    (Ast_path.path_to_string c.path) (show c.ours) (show c.theirs)

let conflicts_to_string (cs : conflict list) : string =
  String.concat "; " (List.map conflict_to_string cs)

(* ---- helpers ---- *)

let edit_path (e : Sexp_diff.edit) : Ast_path.path =
  match e with
  | Sexp_diff.Replace (p, _, _) -> p
  | Sexp_diff.Insert (p, _, _) -> p
  | Sexp_diff.Delete (p, _) -> p

let rec step_eq (a : Ast_path.step) (b : Ast_path.step) : bool =
  match (a, b) with
  | Ast_path.IfTest, Ast_path.IfTest | Ast_path.IfThen, Ast_path.IfThen
  | Ast_path.IfElse, Ast_path.IfElse
  | Ast_path.LambdaParams, Ast_path.LambdaParams
  | Ast_path.LambdaBody, Ast_path.LambdaBody
  | Ast_path.LetName, Ast_path.LetName | Ast_path.LetVal, Ast_path.LetVal
  | Ast_path.LetBody, Ast_path.LetBody
  | Ast_path.DefineName, Ast_path.DefineName
  | Ast_path.DefineVal, Ast_path.DefineVal
  | Ast_path.SeqA, Ast_path.SeqA | Ast_path.SeqB, Ast_path.SeqB
  | Ast_path.QuoteBody, Ast_path.QuoteBody | Ast_path.AppFn, Ast_path.AppFn
  | Ast_path.LetrecBody, Ast_path.LetrecBody ->
      true
  | Ast_path.LambdaParam i, Ast_path.LambdaParam j -> i = j
  | Ast_path.AppArg i, Ast_path.AppArg j -> i = j
  | Ast_path.PrimArg i, Ast_path.PrimArg j -> i = j
  | Ast_path.LetrecBinding x, Ast_path.LetrecBinding y -> x = y
  | _ -> false

(* strict-prefix test on step lists (structural, not string) *)
let is_prefix (p1 : Ast_path.path) (p2 : Ast_path.path) : bool =
  let rec go x y =
    match (x, y) with
    | [], _ -> true
    | _ :: _, [] -> false
    | a :: r1, b :: r2 -> step_eq a b && go r1 r2
  in
  go p1 p2

(* [p1] strictly under [p2] (proper descendant) *)
let is_under (p1 : Ast_path.path) (p2 : Ast_path.path) : bool =
  is_prefix p2 p1
  &&
  let rec len = function [] -> 0 | _ :: r -> 1 + len r in
  len p1 > len p2

(* the LIST CONTAINER a list-child edit addresses (its parent path) *)
let list_parent (p : Ast_path.path) : Ast_path.path option =
  match List.rev p with
  | (Ast_path.AppArg _ | Ast_path.PrimArg _ | Ast_path.LambdaParam _
    | Ast_path.LetrecBinding _) :: rest ->
      Some (List.rev rest)
  | _ -> None

(* Two edit groups (same path, one per side) are the SAME edit when
   they match kind-for-kind with equal payloads (hash-compared). *)
let groups_equal (g1 : Sexp_diff.edit list) (g2 : Sexp_diff.edit list) : bool =
  List.length g1 = List.length g2
  && List.for_all2
       (fun a b ->
          match (a, b) with
          | Sexp_diff.Replace (_, _, x), Sexp_diff.Replace (_, _, y) ->
              Ast_diff.ast_hash x = Ast_diff.ast_hash y
          | Sexp_diff.Insert (_, i, x), Sexp_diff.Insert (_, j, y) ->
              i = j && Ast_diff.ast_hash x = Ast_diff.ast_hash y
          | Sexp_diff.Delete (_, i), Sexp_diff.Delete (_, j) -> i = j
          | _ -> false)
       g1 g2

(* A side's net value at a path, from its own edit group: the LAST
   edit's payload (a Delete+Insert replace-via-splice reports the
   inserted payload; an all-Delete group reports None = deleted). *)
let group_value (g : Sexp_diff.edit list) : Ast.expr option =
  let rec go = function
    | [] -> None
    | [ Sexp_diff.Delete _ ] -> None
    | [ Sexp_diff.Replace (_, _, to_) ] -> Some to_
    | [ Sexp_diff.Insert (_, _, x) ] -> Some x
    | _ :: r -> go r
  in
  go g

(* ---- merge ---- *)

(* Per-path disposition: OKeep = ours' script survives at this path
   (ours-only edits, and shared edits kept once via ours' copy);
   TKeep = theirs-only edit survives; Dropped = conflicted away — the
   base version stands there. *)
type status = OKeep | TKeep | Dropped

let merge (base : Ast.expr) (ours : Ast.expr) (theirs : Ast.expr) : result =
  let oeds = Sexp_diff.diff base ours in
  let teds = Sexp_diff.diff base theirs in
  let key = Ast_path.path_to_string in
  (* group one side's script by path key, preserving script order *)
  let group (eds : Sexp_diff.edit list) :
      string list * (string, Ast_path.path * Sexp_diff.edit list) Hashtbl.t =
    let tbl : (string, Ast_path.path * Sexp_diff.edit list) Hashtbl.t =
      Hashtbl.create 32
    in
    let order = ref [] in
    List.iter
      (fun e ->
         let p = edit_path e in
         let k = key p in
         match Hashtbl.find_opt tbl k with
         | None ->
             Hashtbl.add tbl k (p, [ e ]);
             order := k :: !order
         | Some (_, es) -> Hashtbl.replace tbl k (p, es @ [ e ]))
      eds;
    (List.rev !order, tbl)
  in
  let okeys, otbl = group oeds in
  let tkeys, ttbl = group teds in
  let st : (string, status) Hashtbl.t = Hashtbl.create 32 in
  let status k = Hashtbl.find st k in
  let drop k = Hashtbl.replace st k Dropped in
  let conflicts = ref [] in
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let add_conflict (path : Ast_path.path) (ov : Ast.expr option)
      (tv : Ast.expr option) =
    let k = key path in
    if not (Hashtbl.mem seen k) then begin
      Hashtbl.add seen k ();
      conflicts := { path; ours = ov; theirs = tv } :: !conflicts
    end
  in
  (* phase A — same path on both sides: identical -> keep once (ours'
     copy; theirs' is excluded at compose because its status is not
     TKeep); different -> CONFLICT at the exact path. *)
  List.iter
    (fun ok ->
       match Hashtbl.find_opt ttbl ok with
       | None -> Hashtbl.replace st ok OKeep (* ours-only *)
       | Some (_, tg) ->
           let (_, og) = Hashtbl.find otbl ok in
           if groups_equal og tg then Hashtbl.replace st ok OKeep
           else begin
             let (p, _) = Hashtbl.find otbl ok in
             add_conflict p (group_value og) (group_value tg);
             drop ok
           end)
    okeys;
  (* theirs-only keys (never touch keys phase A already dispositioned) *)
  List.iter
    (fun tk -> if not (Hashtbl.mem st tk) then Hashtbl.replace st tk TKeep)
    tkeys;
  (* phase B — ancestor/descendant across sides: CONFLICT at the
     ancestor path; drop the ancestor-side edit and every edit strictly
     under it on the other side. *)
  let live_under (anc : Ast_path.path)
      (tbl : (string, Ast_path.path * 'a) Hashtbl.t) (keys : string list) =
    List.iter
      (fun k ->
         if status k <> Dropped then
           let (p, _) = Hashtbl.find tbl k in
           if is_under p anc then drop k)
      keys
  in
  List.iter
    (fun ok ->
       if status ok = Dropped then ()
       else
         let (op, _) = Hashtbl.find otbl ok in
         List.iter
           (fun tk ->
              if status tk = Dropped then ()
              else
                let (tp, _) = Hashtbl.find ttbl tk in
                if is_prefix op tp && not (is_prefix tp op) then begin
                  (* ours' path is the strict ancestor *)
                  add_conflict op
                    (Ast_path.select_path ours op)
                    (Ast_path.select_path theirs op);
                  drop ok;
                  live_under op ttbl tkeys
                end
                else if is_prefix tp op && not (is_prefix op tp) then begin
                  (* theirs' path is the strict ancestor *)
                  add_conflict tp
                    (Ast_path.select_path ours tp)
                    (Ast_path.select_path theirs tp);
                  drop tk;
                  live_under tp otbl okeys
                end)
           tkeys)
    okeys;
  (* phase C — both sides edited the SAME LIST CONTAINER (at different
     paths, else phases A/B handled it): cross-side index frames cannot
     be interleaved soundly -> CONFLICT at the parent path; drop every
     surviving edit in that list (both sides, shared included). *)
  let by_list :
    (string, Ast_path.path * string list * string list) Hashtbl.t =
    Hashtbl.create 16
  in
  let list_order = ref [] in
  let register tbl keys side =
    List.iter
      (fun k ->
         if status k <> Dropped then
           let (p, _) = Hashtbl.find tbl k in
           match list_parent p with
           | None -> ()
           | Some parent ->
               let pk = key parent in
               (match Hashtbl.find_opt by_list pk with
               | None ->
                   Hashtbl.add by_list pk (parent, [], []);
                   list_order := pk :: !list_order
               | Some _ -> ());
               let (pp, os, ts) = Hashtbl.find by_list pk in
               let (os, ts) =
                 match side with
                 | `Ours -> (k :: os, ts)
                 | `Theirs -> (os, k :: ts)
               in
               Hashtbl.replace by_list pk (pp, os, ts))
      keys
  in
  register otbl okeys `Ours;
  register ttbl tkeys `Theirs;
  List.iter
    (fun pk ->
       let (parent, os, ts) = Hashtbl.find by_list pk in
       (* non-shared live keys per side: a shared identical edit was
          already deduped and does not make the frames interact *)
       let nonshared side_keys other_tbl =
         List.filter
           (fun k -> not (Hashtbl.mem other_tbl k) && status k <> Dropped)
           side_keys
       in
       let o_live = nonshared os ttbl in
       let t_live = nonshared ts otbl in
       if o_live <> [] && t_live <> [] then begin
         add_conflict parent
           (Ast_path.select_path ours parent)
           (Ast_path.select_path theirs parent);
         List.iter drop os;
         List.iter drop ts
       end)
    (List.rev !list_order);
  (* compose: ours' surviving script (in ours' order), then theirs'
     surviving TKeep edits (in theirs' order). Survivors never share a
     list container, so per-list application-time frames stay intact. *)
  let kept (eds : Sexp_diff.edit list) (want : status) : Sexp_diff.edit list =
    List.filter
      (fun e ->
         match Hashtbl.find_opt st (key (edit_path e)) with
         | Some s -> s = want
         | None -> false)
      eds
  in
  {
    base;
    edits = kept oeds OKeep @ kept teds TKeep;
    conflicts = List.rev !conflicts;
  }
