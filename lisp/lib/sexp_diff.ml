(** Pure structural diff of two exprs into a FLAT path-keyed edit
    script, plus [apply] (branching sexp-diff; ast-nodes parts 1-2).

    NO second comparison algorithm: every decision is [Ast_diff]'s,
    1:1. [edits] consults [Ast_diff.diff] for every pair — the same
    Same / Replaced / Node verdicts, including the per-subtree
    ast_hash short-circuit — and recovers list positions with
    [Ast_diff.lcs_pairs] under the same [Ast_diff.ast_hash] key the
    walk uses (App args, Prim args; Letrec bindings keyed by NAME).
    Lambda params stay positional (the walk's locked non-goal: param
    order is visible, never normalized). This module only FLATTENS
    those decisions into the ast-nodes edit grammar:

      Replace (path, from, to)   node at [path] replaced
      Insert (path, idx, expr)   list-child inserted
      Delete (path, idx)         list-child removed

    Paths are [Ast_path] element paths. Insert/Delete name the
    AFFECTED LIST CHILD: the last step is app.arg[i] / prim.arg[i] /
    lambda.param[i] / letrec.binding[NAME] over the parent node's
    list (App/Prim args, Lambda params, Letrec bindings). For a
    letrec insert the binding NAME rides in the path step and the
    payload is the binding BODY — the name is never lost.

    Application semantics (the invariant the round-trip property
    tests): edits apply strictly LEFT-TO-RIGHT in script order, and
    every index is the list position AT THE MOMENT THAT EDIT IS
    APPLIED. For the script [diff] emits this means: everything that
    survives carries its TARGET position (so a mid-sequence insert
    never shifts the paths of following siblings), and each deletion
    carries the element's current position among the not-yet-placed
    tail. The Replaced-with-absent-sentinel ambiguity of the delta
    tree never reaches this flat form: one-sided list children become
    Insert/Delete from the LCS pairing itself, never by sniffing the
    sentinel — a literal [nil] arg changing to something else is a
    Delete+Insert pair, not an "insert" misread.

    [apply] is total: a Replace/Insert/Delete whose path no longer
    applies (wrong ctor, out-of-range index, unknown binding) is a
    no-op, never an exception. Round-trip correctness
    ([apply (diff a b) a = b]) is property-tested in
    lisp/test/sexp_diff_test.ml over generated pairs of all 11 expr
    constructors.

    Pure, no I/O — testable with [dune runtest lisp] alone. Consumer
    (planned): three-way merge groups these scripts by path
    (branching ast-nodes part 3 / subsection ast-merge). *)

type edit =
  | Replace of Ast_path.path * Ast.expr * Ast.expr
  | Insert of Ast_path.path * int * Ast.expr
  | Delete of Ast_path.path * int

(** Human-readable one-line form of an edit (error messages / logs). *)
let edit_to_string (e : edit) : string =
  match e with
  | Replace (p, from, to_) ->
      Printf.sprintf "Replace(%s, %s -> %s)"
        (Ast_path.path_to_string p)
        (Reader.pp_expr_flat from) (Reader.pp_expr_flat to_)
  | Insert (p, idx, x) ->
      Printf.sprintf "Insert(%s, %d, %s)"
        (Ast_path.path_to_string p) idx (Reader.pp_expr_flat x)
  | Delete (p, idx) ->
      Printf.sprintf "Delete(%s, %d)" (Ast_path.path_to_string p) idx

let edits_to_string (eds : edit list) : string =
  "[" ^ String.concat "; " (List.map edit_to_string eds) ^ "]"

(* Duplicate binding names make letrec.binding[NAME] addressing
   ambiguous: two matched pairs would emit two identical-path Replaces
   that both mutate the FIRST binding, and a same-named insert landing
   before a matched binding steals the first-match address. So when
   EITHER side carries a duplicate name, the whole bindings container
   diffs POSITIONALLY: every from-binding is deleted at its (current)
   position, every to-binding inserted at its target position — the
   names still ride in the path steps, but selection is never by name. *)
let has_duplicate_names (bs : (string * _) list) : bool =
  let seen = Hashtbl.create 16 in
  List.exists
    (fun (n, _) ->
       if Hashtbl.mem seen n then true
       else begin
         Hashtbl.add seen n ();
         false
       end)
    bs

(** Structural diff [from -> to] as a flat edit script. [] when the
    exprs are content-identical (the [Ast_diff] hash short-circuit). *)
let rec diff (f : Ast.expr) (t : Ast.expr) : edit list = edits [] f t

(** The decision for the pair [f]/[t] comes from [Ast_diff.diff]; the
    ctor arm below only DECOMPOSES what the walk already decided. *)
and edits (path : Ast_path.path) (f : Ast.expr) (t : Ast.expr) : edit list =
  match Ast_diff.diff f t with
  | Ast_diff.Same -> []
  | Ast_diff.Replaced { from; to_ } -> [ Replace (path, from, to_) ]
  | Ast_diff.Node { ctor; _ } -> (
      match (ctor, f, t) with
      | "if", Ast.If (f1, f2, f3), Ast.If (t1, t2, t3) ->
          edits (path @ [ Ast_path.IfTest ]) f1 t1
          @ edits (path @ [ Ast_path.IfThen ]) f2 t2
          @ edits (path @ [ Ast_path.IfElse ]) f3 t3
      | "seq", Ast.Seq (a1, b1), Ast.Seq (a2, b2) ->
          edits (path @ [ Ast_path.SeqA ]) a1 a2
          @ edits (path @ [ Ast_path.SeqB ]) b1 b2
      | "lambda", Ast.Lambda (ps1, b1), Ast.Lambda (ps2, b2) ->
          param_edits path ps1 ps2 @ edits (path @ [ Ast_path.LambdaBody ]) b1 b2
      | "let", Ast.Let (n1, v1, b1), Ast.Let (n2, v2, b2) ->
          (if n1 = n2 then []
           else [ Replace (path @ [ Ast_path.LetName ], Ast.var n1, Ast.var n2) ])
          @ edits (path @ [ Ast_path.LetVal ]) v1 v2
          @ edits (path @ [ Ast_path.LetBody ]) b1 b2
      | "define", Ast.Define (n1, v1), Ast.Define (n2, v2) ->
          (if n1 = n2 then []
           else [ Replace (path @ [ Ast_path.DefineName ], Ast.var n1, Ast.var n2) ])
          @ edits (path @ [ Ast_path.DefineVal ]) v1 v2
      | "quote", Ast.Quote e1, Ast.Quote e2 ->
          edits (path @ [ Ast_path.QuoteBody ]) e1 e2
      | "app", Ast.App (fh, args1), Ast.App (th, args2) ->
          edits (path @ [ Ast_path.AppFn ]) fh th
          @ list_edits ~path ~elem:(fun i -> Ast_path.AppArg i) args1 args2
      | "prim", Ast.Prim (_, args1), Ast.Prim (_, args2) ->
          (* a Prim name change is a whole-node Replaced (walk decision),
             so both sides here share the same prim name *)
          list_edits ~path ~elem:(fun i -> Ast_path.PrimArg i) args1 args2
      | "letrec", Ast.Letrec (bs1, b1), Ast.Letrec (bs2, b2) ->
          binding_edits path bs1 bs2 @ edits (path @ [ Ast_path.LetrecBody ]) b1 b2
      | _ ->
          (* unreachable: Node implies same ctor with same Prim name *)
          [ Replace (path, f, t) ])

(** Lambda params: positional pairing, mirroring
    [Ast_diff.positional_children] (order matters, no LCS). [i] walks
    the source params, [k] is the CURRENT/TARGET position (deletes do
    not advance it — after removing a param, the next one sits at the
    same application-time index). *)
and param_edits (path : Ast_path.path) (ps1 : string list) (ps2 : string list) :
    edit list =
  let rec go i k ps1 ps2 =
    match (ps1, ps2) with
    | [], [] -> []
    | p1 :: r1, p2 :: r2 ->
        edits (path @ [ Ast_path.LambdaParam k ]) (Ast.var p1) (Ast.var p2)
        @ go (i + 1) (k + 1) r1 r2
    | _ :: r1, [] ->
        Delete (path @ [ Ast_path.LambdaParam k ], k) :: go (i + 1) k r1 []
    | [], p2 :: r2 ->
        Insert (path @ [ Ast_path.LambdaParam k ], k, Ast.var p2)
        :: go i (k + 1) [] r2
  in
  go 0 0 ps1 ps2

(* Shared walk over a LIST container's LCS pairing — the exact walk
   order of [Ast_diff.list_children] (matched pair: flush the target
   elements before it as inserts, then recurse; lone from-side element
   at current position [j]: Delete; lone to-side element: Insert).
   Generic in the element-step constructor so it serves App args
   ([AppArg i]) and Prim args ([PrimArg i]). *)
and list_edits ~(path : Ast_path.path) ~(elem : int -> Ast_path.step)
    (xs : Ast.expr list) (ys : Ast.expr list) : edit list =
  let xa = Array.of_list xs and ya = Array.of_list ys in
  let n = Array.length xa and m = Array.length ya in
  let partner = Hashtbl.create 16 in
  List.iter (fun (i, j) -> Hashtbl.replace partner i j)
    (Ast_diff.lcs_pairs Ast_diff.ast_hash xs ys);
  (* APPEND-only accumulator: the script must stay in walk order
     (left-to-right); a matched chunk is appended, never prepended. *)
  let out = ref [] in
  let emit e = out := !out @ [ e ] in
  let i = ref 0 and j = ref 0 in
  while !i < n || !j < m do
    if !i < n && Hashtbl.mem partner !i then begin
      let jj = Hashtbl.find partner !i in
      (* ya[j..jj-1] insert before the matched element *)
      while !j < jj do
        emit (Insert (path @ [ elem !j ], !j, ya.(!j)));
        incr j
      done;
      out := !out @ edits (path @ [ elem jj ]) xa.(!i) ya.(jj);
      incr i;
      incr j
    end
    else if !i < n then begin
      (* xa[i] dropped: its current position is j (everything before it
         is already placed) *)
      emit (Delete (path @ [ elem !j ], !j));
      incr i
    end
    else begin
      emit (Insert (path @ [ elem !j ], !j, ya.(!j)));
      incr j
    end
  done;
  !out

(* Letrec bindings: LCS keyed on NAME (the walk's key), bodies recursed
   under [letrec.binding[NAME]]. Inserts carry the binding name in the
   path step and the BODY as the payload. *)
and binding_edits (path : Ast_path.path)
    (bs1 : (string * Ast.expr) list) (bs2 : (string * Ast.expr) list) :
    edit list =
  (* APPEND-only accumulator — see list_edits. Critical here: a matched
     (same-name) binding whose BODY changed contributes a non-empty
     chunk, and edits emitted after it (later deletes/inserts) must
     stay AFTER it in the script. *)
  let out = ref [] in
  let emit e = out := !out @ [ e ] in
  if has_duplicate_names bs1 || has_duplicate_names bs2 then begin
    (* positional degradation — see has_duplicate_names: every from-
       binding is deleted at its current position (0 after each prior
       delete), every to-binding inserted at its target position. *)
    List.iter
      (fun (nm, _) -> emit (Delete (path @ [ Ast_path.LetrecBinding nm ], 0)))
      bs1;
    List.iteri
      (fun k (nm, e) ->
        emit (Insert (path @ [ Ast_path.LetrecBinding nm ], k, e)))
      bs2;
    !out
  end
  else begin
    let xa = Array.of_list bs1 and ya = Array.of_list bs2 in
    let n = Array.length xa and m = Array.length ya in
    let partner = Hashtbl.create 16 in
    List.iter (fun (i, j) -> Hashtbl.replace partner i j)
      (Ast_diff.lcs_pairs (fun (nm, _) -> nm) bs1 bs2);
    let i = ref 0 and j = ref 0 in
    while !i < n || !j < m do
      if !i < n && Hashtbl.mem partner !i then begin
        let jj = Hashtbl.find partner !i in
        while !j < jj do
          let (nmj, ej) = ya.(!j) in
          emit (Insert (path @ [ Ast_path.LetrecBinding nmj ], !j, ej));
          incr j
        done;
        let (_, e1) = xa.(!i) in
        let (nm2, e2) = ya.(jj) in
        out := !out @ edits (path @ [ Ast_path.LetrecBinding nm2 ]) e1 e2;
        incr i;
        incr j
      end
      else if !i < n then begin
        let (nmi, _) = xa.(!i) in
        emit (Delete (path @ [ Ast_path.LetrecBinding nmi ], !j));
        incr i
      end
      else begin
        let (nmj, ej) = ya.(!j) in
        emit (Insert (path @ [ Ast_path.LetrecBinding nmj ], !j, ej));
        incr j
      end
    done;
    !out
  end

(* ---- apply ---- *)

(* Insert [x] so it lands at index [idx] (clamped to [0, len]); idx =
   len appends. *)
let splice_insert (lst : 'a list) (idx : int) (x : 'a) : 'a list =
  let idx = max 0 idx in
  let rec go k = function
    | [] -> [ x ]
    | hd :: tl -> if k = idx then x :: hd :: tl else hd :: go (k + 1) tl
  in
  go 0 lst

(* Remove the element at index [idx]; out-of-range leaves the list
   unchanged. *)
let splice_delete (lst : 'a list) (idx : int) : 'a list =
  let rec go k = function
    | [] -> []
    | _ :: tl when k = idx -> tl
    | hd :: tl -> hd :: go (k + 1) tl
  in
  go 0 lst

(* Apply one list-shaped edit (Insert/Delete) to the parent node:
   the last path step says WHICH list of the parent changes. *)
let apply_list_edit (parent : Ast.expr) (last : Ast_path.step) (idx : int)
    (payload : Ast.expr option) : Ast.expr =
  match (last, parent) with
  | Ast_path.AppArg _, Ast.App (f, args) -> (
      match payload with
      | Some x -> Ast.App (f, splice_insert args idx x)
      | None -> Ast.App (f, splice_delete args idx))
  | Ast_path.PrimArg _, Ast.Prim (n, args) -> (
      match payload with
      | Some x -> Ast.Prim (n, splice_insert args idx x)
      | None -> Ast.Prim (n, splice_delete args idx))
  | Ast_path.LambdaParam _, Ast.Lambda (ps, b) -> (
      match payload with
      | Some (Ast.Var s) -> Ast.Lambda (splice_insert ps idx s, b)
      | Some _ -> parent (* params are names: a non-Var payload cannot apply *)
      | None -> Ast.Lambda (splice_delete ps idx, b))
  | Ast_path.LetrecBinding nm, Ast.Letrec (bs, b) -> (
      match payload with
      | Some body -> Ast.Letrec (splice_insert bs idx (nm, body), b)
      | None -> Ast.Letrec (splice_delete bs idx, b))
  | _ -> parent (* last step is not a list-child step: no-op *)

(** Replay an edit script onto [root], left-to-right. Every index is
    the list position at the moment its edit is applied (the invariant
    [diff]'s output honors). Edits whose path no longer applies are
    no-ops: [apply] is total. *)
let apply (eds : edit list) (root : Ast.expr) : Ast.expr =
  let butlast p = match List.rev p with _ :: r -> List.rev r | [] -> [] in
  let last_step p = List.hd (List.rev p) in
  let apply_splice (root : Ast.expr) (p : Ast_path.path) (idx : int)
      (payload : Ast.expr option) : Ast.expr =
    let parent_path = butlast p in
    match Ast_path.select_path root parent_path with
    | Some parent ->
        let parent' = apply_list_edit parent (last_step p) idx payload in
        Ast_path.replace_path parent_path parent' root
    | None -> root
  in
  List.fold_left
    (fun root ed ->
       match ed with
       | Replace (p, _, to_) -> Ast_path.replace_path p to_ root
       | Insert (p, idx, x) -> apply_splice root p idx (Some x)
       | Delete (p, idx) -> apply_splice root p idx None)
    root eds
