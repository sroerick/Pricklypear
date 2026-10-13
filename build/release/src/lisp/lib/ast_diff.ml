(** Pure structural diff over [Ast.expr] trees.

    Consumer: [branching.diff] — [Store.diff_branches] feeds the parsed
    from/to bodies of a modified [function_defs] row into [diff] so the
    /diff page can overlay the delta tree on [Ast_html.pp_expr_html].

    No I/O, no Postgres — tests with [dune runtest lisp] alone, like
    reader / eval / env.

    Semantics authority: [branching.diff]. This module is the pure
    walker (lisp-interpreter.ast-diff), NOT the surface prim — a
    programmatic [(diff-ast name from to)] prim is explicit v2.

    Non-goals (v1, per branching.diff): rename detection (a bound-var
    rename surfaces as Replaced at the binding site AND every reference
    site), alpha-equivalence (bound names are NOT normalized), and
    param-order is a *positional* child delta, not "renamed params". *)

let sha1_hex (s : string) : string =
  let d = Digestif.SHA1.digest_string s in
  Digestif.SHA1.to_hex d

(** Content identity: sha1 hex of [Reader.pp_expr]. Bytes-identical to
    [Analyze.ast_hash] in image/lib/analyze.ml (same sha1_hex over the
    same pp_expr string) so the fast path below agrees with the SQL
    layer's hashes. *)
let ast_hash (e : Ast.expr) : string = sha1_hex (Reader.pp_expr e)

type delta =
  | Same
  | Replaced of { from : Ast.expr; to_ : Ast.expr }
  | Node of { ctor : string; children : (string * delta) list }

(** Sentinel for the absent side of a list-level add/remove. The locked
    [delta] type has no Added/Removed variant, so an element present on
    only one side of a list (App arg / Letrec binding / Lambda param) is
    a [Replaced] whose other side is this sentinel. A renderer can
    present a [Replaced] where one side is the sentinel as pure
    add/remove. *)
let absent = Ast.nil

(** Longest common subsequence of index pairs, keyed by [key]. Result
    pairs are strictly increasing in both indices (standard LCS
    back-track). *)
let lcs_pairs (key : 'a -> string) (xs : 'a list) (ys : 'a list) :
    (int * int) list =
  let xa = Array.of_list xs and ya = Array.of_list ys in
  let n = Array.length xa and m = Array.length ya in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      dp.(i).(j) <-
        if key xa.(i) = key ya.(j) then 1 + dp.(i + 1).(j + 1)
        else max dp.(i + 1).(j) dp.(i).(j + 1)
    done
  done;
  let rec go i j acc =
    if i < n && j < m then
      if key xa.(i) = key ya.(j) then go (i + 1) (j + 1) ((i, j) :: acc)
      else if dp.(i + 1).(j) >= dp.(i).(j + 1) then go (i + 1) j acc
      else go i (j + 1) acc
    else List.rev acc
  in
  go 0 0 []

(** List children from a list-LCS over [xs]/[ys] matched at [pairs].
    Matched pairs recurse [diff]; a lone element on only one side is
    converted to a delta by [wrap] (which decides which side is absent
    from [side]). Generic over the element type so it serves both App
    args (expr) and Letrec bindings (string * expr). *)
let list_children ~(label : 'a -> string) ~(diff : 'a -> 'a -> delta)
    ~(wrap : [ `Left | `Right ] -> 'a -> delta)
    (xs : 'a list) (ys : 'a list) (pairs : (int * int) list) :
    (string * delta) list =
  let xa = Array.of_list xs and ya = Array.of_list ys in
  let n = Array.length xa and m = Array.length ya in
  let pair_map = Hashtbl.create 16 in
  List.iter (fun (i, j) -> Hashtbl.replace pair_map i j) pairs;
  let out = ref [] in
  let i = ref 0 and j = ref 0 in
  while !i < n || !j < m do
    if !i < n && Hashtbl.mem pair_map !i then begin
      let jj = Hashtbl.find pair_map !i in
      while !j < jj do
        out := (label ya.(!j), wrap `Right ya.(!j)) :: !out;
        incr j
      done;
      out := (label xa.(!i), diff xa.(!i) ya.(!j)) :: !out;
      incr i;
      incr j
    end
    else if !i < n then begin
      out := (label xa.(!i), wrap `Left xa.(!i)) :: !out;
      incr i
    end
    else begin
      out := (label ya.(!j), wrap `Right ya.(!j)) :: !out;
      incr j
    end
  done;
  List.rev !out

(** Positional children: pair [xs]/[ys] up by index (no LCS). Used where
    order matters and is meant to be visible (Lambda params — see
    non-goal "param order matters"). Generic via [wrap] like
    [list_children]. *)
let positional_children ~(label : 'a -> string) ~(diff : 'a -> 'a -> delta)
    ~(wrap : [ `Left | `Right ] -> 'a -> delta)
    (xs : 'a list) (ys : 'a list) : (string * delta) list =
  let rec go xs ys acc =
    match xs, ys with
    | [], [] -> List.rev acc
    | x :: xr, y :: yr -> go xr yr ((label x, diff x y) :: acc)
    | x :: xr, [] -> go xr [] ((label x, wrap `Left x) :: acc)
    | [], y :: yr -> go [] yr ((label y, wrap `Right y) :: acc)
  in
  go xs ys []

(** Structural diff between two exprs.

    Fast path first: if the content hashes are equal, the trees are
    identical and we skip the walk entirely (returns [Same]). The 90%
    common case (unchanged defs between branches) costs one hash
    compare per row, not a tree walk. *)
let rec diff (from_e : Ast.expr) (to_e : Ast.expr) : delta =
  if ast_hash from_e = ast_hash to_e then Same else walk from_e to_e

and walk (f : Ast.expr) (t : Ast.expr) : delta =
  let wrap_expr side e =
    match side with
    | `Left -> Replaced { from = e; to_ = absent }
    | `Right -> Replaced { from = absent; to_ = e }
  in
  match f, t with
  (* Leaf constructors: structural = on the payload. *)
  | Var a, Var b -> if a = b then Same else Replaced { from = f; to_ = t }
  | Lit a, Lit b -> if a = b then Same else Replaced { from = f; to_ = t }
  (* Fixed-arity constructors: recurse positionally. *)
  | If (a1, a2, a3), If (b1, b2, b3) ->
      Node {
        ctor = "if";
        children = [
          ("test", diff a1 b1);
          ("then", diff a2 b2);
          ("else", diff a3 b3);
        ];
      }
  | Seq (a1, a2), Seq (b1, b2) ->
      Node { ctor = "seq"; children = [ ("a", diff a1 b1); ("b", diff a2 b2) ] }
  | Lambda (ps1, b1), Lambda (ps2, b2) ->
      let params =
        Node {
          ctor = "params";
          children =
            positional_children ~label:(fun _ -> "param")
              ~diff:(fun a b ->
                if a = b then Same
                else Replaced { from = Ast.var a; to_ = Ast.var b })
              ~wrap:(fun side s ->
                match side with
                | `Left -> Replaced { from = Ast.var s; to_ = absent }
                | `Right -> Replaced { from = absent; to_ = Ast.var s })
              ps1 ps2;
        }
      in
      Node { ctor = "lambda"; children = [ ("params", params); ("body", diff b1 b2) ] }
  | Let (n1, v1, b1), Let (n2, v2, b2) ->
      Node {
        ctor = "let";
        children = [
          ("name", if n1 = n2 then Same else Replaced { from = Ast.var n1; to_ = Ast.var n2 });
          ("val", diff v1 v2);
          ("body", diff b1 b2);
        ];
      }
  | Define (n1, v1), Define (n2, v2) ->
      Node {
        ctor = "define";
        children = [
          ("name", if n1 = n2 then Same else Replaced { from = Ast.var n1; to_ = Ast.var n2 });
          ("val", diff v1 v2);
        ];
      }
  | Quote e1, Quote e2 -> Node { ctor = "quote"; children = [ ("body", diff e1 e2) ] }
  (* List children: App head recurses; args use list-LCS keyed on hash so
     an insertion mid-sequence does not mark every following arg changed. *)
  | App (fh, args1), App (th, args2) ->
      Node {
        ctor = "app";
        children =
          ("fn", diff fh th)
          :: list_children ~label:(fun _ -> "arg") ~diff ~wrap:wrap_expr
               args1 args2 (lcs_pairs ast_hash args1 args2);
      }
  | Prim (n1, a1), Prim (n2, a2) ->
      if n1 <> n2 then Replaced { from = f; to_ = t }
      else
        Node {
          ctor = "prim";
          children =
            list_children ~label:(fun _ -> "arg") ~diff ~wrap:wrap_expr a1 a2
              (lcs_pairs ast_hash a1 a2);
        }
  | Letrec (bs1, b1), Letrec (bs2, b2) ->
      Node {
        ctor = "letrec";
        children =
          list_children ~label:(fun (n, _) -> n)
            ~diff:(fun (_, e1) (_, e2) -> diff e1 e2)
            ~wrap:(fun side (_, e) ->
              match side with
              | `Left -> Replaced { from = e; to_ = absent }
              | `Right -> Replaced { from = absent; to_ = e })
            bs1 bs2 (lcs_pairs (fun (n, _) -> n) bs1 bs2)
          @ [ ("body", diff b1 b2) ];
      }
  (* Different constructors: structural replacement. *)
  | _, _ -> Replaced { from = f; to_ = t }
