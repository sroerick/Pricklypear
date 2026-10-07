(** Addressable AST nodes: paths over [Ast.expr] (branching ast-nodes
    parts 1-2, 2026-08-16 design, BORGE-1786925842).

    A node inside a whole-expr body is addressed by a PATH from the
    root, in the same vocabulary the [Ast_diff] walk already labels
    its children with:

      positional: if.test if.then if.else lambda.params lambda.body
                  let.name let.val let.body define.name define.val
                  seq.a seq.b quote.body
      list:       app.fn app.arg[i] prim.arg[i]
                  letrec.binding[NAME] lambda.param[i]

    Example path: [lambda.body.app.arg[1].app.fn] — the head expr of
    the first argument of the lambda body. Identity is the path
    itself, computed on the fly — NOT a materialized table. Pure
    helpers, no I/O, no schema change: function bodies stay whole-expr
    JSONB.

    [select_path] returns the sub-expr at the path ([None] when the
    path does not apply: wrong ctor, index out of range, unknown
    binding name). [replace_path] returns the root with the node at
    the path replaced; a non-applying path leaves the tree UNCHANGED
    (total function — a stale edit degrades to a no-op, it never
    crashes; callers that need to know re-[select_path] and compare).

    [lambda.params] is a synthetic container in the [Ast_diff] walk (a
    params LIST is not an [expr]). It stays in the vocabulary for
    grouping/rendering, but [select_path] answers [None] for it —
    address individual params through [LambdaParam i], which surfaces
    the name as a [Var] (the same choice [Ast_diff] makes: param
    deltas are [Replaced { from = Var a; to_ = Var b }]). [LetName] /
    [DefineName] likewise surface the name as a [Var]; renaming
    through [replace_path] requires the replacement to be a [Var]
    (anything else is a no-op, mirroring the leaf-ness of names).

    Letrec bindings are addressed BY NAME ([LetrecBinding "f"]), not
    by position — names are the stable identity of a binding across
    the list-LCS in [Ast_diff]. [select_path] finds the first binding
    with that name; [replace_path] rewrites the first binding with
    that name and leaves the rest alone. *)

type step =
  | IfTest
  | IfThen
  | IfElse
  | LambdaParams  (* synthetic container — see header note *)
  | LambdaBody
  | LambdaParam of int
  | LetName
  | LetVal
  | LetBody
  | DefineName
  | DefineVal
  | SeqA
  | SeqB
  | QuoteBody
  | AppFn
  | AppArg of int
  | PrimArg of int
  | LetrecBinding of string
  | LetrecBody

type path = step list

let step_to_string (s : step) : string =
  match s with
  | IfTest -> "if.test"
  | IfThen -> "if.then"
  | IfElse -> "if.else"
  | LambdaParams -> "lambda.params"
  | LambdaBody -> "lambda.body"
  | LambdaParam i -> Printf.sprintf "lambda.param[%d]" i
  | LetName -> "let.name"
  | LetVal -> "let.val"
  | LetBody -> "let.body"
  | DefineName -> "define.name"
  | DefineVal -> "define.val"
  | SeqA -> "seq.a"
  | SeqB -> "seq.b"
  | QuoteBody -> "quote.body"
  | AppFn -> "app.fn"
  | AppArg i -> Printf.sprintf "app.arg[%d]" i
  | PrimArg i -> Printf.sprintf "prim.arg[%d]" i
  | LetrecBinding n -> "letrec.binding[" ^ n ^ "]"
  | LetrecBody -> "letrec.body"

let path_to_string (p : path) : string =
  String.concat "." (List.map step_to_string p)

(** The sub-expr at [p], or [None] when [p] does not apply. *)
let rec select_path (e : Ast.expr) (p : path) : Ast.expr option =
  match p with
  | [] -> Some e
  | step :: rest -> (
      let child : Ast.expr option =
        match (step, e) with
        | IfTest, Ast.If (t, _, _) -> Some t
        | IfThen, Ast.If (_, th, _) -> Some th
        | IfElse, Ast.If (_, _, el) -> Some el
        | LambdaBody, Ast.Lambda (_, b) -> Some b
        | LambdaParam i, Ast.Lambda (ps, _) -> (
            match List.nth_opt ps i with Some s -> Some (Ast.Var s) | None -> None)
        | LetName, Ast.Let (n, _, _) -> Some (Ast.Var n)
        | LetVal, Ast.Let (_, v, _) -> Some v
        | LetBody, Ast.Let (_, _, b) -> Some b
        | DefineName, Ast.Define (n, _) -> Some (Ast.Var n)
        | DefineVal, Ast.Define (_, v) -> Some v
        | SeqA, Ast.Seq (a, _) -> Some a
        | SeqB, Ast.Seq (_, b) -> Some b
        | QuoteBody, Ast.Quote q -> Some q
        | AppFn, Ast.App (f, _) -> Some f
        | AppArg i, Ast.App (_, args) -> List.nth_opt args i
        | PrimArg i, Ast.Prim (_, args) -> List.nth_opt args i
        | LetrecBinding name, Ast.Letrec (bs, _) -> (
            match List.assoc_opt name bs with Some b -> Some b | None -> None)
        | LetrecBody, Ast.Letrec (_, b) -> Some b
        | _ -> None (* step does not apply to this ctor (incl. LambdaParams) *)
      in
      match child with Some c -> select_path c rest | None -> None)

(* Replace the element at index [i] of [lst] with [f old]; out-of-range
   [i] leaves the list unchanged. *)
let list_replace_at (lst : 'a list) (i : int) (f : 'a -> 'a) : 'a list =
  List.mapi (fun j x -> if j = i then f x else x) lst

(** The root with the node at [p] replaced by [repl]. A non-applying
    path returns the tree unchanged (total, no exception). *)
let rec replace_path (p : path) (repl : Ast.expr) (e : Ast.expr) : Ast.expr =
  match p with
  | [] -> repl
  | step :: rest -> (
      match (step, e) with
      | IfTest, Ast.If (t, th, el) -> Ast.If (replace_path rest repl t, th, el)
      | IfThen, Ast.If (t, th, el) -> Ast.If (t, replace_path rest repl th, el)
      | IfElse, Ast.If (t, th, el) -> Ast.If (t, th, replace_path rest repl el)
      | LambdaBody, Ast.Lambda (ps, b) -> Ast.Lambda (ps, replace_path rest repl b)
      | LambdaParam i, Ast.Lambda (ps, b) -> (
          match rest with
          | [] -> (
              (* names are leaves: the replacement must be a Var *)
              match repl with
              | Ast.Var s -> Ast.Lambda (list_replace_at ps i (fun _ -> s), b)
              | _ -> Ast.Lambda (ps, b))
          | _ -> Ast.Lambda (ps, b) (* a Var has no children: deeper path no-ops *))
      | LetName, Ast.Let (n, v, b) -> (
          match rest with
          | [] -> (
              match repl with
              | Ast.Var s -> Ast.Let (s, v, b)
              | _ -> Ast.Let (n, v, b))
          | _ -> Ast.Let (n, v, b))
      | LetVal, Ast.Let (n, v, b) -> Ast.Let (n, replace_path rest repl v, b)
      | LetBody, Ast.Let (n, v, b) -> Ast.Let (n, v, replace_path rest repl b)
      | DefineName, Ast.Define (n, v) -> (
          match rest with
          | [] -> (
              match repl with
              | Ast.Var s -> Ast.Define (s, v)
              | _ -> Ast.Define (n, v))
          | _ -> Ast.Define (n, v))
      | DefineVal, Ast.Define (n, v) -> Ast.Define (n, replace_path rest repl v)
      | SeqA, Ast.Seq (a, b) -> Ast.Seq (replace_path rest repl a, b)
      | SeqB, Ast.Seq (a, b) -> Ast.Seq (a, replace_path rest repl b)
      | QuoteBody, Ast.Quote q -> Ast.Quote (replace_path rest repl q)
      | AppFn, Ast.App (f, args) -> Ast.App (replace_path rest repl f, args)
      | AppArg i, Ast.App (f, args) ->
          Ast.App (f, list_replace_at args i (replace_path rest repl))
      | PrimArg i, Ast.Prim (n, args) ->
          Ast.Prim (n, list_replace_at args i (replace_path rest repl))
      | LetrecBinding name, Ast.Letrec (bs, b) ->
          let rec go = function
            | [] -> bs (* unknown binding name: unchanged *)
            | (n, e) :: rest_bs ->
                if n = name then (n, replace_path rest repl e) :: rest_bs
                else (n, e) :: go rest_bs
          in
          Ast.Letrec (go bs, b)
      | LetrecBody, Ast.Letrec (bs, b) ->
          Ast.Letrec (bs, replace_path rest repl b)
      | _ -> e (* non-applying step: unchanged *))
