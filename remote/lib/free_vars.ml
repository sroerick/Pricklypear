(** remote/lib/free_vars.ml — free-variable analysis over a Nopales AST.

    Used by [--lens-pull-tree] (remote/bin/main.ml) to walk the transitive
    closure of a fetched def's dependencies: every free [Var] is a
    candidate def to pull from the image. The reader produces special
    forms as [App(Var "lambda", …)] (eval desugars later), so we dispatch
    on those App forms — binding lambda params / let names so they
    aren't mistaken for deps, and treating [quote]/[quasiquote] bodies
    as data (not code). Kernel special-form names are never deps.

    Pure (no I/O, no env); unit-tested in remote/test/free_vars_test.ml. *)

open Pricklypear_lisp

(** [lambda_params] extracts param names from the reader's App form of a
    lambda param list: (n) -> App(Var "n",[]); (a b) ->
    App(Var "a",[Var "b"]); () -> Lit VNil. *)
let lambda_params (pe : Ast.expr) : string list =
  match pe with
  | Ast.Lit Ast.VNil -> []
  | Ast.App (Ast.Var p, rest) ->
      p :: List.filter_map (function Ast.Var s -> Some s | _ -> None) rest
  | _ -> []

(** [let_bindings] extracts (name, val_expr) pairs from the reader's App
    form of a let/letrec binding list: ((a 1) (b 2)) ->
    App(App(Var "a",[1]), [App(Var "b",[2])]). *)
let let_bindings (be : Ast.expr) : (string * Ast.expr) list =
  match be with
  | Ast.Lit Ast.VNil -> []
  | Ast.App (first, rest) ->
      List.filter_map
        (fun b ->
          match b with
          | Ast.App (Ast.Var n, [ val_e ]) -> Some (n, val_e)
          | _ -> None)
        (first :: rest)
  | _ -> []

(** Free variables referenced in an [expr], minus [bound]. Every free
    [Var] is a candidate def to pull. The reader produces special forms
    as [App(Var "lambda", …)] (eval desugars later), so we dispatch on
    those App forms: binding lambda params / let names, treating
    [quote]/[quasiquote] bodies as data. Kernel special-form names are
    never deps. The desugared AST nodes (Lambda/Let/…) are also handled
    so the walker is correct against either shape. *)
let rec free_vars (bound : string list) (e : Ast.expr) : string list =
  let open Ast in
  let is_kernel n = List.mem n Eval.kernel_special_names in
  match e with
  | Var s -> if List.mem s bound || is_kernel s then [] else [ s ]
  | Lit _ -> []
  | App (Var f, args) when is_kernel f ->
      (match f, args with
       | "lambda", params_e :: bodies ->
           let params = lambda_params params_e in
           List.concat_map (free_vars (params @ bound)) bodies
       | "let", bindings_e :: body :: _ ->
           let bs = let_bindings bindings_e in
           let names = List.map fst bs in
           let vals = List.map snd bs in
           List.concat_map (free_vars bound) vals @ free_vars (names @ bound) body
       | "letrec", bindings_e :: body :: _ ->
           let bs = let_bindings bindings_e in
           let names = List.map fst bs in
           let bound' = names @ bound in
           List.concat_map (free_vars bound') (List.map snd bs)
           @ free_vars bound' body
       | "define", name_or_app :: rest ->
           (* (define name val) or (define (name ps…) body…). For the
              function form, the params in the (name ps…) App are bound
              in the body — extract them so they aren't mistaken for
              deps. The name itself is bound (the def being defined). *)
           let bound' =
             match name_or_app with
             | Ast.App (Ast.Var _, pargs) ->
                 List.filter_map (function Ast.Var s -> Some s | _ -> None) pargs
             | _ -> []
           in
           List.concat_map (free_vars (bound' @ bound)) rest
       | ("quote"), _ -> []
       | ("quasiquote" | "unquote" | "unquote-splicing"), _ -> []
       | ("defmacro" | "macroexpand" | "macroexpand-1" | "gensym" | "error"), _ ->
           []
       | ( "if" | "begin" | "seq" | "cond" | "and" | "or" | "match" | "guard"
         | "apply" | "list/map" | "list/filter" | "list/foldl" | "list/foldr"
         | "prim" | "delay" | "force" | "with-transaction" | "with-step" ), _
         ->
           List.concat_map (free_vars bound) args
       | _ -> List.concat_map (free_vars bound) args)
  | App (f, args) ->
      free_vars bound f @ List.concat_map (free_vars bound) args
  | Let (name, val_e, body) ->
      free_vars bound val_e @ free_vars (name :: bound) body
  | Letrec (bindings, body) ->
      let names = List.map fst bindings in
      let bound' = names @ bound in
      List.concat_map (fun (_, e') -> free_vars bound' e') bindings
      @ free_vars bound' body
  | If (t, a, b) -> free_vars bound t @ free_vars bound a @ free_vars bound b
  | Lambda (params, body) -> free_vars (params @ bound) body
  | Prim (_, args) -> List.concat_map (free_vars bound) args
  | Seq (a, b) -> free_vars bound a @ free_vars bound b
  | Quote _ -> []
  | Define (name, val_e) -> free_vars (name :: bound) val_e

(** Free vars across a whole source string (all top-level forms). Used by
    [--lens-pull-tree] to scan a fetched def's source for transitive
    deps. Returns [] on a reader error (conservative: a def we can't
    parse has no scan-able deps — the puller will still materialize the
    def itself; it just won't recurse its body). *)
let free_vars_of_source (src : string) : string list =
  try List.concat_map (free_vars []) (Reader.read_all src)
  with _ -> []
