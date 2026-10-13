(** Quote/value ↔ expr bridge — split out of eval.ml (i-1788671219
    walker split). Pure over Ast.expr / Ast.value; eval.ml re-exports
    [quote_to_value] / [reify_expr] / [value_to_expr] / [name_of_atom] /
    [head_name] via aliases (image, pure, agent, remote, and tests use
    the Eval.* names). *)
open Ast

(** Quote any expr into a first-class value (full s-expr quote).
    Compounds become nested VLists so quoted data is usable in Lisp.
    Vars become VSymbol; string lits stay VStr (no collapse). *)
let rec quote_to_value (e : expr) : value =
  let sym s = VSymbol s in
  match e with
  | Lit v -> v
  | Var s -> VSymbol s
  | App (h, args) ->
      VList (quote_to_value h :: List.map quote_to_value args)
  | Quote e' -> VList [ sym "quote"; quote_to_value e' ]
  | If (t, th, el) ->
      VList [ sym "if"; quote_to_value t; quote_to_value th; quote_to_value el ]
  | Lambda (params, body) ->
      VList [ sym "lambda"
            ; VList (List.map (fun p -> VSymbol p) params)
            ; quote_to_value body ]
  | Let (name, v, body) ->
      VList [ sym "let"
            ; VList [ VList [ VSymbol name; quote_to_value v ] ]
            ; quote_to_value body ]
  | Letrec (bindings, body) ->
      VList [ sym "letrec"
            ; VList (List.map (fun (n, e') ->
                VList [ VSymbol n; quote_to_value e' ]) bindings)
            ; quote_to_value body ]
  | Define (name, v) ->
      VList [ sym "define"; VSymbol name; quote_to_value v ]
  | Seq (a, b) ->
      VList [ sym "seq"; quote_to_value a; quote_to_value b ]
  | Prim (name, args) ->
      VList (sym name :: List.map quote_to_value args)

(** Macro reify: same as quote_to_value now that symbols exist.
    __str__ tagging retained only via value_to_expr compat path. *)
let reify_expr (e : expr) : value = quote_to_value e

(** Name atom: VSymbol preferred; bare VStr still accepted as a name
    during compat (old trees / hand-built lists of strings). *)
let name_of_atom ?(ctx = "value_to_expr") = function
  | VSymbol s | VStr s -> s
  | _ ->
      raise (Interp_error (User_error (ctx ^ ": expected symbol or string name", "bad-args")))

(** Head name of a value if it is a symbol or string atom. *)
let head_name = function
  | VSymbol s | VStr s -> Some s
  | _ -> None

(** Inverse of quote_to_value / macro expansion trees. *)
let rec value_to_expr (v : value) : expr =
  match v with
  | VList [ VStr "__str__"; VStr s ] -> Lit (VStr s) (* transitional *)
  | VList [ VSymbol "__str__"; VStr s ] -> Lit (VStr s)
  | VInt _ | VFloat _ | VBool _ | VNil as lit -> Lit lit
  | VSymbol s -> Var s
  | VStr s -> Lit (VStr s) (* string data, not a name *)
  | VList [] -> Lit VNil
  | VList (h :: t) ->
      (match head_name h, t with
       | Some "quote", [ x ] -> Quote (value_to_expr x)
       | Some "if", [ te; th; el ] ->
           If (value_to_expr te, value_to_expr th, value_to_expr el)
       | Some "lambda", [ VList params; body ] ->
           let ps =
             List.map
               (fun p -> name_of_atom ~ctx:"value_to_expr:lambda" p)
               params
           in
           Lambda (ps, value_to_expr body)
       | Some "define", [ name_v; body ] ->
           Define
             ( name_of_atom ~ctx:"value_to_expr:define" name_v
             , value_to_expr body )
       | Some "let", [ VList binds; body ] ->
           (match binds with
            | [ VList [ name_v; bv ] ] ->
                Let
                  ( name_of_atom ~ctx:"value_to_expr:let" name_v
                  , value_to_expr bv
                  , value_to_expr body )
            | _ ->
                App
                  ( Var "let"
                  , [ value_to_expr (VList binds); value_to_expr body ] ))
       | Some "seq", a :: b :: rest ->
           let rec fold x xs =
             match xs with
             | [] -> value_to_expr x
             | y :: ys -> Seq (value_to_expr x, fold y ys)
           in
           fold a (b :: rest)
       | _ -> App (value_to_expr h, List.map value_to_expr t))
  | VPair (a, b) ->
      App (Var "cons", [ value_to_expr a; value_to_expr b ])
  | VLambda _ | VPrim _ | VRef _ | VMacro _ | VThunk _ ->
      raise
        (Interp_error
           (User_error ("value_to_expr: cannot reify live closure/prim/ref/macro/thunk", "bad-args")))
