(** Pricklypear Lisp — Evaluator (H1.7 direct-style).

    Takes an env and an expr, returns a value (or raises [Interp_error]).
    I/O prims use [Control.perform_prim] (OCaml 5 effects). The image rim
    ([Control_rim.run_lwt]) or pure tests ([Control.run_sync]) service
    [Prim_call]. No Lwt in this module.

    Per spec [lisp-interpreter.eval]: cooperative [budget], special forms,
    define binds env; image persists. *)
open Ast

(** A budget tracks a per-call step limit. Counted down at each eval
    step. When zero, raises [Interp_error Budget_exceeded]. *)
type budget = {
  mutable steps : int;
  limit : int;
}

let default_limit = 100_000

let make_budget ?(limit = default_limit) () : budget = { steps = 0; limit }

let tick (b : budget) : unit =
  b.steps <- b.steps + 1;
  if b.steps > b.limit then raise (Interp_error Budget_exceeded)

(** A primitive for pure tests / name registration: sync function.
    Image I/O prims are NOT stored here as Lwt — the rim handler owns those.
    Eval always [perform]s; [run_sync] invokes these sync fns. *)
type prim_fn = value list -> value

type prim_table = (string, prim_fn) Hashtbl.t

let make_prim_table () : prim_table = Hashtbl.create 32
let register_prim (tbl : prim_table) (name : string) (fn : prim_fn) : unit =
  Hashtbl.replace tbl name fn
let lookup_prim (tbl : prim_table) (name : string) : prim_fn option =
  Hashtbl.find_opt tbl name

(** Special-form recognition: an [App] whose head is [Var name] for one of
    these names is dispatched as the special form, not as a function call. *)
let is_special_form (name : string) : bool =
  match name with
  | "lambda" | "let" | "letrec" | "if" | "define" | "quote" | "seq"
  | "prim" | "with-transaction"
  | "quasiquote" | "unquote" | "unquote-splicing" | "defmacro"
  | "macroexpand-1" | "macroexpand" | "gensym"
  | "cond" | "and" | "or" -> true
  | _ -> false

let kernel_special_names =
  [ "lambda"; "let"; "letrec"; "if"; "define"; "quote"; "seq"; "prim";
    "with-transaction"; "quasiquote"; "unquote"; "unquote-splicing";
    "defmacro"; "macroexpand-1"; "macroexpand"; "gensym";
    "cond"; "and"; "or" ]

let truthy (v : value) : bool =
  match v with
  | VBool false | VNil -> false
  | _ -> true

(* Quote any expr into a first-class value (full s-expr quote).
   Compounds become nested VLists so quoted data is usable in Lisp. *)
(* Public quote: symbols and strings both become VStr (historical collapse). *)
let rec quote_to_value (e : expr) : value =
  match e with
  | Lit v -> v
  | Var s -> VStr s
  | App (h, args) ->
      VList (quote_to_value h :: List.map quote_to_value args)
  | Quote e' -> VList [ VStr "quote"; quote_to_value e' ]
  | If (t, th, el) ->
      VList [ VStr "if"; quote_to_value t; quote_to_value th; quote_to_value el ]
  | Lambda (params, body) ->
      VList [ VStr "lambda"
            ; VList (List.map (fun p -> VStr p) params)
            ; quote_to_value body ]
  | Let (name, v, body) ->
      VList [ VStr "let"
            ; VList [ VList [ VStr name; quote_to_value v ] ]
            ; quote_to_value body ]
  | Letrec (bindings, body) ->
      VList [ VStr "letrec"
            ; VList (List.map (fun (n, e') ->
                VList [ VStr n; quote_to_value e' ]) bindings)
            ; quote_to_value body ]
  | Define (name, v) ->
      VList [ VStr "define"; VStr name; quote_to_value v ]
  | Seq (a, b) ->
      VList [ VStr "seq"; quote_to_value a; quote_to_value b ]
  | Prim (name, args) ->
      VList (VStr name :: List.map quote_to_value args)

(** Macro reify: tag string literals so they survive value_to_expr as Lit VStr
    rather than Var (symbols are plain VStr). *)
let rec reify_expr (e : expr) : value =
  match e with
  | Lit (VStr s) -> VList [ VStr "__str__"; VStr s ]
  | Lit v -> v
  | Var s -> VStr s
  | App (h, args) ->
      VList (reify_expr h :: List.map reify_expr args)
  | Quote e' -> VList [ VStr "quote"; reify_expr e' ]
  | If (t, th, el) ->
      VList [ VStr "if"; reify_expr t; reify_expr th; reify_expr el ]
  | Lambda (params, body) ->
      VList [ VStr "lambda"
            ; VList (List.map (fun p -> VStr p) params)
            ; reify_expr body ]
  | Let (name, v, body) ->
      VList [ VStr "let"
            ; VList [ VList [ VStr name; reify_expr v ] ]
            ; reify_expr body ]
  | Letrec (bindings, body) ->
      VList [ VStr "letrec"
            ; VList (List.map (fun (n, e') ->
                VList [ VStr n; reify_expr e' ]) bindings)
            ; reify_expr body ]
  | Define (name, v) ->
      VList [ VStr "define"; VStr name; reify_expr v ]
  | Seq (a, b) ->
      VList [ VStr "seq"; reify_expr a; reify_expr b ]
  | Prim (name, args) ->
      VList (VStr name :: List.map reify_expr args)

(** Inverse of [reify_expr] / tagged quasiquote for macro expansion. *)
let rec value_to_expr (v : value) : expr =
  match v with
  | VList [ VStr "__str__"; VStr s ] -> Lit (VStr s)
  | VInt _ | VFloat _ | VBool _ | VNil as lit -> Lit lit
  | VStr s -> Var s
  | VList [] -> Lit VNil
  | VList (VStr "quote" :: [x]) -> Quote (value_to_expr x)
  | VList (VStr "if" :: [t; th; el]) ->
      If (value_to_expr t, value_to_expr th, value_to_expr el)
  | VList (VStr "lambda" :: VList params :: [body]) ->
      let ps =
        List.map
          (function
            | VStr p -> p
            | _ -> raise (Interp_error (User_error "value_to_expr: lambda param")))
          params
      in
      Lambda (ps, value_to_expr body)
  | VList (VStr "define" :: VStr name :: [body]) ->
      Define (name, value_to_expr body)
  | VList (VStr "let" :: VList binds :: [body]) ->
      (* only single-bind sugar from reify; multi via App form *)
      (match binds with
       | [ VList [ VStr name; v ] ] ->
           Let (name, value_to_expr v, value_to_expr body)
       | _ ->
           App (Var "let", [ value_to_expr (VList binds); value_to_expr body ]))
  | VList (VStr "seq" :: a :: b :: rest) ->
      let rec fold x xs =
        match xs with
        | [] -> value_to_expr x
        | y :: ys -> Seq (value_to_expr x, fold y ys)
      in
      fold a (b :: rest)
  | VList (h :: t) ->
      App (value_to_expr h, List.map value_to_expr t)
  | VPair (a, b) ->
      App (Var "cons", [ value_to_expr a; value_to_expr b ])
  | VLambda _ | VPrim _ | VRef _ | VMacro _ ->
      raise
        (Interp_error
           (User_error "value_to_expr: cannot reify live closure/prim/ref/macro"))

let gensym_counter = ref 0

let gensym ?(prefix = "g") () : string =
  incr gensym_counter;
  prefix ^ "$" ^ string_of_int !gensym_counter

let rec eval
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  (e : expr)
  : value =
  tick budget;
  match e with
  | Var name ->
      (match Env.lookup env name with
       | Some v -> v
       | None -> raise (Interp_error (Unbound_var name)))
  | Lit v -> v
  | Quote e' -> quote_to_value e'
  | If (test, then_e, else_e) ->
      let v = eval ~budget ~prims env test in
      (match v with
       | VBool b ->
           if b then eval ~budget ~prims env then_e
           else eval ~budget ~prims env else_e
       | VNil -> eval ~budget ~prims env else_e
       | _ -> eval ~budget ~prims env then_e)
  | Seq (a, b) ->
      ignore (eval ~budget ~prims env a);
      eval ~budget ~prims env b
  | Let (name, val_expr, body) ->
      let v = eval ~budget ~prims env val_expr in
      let env' = Env.child env in
      Env.define env' name v;
      eval ~budget ~prims env' body
  | Letrec (bindings, body) ->
      let env' = Env.child env in
      List.iter (fun (n, _) -> Env.define env' n VNil) bindings;
      List.iter
        (fun (n, e') ->
          let v = eval ~budget ~prims env' e' in
          Env.define env' n v)
        bindings;
      eval ~budget ~prims env' body
  | Lambda (params, body) -> VLambda (params, body, env)
  | Define (name, val_expr) ->
      let v = eval ~budget ~prims env val_expr in
      Env.define env name v;
      v
  | Prim (name, args) ->
      let arg_vals = List.map (eval ~budget ~prims env) args in
      (* Membership checked by the effect handler (run_sync / Control_rim). *)
      let _ = prims in
      Control.perform_prim name arg_vals
  | App (head, args) ->
      (match head with
       | Var name when is_special_form name ->
           eval_special_form ~budget ~prims env name (head :: args)
       | Var name ->
           (match Env.lookup env name with
            | Some (VMacro (params, body, clo_env)) ->
                expand_and_eval_macro ~budget ~prims env
                  ~params ~body ~clo_env ~arg_exprs:args
            | _ ->
                let f = eval ~budget ~prims env head in
                let arg_vals = List.map (eval ~budget ~prims env) args in
                apply ~budget ~prims f arg_vals)
       | _ ->
           let f = eval ~budget ~prims env head in
           let arg_vals = List.map (eval ~budget ~prims env) args in
           apply ~budget ~prims f arg_vals)

and expand_and_eval_macro
  ~(budget : budget)
  ~(prims : prim_table)
  (call_env : env)
  ~(params : string list)
  ~(body : expr)
  ~(clo_env : env)
  ~(arg_exprs : expr list)
  : value =
  tick budget;
  let expected = List.length params
  and got = List.length arg_exprs in
  if expected <> got then
    raise (Interp_error (Arity_mismatch ("macro", expected, got)));
  let arg_vals =
    List.map
      (fun e ->
        match e with
        | Lit v -> v
        | _ -> reify_expr e)
      arg_exprs
  in
  let menv = Env.child clo_env in
  List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
  let expansion_v = eval ~budget ~prims menv body in
  let expansion_e =
    try value_to_expr expansion_v
    with Interp_error _ as ex -> raise ex
       | _ ->
           raise
             (Interp_error
                (User_error "macro expansion is not a valid expr tree"))
  in
  eval ~budget ~prims call_env expansion_e

and eval_special_form
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  (name : string)
  (full_args : expr list)
  : value =
  match name, full_args with
  | "lambda", [ _head; params_expr; body ] ->
      let params =
        match params_expr with
        | Lit VNil -> []
        | App (Var p, rest) ->
            p
            :: List.map
                 (fun e ->
                   match e with
                   | Var s -> s
                   | _ ->
                       raise
                         (Interp_error (User_error "lambda param not a symbol")))
                 rest
        | _ -> raise (Interp_error (User_error "malformed lambda params"))
      in
      if List.mem "." params then
        raise
          (Interp_error
             (User_error
                "dotted rest params not supported (reader treats \".\" as a \
                 symbol); use fixed-arity params"));
      VLambda (params, body, env)
  | "let", _ ->
      (match full_args with
       | [ _; bindings_expr; body ] ->
           (match bindings_expr with
            | Lit VNil -> eval ~budget ~prims env body
            | App (App (Var _, _) as first_binding, rest_bindings) ->
                let all_bindings = first_binding :: rest_bindings in
                let env' = Env.child env in
                List.iter
                  (fun bp ->
                    match bp with
                    | App (Var n, [ val_expr ]) ->
                        let v = eval ~budget ~prims env val_expr in
                        Env.define env' n v
                    | _ -> raise (Interp_error (User_error "bad let binding")))
                  all_bindings;
                eval ~budget ~prims env' body
            | _ -> raise (Interp_error (User_error "bad let bindings")))
       | _ -> raise (Interp_error (User_error "let needs bindings + body")))
  | "if", [ _; test; then_e; else_e ] ->
      let v = eval ~budget ~prims env test in
      (match v with
       | VBool b -> eval ~budget ~prims env (if b then then_e else else_e)
       | VNil -> eval ~budget ~prims env else_e
       | _ -> eval ~budget ~prims env then_e)
  | "define", [ _; Var n; val_expr ] ->
      let v = eval ~budget ~prims env val_expr in
      Env.define env n v;
      v
  | "quote", [ _; e' ] -> quote_to_value e'
  | "seq", _head :: bodies ->
      (match bodies with
       | [] -> VNil
       | [ only ] -> eval ~budget ~prims env only
       | first :: rest ->
           ignore (eval ~budget ~prims env first);
           eval_special_form ~budget ~prims env "seq" (Var "seq" :: rest))
  | "prim", _ ->
      (match full_args with
       | [ _; Var prim_name; body ] ->
           let arg_vals = [ eval ~budget ~prims env body ] in
           Control.perform_prim prim_name arg_vals
       | _ -> raise (Interp_error (User_error "prim needs name + arg")))
  | "with-transaction", _ ->
      let body_expr_opt =
        match full_args with
        | [ _; body_expr ] -> Some body_expr
        | [ _; _isolation; body_expr ] -> Some body_expr
        | _ -> None
      in
      (match body_expr_opt with
       | None ->
           raise
             (Interp_error
                (User_error "with-transaction needs a lambda body"))
       | Some body_expr ->
           let lambda_v = eval ~budget ~prims env body_expr in
           (match lambda_v with
            | VLambda _ ->
                Control.perform_prim "__with_transaction__" [ lambda_v ]
            | _ ->
                raise
                  (Interp_error
                     (User_error "with-transaction body must be a lambda"))))
  | "gensym", [ _ ] -> VStr (gensym ())
  | "gensym", [ _; Lit (VStr prefix) ] | "gensym", [ _; Var prefix ] ->
      VStr (gensym ~prefix ())
  | "and", _head :: args ->
      let rec loop = function
        | [] -> VBool true
        | [ last ] -> eval ~budget ~prims env last
        | e' :: rest ->
            let v = eval ~budget ~prims env e' in
            if truthy v then loop rest else v
      in
      loop args
  | "or", _head :: args ->
      let rec loop = function
        | [] -> VBool false
        | [ last ] -> eval ~budget ~prims env last
        | e' :: rest ->
            let v = eval ~budget ~prims env e' in
            if truthy v then v else loop rest
      in
      loop args
  | "cond", _head :: clauses ->
      let rec loop = function
        | [] -> VNil
        | clause :: rest ->
            let test_e, body_e =
              match clause with
              | App (t, [ b ]) -> (t, b)
              | App (t, b :: bs) ->
                  let rec fold x xs =
                    match xs with
                    | [] -> x
                    | y :: ys -> Seq (x, fold y ys)
                  in
                  (t, fold b bs)
              | Lit _ | Var _ as t -> (t, t)
              | _ ->
                  raise
                    (Interp_error
                       (User_error "cond clause must be (test body)"))
            in
            let always =
              match test_e with
              | Var "else" | Lit (VBool true) -> true
              | _ -> false
            in
            if always then eval ~budget ~prims env body_e
            else
              let tv = eval ~budget ~prims env test_e in
              if truthy tv then eval ~budget ~prims env body_e
              else loop rest
      in
      loop clauses
  | "quasiquote", [ _; template ] ->
      eval_quasiquote ~budget ~prims env ~depth:1 template
  | "unquote", _ | "unquote-splicing", _ ->
      raise (Interp_error (User_error "unquote outside quasiquote"))
  | "defmacro", [ _; Var name; params_expr; body ] ->
      if List.mem name kernel_special_names then
        raise
          (Interp_error
             (User_error ("cannot defmacro kernel special form: " ^ name)));
      let params =
        match params_expr with
        | Lit VNil -> []
        | App (Var p, rest) ->
            p
            :: List.map
                 (function
                   | Var s -> s
                   | _ ->
                       raise
                         (Interp_error
                            (User_error "defmacro param not a symbol")))
                 rest
        | _ -> raise (Interp_error (User_error "malformed defmacro params"))
      in
      if List.mem "." params then
        raise
          (Interp_error
             (User_error
                "dotted rest params not supported (reader treats \".\" as a \
                 symbol); use fixed-arity params, e.g. (name val body)"));
      let m = VMacro (params, body, env) in
      Env.define env name m;
      m
  | "macroexpand-1", [ _; form ] ->
      (match form with
       | App (Var name, args) ->
           (match Env.lookup env name with
            | Some (VMacro (params, body, clo_env)) ->
                let expected = List.length params
                and got = List.length args in
                if expected <> got then
                  raise
                    (Interp_error (Arity_mismatch ("macro", expected, got)));
                let arg_vals =
                  List.map
                    (fun e ->
                      match e with
                      | Lit v -> v
                      | _ -> reify_expr e)
                    args
                in
                let menv = Env.child clo_env in
                List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
                eval ~budget ~prims menv body
            | _ -> reify_expr form)
       | _ -> reify_expr form)
  | "macroexpand", [ _; form ] ->
      let rec loop e =
        tick budget;
        match e with
        | App (Var name, args) ->
            (match Env.lookup env name with
             | Some (VMacro (params, body, clo_env)) ->
                 let expected = List.length params
                 and got = List.length args in
                 if expected <> got then
                   raise
                     (Interp_error (Arity_mismatch ("macro", expected, got)));
                 let arg_vals =
                   List.map
                     (fun e' ->
                       match e' with
                       | Lit v -> v
                       | _ -> reify_expr e')
                     args
                 in
                 let menv = Env.child clo_env in
                 List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
                 let v = eval ~budget ~prims menv body in
                 let e' = value_to_expr v in
                 loop e'
             | _ -> reify_expr e)
        | _ -> reify_expr e
      in
      loop form
  | "letrec", _ ->
      raise
        (Interp_error
           (User_error "letrec via App form not yet supported"))
  | other, _ ->
      raise
        (Interp_error (User_error ("malformed special form: " ^ other)))

and eval_quasiquote
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  ~(depth : int)
  (template : expr)
  : value =
  tick budget;
  match template with
  | App (Var "unquote", [ e ]) when depth = 1 ->
      eval ~budget ~prims env e
  | App (Var "unquote", [ e ]) ->
      let inner = eval_quasiquote ~budget ~prims env ~depth:(depth - 1) e in
      VList [ VStr "unquote"; inner ]
  | App (Var "unquote-splicing", _) when depth = 1 ->
      raise
        (Interp_error (User_error "unquote-splicing not in list context"))
  | App (Var "quasiquote", [ e ]) ->
      let inner =
        eval_quasiquote ~budget ~prims env ~depth:(depth + 1) e
      in
      VList [ VStr "quasiquote"; inner ]
  | App (h, args) ->
      let head_part =
        match h with
        | Var "unquote-splicing" when depth = 1 ->
            raise
              (Interp_error
                 (User_error "unquote-splicing as list head not supported"))
        | _ ->
            let hv = eval_quasiquote ~budget ~prims env ~depth h in
            [ hv ]
      in
      let tail_parts =
        List.map
          (fun a ->
            match a with
            | App (Var "unquote-splicing", [ e ]) when depth = 1 ->
                let v = eval ~budget ~prims env e in
                (match v with
                 | VList xs -> xs
                 | VNil -> []
                 | _ ->
                     raise
                       (Interp_error
                          (User_error "unquote-splicing needs a list")))
            | _ ->
                let v = eval_quasiquote ~budget ~prims env ~depth a in
                [ v ])
          args
      in
      VList (head_part @ List.concat tail_parts)
  | Lit (VStr s) -> VList [ VStr "__str__"; VStr s ]
  | Lit v -> v
  | Var s -> VStr s
  | Quote e ->
      let v = eval_quasiquote ~budget ~prims env ~depth e in
      VList [ VStr "quote"; v ]
  | other -> reify_expr other

and apply
  ~(budget : budget)
  ~(prims : prim_table)
  (f : value)
  (args : value list)
  : value =
  match f with
  | VLambda (params, body, clo_env) ->
      let expected = List.length params
      and got = List.length args in
      if expected <> got then
        raise (Interp_error (Arity_mismatch ("lambda", expected, got)));
      let call_env = Env.child clo_env in
      List.iter2 (fun p a -> Env.define call_env p a) params args;
      eval ~budget ~prims call_env body
  | VMacro _ ->
      raise
        (Interp_error
           (User_error
              "macro must be applied by name (not as first-class apply)"))
  | VPrim name ->
      let _ = prims in
      Control.perform_prim name args
  | other -> raise (Interp_error (Not_a_function other))

let eval_string_direct ~(prims : prim_table) (env : env) (s : string) : value =
  let budget = make_budget () in
  let e = Reader.read_one s in
  eval ~budget ~prims env e

(** Evaluate under [Control.run_sync] (pure tests; sync prim table). *)
let eval_string ~(prims : prim_table) (env : env) (s : string) : value =
  Control.run_sync ~prims (fun () -> eval_string_direct ~prims env s)

let empty_prims : prim_table = make_prim_table ()

(** Aliases kept for call-site stability during H1 cleanup. *)
let apply_eff = apply
let eval_eff = eval
let eval_string_eff = eval_string_direct
let eval_string_auto = eval_string
