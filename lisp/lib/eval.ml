(** Pricklypear Lisp — Evaluator.

    Lwt-typed. Takes an env and an expr, returns a value (or raises
    [Interp_error]) wrapped in a promise `value Lwt.t`. All side effects
    go through [Prim] calls, which the IMAGE layer wires to real
    Postgres/HTTP. The interpreter itself does no I/O; pure primitives
    just wrap their results in [Lwt.return].

    Per spec [lisp-interpreter.eval]:

      - Cooperative evaluation: the eval loop is stepped with a
        [Budget.t] counter checked at every Prim invocation and every
        recursive eval depth. When exceeded, that ONE call aborts; the
        session continues. Prevents "user's bad function wedges the image".

      - Special forms [lambda/let/letrec/if/define/quote] handled in
        eval, not the reader.

      - [define] at top-level adds binding to the session env AND
        (in the IMAGE layer) persists to the store. Here it just binds;
        the image layer's primitive wrapper handles persistence.

      - Primitives are dispatched by name through a [prim_table] the
        IMAGE layer registers at boot. The interpreter is Lwt-typed so
        that DB-touching primitives (fork, merge, get, create, etc.)
        can return promises directly, with no special-case intercept
        machinery at the REPL. Pure primitives wrap their results in
        [Lwt.return].

    Per directive: threading Lwt through eval is the unblock for the
    data half of the OS. Reads (`(get 'type id)`) need the DB result as
    the return value; the sync ABI `value list -> value` could not
    provide that. *)
open Lwt
open Lwt.Syntax
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

(** A primitive: takes evaluated args, returns a value promise.
    Pure primitives wrap results in [Lwt.return]; DB-touching
    primitives return the DB promise directly. *)
type prim_fn = value list -> value Lwt.t

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
  : value Lwt.t =
  tick budget;
  match e with
  | Var name ->
      (match Env.lookup env name with
       | Some v -> Lwt.return v
       | None -> raise (Interp_error (Unbound_var name)))
  | Lit v -> Lwt.return v
  | Quote e' ->
      (* Quote any s-expr into a value form (BORGE-1783383905):
         Lit -> value, Var -> VStr name, App -> VList of quoted parts,
         other expr constructors -> nested VList encoding. *)
      Lwt.return (quote_to_value e')
  | If (test, then_e, else_e) ->
      let* v = eval ~budget ~prims env test in
      (match v with
       | VBool b -> if b then eval ~budget ~prims env then_e
                    else eval ~budget ~prims env else_e
       | VNil -> eval ~budget ~prims env else_e
       | _ -> eval ~budget ~prims env then_e)  (* non-nil, non-false is truthy *)
  | Seq (a, b) ->
      let* _ = eval ~budget ~prims env a in
      eval ~budget ~prims env b
  | Let (name, val_expr, body) ->
      let* v = eval ~budget ~prims env val_expr in
      let env' = Env.child env in
      Env.define env' name v;
      eval ~budget ~prims env' body
  | Letrec (bindings, body) ->
      let env' = Env.child env in
      (* pre-bind all names to VNil, then evaluate each binding *)
      List.iter (fun (n, _) -> Env.define env' n VNil) bindings;
      let* _ =
        Lwt_list.iter_s
          (fun (n, e) -> let* v = eval ~budget ~prims env' e in
                         Env.define env' n v; Lwt.return_unit)
          bindings
      in
      eval ~budget ~prims env' body
  | Lambda (params, body) -> Lwt.return (VLambda (params, body, env))
  | Define (name, val_expr) ->
      let* v = eval ~budget ~prims env val_expr in
      Env.define env name v;
      Lwt.return v  (* define returns the bound value *)
  | Prim (name, args) ->
      (* Evaluate args left-to-right in sequence (let* over list). *)
      let* arg_vals =
        Lwt_list.map_s (eval ~budget ~prims env) args
      in
      (match lookup_prim prims name with
       | Some fn -> fn arg_vals
       | None -> raise (Interp_error (Prim_error (name, "unregistered primitive"))))
  | App (head, args) ->
      (* Special forms first; then macro expand if head is bound to VMacro;
         else ordinary application. *)
      (match head with
       | Var name when is_special_form name ->
           eval_special_form ~budget ~prims env name (head :: args)
       | Var name ->
           (match Env.lookup env name with
            | Some (VMacro (params, body, clo_env)) ->
                expand_and_eval_macro ~budget ~prims env
                  ~params ~body ~clo_env ~arg_exprs:args
            | _ ->
                let* f = eval ~budget ~prims env head in
                let* arg_vals = Lwt_list.map_s (eval ~budget ~prims env) args in
                apply ~budget ~prims f arg_vals)
       | _ ->
           let* f = eval ~budget ~prims env head in
           let* arg_vals = Lwt_list.map_s (eval ~budget ~prims env) args in
           apply ~budget ~prims f arg_vals)

and expand_and_eval_macro
  ~(budget : budget)
  ~(prims : prim_table)
  (call_env : env)
  ~(params : string list)
  ~(body : expr)
  ~(clo_env : env)
  ~(arg_exprs : expr list)
  : value Lwt.t =
  tick budget;
  let expected = List.length params
  and got = List.length arg_exprs in
  if expected <> got then
    raise (Interp_error (Arity_mismatch ("macro", expected, got)));
  let arg_vals = List.map reify_expr arg_exprs in
  let menv = Env.child clo_env in
  List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
  let* expansion_v = eval ~budget ~prims menv body in
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
  : value Lwt.t =
  (* these mirror the [Let/Letrec/If/Seq/Lambda/Define/Quote] expr cases,
     but for the surface-syntax App form (when read from text). The
     reader produces App(Var "if", [t; th; el]) rather than If(...). *)
  match name, full_args with
  | "lambda", [ _head; params_expr; body ] ->
      (* full_args = [Var "lambda"; params_expr; body]. Params can be:
         - ()  -> Lit VNil (zero params)
         - (p1 p2 ...) -> App(Var p1, [Var p2; ...]) — collect head + args
         Reader does NOT treat "." as dotted-pair syntax, so (a . b) is
         four symbols including ".". Reject that with a clear error. *)
      let params =
        match params_expr with
        | Lit VNil -> []
        | App (Var p, rest) ->
            (* head is the first param, rest are the rest *)
            p :: List.map (fun e ->
              match e with
              | Var s -> s
              | _ -> raise (Interp_error (User_error "lambda param not a symbol")))
              rest
        | _ -> raise (Interp_error (User_error "malformed lambda params"))
      in
      if List.mem "." params then
        raise
          (Interp_error
             (User_error
                "dotted rest params not supported (reader treats \".\" as a \
                 symbol); use fixed-arity params"));
      Lwt.return (VLambda (params, body, env))
  | "let", _ ->
      (* (let ((name val) ...) body) — full_args = [Var "let"; bindings; body] *)
      (match full_args with
       | [ _; bindings_expr; body ] ->
           (match bindings_expr with
            | Lit VNil ->
                (* (let () body) *)
                eval ~budget ~prims env body
            | App (App (Var _, _) as first_binding, rest_bindings) ->
                (* bindings list: head is the first binding, rest are more *)
                let all_bindings = first_binding :: rest_bindings in
                let env' = Env.child env in
                let* _ =
                  Lwt_list.iter_s
                    (fun bp ->
                      match bp with
                      | App (Var name, [ val_expr ]) ->
                          let* v = eval ~budget ~prims env val_expr in
                          Env.define env' name v;
                          Lwt.return_unit
                      | _ -> raise (Interp_error (User_error "bad let binding")))
                    all_bindings
                in
                eval ~budget ~prims env' body
            | App (Var name, [ val_expr ]) when full_args = [] ->
                (* single binding without outer parens — not standard; skip *)
                raise (Interp_error (User_error "let bindings missing"))
            | _ -> raise (Interp_error (User_error "bad let bindings")))
       | _ -> raise (Interp_error (User_error "let needs bindings + body")))
  | "if", [ _; test; then_e; else_e ] ->
      let* v = eval ~budget ~prims env test in
      (match v with
       | VBool b -> eval ~budget ~prims env (if b then then_e else else_e)
       | VNil -> eval ~budget ~prims env else_e
       | _ -> eval ~budget ~prims env then_e)
  | "define", [ _; Var name; val_expr ] ->
      let* v = eval ~budget ~prims env val_expr in
      Env.define env name v;
      Lwt.return v
  | "quote", [ _; e' ] ->
      Lwt.return (quote_to_value e')
  | "seq", _head :: bodies ->
      (* Surface (seq e1 e2 … en): evaluate left-to-right for effects,
         return the last body's value. (seq) → VNil; (seq e) → e.
         Binary Ast.Seq constructor remains for internal chains; this
         branch handles the reader App form with any arity. *)
      (match bodies with
       | [] -> Lwt.return VNil
       | [only] -> eval ~budget ~prims env only
       | first :: rest ->
           let* _ = eval ~budget ~prims env first in
           eval_special_form ~budget ~prims env "seq"
             (Var "seq" :: rest))
  | "prim", _ ->
      (* (prim name args...) — full_args = [Var "prim"; Var name; args...] *)
      (match full_args with
       | [ _; Var prim_name; body ] ->
           let* arg_vals =
             Lwt_list.map_s (eval ~budget ~prims env) [body]
           in
           (match lookup_prim prims prim_name with
            | Some fn -> fn arg_vals
            | None -> raise (Interp_error (Prim_error (prim_name, "unregistered primitive"))))
       | _ -> raise (Interp_error (User_error "prim needs name + arg")))
  | "letrec", _ ->
      raise (Interp_error (User_error "letrec via App form not yet supported"))
  | "with-transaction", _ ->
      (* (with-transaction (lambda () body...)) -> runs the lambda body
         inside the dynamic transaction binding established by the
         image layer's private __with_transaction__ prim. The prim
         receives the VLambda value, calls it via Eval.apply inside
         Store.with_dynamic_transaction (so prims called from the body
         consult the in-transaction pgx conn via current_conn).

         Optional isolation arg (v1.5): (with-transaction 'read-committed
         ...) — currently ignored, defaults to SERIALIZABLE.

         We DON'T handle the body directly in lisp/ (the transactional
         binding is I/O — pgx — lives in the image layer); we delegate
         to the registered __with_transaction__ prim. If no image-layer
         prim is registered (e.g. pure lisp tests), we raise a clean
         error so the absence is observable. *)
      (match full_args with
       | [ _; body_expr ] ->
           let* lambda_v = eval ~budget ~prims env body_expr in
           (match lambda_v with
            | VLambda _ ->
                (match lookup_prim prims "__with_transaction__" with
                 | Some fn -> fn [lambda_v]
                 | None ->
                     raise (Interp_error
                              (Prim_error ("with-transaction",
                                           "no __with_transaction__ prim registered"))))
            | _ ->
                raise (Interp_error
                         (User_error "with-transaction body must be a lambda")))
       | [ _; _isolation_arg; body_expr ] ->
           (* (with-transaction 'isolation body) — isolation honored in v1.5 *)
           let* lambda_v = eval ~budget ~prims env body_expr in
           (match lambda_v with
            | VLambda _ ->
                (match lookup_prim prims "__with_transaction__" with
                 | Some fn -> fn [lambda_v]
                 | None ->
                     raise (Interp_error
                              (Prim_error ("with-transaction",
                                           "no __with_transaction__ prim registered"))))
            | _ ->
                raise (Interp_error
                         (User_error "with-transaction body must be a lambda")))
       | _ ->
           raise (Interp_error
                    (User_error "with-transaction needs a lambda body")))
  | "gensym", [ _ ] ->
      Lwt.return (VStr (gensym ()))
  | "gensym", [ _; Lit (VStr prefix) ] | "gensym", [ _; Var prefix ] ->
      Lwt.return (VStr (gensym ~prefix ()))
  | "and", _head :: args ->
      (* (and e1 e2 …) short-circuit; (and) → #t; returns last truthy / first falsey *)
      let rec loop = function
        | [] -> Lwt.return (VBool true)
        | [ last ] -> eval ~budget ~prims env last
        | e :: rest ->
            let* v = eval ~budget ~prims env e in
            if truthy v then loop rest else Lwt.return v
      in
      loop args
  | "or", _head :: args ->
      (* (or e1 e2 …) short-circuit; (or) → #f; returns first truthy / last falsey *)
      let rec loop = function
        | [] -> Lwt.return (VBool false)
        | [ last ] -> eval ~budget ~prims env last
        | e :: rest ->
            let* v = eval ~budget ~prims env e in
            if truthy v then Lwt.return v else loop rest
      in
      loop args
  | "cond", _head :: clauses ->
      (* (cond (test body) …) — body is single expr; test #t or else is always true *)
      let rec loop = function
        | [] -> Lwt.return VNil
        | clause :: rest ->
            let test_e, body_e =
              match clause with
              | App (t, [ b ]) -> (t, b)
              | App (t, b :: bs) ->
                  (* multi-body: treat as seq *)
                  let rec fold x xs =
                    match xs with
                    | [] -> x
                    | y :: ys -> Seq (x, fold y ys)
                  in
                  (t, fold b bs)
              | Lit _ | Var _ as t ->
                  (* bare test with no body → return test value if truthy *)
                  (t, t)
              | _ ->
                  raise (Interp_error (User_error "cond clause must be (test body)"))
            in
            let always =
              match test_e with
              | Var "else" | Lit (VBool true) -> true
              | _ -> false
            in
            if always then eval ~budget ~prims env body_e
            else
              let* tv = eval ~budget ~prims env test_e in
              if truthy tv then eval ~budget ~prims env body_e else loop rest
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
      (* Same reader limitation as lambda: "." is a symbol, not rest syntax.
         Inbox c7-2611 repro used (name val . body) and looked like a let-crash. *)
      if List.mem "." params then
        raise
          (Interp_error
             (User_error
                "dotted rest params not supported (reader treats \".\" as a \
                 symbol); use fixed-arity params, e.g. (name val body)"));
      let m = VMacro (params, body, env) in
      Env.define env name m;
      Lwt.return m
  | "macroexpand-1", [ _; form ] ->
      (match form with
       | App (Var name, args) ->
           (match Env.lookup env name with
            | Some (VMacro (params, body, clo_env)) ->
                let expected = List.length params
                and got = List.length args in
                if expected <> got then
                  raise (Interp_error (Arity_mismatch ("macro", expected, got)));
                let arg_vals = List.map reify_expr args in
                let menv = Env.child clo_env in
                List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
                eval ~budget ~prims menv body
            | _ -> Lwt.return (reify_expr form))
       | _ -> Lwt.return (reify_expr form))
  | "macroexpand", [ _; form ] as whole ->
      (* expand until fixed point or budget *)
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
                 let arg_vals = List.map reify_expr args in
                 let menv = Env.child clo_env in
                 List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
                 let* v = eval ~budget ~prims menv body in
                 let e' = value_to_expr v in
                 loop e'
             | _ -> Lwt.return (reify_expr e))
        | _ -> Lwt.return (reify_expr e)
      in
      let _ = whole in
      loop form
  | _ ->
      raise (Interp_error (User_error ("malformed special form: " ^ name)))

and eval_quasiquote
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  ~(depth : int)
  (template : expr)
  : value Lwt.t =
  tick budget;
  match template with
  | App (Var "unquote", [ e ]) when depth = 1 ->
      eval ~budget ~prims env e
  | App (Var "unquote", [ e ]) ->
      let* inner = eval_quasiquote ~budget ~prims env ~depth:(depth - 1) e in
      Lwt.return (VList [ VStr "unquote"; inner ])
  | App (Var "unquote-splicing", _) when depth = 1 ->
      raise
        (Interp_error
           (User_error "unquote-splicing not in list context"))
  | App (Var "quasiquote", [ e ]) ->
      let* inner = eval_quasiquote ~budget ~prims env ~depth:(depth + 1) e in
      Lwt.return (VList [ VStr "quasiquote"; inner ])
  | App (h, args) ->
      (* list template: map qq; splice unquote-splicing at depth 1 *)
      let* head_part =
        match h with
        | Var "unquote-splicing" when depth = 1 ->
            raise
              (Interp_error
                 (User_error "unquote-splicing as list head not supported"))
        | _ ->
            let* hv = eval_quasiquote ~budget ~prims env ~depth h in
            Lwt.return [ hv ]
      in
      let* tail_parts =
        Lwt_list.map_s
          (fun a ->
            match a with
            | App (Var "unquote-splicing", [ e ]) when depth = 1 ->
                let* v = eval ~budget ~prims env e in
                (match v with
                 | VList xs -> Lwt.return xs
                 | VNil -> Lwt.return []
                 | _ ->
                     raise
                       (Interp_error
                          (User_error "unquote-splicing needs a list")))
            | _ ->
                let* v = eval_quasiquote ~budget ~prims env ~depth a in
                Lwt.return [ v ])
          args
      in
      Lwt.return (VList (head_part @ List.concat tail_parts))
  | Lit (VStr s) ->
      (* Tag so macro value_to_expr keeps string lits (not symbols). *)
      Lwt.return (VList [ VStr "__str__"; VStr s ])
  | Lit v -> Lwt.return v
  | Var s -> Lwt.return (VStr s)
  | Quote e ->
      let* v = eval_quasiquote ~budget ~prims env ~depth e in
      Lwt.return (VList [ VStr "quote"; v ])
  | other ->
      Lwt.return (reify_expr other)

and apply
  ~(budget : budget)
  ~(prims : prim_table)
  (f : value)
  (args : value list)
  : value Lwt.t =
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
           (User_error "macro must be applied by name (not as first-class apply)"))
  | VPrim name ->
      (match lookup_prim prims name with
       | Some fn -> fn args
       | None -> raise (Interp_error (Prim_error (name, "unregistered primitive"))))
  | other -> raise (Interp_error (Not_a_function other))

(** Convenience: evaluate an s-expression string with a fresh budget. *)
let eval_string
  ~(prims : prim_table)
  (env : env)
  (s : string)
  : value Lwt.t =
  let budget = make_budget () in
  let e = Reader.read_one s in
  eval ~budget ~prims env e
