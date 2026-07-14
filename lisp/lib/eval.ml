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
  | "prim" | "with-transaction" -> true
  | _ -> false

(* Quote any expr into a first-class value (full s-expr quote).
   Compounds become nested VLists so quoted data is usable in Lisp. *)
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
      (* First: is this a special form? Special forms are recognized
         when the head is a [Var] naming one. Note: [args] here is the
         list AFTER the head; we prepend head when dispatching so the
         special-form handler sees the full argument list. *)
      (match head with
       | Var name when is_special_form name ->
           eval_special_form ~budget ~prims env name (head :: args)
       | _ ->
           (* ordinary application *)
           let* f = eval ~budget ~prims env head in
           let* arg_vals = Lwt_list.map_s (eval ~budget ~prims env) args in
           apply ~budget ~prims f arg_vals)

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
         - (p1 p2 ...) -> App(Var p1, [Var p2; ...]) — collect head + args *)
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
  | _ ->
      raise (Interp_error (User_error ("malformed special form: " ^ name)))

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
