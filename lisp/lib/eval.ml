(** Pricklypear Lisp — Evaluator (H1.7 direct-style).

    Takes an env and an expr, returns a value (or raises [Interp_error]).
    I/O prims use [Control.perform_prim] (OCaml 5 effects). The image rim
    ([Control_rim.run]) or pure tests ([Control.run_sync]) service
    [Prim_call]. No Lwt in this module.

    Per spec [lisp-interpreter.eval]: cooperative [budget], special forms,
    define binds env; image persists. *)
open Ast

(** Budget accounting, the match-pattern walker, and the quote/value ↔
    expr bridge live in Budget.ml / Pattern.ml / Quote_value.ml (i-1788671219
    walker split: eval.ml sheds three walkers, keeps the evaluator).
    Aliases below preserve the historical [Eval.*] surface consumed by
    image/lib/session.ml, pure/prims_pure.ml, agent, remote, cli and
    the test suites. *)
type budget = Budget.budget

let default_limit = Budget.default_limit
let make_budget = Budget.make_budget
let current_budget = Budget.current_budget
let last_budget = Budget.last_budget
let with_scope = Budget.with_scope
let current = Budget.current
let current_or_fresh = Budget.current_or_fresh
let budget_remaining_steps = Budget.budget_remaining_steps
let current_steps = Budget.current_steps
let note_db_hit = Budget.note_db_hit
let note_db_ms = Budget.note_db_ms
let note_prim_db = Budget.note_prim_db
let tick = Budget.tick

(** A primitive for pure tests / name registration: sync function.
    Image I/O prims are NOT stored here as Lwt — the rim handler owns those.
    Eval always [perform]s; [run_sync] invokes these sync fns. *)
type prim_fn = value list -> value

(** Name -> [prim_fn] table for pure/sync prims. Image I/O prims are
    served by the rim handler, not stored here. *)
type prim_table = (string, prim_fn) Hashtbl.t

(** Create an empty [prim_table]. *)
let make_prim_table () : prim_table = Hashtbl.create 32

(** Register [fn] in [tbl] under [name] (replaces any existing binding). *)
let register_prim (tbl : prim_table) (name : string) (fn : prim_fn) : unit =
  Hashtbl.replace tbl name fn

(** Look up [name] in [tbl], returning the registered prim if present. *)
let lookup_prim (tbl : prim_table) (name : string) : prim_fn option =
  Hashtbl.find_opt tbl name

(** The single source of truth for kernel special-form names. An [App]
    whose head is [Var name] for one of these names is dispatched as the
    special form, not as a function call; the same list is the reserved
    set refused by [assert_bindable_name], the name set of the
    [Special_forms.catalog] doc index (lisp/lib/special_forms.ml), and
    the bound-kernel set in image/lib/call_graph.ml. ONE list so dispatch
    and bindability can never drift — a name dispatchable yet
    user-definable is exactly the bug class [assert_bindable_name]
    exists to close (i-1788671219). Parity with the catalog is asserted
    in lisp/test/eval_test.ml. *)
let special_form_names : string list =
  [ "lambda"; "let"; "letrec"; "if"; "define"; "quote"; "seq"; "begin";
    "match"; "guard"; "error"; "prim";
    "with-transaction"; "with-step"; "quasiquote"; "unquote"; "unquote-splicing";
    "defmacro"; "macroexpand-1"; "macroexpand"; "gensym";
    "cond"; "and"; "or";
    (* Higher-order list ops: OCaml loops + apply, not recursive Lisp.
       Avoids ~O(n) eval-frame overhead per element (budget P0). *)
    "apply"; "list/map"; "list/filter"; "list/foldl"; "list/foldr";
    (* lazy-env-thunks: explicit delay/force (Var lookup auto-forces) *)
    "delay"; "force" ]

(** Dispatch table for [is_special_form], built once from
    [special_form_names] so dispatch consumes the list by construction
    (a name added to the list is dispatchable; nothing else is). *)
let special_form_tbl : (string, unit) Hashtbl.t =
  let t = Hashtbl.create (2 * List.length special_form_names) in
  List.iter (fun n -> Hashtbl.replace t n ()) special_form_names;
  t

(** Special-form recognition: an [App] whose head is [Var name] for one of
    [special_form_names] is dispatched as the special form, not as a
    function call. *)
let is_special_form (name : string) : bool =
  Hashtbl.mem special_form_tbl name

(** The full set of reserved kernel special-form names (see
    [special_form_names]). Used by [assert_bindable_name] to refuse
    redefinition, and by remote/lib/free_vars.ml as the kernel-name
    gate. Alias of the single source list. *)
let kernel_special_names = special_form_names

(** Refuse define/defmacro of kernel special forms and of names currently
    bound to [VPrim] (registered kernel prims). Closes BORGE-1785213238:
    a user/AI (define is-admin? …) / (define create …) must not shadow the
    prim the image and effect gates rely on. Special forms are always
    reserved; prims are reserved once bound as [VPrim] in [env] (image
    binds the full catalog at boot / rehydrate before load_defs). *)
let assert_bindable_name (env : env) (name : string) : unit =
  if List.mem name kernel_special_names then
    raise
      (Interp_error
         (User_error ("cannot redefine kernel special form: " ^ name, "bad-args")));
  match Env.lookup env name with
  | Some (VPrim _) ->
      raise
        (Interp_error
           (User_error ("cannot redefine kernel primitive: " ^ name, "bad-args")))
  | _ -> ()

(** Desugar multiple body exprs to a single expr (begin/seq chain). *)
let body_of_exprs = function
  | [] -> Lit VNil
  | [ e ] -> e
  | e1 :: e2 :: rest ->
      let rec fold a = function
        | [] -> a
        | x :: xs -> Seq (a, fold x xs)
      in
      fold e1 (e2 :: rest)

(** Leading string literal is a docstring only when followed by ≥1 body form.
    A lone string body is a value-returning body, not metadata.
    Per spec [lisp-interpreter.define-docstring]. *)
let peel_leading_doc (bodies : expr list) : string option * expr list =
  match bodies with
  | Lit (VStr doc) :: (_ :: _ as rest) -> (Some doc, rest)
  | _ -> (None, bodies)

(** Parse the bindings shape shared by [let] and [letrec] (previously two
    copy-pasted match blocks, i-1788671219): [( [(name val) …] )] as an
    App-chain of (name val) pairs, or empty [Lit VNil]. Returns
    (name, val_expr) pairs in source order. [label] names the form in
    error messages ("let" / "letrec") so messages stay identical to the
    pre-dedupe text. *)
let parse_binding_pairs ~(label : string) (bindings_expr : expr)
  : (string * expr) list =
  match bindings_expr with
  | Lit VNil -> []
  | App (App (Var _, _) as first_binding, rest_bindings) ->
      List.map
        (function
          | App (Var n, [ val_expr ]) -> (n, val_expr)
          | _ ->
              raise
                (Interp_error
                   (User_error ("bad " ^ label ^ " binding", "bad-args"))))
        (first_binding :: rest_bindings)
  | _ ->
      raise
        (Interp_error
           (User_error ("bad " ^ label ^ " bindings", "bad-args")))

(** Extract parameter names from [define] sugar param exprs; an
    [App (Var c, [_])] is accepted as an optional-with-default [c]. *)
let params_of_define_sugar (param_exprs : expr list) : string list =
  List.map
    (function
     | Var p -> p
     | App (Var p, [ _default ]) ->
         (* optional with default: (c default) — allowed in define sugar *)
         p
     | _ ->
         raise
           (Interp_error
              (User_error ("define function params must be names", "bad-args"))))
    param_exprs

(** Rewrite a lambda-shaped expr so a leading multi-body docstring is removed
    from the stored/bound body. Returns (body_expr, doc_opt). *)
let strip_lambda_doc (e : expr) : expr * string option =
  match e with
  | App (Var "lambda", params_expr :: bodies) ->
      let doc, bodies' = peel_leading_doc bodies in
      (App (Var "lambda", params_expr :: bodies'), doc)
  | Lambda (params, body) ->
      (* Constructor form is already a single body; only Seq-peel if the
         first side of a top Seq is a string lit and we want cleanliness.
         Keep simple: no peel on pre-built Lambda (tests/AST path). *)
      let _ = params in
      (Lambda (params, body), None)
  | _ -> (e, None)

(** Parsed top-level (define …) for eval + image persist.
    [body] is the expr to bind/persist (lambda leading-doc already stripped). *)
type define_shape = {
  name : string;
  body : expr;
  doc : string option;
}

(** Parse a top-level [(define name body) / (define (name params…) …)]
    (or a pre-constructed [Define] node) into a [define_shape], stripping
    any leading lambda docstring. Raises on malformed forms. *)
let parse_top_define (e : expr) : define_shape option =
  match e with
  | App (Var "define", [ Var name; body_expr ]) ->
      let body, doc = strip_lambda_doc body_expr in
      Some { name; body; doc }
  | App (Var "define", [ Var name; body_expr; Lit (VStr doc) ]) ->
      let body, _lam_doc = strip_lambda_doc body_expr in
      (* Trailing define docstring wins over lambda-internal. *)
      Some { name; body; doc = Some doc }
  | App (Var "define", [ Var _; _; other ]) ->
      let _ = other in
      raise
        (Interp_error
           (User_error
              ("define docstring must be a string literal \"…\"", "bad-args")))
  | App (Var "define", App (Var name, param_exprs) :: bodies) ->
      let doc, bodies' = peel_leading_doc bodies in
      let params = params_of_define_sugar param_exprs in
      if List.mem "." params then
        raise
          (Interp_error
             (User_error ("dotted rest params not supported in define sugar", "bad-args")));
      let body = body_of_exprs bodies' in
      Some { name; body = Lambda (params, body); doc }
  | Define (name, body_expr) ->
      let body, doc = strip_lambda_doc body_expr in
      Some { name; body; doc }
  | _ -> None

(** Match-pattern walker moved to Pattern.ml (walker split); alias keeps
    the Eval surface. *)
let match_pat = Pattern.match_pat

(** Lisp truthiness: [false] and [nil] are falsy, everything else truthy. *)
let truthy (v : value) : bool =
  match v with
  | VBool false | VNil -> false
  | _ -> true

(** Quote/value ↔ expr bridge moved to Quote_value.ml (walker split);
    aliases keep the Eval surface for image/pure/agent/remote/tests. *)
let quote_to_value = Quote_value.quote_to_value
let reify_expr = Quote_value.reify_expr
let name_of_atom = Quote_value.name_of_atom
let head_name = Quote_value.head_name
let value_to_expr = Quote_value.value_to_expr

(** Global counter backing [gensym]. *)
let gensym_counter = ref 0

(** Generate a unique symbol name [prefix$N] using a global counter. *)
let gensym ?(prefix = "g") () : string =
  incr gensym_counter;
  prefix ^ "$" ^ string_of_int !gensym_counter

(** Flatten a Lisp list value (VList or VPair chain or VNil) to OCaml list. *)
let rec value_to_list (v : value) : value list =
  match v with
  | VNil | VList [] -> []
  | VList xs -> xs
  | VPair (a, b) -> a :: value_to_list b
  | other ->
      raise
        (Interp_error
           (User_error
              ("list op needs a list, got " ^ pp_value_short other, "bad-args")))

(** Trampoline step: [Done] finishes; [Cont] continues without OCaml stack. *)
type step =
  | Done of value
  | Cont of env * expr

(** One evaluation step. Tail positions return [Cont]; non-tails force via [eval]. *)
let rec eval_step
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  (e : expr)
  : step =
  (* Publish budget for prims (budget-remaining) and nested helpers. *)
  current_budget := Some budget;
  tick budget;
  match e with
  | Var name ->
      (* Keyword atoms (:target, :swap, …) are self-evaluating symbols so
         prim kwargs like (part html :target "#x") need no quote. *)
      if String.length name > 0 && name.[0] = ':' then Done (VSymbol name)
      else
        (match Env.lookup env name with
         | Some v -> Done v
         | None -> raise (Interp_error (Unbound_var name)))
  | Lit v -> Done v
  | Quote e' -> Done (quote_to_value e')
  | If (test, then_e, else_e) ->
      let v = eval ~budget ~prims env test in
      (match v with
       | VBool false | VNil -> Cont (env, else_e)
       | _ -> Cont (env, then_e))
  | Seq (a, b) ->
      ignore (eval ~budget ~prims env a);
      Cont (env, b)
  | Let (name, val_expr, body) ->
      let v = eval ~budget ~prims env val_expr in
      let env' = Env.child env in
      Env.define env' name v;
      Cont (env', body)
  | Letrec (bindings, body) ->
      let env' = Env.child env in
      List.iter (fun (n, _) -> Env.define env' n VNil) bindings;
      List.iter
        (fun (n, e') ->
          let v = eval ~budget ~prims env' e' in
          Env.define env' n v)
        bindings;
      Cont (env', body)
  | Lambda (params, body) -> Done (VLambda (params, body, env))
  | Define (name, val_expr) ->
      (* parse_top_define always succeeds for a Define node; eval_define
         owns the binding (shared with the surface "define" form). *)
      eval_define ~budget ~prims env (Define (name, val_expr))
  | Prim (name, args) ->
      let arg_vals = List.map (eval ~budget ~prims env) args in
      let _ = prims in
      budget.prim_calls <- budget.prim_calls + 1;
      Done (Control.perform_prim name arg_vals)
  | App (head, args) ->
      (match head with
       | Var name when is_special_form name ->
           eval_special_form ~budget ~prims env name (head :: args)
       | Var name ->
           (match Env.lookup env name with
            | Some (VMacro (params, body, clo_env)) ->
                expand_and_eval_macro ~budget ~prims env
                  ~name ~params ~body ~clo_env ~arg_exprs:args
            | _ ->
                let f = eval ~budget ~prims env head in
                let arg_vals = List.map (eval ~budget ~prims env) args in
                let step = apply_step ~budget ~prims f arg_vals in
                (match f, step with
                 | VLambda _, Cont _ -> budget.current_fn <- Some name
                 | VPrim _, Done _ ->
                     budget.prim_calls <- budget.prim_calls + 1
                 | _ -> ());
                step)
       | _ ->
           let f = eval ~budget ~prims env head in
           let arg_vals = List.map (eval ~budget ~prims env) args in
           let step = apply_step ~budget ~prims f arg_vals in
           (match f with
            | VPrim _ -> budget.prim_calls <- budget.prim_calls + 1
            | _ -> ());
           step)

(** Force evaluation to a value (trampoline driver). *)
and eval
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  (e : expr)
  : value =
  let prev_fn = budget.current_fn in
  let rec loop env e =
    match eval_step ~budget ~prims env e with
    | Done v ->
        budget.current_fn <- prev_fn;
        v
    | Cont (env', e') -> loop env' e'
  in
  loop env e

(** THE macro-expansion engine — one implementation for all four call
    sites (i-1788671219: the App dispatch, the [macroexpand_expr] walker,
    and the [macroexpand-1] / [macroexpand] special forms previously
    re-implemented this binding-and-eval loop four times, with subtle
    arg-reification drift between them). Binds the macro's [params] to
    the ARG EXPRS reified as quoted values (not evaluated: "foo" → VStr,
    bare foo → VSymbol) in a child of the macro's closure env, evaluates
    the macro body there, and returns the raw expansion VALUE. Enforces
    macro arity. Callers reify with [value_to_expr] where an expr tree is
    needed. Raises [Interp_error] if the expansion is not reifiable
    ([value_to_expr]'s error — the old "not a valid expr tree" wrapper
    arm was dead: value_to_expr only raises Interp_error). *)
and expand_macro_once
    ~(budget : budget)
    ~(prims : prim_table)
    ~(name : string)
    ~(params : string list)
    ~(body : expr)
    ~(clo_env : env)
    (arg_exprs : expr list)
  : value =
  tick budget;
  let expected = List.length params
  and got = List.length arg_exprs in
  if expected <> got then
    (* Name the macro: a bare "macro arity 2 vs 3" told you nothing about
       which stored binding drifted (the 2026-09-11 /guestbook hunt). *)
    raise (Interp_error (Arity_mismatch ("macro " ^ name, expected, got)));
  let arg_vals = List.map reify_expr arg_exprs in
  let menv = Env.child clo_env in
  List.iter2 (fun p a -> Env.define menv p a) params arg_vals;
  eval ~budget ~prims menv body

(** Expand and evaluate a macro application by name (evaluator App
    dispatch path). Thin wrapper over [expand_macro_once]; the expansion
    is [value_to_expr]-reified and re-evaluated in [call_env] via [Cont]. *)
and expand_and_eval_macro
  ~(budget : budget)
  ~(prims : prim_table)
  (call_env : env)
  ~(name : string)
  ~(params : string list)
  ~(body : expr)
  ~(clo_env : env)
  ~(arg_exprs : expr list)
  : step =
  let expansion_e =
    value_to_expr
      (expand_macro_once ~budget ~prims ~name ~params ~body ~clo_env arg_exprs)
  in
  Cont (call_env, expansion_e)

(** Fully macroexpand [e] in [env], returning an expr tree.
    Used by the public-route gate and AI install effect checks so
    [Effect.infer] sees EXPANDED bodies (lisp-interpreter.effects §4;
    BORGE-1785212882). Keeps [Effect.infer] pure — expansion runs here
    with the eval rim (macro bodies are expected to be pure templates).

    Does not expand under [Quote]. Recurses into lambda/let/if/seq/app
    subforms. When the head of an app is a [VMacro], expands once and
    loops (same as the [macroexpand] special form). *)
and macroexpand_expr
    ?(budget = make_budget ())
    ?(prims : prim_table = make_prim_table ())
    (env : env)
    (e : expr) : expr =
  tick budget;
  let go e' = macroexpand_expr ~budget ~prims env e' in
  match e with
  | Quote _ as q -> q
  | Lit _ | Var _ as atom -> atom
  | Lambda (ps, body) -> Lambda (ps, go body)
  | If (a, b, c) -> If (go a, go b, go c)
  | Seq (a, b) -> Seq (go a, go b)
  | Let (n, v, body) -> Let (n, go v, go body)
  | Letrec (binds, body) ->
      Letrec (List.map (fun (n, be) -> (n, go be)) binds, go body)
  | Define (n, body) -> Define (n, go body)
  | Prim (name, args) -> Prim (name, List.map go args)
  | App (Var name, args) ->
      (match Env.lookup env name with
       | Some (VMacro (params, body, clo_env)) ->
           go
             (value_to_expr
                (expand_macro_once ~budget ~prims ~name ~params ~body ~clo_env args))
       | _ -> App (Var name, List.map go args))
  | App (h, args) -> App (go h, List.map go args)

(** Bind one [define] — shared by the [Define] AST node (eval_step) and
    the surface [define] special form (eval_special_form); previously two
    copy-pasted bind-and-store blocks (i-1788671219). [form] is the whole
    (define …) app / Define node. [parse_top_define] handles the define
    sugar (function-style params, docstrings) and strips the lambda
    leading-doc; Lambda bodies short-circuit to a closure without
    re-eval. Returns [Done] with the bound value. *)
and eval_define
    ~(budget : budget)
    ~(prims : prim_table)
    (env : env)
    (form : expr) : step =
  match parse_top_define form with
  | Some { name; body; doc = _ } ->
      assert_bindable_name env name;
      let v =
        match body with
        | Lambda (params, b) -> VLambda (params, b, env)
        | other -> eval ~budget ~prims env other
      in
      Env.define env name v;
      Done v
  | None ->
      raise
        (Interp_error
           (User_error
              ("define needs (define name expr [\"doc\"]) or \
                (define (name args…) [\"doc\"] body…)", "bad-args")))

(** Dispatch a special form by [name] (an [App] whose head is a [Var]
    recognized by [is_special_form]). [full_args] is head :: rest where
    head is the [Var name]. Handles lambda/let/define/cond/match/guard/
    quasiquote/defmacro, the list higher-order operators, and more;
    returns a [step]. *)
and eval_special_form
  ~(budget : budget)
  ~(prims : prim_table)
  (env : env)
  (name : string)
  (full_args : expr list)
  : step =
  match name, full_args with
  | "lambda", _head :: params_expr :: bodies ->
      let _doc, bodies' = peel_leading_doc bodies in
      let body = body_of_exprs bodies' in
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
                         (Interp_error (User_error ("lambda param not a symbol", "bad-args"))))
                 rest
        | _ -> raise (Interp_error (User_error ("malformed lambda params", "bad-args")))
      in
      if List.mem "." params then
        raise
          (Interp_error
             (User_error
                ("dotted rest params not supported (reader treats \".\" as a \
                 symbol); use fixed-arity params", "bad-args")));
      Done (VLambda (params, body, env))
  | "let", _ ->
      (match full_args with
       | [ _; bindings_expr; body ] ->
           (match bindings_expr with
            | Lit VNil -> Cont (env, body)
            | _ ->
                let bindings = parse_binding_pairs ~label:"let" bindings_expr in
                let env' = Env.child env in
                List.iter
                  (fun (n, val_expr) ->
                    let v = eval ~budget ~prims env val_expr in
                    Env.define env' n v)
                  bindings;
                Cont (env', body))
       | _ -> raise (Interp_error (User_error ("let needs bindings + body", "bad-args"))))
  | "if", [ _; test; then_e; else_e ] ->
      let v = eval ~budget ~prims env test in
      (match v with
       | VBool false | VNil -> Cont (env, else_e)
       | _ -> Cont (env, then_e))
  | "delay", [ _; body ] ->
      (* lisp-interpreter.lazy-env-thunks: capture the recipe, cook on
         first force. The payload evaluator charges the ACTIVE budget
         at force time (the request that forces pays) when one exists,
         else a fresh one, and reuses this eval's prim table. Resolves
         through the fiber scope (i-1788670103-5), so a concurrent
         request's publish never steals the charge. *)
      let force env' expr' =
        let b = Budget.current_or_fresh () in
        eval ~budget:b ~prims env' expr'
      in
      Done
        (VThunk
           (ref
              (Unforced
                 { thunk_body = body; thunk_env = env; thunk_force = force })))
  | "delay", _ ->
      raise (Interp_error (User_error ("delay needs exactly one body", "bad-args")))
  | "force", [ _; x ] ->
      let v = eval ~budget ~prims env x in
      Done (Env.force_value v)
  | "force", _ ->
      raise (Interp_error (User_error ("force needs exactly one arg", "bad-args")))
  | "define", _ ->
      (* eval_define owns binding; rebuild the whole (define …) app —
         full_args is head :: rest where head is Var "define". *)
      eval_define ~budget ~prims env (App (Var "define", List.tl full_args))
  | "quote", [ _; e' ] -> Done (quote_to_value e')
  | "begin", _head :: bodies | "seq", _head :: bodies ->
      (match bodies with
       | [] -> Done VNil
       | [ only ] -> Cont (env, only)
       | first :: rest ->
           ignore (eval ~budget ~prims env first);
           eval_special_form ~budget ~prims env "seq" (Var "seq" :: rest))
  | "error", _head :: args ->
      let kind, msg =
        match args with
        | [ Lit (VStr m) ] | [ Lit (VSymbol m) ] | [ Var m ] -> ("user_error", m)
        | [ e ] ->
            (match eval ~budget ~prims env e with
             | VStr m | VSymbol m -> ("user_error", m)
             | other -> ("user_error", pp_value_short other))
        | [ ke; me ] ->
            let k =
              match eval ~budget ~prims env ke with
              | VStr s | VSymbol s -> s
              | other -> pp_value_short other
            in
            let m =
              match eval ~budget ~prims env me with
              | VStr s | VSymbol s -> s
              | other -> pp_value_short other
            in
            (k, m)
        | _ -> ("user_error", "error")
      in
      raise (Interp_error (User_error (msg, kind)))
  | "guard", _head :: handler_expr :: bodies when bodies <> [] ->
      let body = body_of_exprs bodies in
      (try Done (eval ~budget ~prims env body)
       with
       | Interp_error Budget_exceeded as ex -> raise ex
       | Interp_error e ->
           let kind = interp_error_kind e in
           let err_v =
             VList
               [ VSymbol "error"
               ; VStr (pp_interp_error e)
               ; VSymbol kind
               ]
           in
           let hv = eval ~budget ~prims env handler_expr in
           (match hv with
            | VLambda (params, hbody, clo) ->
                let call = Env.child clo in
                (match params with
                 | [] -> Done (eval ~budget ~prims call hbody)
                 | [ p ] ->
                     Env.define call p err_v;
                     Done (eval ~budget ~prims call hbody)
                 | _ ->
                     raise
                       (Interp_error
                          (User_error ("guard handler must be 0- or 1-arg lambda", "bad-args"))))
            | _ ->
                raise
                  (Interp_error
                     (User_error ("guard handler must be a lambda", "bad-args")))))
  | "guard", _ ->
      raise
        (Interp_error
           (User_error ("guard needs (guard handler-lambda body…)", "bad-args")))
  | "match", _head :: scrut_expr :: clauses ->
      let scrut = eval ~budget ~prims env scrut_expr in
      let rec try_clauses = function
        | [] ->
            raise
              (Interp_error (User_error ("match: no matching clause", "bad-args")))
        | clause :: rest ->
            let pat, bodies =
              match clause with
              | App (pat, bs) -> pat, bs
              | _ ->
                  raise
                    (Interp_error
                       (User_error ("match clause must be (pat body…)", "bad-args")))
            in
            (match match_pat pat scrut with
             | None -> try_clauses rest
             | Some binds ->
                 let env' = Env.child env in
                 List.iter (fun (n, v) -> Env.define env' n v) binds;
                 Cont (env', body_of_exprs bodies))
      in
      try_clauses clauses
  | "prim", _ ->
      (match full_args with
       | [ _; Var prim_name; body ] ->
           let arg_vals = [ eval ~budget ~prims env body ] in
           Done (Control.perform_prim prim_name arg_vals)
       | _ -> raise (Interp_error (User_error ("prim needs name + arg", "bad-args"))))
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
                (User_error ("with-transaction needs a lambda body", "bad-args")))
       | Some body_expr ->
           let lambda_v = eval ~budget ~prims env body_expr in
           (match lambda_v with
            | VLambda _ ->
                Done
                  (Control.perform_prim "__with_transaction__" [ lambda_v ])
            | _ ->
                raise
                  (Interp_error
                     (User_error ("with-transaction body must be a lambda", "bad-args")))))
  | "with-step", _ ->
      (* (with-step "name" (lambda () body)) — a checkpoint boundary. In a
         job, the __step__ prim in session.ml memoizes the thunk's result
         under (job-id, name); outside a job it is a plain thunk call.
         Adjacent to with-transaction for the exactly-once
         (with-transaction (lambda () (with-step …))) composition. Named
         with-step (NOT step) to avoid colliding with the pre-existing
         libs/rb `step` recipe-DSL function. See borge/jobs.borg
         step-checkpoints. *)
      (match full_args with
       | [ _; name_expr; body_expr ] ->
           let name_v = eval ~budget ~prims env name_expr in
           (match name_v with
            | VStr name ->
                let lambda_v = eval ~budget ~prims env body_expr in
                (match lambda_v with
                 | VLambda _ ->
                     Done (Control.perform_prim "__step__" [ VStr name; lambda_v ])
                 | _ ->
                     raise
                       (Interp_error
                          (User_error ("with-step body must be a lambda", "bad-args"))))
            | _ ->
                raise
                  (Interp_error
                     (User_error ("with-step needs a string name", "bad-args"))))
       | _ ->
           raise
             (Interp_error
                (User_error ("with-step needs (with-step \"name\" (lambda () body))", "bad-args"))))
  | "gensym", [ _ ] -> Done (VSymbol (gensym ()))
  | "gensym", [ _; Lit (VStr prefix) ] | "gensym", [ _; Var prefix ]
  | "gensym", [ _; Lit (VSymbol prefix) ] ->
      Done (VSymbol (gensym ~prefix ()))
  | "and", _head :: args ->
      let rec loop = function
        | [] -> Done (VBool true)
        | [ last ] -> Cont (env, last)
        | e' :: rest ->
            let v = eval ~budget ~prims env e' in
            if truthy v then loop rest else Done v
      in
      loop args
  | "or", _head :: args ->
      let rec loop = function
        | [] -> Done (VBool false)
        | [ last ] -> Cont (env, last)
        | e' :: rest ->
            let v = eval ~budget ~prims env e' in
            if truthy v then Done v else loop rest
      in
      loop args
  | "cond", _head :: clauses ->
      let rec loop = function
        | [] -> Done VNil
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
                       (User_error ("cond clause must be (test body)", "bad-args")))
            in
            let always =
              match test_e with
              | Var "else" | Lit (VBool true) -> true
              | _ -> false
            in
            if always then Cont (env, body_e)
            else
              let tv = eval ~budget ~prims env test_e in
              if truthy tv then Cont (env, body_e)
              else loop rest
      in
      loop clauses
  | "quasiquote", [ _; template ] ->
      Done (eval_quasiquote ~budget ~prims env ~depth:1 template)
  | "unquote", _ | "unquote-splicing", _ ->
      raise (Interp_error (User_error ("unquote outside quasiquote", "bad-args")))
  | "defmacro", [ _; Var name; params_expr; body ] ->
      assert_bindable_name env name;
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
                            (User_error ("defmacro param not a symbol", "bad-args"))))
                 rest
        | _ -> raise (Interp_error (User_error ("malformed defmacro params", "bad-args")))
      in
      if List.mem "." params then
        raise
          (Interp_error
             (User_error
                ("dotted rest params not supported (reader treats \".\" as a \
                 symbol); use fixed-arity params, e.g. (name val body)", "bad-args")));
      let m = VMacro (params, body, env) in
      Env.define env name m;
      Done m
  | "macroexpand-1", [ _; form ] ->
      (match form with
       | App (Var name, args) ->
           (match Env.lookup env name with
            | Some (VMacro (params, body, clo_env)) ->
                Done (expand_macro_once ~budget ~prims ~name ~params ~body ~clo_env args)
            | _ -> Done (reify_expr form))
       | _ -> Done (reify_expr form))
  | "macroexpand", [ _; form ] ->
      let rec loop e =
        tick budget;
        match e with
        | App (Var name, args) ->
            (match Env.lookup env name with
             | Some (VMacro (params, body, clo_env)) ->
                 loop
                   (value_to_expr
                      (expand_macro_once ~budget ~prims ~name ~params ~body ~clo_env args))
             | _ -> reify_expr e)
        | _ -> reify_expr e
      in
      Done (loop form)
  | "letrec", _ ->
      (match full_args with
       | [ _; bindings_expr; body ] ->
           (match bindings_expr with
            | Lit VNil -> Cont (env, body)
            | _ ->
                let bindings =
                  parse_binding_pairs ~label:"letrec" bindings_expr
                in
                let env' = Env.child env in
                (* Backpatch: pre-bind all names to VNil so binding exprs
                   (lambdas) can close over env' and refer to each other
                   for mutual recursion, then evaluate and overwrite. *)
                List.iter (fun (n, _) -> Env.define env' n VNil) bindings;
                List.iter
                  (fun (n, val_expr) ->
                    let v = eval ~budget ~prims env' val_expr in
                    Env.define env' n v)
                  bindings;
                Cont (env', body))
       | _ -> raise (Interp_error (User_error ("letrec needs bindings + body", "bad-args"))))
  (* apply: (apply f args-list) applies f to the list of args.
     First-class function application — reuses the same apply helper
     that list/map etc. use, so closures and VPrims both work. *)
  | "apply", [ _; f_expr; args_expr ] ->
      let f = eval ~budget ~prims env f_expr in
      let args = value_to_list (eval ~budget ~prims env args_expr) in
      Done (apply ~budget ~prims f args)
  | "apply", _ -> raise (Interp_error (User_error ("apply needs (f args)", "bad-args")))
  (* list/map, list/filter, list/foldl — OCaml loops + apply.
     One special-form tick total instead of O(n) recursive Lisp frames.
     Closure bodies still tick normally via apply. *)
  | "list/map", [ _; f_expr; xs_expr ] ->
      let f = eval ~budget ~prims env f_expr in
      let xs = value_to_list (eval ~budget ~prims env xs_expr) in
      Done (VList (List.map (fun x -> apply ~budget ~prims f [ x ]) xs))
  | "list/filter", [ _; p_expr; xs_expr ] ->
      let p = eval ~budget ~prims env p_expr in
      let xs = value_to_list (eval ~budget ~prims env xs_expr) in
      Done
        (VList
           (List.filter
              (fun x -> truthy (apply ~budget ~prims p [ x ]))
              xs))
  | "list/foldl", [ _; f_expr; init_expr; xs_expr ] ->
      let f = eval ~budget ~prims env f_expr in
      let init = eval ~budget ~prims env init_expr in
      let xs = value_to_list (eval ~budget ~prims env xs_expr) in
      Done
        (List.fold_left
           (fun acc x -> apply ~budget ~prims f [ acc; x ])
           init xs)
  | "list/foldr", [ _; f_expr; init_expr; xs_expr ] ->
      (* Right fold via reverse + left fold with swapped arg order:
         foldr f init [a;b;c] = f a (f b (f c init))
         = foldl (fun acc x -> f x acc) init (rev xs).
         Tail-recursive (OCaml loops), one special-form tick. *)
      let f = eval ~budget ~prims env f_expr in
      let init = eval ~budget ~prims env init_expr in
      let xs = value_to_list (eval ~budget ~prims env xs_expr) in
      Done
        (List.fold_left
           (fun acc x -> apply ~budget ~prims f [ x; acc ])
           init (List.rev xs))
  | "list/map", _ ->
      raise (Interp_error (User_error ("list/map needs (f xs)", "bad-args")))
  | "list/filter", _ ->
      raise (Interp_error (User_error ("list/filter needs (p xs)", "bad-args")))
  | "list/foldl", _ ->
      raise (Interp_error (User_error ("list/foldl needs (f init xs)", "bad-args")))
  | "list/foldr", _ ->
      raise (Interp_error (User_error ("list/foldr needs (f init xs)", "bad-args")))
  | other, _ ->
      raise
        (Interp_error (User_error ("malformed special form: " ^ other, "bad-args")))

(** Evaluate a quasiquote [template] at nesting [depth]. Unquote at
    depth 1 evaluates the inner expr; deeper unquotes are preserved as
    nested lists; unquote-splicing splices list elements. *)
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
      VList [ VSymbol "unquote"; inner ]
  | App (Var "unquote-splicing", _) when depth = 1 ->
      raise
        (Interp_error (User_error ("unquote-splicing not in list context", "bad-args")))
  | App (Var "quasiquote", [ e ]) ->
      let inner =
        eval_quasiquote ~budget ~prims env ~depth:(depth + 1) e
      in
      VList [ VSymbol "quasiquote"; inner ]
  | App (h, args) ->
      let head_part =
        match h with
        | Var "unquote-splicing" when depth = 1 ->
            raise
              (Interp_error
                 (User_error ("unquote-splicing as list head not supported", "bad-args")))
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
                          (User_error ("unquote-splicing needs a list", "bad-args"))))
            | _ ->
                let v = eval_quasiquote ~budget ~prims env ~depth a in
                [ v ])
          args
      in
      VList (head_part @ List.concat tail_parts)
  | Lit v -> v
  | Var s -> VSymbol s
  | Quote e ->
      let v = eval_quasiquote ~budget ~prims env ~depth e in
      VList [ VSymbol "quote"; v ]
  | other -> reify_expr other

(** Apply [f] to [args], returning a [step] without forcing. Closures
    (with &optional/&rest support) bind params and return [Cont]; VPrims
    perform the effect; a [VMacro] applied first-class is an error. *)
and apply_step
  ~(budget : budget)
  ~(prims : prim_table)
  (f : value)
  (args : value list)
  : step =
  match f with
  | VLambda (params, body, clo_env) ->
      (* Parse param spec: required, &optional, &rest markers.
         params is a flat string list; &optional and &rest are markers.
         Layout: required... [&optional opt...] [&rest name]. *)
      let has_marker =
        List.exists (fun p -> p = "&optional" || p = "&rest") params
      in
      if not has_marker then begin
        (* Fixed arity — original path *)
        let expected = List.length params and got = List.length args in
        if expected <> got then
          raise (Interp_error (Arity_mismatch ("lambda", expected, got)));
        let call_env = Env.child clo_env in
        List.iter2 (fun p a -> Env.define call_env p a) params args;
        Cont (call_env, body)
      end else begin
        (* Parse markers *)
        let rec split_params ps required opt_phase opt_acc rest_acc =
          match ps with
          | [] -> (List.rev required, List.rev opt_acc, rest_acc)
          | p :: rest when p = "&optional" ->
              split_params rest required true opt_acc rest_acc
          | p :: rest when p = "&rest" ->
              (match rest with
               | [ r ] ->
                   split_params [] required false opt_acc (Some r)
               | _ ->
                   raise
                     (Interp_error
                        (User_error ("&rest must be last and have one name", "bad-args"))))
          | p :: rest when opt_phase ->
              split_params rest required true (p :: opt_acc) rest_acc
          | p :: rest ->
              if opt_phase then
                split_params rest required true (p :: opt_acc) rest_acc
              else
                split_params rest (p :: required) false opt_acc rest_acc
        in
        let (required, optionals, rest_name) =
          split_params params [] false [] None
        in
        let n_required = List.length required
        and n_optional = List.length optionals
        and n_args = List.length args in
        (* Arity check: at least required, at most required+optional (if no rest) *)
        if n_args < n_required then
          raise (Interp_error (Arity_mismatch ("lambda", n_required, n_args)));
        (match rest_name with
         | None ->
             if n_args > n_required + n_optional then
               raise
                 (Interp_error
                    (Arity_mismatch ("lambda", n_required + n_optional, n_args)))
         | Some _ -> ());
        let call_env = Env.child clo_env in
        (* Bind required *)
        let req_args = List.take n_required args in
        List.iter2 (fun p a -> Env.define call_env p a) required req_args;
        (* Bind optional (fill missing with empty string VStr "") *)
        let remaining = List.drop n_required args in
        let opt_args = List.take n_optional remaining in
        let n_supplied_opt = List.length opt_args in
        let supplied_opt_names = List.take n_supplied_opt optionals in
        List.iter2
          (fun p a -> Env.define call_env p a)
          supplied_opt_names opt_args;
        (* Fill missing optionals with "" *)
        let missing_opt = List.drop n_supplied_opt optionals in
        List.iter (fun p -> Env.define call_env p (VStr "")) missing_opt;
        (* Bind rest *)
        (match rest_name with
         | Some r ->
             let rest_args = List.drop n_optional remaining in
             Env.define call_env r (VList rest_args)
         | None -> ());
        Cont (call_env, body)
      end
  | VMacro _ ->
      raise
        (Interp_error
           (User_error
              ("macro must be applied by name (not as first-class apply)", "bad-args")))
  | VPrim name ->
      let _ = prims in
      (* prim_calls counted at App/Prim call sites *)
      Done (Control.perform_prim name args)
  | other -> raise (Interp_error (Not_a_function other))

(** Apply [f] to [args], forcing the result to a [value] via [eval] when
    the closure returns [Cont]. *)
and apply
  ~(budget : budget)
  ~(prims : prim_table)
  (f : value)
  (args : value list)
  : value =
  match apply_step ~budget ~prims f args with
  | Done v -> v
  | Cont (env, body) -> eval ~budget ~prims env body

(** Eval one toplevel form read from [s] directly, without any rim —
    the caller owns the [Prim_call] effect. Records the fresh budget in
    [last_budget] for image persistence and establishes it as the
    FIBER-SCOPED current budget for the whole eval (i-1788670103-5): prim
    bodies (including post-yield DB accounting) resolve the calling
    eval's budget through the scope, never a concurrently-published
    ref. *)
let eval_string_direct ~(prims : prim_table) (env : env) ~(budget : budget)
    (s : string) : value =
  Budget.with_scope (Some budget) (fun () ->
      last_budget := Some budget;
      let e = Reader.read_one s in
      eval ~budget ~prims env e)

(** Evaluate under [Control.run_sync] (pure tests; sync prim table). *)
let eval_string ~(prims : prim_table) (env : env) (s : string) : value =
  Control.run_sync ~prims (fun () ->
      eval_string_direct ~prims env ~budget:(make_budget ()) s)

(** Evaluate all toplevel forms in [s], sequentially, under the given
    prim table but WITHOUT installing any rim — the caller hosts the
    [Prim_call] effect (hosts: image Control_rim in session, agent
    Control.run_with_rim). Returns the last form's value. Used by host
    plugin/file loaders that want file-per-file or form-per-form eval
    without string re-rendering. (Added for borge/agent.borg
    nopales-embed.) *)
let read_eval_all_direct ~(prims : prim_table) (env : env) (s : string)
    : value =
  let forms = Reader.read_all s in
  let budget = make_budget () in
  last_budget := Some budget;
  Budget.with_scope (Some budget) (fun () ->
      List.fold_left
        (fun _acc form -> eval ~budget ~prims env form)
        VNil forms)

(** An empty [prim_table] convenience value. *)
let empty_prims : prim_table = make_prim_table ()

(** Aliases kept for call-site stability during H1 cleanup. *)
let apply_eff = apply
let eval_eff = eval
let eval_string_eff = eval_string_direct
let eval_string_auto = eval_string
