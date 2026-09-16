(** Public-route handler effect gate (pure).

    Decides whether a handler may be published as a public route by
    inferring its effects from the in-memory env (synchronous, fresh)
    and checking them against the public-route allowlist. Fail-closed:
    an unknown or non-function handler is DENIED, never silently allowed.

    Pure: no Lwt, no DB. The image's [Session.check_public_handler]
    (session.ml) delegates here so the gate is unit-testable without
    Postgres.

    Addresses BORGE-1785212463 / BORGE-1785212603: the previous gate read
    function_defs from the store (stale after the define persist race)
    and DEFAULTED TO ALLOW when the handler was absent — so a typo or an
    eval-order race silently published a route with no effect check.

    BORGE-1785212882: bodies are macroexpanded before [Effect.infer] so a
    macro cannot hide WriteDb/Publish/etc. from the allowlist. Expansion
    uses [Eval.macroexpand_expr]; [Effect.infer] stays pure. *)
open Ast

(** Macroexpand [body] in [env]. Propagates [Interp_error]; other
    failures leave the body unexpanded (still fail-closed if effects
    remain hidden — prefer a correct expand, don't crash the gate). *)
let expand_body (env : Env.env) (body : expr) : expr =
  try Eval.macroexpand_expr env body
  with Interp_error _ as ex -> raise ex
     | _ -> body

(** Collect (name, body) for every VLambda visible in [env] (walks the
    parent chain). Values are forced with [Env.force_value] first: under
    the lazy-env model (lisp-interpreter.lazy-env-thunks) package defines
    bind as VThunk recipes, and a gate that only matched raw VLambda
    cells saw none of the session's helpers — their applied names then
    fell through to the strict prim lookup and inferred as EffSet.top,
    denying every published public route (prod /welcome 500, 2026-09-11).
    Forcing is the memoized single observation point dispatch itself
    uses, so the gate does no work the request would not do.

    A binding whose force raises (any exception — the prod chain carries
    defines that fail under anon, e.g. admin-gated prim bodies) is
    SKIPPED, not propagated: it just won't be in known_fn, so if the
    handler actually applies that name, inference falls through to the
    strict prim lookup and fails closed with the normal denial message
    instead of 500ing the gate. Bodies are macroexpanded so sibling
    known_fn resolution also sees through macros.

    Same contract for a sibling whose MACROEXPANSION raises (2026-09-13
    prod /welcome outage: a DB-only define used the 3-arg (let* binds b1
    b2) form, std's let* macro is fixed-arity 2, expand_macro_once raised
    Interp_error mid-fold, and every gated public route 500'd
    "macro let* arity 2 vs 3"). The broken sibling is skipped — a handler
    that actually applies it denies with the normal unresolved-name
    message; the handler's OWN body stays strict (expand_body
    propagates, and check_public_handler turns that into a denial). *)
let env_lambda_bindings (env : Env.env) : (string * expr) list =
  Env.fold env
    (fun name v acc ->
      match (try Env.force_value v with _ -> v) with
      | VLambda (_, body, _) ->
          (match expand_body env body with
           | expanded -> (name, expanded) :: acc
           | exception Interp_error _ -> acc)
      | _ -> acc)
    []

(** Check whether [handler] may be published as a public route.

    Returns [Ok ()] if the handler is a defined function (or prim) whose
    inferred effects fit the public-route allowlist; [Error msg] otherwise.
    Unknown / non-function handlers return [Error] (fail-closed). *)
let check_public_handler ~(env : Env.env) (handler : string)
    : (unit, string) result =
  let allow = Effect.public_route_allow () in
  let check fx =
    if Effect.policy_allows ~allow fx then Ok ()
    else
      Error
        (Printf.sprintf
           "public route denied: handler %S effects %s (allow %s; set PP_PUBLIC_ALLOW_WRITE=1 for write-db)"
           handler
           (Effect.EffSet.pp fx)
           (Effect.public_route_allow_pp ()))
  in
  (* Lazy-env model (lisp-interpreter.lazy-env-thunks): package defines
     bind as VThunk recipes. An UNFORCED lookup is neither VLambda nor
     VPrim, so it fell through to the deny arm — silently killing every
     fresh package's (public-route ...) at boot seed (welcome's route
     rows predate the lazy model, which is why they kept working while
     new public faces silently stayed private).

     FORCING the thunk (evaluating to its value) is ALSO wrong: the
     gate runs at boot seed and at publish time, outside any request
     context, but handler bodies may read fiber-locals (request-param,
     request-method) — seed-layout's /css/layout.css face tripped this
     on 2026-12-30. A gate must INFER EFFECTS FROM SOURCE, not run the
     program. So: read the thunk BODY out of the cell (no execution)
     and walk it like a lambda body. If the cell is already Forced, the
     memoized value is a VLambda (its body) or not-a-function (deny);
     if it was Forced to failure (Failed), deny fail-closed. *)
  let deny_not_a_function () =
    Error
      (Printf.sprintf
         "public route denied: handler %S is not a defined function"
         handler)
  in
  (* NOT Env.lookup: lookup forces VThunks, and forcing EXECUTES the
     handler body — under no current_request that raises "no active
     request" for any request-context prim before we ever see the cell.
     Raw walk of the public env records only (Env.env fields are plain
     values in ast.ml). *)
  let raw_lookup (e : Env.env) (name : string) : value option =
    let rec go e =
      match Hashtbl.find_opt e.bindings name with
      | Some v -> Some v
      | None ->
        (match e.parent with
         | Some p -> go p
         | None -> None)
    in
    go e
  in
  match raw_lookup env handler with
  | None -> deny_not_a_function ()
  | Some (VLambda (_, body, _)) | Some (VThunk { contents = Unforced { thunk_body = body; _ } }) ->
      (try
         let known_fn =
           Effect.known_fn_of_bindings (env_lambda_bindings env)
         in
         check
           (Effect.infer ~known_fn ~prims:Effect.strict_prim_effects
              (expand_body env body))
       with
       | Effect.Unresolved_name n ->
           Error
             (Printf.sprintf
                "public route denied: handler %S references unresolvable name %S (not a visible function or classified prim)"
                handler n)
       | Interp_error e ->
         Error
           (Printf.sprintf
              "public route denied: handler %S failed the effect walk: %s"
              handler
              (match e with
               | User_error (m, _) -> m
               | Arity_mismatch (n, exp, got) ->
                   Printf.sprintf "%s arity %d vs %d" n exp got
               | _ -> "interp error"))
       | e ->
         Error
           (Printf.sprintf
              "public route denied: handler %S gate walk failed: %s"
              handler (Printexc.to_string e)))
  | Some (VPrim name) ->
      (try check (Effect.strict_prim_effects name)
       with
       | Effect.Unresolved_name n ->
         Error
           (Printf.sprintf
              "public route denied: handler %S references unresolvable name %S (not a visible function or classified prim)"
              name n))
  | Some (VThunk { contents = Forced (VLambda (_, body, _)) }) ->
      (try
         let known_fn =
           Effect.known_fn_of_bindings (env_lambda_bindings env)
         in
         check
           (Effect.infer ~known_fn ~prims:Effect.strict_prim_effects
              (expand_body env body))
       with
       | Effect.Unresolved_name n ->
           Error
             (Printf.sprintf
                "public route denied: handler %S references unresolvable name %S (not a visible function or classified prim)"
                handler n)
       | Interp_error _ | _ ->
         Error
           (Printf.sprintf
              "public route denied: handler %S failed the effect walk"
              handler))
  | Some (VThunk _) -> deny_not_a_function ()
  | Some _ -> deny_not_a_function ()
