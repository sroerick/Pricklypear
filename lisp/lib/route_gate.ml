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
    parent chain). Bodies are macroexpanded so sibling known_fn resolution
    also sees through macros. *)
let env_lambda_bindings (env : Env.env) : (string * expr) list =
  Env.fold env
    (fun name v acc ->
      match v with
      | VLambda (_, body, _) -> (name, expand_body env body) :: acc
      | _ -> acc)
    []

(** Check whether [handler] may be published as a public route.

    Returns [Ok ()] if the handler is a defined function (or prim) whose
    inferred effects fit the public-route allowlist; [Error msg] otherwise.
    Unknown / non-function handlers return [Error] (fail-closed). *)
let check_public_handler ~(env : Env.env) (handler : string)
    : (unit, string) result =
  let known_fn = Effect.known_fn_of_bindings (env_lambda_bindings env) in
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
  match Env.lookup env handler with
  | Some (VLambda (_, body, _)) ->
      (try check (Effect.infer ~known_fn (expand_body env body))
       with Interp_error e ->
         Error
           (Printf.sprintf
              "public route denied: handler %S macroexpand failed: %s"
              handler
              (match e with
               | User_error (m, _) -> m
               | Arity_mismatch (n, exp, got) ->
                   Printf.sprintf "%s arity %d vs %d" n exp got
               | _ -> "interp error")))
  | Some (VPrim name) -> check (Effect.effects_for_prim name)
  | Some _ | None ->
      Error
        (Printf.sprintf
           "public route denied: handler %S is not a defined function"
           handler)
