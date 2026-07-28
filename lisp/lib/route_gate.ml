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
    eval-order race silently published a route with no effect check. *)
open Ast

(** Collect (name, body) for every VLambda visible in [env] (walks the
    parent chain). Used to build the known_fn cross-reference so a
    handler's calls to sibling user functions resolve during inference. *)
let env_lambda_bindings (env : Env.env) : (string * expr) list =
  Env.fold env
    (fun name v acc ->
      match v with
      | VLambda (_, body, _) -> (name, body) :: acc
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
  | Some (VLambda (_, body, _)) -> check (Effect.infer ~known_fn body)
  | Some (VPrim name) -> check (Effect.effects_for_prim name)
  | Some _ | None ->
      Error
        (Printf.sprintf
           "public route denied: handler %S is not a defined function"
           handler)
