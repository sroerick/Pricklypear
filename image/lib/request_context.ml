(** Per-request context for lock-free page work (fixes BORGE-1784434193 cotenant race).

    After the identity gate (request_mutex), a fresh Request_context.t is
    built from the attached Session state and passed to all subsequent
    page work (route handlers, /api/eval, prims, render). This prevents
    anonymous attach + checkout_to + rehydrate from flipping manifest/env/routes
    under an in-flight personal-branch page.

    Phase 1: only manifest is isolated (the cotenant username/branch fields).
    Later phases will move env, routes, and store into the context.
*)

open Pricklypear_lisp
module Manifest = Pricklypear_common.Manifest

type t = {
  manifest : Manifest.t;
  env : Pricklypear_lisp.Ast.env;
  routes : (string, string * string) Hashtbl.t;
  (* store : Store.t;                       -- Phase 3 *)
}

let make ~manifest ~env ~routes : t =
  { manifest; env; routes }

let manifest (t : t) = t.manifest

let env (t : t) = t.env

let routes (t : t) = t.routes

(** Snapshot the live session routes table by COPYING it, so a concurrent
    rehydrate (Hashtbl.clear t.routes) during this request's page work cannot
    mutate the snapshot. The env and manifest fields are swapped by reference
    on rehydrate/attach (new records, old stable), so holding the reference
    IS a snapshot; routes is in-place-cleared so it must be copied. *)
let copy_routes (src : (string, string * string) Hashtbl.t)
  : (string, string * string) Hashtbl.t =
  Hashtbl.copy src

(** Sync route lookup against the snapshot. Returns None if not registered
    on the snapshot's branch. Used by HTTP dispatch after the gate, where a
    concurrent attach must not flip the table under us. *)
let lookup_route (ctx : t) (route : string) : string option =
  match Hashtbl.find_opt ctx.routes route with
  | Some (handler, _) -> Some handler
  | None -> None

let lookup_route_meta (ctx : t) (route : string) : (string * string) option =
  Hashtbl.find_opt ctx.routes route

(** Fiber-local binding for the current request context (Eio floor).
    Set by the HTTP handler after the identity gate; read by prims
    that need stable manifest (whoami, current-branch, author checks, etc.).
    This avoids the cotenant race without changing the prim_fn ABI. *)
let current_key : t Eio.Fiber.key = Eio.Fiber.create_key ()

let get () : t option = Eio.Fiber.get current_key

let with_current (ctx : t) (f : unit -> 'a) : 'a =
  (* Also bind Store_db.pp_user so SET LOCAL pp.user sees this request's
     identity inside with_transaction (user-scoped data / RLS floor). *)
  Eio.Fiber.with_binding current_key ctx (fun () ->
      Store_db.with_pp_user ctx.manifest.username f)
