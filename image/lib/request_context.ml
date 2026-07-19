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
  (* env : Pricklypear_lisp.Env.t;  (* deferred: dune visibility / submodule export issue *) *)
  (* routes : (string, Pricklypear_lisp.Ast.value) Hashtbl.t; *)
  (* store : Store.t;                       -- Phase 3 *)
}

let make ~manifest : t =
  { manifest }

let manifest (t : t) = t.manifest

(** Lwt dynamic binding key for the current request context.
    Set by the HTTP handler after the identity gate; read by prims
    that need stable manifest (whoami, current-branch, author checks, etc.).
    This avoids the cotenant race without changing the prim_fn ABI. *)
let current : t Lwt.key = Lwt.new_key ()
