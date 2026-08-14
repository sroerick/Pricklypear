(** Image rim: service [Control.Prim_call] with direct-style prim tables.

    Lives outside [lisp/] so the interpreter stays free of image I/O deps.
    Prim table entries are [value list -> value] (sync). Process I/O blocks
    the Eio fiber inside prim bodies via Store_db.

    Nested Prim_call: while a prim body runs, [Control.direct_prims] is set
    so nested eval (eval-string, load-library internals, etc.) dispatches
    prims by direct table lookup instead of Effect.perform — effect handlers
    do not re-enter for nested performs from within the handler body. *)

open Pricklypear_lisp
open Ast

let run ~(prims : (string, Ast.value list -> Ast.value) Hashtbl.t)
    (thunk : unit -> 'a) : 'a =
  Control.run_with_rim ~prims thunk

(** Alias kept for call sites that still say run_lwt. *)
let run_lwt = run
