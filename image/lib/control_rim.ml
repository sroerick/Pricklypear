(** Image rim: service [Control.Prim_call] with direct-style prim tables.

    Lives outside [lisp/] so the interpreter stays free of image I/O deps.
    Prim table entries are [value list -> value] (sync). Process I/O blocks
    the Eio fiber inside prim bodies via Store_db.

    Nested Prim_call: while a prim body runs, [Control.direct_prims] is set
    so nested eval (eval-string, load-library internals, etc.) dispatches
    prims by direct table lookup instead of Effect.perform — effect handlers
    do not re-enter for nested performs from within the handler body. *)

open Pricklypear_lisp
module Effect = Stdlib.Effect

let run
    ~(prims : (string, Ast.value list -> Ast.value) Hashtbl.t)
    (thunk : unit -> 'a) : 'a =
  let open Effect.Deep in
  let prev = !Control.direct_prims in
  Control.direct_prims := Some prims;
  Fun.protect
    ~finally:(fun () -> Control.direct_prims := prev)
    (fun () ->
      match_with thunk ()
        { retc = (fun v -> v)
        ; exnc = (fun e -> raise e)
        ; effc =
            (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Control.Prim_call (name, args) ->
                  Some
                    (fun (k : (a, _) continuation) ->
                      match Hashtbl.find_opt prims name with
                      | None ->
                          Effect.Deep.discontinue k
                            (Ast.Interp_error
                               (Ast.Prim_error
                                  (name, "unregistered primitive")))
                      | Some fn ->
                          (try Effect.Deep.continue k (fn args) with
                           | exn -> Effect.Deep.discontinue k exn))
              | _ -> None)
        })

(** Alias kept for call sites that still say run_lwt. *)
let run_lwt = run
