(** Image rim: service [Control.Prim_call] with Lwt prim tables (H1.7).

    Lives outside [lisp/] so the interpreter stays Lwt-free. *)

open Pricklypear_lisp
module Effect = Stdlib.Effect

let run_lwt
    ~(prims : (string, Ast.value list -> Ast.value Lwt.t) Hashtbl.t)
    (thunk : unit -> 'a) : 'a Lwt.t =
  let open Effect.Deep in
  match_with thunk ()
    { retc = (fun v -> Lwt.return v)
    ; exnc = (fun e -> Lwt.fail e)
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
                           (Ast.Prim_error (name, "unregistered primitive")))
                  | Some fn ->
                      Lwt.try_bind
                        (fun () -> fn args)
                        (fun v -> Effect.Deep.continue k v)
                        (fun exn -> Effect.Deep.discontinue k exn))
          | _ -> None)
    }
