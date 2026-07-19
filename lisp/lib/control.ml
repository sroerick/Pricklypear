(** Host control effects for direct-style eval (H1).

    Orthogonal to guest capability effects in [effect.ml].
    Nopales users never see [perform], Lwt, or these constructors.

    See docs/runtime-effects-transition.md Appendix D.

    H1.7: this module is Lwt-free. The image rim provides
    [Control_rim.run_lwt] to service [Prim_call] with Lwt prim tables. *)

open Ast
module Effect = Stdlib.Effect

(** Coarse prim dispatch. Handlers (sync tests or image Lwt rim) interpret. *)
type _ Effect.t +=
  | Prim_call : string * value list -> value Effect.t

let perform_prim (name : string) (args : value list) : value =
  Effect.perform (Prim_call (name, args))

type control_mode = Lwt_mode | Eff_mode

let control_mode_of_string = function
  | "lwt" | "promise" -> Lwt_mode
  | "eff" | "effect" | "effects" | _ -> Eff_mode

(** [PP_EVAL_CONTROL=lwt|eff] — default **eff** (H1.6).
    [lwt] is a legacy escape; after H1.7 the monadic eval path is gone and
    lwt mode only changes nothing if the image always uses the rim handler. *)
let control_mode_from_env () : control_mode =
  match Sys.getenv_opt "PP_EVAL_CONTROL" with
  | Some s -> control_mode_of_string (String.lowercase_ascii (String.trim s))
  | None -> Eff_mode

(** Sync prim table for pure lisp tests (no services). *)
type sync_prim_fn = value list -> value
type sync_prim_table = (string, sync_prim_fn) Hashtbl.t

let run_sync ~(prims : sync_prim_table) (thunk : unit -> 'a) : 'a =
  let open Effect.Deep in
  match_with thunk ()
    { retc = (fun v -> v)
    ; exnc = (fun e -> raise e)
    ; effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Prim_call (name, args) ->
              Some
                (fun (k : (a, _) continuation) ->
                  match Hashtbl.find_opt prims name with
                  | None ->
                      discontinue k
                        (Interp_error
                           (Prim_error (name, "unregistered primitive")))
                  | Some fn ->
                      try continue k (fn args)
                      with exn -> discontinue k exn)
          | _ -> None)
    }
