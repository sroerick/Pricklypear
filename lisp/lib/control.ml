(** Host control effects for direct-style eval (H1).

    Orthogonal to guest capability effects in [effect.ml].
    Nopales users never see [perform], Lwt, or these constructors.

    See docs/runtime-effects-transition.md Appendix D. *)

open Ast
module Effect = Stdlib.Effect

(** Coarse prim dispatch. Image Lwt (or later Eio) handlers interpret this. *)
type _ Effect.t +=
  | Prim_call : string * value list -> value Effect.t

let perform_prim (name : string) (args : value list) : value =
  Effect.perform (Prim_call (name, args))

type control_mode = Lwt_mode | Eff_mode

let control_mode_of_string = function
  | "eff" | "effect" | "effects" -> Eff_mode
  | "lwt" | "promise" | _ -> Lwt_mode

(** [PP_EVAL_CONTROL=lwt|eff] — default **eff** after H1.6 soak (2026-07-18).
    Set PP_EVAL_CONTROL=lwt to force the legacy monadic path. *)
let control_mode_from_env () : control_mode =
  match Sys.getenv_opt "PP_EVAL_CONTROL" with
  | Some s -> control_mode_of_string (String.lowercase_ascii (String.trim s))
  | None -> Eff_mode

(** Run a direct-style thunk, servicing [Prim_call] via an Lwt prim table.
    This is the session/HTTP rim bridge while the image still speaks Lwt. *)
let run_lwt
    ~(prims : (string, value list -> value Lwt.t) Hashtbl.t)
    (thunk : unit -> 'a) : 'a Lwt.t =
  let open Effect.Deep in
  match_with thunk ()
    { retc = (fun v -> Lwt.return v)
    ; exnc = (fun e -> Lwt.fail e)
    ; effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Prim_call (name, args) ->
              Some
                (fun (k : (a, _) continuation) ->
                  match Hashtbl.find_opt prims name with
                  | None ->
                      Effect.Deep.discontinue k
                        (Interp_error
                           (Prim_error (name, "unregistered primitive")))
                  | Some fn ->
                      Lwt.try_bind
                        (fun () -> fn args)
                        (fun v -> Effect.Deep.continue k v)
                        (fun exn -> Effect.Deep.discontinue k exn))
          | _ -> None)
    }
