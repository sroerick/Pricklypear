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

(** Optional direct prim table for nested eval under an active rim.
    When set, [perform_prim] calls the table instead of [Effect.perform],
    avoiding Unhandled nested Prim_call (handler context) and
    Continuation_already_resumed from nested match_with. *)
let direct_prims : (string, value list -> value) Hashtbl.t option ref =
  ref None

(** Dispatch a prim call: honors [direct_prims] when set, else escalates to
    [Effect.perform] so the active rim handler (or test handler) services it.
    Raises [Interp_error] if a direct table is set but lacks [name]. *)
let perform_prim (name : string) (args : value list) : value =
  match !direct_prims with
  | Some tbl ->
      (match Hashtbl.find_opt tbl name with
       | Some fn -> fn args
       | None ->
           raise
             (Interp_error (Prim_error (name, "unregistered primitive", "prim_error"))))
  | None -> Effect.perform (Prim_call (name, args))

type control_mode = Lwt_mode | Eff_mode

(** Parse a control-mode string into a [control_mode]; unknown strings
    default to [Eff_mode]. *)
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

(** Run [thunk] under a sync effect handler that services [Prim_call] via
    [prims]; unregistered names abort the continuation with [Interp_error]. *)
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
                           (Prim_error (name, "unregistered primitive", "prim_error")))
                  | Some fn ->
                      (* Eval prim body then resume [k] once; a prim-body or
                         continuation exception propagates to [exnc]. Wrapping
                         [continue k (fn args)] in a try would [discontinue]
                         an already-resumed [k]. *)
                      continue k (fn args))
          | _ -> None)
    }

(** Host rim: service [Prim_call] with a sync prim table, setting
    [direct_prims] for nested eval (borge/agent.borg nopales-embed).

    Moved down from image/lib/control_rim.ml so both hosts (image and
    the agent binary) share one rim. Behavior identical: handler
    dispatches to the table; while a prim body runs, [direct_prims] is
    set so nested eval (eval-string, plugin callbacks, loaders)
    dispatches prims by direct lookup instead of re-entering the
    handler (effect handlers do not catch performs from within the
    handler body). *)
let run_with_rim ~(prims : sync_prim_table) (thunk : unit -> 'a) : 'a =
  let open Effect.Deep in
  let prev = !direct_prims in
  direct_prims := Some prims;
  Fun.protect
    ~finally:(fun () -> direct_prims := prev)
    (fun () ->
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
                               (Prim_error
                                  (name, "unregistered primitive", "prim_error")))
                      | Some fn ->
                          (* Same prim-body/continuation split as
                             [run_sync]: [direct_prims] usually means
                             [Prim_call] never reaches here, but be
                             correct if it does. *)
                          continue k (fn args))
              | _ -> None)
        })
