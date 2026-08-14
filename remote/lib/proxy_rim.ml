(** remote/lib/proxy_rim.ml (borge/remote-eval.borg client).

    A second rim for the SAME [Control.Prim_call] seam the image rim
    services (lisp/lib/control.ml, [Control.run_with_rim]). Where the
    image services [Prim_call(name, args)] from the in-process store
    prim table, the proxy rim forwards every prim call that is not a
    local pure prim to the server via [transport] (POST /api/prim) and
    [continue]s with the deserialized value — or [discontinue]s with
    the re-raised exception (reconstructed by Prims_pure.error_of_json)
    so a remote client observes identical exceptions to an in-process
    eval.

    PROXY DISCIPLINE (spec): the rim is a PURE proxy with zero
    interpretation logic. The only allowed local behavior is "dispatch
    to a local pure prim table" or "forward to /api/prim". Nothing in
    between — the moment it grows its own meaning for a prim there are
    two dialects.

    NOTE on Control.direct_prims: the image sets it so nested eval
    inside a prim body dispatches by direct table lookup (its table is
    TOTAL there). Here the local table (pure prims only) is
    INTENTIONALLY partial, so setting direct_prims to it would make
    every DB/other prim raise "unregistered primitive" instead of
    proxying. We therefore leave direct_prims empty and let the
    Effect.Deep handler intercept EVERY [Prim_call]; the local/proxy
    split happens in the handler, not in direct_prims. This is safe
    because no local pure prim performs a nested eval from within a
    handler body (the case direct_prims exists to serve).

    The [transport] boundary is what makes the rim unit-testable: the
    Eio HTTP transport (remote/lib/http.ml) is swapped for a mock in
    the tests, so no live server is required. *)

open Pricklypear_lisp
open Ast
module Effect = Stdlib.Effect

(** name -> args -> (value | exn). The production transport POSTs to
    /api/prim and uses the shared value/error codecs; a test transport
    records calls and returns canned values. *)
type transport = string -> value list -> (value, exn) Result.t

(** Run [thunk] under the proxy rim. Prim calls resolvable in
    [local_prims] are serviced locally (pure compute stays local, zero
    HTTP); everything else forwards to [transport] and, on error,
    re-raises the SAME exn the local rim would. *)
let run ~(local_prims : Eval.prim_table) ~(transport : transport)
    (thunk : unit -> 'a) : 'a =
  let open Effect.Deep in
  match_with thunk ()
    { retc = (fun v -> v)
    ; exnc = (fun e -> raise e)
    ; effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Control.Prim_call (name, args) ->
              Some
                (fun (k : (a, _) continuation) ->
                  match Hashtbl.find_opt local_prims name with
                  | Some fn ->
                      (try continue k (fn args) with
                       | exn -> discontinue k exn)
                  | None ->
                      (match transport name args with
                       | Ok v -> continue k v
                       | Error exn -> discontinue k exn))
          | _ -> None)
    }
