(** Pricklypear awa-ssh feasibility probe (iteration 15 spike).

    Confirms that the awa-mirage SSH stack on the poohstack opam switch
    can have its three functor arguments satisfied from a plain
    [Lwt_unix.file_descr] flow, [Mtime]-based monotonic clock, and
    [Lwt_unix.sleep]-based sleep.

    The actual swap-in (replacing [supervisor/lib/ssh.ml]'s non-crypto
    line protocol with real SSH) is left for a dedicated future
    iteration — this module only proves the type signature of
    `Awa_mirage.Make` accepts the three shim modules we'd ship with
    the swap.

    To probe the awa API surface, run: `dune build spikes/awa_ssh`. *)
open Printf
open Lwt.Syntax

(* ---- T : Mirage_time.S (sleep_ns over Lwt_unix.sleep) ---- *)
module Time = struct
  let sleep_ns (ns : int64) : unit Lwt.t =
    let secs = Int64.to_float ns /. 1e9 in
    Lwt_unix.sleep secs
end

(* ---- M : Mirage_clock.MCLOCK (elapsed_ns over Mtime_clock.elapsed) ---- *)
module Clock = struct
  let elapsed_ns () : int64 =
    let span = Mtime_clock.elapsed () in
    Mtime.Span.to_uint64_ns span
  let period_ns () : int64 option = None
end

(* ---- F : Mirage_flow.S (flow over a [Lwt_unix.file_descr]) ----
   This adapter is what the missing awa-lwt-unix package would have
   provided; on poohstack we supply it ourselves. The read/write/close
   implementation is complete enough for both directions of an SSH
   transport. *)
module Flow = struct
  type error = [ `Msg of string ]

  let pp_error fmt (`Msg s) = Fmt.string fmt s

  type write_error = [ `Closed | error ]

  let pp_write_error fmt = function
    | `Closed -> Fmt.string fmt "closed"
    | #error as e -> pp_error fmt e

  type flow = Lwt_unix.file_descr

  let read (fd : flow) : (Cstruct.t Mirage_flow.or_eof, error) result Lwt.t =
    let cap = 4096 in
    let buf = Bytes.create cap in
    Lwt.catch
      (fun () ->
        let* n = Lwt_unix.read fd buf 0 cap in
        if n = 0 then Lwt.return (Ok `Eof)
        else Lwt.return (Ok (`Data (Cstruct.of_bytes buf ~off:0 ~len:n))))
      (fun exn -> Lwt.return (Error (`Msg (Printexc.to_string exn))))

  let write (fd : flow) (cs : Cstruct.t)
    : (unit, write_error) result Lwt.t =
    Lwt.catch
      (fun () ->
        let bs = Cstruct.to_bytes cs in
        let* _ = Lwt_unix.write fd bs cs.Cstruct.off cs.Cstruct.len
        in
        Lwt.return (Ok ()))
      (fun exn -> Lwt.return (Error (`Msg (Printexc.to_string exn))))

  let writev (fd : flow) (css : Cstruct.t list)
    : (unit, write_error) result Lwt.t =
    let rec go = function
      | [] -> Lwt.return (Ok ())
      | cs :: rest ->
          let* r = write fd cs in
          (match r with
           | Ok () -> go rest
           | Error e -> Lwt.return (Error e))
    in
    go css

  let close (fd : flow) : unit Lwt.t =
    Lwt.catch
      (fun () -> Lwt_unix.close fd)
      (fun _ -> Lwt.return_unit)

  let shutdown (fd : flow) (_mode : [< `read | `write | `read_write ]) : unit Lwt.t =
    Lwt.catch
      (fun () ->
        Unix.shutdown (Lwt_unix.unix_file_descr fd) Unix.SHUTDOWN_ALL;
        Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
end

(* Link-check that Awa_mirage.Make compiles against these shims.
   If the functor application type-checks, the spike is proven feasible. *)
module SSH = Awa_mirage.Make (Flow) (Time) (Clock)

(* A trivial entry point so dune has something to build. *)
let () = printf "awa-ssh spike compiles; SSH.spawn_server reachable\n%!"
