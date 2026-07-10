(** Pricklypear Supervisor — Awa SSH functor-unit shims.

    Promoted from [spikes/awa_ssh/spike.ml] (it15 feasibility probe).
    Provides [Mirage_flow.S], [Mirage_time.S], [Mirage_clock.MCLOCK]
    implementations backed by plain [Lwt_unix]/[Mtime] so that
    [Awa_mirage.Make] can be instantiated on poohstack (which does
    NOT ship [awa-lwt-unix] or [mirage-clock-lwt]).

    See BORGE-1783439434 for the awa 0.3.1 static-auth-db constraint
    this stack rides on. *)
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
   Same adapter the hypothetical awa-lwt-unix package would have
   shipped on poohstack; we provide it ourselves. *)
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
        let* _ = Lwt_unix.write fd bs cs.Cstruct.off cs.Cstruct.len in
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

  let shutdown (fd : flow) (_mode : [< `read | `write | `read_write ])
      : unit Lwt.t =
    Lwt.catch
      (fun () ->
        Unix.shutdown (Lwt_unix.unix_file_descr fd) Unix.SHUTDOWN_ALL;
        Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
end
