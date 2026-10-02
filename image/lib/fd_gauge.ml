(** Pricklypear Image — file-descriptor gauge.

    The 2026-10-02 prod wedge was fd exhaustion: accept loops died with
    [Eio.Io (Too many open files, "accept")] while /health still answered
    200, and nothing recorded the fd count on the way down. This module is
    the missing gauge — a cheap open-fd count exposed on /health and
    logged periodically, so the next slow accumulation is visible long
    before accept fails.

    Why not [ls /dev/fd]: on OpenBSD /dev/fd is a static 64-entry
    descriptor table, not a per-process view, so it reports 64 always.
    The reliable portable count is an [fstat] sweep over fd numbers; with
    the service's login-class cap (1024) that is ~1024 cheap syscalls.

    The soft limit is read once at first use via [sh -c "ulimit -n"]
    (the image already shells out to curl for egress; no C stubs, no
    rlimit binding in OCaml's Unix). Errors fall back to 1024. *)

let fallback_limit = 1024

let limit : int Lazy.t =
  lazy
    (try
       let ic =
         Unix.open_process_args_in "/bin/sh"
           [| "sh"; "-c"; "ulimit -n" |]
       in
       let line = try input_line ic with End_of_file -> "" in
       (try ignore (Unix.close_process_in ic) with _ -> ());
       match int_of_string_opt (String.trim line) with
       | Some n when n > 0 -> n
       | _ -> fallback_limit
     with _ -> fallback_limit)

(** Number of currently-open file descriptors. Scans [0 .. limit-1];
    an fd that [fstat] rejects is closed. The scan itself opens nothing,
    so its own footprint is zero. *)
let count () : int =
  let lim = Lazy.force limit in
  let n = ref 0 in
  for fd = 0 to lim - 1 do
    match Unix.fstat (Obj.magic fd : Unix.file_descr) with
    | _ -> incr n
    | exception _ -> ()
  done;
  !n

(** Fraction of the soft limit currently in use, in [0.0, 1.0]. *)
let fraction () : float =
  let lim = Lazy.force limit in
  if lim <= 0 then 0.0 else float_of_int (count ()) /. float_of_int lim

let watch_secs = 60.0

(** High-water warning ratio: log a warning once open fds cross this
    share of the limit. 75% leaves headroom before accept starts failing. *)
let warn_ratio = 0.75

(** Background fiber forked at boot: one gauge line per minute, with a
    loud warning once the high-water ratio is crossed. Best-effort;
    never raises out of the loop (a gauge must not take the image down). *)
let watch_loop ~(clock : _ Eio.Time.clock) : unit =
  let rec loop () =
    (try
       let open_fds = count () in
       let lim = Lazy.force limit in
       let pct =
         if lim <= 0 then 0 else (100 * open_fds) / lim
       in
       if lim > 0 && float_of_int open_fds /. float_of_int lim >= warn_ratio
       then
         ignore
           (Ops_log.warn ~component:"fd-gauge"
              ~context:
                [ ("open_fds", string_of_int open_fds);
                  ("limit", string_of_int lim) ]
              (Printf.sprintf
                 "fd high-water: %d/%d (%d%%) — accept will fail if this \
                  keeps climbing" open_fds lim pct))
       else
         Printf.eprintf "[fd-gauge] open=%d limit=%d (%d%%)\n%!" open_fds
           lim pct
     with exn ->
       Printf.eprintf "[fd-gauge] sample failed: %s\n%!"
         (Printexc.to_string exn));
    Eio.Time.sleep clock watch_secs;
    loop ()
  in
  loop ()
