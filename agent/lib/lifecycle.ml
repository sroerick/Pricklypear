(** glochid process lifecycle: PID file + signal-based control.

    Killable:  SIGTERM/SIGINT → watcher fiber resolves [shutdown_promise],
               [Http_ui.serve] returns, the Eio switch cancels the accept
               loop, and [main] exits cleanly.

    Reloadable: SIGHUP → watcher calls [on_reload], which re-evaluates
                [Surface.load] + [Plugins.load_all] against the shared
                runtime env. The HTTP listener never restarts; the next
                render sees the new definitions.

    Restartable: the PID file ([~/.config/agent/glochid.pid]) lets an
                 external wrapper (systemd user unit, shell script,
                 omarchy/QML tray widget) find and signal the running
                 process without pgrep races.

    Signal delivery is the self-pipe pattern: the Sys.signal handler does
    ONE async-signal-safe write(2) into a pipe; a dedicated fiber awaits
    readability on the other end and dispatches.  OCaml's set_signal
    handler must not call into Eio. *)

open Eio_unix.Stdenv

let pid_file () : string =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some d when d <> "" -> Filename.concat d "agent/glochid.pid"
  | _ ->
      (match Sys.getenv_opt "HOME" with
       | Some h when h <> "" -> Filename.concat h ".config/agent/glochid.pid"
       | _ -> ".config/agent/glochid.pid")

let write_pid_file () : unit =
  let path = pid_file () in
  Config.mkdir_p (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  output_string oc (string_of_int (Unix.getpid ()));
  output_char oc '\n';
  close_out oc;
  Unix.rename tmp path

let remove_pid_file () : unit =
  (try Sys.remove (pid_file ()) with Sys_error _ -> ())

(** Read the pidfile and signal it.  Returns [Ok ()] when a signal was
    delivered to a live process, [Error msg] otherwise (no pidfile, stale
    pid, or ESRCH). *)
let signal_running (sig_ : Sys.signal) : (unit, string) result =
  let path = pid_file () in
  if not (Sys.file_exists path) then
    Error ("no pid file at " ^ path)
  else
    (try
       let ic = open_in_bin path in
       let s = In_channel.input_all ic in
       close_in ic;
       match int_of_string_opt (String.trim s) with
       | None -> Error ("unparsable pid in " ^ path)
       | Some pid ->
           (try Unix.kill pid 0; Ok pid  (* liveness check *)
            with
            | Unix.Unix_error (Unix.ESRCH, _, _) ->
                Error ("stale pid " ^ string_of_int pid ^ " — process not running")
            | Unix.Unix_error (e, _, _) ->
                Error ("cannot probe pid " ^ string_of_int pid ^ ": " ^ Unix.error_message e))
           |> Result.map (fun pid -> Unix.kill pid sig_; ())
     with Sys_error e -> Error e)

(** Watcher fiber: awaits [read_fd] once per signal byte; each byte
    dispatches by index.  Bytes are written async-signal-safe from the
    Sys.signal handlers — the fiber is the only place actual work runs. *)
let watch ~read_fd ~dispatch : unit =
  let buf = Bytes.create 1 in
  (try
     while true do
       Eio_unix.await_readable read_fd;
       let n = Unix.read read_fd buf 0 1 in
       if n = 1 then
         (try dispatch (Bytes.get buf 0) with
          | e -> Printf.eprintf "[lifecycle] dispatch error: %s\n%!" (Printexc.to_string e))
     done
   with Eio.Cancel.Cancelled _ -> ()  (* normal shutdown path *)
      | e -> Printf.eprintf "[lifecycle] watcher died: %s\n%!" (Printexc.to_string e))

(** Set up the self-pipe and install handlers for {SIGTERM,SIGINT,
    SIGHUP} → single-byte pipe writes.  Returns
    [(watcher_fiber_ok, read_fd_for_switch, shutdown_promise)] — caller
    forks [watcher_fiber] under its switch and weaves [shutdown_promise]
    into the accept loop.  [on_reload] fires on HUP; the promise
    resolves on TERM/INT. *)
let install ~(on_reload : unit -> unit) :
  unit Eio.Promise.t * (unit -> unit) =
  let read_fd, write_fd = Unix.pipe ~cloexec:true () in
  let shutdown_promise, shutdown_resolver = Eio.Promise.create () in
  (* Async-signal-safe: one byte, no allocation. *)
  let write_byte b = (try ignore (Unix.write_substring write_fd b 0 1) with _ -> ()) in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> write_byte "T"));
  Sys.set_signal Sys.sigint  (Sys.Signal_handle (fun _ -> write_byte "I"));
  Sys.set_signal Sys.sighup  (Sys.Signal_handle (fun _ -> write_byte "H"));
  let fiber () =
    watch ~read_fd ~dispatch:(fun c ->
      match c with
      | 'T' | 'I' ->
          (* Resolve only once — resolving twice raises. *)
          if not (Eio.Promise.is_resolved shutdown_promise) then
            Eio.Promise.resolve shutdown_resolver ()
      | 'H' -> on_reload ()
      | _ -> ())
  in
  (shutdown_promise, fiber)
