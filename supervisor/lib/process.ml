(** Pricklypear Supervisor — Process abstraction.

    Per spec [supervisor.unikernel-target]: in dev (mirage/unix target),
    the supervisor posix_spawn's child image unikernels. In prod (Solo5),
    it'd request new VMs from the hypervisor. We hide that behind one
    interface.

    For the minimal supervisor: posix_spawn the image unikernel binary
    (built from image/bin/unikernel.ml) with env vars populated from
    the manifest. Track the child PID per instance. kill = Lwt_unix.kill. *)
open Lwt.Syntax

(* An abstract image process handle. In dev: a Lwt_unix.child process.
   In prod: a Solo5 VM ID (TODO — different concrete impl). *)
type t = {
  pid : int;                     (* child pid, used for kill *)
  started_at : float;
  mutable alive : bool;
}

(* The image unikernel binary path. Configurable via PP_IMAGE_BIN. *)
let image_bin_path () : string =
  match Sys.getenv_opt "PP_IMAGE_BIN" with
  | Some p -> p
  | None ->
      (* Default: the one built by `dune build image`. *)
      "_build/default/image/bin/unikernel.exe"

(* Spawn an image instance.

   [manifest] gives us the env vars to set (user_id, username,
   branch_id, branch_name, boot_mode, image_version).
   [http_port] and [repl_port] are the ports the image should bind
   (supervisor allocates these; default 0 = auto-pick TODO). *)
let boot
    ~(manifest : Pricklypear_common.Manifest.t)
    ~http_port ~repl_port
    : (t, string) result Lwt.t =
  let bin = image_bin_path () in
  if not (Sys.file_exists bin) then
    Lwt.return_error (Printf.sprintf "image binary not found: %s" bin)
  else
    let env =
      Array.append (Unix.environment ())
        [| "PP_USER_ID=" ^ manifest.user_id;
           "PP_USERNAME=" ^ manifest.username;
           "PP_BRANCH_ID=" ^ manifest.branch_id;
           "PP_BRANCH_NAME=" ^ manifest.branch_name;
           "PP_BOOT_MODE=" ^ Pricklypear_common.Manifest.boot_mode_to_string manifest.boot_mode;
           "PP_IMAGE_VERSION=" ^ manifest.image_version;
           "PP_HTTP_PORT=" ^ string_of_int http_port;
           "PP_REPL_PORT=" ^ string_of_int repl_port;
        |]
    in
    Lwt.catch
      (fun () ->
        let pid = Lwt_unix.fork () in
        if pid = 0 then begin
          (* Child: exec the image binary. *)
          Unix.execvpe bin [| bin |] env
        end;
        let handle = { pid; started_at = Unix.gettimeofday (); alive = true } in
        Printf.eprintf "[supervisor.process] spawned image pid=%d (user=%s, http=%d, repl=%d)\n"
          pid manifest.username http_port repl_port;
        Lwt.return_ok handle)
      (fun exn ->
        Lwt.return_error (Printexc.to_string exn))

(* Kill a spawned image process. SIGTERM, then SIGKILL after 2s.
   Uses Unix.kill (sync syscall) since we have an int pid, not a
   Lwt_unix.file_descr. TODO: timeout-and-SIGKILL escalation. *)
let kill (t : t) : unit Lwt.t =
  if t.alive then begin
    t.alive <- false;
    Printf.eprintf "[supervisor.process] killing image pid=%d\n" t.pid;
    Lwt.catch
      (fun () ->
        Unix.kill t.pid Sys.sigterm;
        Lwt.return_unit)
      (fun exn ->
        Printf.eprintf "[supervisor.process] kill failed: %s\n" (Printexc.to_string exn);
        Lwt.return_unit)
  end else Lwt.return_unit
