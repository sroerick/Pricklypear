(** Pricklypear Supervisor — SSH front door.

    Per spec [supervisor.ssh-interface]:
      1. User runs: ssh alice@pricklypear.host
      2. Supervisor listens on port 22 (awa-ssh), accepts connection.
      3. SSH handshake + key exchange (awa-ssh).
      4. Auth: supervisor calls Auth_client.verify_ssh_key
         (which delegates to the auth unikernel).
      5. Supervisor boots (boot-on-connect) or attaches (always-on) image.
      6. Supervisor opens a channel to image's internal REPL endpoint and
         forwards the user's PTY stream to it.
      7. Image presents REPL prompt.
      8. On disconnect, supervisor notes session end.

    For the minimal supervisor: NON-CRYPTO transport. Accepts plain TCP
    on ssh_port; uses a line-based auth protocol so we can demo the full
    end-to-end wire-up (verify → boot image → forward to REPL → run Lisp
    → tear down) without awa-ssh's TLS/PTY machinery. The protocol:
       client sends a single line: the SSH public key (verbatim).
       server responds with one line ending in \\n:
         OK <user_id>            -> verified; subsequent bytes forwarded
         DENIED                  -> access denied; connection closed
       If OK, server boots/attaches the user's image, opens a TCP
       connection to the image's REPL endpoint on 127.0.0.1:<repl_port>,
       and forwards bytes bidirectionally until EOF.
    When awa-ssh is wired in (future plan pass), the awa-ssh channel
    will replace both the key-line read AND the byte forwarding. *)
open Lwt.Syntax
open Lwt.Infix

let default_port = 2222  (* 22 in prod; 2222 in dev to avoid sudo *)

(* Default branch for a session. v1 product cut: every image boots on
   system/main (real UUID from migration 001). Users can (fork) to a
   personal branch from the REPL/UI. Earlier synthetic "user-main"
   branch_ids were not UUIDs and broke function_defs FK inserts, so
   defines never persisted and inspect/reload failed. *)
let system_main_branch_id = "00000000-0000-0000-0000-000000000001"
let system_main_branch_name = "system/main"

(* Forward bytes between two bidirectional streams until either side
   closes. Returns when BOTH directions are done. *)
let proxy (a_in : Lwt_io.input_channel) (a_out : Lwt_io.output_channel)
          (b_in : Lwt_io.input_channel) (b_out : Lwt_io.output_channel)
    : unit Lwt.t =
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  (* One direction: copy bytes from src to dst until EOF. *)
  let copy_dir src dst =
    let rec loop () =
      let* n = Lwt_io.read_into src buf 0 buf_size in
      if n = 0 then Lwt.return_unit
      else
        let* _written = Lwt_io.write_from dst buf 0 n in
        let* () = Lwt_io.flush dst in
        loop ()
    in
    loop ()
  in
  let a_to_b = copy_dir a_in b_out in
  let b_to_a = copy_dir b_in a_out in
  (* When EITHER direction ends, the other side will eventually hit EOF
     when its peer closes; we use Lwt.pick so we return as soon as one
     side closes, then close everything. *)
  Lwt.pick [ a_to_b; b_to_a ]

(* Build a manifest for this user. Usesensible defaults for boot_mode
   (boot-on-connect), image_version (dev), no feature flags. *)
let build_manifest
    ~(user_id : string)
    ~(username : string)
    : Pricklypear_common.Manifest.t =
  Pricklypear_common.Manifest.make
    ~user_id ~username
    ~branch_id:system_main_branch_id
    ~branch_name:system_main_branch_name
    ~boot_mode:Pricklypear_common.Manifest.Boot_on_connect
    ~image_version:"dev"
    ()

let rec serve
    ~(fleet : Fleet.t)
    ~(auth_cfg : Auth_client.config)
    ?(port = default_port) ()
    : unit Lwt.t =
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind sock addr in
  Lwt_unix.listen sock 5;
  Printf.eprintf "[supervisor.ssh] listening on 127.0.0.1:%d \
                    (NON-CRYPTO line-protocol; awa-ssh deferred)\n" port;
  flush stderr;
  let rec loop () =
    let* (fd, _peer) = Lwt_unix.accept sock in
    (* Each connection handled in an async task so the main loop can
       accept the next. *)
    Lwt.async (fun () ->
      handle_conn ~fleet ~auth_cfg fd
        >>= fun _ -> Lwt.return_unit
        >>= fun () ->
        (try Lwt_unix.close fd with _ -> Lwt.return_unit));
    loop ()
  in
  loop ()

and handle_conn
    ~(fleet : Fleet.t)
    ~(auth_cfg : Auth_client.config)
    (fd : Lwt_unix.file_descr) : unit Lwt.t =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let* () = Lwt_io.write oc "pricklypear supervisor. Send your SSH public key as the first line.\n" in
  let* () = Lwt_io.flush oc in
  (* Read the first line = the SSH key. *)
  let* line_opt = Lwt_io.read_line_opt ic in
  match line_opt with
  | None ->
      (* Client disconnected without sending a key. *)
      Lwt.return_unit
  | Some raw_key ->
      let key = String.trim raw_key in
      if key = "" then begin
        let* () = Lwt_io.write oc "DENIED (empty key)\n" in
        let* () = Lwt_io.flush oc in
        Lwt.return_unit
      end else begin
        let req = { Pricklypear_common.Auth_types.key } in
        let* resp = Auth_client.verify_ssh_key auth_cfg req in
        (match resp with
         | Pricklypear_common.Auth_types.Denied ->
             let* () = Lwt_io.write oc "DENIED\n" in
             let* () = Lwt_io.flush oc in
             Lwt.return_unit
         | Pricklypear_common.Auth_types.Ok user_id ->
             (* Look up an existing image for this user, or boot one. *)
             let* inst =
               match Fleet.find_by_user fleet user_id with
               | Some inst -> Lwt.return inst
               | None ->
                   let username = Printf.sprintf "user-%s" (String.sub user_id 0 8) in
                   let manifest = build_manifest ~user_id ~username in
                   let* boot_res = Fleet.boot fleet ~manifest in
                   (match boot_res with
                    | Error e ->
                        let* () = Lwt_io.write oc
                          (Printf.sprintf "DENIED (image boot failed: %s)\n" e) in
                        let* () = Lwt_io.flush oc in
                        Lwt.fail (Failure "image boot failed")
                    | Ok inst ->
                        Printf.eprintf "[supervisor.ssh] booted image id=%s for user=%s on http=%d repl=%d\n"
                          inst.id user_id inst.http_port inst.repl_port;
                        flush stderr;
                        Lwt.return inst)
             in
             let* () = Lwt_io.write oc
               (Printf.sprintf "OK %s\nimage=%s repl_endpoint=127.0.0.1:%d\n---\n"
                  user_id inst.id inst.repl_port) in
             let* () = Lwt_io.flush oc in
             (* Open a TCP connection to the image's REPL endpoint and
                proxy bytes bidirectionally. *)
             let repl_addr =
               Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, inst.repl_port)
             in
             Lwt.catch
               (fun () ->
                 let repl_fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
                 (* The image may take a few hundred ms to bind its
                    REPL endpoint after the supervisor's Process.boot
                    returns (we exec the image and don't wait for
                    readiness). Retry connect a few times with
                    backoff to amortize that race. *)
                 let rec connect_loop attempts =
                   if attempts >= 20 then
                     Lwt.fail (Failure "repl endpoint never came up")
                   else
                     Lwt.catch
                       (fun () -> Lwt_unix.connect repl_fd repl_addr)
                       (fun exn ->
                         let* () = Lwt_unix.sleep (0.1 *. float_of_int (attempts + 1)) in
                         connect_loop (attempts + 1))
                 in
                 let* () = connect_loop 0 in
                 let repl_ic = Lwt_io.of_fd ~mode:Lwt_io.Input repl_fd in
                 let repl_oc = Lwt_io.of_fd ~mode:Lwt_io.Output repl_fd in
                 (* Bi-directional proxy until either side closes. *)
                 let* () = proxy ic oc repl_ic repl_oc in
                 (try Lwt_io.close repl_ic with _ -> Lwt.return_unit)
                 >>= fun () ->
                 (try Lwt_io.close repl_oc with _ -> Lwt.return_unit)
                 >>= fun () -> Lwt.return_unit)
               (fun exn ->
                 Printf.eprintf "[supervisor.ssh] repl proxy failed: %s\n"
                   (Printexc.to_string exn);
                 flush stderr;
                 Lwt.return_unit))
      end
