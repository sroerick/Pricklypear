(** Pricklypear Supervisor — Real SSH via awa-mirage (0.6.x).

    Per spec [supervisor.ssh-interface]: the supervisor's front door is
    "ssh alice@pricklypear.host". [Ssh.serve] handles the v1 line protocol
    (hand-rolled non-crypto). This module is the awa-mirage SSH handshake
    path — real SSH transport — gated behind PP_AWA_SSH_PORT.

    awa-mirage 0.6: Make takes only Mirage_flow.S; Auth.db lives in
    Awa_mirage.Auth; spawn_server server user_db msgs flow exec_callback.
    exec_callback still does not receive the authenticated username
    (identity propagation still deferred). Line-protocol remains default. *)
open Lwt.Syntax
open Lwt.Infix

let pp = Printf.eprintf
let flush' () = flush stderr

let default_port = 12223  (* awa-ssh real-SSH dev port; PP_SSH_PORT (2222) is the line protocol *)

(* awa-mirage 0.6: functor takes only Flow. *)
module SSH = Awa_mirage.Make (Ssh_awa_shims.Flow)

(* Build the exec_callback for one connection. The callback gets the
   SSH channel's ic/oc functions (Cstruct-based) and bridges them to
   the image's REPL endpoint.

   This callback handles a single Shell OR Channel request per
   connection (awa 0.3.1 schema). Per-connection state: the booted
   instance id (or None if the image couldn't boot). *)
let make_exec_callback
    ~(fleet : Fleet.t)
    : SSH.exec_callback =
  let boot_res_ref : Fleet.instance option ref = ref None in
  fun req ->
    match req with
    | SSH.Pty_req _ | SSH.Pty_set _ | SSH.Set_env _ ->
        (* Terminal/env metadata we ignore for v1. Just acknowledge. *)
        Lwt.return_unit
    | (SSH.Channel _ | SSH.Shell _) as shell_req ->
        let (ic, oc, _ec, cmd_opt) =
          match shell_req with
          | SSH.Shell { ic; oc; ec } -> (ic, oc, ec, None)
          | SSH.Channel { cmd; ic; oc; ec } -> (ic, oc, ec, Some cmd)
          | _ -> (* shouldn't happen; matched above *) assert false
        in
        (* Boot-on-connect: a fresh image under the supervisor's
           system context. Identity propagation deferred per the
           comment in this module's header. *)
        let user_id = "system-via-awa"
        and username = "admin" in
        let* inst =
          match !boot_res_ref with
          | Some inst -> Lwt.return inst
          | None ->
              match Fleet.find_by_user fleet user_id with
              | Some inst -> boot_res_ref := Some inst; Lwt.return inst
              | None ->
                  let manifest =
                    Ssh.build_manifest ~user_id ~username
                  in
                  let* boot_res = Fleet.boot fleet ~manifest in
                  (match boot_res with
                   | Error e ->
                       pp "[supervisor.ssh_awa] image boot failed: %s\n" e;
                       flush' ();
                       Lwt.fail (Failure e)
                   | Ok inst ->
                       pp "[supervisor.ssh_awa] booted image id=%s http=%d repl=%d\n"
                         inst.id inst.http_port inst.repl_port;
                       flush' ();
                       boot_res_ref := Some inst;
                       Lwt.return inst)
        in
        (* Connect to the image's REPL endpoint (see comment in Ssh.handle_conn:
           image boot races supervisor's connect — retry with backoff). *)
        let repl_addr =
          Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, inst.repl_port)
        in
        let rec connect_loop attempts =
          if attempts >= 20 then Lwt.fail (Failure "repl endpoint never came up")
          else
            Lwt.catch
              (fun () ->
                let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
                let+ () = Lwt_unix.connect fd repl_addr in
                fd)
              (fun _exn ->
                let* () = Lwt_unix.sleep (0.1 *. float_of_int (attempts + 1)) in
                connect_loop (attempts + 1))
        in
        let* repl_fd =
          Lwt.catch
            (fun () -> connect_loop 0)
            (fun exn ->
              pp "[supervisor.ssh_awa] could not connect to image REPL: %s\n"
                (Printexc.to_string exn);
              flush' ();
              Lwt.fail exn)
        in
        (* Convert the image's Lwt_unix.file_descr to Lwt_io channels. *)
        let repl_ic = Lwt_io.of_fd ~mode:Lwt_io.Input repl_fd in
        let repl_oc = Lwt_io.of_fd ~mode:Lwt_io.Output repl_fd in

        (* For [SSH.Channel { cmd; ... }] requests (i.e.
           `ssh user@host '<command>'`), the command string IS in cmd,
           NOT in stdin. We send cmd as a line to the image REPL first,
           so its output gets piped back through image_to_user. For
           [SSH.Shell] (interactive session), cmd_opt is None and we
           just pipe bidirectionally.

           KNOWN LIMITATION (awa 0.3.1): there's no exposed API to
           signal [SSH_MSG_CHANNEL_EOF] / [SSH_MSG_CHANNEL_CLOSE]
           from inside the exec_callback. awa only emits EOF on stop
           hook or when [ic] returns [`Eof]. For an interactive Shell
           session, the user types [:quit] (line protocol) or SSH EOF
           cleanly ends both sides. For a non-interactive Channel
           [ssh user@host 'cmd'], the user's ssh client sends a command
           and waits for channel EOF — instead it times out / hangs
           until the network close. Most users will use the interactive
           form; the Channel form is a UX nicety to add after the awa
           0.5.x upgrade (BORGE-1783439434). *)
        Lwt.return_unit
        >>= fun () ->

        (* Bidirectional proxy. KEY INSIGHT (it24 fix to it21 bug):
           use [Lwt.join] (wait for BOTH directions) instead of
           [Lwt.pick] (return on EITHER). With [Lwt.pick], when the
           user stdin hits EOF first (which it does for both
           non-interactive `ssh user@host 'cmd'` AND for
           pipe-redirected interactive sessions once the pipe closes),
           [Lwt.pick] returned immediately and closed the channels
           before the image had finished draining its output. Result:
           connection closed-by-remote-host with no REPL output.
           [Lwt.join] waits for BOTH sides to reach EOF — including
           the image-to-user direction, which drains to its own EOF
           when the image's REPL is closed (via :quit or image exit). *)
        let buf = Bytes.create 4096 in
        let user_to_image =
          (* For Channel requests without interactive stdin: send the
             command line to the image REPL once before the loop, so
             the user doesn't need to retype it inside the shell. *)
          (match cmd_opt with
           | Some cmd ->
               let* () = Lwt_io.write repl_oc (cmd ^ "\n") in
               Lwt_io.flush repl_oc
           | None -> Lwt.return_unit)
          >>= fun () ->
          let rec loop () =
            let* chunk_or_eof = ic () in
            match chunk_or_eof with
            | `Eof -> Lwt.return_unit
            | `Data cs ->
                let s = Cstruct.to_string cs in
                let* () = Lwt_io.write repl_oc s in
                let* () = Lwt_io.flush repl_oc in
                loop ()
          in loop ()
        in
        (* image -> user: read from image REPL socket, push via awa's oc (). *)
        let image_to_user =
          let rec loop () =
            let* n = Lwt_io.read_into repl_ic buf 0 4096 in
            if n = 0 then Lwt.return_unit
            else
              let cs = Cstruct.of_bytes buf ~off:0 ~len:n in
              let* () = oc cs in
              loop ()
          in loop ()
        in
        let* () = Lwt.join [ user_to_image; image_to_user ] in
        (try Lwt_io.close repl_ic with _ -> Lwt.return_unit)
        >>= fun () ->
        (try Lwt_io.close repl_oc with _ -> Lwt.return_unit)
        >>= fun () -> Lwt.return_unit

let serve
    ~(fleet : Fleet.t)
    ~(auth_db : Awa_mirage.Auth.db)
    ~(hostkey : Awa.Hostkey.priv)
    ?(port = default_port)
    () : unit Lwt.t =
  let server, msgs = Awa.Server.make hostkey in
  let exec_callback = make_exec_callback ~fleet in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock =
    Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind sock addr in
  Lwt_unix.listen sock 16;
  pp "[supervisor.ssh_awa] awa-ssh listener on 127.0.0.1:%d \
      (real SSH; auth-db has %d user(s))\n"
    port (List.length auth_db);
  flush' ();
  let rec loop () =
    let* (fd, _peer) = Lwt_unix.accept sock in
    Lwt.async (fun () ->
      (* awa-mirage 0.6: spawn_server server user_db msgs flow exec_callback *)
      Lwt.catch
        (fun () ->
          let* _t = SSH.spawn_server server auth_db msgs fd exec_callback in
          Lwt.return_unit)
        (fun exn ->
          pp "[supervisor.ssh_awa] connection error: %s\n"
            (Printexc.to_string exn);
          flush' ();
          Lwt.return_unit)
      >>= fun () ->
      (try Lwt_unix.close fd with _ -> Lwt.return_unit));
    loop ()
  in
  loop ()

(* Build the auth db + hostkey at supervisor boot, then serve.
   Used by boot.ml as a one-shot setup-and-listen. *)
let start
    ~(fleet : Fleet.t)
    ~(store : Store.t)
    ~(auth_cfg : Auth_client.config)
    ?(port = default_port)
    () : unit Lwt.t =
  let* hostkey = Ssh_keys.hostkey store in
  let* entries = Auth_client.list_ssh_credentials auth_cfg in
  let auth_db = Ssh_keys.build_auth_db entries in
  serve ~fleet ~auth_db ~hostkey ~port ()
