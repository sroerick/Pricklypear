(** Pricklypear Supervisor — Boot.

    Per spec [migrations.applied-by-supervisor] + [supervisor.admin-http]:
      1. Connect to PG (config from env).
      2. Run pending migrations.
      3. Start the admin HTTP server (default :8089).
      4. Start the SSH listener (default :2222).
      5. Start the health-check polling loop (TODO; every 10s).
    Supervisor is then serving; blocks forever on the http server.

    The admin HTTP server binds 0.0.0.0 (intended localhost in dev;
    prod fronted by relayd). The SSH listener binds 127.0.0.1 only
    in dev (set PP_SSH_BIND=0.0.0.0 to expose; not for prod. *)
open Lwt.Syntax

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft
let int_env_or name dft = int_of_string (env_or name (string_of_int dft))

let start () : unit Lwt.t =
  Printf.eprintf "[supervisor.boot] starting up...\n"; flush stderr;
  (* Initialize the Mirage_crypto RNG up-front. Required by awa-ssh
     (hostkey generation uses Mirage_crypto_rng.generate). Other
     supervisor code paths don't need crypto; this is a cheap,
     one-shot entropic-seed + Fortuna setup that's safe to always
     run and conditions the awa-ssh path for when PP_AWA_SSH_PORT
     is set. See BORGE-1783439434. *)
  (try Mirage_crypto_rng_unix.use_default ();
     Printf.eprintf "[supervisor.boot] RNG initialized (awa-ssh path ready)\n";
     flush stderr
   with exn ->
     Printf.eprintf "[supervisor.boot] RNG init failed (awa-ssh path disabled): %s\n"
       (Printexc.to_string exn);
     flush stderr);
  let store_cfg = Store.config_from_env () in
  let store = Store.make ~cfg:store_cfg () in
  let* connected = Store.connect store in
  let* () =
    if connected then begin
      Printf.eprintf "[supervisor.boot] running pending migrations...\n"; flush stderr;
      let* mig_res = Store.run_pending_migrations store in
      (match mig_res with
       | Error e ->
           Printf.eprintf "[supervisor.boot] MIGRATION FAILED: %s\n\
                            refusing to boot images; admin still serving.\n" e; flush stderr
       | Ok () ->
           Printf.eprintf "[supervisor.boot] migrations up to date\n"; flush stderr);
      Lwt.return_unit
    end else begin
      Printf.eprintf "[supervisor.boot] \
                        PG not connected; running in degraded mode \
                        (no migrations, admin stats only)\n"; flush stderr;
      Lwt.return_unit
    end
  in
  let fleet = Fleet.make () in
  let auth_cfg = Auth_client.config_from_env () in
  let admin_port = int_env_or "PP_ADMIN_PORT" 8089 in
  let ssh_port = int_env_or "PP_SSH_PORT" 2222 in
  let _ = Lwt.async (fun () ->
    Ssh.serve ~fleet ~auth_cfg ~port:ssh_port ())
  in
  (* Real-SSH path via awa-mirage (it21): OPT-IN. Set PP_AWA_SSH_PORT
     (default unset) to enable; otherwise no awa-ssh listener starts.
     The awa-ssh library pulls in mirage-crypto-pk etc even when not
     actively used, so this is the only AC-link of the new code path
     availed from boot — see BORGE-1783439434 for the awa 0.3.1
     static-auth-db constraint. *)
  (match Sys.getenv_opt "PP_AWA_SSH_PORT" with
   | Some pstr ->
       let awa_port = int_of_string pstr in
       let _ = Lwt.async (fun () ->
         Ssh_awa.start ~fleet ~store ~auth_cfg ~port:awa_port ()) in
       ()
   | None -> ());
  Printf.eprintf "[supervisor.boot] admin=0.0.0.0:%d ssh=127.0.0.1:%d\n"
    admin_port ssh_port; flush stderr;
  Printf.eprintf "[supervisor.boot] ready\n"; flush stderr;
  Admin.serve ~fleet ~port:admin_port ()
