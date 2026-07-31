(** Pricklypear Image — Boot.

    Single-image entry (see [image.boot] / [single-image]):
      1. Build boot manifest from env (or defaults).
      2. Connect store, bootstrap admin if needed.
      3. Load function_defs for the branch into the session env.
      4. Start optional TCP REPL (loopback line protocol) as a fiber.
      5. Start HTTP via Httpun_eio ([start_eio]).

    Env knobs include PP_USERNAME, PP_BRANCH_ID, PP_BRANCH_NAME,
    PP_BOOT_MODE, PP_IMAGE_VERSION, PP_HTTP_PORT, PP_REPL_PORT — set by
    [scripts/dev.sh] or the host service unit. *)

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft

let manifest_from_env () : Pricklypear_common.Manifest.t =
  let boot_mode =
    match env_or "PP_BOOT_MODE" "on-connect" with
    | "always-on" -> Pricklypear_common.Manifest.Boot_always_on
    | _ -> Pricklypear_common.Manifest.Boot_on_connect
  in
  Pricklypear_common.Manifest.make
    ~user_id:(env_or "PP_USER_ID" "dev-user")
    ~username:(env_or "PP_USERNAME" "dev")
    ~branch_id:
      (env_or "PP_BRANCH_ID" "00000000-0000-0000-0000-000000000001")
    ~branch_name:(env_or "PP_BRANCH_NAME" "main")
    ~boot_mode
    ~image_version:(env_or "PP_IMAGE_VERSION" "0.1.0-dev")
    ()

(** Shared setup: RNG, store connect, session. Returns session + ports. *)
let prepare () : Session.t * int * int =
  ignore (Ops_log.info ~component:"boot" "Pricklypear image starting up");
  Printf.eprintf "[boot] Pricklypear image starting up...\n%!";
  Mirage_crypto_rng_unix.use_default ();
  Random.self_init ();
  let manifest = manifest_from_env () in
  let cfg = Store.config_from_env () in
  let store = Store.make ~cfg ~branch_id:manifest.branch_id () in
  Store.connect store;
  ignore (Auth_store.bootstrap_admin_if_empty store);
  let session = Session.make ~manifest ~store () in
  let http_port = int_of_string (env_or "PP_HTTP_PORT" "8080") in
  let repl_port = int_of_string (env_or "PP_REPL_PORT" "7878") in
  Printf.eprintf "[boot] REPL endpoint 127.0.0.1:%d (internal)\n%!" repl_port;
  Printf.eprintf "[boot] user=%s branch=%s version=%s\n%!"
    manifest.username manifest.branch_name manifest.image_version;
  (session, http_port, repl_port)

(** Eio path: prepare + fork REPL fiber + Httpun_eio accept loop.
    Call under Eio_main + Io_eio.with_ctx + Lwt_eio.with_event_loop. *)
let start_eio ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) : unit =
  let session, http_port, repl_port = prepare () in
  (* REPL still uses Lwt_unix sockets under Lwt_eio. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        Lwt_eio.run_lwt (fun () ->
            Repl_endpoint.serve ~session ~port:repl_port ())
      with exn ->
        Printf.eprintf "[boot] REPL fiber exited: %s\n%!"
          (Printexc.to_string exn));
  Printf.eprintf "[boot] serving http://0.0.0.0:%d/  (htmx, httpun-eio)\n%!"
    http_port;
  Http.serve_eio ~session ~env ~sw ~port:http_port ()

(** Legacy name — same as start_eio without env (not used by main). *)
let start () : unit =
  failwith "Boot.start: use Boot.start_eio under Eio_main (image/bin/main.ml)"
