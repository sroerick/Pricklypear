(** Pricklypear Image — Boot.

    Single-image entry (see [image.boot] / [single-image]):
      1. Build boot manifest from env (or defaults).
      2. Connect store, apply migrations, bootstrap admin if needed.
      3. Load function_defs for the branch into the session env.
      4. Start optional TCP REPL (loopback line protocol).
      5. Start HTTP (httpaf-lwt-unix) including /health.

    Env knobs include PP_USERNAME, PP_BRANCH_ID, PP_BRANCH_NAME,
    PP_BOOT_MODE, PP_IMAGE_VERSION, PP_HTTP_PORT, PP_REPL_PORT — set by
    [scripts/dev.sh] or the host service unit. *)
open Lwt.Syntax

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
    ~branch_id:(env_or "PP_BRANCH_ID"
                  "00000000-0000-0000-0000-000000000001")
    ~branch_name:(env_or "PP_BRANCH_NAME" "main")
    ~boot_mode
    ~image_version:(env_or "PP_IMAGE_VERSION" "0.1.0-dev")
    ()

let start () : unit Lwt.t =
  ignore (Ops_log.info ~component:"boot" "Pricklypear image starting up");
  Printf.eprintf "[boot] Pricklypear image starting up...\n";
  (* RNG required for password salt + session tokens (auth). *)
  Mirage_crypto_rng_unix.use_default ();
  let manifest = manifest_from_env () in
  let cfg = Store.config_from_env () in
  let store = Store.make ~cfg ~branch_id:manifest.branch_id () in
  (* Try to connect to PG but don't fail boot if it's down — the
     minimal image serves system pages with no DB. *)
  let* () = Store.connect store in
  (* First-boot admin when users table is empty (auth.bootstrap-admin). *)
  let* _ = Auth_store.bootstrap_admin_if_empty store in
  let session = Session.make ~manifest ~store () in
  let http_port = int_of_string (env_or "PP_HTTP_PORT" "8080") in
  let repl_port = int_of_string (env_or "PP_REPL_PORT" "7878") in
  let _ = Lwt.async (fun () -> Repl_endpoint.serve ~session ~port:repl_port ()) in
  Printf.eprintf "[boot] serving http://0.0.0.0:%d/  (htmx)\n" http_port;
  Printf.eprintf "[boot] REPL endpoint 127.0.0.1:%d (internal)\n" repl_port;
  Printf.eprintf "[boot] user=%s branch=%s version=%s\n"
    manifest.username manifest.branch_name manifest.image_version;
  (* Block on the HTTP server promise (never resolves until shutdown).
     The REPL endpoint runs in async. *)
  Http.serve ~session ~port:http_port ()
