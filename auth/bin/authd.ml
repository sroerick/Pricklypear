(** Pricklypear Auth — authd unikernel entry point.

    Usage:
        pricklypear-authd-exe

    Configuration via env vars (see Store.config_from_env,
    Boot.start, Http.serve):
      PP_DB_HOST=127.0.0.1 PP_DB_PORT=5433 PP_DB_NAME=pricklypear
      PP_DB_USER=pricklypear_auth PP_DB_PASS="change-me-on-prod"
      PP_AUTH_PORT=7879
      PP_BOOTSTRAP_ADMIN_KEY="ssh-ed25519 AAAA..."  (first boot)
    *)
let () =
  Lwt_main.run (Pricklypear_auth_lib.Boot.start ())
