(** Pricklypear Supervisor — unikernel entry point.

    Usage:
        pricklypear-supervisor-exe

    Configuration via env vars (see Store.config_from_env,
    Boot.start, Admin.serve, Ssh.serve). Defaults:
        PP_DB_HOST=127.0.0.1  PP_DB_PORT=5433  PP_DB_NAME=pricklypear
        PP_DB_USER=postgres  PP_DB_PASS=""
        PP_ADMIN_PORT=8089  PP_SSH_PORT=2222

    (The supervisor does NOT connect as pricklypear_auth; that role is
    reserved for the auth unikernel. The supervisor connects with its
    own role — in dev that's plain 'postgres'.) *)
let () =
  Lwt_main.run (Pricklypear_supervisor_lib.Boot.start ())
