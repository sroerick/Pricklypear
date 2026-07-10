(** Pricklypear Image — Unikernel entry point.

    For the minimal image this is a plain OCaml/Lwt_unix program (the
    "mirage/unix" target equivalent — Solo5 deferred per spec, but the
    stack supports both; the HTTP/PG code carries no migration tax).

    Usage:
        pricklypear-image-unikernel

    All configuration is via env vars; see [Boot.manifest_from_env] and
    [Store.config_from_env]. *)
let () =
  Lwt_main.run (Pricklypear_image_lib.Boot.start ())
