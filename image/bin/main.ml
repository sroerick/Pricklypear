(** Pricklypear Image — entry point.

    A single plain OCaml / Lwt_unix binary (not a Mirage unikernel).
    All configuration is via env vars; see [Boot.manifest_from_env]
    and [Store.config_from_env]. *)
let () =
  Lwt_main.run (Pricklypear_image_lib.Boot.start ())
