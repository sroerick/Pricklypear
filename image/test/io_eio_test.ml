(** H3.1 smoke: Io_eio.Db health + one query under Eio_main.

    Needs local PG (scripts/dev.sh). Soft-fails unless PP_REQUIRE_EIO_DB=1. *)

module IE = Pricklypear_image_lib.Io_eio

let () =
  if Sys.getenv_opt "PP_SKIP_EIO_DB" = Some "1" then (
    print_endline "io_eio_test: skipped (PP_SKIP_EIO_DB=1)";
    exit 0);
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let ctx : IE.Ctx.t =
      { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
    in
    IE.with_ctx ctx (fun () ->
        let cfg = IE.Db.config_from_env () in
        let store =
          IE.Db.make ~cfg
            ~branch_id:"00000000-0000-0000-0000-000000000001" ()
        in
        let n = IE.Db.health_query_sync store in
        if n <> 1 then failwith (Printf.sprintf "health expected 1 row, got %d" n);
        let rows =
          IE.Db.simple_query_sync store "SELECT current_database()"
        in
        (match rows with
         | [ [ db ] ] ->
             Printf.printf "io_eio_test: OK health=1 database=%s\n%!" db
         | _ ->
             failwith
               ("unexpected current_database rows: "
               ^ string_of_int (List.length rows))))
  with e ->
    let msg = Printexc.to_string e in
    if Sys.getenv_opt "PP_REQUIRE_EIO_DB" = Some "1" then (
      prerr_endline ("io_eio_test FAIL: " ^ msg);
      exit 1)
    else (
      Printf.eprintf
        "io_eio_test: soft-fail (set PP_REQUIRE_EIO_DB=1 to enforce): %s\n%!" msg;
      exit 0)
