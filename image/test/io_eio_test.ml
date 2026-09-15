(** H3.1 smoke: Io_eio sequencer invariant + optional DB health under Eio_main.

    Sequencer self-test always runs (no PG).
    DB probe needs local PG (scripts/dev.sh). Soft-fails unless PP_REQUIRE_EIO_DB=1. *)

module IE = Pricklypear_image_lib.Io_eio

let () =
  (* invariant 6: always, no PG required *)
  (try
     Eio_main.run @@ fun _env ->
     if not (IE.sequencer_survives_raise ()) then
       failwith "sequencer_survives_raise returned false";
     print_endline "io_eio_test: OK sequencer survives raise"
   with e ->
     prerr_endline
       ("io_eio_test FAIL sequencer: " ^ Printexc.to_string e);
     exit 1);
  if Sys.getenv_opt "PP_SKIP_EIO_DB" = Some "1" then (
    print_endline "io_eio_test: DB skipped (PP_SKIP_EIO_DB=1)";
    exit 0);
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let ctx : IE.Ctx.t =
      Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
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
