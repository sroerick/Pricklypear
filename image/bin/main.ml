(** Pricklypear Image — entry point.

    PP_IO=lwt (default): Lwt_main + httpun-lwt-unix + Io_lwt PG.
    PP_IO=eio: Eio_main + native Httpun_eio + Io_eio PG (Lwt_eio only for
    REPL + prepare bootstrap). *)

open Pricklypear_image_lib

let run_lwt_main () = Lwt_main.run (Boot.start ())

let run_eio_main () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _token ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Io_eio.Ctx.t =
    { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
  in
  Io_eio.with_ctx ctx (fun () ->
      (try
         let cfg = Io_eio.Db.config_from_env () in
         let st =
           Io_eio.Db.make ~cfg
             ~branch_id:"00000000-0000-0000-0000-000000000001" ()
         in
         let n = Io_eio.Db.health_query_sync st in
         Printf.eprintf "[boot] PP_IO=eio Io_eio.Db health rows=%d\n%!" n
       with e ->
         Printf.eprintf "[boot] PP_IO=eio Io_eio.Db health failed: %s\n%!"
           (Printexc.to_string e));
      Boot.start_eio ~env ~sw)

let () =
  match Io_eio.io_mode_from_env () with
  | `Eio ->
      Printf.eprintf
        "[boot] PP_IO=eio (Eio_main + Httpun_eio + Io_eio PG)\n%!";
      run_eio_main ()
  | `Lwt -> run_lwt_main ()
