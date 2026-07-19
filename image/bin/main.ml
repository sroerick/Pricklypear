(** Pricklypear Image — entry point.

    Default: Lwt_main (PP_IO=lwt or unset).
    H3: PP_IO=eio runs the same Boot.start under Eio_main + Lwt_eio, with
    an ambient Io_eio context (and a boot-time Eio PG health query). *)

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
      (* H3.1: prove Eio PG path before handing off to Lwt image. *)
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
      Lwt_eio.run_lwt (fun () -> Boot.start ()))

let () =
  match Io_eio.io_mode_from_env () with
  | `Eio ->
      Printf.eprintf "[boot] PP_IO=eio (Eio_main + Lwt_eio + Io_eio.Db smoke)\n%!";
      run_eio_main ()
  | `Lwt -> run_lwt_main ()
