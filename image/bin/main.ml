(** Pricklypear Image — entry point.

    Always Eio_main + Httpun_eio + Io_eio PG. Lwt remains only under
    Lwt_eio for the optional TCP REPL. See single-image.runtime. *)

open Pricklypear_image_lib

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _token ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Io_eio.Ctx.t =
    { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
  in
  Io_eio.with_ctx ctx (fun () ->
      Printf.eprintf
        "[boot] Eio_main + Httpun_eio + Io_eio PG\n%!";
      (try
         let cfg = Io_eio.Db.config_from_env () in
         let st =
           Io_eio.Db.make ~cfg
             ~branch_id:"00000000-0000-0000-0000-000000000001" ()
         in
         let n = Io_eio.Db.health_query_sync st in
         Printf.eprintf "[boot] Io_eio.Db health rows=%d\n%!" n
       with e ->
         Printf.eprintf "[boot] Io_eio.Db health failed: %s\n%!"
           (Printexc.to_string e));
      Boot.start_eio ~env ~sw)
