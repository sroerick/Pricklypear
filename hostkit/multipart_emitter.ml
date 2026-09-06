(** Multipart_emitter — the multipart/mixed htmx-fragment stream
    emitter shared by the two local surfaces (agent + pp-remote) and
    originally ported from image/lib/http_agent.ml. ONE home for the
    write watchdog: before i-1788671304-d the CLOSE-WAIT write-watchdog
    lived in THREE homes (agent http_ui, remote http_ui, image
    http_agent.ml), each comment pointing at the others — the next
    httpun behavior change would get fixed once and missed twice. The
    image keeps its own copy only because image/ does not link the
    agent/remote libs. *)

(* Bail out of the multipart drain loop when the client disconnects. *)
exception Peer_gone

(* One multipart part frame + flush. Mirrors the image's mp_write_part:
   --boundary + Content-Type: text/html + optional HX-Target/HX-Swap +
   blank + html. Returns `Peer_gone on closed writer so the drain loop
   stops cleanly.

   WRITE WATCHDOG (the ONE home — image/lib/http_agent.ml carries a
   copy only because image/ links neither agent nor remote libs): a
   client that disconnects mid-stream leaves the socket in CLOSE-WAIT —
   writes keep SUCCEEDING into the kernel send buffer until it fills,
   then the write/flush blocks FOREVER, wedging the turn (and with it
   the turn/req mutex, so every later /chat parks). On some teardowns
   gluten stops servicing writes without closing the Body.Writer, so
   the flush callback never fires either way. Race the whole write path
   against a 5s sleep: a healthy part write completes in milliseconds;
   a timeout means the connection is gone → Peer_gone. *)
let mp_write_part ~(env : Eio_unix.Stdenv.base) (body : Httpun.Body.Writer.t)
    ~boundary ?(target : string option) ?(swap : string option)
    (html : string) : [ `Ok | `Peer_gone ] =
  let buf = Buffer.create 128 in
  Buffer.add_string buf ("--" ^ boundary ^ "\r\n");
  Buffer.add_string buf "Content-Type: text/html; charset=utf-8\r\n";
  (match target with Some t -> Buffer.add_string buf ("HX-Target: " ^ t ^ "\r\n") | None -> ());
  (match swap with Some s -> Buffer.add_string buf ("HX-Swap: " ^ s ^ "\r\n") | None -> ());
  Buffer.add_string buf "\r\n";
  Buffer.add_string buf html;
  Buffer.add_string buf "\r\n";
  let chunk = Buffer.contents buf in
  let write_and_flush () =
    Httpun.Body.Writer.write_string body chunk;
    let promise, resolver = Eio.Promise.create () in
    let wake () =
      if not (Eio.Promise.is_resolved promise) then Eio.Promise.resolve resolver ()
    in
    (try Httpun.Body.Writer.flush body (fun () -> wake ())
     with exn when Http_kit.is_closed_writer_exn exn -> wake ());
    Eio.Promise.await promise
  in
  try
    (* Race the WHOLE write path (write_string + flush + await): a dead
       peer in CLOSE-WAIT accepts writes until the send buffer fills,
       then the write blocks FOREVER — a healthy part write completes in
       milliseconds, so a 5s timeout means the connection is gone. *)
    let timed_out =
      Eio.Fiber.any
        [ (fun () -> write_and_flush (); false)
        ; (fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 5.0; true) ]
    in
    if timed_out then `Peer_gone else `Ok
  with exn when Http_kit.is_closed_writer_exn exn ->
    `Peer_gone

let mp_close (body : Httpun.Body.Writer.t) ~boundary : unit =
  let chunk = "--" ^ boundary ^ "--\r\n" in
  (try Httpun.Body.Writer.write_string body chunk
   with exn when Http_kit.is_closed_writer_exn exn -> ());
  Http_kit.close_writer_quiet body
