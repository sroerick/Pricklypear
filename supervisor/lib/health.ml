(** Pricklypear Supervisor — Health checking.

    Per spec [supervisor.health]: periodically (every 10s) ping each
    running image's HTTP /health endpoint. Response within timeout ->
    healthy. No response in N tries -> unhealthy. Unhealthy X seconds
    -> supervisor kills and either restarts (always-on) or marks dead
    (boot-on-connect).

    For the minimal supervisor: a [ping] function that opens a plain
    TCP socket, sends a minimal HTTP/1.0 GET /health, parses the JSON
    response. Returns the parsed [health_response] or [H_dead] on failure.

    The polling loop itself is started by [serve]. *)
open Lwt.Syntax
open Lwt.Infix

let http_get_json
    ~host ~port ~path
    : (Yojson.Safe.t, string) result Lwt.t =
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let* res =
    Lwt.catch
      (fun () ->
        let* () = Lwt_unix.connect sock addr in
        let req =
          Printf.sprintf "GET %s HTTP/1.0\r\nHost: %s\r\nConnection: close\r\n\r\n"
            path host
        in
        let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
        let* () = Lwt_io.write oc req in
        let* () = Lwt_io.flush oc in
        let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
        let* lines = Lwt_stream.to_list (Lwt_io.read_lines ic) in
        Lwt.return (Ok (String.concat "\n" lines)))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
  in
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.close sock in
      Lwt.return_unit)
    (fun _ -> Lwt.return_unit)
    >>= fun () ->
  match res with
  | Error e -> Lwt.return_error e
  | Ok buf ->
      (* Find the body (after \r\n\r\n) *)
      let body =
        match String.index_opt buf '\n' with
        | None -> buf
        | Some _ ->
            (* split on "\r\n\r\n" *)
            let sep = "\r\n\r\n" in
            let slen = String.length sep in
            let rec find i =
              if i + slen > String.length buf then buf
              else if String.sub buf i slen = sep then
                String.sub buf (i + slen) (String.length buf - i - slen)
              else find (i + 1)
            in
            find 0
      in
      let body = String.trim body in
      try Lwt.return_ok (Yojson.Safe.from_string body)
      with Yojson.Json_error msg -> Lwt.return_error msg

(* Ping an image instance's /health endpoint. Returns Either a parsed
   health_response (the supervisor can read ok/uptime/etc) or H_dead
   if the image didn't respond. *)
let ping
    ~host ~port
    : Pricklypear_common.Protocol.health_response Lwt.t =
  let* res = http_get_json ~host ~port ~path:"/health" in
  match res with
  | Error _ ->
      Lwt.return
        { Pricklypear_common.Protocol.ok = false;
          branch = "";
          uptime_seconds = 0;
          error = Some "ping failed" }
  | Ok j ->
      (match Pricklypear_common.Protocol.health_response_of_json j with
       | Ok h -> Lwt.return h
       | Error msg ->
           Lwt.return
             { Pricklypear_common.Protocol.ok = false;
               branch = "";
               uptime_seconds = 0;
               error = Some ("parse error: " ^ msg) })
