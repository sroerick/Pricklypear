(* Tests for Pp_http — the ONE HTTP/1.1 client (i-1788671008-e).

   Two layers:
   - pure: split_base / status / header / body-framing parse over
     first-class readers (the exact functions the wire path calls);
   - wire: real loopback sockets (in-process listener) for round-trips,
     the DEADLINE (silent server + slow-body wedge), non-crashy TLS
     against a plain server, and connection-refused-as-Error. *)

open Eio.Std

let show_endpoint (e : Pp_http.endpoint) =
  Printf.sprintf "{host=%s;port=%d;tls=%b}" e.host e.port e.tls

let show_endpoint_result = function
  | Ok e -> "Ok " ^ show_endpoint e
  | Error e -> "Error " ^ e

let check_endpoint name expected actual =
  Alcotest.(check string) name (show_endpoint_result expected)
    (show_endpoint_result actual)

let ok_endpoint host port tls = Ok { Pp_http.host; port; tls }

let check_endpoint_err name prefix = function
  | Error e ->
      Alcotest.(check bool) name
        (String.length e >= String.length prefix
         && String.sub e 0 (String.length prefix) = prefix)
        true
  | Ok e -> Alcotest.failf "expected Error %S, got Ok %s" prefix (show_endpoint e)

(* ---- pure parse cases ---- *)

let test_split_base () =
  check_endpoint "plain http with port"
    (ok_endpoint "127.0.0.1" 18080 false)
    (Pp_http.split_base "http://127.0.0.1:18080");
  check_endpoint "https default port" (ok_endpoint "api.synthetic.new" 443 true)
    (Pp_http.split_base "https://api.synthetic.new");
  (* extra path segments ignored — v0 behavior of both predecessors
     (callers append their own /v1/... paths) *)
  check_endpoint "base with path drops it"
    (ok_endpoint "api.synthetic.new" 443 true)
    (Pp_http.split_base "https://api.synthetic.new/openai/v1");
  check_endpoint "trailing slash" (ok_endpoint "host" 80 false)
    (Pp_http.split_base "http://host/");
  check_endpoint_err "bad port is an Error (was a crash)" "bad base_url"
    (Pp_http.split_base "http://host:abc");
  check_endpoint_err "wrong scheme" "bad base_url" (Pp_http.split_base "ftp://host");
  check_endpoint_err "empty" "bad base_url" (Pp_http.split_base "");
  check_endpoint_err "scheme only" "bad base_url" (Pp_http.split_base "http://")

let test_status_line () =
  Alcotest.(check int) "200" 200 (Pp_http.parse_status_line "HTTP/1.1 200 OK");
  Alcotest.(check int) "404" 404 (Pp_http.parse_status_line "HTTP/1.0 404 Not Found");
  Alcotest.(check int) "garbage" 0 (Pp_http.parse_status_line "garbage");
  Alcotest.(check int) "non-numeric code" 0 (Pp_http.parse_status_line "HTTP/1.1 abc")

let test_parse_headers () =
  Alcotest.(check (list (pair string string)))
    "lowercase + trim + skip malformed"
    [ ("host", "x"); ("transfer-encoding", "Chunked"); ("empty", "") ]
    (Pp_http.parse_headers [ "Host: x"; "badline"; "TRANSFER-ENCODING:  Chunked "; "Empty:" ])

(* Fake byte-stream readers with the same shapes the wire path binds:
   line = next \r\n-terminated trimmed segment, take = next n bytes,
   take_all = rest. *)
let readers_of s =
  let pos = ref 0 in
  let line () =
    if !pos >= String.length s then ""
    else
      let nl =
        try String.index_from s !pos '\n' with Not_found -> String.length s
      in
      let l = String.sub s !pos (nl - !pos) in
      pos := min (nl + 1) (String.length s);
      String.trim l
  in
  let take n =
    let r = String.sub s !pos n in
    pos := !pos + n;
    r
  in
  let take_all () =
    let r = String.sub s !pos (String.length s - !pos) in
    pos := String.length s;
    r
  in
  (line, take, take_all)

let read_body ?(headers = []) stream =
  let line, take, take_all = readers_of stream in
  Pp_http.read_response_body ~line ~take ~take_all ~headers

let test_body_framing () =
  Alcotest.(check string) "chunked" "Wikipedia"
    (read_body "4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n" ~headers:[ ("transfer-encoding", "chunked") ]);
  Alcotest.(check string) "chunked with extension" "0123456789"
    (read_body "A;foo=bar\r\n0123456789\r\n0\r\n\r\n" ~headers:[ ("transfer-encoding", "chunked") ]);
  Alcotest.(check string) "content-length" "hello world"
    (read_body "hello world and more" ~headers:[ ("content-length", "11") ]);
  Alcotest.(check string) "no length reads to EOF" "hello world"
    (read_body "hello world");
  (* garbage content-length degrades to read-to-EOF (was an
     int_of_string crash) *)
  Alcotest.(check string) "garbage content-length" "hello"
    (read_body "hello" ~headers:[ ("content-length", "not-a-number") ])

(* ---- wire cases (loopback) ---- *)

let env : Eio_unix.Stdenv.base ref = ref (Obj.magic 0)

let port_of socket =
  match Eio.Net.listening_addr socket with
  | `Tcp (_, p) -> p
  | _ -> failwith "listener not tcp"

(* Fork a one-shot loopback listener as a background fiber. eio's Switch.run
   success path never cancels leftover fibers — it AWAITS them — so a
   plain [Fiber.fork] server keeps the switch open forever whenever the
   handler blocks (the silent/slow-body deadline cases hung exactly
   there, AFTER the client asserts had passed). [Fiber.fork_daemon] is
   eio's own prescribed pattern for test servers (README "run_server") —
   the eio API name is fixed; think "background": the fiber is cancelled
   once the test body is done. One-shot like
   [Eio.Net.accept_fork] (which also serves a single connection and
   returns): each wire test below makes exactly one request. *)
let serve ~sw ~handler =
  let net = Eio.Stdenv.net !env in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:false ~backlog:8 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = port_of socket in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      match Eio.Net.accept ~sw socket with
      | flow, _addr ->
          (try handler flow with _ -> ());
          (try Eio.Flow.close flow with _ -> ());
          `Stop_daemon
      | exception _ex -> `Stop_daemon (* cancelled at switch end (or listener error) *));
  port

let write_response flow resp = Eio.Flow.copy_string resp flow

let request ?timeout_s ~port ~tls ?(meth = "POST") ?(path = "/x")
    ?(headers = []) ?(body = "") () =
  Pp_http.request ?timeout_s
    ~env:!env
    ~endpoint:{ Pp_http.host = "127.0.0.1"; port; tls }
    ~meth ~path ~headers ~body ()

let test_roundtrip_content_length () =
  Eio.Switch.run (fun sw ->
      let port =
        serve ~sw ~handler:(fun flow ->
            write_response flow
              "HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: 13\r\n\r\nhello, world\n")
      in
      match request ~port ~tls:false ~body:"req-body" () with
      | Error e -> Alcotest.failf "expected Ok, got Error %s" e
      | Ok r ->
          Alcotest.(check int) "status" 201 r.Pp_http.status;
          Alcotest.(check string) "body" "hello, world\n" r.Pp_http.body;
          Alcotest.(check (list (pair string string))) "content-type header"
            [ ("content-type", "application/json") ]
            (List.filter (fun (k, _) -> k = "content-type") r.Pp_http.headers))

let test_roundtrip_chunked () =
  Eio.Switch.run (fun sw ->
      let port =
        serve ~sw ~handler:(fun flow ->
            write_response flow
              "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n")
      in
      match request ~port ~tls:false () with
      | Error e -> Alcotest.failf "expected Ok, got Error %s" e
      | Ok r ->
          Alcotest.(check int) "status" 200 r.Pp_http.status;
          Alcotest.(check string) "de-chunked body" "Wikipedia" r.Pp_http.body)

let test_status_passthrough () =
  (* Pp_http returns ANY status typed; 2xx-vs-error policy is the
     caller's (provider/transport keep their exact error strings). *)
  Eio.Switch.run (fun sw ->
      let port =
        serve ~sw ~handler:(fun flow ->
            write_response flow
              "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 5\r\n\r\nnope\n")
      in
      match request ~port ~tls:false () with
      | Error e -> Alcotest.failf "expected typed Ok(500), got Error %s" e
      | Ok r ->
          Alcotest.(check int) "status" 500 r.Pp_http.status;
          Alcotest.(check string) "error body" "nope\n" r.Pp_http.body)

let test_deadline_silent_server () =
  Eio.Switch.run (fun sw ->
      let port =
        serve ~sw ~handler:(fun _flow ->
            (* accept and never respond — the wedged-provider case *)
            let _ = Fiber.await_cancel () in
            ())
      in
      let t0 = Eio.Time.now (Eio.Stdenv.clock !env) in
      (match request ~timeout_s:0.3 ~port ~tls:false () with
       | Error e ->
           Alcotest.(check bool) "timeout error message"
             (String.length e >= 7 && String.sub e 0 7 = "timeout")
             true
       | Ok r -> Alcotest.failf "expected deadline Error, got Ok %d" r.Pp_http.status);
      let dt = Eio.Time.now (Eio.Stdenv.clock !env) -. t0 in
      Alcotest.(check bool) "deadline actually fired (well under a hang)"
        (dt < 10.0) true)

let test_deadline_slow_body () =
  Eio.Switch.run (fun sw ->
      let port =
        serve ~sw ~handler:(fun flow ->
            write_response flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\nshort";
            (* headers + 5 bytes, then silence: Buf_read.take blocks
               forever without the deadline — the exact size-not-time
               gap this client closes *)
            let _ = Fiber.await_cancel () in
            ())
      in
      match request ~timeout_s:0.3 ~port ~tls:false () with
      | Error e ->
          Alcotest.(check bool) "slow-body wedge hits the deadline"
            (String.length e >= 7 && String.sub e 0 7 = "timeout")
            true
      | Ok r -> Alcotest.failf "expected deadline Error, got Ok %d" r.Pp_http.status)

let test_tls_against_plain_server () =
  (* https to a plain listener: the handshake fails and degrades to an
     Error string — the old Result.get_ok path would have panicked. *)
  Eio.Switch.run (fun sw ->
      let port =
        serve ~sw ~handler:(fun flow ->
            write_response flow "garbage not a tls response";
            (try Eio.Flow.close flow with _ -> ()))
      in
      match request ~timeout_s:2.0 ~port ~tls:true () with
      | Error _e -> ()
      | Ok r -> Alcotest.failf "expected tls failure Error, got Ok %d" r.Pp_http.status)

let test_connection_refused () =
  (* port 1 on loopback is closed: Connection_failure degrades to an
     Error string (the old code let the exn fly out of the provider). *)
  match request ~timeout_s:2.0 ~port:1 ~tls:false () with
  | Error _e -> ()
  | Ok r -> Alcotest.failf "expected connect Error, got Ok %d" r.Pp_http.status

let () =
  Eio_main.run (fun e ->
      env := e;
      let open Eio in
      let run name fn = Alcotest.test_case name `Quick fn in
      Alcotest.run "pp_http"
        [
          ( "pure",
            [
              run "split-base" test_split_base;
              run "status-line" test_status_line;
              run "parse-headers" test_parse_headers;
              run "body-framing" test_body_framing;
            ] );
          ( "wire",
            [
              run "roundtrip-content-length" test_roundtrip_content_length;
              run "roundtrip-chunked" test_roundtrip_chunked;
              run "status-passthrough" test_status_passthrough;
              run "deadline-silent-server" test_deadline_silent_server;
              run "deadline-slow-body" test_deadline_slow_body;
              run "tls-against-plain-server" test_tls_against_plain_server;
              run "connection-refused" test_connection_refused;
            ] );
        ])
