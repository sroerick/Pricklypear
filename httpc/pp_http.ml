(* Pp_http — the ONE HTTP/1.1 client for the hand-rolled transports
   (issue i-1788671008-e).

   Httpun_eio.Client's gluten runtime insists on a socket-typed flow,
   which TLS flows can't satisfy; a 1.1 request/response with
   [connection: close] is simple enough to do honestly by hand. This
   module owns that wire exactly once:

   - URL split ([split_base]) — was copy-pasted in provider.ml,
     http_transport.ml, soil_strip.ml, and cli pp_client.eval_url;
   - connect + TLS with NON-CRASHY errors — the predecessors panicked
     via [Result.get_ok] on TLS config/auth and [List.hd] on
     getaddrinfo (transient DNS failure = crash);
   - request/header parse + chunked / content-length / read-to-EOF
     framing;
   - an OPTIONAL deadline: the whole connect-read runs on an internal
     switch raced against the clock, so a wedged peer (the exact
     failure mode the http_ui write-watchdog was hand-built for)
     returns [Error "timeout: …"] and releases the socket instead of
     parking the calling fiber forever. [Buf_read.max_size] caps size,
     not time — the deadline closes that gap.

   POLICY STAYS AT THE CALLERS: 2xx-vs-error mapping, JSON decoding,
   and the default timeout (pass [~timeout_s] to get one; [None] keeps
   the old wait-forever shape). The response is returned typed for ANY
   status so callers can render their own error strings byte-identically.

   Each request runs on its OWN internal switch (not the caller's) so
   deadline cancellation actually closes the socket — the established
   pattern of llm/lib/tools.ml tool_bash. *)

type endpoint = { host : string; port : int; tls : bool }
(** The scheme://host[:port] part of a base URL. Extra path segments in
    the base are IGNORED — v0 behavior of both predecessors, and
    load-bearing: chat/model paths are appended by the caller, so a
    base URL carrying its own path (e.g. https://api.synthetic.new/openai/v1)
    must not double them. IPv6 literals keep the old (broken) parse
    shape: whole hostport as host + default port. *)

let default_max_body = 20 * 1024 * 1024
(** The [Buf_read.max_size] both predecessor readers used. *)

type response = {
  status : int;                      (* 0 = unparseable status line *)
  headers : (string * string) list;  (* names lowercased, values trimmed *)
  body : string;
}

(* ---- URL split (was provider.ml / http_transport.ml parse_base) ---- *)

let default_port tls = if tls then 443 else 80

let split_base (base : string) : (endpoint, string) Result.t =
  let bad () =
    Error (Printf.sprintf "bad base_url: %s (expected http(s)://host[:port])" base)
  in
  match String.split_on_char '/' base with
  | "https:" :: "" :: hostport :: _ when hostport <> "" -> (
      match String.split_on_char ':' hostport with
      | [ h ] -> Ok { host = h; port = default_port true; tls = true }
      | [ h; p ] -> (
          match int_of_string_opt p with
          | Some port -> Ok { host = h; port; tls = true }
          | None -> bad ())
      (* multi-colon (IPv6 literal &c.): old parsers kept the whole
         hostport as host with the default port *)
      | _ -> Ok { host = hostport; port = default_port true; tls = true })
  | "http:" :: "" :: hostport :: _ when hostport <> "" -> (
      match String.split_on_char ':' hostport with
      | [ h ] -> Ok { host = h; port = default_port false; tls = false }
      | [ h; p ] -> (
          match int_of_string_opt p with
          | Some port -> Ok { host = h; port; tls = false }
          | None -> bad ())
      | _ -> Ok { host = hostport; port = default_port false; tls = false })
  | _ -> bad ()

(* ---- head/body parse (factored over first-class readers so the
   framing logic is unit-testable without a socket) ---- *)

let parse_status_line (line : string) : int =
  match String.split_on_char ' ' line with
  | _http :: c :: _ -> (match int_of_string_opt c with Some n -> n | None -> 0)
  | _ -> 0

let parse_header_line (line : string) : (string * string) option =
  match String.index_opt line ':' with
  | Some i ->
      Some
        ( String.lowercase_ascii (String.sub line 0 i)
        , String.trim (String.sub line (i + 1) (String.length line - i - 1)) )
  | None -> None

let parse_headers (lines : string list) : (string * string) list =
  List.filter_map parse_header_line lines

(* Read header lines until the blank line (malformed lines skipped —
   the old readers' behavior). *)
let rec read_header_lines ~(line : unit -> string) acc =
  let l = line () in
  if l = "" then List.rev acc
  else
    (match parse_header_line l with
     | Some h -> read_header_lines ~line (h :: acc)
     | None -> read_header_lines ~line acc)

(* Body framing: chunked (tolerating ";ext" chunk extensions) /
   content-length / read-to-EOF. The old readers crashed on a garbage
   content-length ([int_of_string] on non-number); here garbage
   degrades to read-to-EOF. An unparseable chunk-size line ended the
   body there in the old readers too. *)
let read_response_body ~(line : unit -> string) ~(take : int -> string)
    ~(take_all : unit -> string) ~(headers : (string * string) list) : string =
  match List.assoc_opt "transfer-encoding" headers with
  | Some te when String.lowercase_ascii te = "chunked" ->
      let buf = Buffer.create 8192 in
      let chunk_size size_line =
        let hex =
          match String.index_opt size_line ';' with
          | Some i -> String.sub size_line 0 i
          | None -> size_line
        in
        int_of_string_opt ("0x" ^ String.trim hex)
      in
      let rec chunks () =
        match chunk_size (line ()) with
        | Some 0 -> ignore (line ())
          (* trailing CRLF after the last chunk *)
        | Some n ->
            Buffer.add_string buf (take n);
            ignore (line ());
            (* CRLF after each chunk *)
            chunks ()
        | None -> ()
      in
      chunks ();
      Buffer.contents buf
  | _ -> (
      match List.assoc_opt "content-length" headers with
      | Some n -> (
          match int_of_string_opt (String.trim n) with
          | Some len when len >= 0 -> take len
          | _ -> take_all ())
      | None -> take_all ())

(* ---- connection ---- *)

(* Non-crashy getaddrinfo: empty result / resolver failure are Errors,
   never [List.hd] on an empty list. *)
let pick_addr ~(net : [ `Generic ] Eio.Net.ty Eio.Std.r) ~(endpoint : endpoint)
    : (Eio.Net.Sockaddr.stream, string) Result.t =
  match
    Eio.Net.getaddrinfo_stream ~service:(string_of_int endpoint.port) net
      endpoint.host
  with
  | [] -> Error (Printf.sprintf "dns: no addresses for %s" endpoint.host)
  | addr :: _ -> Ok addr
  | exception exn ->
      Error
        (Printf.sprintf "dns lookup failed for %s: %s" endpoint.host
           (Printexc.to_string exn))

(* TLS setup without [Result.get_ok] panics: trust anchors, config, and
   the handshake itself all degrade to Error. [raw] is NOT closed here
   on failure — the caller owns cleanup. *)
let tls_flow ~(host : string) raw
    : (Eio.Flow.two_way_ty Eio.Std.r, string) Result.t =
  match Ca_certs_nss.authenticator () with
  | Error (`Msg m) -> Error ("tls: trust anchors unavailable: " ^ m)
  | Ok auth -> (
      match Tls.Config.client ~authenticator:auth () with
      | Error (`Msg m) -> Error ("tls: config: " ^ m)
      | Ok tls_config -> (
          try
            let host = Domain_name.host_exn (Domain_name.of_string_exn host) in
            Ok
              ((Tls_eio.client_of_flow tls_config ~host raw
                 : Tls_eio.t :> Eio.Flow.two_way_ty Eio.Std.r))
          with exn ->
            Error
              (Printf.sprintf "tls handshake with %s failed: %s" host
                 (Printexc.to_string exn))))

(* One full request on an internal switch. Raises only from read/write
   paths (Buf_read/TLS), which the deadline-aware wrapper turns into
   Error strings. *)
let run_once ~(env : Eio_unix.Stdenv.base) ~(endpoint : endpoint)
    ~(meth : string) ~(path : string) ~(content_type : string option)
    ~(headers : (string * string) list) ~(body : string) ~(max_body : int) :
    (response, string) Result.t =
  Eio.Switch.run (fun sw ->
      let open Eio in
      let net = (Stdenv.net env :> [ `Generic ] Net.ty Std.r) in
      match pick_addr ~net ~endpoint with
      | Error _ as e -> e
      | Ok addr -> (
          try
            let raw = Net.connect ~sw net addr in
            match
              if endpoint.tls then tls_flow ~host:endpoint.host raw
              else Ok ((raw :> Flow.two_way_ty Std.r))
            with
            | Error e ->
                (try Flow.shutdown raw `All with _ -> ());
                Error e
            | Ok flow ->
                let hdrs =
                  String.concat "\r\n"
                    (List.map (fun (k, v) -> k ^ ": " ^ v)
                       ([ ("host", endpoint.host) ]
                       @ (match content_type with
                          | Some ct -> [ ("content-type", ct) ]
                          | None -> [])
                       @ [ ( "content-length"
                           , string_of_int (String.length body) )
                         ; ("connection", "close") ]
                       @ headers))
                in
                let request_text =
                  Printf.sprintf "%s %s HTTP/1.1\r\n%s\r\n\r\n%s" meth path
                    hdrs body
                in
                Flow.copy_string request_text flow;
                let br =
                  Buf_read.of_flow ~initial_size:4096 ~max_size:max_body flow
                in
                let line () = String.trim (Buf_read.line br) in
                let status = parse_status_line (line ()) in
                let hdrs = read_header_lines ~line [] in
                let resp_body =
                  read_response_body ~line
                    ~take:(fun n -> Buf_read.take n br)
                    ~take_all:(fun () -> Buf_read.take_all br)
                    ~headers:hdrs
                in
                let resp = { status; headers = hdrs; body = resp_body } in
                (try Flow.shutdown flow `All with _ -> ());
                Ok resp
          with exn ->
            (* NEVER swallow cancellation (deadline / switch teardown):
               a fiber that catches Cancelled and returns normally wedges
               its canceller (observed: with_timeout never completing). *)
            (match exn with
             | Eio.Cancel.Cancelled _ as c -> raise c
             | _ ->
                 Error
                   (Printf.sprintf "%s: %s" (Printexc.exn_slot_name exn)
                      (Printexc.to_string exn)))))

(* The public entry: [?timeout_s] None = wait forever (old shape);
   Some t = the whole connect+read races the clock, and on deadline the
   internal switch is cancelled, closing the socket. *)
let request ?timeout_s ?(max_body = default_max_body)
    ~(env : Eio_unix.Stdenv.base) ~(endpoint : endpoint) ~(meth : string)
    ~(path : string) ?(content_type : string option = Some "application/json")
    ~(headers : (string * string) list) ~(body : string) () :
    (response, string) Result.t =
  match timeout_s with
  | None -> run_once ~env ~endpoint ~meth ~path ~content_type ~headers ~body ~max_body

  | Some t -> (
      (* eio 1.3's with_timeout wants the body to return a result whose
         error slot unifies with `Timeout; wrap and flatten. *)
      match
        Eio.Time.with_timeout (Eio.Stdenv.clock env) t (fun () ->
            Ok
              (run_once ~env ~endpoint ~meth ~path ~content_type ~headers
                 ~body ~max_body))
      with
      | Ok (Ok r) -> Ok r
      | Ok (Error e) -> Error e
      | Error `Timeout ->
          Error (Printf.sprintf "timeout: no complete response within %gs" t))
