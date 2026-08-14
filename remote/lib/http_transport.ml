(** remote/lib/http.ml (borge/remote-eval.borg client).

    The production [Proxy_rim.transport]: POST /api/prim to the image
    over a hand-rolled HTTP/1.1 connection on an Eio flow (plain or
    TLS). No curl — the agent's [Provider.post_json] is the reference
    pattern; gluten (Httpun_eio.Client) refuses non-socket TLS flows,
    and a 1.1 request/response with `connection: close` is honest.

    Auth is HTTP Basic (user:pass base64), the SAME path /api/eval and
    /api/prim accept on the image. Identity is server-asserted: the
    client only presents credentials; whoami/current-author come back
    FROM the server. The transport returns (value, exn) Result so it
    slots directly into [Proxy_rim.run] — errors are re-raised by the
    rim via [Prims_pure.error_of_json] so the client sees identical
    exceptions to in-process eval. *)

open Pricklypear_lisp
open Ast

type cfg = {
  base_url : string;   (* e.g. http://127.0.0.1:18080 *)
  user : string;
  password : string;
}

let trim_url base =
  let base = String.trim base in
  if base = "" then "http://127.0.0.1:18080"
  else if base.[String.length base - 1] = '/' then
    String.sub base 0 (String.length base - 1)
  else base

(** Split a http(s)://host[:port] base into (host, port, tls). *)
let parse_base base =
  match String.split_on_char '/' (trim_url base) with
  | "https:" :: "" :: hostport :: _ ->
      let h, p =
        match String.split_on_char ':' hostport with
        | [ h; p ] -> (h, int_of_string p)
        | _ -> (hostport, 443)
      in
      (h, p, true)
  | "http:" :: "" :: hostport :: _ ->
      let h, p =
        match String.split_on_char ':' hostport with
        | [ h; p ] -> (h, int_of_string p)
        | _ -> (hostport, 80)
      in
      (h, p, false)
  | _ -> failwith ("bad base_url: " ^ base)

let basic_auth_header ~user ~password =
  let cred = user ^ ":" ^ password in
  let b64 = Base64.encode_string ~pad:true cred in
  ("authorization", "Basic " ^ b64)

(** Read the response body off [br] honoring transfer-encoding /
    content-length, mirroring agent/lib/provider.ml post_json. *)
let read_body br ~headers =
  let line () = String.trim (Eio.Buf_read.line br) in
  match List.assoc_opt "transfer-encoding" headers with
  | Some te when String.lowercase_ascii te = "chunked" ->
      let buf = Buffer.create 8192 in
      let rec chunks () =
        let size_line = line () in
        match int_of_string_opt ("0x" ^ size_line) with
        | Some 0 -> ignore (line ()) (* trailing CRLF *)
        | Some n ->
            Buffer.add_string buf (Eio.Buf_read.take n br);
            ignore (line ()); (* CRLF after chunk *)
            chunks ()
        | None -> ()
      in
      chunks ();
      Buffer.contents buf
  | _ ->
      (match List.assoc_opt "content-length" headers with
       | Some n -> Eio.Buf_read.take (int_of_string n) br
       | None -> Eio.Buf_read.take_all br)

(** One POST to /api/prim with {name, args:[json…]}. Returns the parsed
    JSON response (or an Error message). *)
let post_prim
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    ~(name : string) ~(args_json : Yojson.Safe.t)
    : (Yojson.Safe.t, string) Result.t =
  let open Eio in
  let host, port, tls = parse_base cfg.base_url in
  let body =
    Yojson.Safe.to_string (`Assoc [ ("name", `String name); ("args", args_json) ])
  in
  let net = (Stdenv.net env :> [ `Generic ] Net.ty Std.r) in
  let addr =
    Net.getaddrinfo_stream ~service:(string_of_int port) net host |> List.hd
  in
  let raw = Net.connect ~sw net addr in
  let flow : Flow.two_way_ty Std.r =
    if tls then
      let auth = Result.get_ok (Ca_certs_nss.authenticator ()) in
      let tls_config = Result.get_ok (Tls.Config.client ~authenticator:auth ()) in
      (Tls_eio.client_of_flow tls_config
         ~host:(Domain_name.host_exn (Domain_name.of_string_exn host))
         raw
       : Tls_eio.t :> Flow.two_way_ty Std.r)
    else (raw :> Flow.two_way_ty Std.r)
  in
  let request_text =
    let hdrs =
      String.concat "\r\n"
        (List.map
           (fun (k, v) -> k ^ ": " ^ v)
           ([ ("host", host); ("content-type", "application/json");
              ("content-length", string_of_int (String.length body));
              ("connection", "close")
            ; basic_auth_header ~user:cfg.user ~password:cfg.password ]))
    in
    Printf.sprintf "POST /api/prim HTTP/1.1\r\n%s\r\n\r\n%s" hdrs body
  in
  Flow.copy_string request_text flow;
  let br = Buf_read.of_flow ~initial_size:4096 ~max_size:(20 * 1024 * 1024) flow in
  let line () = String.trim (Buf_read.line br) in
  let status_line = line () in
  let code =
    match String.split_on_char ' ' status_line with
    | _http :: c :: _ -> (try int_of_string c with _ -> 0)
    | _ -> 0
  in
  let rec read_headers acc =
    let l = line () in
    if l = "" then List.rev acc
    else
      (match String.index_opt l ':' with
       | Some i ->
           read_headers
             ((String.lowercase_ascii (String.sub l 0 i),
               String.trim (String.sub l (i + 1) (String.length l - i - 1))) :: acc)
       | None -> read_headers acc)
  in
  let headers = read_headers [] in
  let body = read_body br ~headers in
  (try Flow.shutdown flow `All with _ -> ());
  if code >= 200 && code < 300 then
    (try Ok (Yojson.Safe.from_string body)
     with Yojson.Json_error e -> Error ("bad json: " ^ e))
  else Error (Printf.sprintf "HTTP %d: %s" code body)

(** Build a [Proxy_rim.transport] from an Eio env + [cfg]. The
    transport is the function the proxy rim calls per Prim_call: it
    serializes args, POSTs, and returns (Ok value | Error exn). A
    server-only refusal ({ok:false, kind:"server_only"}) is surfaced
    as an [Interp_error (Prim_error …)] so it behaves like any other
    prim failure to the client. *)
let transport
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    : Proxy_rim.transport =
  fun (name : string) (args : value list) ->
    let args_json = `List (List.map Prims_pure.value_to_json args) in
    match post_prim ~env ~sw ~cfg ~name ~args_json with
    | Error msg ->
        Error
          (Ast.Interp_error
             (Ast.Prim_error (name, "transport: " ^ msg, "transport")) : exn)
    | Ok j ->
        let open Yojson.Safe.Util in
        match j |> member "ok" with
        | `Bool true ->
            (try Ok (Prims_pure.json_to_value (j |> member "value"))
             with _ -> Error (Ast.Interp_error (Ast.Prim_error (name, "bad value json", "prim_error")) : exn))
        | _ ->
            let kind = j |> member "kind" |> to_string_option |> Option.value ~default:"error" in
            let msg = j |> member "error" |> to_string_option |> Option.value ~default:"prim failed" in
            (* A server-side error_to_json payload carries the structured
               Interp_error/Reader_error; reconstruct it so the client
               raises the SAME exn. Otherwise (server_only / generic)
               synthesize a Prim_error naming the prim. *)
            (match j |> member "kind" with
             | `String k when k = "interp_error" || k = "parse_error" ->
                 (try Error (Prims_pure.error_of_json j) with _ ->
                  Error (Ast.Interp_error (Ast.Prim_error (name, msg, kind)) : exn))
             | _ -> Error (Ast.Interp_error (Ast.Prim_error (name, msg, kind)) : exn))
