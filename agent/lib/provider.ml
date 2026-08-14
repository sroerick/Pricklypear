(** Provider abstraction (borge/agent.borg agent-loop).

    Wire-shape-neutral message model + per-provider HTTP. v0 provider:
    OpenAI-compatible chat completions (covers synthetic.new and every
    gateway we use, Anthropic included via its compat endpoint). Accepts
    [mock_base_url] so tests run against a local h1 server; no curl anywhere. *)

type message = {
  role : string;             (* "user" | "assistant" | "tool" *)
  content : Yojson.Safe.t;   (* provider-shaped; pass through *)
  tool_call_id : string option;  (* OpenAI tool-result routing; None for non-tool msgs *)
}

type tool_def = {
  name : string;
  description : string;
  schema : Yojson.Safe.t;    (* JSON schema of parameters *)
}

type tool_call = { id : string; name : string; input : Yojson.Safe.t }

type delta = Text of string | Done

type reply = {
  text : string;
  calls : tool_call list;
  stop_reason : string;
  raw : Yojson.Safe.t;       (* assistant message in provider shape,
                                for appending to history verbatim *)
}

type cfg = {
  base_url : string;
  api_key : string;
  model : string;
  system : string;
}

(* HTTP/1.1 POST written directly on an Eio flow (plain or TLS).
   Httpun_eio.Client's gluten runtime insists on a socket-typed flow,
   which TLS flows can't satisfy; a 1.1 request/response with
   `connection: close` is simple enough to do honestly by hand.
   Handles content-length and chunked framing. Collects the full body;
   SSE streaming lands once the loop shape settles — providers already
   page replies through [on_delta] so the API is streaming-ready. *)
let post_json ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ~path
    ~extra_headers ~body : (Yojson.Safe.t, string) Result.t =
  let open Eio in
  let host, port, tls =
    match String.split_on_char '/' cfg.base_url with
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
    | _ -> failwith ("bad base_url: " ^ cfg.base_url)
  in
  let net = (Stdenv.net env :> [ `Generic ] Net.ty Std.r) in
  let addr = Net.getaddrinfo_stream ~service:(string_of_int port) net host |> List.hd in
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
        (List.map (fun (k, v) -> k ^ ": " ^ v)
           ([ ("host", host); ("content-type", "application/json");
              ("content-length", string_of_int (String.length body));
              ("connection", "close") ]
            @ extra_headers))
    in
    Printf.sprintf "POST %s HTTP/1.1\r\n%s\r\n\r\n%s" path hdrs body
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
  let body =
    match List.assoc_opt "transfer-encoding" headers with
    | Some te when String.lowercase_ascii te = "chunked" ->
        let buf = Buffer.create 8192 in
        let rec chunks () =
          let size_line = line () in
          match int_of_string_opt ("0x" ^ size_line) with
          | Some 0 -> ignore (line ()) (* trailing CRLF *)
          | Some n ->
              Buffer.add_string buf (Buf_read.take n br);
              ignore (line ()); (* CRLF after chunk *)
              chunks ()
          | None -> ()
        in
        chunks ();
        Buffer.contents buf
    | _ ->
        (match List.assoc_opt "content-length" headers with
         | Some n -> Buf_read.take (int_of_string n) br
         | None -> Buf_read.take_all br)
  in
  (try Flow.shutdown flow `All with _ -> ());
  if code >= 200 && code < 300 then
    (try Ok (Yojson.Safe.from_string body)
     with Yojson.Json_error e -> Error ("bad json: " ^ e))
  else Error (Printf.sprintf "HTTP %d: %s" code body)

(* ---- OpenAI-compatible (chat completions) ---- *)

module Openai = struct
  let tool_json (t : tool_def) : Yojson.Safe.t =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String t.name);
              ("description", `String t.description);
              ("parameters", t.schema);
            ] );
      ]

  let request_body ~(cfg : cfg) ~(messages : message list)
      ~(tools : tool_def list) : string =
    let msgs =
      `Assoc [ ("role", `String "system"); ("content", `String cfg.system) ]
      :: List.map
           (fun m ->
             `Assoc
               (("role", `String m.role) :: ("content", m.content)
                :: (match m.tool_call_id with
                    | Some id -> [ ("tool_call_id", `String id) ]
                    | None -> [])))
           messages
    in
    Yojson.Safe.to_string
      (`Assoc
        ([
           ("model", `String cfg.model);
           ("messages", `List msgs);
         ]
         @ (match tools with [] -> [] | ts -> [ ("tools", `List (List.map tool_json ts)) ])))

  let parse_reply (j : Yojson.Safe.t) : reply =
    let open Yojson.Safe.Util in
    let choice = j |> member "choices" |> index 0 in
    let msg = choice |> member "message" in
    let text =
      match msg |> member "content" with
      | `String s -> s
      | `Null -> ""
      | other -> Yojson.Safe.to_string other
    in
    let calls =
      match msg |> member "tool_calls" with
      | `List tcs ->
          List.map
            (fun tc ->
              let fn = tc |> member "function" in
              {
                id = tc |> member "id" |> to_string;
                name = fn |> member "name" |> to_string;
                input =
                  (match fn |> member "arguments" with
                   | `String s -> (
                       try Yojson.Safe.from_string s with _ -> `Assoc [])
                   | other -> other);
              })
            tcs
      | _ -> []
    in
    { text; calls; stop_reason = choice |> member "finish_reason" |> to_string_option |> Option.value ~default:"stop"
    ; raw = msg }

  let complete ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg)
      ~(messages : message list) ~(tools : tool_def list)
      ~(on_delta : delta -> unit) =
    match
      post_json ~env ~sw ~cfg ~path:"/v1/chat/completions"
        ~extra_headers:[ ("authorization", "Bearer " ^ cfg.api_key) ]
        ~body:(request_body ~cfg ~messages ~tools)
    with
    | Error e -> Error e
    | Ok j ->
        let r = parse_reply j in
        on_delta (Text r.text);
        Ok r
end


