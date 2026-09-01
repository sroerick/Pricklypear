(** Provider abstraction (borge/agent.borg agent-loop).

    Wire-shape-neutral message model + per-provider HTTP. v0 provider:
    OpenAI-compatible chat completions (covers synthetic.new and every
    gateway we use, Anthropic included via its compat endpoint). Accepts
    [mock_base_url] so tests run against a local h1 server; no curl anywhere. *)

type message = {
  role : string;             (* "user" | "assistant" | "tool" *)
  content : Yojson.Safe.t;   (* string | null | content-part array; NEVER the
                                whole assistant message object *)
  tool_call_id : string option;  (* OpenAI tool-result routing; None for non-tool msgs *)
  tool_calls : Yojson.Safe.t option;  (* replayed verbatim on assistant history
                                         entries: OpenAI requires an assistant
                                         message's tool_calls when tool-role
                                         messages follow it *)
}

type tool_def = {
  name : string;
  description : string;
  schema : Yojson.Safe.t;    (* JSON schema of parameters *)
}

type tool_call = { id : string; name : string; input : Yojson.Safe.t }

type delta = Text of string | Done | Reasoning of string
(* Reasoning: GLM/DeepSeek-class models stream their chain-of-thought as
   separate `reasoning_content` deltas — except Synthetic's gateway, which
   emits plain `reasoning` deltas (observed on hf:zai-org/GLM-4.7-Flash,
   2026-08 wire probe: 228 `reasoning` chunks, zero `reasoning_content`).
   Both keys are accepted everywhere reasoning is read. The surface renders
   it into the collapsible "thinking + tools" zone instead of the reply
   bubble. *)

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

(* The OpenAI wire codec (types, tool_def_to_json, tool-call parsing,
   XML-in-content fallback, GLM reasoning_content handling) is shared
   with the image agent via common/lib/llm_codec.ml. This module owns
   only the Eio transport + the host-specific [message]/[reply]
   shapes (the standalone agent round-trips raw message JSON in
   history, so [reply.raw] is the assistant message object). *)
module Llm_codec = Pricklypear_common.Llm_codec

(* HTTP/1.1 POST written directly on an Eio flow (plain or TLS).
   Httpun_eio.Client's gluten runtime insists on a socket-typed flow,
   which TLS flows can't satisfy; a 1.1 request/response with
   `connection: close` is simple enough to do honestly by hand.
   Handles content-length and chunked framing. Returns the raw response
   body string on 2xx (error string otherwise); callers parse it as
   JSON ([post_json]) or as SSE event lines ([Openai.complete]). *)
let request_body ~meth ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ~path
    ~extra_headers ~body : (string, string) Result.t =
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
    Printf.sprintf "%s %s HTTP/1.1\r\n%s\r\n\r\n%s" meth path hdrs body
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
  if code >= 200 && code < 300 then Ok body
  else Error (Printf.sprintf "HTTP %d: %s" code body)

let post_body ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ~path
    ~extra_headers ~body : (string, string) Result.t =
  request_body ~meth:"POST" ~env ~sw ~cfg ~path ~extra_headers ~body

let get_body ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ~path
    ~extra_headers : (string, string) Result.t =
  request_body ~meth:"GET" ~env ~sw ~cfg ~path ~extra_headers ~body:""

(* Convenience wrapper: POST and parse the response as JSON. *)
let post_json ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ~path
    ~extra_headers ~body : (Yojson.Safe.t, string) Result.t =
  match post_body ~env ~sw ~cfg ~path ~extra_headers ~body with
  | Error e -> Error e
  | Ok b ->
      (try Ok (Yojson.Safe.from_string b)
       with Yojson.Json_error e -> Error ("bad json: " ^ e))

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
      ~(tools : tool_def list) ~(stream : bool) : string =
    let msgs =
      `Assoc [ ("role", `String "system"); ("content", `String cfg.system) ]
      :: List.map
           (fun m ->
             `Assoc
               (("role", `String m.role) :: ("content", m.content)
                :: (match m.tool_call_id with
                    | Some id -> [ ("tool_call_id", `String id) ]
                    | None -> [])
               @ (match m.tool_calls with
                  | Some tcs -> [ ("tool_calls", tcs) ]
                  | None -> [])))
           messages
    in
    Yojson.Safe.to_string
      (`Assoc
        ([
           ("model", `String cfg.model);
           ("messages", `List msgs);
         ]
         @ (if stream then [ ("stream", `Bool true) ] else [])
         @ (match tools with [] -> [] | ts -> [ ("tools", `List (List.map tool_json ts)) ])))

  let parse_reply (j : Yojson.Safe.t) : reply =
    let open Yojson.Safe.Util in
    let choice = j |> member "choices" |> index 0 in
    let msg = choice |> member "message" in
    let stop_reason =
      choice |> member "finish_reason" |> to_string_option
      |> Option.value ~default:"stop"
    in
    (* Tool calls: shared parser handles the API [tool_calls] field AND
       the XML-in-content fallback (GLM-class models that dump tool
       calls as XML tags inside [content] instead of using the API
       field). The image agent has bled for this; the standalone agent
       inherits it. *)
    let api_tcs = Llm_codec.parse_tool_calls_from_msg msg in
    let tcs, content_for_xml =
      match api_tcs with
      | _ :: _ -> (api_tcs, None)
      | [] ->
          let content_opt =
            match msg |> member "content" |> to_string_option with
            | Some c when String.trim c <> "" -> Some c
            | _ -> None
          in
          (match content_opt with
           | Some c ->
               let xml_tcs = Llm_codec.parse_xml_tool_calls c in
               (xml_tcs, if xml_tcs = [] then None else Some "")
           | None -> ([], None))
    in
    let calls =
      List.map
        (fun (tc : Llm_codec.tool_call) ->
          let input =
            match Yojson.Safe.from_string tc.arguments with
            | json -> json
            | exception _ -> `Assoc []
          in
          { id = tc.id; name = tc.name; input })
        tcs
    in
    let text =
      (* Prefer explicit content; GLM-class models sometimes fill
         reasoning_content and leave content null — fall back to it so a
         pure-text reply is not lost. *)
      let c =
        match content_for_xml with
        | Some s -> s
        | None -> (
            match msg |> member "content" |> to_string_option with
            | Some s -> s
            | None -> "")
      in
      if c <> "" then c
      else
        (match msg |> member "reasoning_content" |> to_string_option with
         | Some s -> s
         | None ->
             (match msg |> member "reasoning" |> to_string_option with
              | Some s -> s
              | None -> ""))
    in
    { text; calls; stop_reason; raw = msg }

  (* Reconstruct a [reply] from the list of SSE [data:] payloads in a
     streaming response. [on_delta (Text s)] is fired per content chunk
     as it is decoded; [Done] is fired once after the stream ends.
     Tool calls arrive as fragments: the first chunk for an [index]
     carries [id] + [function.name], later chunks carry
     [function.arguments] pieces that must be concatenated. We
     accumulate per-index, then build a synthetic assistant [message]
     JSON object and run it through the shared [parse_reply] so the
     XML-fallback / reasoning_content logic is reused unchanged. This
     is a pure function so it can be unit-tested without a socket. *)
  let parse_stream_lines (lines : string list) ~(on_delta : delta -> unit)
      : reply =
    let open Yojson.Safe.Util in
    let text_buf = Buffer.create 128 in
    let reasoning_buf = Buffer.create 128 in
    (* index -> accumulated tool-call fragment {"id";"name";arguments buf} *)
    let tool_bufs : (int, string * string * Buffer.t) Hashtbl.t = Hashtbl.create 4 in
    let finish = ref "stop" in
    List.iter
      (fun data ->
        if data <> "[DONE]" then
          (try
             let json = Yojson.Safe.from_string data in
             match json |> member "choices" |> to_list with
             | choice :: _ ->
                 (match choice |> member "finish_reason" |> to_string_option with
                  | Some f when f <> "" -> finish := f
                  | _ -> ());
                 let delta = choice |> member "delta" in
                 let reasoning_chunk =
                   match delta |> member "reasoning_content" |> to_string_option with
                   | Some s when s <> "" -> Some s
                   | _ ->
                       (* Synthetic OpenAI-compat streams thinking as
                          plain "reasoning" deltas. *)
                       (match delta |> member "reasoning" |> to_string_option with
                        | Some s when s <> "" -> Some s
                        | _ -> None)
                 in
                 (match reasoning_chunk with
                  | Some s ->
                      Buffer.add_string reasoning_buf s;
                      on_delta (Reasoning s)
                  | None -> ());
                 (match delta |> member "content" |> to_string_option with
                  | Some s when s <> "" ->
                      Buffer.add_string text_buf s;
                      on_delta (Text s)
                  | _ -> ());
                 (match delta |> member "tool_calls" with
                  | `List tcs ->
                      List.iter
                        (fun tc ->
                          let idx =
                            match tc |> member "index" with
                            | `Int i -> i
                            | `Intlit s -> (try int_of_string s with _ -> 0)
                            | _ -> 0
                          in
                          let id', name', args =
                            match Hashtbl.find_opt tool_bufs idx with
                            | Some e -> e
                            | None ->
                                let e = ("", "", Buffer.create 64) in
                                Hashtbl.replace tool_bufs idx e;
                                e
                          in
                          let id'' =
                            match tc |> member "id" |> to_string_option with
                            | Some i when i <> "" -> i
                            | _ -> id'
                          in
                          let fn = tc |> member "function" in
                          let name'' =
                            match fn |> member "name" |> to_string_option with
                            | Some n when n <> "" -> n
                            | _ -> name'
                          in
                          (match fn |> member "arguments" |> to_string_option with
                           | Some a when a <> "" -> Buffer.add_string args a
                           | _ -> ());
                          Hashtbl.replace tool_bufs idx (id'', name'', args))
                        tcs
                  | _ -> ())
             | [] -> ()
           with _ -> ()) (* malformed data line -> skip, lenient *))
      lines;
    let text = Buffer.contents text_buf in
    let tool_calls =
      Hashtbl.fold
        (fun _ (id, name, args) acc ->
          if name = "" then acc
          else
            {
              id = (if id = "" then "tool-call" else id);
              name;
              input =
                (match Yojson.Safe.from_string (Buffer.contents args) with
                 | j -> j
                 | exception _ -> `Assoc []);
            }
            :: acc)
        tool_bufs []
    in
    (* Synthetic assistant message, fed back through [parse_reply] so the
       shared XML/GLM fallbacks and [raw] history shape are preserved. *)
    let reasoning = Buffer.contents reasoning_buf in
    let msg =
      `Assoc
        ([ ("role", `String "assistant"); ("content", `String text) ]
        @ (if reasoning = "" then []
           else [ ("reasoning_content", `String reasoning) ])
        @ (if tool_calls = [] then []
           else
             [
               ( "tool_calls",
                 `List
                   (List.map
                      (fun (tc : tool_call) ->
                        `Assoc
                          [
                            ("id", `String tc.id);
                            ("type", `String "function");
                            ( "function",
                              `Assoc
                                [
                                  ("name", `String tc.name);
                                  ( "arguments",
                                    `String (Yojson.Safe.to_string tc.input) );
                                ] );
                          ])
                      tool_calls) );
             ]))
    in
    let response =
      `Assoc
        [
          ( "choices",
            `List
              [
                `Assoc
                  [ ("message", msg); ("finish_reason", `String !finish) ];
              ] );
        ]
    in
    let r = parse_reply response in
    on_delta Done;
    r

  (* Parse one SSE [data:] line from the raw body: returns the payload
     after the "data: " prefix. Empty/comment lines yield None. *)
  let sse_data_payload (line : string) : string option =
    let line = String.trim line in
    if String.length line >= 5 && String.sub line 0 5 = "data:" then
      let rest = String.trim (String.sub line 5 (String.length line - 5)) in
      if rest = "" then None else Some rest
    else None

  let complete ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg)
      ~(messages : message list) ~(tools : tool_def list)
      ~(on_delta : delta -> unit) =
    match
      post_body ~env ~sw ~cfg ~path:"/v1/chat/completions"
        ~extra_headers:[ ("authorization", "Bearer " ^ cfg.api_key) ]
        ~body:(request_body ~cfg ~messages ~tools ~stream:true)
    with
    | Error e -> Error e
    | Ok body ->
        (* v0 streaming: the whole de-chunked body is read, then SSE
           lines are dispatched (parsing-incremental but not
           network-incremental — acceptable v0; matches the image's
           line-loop shape). *)
        let data_lines =
          String.split_on_char '\n' body
          |> List.filter_map sse_data_payload
        in
        Ok (parse_stream_lines data_lines ~on_delta)

  (** GET {base}/models — the provider ABI's own model catalog
      (borge/agent.borg llm-config: the dropdown's source is the ABI,
      NOT replicated user data). Returns model ids from the OpenAI
      "data" array; Error on transport/HTTP failure (the surface then
      renders the current model as the only option). A missing key just
      round-trips the provider's 401 as an Error string. *)
  let list_models ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ()
      : (string list, string) Result.t =
    match
      get_body ~env ~sw ~cfg ~path:"/v1/models"
        ~extra_headers:[ ("authorization", "Bearer " ^ cfg.api_key) ]
    with
    | Error e -> Error e
    | Ok body ->
        (try
           match Yojson.Safe.from_string body with
           | `Assoc kv ->
               (match List.assoc_opt "data" kv with
                | Some (`List ms) ->
                    Ok
                      (List.filter_map
                         (function
                          | `Assoc m ->
                              (match List.assoc_opt "id" m with
                               | Some (`String s) when s <> "" -> Some s
                               | _ -> None)
                          | _ -> None)
                         ms)
                | _ -> Error "no models array in /models response")
           | _ -> Error "bad /models response shape"
         with exn -> Error (Printexc.to_string exn))
end


