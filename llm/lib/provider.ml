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

(* HTTP/1.1 POST via the ONE shared client (httpc/pp_http.ml,
   i-1788671008-e) — this was the hand-rolled Eio-flow original that
   Pp_http was extracted from. Returns the raw response body string on
   2xx (error string otherwise); callers parse it as JSON ([post_json])
   or as SSE event lines ([Openai.complete]).

   [sw] is retained in the signature because this function sits under
   the agent-loop seam (Loop.run passes its turn switch), but the wire
   now runs on Pp_http's OWN internal switch. Caller cancellation still
   unwinds the in-flight request (the request body executes inline on
   the calling fiber, so a cancelled fiber tears the switch — and the
   socket — down). [~timeout_s] is available on Pp_http.request and is
   deliberately NOT defaulted here: streaming completions legitimately
   run for minutes, so the old wait-forever shape is kept (the deadline
   capability + its tests live in httpc/test). URL-split, getaddrinfo
   (List.hd), and TLS setup failures that used to CRASH (failwith /
   empty-list hd / Result.get_ok panics) now degrade to Error strings.
   Non-2xx responses keep the exact ["HTTP %d: %s"] error shape. *)
let request_body ~meth ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : cfg) ~path
    ~extra_headers ~body : (string, string) Result.t =
  ignore sw;
  match Pp_http.split_base cfg.base_url with
  | Error e -> Error e
  | Ok endpoint -> (
      match
        Pp_http.request ~env ~endpoint ~meth ~path ~headers:extra_headers
          ~body ()
      with
      | Error e -> Error e
      | Ok r ->
          if r.Pp_http.status >= 200 && r.Pp_http.status < 300 then
            Ok r.Pp_http.body
          else
            Error (Printf.sprintf "HTTP %d: %s" r.Pp_http.status r.Pp_http.body))

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
     streaming response. The accumulation engine (text/reasoning deltas,
     tool-call argument fragments, synthetic assistant message) lives in
     the shared codec (common/lib/llm_codec.ml) beside
     [parse_turn_response]; this wrapper maps the engine's callbacks
     onto the provider [delta] events and runs the synthetic response
     through [parse_reply] so the XML-fallback / reasoning_content
     logic and [raw] history shape are reused unchanged. *)
  let parse_stream_lines (lines : string list) ~(on_delta : delta -> unit)
      : reply =
    let r =
      Llm_codec.reassemble_stream
        ~on_text:(fun s -> on_delta (Text s))
        ~on_reasoning:(fun s -> on_delta (Reasoning s))
        lines
    in
    let reply = parse_reply r.Llm_codec.response in
    on_delta Done;
    reply

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
          |> List.filter_map Llm_codec.sse_data_payload
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


