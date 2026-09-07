(** Pricklypear Common — OpenAI chat-completions wire codec.

    Pure yojson: the types and encoders/decoders for the OpenAI-compatible
    chat completions API shape shared by the image agent
    ([image/lib/llm_client.ml]) and the standalone agent
    ([agent/lib/provider.ml]). No transport, no Lwt, no Eio, no curl —
    each host owns its wire (curl vs hand-rolled Eio HTTP); this module
    owns the value<->json boundary.

    Spec gate ([borge/agent.borg] / [borge/remote-eval.borg]): a shared,
    tested value<->json codec. The robustness the image has bled for —
    GLM-class [reasoning_content] selection and XML-in-content tool-call
    fallback — lives here so the standalone agent inherits it. *)

(** A tool call as it appears on the wire: [arguments] is a JSON object
    string (OpenAI encodes function arguments as a stringified object). *)
type tool_call = {
  id : string;
  name : string;
  arguments : string;  (* JSON object string *)
}

(** A tool definition for the [tools] request field. [parameters] is a
    JSON Schema object. *)
type tool_def = {
  name : string;
  description : string;
  parameters : Yojson.Safe.t;  (* JSON Schema object *)
}

(** One chat-turn outcome. [raw] carries the assistant [message] object
    verbatim so hosts that round-trip history (the standalone agent
    appends the raw message back) can do so without re-encoding. Hosts
    that store typed history (the image) ignore it. *)
type turn_result =
  | Final of { content : string; raw : Yojson.Safe.t }
  | Need_tools of {
      content : string option;
      tool_calls : tool_call list;
      raw : Yojson.Safe.t;
    }
  | Turn_error of string

(** OpenAI-compatible error bodies carry [{"error":{"message":…,
    "type":…,"code":…}}] with NO choices (4xx/5xx + some gateway 200s).
    Surface the provider's own message instead of a bare
    [Type_error("Expected array, got null")] — the undiagnosable string
    the wake loop wedged on. Pure + total: missing fields degrade to
    the raw json excerpt. *)
let provider_error_message (json : Yojson.Safe.t) : string option =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Null | `String "" -> None
  | `String s when s = "" -> None
  | `String s -> Some s
  | err ->
      let msg =
        match err |> member "message" |> to_string_option with
        | Some m when String.trim m <> "" -> m
        | _ ->
            (match err with
             | `String s when s <> "" -> s
             | _ -> Yojson.Safe.to_string err)
      in
      let tag =
        match err |> member "type" |> to_string_option with
        | Some t -> t
        | None ->
            (match err |> member "code" with
             | `String c -> c
             | `Int i -> string_of_int i
             | _ -> "")
      in
      Some
        (if tag = "" then msg else Printf.sprintf "%s (%s)" msg tag)

let string_field (msg : Yojson.Safe.t) (key : string) : string =
  match Yojson.Safe.Util.(msg |> member key |> to_string_option) with
  | Some s -> s
  | None -> ""

(** True when [s] looks like a complete JSON value (object/array/string). *)
let looks_complete_json (s : string) : bool =
  let s = String.trim s in
  if s = "" then false
  else
    (try
       let _ = Yojson.Safe.from_string s in
       true
     with _ -> false)

let find_substring ~(hay : string) ~(needle : string) ~(from : int) : int option =
  let nlen = String.length needle in
  let hlen = String.length hay in
  let rec go i =
    if i + nlen > hlen then None
    else if String.sub hay i nlen = needle then Some i
    else go (i + 1)
  in
  go from

let tool_def_to_json (t : tool_def) : Yojson.Safe.t =
  `Assoc
    [ ("type", `String "function")
    ; ("function",
       `Assoc
         [ ("name", `String t.name)
         ; ("description", `String t.description)
         ; ("parameters", t.parameters)
         ])
    ]

(* XML-in-content tool-call fallback delimiters. Built by concatenation
   so the literal tags never appear as a tool-call boundary in this
   source. GLM-class models emit these tags inside [content] instead of
   using the API [tool_calls] field; the format is a tool_call block
   wrapping arg_key/arg_value pairs naming the function and its JSON
   arguments. *)
let xml_open_tag      = "<" ^ "tool_call>"
let xml_close_tag     = "<" ^ "/tool_call>"
let xml_arg_key_open  = "<" ^ "arg_key>"
let xml_arg_key_close = "<" ^ "/arg_key>"
let xml_arg_val_open  = "<" ^ "arg_value>"
let xml_arg_val_close = "<" ^ "/arg_value>"

(** Recover when models dump tools as XML in content instead of API
    tool_calls. Deliberately lenient: it finds the head token before the
    first arg tag and reads key/value pairs by tag bracketing. *)
let parse_xml_tool_calls (text : string) : tool_call list =
  let rec find_blocks acc from =
    match find_substring ~hay:text ~needle:xml_open_tag ~from with
    | None -> List.rev acc
    | Some start ->
        let body_start = start + String.length xml_open_tag in
        (match find_substring ~hay:text ~needle:xml_close_tag ~from:body_start with
         | None -> List.rev acc
         | Some stop ->
             let block = String.sub text body_start (stop - body_start) in
             find_blocks (block :: acc) (stop + String.length xml_close_tag))
  in
  let parse_args block =
    let rec collect acc from =
      match find_substring ~hay:block ~needle:xml_arg_key_open ~from with
      | None -> List.rev acc
      | Some k0 ->
          let k1 = k0 + String.length xml_arg_key_open in
          (match find_substring ~hay:block ~needle:xml_arg_key_close ~from:k1 with
           | None -> List.rev acc
           | Some k2 ->
               let key = String.trim (String.sub block k1 (k2 - k1)) in
               let after_key = k2 + String.length xml_arg_key_close in
               (match find_substring ~hay:block ~needle:xml_arg_val_open ~from:after_key with
                | None -> List.rev acc
                | Some v0 ->
                    let v1 = v0 + String.length xml_arg_val_open in
                    (match find_substring ~hay:block ~needle:xml_arg_val_close ~from:v1 with
                     | None -> List.rev acc
                     | Some v2 ->
                         let value = String.sub block v1 (v2 - v1) in
                         collect ((key, value) :: acc)
                           (v2 + String.length xml_arg_val_close))))
    in
    collect [] 0
  in
  let parse_one idx block =
    let block = String.trim block in
    let name =
      match find_substring ~hay:block ~needle:xml_arg_key_open ~from:0 with
      | Some i -> String.trim (String.sub block 0 i)
      | None ->
          (match String.split_on_char '\n' block with
           | n :: _ -> String.trim n
           | [] -> "")
    in
    let name =
      match String.split_on_char ' ' (String.trim name) with
      | x :: _ -> String.trim x
      | [] -> String.trim name
    in
    if name = "" then None
    else
      let args = parse_args block in
      let arguments =
        Yojson.Safe.to_string
          (`Assoc (List.map (fun (k, v) -> (k, `String v)) args))
      in
      Some
        { id = Printf.sprintf "xml-tool-%d" (idx + 1)
        ; name
        ; arguments
        }
  in
  find_blocks [] 0
  |> List.mapi parse_one
  |> List.filter_map (fun x -> x)

(** Parse the [tool_calls] array off an assistant [message] object. *)
let parse_tool_calls_from_msg (msg : Yojson.Safe.t) : tool_call list =
  let open Yojson.Safe.Util in
  match msg |> member "tool_calls" with
  | `Null -> []
  | `List xs ->
      List.filter_map
        (fun tc ->
          try
            let id = tc |> member "id" |> to_string in
            let fn = tc |> member "function" in
            let name = fn |> member "name" |> to_string in
            let arguments =
              match fn |> member "arguments" with
              | `String s -> s
              | other -> Yojson.Safe.to_string other
            in
            Some { id; name; arguments }
          with _ -> None)
        xs
  | _ -> []

(** Parse a parsed chat-completions response into a [turn_result].
    Captures the assistant [message] object as [raw]. *)
(** Parse a non-empty choices list into a turn_result (the body of the
    old inline match, extracted so the error-body / shape guards above
    keep one definition of the choice path). *)
let parse_choices (choices : Yojson.Safe.t list) : turn_result option =
  let open Yojson.Safe.Util in
  match choices with
  | [] -> None
  | choice :: _ ->
      let msg = choice |> member "message" in
      let finish =
        match choice |> member "finish_reason" |> to_string_option with
        | Some f -> f
        | None -> ""
      in
      let content_opt =
        match msg |> member "content" |> to_string_option with
        | Some c when String.trim c <> "" -> Some c
        | _ -> None
      in
      let tcs = parse_tool_calls_from_msg msg in
      if tcs <> [] then
        Some (Need_tools { content = content_opt; tool_calls = tcs; raw = msg })
      else
        (* Fallback: XML tool_call blocks in content (common model failure). *)
        let xml_tcs =
          match content_opt with
          | Some c -> parse_xml_tool_calls c
          | None -> []
        in
        if xml_tcs <> [] then
          Some (Need_tools { content = None; tool_calls = xml_tcs; raw = msg })
        else if finish = "tool_calls" then
          Some (Turn_error "LLM: finish_reason=tool_calls but no tool_calls payload")
        else
          (match content_opt with
           | Some c -> Some (Final { content = c; raw = msg })
           | None ->
               let reasoning_alias =
                 match msg |> member "reasoning_content" |> to_string_option with
                 | Some r when String.trim r <> "" -> Some r
                 | _ ->
                     (* Synthetic streams/thinking messages carry plain
                        "reasoning" (agent provider.ml has the wire probe). *)
                     (match msg |> member "reasoning" |> to_string_option with
                      | Some r when String.trim r <> "" -> Some r
                      | _ -> None)
               in
               (match reasoning_alias with
                | Some r -> Some (Final { content = r; raw = msg })
                | None ->
                    Some
                      (Turn_error
                         (Printf.sprintf "LLM: empty content (finish=%s)" finish))))

let parse_turn_response_json (json : Yojson.Safe.t) : turn_result =
  try
    let open Yojson.Safe.Util in
    (match provider_error_message json with
     | Some msg -> Turn_error ("provider error: " ^ msg)
     | None ->
         (match json |> member "choices" with
          | `List (_ :: _ as choices) ->
              (match parse_choices choices with
               | Some r -> r
               | None -> Turn_error "LLM: empty choices")
          | `List [] -> Turn_error "LLM: empty choices"
          | _ ->
              (* choices missing/null/non-array with no error body *)
              Turn_error "LLM: no choices in response"))
  with e ->
    Turn_error ("LLM parse error: " ^ Printexc.to_string e)

(** Parse a raw chat-completions response body string. *)
let parse_turn_response (body : string) : turn_result =
  try parse_turn_response_json (Yojson.Safe.from_string body)
  with Yojson.Json_error e ->
    let trunc =
      if String.length body > 300 then String.sub body 0 300 ^ "..." else body
    in
    Turn_error ("LLM parse error: " ^ e ^ " body=" ^ trunc)

(** Extract the assistant text content from a non-streaming response body.
    Handles GLM-class models that fill [message.reasoning_content] and
    leave [content] null or truncated, preferring whichever field parses
    as complete JSON when the other is empty/truncated. Returns the raw
    content string (not parsed) so callers decide what to do with it. *)
let extract_content (body : string) : (string, string) result =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    (match provider_error_message json with
     | Some msg -> Error ("provider error: " ^ msg)
     | None ->
         let choices = json |> member "choices" |> to_list in
         match choices with
    | [] -> Error "LLM: empty choices"
    | choice :: _ ->
        let msg = choice |> member "message" in
        let finish =
          match choice |> member "finish_reason" |> to_string_option with
          | Some f -> f
          | None -> "?"
        in
        let content_field = string_field msg "content" in
        let reasoning =
          let r1 = string_field msg "reasoning_content" in
          if r1 <> "" then r1 else string_field msg "reasoning"
        in
        let content =
          match content_field, reasoning with
          | c, _ when c <> "" && looks_complete_json c -> c
          | _, r when r <> "" && looks_complete_json r -> r
          | c, r when c <> "" && r <> "" ->
              (* truncated content + reasoning: give the parser both *)
              c ^ "\n" ^ r
          | c, _ when c <> "" -> c
          | _, r when r <> "" -> r
          | _ -> ""
        in
        if content = "" then
          Error (Printf.sprintf "LLM: empty content (finish=%s)" finish)
        else Ok content)
  with e ->
    let trunc =
      if String.length body > 300 then String.sub body 0 300 ^ "..." else body
    in
    Error ("LLM parse error: " ^ Printexc.to_string e ^ " body=" ^ trunc)

(** ---- Streaming reassembly (SSE) ----

    ONE engine for reassembling an OpenAI [stream:true] response from
    its SSE [data:] lines: text + reasoning deltas (incl. the Synthetic
    plain-"reasoning" alias), tool-call argument fragments split across
    chunks, and [finish_reason]. Formerly re-implemented in
    llm/lib/provider.ml (the full engine) and hand-looped in
    image/lib/llm_client.ml (line parse); this module owns it beside
    [parse_turn_response] so the wire shape has one definition
    (i-1788671587-9). Pure: unit-testable without a socket. *)

(* Parse one SSE [data:] line from the raw body: returns the payload
   after the "data:" prefix. Empty/comment lines yield None. *)
let sse_data_payload (line : string) : string option =
  let line = String.trim line in
  if String.length line >= 5 && String.sub line 0 5 = "data:" then
    let rest = String.trim (String.sub line 5 (String.length line - 5)) in
    if rest = "" then None else Some rest
  else None

(** One reassembled streaming response, before host-shape mapping.
    [response] is the synthetic FULL chat-completions response (a
    choices wrapper over a synthetic assistant [message]) so hosts run
    it through their usual response parser ([parse_turn_response_json]
    here, [Openai.parse_reply] in the standalone agent) and inherit the
    XML-fallback / reasoning_content handling and history [raw] shape
    unchanged. *)
type stream_result = {
  text : string;
  reasoning : string;
  tool_calls : tool_call list;  (* arguments = concatenated fragments *)
  finish : string;
  response : Yojson.Safe.t;
}

(** Reassemble [lines] (the SSE [data:] payloads of one streaming
    response, in order) into a [stream_result]. [on_text] /
    [on_reasoning] fire per content/reasoning chunk as it is decoded
    (in stream order) so incremental surfaces keep their per-chunk
    events. Malformed data lines are skipped (lenient, as on the
    wire). *)
let reassemble_stream ?(on_text : string -> unit = fun _ -> ())
    ?(on_reasoning : string -> unit = fun _ -> ())
    (lines : string list) : stream_result =
  let open Yojson.Safe.Util in
  let text_buf = Buffer.create 128 in
  let reasoning_buf = Buffer.create 128 in
  (* index -> accumulated tool-call fragment {id; name; arguments buf} *)
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
                    on_reasoning s
                | None -> ());
               (match delta |> member "content" |> to_string_option with
                | Some s when s <> "" ->
                    Buffer.add_string text_buf s;
                    on_text s
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
  let reasoning = Buffer.contents reasoning_buf in
  let tool_calls =
    Hashtbl.fold
      (fun _ (id, name, args) acc ->
        if name = "" then acc
        else
          (* Canonicalize the concatenated fragments the way the
             reassembled wire would carry them (malformed -> {}). *)
          let arguments =
            match Yojson.Safe.from_string (Buffer.contents args) with
            | j -> Yojson.Safe.to_string j
            | exception _ -> "{}"
          in
          { id = (if id = "" then "tool-call" else id); name; arguments }
          :: acc)
      tool_bufs []
  in
  (* Synthetic assistant message, so the shared XML/GLM fallbacks and
     [raw] history shape apply when hosts parse [response]. *)
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
                         [ ("id", `String tc.id)
                         ; ("type", `String "function")
                         ; ( "function",
                             `Assoc
                               [ ("name", `String tc.name)
                               ; ("arguments", `String tc.arguments) ])
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
  { text; reasoning; tool_calls; finish = !finish; response }

(** Extract delta content from one streaming SSE JSON payload ([DONE]
    or a [choices[0].delta.content] chunk). *)
let extract_stream_delta (data : string) : string option =
  if data = "[DONE]" then None
  else
    (try
       let json = Yojson.Safe.from_string data in
       let open Yojson.Safe.Util in
       match json |> member "choices" |> to_list with
       | [] -> None
       | choice :: _ ->
           let delta = choice |> member "delta" in
           (match delta |> member "content" |> to_string_option with
            | Some s when s <> "" -> Some s
            | _ -> None)
     with _ -> None)
