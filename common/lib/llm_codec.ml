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
let parse_turn_response_json (json : Yojson.Safe.t) : turn_result =
  try
    let open Yojson.Safe.Util in
    match json |> member "choices" |> to_list with
    | [] -> Turn_error "LLM: empty choices"
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
          Need_tools { content = content_opt; tool_calls = tcs; raw = msg }
        else
          (* Fallback: XML tool_call blocks in content (common model failure). *)
          let xml_tcs =
            match content_opt with
            | Some c -> parse_xml_tool_calls c
            | None -> []
          in
          if xml_tcs <> [] then
            Need_tools { content = None; tool_calls = xml_tcs; raw = msg }
          else if finish = "tool_calls" then
            Turn_error "LLM: finish_reason=tool_calls but no tool_calls payload"
          else
            (match content_opt with
             | Some c -> Final { content = c; raw = msg }
             | None ->
                 (match msg |> member "reasoning_content" |> to_string_option with
                  | Some r when String.trim r <> "" -> Final { content = r; raw = msg }
                  | _ ->
                      Turn_error
                        (Printf.sprintf "LLM: empty content (finish=%s)" finish)))
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
        else Ok content
  with e ->
    let trunc =
      if String.length body > 300 then String.sub body 0 300 ^ "..." else body
    in
    Error ("LLM parse error: " ^ Printexc.to_string e ^ " body=" ^ trunc)

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
