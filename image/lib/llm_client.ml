(** OpenAI-compatible LLM client for Pricklypear AI authoring.

    Uses curl for HTTPS (cohttp-lwt-unix not on poohstack). Config via env:
      LLM_API_BASE  default https://api.synthetic.new/openai/v1
      LLM_API_KEY   required for live calls
      LLM_MODEL     default hf:zai-org/GLM-4.7-Flash

    Spec [ai-authoring.tools]: propose functions on the current branch
    with agent provenance. This module only does HTTP; session/http
    layers own store writes. *)
open Lwt.Syntax

let env_or name dft =
  match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | _ -> dft

let api_base () = env_or "LLM_API_BASE" "https://api.synthetic.new/openai/v1"
let api_key () = env_or "LLM_API_KEY" ""
let model () = env_or "LLM_MODEL" "hf:zai-org/GLM-4.7-Flash"

type call_result = (string, string) result

(** Progress events for live sinks (SSE / REPL). Not a capability effect. *)
type agent_event =
  | Token of string
  | Done
  | Fail of string

let shell_quote (s : string) : string =
  (* single-quote for POSIX sh, escape embedded single quotes *)
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '\'';
  String.iter
    (function
      | '\'' -> Buffer.add_string buf "'\\''"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let extract_content (body : string) : call_result =
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
        let content =
          match msg |> member "content" |> to_string_option with
          | Some c when c <> "" -> c
          | _ ->
              (match msg |> member "reasoning_content" |> to_string_option with
               | Some r when r <> "" -> r
               | _ ->
                   match msg |> member "reasoning" |> to_string_option with
                   | Some r when r <> "" -> r
                   | _ -> "")
        in
        if content = "" then
          Error (Printf.sprintf "LLM: empty content (finish=%s)" finish)
        else Ok content
  with e ->
    let trunc =
      if String.length body > 300 then String.sub body 0 300 ^ "..." else body
    in
    Error ("LLM parse error: " ^ Printexc.to_string e ^ " body=" ^ trunc)

let call ?(temperature = 0.2) ?(max_tokens = 4096)
    ~(system : string) ~(user : string) () : call_result Lwt.t =
  let key = api_key () in
  if key = "" then
    Lwt.return (Error "LLM_API_KEY not set")
  else
    let body_json =
      `Assoc
        [ ("model", `String (model ()))
        ; ("messages",
           `List
             [ `Assoc [("role", `String "system"); ("content", `String system)]
             ; `Assoc [("role", `String "user"); ("content", `String user)]
             ])
        ; ("temperature", `Float temperature)
        ; ("max_tokens", `Int max_tokens)
        ]
    in
    let body_str = Yojson.Safe.to_string body_json in
    let url = api_base () ^ "/chat/completions" in
    (* Write body to a temp file to avoid shell length limits. *)
    let tmp = Filename.temp_file "pp_llm_" ".json" in
    let oc = open_out tmp in
    output_string oc body_str;
    close_out oc;
    let cmd =
      Printf.sprintf
        "curl -sS --max-time 180 -X POST %s \
         -H %s -H %s --data-binary @%s 2>&1"
        (shell_quote url)
        (shell_quote "Content-Type: application/json")
        (shell_quote ("Authorization: Bearer " ^ key))
        (shell_quote tmp)
    in
    Printf.eprintf "[llm] POST %s model=%s\n" url (model ());
    Lwt.catch
      (fun () ->
        let* stdout =
          Lwt_process.pread ~timeout:200. (Lwt_process.shell cmd)
        in
        (try Sys.remove tmp with _ -> ());
        if String.trim stdout = "" then
          Lwt.return (Error "LLM: empty curl response")
        else if String.length stdout > 12
                && (String.sub stdout 0 6 = "curl: "
                    || String.sub stdout 0 5 = "curl ") then
          Lwt.return (Error ("curl: " ^ String.trim stdout))
        else
          Lwt.return (extract_content stdout))
      (fun exn ->
        (try Sys.remove tmp with _ -> ());
        Lwt.return (Error ("LLM transport: " ^ Printexc.to_string exn)))

(** Strip optional markdown fences from model output. *)
let strip_fences (s : string) : string =
  let s = String.trim s in
  if String.length s >= 3 && String.sub s 0 3 = "```" then
    let start =
      match String.index_opt s '\n' with
      | Some i -> i + 1
      | None -> 3
    in
    let rest = String.sub s start (String.length s - start) in
    match String.rindex_opt rest '`' with
    | Some i when i >= 3 && String.sub rest (i - 2) 3 = "```" ->
        String.trim (String.sub rest 0 (i - 2))
    | _ ->
        (match String.rindex_opt rest '\n' with
         | Some i -> String.trim (String.sub rest 0 i)
         | None -> String.trim rest)
  else s

(** Pull the first balanced s-expression from model noise
    (reasoning essays, markdown, numbered lists). *)
let extract_lisp_sexpr (s : string) : (string, string) result =
  let s = strip_fences s in
  let len = String.length s in
  let rec find_open i =
    if i >= len then None
    else if s.[i] = '(' then Some i
    else find_open (i + 1)
  in
  match find_open 0 with
  | None ->
      Error "no Lisp s-expression found in model output (expected a (lambda ...) form)"
  | Some start ->
      let rec scan i depth in_str =
        if i >= len then Error "unbalanced parentheses in model output"
        else
          let c = s.[i] in
          if in_str then
            if c = '\\' && i + 1 < len then scan (i + 2) depth true
            else if c = '"' then scan (i + 1) depth false
            else scan (i + 1) depth true
          else
            match c with
            | '"' -> scan (i + 1) depth true
            | '(' -> scan (i + 1) (depth + 1) false
            | ')' ->
                if depth <= 1 then
                  Ok (String.trim (String.sub s start (i - start + 1)))
                else scan (i + 1) (depth - 1) false
            | ';' ->
                (* skip line comment *)
                let rec skip j =
                  if j >= len || s.[j] = '\n' then j else skip (j + 1)
                in
                scan (skip i) depth false
            | _ -> scan (i + 1) depth false
      in
      (match scan (start + 1) 1 false with
       | Error e -> Error e
       | Ok sexpr ->
           (* Prefer a lambda/list form; reject tiny garbage. *)
           if String.length sexpr < 3 then
             Error "extracted s-expression too short"
           else Ok sexpr)

(** Extract delta content from one streaming SSE JSON payload. *)
let extract_stream_delta (data : string) : string option =
  if data = "[DONE]" then None
  else
    try
      let json = Yojson.Safe.from_string data in
      let open Yojson.Safe.Util in
      match json |> member "choices" |> to_list with
      | [] -> None
      | choice :: _ ->
          let delta = choice |> member "delta" in
          (match delta |> member "content" |> to_string_option with
           | Some s when s <> "" -> Some s
           | _ -> None)
    with _ -> None

(** Unified agent chat: system=seed, user=prompt.
    When [stream] is true, uses OpenAI stream:true and fires [on_event]
    for each token. Always returns the final concatenated text. *)
let agent_chat
    ?(temperature = 0.3)
    ?(max_tokens = 4096)
    ?(stream = false)
    ?(on_event : agent_event -> unit = fun _ -> ())
    ~(seed : string)
    ~(prompt : string)
    () : call_result Lwt.t =
  let key = api_key () in
  if key = "" then begin
    on_event (Fail "LLM_API_KEY not set");
    Lwt.return (Error "LLM_API_KEY not set")
  end else if not stream then begin
    let* res = call ~temperature ~max_tokens ~system:seed ~user:prompt () in
    (match res with
     | Ok text -> on_event (Token text); on_event Done; Lwt.return (Ok text)
     | Error e -> on_event (Fail e); Lwt.return (Error e))
  end else
    let body_json =
      `Assoc
        [ ("model", `String (model ()))
        ; ("messages",
           `List
             [ `Assoc [("role", `String "system"); ("content", `String seed)]
             ; `Assoc [("role", `String "user"); ("content", `String prompt)]
             ])
        ; ("temperature", `Float temperature)
        ; ("max_tokens", `Int max_tokens)
        ; ("stream", `Bool true)
        ]
    in
    let body_str = Yojson.Safe.to_string body_json in
    let url = api_base () ^ "/chat/completions" in
    let tmp = Filename.temp_file "pp_llm_stream_" ".json" in
    let oc = open_out tmp in
    output_string oc body_str;
    close_out oc;
    let cmd =
      Printf.sprintf
        "curl -sS -N --max-time 180 -X POST %s \
         -H %s -H %s --data-binary @%s 2>&1"
        (shell_quote url)
        (shell_quote "Content-Type: application/json")
        (shell_quote ("Authorization: Bearer " ^ key))
        (shell_quote tmp)
    in
    Printf.eprintf "[llm] STREAM %s model=%s\n" url (model ());
    let buf = Buffer.create 4096 in
    Lwt.catch
      (fun () ->
        let proc = Lwt_process.open_process_in (Lwt_process.shell cmd) in
        let rec loop () =
          let* line_opt = Lwt_io.read_line_opt proc#stdout in
          match line_opt with
          | None ->
              let* st = proc#close in
              (try Sys.remove tmp with _ -> ());
              ignore st;
              let text = Buffer.contents buf in
              if String.trim text = "" then begin
                on_event (Fail "LLM stream: empty content");
                Lwt.return (Error "LLM stream: empty content")
              end else begin
                on_event Done;
                Lwt.return (Ok text)
              end
          | Some line ->
              let line = String.trim line in
              if line = "" then loop ()
              else if String.length line >= 6
                      && String.sub line 0 6 = "data: " then
                let data = String.sub line 6 (String.length line - 6) |> String.trim in
                if data = "[DONE]" then loop ()
                else begin
                  (match extract_stream_delta data with
                   | Some tok ->
                       Buffer.add_string buf tok;
                       on_event (Token tok)
                   | None -> ());
                  loop ()
                end
              else if String.length line >= 5
                      && (String.sub line 0 5 = "curl "
                          || String.sub line 0 5 = "curl:") then begin
                (try Sys.remove tmp with _ -> ());
                on_event (Fail line);
                let* _ = proc#close in
                Lwt.return (Error line)
              end else
                loop ()
        in
        loop ())
      (fun exn ->
        (try Sys.remove tmp with _ -> ());
        let e = "LLM stream transport: " ^ Printexc.to_string exn in
        on_event (Fail e);
        Lwt.return (Error e))


(** ---- Multi-turn chat + OpenAI tools (agent loop) ---- *)

type role = System | User | Assistant | Tool

type tool_call = {
  id : string;
  name : string;
  arguments : string;  (* JSON object string *)
}

type chat_message =
  | Msg of { role : role; content : string }
  | Assistant_tools of { content : string option; tool_calls : tool_call list }
  | Tool_result of { tool_call_id : string; content : string }

type tool_def = {
  name : string;
  description : string;
  parameters : Yojson.Safe.t;  (* JSON Schema object *)
}

type turn_result =
  | Final of string
  | Need_tools of { content : string option; tool_calls : tool_call list }
  | Turn_error of string

let role_to_string = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"

let message_to_json = function
  | Msg { role; content } ->
      `Assoc
        [ ("role", `String (role_to_string role))
        ; ("content", `String content)
        ]
  | Assistant_tools { content; tool_calls } ->
      let tcs =
        List.map
          (fun (tc : tool_call) ->
            `Assoc
              [ ("id", `String tc.id)
              ; ("type", `String "function")
              ; ("function",
                 `Assoc
                   [ ("name", `String tc.name)
                   ; ("arguments", `String tc.arguments)
                   ])
              ])
          tool_calls
      in
      let base =
        [ ("role", `String "assistant")
        ; ("tool_calls", `List tcs)
        ]
      in
      let base =
        match content with
        | Some c when c <> "" -> ("content", `String c) :: base
        | _ -> ("content", `Null) :: base
      in
      `Assoc base
  | Tool_result { tool_call_id; content } ->
      `Assoc
        [ ("role", `String "tool")
        ; ("tool_call_id", `String tool_call_id)
        ; ("content", `String content)
        ]

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

let parse_tool_calls (msg : Yojson.Safe.t) : tool_call list =
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

let parse_turn_response (body : string) : turn_result =
  try
    let json = Yojson.Safe.from_string body in
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
        let tcs = parse_tool_calls msg in
        if tcs <> [] || finish = "tool_calls" then
          Need_tools { content = content_opt; tool_calls = tcs }
        else
          (match content_opt with
           | Some c -> Final c
           | None ->
               (* some models put answer only in reasoning *)
               (match msg |> member "reasoning_content" |> to_string_option with
                | Some r when String.trim r <> "" -> Final r
                | _ -> Turn_error (Printf.sprintf "LLM: empty content (finish=%s)" finish)))
  with e ->
    let trunc =
      if String.length body > 300 then String.sub body 0 300 ^ "..." else body
    in
    Turn_error ("LLM parse error: " ^ Printexc.to_string e ^ " body=" ^ trunc)

(** One non-streaming chat completions call over a full message list. *)
let chat_turn
    ?(temperature = 0.2)
    ?(max_tokens = 4096)
    ?(tools : tool_def list = [])
    ~(messages : chat_message list)
    () : turn_result Lwt.t =
  let key = api_key () in
  if key = "" then
    Lwt.return (Turn_error "LLM_API_KEY not set")
  else
    let msgs = `List (List.map message_to_json messages) in
    let fields =
      [ ("model", `String (model ()))
      ; ("messages", msgs)
      ; ("temperature", `Float temperature)
      ; ("max_tokens", `Int max_tokens)
      ]
    in
    let fields =
      if tools = [] then fields
      else
        ("tools", `List (List.map tool_def_to_json tools))
        :: ("tool_choice", `String "auto")
        :: fields
    in
    let body_str = Yojson.Safe.to_string (`Assoc fields) in
    let url = api_base () ^ "/chat/completions" in
    let tmp = Filename.temp_file "pp_llm_turn_" ".json" in
    let oc = open_out tmp in
    output_string oc body_str;
    close_out oc;
    let cmd =
      Printf.sprintf
        "curl -sS --max-time 180 -X POST %s \
         -H %s -H %s --data-binary @%s 2>&1"
        (shell_quote url)
        (shell_quote "Content-Type: application/json")
        (shell_quote ("Authorization: Bearer " ^ key))
        (shell_quote tmp)
    in
    Printf.eprintf "[llm] TURN %s model=%s tools=%d msgs=%d\n%!"
      url (model ()) (List.length tools) (List.length messages);
    Lwt.catch
      (fun () ->
        let* stdout =
          Lwt_process.pread ~timeout:200. (Lwt_process.shell cmd)
        in
        (try Sys.remove tmp with _ -> ());
        if String.trim stdout = "" then
          Lwt.return (Turn_error "LLM: empty curl response")
        else if String.length stdout > 6
                && String.sub stdout 0 5 = "curl " then
          Lwt.return (Turn_error ("curl: " ^ String.trim stdout))
        else
          Lwt.return (parse_turn_response stdout))
      (fun exn ->
        (try Sys.remove tmp with _ -> ());
        Lwt.return (Turn_error ("LLM transport: " ^ Printexc.to_string exn)))

(** Ask the model to write one Lisp function body (s-expr).
    Returns source text for (define name <body>). *)
let propose_function_source
    ~(name : string)
    ~(spec : string)
    ?(context = "")
    () : call_result Lwt.t =
  let system =
    "You are a Pricklypear Lisp author. Respond ONLY with a single Lisp \
     s-expression that is the BODY of a function (typically a lambda). \
     No markdown fences, no explanation, no (define ...) wrapper. \
     Available prims include: + - * = < > <= >= not list car cdr cons \
     null? pair? count string-append str type-of whoami list-bindings \
     dict-get dict-set dict-keys get create update delete rows deftype \
     fork merge route checkout render-html. Use quote for symbols/data. \
     Prefer small pure functions unless the task needs DB/IO prims."
  in
  let user =
    Printf.sprintf
      "Function name: %s\nRequirement:\n%s\n%s\nWrite only the body s-expression."
      name spec
      (if context = "" then "" else "Context:\n" ^ context ^ "\n")
  in
  let* res = call ~system ~user () in
  match res with
  | Error e -> Lwt.return (Error e)
  | Ok text ->
      let cleaned = strip_fences text in
      Lwt.return (Ok cleaned)

(** Surgical AST author: rewrite ONE component (coding tool). *)
let rewrite_component_source
    ~(mode : string)  (* "replace" | "wrap" *)
    ~(name : string)
    ~(concept : string)
    ~(current_pretty : string)
    () : call_result Lwt.t =
  let system =
    "You are editing a Pricklypear Lisp AST as a coding tool. " ^
    "Return ONLY one replacement s-expression (typically a lambda). " ^
    "No markdown fences, no commentary, no (define ...) wrapper. " ^
    "You write ONE component of a larger program tree. " ^
    "Available prims: + - * = < > <= >= not list car cdr cons null? pair? " ^
    "count string-append str type-of dict-get dict-set dict-keys " ^
    "get create update delete rows deftype render-html whoami. " ^
    "Prefer pure functions when possible."
  in
  let user =
    if mode = "wrap" then
      Printf.sprintf
        "Mode: WRAP (llm-outer)\nFunction name: %s\n\
         User concept: %s\n\
         Current function (pretty):\n%s\n\
         Write a new lambda that wraps this behavior (call-through or \
         carefully inline). Preserve the essential behavior and add \
         ONLY the requested responsibility. Return one lambda only."
        name concept current_pretty
    else
      Printf.sprintf
        "Mode: REPLACE body (llm-inner)\nFunction name: %s\n\
         User concept: %s\n\
         Current function (pretty):\n%s\n\
         Write a replacement function body (usually a lambda) that \
         satisfies the concept. Return one s-expression only."
        name concept current_pretty
  in
  let* res = call ~system ~user () in
  match res with
  | Error e -> Lwt.return (Error e)
  | Ok text -> Lwt.return (Ok (strip_fences text))

(** Seed: smallest clean core for growth. *)
let seed_core_source ~(requirement : string) () : call_result Lwt.t =
  let system =
    "You write the SMALLEST and CLEANEST Pricklypear Lisp nucleus function. " ^
    "Return ONLY one lambda s-expression. No markdown, no define wrapper. " ^
    "Prefer pure computation. No wrapping, no IO unless absolutely required."
  in
  let user =
    Printf.sprintf
      "Requirement:\n%s\nWrite the smallest pure-ish lambda that captures the essential core."
      requirement
  in
  let* res = call ~system ~user () in
  match res with
  | Error e -> Lwt.return (Error e)
  | Ok text -> Lwt.return (Ok (strip_fences text))

(** Judge growth: returns Ok "done" or Ok "next: <responsibility>" *)
let growth_judge
    ~(requirement : string)
    ~(current_pretty : string)
    ~(step : int)
    () : call_result Lwt.t =
  let system =
    "You judge whether a growing Lisp function set satisfies a requirement. " ^
    "Respond with EXACTLY one line: either DONE or NEXT: <single short responsibility>. " ^
    "No markdown. NEXT must be one outer wrap responsibility only."
  in
  let user =
    Printf.sprintf
      "Requirement:\n%s\nStep: %d\nCurrent tip function:\n%s\nJudge."
      requirement step current_pretty
  in
  let* res = call ~system ~user ~temperature:0.1 () in
  match res with
  | Error e -> Lwt.return (Error e)
  | Ok text -> Lwt.return (Ok (String.trim (strip_fences text)))
