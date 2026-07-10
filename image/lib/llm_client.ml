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
