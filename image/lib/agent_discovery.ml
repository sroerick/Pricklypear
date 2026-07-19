(** Public agent bootstrap documents (no secrets).

    Hard-public:
      GET /.well-known/agent.json  — machine contract
      GET /agent.txt               — prose instructions

    Eval stays private: POST /api/eval requires HTTP Basic or session.
    /agent (no suffix) remains the private HTML agent portal.
*)

let header_ci (headers : (string * string) list) (name : string) : string option =
  let lower = String.lowercase_ascii name in
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii k = lower then Some v else None)
    headers

(** Prefer reverse-proxy headers, then Host. Never invent a password. *)
let base_url_of_headers (headers : (string * string) list) : string option =
  let host =
    match header_ci headers "x-forwarded-host" with
    | Some h -> Some (String.trim h)
    | None ->
        match header_ci headers "host" with
        | Some h -> Some (String.trim h)
        | None -> None
  in
  match host with
  | None | Some "" -> None
  | Some host ->
      let proto =
        match header_ci headers "x-forwarded-proto" with
        | Some p when String.trim p <> "" -> String.lowercase_ascii (String.trim p)
        | _ ->
            (* Default https for public hosts; plain http for loopback. *)
            let loopback =
              host = "localhost"
              || String.starts_with ~prefix:"localhost:" host
              || String.starts_with ~prefix:"127." host
              || String.starts_with ~prefix:"[::1]" host
            in
            if loopback then "http" else "https"
      in
      Some (proto ^ "://" ^ host)

let philosophy =
  "The image is the API. Eval Lisp on the running habitat; do not invent \
   REST endpoints for app features (todos, notes, bible, …)."

let first_calls =
  [ "(whoami)"
  ; "(current-branch)"
  ; "(list-bindings)"
  ; "(load-library \"todo\")"
  ; "(todo/list)"
  ; "(load-library \"bible\")"
  ; "(bible/chapter \"John\" 3)"
  ]

let rules =
  [ "One Lisp expression per POST /api/eval unless you own multi-step state yourself."
  ; "Load libraries before calling package functions: (load-library \"todo\")."
  ; "Prefer a personal branch before defining things: (checkout \"USER/personal\")."
  ; "Two function kinds: DATA builders take explicit args and work from /api/eval \
     (e.g. bible/chapter, bible/chapter-html with book/chapter); HTTP HANDLERS \
     (*/page, */reader) use request-method/request-param and return \
     \"no active request\" outside a browser hit — that is expected, not a crash."
  ; "Do not invent app-specific REST APIs — call Lisp instead."
  ; "Never put passwords in code you write back to the image."
  ; "Unauthenticated /api/eval is rejected; /agent (HTML portal) is private."
  ; "Dotted rest params (a . b) are NOT supported; use fixed-arity params."
  ; "File product issues as notes in the inbox when you cannot fix OCaml; \
     do not invent REST for filing."
  ]

let curl_example (base : string) : string =
  Printf.sprintf
    "curl -sS -u USER:PASS -H 'Content-Type: application/json' \
-H 'Accept: application/json' \
-d '{\"expr\":\"(whoami)\"}' %s/api/eval"
    base

let python_example (base : string) : string =
  Printf.sprintf
{|import base64, json, urllib.request
base = %S
user, password = "USER", "PASS"
expr = "(whoami)"
req = urllib.request.Request(
    base.rstrip("/") + "/api/eval",
    data=json.dumps({"expr": expr}).encode(),
    method="POST",
    headers={
        "Content-Type": "application/json",
        "Accept": "application/json",
        "Authorization": "Basic " + base64.b64encode(
            f"{user}:{password}".encode()).decode("ascii"),
    },
)
print(urllib.request.urlopen(req).read().decode())
|}
    base

let agent_json ?(base_url : string option = None) () : Yojson.Safe.t =
  let base_field =
    match base_url with
    | Some b -> [ ("base_url", `String b) ]
    | None ->
        [ ( "base_url_note"
          , `String
              "Set base_url to the origin you fetched this document from \
               (scheme + host)." ) ]
  in
  let example_base =
    match base_url with Some b -> b | None -> "https://pricklypear.rocks"
  in
  `Assoc
    ( [ ("name", `String "pricklypear")
      ; ("version", `Int 2)
      ; ( "philosophy"
        , `String philosophy )
      ]
    @ base_field
    @ [ ( "auth"
        , `Assoc
            [ ("scheme", `String "HTTP Basic")
            ; ( "realm"
              , `String
                  "image credentials table (same username/password as /login)" )
            ; ( "how_to_get_credentials"
              , `String
                  "Human supplies username + password once. Create users at \
                   /admin/users after bootstrap login. Do not scrape passwords \
                   from this document — there are none." )
            ; ("login_url", `String "/login")
            ; ( "session_cookie"
              , `String
                  "Browser clients may use cookie pp_session (HttpOnly) after \
                   POST /login; agents usually use Basic auth." )
            ; ( "notes"
              , `List
                  [ `String "Use the image password, not nginx/edge htpasswd."
                  ; `String "No OAuth/JWT in v1."
                  ; `String "Never log or commit the password."
                  ] )
            ] )
      ; ( "endpoints"
        , `Assoc
            [ ( "eval"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/eval")
                  ; ("auth", `String "required")
                  ; ("content_type", `String "application/json")
                  ; ( "request"
                    , `Assoc [ ("expr", `String "(+ 1 2)") ] )
                  ; ( "response_ok"
                    , `Assoc
                        [ ("ok", `Bool true)
                        ; ("value", `String "3")
                        ] )
                  ; ( "response_err"
                    , `Assoc
                        [ ("ok", `Bool false)
                        ; ("error", `String "unbound: foo")
                        ] )
                  ] )
            ; ( "health"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/health")
                  ; ("auth", `String "none")
                  ] )
            ; ( "repl_ui"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/repl")
                  ; ("auth", `String "session or basic")
                  ] )
            ; ( "discovery_json"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/.well-known/agent.json")
                  ; ("auth", `String "none")
                  ] )
            ; ( "discovery_text"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/agent.txt")
                  ; ("auth", `String "none")
                  ] )
            ] )
      ; ( "bootstrap"
        , `Assoc
            [ ( "first_calls"
              , `List (List.map (fun s -> `String s) first_calls) )
            ; ("rules", `List (List.map (fun s -> `String s) rules))
            ] )
      ; ( "examples"
        , `Assoc
            [ ("curl", `String (curl_example example_base))
            ; ("python", `String (python_example example_base))
            ] )
      ; ( "related"
        , `Assoc
            [ ("welcome", `String "/welcome")
            ; ("source_browser", `String "/code")
            ; ("book_pdf", `String "/book.pdf")
            ; ("source_tarball", `String "/src.tgz")
            ; ("login", `String "/login")
            ; ( "agent_portal"
              , `String
                  "/agent (private HTML UI — not this discovery document)" )
            ] )
      ] )

let agent_markdown ?(base_url : string option = None) () : string =
  let origin =
    match base_url with
    | Some b -> b
    | None -> "https://pricklypear.rocks"
  in
  let first =
    first_calls
    |> List.map (fun s -> "- `" ^ s ^ "`")
    |> String.concat "\n"
  in
  let rule_lines =
    rules
    |> List.mapi (fun i r -> string_of_int (i + 1) ^ ". " ^ r)
    |> String.concat "\n"
  in
  String.concat "\n"
    [ "# Pricklypear agent instructions"
    ; ""
    ; philosophy
    ; ""
    ; "## Origin"
    ; ""
    ; "Base URL (this host): " ^ origin
    ; "Machine contract: GET " ^ origin ^ "/.well-known/agent.json"
    ; ""
    ; "## Auth"
    ; ""
    ; "Scheme: HTTP Basic against the image credentials table"
    ; "(same username/password as " ^ origin ^ "/login)."
    ; ""
    ; "How to get credentials: ask the human once. Store them in a"
    ; "local secrets file (mode 600). This document never contains"
    ; "passwords. Create users at /admin/users after bootstrap login."
    ; ""
    ; "Notes:"
    ; "- Use the image password, not nginx/edge htpasswd."
    ; "- No OAuth/JWT in v1."
    ; "- Browser clients may use cookie pp_session after POST /login;"
    ; "  agents usually use Basic auth."
    ; ""
    ; "## Eval API"
    ; ""
    ; "POST " ^ origin ^ "/api/eval"
    ; "Content-Type: application/json"
    ; "Accept: application/json"
    ; "Authorization: Basic <base64(user:pass)>"
    ; ""
    ; "Request body:"
    ; ""
    ; "    {\"expr\":\"(+ 1 2)\"}"
    ; ""
    ; "Response:"
    ; ""
    ; "    {\"ok\": true, \"value\": \"3\"}"
    ; "    {\"ok\": false, \"error\": \"unbound: foo\"}"
    ; ""
    ; "Unauthenticated calls are rejected (401)."
    ; "Errors are plain strings (arity mismatch, unbound, no active request)."
    ; ""
    ; "## First calls"
    ; ""
    ; first
    ; ""
    ; "## Two kinds of functions"
    ; ""
    ; "1. DATA / builders — take explicit arguments; safe from /api/eval:"
    ; "     (bible/chapter \"John\" 3)"
    ; "     (bible/chapter-html \"John\" 3 ())"
    ; "     (todo/list) after (load-library \"todo\")"
    ; "2. HTTP HANDLERS — read (request-method)/(request-param ...);"
    ; "     from /api/eval they return: no active request"
    ; "     Examples: bible/chapter-reader, todo/page, notes/page, */page."
    ; "     That message is expected. Call data functions, or hit the route"
    ; "     in a browser / with a real HTTP request."
    ; ""
    ; "Inspect source: (function-source \"bible/chapter-html\")"
    ; "List a package: (package-bindings \"bible\") after load-library."
    ; ""
    ; "## Application libraries (load-library)"
    ; ""
    ; "- std, ui, layout, welcome — shell / HTML helpers"
    ; "- todo, notes, outline, plan, contacts, bookmarks, chat"
    ; "- calendar, flashcards, bible, dabar, weather, signup, ai"
    ; "Cold image after restart: (load-library \"NAME\") before calling."
    ; ""
    ; "## Rules"
    ; ""
    ; rule_lines
    ; ""
    ; "## curl example"
    ; ""
    ; "    " ^ curl_example origin
    ; ""
    ; "BusyBox wget often lacks --user/--password; prefer curl or python."
    ; ""
    ; "## python example"
    ; ""
    ; "```python"
    ; python_example origin
    ; "```"
    ; ""
    ; "## Filing issues"
    ; ""
    ; "Prefer notes inbox (notes/doc/ensure-inbox + outline nodes) over"
    ; "inventing bug-tracker REST. Keep OCaml-kernel bugs precise with"
    ; "repro expressions."
    ; ""
    ; "## Related public surfaces"
    ; ""
    ; "- Welcome: " ^ origin ^ "/welcome"
    ; "- Source browser: " ^ origin ^ "/code"
    ; "- Book PDF: " ^ origin ^ "/book.pdf"
    ; "- Health: " ^ origin ^ "/health"
    ; "- Interactive REPL UI (auth required): " ^ origin ^ "/repl"
    ; "- Private agent portal (auth required): " ^ origin ^ "/agent"
    ; ""
    ; "## What not to do"
    ; ""
    ; "- Do not open or rely on the raw TCP REPL port from the internet."
    ; "- Do not invent per-app REST endpoints."
    ; "- Do not put secrets into (define …) forms or public routes."
    ; "- Do not treat \"no active request\" as a host crash — call a data fn."
    ; ""
    ]

let serve_agent_json (reqd : Httpun.Reqd.t)
    ~(headers : (string * string) list) : unit =
  let base_url = base_url_of_headers headers in
  let j = agent_json ~base_url () in
  Http_core.text_response reqd
    ~content_type:"application/json; charset=utf-8"
    (Yojson.Safe.pretty_to_string j)

let serve_agent_txt (reqd : Httpun.Reqd.t)
    ~(headers : (string * string) list) : unit =
  let base_url = base_url_of_headers headers in
  let body = agent_markdown ~base_url () in
  Http_core.text_response reqd
    ~content_type:"text/plain; charset=utf-8" body
