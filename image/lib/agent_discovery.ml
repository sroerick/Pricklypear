(** Public agent bootstrap documents (no secrets).

    Hard-public:
      GET /.well-known/agent.json  — machine contract
      GET /agent.txt               — prose instructions

    Eval stays private: POST /api/eval requires HTTP Basic or session.
    /agent (no suffix) remains the private HTML agent portal.
*)

(** Prefer reverse-proxy headers, then Host. Never invent a password. *)
let base_url_of_headers (headers : (string * string) list) : string option =
  let host =
    match Http_kit.header_ci headers "x-forwarded-host" with
    | Some h -> Some (String.trim h)
    | None ->
        match Http_kit.header_ci headers "host" with
        | Some h -> Some (String.trim h)
        | None -> None
  in
  match host with
  | None | Some "" -> None
  | Some host ->
      let proto =
        match Http_kit.header_ci headers "x-forwarded-proto" with
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
  ; "(list-packages)"
  ; "(notes/doc/get \"agents\")"
  ; "(describe \"todo\")"
  ; "(apropos-docs \"todo/item\")"
  ; "(function-doc \"todo/item/add\")"
  ; "(count (todo/list))"
  ]

let rules =
  [ "One Lisp expression per POST /api/eval unless you own multi-step state yourself."
  ; "Package functions bind LAZILY from your branch chain — call them "
    ^ "directly, no (load-library) needed. load-library is a DEPLOY tool: "
    ^ "it re-evals the release file onto your current branch (warns + pins "
    ^ "on non-main). Never use it in repair scripts or route handlers."
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
  ; "Read the live agents note: (notes/doc/get \"agents\"). It is the "
    ^ "habitat AGENTS.md — edit it with notes/doc/upsert when agent "
    ^ "guidance should change."
  ; "Discover selectively — do not dump all bindings into context: \
     (list-packages), (describe \"pkg\"), (package-bindings \"pkg\"), \
     (package-exports \"pkg\"), (apropos-docs \"pat\"), (function-doc name)."
  ; "Build a REPL client from this document: HTTP Basic + POST /api/eval \
     JSON {\"expr\":\"…\"}. Prefer that over inventing REST. curl and Python \
     examples are below; local hosts may use the pp-eval CLI."
  ; "Knowledge wiki (Karpathy loop): (wiki/ingest-from-source …), \
     (wiki/query …), (wiki/lint); wiki links are [[slug]] on outline bodies."
  ; "Binary media (images/PDF/audio): do NOT push multi-KiB bytes through /api/eval \
     (64 KiB cap). Use POST /api/media (Basic auth; raw body, JSON data_b64, or \
     multipart field name=file; 5 MiB images/pdf, 64 MiB audio/*) or \
     scripts/pp_media.py. Browser UI: \
     GET /media + POST /media/upload. Serve via GET /media/<id> (supports \
     HTTP Range for audio seek). Meta via \
     (media-meta id) / (media-list); tiny fixtures only via (media-put …)."
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
      ; ("version", `Int 3)
      ; ( "philosophy"
        , `String philosophy )
      ; ( "live_docs"
        , `Assoc
            [ ( "agents_note"
              , `Assoc
                  [ ("slug", `String "agents")
                  ; ( "how"
                    , `String
                        "(notes/doc/get \"agents\") — living AGENTS.md \
                         for this habitat; agents may notes/doc/upsert it."
                    )
                  ; ("browser_path", `String "/notes?slug=agents")
                  ] )
            ; ( "wiki_schema"
              , `Assoc
                  [ ("slug", `String "wiki-schema")
                  ; ( "how"
                    , `String
                        "(wiki/schema) or (notes/doc/get \"wiki-schema\"). \
                         LLM preamble for wiki ingest/query/lint."
                    )
                  ] )
            ] )
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
            ; ( "media_upload"
              , `Assoc
                  [ ("method", `String "POST")
                  ; ("path", `String "/api/media")
                  ; ("auth", `String "required")
                  ; ( "content_type"
                    , `String
                        "image/*|application/pdf (raw body); application/json with data_b64; \
                         or multipart/form-data field name=file" )
                  ; ( "max_bytes"
                    , `Int 5242880 )
                  ; ( "headers"
                    , `Assoc
                        [ ( "X-Filename"
                          , `String "optional original filename (raw body mode)" )
                        ] )
                  ; ( "response_ok"
                    , `Assoc
                        [ ("ok", `Bool true)
                        ; ("id", `String "uuid")
                        ; ("url", `String "/media/<uuid>")
                        ; ("content_type", `String "image/png")
                        ; ("byte_size", `Int 1234)
                        ; ("sha256", `String "hex")
                        ] )
                  ; ( "client"
                    , `String "scripts/pp_media.py put ./shot.png" )
                  ] )
            ; ( "media_get"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/media/<uuid>")
                  ; ("auth", `String "required")
                  ; ("returns", `String "raw bytes with stored Content-Type")
                  ] )
            ; ( "media_list"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/api/media")
                  ; ("auth", `String "required")
                  ; ("returns", `String "JSON meta only (no bytes)")
                  ] )
            ; ( "media_browser"
              , `Assoc
                  [ ("method", `String "GET")
                  ; ("path", `String "/media")
                  ; ("auth", `String "session or basic")
                  ; ( "upload"
                    , `String "POST /media/upload multipart field name=file" )
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
            ; ( "discovery"
              , `Assoc
                  [ ( "list_packages"
                    , `String "(list-packages) → top-level package prefixes" )
                  ; ( "describe"
                    , `String
                        "(describe \"todo\") or (describe \"todo/item/add\")" )
                  ; ( "package_bindings"
                    , `String
                        "(package-bindings \"todo\") → all names under todo/" )
                  ; ( "package_exports"
                    , `String
                        "(package-exports \"todo\") → shallow pkg/local names" )
                  ; ( "apropos_docs"
                    , `String
                        "(apropos-docs \"todo/item\") → ((name doc) …)" )
                  ; ( "function_doc"
                    , `String "(function-doc \"name\") → docstring | ()" )
                  ; ( "repl_client"
                    , `String
                        "Build client: HTTP Basic + POST /api/eval JSON {expr}. See examples.curl / examples.python; local pp-eval CLI." )
                  ] )
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
    ; "### Delegation: act AS your human (on-behalf-of tokens)"
    ; ""
    ; "Preferred for a designated agent that works inside the owner's"
    ; "data space (logging workouts, todos, notes as them): the OWNER"
    ; "mints a delegated token once and hands it to the agent:"
    ; ""
    ; "  (mint-delegated-token \"<agent-username>\" \"label\")"
    ; ""
    ; "Returns the raw token ONCE. The agent then authenticates with"
    ; "Authorization: Bearer <token> instead of Basic. Effects (auth."
    ; "delegation-tokens):"
    ; ""
    ; "- (whoami) returns the OWNER; rows are created as the owner"
    ; "  (author=owner, owner's datum space) — (current-actor) returns"
    ; "  the agent, for field-level provenance."
    ; "- DATA ONLY: the session may not persist code. define/route,"
    ; "  load-library of non-seeded packages, reseed-package, deftype,"
    ; "  and checkout/fork/merge are denied; no admin capability even"
    ; "  when the owner is admin."
    ; "- Revocation: (list-delegated-tokens) on the owner's session,"
    ; "  then (revoke-delegated-token \"<id>\"); takes effect on the"
    ; "  holder's next request."
    ; ""
    ; "Owners: prefer this over handing out your own api_key/password —"
    ; "it is scoped to data and carries provenance."
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
    ; "     (todo/list)"
    ; "2. HTTP HANDLERS — read (request-method)/(request-param ...);"
    ; "     from /api/eval they return: no active request"
    ; "     Examples: bible/chapter-reader, todo/page, notes/page, */page."
    ; "     That message is expected. Call data functions, or hit the route"
    ; "     in a browser / with a real HTTP request."
    ; ""
    ; "## Selective discovery (REPL)"
    ; ""
    ; "Do not dump (list-bindings) into the model context (hundreds of names)."
    ; "Expand one package or name at a time:"
    ; ""
    ; "  (list-packages)"
    ; "  (describe \"todo\")              ; package summary + shallow exports"
    ; "  (package-exports \"todo\")       ; shallow pkg/local names"
    ; "  (package-bindings \"todo\")      ; ALL names under todo/ (any depth)"
    ; "  (apropos-docs \"todo/item\")     ; ((name doc) …)"
    ; "  (function-doc \"todo/item/add\")"
    ; "  (function-source \"todo/item/add\")"
    ; "  (function-ast \"todo/item/add\")"
    ; "  (read-string \"(lambda (x) x)\")  ; parse LLM Lisp text to data"
    ; "  (package-exports \"std\")         ; frozen kernel/std surface"
    ; ""
    ; "Human browser: " ^ origin ^ "/ref (packages) or " ^ origin ^ "/ref?q=todo"
    ; ""
    ; "## How to build a REPL client"
    ; ""
    ; "The image is the API. Your client only needs:"
    ; "  1. Base URL (this host)"
    ; "  2. HTTP Basic username/password (from a human; never in this file)"
    ; "  3. POST /api/eval with JSON body {\"expr\":\"(whoami)\"}"
    ; "  4. Parse JSON {ok, value|error}"
    ; ""
    ; "curl:"
    ; ""
    ; "```"
    ; curl_example origin
    ; "```"
    ; ""
    ; "Python:"
    ; ""
    ; "```"
    ; python_example origin
    ; "```"
    ; ""
    ; "Local laptop: OCaml pp-eval CLI (see repo skill pricklypear-eval) or"
    ; "scripts/pp-eval.sh after sourcing ~/.config/pricklypear/env."
    ; ""
    ; "## Symbols vs strings (code-as-data)"
    ; ""
    ; "Quoted bare names are symbols, not strings:"
    ; "  (symbol? 'lambda) => #t"
    ; "  (string? \"lambda\") => #t"
    ; "  (symbol=? (car (read-string \"(lambda (x) x)\")) 'lambda) => #t"
    ; "Walk trees with symbol=?, not string-eq on car."
    ; "  (string-eq (symbol->string (car form)) \"lambda\") is OK if needed."
    ; "CRUD still accepts 'type-name (symbols coerced at the DB boundary)."
    ; ""
    ; "## Application libraries"
    ; ""
    ; "- std, ui, layout, welcome — shell / HTML helpers"
    ; "- todo, notes, outline, plan, contacts, bookmarks, chat"
    ; "- calendar, flashcards, bible, dabar, weather, signup, ai"
    ; "Names bind lazily from your branch chain — no load step."
    ; "(load-library \"pkg\") is the deploy/repair tool: it re-evals the "
    ^ "RELEASE file onto the CURRENT branch and warns on non-main (it "
    ^ "pins bodies that differ). Never in repair scripts or handlers."
    ; ""
    ; "## Live agents note (habitat AGENTS.md)"
    ; ""
    ; "After (load-library \"notes\"), read:"
    ; ""
    ; "    (notes/doc/get \"agents\")"
    ; ""
    ; "Browser: " ^ origin ^ "/notes?slug=agents"
    ; "This note is the living agent playbook for this habitat. Agents"
    ; "SHOULD update it (notes/doc/upsert) when they learn durable"
    ; "conventions, new libraries, or wiki workflow changes — same as"
    ; "editing AGENTS.md in a git repo, but durable in Postgres on the"
    ; "current branch. Also see (notes/doc/get \"wiki-schema\") for the"
    ; "wiki LLM preamble."
    ; ""
    ; "## Notes / wiki (image is the API)"
    ; ""
    ; "    (load-library \"notes\")"
    ; "    (notes/doc/list)"
    ; "    (notes/doc/get \"pricklypear\")"
    ; "    (roam/backlinks \"pricklypear\")"
    ; "    (wiki/ingest-from-source \"…raw source text…\")"
    ; "    (wiki/query \"…\")"
    ; "    (wiki/lint)"
    ; ""
    ; "Outline nodes support op=eval (Lisp with self/doc/slug bound)."
    ; "Wiki links in bodies: [[slug]] or [[slug|label]]."
    ; ""
    ; "## Media upload (images / PDF / files)"
    ; ""
    ; "Do NOT push multi-KiB binaries through POST /api/eval — the eval"
    ; "body cap is 64 KiB. Use the dedicated media endpoints instead."
    ; ""
    ; "### Write (auth required — HTTP Basic or session)"
    ; ""
    ; "POST " ^ origin ^ "/api/media"
    ; "  Max body: 5 MiB (images/pdf), 64 MiB (audio/* and"
    ; "  application/octet-stream — the files-flavor carrier for"
    ; "  whole-file sync via scripts/pp_files.py)."
    ; "  Allowed Content-Types:"
    ; "    image/png image/jpeg image/gif image/webp image/svg+xml"
    ; "    application/pdf"
    ; "    audio/mpeg audio/ogg audio/flac audio/wav audio/mp4"
    ; "    audio/aac audio/webm"
    ; "    application/octet-stream"
    ; ""
    ; "Body modes:"
    ; "  1. Raw binary — set Content-Type to the media type;"
    ; "     optional header X-Filename: original-name.png"
    ; "  2. JSON — Content-Type: application/json"
    ; "       {\"content_type\":\"image/png\",\"data_b64\":\"…\","
    ; "        \"filename\":\"shot.png\"}   // filename optional"
    ; "  3. Multipart — Content-Type: multipart/form-data"
    ; "       field name=file (or name=media) with filename + part"
    ; "       Content-Type"
    ; ""
    ; "Success (201):"
    ; "  {\"ok\":true,\"id\":\"<uuid>\",\"url\":\"/media/<uuid>\","
    ; "   \"content_type\":\"image/png\",\"byte_size\":N,\"sha256\":\"…\"}"
    ; ""
    ; "### Read / list / delete"
    ; ""
    ; "GET  " ^ origin ^ "/media/<uuid>     → raw bytes (auth; Range/206 for audio seek)"
    ; "GET  " ^ origin ^ "/api/media        → JSON meta list (no bytes)"
    ; "GET  " ^ origin ^ "/api/media/by-sha256/<64-hex> → meta if you already"
    ; "                              hold those bytes (files-lens dedup probe)"
    ; "DELETE " ^ origin ^ "/api/media/<uuid> → author or admin"
    ; "GET  " ^ origin ^ "/media           → browser gallery + upload form"
    ; "POST " ^ origin ^ "/media/upload    → browser multipart form"
    ; ""
    ; "### Client helper"
    ; ""
    ; "    # same env as pp_eval: PP_URL PP_USER PP_PASS"
    ; "    python3 scripts/pp_media.py put ./shot.png"
    ; "    python3 scripts/pp_media.py get <uuid> -o out.png"
    ; "    python3 scripts/pp_media.py list"
    ; "    python3 scripts/pp_media.py delete <uuid>"
    ; ""
    ; "### Whole-directory file sync (files-lens)"
    ; ""
    ; "    python3 scripts/pp_files.py init ~/ppfiles    # once"
    ; "    python3 scripts/pp_files.py sync  ~/ppfiles   # pull+push, conflict policy in borge/storage.borg"
    ; ""
    ; "### curl (raw PNG)"
    ; ""
    ; "    curl -sS -u USER:PASS -X POST " ^ origin ^ "/api/media \\"
    ; "      -H 'Content-Type: image/png' -H 'X-Filename: shot.png' \\"
    ; "      --data-binary @shot.png"
    ; ""
    ; "### curl (multipart)"
    ; ""
    ; "    curl -sS -u USER:PASS -F 'file=@shot.png;type=image/png' \\"
    ; "      " ^ origin ^ "/api/media"
    ; ""
    ; "### Lisp (meta only — not bulk upload)"
    ; ""
    ; "    (media-meta \"<uuid>\")     ; JSON meta or ()"
    ; "    (media-list)               ; your recent meta"
    ; "    (media-url \"<uuid>\")      ; \"/media/<uuid>\""
    ; "    (media-delete \"<uuid>\")"
    ; "    (media-put \"image/png\" b64 [filename])  ; ≤256 KiB only"
    ; ""
    ; "Embed in a note node body (wiki syntax):"
    ; "  [[media:<uuid>]]"
    ; "  [[media:<uuid>|alt text]]"
    ; "Renders <img src=\"/media/<uuid>\"> when the note is viewed (auth"
    ; "required to fetch the image). Ownership is the uploading author."
    ; ""
    ; "Reference media from notes/HTML with the returned /media/<id> URL"
    ; "(auth required to fetch)."
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
    ; "repro expressions. Durable agent guidance belongs in the live"
    ; "agents note (slug agents), not only in git AGENTS.md."
    ; ""
    ; "## Related public surfaces"
    ; ""
    ; "- Welcome: " ^ origin ^ "/welcome"
    ; "- Source browser: " ^ origin ^ "/code"
    ; "- Book PDF: " ^ origin ^ "/book.pdf"
    ; "- Health: " ^ origin ^ "/health"
    ; "- Interactive REPL UI (auth required): " ^ origin ^ "/repl"
    ; "- Private agent portal (auth required): " ^ origin ^ "/agent"
    ; "- Media gallery / upload (auth required): " ^ origin ^ "/media"
    ; ""
    ; "## What not to do"
    ; ""
    ; "- Do not open or rely on the raw TCP REPL port from the internet."
    ; "- Do not invent per-app REST endpoints."
    ; "- Do not put secrets into (define …) forms or public routes."
    ; "- Do not treat \"no active request\" as a host crash — call a data fn."
    ; "- Do not base64 multi-MiB images into /api/eval — use POST /api/media."
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
