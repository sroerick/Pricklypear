(** Pricklypear Image — HTTP shared infrastructure.
    Responses, chrome, forms, auth gate helpers, login/admin.
    Page handlers live in [Http_pages]; dispatch in [Http]. *)
open Pure_html

let default_port = 8080

(* ------------------------------------------------------------------
   PEER-DISCONNECT SAFETY (image.http-robustness invariants 1-2).

   httpaf raises Failure("cannot write to closed writer") when you write
   to a response whose client has already disconnected. Under real
   traffic (people aborting page loads) an uncaught instance of this
   exception killed the single-image process repeatedly (prod incident
   2026-07-16). Every response write must tolerate a gone peer.
   Promoted from http_agent.ml's SSE-only guard; shared by all response
   helpers + the httpaf error_handler. *)
let is_closed_writer_exn (exn : exn) : bool =
  match exn with
  | Failure msg ->
      String.equal msg "cannot write to closed writer"
      || String.starts_with ~prefix:"cannot write to closed writer" msg
  | _ -> false

(** Write a response body string, swallowing the closed-writer Failure
    that httpaf raises when the peer has gone. Returns () either way. *)
let safe_respond_with_string (reqd : Httpun.Reqd.t)
    (resp : Httpun.Response.t) (body : string) : unit =
  try Httpun.Reqd.respond_with_string reqd resp body
  with exn when is_closed_writer_exn exn -> ()

(** Close a body writer, swallowing the closed-writer Failure. *)
let safe_close_writer (body : Httpun.Body.Writer.t) : unit =
  try Httpun.Body.Writer.close body
  with exn when is_closed_writer_exn exn -> ()

(* ------------------------------------------------------------------
   ACCEPT-SIDE CONNECTION CAP (image.http-robustness invariant 5).

   Bounds in-flight requests so a traffic spike can't exhaust fds or
   memory. Default 256; configurable via PP_HTTP_MAX_INFLIGHT. Requests
   beyond the cap are rejected with 503 instead of being accepted into
   an unbounded queue. This is a safety valve, not a throughput target. *)
let max_inflight =
  match Sys.getenv_opt "PP_HTTP_MAX_INFLIGHT" with
  | Some s -> (match int_of_string_opt s with Some n when n > 0 -> n | _ -> 256)
  | None -> 256

(* Counting gate for concurrent in-flight page work (Eio floor).
   Overflow waits for a slot rather than consuming unbounded memory/fds. *)
type inflight_gate = {
  mutable count : int;
  limit : int;
  mutex : Eio.Mutex.t;
  cond : Eio.Condition.t;
}

let inflight_sem =
  { count = 0; limit = max_inflight;
    mutex = Eio.Mutex.create (); cond = Eio.Condition.create () }

let inflight_acquire () : unit =
  Eio.Mutex.lock inflight_sem.mutex;
  while inflight_sem.count >= inflight_sem.limit do
    Eio.Condition.await inflight_sem.cond inflight_sem.mutex
  done;
  inflight_sem.count <- inflight_sem.count + 1;
  Eio.Mutex.unlock inflight_sem.mutex

let inflight_release () : unit =
  Eio.Mutex.lock inflight_sem.mutex;
  inflight_sem.count <- inflight_sem.count - 1;
  Eio.Condition.broadcast inflight_sem.cond;
  Eio.Mutex.unlock inflight_sem.mutex

let emergency_css : string =
  (* Labeled recovery only — NOT the product design system.
     Product CSS comes from Lisp (ui/css) via Session.ui_css. *)
  "/* emergency-css */\n\
   body{font-family:ui-monospace,Menlo,monospace;margin:1rem;\n\
   background:#fff3cd;color:#1a1a1a;line-height:1.4}\n\
   .emergency-banner{display:block;width:100%;padding:.4rem .55rem;\n\
   border:2px solid #8b0000;background:#fff3cd;color:#1a1a1a;\n\
   font-size:.85rem;font-family:Courier,monospace;margin:.35rem 0}\n\
   .emergency-banner a{color:#000080}\n\
   nav.emergency-nav{width:100%;margin-top:.35rem}\n\
   .emergency-home{border:2px solid #8b0000;background:#fff3cd;padding:.75rem}\n\
   a{color:#000080}\n\
   header{border-bottom:2px solid #000;padding:.5rem;background:#fff}\n\
   main{padding:.75rem}\n\
   .card{border:2px solid #000;background:#fff;padding:.75rem;margin:.5rem 0}\n\
   .err{color:#8b0000;font-weight:700}\n\
   .ok{color:#006600;font-weight:700}\n\
   button,.btn,input[type=submit]{font:inherit;padding:.45rem .75rem;\n\
   border:2px solid #000;background:#eee;cursor:pointer}\n\
   table{border-collapse:collapse;width:100%}\n\
   th,td{border:1px solid #808080;padding:.35rem}\n\
   pre,textarea,input[type=text],input[type=password]{\n\
   font-family:ui-monospace,Menlo,monospace;width:100%;\n\
   box-sizing:border-box;border:1px solid #000;padding:.4rem}\n\
   .badge.impact{background:#ffe8a3;border:1px solid #8b6914;padding:.15rem .4rem}\n\
   .bb-browser{display:flex;flex-direction:column;gap:.75rem}\n\
   .bb-three{display:block}\n\
   .bb-three .bb-col{margin:.5rem 0}\n\
   .bb-list{list-style:none;padding:0;margin:.25rem 0}\n\
   .bb-item a{display:block;padding:.2rem .35rem;text-decoration:none;\n\
   border-left:3px solid transparent}\n\
   .bb-item.is-active a{font-weight:700;background:#ece9e0;border-left:3px solid #000}\n\
   .bb-source{max-height:55vh;overflow:auto;padding:.6rem;border:1px solid #000}\n\
   @media (min-width:900px){\n\
   .bb-three{display:grid;grid-template-columns:12rem 14rem 1fr;gap:.75rem;\n\
   align-items:stretch;min-height:16rem}\n\
   .bb-three .bb-col{max-height:42vh;overflow:auto;margin:0}\n\
   }\n\
   "

let html_response (reqd : Httpun.Reqd.t) ?(status = `OK)
    ?session ?(css : string option) (body : Pure_html.node) : unit =
  let css =
    match css with
    | Some c when String.trim c <> "" -> c
    | _ ->
        (match session with
         | Some s ->
             (match Session.ui_css s with
              | Some c when String.trim c <> "" -> c
              | _ -> emergency_css)
         | None -> emergency_css)
  in
  let html_doc = HTML.html [HTML.lang "en"] [
    HTML.head [] [
      HTML.title [] "Pricklypear";
      HTML.meta [HTML.charset "utf-8"];
      (* Mobile-first: prevent tiny text, enable proper layout *)
      HTML.meta [HTML.name "viewport";
                 HTML.content "width=device-width, initial-scale=1"];
      HTML.style [] "%s" css;

      (* htmx for create-in-place editor empty state *)
      HTML.script [HTML.src "https://unpkg.com/htmx.org@1.9.12"] "";
      (* Close nav dropdowns on outside click / when another opens *)
      HTML.script [] "%s"
        ("document.addEventListener('click',function(e){" ^
         (* Outline: only .ol-fold on the summary toggles collapse. *)
         "var sum=e.target&&e.target.closest?e.target.closest('summary.ol-sum'):null;" ^
         "if(sum&&!(e.target.closest&&e.target.closest('.ol-fold'))){e.preventDefault();}" ^
         "document.querySelectorAll('details.menu-drop[open],details.ol-actions[open]').forEach(function(d){" ^
         "if(!d.contains(e.target))d.removeAttribute('open');});});" ^
         "document.addEventListener('toggle',function(e){" ^
         "var d=e.target;if(!d||!d.classList)return;" ^
         "var cls=d.classList.contains('menu-drop')?'menu-drop':" ^
         "(d.classList.contains('ol-actions')?'ol-actions':null);" ^
         "if(!cls||!d.open)return;" ^
         "document.querySelectorAll('details.'+cls+'[open]').forEach(function(o){" ^
         "if(o!==d)o.removeAttribute('open');});},true);");
    ];
    HTML.body [] [body];
  ] in
  let s = Pure_html.to_string html_doc in
  let headers =
    Httpun.Headers.of_list
      [("content-type", "text/html; charset=utf-8");
       ("content-length", string_of_int (String.length s))]
  in
  let resp = Httpun.Response.create ~headers status in
  safe_respond_with_string reqd resp s

let text_response (reqd : Httpun.Reqd.t) ?(status = `OK) ~content_type (s : string) : unit =
  let headers =
    Httpun.Headers.of_list
      [("content-type", content_type);
       ("content-length", string_of_int (String.length s))]
  in
  let resp = Httpun.Response.create ~headers status in
  safe_respond_with_string reqd resp s

let json_response (reqd : Httpun.Reqd.t) ?(status = `OK) (j : Yojson.Safe.t) : unit =
  let s = Yojson.Safe.to_string j in
  text_response reqd ~status ~content_type:"application/json" s

(** Labeled emergency nav only — NOT a product menu twin.
    Product chrome is Lisp (ui/nav). See image.system-browser. *)
let emergency_nav_nodes () : Pure_html.node list =
  [ HTML.span [HTML.class_ "emergency-banner"]
      [ txt "EMERGENCY NAV — ui/nav missing or failed · "
      ; HTML.a [HTML.href "/bindings?pkg=ui"] [txt "fix ui shell"]
      ; txt " · "
      ; HTML.a [HTML.href "/"] [txt "home"]
      ; txt " · "
      ; HTML.a [HTML.href "/login"] [txt "login"]
      ; txt " · "
      ; HTML.a [HTML.href "/health"] [txt "health"]
      ]
  ]

let value_to_html_string (v : Pricklypear_lisp.Ast.value) : string =
  match Primitives.is_raw_html v with
  | Some s -> s
  | None ->
      (match v with
       | Pricklypear_lisp.Ast.VStr s -> s
       | _ -> Pricklypear_lisp.Reader.pp_value v)

let try_ui_fn (session : Session.t) (name : string) : string option =
  (try
      let r = Session.call_handler session name in
      match r with
      | Session.Handler_ok v -> (Some (value_to_html_string v))
      | Session.Handler_unbound ->
          Printf.eprintf "[http] try_ui_fn %s: unbound\n%!" name;
          None
      | Session.Handler_error msg ->
          Printf.eprintf "[http] try_ui_fn %s: %s\n%!" name msg;
          None
    with exn ->
      Printf.eprintf "[http] try_ui_fn %s failed: %s\n%!"
        name (Printexc.to_string exn);
      None)

(** Fragment / partial: no document chrome (htmx swap target). *)
let is_partials_path (path : string) : bool =
  String.length path >= 10 && String.sub path 0 10 = "/partials/"

let header_list (req : Httpun.Request.t) : (string * string) list =
  Httpun.Headers.to_list req.headers
  |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))

let wants_fragment ~(path : string) ~(headers : (string * string) list) : bool =
  if is_partials_path path then true
  else
    match List.assoc_opt "hx-request" headers with
    | Some v when v <> "" && String.lowercase_ascii v <> "false" -> true
    | _ -> false

(* ---------- Auth (single-image Option B: in-process sessions) ---------- *)

let cookie_name = "pp_session"

(** Per-user subdomains (auth.user-subdomains). Empty PP_BASE_DOMAIN = off. *)
let base_domain () =
  match Sys.getenv_opt "PP_BASE_DOMAIN" with
  | Some s when String.trim s <> "" -> Some (String.lowercase_ascii (String.trim s))
  | _ -> None

let host_header (headers : (string * string) list) : string option =
  let rec find = function
    | [] -> None
    | (k, v) :: _ when String.lowercase_ascii k = "host" ->
        let host =
          match String.index_opt v ':' with
          | Some i -> String.sub v 0 i
          | None -> v
        in
        Some (String.lowercase_ascii host)
    | _ :: rest -> find rest
  in
  find headers

type host_mode =
  | Host_legacy
  | Host_apex
  | Host_garden of string
  | Host_unknown

let username_dns_ok (name : string) : bool =
  let n = String.length name in
  n > 0 && n <= 63
  && (match name.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
  && (match name.[n - 1] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
  &&
  let rec loop i =
    if i >= n then true
    else
      match name.[i] with
      | 'a' .. 'z' | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> false
  in
  loop 0

let classify_host (headers : (string * string) list) : host_mode =
  match base_domain (), host_header headers with
  | None, _ -> Host_legacy
  | Some base, None -> Host_apex
  | Some base, Some host ->
      if host = base then Host_apex
      else
        let suffix = "." ^ base in
        let hl = String.length host
        and sl = String.length suffix in
        if hl > sl && String.sub host (hl - sl) sl = suffix then
          let name = String.sub host 0 (hl - sl) in
          if username_dns_ok name && not (String.contains name '.') then
            Host_garden name
          else Host_unknown
        else Host_unknown

let garden_url ~(username : string) ~(path : string) : string =
  match base_domain () with
  | None -> path
  | Some base ->
      let p = if path = "" then "/" else path in
      "https://" ^ username ^ "." ^ base ^ p

(** Serialize request identity attach — cotenant multi-user shares one
    Session.t; concurrent Alice/Bob would otherwise race branch/env. *)
let request_mutex = Eio.Mutex.create ()

let auth_disabled () : bool =
  match Sys.getenv_opt "PP_AUTH_DISABLED" with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false

let has_prefix (path : string) (prefix : string) : bool =
  path = prefix
  || (String.length path > String.length prefix
      && String.sub path 0 (String.length prefix) = prefix
      && path.[String.length prefix] = '/')

let is_system_hard_public (path : string) : bool =
  path = "/login"
  || path = "/health"
  || path = "/logout"
  || path = "/welcome"
  || path = "/background"
  || path = "/roadmap"
  || path = "/signup"
  || path = "/guestbook"
  || path = "/book.pdf"
  || path = "/src.tgz"
  || path = "/code"
  || has_prefix path "/code"
  || path = "/assets"
  || has_prefix path "/assets"
  || path = "/.well-known/agent.json"
  || path = "/agent.txt"

(** Kernel paths Lisp cannot open to the world (auth.route-visibility). *)
let is_system_hard_private (path : string) : bool =
  let prefixes =
    [ "/admin"
    ; "/analytics"
    ; "/data"
    ; "/api"
    ; "/media"
    ; "/bindings"
    ; "/binding"
    ; "/edit"
    ; "/repl"
    ; "/branches"
    ; "/types"
    ; "/pages"
    ; "/page"
    ; "/inspect"
    ; "/agent"
    ]
  in
  List.exists
    (fun p ->
      path = p
      || (String.length path > String.length p
          && String.sub path 0 (String.length p) = p
          && path.[String.length p] = '/'))
    prefixes
  || path = "/"

(* Back-compat name used only for hard public. *)
let is_public_path (path : string) : bool = is_system_hard_public path

type path_class =
  [ `Hard_public | `Hard_private | `User of string * string | `Unknown ]

(** Classify path against a routes table. The table is the per-request
    snapshot when called post-gate (image.per-request-context), or the
    live Session.routes when called under the request_mutex (pre-gate —
    attach has not run for THIS request, but the mutex excludes concurrent
    rehydrate, so live is correct there). Returns `User (handler, visibility). *)
let classify_path_with_routes
    (routes : (string, string * string) Hashtbl.t) (path : string) : path_class =
  if is_system_hard_public path then `Hard_public
  else if is_system_hard_private path then `Hard_private
  else
    match Hashtbl.find_opt routes path with
    | Some (handler, vis) -> `User (handler, vis)
    | None -> `Unknown

(** Classify path for the request gate.
    Returns [`Hard_public | `Hard_private | `User of string * string
    | `Unknown] where User is (handler, visibility). *)
let classify_path (session : Session.t) (path : string) : path_class =
  classify_path_with_routes (Session.routes session) path

let parse_cookies (headers : (string * string) list) : (string * string) list =
  match List.assoc_opt "cookie" headers with
  | None -> []
  | Some raw ->
      String.split_on_char ';' raw
      |> List.filter_map (fun part ->
             let part = String.trim part in
             match String.index_opt part '=' with
             | None -> None
             | Some i ->
                 let k = String.trim (String.sub part 0 i) in
                 let v =
                   String.trim
                     (String.sub part (i + 1) (String.length part - i - 1))
                 in
                 if k = "" then None else Some (k, v))

let cookie_value (headers : (string * string) list) (name : string)
  : string option =
  List.assoc_opt name (parse_cookies headers)

let b64_decode_opt (s : string) : string option =
  match Base64.decode ~pad:true s with
  | Ok raw -> Some raw
  | Error _ ->
      (match Base64.decode ~pad:false s with
       | Ok raw -> Some raw
       | Error _ -> None)

(** Parse "Basic base64(user:pass)" Authorization header. *)
let parse_basic_auth (headers : (string * string) list)
  : (string * string) option =
  match List.assoc_opt "authorization" headers with
  | None -> None
  | Some v ->
      let v = String.trim v in
      if String.length v < 7 then None
      else
        let scheme = String.lowercase_ascii (String.sub v 0 5) in
        if scheme <> "basic" then None
        else
          let b64 = String.trim (String.sub v 5 (String.length v - 5)) in
          match b64_decode_opt b64 with
          | None -> None
          | Some decoded ->
              (match String.index_opt decoded ':' with
               | None -> None
               | Some i ->
                   let u = String.sub decoded 0 i in
                   let p =
                     String.sub decoded (i + 1)
                       (String.length decoded - i - 1)
                   in
                   Some (u, p))

(** Parse an Authorization: Bearer <token> header. Returns the raw token. *)
let parse_bearer_auth (headers : (string * string) list) : string option =
  match List.assoc_opt "authorization" headers with
  | None -> None
  | Some v ->
      let v = String.trim v in
      if String.length v < 8 then None
      else
        let scheme = String.lowercase_ascii (String.sub v 0 6) in
        if scheme <> "bearer" then None
        else Some (String.trim (String.sub v 6 (String.length v - 6)))

let set_cookie_header ~(token : string) : string =
  (* Dev is plain HTTP — omit Secure. Path=/; HttpOnly; SameSite=Lax.
     PP_BASE_DOMAIN scopes cookie across subdomains. *)
  let domain =
    match base_domain () with
    | Some b -> "; Domain=." ^ b
    | None -> ""
  in
  Printf.sprintf "%s=%s; Path=/; HttpOnly; SameSite=Lax; Max-Age=%d%s"
    cookie_name token (60 * 60 * 24 * 14) domain

let clear_cookie_header () : string =
  let domain =
    match base_domain () with
    | Some b -> "; Domain=." ^ b
    | None -> ""
  in
  Printf.sprintf
    "%s=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0%s" cookie_name domain

let redirect_with_cookie (reqd : Httpun.Reqd.t) ~(location : string)
    ~(set_cookie : string) : unit =
  let headers =
    Httpun.Headers.of_list
      [ ("location", location)
      ; ("set-cookie", set_cookie)
      ; ("content-length", "0") ]
  in
  let resp = Httpun.Response.create ~headers `See_other in
  safe_respond_with_string reqd resp ""

let unauthorized_api (reqd : Httpun.Reqd.t) : unit =
  json_response reqd ~status:`Unauthorized
    (`Assoc
       [ ("ok", `Bool false)
       ; ("error", `String "authentication required") ])

(** Resolve identity for this request: cookie session, then Basic auth.
    Returns None if unauthenticated. *)
let resolve_identity (session : Session.t)
    ~(headers : (string * string) list)
  : Auth_store.user option =
  let store = Session.store session in
  match cookie_value headers cookie_name with
  | Some token when token <> "" ->
      let row = Auth_store.lookup_session store ~raw_token:token in
      (match row with
       | Some s ->
           (* Fetch the full user (user_type, parent_user_id) — the session
              row only carries id/username/is_admin. *)
           let user_opt = Auth_store.get_user_by_id store s.user_id in
           (match user_opt with
            | Some u -> Some u
            | None -> None)
       | None ->
           (* Fall through to Basic if cookie stale. *)
           (match parse_basic_auth headers with
            | None -> None
            | Some (u, p) ->
                Auth_store.verify_password store ~username:u ~password:p ()))
  | _ ->
      (* No cookie: try Bearer token, then Basic. *)
      (match parse_bearer_auth headers with
       | Some token when token <> "" ->
           Auth_store.verify_token store ~raw_token:token ()
       | _ ->
           (match parse_basic_auth headers with
            | None -> None
            | Some (u, p) ->
                Auth_store.verify_password store ~username:u ~password:p ()))

let attach_from_user (session : Session.t) (user : Auth_store.user)
    ?(auth_token : string option = None)
    ?(checkout_personal_now = false) () : unit =
  Session.attach_identity session
    ~user_id:user.Auth_store.id
    ~username:user.Auth_store.username
    ~is_admin:user.Auth_store.is_admin
    ~auth_token
    ~checkout_personal_now
    ~user_type:user.Auth_store.user_type
    ~parent_user_id:user.Auth_store.parent_user_id
    ()

let fragment_response (reqd : Httpun.Reqd.t) ?(status = `OK) (html : string)
    : unit =
  text_response reqd ~status ~content_type:"text/html; charset=utf-8" html

let chrome ?nav_html ~(session : Session.t)
    ?(manifest = Session.manifest session) ~(title : string)
    (content : Pure_html.node list) : Pure_html.node =
  let nav_html =
    match nav_html with
    | Some _ as s -> s
    | None -> Session.ui_nav_html session
  in
  let nav =
    match nav_html with
    | Some html when String.trim html <> "" ->
        HTML.nav [] [txt ~raw:true "%s" html]
    | _ -> HTML.nav [HTML.class_ "emergency-nav"] (emergency_nav_nodes ())
  in
  (* Metadata reads the SNAPSHOT manifest when callers pass one
     (image.per-request-context), so a concurrent attach_anonymous
     cannot paint "user=anonymous" on an authed page's header. Callers
     outside the HTTP gate fall back to the live session manifest. *)
  HTML.div [] [
    HTML.header [] [
      HTML.span [HTML.class_ "brand"] [txt "Pricklypear"];
      HTML.span [HTML.class_ "meta"] [
        txt "user=%s  branch=%s  up=%ds"
          manifest.username
          manifest.branch_name
          (Session.uptime_seconds session);
      ];
      nav;
    ];
    HTML.main [] (
      (if String.trim title = "" then content
       else HTML.h1 [] [txt "%s" title] :: content)
    );
  ]

let chrome_lwt ~(session : Session.t) ~(title : string)
    (content : Pure_html.node list) : Pure_html.node =
  let nav_html = try_ui_fn session "ui/nav" in
  (chrome ?nav_html ~session ~title content)

let read_body (reqd : Httpun.Reqd.t) : string =
  let body = Httpun.Reqd.request_body reqd in
  let promise, resolver = Eio.Promise.create () in
  let buf = Buffer.create 1024 in
  let rec sched () =
    Httpun.Body.Reader.schedule_read body
      ~on_eof:(fun () -> Eio.Promise.resolve resolver (Buffer.contents buf))
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
        sched ())
  in
  sched ();
  Eio.Promise.await promise

let url_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else match s.[i] with
    | '+' -> Buffer.add_char buf ' '; loop (i + 1)
    | '%' when i + 2 < len ->
        let hex = String.sub s (i + 1) 2 in
        (try
           let code = int_of_string ("0x" ^ hex) in
           Buffer.add_char buf (Char.chr code)
         with _ -> Buffer.add_char buf '%');
        loop (i + 3)
    | c -> Buffer.add_char buf c; loop (i + 1)
  in
  loop 0

(** application/x-www-form-urlencoded / query component encode. *)
let url_encode (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~') as c ->
          Buffer.add_char buf c
      | ' ' -> Buffer.add_string buf "%20"
      | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

(** Same-origin relative path (+ optional query/hash) for post-login return.
    Rejects scheme-relative URLs, off-site targets, and control chars.
    Colons are allowed only in the query/fragment (e.g. url=https://…),
    never in the path (blocks scheme smuggling). *)
let safe_internal_next (candidate : string) : string option =
  let s = String.trim candidate in
  let len = String.length s in
  if len = 0 || len > 2048 then None
  else if s.[0] <> '/' then None
  else if len >= 2 && s.[1] = '/' then None
  else if String.contains s '\n' || String.contains s '\r' then None
  else
    let path_part =
      match String.index_opt s '?', String.index_opt s '#' with
      | Some q, Some h -> String.sub s 0 (min q h)
      | Some q, None -> String.sub s 0 q
      | None, Some h -> String.sub s 0 h
      | None, None -> s
    in
    if String.contains path_part ':' then None else Some s

let form_field (body : string) (name : string) : string option =
  let pairs = String.split_on_char '&' body in
  let rec find = function
    | [] -> None
    | p :: rest ->
        (match String.split_on_char '=' p with
         | k :: vs ->
             if url_decode k = name then
               Some (url_decode (String.concat "=" vs))
             else find rest
         | _ -> find rest)
  in
  find pairs

let parse_form_body (body : string) : (string * string) list =
  if body = "" then []
  else
    String.split_on_char '&' body
    |> List.filter_map (fun p ->
           if p = "" then None
           else
             match String.split_on_char '=' p with
             | k :: vs ->
                 Some (url_decode k, url_decode (String.concat "=" vs))
             | _ -> None)

(** Merge query + form params; form fields win on key conflict (it33). *)
let merge_params ~(query : (string * string) list)
    ~(form : (string * string) list) : (string * string) list =
  let tbl = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) query;
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) form;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

let parse_query (target : string) : (string * string) list =
  match String.index_opt target '?' with
  | None -> []
  | Some i ->
      let q = String.sub target (i + 1) (String.length target - i - 1) in
      String.split_on_char '&' q
      |> List.filter_map (fun p ->
             match String.split_on_char '=' p with
             | k :: vs ->
                 Some (url_decode k, url_decode (String.concat "=" vs))
             | _ -> None)

let query_param (target : string) (key : string) : string option =
  List.assoc_opt key (parse_query target)

let max_user_body_bytes = 64 * 1024

let redirect_response (reqd : Httpun.Reqd.t) (location : string) : unit =
  let headers =
    Httpun.Headers.of_list
      [ ("location", location)
      ; ("content-length", "0")
      ]
  in
  let resp = Httpun.Response.create ~headers `See_other in
  safe_respond_with_string reqd resp ""

let require_login_redirect (reqd : Httpun.Reqd.t) ~(path : string)
    ~(target : string) : unit =
  if String.length path >= 5 && String.sub path 0 5 = "/api/" then
    unauthorized_api reqd
  else if path = "/" then
    (* Public host: anonymous workspace root → welcome landing. *)
    redirect_response reqd "/welcome"
  else
    match safe_internal_next target with
    | Some next when next <> "/login" && not (String.starts_with ~prefix:"/login?" next)
      ->
        redirect_response reqd ("/login?next=" ^ url_encode next)
    | _ -> redirect_response reqd "/login"

let login_form_page ?(error = "") ?(next = "") (reqd : Httpun.Reqd.t) : unit =
  let err =
    if error = "" then HTML.div [] []
    else HTML.p [HTML.class_ "err"] [txt "%s" error]
  in
  let next =
    let from_arg = String.trim next in
    let candidate =
      if from_arg <> "" then from_arg
      else
        let target = (Httpun.Reqd.request reqd).Httpun.Request.target in
        Option.value (query_param target "next") ~default:""
    in
    match safe_internal_next candidate with Some n -> n | None -> ""
  in
  let next_field =
    if next = "" then HTML.div [] []
    else
      HTML.input
        [ HTML.type_ "hidden"
        ; HTML.name "next"
        ; HTML.value "%s" next
        ]
  in
  let body =
    HTML.html [HTML.lang "en"] [
      HTML.head [] [
        HTML.title [] "Pricklypear — Sign in";
        HTML.meta [HTML.charset "utf-8"];
        HTML.meta [HTML.name "viewport";
                   HTML.content "width=device-width, initial-scale=1"];
        HTML.style [] "%s"
          "body{font-family:ui-monospace,Menlo,monospace;max-width:28rem;
           margin:4rem auto;padding:1rem;background:#f4f1ea;color:#1a1a1a}
           .card{border:2px solid #1a1a1a;background:#fff;padding:1.25rem}
           h1{font-size:1.25rem;margin:0 0 1rem}
           label{display:block;margin:.75rem 0 .25rem;font-size:.85rem}
           input[type=text],input[type=password]{width:100%;box-sizing:border-box;
           border:1px solid #1a1a1a;padding:.4rem;font:inherit}
           button{margin-top:1rem;border:2px solid #1a1a1a;background:#1a1a1a;
           color:#fff;padding:.45rem .9rem;font:inherit;cursor:pointer}
           .err{color:#8b0000;margin:.5rem 0}
           .muted{color:#555;font-size:.85rem;margin-top:1rem}";
      ];
      HTML.body [] [
        HTML.div [HTML.class_ "card"] [
          HTML.h1 [] [txt "Sign in"];
          err;
          HTML.form [HTML.method_ `POST; HTML.action "/login"] [
            next_field;
            HTML.label [HTML.for_ "username"] [txt "Username"];
            HTML.input [HTML.type_ "text"; HTML.name "username";
                        HTML.id "username"; HTML.required;
                        HTML.autocomplete `username];
            HTML.label [HTML.for_ "password"] [txt "Password"];
            HTML.input [HTML.type_ "password"; HTML.name "password";
                        HTML.id "password"; HTML.required;
                        HTML.autocomplete `current_password];
            HTML.button [HTML.type_ "submit"] [txt "Log in"];
          ];
          HTML.p [HTML.class_ "muted"] [
            txt "Your garden: username/personal after login.";
          ];
        ];
      ];
    ]
  in
  html_response reqd body

let post_login (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let username =
    Option.value (form_field body "username") ~default:"" |> String.trim
  in
  let password = Option.value (form_field body "password") ~default:"" in
  let next_raw = Option.value (form_field body "next") ~default:"" in
  let next_ok = safe_internal_next next_raw in
  if username = "" || password = "" then begin
    login_form_page ~error:"Invalid username or password"
      ?next:(match next_ok with Some n -> Some n | None -> None) reqd;
    ()
  end else
    let store = Session.store session in
    let user_opt =
      Auth_store.verify_password store ~username ~password ()
    in
    match user_opt with
    | None ->
        login_form_page ~error:"Invalid username or password"
          ?next:(match next_ok with Some n -> Some n | None -> None) reqd;
        ()
    | Some user ->
        let minted =
          Auth_store.mint_session store ~user_id:user.Auth_store.id ()
        in
        (match minted with
         | Error e ->
             login_form_page ~error:("Login failed: " ^ e)
               ?next:(match next_ok with Some n -> Some n | None -> None) reqd;
             ()
         | Ok token ->
             let () =
               attach_from_user session user ~auth_token:(Some token)
                 ~checkout_personal_now:true ()
             in
             let location =
               match next_ok with
               | Some n -> n
               | None ->
                   (match base_domain () with
                    | Some base ->
                        (* Prefer relative if already on garden host; else absolute. *)
                        let headers = header_list (Httpun.Reqd.request reqd) in
                        (match classify_host headers with
                         | Host_garden g when g = user.Auth_store.username -> "/"
                         | _ ->
                             "http://" ^ user.Auth_store.username ^ "." ^ base ^ "/")
                    | None -> "/")
             in
             redirect_with_cookie reqd ~location
               ~set_cookie:(set_cookie_header ~token);
             ())

let post_logout (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let headers = header_list (Httpun.Reqd.request reqd) in
  let token = cookie_value headers cookie_name in
  let store = Session.store session in
  let () =
    match token with
    | Some t when t <> "" -> Auth_store.revoke_session store ~raw_token:t ()
    | _ -> ()
  in
  Session.clear_auth_token session;
  redirect_with_cookie reqd ~location:"/login"
    ~set_cookie:(clear_cookie_header ());
  ()

(** One-shot display of a freshly issued API token (never stored in URL). *)
let admin_token_once_page ~(session : Session.t) ~(reqd : Httpun.Reqd.t)
    ~(username : string) ~(raw : string) : unit =
  html_response reqd ~session
    (chrome ~session ~title:"API token" [
       HTML.div [HTML.class_ "card"] [
         HTML.h2 [] [txt "Token issued"];
         HTML.p [] [
           txt "Copy now — shown once. User: ";
           HTML.code [] [txt "%s" username];
         ];
         HTML.p [] [HTML.code [] [txt "%s" raw]];
         HTML.p [HTML.class_ "muted"] [
           txt "Use as Authorization: Bearer <token> for /api/eval.";
         ];
         HTML.p [] [HTML.a [HTML.href "/admin/users"] [txt "Back to users"]];
       ];
     ])

let admin_err ~(session : Session.t) ~(reqd : Httpun.Reqd.t) ~title msg =
  html_response reqd ~session ~status:`Bad_request
    (chrome ~session ~title [
       HTML.p [HTML.class_ "err"] [txt "%s" msg];
       HTML.p [] [HTML.a [HTML.href "/admin/users"] [txt "Back"]];
     ]);
  ()

let admin_users_page (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  if not (Session.is_admin session) then begin
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"Forbidden" [
         HTML.p [HTML.class_ "err"] [txt "admin only"];
       ]);
    ()
  end else
    let store = Session.store session in
    let users = Auth_store.list_users store in
    (* Per-user promote flag + API tokens (admin agent/token surface —
       BORGE-1785120601). *)
    let user_extras =
      List.map
        (fun (u : Auth_store.user) ->
          let promote =
            if u.Auth_store.user_type = "human" then
              Auth_store.get_allow_agent_promote store ~user_id:u.id
            else false
          in
          let tokens = Auth_store.list_tokens store ~user_id:u.id in
          (u, promote, tokens))
        users
    in
    let human_names =
      List.filter_map
        (fun (u : Auth_store.user) ->
          if u.user_type = "human" then Some u.username else None)
        users
    in
    let rows =
      List.map
        (fun ((u : Auth_store.user), promote, _tokens) ->
          let promote_cell =
            if u.user_type <> "human" then
              HTML.td [] [txt "—"]
            else
              HTML.td [] [
                HTML.form
                  [ HTML.method_ `POST; HTML.action "/admin/users" ]
                  [ HTML.input
                      [ HTML.type_ "hidden"; HTML.name "action";
                        HTML.value "set_allow_agent_promote" ]
                  ; HTML.input
                      [ HTML.type_ "hidden"; HTML.name "user_id";
                        HTML.value "%s" u.id ]
                  ; HTML.input
                      [ HTML.type_ "hidden"; HTML.name "value";
                        HTML.value (if promote then "0" else "1") ]
                  ; HTML.button [HTML.type_ "submit"]
                      [ txt "%s"
                          (if promote then "on (click=off)"
                           else "off (click=on)") ]
                  ]
              ]
          in
          HTML.tr [] [
            HTML.td [] [txt "%s" u.username];
            HTML.td [] [txt "%s" u.user_type];
            HTML.td [] [txt "%s" (if u.is_admin then "yes" else "no")];
            promote_cell;
            HTML.td [] [HTML.code [] [txt "%s" u.id]];
            HTML.td [] [
              HTML.form
                [ HTML.method_ `POST; HTML.action "/admin/users" ]
                [ HTML.input
                    [ HTML.type_ "hidden"; HTML.name "action";
                      HTML.value "revoke_sessions" ]
                ; HTML.input
                    [ HTML.type_ "hidden"; HTML.name "user_id";
                      HTML.value "%s" u.id ]
                ; HTML.button [HTML.type_ "submit"] [txt "Revoke sessions"]
                ];
              HTML.form
                [ HTML.method_ `POST; HTML.action "/admin/users" ]
                [ HTML.input
                    [ HTML.type_ "hidden"; HTML.name "action";
                      HTML.value "issue_token" ]
                ; HTML.input
                    [ HTML.type_ "hidden"; HTML.name "user_id";
                      HTML.value "%s" u.id ]
                ; HTML.input
                    [ HTML.type_ "text"; HTML.name "label";
                      HTML.placeholder "label (optional)" ]
                ; HTML.button [HTML.type_ "submit"] [txt "Issue token"]
                ];
            ];
          ])
        user_extras
    in
    let token_sections =
      List.filter_map
        (fun ((u : Auth_store.user), _p, tokens) ->
          match tokens with
          | [] -> None
          | ts ->
              let trows =
                List.map
                  (fun (tm : Auth_store.token_meta) ->
                    let lab =
                      match tm.label with Some l -> l | None -> "(none)"
                    in
                    let last =
                      match tm.last_used_at with
                      | Some s -> s
                      | None -> "never"
                    in
                    HTML.tr [] [
                      HTML.td [] [HTML.code [] [txt "%s" tm.id]];
                      HTML.td [] [txt "%s" lab];
                      HTML.td [] [txt "…%s" tm.token_last4];
                      HTML.td [] [txt "%s" tm.created_at];
                      HTML.td [] [txt "%s" last];
                      HTML.td [] [
                        HTML.form
                          [ HTML.method_ `POST; HTML.action "/admin/users" ]
                          [ HTML.input
                              [ HTML.type_ "hidden"; HTML.name "action";
                                HTML.value "rotate_token" ]
                          ; HTML.input
                              [ HTML.type_ "hidden"; HTML.name "credential_id";
                                HTML.value "%s" tm.id ]
                          ; HTML.input
                              [ HTML.type_ "hidden"; HTML.name "user_id";
                                HTML.value "%s" u.id ]
                          ; HTML.button [HTML.type_ "submit"] [txt "Rotate"]
                          ];
                        HTML.form
                          [ HTML.method_ `POST; HTML.action "/admin/users" ]
                          [ HTML.input
                              [ HTML.type_ "hidden"; HTML.name "action";
                                HTML.value "revoke_token" ]
                          ; HTML.input
                              [ HTML.type_ "hidden"; HTML.name "credential_id";
                                HTML.value "%s" tm.id ]
                          ; HTML.button [HTML.type_ "submit"] [txt "Revoke"]
                          ];
                      ];
                    ])
                  ts
              in
              Some
                (HTML.div [HTML.class_ "card"] [
                   HTML.h3 [] [txt "Tokens: %s" u.username];
                   HTML.table [] [
                     HTML.thead [] [
                       HTML.tr [] [
                         HTML.th [] [txt "id"];
                         HTML.th [] [txt "label"];
                         HTML.th [] [txt "last4"];
                         HTML.th [] [txt "created"];
                         HTML.th [] [txt "last used"];
                         HTML.th [] [txt "actions"];
                       ];
                     ];
                     HTML.tbody [] trows;
                   ];
                 ]))
        user_extras
    in
    let parent_options =
      match human_names with
      | [] -> [ HTML.option [HTML.value ""] "%s" "(no humans)" ]
      | names ->
          List.map
            (fun n -> HTML.option [HTML.value "%s" n] "%s" n)
            names
    in
    let body =
      chrome ~session ~title:"Users" [
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "Users"];
          HTML.p [HTML.class_ "muted"] [
            txt
              "Agents, API tokens, and allow_agent_promote. Raw tokens are \
               shown once after issue/rotate.";
          ];
          HTML.table [] [
            HTML.thead [] [
              HTML.tr [] [
                HTML.th [] [txt "username"];
                HTML.th [] [txt "type"];
                HTML.th [] [txt "admin"];
                HTML.th [] [txt "agent promote"];
                HTML.th [] [txt "id"];
                HTML.th [] [txt "actions"];
              ];
            ];
            HTML.tbody [] rows;
          ];
        ];
        HTML.div [] token_sections;
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "Create user"];
          HTML.form [HTML.method_ `POST; HTML.action "/admin/users"] [
            HTML.input
              [ HTML.type_ "hidden"; HTML.name "action";
                HTML.value "create" ];
            HTML.p [] [
              HTML.label [] [txt "username "];
              HTML.input [HTML.type_ "text"; HTML.name "username"; HTML.required];
            ];
            HTML.p [] [
              HTML.label [] [txt "password "];
              HTML.input [HTML.type_ "password"; HTML.name "password"; HTML.required];
            ];
            HTML.p [] [
              HTML.label [] [
                HTML.input [HTML.type_ "checkbox"; HTML.name "is_admin"; HTML.value "1"];
                txt " is_admin";
              ];
            ];
            HTML.button [HTML.type_ "submit"] [txt "Create"];
          ];
        ];
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "Create agent"];
          HTML.p [HTML.class_ "muted"] [
            txt "Creates <parent>/agent (idempotent). No password — use API tokens.";
          ];
          HTML.form [HTML.method_ `POST; HTML.action "/admin/users"] [
            HTML.input
              [ HTML.type_ "hidden"; HTML.name "action";
                HTML.value "create_agent" ];
            HTML.p [] [
              HTML.label [] [txt "parent human "];
              HTML.select [HTML.name "parent_username"; HTML.required]
                parent_options;
            ];
            HTML.button [HTML.type_ "submit"] [txt "Create agent"];
          ];
        ];
        HTML.p [] [
          HTML.form [HTML.method_ `POST; HTML.action "/logout"] [
            HTML.button [HTML.type_ "submit"] [txt "Log out"];
          ];
        ];
      ]
    in
    html_response reqd ~session body;
    ()

let post_admin_users (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  if not (Session.is_admin session) then begin
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"Forbidden" [
         HTML.p [HTML.class_ "err"] [txt "admin only"];
       ]);
    ()
  end else
    let body = read_body reqd in
    let action =
      Option.value (form_field body "action") ~default:"create"
      |> String.trim
    in
    let store = Session.store session in
    let field name =
      Option.value (form_field body name) ~default:"" |> String.trim
    in
    match action with
    | "revoke_sessions" ->
        let user_id = field "user_id" in
        if user_id = "" then
          admin_err ~session ~reqd ~title:"Revoke sessions" "user_id required"
        else
          let n = Auth_store.revoke_all_sessions store ~user_id in
          let () =
            Auth_store.log_auth store ~user_id ~kind:"admin_revoke_sessions"
              ~success:true ()
          in
          ignore n;
          redirect_response reqd "/admin/users";
          ()
    | "create_agent" ->
        let parent = field "parent_username" in
        if parent = "" then
          admin_err ~session ~reqd ~title:"Create agent" "parent_username required"
        else
          let r = Auth_store.create_agent store ~parent_username:parent in
          (match r with
           | Error e -> admin_err ~session ~reqd ~title:"Create agent" e
           | Ok _u ->
               redirect_response reqd "/admin/users";
               ())
    | "issue_token" ->
        let user_id = field "user_id" in
        let label =
          match field "label" with "" -> None | s -> Some s
        in
        if user_id = "" then
          admin_err ~session ~reqd ~title:"Issue token" "user_id required"
        else
          let uopt = Auth_store.get_user_by_id store user_id in
          (match uopt with
           | None ->
               admin_err ~session ~reqd ~title:"Issue token" "no such user"
           | Some u ->
               let r =
                 Auth_store.issue_token store ~user_id ~label:label ()
               in
               (match r with
                | Error e -> admin_err ~session ~reqd ~title:"Issue token" e
                | Ok raw ->
                    admin_token_once_page ~session ~reqd
                      ~username:u.username ~raw;
                    ()))
    | "revoke_token" ->
        let credential_id = field "credential_id" in
        if credential_id = "" then
          admin_err ~session ~reqd ~title:"Revoke token"
            "credential_id required"
        else
          let r = Auth_store.revoke_token store ~credential_id in
          (match r with
           | Error e -> admin_err ~session ~reqd ~title:"Revoke token" e
           | Ok () ->
               redirect_response reqd "/admin/users";
               ())
    | "rotate_token" ->
        let credential_id = field "credential_id" in
        let user_id = field "user_id" in
        let label =
          match field "label" with "" -> None | s -> Some s
        in
        if credential_id = "" || user_id = "" then
          admin_err ~session ~reqd ~title:"Rotate token"
            "credential_id and user_id required"
        else
          let uopt = Auth_store.get_user_by_id store user_id in
          (match uopt with
           | None ->
               admin_err ~session ~reqd ~title:"Rotate token" "no such user"
           | Some u ->
               let r =
                 match label with
                 | Some s ->
                     Auth_store.rotate_token store ~credential_id ~user_id
                       ~label:s ()
                 | None ->
                     Auth_store.rotate_token store ~credential_id ~user_id ()
               in
               (match r with
                | Error e -> admin_err ~session ~reqd ~title:"Rotate token" e
                | Ok raw ->
                    admin_token_once_page ~session ~reqd
                      ~username:u.username ~raw;
                    ()))
    | "set_allow_agent_promote" ->
        let user_id = field "user_id" in
        let value =
          match field "value" with
          | "1" | "on" | "true" | "yes" -> true
          | _ -> false
        in
        if user_id = "" then
          admin_err ~session ~reqd ~title:"Agent promote" "user_id required"
        else
          let r =
            Auth_store.set_allow_agent_promote store ~user_id ~value
          in
          (match r with
           | Error e -> admin_err ~session ~reqd ~title:"Agent promote" e
           | Ok () ->
               redirect_response reqd "/admin/users";
               ())
    | "create" | _ ->
        let username = field "username" in
        let password = Option.value (form_field body "password") ~default:"" in
        let is_admin =
          match form_field body "is_admin" with
          | Some ("1" | "on" | "true") -> true
          | _ -> false
        in
        if username = "" || password = "" then
          admin_err ~session ~reqd ~title:"Create user"
            "username and password required"
        else
          let created = Auth_store.create_user store ~username ~is_admin in
          (match created with
           | Error e -> admin_err ~session ~reqd ~title:"Create user" e
           | Ok user ->
               let pw =
                 Auth_store.set_password store ~user_id:user.Auth_store.id
                   ~password
               in
               (match pw with
                | Error e ->
                    html_response reqd ~session ~status:`Internal_server_error
                      (chrome ~session ~title:"Create user" [
                         HTML.p [HTML.class_ "err"] [txt "password: %s" e];
                       ]);
                    ()
                | Ok () ->
                    let () =
                      Auth_store.log_auth store ~user_id:user.Auth_store.id
                        ~kind:"admin_create_user" ~success:true ()
                    in
                    redirect_response reqd "/admin/users";
                    ()))

(** Generate a MIME boundary that does not appear in any part body. *)
let fresh_multipart_boundary (bodies : string list) : string =
  let rec try_n n =
    if n > 8 then
      raise
        (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.User_error
              "multipart: could not choose a unique boundary"))
    else
      let b =
        Printf.sprintf "pp%x%x" (Random.bits ()) (Random.bits ())
      in
      if List.exists (fun body ->
           let rec find i =
             let bl = String.length b and n = String.length body in
             if i + bl > n then false
             else if String.sub body i bl = b then true
             else find (i + 1)
           in
           find 0)
         bodies
      then try_n (n + 1)
      else b
  in
  try_n 0

(** Frame a multipart/mixed body (eager v1). Peer-gone on write is swallowed.
    Streaming write+flush per part is available later; the reactive payoff is
    multiple targeted parts in one response. Effect-safe: no Eio.Promise
    await on the handler fiber (Continuation_already_resumed under Prim_call). *)
let multipart_response (reqd : Httpun.Reqd.t)
    (mp : Prims_pure.mime_multipart) : unit =
  let bodies = List.map (fun (p : Prims_pure.mime_part) -> p.body) mp.parts in
  let boundary = fresh_multipart_boundary bodies in
  let buf = Buffer.create 256 in
  List.iter
    (fun (p : Prims_pure.mime_part) ->
      Buffer.add_string buf ("--" ^ boundary ^ "\r\n");
      Buffer.add_string buf ("Content-Type: " ^ p.content_type ^ "\r\n");
      (match p.target with
       | Some t -> Buffer.add_string buf ("HX-Target: " ^ t ^ "\r\n")
       | None -> ());
      (match p.swap with
       | Some s -> Buffer.add_string buf ("HX-Swap: " ^ s ^ "\r\n")
       | None -> ());
      (match p.id with
       | Some i -> Buffer.add_string buf ("HX-Part-ID: " ^ i ^ "\r\n")
       | None -> ());
      Buffer.add_string buf "\r\n";
      Buffer.add_string buf p.body;
      Buffer.add_string buf "\r\n")
    mp.parts;
  Buffer.add_string buf ("--" ^ boundary ^ "--\r\n");
  let body = Buffer.contents buf in
  let headers =
    let base =
      [ ( "content-type"
        , "multipart/mixed; boundary=" ^ boundary )
      ; ("cache-control", "no-cache")
      ; ("content-length", string_of_int (String.length body))
      ]
    in
    let base =
      match mp.envelope_target with
      | Some t -> ("hx-target", t) :: base
      | None -> base
    in
    let base =
      match mp.envelope_swap with
      | Some s -> ("hx-swap", s) :: base
      | None -> base
    in
    Httpun.Headers.of_list base
  in
  let resp = Httpun.Response.create ~headers `OK in
  safe_respond_with_string reqd resp body

let respond_handler_value
    ~(session : Session.t)
    ~(reqd : Httpun.Reqd.t)
    ~(path : string)
    ~(fragment : bool)
    (value : Pricklypear_lisp.Ast.value) : unit =
  (* Pull the per-request snapshot manifest (if bound, sync via Lwt.get)
     so chrome renders this request's identity, not whatever a concurrent
     attach flipped. *)
  let manifest =
    match Request_context.get () with
    | Some ctx -> Request_context.manifest ctx
    | None -> Session.manifest session
  in
  match Primitives.is_redirect value with
  | Some loc -> redirect_response reqd loc
  | None ->
      (match Primitives.is_multipart value with
       | Some mp -> multipart_response reqd mp
       | None ->
           if fragment then
             let html =
               match Primitives.is_raw_html value with
               | Some raw -> raw
               | None ->
                   (match value with
                    | Pricklypear_lisp.Ast.VStr s -> s
                    | _ -> Pricklypear_lisp.Reader.pp_value value)
             in
             fragment_response reqd html
           else
             let body =
               match Primitives.is_raw_html value with
               | Some raw_html ->
                   chrome ~manifest ~session ~title:path
                     [ txt ~raw:true "%s" raw_html ]
               | None ->
                   (match value with
                    | Pricklypear_lisp.Ast.VStr s ->
                        chrome ~manifest ~session ~title:path
                          [ HTML.p [] [txt "%s" s] ]
                    | _ ->
                        let rendered =
                          Pricklypear_lisp.Reader.pp_value value
                        in
                        chrome ~manifest ~session ~title:path [
                          HTML.pre [] [txt "%s" rendered];
                        ])
             in
             html_response reqd ~session body)

(** /settings/keys — per-user LLM provider key (auth.user-llm-keys). *)
let settings_keys_page (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  let username = Session.username session in
  if username = "anonymous" then begin
    redirect_response reqd "/login";
    ()
  end else
    let user_opt =
      Auth_store.get_user_by_username (Session.store session) username
    in
    match user_opt with
    | None ->
        html_response reqd ~session ~status:`Unauthorized
          (chrome ~session ~title:"Keys" [
             HTML.p [HTML.class_ "err"] [txt "not signed in"];
           ]);
        ()
    | Some user ->
        let meta =
          Auth_store.get_llm_key_meta (Session.store session)
            ~user_id:user.Auth_store.id
        in
        let status_line =
          match meta with
          | None ->
              if Llm_client.api_key () <> "" then
                "No personal key. Operator LLM_API_KEY env is set (fallback)."
              else
                "No personal key and no LLM_API_KEY env."
          | Some m ->
              Printf.sprintf "Configured · last4=%s%s%s"
                m.last4
                (match m.base with Some b -> " · base=" ^ b | None -> "")
                (match m.model with Some mo -> " · model=" ^ mo | None -> "")
        in
        let body =
          chrome ~session ~title:"LLM keys" [
            HTML.h1 [] [txt "LLM keys"];
            HTML.p [HTML.class_ "muted"] [
              txt "Bring your own provider key. Never shown again after save — only last4.";
            ];
            HTML.p [] [txt "%s" status_line];
            HTML.div [HTML.class_ "card"] [
              HTML.form [HTML.method_ `POST; HTML.action "/settings/keys"] [
                HTML.input [HTML.type_ "hidden"; HTML.name "action"; HTML.value "set"];
                HTML.p [] [
                  HTML.label [] [txt "API key"];
                  HTML.input [HTML.type_ "password"; HTML.name "api_key"; HTML.required];
                ];
                HTML.p [] [
                  HTML.label [] [txt "Base URL (optional)"];
                  HTML.input [HTML.type_ "text"; HTML.name "base";
                              HTML.placeholder "https://api.synthetic.new/openai/v1"];
                ];
                HTML.p [] [
                  HTML.label [] [txt "Model (optional)"];
                  HTML.input [HTML.type_ "text"; HTML.name "model"];
                ];
                HTML.button [HTML.type_ "submit"] [txt "Save key"];
              ];
            ];
            HTML.div [HTML.class_ "card"] [
              HTML.form [HTML.method_ `POST; HTML.action "/settings/keys"] [
                HTML.input [HTML.type_ "hidden"; HTML.name "action"; HTML.value "clear"];
                HTML.button [HTML.type_ "submit"] [txt "Clear key"];
              ];
            ];
          ]
        in
        html_response reqd ~session body;
        ()

let post_settings_keys (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  let username = Session.username session in
  if username = "anonymous" then begin
    redirect_response reqd "/login";
    ()
  end else
    let user_opt =
      Auth_store.get_user_by_username (Session.store session) username
    in
    match user_opt with
    | None ->
        redirect_response reqd "/login";
        ()
    | Some user ->
        let body = read_body reqd in
        let action =
          Option.value (form_field body "action") ~default:"set" |> String.trim
        in
        if action = "clear" then
          let _r =
            Auth_store.clear_llm_key (Session.store session)
              ~user_id:user.Auth_store.id
          in
          redirect_response reqd "/settings/keys";
          ()
        else
          let api_key =
            Option.value (form_field body "api_key") ~default:"" |> String.trim
          in
          let base =
            match form_field body "base" with
            | Some s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          in
          let model =
            match form_field body "model" with
            | Some s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          in
          let r =
            Auth_store.set_llm_key (Session.store session)
              ~user_id:user.Auth_store.id ~api_key ~base ~model ()
          in
          (match r with
           | Error e ->
               html_response reqd ~session ~status:`Bad_request
                 (chrome ~session ~title:"LLM keys" [
                    HTML.p [HTML.class_ "err"] [txt "%s" e];
                    HTML.p [] [HTML.a [HTML.href "/settings/keys"] [txt "Back"]];
                  ]);
               ()
           | Ok _ ->
               redirect_response reqd "/settings/keys";
               ())

let settings_ssh_page (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  let username = Session.username session in
  if username = "anonymous" then begin
    redirect_response reqd "/login";
    ()
  end else
    let user_opt =
      Auth_store.get_user_by_username (Session.store session) username
    in
    match user_opt with
    | None ->
        redirect_response reqd "/login";
        ()
    | Some user ->
        let keys =
          Auth_store.list_ssh_keys (Session.store session)
            ~user_id:user.Auth_store.id
        in
        let key_rows =
          List.map
            (fun (k : Auth_store.ssh_key_meta) ->
              let label =
                match k.label with Some l -> l | None -> ""
              in
              HTML.tr [] [
                HTML.td [] [HTML.code [] [txt "%s" k.fingerprint]];
                HTML.td [] [txt "%s" label];
                HTML.td [] [
                  HTML.form
                    [ HTML.method_ `POST; HTML.action "/settings/ssh" ]
                    [ HTML.input
                        [ HTML.type_ "hidden"; HTML.name "action";
                          HTML.value "revoke" ];
                      HTML.input
                        [ HTML.type_ "hidden"; HTML.name "id";
                          HTML.value "%s" k.id ];
                      HTML.button [HTML.type_ "submit"] [txt "Revoke"];
                    ];
                ];
              ])
            keys
        in
        let body =
          chrome ~session ~title:"SSH keys" [
            HTML.h1 [] [txt "SSH keys"];
            HTML.p [HTML.class_ "muted"] [
              txt "Register an OpenSSH public key for the REPL door (auth.user-ssh-repl).";
            ];
            HTML.div [HTML.class_ "card"] [
              HTML.h2 [] [txt "Your keys"];
              (if keys = [] then
                 HTML.p [HTML.class_ "muted"] [txt "No keys yet."]
               else
                 HTML.table [] (
                   HTML.thead [] [
                     HTML.tr [] [
                       HTML.th [] [txt "fingerprint"];
                       HTML.th [] [txt "label"];
                       HTML.th [] [txt ""];
                     ];
                   ]
                   :: key_rows
                 ));
            ];
            HTML.div [HTML.class_ "card"] [
              HTML.h2 [] [txt "Add key"];
              HTML.form [HTML.method_ `POST; HTML.action "/settings/ssh"] [
                HTML.input [HTML.type_ "hidden"; HTML.name "action"; HTML.value "add"];
                HTML.p [] [
                  HTML.label [] [txt "OpenSSH public key"];
                  HTML.textarea [HTML.name "openssh"; HTML.required] "%s" "";
                ];
                HTML.p [] [
                  HTML.label [] [txt "Label (optional)"];
                  HTML.input [HTML.type_ "text"; HTML.name "label"; HTML.placeholder "laptop"];
                ];
                HTML.button [HTML.type_ "submit"] [txt "Add key"];
              ];
            ];
          ]
        in
        html_response reqd ~session body;
        ()

let post_settings_ssh (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  let username = Session.username session in
  if username = "anonymous" then begin
    redirect_response reqd "/login";
    ()
  end else
    let user_opt =
      Auth_store.get_user_by_username (Session.store session) username
    in
    match user_opt with
    | None ->
        redirect_response reqd "/login";
        ()
    | Some user ->
        let body = read_body reqd in
        let action =
          Option.value (form_field body "action") ~default:"add" |> String.trim
        in
        if action = "revoke" then
          let id =
            Option.value (form_field body "id") ~default:"" |> String.trim
          in
          let _r =
            Auth_store.revoke_ssh_key (Session.store session)
              ~user_id:user.Auth_store.id ~id_or_fingerprint:id
          in
          redirect_response reqd "/settings/ssh";
          ()
        else
          let openssh =
            Option.value (form_field body "openssh") ~default:"" |> String.trim
          in
          let label =
            match form_field body "label" with
            | Some s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          in
          let r =
            Auth_store.add_ssh_key (Session.store session)
              ~user_id:user.Auth_store.id ~openssh ~label ()
          in
          (match r with
           | Error e ->
               html_response reqd ~session ~status:`Bad_request
                 (chrome ~session ~title:"SSH keys" [
                    HTML.p [HTML.class_ "err"] [txt "%s" e];
                    HTML.p [] [HTML.a [HTML.href "/settings/ssh"] [txt "Back"]];
                  ]);
               ()
           | Ok _ ->
               redirect_response reqd "/settings/ssh";
               ())

