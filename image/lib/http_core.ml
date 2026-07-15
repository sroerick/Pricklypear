(** Pricklypear Image — HTTP shared infrastructure.
    Responses, chrome, forms, auth gate helpers, login/admin.
    Page handlers live in [Http_pages]; dispatch in [Http]. *)
open Lwt.Syntax
open Pure_html

let default_port = 8080

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
   "

let html_response (reqd : Httpaf.Reqd.t) ?(status = `OK)
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
    Httpaf.Headers.of_list
      [("content-type", "text/html; charset=utf-8");
       ("content-length", string_of_int (String.length s))]
  in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp s

let text_response (reqd : Httpaf.Reqd.t) ?(status = `OK) ~content_type (s : string) : unit =
  let headers =
    Httpaf.Headers.of_list
      [("content-type", content_type);
       ("content-length", string_of_int (String.length s))]
  in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp s

let json_response (reqd : Httpaf.Reqd.t) ?(status = `OK) (j : Yojson.Safe.t) : unit =
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

let try_ui_fn (session : Session.t) (name : string) : string option Lwt.t =
  Lwt.catch
    (fun () ->
      let* r = Session.call_handler session name in
      match r with
      | Some v -> Lwt.return (Some (value_to_html_string v))
      | None ->
          Printf.eprintf "[http] try_ui_fn %s: unbound or no value\n%!" name;
          Lwt.return None)
    (fun exn ->
      Printf.eprintf "[http] try_ui_fn %s failed: %s\n%!"
        name (Printexc.to_string exn);
      Lwt.return None)

(** Fragment / partial: no document chrome (htmx swap target). *)
let is_partials_path (path : string) : bool =
  String.length path >= 10 && String.sub path 0 10 = "/partials/"

let header_list (req : Httpaf.Request.t) : (string * string) list =
  Httpaf.Headers.to_list req.headers
  |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))

let wants_fragment ~(path : string) ~(headers : (string * string) list) : bool =
  if is_partials_path path then true
  else
    match List.assoc_opt "hx-request" headers with
    | Some v when v <> "" && String.lowercase_ascii v <> "false" -> true
    | _ -> false

(* ---------- Auth (single-image Option B: in-process sessions) ---------- *)

let cookie_name = "pp_session"

(** Serialize request identity attach — cotenant multi-user shares one
    Session.t; concurrent Alice/Bob would otherwise race branch/env. *)
let request_mutex = Lwt_mutex.create ()

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
  || path = "/book.pdf"
  || path = "/src.tgz"
  || path = "/code"
  || has_prefix path "/code"
  || path = "/assets"
  || has_prefix path "/assets"

(** Kernel paths Lisp cannot open to the world (auth.route-visibility). *)
let is_system_hard_private (path : string) : bool =
  let prefixes =
    [ "/admin"
    ; "/analytics"
    ; "/data"
    ; "/api"
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

(** Classify path for the request gate.
    Returns [`Hard_public | `Hard_private | `User of string * string
    | `Unknown] where User is (handler, visibility). *)
let classify_path (session : Session.t) (path : string) =
  if is_system_hard_public path then `Hard_public
  else if is_system_hard_private path then `Hard_private
  else
    match Session.lookup_route_meta session path with
    | Some (handler, vis) -> `User (handler, vis)
    | None -> `Unknown

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

let set_cookie_header ~(token : string) : string =
  (* Dev is plain HTTP — omit Secure. Path=/; HttpOnly; SameSite=Lax. *)
  Printf.sprintf "%s=%s; Path=/; HttpOnly; SameSite=Lax; Max-Age=%d"
    cookie_name token (60 * 60 * 24 * 14)

let clear_cookie_header () : string =
  Printf.sprintf
    "%s=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0" cookie_name

let redirect_with_cookie (reqd : Httpaf.Reqd.t) ~(location : string)
    ~(set_cookie : string) : unit =
  let headers =
    Httpaf.Headers.of_list
      [ ("location", location)
      ; ("set-cookie", set_cookie)
      ; ("content-length", "0") ]
  in
  let resp = Httpaf.Response.create ~headers `See_other in
  Httpaf.Reqd.respond_with_string reqd resp ""

let unauthorized_api (reqd : Httpaf.Reqd.t) : unit =
  json_response reqd ~status:`Unauthorized
    (`Assoc
       [ ("ok", `Bool false)
       ; ("error", `String "authentication required") ])

(** Resolve identity for this request: cookie session, then Basic auth.
    Returns None if unauthenticated. *)
let resolve_identity (session : Session.t)
    ~(headers : (string * string) list)
  : Auth_store.user option Lwt.t =
  let store = Session.store session in
  match cookie_value headers cookie_name with
  | Some token when token <> "" ->
      let* row = Auth_store.lookup_session store ~raw_token:token in
      (match row with
       | Some s ->
           Lwt.return_some
             { Auth_store.id = s.user_id;
               username = s.username;
               is_admin = s.is_admin }
       | None ->
           (* Fall through to Basic if cookie stale. *)
           (match parse_basic_auth headers with
            | None -> Lwt.return_none
            | Some (u, p) ->
                Auth_store.verify_password store ~username:u ~password:p ()))
  | _ ->
      (match parse_basic_auth headers with
       | None -> Lwt.return_none
       | Some (u, p) ->
           Auth_store.verify_password store ~username:u ~password:p ())

let attach_from_user (session : Session.t) (user : Auth_store.user)
    ?(auth_token : string option = None)
    ?(checkout_personal_now = false) () : unit Lwt.t =
  Session.attach_identity session
    ~user_id:user.Auth_store.id
    ~username:user.Auth_store.username
    ~is_admin:user.Auth_store.is_admin
    ~auth_token
    ~checkout_personal_now
    ()

let fragment_response (reqd : Httpaf.Reqd.t) ?(status = `OK) (html : string)
    : unit =
  text_response reqd ~status ~content_type:"text/html; charset=utf-8" html

let chrome ?nav_html ~(session : Session.t) ~(title : string)
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
  HTML.div [] [
    HTML.header [] [
      HTML.span [HTML.class_ "brand"] [txt "Pricklypear"];
      HTML.span [HTML.class_ "meta"] [
        txt "user=%s  branch=%s  up=%ds"
          (Session.username session)
          (Session.branch_name session)
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
    (content : Pure_html.node list) : Pure_html.node Lwt.t =
  let* nav_html = try_ui_fn session "ui/nav" in
  Lwt.return (chrome ?nav_html ~session ~title content)

let read_body (reqd : Httpaf.Reqd.t) : string Lwt.t =
  let body = Httpaf.Reqd.request_body reqd in
  let promise, wakeup = Lwt.task () in
  let buf = Buffer.create 1024 in
  let rec sched () =
    Httpaf.Body.schedule_read body
      ~on_eof:(fun () -> Lwt.wakeup_later wakeup (Buffer.contents buf))
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
        sched ())
  in
  sched ();
  promise

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

let redirect_response (reqd : Httpaf.Reqd.t) (location : string) : unit =
  let headers =
    Httpaf.Headers.of_list
      [ ("location", location)
      ; ("content-length", "0")
      ]
  in
  let resp = Httpaf.Response.create ~headers `See_other in
  Httpaf.Reqd.respond_with_string reqd resp ""

let require_login_redirect (reqd : Httpaf.Reqd.t) ~(path : string) : unit =
  if String.length path >= 5 && String.sub path 0 5 = "/api/" then
    unauthorized_api reqd
  else if path = "/" then
    (* Public host: anonymous workspace root → welcome landing. *)
    redirect_response reqd "/welcome"
  else
    redirect_response reqd "/login"

let login_form_page ?(error = "") (reqd : Httpaf.Reqd.t) : unit =
  let err =
    if error = "" then HTML.div [] []
    else HTML.p [HTML.class_ "err"] [txt "%s" error]
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

let post_login (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let username =
    Option.value (form_field body "username") ~default:"" |> String.trim
  in
  let password = Option.value (form_field body "password") ~default:"" in
  if username = "" || password = "" then begin
    login_form_page ~error:"Invalid username or password" reqd;
    Lwt.return_unit
  end else
    let store = Session.store session in
    let* user_opt =
      Auth_store.verify_password store ~username ~password ()
    in
    match user_opt with
    | None ->
        login_form_page ~error:"Invalid username or password" reqd;
        Lwt.return_unit
    | Some user ->
        let* minted =
          Auth_store.mint_session store ~user_id:user.Auth_store.id ()
        in
        (match minted with
         | Error e ->
             login_form_page ~error:("Login failed: " ^ e) reqd;
             Lwt.return_unit
         | Ok token ->
             let* () =
               attach_from_user session user ~auth_token:(Some token)
                 ~checkout_personal_now:true ()
             in
             redirect_with_cookie reqd ~location:"/"
               ~set_cookie:(set_cookie_header ~token);
             Lwt.return_unit)

let post_logout (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let headers = header_list (Httpaf.Reqd.request reqd) in
  let token = cookie_value headers cookie_name in
  let store = Session.store session in
  let* () =
    match token with
    | Some t when t <> "" -> Auth_store.revoke_session store ~raw_token:t ()
    | _ -> Lwt.return_unit
  in
  Session.clear_auth_token session;
  redirect_with_cookie reqd ~location:"/login"
    ~set_cookie:(clear_cookie_header ());
  Lwt.return_unit

let admin_users_page (session : Session.t) (reqd : Httpaf.Reqd.t)
  : unit Lwt.t =
  if not (Session.is_admin session) then begin
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"Forbidden" [
         HTML.p [HTML.class_ "err"] [txt "admin only"];
       ]);
    Lwt.return_unit
  end else
    let store = Session.store session in
    let* users = Auth_store.list_users store in
    let rows =
      List.map
        (fun (u : Auth_store.user) ->
          HTML.tr [] [
            HTML.td [] [txt "%s" u.Auth_store.username];
            HTML.td [] [txt "%s" (if u.Auth_store.is_admin then "yes" else "no")];
            HTML.td [] [HTML.code [] [txt "%s" u.Auth_store.id]];
            HTML.td [] [
              HTML.form
                [ HTML.method_ `POST; HTML.action "/admin/users" ]
                [ HTML.input
                    [ HTML.type_ "hidden"; HTML.name "action";
                      HTML.value "revoke_sessions" ]
                ; HTML.input
                    [ HTML.type_ "hidden"; HTML.name "user_id";
                      HTML.value "%s" u.Auth_store.id ]
                ; HTML.button [HTML.type_ "submit"] [txt "Revoke sessions"]
                ]
            ];
          ])
        users
    in
    let body =
      chrome ~session ~title:"Users" [
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "Users"];
          HTML.table [] [
            HTML.thead [] [
              HTML.tr [] [
                HTML.th [] [txt "username"];
                HTML.th [] [txt "admin"];
                HTML.th [] [txt "id"];
                HTML.th [] [txt "actions"];
              ];
            ];
            HTML.tbody [] rows;
          ];
        ];
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
        HTML.p [] [
          HTML.form [HTML.method_ `POST; HTML.action "/logout"] [
            HTML.button [HTML.type_ "submit"] [txt "Log out"];
          ];
        ];
      ]
    in
    html_response reqd ~session body;
    Lwt.return_unit

let post_admin_users (session : Session.t) (reqd : Httpaf.Reqd.t)
  : unit Lwt.t =
  if not (Session.is_admin session) then begin
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"Forbidden" [
         HTML.p [HTML.class_ "err"] [txt "admin only"];
       ]);
    Lwt.return_unit
  end else
    let* body = read_body reqd in
    let action =
      Option.value (form_field body "action") ~default:"create"
      |> String.trim
    in
    if action = "revoke_sessions" then
      let user_id =
        Option.value (form_field body "user_id") ~default:"" |> String.trim
      in
      if user_id = "" then begin
        html_response reqd ~session ~status:`Bad_request
          (chrome ~session ~title:"Revoke sessions" [
             HTML.p [HTML.class_ "err"] [txt "user_id required"];
             HTML.p [] [HTML.a [HTML.href "/admin/users"] [txt "Back"]];
           ]);
        Lwt.return_unit
      end else
        let store = Session.store session in
        let* n = Auth_store.revoke_all_sessions store ~user_id in
        let* () =
          Auth_store.log_auth store ~user_id ~kind:"admin_revoke_sessions"
            ~success:true ()
        in
        ignore n;
        redirect_response reqd "/admin/users";
        Lwt.return_unit
    else
      let username =
        Option.value (form_field body "username") ~default:"" |> String.trim
      in
      let password = Option.value (form_field body "password") ~default:"" in
      let is_admin =
        match form_field body "is_admin" with
        | Some ("1" | "on" | "true") -> true
        | _ -> false
      in
      if username = "" || password = "" then begin
        html_response reqd ~session ~status:`Bad_request
          (chrome ~session ~title:"Create user" [
             HTML.p [HTML.class_ "err"] [txt "username and password required"];
             HTML.p [] [HTML.a [HTML.href "/admin/users"] [txt "Back"]];
           ]);
        Lwt.return_unit
      end else
        let store = Session.store session in
        let* created = Auth_store.create_user store ~username ~is_admin in
        match created with
        | Error e ->
            html_response reqd ~session ~status:`Bad_request
              (chrome ~session ~title:"Create user" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/admin/users"] [txt "Back"]];
               ]);
            Lwt.return_unit
        | Ok user ->
            let* pw =
              Auth_store.set_password store ~user_id:user.Auth_store.id
                ~password
            in
            (match pw with
             | Error e ->
                 html_response reqd ~session ~status:`Internal_server_error
                   (chrome ~session ~title:"Create user" [
                      HTML.p [HTML.class_ "err"] [txt "password: %s" e];
                    ]);
                 Lwt.return_unit
             | Ok () ->
                 let* () =
                   Auth_store.log_auth store ~user_id:user.Auth_store.id
                     ~kind:"admin_create_user" ~success:true ()
                 in
                 redirect_response reqd "/admin/users";
                 Lwt.return_unit)

let respond_handler_value
    ~(session : Session.t)
    ~(reqd : Httpaf.Reqd.t)
    ~(path : string)
    ~(fragment : bool)
    (value : Pricklypear_lisp.Ast.value) : unit =
  match Primitives.is_redirect value with
  | Some loc -> redirect_response reqd loc
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
              chrome ~session ~title:path [ txt ~raw:true "%s" raw_html ]
          | None ->
              (match value with
               | Pricklypear_lisp.Ast.VStr s ->
                   chrome ~session ~title:path [ HTML.p [] [txt "%s" s] ]
               | _ ->
                   let rendered =
                     Pricklypear_lisp.Reader.pp_value value
                   in
                   chrome ~session ~title:path [
                     HTML.pre [] [txt "%s" rendered];
                   ])
        in
        html_response reqd ~session body

(** /settings/keys — per-user LLM provider key (auth.user-llm-keys). *)
let settings_keys_page (session : Session.t) (reqd : Httpaf.Reqd.t)
  : unit Lwt.t =
  let username = Session.username session in
  if username = "anonymous" then begin
    redirect_response reqd "/login";
    Lwt.return_unit
  end else
    let* user_opt =
      Auth_store.get_user_by_username (Session.store session) username
    in
    match user_opt with
    | None ->
        html_response reqd ~session ~status:`Unauthorized
          (chrome ~session ~title:"Keys" [
             HTML.p [HTML.class_ "err"] [txt "not signed in"];
           ]);
        Lwt.return_unit
    | Some user ->
        let* meta =
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
        Lwt.return_unit

let post_settings_keys (session : Session.t) (reqd : Httpaf.Reqd.t)
  : unit Lwt.t =
  let username = Session.username session in
  if username = "anonymous" then begin
    redirect_response reqd "/login";
    Lwt.return_unit
  end else
    let* user_opt =
      Auth_store.get_user_by_username (Session.store session) username
    in
    match user_opt with
    | None ->
        redirect_response reqd "/login";
        Lwt.return_unit
    | Some user ->
        let* body = read_body reqd in
        let action =
          Option.value (form_field body "action") ~default:"set" |> String.trim
        in
        if action = "clear" then
          let* _r =
            Auth_store.clear_llm_key (Session.store session)
              ~user_id:user.Auth_store.id
          in
          redirect_response reqd "/settings/keys";
          Lwt.return_unit
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
          let* r =
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
               Lwt.return_unit
           | Ok _ ->
               redirect_response reqd "/settings/keys";
               Lwt.return_unit)

