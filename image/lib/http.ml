(** Pricklypear Image — HTTP server.

    System pages + browser product surfaces (it31 dream cut):
      /                  system browser shell
      /health            JSON health
      /branches /bindings /binding/<name> /types
      /repl              browser REPL UI
      /api/eval          POST {expr} -> {ok,value|error}
      /edit/<name>       function editor GET/POST
      /inspect/binding/<name>
      /inspect/row/<type>/<id>
      /api/ai/propose    POST {name,spec} -> AI define on branch
      user routes via (route ...)
*)
open Lwt.Syntax
open Pure_html

let default_port = 8080

let html_response (reqd : Httpaf.Reqd.t) ?(status = `OK) (body : Pure_html.node) : unit =
  let html_doc = HTML.html [HTML.lang "en"] [
    HTML.head [] [
      HTML.title [] "Pricklypear";
      HTML.meta [HTML.charset "utf-8"];
      (* Mobile-first: prevent tiny text, enable proper layout *)
      HTML.meta [HTML.name "viewport";
                 HTML.content "width=device-width, initial-scale=1"];
      HTML.style [] "%s"
        (* Classic Mac / Smalltalk / mainframe terminal — flat, bordered, readable.
           No gradients, no pill buttons, no dark "LLM dashboard" chrome. *)
        "*{box-sizing:border-box}
         html{font-size:16px;-webkit-text-size-adjust:100%}
         body{margin:0;background:#d4d0c8;color:#000;
              font-family:Geneva,\"Lucida Grande\",\"DejaVu Sans\",Verdana,sans-serif;
              line-height:1.4}
         a{color:#000080;text-decoration:underline}
         a:visited{color:#551a8b}
         a:hover{color:#000}
         header{background:#fff;border-bottom:2px solid #000;padding:.5rem .75rem}
         header .brand{font-weight:700;font-size:1.1rem;letter-spacing:.02em;
                       color:#000;margin-right:.75rem}
         header .meta{display:block;font-family:\"Courier New\",Courier,monospace;
                      font-size:.8rem;color:#333;margin:.25rem 0 .5rem}
         nav{display:flex;flex-wrap:wrap;gap:.35rem .5rem;align-items:center}
         /* Links + dropdowns share the same chrome size — don't let a single
            top-level link (home) flex-grow while apps/system stay compact. */
         nav > a,.menu-drop{flex:0 0 auto}
         nav a,.menu-drop-btn{display:inline-block;padding:.45rem .55rem;min-height:2.5rem;
               line-height:1.5;border:1px solid #000;background:#c0c0c0;
               color:#000;text-decoration:none;font-size:.9rem;cursor:pointer;
               list-style:none;box-sizing:border-box}
         nav a:visited{color:#000}
         nav a:hover,nav a:focus,.menu-drop-btn:hover{background:#fff}
         nav.emergency-nav{width:100%;margin-top:.35rem}
         .emergency-banner{display:block;width:100%;padding:.4rem .55rem;
           border:2px solid #8b0000;background:#fff3cd;color:#1a1a1a;
           font-size:.85rem;font-family:Courier,monospace}
         .emergency-banner a{color:#000080}
         .emergency-home{border:2px solid #8b0000;background:#fff3cd}
         /* Dropdown menus (details/summary — works without JS, mobile-friendly) */
         .menu-drop{position:relative;display:inline-block;margin:0}
         .menu-drop>summary{list-style:none}
         .menu-drop>summary::-webkit-details-marker{display:none}
         .menu-drop>summary::after{content:\" v\";font-size:.7em}
         .menu-drop-panel{display:none;position:absolute;left:0;right:auto;top:100%;
           z-index:50;min-width:10rem;max-width:min(16rem,calc(100vw - 1rem));
           margin-top:2px;padding:.25rem;box-sizing:border-box;
           background:#fff;border:2px solid #000;box-shadow:2px 2px 0 #808080}
         .menu-drop[open]>.menu-drop-panel{display:block}
         .menu-drop-panel a{display:block;width:100%;margin:0 0 .2rem;
           white-space:nowrap;box-sizing:border-box}
         .menu-drop-panel a:last-child{margin-bottom:0}
         /* Right-edge menus (e.g. Apps) open leftward so panels stay on-screen */
         @media (max-width:40rem){
           header,nav{overflow:visible}
           .menu-drop-panel{left:auto;right:0;
             max-width:calc(100vw - .75rem)}
         }
         main{padding:.75rem;max-width:52rem;margin:0 auto}
         h1{font-size:1.25rem;font-weight:700;margin:.25rem 0 .75rem;
            border-bottom:1px solid #000;padding-bottom:.25rem}
         h2{font-size:1.05rem;font-weight:700;margin:0 0 .5rem}
         p{margin:.5rem 0}
         .card{background:#fff;border:2px solid #000;padding:.75rem;margin:0 0 .75rem}
         table{border-collapse:collapse;width:100%;background:#fff;
               border:1px solid #000;font-size:.9rem}
         th,td{border:1px solid #808080;padding:.45rem .5rem;text-align:left;
               vertical-align:top;word-break:break-word}
         th{background:#c0c0c0;font-weight:700;border-color:#000}
         tr:nth-child(even) td{background:#ece9e0}
         pre,textarea,input[type=text],input[type=search],input:not([type]){
           font-family:\"Courier New\",Courier,monospace;font-size:1rem;
           background:#fff;color:#000;border:1px solid #000;border-radius:0;
           padding:.5rem;width:100%;max-width:100%}
         pre{white-space:pre-wrap;overflow-x:auto;line-height:1.35}
         textarea{min-height:16rem;line-height:1.35;white-space:pre;tab-size:2}
         button,.btn,input[type=submit]{
           font:inherit;font-size:1rem;cursor:pointer;
           background:#c0c0c0;color:#000;border:2px outset #fff;
           border-radius:0;padding:.5rem .85rem;min-height:2.75rem}
         button:active,.btn:active{border-style:inset}
         button.secondary,.btn.secondary{background:#d4d0c8}
         a.btn{display:inline-block;text-decoration:none;color:#000;
               line-height:1.2;vertical-align:middle}
         a.btn:visited{color:#000}
         label{font-weight:700}
         .meta,.muted{color:#333;font-size:.9rem}
         .err{color:#800000;font-weight:700}
         .ok{color:#006600;font-weight:700}
         #out{white-space:pre-wrap;min-height:4rem;background:#fff;
              border:1px solid #000;padding:.5rem;
              font-family:\"Courier New\",Courier,monospace}
         ul.actions{list-style:none;padding:0;margin:.5rem 0}
         ul.actions li{margin:.4rem 0}
         /* Outline notes */
         .ol-root{margin:.5rem 0}
         .ol-node{position:relative;margin:.25rem 0 .25rem .35rem;
           border-left:2px solid #808080;padding-left:.35rem}
         .ol-sum{cursor:default;font-weight:700;display:flex;align-items:center;
           gap:.35rem;flex-wrap:nowrap;min-width:0;padding-right:16rem}
         /* Only the fold chevron toggles open/closed (JS preventDefault on rest of summary). */
         .ol-fold{font-size:.75em;flex:0 0 auto;display:inline-flex;align-items:center;
           justify-content:center;user-select:none;cursor:pointer;line-height:1;
           min-width:1.6rem;min-height:1.6rem;padding:.2rem;margin:0 .05rem 0 -.1rem;
           color:#333}
         .ol-fold:hover{background:#fff;border:1px solid #808080;padding:calc(.2rem - 1px)}
         details.ol-node > .ol-sum .ol-fold::before{content:\"v\"}
         details.ol-node:not([open]) > .ol-sum .ol-fold::before{content:\">\"}
         /* Read-only state badge (not a button) — hidden when not a todo. */
         .ol-todo{display:inline-block;flex:0 0 auto;width:auto;margin:0 .15rem 0 0;
           padding:.05rem .3rem;border:1px solid #666;background:transparent;
           font-size:.7rem;font-weight:700;line-height:1.3;letter-spacing:.02em;
           color:#333;vertical-align:middle}
         .ol-todo.st-TODO{border-color:#886600;color:#664400;background:#fff8dc}
         .ol-todo.st-DONE{border-color:#226622;color:#226622;background:#e8f5e8}
         .ol-node.st-DONE > .ol-sum .ol-title{text-decoration:line-through;color:#555;
           font-weight:500}
         .ol-title{outline:none;padding:.15rem .25rem;border-radius:0;flex:1 1 auto;
           min-width:0;word-break:break-word;white-space:normal;line-height:1.35;
           cursor:text;font-weight:700}
         .ol-title:focus{background:#fff;border:1px solid #000}
         .ol-body{margin:.25rem 0 .35rem;white-space:pre-wrap;font-size:.95rem;
           outline:none;padding:.25rem;min-width:0;word-break:break-word}
         .ol-body:focus{background:#fff;border:1px solid #000}
         .ol-kids{margin-left:.15rem;min-width:0}
         /* Actions: DOM before kids; desktop = right of title row; mobile = ··· menu */
         details.ol-actions{position:absolute;top:.1rem;right:0;z-index:3;margin:0;display:block}
         details.ol-actions[open]{z-index:40}
         details.ol-actions > summary.ol-actions-sum{display:none;list-style:none;cursor:pointer;
           user-select:none;width:auto;min-width:1.75rem;min-height:1.75rem;padding:.1rem .3rem;
           border:1px solid #666;background:transparent;font-weight:700;color:#333;
           text-align:center;line-height:1.3;border-radius:0}
         details.ol-actions > summary.ol-actions-sum:hover,
         details.ol-actions[open] > summary.ol-actions-sum{background:#fff;border-color:#000}
         details.ol-actions > summary.ol-actions-sum::-webkit-details-marker{display:none}
         .ol-actions-panel{display:flex !important;flex-direction:row;flex-wrap:nowrap;
           justify-content:flex-end;align-items:center;gap:.25rem;margin:0;padding:0;
           background:transparent;border:none}
         .ol-btn{font-size:.75rem;padding:.25rem .45rem;min-height:2rem;width:auto;
           flex:0 0 auto;margin:0;border:1px solid #000;background:#d4d0c8;cursor:pointer}
         .ol-btn:hover{background:#fff}
         .ol-del{color:#800000}
         .ol-debug{margin:.75rem 0;opacity:.6}
         .ol-debug > summary{cursor:pointer;font-size:.85rem}
         /* Branch browser */
         .br-table-wrap{overflow-x:auto}
         .br-table{width:100%;font-size:.9rem}
         .br-table th,.br-table td{padding:.35rem .4rem;vertical-align:top}
         tr.br-current{background:#fff8dc}
         .br-stats{display:flex;flex-wrap:wrap;gap:.5rem;margin:.5rem 0}
         .br-stat{padding:.4rem .6rem;margin:0}
         .br-diff{display:grid;grid-template-columns:1fr 1fr;gap:.5rem}
         @media(max-width:700px){.br-diff{grid-template-columns:1fr}}
         form.inline-form{display:inline}
         form.inline-form button{margin-left:.2rem}
         /* Unified todos */
         .todo-layout{display:grid;grid-template-columns:12rem 1fr;gap:.75rem}
         @media(max-width:700px){.todo-layout{grid-template-columns:1fr}}
         .todo-nav{list-style:none;padding:0;margin:0}
         .todo-nav-item{margin:.25rem 0}
         .todo-nav-item.is-active a{font-weight:700;border-left:3px solid #000;padding-left:.35rem}
         .todo-new-list{display:flex;flex-direction:column;gap:.25rem;margin-top:.5rem}
         .todo-new-list-input{font-size:.85rem;min-height:2rem;padding:.25rem}
         .todo-box.st-TODO{background:#fff8dc}
         .todo-box.st-DONE{background:#cfc;text-decoration:line-through}
         .todo-box.st-WAITING{background:#e0e8ff}
         .todo-box.st-CANCELLED{background:#ddd;color:#666}
         .todo-box.st-ARCHIVED{background:#eee;color:#555;border-style:dashed}
         li.todo-item.is-archived .todo-title{color:#888;font-style:italic}
         .todo-from-note .todo-src{font-size:.8rem;margin-left:.35rem}
         /* legacy class hooks (todo package may still emit them) */
         .ol-todo-btn{cursor:pointer}
         .ol-todo-btn.st-TODO{background:#fff8dc}
         .ol-todo-btn.st-DONE{background:#cfc}
         .ol-todo-btn.st-WAITING{background:#e0e8ff}
         /* Calendar */
         .cal-nav{display:flex;align-items:center;gap:.75rem;margin:.5rem 0;flex-wrap:wrap}
         .cal-nav a{text-decoration:none;font-weight:700}
         .cal-grid{display:grid;grid-template-columns:repeat(7,1fr);gap:2px;
           background:#808080;border:2px solid #000;max-width:42rem}
         .cal-dow{background:#c0c0c0;text-align:center;font-weight:700;
           padding:.25rem;font-size:.8rem}
         .cal-cell{background:#fff;min-height:3.5rem;padding:.2rem;
           display:block;color:#000;text-decoration:none}
         a .cal-cell:hover{background:#fff8dc}
         .cal-empty{background:#d4d0c8;min-height:3.5rem}
         .cal-today{outline:2px solid #000;background:#ffffe0}
         .cal-dom{font-weight:700;font-size:.85rem}
         .cal-marks{display:flex;flex-wrap:wrap;gap:2px;margin-top:.15rem}
         .cal-dot{font-size:.65rem;padding:0 .2rem;border:1px solid #000;
           background:#c0c0c0;line-height:1.2}
         .cal-dot-ev{background:#cfc}
         .cal-dot-todo{background:#fff8dc}
         .cal-dot-note{background:#e0e8ff}
         .cal-list{list-style:none;padding:.25rem;margin:0}
         .cal-event,.cal-todo{display:flex;gap:.5rem;align-items:center;
           padding:.35rem 0;border-bottom:1px solid #ccc}
         .cal-time{font-size:.8rem;min-width:3.5rem;color:#333}
         .cal-day-panel label{display:block;margin-top:.4rem}
         /* Weather widget */
         .weather-card .wx-row{display:flex;align-items:baseline;gap:.75rem;flex-wrap:wrap}
         .weather-card .wx-temp{font-size:2rem;font-weight:700;line-height:1}
         .weather-card .wx-meta{font-size:.9rem;color:#333}
         .weather-card .wx-loc{font-weight:700;margin:.25rem 0}
         .weather-card .wx-form{display:flex;gap:.35rem;flex-wrap:wrap;margin-top:.5rem}
         .weather-card .wx-form input[type=text]{flex:1 1 8rem;min-width:0}
         .weather-card .wx-form button{width:auto;min-width:4.5rem}
         .weather-card .wx-err{color:#800000;font-weight:700}
         /* Nav capture dropdown → inbox note */
         .capture-panel{min-width:16rem;max-width:min(22rem,calc(100vw - 1rem));padding:.4rem}
         .capture-nav-form{display:flex;flex-direction:column;gap:.35rem;margin:0}
         .capture-nav-form input[type=text],.capture-nav-form textarea{
           width:100%;box-sizing:border-box;font-size:.9rem}
         .capture-nav-form .capture-body{min-height:4rem}
         .capture-nav-form .capture-row{display:flex;flex-wrap:wrap;gap:.35rem;
           align-items:center}
         .capture-nav-form .capture-row button{width:auto;min-width:4.5rem;margin:0}
         .capture-nav-form #capture-status{font-size:.85rem;min-height:1.1em}
         .capture-nav-form .ok{color:#226622;font-weight:700}
         .capture-nav-form .err{color:#800000;font-weight:700}
         .capture-card label{display:block;margin:.35rem 0 .15rem;font-weight:700}
         .capture-card .capture-body{width:100%;box-sizing:border-box;min-height:4.5rem}
         .capture-card .capture-row{display:flex;flex-wrap:wrap;gap:.5rem;
           align-items:center;margin-top:.5rem}
         .capture-card .capture-row button{width:auto;min-width:6rem}
         /* Todo list */
         .todo-add-label{display:block;font-weight:bold;margin:0 0 .35rem}
         .todo-add-row{display:flex;gap:.5rem;align-items:stretch;flex-wrap:wrap}
         .todo-add-row input[type=text]{flex:1 1 12rem;min-width:0}
         .todo-add-row button{flex:0 0 auto;width:auto;min-width:5rem}
         ul.todo-list{list-style:none;padding:.25rem .5rem;margin:0 0 .75rem}
         li.todo-item{display:flex;align-items:center;gap:.5rem;
           padding:.45rem 0;border-bottom:1px solid #c0c0c0}
         li.todo-item:last-child{border-bottom:none}
         li.todo-item form{margin:0;display:inline}
         li.todo-item .todo-box{font-family:ui-monospace,Menlo,monospace;
           min-width:2.6rem;padding:.25rem .35rem;width:auto}
         li.todo-item .todo-title{flex:1 1 auto;min-width:0;word-break:break-word}
         li.todo-item.is-done .todo-title{text-decoration:line-through;color:#666}
         li.todo-item .todo-del button,
         li.todo-item .todo-archive button{width:auto;padding:.25rem .45rem;font-size:.85rem}
         li.todo-nav-list{display:flex;align-items:center;gap:.25rem;flex-wrap:wrap}
         li.todo-nav-list > a{flex:1 1 auto;min-width:0}
         li.todo-nav-list form.todo-list-del{margin:0;display:inline}
         li.todo-nav-list .todo-list-del button{width:auto;min-width:1.6rem;
           padding:.1rem .3rem;font-size:.8rem;line-height:1.2}
         li.note-item{display:flex;align-items:center;gap:.5rem;
           padding:.35rem 0;border-bottom:1px solid #c0c0c0}
         li.note-item > a{flex:1 1 auto}
         li.note-item form.note-del{margin:0}
         li.note-item .note-del button{width:auto;padding:.25rem .45rem;font-size:.85rem}
         .note-title-row{display:flex;flex-wrap:wrap;align-items:center;gap:.75rem}
         .note-title-row h1{flex:1 1 auto;margin:0;border-bottom:none;padding:0}
         .note-title-row .note-del-page button{width:auto}
         /* Bible chapter: only <sup> verse nums, not browser list markers */
         ol.bible-chapter{list-style:none;padding-left:0;margin:.25rem 0 1rem}
         ol.bible-chapter>li{margin:.35rem 0;line-height:1.45;padding-left:0}
         ol.bible-chapter>li>sup{font-size:.75em;font-weight:700;margin-right:.2em;
           color:#333;vertical-align:super}
         .bible-go label,.bible-search label{display:inline-block;margin:.2rem .45rem .2rem 0}
         .bible-go select,.bible-search input[type=text]{width:auto;min-width:6rem;
           max-width:100%;margin-right:.35rem}
         /* Mobile: stack denser chrome, full-width form controls.
            Outline editor buttons must stay compact — global width:100%
            was turning TODO · chips into full-row gray bars. */
         @media (max-width:40rem){
           main{padding:.5rem}
           header{padding:.5rem}
           nav{gap:.3rem;width:100%}
           /* Equal-width toolbar: home | apps | system */
           nav > a,.menu-drop{flex:1 1 0;min-width:0;text-align:center}
           nav > a{display:block}
           .menu-drop-btn{display:block;width:100%;text-align:center}
           table{display:block;overflow-x:auto;-webkit-overflow-scrolling:touch}
           h1{font-size:1.15rem}
           button,.btn{width:100%;margin:.2rem 0}
           .card form button,.card form .btn{width:auto;min-width:44%}
           button.ol-btn,button.ol-todo-btn,
           .ol-btn,.ol-todo,.ol-todo-btn{width:auto;margin:0}
           .ol-node{margin-left:.1rem;padding-left:.3rem}
           .ol-sum{padding-right:2.75rem;align-items:center}
           details.ol-actions > summary.ol-actions-sum{display:inline-block}
           .ol-actions-panel{display:none !important;position:absolute;right:0;top:100%;
             margin-top:2px;flex-direction:column;flex-wrap:nowrap;align-items:stretch;
             min-width:7rem;padding:.2rem;background:#fff;border:1px solid #000}
           details.ol-actions[open] > .ol-actions-panel{display:flex !important}
           details.ol-actions[open] > .ol-actions-panel .ol-btn{width:100%;text-align:left;
             min-height:2.1rem;background:#fff}
         }";
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

let is_public_path (path : string) : bool =
  path = "/login" || path = "/health" || path = "/logout"

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
    html_response reqd ~status:`Forbidden
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
    html_response reqd body;
    Lwt.return_unit

let post_admin_users (session : Session.t) (reqd : Httpaf.Reqd.t)
  : unit Lwt.t =
  if not (Session.is_admin session) then begin
    html_response reqd ~status:`Forbidden
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
        html_response reqd ~status:`Bad_request
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
        html_response reqd ~status:`Bad_request
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
            html_response reqd ~status:`Bad_request
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
                 html_response reqd ~status:`Internal_server_error
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
        html_response reqd body

let branch_ref_of (b : Pricklypear_common.Branch.t) : string =
  b.Pricklypear_common.Branch.owner ^ "/" ^ b.Pricklypear_common.Branch.name

let branch_label (b : Pricklypear_common.Branch.t) : string =
  let r = branch_ref_of b in
  if b.owner = "system" && b.name = "main" then r ^ " (defaults)"
  else if b.name = "personal" then r ^ " (garden)"
  else r

let id_to_ref_map (branches : Pricklypear_common.Branch.t list)
  : (string, string) Hashtbl.t =
  let h = Hashtbl.create 16 in
  List.iter
    (fun (b : Pricklypear_common.Branch.t) ->
      Hashtbl.replace h b.id (branch_ref_of b))
    branches;
  h

let parent_ref_of (idmap : (string, string) Hashtbl.t)
    (b : Pricklypear_common.Branch.t) : string =
  match b.parent_id with
  | None -> "—"
  | Some pid ->
      (match Hashtbl.find_opt idmap pid with
       | Some r -> r
       | None -> String.sub pid 0 (min 8 (String.length pid)))

let short_ts (s : string) : string =
  (* "2026-07-12 03:04:05.123+00" → first 16 chars-ish *)
  if String.length s <= 16 then s else String.sub s 0 16

let branches_flash (msg : string) : Pure_html.node list =
  if String.trim msg = "" then []
  else [ HTML.p [HTML.class_ "ok"] [txt "%s" msg] ]

let branches_err (msg : string) : Pure_html.node list =
  if String.trim msg = "" then []
  else [ HTML.p [HTML.class_ "err"] [txt "%s" msg] ]

let checkout_form ~(ref_ : string) ~(current : bool) : Pure_html.node =
  if current then
    HTML.span [HTML.class_ "muted"] [txt "current"]
  else
    HTML.form
      [ HTML.method_ `POST
      ; HTML.action "/branches/checkout"
      ; HTML.enctype `urlencoded
      ; HTML.class_ "inline-form"
      ]
      [ HTML.input [HTML.type_ "hidden"; HTML.name "ref"; HTML.value "%s" ref_]
      ; HTML.button [HTML.type_ "submit"; HTML.class_ "ol-btn"] [txt "Checkout"]
      ]

let list_branches_page (session : Session.t) (reqd : Httpaf.Reqd.t)
    ?(msg = "") ?(err = "") () : unit Lwt.t =
  let store = Session.store session in
  let current = Session.branch_name session in
  let* branches = Store.list_branches store in
  let idmap = id_to_ref_map branches in
  let* rows =
    Lwt_list.map_s
      (fun (b : Pricklypear_common.Branch.t) ->
        let ref_ = branch_ref_of b in
        let is_cur = ref_ = current || (current = "main" && ref_ = "system/main") in
        let* n_fn = Store.count_functions_on_branch store ~branch_id:b.id in
        let* n_rt = Store.count_routes_on_branch store ~branch_id:b.id in
        let* ov =
          Store.list_overrides store ~branch_id:b.id ~parent_id:b.parent_id
        in
        let n_ov = List.length ov in
        let star = if is_cur then "★" else "" in
        Lwt.return
          (HTML.tr
             (if is_cur then [HTML.class_ "br-current"] else [])
             [ HTML.td [] [txt "%s" star]
             ; HTML.td []
                 [ HTML.a [HTML.href "%s" ("/branches?ref=" ^ ref_)]
                     [txt "%s" (branch_label b)] ]
             ; HTML.td [] [txt "%s" (parent_ref_of idmap b)]
             ; HTML.td [] [txt "%d" n_fn]
             ; HTML.td [] [txt "%d" n_ov]
             ; HTML.td [] [txt "%d" n_rt]
             ; HTML.td [HTML.class_ "muted"] [txt "%s" (short_ts b.created_at)]
             ; HTML.td []
                 [ HTML.a
                     [ HTML.href "%s" ("/branches?ref=" ^ ref_)
                     ; HTML.class_ "btn secondary" ]
                     [txt "Open"]
                 ; txt " "
                 ; checkout_form ~ref_ ~current:is_cur
                 ]
             ]))
      branches
  in
  let fork_options =
    List.map
      (fun (b : Pricklypear_common.Branch.t) ->
        let ref_ = branch_ref_of b in
        HTML.option [HTML.value "%s" ref_] "%s" ref_)
      branches
  in
  let body =
    chrome ~session ~title:"Branches"
      (branches_flash msg @ branches_err err @
       [ HTML.p [HTML.class_ "muted"]
           [ txt "On: "
           ; HTML.strong [] [txt "%s" current]
           ; txt " — code branches; data_rows are shared."
           ]
       ; (match branches with
          | [] -> HTML.p [] [txt "(no branches in store)"]
          | _ ->
              HTML.div [HTML.class_ "br-table-wrap"]
                [ HTML.table [HTML.class_ "br-table"]
                    [ HTML.thead []
                        [ HTML.tr []
                            [ HTML.th [] [txt ""]
                            ; HTML.th [] [txt "branch"]
                            ; HTML.th [] [txt "parent"]
                            ; HTML.th [] [txt "own code"]
                            ; HTML.th [] [txt "overrides"]
                            ; HTML.th [] [txt "routes"]
                            ; HTML.th [] [txt "created"]
                            ; HTML.th [] [txt "actions"]
                            ]
                        ]
                    ; HTML.tbody [] rows
                    ]
                ])
       ; HTML.div [HTML.class_ "card"]
           [ HTML.h2 [] [txt "Create branch (full fork)"]
           ; HTML.p [HTML.class_ "muted"]
               [txt "Copies code from parent. Garden (personal) stays an empty inherit child."]
           ; HTML.form
               [ HTML.method_ `POST
               ; HTML.action "/branches/fork"
               ; HTML.enctype `urlencoded
               ]
               [ HTML.p []
                   [ HTML.label [] [txt "From "]
                   ; HTML.select [HTML.name "from"] fork_options
                   ]
               ; HTML.p []
                   [ HTML.label [] [txt "New name "]
                   ; HTML.input
                       [ HTML.type_ "text"
                       ; HTML.name "name"
                       ; HTML.placeholder "experiment"
                       ; HTML.required
                       ]
                   ]
               ; HTML.p []
                   [ HTML.button [HTML.type_ "submit"] [txt "Fork"] ]
               ]
           ]
       ])
  in
  html_response reqd body;
  Lwt.return_unit

let branch_detail_page (session : Session.t) (reqd : Httpaf.Reqd.t)
    ~(ref_ : string) ?(msg = "") ?(err = "") () : unit Lwt.t =
  let store = Session.store session in
  let current = Session.branch_name session in
  let owner, name =
    match String.split_on_char '/' ref_ with
    | [o; n] -> o, n
    | [n] -> "system", n
    | _ -> "system", ref_
  in
  let* found = Store.lookup_branch_id_by_name store ~owner ~name in
  match found with
  | None ->
      list_branches_page session reqd ~err:("no such branch: " ^ ref_) ()
  | Some bid ->
      let* branches = Store.list_branches store in
      let idmap = id_to_ref_map branches in
      let b_opt =
        List.find_opt
          (fun (b : Pricklypear_common.Branch.t) -> b.id = bid)
          branches
      in
      let parent_id =
        match b_opt with Some b -> b.parent_id | None -> None
      in
      let parent_ref =
        match b_opt with
        | Some b -> parent_ref_of idmap b
        | None -> "—"
      in
      let is_cur =
        ref_ = current
        || (owner ^ "/" ^ name) = current
      in
      let* n_fn = Store.count_functions_on_branch store ~branch_id:bid in
      let* n_rt = Store.count_routes_on_branch store ~branch_id:bid in
      let* ov = Store.list_overrides store ~branch_id:bid ~parent_id in
      let* own = Store.list_own_functions store ~branch_id:bid in
      let* routes = Store.list_own_routes store ~branch_id:bid in
      let n_ov = List.length ov in
      let override_rows =
        List.map
          (fun (fname, child_u, parent_u) ->
            let kind =
              match parent_u with
              | None -> "new"
              | Some pu when pu > child_u -> "main newer"
              | Some _ -> "override"
            in
            let kind_cls =
              match kind with
              | "main newer" -> "err"
              | "new" -> "ok"
              | _ -> "muted"
            in
            HTML.tr []
              [ HTML.td []
                  [ HTML.a [HTML.href "%s" ("/binding/" ^ fname)] [txt "%s" fname] ]
              ; HTML.td [HTML.class_ "%s" kind_cls] [txt "%s" kind]
              ; HTML.td [HTML.class_ "muted"] [txt "%s" (short_ts child_u)]
              ; HTML.td []
                  [ HTML.a
                      [ HTML.href "%s"
                          ("/branches?ref=" ^ ref_ ^ "&diff=" ^ fname)
                      ]
                      [txt "diff"]
                  ; txt " "
                  ; HTML.form
                      [ HTML.method_ `POST
                      ; HTML.action "/branches/unoverride"
                      ; HTML.enctype `urlencoded
                      ; HTML.class_ "inline-form"
                      ]
                      [ HTML.input
                          [ HTML.type_ "hidden"
                          ; HTML.name "ref"
                          ; HTML.value "%s" ref_
                          ]
                      ; HTML.input
                          [ HTML.type_ "hidden"
                          ; HTML.name "name"
                          ; HTML.value "%s" fname
                          ]
                      ; HTML.button
                          [ HTML.type_ "submit"
                          ; HTML.class_ "ol-btn"
                          ; HTML.title_ "Delete pin; inherit parent"
                          ]
                          [txt "use parent"]
                      ]
                  ]
              ])
          ov
      in
      let own_rows =
        List.map
          (fun (fname, author, upd) ->
            HTML.tr []
              [ HTML.td []
                  [ HTML.a [HTML.href "%s" ("/binding/" ^ fname)] [txt "%s" fname] ]
              ; HTML.td [] [txt "%s" author]
              ; HTML.td [HTML.class_ "muted"] [txt "%s" (short_ts upd)]
              ])
          own
      in
      let route_rows =
        List.map
          (fun (route, handler) ->
            HTML.tr []
              [ HTML.td [] [HTML.code [] [txt "%s" route]]
              ; HTML.td []
                  [ HTML.a [HTML.href "%s" ("/binding/" ^ handler)]
                      [txt "%s" handler] ]
              ])
          routes
      in
      let promote_checks =
        List.map
          (fun (fname, _, _) ->
            HTML.p []
              [ HTML.label []
                  [ HTML.input
                      [ HTML.type_ "checkbox"
                      ; HTML.name "name"
                      ; HTML.value "%s" fname
                      ]
                  ; txt " %s" fname
                  ]
              ])
          ov
      in
      (* optional diff *)
      let target = reqd |> Httpaf.Reqd.request |> fun r -> r.Httpaf.Request.target in
      let diff_name = query_param target "diff" in
      let* diff_block =
        match diff_name, parent_id with
        | None, _ | Some "", _ -> Lwt.return []
        | Some dname, None ->
            let* child =
              Store.lookup_function_on_branch store ~branch_id:bid ~name:dname
            in
            Lwt.return
              [ HTML.div [HTML.class_ "card"]
                  [ HTML.h2 [] [txt "Diff: %s (no parent)" dname]
                  ; (match child with
                     | None -> HTML.p [] [txt "(missing)"]
                     | Some (e, _, _) ->
                         HTML.pre []
                           [txt "%s" (Pricklypear_lisp.Reader.pp_expr e)])
                  ]
              ]
        | Some dname, Some pid ->
            let* child =
              Store.lookup_function_on_branch store ~branch_id:bid ~name:dname
            in
            let* parent =
              Store.lookup_function_on_branch store ~branch_id:pid ~name:dname
            in
            let show = function
              | None -> "(absent)"
              | Some (e, _, _) -> Pricklypear_lisp.Reader.pp_expr e
            in
            Lwt.return
              [ HTML.div [HTML.class_ "card"]
                  [ HTML.h2 [] [txt "Diff: %s" dname]
                  ; HTML.div [HTML.class_ "br-diff"]
                      [ HTML.div []
                          [ HTML.h3 [] [txt "parent"]
                          ; HTML.pre [] [txt "%s" (show parent)]
                          ]
                      ; HTML.div []
                          [ HTML.h3 [] [txt "this branch"]
                          ; HTML.pre [] [txt "%s" (show child)]
                          ]
                      ]
                  ]
              ]
      in
      let body =
        chrome ~session ~title:("Branch " ^ ref_)
          (branches_flash msg @ branches_err err @
           [ HTML.p []
               [ HTML.a [HTML.href "/branches"] [txt "← all branches"]
               ; txt " · parent: "
               ; (if parent_ref = "—" then txt "—"
                  else
                    HTML.a [HTML.href "%s" ("/branches?ref=" ^ parent_ref)]
                      [txt "%s" parent_ref])
               ; txt " · "
               ; checkout_form ~ref_ ~current:is_cur
               ]
           ; HTML.div [HTML.class_ "br-stats"]
               [ HTML.span [HTML.class_ "card br-stat"]
                   [txt "own functions: %d" n_fn]
               ; HTML.span [HTML.class_ "card br-stat"]
                   [txt "overrides: %d" n_ov]
               ; HTML.span [HTML.class_ "card br-stat"]
                   [txt "own routes: %d" n_rt]
               ]
           ; HTML.div [HTML.class_ "card"]
               [ HTML.h2 [] [txt "Overrides / own pins"]
               ; HTML.p [HTML.class_ "muted"]
                   [txt "Rows on this branch. «use parent» deletes the pin (unoverride)."]
               ; (if override_rows = [] then
                    HTML.p [HTML.class_ "muted"] [txt "(none — fully inherited)"]
                  else
                    HTML.table [HTML.class_ "br-table"]
                      [ HTML.thead []
                          [ HTML.tr []
                              [ HTML.th [] [txt "name"]
                              ; HTML.th [] [txt "kind"]
                              ; HTML.th [] [txt "updated"]
                              ; HTML.th [] [txt "actions"]
                              ]
                          ]
                      ; HTML.tbody [] override_rows
                      ])
               ]
           ; HTML.div [HTML.class_ "card"]
               [ HTML.h2 [] [txt "All own code"]
               ; (if own_rows = [] then
                    HTML.p [HTML.class_ "muted"] [txt "(empty branch)"]
                  else
                    HTML.table [HTML.class_ "br-table"]
                      [ HTML.thead []
                          [ HTML.tr []
                              [ HTML.th [] [txt "name"]
                              ; HTML.th [] [txt "author"]
                              ; HTML.th [] [txt "updated"]
                              ]
                          ]
                      ; HTML.tbody [] own_rows
                      ])
               ]
           ; HTML.div [HTML.class_ "card"]
               [ HTML.h2 [] [txt "Own routes"]
               ; (if route_rows = [] then
                    HTML.p [HTML.class_ "muted"] [txt "(none on this branch)"]
                  else
                    HTML.table [HTML.class_ "br-table"]
                      [ HTML.thead []
                          [ HTML.tr []
                              [ HTML.th [] [txt "route"]
                              ; HTML.th [] [txt "handler"]
                              ]
                          ]
                      ; HTML.tbody [] route_rows
                      ])
               ]
           ; HTML.div [HTML.class_ "card"]
               [ HTML.h2 [] [txt "Promote to system/main"]
               ; HTML.p [HTML.class_ "muted"]
                   [txt "Select pins to merge into live defaults. Also pp_sync pull + git push for durable libs/*.pp."]
               ; (if promote_checks = [] then
                    HTML.p [HTML.class_ "muted"] [txt "(nothing to promote)"]
                  else
                    HTML.form
                      [ HTML.method_ `POST
                      ; HTML.action "/branches/promote"
                      ; HTML.enctype `urlencoded
                      ]
                      (HTML.input
                         [ HTML.type_ "hidden"
                         ; HTML.name "ref"
                         ; HTML.value "%s" ref_
                         ]
                       :: promote_checks
                       @ [ HTML.p []
                             [ HTML.button [HTML.type_ "submit"]
                                 [txt "Merge selected → system/main"] ]
                         ]))
               ]
           ]
           @ diff_block)
      in
      html_response reqd body;
      Lwt.return_unit

let branches_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let target = (Httpaf.Reqd.request reqd).target in
  match query_param target "ref" with
  | Some ref_ when String.trim ref_ <> "" ->
      branch_detail_page session reqd ~ref_:(String.trim ref_) ()
  | _ -> list_branches_page session reqd ()

let branches_checkout (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let ref_ = Option.value (form_field body "ref") ~default:"" |> String.trim in
  if ref_ = "" then list_branches_page session reqd ~err:"ref required" ()
  else
    let* r = Session.checkout_ref session ref_ in
    match r with
    | Error e -> list_branches_page session reqd ~err:e ()
    | Ok () ->
        redirect_response reqd ("/branches?ref=" ^ ref_);
        Lwt.return_unit

let branches_fork (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let from_ = Option.value (form_field body "from") ~default:"system/main" in
  let name = Option.value (form_field body "name") ~default:"" |> String.trim in
  if name = "" then list_branches_page session reqd ~err:"name required" ()
  else
    let owner = Session.username session in
    let child_ref =
      if String.contains name '/' then name else owner ^ "/" ^ name
    in
    let* r = Session.fork_ref session ~parent_ref:from_ ~child_ref in
    match r with
    | Error e -> list_branches_page session reqd ~err:e ()
    | Ok _ ->
        list_branches_page session reqd
          ~msg:("forked " ^ child_ref ^ " from " ^ from_) ()

let branches_unoverride (session : Session.t) (reqd : Httpaf.Reqd.t)
  : unit Lwt.t =
  let* body = read_body reqd in
  let ref_ = Option.value (form_field body "ref") ~default:"" |> String.trim in
  let name = Option.value (form_field body "name") ~default:"" |> String.trim in
  (* Checkout target branch first if needed so delete hits the right id. *)
  let* _ =
    if ref_ <> "" && Session.branch_name session <> ref_ then
      Session.checkout_ref session ref_
    else Lwt.return (Ok ())
  in
  let* r = Session.unoverride session name in
  match r with
  | Error e ->
      branch_detail_page session reqd ~ref_ ~err:e ()
  | Ok true ->
      branch_detail_page session reqd ~ref_
        ~msg:("unoverrode " ^ name ^ " — now inherits parent") ()
  | Ok false ->
      branch_detail_page session reqd ~ref_
        ~err:("no pin named " ^ name ^ " on this branch") ()

let branches_promote (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let ref_ = Option.value (form_field body "ref") ~default:"" |> String.trim in
  let form = parse_form_body body in
  let names =
    List.filter_map
      (fun (k, v) -> if k = "name" && String.trim v <> "" then Some v else None)
      form
  in
  let* r = Session.promote_to_main session ~source_ref:ref_ ~names in
  match r with
  | Error e -> branch_detail_page session reqd ~ref_ ~err:e ()
  | Ok n ->
      branch_detail_page session reqd ~ref_
        ~msg:(Printf.sprintf "promoted %d function(s) → system/main" n) ()

let show_binding_page (session : Session.t) (name : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* found = Store.lookup_function (Session.store session) name in
  let body =
    match found with
    | None ->
        chrome ~session ~title:name [
          HTML.p [] [txt "(no such binding on this branch)"];
          HTML.p [] [HTML.a [HTML.href "%s" ("/edit/" ^ name)] [txt "create it"]];
        ]
    | Some (expr, author) ->
        let pp = Pricklypear_lisp.Reader.pp_expr expr in
        chrome ~session ~title:name [
          HTML.p [] [txt "author: %s" author];
          HTML.pre [] [txt "%s" pp];
          HTML.p [] [
            HTML.a [HTML.href "%s" (Packages.edit_href name)] [txt "edit"];
            txt " · ";
            HTML.a [HTML.href "%s" (Packages.inspect_href name)] [txt "inspect"];
          ];
        ]
  in
  html_response reqd body;
  Lwt.return_unit

let list_types_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* types = Store.list_types (Session.store session) in
  let rows =
    List.map
      (fun (name, fields) ->
        let field_list = String.concat ", " fields in
        HTML.tr [] [
          HTML.td [] [txt "%s" name];
          HTML.td [] [txt "%s" field_list];
        ])
      types
  in
  let body =
    chrome ~session ~title:("Data types (" ^ Session.branch_name session ^ ")") [
      HTML.p [HTML.class_ "meta"] [
        txt "Declared shapes for Data rows. Create instances via (create 'type \"{...}\") in the REPL.";
      ];
      (match types with
       | [] ->
           HTML.p [] [txt "(no types yet; declare via (deftype 'name 'field1 ...))"]
       | _ ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "type"];
                 HTML.th [] [txt "fields"];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd body;
  Lwt.return_unit

let about_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit =
  let body =
    chrome ~session ~title:"About Pricklypear" [
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "What is this?"];
        HTML.p [] [txt
          "Pricklypear is a multi-user live computing environment. You log into your running image, write Lisp functions that are stored as durable data in Postgres, branch code like git while sharing data, bind functions to URLs, and optionally let AI propose functions onto your branch."];
        HTML.p [] [txt
          "It is Smalltalk-like (live, inspectable, editable) without being Smalltalk (no single-user OOP image dump). The image is the CPU; Postgres is the disk."];
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Core concepts"];
        HTML.ul [] [
          HTML.li [] [txt "Image — your running personal process (REPL + HTTP). Disposable; rebuildable from the store."];
          HTML.li [] [txt "Store — Postgres holds functions, types, routes, data rows, auth, branches."];
          HTML.li [] [txt "Function / binding — a named live value, usually a lambda. Top-level (define ...) persists it."];
          HTML.li [] [txt "Branch — versioned code world (system/main, alice/experiment). Code branches; data is shared."];
          HTML.li [] [txt "Type + row — (deftype ...) then (create/get/update/rows ...) over shared data_rows."];
          HTML.li [] [txt "Route / page — (route \"/path\" \"handler\") makes a URL call a function."];
          HTML.li [] [txt "AI propose — model writes a function on the current branch as agent:<model>; you keep or discard."];
        ];
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "How to access"];
        HTML.ul [] [
          HTML.li [] [txt "Start stack: scripts/dev.sh start"];
          HTML.li [] [txt "Text login/REPL: scripts/repl.sh  (SSH public key line protocol on :17822)"];
          HTML.li [] [txt "This browser UI: usually http://127.0.0.1:18080/ after an image boots"];
          HTML.li [] [txt "Browser: /  Workspace home · /about · /bindings Functions · /pages Pages · /types Data · /repl · /edit/<name>"];
          HTML.li [] [txt "Auth admin API: http://127.0.0.1:17879  (create users, tokens)"];
          HTML.li [] [txt "Full docs: docs/concepts-and-horizon.md in the repo"];
        ];
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "5-minute first tour"];
        HTML.pre [] [txt "%s"
          "1. scripts/repl.sh\n\
2. (define hi-page (lambda () (render-html \"<h1>Hello</h1>\")))\n\
3. (route \"/hello\" \"hi-page\")\n\
4. open /hello in the browser\n\
5. edit at /edit/hi-page\n\
6. optional: (propose-function \"double\" \"lambda of int x -> (* x 2)\")"];
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Long-term possibilities"];
        HTML.ul [] [
          HTML.li [] [txt "Personal OS — always-on image; notes, CRM, automations as live functions."];
          HTML.li [] [txt "Community commons — system/main as shared tools; personal gardens as branches."];
          HTML.li [] [txt "AI workshop — AI cohabits the image; propose apps (types+fns+routes); merge selectively."];
          HTML.li [] [txt "Internal tools platform — inspectable live ops/admin tools without redeploying for every change."];
          HTML.li [] [txt "Education — legible multi-user systems/PL lab with forks and merges."];
          HTML.li [] [txt "End-user products — friendlier skin (Functions/Pages/Data) on the same substrate."];
          HTML.li [] [txt "Hardened service — Solo5, real SSH/web login, stronger auth and permissions."];
        ];
        HTML.p [] [
          HTML.a [HTML.href "/"] [txt "back home"];
          txt " · ";
          HTML.a [HTML.href "/repl"] [txt "open REPL"];
        ];
      ];
    ]
  in
  html_response reqd body

(* Page path for inspector: "/hello" -> "hello" for URL segment. *)
let page_inspect_path (route : string) : string =
  if String.length route > 0 && route.[0] = '/' then
    String.sub route 1 (String.length route - 1)
  else route

let pages_list_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit =
  let routes = Session.list_pages session in
  let rows =
    List.map
      (fun (route, handler) ->
        let inspect = "/page/" ^ page_inspect_path route in
        HTML.tr [] [
          HTML.td [] [HTML.a [HTML.href "%s" route] [txt "%s" route]];
          HTML.td [] [
            HTML.a [HTML.href "%s" (Packages.edit_href handler)] [txt "%s" handler];
          ];
          HTML.td [] [
            HTML.a [HTML.href "%s" route] [txt "open"];
            txt " · ";
            HTML.a [HTML.href "%s" (Packages.edit_href handler)] [txt "edit function"];
            txt " · ";
            HTML.a [HTML.href "%s" inspect] [txt "inspect page"];
          ];
        ])
      routes
  in
  let body =
    chrome ~session ~title:"Pages" [
      HTML.p [HTML.class_ "meta"] [
        txt "Page = URL endpoint. Function = code it runs. Edit functions; open pages.";
      ];
      (match routes with
       | [] ->
           HTML.p [] [
             txt "(no pages yet — use the Hello recipe on Home, or (route \"/x\" \"fn\") in the REPL)";
           ]
       | _ ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "page (URL)"];
                 HTML.th [] [txt "function (handler)"];
                 HTML.th [] [txt "actions"];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd body

(** Labeled emergency home — NOT a second product workspace.
    Real home is Lisp ui/home (libs/ui). See image.system-browser. *)
let emergency_home_content (session : Session.t) : Pure_html.node list =
  let who =
    Printf.sprintf
      "You are %s on branch %s. Product home is Lisp (ui/home); this page is host recovery only."
      (Session.username session) (Session.branch_name session)
  in
  [ HTML.div [HTML.class_ "card emergency-home"] [
      HTML.h2 [] [txt "EMERGENCY HOME — ui/home missing or failed"];
      HTML.p [] [txt "%s" who];
      HTML.p [] [
        HTML.a [HTML.href "/bindings?pkg=ui"] [txt "Open ui package"];
        txt " · ";
        HTML.a [HTML.href "/bindings"] [txt "Functions"];
        txt " · ";
        HTML.a [HTML.href "/repl"] [txt "REPL"];
        txt " · ";
        HTML.a [HTML.href "/about"] [txt "About"];
        txt " · ";
        HTML.a [HTML.href "/login"] [txt "Login"];
      ];
      HTML.p [HTML.class_ "muted"] [
        txt "%s"
          "Fix: ensure system/main has ui/* from seed, or delete a broken personal pin of ui/home / ui/menu / ui/nav.";
      ];
    ]
  ]

let home_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  (* Nav comes from Session.ui_nav_html (refreshed in request launch). *)
  let* home_html = try_ui_fn session "ui/home" in
  let content =
    match home_html with
    | Some html when String.trim html <> "" ->
        [ txt ~raw:true "%s" html ]
    | _ -> emergency_home_content session
  in
  let body = chrome ~session ~title:"" content in
  html_response reqd body;
  Lwt.return_unit

let repl_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit =
  let js =
    "const out=document.getElementById('out');" ^
    "document.getElementById('run').onclick=async()=>{" ^
    "const expr=document.getElementById('expr').value;" ^
    "out.textContent='...';" ^
    "try{" ^
    "const r=await fetch('/api/eval',{method:'POST'," ^
    "headers:{'content-type':'application/json'}," ^
    "body:JSON.stringify({expr})});" ^
    "const j=await r.json();" ^
    "out.textContent=j.ok?j.value:('error: '+j.error);" ^
    "out.className=j.ok?'ok':'err';" ^
    "}catch(e){out.textContent=String(e);out.className='err';}" ^
    "};" ^
    "document.getElementById('clear').onclick=()=>{out.textContent='';};"
  in
  let body =
    chrome ~session ~title:"Browser REPL" [
      HTML.div [HTML.class_ "card"] [
        HTML.p [] [txt "Eval against the live session env. (define ...) persists."];
        HTML.textarea [HTML.id "expr"] "%s" "(+ 1 2)";
        HTML.p [] [
          HTML.button [HTML.id "run"; HTML.type_ "button"] [txt "Eval"];
          txt " ";
          HTML.button [HTML.id "clear"; HTML.type_ "button"; HTML.class_ "secondary"]
            [txt "Clear out"];
        ];
        HTML.pre [HTML.id "out"] [txt ""];
      ];
      HTML.script [] "%s" js;
    ]
  in
  html_response reqd body

(* Default stub body for a brand-new function. *)
let stub_body (name : string) : string =
  Printf.sprintf "(lambda ()\n  \"TODO: implement %s\")" name

(* Static user-function callees: walk AST App heads that are Vars. *)
let rec collect_var_apps (acc : string list) (e : Pricklypear_lisp.Ast.expr)
    : string list =
  match e with
  | Pricklypear_lisp.Ast.Var _ | Pricklypear_lisp.Ast.Lit _ -> acc
  | Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var f, args) ->
      List.fold_left collect_var_apps (f :: acc) args
  | Pricklypear_lisp.Ast.App (f, args) ->
      List.fold_left collect_var_apps (collect_var_apps acc f) args
  | Pricklypear_lisp.Ast.Let (_, e1, e2) ->
      collect_var_apps (collect_var_apps acc e1) e2
  | Pricklypear_lisp.Ast.Letrec (binds, body) ->
      let acc =
        List.fold_left (fun a (_, e) -> collect_var_apps a e) acc binds
      in
      collect_var_apps acc body
  | Pricklypear_lisp.Ast.If (c, t, e) ->
      collect_var_apps (collect_var_apps (collect_var_apps acc c) t) e
  | Pricklypear_lisp.Ast.Lambda (_, body) -> collect_var_apps acc body
  | Pricklypear_lisp.Ast.Prim (_, args) ->
      List.fold_left collect_var_apps acc args
  | Pricklypear_lisp.Ast.Seq (a, b) ->
      collect_var_apps (collect_var_apps acc a) b
  | Pricklypear_lisp.Ast.Quote _ -> acc
  | Pricklypear_lisp.Ast.Define (_, body) -> collect_var_apps acc body

let page_function_graph (session : Session.t) ~(handler : string)
    : (string * string) list Lwt.t =
  let max_depth = 8 in
  let rec walk depth seen name role acc =
    if depth > max_depth || List.mem name seen then Lwt.return acc
    else
      let* found = Store.lookup_function (Session.store session) name in
      match found with
      | None -> Lwt.return ((name, role ^ " (missing)") :: acc)
      | Some (expr, _) ->
          let acc = (name, role) :: acc in
          let seen = name :: seen in
          let candidates = collect_var_apps [] expr in
          let rec filter_bind names out =
            match names with
            | [] -> Lwt.return (List.rev out)
            | n :: rest ->
                let* f = Store.lookup_function (Session.store session) n in
                (match f with
                 | Some _ when n <> name && not (List.mem n out) ->
                     filter_bind rest (n :: out)
                 | _ -> filter_bind rest out)
          in
          let* callees = filter_bind candidates [] in
          let rec fold_c cs acc =
            match cs with
            | [] -> Lwt.return acc
            | c :: rest ->
                let* acc = walk (depth + 1) seen c "callee" acc in
                fold_c rest acc
          in
          fold_c callees acc
  in
  let* rows = walk 0 [] handler "handler" [] in
  Lwt.return (List.rev rows)

let contains_ci (hay : string) (needle : string) : bool =
  if needle = "" then true
  else
    let h = String.lowercase_ascii hay in
    let n = String.lowercase_ascii needle in
    let rec loop i =
      if i + String.length n > String.length h then false
      else if String.sub h i (String.length n) = n then true
      else loop (i + 1)
    in
    loop 0

(** function-name → routes that use it as the *direct* HTTP handler.

    Previously this ran [page_function_graph] for every route (recursive
    AST walk + Store.lookup_function per callee, depth 8) — hundreds of
    PG round-trips on every /bindings load. "Used by" on the package
    list only needs the direct handler mapping; deep graphs stay on
    /page/<route> inspect. *)
let used_by_pages_map (session : Session.t)
    : (string, string list) Hashtbl.t Lwt.t =
  let routes = Session.list_pages session in
  let tbl : (string, string list) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (route, handler) ->
      let prev = try Hashtbl.find tbl handler with Not_found -> [] in
      if List.mem route prev then ()
      else Hashtbl.replace tbl handler (route :: prev))
    routes;
  Lwt.return tbl

let list_bindings_page (session : Session.t) (reqd : Httpaf.Reqd.t)
    : unit Lwt.t =
  let target = (Httpaf.Reqd.request reqd).Httpaf.Request.target in
  let q = Option.value (query_param target "q") ~default:"" |> String.trim in
  let pkg_q = query_param target "pkg" in
  let fn_q = query_param target "fn" in
  let* defs_meta =
    Store.list_function_names_meta (Session.store session)
  in
  (* Effective view only: one row per name (closest branch wins). *)
  let defs =
    List.map (fun (n, e, a, d) -> (n, e, a, d)) defs_meta
  in
  let* used_by = used_by_pages_map session in
  let kernel = Session.kernel_catalog session in
  let matches_ws (name, _body, author, _depth) =
    q = ""
    || contains_ci name q
    || contains_ci (Packages.package_of_name name) q
    || contains_ci author q
  in
  let defs_f = List.filter matches_ws defs in
  (* group_by_package expects 3-tuples; strip depth for grouping. *)
  let defs_for_group =
    List.map (fun (n, e, a, _) -> (n, e, a)) defs_f
  in
  let groups = Packages.group_by_package defs_for_group in
  let depth_of name =
    match List.find_opt (fun (n, _, _, _) -> n = name) defs_f with
    | Some (_, _, _, d) -> d
    | None -> 0
  in
  let origin_badge depth author =
    if depth = 0 then
      HTML.span [HTML.class_ "ok"] [txt "own"]
    else
      HTML.span [HTML.class_ "muted"]
        [txt "inherited · %s" author]
  in
  let used_links name =
    let routes = try Hashtbl.find used_by name with Not_found -> [] in
    match List.rev routes with
    | [] -> HTML.span [HTML.class_ "muted"] [txt "—"]
    | rs ->
        HTML.span []
          (List.concat
             (List.mapi
                (fun i route ->
                  let link =
                    HTML.a
                      [HTML.href "%s" ("/page/" ^ page_inspect_path route)]
                      [txt "%s" route]
                  in
                  if i = 0 then [link] else [txt " "; link])
                rs))
  in
  let pkg_href pkg =
    "/bindings?pkg=" ^ Packages.name_encode pkg
  in
  let fn_href pkg fn =
    "/bindings?pkg=" ^ Packages.name_encode pkg ^ "&fn="
    ^ Packages.name_encode fn
  in
  match pkg_q, fn_q with
  | Some "kernel", Some fname ->
      let summary =
        match List.assoc_opt fname kernel with
        | Some s -> s
        | None -> "unknown primitive"
      in
      let body =
        chrome ~session ~title:("Primitive " ^ fname) [
          HTML.p [] [
            HTML.a [HTML.href "/bindings"] [txt "Functions"];
            txt " › ";
            HTML.a [HTML.href "/bindings?pkg=kernel"] [txt "(kernel)"];
            txt " › %s" fname;
          ];
          HTML.div [HTML.class_ "card"] [
            HTML.p [] [
              HTML.span [HTML.class_ "muted"] [txt "kind: "];
              txt "primitive (OCaml)";
            ];
            HTML.p [] [
              HTML.span [HTML.class_ "muted"] [txt "name: "];
              txt "%s" fname;
            ];
            HTML.p [] [txt "%s" summary];
            HTML.p [HTML.class_ "muted"] [
              txt "Built into the image kernel — not stored as Lisp. App logic should usually be Lisp functions you can edit.";
            ];
          ];
        ]
      in
      html_response reqd body;
      Lwt.return_unit
  | Some pkg, Some local ->
      let full =
        if String.contains local '/' then local
        else if pkg = "" || pkg = "(root)" || pkg = "root" then local
        else Packages.join_name ~package:pkg ~local
      in
      let* found = Store.lookup_function (Session.store session) full in
      let crumb_pkg = if pkg = "(root)" then "" else pkg in
      let body =
        match found with
        | None ->
            chrome ~session ~title:full [
              HTML.p [] [
                HTML.a [HTML.href "/bindings"] [txt "Functions"];
                txt " › ";
                HTML.a [HTML.href "%s" (pkg_href pkg)]
                  [txt "%s" (Packages.package_label crumb_pkg)];
              ];
              HTML.p [HTML.class_ "err"] [txt "no function %s" full];
              HTML.p [] [
                HTML.a [HTML.href "%s" (Packages.edit_href full)] [txt "create it"];
              ];
            ]
        | Some (expr, author) ->
            let badge =
              Pricklypear_lisp.Effect.EffSet.badge
                (Pricklypear_lisp.Effect.infer expr)
            in
            let pp = Pricklypear_lisp.Reader.pp_expr expr in
            let depth = depth_of full in
            chrome ~session ~title:full [
              HTML.p [] [
                HTML.a [HTML.href "/bindings"] [txt "Functions"];
                txt " › ";
                HTML.a [HTML.href "%s" (pkg_href pkg)]
                  [txt "%s" (Packages.package_label crumb_pkg)];
                txt " › %s" (Packages.local_name full);
              ];
              HTML.div [HTML.class_ "card"] [
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "kind: "];
                  txt "function (Lisp)";
                ];
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "origin: "];
                  origin_badge depth author;
                ];
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "author: "];
                  txt "%s" author;
                ];
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "effects: "];
                  txt "%s" badge;
                ];
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "used by: "];
                  used_links full;
                ];
                HTML.h2 [] [txt "definition"];
                HTML.pre [] [txt "%s" pp];
                HTML.p [] [
                  HTML.a
                    [ HTML.href "%s" (Packages.edit_href full)
                    ; HTML.class_ "btn" ]
                    [txt "Edit"];
                  txt " ";
                  HTML.a
                    [ HTML.href "%s" (Packages.inspect_href full)
                    ; HTML.class_ "btn secondary" ]
                    [txt "Inspect AST"];
                ];
              ];
            ]
      in
      html_response reqd body;
      Lwt.return_unit
  | Some "kernel", None ->
      let rows =
        if q = "" then kernel
        else List.filter (fun (n, s) -> contains_ci n q || contains_ci s q) kernel
      in
      let items =
        List.map
          (fun (n, summary) ->
            HTML.tr [] [
              HTML.td [] [
                HTML.a [HTML.href "%s" (fn_href "kernel" n)] [txt "%s" n];
              ];
              HTML.td [] [txt "primitive"];
              HTML.td [] [txt "%s" summary];
            ])
          rows
      in
      let body =
        chrome ~session ~title:"Package (kernel)" [
          HTML.p [] [HTML.a [HTML.href "/bindings"] [txt "← all packages"]];
          HTML.p [HTML.class_ "meta"] [
            txt "Built-in OCaml primitives. Read-only. Prefer app logic as Lisp.";
          ];
          HTML.table [] [
            HTML.thead [] [
              HTML.tr [] [
                HTML.th [] [txt "name"];
                HTML.th [] [txt "kind"];
                HTML.th [] [txt "summary"];
              ];
            ];
            HTML.tbody [] items;
          ];
        ]
      in
      html_response reqd body;
      Lwt.return_unit
  | Some pkg, None ->
      let pkg_key = if pkg = "(root)" || pkg = "root" then "" else pkg in
      let rows =
        List.filter
          (fun (name, _, _, _) -> Packages.package_of_name name = pkg_key)
          defs_f
      in
      let items =
        List.map
          (fun (name, body, author, depth) ->
            let badge =
              Pricklypear_lisp.Effect.EffSet.badge
                (Pricklypear_lisp.Effect.infer body)
            in
            let local = Packages.local_name name in
            HTML.tr [] [
              HTML.td [] [
                HTML.a [HTML.href "%s" (fn_href pkg local)] [txt "%s" name];
              ];
              HTML.td [] [txt "%s" badge];
              HTML.td [] [origin_badge depth author];
              HTML.td [] [used_links name];
            ])
          rows
      in
      let body =
        chrome ~session ~title:("Package " ^ Packages.package_label pkg_key) [
          HTML.p [] [HTML.a [HTML.href "/bindings"] [txt "← all packages"]];
          HTML.p [HTML.class_ "muted"] [
            txt "Effective view for branch %s — one row per name (own or inherited)."
              (Session.branch_name session);
          ];
          (match items with
           | [] -> HTML.p [] [txt "(no functions in this package)"]
           | _ ->
               HTML.table [] [
                 HTML.thead [] [
                   HTML.tr [] [
                     HTML.th [] [txt "function"];
                     HTML.th [] [txt "effects"];
                     HTML.th [] [txt "origin"];
                     HTML.th [] [txt "used by"];
                   ];
                 ];
                 HTML.tbody [] items;
               ]);
          HTML.p [] [
            HTML.a
              [ HTML.href "%s"
                  (if pkg_key = "" then "/edit/?local="
                   else
                     "/edit/?package=" ^ Packages.name_encode pkg_key
                     ^ "&local=") ]
              [txt "new in this package…"];
          ];
        ]
      in
      html_response reqd body;
      Lwt.return_unit
  | None, _ ->
      let ws_pkgs =
        List.map
          (fun (pkg, rows) ->
            let label = Packages.package_label pkg in
            let key = if pkg = "" then "root" else pkg in
            (label, pkg_href key, List.length rows, "workspace"))
          groups
      in
      let all_pkgs =
        ( "(kernel)"
        , "/bindings?pkg=kernel"
        , List.length kernel
        , "kernel" )
        :: ws_pkgs
      in
      let filtered =
        if q = "" then all_pkgs
        else List.filter (fun (label, _, _, _) -> contains_ci label q) all_pkgs
      in
      let items =
        List.map
          (fun (label, href, n, kind) ->
            HTML.tr [] [
              HTML.td [] [HTML.a [HTML.href "%s" href] [txt "%s" label]];
              HTML.td [] [txt "%s" kind];
              HTML.td [] [txt "%d" n];
            ])
          filtered
      in
      let body =
        chrome ~session
          ~title:("Functions (" ^ Session.branch_name session ^ ")")
          [ HTML.p [HTML.class_ "meta"] [
              txt "Drill: package → function → definition. Kernel = OCaml prims (read-only). Workspace = Lisp you can edit.";
            ]
          ; HTML.div [HTML.class_ "card"] [
              HTML.form [HTML.method_ `GET; HTML.action "/bindings"] [
                HTML.p [] [
                  HTML.label [HTML.for_ "q"] [txt "search "];
                  HTML.input
                    [ HTML.type_ "text"
                    ; HTML.name "q"
                    ; HTML.id "q"
                    ; HTML.value "%s" q
                    ; HTML.placeholder "package or name" ];
                  txt " ";
                  HTML.button [HTML.type_ "submit"] [txt "Filter"];
                ];
              ];
            ]
          ; HTML.div [HTML.class_ "card"] [
              HTML.h2 [] [txt "New function"];
              HTML.form [HTML.method_ `GET; HTML.action "/edit/"] [
                HTML.p [] [
                  HTML.label [] [txt "package "];
                  HTML.input
                    [ HTML.type_ "text"
                    ; HTML.name "package"
                    ; HTML.placeholder "todo" ];
                  txt " ";
                  HTML.label [] [txt "name "];
                  HTML.input
                    [ HTML.type_ "text"
                    ; HTML.name "local"
                    ; HTML.placeholder "list" ];
                  txt " ";
                  HTML.button [HTML.type_ "submit"] [txt "Create / edit"];
                ];
              ];
            ]
          ; HTML.table [] [
              HTML.thead [] [
                HTML.tr [] [
                  HTML.th [] [txt "package"];
                  HTML.th [] [txt "kind"];
                  HTML.th [] [txt "count"];
                ];
              ];
              HTML.tbody [] items;
            ]
          ; HTML.p [HTML.class_ "muted"] [
              txt "Tip: open (kernel) to browse create, +, render-html, propose-function, …";
            ]
          ]
      in
      html_response reqd body;
      Lwt.return_unit

let editor_form_partial ~(name : string) ~(body_src : string) ?(msg = "") ()
    : Pure_html.node =
  let flash =
    if msg = "" then HTML.div [] []
    else
      HTML.p
        [ HTML.class_
            (if String.length msg >= 5 && String.sub msg 0 5 = "error" then "err"
             else "ok") ]
        [ txt "%s" msg ]
  in
  HTML.div [HTML.id "editor-root"] [
    flash;
    HTML.div [HTML.class_ "card"] [
      HTML.form
        [ HTML.method_ `POST
        ; HTML.action "%s" ("/edit/" ^ name)
        ; HTML.enctype `urlencoded ]
        [ HTML.p [] [
            HTML.label [HTML.for_ "name"] [txt "name "];
            HTML.input
              [ HTML.type_ "text"
              ; HTML.name "name"
              ; HTML.id "name"
              ; HTML.value "%s" name ];
          ]
        ; HTML.p [] [HTML.label [HTML.for_ "body"] [txt "body (s-expression)"]]
        ; (HTML.textarea [HTML.name "body"; HTML.id "body"] "%s" body_src)
        ; HTML.p [] [
            HTML.button [HTML.type_ "submit"] [txt "Save (define + persist)"];
            txt " ";
            HTML.a
              [ HTML.href "%s" ("/binding/" ^ name)
              ; HTML.class_ "btn secondary" ]
              [ txt "View" ];
          ];
        ];
    ];
  ]

let route_for_edit_name (session : Session.t) (name : string)
    : (string * string) option =
  let route = "/" ^ name in
  match Session.lookup_route session route with
  | Some handler -> Some (route, handler)
  | None ->
      (match Session.lookup_route session name with
       | Some handler -> Some (name, handler)
       | None -> None)

let missing_editor_card (session : Session.t) (name : string) : Pure_html.node =
  let route_opt = route_for_edit_name session name in
  let route_block =
    match route_opt with
    | None -> HTML.div [] []
    | Some (route, handler) ->
        HTML.div [HTML.class_ "card"] [
          HTML.p [] [
            txt "There is a page ";
            HTML.a [HTML.href "%s" route] [txt "%s" route];
            txt " that currently runs function ";
            HTML.span [HTML.class_ "ok"] [txt "%s" handler];
            txt ".";
          ];
          HTML.p [] [
            HTML.a [HTML.href "%s" (Packages.edit_href handler); HTML.class_ "btn"]
              [txt "Edit %s" handler];
            txt " ";
            HTML.a
              [ HTML.href "%s" ("/page/" ^ page_inspect_path route)
              ; HTML.class_ "btn secondary" ]
              [ txt "Inspect page" ];
          ];
        ]
  in
  let create_form =
    HTML.div [HTML.class_ "card"] [
      HTML.p [] [txt "Function \"%s\" does not exist." name];
      HTML.p [HTML.class_ "muted"] [
        txt "Create it here — the editor loads in place (htmx).";
      ];
      HTML.form
        [ HTML.method_ `POST
        ; HTML.action "/api/edit/create"
        ; HTML.enctype `urlencoded
        ; Hx.post "/api/edit/create"
        ; Hx.target "#editor-root"
        ; Hx.swap "outerHTML" ]
        [ HTML.input [HTML.type_ "hidden"; HTML.name "name"; HTML.value "%s" name]
        ; (match route_opt with
           | Some (route, _) ->
               HTML.p [] [
                 HTML.label [] [
                   HTML.input
                     [ HTML.type_ "checkbox"
                     ; HTML.name "rebind_route"
                     ; HTML.value "1" ];
                   txt " also point page %s at this new function" route;
                 ];
               ]
           | None -> HTML.div [] [])
        ; HTML.p [] [
            HTML.button [HTML.type_ "submit"] [txt "Create function %s" name];
            txt " ";
            HTML.a [HTML.href "/bindings"; HTML.class_ "btn secondary"]
              [txt "Functions list"];
          ];
        ];
    ]
  in
  HTML.div [HTML.id "editor-root"] [route_block; create_form]

let editor_page (session : Session.t) (name : string) ?(msg = "")
    ?(body_src = "") (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* found = Store.lookup_function (Session.store session) name in
  let body =
    match found, body_src with
    | _, src when src <> "" ->
        chrome ~session ~title:("Edit " ^ name)
          [ editor_form_partial ~name ~body_src:src ~msg () ]
    | Some (expr, _), _ ->
        let initial = Pricklypear_lisp.Reader.pp_expr expr in
        chrome ~session ~title:("Edit " ^ name)
          [ editor_form_partial ~name ~body_src:initial ~msg () ]
    | None, _ ->
        chrome ~session ~title:("Edit " ^ name)
          [ missing_editor_card session name ]
  in
  html_response reqd body;
  Lwt.return_unit

let api_edit_create (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let name =
    match form_field body "name" with
    | Some n when String.trim n <> "" -> String.trim n
    | _ -> ""
  in
  if name = "" then begin
    html_response reqd ~status:`Bad_request
      (HTML.div [HTML.class_ "err"] [txt "name required"]);
    Lwt.return_unit
  end else
    let rebind =
      match form_field body "rebind_route" with
      | Some ("1" | "on" | "true") -> true
      | _ -> false
    in
    let body_src = stub_body name in
    let* res =
      Session.define_binding session ~name ~body_src
        ~author:(Session.username session)
    in
    match res with
    | Error e ->
        html_response reqd ~status:`Bad_request
          (HTML.div [HTML.id "editor-root"; HTML.class_ "err"]
             [txt "create failed: %s" e]);
        Lwt.return_unit
    | Ok _ ->
        let* () =
          if rebind then
            Session.set_route session ~route:("/" ^ name) ~handler:name
          else Lwt.return_unit
        in
        let msg =
          if rebind then
            Printf.sprintf "created %s and pointed /%s at it" name name
          else Printf.sprintf "created %s — edit and save" name
        in
        let node = editor_form_partial ~name ~body_src ~msg () in
        (* Partial only — htmx swaps into #editor-root *)
        let s = Pure_html.to_string node in
        let headers =
          Httpaf.Headers.of_list
            [ ("content-type", "text/html; charset=utf-8")
            ; ("content-length", string_of_int (String.length s)) ]
        in
        let resp = Httpaf.Response.create ~headers `OK in
        Httpaf.Reqd.respond_with_string reqd resp s;
        Lwt.return_unit

let page_inspector (session : Session.t) (path_seg : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let route =
    if path_seg = "" then "/"
    else if path_seg.[0] = '/' then path_seg
    else "/" ^ path_seg
  in
  match Session.lookup_route session route with
  | None ->
      html_response reqd ~status:`Not_found
        (chrome ~session ~title:("Page " ^ route) [
           HTML.p [HTML.class_ "err"] [txt "no page registered for %s" route];
           HTML.p [] [HTML.a [HTML.href "/pages"] [txt "all pages"]];
         ]);
      Lwt.return_unit
  | Some handler ->
      let* graph = page_function_graph session ~handler in
      let* found = Store.lookup_function (Session.store session) handler in
      let body_block =
        match found with
        | None ->
            HTML.p [HTML.class_ "err"] [txt "handler function missing in store"]
        | Some (expr, author) ->
            HTML.div [] [
              HTML.p [HTML.class_ "muted"] [txt "author: %s" author];
              HTML.pre [] [txt "%s" (Pricklypear_lisp.Reader.pp_expr expr)];
            ]
      in
      let rows =
        List.map
          (fun (n, role) ->
            HTML.tr [] [
              HTML.td [] [txt "%s" n];
              HTML.td [] [txt "%s" role];
              HTML.td [] [
                HTML.a [HTML.href "%s" (Packages.edit_href n)] [txt "edit"];
                txt " · ";
                HTML.a [HTML.href "%s" (Packages.inspect_href n)] [txt "inspect"];
              ];
            ])
          graph
      in
      let body =
        chrome ~session ~title:("Page " ^ route) [
          HTML.div [HTML.class_ "card"] [
            HTML.h2 [] [txt "Routing"];
            HTML.p [] [
              txt "Page URL: ";
              HTML.a [HTML.href "%s" route] [txt "%s" route];
            ];
            HTML.p [] [
              txt "Handler function: ";
              HTML.a [HTML.href "%s" (Packages.edit_href handler)] [txt "%s" handler];
            ];
            HTML.p [] [
              HTML.a [HTML.href "%s" route; HTML.class_ "btn"] [txt "Open page"];
              txt " ";
              HTML.a [HTML.href "%s" (Packages.edit_href handler); HTML.class_ "btn secondary"]
                [txt "Edit handler"];
            ];
          ];
          HTML.div [HTML.class_ "card"] [
            HTML.h2 [] [txt "Functions this page uses"];
            HTML.p [HTML.class_ "muted"] [
              txt "Static call graph from the handler body (user functions only).";
            ];
            HTML.table [] [
              HTML.thead [] [
                HTML.tr [] [
                  HTML.th [] [txt "function"];
                  HTML.th [] [txt "role"];
                  HTML.th [] [txt "actions"];
                ];
              ];
              HTML.tbody [] rows;
            ];
          ];
          HTML.div [HTML.class_ "card"] [
            HTML.h2 [] [txt "Entry body (handler)"];
            body_block;
          ];
        ]
      in
      html_response reqd body;
      Lwt.return_unit

let inspect_binding_page (session : Session.t) (name : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* found = Store.lookup_function (Session.store session) name in
  let body =
    match found with
    | None ->
        chrome ~session ~title:("Inspect " ^ name) [
          HTML.p [HTML.class_ "err"] [txt "not found"];
        ]
    | Some (expr, author) ->
        let pp = Pricklypear_lisp.Reader.pp_expr expr in
        let j =
          try Yojson.Safe.pretty_to_string
                (Pricklypear_lisp.Ast.expr_to_json expr)
          with _ -> "(json unavailable)"
        in
        chrome ~session ~title:("Inspect binding " ^ name) [
          HTML.div [HTML.class_ "card"] [
            HTML.p [] [txt "author: %s" author];
            HTML.p [] [txt "branch: %s" (Session.branch_name session)];
            HTML.h2 [] [txt "pretty"];
            HTML.pre [] [txt "%s" pp];
            HTML.h2 [] [txt "AST JSON"];
            HTML.pre [] [txt "%s" j];
            HTML.p [] [
              HTML.a [HTML.href "%s" (Packages.edit_href name)] [txt "edit"];
            ];
          ];
        ]
  in
  html_response reqd body;
  Lwt.return_unit

let inspect_row_page (session : Session.t) (type_name : string) (id : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* v = Crud.get (Session.store session) ~type_name ~id in
  let body =
    match v with
    | Pricklypear_lisp.Ast.VNil ->
        chrome ~session ~title:("Inspect row " ^ type_name ^ "/" ^ id) [
          HTML.p [HTML.class_ "err"] [txt "row not found"];
        ]
    | Pricklypear_lisp.Ast.VStr fields ->
        let pretty =
          try Yojson.Safe.pretty_to_string (Yojson.Safe.from_string fields)
          with _ -> fields
        in
        chrome ~session ~title:("Inspect row " ^ type_name ^ "/" ^ id) [
          HTML.div [HTML.class_ "card"] [
            HTML.pre [] [txt "%s" pretty];
          ];
        ]
    | other ->
        chrome ~session ~title:("Inspect row " ^ type_name ^ "/" ^ id) [
          HTML.pre [] [txt "%s" (Pricklypear_lisp.Reader.pp_value other)];
        ]
  in
  html_response reqd body;
  Lwt.return_unit

let api_eval (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let expr_opt =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          (match List.assoc_opt "expr" items with
           | Some (`String s) -> Some s
           | _ -> None)
      | `String s -> Some s
      | _ -> None
    with _ ->
      (* also accept raw text body *)
      if String.trim body = "" then None else Some body
  in
  match expr_opt with
  | None ->
      json_response reqd ~status:`Bad_request
        (`Assoc [("ok", `Bool false); ("error", `String "expr required")]);
      Lwt.return_unit
  | Some expr ->
      Lwt.catch
        (fun () ->
          let* v = Session.eval_one session expr in
          let rendered = Pricklypear_lisp.Reader.pp_value v in
          json_response reqd
            (`Assoc [("ok", `Bool true); ("value", `String rendered)]);
          Lwt.return_unit)
        (fun exn ->
          let msg =
            match exn with
            | Pricklypear_lisp.Ast.Interp_error e ->
                (match e with
                 | Pricklypear_lisp.Ast.Parse_error (m, l, c) ->
                     Printf.sprintf "parse error: %s (%d:%d)" m l c
                 | Pricklypear_lisp.Ast.User_error m -> m
                 | Pricklypear_lisp.Ast.Unbound_var s -> "unbound: " ^ s
                 | Pricklypear_lisp.Ast.Budget_exceeded -> "budget exceeded"
                 | _ -> Printexc.to_string exn)
            | _ -> Printexc.to_string exn
          in
          json_response reqd
            (`Assoc [("ok", `Bool false); ("error", `String msg)]);
          Lwt.return_unit)

let save_edit (session : Session.t) (path_name : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let name =
    match form_field body "name" with
    | Some n when String.trim n <> "" -> String.trim n
    | _ -> path_name
  in
  let body_src = Option.value (form_field body "body") ~default:"" in
  let* res =
    Session.define_binding session ~name ~body_src
      ~author:(Session.username session)
  in
  match res with
  | Ok v ->
      let msg =
        Printf.sprintf "saved %s => %s" name
          (Pricklypear_lisp.Reader.pp_value v)
      in
      editor_page session name ~msg ~body_src reqd
  | Error e ->
      editor_page session name ~msg:("error: " ^ e) ~body_src reqd

let sse_write (body : [ `write ] Httpaf.Body.t) (event_json : string) : unit Lwt.t =
  let chunk = "data: " ^ event_json ^ "\n\n" in
  Httpaf.Body.write_string body chunk;
  let promise, wakeup = Lwt.task () in
  Httpaf.Body.flush body (fun () ->
    if Lwt.state promise = Lwt.Sleep then Lwt.wakeup_later wakeup ());
  (* Don't hang forever if the peer never acks the flush. *)
  Lwt.pick
    [ promise
    ; (let* () = Lwt_unix.sleep 2.0 in Lwt.return_unit)
    ]

let api_agent (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let seed, prompt, want_stream =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          let seed =
            match List.assoc_opt "seed" items with
            | Some (`String s) -> s
            | _ -> "You are the co-pilot for a Pricklypear multi-user Lisp OS image (software, not a cactus). Prefer CONTEXT; if unknown say so."
          in
          let prompt =
            match List.assoc_opt "prompt" items with
            | Some (`String s) -> s
            | _ ->
                (match List.assoc_opt "q" items with
                 | Some (`String s) -> s
                 | _ -> "")
          in
          let stream =
            match List.assoc_opt "stream" items with
            | Some (`Bool b) -> b
            | Some (`String "1") | Some (`String "true") -> true
            | Some (`String "0") | Some (`String "false") -> false
            | _ -> true
          in
          (seed, prompt, stream)
      | _ -> ("", "", true)
    with _ ->
      let seed =
        Option.value (form_field body "seed")
          ~default:"You are the co-pilot for a Pricklypear multi-user Lisp OS image (software, not a cactus). Prefer CONTEXT; if unknown say so."
      in
      let prompt =
        match form_field body "prompt" with
        | Some p -> p
        | None -> Option.value (form_field body "q") ~default:""
      in
      (seed, prompt, true)
  in
  if String.trim prompt = "" then begin
    json_response reqd
      (`Assoc [ ("ok", `Bool false); ("error", `String "need prompt") ]);
    Lwt.return_unit
  end else if not want_stream then begin
    let* v = Session.agent session ~seed ~prompt () in
    let text =
      match v with
      | Pricklypear_lisp.Ast.VStr s -> s
      | _ -> Pricklypear_lisp.Reader.pp_value v
    in
    json_response reqd
      (`Assoc [ ("ok", `Bool true); ("text", `String text) ]);
    Lwt.return_unit
  end else begin
    (* Live SSE: stream tokens as they arrive. Fast-fail (no API key)
       still works because on_event fires before any network wait. *)
    let headers =
      Httpaf.Headers.of_list
        [ ("content-type", "text/event-stream; charset=utf-8")
        ; ("cache-control", "no-cache")
        ; ("connection", "keep-alive")
        ; ("x-accel-buffering", "no")  (* disable nginx response buffering *)
        ]
    in
    let resp = Httpaf.Response.create ~headers `OK in
    let body_w =
      Httpaf.Reqd.respond_with_streaming ~flush_headers_immediately:true
        reqd resp
    in
    let queue : string Queue.t = Queue.create () in
    let cond = Lwt_condition.create () in
    let finished = ref false in
    let push_json (j : string) : unit =
      Queue.add j queue;
      Lwt_condition.signal cond ()
    in
    let on_event = function
      | Llm_client.Token tok ->
          push_json
            (Yojson.Safe.to_string
               (`Assoc [ ("t", `String "token"); ("s", `String tok) ]))
      | Llm_client.Done ->
          push_json (Yojson.Safe.to_string (`Assoc [ ("t", `String "done") ]));
          finished := true;
          Lwt_condition.signal cond ()
      | Llm_client.Fail e ->
          push_json
            (Yojson.Safe.to_string
               (`Assoc [ ("t", `String "error"); ("s", `String e) ]));
          finished := true;
          Lwt_condition.signal cond ()
    in
    (* Producer: LLM stream (or immediate Fail). *)
    Lwt.async (fun () ->
      let* v =
        Session.agent session ~stream:true ~on_event ~seed ~prompt ()
      in
      if not !finished then begin
        (match v with
         | Pricklypear_lisp.Ast.VStr s ->
             push_json
               (Yojson.Safe.to_string
                  (`Assoc [ ("t", `String "done"); ("text", `String s) ]))
         | _ ->
             push_json
               (Yojson.Safe.to_string (`Assoc [ ("t", `String "done") ])));
        finished := true;
        Lwt_condition.signal cond ()
      end;
      Lwt.return_unit);
    (* Consumer: write SSE frames as they enqueue. *)
    let rec drain () =
      if not (Queue.is_empty queue) then
        let j = Queue.take queue in
        let* () = sse_write body_w j in
        drain ()
      else if !finished then begin
        Httpaf.Body.close_writer body_w;
        Lwt.return_unit
      end else
        let* () = Lwt_condition.wait cond in
        drain ()
    in
    drain ()
  end

let api_agent_task (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let prompt =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          (match List.assoc_opt "prompt" items with
           | Some (`String s) -> s
           | _ ->
               (match List.assoc_opt "task" items with
                | Some (`String s) -> s
                | _ -> ""))
      | _ -> ""
    with _ ->
      Option.value (form_field body "prompt")
        ~default:(Option.value (form_field body "task") ~default:"")
  in
  if String.trim prompt = "" then begin
    json_response reqd
      (`Assoc [ ("ok", `Bool false); ("error", `String "need prompt/task") ]);
    Lwt.return_unit
  end else
    let* v = Session.agent_task session ~prompt () in
    let text =
      match v with
      | Pricklypear_lisp.Ast.VStr s -> s
      | _ -> Pricklypear_lisp.Reader.pp_value v
    in
    json_response reqd
      (`Assoc [ ("ok", `Bool true); ("text", `String text) ]);
    Lwt.return_unit

let agent_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  (* Live context so the model is grounded in THIS image, not botany. *)
  let* ctx_v =
    Lwt.catch
      (fun () ->
         Session.eval_one session
           "(string-append \"user=\" (whoami) \" branch=\" (current-branch) \" bindings=\" (str (list-bindings)))")
      (fun _ ->
         Lwt.return
           (Pricklypear_lisp.Ast.VStr
              (Printf.sprintf "user=%s branch=%s"
                 (Session.username session) (Session.branch_name session))))
  in
  let ctx =
    match ctx_v with
    | Pricklypear_lisp.Ast.VStr s -> s
    | _ -> Pricklypear_lisp.Reader.pp_value ctx_v
  in
  let default_seed =
    "You are the co-pilot for a running Pricklypear IMAGE — a multi-user Lisp " ^
    "operating system (software), NOT the cactus plant.\n\n" ^
    "Pricklypear: live per-user images; Lisp functions stored in Postgres; " ^
    "branches version code; routes bind functions to HTTP; UI is htmx. " ^
    "Prims include list-bindings, get, rows, define, route, render-html, agent.\n\n" ^
    "Rules: Prefer facts from the CONTEXT block in the user prompt. " ^
    "If unknown from context, say so. Do not invent bindings or data. " ^
    "Never answer as if Pricklypear is a cactus unless the user clearly asks about the plant."
  in
  let js =
    "const out=document.getElementById('out');" ^
    "const run=document.getElementById('run');" ^
    "const baseCtx=" ^ Yojson.Safe.to_string (`String ctx) ^ ";" ^
    "run.onclick=async()=>{" ^
    "const seed=document.getElementById('seed').value;" ^
    "const q=document.getElementById('prompt').value;" ^
    "const prompt='=== IMAGE CONTEXT ===\\n'+" ^
    "'system: Pricklypear multi-user Lisp OS image (software, not a cactus)\\n'+" ^
    "baseCtx+'\\n=== END CONTEXT ===\\n\\nUser question:\\n'+q;" ^
    "out.textContent='…'; run.disabled=true;" ^
    "try{" ^
    "const r=await fetch('/api/agent',{method:'POST'," ^
    "headers:{'content-type':'application/json'}," ^
    "body:JSON.stringify({seed,prompt,stream:true})});" ^
    "if(!r.ok){out.textContent='HTTP '+r.status;return;}" ^
    "const ct=r.headers.get('content-type')||'';" ^
    "if(ct.indexOf('application/json')>=0){" ^
    "const j=await r.json();" ^
    "out.textContent=(j.text||j.error||JSON.stringify(j));" ^
    "return;}" ^
    "out.textContent='';" ^
    "const reader=r.body.getReader();" ^
    "const dec=new TextDecoder();" ^
    "let buf='';" ^
    "while(true){" ^
    "const {value,done}=await reader.read();" ^
    "if(done)break;" ^
    "buf+=dec.decode(value,{stream:true});" ^
    "const parts=buf.split('\\n\\n');" ^
    "buf=parts.pop();" ^
    "for(const p of parts){" ^
    "const line=p.trim();" ^
    "if(!line.startsWith('data:'))continue;" ^
    "try{" ^
    "const j=JSON.parse(line.slice(5).trim());" ^
    "if(j.t==='token')out.textContent+=j.s;" ^
    "else if(j.t==='error')out.textContent+=(out.textContent?'\\n':'')+'[error] '+j.s;" ^
    "else if(j.t==='done'&&j.text&&!out.textContent)out.textContent=j.text;" ^
    "}catch(_e){}}}" ^
    "if(!out.textContent)out.textContent='(empty response)';" ^
    "}catch(e){out.textContent=String(e);}" ^
    "finally{run.disabled=false;}" ^
    "};"
  in
  let body =
    chrome ~session ~title:"Agent" [
      HTML.div [HTML.class_ "card"] [
        HTML.p [] [txt
          "Co-pilot for this image. Seed = persona; prompt = your question."];
        HTML.p [HTML.class_ "muted"] [txt
          "Each run injects live context (user/branch/bindings). Lisp: (ai/ask \"…\")."];
        HTML.p [] [
          HTML.label [HTML.for_ "seed"] [txt "seed (system)"];
        ];
        HTML.textarea [HTML.name "seed"; HTML.id "seed"]
          "%s" default_seed;
        HTML.p [] [
          HTML.label [HTML.for_ "prompt"] [txt "prompt (your question)"];
        ];
        HTML.textarea [HTML.name "prompt"; HTML.id "prompt"]
          "%s" "What is this image, and what branch am I on?";
        HTML.p [] [
          HTML.button [HTML.type_ "button"; HTML.id "run"] [txt "Run agent"];
        ];
        HTML.pre [HTML.id "out"; HTML.class_ "card"] [txt ""];
      ];
      HTML.script [] "%s" js;
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Write a function"];
        HTML.p [HTML.class_ "muted"] [txt
          "Generates a Lisp body and installs it on this branch as agent:<model>. Same as (ai/write name spec)."];
        HTML.p [] [
          HTML.label [HTML.for_ "fn_name"] [txt "name "];
          HTML.input [HTML.type_ "text"; HTML.name "fn_name"; HTML.id "fn_name";
                      HTML.value "demo/double"; HTML.placeholder "pkg/name"];
        ];
        HTML.p [] [
          HTML.label [HTML.for_ "fn_spec"] [txt "spec"];
        ];
        HTML.textarea [HTML.name "fn_spec"; HTML.id "fn_spec"]
          "%s" "A pure lambda of one int x that returns (* x 2).";
        HTML.p [] [
          HTML.button [HTML.type_ "button"; HTML.id "write_fn"] [txt "Generate & install"];
        ];
        HTML.pre [HTML.id "write_out"; HTML.class_ "card"] [txt ""];
      ];
      HTML.script [] "%s" (
        "const wout=document.getElementById('write_out');" ^
        "const wbtn=document.getElementById('write_fn');" ^
        "wbtn.onclick=async()=>{" ^
        "const name=document.getElementById('fn_name').value.trim();" ^
        "const spec=document.getElementById('fn_spec').value;" ^
        "wout.textContent='…'; wbtn.disabled=true;" ^
        "try{" ^
        "const r=await fetch('/api/ai/propose',{method:'POST'," ^
        "headers:{'content-type':'application/json'}," ^
        "body:JSON.stringify({name,spec})});" ^
        "const j=await r.json();" ^
        "wout.textContent=j.result||j.error||JSON.stringify(j);" ^
        "}catch(e){wout.textContent=String(e);}" ^
        "finally{wbtn.disabled=false;}" ^
        "};"
      );

      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Task (tool agent)"];
        HTML.p [HTML.class_ "muted"] [txt
          "Multi-turn: can list bindings, inspect functions, and install several functions. Lisp: (ai/do \"…\")."];
        HTML.textarea [HTML.name "task"; HTML.id "task"]
          "%s" "Create pure functions demo/inc and demo/dec (int -> int), install both, then say how to call them.";
        HTML.p [] [
          HTML.button [HTML.type_ "button"; HTML.id "task_run"] [txt "Run task"];
        ];
        HTML.pre [HTML.id "task_out"; HTML.class_ "card"] [txt ""];
      ];
      HTML.script [] "%s" (
        "const tout=document.getElementById('task_out');" ^
        "const tbtn=document.getElementById('task_run');" ^
        "tbtn.onclick=async()=>{" ^
        "const prompt=document.getElementById('task').value;" ^
        "tout.textContent='… (may take a while)'; tbtn.disabled=true;" ^
        "try{" ^
        "const r=await fetch('/api/agent-task',{method:'POST'," ^
        "headers:{'content-type':'application/json'}," ^
        "body:JSON.stringify({prompt})});" ^
        "const j=await r.json();" ^
        "tout.textContent=j.text||j.error||JSON.stringify(j);" ^
        "}catch(e){tout.textContent=String(e);}" ^
        "finally{tbtn.disabled=false;}" ^
        "};"
      );

    ]
  in
  html_response reqd body;
  Lwt.return_unit

let ai_propose (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let name_opt, spec_opt =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          let name =
            match List.assoc_opt "name" items with
            | Some (`String s) -> Some s
            | _ -> None
          in
          let spec =
            match List.assoc_opt "spec" items with
            | Some (`String s) -> Some s
            | _ -> None
          in
          (name, spec)
      | _ -> (None, None)
    with _ ->
      let pkg = Option.value (form_field body "package") ~default:"" in
      let local = Option.value (form_field body "local") ~default:"" in
      let name =
        match form_field body "name" with
        | Some n when String.trim n <> "" -> Some (String.trim n)
        | _ ->
            let j = Packages.join_name ~package:pkg ~local in
            if j = "" then None else Some j
      in
      (name, form_field body "spec")
  in
  match name_opt, spec_opt with
  | Some name, Some spec when name <> "" && spec <> "" ->
      let* v = Session.propose_function session ~name ~spec in
      let rendered = Pricklypear_lisp.Reader.pp_value v in
      let wants_html =
        match Httpaf.Headers.get (Httpaf.Reqd.request reqd).headers "content-type" with
        | Some ct when String.length ct >= 33
            && String.sub ct 0 33 = "application/x-www-form-urlencoded" -> true
        | _ ->
            (match form_field body "name" with Some _ -> true | None -> false)
      in
      if wants_html then
        let body_node =
          chrome ~session ~title:("AI propose " ^ name) [
            HTML.div [HTML.class_ "card"] [
              HTML.pre [] [txt "%s" rendered];
              HTML.p [] [
                HTML.a [HTML.href "%s" ("/edit/" ^ name)] [txt "open editor"];
                txt " · ";
                HTML.a [HTML.href "%s" (Packages.inspect_href name)] [txt "inspect"];
                txt " · ";
                HTML.a [HTML.href "/bindings"] [txt "functions"];
              ];
            ];
          ]
        in
        html_response reqd body_node;
        Lwt.return_unit
      else begin
        json_response reqd
          (`Assoc [ ("ok", `Bool true)
                  ; ("result", `String rendered)
                  ; ("name", `String name) ]);
        Lwt.return_unit
      end
  | _ ->
      json_response reqd ~status:`Bad_request
        (`Assoc [("ok", `Bool false); ("error", `String "name and spec required")]);
      Lwt.return_unit

let request_handler (session : Session.t) (_sockaddr : Unix.sockaddr)
    (reqd : Httpaf.Reqd.t) : unit =
  let req = Httpaf.Reqd.request reqd in
  let target = req.Httpaf.Request.target in
  let path =
    match String.index_opt target '?' with
    | Some i -> String.sub target 0 i
    | None -> target
  in
  let meth = req.Httpaf.Request.meth in
  let headers = header_list req in
  (* Thunk page: Lwt promises are eager — must not start page work
     before auth middleware decides proceed. *)
  let launch (page_fn : unit -> unit Lwt.t) : unit =
    Lwt.async (fun () ->
      Lwt_mutex.with_lock request_mutex (fun () ->
        Lwt.catch
          (fun () ->
            let* proceed =
              if auth_disabled () || is_public_path path then
                Lwt.return true
              else
                let* user_opt = resolve_identity session ~headers in
                match user_opt with
                | None ->
                    require_login_redirect reqd ~path;
                    Lwt.return false
                | Some user ->
                    let tok = cookie_value headers cookie_name in
                    let* () =
                      attach_from_user session user ~auth_token:tok
                        ~checkout_personal_now:false ()
                    in
                    Lwt.return true
            in
            if not proceed then Lwt.return_unit
            else
              (* Refresh ui/nav cache every request so Lisp chrome is shared
                 by home, system pages, and user routes — not only /. *)
              let* () = Session.refresh_ui_shell session in
              page_fn ())
          (fun exn ->
            let body =
              chrome ~session ~title:"Error" [
                HTML.p [HTML.class_ "err"] [txt "%s" (Printexc.to_string exn)];
              ]
            in
            html_response reqd ~status:`Internal_server_error body;
            Lwt.return_unit)))
  in
  match meth, path with
  | `GET, "/login" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         login_form_page reqd;
         Lwt.return_unit)
  | `POST, "/login" -> launch (fun () -> post_login session reqd)
  | `POST, "/logout" -> launch (fun () -> post_logout session reqd)
  | `GET, "/admin/users" -> launch (fun () -> admin_users_page session reqd)
  | `POST, "/admin/users" -> launch (fun () -> post_admin_users session reqd)
  | `GET, "/" -> launch (fun () -> home_page session reqd)
  | `GET, "/about" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         about_page session reqd;
         Lwt.return_unit)
  | `GET, "/pages" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         pages_list_page session reqd;
         Lwt.return_unit)
  | `POST, "/api/recipe/hello" ->
      launch (fun () ->
        let* res = Session.recipe_hello session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Hello recipe" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/hello"] [txt "Open /hello"];
                     txt " · ";
                     HTML.a [HTML.href "/edit/hi-page"] [txt "Edit hi-page"];
                     txt " · ";
                     HTML.a [HTML.href "/pages"] [txt "Pages"];
                     txt " · ";
                     HTML.a [HTML.href "/"] [txt "Home"];
                   ];
                 ];
               ]
             in
             html_response reqd body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Hello recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `POST, "/api/recipe/todo" ->
      launch (fun () ->
        let* res = Session.recipe_todo session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Todo library" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/todos"] [txt "Open /todos"];
                     txt " · ";
                     HTML.a [HTML.href "/bindings?pkg=todo"] [txt "Package todo"];
                     txt " · ";
                     HTML.a [HTML.href "/pages"] [txt "Pages"];
                   ];
                 ];
               ]
             in
             html_response reqd body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Todo recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `POST, "/api/recipe/notes" ->
      launch (fun () ->
        let* res = Session.recipe_notes session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Notes library" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/notes"] [txt "Open /notes"];
                     txt " · ";
                     HTML.a [HTML.href "/bindings?pkg=notes"] [txt "Package notes"];
                     txt " · ";
                     HTML.a [HTML.href "/bindings?pkg=outline"] [txt "outline"];
                   ];
                   HTML.p [HTML.class_ "muted"] [
                     txt "Outline-first notes (org-shaped). Content lives in data_rows.";
                   ];
                 ];
               ]
             in
             html_response reqd body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Notes recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `POST, "/api/recipe/dabar" ->
      launch (fun () ->
        let* res = Session.recipe_dabar session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Dabar library" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/dabar?book=John&chapter=3"]
                       [txt "Open /dabar"];
                     txt " · ";
                     HTML.a [HTML.href "/dabar?book=John&chapter=3&verse=16"]
                       [txt "Study John 3:16"];
                     txt " · ";
                     HTML.a [HTML.href "/strongs?n=G2316"] [txt "Strong's G2316"];
                   ];
                   HTML.p [HTML.class_ "muted"] [
                     txt "Seeds: scripts/seed_bible_bsb.py · seed_strongs.py · seed_interlinear_dogfood.py";
                   ];
                 ];
               ]
             in
             html_response reqd body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Dabar recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `GET, "/health" ->
      let h =
        { Pricklypear_common.Protocol.ok = true;
          branch = Session.branch_name session;
          uptime_seconds = Session.uptime_seconds session;
          error = None }
      in
      let j = Pricklypear_common.Protocol.health_response_to_json h in
      text_response reqd ~content_type:"application/json"
        (Yojson.Safe.pretty_to_string j)
  | `GET, "/branches" -> launch (fun () -> branches_page session reqd)
  | `POST, "/branches/checkout" -> launch (fun () -> branches_checkout session reqd)
  | `POST, "/branches/fork" -> launch (fun () -> branches_fork session reqd)
  | `POST, "/branches/unoverride" -> launch (fun () -> branches_unoverride session reqd)
  | `POST, "/branches/promote" -> launch (fun () -> branches_promote session reqd)
  | `GET, "/bindings" -> launch (fun () -> list_bindings_page session reqd)
  | `GET, "/types" -> launch (fun () -> list_types_page session reqd)
  | `GET, "/repl" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         repl_page session reqd;
         Lwt.return_unit)
  | `POST, "/api/eval" -> launch (fun () -> api_eval session reqd)
  | `POST, "/api/agent" -> launch (fun () -> api_agent session reqd)
  | `POST, "/api/agent-task" -> launch (fun () -> api_agent_task session reqd)
  | `POST, "/api/ai/propose" -> launch (fun () -> ai_propose session reqd)
  | `GET, "/agent" -> launch (fun () -> agent_page session reqd)
  | `POST, "/api/edit/create" -> launch (fun () -> api_edit_create session reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/page/" ->
      let seg = String.sub path 6 (String.length path - 6) in
      launch (fun () -> page_inspector session seg reqd)
  | `GET, "/page" | `GET, "/page/" ->
      launch (fun () -> page_inspector session "" reqd)
  | `GET, path when String.length path >= 17
                 && String.sub path 0 17 = "/inspect/binding/" ->
      let name =
        Packages.name_decode (String.sub path 17 (String.length path - 17))
      in
      launch (fun () -> inspect_binding_page session name reqd)
  | `GET, path when String.length path >= 13
                 && String.sub path 0 13 = "/inspect/row/" ->
      let rest = String.sub path 13 (String.length path - 13) in
      (match String.split_on_char '/' rest with
       | [ty; id] -> launch (fun () -> inspect_row_page session ty id reqd)
       | _ ->
           html_response reqd ~status:`Bad_request
             (chrome ~session ~title:"Bad inspect path" [
                HTML.p [] [txt "use /inspect/row/<type>/<id>"];
              ]))
  | `GET, path when String.length path >= 9
                 && String.sub path 0 9 = "/binding/" ->
      let name =
        Packages.name_decode (String.sub path 9 (String.length path - 9))
      in
      launch (fun () -> show_binding_page session name reqd)
  | `GET, "/edit" | `GET, "/edit/" ->
      (* package + local query form from Functions browser *)
      let pkg = Option.value (query_param target "package") ~default:"" in
      let local = Option.value (query_param target "local") ~default:"" in
      let name = Packages.join_name ~package:pkg ~local in
      if name = "" then
        html_response reqd
          (chrome ~session ~title:"New function" [
             HTML.p [] [txt "Provide package and/or name (see Functions)."];
             HTML.p [] [HTML.a [HTML.href "/bindings"] [txt "Functions"]];
           ])
      else launch (fun () -> editor_page session name reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (fun () -> editor_page session name reqd)
  | `POST, path when String.length path >= 6
                  && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (fun () -> save_edit session name reqd)
  | ((`GET | `POST) as umeth), _ ->
      (* User routes: GET + POST (it33). Same path → same handler;
         request-* prims expose method/params for branching. *)
      (match Session.lookup_route session path with
       | Some handler_name ->
           let page () : unit Lwt.t =
             let* body_str =
               match umeth with
               | `POST -> read_body reqd
               | `GET -> Lwt.return ""
             in
             if String.length body_str > max_user_body_bytes then begin
               text_response reqd ~status:`Payload_too_large
                 ~content_type:"text/plain"
                 "request body too large (max 64KiB)";
               Lwt.return_unit
             end else begin
               let query = parse_query target in
               let form =
                 match umeth with
                 | `GET -> []
                 | `POST ->
                     let ct =
                       match
                         Httpaf.Headers.get req.headers "content-type"
                       with
                       | Some s -> s
                       | None -> ""
                     in
                     if String.length ct >= 33
                        && String.sub ct 0 33
                           = "application/x-www-form-urlencoded"
                        || ct = ""
                           && String.contains body_str '='
                     then parse_form_body body_str
                     else []
               in
               let params = merge_params ~query ~form in
               let headers = header_list req in
               let fragment = wants_fragment ~path ~headers in
               let method_ : Session.http_method =
                 match umeth with `GET -> `GET | `POST -> `POST
               in
               let request : Session.request =
                 { method_; path; params; body = body_str; headers }
               in
               let* result =
                 Session.call_handler session ~request handler_name
               in
               (match result with
                | Some value ->
                    respond_handler_value ~session ~reqd ~path ~fragment value;
                    Lwt.return_unit
                | None ->
                    if fragment then
                      fragment_response reqd ~status:`Internal_server_error
                        ("handler " ^ handler_name ^ " returned no value")
                    else
                      html_response reqd ~status:`Internal_server_error
                        (chrome ~session ~title:"Route handler error" [
                           HTML.p []
                             [txt "handler %s returned no value" handler_name];
                         ]);
                    Lwt.return_unit)
             end
           in
           launch (fun () -> page ())
       | None ->
           let headers = header_list req in
           if wants_fragment ~path ~headers then
             fragment_response reqd ~status:`Not_found
               ("no partial for " ^ path)
           else
             html_response reqd ~status:`Not_found
               (chrome ~session ~title:"Not found" [
                  HTML.p [] [txt "no route for %s" path];
                ]))
  | _, _ ->
      let headers = header_list req in
      if wants_fragment ~path ~headers then
        fragment_response reqd ~status:`Not_found ("no partial for " ^ path)
      else
        html_response reqd ~status:`Not_found
          (chrome ~session ~title:"Not found" [
             HTML.p [] [txt "no route for %s" path];
           ])

let error_handler (_sockaddr : Unix.sockaddr)
    ?(request : Httpaf.Request.t option)
    (err : Httpaf.Server_connection.error)
    (respond : Httpaf.Headers.t -> [> `write ] Httpaf.Body.t) : unit =
  let status, msg =
    match err with
    | `Bad_request -> `Bad_request, "bad request"
    | `Bad_gateway -> `Bad_gateway, "bad gateway"
    | `Internal_server_error -> `Internal_server_error, "internal server error"
    | `Exn exn -> `Internal_server_error, Printexc.to_string exn
  in
  let _ = request in
  let body_str =
    Pure_html.to_string
      (HTML.html [HTML.lang "en"] [
         HTML.head [] [HTML.title [] "Pricklypear Error"];
         HTML.body [] [
           HTML.h1 [] [txt "Error"];
           HTML.p [] [txt "%s" msg];
         ];
       ])
  in
  let headers =
    Httpaf.Headers.of_list
      [("content-type", "text/html");
       ("content-length", string_of_int (String.length body_str))]
  in
  let body_w = respond headers in
  Httpaf.Body.write_string body_w body_str;
  Httpaf.Body.close_writer body_w

let serve ~(session : Session.t) ?(port = default_port) () : unit Lwt.t =
  let handler = request_handler session in
  let establisher =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:handler
      ~error_handler
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  Printf.eprintf "[http] serving htmx on 0.0.0.0:%d\n" port;
  let* _server = Lwt_io.establish_server_with_client_socket addr establisher in
  let waiter, _waker = Lwt.wait () in
  waiter
