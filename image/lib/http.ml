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
         nav a{display:inline-block;padding:.45rem .55rem;min-height:2.5rem;
               line-height:1.5;border:1px solid #000;background:#c0c0c0;
               color:#000;text-decoration:none;font-size:.9rem}
         nav a:visited{color:#000}
         nav a:hover,nav a:focus{background:#fff}
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
         textarea{min-height:12rem;line-height:1.35}
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
         /* Mobile: stack denser chrome, full-width controls */
         @media (max-width:40rem){
           main{padding:.5rem}
           header{padding:.5rem}
           nav{gap:.3rem}
           nav a{flex:1 1 auto;text-align:center;min-width:4.5rem}
           table{display:block;overflow-x:auto;-webkit-overflow-scrolling:touch}
           h1{font-size:1.15rem}
           button,.btn{width:100%;margin:.2rem 0}
           .card form button,.card form .btn{width:auto;min-width:44%}
         }";
      (* htmx for create-in-place editor empty state *)
      HTML.script [HTML.src "https://unpkg.com/htmx.org@1.9.12"] "";
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

let chrome ~(session : Session.t) ~(title : string) (content : Pure_html.node list)
    : Pure_html.node =
  HTML.div [] [
    HTML.header [] [
      HTML.span [HTML.class_ "brand"] [txt "Pricklypear"];
      HTML.span [HTML.class_ "meta"] [
        txt "user=%s  branch=%s  up=%ds"
          (Session.username session)
          (Session.branch_name session)
          (Session.uptime_seconds session);
      ];
      HTML.nav [] [
        HTML.a [HTML.href "/"] [txt "home"];
        HTML.a [HTML.href "/about"] [txt "about"];
        HTML.a [HTML.href "/branches"] [txt "branches"];
        HTML.a [HTML.href "/bindings"] [txt "functions"];
        HTML.a [HTML.href "/types"] [txt "data"];
        HTML.a [HTML.href "/pages"] [txt "pages"];
        HTML.a [HTML.href "/repl"] [txt "repl"];
      ];
    ];
    HTML.main [] (
      HTML.h1 [] [txt "%s" title] :: content
    );
  ]

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

let list_branches_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* branches = Store.list_branches (Session.store session) in
  let rows =
    List.map
      (fun (b : Pricklypear_common.Branch.t) ->
        let owner = b.Pricklypear_common.Branch.owner in
        let name = b.Pricklypear_common.Branch.name in
        let parent =
          match b.Pricklypear_common.Branch.parent_id with
          | None -> "-"
          | Some pid -> String.sub pid 0 (min 8 (String.length pid))
        in
        let created = b.Pricklypear_common.Branch.created_at in
        HTML.tr [] [
          HTML.td [] [txt "%s/%s" owner name];
          HTML.td [] [txt "%s" parent];
          HTML.td [] [txt "%s" created];
        ])
      branches
  in
  let body =
    chrome ~session ~title:"Branches" [
      (match branches with
       | [] -> HTML.p [] [txt "(no branches in store)"]
       | _ ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "branch"];
                 HTML.th [] [txt "parent"];
                 HTML.th [] [txt "created"];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd body;
  Lwt.return_unit

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

let home_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit =
  let body =
    chrome ~session ~title:"Workspace" [
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Workspace"];
        HTML.p [] [
          txt "You are %s on branch %s. This is your live image — functions are durable data, pages are functions bound to URLs."
            (Session.username session) (Session.branch_name session);
        ];
        HTML.p [] [
          HTML.a [HTML.href "/about"] [txt "New here? Read About — concepts, access, and long-term possibilities"];
        ];
        HTML.ul [] [
          HTML.li [] [HTML.a [HTML.href "/about"] [txt "About / concepts"]];
          HTML.li [] [HTML.a [HTML.href "/bindings"] [txt "Functions"]];
          HTML.li [] [HTML.a [HTML.href "/pages"] [txt "Pages"]];
          HTML.li [] [HTML.a [HTML.href "/types"] [txt "Data types"]];
          HTML.li [] [HTML.a [HTML.href "/repl"] [txt "REPL (do-it)"]];
          HTML.li [] [HTML.a [HTML.href "/bindings"] [txt "Functions"]];
          HTML.li [] [HTML.a [HTML.href "/branches"] [txt "Branches"]];
        ];
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "First-run: make a Hello page"];
        HTML.p [] [txt
          "One click creates a function (hi-page) and a page at /hello. No Lisp required — then edit it."];
        HTML.form [HTML.method_ `POST; HTML.action "/api/recipe/hello";
                   HTML.enctype `urlencoded] [
          HTML.button [HTML.type_ "submit"] [txt "Create Hello page"];
        ];
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "AI propose (current branch)"];
        HTML.form [HTML.method_ `POST; HTML.action "/api/ai/propose";
                   HTML.enctype `urlencoded] [
          HTML.p [] [
            HTML.label [HTML.for_ "package"] [txt "package "];
            HTML.input [HTML.type_ "text"; HTML.name "package"; HTML.id "package";
                        HTML.placeholder "todo (optional)"];
            txt " ";
            HTML.label [HTML.for_ "local"] [txt "name "];
            HTML.input [HTML.type_ "text"; HTML.name "local"; HTML.id "local";
                        HTML.value "greet"; HTML.placeholder "greet"];
          ];
          HTML.p [HTML.class_ "muted"] [
            txt "Full name = package/name (or just name if package empty). Prefer todo/list style.";
          ];
          HTML.p [] [
            HTML.label [HTML.for_ "spec"] [txt "spec"];
          ];
          HTML.textarea [HTML.name "spec"; HTML.id "spec"; HTML.required]
            "%s" "A lambda that takes one string name and returns a greeting string using string-append.";
          HTML.p [] [HTML.button [HTML.type_ "submit"] [txt "Propose with AI"]];
        ];
      ];
    ]
  in
  html_response reqd body

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

(** Build function-name -> route list using static page graphs. *)
let used_by_pages_map (session : Session.t)
    : (string, string list) Hashtbl.t Lwt.t =
  let routes = Session.list_pages session in
  let tbl : (string, string list) Hashtbl.t = Hashtbl.create 32 in
  let add fn route =
    let prev = try Hashtbl.find tbl fn with Not_found -> [] in
    if List.mem route prev then ()
    else Hashtbl.replace tbl fn (route :: prev)
  in
  let rec fold_routes rs =
    match rs with
    | [] -> Lwt.return tbl
    | (route, handler) :: rest ->
        let* graph = page_function_graph session ~handler in
        List.iter (fun (fn, _) -> add fn route) graph;
        fold_routes rest
  in
  fold_routes routes

let list_bindings_page (session : Session.t) (reqd : Httpaf.Reqd.t)
    : unit Lwt.t =
  let target = (Httpaf.Reqd.request reqd).Httpaf.Request.target in
  let q = Option.value (query_param target "q") ~default:"" |> String.trim in
  let author_f =
    Option.value (query_param target "author") ~default:"" |> String.trim
  in
  let* defs = Store.list_function_names (Session.store session) in
  let* used_by = used_by_pages_map session in
  let matches (name, _body, author) =
    let q_ok =
      q = ""
      || contains_ci name q
      || contains_ci (Packages.package_of_name name) q
      || contains_ci author q
    in
    let a_ok =
      match author_f with
      | "" | "all" -> true
      | "agent" ->
          String.length author >= 6 && String.sub author 0 6 = "agent:"
      | "human" ->
          not (String.length author >= 6 && String.sub author 0 6 = "agent:")
      | _ -> true
    in
    q_ok && a_ok
  in
  let defs = List.filter matches defs in
  let groups = Packages.group_by_package defs in
  let used_links name =
    let routes = try Hashtbl.find used_by name with Not_found -> [] in
    match List.rev routes with
    | [] -> HTML.span [HTML.class_ "muted"] [txt "—"]
    | rs ->
        let nodes =
          List.concat
            (List.mapi
               (fun i route ->
                 let link =
                   HTML.a
                     [HTML.href "%s" ("/page/" ^ page_inspect_path route)]
                     [txt "%s" route]
                 in
                 if i = 0 then [link] else [txt " "; link])
               rs)
        in
        HTML.span [] nodes
  in
  let group_blocks =
    List.map
      (fun (pkg, rows) ->
        let header =
          HTML.tr [] [
            HTML.td
              [HTML.colspan 5; HTML.class_ "muted"]
              [ txt "package: %s  (%d)"
                  (Packages.package_label pkg) (List.length rows) ];
          ]
        in
        let body_rows =
          List.map
            (fun (name, body, author) ->
              let badge =
                Pricklypear_lisp.Effect.EffSet.badge
                  (Pricklypear_lisp.Effect.infer body)
              in
              HTML.tr [] [
                HTML.td [] [
                  HTML.a [HTML.href "%s" (Packages.binding_href name)]
                    [txt "%s" name];
                ];
                HTML.td [] [txt "%s" badge];
                HTML.td [] [txt "%s" author];
                HTML.td [] [used_links name];
                HTML.td [] [
                  HTML.a [HTML.href "%s" (Packages.edit_href name)] [txt "edit"];
                  txt " · ";
                  HTML.a [HTML.href "%s" (Packages.inspect_href name)]
                    [txt "inspect"];
                ];
              ])
            rows
        in
        header :: body_rows)
      groups
    |> List.flatten
  in
  let body =
    chrome ~session ~title:("Functions (" ^ Session.branch_name session ^ ")") [
      HTML.p [HTML.class_ "meta"] [
        txt "Packages group names by prefix (todo/list → package todo). Branch is version; package is organization.";
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.form [HTML.method_ `GET; HTML.action "/bindings"] [
          HTML.p [] [
            HTML.label [HTML.for_ "q"] [txt "search "];
            HTML.input
              [ HTML.type_ "text"
              ; HTML.name "q"
              ; HTML.id "q"
              ; HTML.value "%s" q
              ; HTML.placeholder "name, package, author" ];
            txt " ";
            HTML.button [HTML.type_ "submit"] [txt "Filter"];
            txt " ";
            HTML.a [HTML.href "/bindings"] [txt "clear"];
            txt " · ";
            HTML.a [HTML.href "/bindings?author=human"] [txt "human"];
            txt " · ";
            HTML.a [HTML.href "/bindings?author=agent"] [txt "agent:*"];
          ];
        ];
      ];
      HTML.div [HTML.class_ "card"] [
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
          HTML.p [HTML.class_ "muted"] [
            txt "Joins to package/name (e.g. todo/list). Leave package empty for root names.";
          ];
        ];
      ];
      (match defs with
       | [] ->
           HTML.p [] [
             txt "(no functions match — try Hello recipe, clear filter, or create above)";
           ]
       | _ ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "function"];
                 HTML.th [] [txt "effects"];
                 HTML.th [] [txt "author"];
                 HTML.th [] [txt "used by pages"];
                 HTML.th [] [txt "actions"];
               ];
             ];
             HTML.tbody [] group_blocks;
           ]);
      HTML.p [] [
        HTML.a [HTML.href "/repl"] [txt "open REPL"];
        txt " · ";
        HTML.a [HTML.href "/pages"] [txt "pages"];
        txt " · ";
        HTML.a [HTML.href "/"] [txt "home"];
      ];
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
  let launch (page : unit Lwt.t) : unit =
    Lwt.async (fun () ->
      Lwt.catch (fun () -> page)
        (fun exn ->
          let body =
            chrome ~session ~title:"Error" [
              HTML.p [HTML.class_ "err"] [txt "%s" (Printexc.to_string exn)];
            ]
          in
          html_response reqd ~status:`Internal_server_error body;
          Lwt.return_unit))
  in
  match meth, path with
  | `GET, "/" -> home_page session reqd
  | `GET, "/about" -> about_page session reqd
  | `GET, "/pages" -> pages_list_page session reqd
  | `POST, "/api/recipe/hello" ->
      launch
        (let* res = Session.recipe_hello session in
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
  | `GET, "/branches" -> launch (list_branches_page session reqd)
  | `GET, "/bindings" -> launch (list_bindings_page session reqd)
  | `GET, "/types" -> launch (list_types_page session reqd)
  | `GET, "/repl" -> repl_page session reqd
  | `POST, "/api/eval" -> launch (api_eval session reqd)
  | `POST, "/api/ai/propose" -> launch (ai_propose session reqd)
  | `POST, "/api/edit/create" -> launch (api_edit_create session reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/page/" ->
      let seg = String.sub path 6 (String.length path - 6) in
      launch (page_inspector session seg reqd)
  | `GET, "/page" | `GET, "/page/" ->
      launch (page_inspector session "" reqd)
  | `GET, path when String.length path >= 18
                 && String.sub path 0 18 = "/inspect/binding/" ->
      let name =
        Packages.name_decode (String.sub path 18 (String.length path - 18))
      in
      launch (inspect_binding_page session name reqd)
  | `GET, path when String.length path >= 13
                 && String.sub path 0 13 = "/inspect/row/" ->
      let rest = String.sub path 13 (String.length path - 13) in
      (match String.split_on_char '/' rest with
       | [ty; id] -> launch (inspect_row_page session ty id reqd)
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
      launch (show_binding_page session name reqd)
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
      else launch (editor_page session name reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (editor_page session name reqd)
  | `POST, path when String.length path >= 6
                  && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (save_edit session name reqd)
  | _, _ ->
      (* User routes only on GET for now *)
      (match Session.lookup_route session path with
       | Some handler_name when meth = `GET ->
           let page () : unit Lwt.t =
             let* result = Session.call_handler session handler_name in
             (match result with
              | Some value ->
                  let body =
                    match Primitives.is_raw_html value with
                    | Some raw_html ->
                        chrome ~session ~title:path [
                          txt ~raw:true "%s" raw_html;
                        ]
                    | None ->
                        (match value with
                         | Pricklypear_lisp.Ast.VStr s ->
                             chrome ~session ~title:path [
                               HTML.p [] [txt "%s" s];
                             ]
                         | _ ->
                             let rendered =
                               Pricklypear_lisp.Reader.pp_value value
                             in
                             chrome ~session ~title:path [
                               HTML.pre [] [txt "%s" rendered];
                             ])
                  in
                  html_response reqd body;
                  Lwt.return_unit
              | None ->
                  html_response reqd ~status:`Internal_server_error
                    (chrome ~session ~title:"Route handler error" [
                       HTML.p [] [txt "handler %s returned no value" handler_name];
                     ]);
                  Lwt.return_unit)
           in
           launch (page ())
       | _ ->
           html_response reqd ~status:`Not_found
             (chrome ~session ~title:"Not found" [
                HTML.p [] [txt "no route for %s" path];
              ]))

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
