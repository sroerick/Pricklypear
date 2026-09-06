(** Pricklypear Image — HTTP system & product page handlers. *)
open Pure_html
open Http_core
open Http_data
open Http_branches
open Http_agent

let show_binding_page (session : Session.t) (name : string)
    (reqd : Httpun.Reqd.t) : unit =
  let found = Store.lookup_function (Session.store session) name in
  let body =
    match found with
    | None ->
        chrome ~session ~title:name [
          HTML.p [] [txt "(no such binding on this branch)"];
          HTML.p [] [HTML.a [HTML.href "%s" ("/edit/" ^ name)] [txt "create it"]];
        ]
    | Some (expr, author, _doc) ->
        let snap = Call_graph.snapshot_of_session session in
        let mutual = Call_graph.mutual_of snap name in
        let cx =
          Pricklypear_lisp.Complexity.badge
            (Pricklypear_lisp.Complexity.classify ~fn_name:name expr)
        in
        let db_prims = Db_cost.db_prims_of_expr expr in
        let db_nodes =
          if db_prims = [] then [ txt "" ]
          else
            List.concat_map
              (fun p ->
                [
                  HTML.span [ HTML.class_ "badge db-cost" ]
                    [ txt "%s: %s" p (Db_cost.badge (Db_cost.static_cost p)) ];
                  txt " ";
                ])
              db_prims
        in
        let stats_node =
          match
            Crud.find_row (Session.store session) ~type_name:"function_stat"
              ~field:"fn" ~value:name
          with
          | Pricklypear_lisp.Ast.VStr fields_json ->
              (try
                 let j = Yojson.Safe.from_string fields_json in
                 let open Yojson.Safe.Util in
                 let steps = member "steps" j |> to_int_option |> Option.value ~default:0 in
                 let prims = member "prim_calls" j |> to_int_option |> Option.value ~default:0 in
                 let dbh = member "db_hits" j |> to_int_option |> Option.value ~default:0 in
                 HTML.p [ HTML.class_ "meta last-run" ]
                   [ txt "last run: %d steps, %d prim calls, %d DB hits" steps prims dbh ]
               with _ -> HTML.p [ HTML.class_ "muted" ] [ txt "" ])
          | _ -> HTML.p [ HTML.class_ "muted" ] [ txt "(no measured run yet)" ]
        in
        let explain_text =
          match Analyze.explain_function (Session.store session) ~fn_name:name with
          | Ok t -> t
          | Error _ -> ""
        in
        chrome ~session ~title:name [
          HTML.p [] [txt "author: %s" author];
          HTML.p []
            (Ast_html.recursion_badges ~fn_name:name expr
             @ [
                 txt " ";
                 HTML.span [ HTML.class_ "badge complexity" ] [ txt "%s" cx ];
                 txt " ";
               ]
             @ db_nodes);
          stats_node;
          Ast_html.pp_expr_html ~fn_name:name ~mutual expr;
          HTML.p [] [
            HTML.button
              [
                HTML.type_ "button";
                HTML.class_ "btn";
                Hx.post "%s" ("/api/analyze/" ^ Packages.name_encode name);
                Hx.target "#analyze-result";
                Hx.swap "innerHTML";
              ]
              [ txt "Analyze" ];
            txt " ";
            HTML.button
              [
                HTML.type_ "button";
                HTML.class_ "btn secondary";
                Hx.post "%s" ("/api/analyze/" ^ Packages.name_encode name ^ "?llm=1");
                Hx.target "#analyze-result";
                Hx.swap "innerHTML";
              ]
              [ txt "Analyze (LLM)" ];
            txt " ";
            HTML.button
              [
                HTML.type_ "button";
                HTML.class_ "btn secondary";
                Hx.post "%s" ("/api/explain/" ^ Packages.name_encode name);
                Hx.target "#explain-result";
                Hx.swap "innerHTML";
              ]
              [ txt "Explain" ];
          ];
          HTML.div [ HTML.id "analyze-result" ] [ txt "" ];
          HTML.details [ HTML.class_ "explain-panel" ]
            [
              HTML.summary [] [ txt "Inside-out breakdown" ];
              HTML.div [ HTML.id "explain-result" ]
                [
                  HTML.pre [ HTML.class_ "bb-source explain-layer" ]
                    [ txt "%s" explain_text ];
                ];
            ];
          HTML.p [] [
            HTML.a [HTML.href "%s" (Packages.edit_href name)] [txt "edit"];
            txt " · ";
            HTML.a [HTML.href "%s" (Packages.inspect_href name)] [txt "inspect"];
          ];
        ]
  in
  html_response reqd ~session body;
  ()

open Http_data

let about_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
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
          HTML.li [] [txt "Type + row — (deftype ...) then (create/get/update/rows ...); rows are per-user plus shared seed."];
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
  html_response reqd ~session body

(* Page path for inspector: "/hello" -> "hello" for URL segment. *)
let page_inspect_path (route : string) : string =
  if String.length route > 0 && route.[0] = '/' then
    String.sub route 1 (String.length route - 1)
  else route

let pages_list_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let routes = Session.list_pages_meta session in
  let rows =
    List.map
      (fun (route, handler, vis) ->
        let inspect = "/page/" ^ page_inspect_path route in
        HTML.tr [] [
          HTML.td [] [HTML.a [HTML.href "%s" route] [txt "%s" route]];
          HTML.td [] [
            HTML.a [HTML.href "%s" (Packages.edit_href handler)] [txt "%s" handler];
          ];
          HTML.td [] [txt "%s" vis];
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
        txt "Page = URL endpoint. Function = code it runs. Visibility: private (login) or public (anonymous GET). ";
        txt "(route path handler 'public) to open a page.";
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
                 HTML.th [] [txt "visibility"];
                 HTML.th [] [txt "actions"];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd ~session body

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

let home_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  (* Nav comes from Session.ui_nav_html (refreshed in request launch). *)
  let home_html = try_ui_fn session "ui/home" in
  let content =
    match home_html with
    | Some html when String.trim html <> "" ->
        [ txt ~raw:true "%s" html ]
    | _ -> emergency_home_content session
  in
  let body = chrome ~session ~title:"" content in
  html_response reqd ~session body;
  ()

let repl_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
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
  html_response reqd ~session body

(* Default stub body for a brand-new function. *)
let stub_body (name : string) : string =
  Printf.sprintf "(lambda ()\n  \"TODO: implement %s\")" name

(* Static call graph — shared module (image.inspector I1). *)
let page_function_graph = Call_graph.page_function_graph

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
    : (string, string list) Hashtbl.t =
  let routes = Session.list_pages session in
  let tbl : (string, string list) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (route, handler) ->
      let prev = try Hashtbl.find tbl handler with Not_found -> [] in
      if List.mem route prev then ()
      else Hashtbl.replace tbl handler (route :: prev))
    routes;
  tbl

let editor_form_partial ~(name : string) ~(body_src : string) ?(msg = "")
    ?(return_to = "") () : Pure_html.node =
  let flash =
    if msg = "" then HTML.div [] []
    else
      HTML.p
        [ HTML.class_
            (if String.length msg >= 5 && String.sub msg 0 5 = "error" then "err"
             else "ok") ]
        [ txt "%s" msg ]
  in
  let view_href =
    if return_to <> "" then
      (* Stay in bindings browser; drop tab=edit → definition. *)
      (try
         let base =
           match String.split_on_char '&' return_to with
           | [] -> return_to
           | parts ->
               parts
               |> List.filter (fun p ->
                      not (String.starts_with ~prefix:"tab=" p)
                      && not (String.starts_with ~prefix:"msg=" p))
               |> String.concat "&"
         in
         base
       with _ -> return_to)
    else "/binding/" ^ Packages.name_encode name
  in
  HTML.div [HTML.id "editor-root"] [
    flash;
    HTML.div [HTML.class_ "card bb-edit-form"] [
      HTML.form
        [ HTML.method_ `POST
        ; HTML.action "%s" ("/edit/" ^ Packages.name_encode name)
        ; HTML.enctype `urlencoded ]
        [ (if return_to = "" then HTML.div [] []
           else
             HTML.input
               [ HTML.type_ "hidden"
               ; HTML.name "return_to"
               ; HTML.value "%s" return_to ]);
          HTML.p [] [
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
              [ HTML.href "%s" view_href; HTML.class_ "btn secondary" ]
              [ txt "View" ];
          ];
        ];
    ];
  ]

let list_bindings_page (session : Session.t) (reqd : Httpun.Reqd.t)
    : unit =
  let target = (Httpun.Reqd.request reqd).Httpun.Request.target in
  let q = Option.value (query_param target "q") ~default:"" |> String.trim in
  let pkg_q = query_param target "pkg" in
  let fn_q = query_param target "fn" in
  let defs_meta =
    Store.list_function_names_meta (Session.store session)
  in
  (* Effective view only: one row per name (closest branch wins). *)
  let defs =
    List.map (fun (n, e, a, d) -> (n, e, a, d)) defs_meta
  in
  let used_by = used_by_pages_map session in
  let kernel = Session.kernel_catalog session in
  let matches_ws (name, _body, author, _depth) =
    q = ""
    || contains_ci name q
    || contains_ci (Packages.package_of_name name) q
    || contains_ci author q
  in
  let defs_f = List.filter matches_ws defs in
  (* H0.2: derive-on-read known_fn so badges account for callees. *)
  let known_fn =
    Pricklypear_lisp.Effect.known_fn_of_bindings
      (List.map (fun (n, e, _, _) -> (n, e)) defs_f)
  in
  let badge_of expr =
    Pricklypear_lisp.Effect.EffSet.badge
      (Pricklypear_lisp.Effect.infer ~known_fn expr)
  in
  let txn_hints_of expr =
    Pricklypear_lisp.Effect.txn_lint ~known_fn expr
  in
  let txn_hint_nodes expr =
    match txn_hints_of expr with
    | [] -> []
    | hs ->
        [ HTML.div [HTML.class_ "card"]
            (HTML.p [HTML.class_ "muted"] [txt "txn lint (soft):"]
             :: List.map
                  (fun h -> HTML.p [HTML.class_ "err"] [txt "%s" h])
                  hs)
        ]
  in
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
  let fn_href ?tab pkg fn =
    let base =
      "/bindings?pkg=" ^ Packages.name_encode pkg ^ "&fn="
      ^ Packages.name_encode fn
    in
    match tab with
    | None | Some "" | Some "view" -> base
    | Some t -> base ^ "&tab=" ^ t
  in
  let tab_q =
    match query_param target "tab" with
    | Some t -> String.lowercase_ascii (String.trim t)
    | None -> "view"
  in
  let tab_q =
    match tab_q with
    | "edit" | "inspect" | "observe" | "observability" ->
        if tab_q = "observability" then "observe" else tab_q
    | _ -> "view"
  in
  let flash_msg =
    Option.value (query_param target "msg") ~default:"" |> String.trim
  in
  match pkg_q, fn_q with
  | Some "special-forms", Some fname ->
      let entry =
        Pricklypear_lisp.Special_forms.find fname
      in
      let body =
        match entry with
        | None ->
            chrome ~session ~title:("Special form " ^ fname) [
              HTML.p [] [
                HTML.a [HTML.href "/bindings"] [txt "Functions"];
                txt " › ";
                HTML.a [HTML.href "/bindings?pkg=special-forms"]
                  [txt "(special-forms)"];
              ];
              HTML.p [HTML.class_ "err"] [txt "unknown special form %s" fname];
            ]
        | Some e ->
            chrome ~session ~title:("Special form " ^ e.name) [
              HTML.p [] [
                HTML.a [HTML.href "/bindings"] [txt "Functions"];
                txt " › ";
                HTML.a [HTML.href "/bindings?pkg=special-forms"]
                  [txt "(special-forms)"];
                txt " › %s" e.name;
              ];
              HTML.div [HTML.class_ "card"] [
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "kind: "];
                  txt "special form (evaluator)";
                ];
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "name: "];
                  txt "%s" e.name;
                ];
                HTML.p [] [
                  HTML.span [HTML.class_ "muted"] [txt "arity: "];
                  txt "%s" e.arity;
                ];
                HTML.p [] [txt "%s" e.summary];
                HTML.h2 [] [txt "example"];
                HTML.pre [] [txt "%s" e.examples];
                HTML.p [HTML.class_ "muted"] [
                  txt "Handled in lisp/lib/eval.ml — not an env binding, not a prim, not editable as Lisp. Change requires OCaml + tests + image deploy.";
                ];
              ];
            ]
      in
      html_response reqd ~session body;
      ()
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
      html_response reqd ~session body;
      ()
  | Some pkg, Some local ->
      let full =
        if String.contains local '/' then local
        else if pkg = "" || pkg = "(root)" || pkg = "root" then local
        else Packages.join_name ~package:pkg ~local
      in
      let found = Store.lookup_function (Session.store session) full in
      let crumb_pkg = if pkg = "(root)" then "" else pkg in
      let body =
        match found with
        | None ->
            (chrome ~session ~title:full [
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
              ])
        | Some (expr, author, _doc) ->
            let badge = badge_of expr in
            let cx_badge =
              Pricklypear_lisp.Complexity.badge
                (Pricklypear_lisp.Complexity.classify ~fn_name:full expr)
            in
            let depth = depth_of full in
            let snap = Call_graph.snapshot_of_session session in
            let callees = Call_graph.callees snap ~name:full ~depth:1 in
            let callers = Call_graph.callers snap full in
            let routes_ub = Call_graph.used_by_routes snap full in
            let holes = Call_graph.holes snap full in
            let impact_badge =
              match
                List.find_opt
                  (fun (e : Session_types.impact_event) -> e.name = full)
                  session.impact_queue
              with
              | Some e when e.status = "pending" ->
                  HTML.p [] [
                    HTML.span [HTML.class_ "badge impact"]
                      [txt "impact pending · %d dependents · %d routes"
                         (List.length e.dependents) (List.length e.routes)];
                  ]
              | _ -> HTML.p [HTML.class_ "muted"] [txt ""]
            in
            let name_href n =
              let pkg = Packages.package_of_name n in
              let local = Packages.local_name n in
              fn_href pkg local
            in
            let name_links names =
              if names = [] then [txt "(none)"]
              else
                List.concat_map
                  (fun n ->
                    [ HTML.a [HTML.href "%s" (name_href n)] [txt "%s" n]
                    ; txt " "
                    ])
                  names
            in
            let route_links paths =
              if paths = [] then [txt "(none)"]
              else
                List.concat_map
                  (fun p ->
                    let seg =
                      if String.length p > 0 && p.[0] = '/' then
                        String.sub p 1 (String.length p - 1)
                      else p
                    in
                    [ HTML.a [HTML.href "%s" ("/page/" ^ seg)] [txt "%s" p]
                    ; txt " "
                    ])
                  paths
            in
            let hole_nodes =
              if holes = [] then HTML.p [HTML.class_ "muted"] [txt "no holes"]
              else
                HTML.ul []
                  (List.map
                     (fun h ->
                       HTML.li [] [
                         txt "%s " h;
                         HTML.a
                           [ HTML.href "%s" (Packages.edit_href h)
                           ; HTML.class_ "btn secondary" ]
                           [txt "Stub/Edit"];
                       ])
                     holes)
            in
            (* Smalltalk/iTunes column browser:
               top = packages | functions | meta
               bottom = full-width definition (code pane). *)
            let pkg_selected p =
              let key = if p = "" then "root" else p in
              key = pkg || p = crumb_pkg || Packages.package_label p = pkg
            in
            let pkg_col =
              HTML.div [HTML.class_ "card bb-col"] [
                HTML.h3 [] [txt "Packages"];
                HTML.ul [HTML.class_ "bb-list"]
                  (List.map
                     (fun (p, _) ->
                       let key = if p = "" then "root" else p in
                       let lab = Packages.package_label p in
                       let cls =
                         if pkg_selected p then "bb-item is-active" else "bb-item"
                       in
                       HTML.li [HTML.class_ "%s" cls] [
                         HTML.a [HTML.href "%s" (pkg_href key)] [txt "%s" lab];
                       ])
                     groups);
              ]
            in
            let fns_in_pkg =
              let rows =
                try List.assoc crumb_pkg groups with Not_found ->
                  (try List.assoc pkg groups with Not_found -> [])
              in
              HTML.div [HTML.class_ "card bb-col"] [
                HTML.h3 [] [txt "%s" (Packages.package_label crumb_pkg)];
                HTML.ul [HTML.class_ "bb-list"]
                  (List.map
                     (fun (n, _, _) ->
                       let loc = Packages.local_name n in
                       let cls =
                         if n = full then "bb-item is-active" else "bb-item"
                       in
                       HTML.li [HTML.class_ "%s" cls] [
                         HTML.a [HTML.href "%s" (fn_href pkg loc)] [txt "%s" loc];
                       ])
                     rows);
              ]
            in
            let meta_col =
              HTML.div [HTML.class_ "card bb-col bb-meta"] (
              [ HTML.p [HTML.class_ "bb-crumb"] [
                   HTML.a [HTML.href "/bindings"] [txt "Functions"];
                   txt " › ";
                   HTML.a [HTML.href "%s" (pkg_href pkg)]
                     [txt "%s" (Packages.package_label crumb_pkg)];
                   txt " › ";
                   HTML.strong [] [txt "%s" (Packages.local_name full)];
                 ];
                 impact_badge;
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
                   HTML.span [HTML.class_ "muted"] [txt "used by (handler): "];
                   used_links full;
                 ];
                 HTML.p []
                   ([ HTML.span [HTML.class_ "muted"] [txt "used-by-routes: "] ]
                    @ route_links routes_ub);
                 HTML.p []
                   ([ HTML.span [HTML.class_ "muted"] [txt "callers: "] ]
                    @ name_links callers);
                 HTML.p []
                   ([ HTML.span [HTML.class_ "muted"] [txt "callees: "] ]
                    @ name_links callees);
                 HTML.p [] [
                   HTML.a [HTML.href "/changes"] [txt "Branch changes"];
                 ];
                 HTML.h3 [] [txt "Holes"];
                 hole_nodes;
               ]
               @ txn_hint_nodes expr)
            in
            let mutual = Call_graph.mutual_of snap full in
            let tab_link id label =
              let active = tab_q = id in
              HTML.a
                [
                  HTML.href "%s" (fn_href ~tab:id pkg local);
                  HTML.class_ "%s"
                    (if active then "bb-tab is-active" else "bb-tab");
                ]
                [ txt "%s" label ]
            in
            let tabs =
              HTML.div []
                [
                  (* Inline critical tab chrome so stale ui/css seeds still look right. *)
                  HTML.style [] "%s"
                    ".bb-tabs{display:flex;gap:.35rem;flex-wrap:wrap;margin:.4rem 0 .6rem}
.bb-tab{display:inline-block;padding:.28rem .65rem;border:1px solid #000;text-decoration:none;color:inherit;background:#f7f5ef}
.bb-tab.is-active{background:#1a1a1a;color:#f7f5ef;font-weight:700}
.bb-tab:hover{background:#ece9e0}";
                  HTML.nav [ HTML.class_ "bb-tabs" ]
                    [
                      tab_link "view" "Definition";
                      tab_link "edit" "Edit";
                      tab_link "inspect" "Inspect";
                      tab_link "observe" "Observability";
                    ];
                ]
            in
            let flash_node =
              if flash_msg = "" then HTML.div [] []
              else
                HTML.p
                  [
                    HTML.class_
                      (if String.length flash_msg >= 5
                          && String.sub flash_msg 0 5 = "error"
                       then "err"
                       else "ok");
                  ]
                  [ txt "%s" flash_msg ]
            in
            let view_pane =
              Ast_html.pp_expr_html ~fn_name:full ~mutual expr
            in
            let edit_return = fn_href ~tab:"edit" pkg local in
            let edit_pane =
              let src = Pricklypear_lisp.Reader.pp_expr expr in
              editor_form_partial ~name:full ~body_src:src ~msg:flash_msg
                ~return_to:edit_return ()
            in
            let inspect_pane =
              let j =
                try
                  Yojson.Safe.pretty_to_string
                    (Pricklypear_lisp.Ast.expr_to_json expr)
                with _ -> "(json unavailable)"
              in
              HTML.div [ HTML.class_ "bb-inspect" ]
                [
                  HTML.p []
                    (Ast_html.recursion_badges ~fn_name:full expr
                     @ [
                         txt " ";
                         HTML.span [ HTML.class_ "badge complexity" ]
                           [ txt "%s" cx_badge ];
                       ]);
                  HTML.h3 [] [ txt "pretty" ];
                  Ast_html.pp_expr_html ~fn_name:full ~mutual expr;
                  HTML.h3 [] [ txt "AST JSON" ];
                  HTML.pre [ HTML.class_ "bb-source" ] [ txt "%s" j ];
                ]
            in
            let observe_pane =
              let db_prims = Db_cost.db_prims_of_expr expr in
              let db_nodes =
                if db_prims = [] then [ txt "" ]
                else
                  List.concat_map
                    (fun p ->
                      [
                        HTML.span [ HTML.class_ "badge db-cost" ]
                          [
                            txt "%s: %s" p
                              (Db_cost.badge (Db_cost.static_cost p));
                          ];
                        txt " ";
                      ])
                    db_prims
              in
              let stats_node =
                match
                  Crud.find_row (Session.store session)
                    ~type_name:"function_stat" ~field:"fn" ~value:full
                with
                | Pricklypear_lisp.Ast.VStr fields_json ->
                    (try
                       let j = Yojson.Safe.from_string fields_json in
                       let open Yojson.Safe.Util in
                       let steps =
                         member "steps" j |> to_int_option
                         |> Option.value ~default:0
                       in
                       let prims =
                         member "prim_calls" j |> to_int_option
                         |> Option.value ~default:0
                       in
                       let dbh =
                         member "db_hits" j |> to_int_option
                         |> Option.value ~default:0
                       in
                       HTML.p [ HTML.class_ "meta last-run" ]
                         [
                           txt
                             "last run: %d steps, %d prim calls, %d DB hits"
                             steps prims dbh;
                         ]
                     with _ -> HTML.p [ HTML.class_ "muted" ] [ txt "" ])
                | _ ->
                    HTML.p [ HTML.class_ "muted" ]
                      [ txt "(no measured run yet)" ]
              in
              let explain_text =
                match
                  Analyze.explain_function (Session.store session)
                    ~fn_name:full
                with
                | Ok t -> t
                | Error _ -> ""
              in
              HTML.div [ HTML.class_ "bb-observe" ]
                [
                  HTML.p []
                    (Ast_html.recursion_badges ~fn_name:full expr
                     @ [
                         txt " ";
                         HTML.span [ HTML.class_ "badge complexity" ]
                           [ txt "%s" cx_badge ];
                         txt " ";
                       ]
                     @ db_nodes);
                  stats_node;
                  Ast_html.pp_expr_html ~fn_name:full ~mutual expr;
                  HTML.p []
                    [
                      HTML.button
                        [
                          HTML.type_ "button";
                          HTML.class_ "btn";
                          Hx.post "%s"
                            ("/api/analyze/" ^ Packages.name_encode full);
                          Hx.target "#analyze-result";
                          Hx.swap "innerHTML";
                        ]
                        [ txt "Analyze" ];
                      txt " ";
                      HTML.button
                        [
                          HTML.type_ "button";
                          HTML.class_ "btn secondary";
                          Hx.post "%s"
                            ("/api/analyze/"
                             ^ Packages.name_encode full
                             ^ "?llm=1");
                          Hx.target "#analyze-result";
                          Hx.swap "innerHTML";
                        ]
                        [ txt "Analyze (LLM)" ];
                      txt " ";
                      HTML.button
                        [
                          HTML.type_ "button";
                          HTML.class_ "btn secondary";
                          Hx.post "%s"
                            ("/api/explain/" ^ Packages.name_encode full);
                          Hx.target "#explain-result";
                          Hx.swap "innerHTML";
                        ]
                        [ txt "Explain" ];
                    ];
                  HTML.div [ HTML.id "analyze-result" ] [ txt "" ];
                  HTML.details [ HTML.class_ "explain-panel" ]
                    [
                      HTML.summary [] [ txt "Inside-out breakdown" ];
                      HTML.div [ HTML.id "explain-result" ]
                        [
                          HTML.pre
                            [ HTML.class_ "bb-source explain-layer" ]
                            [ txt "%s" explain_text ];
                        ];
                    ];
                ]
            in
            let pane_body =
              match tab_q with
              | "edit" -> edit_pane
              | "inspect" -> inspect_pane
              | "observe" -> observe_pane
              | _ -> view_pane
            in
            let code_pane =
              HTML.div [ HTML.class_ "card bb-code" ]
                [
                  HTML.div [ HTML.class_ "bb-code-head" ]
                    [
                      HTML.h2 [] [ txt "%s · %s" full
                        (match tab_q with
                         | "edit" -> "edit"
                         | "inspect" -> "inspect"
                         | "observe" -> "observability"
                         | _ -> "definition") ];
                      HTML.p [ HTML.class_ "bb-code-actions" ]
                        [
                          HTML.span [ HTML.class_ "badge complexity" ]
                            [ txt "%s" cx_badge ];
                        ];
                    ];
                  tabs;
                  (if tab_q = "edit" then HTML.div [] [] else flash_node);
                  pane_body;
                ]
            in
            (chrome ~session ~title:full
                 [ HTML.div [HTML.class_ "bb-browser"]
                     [ HTML.div [HTML.class_ "bb-three"]
                         [ pkg_col; fns_in_pkg; meta_col ]
                     ; code_pane
                     ] ])
      in
      html_response reqd ~session body;
      ()
  | Some "special-forms", None ->
      let rows =
        let all = Pricklypear_lisp.Special_forms.summaries in
        if q = "" then all
        else List.filter (fun (n, s) -> contains_ci n q || contains_ci s q) all
      in
      let items =
        List.map
          (fun (n, summary) ->
            HTML.tr [] [
              HTML.td [] [
                HTML.a [HTML.href "%s" (fn_href "special-forms" n)] [txt "%s" n];
              ];
              HTML.td [] [txt "special form"];
              HTML.td [] [txt "%s" summary];
            ])
          rows
      in
      let body =
        chrome ~session ~title:"Package (special-forms)" [
          HTML.p [] [HTML.a [HTML.href "/bindings"] [txt "← all packages"]];
          HTML.p [HTML.class_ "meta"] [
            txt "Evaluator special forms (syntax). Not env bindings, not prims. Read-only.";
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
      html_response reqd ~session body;
      ()
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
      html_response reqd ~session body;
      ()
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
            let badge = badge_of body in
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
      html_response reqd ~session body;
      ()
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
        ( "(special-forms)"
        , "/bindings?pkg=special-forms"
        , List.length Pricklypear_lisp.Special_forms.summaries
        , "language" )
        :: ( "(kernel)"
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
              txt "Tip: open (special-forms) for seq/if/lambda, (kernel) for create/+/render-html, …";
            ]
          ]
      in
      html_response reqd ~session body;
      ()

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
    ?(body_src = "") (reqd : Httpun.Reqd.t) : unit =
  let found = Store.lookup_function (Session.store session) name in
  let body =
    match found, body_src with
    | _, src when src <> "" ->
        chrome ~session ~title:("Edit " ^ name)
          [ editor_form_partial ~name ~body_src:src ~msg () ]
    | Some (expr, _, _), _ ->
        let initial = Pricklypear_lisp.Reader.pp_expr expr in
        chrome ~session ~title:("Edit " ^ name)
          [ editor_form_partial ~name ~body_src:initial ~msg () ]
    | None, _ ->
        chrome ~session ~title:("Edit " ^ name)
          [ missing_editor_card session name ]
  in
  html_response reqd ~session body;
  ()

let api_edit_create (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let name =
    match form_field body "name" with
    | Some n when String.trim n <> "" -> String.trim n
    | _ -> ""
  in
  if name = "" then begin
    html_response reqd ~session ~status:`Bad_request
      (HTML.div [HTML.class_ "err"] [txt "name required"]);
    ()
  end else
    let rebind =
      match form_field body "rebind_route" with
      | Some ("1" | "on" | "true") -> true
      | _ -> false
    in
    let body_src = stub_body name in
    let res =
      Session.define_binding session ~name ~body_src
        ~author:(Session.username session)
    in
    match res with
    | Error e ->
        html_response reqd ~session ~status:`Bad_request
          (HTML.div [HTML.id "editor-root"; HTML.class_ "err"]
             [txt "create failed: %s" e]);
        ()
    | Ok _ ->
        let () =
          if rebind then
            Session.set_route session ~route:("/" ^ name) ~handler:name ()
          else ()
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
          Httpun.Headers.of_list
            [ ("content-type", "text/html; charset=utf-8")
            ; ("content-length", string_of_int (String.length s)) ]
        in
        let resp = Httpun.Response.create ~headers `OK in
        safe_respond_with_string reqd resp s;
        ()

let page_inspector (session : Session.t) (path_seg : string)
    (reqd : Httpun.Reqd.t) : unit =
  let route =
    if path_seg = "" then "/"
    else if path_seg.[0] = '/' then path_seg
    else "/" ^ path_seg
  in
  match Session.lookup_route session route with
  | None ->
      html_response reqd ~session ~status:`Not_found
        (chrome ~session ~title:("Page " ^ route) [
           HTML.p [HTML.class_ "err"] [txt "no page registered for %s" route];
           HTML.p [] [HTML.a [HTML.href "/pages"] [txt "all pages"]];
         ]);
      ()
  | Some handler ->
      let graph = page_function_graph session ~handler in
      let found = Store.lookup_function (Session.store session) handler in
      let body_block =
        match found with
        | None ->
            HTML.p [HTML.class_ "err"] [txt "handler function missing in store"]
        | Some (expr, author, _doc) ->
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
      html_response reqd ~session body;
      ()

let inspect_binding_page (session : Session.t) (name : string)
    (reqd : Httpun.Reqd.t) : unit =
  let found = Store.lookup_function (Session.store session) name in
  let defs = Store.list_function_names (Session.store session) in
  let known_fn =
    Pricklypear_lisp.Effect.known_fn_of_bindings
      (List.map (fun (n, e, _) -> (n, e)) defs)
  in
  let body =
    match found with
    | None ->
        chrome ~session ~title:("Inspect " ^ name) [
          HTML.p [HTML.class_ "err"] [txt "not found"];
        ]
    | Some (expr, author, _doc) ->
        let j =
          try Yojson.Safe.pretty_to_string
                (Pricklypear_lisp.Ast.expr_to_json expr)
          with _ -> "(json unavailable)"
        in
        let badge =
          Pricklypear_lisp.Effect.EffSet.badge
            (Pricklypear_lisp.Effect.infer ~known_fn expr)
        in
        let cx =
          Pricklypear_lisp.Complexity.badge
            (Pricklypear_lisp.Complexity.classify ~fn_name:name expr)
        in
        let snap = Call_graph.snapshot_of_session session in
        let mutual = Call_graph.mutual_of snap name in
        let txn_nodes =
          match Pricklypear_lisp.Effect.txn_lint ~known_fn expr with
          | [] -> []
          | hs ->
              [ HTML.div [HTML.class_ "card"]
                  (HTML.p [HTML.class_ "muted"] [txt "txn lint (soft):"]
                   :: List.map
                        (fun h -> HTML.p [HTML.class_ "err"] [txt "%s" h])
                        hs)
              ]
        in
        chrome ~session ~title:("Inspect binding " ^ name)
          ([ HTML.div [HTML.class_ "card"] [
               HTML.p [] [txt "author: %s" author];
               HTML.p [] [txt "branch: %s" (Session.branch_name session)];
               HTML.p [] [txt "effects: %s" badge];
               HTML.p []
                 (Ast_html.recursion_badges ~fn_name:name expr
                  @ [ txt " "; HTML.span [HTML.class_ "badge complexity"] [txt "%s" cx] ]);
               HTML.h2 [] [txt "pretty"];
               Ast_html.pp_expr_html ~fn_name:name ~mutual expr;
               HTML.h2 [] [txt "AST JSON"];
               HTML.pre [] [txt "%s" j];
               HTML.p [] [
                 HTML.a [HTML.href "%s" (Packages.edit_href name)] [txt "edit"];
                 txt " · ";
                 HTML.a [HTML.href "%s" ("/binding/" ^ Packages.name_encode name)]
                   [txt "observability"];
               ];
             ];
           ]
           @ txn_nodes)
  in
  html_response reqd ~session body;
  ()

let inspect_row_page (session : Session.t) (type_name : string) (id : string)
    (reqd : Httpun.Reqd.t) : unit =
  (* Canonical row URL is /data/<type>/<id> (image.data-browser). *)
  ignore session;
  redirect_response reqd
    (Printf.sprintf "/data/%s/%s" type_name id);
  ()

let api_eval (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
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
      ()
  | Some expr ->
      (try
      let trace = Sys.getenv_opt "PP_EVAL_TRACE" <> None in
      let t0 = if trace then Unix.gettimeofday () else 0.0 in
      let v = Session.eval_one session expr in
      let t1 = if trace then Unix.gettimeofday () else 0.0 in
          (* Serialize the Lisp value as NATIVE JSON (not a pp_value
             string). CRUD read prims return fields as a VStr of JSON
             text; pp_value would backslash-escape those quotes and
             json_response would re-encode again => triple-escaped
             output. value_to_json re-parses JSON-lookalike VStrs into
             real JSON objects/arrays so field data round-trips as
             clean, valid JSON (i-fields-as-string-2026-08-12). *)
          let j = `Assoc [("ok", `Bool true); ("value", Prims_pure.value_to_json v)] in
          let t2 = if trace then Unix.gettimeofday () else 0.0 in
          if trace then
            Printf.eprintf "[eval-trace] api_eval           eval %7.1f ms  json %7.1f ms\n%!"
              ((t1 -. t0) *. 1000.0) ((t2 -. t1) *. 1000.0);
          json_response reqd j;
          ()
    with exn ->
      (* Shared codec (remote-eval.borg substrate): same wire shape as
         before, factored into pure/prims_pure.ml so the image and a
         remote proxy rim link the SAME error<->JSON codec. *)
          json_response reqd (Prims_pure.error_to_json exn);
          ())

(* POST /api/prim (borge/remote-eval.borg server-endpoint).

   One prim call per request, dispatched through the session prim table
   (the SAME table /api/eval dispatches through — the one
   [register_session_prims] + control/CRUD registrations build) so
   identity/author/branch are exactly as an in-process eval. Body:
   {name: str, args: [json…]}. Args deserialize via the shared codec
   (Prims_pure.json_to_value), the result serializes via
   Prims_pure.value_to_json, and failures use the shared
   Prims_pure.error_to_json — so a remote client observes identical
   values and exceptions to an in-process eval.

   GRANULARITY: per-DB-hit, not per-expression (see placement in the
   spec for the mitigation). Same auth path as /api/eval (basic-auth /
   cookie session). No new privilege — a prim call is equivalent
   privilege to an eval that performs the prim.

   REFUSE-OR-FORWARD: prims that MUST execute server-side to mean
   anything (route/fork/merge/checkout/auth-admin) are refused with a
   structured {ok:false, kind:"server_only", name, reason} error citing
   locality rather than pretending to proxy them. Classification is
   DERIVED from prim-registry metadata (Primitives.is_server_only — set
   where the prim was registered), never a hand-maintained list. *) 
let api_prim (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let name, args =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          let n =
            match List.assoc_opt "name" items with
            | Some (`String s) -> s
            | _ -> ""
          in
          let a =
            match List.assoc_opt "args" items with
            | Some (`List xs) -> List.map Prims_pure.json_to_value xs
            | _ -> []
          in
          (n, a)
      | _ -> ("", [])
    with _ -> ("", [])
  in
  if name = "" then
    json_response reqd ~status:`Bad_request
      (`Assoc [("ok", `Bool false); ("error", `String "name required")])
  else
    match Primitives.dispatch session.prims name args with
    | Primitives.Prim_refused_server_only (nm, reason) ->
        (* Refuse: server-only prim must run on the server's authority
           (branch state, locks, auth), not through a remote proxy. *) 
        json_response reqd
          (`Assoc
             [ ("ok", `Bool false)
             ; ("kind", `String "server_only")
             ; ("name", `String nm)
             ; ("reason", `String reason) ])
    | Primitives.Prim_ok v ->
        json_response reqd
          (`Assoc
             [("ok", `Bool true); ("value", Prims_pure.value_to_json v)])
    | Primitives.Prim_error exn ->
        json_response reqd (Prims_pure.error_to_json exn);
    ()

let save_edit (session : Session.t) (path_name : string)
    (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let name =
    match form_field body "name" with
    | Some n when String.trim n <> "" -> String.trim n
    | _ -> path_name
  in
  let body_src = Option.value (form_field body "body") ~default:"" in
  let return_to =
    match form_field body "return_to" with
    | Some s when String.trim s <> "" -> String.trim s
    | _ -> ""
  in
  let res =
    Session.define_binding session ~name ~body_src
      ~author:(Session.username session)
  in
  match res with
  | Ok v ->
      let () = Call_graph.record_define_side_effects session ~name in
      let impact = Call_graph.impact_summary_text session name in
      let msg =
        Printf.sprintf "saved %s => %s\n%s" name
          (Pricklypear_lisp.Reader.pp_value v) impact
      in
      if return_to <> "" then begin
        let sep = if String.contains return_to '?' then "&" else "?" in
        redirect_response reqd
          (return_to ^ sep ^ "msg=" ^ url_encode msg)
      end else editor_page session name ~msg ~body_src reqd
  | Error e ->
      let msg = "error: " ^ e in
      if return_to <> "" then begin
        let sep = if String.contains return_to '?' then "&" else "?" in
        redirect_response reqd
          (return_to ^ sep ^ "msg=" ^ url_encode msg)
      end else editor_page session name ~msg ~body_src reqd

