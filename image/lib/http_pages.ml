(** Pricklypear Image — HTTP system & product page handlers. *)
open Lwt.Syntax
open Pure_html
open Http_core
open Http_data
open Http_branches
open Http_agent

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
  html_response reqd ~session body;
  Lwt.return_unit

open Http_data

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
  html_response reqd ~session body

(* Page path for inspector: "/hello" -> "hello" for URL segment. *)
let page_inspect_path (route : string) : string =
  if String.length route > 0 && route.[0] = '/' then
    String.sub route 1 (String.length route - 1)
  else route

let pages_list_page (session : Session.t) (reqd : Httpaf.Reqd.t) : unit =
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
  html_response reqd ~session body;
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
  html_response reqd ~session body

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
  let fn_href pkg fn =
    "/bindings?pkg=" ^ Packages.name_encode pkg ^ "&fn="
    ^ Packages.name_encode fn
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
      Lwt.return_unit
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
            let badge = badge_of expr in
            let pp = Pricklypear_lisp.Reader.pp_expr expr in
            let depth = depth_of full in
            chrome ~session ~title:full
              ([ HTML.p [] [
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
                 ];
               ]
               @ txn_hint_nodes expr
               @ [ HTML.div [HTML.class_ "card"] [
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
                 ])
      in
      html_response reqd ~session body;
      Lwt.return_unit
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
      html_response reqd ~session body;
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
  html_response reqd ~session body;
  Lwt.return_unit

let api_edit_create (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let name =
    match form_field body "name" with
    | Some n when String.trim n <> "" -> String.trim n
    | _ -> ""
  in
  if name = "" then begin
    html_response reqd ~session ~status:`Bad_request
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
        html_response reqd ~session ~status:`Bad_request
          (HTML.div [HTML.id "editor-root"; HTML.class_ "err"]
             [txt "create failed: %s" e]);
        Lwt.return_unit
    | Ok _ ->
        let* () =
          if rebind then
            Session.set_route session ~route:("/" ^ name) ~handler:name ()
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
        safe_respond_with_string reqd resp s;
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
      html_response reqd ~session ~status:`Not_found
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
      html_response reqd ~session body;
      Lwt.return_unit

let inspect_binding_page (session : Session.t) (name : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* found = Store.lookup_function (Session.store session) name in
  let* defs = Store.list_function_names (Session.store session) in
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
    | Some (expr, author) ->
        let pp = Pricklypear_lisp.Reader.pp_expr expr in
        let j =
          try Yojson.Safe.pretty_to_string
                (Pricklypear_lisp.Ast.expr_to_json expr)
          with _ -> "(json unavailable)"
        in
        let badge =
          Pricklypear_lisp.Effect.EffSet.badge
            (Pricklypear_lisp.Effect.infer ~known_fn expr)
        in
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
               HTML.h2 [] [txt "pretty"];
               HTML.pre [] [txt "%s" pp];
               HTML.h2 [] [txt "AST JSON"];
               HTML.pre [] [txt "%s" j];
               HTML.p [] [
                 HTML.a [HTML.href "%s" (Packages.edit_href name)] [txt "edit"];
               ];
             ];
           ]
           @ txn_nodes)
  in
  html_response reqd ~session body;
  Lwt.return_unit

let inspect_row_page (session : Session.t) (type_name : string) (id : string)
    (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  (* Canonical row URL is /data/<type>/<id> (image.data-browser). *)
  ignore session;
  redirect_response reqd
    (Printf.sprintf "/data/%s/%s" type_name id);
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
          (* Use the full printer — partial match left Arity_mismatch /
             Type_mismatch / etc. as opaque "Interp_error(_)" to agents. *)
          let msg = Agent_tools.pp_interp_exn exn in
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

