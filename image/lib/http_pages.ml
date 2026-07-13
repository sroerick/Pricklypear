(** Pricklypear Image — HTTP system & product page handlers.
    Depends on [Http_core] for chrome/forms/responses. *)
open Lwt.Syntax
open Pure_html
open Http_core
open Http_data

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
  html_response reqd ~session body;
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
      html_response reqd ~session body;
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

let default_http_agent_seed =
  (* Short fallback when no session seed; tools sessions should prefer
     Session.default_agent_seed (dogfood + register_route rules). *)
  "You are the co-pilot for a Pricklypear multi-user Lisp OS image " ^
  "(software, not a cactus). Prefer CONTEXT; if unknown say so. " ^
  "Use structured tools only (never <tool_call> XML). " ^
  "install_function for lambdas; register_route for HTTP paths; " ^
  "data_rows via create/rows (not outline/*); inspect todo/page first."

(** POST /api/agent — durable sessions.
    Body JSON:
      {prompt, seed?, session_id?, tools?, max_steps?}
    Creates a session when session_id omitted; always returns session_id.
    Progress SSE when Accept prefers event-stream or stream:true; default
    is non-stream JSON (token streaming deferred / flaky on Synthetic). *)
(** Pull the human line out of the HTTP agent prompt wrapper for titles/UI. *)
let agent_user_visible_text (s : string) : string =
  let s = String.trim s in
  let key = "\nUser:\n" in
  let rec find_sub hay needle i =
    let nlen = String.length needle in
    let hlen = String.length hay in
    if i + nlen > hlen then None
    else if String.sub hay i nlen = needle then Some i
    else find_sub hay needle (i + 1)
  in
  match find_sub s key 0 with
  | Some i ->
      String.trim
        (String.sub s (i + String.length key)
           (String.length s - i - String.length key))
  | None ->
      if String.length s >= 20
         && String.sub s 0 20 = "=== IMAGE CONTEXT ===" then
        let n = min 80 (String.length s) in
        String.sub s 0 n ^ (if String.length s > 80 then "…" else "")
      else s

let agent_session_label (s : Ai_store.session) : string =
  let short = String.sub s.id 0 (min 8 (String.length s.id)) in
  match s.title with
  | Some t when String.trim t <> "" ->
      let t = agent_user_visible_text t in
      (* Old rows stored the full IMAGE CONTEXT wrapper as title (truncated
         before User:). Don't show that dump in the chat list. *)
      if String.trim t = ""
         || (String.length t >= 20
             && String.sub t 0 20 = "=== IMAGE CONTEXT ===")
         || String.starts_with ~prefix:"=== IMAGE CONTEXT" t then
        "chat " ^ short
      else if String.length t > 72 then String.sub t 0 72 ^ "…"
      else t
  | _ -> "chat " ^ short

let api_agent (session : Session.t) (reqd : Httpaf.Reqd.t) : unit Lwt.t =
  let* body = read_body reqd in
  let seed, prompt, session_id_opt, tools, max_steps, want_stream =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          let seed =
            match List.assoc_opt "seed" items with
            | Some (`String s) -> s
            | _ -> default_http_agent_seed
          in
          let prompt =
            match List.assoc_opt "prompt" items with
            | Some (`String s) -> s
            | _ ->
                (match List.assoc_opt "q" items with
                 | Some (`String s) -> s
                 | _ -> "")
          in
          let sid =
            match List.assoc_opt "session_id" items with
            | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          in
          let tools =
            match List.assoc_opt "tools" items with
            | Some (`Bool b) -> b
            | Some (`String "0") | Some (`String "false") -> false
            | _ -> true
          in
          let max_steps =
            match List.assoc_opt "max_steps" items with
            | Some (`Int n) -> n
            | Some (`String s) ->
                (match int_of_string_opt s with Some n -> n | None -> 16)
            | _ -> 16
          in
          let stream =
            match List.assoc_opt "stream" items with
            | Some (`Bool b) -> b
            | Some (`String "1") | Some (`String "true") -> true
            | Some (`String "0") | Some (`String "false") -> false
            | _ -> false
          in
          (seed, prompt, sid, tools, max_steps, stream)
      | _ -> (default_http_agent_seed, "", None, true, 16, false)
    with _ ->
      let seed =
        Option.value (form_field body "seed") ~default:default_http_agent_seed
      in
      let prompt =
        match form_field body "prompt" with
        | Some p -> p
        | None -> Option.value (form_field body "q") ~default:""
      in
      let sid = form_field body "session_id" in
      (seed, prompt, sid, true, 16, false)
  in
  if String.trim prompt = "" then begin
    json_response reqd
      (`Assoc [ ("ok", `Bool false); ("error", `String "need prompt") ]);
    Lwt.return_unit
  end else
    let* sid_r =
      match session_id_opt with
      | Some sid -> Lwt.return (Ok sid)
      | None ->
          let title_opt =
            let p = agent_user_visible_text prompt in
            if String.trim p = "" then None
            else
              Some
                (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
          in
          let* v =
            Session.agent_session_new session ~seed ~tools ?title:title_opt ()
          in
          (match v with
           | Pricklypear_lisp.Ast.VStr s
             when not (String.starts_with ~prefix:"agent-session-new error" s) ->
               Lwt.return (Ok s)
           | Pricklypear_lisp.Ast.VStr e -> Lwt.return (Error e)
           | other ->
               Lwt.return
                 (Error (Pricklypear_lisp.Reader.pp_value other)))
    in
    match sid_r with
    | Error e ->
        json_response reqd
          (`Assoc [ ("ok", `Bool false); ("error", `String e) ]);
        Lwt.return_unit
    | Ok sid ->
        if not want_stream then begin
          let* v =
            Session.agent_say session ~session_id:sid ~prompt ~max_steps ()
          in
          let text =
            match v with
            | Pricklypear_lisp.Ast.VStr s -> s
            | _ -> Pricklypear_lisp.Reader.pp_value v
          in
          json_response reqd
            (`Assoc
               [ ("ok", `Bool true)
               ; ("session_id", `String sid)
               ; ("text", `String text)
               ]);
          Lwt.return_unit
        end else begin
          (* Progress SSE (tool/token events), not provider token stream. *)
          let headers =
            Httpaf.Headers.of_list
              [ ("content-type", "text/event-stream; charset=utf-8")
              ; ("cache-control", "no-cache")
              ; ("connection", "keep-alive")
              ; ("x-accel-buffering", "no")
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
          push_json
            (Yojson.Safe.to_string
               (`Assoc
                  [ ("t", `String "session")
                  ; ("session_id", `String sid)
                  ]));
          let on_event = function
            | Llm_client.Token tok ->
                push_json
                  (Yojson.Safe.to_string
                     (`Assoc [ ("t", `String "token"); ("s", `String tok) ]))
            | Llm_client.Done ->
                push_json
                  (Yojson.Safe.to_string (`Assoc [ ("t", `String "done") ]));
                finished := true;
                Lwt_condition.signal cond ()
            | Llm_client.Fail e ->
                push_json
                  (Yojson.Safe.to_string
                     (`Assoc [ ("t", `String "error"); ("s", `String e) ]));
                finished := true;
                Lwt_condition.signal cond ()
          in
          Lwt.async (fun () ->
            let* v =
              Session.agent_say session ~session_id:sid ~prompt ~max_steps
                ~on_event ()
            in
            if not !finished then begin
              (match v with
               | Pricklypear_lisp.Ast.VStr s ->
                   push_json
                     (Yojson.Safe.to_string
                        (`Assoc
                           [ ("t", `String "done")
                           ; ("text", `String s)
                           ; ("session_id", `String sid)
                           ]))
               | _ ->
                   push_json
                     (Yojson.Safe.to_string
                        (`Assoc [ ("t", `String "done") ])));
              finished := true;
              Lwt_condition.signal cond ()
            end;
            Lwt.return_unit);
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
  (* Alias: create tools session + say (same as /api/agent with tools). *)
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
  (* Durable chats: list + open with ?session=id (agent-portal UI). *)
  let target = (Httpaf.Reqd.request reqd).Httpaf.Request.target in
  let open_sid =
    match query_param target "session" with
    | Some s when String.trim s <> "" -> Some (String.trim s)
    | _ -> None
  in
  let ctx =
    Printf.sprintf "user=%s branch=%s"
      (Session.username session) (Session.branch_name session)
  in
  let default_seed = Session.default_agent_seed session in
  let* sessions =
    Ai_store.list_sessions (Session.store session)
      ~owner:(Session.username session) ~limit:40 ()
  in
  let* open_sess, history =
    match open_sid with
    | None -> Lwt.return (None, [])
    | Some id ->
        let* s_opt =
          Ai_store.get_session (Session.store session)
            ~owner:(Session.username session) ~id
        in
        (match s_opt with
         | None -> Lwt.return (None, [])
         | Some s ->
             let* pairs =
               Ai_store.history_summaries (Session.store session) ~session_id:id
             in
             Lwt.return (Some s, pairs))
  in
  let seed_value =
    match open_sess with
    | Some s when String.trim s.seed <> "" -> s.seed
    | _ -> default_seed
  in
  let tools_checked =
    match open_sess with Some s -> s.tools | None -> true
  in
  let sid_display =
    match open_sess with Some s -> s.id | None -> "(new on send)"
  in
  let history_nodes =
    List.map
      (fun (role, content) ->
        let role_label =
          match role with
          | "user" -> "you"
          | "assistant" -> "agent"
          | other -> other
        in
        let shown =
          if role = "user" then agent_user_visible_text content else content
        in
        let shown =
          if String.length shown > 4000 then
            String.sub shown 0 4000 ^ "\n…(truncated)"
          else shown
        in
        HTML.div
          [ HTML.class_ "card"; HTML.style_ "margin:.4rem 0" ]
          [ HTML.b [] [txt "%s" role_label]
          ; HTML.pre
              [ HTML.style_ "white-space:pre-wrap;margin:.35rem 0 0" ]
              [txt "%s" shown]
          ])
      history
  in
  let session_list_nodes =
    if sessions = [] then
      [ HTML.p [HTML.class_ "muted"] [txt "No chats yet — send a message."] ]
    else
      List.map
        (fun (s : Ai_store.session) ->
          let active =
            match open_sess with
            | Some o when o.id = s.id -> true
            | _ -> false
          in
          let label = agent_session_label s in
          let href = "/agent?session=" ^ s.id in
          HTML.li
            [ HTML.style_
                (if active then "font-weight:700;margin:.25rem 0"
                 else "margin:.25rem 0")
            ]
            [ HTML.a [HTML.href "%s" href] [txt "%s" label]
            ; HTML.span [HTML.class_ "muted"]
                [txt "%s" (" · " ^ s.updated_at)]
            ])
        sessions
  in
  let initial_sid_js =
    match open_sess with
    | Some s -> Yojson.Safe.to_string (`String s.id)
    | None -> "null"
  in
  let tools_input =
    if tools_checked then
      HTML.input
        [ HTML.type_ "checkbox"
        ; HTML.name "tools"
        ; HTML.id "tools"
        ; HTML.value "1"
        ; HTML.checked
        ]
    else
      HTML.input
        [ HTML.type_ "checkbox"
        ; HTML.name "tools"
        ; HTML.id "tools"
        ; HTML.value "1"
        ]
  in
  let js =
    "const log=document.getElementById('log');" ^
    "const msg=document.getElementById('msg');" ^
    "const send=document.getElementById('send');" ^
    "const neu=document.getElementById('newchat');" ^
    "const sidEl=document.getElementById('sid');" ^
    "const toolsEl=document.getElementById('tools');" ^
    "const seedEl=document.getElementById('seed');" ^
    "const baseCtx=" ^ Yojson.Safe.to_string (`String ctx) ^ ";" ^
    "let sid=" ^ initial_sid_js ^ ";" ^
    "function add(role,text){" ^
    "const d=document.createElement('div');" ^
    "d.className='card';" ^
    "d.style.margin='.4rem 0';" ^
    "d.innerHTML='<b>'+role+'</b><pre style=\"white-space:pre-wrap;margin:.35rem 0 0\">'+" ^
    "String(text).replace(/[&<>]/g,c=>({\"&\":\"&amp;\",\"<\":\"&lt;\",\">\":\"&gt;\"}[c]))+" ^
    "'</pre>';log.appendChild(d);log.scrollTop=log.scrollHeight;}" ^
    "function setSid(s){sid=s||null;sidEl.textContent=sid?sid:'(new on send)';" ^
    "if(sid){const u=new URL(window.location.href);u.searchParams.set('session',sid);" ^
    "history.replaceState({},'',u.pathname+u.search);}" ^
    "else{history.replaceState({},'','/agent');}}" ^
    "setSid(sid);" ^
    "neu.onclick=()=>{window.location.href='/agent';};" ^
    "send.onclick=async()=>{" ^
    "const q=msg.value.trim();if(!q)return;" ^
    "const seed=seedEl.value;" ^
    "const tools=toolsEl.checked;" ^
    "const prompt='=== IMAGE CONTEXT ===\\n'+" ^
    "'system: Pricklypear multi-user Lisp OS image (software)\\n'+" ^
    "baseCtx+'\\n=== END CONTEXT ===\\n\\nUser:\\n'+q;" ^
    "add('you',q);msg.value='';send.disabled=true;" ^
    "try{" ^
    "const body={seed,prompt,tools,stream:false};" ^
    "if(sid)body.session_id=sid;" ^
    "const r=await fetch('/api/agent',{method:'POST'," ^
    "headers:{'content-type':'application/json'}," ^
    "body:JSON.stringify(body)});" ^
    "const j=await r.json();" ^
    "if(j.session_id)setSid(j.session_id);" ^
    "add('agent',j.text||j.error||JSON.stringify(j));" ^
    "if(j.session_id){" ^
    "const href='/agent?session='+encodeURIComponent(j.session_id);" ^
    "if(!document.querySelector('#chat-list a[href=\"'+href+'\"]'))" ^
    " setTimeout(function(){window.location.href=href;},350);}" ^
    "}catch(e){add('error',String(e));}" ^
    "finally{send.disabled=false;msg.focus();}" ^
    "};" ^
    "msg.addEventListener('keydown',e=>{if(e.key==='Enter'&&!e.shiftKey){e.preventDefault();send.click();}});"
  in
  let body =
    chrome ~session ~title:"Agent" [
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Chats"];
        HTML.p [HTML.class_ "muted"] [txt
          "Durable threads in Postgres. Open one to resume; New chat starts empty."];
        HTML.p [] [
          HTML.a [HTML.href "/agent"; HTML.class_ "btn"] [txt "New chat"];
        ];
        HTML.ul [HTML.id "chat-list"] session_list_nodes;
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Conversation"];
        HTML.p [] [
          txt "session: ";
          HTML.code [HTML.id "sid"] [txt "%s" sid_display];
        ];
        HTML.p [] [
          HTML.label [HTML.for_ "tools"] [
            tools_input;
            txt " tools (inspect / install)"
          ];
        ];
        HTML.details [] [
          HTML.summary [] [txt "seed (system persona)"];
          HTML.textarea [HTML.name "seed"; HTML.id "seed"] "%s" seed_value;
        ];
        HTML.div
          [ HTML.id "log"
          ; HTML.style_
              "max-height:28rem;overflow:auto;border:1px solid #ccc;padding:.5rem;margin:.5rem 0"
          ]
          history_nodes;
        HTML.p [] [
          HTML.label [HTML.for_ "msg"] [txt "message"];
        ];
        HTML.textarea [HTML.name "msg"; HTML.id "msg"]
          "%s" "What branch am I on?";
        HTML.p [] [
          HTML.button [HTML.type_ "button"; HTML.id "send"] [txt "Send"];
          txt " ";
          HTML.button [HTML.type_ "button"; HTML.id "newchat"] [txt "New chat"];
        ];
      ];
      HTML.script [] "%s" js;
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Write a function"];
        HTML.p [HTML.class_ "muted"] [txt
          "One-shot authoring (not chat). Same as (ai/write name spec)."];
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
    ]
  in
  html_response reqd ~session body;
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
        html_response reqd ~session body_node;
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

