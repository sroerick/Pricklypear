(** Branch browser UI: list, fork, checkout, promote. *)
open Pure_html
open Http_core

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

let list_branches_page (session : Session.t) (reqd : Httpun.Reqd.t)
    ?(msg = "") ?(err = "") () : unit =
  let store = Session.store session in
  let current = Session.branch_name session in
  let branches = Store.list_branches store in
  let idmap = id_to_ref_map branches in
  let rows =
    List.map
      (fun (b : Pricklypear_common.Branch.t) ->
        let ref_ = branch_ref_of b in
        let is_cur = ref_ = current || (current = "main" && ref_ = "system/main") in
        let n_fn = Store.count_functions_on_branch store ~branch_id:b.id in
        let n_rt = Store.count_routes_on_branch store ~branch_id:b.id in
        let ov =
          Store.list_overrides store ~branch_id:b.id ~parent_id:b.parent_id
        in
        let n_ov = List.length ov in
        let star = if is_cur then "★" else "" in
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
  ()

let branch_detail_page (session : Session.t) (reqd : Httpun.Reqd.t)
    ~(ref_ : string) ?(msg = "") ?(err = "") () : unit =
  let store = Session.store session in
  let current = Session.branch_name session in
  let owner, name =
    match String.split_on_char '/' ref_ with
    | [o; n] -> o, n
    | [n] -> "system", n
    | _ -> "system", ref_
  in
  let found = Store.lookup_branch_id_by_name store ~owner ~name in
  match found with
  | None ->
      list_branches_page session reqd ~err:("no such branch: " ^ ref_) ()
  | Some bid ->
      let branches = Store.list_branches store in
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
      let n_fn = Store.count_functions_on_branch store ~branch_id:bid in
      let n_rt = Store.count_routes_on_branch store ~branch_id:bid in
      let ov = Store.list_overrides store ~branch_id:bid ~parent_id in
      let own = Store.list_own_functions store ~branch_id:bid in
      let routes = Store.list_own_routes store ~branch_id:bid in
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
      let target = reqd |> Httpun.Reqd.request |> fun r -> r.Httpun.Request.target in
      let diff_name = query_param target "diff" in
      let diff_block =
        match diff_name, parent_id with
        | None, _ | Some "", _ -> []
        | Some dname, None ->
            let child =
              Store.lookup_function_on_branch store ~branch_id:bid ~name:dname
            in
            [ HTML.div [HTML.class_ "card"]
                  [ HTML.h2 [] [txt "Diff: %s (no parent)" dname]
                  ; (match child with
                     | None -> HTML.p [] [txt "(missing)"]
                     | Some (e, _, _, _) ->
                         HTML.pre []
                           [txt "%s" (Pricklypear_lisp.Reader.pp_expr e)])
                  ]
              ]
        | Some dname, Some pid ->
            let child =
              Store.lookup_function_on_branch store ~branch_id:bid ~name:dname
            in
            let parent =
              Store.lookup_function_on_branch store ~branch_id:pid ~name:dname
            in
            let show = function
              | None -> "(absent)"
              | Some (e, _, _, _) -> Pricklypear_lisp.Reader.pp_expr e
            in
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
               [ HTML.h2 [] [txt "Changes (activity)"]
               ; HTML.p [HTML.class_ "muted"]
                   [txt "Own function_defs on this branch, newest first. "]
               ; HTML.p []
                   [ HTML.a [HTML.href "%s" ("/changes?ref=" ^ ref_)]
                       [txt "Open /changes"] ]
               ; (if own_rows = [] then
                    HTML.p [HTML.class_ "muted"] [txt "(empty branch)"]
                  else
                    let sorted =
                      List.sort
                        (fun (_, _, a) (_, _, b) -> String.compare b a)
                        own
                    in
                    let change_rows =
                      List.map
                        (fun (fname, author, upd) ->
                          let kind =
                            match
                              List.find_opt
                                (fun (n, _, _) -> n = fname) ov
                            with
                            | Some (_, _, None) -> "new"
                            | Some (_, _, Some _) -> "override"
                            | None -> "own"
                          in
                          HTML.tr []
                            [ HTML.td [HTML.class_ "muted"]
                                [txt "%s" (short_ts upd)]
                            ; HTML.td []
                                [ HTML.a
                                    [HTML.href "%s" ("/binding/" ^ fname)]
                                    [txt "%s" fname] ]
                            ; HTML.td [] [txt "%s" kind]
                            ; HTML.td [] [txt "%s" author]
                            ; HTML.td []
                                [ HTML.a
                                    [ HTML.href "%s"
                                        ("/branches?ref=" ^ ref_
                                         ^ "&diff=" ^ fname) ]
                                    [txt "diff"]
                                ; txt " "
                                ; HTML.a
                                    [HTML.href "%s"
                                       (Packages.edit_href fname)]
                                    [txt "edit"]
                                ]
                            ])
                        sorted
                    in
                    HTML.table [HTML.class_ "br-table"]
                      [ HTML.thead []
                          [ HTML.tr []
                              [ HTML.th [] [txt "when"]
                              ; HTML.th [] [txt "name"]
                              ; HTML.th [] [txt "class"]
                              ; HTML.th [] [txt "author"]
                              ; HTML.th [] [txt "actions"]
                              ]
                          ]
                      ; HTML.tbody [] change_rows
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
               ; (let soft =
                    Session.promote_soft_warnings session
                      ~names:(List.map (fun (n, _, _) -> n) ov)
                  in
                  if soft = "" then
                    HTML.p [HTML.class_ "muted"]
                      [txt "%s"
                         "Soft lint: load-library spec, then open /spec"]
                  else
                    HTML.p [HTML.class_ "spec-soft"]
                      [txt "Soft lint (non-blocking): %s" soft])
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
      ()

let branches_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let target = (Httpun.Reqd.request reqd).target in
  match query_param target "ref" with
  | Some ref_ when String.trim ref_ <> "" ->
      branch_detail_page session reqd ~ref_:(String.trim ref_) ()
  | _ -> list_branches_page session reqd ()

(** GET /changes[?ref=owner/name] — branch change set (branching.change-set). *)
let changes_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let target = (Httpun.Reqd.request reqd).target in
  let ref_ =
    match query_param target "ref" with
    | Some r when String.trim r <> "" -> String.trim r
    | _ -> Session.branch_name session
  in
  let owner, name =
    match String.split_on_char '/' ref_ with
    | [o; n] -> (o, n)
    | _ -> (Session.username session, ref_)
  in
  let store = Session.store session in
  let bid_opt = Store.lookup_branch_id_by_name store ~owner ~name in
  match bid_opt with
  | None ->
      html_response reqd ~session ~status:`Not_found
        (chrome ~session ~title:"Changes"
           [ HTML.p [HTML.class_ "err"] [txt "unknown branch %s" ref_] ]);
      ()
  | Some bid ->
      let parent_id = Store.branch_parent_id store ~branch_id:bid in
      let own = Store.list_own_functions store ~branch_id:bid in
      let routes = Store.list_own_routes store ~branch_id:bid in
      let ov = Store.list_overrides store ~branch_id:bid ~parent_id in
      let sorted =
        List.sort (fun (_, _, a) (_, _, b) -> String.compare b a) own
      in
      let rows =
        List.map
          (fun (fname, author, upd) ->
            let kind =
              match List.find_opt (fun (n, _, _) -> n = fname) ov with
              | Some (_, _, None) -> "new"
              | Some (_, _, Some _) -> "override"
              | None -> "own"
            in
            HTML.tr []
              [ HTML.td [HTML.class_ "muted"] [txt "%s" (short_ts upd)]
              ; HTML.td []
                  [ HTML.a [HTML.href "%s" ("/binding/" ^ fname)]
                      [txt "%s" fname] ]
              ; HTML.td [] [txt "%s" kind]
              ; HTML.td [] [txt "%s" author]
              ; HTML.td []
                  [ HTML.a
                      [HTML.href "%s" (Packages.edit_href fname)]
                      [txt "edit"]
                  ; txt " "
                  ; HTML.a
                      [ HTML.href "%s"
                          ("/branches?ref=" ^ ref_ ^ "&diff=" ^ fname) ]
                      [txt "diff"]
                  ]
              ])
          sorted
      in
      let route_rows =
        List.map
          (fun (path, handler) ->
            HTML.tr []
              [ HTML.td [] [HTML.code [] [txt "%s" path]]
              ; HTML.td [] [txt "%s" handler]
              ; HTML.td [] [txt "route"]
              ])
          routes
      in
      let body =
        chrome ~session ~title:("Changes · " ^ ref_)
          [ HTML.p []
              [ HTML.a [HTML.href "/branches"] [txt "Branches"]
              ; txt " · "
              ; HTML.a [HTML.href "%s" ("/branches?ref=" ^ ref_)]
                  [txt "%s" ref_]
              ; txt " · change set"
              ]
          ; HTML.div [HTML.class_ "card"]
              [ HTML.h2 [] [txt "Functions"]
              ; (if rows = [] then
                   HTML.p [HTML.class_ "muted"] [txt "(no own functions)"]
                 else
                   HTML.table [HTML.class_ "br-table"]
                     [ HTML.thead []
                         [ HTML.tr []
                             [ HTML.th [] [txt "when"]
                             ; HTML.th [] [txt "name"]
                             ; HTML.th [] [txt "class"]
                             ; HTML.th [] [txt "author"]
                             ; HTML.th [] [txt "actions"]
                             ]
                         ]
                     ; HTML.tbody [] rows
                     ])
              ]
          ; HTML.div [HTML.class_ "card"]
              [ HTML.h2 [] [txt "Own routes"]
              ; (if route_rows = [] then
                   HTML.p [HTML.class_ "muted"] [txt "(none)"]
                 else
                   HTML.table [HTML.class_ "br-table"]
                     [ HTML.thead []
                         [ HTML.tr []
                             [ HTML.th [] [txt "route"]
                             ; HTML.th [] [txt "handler"]
                             ; HTML.th [] [txt "class"]
                             ]
                         ]
                     ; HTML.tbody [] route_rows
                     ])
              ]
          ]
      in
      html_response reqd ~session body;
      ()

let branches_checkout (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let ref_ = Option.value (form_field body "ref") ~default:"" |> String.trim in
  if ref_ = "" then list_branches_page session reqd ~err:"ref required" ()
  else
    let r = Session.checkout_ref session ref_ in
    match r with
    | Error e -> list_branches_page session reqd ~err:e ()
    | Ok () ->
        redirect_response reqd ("/branches?ref=" ^ ref_);
        ()

let branches_fork (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let from_ = Option.value (form_field body "from") ~default:"system/main" in
  let name = Option.value (form_field body "name") ~default:"" |> String.trim in
  if name = "" then list_branches_page session reqd ~err:"name required" ()
  else
    let owner = Session.username session in
    let child_ref =
      if String.contains name '/' then name else owner ^ "/" ^ name
    in
    let r = Session.fork_ref session ~parent_ref:from_ ~child_ref in
    match r with
    | Error e -> list_branches_page session reqd ~err:e ()
    | Ok _ ->
        list_branches_page session reqd
          ~msg:("forked " ^ child_ref ^ " from " ^ from_) ()

let branches_unoverride (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  let body = read_body reqd in
  let ref_ = Option.value (form_field body "ref") ~default:"" |> String.trim in
  let name = Option.value (form_field body "name") ~default:"" |> String.trim in
  (* Checkout target branch first if needed so delete hits the right id. *)
  let _ =
    if ref_ <> "" && Session.branch_name session <> ref_ then
      Session.checkout_ref session ref_
    else (Ok ())
  in
  let r = Session.unoverride session name in
  match r with
  | Error e ->
      branch_detail_page session reqd ~ref_ ~err:e ()
  | Ok true ->
      branch_detail_page session reqd ~ref_
        ~msg:("unoverrode " ^ name ^ " — now inherits parent") ()
  | Ok false ->
      branch_detail_page session reqd ~ref_
        ~err:("no pin named " ^ name ^ " on this branch") ()

let branches_promote (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let ref_ = Option.value (form_field body "ref") ~default:"" |> String.trim in
  let form = parse_form_body body in
  let names =
    List.filter_map
      (fun (k, v) -> if k = "name" && String.trim v <> "" then Some v else None)
      form
  in
  let r = Session.promote_to_main session ~source_ref:ref_ ~names in
  match r with
  | Error e -> branch_detail_page session reqd ~ref_ ~err:e ()
  | Ok (n, soft) ->
      let msg =
        if soft = "" then
          Printf.sprintf "promoted %d function(s) → system/main" n
        else
          Printf.sprintf
            "promoted %d function(s) → system/main · soft: %s" n soft
      in
      branch_detail_page session reqd ~ref_ ~msg ()

