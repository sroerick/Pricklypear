(* Pricklypear Image — /tree browser face over the one tree
   (one-tree.tree-browser).

   Generic store-tree navigation on the session lens: breadcrumb from
   tree/ancestors, ONE level per render (CHILDREN-BY-LEVEL, never a
   subtree walk for a page), lazy expansion through htmx partials
   (/partials/tree — one children query per expand), ranked order from
   sibling-order, kind filter, as-of-gen view (?gen=) with head-diff
   highlighting, and a node detail pane (body JSON, created/updated,
   author, rank, out-links/backlinks, edit affordance gated on
   tree/upsert auth rules).

   Complements /data (typed-row tables), /bindings (code), /sys (admin
   store-tree). The existing tree/* (scripture docs) package is
   untouched in v1. Confinement per tree-read-prims: an identity lists
   only its own garden/data prefixes + data.shared + its branch chain's
   store-tree root; private paths are not listable. *)

open Pure_html
open Http_core

module T = Tree_store
module C = Crud

let partial_path = "/partials/tree"

(* ---- confinement (tree-read-prims: identity-granted prefixes) ---- *)

(** True when [session] may LIST [path]. Grammar-scoped: data.shared is
    common; data.u/<note>/<lens> user segments must decode to the
    session identity; b.<branch> must be in the session's branch chain
    (anonymous rides system/main). Intermediate containers that hide a
    user segment (data, data.u, data.note, data.note.u, data.l) are not
    listable — private paths are not enumerable. *)
let path_visible (session : Session.t) (path : string) : bool =
  if not (T.valid_path path) then false
  else
    let uname = Session.username session in
    let user_lab = T.path_label uname in
    let chain =
      List.map T.uuid_label
        (Store.branch_chain (Session.store session)
           ~branch_id:(Session.branch_id session))
    in
    let dec k segs =
      match List.nth_opt segs k with
      | Some s -> (match T.dec_label s with Some d -> d | None -> "")
      | None -> ""
    in
    let raw k segs =
      match List.nth_opt segs k with Some s -> s | None -> ""
    in
    let segs = String.split_on_char '.' path in
    match segs with
    | [ "data" ] -> false
    | "data" :: "shared" :: _ -> true
    | "data" :: "u" :: _ -> dec 2 segs = uname && dec 2 segs <> ""
    (* garden paths store the user as a plain path_label (garden_root) *)
    | "data" :: "note" :: "u" :: _ ->
        raw 3 segs = user_lab && raw 3 segs <> ""
    | "data" :: "l" :: _lens :: _ -> dec 3 segs = uname && dec 3 segs <> ""
    | "data" :: _ -> false (* dev clutter, not a face *)
    | [ "b" ] -> chain <> []
    | "b" :: bid :: _ -> List.mem bid chain
    | _ -> false

(** Resolve the ?path= argument: empty → the identity's garden root;
    RELATIVE logical suffix (fn.<name>, pkg.<name>, …) → resolved
    through the session's branch lens chain (closest-wins, the
    effective override — tree-browser smoke 3); anything else taken as
    an absolute path. *)
let resolve_focus (session : Session.t) (raw : string) :
    string * string (* focus path, lens suffix when given *) =
  let p = String.trim raw in
  if p = "" then (T.garden_root ~user:(Session.username session), "")
  else if
    String.starts_with ~prefix:"b." p || String.starts_with ~prefix:"data." p
  then (p, "")
  else
    let chain =
      Store.branch_chain (Session.store session)
        ~branch_id:(Session.branch_id session)
    in
    let suffix = if String.contains p '.' then p else "fn." ^ p in
    match Io_lwt.Db.current_conn (Session.store session) with
    | Some conn -> (
        match T.lens_get_in_chain conn ~suffix chain with
        | Some n -> (n.path, suffix)
        | None -> (p, suffix))
    | None -> (p, suffix)

let gen_of (q : string -> string option) : int64 option =
  match q "gen" with
  | Some g when String.trim g <> "" -> (
      try Some (Int64.of_string (String.trim g)) with _ -> None)
  | _ -> None

let kind_of (q : string -> string option) : string option =
  match q "kind" with
  | Some k when String.trim k <> "" -> Some (String.trim k)
  | _ -> None

(** The htmx lazy-expand URL for a child row: /partials/tree with the
    query carried through (gen + kind filters persist across expands). *)
let expand_url ~(prefix : string) ~(gen : int64 option)
    ~(kind_filter : string option) : string =
  let b = Buffer.create 64 in
  Buffer.add_string b partial_path;
  Buffer.add_string b "?path=";
  Buffer.add_string b prefix;
  (match gen with
   | Some g ->
       Buffer.add_string b "&gen=";
       Buffer.add_string b (Int64.to_string g)
   | None -> ());
  (match kind_filter with
   | Some k ->
       Buffer.add_string b "&kind=";
       Buffer.add_string b k
   | None -> ());
  Buffer.contents b


(* ---- one-level children fragment (ONE children query per expand) ---- *)

let children_fragment (session : Session.t) ~(prefix : string)
    ~(gen : int64 option) ~(kind_filter : string option) : Pure_html.node =
  match Io_lwt.Db.current_conn (Session.store session) with
  | None -> HTML.p [HTML.class_ "muted"] [txt "(no db)"]
  | Some conn ->
      (* head (hash, rank) per child for the as-of diff highlight
         (?gen= vs head) — rank-only reorders must highlight too *)
      let head_hashes =
        match gen with
        | None -> []
        | Some _ ->
            T.list_children conn ~prefix ()
            |> List.map (fun (n : T.node) ->
                   (n.path, (n.hash, n.rank)))
      in
      let kids =
        match gen with
        | Some g -> T.list_children_as_of conn ~prefix ~gen:g ()
        | None -> T.list_children conn ~prefix ()
      in
      let kids =
        match kind_filter with
        | Some k -> List.filter (fun (n : T.node) -> n.kind = k) kids
        | None -> kids
      in
      let kids = List.filter (fun (n : T.node) -> path_visible session n.path) kids in
      if kids = [] then
        HTML.ul [HTML.class_ "tree-children"]
          [ HTML.li [HTML.class_ "muted"] [txt "(empty)"] ]
      else
        HTML.ul [HTML.class_ "tree-children"]
          (List.map
             (fun (n : T.node) ->
               let changed =
                 match gen with
                 | None -> false
                 | Some _ -> (
                     match List.assoc_opt n.path head_hashes with
                     | Some (h, r) -> h <> n.hash || r <> n.rank
                     | None -> true (* absent at head = newer than gen *))
               in
               let expand =
                 expand_url ~prefix:n.path ~gen ~kind_filter
               in
               HTML.li
                 [ HTML.class_ "%s"
                     (if changed then "tree-row tree-changed" else "tree-row") ]
                 [
                   HTML.a [HTML.href "/tree?path=%s" n.path]
                     [txt "%s" (T.node_display_title n)];
                   txt " ";
                   HTML.span [HTML.class_ "muted"] [txt "%s" n.kind];
                   (match n.rank with
                    | Some r -> HTML.span [HTML.class_ "muted"] [txt " #%s" r]
                    | None -> txt "");
                   (if n.kind <> "link" then
                      HTML.details
                        [ Hx.get "%s" expand
                        ; Hx.target "find .tree-children"
                        ; Hx.swap "innerHTML"
                        ; Hx.trigger "toggle once" ]
                        [
                          HTML.summary [] [txt "+"];
                          HTML.div [HTML.class_ "tree-children"] [];
                        ]
                    else txt "");
                 ])
             kids)

(* ---- breadcrumb (tree/ancestors, visible steps only) ---- *)

let breadcrumb ~(session : Session.t) ~(focus : string) : Pure_html.node =
  let ancestors = try T.path_ancestors focus with Invalid_argument _ -> [] in
  let links =
    ([ ("tree", "/tree") ]
    @ List.filter_map
        (fun a ->
          if path_visible session a then Some (a, "/tree?path=" ^ a) else None)
        ancestors
    @ [ (T.path_label_of focus, "/tree?path=" ^ focus) ])
  in
  let n = List.length links in
  HTML.p [HTML.class_ "crumbs"]
    (List.mapi
       (fun i (label, href) ->
         let item = HTML.a [HTML.href "%s" href] [txt "%s" label] in
         if i = n - 1 then item else HTML.span [] [item; txt " / "])
       links)

(* ---- detail pane ---- *)

let detail_pane (session : Session.t) ~(focus : string) : Pure_html.node =
  match Io_lwt.Db.current_conn (Session.store session) with
  | None -> HTML.p [HTML.class_ "muted"] [txt "(no db)"]
  | Some conn -> (
      match T.get_by_path conn ~path:focus with
      | None -> HTML.p [HTML.class_ "err"] [txt "no node at %s" focus]
      | Some n ->
          let body_json = Yojson.Safe.to_string n.body in
          let out_links = T.list_out_links conn ~path:focus ~limit:50 () in
          let backlinks = T.edges_to conn ~needle:focus ~limit:50 () in
          let uname = Session.username session in
          let can_edit =
            uname <> "anonymous"
            && (T.is_hier_kind n.kind || n.kind = "datum")
            && path_visible session focus
          in
          HTML.div [HTML.class_ "tree-detail"]
            [
              HTML.h2 [] [txt "%s" (T.node_display_title n)];
              HTML.p [HTML.class_ "muted"]
                [
                  txt "%s · author %s · rank %s" n.kind n.author
                    (match n.rank with Some r -> r | None -> "—");
                ];
              HTML.p [HTML.class_ "muted"]
                [txt "created %s · updated %s" n.created_at n.updated_at];
              HTML.pre [] [txt "%s" body_json];
              (if out_links = [] then txt ""
               else
                 HTML.div []
                   [
                     HTML.h3 [] [txt "out-links"];
                     HTML.ul []
                       (List.map
                          (fun (e : T.node) ->
                            let to_p =
                              match T.body_string_field e.body "to" with
                              | Some t -> t
                              | None -> ""
                            in
                            let status =
                              match T.get_by_path conn ~path:to_p with
                              | Some _ -> "resolved"
                              | None -> "dangling"
                            in
                            HTML.li []
                              [
                                HTML.a [HTML.href "/tree?path=%s" to_p]
                                  [txt "%s" to_p];
                                txt " (%s)" status;
                              ])
                          out_links);
                   ]);
              (if backlinks = [] then txt ""
               else
                 HTML.div []
                   [
                     HTML.h3 [] [txt "backlinks (edges)"];
                     HTML.ul []
                       (List.map
                          (fun (e : T.node) ->
                            HTML.li []
                              [
                                HTML.a [HTML.href "/tree?path=%s" e.path]
                                  [txt "%s" e.path];
                              ])
                          backlinks);
                   ]);
              (if can_edit then
                 HTML.details []
                   [
                     HTML.summary [] [txt "edit body (tree/upsert)"];
                     HTML.form
                       [ HTML.method_ `POST; HTML.action "/tree/edit" ]
                       [
                         HTML.input
                           [ HTML.type_ "hidden"
                           ; HTML.name "path"
                           ; HTML.value "%s" focus ];
                         HTML.textarea
                           [HTML.name "body"; HTML.rows 8; HTML.cols 60]
                           "%s" body_json;
                         HTML.br [];
                         HTML.button [HTML.type_ "submit"] [txt "save"];
                       ];
                   ]
               else if uname = "anonymous" then
                 HTML.p [HTML.class_ "muted"] [txt "(sign in to edit)"]
               else txt "");
            ])

(* ---- pages ---- *)

(** GET /tree?path=P [&gen=G] [&kind=K] — the browser page. *)
let tree_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let target = (Httpun.Reqd.request reqd).target in
  let q k = query_param target k in
  let raw_path = match q "path" with Some p -> p | None -> "" in
  let gen = gen_of q in
  let kind_filter = kind_of q in
  let focus, suffix = resolve_focus session raw_path in
  if not (path_visible session focus) then
    html_response reqd ~session
      (chrome ~session ~title:"tree"
         [
           HTML.p [HTML.class_ "err"]
             [txt "path not listable for this identity: %s" focus];
           HTML.p []
             [ HTML.a [HTML.href "/tree"] [txt "back to your garden root"] ];
         ])
  else
    let kind_query =
      match kind_filter with Some k -> "&kind=" ^ k | None -> ""
    in
    let node_exists =
      match Io_lwt.Db.current_conn (Session.store session) with
      | Some conn -> (match T.get_by_path conn ~path:focus with Some _ -> true | None -> false)
      | None -> false
    in
    html_response reqd ~session
      (chrome ~session ~title:"tree"
         [
           breadcrumb ~session ~focus;
           HTML.p [HTML.class_ "muted"]
             [
               match suffix with
               | "" -> txt "%s" focus
               | s -> txt "%s (lens: %s, effective)" focus s;
             ];
           (if gen = None && not node_exists then
              HTML.p [HTML.class_ "err"] [txt "no node at this path"]
            else txt "");
           HTML.form [HTML.method_ `GET; HTML.action "/tree"]
             [
               HTML.input
                 [ HTML.type_ "hidden"; HTML.name "path"; HTML.value "%s" focus ];
               HTML.input
                 [ HTML.type_ "text"; HTML.name "gen"; HTML.placeholder "gen"
                 ; HTML.size "8"
                 ; HTML.value "%s"
                     (match gen with
                      | Some g -> Int64.to_string g
                      | None -> "") ];
               HTML.input
                 [ HTML.type_ "text"; HTML.name "kind"; HTML.placeholder "kind"
                 ; HTML.size "8"
                 ; HTML.value "%s"
                     (match kind_filter with Some k -> k | None -> "") ];
               HTML.button [HTML.type_ "submit"] [txt "view as-of"];
             ];
           (match gen with
            | Some g ->
                HTML.p [HTML.class_ "muted"]
                  [
                    txt "as of gen %Ld — " g;
                    HTML.a
                      [HTML.href "/tree?path=%s%s" focus kind_query]
                      [txt "back to head"];
                  ]
            | None -> txt "");
           HTML.h2 [] [txt "children (one level, ranked)"];
           children_fragment session ~prefix:focus ~gen ~kind_filter;
           HTML.h2 [] [txt "node detail"];
           detail_pane session ~focus;
         ])

(** GET /partials/tree?path=P [&gen=G] [&kind=K] — one-level children
    fragment for htmx lazy expansion (no document chrome). One children
    query per expand (tree-browser smoke: lazy expand issues exactly one
    children query). *)
let tree_partial (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let target = (Httpun.Reqd.request reqd).target in
  let q k = query_param target k in
  let raw_path = match q "path" with Some p -> p | None -> "" in
  let gen = gen_of q in
  let kind_filter = kind_of q in
  let focus, _suffix = resolve_focus session raw_path in
  if not (path_visible session focus) then
    fragment_response reqd "<p class=\"err\">not listable</p>"
  else
    fragment_response reqd
      (Pure_html.to_string
         (children_fragment session ~prefix:focus ~gen ~kind_filter))

(** POST /tree/edit — body-JSON edit affordance honoring tree/upsert
    auth: anonymous denied; the target must be a hier/datum node in a
    visible prefix owned by the session identity. Code nodes edit
    through /binding, not here. *)
let tree_edit (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let form = parse_form_body body in
  let path =
    match List.assoc_opt "path" form with Some p -> String.trim p | None -> ""
  in
  let body_json = match List.assoc_opt "body" form with Some b -> b | None -> "" in
  let uname = Session.username session in
  if uname = "anonymous" then
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"tree"
         [HTML.p [HTML.class_ "err"] [txt "sign in to edit"]])
  else if path = "" then
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"tree" [HTML.p [HTML.class_ "err"] [txt "no path"]])
  else
    match Io_lwt.Db.current_conn (Session.store session) with
    | None ->
        html_response reqd ~session ~status:`Internal_server_error
          (chrome ~session ~title:"tree" [HTML.p [] [txt "no db"]])
    | Some conn -> (
        match T.get_by_path conn ~path with
        | None ->
            html_response reqd ~session ~status:`Not_found
              (chrome ~session ~title:"tree"
                 [HTML.p [HTML.class_ "err"] [txt "no node at %s" path]])
        | Some n ->
            let editable_kind = T.is_hier_kind n.kind || n.kind = "datum" in
            if not (editable_kind && path_visible session path) then
              html_response reqd ~session ~status:`Forbidden
                (chrome ~session ~title:"tree"
                   [
                     HTML.p [HTML.class_ "err"]
                       [txt "this node edits elsewhere (code: /binding)"];
                   ])
            else
              (* tree/upsert auth rules: anonymous denied (above); the
                 write itself runs through Crud.tree_upsert so datum
                 writes keep typed-boundary validation. *)
              let type_name =
                match n.type_name with Some t -> t | None -> n.kind
              in
              let result =
                match
                  C.tree_upsert (Session.store session) ~path ~kind:n.kind
                    ~type_name ~body_json ~author:uname
                with
                | VStr s -> s
                | _ -> "unexpected tree/upsert result"
              in
              let ok = not (String.starts_with ~prefix:"upsert_" result) in
              if ok then
                redirect_response reqd ("/tree?path=" ^ path)
              else
                html_response reqd ~session ~status:`Bad_request
                  (chrome ~session ~title:"tree"
                     [
                       HTML.p [HTML.class_ "err"] [txt "%s" result];
                       HTML.p []
                         [
                           HTML.a [HTML.href "/tree?path=%s" path]
                             [txt "back to node"];
                         ];
                     ]))
