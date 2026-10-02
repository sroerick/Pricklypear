(** Pricklypear Image — /types + /data CRUD browser pages. *)
open Pure_html
open Http_core

let list_types_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let types = Store.list_types (Session.store session) in
  let rows =
    List.map
      (fun (name, fields) ->
        let field_list = String.concat ", " fields in
        HTML.tr [] [
          HTML.td [] [
            HTML.a [HTML.href "/data/%s" name] [txt "%s" name];
          ];
          HTML.td [] [txt "%s" field_list];
          HTML.td [] [
            HTML.a [HTML.href "/data/%s" name] [txt "rows"];
          ];
        ])
      types
  in
  let body =
    chrome ~session ~title:("Data types (" ^ Session.branch_name session ^ ")") [
      HTML.p [HTML.class_ "meta"] [
        txt "Declared shapes for Data rows. Browse/edit instances at ";
        HTML.a [HTML.href "/data"] [txt "/data"];
        txt " or via (create 'type \"{...}\") in the REPL. ";
        txt "(deftype …) declares on the branch you own — system/* is admin-only.";
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
                 HTML.th [] [txt ""];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd ~session body;
  ()

(* ---- data browser (image.data-browser) ---- *)

let data_type_name_ok (s : string) : bool =
  let n = String.length s in
  n > 0 && n <= 128 &&
  (try
     for i = 0 to n - 1 do
       match s.[i] with
       | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '/' | '.' -> ()
       | _ -> raise Exit
     done;
     true
   with Exit -> false)

let json_object_assoc (s : string) : ((string * Yojson.Safe.t) list, string) result =
  try
    match Yojson.Safe.from_string s with
    | `Assoc kvs -> Ok kvs
    | _ -> Error "fields JSON must be an object"
  with exn -> Error ("invalid JSON: " ^ Printexc.to_string exn)

let fields_assoc_of_json (s : string) : (string * Yojson.Safe.t) list =
  match json_object_assoc s with
  | Ok kvs -> kvs
  | Error _ -> []

let string_of_json_field (v : Yojson.Safe.t) : string =
  match v with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Intlit s -> s
  | `Float f -> string_of_float f
  | `Bool b -> if b then "true" else "false"
  | `Null -> ""
  | other -> Yojson.Safe.to_string other

let truncate_cell (s : string) ~(max_len : int) : string =
  if String.length s <= max_len then s
  else String.sub s 0 max_len ^ "…"

let short_id (id : string) : string =
  if String.length id <= 8 then id else String.sub id 0 8

(** Build fields JSON for create/update.
    Prefer non-empty fields_json (must be object). Else declared form
    fields as string values. On update, [base] preserves unknown keys. *)
let build_fields_json
    ~(declared : string list)
    ~(form : (string * string) list)
    ~(fields_json_opt : string option)
    ~(base : (string * Yojson.Safe.t) list)
    : (string, string) result =
  match fields_json_opt with
  | Some raw when String.trim raw <> "" ->
      (match json_object_assoc (String.trim raw) with
       | Ok kvs -> Ok (Yojson.Safe.to_string (`Assoc kvs))
       | Error e -> Error e)
  | _ ->
      let form_tbl = Hashtbl.create 16 in
      List.iter (fun (k, v) -> Hashtbl.replace form_tbl k v) form;
      let declared_keys =
        if declared = [] then
          (* no type_defs: take every form key except control fields *)
          Hashtbl.fold
            (fun k _ acc ->
              if k = "fields_json" || k = "action" then acc else k :: acc)
            form_tbl []
          |> List.sort_uniq String.compare
        else declared
      in
      let updated =
        List.fold_left
          (fun acc key ->
            match Hashtbl.find_opt form_tbl key with
            | None -> acc
            | Some v ->
                (key, `String v) :: List.filter (fun (k, _) -> k <> key) acc)
          base declared_keys
      in
      Ok (Yojson.Safe.to_string (`Assoc updated))

let shared_data_banner () =
  HTML.p [HTML.class_ "meta"] [
    txt "You see your own rows, shared seed data (bible, strongs), and any member spaces. ";
    txt "Data is not versioned by branch. ";
    txt "(data-scope) reports your effective scope. ";
    HTML.a [HTML.href "/types"] [txt "Type defs"];
    txt " · ";
    HTML.a [HTML.href "/data"] [txt "Your data"];
  ]

let data_index_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let store = Session.store session in
  let types = Store.list_types store in
  let orphan_names = Crud.list_data_type_names store in
  let declared = List.map fst types in
  let declared_set =
    List.fold_left (fun acc n -> n :: acc) [] declared
  in
  let orphans =
    List.filter (fun n -> not (List.mem n declared_set)) orphan_names
  in
  (* One grouped scan instead of a per-type COUNT (log/issues /data 49s). *)
  let counts = Crud.counts_by_type store in
  let count_of name =
    match List.assoc_opt name counts with Some n -> n | None -> 0
  in
  let rows =
    List.map
      (fun (name, fields) ->
        let n = count_of name in
        (HTML.tr [] [
             HTML.td [] [
               HTML.a [HTML.href "/data/%s" name] [txt "%s" name];
             ];
             HTML.td [] [txt "%s" (String.concat ", " fields)];
             HTML.td [] [txt "%d" n];
             HTML.td [] [
               HTML.a [HTML.href "/data/%s" name] [txt "browse"];
               txt " · ";
               HTML.a [HTML.href "/data/%s/new" name] [txt "new"];
             ];
           ]))
      types
  in
  let orphan_rows =
    List.map
      (fun name ->
        let n = count_of name in
        (HTML.tr [] [
             HTML.td [] [
               HTML.a [HTML.href "/data/%s" name] [txt "%s" name];
             ];
             HTML.td [] [txt "(no type_def)"];
             HTML.td [] [txt "%d" n];
             HTML.td [] [
               HTML.a [HTML.href "/data/%s" name] [txt "browse"];
             ];
           ]))
      orphans
  in
  let body =
    chrome ~session ~title:"Data" [
      shared_data_banner ();
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Types with rows"];
        (match rows @ orphan_rows with
         | [] ->
             HTML.p [] [
               txt "(no data_rows yet; declare a type via (deftype …) on your own branch — system/* is admin-only — or ";
               HTML.a [HTML.href "/types"] [txt "/types"];
               txt ")";
             ]
         | all ->
             HTML.table [] [
               HTML.thead [] [
                 HTML.tr [] [
                   HTML.th [] [txt "type"];
                   HTML.th [] [txt "fields"];
                   HTML.th [] [txt "rows"];
                   HTML.th [] [txt ""];
                 ];
               ];
               HTML.tbody [] all;
             ]);
      ];
    ]
  in
  html_response reqd ~session body;
  ()

let data_list_page (session : Session.t) (type_name : string)
    (reqd : Httpun.Reqd.t) : unit =
  if not (data_type_name_ok type_name) then begin
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"Bad type" [
         HTML.p [HTML.class_ "err"] [txt "invalid type name"];
         HTML.p [] [HTML.a [HTML.href "/data"] [txt "Back to /data"]];
       ]);
    ()
  end else
    let store = Session.store session in
    let target = (Httpun.Reqd.request reqd).Httpun.Request.target in
    let limit =
      match query_param target "limit" with
      | Some s ->
          (try max 1 (min 200 (int_of_string s)) with _ -> 50)
      | None -> 50
    in
    let offset =
      match query_param target "offset" with
      | Some s -> (try max 0 (int_of_string s) with _ -> 0)
      | None -> 0
    in
    let types = Store.list_types store in
    let declared =
      match List.assoc_opt type_name types with
      | Some fs -> fs
      | None -> []
    in
    let total = Crud.count_by_type store ~type_name in
    let rows = Crud.list_page store ~type_name ~limit ~offset () in
    let col_fields =
      if declared = [] then
        (* infer columns from first row keys *)
        match rows with
        | r :: _ -> List.map fst (fields_assoc_of_json r.Crud.fields_json)
        | [] -> []
      else declared
    in
    let header_cells =
      HTML.th [] [txt "id"]
      :: List.map (fun f -> HTML.th [] [txt "%s" f]) col_fields
    in
    let data_rows =
      List.map
        (fun (r : Crud.row_record) ->
          let assoc = fields_assoc_of_json r.Crud.fields_json in
          let cells =
            List.map
              (fun f ->
                let v =
                  match List.assoc_opt f assoc with
                  | Some j -> string_of_json_field j
                  | None -> ""
                in
                HTML.td [] [txt "%s" (truncate_cell v ~max_len:48)])
              col_fields
          in
          HTML.tr []
            (HTML.td [] [
               HTML.a
                 [HTML.href "/data/%s/%s" type_name r.Crud.id]
                 [txt "%s" (short_id r.Crud.id)];
             ]
             :: cells))
        rows
    in
    let prev_off = max 0 (offset - limit) in
    let next_off = offset + limit in
    let has_prev = offset > 0 in
    let has_next = next_off < total in
    let body =
      chrome ~session ~title:("Data · " ^ type_name) [
        shared_data_banner ();
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "%s" type_name];
          HTML.p [HTML.class_ "meta"] [
            txt "%d row(s) · showing %d–%d · limit %d"
              total
              (if total = 0 then 0 else offset + 1)
              (min total (offset + List.length rows))
              limit;
          ];
          HTML.p [] [
            HTML.a [HTML.href "/data/%s/new" type_name] [txt "New row"];
            txt " · ";
            HTML.a [HTML.href "/data"] [txt "All types"];
            txt " · ";
            HTML.a [HTML.href "/types"] [txt "Type defs"];
          ];
          (match data_rows with
           | [] ->
               HTML.p [] [
                 txt "(no rows yet) ";
                 HTML.a [HTML.href "/data/%s/new" type_name] [txt "Create one"];
               ]
           | _ ->
               HTML.table [] [
                 HTML.thead [] [HTML.tr [] header_cells];
                 HTML.tbody [] data_rows;
               ]);
          HTML.p []
            ((if has_prev then
                [ HTML.a
                    [ HTML.href "/data/%s?limit=%d&offset=%d"
                        type_name limit prev_off ]
                    [txt "← Prev"] ]
              else [])
             @ (if has_prev && has_next then [txt " · "] else [])
             @ (if has_next then
                  [ HTML.a
                      [ HTML.href "/data/%s?limit=%d&offset=%d"
                          type_name limit next_off ]
                      [txt "Next →"] ]
                else []));
        ];
      ]
    in
    html_response reqd ~session body;
    ()

let field_inputs ~(declared : string list)
    ~(values : (string * string) list) : Pure_html.node list =
  let val_of k = Option.value (List.assoc_opt k values) ~default:"" in
  if declared = [] then
    [ HTML.p [HTML.class_ "meta"] [
        txt "No declared fields — use raw JSON below (or deftype first).";
      ] ]
  else
    List.map
      (fun f ->
        HTML.p [] [
          HTML.label [] [txt "%s " f];
          HTML.input
            [ HTML.type_ "text"
            ; HTML.name "%s" f
            ; HTML.value "%s" (val_of f)
            ];
        ])
      declared

let data_new_page (session : Session.t) (type_name : string)
    (reqd : Httpun.Reqd.t) : unit =
  if not (data_type_name_ok type_name) then begin
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"Bad type" [
         HTML.p [HTML.class_ "err"] [txt "invalid type name"];
       ]);
    ()
  end else
    let types = Store.list_types (Session.store session) in
    let declared =
      match List.assoc_opt type_name types with Some fs -> fs | None -> []
    in
    let body =
      chrome ~session ~title:("New · " ^ type_name) [
        shared_data_banner ();
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "Create %s" type_name];
          HTML.form
            [ HTML.method_ `POST; HTML.action "/data/%s" type_name ]
            (field_inputs ~declared ~values:[]
             @ [ HTML.p [] [
                   HTML.label [] [txt "fields_json (optional; wins if set) "];
                   HTML.textarea
                     [ HTML.name "fields_json" ]
                     "%s" "";
                 ]
               ; HTML.button [HTML.type_ "submit"] [txt "Create"]
               ; HTML.p [] [
                   HTML.a [HTML.href "/data/%s" type_name] [txt "Cancel"];
                 ]
               ]);
        ];
      ]
    in
    html_response reqd ~session body;
    ()

let data_create (session : Session.t) (type_name : string)
    (reqd : Httpun.Reqd.t) : unit =
  if not (data_type_name_ok type_name) then begin
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"Bad type" [
         HTML.p [HTML.class_ "err"] [txt "invalid type name"];
       ]);
    ()
  end else
    let store = Session.store session in
    let body = read_body reqd in
    let form = parse_form_body body in
    let types = Store.list_types store in
    let declared =
      match List.assoc_opt type_name types with Some fs -> fs | None -> []
    in
    let fields_json_opt = List.assoc_opt "fields_json" form in
    match
      build_fields_json ~declared ~form ~fields_json_opt ~base:[]
    with
    | Error e ->
        html_response reqd ~session ~status:`Bad_request
          (chrome ~session ~title:"Create failed" [
             HTML.p [HTML.class_ "err"] [txt "%s" e];
             HTML.p [] [
               HTML.a [HTML.href "/data/%s/new" type_name] [txt "Back"];
             ];
           ]);
        ()
    | Ok fields_json ->
        let author = Session.username session in
        (match
           Crud.create_outcome store ~type_name ~fields_json ~author
         with
         | Crud.W_ok id ->
             redirect_response reqd
               (Printf.sprintf "/data/%s/%s" type_name id);
             ()
         | Crud.W_rejected msg ->
             html_response reqd ~session ~status:`Internal_server_error
               (chrome ~session ~title:"Create failed" [
                  HTML.p [HTML.class_ "err"] [txt "%s" msg];
                  HTML.p [] [
                    HTML.a [HTML.href "/data/%s/new" type_name] [txt "Back"];
                  ];
                ]);
             ()
         | Crud.W_no_row ->
             html_response reqd ~session ~status:`Internal_server_error
               (chrome ~session ~title:"Create failed" [
                  HTML.p [HTML.class_ "err"] [txt "create failed"];
                ]);
             ())

let data_detail_page (session : Session.t) (type_name : string)
    (id : string) (reqd : Httpun.Reqd.t) : unit =
  if not (data_type_name_ok type_name) then begin
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"Bad type" [
         HTML.p [HTML.class_ "err"] [txt "invalid type name"];
       ]);
    ()
  end else
    let store = Session.store session in
    let row_opt = Crud.get_row store ~type_name ~id in
    match row_opt with
    | None ->
        html_response reqd ~session ~status:`Not_found
          (chrome ~session ~title:"Row not found" [
             HTML.p [HTML.class_ "err"] [txt "row not found"];
             HTML.p [] [
               HTML.a [HTML.href "/data/%s" type_name] [txt "Back to list"];
             ];
           ]);
        ()
    | Some row ->
        let types = Store.list_types store in
        let declared =
          match List.assoc_opt type_name types with
          | Some fs -> fs
          | None -> []
        in
        let assoc = fields_assoc_of_json row.Crud.fields_json in
        let declared_set = declared in
        let extra_keys =
          List.filter_map
            (fun (k, _) ->
              if List.mem k declared_set then None else Some k)
            assoc
        in
        let values =
          List.map (fun (k, v) -> (k, string_of_json_field v)) assoc
        in
        (* Include extra keys so edit form can preserve/update unknowns. *)
        let form_fields =
          declared
          @ List.map (fun k -> k) extra_keys
        in
        let meta_bits =
          [ HTML.p [HTML.class_ "meta"] [txt "id: %s" row.Crud.id] ]
          (* Space compartment: the row's home path makes which
             compartment it lives in (per-user / shared seed / member
             space) visible at a glance (shared-data-spaces S3). *)
          @ (match Crud.node_path store ~type_name ~id with
             | Pricklypear_lisp.Ast.VStr p ->
                 [HTML.p [HTML.class_ "meta"] [txt "home: %s" p]]
             | _ -> [])
          @ (match row.Crud.author with
             | Some a -> [HTML.p [HTML.class_ "meta"] [txt "author: %s" a]]
             | None -> [])
          @ (match row.Crud.created_at with
             | Some t -> [HTML.p [HTML.class_ "meta"] [txt "created: %s" t]]
             | None -> [])
          @ (match row.Crud.updated_at with
             | Some t -> [HTML.p [HTML.class_ "meta"] [txt "updated: %s" t]]
             | None -> [])
        in
        let pretty =
          try Yojson.Safe.pretty_to_string
                (Yojson.Safe.from_string row.Crud.fields_json)
          with _ -> row.Crud.fields_json
        in
        let body =
          chrome ~session
            ~title:(Printf.sprintf "Data · %s · %s" type_name (short_id id))
            [ shared_data_banner ()
            ; HTML.div [HTML.class_ "card"] (
                [ HTML.h2 [] [txt "%s" type_name] ]
                @ meta_bits
                @ [ HTML.p [] [
                      HTML.a [HTML.href "/data/%s" type_name] [txt "← List"];
                      txt " · ";
                      HTML.a [HTML.href "/data"] [txt "All data"];
                    ]
                  ; HTML.h3 [] [txt "Fields"]
                  ; HTML.pre [] [txt "%s" pretty]
                  ]
              )
            ; HTML.div [HTML.class_ "card"] [
                HTML.h3 [] [txt "Edit"];
                HTML.form
                  [ HTML.method_ `POST
                  ; HTML.action "/data/%s/%s" type_name id
                  ]
                  (field_inputs ~declared:form_fields ~values
                   @ [ HTML.p [] [
                         HTML.label []
                           [txt "fields_json (optional; full replace if set) "];
                         HTML.textarea
                           [ HTML.name "fields_json" ]
                           "%s" "";
                       ]
                     ; HTML.button [HTML.type_ "submit"] [txt "Save"]
                     ]);
              ]
            ; HTML.div [HTML.class_ "card"]
                ([ HTML.h3 [] [txt "History"] ]
                 @ (let hist =
                      Crud.data_history_json store
                        ~viewer:(Session.username session) ~type_name ~id
                    in
                    match hist with
                    | [] ->
                        [ HTML.p [HTML.class_ "meta"]
                            [txt "no version history visible"] ]
                    | _ ->
                        List.map
                          (fun (v : Yojson.Safe.t) ->
                            let str k =
                              match Yojson.Safe.Util.member k v with
                              | `String s -> s
                              | _ -> "?"
                            in
                            let deleted =
                              match Yojson.Safe.Util.member "delete" v with
                              | `Bool b -> b
                              | _ -> false
                            in
                            let body_txt =
                              try Yojson.Safe.pretty_to_string
                                    (Yojson.Safe.Util.member "body" v)
                              with _ -> ""
                            in
                            HTML.div []
                              [ HTML.p [HTML.class_ "meta"]
                                  [txt "gen %s · %s · %s%s"
                                     (str "gen") (str "author")
                                     (str "created_at")
                                     (if deleted then " · deleted" else "")]
                              ; HTML.pre [] [txt "%s" body_txt]
                              ])
                          hist))
            ; HTML.div [HTML.class_ "card"] [
                HTML.h3 [] [txt "Delete"];
                HTML.form
                  [ HTML.method_ `POST
                  ; HTML.action "/data/%s/%s/delete" type_name id
                  ]
                  [ HTML.button [HTML.type_ "submit"] [txt "Delete row"] ];
              ]
            ]
        in
        html_response reqd ~session body;
        ()

let data_update (session : Session.t) (type_name : string) (id : string)
    (reqd : Httpun.Reqd.t) : unit =
  if not (data_type_name_ok type_name) then begin
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"Bad type" [
         HTML.p [HTML.class_ "err"] [txt "invalid type name"];
       ]);
    ()
  end else
    let store = Session.store session in
    let body = read_body reqd in
    let form = parse_form_body body in
    let row_opt = Crud.get_row store ~type_name ~id in
    match row_opt with
    | None ->
        html_response reqd ~session ~status:`Not_found
          (chrome ~session ~title:"Row not found" [
             HTML.p [HTML.class_ "err"] [txt "row not found"];
           ]);
        ()
    | Some row ->
        let types = Store.list_types store in
        let declared =
          match List.assoc_opt type_name types with
          | Some fs -> fs
          | None -> []
        in
        let base = fields_assoc_of_json row.Crud.fields_json in
        let extra_keys =
          List.filter_map
            (fun (k, _) -> if List.mem k declared then None else Some k)
            base
        in
        let form_fields = declared @ extra_keys in
        let fields_json_opt = List.assoc_opt "fields_json" form in
        (match
           build_fields_json ~declared:form_fields ~form ~fields_json_opt
             ~base
         with
         | Error e ->
             html_response reqd ~session ~status:`Bad_request
               (chrome ~session ~title:"Update failed" [
                  HTML.p [HTML.class_ "err"] [txt "%s" e];
                  HTML.p [] [
                    HTML.a
                      [HTML.href "/data/%s/%s" type_name id]
                      [txt "Back"];
                  ];
                ]);
             ()
         | Ok fields_json ->
             (match
                Crud.update_outcome store ~type_name ~id ~fields_json
                  ~caller:(Session.username session)
              with
              | Crud.W_ok _ ->
                  redirect_response reqd
                    (Printf.sprintf "/data/%s/%s" type_name id);
                  ()
              | Crud.W_rejected msg ->
                  (* i-1788670909: a typed-boundary rejection is a client
                     error, not a missing row and not a success — the old
                     shape-sniffing arm folded it into the VStr success
                     redirect. *)
                  html_response reqd ~session ~status:`Bad_request
                    (chrome ~session ~title:"Update failed" [
                       HTML.p [HTML.class_ "err"] [txt "%s" msg];
                       HTML.p [] [
                         HTML.a
                           [HTML.href "/data/%s/%s" type_name id]
                           [txt "Back"];
                       ];
                     ]);
                  ()
              | Crud.W_no_row ->
                  html_response reqd ~session ~status:`Not_found
                    (chrome ~session ~title:"Update failed" [
                       HTML.p [HTML.class_ "err"] [txt "row not found"];
                     ]);
                  ()))

let data_delete (session : Session.t) (type_name : string) (id : string)
    (reqd : Httpun.Reqd.t) : unit =
  if not (data_type_name_ok type_name) then begin
    html_response reqd ~session ~status:`Bad_request
      (chrome ~session ~title:"Bad type" [
         HTML.p [HTML.class_ "err"] [txt "invalid type name"];
       ]);
    ()
  end else
    let store = Session.store session in
    let v =
      Crud.delete_guarded store ~type_name ~id ~caller:(Session.username session)
    in
    match v with
    | Pricklypear_lisp.Ast.VStr _ ->
        redirect_response reqd (Printf.sprintf "/data/%s" type_name);
        ()
    | _ ->
        html_response reqd ~session ~status:`Not_found
          (chrome ~session ~title:"Delete failed" [
             HTML.p [HTML.class_ "err"] [txt "row not found"];
             HTML.p [] [
               HTML.a [HTML.href "/data/%s" type_name] [txt "Back to list"];
             ];
           ]);
        ()


(* ---- page timing + route step ledger (image.page-timing,
        image.budget-ledger) ---- *)

let timing_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  (* Budget-ledger probe: each page load probes up to [probe_batch] of
     the session's own GET routes that have no samples yet, wrapped in
     measure so their samples land in the SAME ring as live traffic and
     the sweep completes progressively across page loads (≤15 × ≤0.25s
     CPU each — bounded). Admin-only page; probes really run handlers;
     the page body carries the side-effects note.

     2026-10 security pass: the "Admin-only" above was a comment, not
     a gate (/db enforces, this didn't). Route names + aggregate render
     costs across the whole image are cross-tenant information —
     enforce the admin check like db_panel_page does. *)
  if not (Session.is_admin session) then begin
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"Forbidden" [
         HTML.p [] [ txt "timing panel is admin-only" ]])
  end
  else begin
  let probe_batch = 15 in
  let aggs0 = Timing.aggregates () in
  let sampled0 = Hashtbl.create 64 in
  List.iter (fun (a : Timing.agg) -> Hashtbl.replace sampled0 a.route ()) aggs0;
  let all_routes = Session.routes session in
  let unprobed =
    Hashtbl.fold
      (fun r (h, _vis) acc ->
        if r = "/timing" || Hashtbl.mem sampled0 r then acc
        else (r, h) :: acc)
      all_routes []
  in
  let unprobed = List.sort compare unprobed in
  List.iteri
    (fun i (r, h) ->
      if i < probe_batch then
        let request : Session.request =
          { method_ = `GET; path = r; params = []; body = ""; headers = [] }
        in
        ignore (Timing.measure ~route:h (fun () ->
            Session.call_handler session ~request h)))
    unprobed;
  let aggs = Timing.aggregates () in
  let budget_limit = Pricklypear_lisp.Budget.default_limit in
  let share_str (a : Timing.agg) : string =
    if a.steps <= 0. then "—"
    else
      let pct = 100. *. a.steps /. float_of_int budget_limit in
      if pct >= 25. then Printf.sprintf "%.0f%% ⚠" pct
      else Printf.sprintf "%.0f%%" pct
  in
  let db_share (a : Timing.agg) : string =
    if a.avg_ms > 0.0001 then
      Printf.sprintf "%.0f%%" (100. *. a.db_ms /. a.avg_ms)
    else "—"
  in
  (* image.db-panel: which db prims the endpoint ran (top 8 by calls). *)
  let prim_breakdown (a : Timing.agg) : Pure_html.node =
    match List.filteri (fun i _ -> i < 8) a.prim_db_agg with
    | [] -> txt "—"
    | prims ->
        let spans =
          List.map
            (fun (k, calls, ms) ->
              HTML.span [ HTML.class_ "mono" ]
                [ txt "%s×%d(%.0fms) " k calls ms ])
            prims
        in
        HTML.span [] spans
  in
  let step_cell (a : Timing.agg) =
    if a.steps > 0. then txt "%.0f" a.steps else txt "—"
  in
  let sampled_rows =
    List.map
      (fun (a : Timing.agg) ->
        HTML.tr [] [
          HTML.td [] [txt "%s" a.route];
          HTML.td [] [txt "%s" (if a.max_ms > 0. then "" else "probe")];
          HTML.td [] [txt "%d" a.count];
          HTML.td [] [step_cell a];
          HTML.td [] [txt "%s" (share_str a)];
          HTML.td [] [txt "%.1f" a.avg_ms];
          HTML.td [] [txt "%.1f" a.max_ms];
          HTML.td [] [txt "%.1f" a.db_ms];
          HTML.td [] [txt "%.1f" a.db_hits];
          HTML.td [] [txt "%s" (db_share a)];
          HTML.td [] [prim_breakdown a];
        ])
      aggs
  in
  (* Route ledger: every user route visible to this session, unprobed
     ones last — the FULL endpoint surface, not just routes traffic
     happened to hit (image.budget-ledger). *)
  let sampled_names = Hashtbl.create 64 in
  List.iter (fun (a : Timing.agg) -> Hashtbl.replace sampled_names a.route ()) aggs;
  let probed_this_load = Hashtbl.create 16 in
  List.iter
    (fun (a : Timing.agg) ->
      if not (Hashtbl.mem sampled0 a.route) then
        Hashtbl.replace probed_this_load a.route ())
    aggs;
  let ledger_rows =
    Hashtbl.fold
      (fun r (h, vis) acc ->
        if Hashtbl.mem sampled_names r then acc
        else
          HTML.tr [] [
            HTML.td [] [txt "%s" r];
            HTML.td [] [txt "%s" h];
            HTML.td [] [txt "%s" vis];
            HTML.td [] [txt "%s" (if Hashtbl.mem probed_this_load r then "probed now" else "—")];
            HTML.td [] [txt "—"];
            HTML.td [] [txt "—"];
          ]
          :: acc)
      all_routes []
  in
  let body =
    chrome ~session ~title:"Page timing / budget ledger" [
      HTML.p [HTML.class_ "meta"] [
        txt "Per user route: eval steps + wall clock. Steps come from the handler's own eval budget (limit %d); budget-share flags ≥25%%." budget_limit;
        txt "  DB share = avg DB wall time / avg total.";
        txt "  Each load of this page probes ≤%d unprobed GET routes for real (side effects possible) — a full sweep accumulates across reloads."
          probe_batch;
      ];
      HTML.h2 [] [txt "Sampled routes (slowest-first by max ms)"];
      (match aggs with
       | [] ->
           HTML.p [HTML.class_ "err"] [
             txt "No route timings measured yet. Reload — the ledger probe starts sampling now.";
           ]
       | _ ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "route"];
                 HTML.th [] [txt "note"];
                 HTML.th [] [txt "samples"];
                 HTML.th [] [txt "steps"];
                 HTML.th [] [txt "budget"];
                 HTML.th [] [txt "avg ms"];
                 HTML.th [] [txt "max ms"];
                 HTML.th [] [txt "db ms"];
                 HTML.th [] [txt "db hits"];
                 HTML.th [] [txt "db share"];
                 HTML.th [] [txt "db prims (calls, avg ms)";
                 ];
               ]
             ];
             HTML.tbody [] sampled_rows;
           ]);
      HTML.h2 [] [txt "Unprobed routes"];
      (match ledger_rows with
       | [] ->
           HTML.p [HTML.class_ "meta"] [
             txt "Every visible user route has samples.";
           ]
       | rows ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "route"];
                 HTML.th [] [txt "handler"];
                 HTML.th [] [txt "visibility"];
                 HTML.th [] [txt "probe"];
                 HTML.th [] [txt ""];
                 HTML.th [] [txt ""];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd ~session body;
  ()
  end

(* ---- db panel (image.db-panel) -------------------------------------
   Global Postgres statistics for the operator. Admin-only. All SQL is
   FIXED — no user input reaches these queries; this is a Postgres
   WINDOW, not a query console. *)

(* Pgx value readers: null-safe, for the fixed-shape panel queries.
   (Pgx.row is a LIST of values in this pgx.) *)
let cell_s (row : Pgx.row) (i : int) : string =
  (try Some (List.nth row i) with _ -> None)
  |> function None -> "" | Some v ->
    (match Pgx.Value.to_string v with Some s -> s | None -> "")

let cell_f (row : Pgx.row) (i : int) : float =
  (try Some (List.nth row i) with _ -> None)
  |> function None -> 0. | Some v ->
    (match Pgx.Value.to_string v with
     | Some s -> (match float_of_string_opt s with Some f -> f | None -> 0.)
     | None -> 0.)

let cell_i (row : Pgx.row) (i : int) : int =
  int_of_float (cell_f row i)


let fmt_bytes (b : float) : string =
  if b >= 1073741824. then Printf.sprintf "%.1f GB" (b /. 1073741824.)
  else if b >= 1048576. then Printf.sprintf "%.1f MB" (b /. 1048576.)
  else if b >= 1024. then Printf.sprintf "%.1f KB" (b /. 1024.)
  else Printf.sprintf "%d B" (int_of_float b)

(* One fixed-SQL panel: named rows selected, 0-safe. *)
let db_panel_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  (* /db is the one face that exposes Postgres internals — admin only. *)
  if not (Session.is_admin session) then
    html_response reqd ~session ~status:`Forbidden
      (chrome ~session ~title:"Forbidden" [
         HTML.p [] [ txt "db panel is admin-only" ] ])
  else begin
    let store = Session.store session in
    (* Read-only catalog queries over the session's connection.
       Swallows exceptions 0-safe — a panel row beats a dead page. *)
    let exec_fixed sql =
      match Store.current_conn store with
      | Some conn -> (try Store_db.exec conn sql with _ -> [])
      | None -> []
    in
    (* ---- pg_stat_database (current database) ---- *)
    let dbstats = exec_fixed
        {sql| SELECT datname,
                     blks_hit, blks_read,
                     xact_commit, xact_rollback,
                     deadlocks, conflicts, temp_bytes,
                     stats_reset::text
              FROM pg_stat_database
              WHERE datname = current_database() |sql}
    in
    let hit_ratio =
      match dbstats with
      | [ r ] when cell_f r 2 > 0. ->
        100. *. cell_f r 1 /. (cell_f r 1 +. cell_f r 2)
      | _ -> 0.
    in
    let db_row =
      match dbstats with
      | [ r ] -> [
          HTML.tr [] [
            HTML.td [] [txt "%s" (cell_s r 0)];
            HTML.td [] [txt "%.1f%%" hit_ratio];
            HTML.td [] [txt "%.0f / %.0f" (cell_f r 1) (cell_f r 2)];
            HTML.td [] [txt "%.0f" (cell_f r 3)];
            HTML.td [] [txt "%.0f" (cell_f r 4)];
            HTML.td [] [txt "%.0f" (cell_f r 5)];
            HTML.td [] [txt "%s" (fmt_bytes (cell_f r 7))];
          ];
        ]
      | _ -> [ HTML.tr [] [ HTML.td [] [ txt "no stats rows (Zero-safe)" ] ] ]
    in
    (* ---- pg_stat_user_tables: missing-index smell (top by seq_scan) ---- *)
    let tabstats = exec_fixed
        {sql| SELECT relname,
                     coalesce(seq_scan,0), coalesce(idx_scan,0),
                     coalesce(n_live_tup,0),
                     coalesce(n_tup_ins,0) + coalesce(n_tup_upd,0)
                       + coalesce(n_tup_del,0)
              FROM pg_stat_user_tables
              WHERE relname NOT LIKE 'tree_%'
                AND relname NOT LIKE '%_mirror'
                AND relname <> 'schema_migrations'
              ORDER BY coalesce(seq_scan,0) DESC
              LIMIT 12 |sql}
    in
    let tab_rows = List.map (fun r ->
        HTML.tr [] [
          HTML.td [] [txt "%s" (cell_s r 0)];
          HTML.td [] [txt "%s" (if cell_f r 1 > 0. && cell_f r 3 > 0. then "⚠" else "")];
          HTML.td [] [txt "%d" (cell_i r 1)];
          HTML.td [] [txt "%d" (cell_i r 2)];
          HTML.td [] [txt "%d" (cell_i r 3)];
          HTML.td [] [txt "%d" (cell_i r 4)];
        ]) tabstats
    in
    (* ---- pg_stat_activity: what is RUNNING right now ---- *)
    let act = exec_fixed
        {sql| SELECT pid, state,
                     coalesce(wait_event_type,''), coalesce(wait_event,''),
                     coalesce(extract(epoch from (now() - query_start)),0)::int,
                     left(regexp_replace(query, E'[\\n\\r\\t ]+', ' ', 'g'), 100)
              FROM pg_stat_activity
              WHERE datname = current_database()
                AND state <> 'idle'
                AND pid <> pg_backend_pid()
              ORDER BY query_start
              LIMIT 12 |sql}
    in
    let act_rows = List.map (fun r ->
        HTML.tr [] [
          HTML.td [] [txt "%d" (cell_i r 0)];
          HTML.td [] [txt "%s" (cell_s r 1)];
          HTML.td [] [txt "%s/%s" (cell_s r 2) (cell_s r 3)];
          HTML.td [] [txt "%ds" (cell_i r 4)];
          HTML.td [] [txt "%s" (cell_s r 5)];
        ]) act
    in
    (* ---- pg_stat_statements: top by total_exec_time (when available) ---- *)
    let has_pss = match exec_fixed
        {sql| SELECT 1 FROM pg_extension WHERE extname = 'pg_stat_statements' |sql} with
      | [_] -> true | _ -> false
    in
    let stmts = if has_pss then exec_fixed
        {sql| SELECT left(regexp_replace(query, E'[\\n\\r\\t ]+', ' ', 'g'), 110),
                     calls, rows,
                     round(mean_exec_time::numeric, 2),
                     round(total_exec_time::numeric, 1)
              FROM pg_stat_statements
              WHERE dbid = (SELECT oid FROM pg_database WHERE datname = current_database())
              ORDER BY total_exec_time DESC
              LIMIT 12 |sql}
        else []
    in
    let stmt_rows = List.map (fun r ->
        HTML.tr [] [
          HTML.td [] [txt "%s" (cell_s r 0)];
          HTML.td [] [txt "%s" (cell_s r 1)];
          HTML.td [] [txt "%d" (cell_i r 2)];
          HTML.td [] [txt "%sms" (cell_s r 3)];
          HTML.td [] [txt "%sms" (cell_s r 4)];
        ]) stmts
    in
    let head3 cols = HTML.tr [] (List.map (fun c -> HTML.th [] [txt "%s" c]) cols) in
    let body =
      chrome ~session ~title:"DB panel" [
        HTML.p [HTML.class_ "meta"] [
          txt "Postgres statistics (read-only catalog views; fixed SQL). Per-endpoint prim breakdown lives on ";
          HTML.a [HTML.href "/timing"] [txt "/timing"];
          txt ".";
        ];
        HTML.h2 [] [txt "Database"];
        HTML.table [] [
          HTML.thead [] [head3 ["database"; "cache hit"; "blocks hit/read"; "xacts"; "rollbacks"; "deadlocks"; "temp bytes"]];
          HTML.tbody [] db_row;
        ];
        HTML.h2 [] [txt "Tables — top by sequential scan"];
        (match tab_rows with
         | [] -> HTML.p [HTML.class_ "meta"] [txt "(no user-table stats yet)"]
         | _ -> HTML.table [] [
             HTML.thead [] [head3 ["table"; ""; "seq scans"; "idx scans"; "live rows"; "writes"]];
             HTML.tbody [] tab_rows;
           ]);
        HTML.h2 [] [txt "Activity — running right now"];
        (match act_rows with
         | [] -> HTML.p [HTML.class_ "meta"] [txt "(nothing running besides this request)"]
         | _ -> HTML.table [] [
             HTML.thead [] [head3 ["pid"; "state"; "wait"; "age"; "query"]];
             HTML.tbody [] act_rows;
           ]);
        HTML.h2 [] [txt "Statements — top by total time"];
        (match stmt_rows with
         | [] ->
             HTML.p [HTML.class_ "meta"] [
               txt "pg_stat_statements is not loaded. Enable with shared_preload_libraries + CREATE EXTENSION pg_stat_statements for per-query accounting.";
             ]
         | _ -> HTML.table [] [
             HTML.thead [] [head3 ["query"; "calls"; "rows"; "mean"; "total"]];
             HTML.tbody [] stmt_rows;
           ]);
      ]
    in
    html_response reqd ~session body
  end
