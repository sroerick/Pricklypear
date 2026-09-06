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
        txt " or via (create 'type \"{...}\") in the REPL.";
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
    txt "You see your own rows plus shared seed data (bible, strongs). ";
    txt "Data is not versioned by branch. ";
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
  let rows =
    List.map
      (fun (name, fields) ->
        let n = Crud.count_by_type store ~type_name:name in
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
        let n = Crud.count_by_type store ~type_name:name in
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
               txt "(no data_rows yet; declare a type via (deftype …) or ";
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


(* ---- page timing (image.page-timing) ---- *)

let timing_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let aggs = Timing.aggregates () in
  let db_share (a : Timing.agg) : string =
    if a.avg_ms > 0.0001 then
      Printf.sprintf "%.0f%%" (100. *. a.db_ms /. a.avg_ms)
    else "—"
  in
  let rows =
    List.map
      (fun (a : Timing.agg) ->
        HTML.tr [] [
          HTML.td [] [txt "%s" a.route];
          HTML.td [] [txt "%d" a.count];
          HTML.td [] [txt "%.1f" a.last_ms];
          HTML.td [] [txt "%.1f" a.avg_ms];
          HTML.td [] [txt "%.1f" a.max_ms];
          HTML.td [] [txt "%.1f" a.db_ms];
          HTML.td [] [txt "%.1f" a.db_hits];
          HTML.td [] [txt "%s" (db_share a)];
        ])
      aggs
  in
  let body =
    chrome ~session ~title:"Page timing" [
      HTML.p [HTML.class_ "meta"] [
        txt "Server-side render cost per user route (last %d total samples; %d per route)."
          Timing.max_total_samples Timing.max_samples_per_route;
        txt "  Sorted slowest-first by max. DB share = avg DB wall time / avg total.";
      ];
      (match aggs with
       | [] ->
           HTML.p [HTML.class_ "err"] [
             txt "No route timings measured yet. Visit some pages first, then reload.";
           ]
       | _ ->
           HTML.table [] [
             HTML.thead [] [
               HTML.tr [] [
                 HTML.th [] [txt "route"];
                 HTML.th [] [txt "samples"];
                 HTML.th [] [txt "last ms"];
                 HTML.th [] [txt "avg ms"];
                 HTML.th [] [txt "max ms"];
                 HTML.th [] [txt "db ms"];
                 HTML.th [] [txt "db hits"];
                 HTML.th [] [txt "db share"];
               ]
             ];
             HTML.tbody [] rows;
           ]);
    ]
  in
  html_response reqd ~session body;
  ()
