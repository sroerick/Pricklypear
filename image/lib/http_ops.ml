(** /analytics page + ops HTTP helpers. *)
open Lwt.Syntax
open Pure_html
open Http_core

let analytics_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit Lwt.t =
  let store = Session.store session in
  let* logins_ok, logins_fail = Ops_incidents.count_logins store in
  let summary = Ops_stats.summary_yojson ~logins_ok ~logins_fail in
  let* incidents = Ops_incidents.list_open_incidents store in
  let recent = Ops_log.tail ~n:30 ~level:Ops_log.Warn () in
  let day =
    match summary with
    | `Assoc xs ->
        (match List.assoc_opt "day" xs with Some (`String d) -> d | _ -> "?")
    | _ -> "?"
  in
  let int_field name =
    match summary with
    | `Assoc xs ->
        (match List.assoc_opt name xs with Some (`Int n) -> n | _ -> 0)
    | _ -> 0
  in
  let top_rows =
    match summary with
    | `Assoc xs ->
        (match List.assoc_opt "top_paths" xs with
         | Some (`List paths) ->
             List.filter_map
               (function
                 | `Assoc p ->
                     let path =
                       match List.assoc_opt "path" p with
                       | Some (`String s) -> s
                       | _ -> "?"
                     in
                     let n =
                       match List.assoc_opt "n" p with
                       | Some (`Int i) -> i
                       | _ -> 0
                     in
                     Some
                       (HTML.tr []
                          [ HTML.td [] [txt "%s" path]
                          ; HTML.td [] [txt "%d" n]
                          ])
                 | _ -> None)
               paths
         | _ -> [])
    | _ -> []
  in
  let incident_items =
    match incidents with
    | `List xs ->
        List.filter_map
          (function
            | `Assoc a ->
                let comp =
                  match List.assoc_opt "component" a with
                  | Some (`String s) -> s
                  | _ -> "?"
                in
                let sample =
                  match List.assoc_opt "sample_msg" a with
                  | Some (`String s) -> s
                  | _ -> ""
                in
                let count =
                  match List.assoc_opt "count" a with
                  | Some (`Int n) -> n
                  | _ -> 1
                in
                let todo =
                  match List.assoc_opt "todo_id" a with
                  | Some (`String s) when s <> "" -> s
                  | _ -> ""
                in
                let sample_short =
                  if String.length sample > 100 then
                    String.sub sample 0 100 ^ "…"
                  else sample
                in
                Some
                  (HTML.li []
                     [ HTML.strong [] [txt "%s ×%d" comp count]
                     ; txt " — %s" sample_short
                     ; (if todo = "" then txt ""
                        else
                          HTML.span []
                            [ txt " "
                            ; HTML.a [HTML.href "/todos"] [txt "todo"]
                            ])
                     ])
            | _ -> None)
          xs
    | _ -> []
  in
  let log_items =
    List.map
      (fun (e : Ops_log.event) ->
        HTML.li []
          [ HTML.code [] [txt "%s" (Ops_log.level_to_string e.level)]
          ; txt " [%s] %s" e.component e.msg
          ])
      (List.rev recent)
  in
  let* body =
    chrome_lwt ~session ~title:"Analytics"
      [ HTML.h1 [] [txt "Analytics"]
      ; HTML.p []
          [ txt
              "Observatory for today (UTC). Errors become todos — see open \
               incidents."
          ]
      ; HTML.div [HTML.class_ "card"]
          [ HTML.h2 [] [txt "Today · %s" day]
          ; HTML.ul []
              [ HTML.li []
                  [ txt "Public unique viewers: %d"
                      (int_field "public_uniques")
                  ]
              ; HTML.li []
                  [txt "Page views: %d" (int_field "page_views")]
              ; HTML.li []
                  [ txt "Logins ok / fail: %d / %d" logins_ok logins_fail
                  ]
              ; HTML.li []
                  [ txt "Active users: %d" (int_field "active_users")
                  ]
              ; HTML.li []
                  [ txt "Errors / fatals (this process): %d / %d"
                      (int_field "errors") (int_field "fatals")
                  ]
              ; HTML.li []
                  [txt "Noise/bot views: %d" (int_field "noise_views")]
              ]
          ]
      ; HTML.div [HTML.class_ "card"]
          [ HTML.h2 [] [txt "Top paths"]
          ; (if top_rows = [] then HTML.p [] [txt "No traffic yet."]
             else
               HTML.table []
                 (HTML.tr []
                    [ HTML.th [] [txt "Path"]
                    ; HTML.th [] [txt "Views"]
                    ]
                 :: top_rows))
          ]
      ; HTML.div [HTML.class_ "card"]
          [ HTML.h2 [] [txt "Open reliability incidents"]
          ; HTML.p []
              [ txt
                  "New error fingerprints auto-file a note + TODO. \
                   Repeats bump the count only."
              ]
          ; (if incident_items = [] then
               HTML.p [HTML.class_ "ok"] [txt "None open."]
             else HTML.ul [] incident_items)
          ; HTML.p []
              [ HTML.a [HTML.href "/todos"] [txt "Open todos →"]
              ; txt " · "
              ; HTML.a [HTML.href "/notes"] [txt "Notes →"]
              ]
          ]
      ; HTML.div [HTML.class_ "card"]
          [ HTML.h2 [] [txt "Recent log (warn+)"]
          ; (if log_items = [] then HTML.p [] [txt "Quiet."]
             else HTML.ul [] log_items)
          ]
      ]
  in
  html_response reqd ~session body;
  Lwt.return_unit
