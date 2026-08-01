(** HTTP surface for on-demand function analysis (htmx fragments). *)
open Pure_html
open Http_core

let fragment_of_json (j : Yojson.Safe.t) : node =
  let open Yojson.Safe.Util in
  let s key =
    match member key j with
    | `String x -> x
    | `Null -> ""
    | other -> Yojson.Safe.to_string other
  in
  let label =
    match s "label" with "" -> s "source" | l -> l
  in
  HTML.div [ HTML.class_ "card analyze-result" ]
    [
      HTML.p []
        [
          HTML.span [ HTML.class_ "badge complexity" ] [ txt "%s" (s "complexity") ];
          txt " ";
          HTML.span [ HTML.class_ "muted" ] [ txt "%s" label ];
          (match s "model" with
           | "" -> txt ""
           | m -> HTML.span [ HTML.class_ "muted" ] [ txt " · %s" m ]);
        ];
      HTML.p [] [ txt "confidence: %s" (s "confidence") ];
      HTML.p [] [ txt "%s" (s "rationale") ];
      (match s "notes" with
       | "" -> txt ""
       | n -> HTML.p [ HTML.class_ "muted" ] [ txt "%s" n ]);
    ]

let fragment_error (msg : string) : node =
  HTML.div [ HTML.class_ "card analyze-result" ]
    [ HTML.p [ HTML.class_ "err" ] [ txt "%s" msg ] ]

let post_analyze (session : Session.t) (name : string)
    (reqd : Httpun.Reqd.t) : unit =
  let store = Session.store session in
  let man = Session.manifest session in
  let author = man.username in
  let user_id =
    if man.user_id = "" || man.user_id = "anonymous" || man.user_id = "public"
    then None
    else Some man.user_id
  in
  let force_llm =
    match Httpun.Reqd.request reqd with
    | { Httpun.Request.target; _ } ->
        (try
           let q =
             match String.split_on_char '?' target with
             | _ :: qs -> String.concat "?" qs
             | _ -> ""
           in
           List.exists
             (fun p -> p = "llm=1" || p = "force=1")
             (String.split_on_char '&' q)
         with _ -> false)
  in
  let node =
    match
      Analyze.analyze_function store ~author ~user_id ~fn_name:name
        ~force_llm
    with
    | Ok j -> fragment_of_json j
    | Error e -> fragment_error e
  in
  fragment_response reqd (Pure_html.to_string node)

let post_explain (session : Session.t) (name : string)
    (reqd : Httpun.Reqd.t) : unit =
  let store = Session.store session in
  let node =
    match Analyze.explain_function store ~fn_name:name with
    | Ok text ->
        HTML.div [ HTML.class_ "card explain-result" ]
          [ HTML.pre [ HTML.class_ "bb-source" ] [ txt "%s" text ] ]
    | Error e -> fragment_error e
  in
  fragment_response reqd (Pure_html.to_string node)
