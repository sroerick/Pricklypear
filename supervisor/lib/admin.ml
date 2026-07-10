(** Pricklypear Supervisor — Admin HTTP surface.

    Per spec [supervisor.admin-http]:
      GET  /                  fleet dashboard
      GET  /fleet             list instances + health (JSON)
      POST /fleet/:id/kill    force-kill an instance
      POST /fleet/:id/rollback?to=vN
      GET  /health            supervisor's own health

    For the minimal supervisor: GET /, GET /fleet (JSON), GET /health.
    POST routes: TODO (need form parsing). Auth: none (localhost bound). *)
open Lwt.Syntax
open Lwt.Infix
open Pure_html

let default_port = 8089

let html_response (reqd : Httpaf.Reqd.t) ?(status = `OK) (body : Pure_html.node) : unit =
  let html_doc = HTML.html [HTML.lang "en"] [
    HTML.head [] [HTML.title [] "Pricklypear Supervisor"];
    HTML.body [] [body];
  ] in
  let s = Pure_html.to_string html_doc in
  let headers =
    Httpaf.Headers.of_list
      [("content-type", "text/html");
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

(* Render the fleet dashboard as an HTML table. *)
let render_dashboard (fleet : Fleet.t) : Pure_html.node =
  let instances = Fleet.list fleet in
  let h_status_str =
    Pricklypear_common.Protocol.health_status_to_string
  in
  let rows =
    List.map (fun (i : Fleet.instance) ->
      HTML.tr [] [
        HTML.td [] [txt "%s" i.id];
        HTML.td [] [txt "%s" i.username];
        HTML.td [] [txt "%s" i.branch_name];
        HTML.td [] [txt "%s" i.image_version];
        HTML.td [] [txt "%d" i.http_port];
        HTML.td [] [txt "%d" i.repl_port];
        HTML.td [] [txt "%s" (h_status_str i.health)];
      ])
      instances
  in
  let count = List.length instances in
  HTML.div [] [
    HTML.h1 [] [txt "Pricklypear Supervisor"];
    HTML.p [] [txt "%d image(s) running" count];
    HTML.table [] [
      HTML.thead [] [
        HTML.tr [] [
          HTML.th [] [txt "id"];
          HTML.th [] [txt "user"];
          HTML.th [] [txt "branch"];
          HTML.th [] [txt "version"];
          HTML.th [] [txt "http"];
          HTML.th [] [txt "repl"];
          HTML.th [] [txt "health"];
        ];
      ];
      HTML.tbody [] rows;
    ];
  ]

(* Render fleet as JSON (list of instances). *)
let fleet_to_json (fleet : Fleet.t) : Yojson.Safe.t =
  let instances = Fleet.list fleet in
  let h_status_str =
    Pricklypear_common.Protocol.health_status_to_string
  in
  `List (
    List.map (fun (i : Fleet.instance) ->
      `Assoc [
        ("id", `String i.id);
        ("user_id", `String i.user_id);
        ("username", `String i.username);
        ("branch_id", `String i.branch_id);
        ("branch_name", `String i.branch_name);
        ("image_version", `String i.image_version);
        ("http_port", `Int i.http_port);
        ("repl_port", `Int i.repl_port);
        ("health", `String (h_status_str i.health));
      ])
      instances)

let request_handler
    (fleet : Fleet.t)
    (_sockaddr : Unix.sockaddr)
    (reqd : Httpaf.Reqd.t) : unit =
  let target = (Httpaf.Reqd.request reqd).Httpaf.Request.target in
  match target with
  | "/" ->
      let body = render_dashboard fleet in
      html_response reqd body
  | "/fleet" ->
      let j = fleet_to_json fleet in
      let s = Yojson.Safe.pretty_to_string j in
      text_response reqd ~content_type:"application/json" s
  | "/health" ->
      let h =
        { Pricklypear_common.Protocol.ok = true;
          branch = "supervisor";
          uptime_seconds = 0;  (* TODO: track supervisor's own uptime *)
          error = None }
      in
      let j = Pricklypear_common.Protocol.health_response_to_json h in
      let s = Yojson.Safe.pretty_to_string j in
      text_response reqd ~content_type:"application/json" s
  | _ ->
      let body =
        HTML.div [] [
          HTML.h1 [] [txt "Not found"];
          HTML.p [] [txt "no route for %s" target];
        ]
      in
      html_response reqd ~status:`Not_found body

let error_handler
    (_sockaddr : Unix.sockaddr)
    ?(request : Httpaf.Request.t option)
    (err : Httpaf.Server_connection.error)
    (respond : Httpaf.Headers.t -> [> `write ] Httpaf.Body.t) : unit =
  let msg =
    match err with
    | `Bad_request -> "bad request"
    | `Bad_gateway -> "bad gateway"
    | `Internal_server_error -> "internal server error"
    | `Exn exn -> Printexc.to_string exn
  in
  let _ = request in
  let body_str =
    Pure_html.to_string
      (HTML.html [HTML.lang "en"] [
        HTML.head [] [HTML.title [] "Supervisor Error"];
        HTML.body [] [
          HTML.div [] [
            HTML.h1 [] [txt "Error"];
            HTML.p [] [txt "%s" msg];
          ];
        ];
      ])
  in
  let headers = Httpaf.Headers.of_list [("content-type", "text/html")] in
  let body_w = respond headers in
  Httpaf.Body.write_string body_w body_str;
  Httpaf.Body.close_writer body_w

let serve
    ~(fleet : Fleet.t)
    ?(port = default_port) ()
    : unit Lwt.t =
  let establisher =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler fleet)
      ~error_handler
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  Printf.eprintf "[supervisor.admin] serving admin on 0.0.0.0:%d\n" port;
  let* _server =
    Lwt_io.establish_server_with_client_socket addr establisher
  in
  let waiter, _waker = Lwt.wait () in
  waiter
