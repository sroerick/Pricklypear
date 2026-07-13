(** Pricklypear Image — HTTP server entry: route dispatch + serve.

    System pages + browser product surfaces:
      /  /health  /branches  /bindings  /types  /data …
      /repl  /api/eval  /edit  /inspect  /agent
      user routes via (route ...)
*)
open Lwt.Syntax
open Pure_html
open Http_core
open Http_data
open Http_pages

let request_handler (session : Session.t) (_sockaddr : Unix.sockaddr)
    (reqd : Httpaf.Reqd.t) : unit =
  let req = Httpaf.Reqd.request reqd in
  let target = req.Httpaf.Request.target in
  let path =
    match String.index_opt target '?' with
    | Some i -> String.sub target 0 i
    | None -> target
  in
  let meth = req.Httpaf.Request.meth in
  let headers = header_list req in
  (* Thunk page: Lwt promises are eager — must not start page work
     before auth middleware decides proceed (auth.route-visibility). *)
  let launch (page_fn : unit -> unit Lwt.t) : unit =
    Lwt.async (fun () ->
      Lwt_mutex.with_lock request_mutex (fun () ->
        Lwt.catch
          (fun () ->
            let* proceed =
              if auth_disabled () then begin
                (* Dev escape: still try attach so whoami is real if cookie. *)
                let* user_opt = resolve_identity session ~headers in
                let* () =
                  match user_opt with
                  | Some user ->
                      let tok = cookie_value headers cookie_name in
                      attach_from_user session user ~auth_token:tok
                        ~checkout_personal_now:false ()
                  | None -> Session.attach_anonymous session
                in
                Lwt.return true
              end else begin
                (* Attach identity BEFORE classify: route table is
                   branch-scoped (inheritance CTE). Anonymous uses
                   system/main so package public routes resolve. *)
                let* user_opt = resolve_identity session ~headers in
                let* () =
                  match user_opt with
                  | Some user ->
                      let tok = cookie_value headers cookie_name in
                      attach_from_user session user ~auth_token:tok
                        ~checkout_personal_now:false ()
                  | None -> Session.attach_anonymous session
                in
                let classif = classify_path session path in
                let needs_auth =
                  match classif with
                  | `Hard_public -> false
                  | `Hard_private | `Unknown -> true
                  | `User (_, vis) -> vis <> "public"
                in
                match user_opt, needs_auth with
                | Some _, _ -> Lwt.return true
                | None, false -> Lwt.return true
                | None, true ->
                    let apiish =
                      String.length path >= 4
                      && String.sub path 0 4 = "/api"
                    in
                    if apiish then unauthorized_api reqd
                    else require_login_redirect reqd ~path;
                    Lwt.return false
              end
            in
            if not proceed then Lwt.return_unit
            else
              (* Refresh ui/nav cache every request so Lisp chrome is shared
                 by home, system pages, and user routes — not only /. *)
              let* () = Session.refresh_ui_shell session in
              page_fn ())
          (fun exn ->
            let body =
              chrome ~session ~title:"Error" [
                HTML.p [HTML.class_ "err"] [txt "%s" (Printexc.to_string exn)];
              ]
            in
            html_response reqd ~session ~status:`Internal_server_error body;
            Lwt.return_unit)))
  in
  match meth, path with
  | `GET, "/login" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         login_form_page reqd;
         Lwt.return_unit)
  | `POST, "/login" -> launch (fun () -> post_login session reqd)
  | `POST, "/logout" -> launch (fun () -> post_logout session reqd)
  | `GET, "/admin/users" -> launch (fun () -> admin_users_page session reqd)
  | `POST, "/admin/users" -> launch (fun () -> post_admin_users session reqd)
  | `GET, "/" -> launch (fun () -> home_page session reqd)
  | `GET, "/about" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         about_page session reqd;
         Lwt.return_unit)
  | `GET, "/pages" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         pages_list_page session reqd;
         Lwt.return_unit)
  | `POST, "/api/recipe/hello" ->
      launch (fun () ->
        let* res = Session.recipe_hello session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Hello recipe" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/hello"] [txt "Open /hello"];
                     txt " · ";
                     HTML.a [HTML.href "/edit/hi-page"] [txt "Edit hi-page"];
                     txt " · ";
                     HTML.a [HTML.href "/pages"] [txt "Pages"];
                     txt " · ";
                     HTML.a [HTML.href "/"] [txt "Home"];
                   ];
                 ];
               ]
             in
             html_response reqd ~session body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Hello recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `POST, "/api/recipe/todo" ->
      launch (fun () ->
        let* res = Session.recipe_todo session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Todo library" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/todos"] [txt "Open /todos"];
                     txt " · ";
                     HTML.a [HTML.href "/bindings?pkg=todo"] [txt "Package todo"];
                     txt " · ";
                     HTML.a [HTML.href "/pages"] [txt "Pages"];
                   ];
                 ];
               ]
             in
             html_response reqd ~session body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Todo recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `POST, "/api/recipe/notes" ->
      launch (fun () ->
        let* res = Session.recipe_notes session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Notes library" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/notes"] [txt "Open /notes"];
                     txt " · ";
                     HTML.a [HTML.href "/bindings?pkg=notes"] [txt "Package notes"];
                     txt " · ";
                     HTML.a [HTML.href "/bindings?pkg=outline"] [txt "outline"];
                   ];
                   HTML.p [HTML.class_ "muted"] [
                     txt "Outline-first notes (org-shaped). Content lives in data_rows.";
                   ];
                 ];
               ]
             in
             html_response reqd ~session body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Notes recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `POST, "/api/recipe/dabar" ->
      launch (fun () ->
        let* res = Session.recipe_dabar session in
         match res with
         | Ok msg ->
             let body =
               chrome ~session ~title:"Dabar library" [
                 HTML.div [HTML.class_ "card"] [
                   HTML.p [HTML.class_ "ok"] [txt "%s" msg];
                   HTML.p [] [
                     HTML.a [HTML.href "/dabar?book=John&chapter=3"]
                       [txt "Open /dabar"];
                     txt " · ";
                     HTML.a [HTML.href "/dabar?book=John&chapter=3&verse=16"]
                       [txt "Study John 3:16"];
                     txt " · ";
                     HTML.a [HTML.href "/strongs?n=G2316"] [txt "Strong's G2316"];
                   ];
                   HTML.p [HTML.class_ "muted"] [
                     txt "Seeds: scripts/seed_bible_bsb.py · seed_strongs.py · seed_interlinear_dogfood.py";
                   ];
                 ];
               ]
             in
             html_response reqd ~session body;
             Lwt.return_unit
         | Error e ->
             let body =
               chrome ~session ~title:"Dabar recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             Lwt.return_unit)
  | `GET, "/health" ->
      let h =
        { Pricklypear_common.Protocol.ok = true;
          branch = Session.branch_name session;
          uptime_seconds = Session.uptime_seconds session;
          error = None }
      in
      let j = Pricklypear_common.Protocol.health_response_to_json h in
      text_response reqd ~content_type:"application/json"
        (Yojson.Safe.pretty_to_string j)
  | `GET, "/branches" -> launch (fun () -> branches_page session reqd)
  | `POST, "/branches/checkout" -> launch (fun () -> branches_checkout session reqd)
  | `POST, "/branches/fork" -> launch (fun () -> branches_fork session reqd)
  | `POST, "/branches/unoverride" -> launch (fun () -> branches_unoverride session reqd)
  | `POST, "/branches/promote" -> launch (fun () -> branches_promote session reqd)
  | `GET, "/bindings" -> launch (fun () -> list_bindings_page session reqd)
  | `GET, "/types" -> launch (fun () -> list_types_page session reqd)
  | `GET, "/data" | `GET, "/data/" ->
      launch (fun () -> data_index_page session reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/data/" ->
      let rest = String.sub path 6 (String.length path - 6) in
      let segs = String.split_on_char '/' rest |> List.filter ((<>) "") in
      (match segs with
       | [ty; "new"] ->
           launch (fun () -> data_new_page session ty reqd)
       | [ty; id; "delete"] ->
           (* GET delete not supported — point at detail *)
           launch (fun () -> data_detail_page session ty id reqd)
       | [ty; id] ->
           launch (fun () -> data_detail_page session ty id reqd)
       | [ty] ->
           launch (fun () -> data_list_page session ty reqd)
       | _ ->
           html_response reqd ~session ~status:`Bad_request
             (chrome ~session ~title:"Bad data path" [
                HTML.p [] [txt "use /data, /data/<type>, /data/<type>/new, /data/<type>/<id>"];
              ]))
  | `POST, path when String.length path >= 6
                  && String.sub path 0 6 = "/data/" ->
      let rest = String.sub path 6 (String.length path - 6) in
      let segs = String.split_on_char '/' rest |> List.filter ((<>) "") in
      (match segs with
       | [ty; id; "delete"] ->
           launch (fun () -> data_delete session ty id reqd)
       | [ty; id] ->
           launch (fun () -> data_update session ty id reqd)
       | [ty] ->
           launch (fun () -> data_create session ty reqd)
       | _ ->
           html_response reqd ~session ~status:`Bad_request
             (chrome ~session ~title:"Bad data path" [
                HTML.p [] [txt "POST /data/<type> | /data/<type>/<id> | …/delete"];
              ]))
  | `GET, "/repl" ->
      launch (fun () ->
        let* () = Lwt.return_unit in
         repl_page session reqd;
         Lwt.return_unit)
  | `POST, "/api/eval" -> launch (fun () -> api_eval session reqd)
  | `POST, "/api/agent" -> launch (fun () -> api_agent session reqd)
  | `POST, "/api/agent-task" -> launch (fun () -> api_agent_task session reqd)
  | `POST, "/api/ai/propose" -> launch (fun () -> ai_propose session reqd)
  | `GET, "/agent" -> launch (fun () -> agent_page session reqd)
  | `POST, "/api/edit/create" -> launch (fun () -> api_edit_create session reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/page/" ->
      let seg = String.sub path 6 (String.length path - 6) in
      launch (fun () -> page_inspector session seg reqd)
  | `GET, "/page" | `GET, "/page/" ->
      launch (fun () -> page_inspector session "" reqd)
  | `GET, path when String.length path >= 17
                 && String.sub path 0 17 = "/inspect/binding/" ->
      let name =
        Packages.name_decode (String.sub path 17 (String.length path - 17))
      in
      launch (fun () -> inspect_binding_page session name reqd)
  | `GET, path when String.length path >= 13
                 && String.sub path 0 13 = "/inspect/row/" ->
      let rest = String.sub path 13 (String.length path - 13) in
      (match String.split_on_char '/' rest with
       | [ty; id] -> launch (fun () -> inspect_row_page session ty id reqd)
       | _ ->
           html_response reqd ~session ~status:`Bad_request
             (chrome ~session ~title:"Bad inspect path" [
                HTML.p [] [txt "use /inspect/row/<type>/<id>"];
              ]))
  | `GET, path when String.length path >= 9
                 && String.sub path 0 9 = "/binding/" ->
      let name =
        Packages.name_decode (String.sub path 9 (String.length path - 9))
      in
      launch (fun () -> show_binding_page session name reqd)
  | `GET, "/edit" | `GET, "/edit/" ->
      (* package + local query form from Functions browser *)
      let pkg = Option.value (query_param target "package") ~default:"" in
      let local = Option.value (query_param target "local") ~default:"" in
      let name = Packages.join_name ~package:pkg ~local in
      if name = "" then
        html_response reqd ~session
          (chrome ~session ~title:"New function" [
             HTML.p [] [txt "Provide package and/or name (see Functions)."];
             HTML.p [] [HTML.a [HTML.href "/bindings"] [txt "Functions"]];
           ])
      else launch (fun () -> editor_page session name reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (fun () -> editor_page session name reqd)
  | `POST, path when String.length path >= 6
                  && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (fun () -> save_edit session name reqd)
  | ((`GET | `POST) as umeth), _ ->
      (* User routes: GET + POST (it33). Same path → same handler;
         request-* prims expose method/params for branching. *)
      (match Session.lookup_route session path with
       | Some handler_name ->
           let page () : unit Lwt.t =
             let* body_str =
               match umeth with
               | `POST -> read_body reqd
               | `GET -> Lwt.return ""
             in
             if String.length body_str > max_user_body_bytes then begin
               text_response reqd ~status:`Payload_too_large
                 ~content_type:"text/plain"
                 "request body too large (max 64KiB)";
               Lwt.return_unit
             end else begin
               let query = parse_query target in
               let form =
                 match umeth with
                 | `GET -> []
                 | `POST ->
                     let ct =
                       match
                         Httpaf.Headers.get req.headers "content-type"
                       with
                       | Some s -> s
                       | None -> ""
                     in
                     if String.length ct >= 33
                        && String.sub ct 0 33
                           = "application/x-www-form-urlencoded"
                        || ct = ""
                           && String.contains body_str '='
                     then parse_form_body body_str
                     else []
               in
               let params = merge_params ~query ~form in
               let headers = header_list req in
               let fragment = wants_fragment ~path ~headers in
               let method_ : Session.http_method =
                 match umeth with `GET -> `GET | `POST -> `POST
               in
               let request : Session.request =
                 { method_; path; params; body = body_str; headers }
               in
               let* result =
                 Session.call_handler session ~request handler_name
               in
               (match result with
                | Some value ->
                    respond_handler_value ~session ~reqd ~path ~fragment value;
                    Lwt.return_unit
                | None ->
                    if fragment then
                      fragment_response reqd ~status:`Internal_server_error
                        ("handler " ^ handler_name ^ " returned no value")
                    else
                      html_response reqd ~session ~status:`Internal_server_error
                        (chrome ~session ~title:"Route handler error" [
                           HTML.p []
                             [txt "handler %s returned no value" handler_name];
                         ]);
                    Lwt.return_unit)
             end
           in
           launch (fun () -> page ())
       | None ->
           let headers = header_list req in
           if wants_fragment ~path ~headers then
             fragment_response reqd ~status:`Not_found
               ("no partial for " ^ path)
           else
             html_response reqd ~session ~status:`Not_found
               (chrome ~session ~title:"Not found" [
                  HTML.p [] [txt "no route for %s" path];
                ]))
  | _, _ ->
      let headers = header_list req in
      if wants_fragment ~path ~headers then
        fragment_response reqd ~status:`Not_found ("no partial for " ^ path)
      else
        html_response reqd ~session ~status:`Not_found
          (chrome ~session ~title:"Not found" [
             HTML.p [] [txt "no route for %s" path];
           ])

let error_handler (_sockaddr : Unix.sockaddr)
    ?(request : Httpaf.Request.t option)
    (err : Httpaf.Server_connection.error)
    (respond : Httpaf.Headers.t -> [> `write ] Httpaf.Body.t) : unit =
  let status, msg =
    match err with
    | `Bad_request -> `Bad_request, "bad request"
    | `Bad_gateway -> `Bad_gateway, "bad gateway"
    | `Internal_server_error -> `Internal_server_error, "internal server error"
    | `Exn exn -> `Internal_server_error, Printexc.to_string exn
  in
  let _ = request in
  let body_str =
    Pure_html.to_string
      (HTML.html [HTML.lang "en"] [
         HTML.head [] [HTML.title [] "Pricklypear Error"];
         HTML.body [] [
           HTML.h1 [] [txt "Error"];
           HTML.p [] [txt "%s" msg];
         ];
       ])
  in
  let headers =
    Httpaf.Headers.of_list
      [("content-type", "text/html");
       ("content-length", string_of_int (String.length body_str))]
  in
  let body_w = respond headers in
  Httpaf.Body.write_string body_w body_str;
  Httpaf.Body.close_writer body_w

let serve ~(session : Session.t) ?(port = default_port) () : unit Lwt.t =
  let handler = request_handler session in
  let establisher =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:handler
      ~error_handler
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  Printf.eprintf "[http] serving htmx on 0.0.0.0:%d\n" port;
  let* _server = Lwt_io.establish_server_with_client_socket addr establisher in
  let waiter, _waker = Lwt.wait () in
  waiter
