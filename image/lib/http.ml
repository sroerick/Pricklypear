(** Pricklypear Image — HTTP server entry: route dispatch + serve.

    System pages + browser product surfaces:
      /  /health  /branches  /bindings  /types  /data …
      /repl  /api/eval  /edit  /inspect  /agent
      public agent bootstrap: /.well-known/agent.json  /agent.txt
      user routes via (route ...)
*)
open Lwt.Syntax
open Lwt
open Request_context
open Pure_html
open Http_core
open Http_data
open Http_branches
open Http_agent
open Http_pages
open Http_ops
open Http_media

let request_handler (session : Session.t) (sockaddr : Unix.sockaddr)
    (reqd : Httpun.Reqd.t) : unit =
  let req = Httpun.Reqd.request reqd in
  let target = req.Httpun.Request.target in
  let path =
    match String.index_opt target '?' with
    | Some i -> String.sub target 0 i
    | None -> target
  in
  let meth = req.Httpun.Request.meth in
  let headers = header_list req in
  (* Thunk page: Lwt promises are eager — must not start page work
     before auth middleware decides proceed (auth.route-visibility).

     image.http-robustness invariant 5: bound the expensive page-work queue
     with the inflight cap so a traffic spike can't accumulate unbounded
     queued work (memory/fds). Acquired here (inside launch) so cheap probes
     like /health that bypass launch are NEVER blocked by page traffic —
     liveness must stay servable under load. Released in finalize so a slot
     is freed whether page_fn succeeded, failed, or the peer went away. *)
  let launch (page_fn : unit -> unit Lwt.t) : unit =
    Lwt.async (fun () ->
      let* () = inflight_acquire () in
      Lwt.finalize
        (fun () ->
          Lwt.catch
            (fun () ->
              (* image.http-robustness invariant 4b: request_mutex guards
                 ONLY the identity-attach gate (resolve_identity + attach +
                 classify + subdomain mismatch + analytics + refresh_ui_shell).
                 It is released BEFORE page_fn runs so page work (Lisp eval, HTML
                 build, agent LLM streaming) is lock-free. Correctness rests on
                 invariant 4a (immutable-env rehydrate: in-flight closures hold
                 their own env record, unaffected by a concurrent rehydrate) +
                 pgx's per-connection Sequencer (DB prims serialize on the single
                 PG conn without this mutex). See borge/image.borg http-robustness. *)
              (* Per-request context snapshot (image.per-request-context):
                 built INSIDE the mutex so it is consistent with this request's
                 attach, then bound via Lwt.with_value so page work reads the
                 snapshot (env/routes/manifest) instead of live Session.t,
                 immune to a concurrent attach_anonymous + checkout_to
                 rehydrating the shared session mid-page. *)
              let snapshot_ctx () =
                Request_context.make
                  ~manifest:(Session.manifest session)
                  ~env:(Session.env session)
                  ~routes:(Request_context.copy_routes (Session.routes session))
              in
              let* proceed_ctx =
                Lwt_mutex.with_lock request_mutex (fun () ->
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
                    Lwt.return (Some (snapshot_ctx ()))
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
                    | Some _, _ -> Lwt.return (Some (snapshot_ctx ()))
                    | None, false -> Lwt.return (Some (snapshot_ctx ()))
                    | None, true ->
                        let apiish =
                          String.length path >= 4
                          && String.sub path 0 4 = "/api"
                        in
                        if apiish then unauthorized_api reqd
                        else require_login_redirect reqd ~path ~target;
                        Lwt.return None
                  end)
              in
              (match proceed_ctx with
               | None -> Lwt.return_unit
               | Some req_ctx ->
              Lwt.with_value Request_context.current (Some req_ctx) @@ fun () ->
              (* Per-request context bound; page work below reads it for
                 env/routes/manifest (call_handler, lookup_route, chrome). *)
              (* Subdomain garden mismatch: private paths require
                 whoami = garden owner. Reads the SNAPSHOT manifest so a
                 concurrent attach cannot flip identity under this check. *)
              let host_mode = classify_host headers in
              let manifest_now = Request_context.manifest req_ctx in
              let uname_now = manifest_now.username in
              let classif_now =
                classify_path_with_routes (Request_context.routes req_ctx) path
              in
                let path_private =
                  match classif_now with
                  | `Hard_public -> false
                  | `User (_, vis) -> vis <> "public"
                  | `Hard_private | `Unknown -> true
                in
                let mismatch =
                  match host_mode with
                  | Host_garden g
                    when path_private && uname_now <> "anonymous"
                         && uname_now <> g -> true
                  | Host_unknown when base_domain () <> None -> true
                  | _ -> false
                in
                if mismatch then begin
                  (match host_mode, base_domain () with
                   | Host_garden _, Some base when uname_now <> "anonymous" ->
                       redirect_response reqd
                         ("http://" ^ uname_now ^ "." ^ base ^ "/")
                   | Host_unknown, _ ->
                       text_response reqd ~status:`Not_found
                         ~content_type:"text/plain" "unknown host"
                   | _ -> redirect_response reqd "/");
                  Lwt.return_unit
                end else begin
                (* Product analytics (in-memory day counters). *)
                let uname = Session.username session in
                let is_authed =
                  uname <> "anonymous" && uname <> "public" && uname <> ""
                in
                Ops_stats.record_request ~path ~is_authed
                  ~username:(if is_authed then Some uname else None)
                  ~headers ~sockaddr;
                (* Refresh ui/nav cache every request so Lisp chrome is shared
                   by home, system pages, and user routes — not only /. *)
                let* () = Session.refresh_ui_shell session in
                (* GATE COMPLETE — request_mutex released above. page_fn runs
                   lock-free (invariant 4b). *)
                page_fn ()
                end
              ))
          (fun exn ->
            (* image.http-robustness invariant 2: if the handler failed
               because the peer went away (closed writer), do NOT try to
               render a 500 page onto the same dead writer — that re-throw
               was the actual crash mechanism. Just close quietly and end. *)
            if is_closed_writer_exn exn then begin
              ignore (Ops_log.info ~component:"http"
                        ~context:[("path", path)]
                        "peer gone (closed writer); skipping 500");
              Lwt.return_unit
            end else begin
            let msg = Printexc.to_string exn in
            Lwt.async (fun () ->
              Ops_incidents.emit_and_maybe_file session
                ~level:Ops_log.Error ~component:"http"
                ~file:(not (Ops_incidents.is_expected_runtime_error exn))
                ~context:[("path", path)] msg);
            let body =
              chrome ~session ~title:"Error" [
                HTML.p [HTML.class_ "err"] [txt "%s" msg];
              ]
            in
            html_response reqd ~session ~status:`Internal_server_error body;
            Lwt.return_unit
            end))
      (fun () -> inflight_release ()))
  in
  (** Lisp user-route if bound; else kernel [fallback]. *)
  let launch_user_route_or (fallback : unit -> unit) : unit =
    (* Resolve the route INSIDE page work, after the gate has attached
       identity and bound the per-request context (image.per-request-context).
       Pre-gate the routes table could be whatever a previous request left
       behind (e.g. anonymous attach -> system/main), causing a personal-branch
       route 404 before THIS request's attach even runs. *)
    launch (fun () ->
      let route_opt =
        match Lwt.get Request_context.current with
        | Some ctx -> Request_context.lookup_route ctx path
        | None -> Session.lookup_route session path
      in
      match route_opt with
      | None ->
          fallback ();
          Lwt.return_unit
      | Some handler_name ->
          let page () : unit Lwt.t =
            let* body_str =
              match meth with
              | `POST -> read_body reqd
              | _ -> Lwt.return ""
            in
          if String.length body_str > max_user_body_bytes then begin
            text_response reqd ~status:`Payload_too_large
              ~content_type:"text/plain"
              "request body too large (max 64KiB)";
            Lwt.return_unit
          end else begin
            let query = parse_query target in
            let form =
              match meth with
              | `POST ->
                  let ct =
                    match Httpun.Headers.get req.headers "content-type" with
                    | Some s -> s
                    | None -> ""
                  in
                  if String.length ct >= 33
                     && String.sub ct 0 33
                        = "application/x-www-form-urlencoded"
                     || ct = "" && String.contains body_str '='
                  then parse_form_body body_str
                  else []
              | _ -> []
            in
            let params = merge_params ~query ~form in
            let fragment = wants_fragment ~path ~headers in
            let method_ : Session.http_method =
              match meth with `POST -> `POST | _ -> `GET
            in
            let request : Session.request =
              { method_; path; params; body = body_str; headers }
            in
            let* result =
              Session.call_handler session ~request handler_name
            in
            (match result with
             | Some value ->
                 (* Public front-door pages: no product chrome (private nav). *)
                 if path = "/welcome"
                    || path = "/background" || path = "/roadmap"
                    || path = "/signup"
                 then begin
                   let title =
                     if path = "/background" then "Background"
                     else if path = "/roadmap" then "Roadmap"
                     else if path = "/signup" then "Signup"
                     else "Pricklypear"
                   in
                   let body =
                     match Primitives.is_raw_html value with
                     | Some raw -> raw
                     | None ->
                         (match value with
                          | Pricklypear_lisp.Ast.VStr s -> s
                          | _ -> Pricklypear_lisp.Reader.pp_value value)
                   in
                   Http_code.serve_public_body reqd ~title body;
                   Lwt.return_unit
                 end else begin
                   respond_handler_value ~session ~reqd ~path ~fragment value;
                   Lwt.return_unit
                 end
             | None ->
                 fallback ();
                 Lwt.return_unit)
          end
        in
        page ())
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
  | `GET, "/settings/keys" -> launch (fun () -> settings_keys_page session reqd)
  | `POST, "/settings/keys" -> launch (fun () -> post_settings_keys session reqd)
  | `GET, "/settings/ssh" -> launch (fun () -> settings_ssh_page session reqd)
  | `POST, "/settings/ssh" -> launch (fun () -> post_settings_ssh session reqd)
  | `GET, "/analytics" -> launch (fun () -> analytics_page session reqd)
  | `GET, "/" -> launch (fun () -> home_page session reqd)
  | `GET, "/welcome" ->
      launch_user_route_or (fun () -> Http_code.fallback_welcome reqd)
  | `GET, "/background" ->
      launch_user_route_or (fun () ->
        Http_code.serve_public_body reqd ~title:"Background"
          "<p><a href=\"/welcome\">welcome</a> · author background page not seeded yet.</p>")
  | `GET, "/roadmap" ->
      launch_user_route_or (fun () ->
        Http_code.serve_public_body reqd ~title:"Roadmap"
          "<p><a href=\"/welcome\">welcome</a> · roadmap page not seeded yet.</p>")
  | `GET, "/signup" | `POST, "/signup" ->
      launch_user_route_or (fun () ->
        Http_code.serve_public_body reqd ~title:"Signup"
          "<p><a href=\"/welcome\">welcome</a> · signup page not seeded yet.</p>")
  | `GET, "/book" ->
      launch_user_route_or (fun () -> Http_code.fallback_book reqd)
  | `GET, "/book.pdf" ->
      launch (fun () ->
        Http_code.handle_public_artifact reqd ~name:"book.pdf";
        Lwt.return_unit)
  | `GET, "/src.tgz" ->
      launch (fun () ->
        Http_code.handle_public_artifact reqd ~name:"src.tgz";
        Lwt.return_unit)
  | `GET, "/code" ->
      launch (fun () ->
        Http_code.handle_code_page reqd ~target;
        Lwt.return_unit)
  | `GET, path when String.length path >= 10
                 && String.sub path 0 10 = "/code/raw/" ->
      let rel = String.sub path 10 (String.length path - 10) in
      launch (fun () ->
        Http_code.handle_raw reqd ~rel;
        Lwt.return_unit)
  | `GET, path when String.length path >= 11
                 && String.sub path 0 11 = "/assets/cm/" ->
      (* Back-compat alias: /assets/cm/* → static/codemirror/* *)
      let rel =
        "codemirror/" ^ String.sub path 11 (String.length path - 11)
      in
      launch (fun () ->
        Http_code.handle_asset reqd ~rel;
        Lwt.return_unit)
  | `GET, path when String.length path >= 8
                 && String.sub path 0 8 = "/assets/" ->
      let rel = String.sub path 8 (String.length path - 8) in
      launch (fun () ->
        Http_code.handle_asset reqd ~rel;
        Lwt.return_unit)
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
  (* Public agent bootstrap — no auth, no secrets. /agent stays private. *)
  | `GET, "/.well-known/agent.json" ->
      Agent_discovery.serve_agent_json reqd ~headers
  | `GET, "/agent.txt" ->
      Agent_discovery.serve_agent_txt reqd ~headers
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
  | `POST, "/api/media" -> launch (fun () -> api_media_post session reqd)
  | `GET, "/api/media" ->
      launch (fun () -> api_media_list session reqd ~target)
  | `DELETE, path when String.length path > 11
                    && String.sub path 0 11 = "/api/media/" ->
      let id = String.sub path 11 (String.length path - 11) in
      launch (fun () -> api_media_delete session reqd ~id)
  | `GET, "/media" | `GET, "/media/" ->
      launch (fun () -> media_page session reqd ~target)
  | `POST, "/media/upload" ->
      launch (fun () -> media_upload_post session reqd)
  | `POST, path when String.length path > 7
                  && String.sub path 0 7 = "/media/"
                  && String.ends_with ~suffix:"/delete" path ->
      let id =
        let rest = String.sub path 7 (String.length path - 7) in
        (* strip trailing /delete *)
        if String.length rest > 7 then
          String.sub rest 0 (String.length rest - 7)
        else rest
      in
      launch (fun () -> media_delete_post session reqd ~id)
  | `GET, path when String.length path > 7
                 && String.sub path 0 7 = "/media/" ->
      let id = String.sub path 7 (String.length path - 7) in
      launch (fun () -> media_get session reqd ~id)
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
         request-* prims expose method/params for branching. Route is
         resolved INSIDE launch (after the gate binds the per-request
         context) so a concurrent attach cannot flip the table under us. *)
      launch (fun () ->
      let route_opt =
        match Lwt.get Request_context.current with
        | Some ctx -> Request_context.lookup_route ctx path
        | None -> Session.lookup_route session path
      in
      match route_opt with
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
                         Httpun.Headers.get req.headers "content-type"
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
           page ()
       | None ->
           let headers = header_list req in
           if wants_fragment ~path ~headers then
             fragment_response reqd ~status:`Not_found
               ("no partial for " ^ path)
           else
             html_response reqd ~session ~status:`Not_found
               (chrome ~session ~title:"Not found" [
                  HTML.p [] [txt "no route for %s" path];
                ]);
           Lwt.return_unit)
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
    ?(request : Httpun.Request.t option)
    (err : Httpun.Server_connection.error)
    (respond : Httpun.Headers.t -> Httpun.Body.Writer.t) : unit =
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
    Httpun.Headers.of_list
      [("content-type", "text/html");
       ("content-length", string_of_int (String.length body_str))]
  in
  let body_w = respond headers in
  try
    Httpun.Body.Writer.write_string body_w body_str;
    safe_close_writer body_w
  with exn when is_closed_writer_exn exn -> ()

let serve ~(session : Session.t) ?(port = default_port) () : unit Lwt.t =
  let handler = request_handler session in
  (* httpun-lwt hands a Gluten-upgradable reqd wrapper; unwrap .reqd. *)
  let request_handler addr (g : Httpun.Reqd.t Gluten.reqd) : unit =
    handler addr g.Gluten.reqd
  in
  let inner_establisher =
    Httpun_lwt_unix.Server.create_connection_handler
      ~request_handler
      ~error_handler
  in
  (* image.http-robustness invariant 1 (defense-in-depth): any exception
     that escapes the inner establisher reaches Lwt.async_exception_hook and
     TERMINATES the process (the actual pricklypear.rocks death vector).
     Catch connection-level failures here so a gone peer or bad connection
     ends the request, not the image.

     NOTE: the inflight cap (invariant 5) is applied INSIDE [launch] in
     request_handler, not here. Keeping it at the connection level would
     gate /health (a cheap probe that bypasses launch) behind queued page
     work and wedge the liveness check under load. Gating inside launch
     bounds the expensive page-work queue while leaving /health and other
     hard-public probes always servable. *)
  let establisher sockaddr fd =
    Lwt.catch
      (fun () -> inner_establisher sockaddr fd)
      (fun exn ->
         if is_closed_writer_exn exn then
           ignore (Ops_log.info ~component:"http"
                     "connection handler: peer gone (closed writer)")
         else
           ignore (Ops_log.warn ~component:"http"
                     ~context:[("phase", "connection")]
                     ("connection handler failed: "
                      ^ Printexc.to_string exn));
         Lwt.return_unit)
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  Printf.eprintf
    "[http] serving htmx (httpun-lwt-unix) on 0.0.0.0:%d (max_inflight=%d)\n%!"
    port max_inflight;
  let* _server = Lwt_io.establish_server_with_client_socket addr establisher in
  let waiter, _waker = Lwt.wait () in
  waiter

(** Convert Eio stream sockaddr → Unix.sockaddr for shared request_handler. *)
let unix_sockaddr_of_eio (sa : Eio.Net.Sockaddr.stream) : Unix.sockaddr =
  match sa with
  | `Unix path -> Unix.ADDR_UNIX path
  | `Tcp (ip, port) ->
      let uip = Eio_unix.Net.Ipaddr.to_unix ip in
      Unix.ADDR_INET (uip, port)

(** Native Eio accept loop via Httpun_eio (H3). Call under Eio_main +
    ambient Io_eio.with_ctx. Blocks the fiber forever. *)
let serve_eio
    ~(session : Session.t)
    ~(env : Eio_unix.Stdenv.base)
    ~(sw : Eio.Switch.t)
    ?(port = default_port)
    () : unit =
  let handler = request_handler session in
  let request_handler (sa : Eio.Net.Sockaddr.stream)
      (g : Httpun.Reqd.t Gluten.reqd) : unit =
    handler (unix_sockaddr_of_eio sa) g.Gluten.reqd
  in
  let error_handler (sa : Eio.Net.Sockaddr.stream) =
    error_handler (unix_sockaddr_of_eio sa)
  in
  let conn_handler =
    Httpun_eio.Server.create_connection_handler
      ~request_handler ~error_handler ~sw
  in
  let net = Eio.Stdenv.net env in
  let listen_addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  (* reuse_port off: OpenBSD SO_REUSEPORT semantics differ and caused
     EADDRINUSE races on rcctl restart when dual-binding. reuse_addr is enough. *)
  let socket =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:false ~backlog:128 ~sw net
      listen_addr
  in
  Printf.eprintf
    "[http] serving htmx (httpun-eio) on 0.0.0.0:%d (max_inflight=%d)\n%!"
    port max_inflight;
  let on_error ex =
    if is_closed_writer_exn ex then
      ignore
        (Ops_log.info ~component:"http"
           "eio connection: peer gone (closed writer)")
    else
      ignore
        (Ops_log.warn ~component:"http"
           ~context:[("phase", "eio-connection")]
           ("eio connection handler failed: " ^ Printexc.to_string ex))
  in
  while true do
    Eio.Net.accept_fork socket ~sw ~on_error (fun client_sock client_addr ->
        try conn_handler client_addr client_sock
        with exn -> on_error exn)
  done
