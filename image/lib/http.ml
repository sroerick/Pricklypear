(** Pricklypear Image — HTTP server entry: route dispatch + serve.

    System pages + browser product surfaces:
      /  /health  /branches  /bindings  /types  /data …
      /timing  /repl  /api/eval  /edit  /inspect  /agent
      public agent bootstrap: /.well-known/agent.json  /agent.txt
      user routes via (route ...)
*)
open Request_context
open Pure_html
open Http_core
open Http_data
open Http_branches
open Http_agent
open Http_pages
open Http_ops
open Http_media

(* Route POST body cap: multipart/form-data may carry media files for
   package upload flows (e.g. music) up to the audio ceiling plus
   framing slack; every other body keeps the 64 KiB form cap.
   Handlers parse these bodies via (request-multipart) and store the
   file part via (media-put-raw). *)
let route_body_cap (content_type : string) : int =
  let ct = String.lowercase_ascii (String.trim content_type) in
  if String.starts_with ~prefix:"multipart/form-data" ct
  then Media.max_http_audio_bytes + 65536
  else max_user_body_bytes

let request_handler ~(sw : Eio.Switch.t) ~(registry : Session_registry.t)
    (session : Session.t) (sockaddr : Unix.sockaddr)
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
  (* i-1787971251: client source_ip for auth_log rows (login/token use). *)
  let source_ip = source_ip_of_sockaddr sockaddr in
  (* Fork page work off the connection fiber (gluten_eio runs request_handler
     on the same fiber as the read loop). Awaiting body reads without forking
     deadlocks: schedule_read callbacks never fire. Same role as the old
     Lwt.async launch under httpun-lwt.

     image.http-robustness invariant 5: bound the expensive page-work queue
     with the inflight cap so a traffic spike can't accumulate unbounded
     queued work (memory/fds). Acquired here (inside launch) so cheap probes
     like /health that bypass launch are NEVER blocked by page traffic —
     liveness must stay servable under load. Released in finalize so a slot
     is freed whether page_fn succeeded, failed, or the peer went away. *)
  (* page_fn receives the SELECTED session (image.per-identity-sessions):
     the identity's own Session.t, never the shared boot session. *)
  let launch (page_fn : Session.t -> unit) : unit =
    Eio.Fiber.fork ~sw (fun () ->
    let () = inflight_acquire () in
    Fun.protect ~finally:(fun () -> inflight_release ()) (fun () ->
      (try
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
              let snapshot_ctx (session : Session.t) =
                Request_context.make
                  ~manifest:(Session.manifest session)
                  ~env:(Session.env session)
                  ~routes:(Request_context.copy_routes (Session.routes session))
              in
              (* SELECT the identity's session, then attach on IT.
                 image.per-identity-sessions: attach selects, never
                 mutates another identity's session; anonymous traffic
                 runs on the parked anonymous session. *)
              (* PP_EVAL_TRACE phase tracing (tmp/eval-fast-path): attribute
                 per-request wall time + DB statements to attach phases. *)
              let trace = Sys.getenv_opt "PP_EVAL_TRACE" <> None in
              let phase (name : string) (t0 : float) (c0 : int) (m0 : float) : float =
                if not trace then Unix.gettimeofday ()
                else begin
                  let t1 = Unix.gettimeofday () in
                  let c1 = Atomic.get Store_db.trace_count in
                  let m1 = Atomic.get Store_db.trace_ms in
                  Printf.eprintf "[eval-trace] %-18s %7.1f ms  db %2d stmts %7.1f ms\n%!"
                    name ((t1 -. t0) *. 1000.0) (c1 - c0) (m1 -. m0);
                  t1
                end
              in
              let attach_and_select () =
                let t0 = Unix.gettimeofday () in
                let c0 = Atomic.get Store_db.trace_count in
                let m0 = Atomic.get Store_db.trace_ms in
                let user_opt =
                  resolve_identity session ~headers ~source_ip ()
                in
                let t1 = phase "resolve_identity" t0 c0 m0 in
                let session = Session_registry.select registry ~user:user_opt in
                let () =
                  match user_opt with
                  | Some user ->
                      let tok = cookie_value headers cookie_name in
                      attach_from_user session user ~auth_token:tok
                        ~checkout_personal_now:false ()
                  | None -> Session.attach_anonymous session
                in
                let t2 = phase "registry+attach" t1 (Atomic.get Store_db.trace_count) (Atomic.get Store_db.trace_ms) in
                let c2 = Atomic.get Store_db.trace_count in
                let m2 = Atomic.get Store_db.trace_ms in
                Session.ensure_fresh session;
                let _ = phase "ensure_fresh" t2 c2 m2 in
                (session, user_opt)
              in
              let proceed_ctx =
                Eio.Mutex.use_ro request_mutex (fun () ->
                  if auth_disabled () then begin
                    (* Dev escape: still try attach so whoami is real if cookie. *)
                    let session, _ = attach_and_select () in
                    (Some (session, snapshot_ctx session))
                  end else begin
                    (* Attach identity BEFORE classify: route table is
                       branch-scoped (inheritance CTE). Anonymous uses
                       system/main so package public routes resolve. *)
                    let session, user_opt = attach_and_select () in
                    let classif = classify_path session path in
                    let needs_auth =
                      match classif with
                      | `Hard_public -> false
                      | `Hard_private | `Unknown -> true
                      | `User (_, vis) -> vis <> "public"
                    in
                    match user_opt, needs_auth with
                    | Some _, _ -> (Some (session, snapshot_ctx session))
                    | None, false -> (Some (session, snapshot_ctx session))
                    | None, true ->
                        let apiish =
                          String.length path >= 4
                          && String.sub path 0 4 = "/api"
                        in
                        if apiish then unauthorized_api reqd
                        else require_login_redirect reqd ~path ~target;
                        None
                  end)
              in
              (match proceed_ctx with
               | None -> ()
               | Some (session, req_ctx) ->
              Request_context.with_current req_ctx @@ fun () ->
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
                   | Host_garden _, Some _ when uname_now <> "anonymous" ->
                       (* https only: a plain-http bounce drops the Secure
                          session cookie and loops the user through login
                          (same rule as the post-login garden redirect,
                          garden_url comment in http_core). i-1788670736. *)
                       redirect_response reqd
                         (garden_url ~username:uname_now ~path:"/")
                   | Host_unknown, _ ->
                       text_response reqd ~status:`Not_found
                         ~content_type:"text/plain" "unknown host"
                   | _ -> redirect_response reqd "/");
                  ()
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
                   by home, system pages, and user routes — not only /.
                   image.boot-request-cost: /api/* never renders chrome, so
                   skip the ui/nav + ui/css eval there entirely (agents
                   hitting POST /api/eval never need the shell). *)
                let () =
                  if String.length path >= 4
                     && String.sub path 0 4 = "/api"
                  then ()
                  else Session.refresh_ui_shell session
                in
                (* GATE COMPLETE — request_mutex released above. page_fn runs
                   lock-free (invariant 4b) on the SELECTED session. *)
                let tgate0 = Unix.gettimeofday () in
                let cgate0 = Atomic.get Store_db.trace_count in
                let mgate0 = Atomic.get Store_db.trace_ms in
                page_fn session;
                ignore (phase "page_fn (handler)" tgate0 cgate0 mgate0)
                end
              )
    with exn ->
      (* image.http-robustness invariant 2: if the handler failed
               because the peer went away (closed writer), do NOT try to
               render a 500 page onto the same dead writer — that re-throw
               was the actual crash mechanism. Just close quietly and end. *)
            if is_closed_writer_exn exn then begin
              ignore (Ops_log.info ~component:"http"
                        ~context:[("path", path)]
                        "peer gone (closed writer); skipping 500");
              ()
            end else if
              (match exn with Http_core.Body_too_large -> true | _ -> false)
            then begin
              (* Oversized request body (PP_MAX_BODY_BYTES): a 413, not a
                 500 — the client, not the server, is at fault. *)
              text_response reqd ~status:`Payload_too_large
                ~content_type:"text/plain" "request body too large";
              ()
            end else begin
            (* Log the real exception to ops; never echo raw exception text
               back to the client (500 bodies are generic — incident is
               correlated server-side via ops log). *)
            Ops_incidents.emit_and_maybe_file session
              ~level:Ops_log.Error ~component:"http"
              ~file:(not (Ops_incidents.is_expected_runtime_error exn))
              ~context:[("path", path)] (Printexc.to_string exn);
            let body =
              chrome ~session ~title:"Error" [
                HTML.p [HTML.class_ "err"]
                  [txt "internal server error — incident logged"];
              ]
            in
            html_response reqd ~session ~status:`Internal_server_error body;
            ()
            end)))
  in
  (** habitat-book.web-renderer: kernel-dispatched book handlers
      (book/page, book/chapter-page, book/code-page in libs/book/lib.pp).
      Same shape as the /book/<pkg> fallthrough: resolve the handler
      INSIDE page work (post-gate per-request context), zero-arg call,
      params ride the request record. No route pins — the kernel owns
      the /book namespace dispatch (spec keeps its /book route pin for
      the unbound-book fallback below). *)
  let launch_book_handler (handler : string) : unit =
    launch (fun session ->
        let request : Session.request =
          { method_ = `GET
          ; path
          ; params = parse_query target
          ; body = ""
          ; headers = header_list req
          }
        in
        match Session.call_handler session ~request handler with
        | Session.Handler_ok value ->
            respond_handler_value ~session ~reqd ~path ~fragment:false value
        | Session.Handler_unbound ->
            text_response reqd ~status:`Not_found ~content_type:"text/plain"
              (String.concat " "
                 [ handler; "unbound — (load-library \"book\")" ])
        | Session.Handler_error msg ->
            text_response reqd ~status:`Internal_server_error
              ~content_type:"text/plain" msg);
    ()
  in
  (* habitat-book.eval-examples: run-button target for nopales example
     blocks. htmx posts the tree-row path (hx-vals); we read the row,
     eval its src via the RESTRICTED example engine
     (Session.example_eval_engine — whitelisted pure + read-only prims,
     static gate, child of the session env, fresh budget), and return a
     replacement .book-example div (hx-target closest, hx-swap
     outerHTML) showing value/error + elapsed. Kernel-dispatched like
     the /book GET surface — no route pins in the /book namespace. *)
  let post_book_eval_example (session : Session.t) : unit =
    let body = read_body reqd in
    let row_path =
      Option.value (form_field body "path") ~default:"" |> String.trim
    in
    let fail ?(status = `Bad_request) msg =
      text_response reqd ~status ~content_type:"text/plain" msg
    in
    let esc = Prims_pure.html_escape_str in
    if row_path = "" then fail "path required"
    else if not (String.starts_with ~prefix:"data.book." row_path) then
      fail "not a book row path"
    else
      match Crud.tree_get (Session.store session) ~path:row_path () with
      | Pricklypear_lisp.Ast.VNil ->
          fail ~status:`Not_found ("no such book row: " ^ row_path)
      | Pricklypear_lisp.Ast.VStr json -> (
          match Yojson.Safe.from_string json with
          | `Assoc fields ->
              (* tree/get rows wrap the payload under "body" (see
                 book/row-body in libs/book/lib.pp); fall back to the
                 top level for flat rows. *)
              let fields =
                match List.assoc_opt "body" fields with
                | Some (`Assoc body) -> body
                | _ -> fields
              in
              let f k =
                match List.assoc_opt k fields with
                | Some (`String s) -> s
                | _ -> ""
              in
              let lang = f "lang" and src = f "src" and expected = f "expected" in
              if lang <> "nopales" then
                fail "captured, not live (ocaml examples do not eval in v1)"
              else if src = "" then fail "example row has no src"
              else
                let t0 = Unix.gettimeofday () in
                let elapsed () =
                  Printf.sprintf "%.0f ms"
                    ((Unix.gettimeofday () -. t0) *. 1000.0)
                in
                let result_html =
                  match Session.example_eval_engine session src with
                  | v ->
                      Printf.sprintf
                        "<div class=\"book-result\">⇒ <code>%s</code> <span class=\"book-elapsed\">(%s)</span></div>"
                        (esc (Pricklypear_lisp.Reader.pp_value v))
                        (elapsed ())
                  | exception Example_eval.Refused reason ->
                      Printf.sprintf
                        "<div class=\"book-error\">refused: %s <span class=\"book-elapsed\">(%s)</span></div>"
                        (esc reason) (elapsed ())
                  | exception exn ->
                      Printf.sprintf
                        "<div class=\"book-error\">error: %s <span class=\"book-elapsed\">(%s)</span></div>"
                        (esc (Printexc.to_string exn))
                        (elapsed ())
                in
                let expected_html =
                  if expected = "" then ""
                  else
                    Printf.sprintf
                      "<p class=\"book-expected\">expect =&gt; %s</p>"
                      (esc expected)
                in
                let html =
                  Printf.sprintf
                    "<div class=\"book-example\"><pre><code>%s</code></pre>%s%s</div>"
                    (esc src) result_html expected_html
                in
                text_response reqd ~content_type:"text/html" html
          | _ -> fail "bad book row JSON"
          | exception _ -> fail "bad book row JSON")
      | _ -> fail "bad book row"
  in
  (** Lisp user-route if bound; else kernel [fallback]. *)
  let launch_user_route_or (fallback : unit -> unit) : unit =
    (* Resolve the route INSIDE page work, after the gate has attached
       identity and bound the per-request context (image.per-request-context).
       Pre-gate the routes table could be whatever a previous request left
       behind (e.g. anonymous attach -> system/main), causing a personal-branch
       route 404 before THIS request's attach even runs. *)
    launch (fun session ->
      let route_opt =
        match Request_context.get () with
        | Some ctx -> Request_context.lookup_route ctx path
        | None -> Session.lookup_route session path
      in
      match route_opt with
      | None ->
          fallback ();
          ()
      | Some handler_name ->
          let page () : unit =
            let body_cap =
              route_body_cap
                (match Httpun.Headers.get req.headers "content-type" with
                 | Some s -> s
                 | None -> "")
            in
            let body_str =
              match meth with
              | `POST -> read_body reqd ~max_bytes:body_cap
              | _ -> ""
            in
          if String.length body_str > body_cap then begin
            text_response reqd ~status:`Payload_too_large
              ~content_type:"text/plain"
              "request body too large";
            ()
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
            let result, _total_ms =
              Timing.measure ~route:handler_name
                (fun () -> Session.call_handler session ~request handler_name)
            in
            (match result with
             | Session.Handler_ok value ->
                 (* Public front-door pages: no product chrome (private nav). *)
                 if path = "/welcome"
                    || path = "/background" || path = "/roadmap"
                    || path = "/signup"
                    || path = "/guestbook"
                 then begin
                   let title =
                     if path = "/background" then "Background"
                     else if path = "/roadmap" then "Roadmap"
                     else if path = "/signup" then "Signup"
                     else if path = "/guestbook" then "Guestbook"
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
                   ()
                 end else begin
                   respond_handler_value ~session ~reqd ~path ~fragment value;
                   ()
                 end
             | Session.Handler_unbound ->
                 fallback ();
                 ()
             | Session.Handler_error msg ->
                 if fragment then
                   fragment_response reqd ~status:`Internal_server_error
                     ("handler " ^ handler_name ^ ": " ^ msg)
                 else
                   html_response reqd ~session ~status:`Internal_server_error
                     (chrome ~session ~title:"Route handler error" [
                        HTML.h1 [] [txt "Route handler error"];
                        HTML.p []
                          [txt "handler %s" handler_name];
                        HTML.pre [] [txt "%s" msg];
                      ]);
                 ())
          end
        in
        page ())
  in
  match meth, path with
  | `GET, "/login" ->
      launch (fun session ->
        let () = () in
         login_form_page reqd;
         ())
  | `POST, "/login" ->
      launch (fun session -> post_login session ~source_ip reqd)
  | `POST, "/logout" -> launch (fun session -> post_logout session reqd)
  | `GET, "/admin/users" -> launch (fun session -> admin_users_page session reqd)
  | `POST, "/admin/users" -> launch (fun session -> post_admin_users session reqd)
  | `GET, "/settings/keys" -> launch (fun session -> settings_keys_page session reqd)
  | `POST, "/settings/keys" -> launch (fun session -> post_settings_keys session reqd)
  | `GET, "/settings/ssh" -> launch (fun session -> settings_ssh_page session reqd)
  | `POST, "/settings/ssh" -> launch (fun session -> post_settings_ssh session reqd)
  | `GET, "/analytics" -> launch (fun session -> analytics_page session reqd)
  | `GET, "/" -> launch (fun session -> home_page session reqd)
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
  | `GET, "/guestbook" | `POST, "/guestbook" ->
      launch_user_route_or (fun () ->
        Http_code.serve_public_body reqd ~title:"Guestbook"
          "<p><a href=\"/welcome\">welcome</a> · guestbook page not seeded yet.</p>")
  | `GET, "/book" ->
      (* habitat-book.web-renderer: the book package's merged index (borg
         chapters + package spec docs) when book/page is bound; else the
         /book route pin (spec/book-page, habitat-workflow.book-view);
         else the static release face. A handler ERROR is surfaced (500),
         never masked by falling through. *)
      launch (fun session ->
          let request : Session.request =
            { method_ = `GET
            ; path
            ; params = parse_query target
            ; body = ""
            ; headers = header_list req
            }
          in
          let serve_handler handler_name =
            match Session.call_handler session ~request handler_name with
            | Session.Handler_ok value ->
                respond_handler_value ~session ~reqd ~path ~fragment:false value;
                true
            | Session.Handler_unbound -> false
            | Session.Handler_error msg ->
                text_response reqd ~status:`Internal_server_error
                  ~content_type:"text/plain" msg;
                true
          in
          let served =
            serve_handler "book/page"
            ||
            (match
               (match Request_context.get () with
                | Some ctx -> Request_context.lookup_route ctx path
                | None -> Session.lookup_route session path)
             with
             | Some handler_name -> serve_handler handler_name
             | None -> false)
          in
          if not served then Http_code.fallback_book reqd;
          ())
  | `GET, path
    when String.length path > 14 && String.sub path 0 14 = "/book/chapter/" ->
      (* habitat-book.web-renderer: borg chapter pages over the seeded
         tree rows (scripts/load-book.sh). Prefix is 14 bytes; >14 means
         a nonempty label follows. *)
      launch_book_handler "book/chapter-page"
  | `GET, path
    when String.length path > 11 && String.sub path 0 11 = "/book/code/" ->
      (* habitat-book.web-renderer: one code-chapter listing. *)
      launch_book_handler "book/code-page"
  | `GET, path
    when String.length path > 13 && String.sub path 0 13 = "/book/course/" ->
      (* habitat-book.course-documents: notes/outline course docs (slug
         course/<suffix>, kind course) assembled into an ordered reading
         list over the seeded chapter rows — compose, never copy. *)
      launch_book_handler "book/course-page"
  | `POST, "/book/eval-example" ->
      launch (fun session -> post_book_eval_example session);
      ()
  | `GET, "/book/verify" ->
      (* habitat-book.eval-examples: (book/verify) as the book's
         self-test — re-evals every nopales example row with an
         `expect` trailer; mismatch/refusal renders as a hole row
         (drift-as-rendering). ?chapter=<slug> scopes to one chapter. *)
      launch_book_handler "book/verify-page"
  | `GET, path
    when String.length path > 6 && String.sub path 0 6 = "/book/" ->
      (* habitat-workflow.book-view: /book/<pkg> → (spec/book-page).
         request-path carries the full path so Nopales can extract pkg.
         habitat-book.web-renderer keeps /book/pkg/<pkg> as an alias:
         the effective path strips the pkg/ segment. *)
      let path =
        if
          String.length path > 10 && String.sub path 0 10 = "/book/pkg/"
        then "/book/" ^ String.sub path 10 (String.length path - 10)
        else path
      in
      launch (fun session ->
          let request : Session.request =
            { method_ = `GET
            ; path
            ; params = parse_query target
            ; body = ""
            ; headers = header_list req
            }
          in
          match Session.call_handler session ~request "spec/book-page" with
          | Session.Handler_ok value ->
              respond_handler_value ~session ~reqd ~path ~fragment:false value
          | Session.Handler_unbound ->
              text_response reqd ~status:`Not_found ~content_type:"text/plain"
                "spec/book-page unbound — (load-library \"spec\")"
          | Session.Handler_error msg ->
              text_response reqd ~status:`Internal_server_error
                ~content_type:"text/plain" msg);
      ()
  | `GET, "/book.pdf" ->
      launch (fun session ->
        Http_code.handle_public_artifact reqd ~name:"book.pdf";
        ())
  | `GET, "/src.tgz" ->
      launch (fun session ->
        Http_code.handle_public_artifact reqd ~name:"src.tgz";
        ())
  | `GET, "/code" ->
      launch (fun session ->
        Http_code.handle_code_page reqd ~target;
        ())
  | `GET, path when String.length path >= 10
                 && String.sub path 0 10 = "/code/raw/" ->
      let rel = String.sub path 10 (String.length path - 10) in
      launch (fun session ->
        Http_code.handle_raw reqd ~rel;
        ())
  | `GET, path when String.length path >= 11
                 && String.sub path 0 11 = "/assets/cm/" ->
      (* Back-compat alias: /assets/cm/* → static/codemirror/* *)
      let rel =
        "codemirror/" ^ String.sub path 11 (String.length path - 11)
      in
      launch (fun session ->
        Http_code.handle_asset reqd ~rel;
        ())
  | `GET, path when String.length path >= 8
                 && String.sub path 0 8 = "/assets/" ->
      let rel = String.sub path 8 (String.length path - 8) in
      launch (fun session ->
        Http_code.handle_asset reqd ~rel;
        ())
  | `GET, "/about" ->
      launch (fun session ->
        let () = () in
         about_page session reqd;
         ())
  | `GET, "/pages" ->
      launch (fun session ->
        let () = () in
         pages_list_page session reqd;
         ())
  | `POST, "/api/recipe/hello" ->
      launch (fun session ->
        let res = Session.recipe_hello session in
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
             ()
         | Error e ->
             let body =
               chrome ~session ~title:"Hello recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             ())
  | `POST, "/api/recipe/todo" ->
      launch (fun session ->
        let res = Session.recipe_todo session in
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
             ()
         | Error e ->
             let body =
               chrome ~session ~title:"Todo recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             ())
  | `POST, "/api/recipe/notes" ->
      launch (fun session ->
        let res = Session.recipe_notes session in
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
             ()
         | Error e ->
             let body =
               chrome ~session ~title:"Notes recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             ())
  | `POST, "/api/recipe/dabar" ->
      launch (fun session ->
        let res = Session.recipe_dabar session in
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
             ()
         | Error e ->
             let body =
               chrome ~session ~title:"Dabar recipe failed" [
                 HTML.p [HTML.class_ "err"] [txt "%s" e];
                 HTML.p [] [HTML.a [HTML.href "/"] [txt "Home"]];
               ]
             in
             html_response reqd ~session ~status:`Internal_server_error body;
             ())
  | `GET, "/health" ->
      (* image.http-robustness invariant 7: /health must probe the same
         store path auth/prims use. Memory-only ok:true hid Eio mutex
         poison and locked out all logins while watchdog saw green. *)
      let branch = Session.branch_name session in
      let uptime = Session.uptime_seconds session in
      let boot_epoch = Session.boot_epoch session in
      let check = Store.health_check (Session.store session) in
      let h, status =
        match check with
        | Ok () ->
            ( { Pricklypear_common.Protocol.ok = true;
                branch;
                uptime_seconds = uptime;
                boot_epoch;
                error = None },
              `OK )
        | Error msg ->
            ( { Pricklypear_common.Protocol.ok = false;
                branch;
                uptime_seconds = uptime;
                boot_epoch;
                error = Some msg },
              `Service_unavailable )
      in
      let j = Pricklypear_common.Protocol.health_response_to_json h in
      text_response reqd ~status ~content_type:"application/json"
        (Yojson.Safe.pretty_to_string j)
  (* Public agent bootstrap — no auth, no secrets. /agent stays private. *)
  | `GET, "/.well-known/agent.json" ->
      Agent_discovery.serve_agent_json reqd ~headers
  | `GET, "/agent.txt" ->
      Agent_discovery.serve_agent_txt reqd ~headers
  | `GET, "/branches" -> launch (fun session -> branches_page session reqd)
  | `GET, "/changes" -> launch (fun session -> changes_page session reqd)
  | `POST, "/branches/checkout" -> launch (fun session -> branches_checkout session reqd)
  | `POST, "/branches/fork" -> launch (fun session -> branches_fork session reqd)
  | `POST, "/branches/unoverride" -> launch (fun session -> branches_unoverride session reqd)
  | `POST, "/branches/promote" -> launch (fun session -> branches_promote session reqd)
  | `GET, "/bindings" -> launch (fun session -> list_bindings_page session reqd)
  | `GET, "/diff" -> launch (fun session -> diff_page session reqd)
  | `GET, "/types" -> launch (fun session -> list_types_page session reqd)
  | `GET, "/timing" -> launch (fun session -> timing_page session reqd)
  | `GET, "/data" | `GET, "/data/" ->
      launch (fun session -> data_index_page session reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/data/" ->
      let rest = String.sub path 6 (String.length path - 6) in
      let segs = String.split_on_char '/' rest |> List.filter ((<>) "") in
      (match segs with
       | [ty; "new"] ->
           launch (fun session -> data_new_page session ty reqd)
       | [ty; id; "delete"] ->
           (* GET delete not supported — point at detail *)
           launch (fun session -> data_detail_page session ty id reqd)
       | [ty; id] ->
           launch (fun session -> data_detail_page session ty id reqd)
       | [ty] ->
           launch (fun session -> data_list_page session ty reqd)
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
           launch (fun session -> data_delete session ty id reqd)
       | [ty; id] ->
           launch (fun session -> data_update session ty id reqd)
       | [ty] ->
           launch (fun session -> data_create session ty reqd)
       | _ ->
           html_response reqd ~session ~status:`Bad_request
             (chrome ~session ~title:"Bad data path" [
                HTML.p [] [txt "POST /data/<type> | /data/<type>/<id> | …/delete"];
              ]))
  | `GET, "/tree" -> launch (fun session -> Http_tree.tree_page session reqd)
  | `GET, "/partials/tree" ->
      launch (fun session -> Http_tree.tree_partial session reqd)
  | `POST, "/tree/edit" -> launch (fun session -> Http_tree.tree_edit session reqd)
  | `GET, "/repl" ->
      launch (fun session ->
        let () = () in
         repl_page session reqd;
         ())
  | `POST, "/api/eval" -> launch (fun session -> api_eval session reqd)
  | `POST, "/api/prim" -> launch (fun session -> api_prim session reqd)
  | `POST, "/api/media" -> launch (fun session -> api_media_post session reqd)
  | `GET, "/api/media" ->
      launch (fun session -> api_media_list session reqd ~target)
  | `DELETE, path when String.length path > 11
                    && String.sub path 0 11 = "/api/media/" ->
      let id = String.sub path 11 (String.length path - 11) in
      launch (fun session -> api_media_delete session reqd ~id)
  | `GET, "/media" | `GET, "/media/" ->
      launch (fun session -> media_page session reqd ~target)
  | `POST, "/media/upload" ->
      launch (fun session -> media_upload_post session reqd)
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
      launch (fun session -> media_delete_post session reqd ~id)
  | `GET, path when String.length path > 7
                 && String.sub path 0 7 = "/media/" ->
      let id = String.sub path 7 (String.length path - 7) in
      launch (fun session -> media_get session reqd ~id)
  | `POST, "/api/agent" -> launch (fun session -> api_agent session reqd)
  | `POST, "/api/agent-task" -> launch (fun session -> api_agent_task session reqd)
  | `POST, "/repl-v2/llm" -> launch (fun session -> repl_llm session reqd)
  | `POST, "/api/ai/propose" -> launch (fun session -> ai_propose session reqd)
  | `GET, "/agent" -> launch (fun session -> agent_page session reqd)
  | `POST, "/api/edit/create" -> launch (fun session -> api_edit_create session reqd)
  | `POST, path when String.length path >= 13
                  && String.sub path 0 13 = "/api/analyze/" ->
      let name =
        let rest = String.sub path 13 (String.length path - 13) in
        let rest =
          match String.index_opt rest '?' with
          | None -> rest
          | Some i -> String.sub rest 0 i
        in
        Packages.name_decode rest
      in
      launch (fun session -> Http_analyze.post_analyze session name reqd)
  | `POST, path when String.length path >= 13
                  && String.sub path 0 13 = "/api/explain/" ->
      let name =
        let rest = String.sub path 13 (String.length path - 13) in
        let rest =
          match String.index_opt rest '?' with
          | None -> rest
          | Some i -> String.sub rest 0 i
        in
        Packages.name_decode rest
      in
      launch (fun session -> Http_analyze.post_explain session name reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/page/" ->
      let seg = String.sub path 6 (String.length path - 6) in
      launch (fun session -> page_inspector session seg reqd)
  | `GET, "/page" | `GET, "/page/" ->
      launch (fun session -> page_inspector session "" reqd)
  | `GET, path when String.length path >= 17
                 && String.sub path 0 17 = "/inspect/binding/" ->
      let name =
        Packages.name_decode (String.sub path 17 (String.length path - 17))
      in
      launch (fun session -> inspect_binding_page session name reqd)
  | `GET, path when String.length path >= 13
                 && String.sub path 0 13 = "/inspect/row/" ->
      let rest = String.sub path 13 (String.length path - 13) in
      (match String.split_on_char '/' rest with
       | [ty; id] -> launch (fun session -> inspect_row_page session ty id reqd)
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
      launch (fun session -> show_binding_page session name reqd)
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
      else launch (fun session -> editor_page session name reqd)
  | `GET, path when String.length path >= 6
                 && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (fun session -> editor_page session name reqd)
  | `POST, path when String.length path >= 6
                  && String.sub path 0 6 = "/edit/" ->
      let name =
        Packages.name_decode (String.sub path 6 (String.length path - 6))
      in
      launch (fun session -> save_edit session name reqd)
  | ((`GET | `POST) as umeth), _ ->
      (* User routes: GET + POST (it33). Same path → same handler;
         request-* prims expose method/params for branching. Route is
         resolved INSIDE launch (after the gate binds the per-request
         context) so a concurrent attach cannot flip the table under us. *)
      launch (fun session ->
      let route_opt =
        match Request_context.get () with
        | Some ctx -> Request_context.lookup_route ctx path
        | None -> Session.lookup_route session path
      in
      match route_opt with
       | Some handler_name ->
           let page () : unit =
             let body_cap =
               route_body_cap
                 (match Httpun.Headers.get req.headers "content-type" with
                  | Some s -> s
                  | None -> "")
             in
             let body_str =
               match umeth with
               | `POST -> read_body reqd ~max_bytes:body_cap
               | `GET -> ""
             in
             if String.length body_str > body_cap then begin
               text_response reqd ~status:`Payload_too_large
                 ~content_type:"text/plain"
                 "request body too large";
               ()
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
               let result, _total_ms =
                 Timing.measure ~route:handler_name
                   (fun () -> Session.call_handler session ~request handler_name)
               in
               (match result with
                | Session.Handler_ok value ->
                    respond_handler_value ~session ~reqd ~path ~fragment value;
                    ()
                | Session.Handler_unbound ->
                    if fragment then begin
                      fragment_response reqd ~status:`Internal_server_error
                        ("handler " ^ handler_name ^ " unbound");
                      ()
                    end else
                      let snap = Call_graph.snapshot_of_session session in
                      let callers = Call_graph.callers snap handler_name in
                      let caller_nodes =
                        if callers = [] then [txt "(none yet)"]
                        else
                          List.concat_map
                            (fun n ->
                              [ HTML.a
                                  [HTML.href "%s" (Packages.binding_href n)]
                                  [txt "%s" n]
                              ; txt " "
                              ])
                            callers
                      in
                      html_response reqd ~session ~status:`Not_found
                        (chrome ~session ~title:"Handler unbound" [
                           HTML.div [HTML.class_ "card"] [
                             HTML.h1 [] [txt "Handler unbound"];
                             HTML.p [] [
                               txt "Route ";
                               HTML.code [] [txt "%s" path];
                               txt " points at ";
                               HTML.code [] [txt "%s" handler_name];
                               txt " which is not bound.";
                             ];
                             HTML.p [] [
                               HTML.a
                                 [ HTML.href "%s"
                                     (Packages.edit_href handler_name)
                                 ; HTML.class_ "btn" ]
                                 [txt "Create stub %s" handler_name];
                               txt " ";
                               HTML.a
                                 [ HTML.href "%s"
                                     (Packages.binding_href handler_name)
                                 ; HTML.class_ "btn secondary" ]
                                 [txt "Inspect"];
                             ];
                             HTML.p []
                               ([ HTML.span [HTML.class_ "muted"]
                                    [txt "static callers: "] ]
                                @ caller_nodes);
                           ];
                         ]);
                      ()
                | Session.Handler_error msg ->
                    if fragment then
                      fragment_response reqd ~status:`Internal_server_error
                        ("handler " ^ handler_name ^ ": " ^ msg)
                    else
                      html_response reqd ~session ~status:`Internal_server_error
                        (chrome ~session ~title:"Route handler error" [
                           HTML.h1 [] [txt "Route handler error"];
                           HTML.p [] [txt "handler %s" handler_name];
                           HTML.pre [] [txt "%s" msg];
                         ]);
                    ())
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
           ())
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
    (* Never echo raw exception text to the client — generic body only. *)
    | `Exn _ -> `Internal_server_error, "internal server error — incident logged"
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
    ~(registry : Session_registry.t)
    ~(env : Eio_unix.Stdenv.base)
    ~(sw : Eio.Switch.t)
    ?(on_bound : unit -> unit = fun () -> ())
    ?(port = default_port)
    () : unit =
  let request_handler (sa : Eio.Net.Sockaddr.stream)
      (g : Httpun.Reqd.t Gluten.reqd) : unit =
    request_handler ~sw ~registry session (unix_sockaddr_of_eio sa) g.Gluten.reqd
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
  (* promote-window task 6 (2026-09-04): residual teardown races used to kill
     the new image with a fatal EADDRINUSE (old image still holding :18080
     when the new one execs). rc_stop's port-free poll narrows the race but
     cannot make it impossible; retry the bind for 10s (0.5s cadence, log
     every ~2s) so the race self-heals. After the budget the exception
     propagates (fatal, as before) and the watchdog owns recovery. *)
  let bind_retry_budget = 10.0 and bind_retry_interval = 0.5 in
  let addr_in_use = function
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) -> true
    (* eio 1.x wraps bind errors:
       Eio.Exn.Io (Eio.Exn.X (Eio_unix.Unix_error (EADDRINUSE, ...)), _) *)
    | Eio.Exn.Io
        (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _) ->
        true
    | _ -> false
  in
  let deadline = Unix.gettimeofday () +. bind_retry_budget in
  let rec listen_retry attempts () =
    match
      Eio.Net.listen ~reuse_addr:true ~reuse_port:false ~backlog:128 ~sw net
        listen_addr
    with
    | socket -> socket
    | exception e
      when addr_in_use e && Unix.gettimeofday () < deadline ->
        let attempts = attempts + 1 in
        if attempts = 1 || attempts mod 4 = 0 then begin
          let msg =
            Printf.sprintf "[http] :%d in use, bind retry %d (budget %.0fs)"
              port attempts bind_retry_budget
          in
          Printf.eprintf "%s\n%!" msg;
          ignore
            (Ops_log.warn ~component:"http"
               ~context:[ ("phase", "bind-retry") ]
               msg)
        end;
        Eio.Time.sleep (Eio.Stdenv.clock env) bind_retry_interval;
        listen_retry attempts ()
  in
  let socket = listen_retry 0 () in
  (* promote-window task 7 (2026-09-04): boot-phase hook — fires after the
     listen socket is bound (possibly after bind retries), before the accept
     loop starts. Optional, no-op default, so the dependency direction
     http.ml ← boot.ml is preserved. *)
  on_bound ();
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
