(** Pricklypear Image — HTTP server entry: route dispatch + serve.

    System pages + browser product surfaces:
      /  /health  /branches  /bindings  /types  /data …
      /timing  /repl  /api/eval  /edit  /inspect  /agent
      public agent bootstrap: /.well-known/agent.json  /agent.txt
      user routes via (route ...)

    Dispatch is a ROUTER TABLE ([routes] below): one ordered list of
    (methods, pattern, guard, handler) entries scanned first-match-wins,
    with the GET/POST user-route catch-all and the any-method not-found
    entry last. Replaces the former ~80-arm match with ~20 hand-rolled
    String.length/String.sub prefix guards (i-1788671135-a). Every
    response that renders session chrome goes through [launch]; only
    /health and the public agent bootstrap answer directly (liveness
    probes must never queue behind page work — image.http-robustness
    invariant 5). *)
open Request_context
open Pure_html
open Http_core
open Http_data
open Http_branches
open Http_agent
open Http_pages
open Http_ops
open Http_media

(* === Router plumbing === *)

(** One request's immutable dispatch environment, built in
    request_handler and handed to the matching route handler. [session]
    is the shared boot session: [launch]'s gate and the /health probe
    read it; page work receives the SELECTED per-identity session
    (image.per-identity-sessions) through launch instead. *)
type route_env = {
  sw : Eio.Switch.t;
  registry : Session_registry.t;
  session : Session.t;
  reqd : Httpun.Reqd.t;
  req : Httpun.Request.t;
  target : string;
  path : string;
  meth : Httpun.Method.t;
  headers : (string * string) list;
  source_ip : string;
  sockaddr : Unix.sockaddr;
}

type pattern =
  | Exact of string  (* full path equality *)
  | Exact_any of string list  (* any of these full paths *)
  | Prefix of string  (* starts_with; the rest may be empty *)
  | Prefix_nonempty of string  (* starts_with AND a nonempty rest *)

type route = {
  name : string;  (* stable id for tests and debugging *)
  meths : Httpun.Method.t list;  (* [] = any method *)
  pattern : pattern;
  guard : string -> bool;  (* rare extra path condition *)
  handler : route_env -> unit;
}

let rt (name : string) (meths : Httpun.Method.t list) (pattern : pattern)
    (handler : route_env -> unit) : route =
  { name; meths; pattern; guard = (fun _ -> true); handler }

let rtg (name : string) (meths : Httpun.Method.t list) (pattern : pattern)
    (guard : string -> bool) (handler : route_env -> unit) : route =
  { name; meths; pattern; guard; handler }

let meth_matches (meths : Httpun.Method.t list) (m : Httpun.Method.t) : bool =
  match meths with [] -> true | _ -> List.exists (fun m' -> m' = m) meths

let path_matches (p : pattern) (path : string) : bool =
  match p with
  | Exact s -> path = s
  | Exact_any ss -> List.mem path ss
  | Prefix s -> String.starts_with ~prefix:s path
  | Prefix_nonempty s ->
      String.length path > String.length s && String.starts_with ~prefix:s path

(* Suffix after a matched prefix (callers pass their own prefix literal). *)
let prefix_rest (p : string) (path : string) : string =
  String.sub path (String.length p) (String.length path - String.length p)

(* page_fn receives the SELECTED session (image.per-identity-sessions):
   the identity's own Session.t, never the shared boot session. *)
let launch (env : route_env) (page_fn : Session.t -> unit) : unit =
  let { sw; registry; session; reqd; target; path; headers; source_ip; sockaddr; _ }
      =
    env
  in
  Eio.Fiber.fork ~sw (fun () ->
      (* Per-identity fair-share (card 1bdeb856): acquired BEFORE the
         global cap so one tenant's slow evals cannot occupy every
         global slot and starve others. The key is derived cheaply from
         headers (auth/cookie) / source_ip — identity itself is not yet
         attached, but the credential IS the identity for gating. *)
      let gate_key = identity_gate_key ~headers ~source_ip in
      let () = per_user_acquire gate_key in
      Fun.protect ~finally:(fun () -> per_user_release gate_key) (fun () ->
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
                attach, then bound so page work reads the snapshot
                (env/routes/manifest) instead of live Session.t, immune to a
                concurrent attach_anonymous + checkout_to rehydrating the
                shared session mid-page. *)
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
             let phase (name : string) (t0 : float) (c0 : int) (m0 : float) : float
                 =
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
               (* auth.delegation-tokens: identity resolve returns the
                  user (for a delegated hit, the OWNER) plus the agent
                  actor when the Bearer was a delegated token. *)
               let identity_opt = resolve_identity session ~headers ~source_ip () in
               let user_opt = Option.map fst identity_opt in
               let actor = Option.bind identity_opt snd in
               let t1 = phase "resolve_identity" t0 c0 m0 in
               let session = Session_registry.select registry ~user:user_opt ~actor in
               let () =
                 match user_opt with
                 | Some user ->
                     let tok = cookie_value headers cookie_name in
                     attach_from_user session user ~auth_token:tok
                       ~delegated:(actor <> None) ~actor
                       ~checkout_personal_now:false ()
                 | None -> Session.attach_anonymous session
               in
               let t2 =
                 phase "registry+attach" t1
                   (Atomic.get Store_db.trace_count)
                   (Atomic.get Store_db.trace_ms)
               in
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
                     Some (session, snapshot_ctx session)
                   end
                   else begin
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
                     match (user_opt, needs_auth) with
                     | Some _, _ -> Some (session, snapshot_ctx session)
                     | None, false -> Some (session, snapshot_ctx session)
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
                    classify_path_with_routes
                      (Request_context.routes req_ctx)
                      path
                  in
                  let path_private =
                    match classif_now with
                    | `Hard_public -> false
                    | `User (_, vis) -> vis <> "public"
                    | `Hard_private | `Unknown -> true
                  in
                  let mismatch =
                    match (host_mode, base_domain ()) with
                    | Host_garden g, _
                      when path_private && uname_now <> "anonymous"
                           && uname_now <> g
                      -> true
                    | Host_unknown, Some _ -> true
                    | _ -> false
                  in
                  if mismatch then begin
                    (match (host_mode, base_domain ()) with
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
                  end
                  else begin
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
                      if String.length path >= 4 && String.sub path 0 4 = "/api"
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
                  end)
           with exn ->
             (* image.http-robustness invariant 2: if the handler failed
                because the peer went away (closed writer), do NOT try to
                render a 500 page onto the same dead writer — that re-throw
                was the actual crash mechanism. Just close quietly and end. *)
             if is_closed_writer_exn exn then begin
               ignore
                 (Ops_log.info ~component:"http"
                    ~context:[ ("path", path) ]
                    "peer gone (closed writer); skipping 500");
               ()
             end
             else if
               match exn with Http_core.Body_too_large -> true | _ -> false
             then begin
               (* Oversized request body (PP_MAX_BODY_BYTES): a 413, not a
                  500 — the client, not the server, is at fault. *)
               text_response reqd ~status:`Payload_too_large
                 ~content_type:"text/plain" "request body too large";
               ()
             end
             else begin
               (* Log the real exception to ops; never echo raw exception text
                  back to the client (500 bodies are generic — incident is
                  correlated server-side via ops log). *)
               Ops_incidents.emit_and_maybe_file session
                 ~level:Ops_log.Error ~component:"http"
                 ~file:(not (Ops_incidents.is_expected_runtime_error exn))
                 ~context:[ ("path", path) ]
                 (Printexc.to_string exn);
               let body =
                 chrome ~session ~title:"Error"
                   [ HTML.p [ HTML.class_ "err" ]
                       [ txt "internal server error — incident logged" ] ]
               in
               html_response reqd ~session ~status:`Internal_server_error body;
               ()
             end))))

(** habitat-book.web-renderer: kernel-dispatched book handlers
    (book/page, book/chapter-page, book/code-page in libs/book/lib.pp).
    Same shape as the /book/<pkg> fallthrough: resolve the handler
    INSIDE page work (post-gate per-request context), zero-arg call,
    params ride the request record. No route pins — the kernel owns
    the /book namespace dispatch (spec keeps its /book route pin for
    the unbound-book fallback below). *)
let launch_book_handler (env : route_env) (handler : string) : unit =
  launch env (fun session ->
      let request : Session.request =
        { method_ = `GET
        ; path = env.path
        ; params = parse_query env.target
        ; body = ""
        ; headers = env.headers
        }
      in
      match Session.call_handler session ~request handler with
      | Session.Handler_ok value ->
          respond_handler_value ~session ~reqd:env.reqd ~path:env.path
            ~fragment:false value
      | Session.Handler_unbound ->
          text_response env.reqd ~status:`Not_found ~content_type:"text/plain"
            (String.concat " " [ handler; "unbound — (load-library \"book\")" ])
      | Session.Handler_error msg ->
          text_response env.reqd ~status:`Internal_server_error
            ~content_type:"text/plain" msg)

(** habitat-book.eval-examples: run-button target for nopales example
    blocks. htmx posts the tree-row path (hx-vals); we read the row,
    eval its src via the RESTRICTED example engine
    (Session.example_eval_engine — whitelisted pure + read-only prims,
    static gate, child of the session env, fresh budget), and return a
    replacement .book-example div (hx-target closest, hx-swap
    outerHTML) showing value/error + elapsed. Kernel-dispatched like
    the /book GET surface — no route pins in the /book namespace. *)
let post_book_eval_example (env : route_env) (session : Session.t) : unit =
  let reqd = env.reqd in
  let body = read_body reqd in
  let row_path =
    Option.value (form_field body "path") ~default:"" |> String.trim
  in
  let fail ?(status = `Bad_request) msg =
    text_response reqd ~status ~content_type:"text/plain" msg
  in
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
                Printf.sprintf "%.0f ms" ((Unix.gettimeofday () -. t0) *. 1000.0)
              in
              let elapsed_node () =
                HTML.span [ HTML.class_ "book-elapsed" ] [ txt "(%s)" (elapsed ()) ]
              in
              (* The htmx replacement fragment, built as a pure-html
                 tree instead of hand-assembled sprintf HTML (which
                 hand-escaped everything, e.g. the literal =&gt;).
                 Text nodes escape & < >; quotes stay literal — valid
                 HTML text either way. *)
              let result_node =
                match Session.example_eval_engine session src with
                | v ->
                    HTML.div [ HTML.class_ "book-result" ]
                      [ txt "⇒ "
                      ; HTML.code []
                          [ txt "%s" (Pricklypear_lisp.Reader.pp_value v) ]
                      ; txt " "
                      ; elapsed_node () ]
                | exception Example_eval.Refused reason ->
                    HTML.div [ HTML.class_ "book-error" ]
                      [ txt "refused: "; txt "%s" reason; txt " "
                      ; elapsed_node () ]
                | exception exn ->
                    HTML.div [ HTML.class_ "book-error" ]
                      [ txt "error: "; txt "%s" (Printexc.to_string exn)
                      ; txt " "; elapsed_node () ]
              in
              let expected_node =
                if expected = "" then None
                else Some (HTML.p [ HTML.class_ "book-expected" ] [ txt "expect => %s" expected ])
              in
              (* The trailing empty txt inside pre keeps the
                 pretty-printer from wrapping the element in newlines
                 (whitespace is significant inside pre, and the htmx
                 replacement must render the example source exactly). *)
              let node =
                HTML.div [ HTML.class_ "book-example" ]
                  ([ HTML.pre [] [ HTML.code [] [ txt "%s" src ]; txt "" ]
                   ; result_node ]
                  @
                  match expected_node with Some n -> [ n ] | None -> [])
              in
              fragment_response reqd (Pure_html.to_string node)
        | _ -> fail "bad book row JSON"
        | exception _ -> fail "bad book row JSON")
    | _ -> fail "bad book row"

(** ONE user-route invocation pipeline — body read -> cap -> form parse ->
    merge_params -> call_handler -> respond — for every arm that
    dispatches into Nopales route handlers. Was two drifted ~110-line
    copies (launch_user_route_or + the GET/POST catch-all): the public
    faces fall to [fallback] both when no route is bound and when the
    bound handler is unbound; the catch-all renders the plain 404 for
    no-route and the Call_graph unbound page for an unbound handler —
    both behaviors preserved via [fallback]/[on_unbound].
    Resolve the route INSIDE page work, after the gate has attached
    identity and bound the per-request context
    (image.per-request-context). Pre-gate the routes table could be
    whatever a previous request left behind (e.g. anonymous attach ->
    system/main), causing a personal-branch route 404 before THIS
    request's attach even runs. The former per-copy
    [if String.length body > cap] 413 checks are gone — dead since
    read_body raises Body_too_large at the cap, which launch maps to a
    413. *)
let invoke_user_route (env : route_env)
    ~(fallback : session:Session.t -> unit)
    ~(on_unbound : session:Session.t -> handler:string -> unit) : unit =
  launch env (fun session ->
      let route_opt =
        match Request_context.get () with
        | Some ctx -> Request_context.lookup_route ctx env.path
        | None -> Session.lookup_route session env.path
      in
      match route_opt with
      | None -> fallback ~session
      | Some handler_name ->
          let is_post = match env.meth with `POST -> true | _ -> false in
          let content_type =
            match Httpun.Headers.get env.req.headers "content-type" with
            | Some s -> s
            | None -> ""
          in
          let body_cap =
            request_body_cap ~kind:`Form (Some content_type)
          in
          let body_str =
            if is_post then read_body env.reqd ~max_bytes:body_cap else ""
          in
          let query = parse_query env.target in
          let form =
            if not is_post then []
            else if
              String.length content_type >= 33
              && String.sub content_type 0 33
                 = "application/x-www-form-urlencoded"
              || content_type = "" && String.contains body_str '='
            then parse_form_body body_str
            else []
          in
          let params = merge_params ~query ~form in
          let fragment = wants_fragment ~path:env.path ~headers:env.headers in
          let method_ : Session.http_method = if is_post then `POST else `GET in
          let request : Session.request =
            { method_; path = env.path; params; body = body_str; headers = env.headers }
          in
          let result, _total_ms =
            Timing.measure ~route:handler_name
              (fun () -> Session.call_handler session ~request handler_name)
          in
          match result with
          | Session.Handler_ok value ->
              (* Public front-door pages: no product chrome (private nav). *)
              if env.path = "/welcome"
                 || env.path = "/background" || env.path = "/roadmap"
                 || env.path = "/signup"
                 || env.path = "/guestbook"
              then begin
                let title =
                  if env.path = "/background" then "Background"
                  else if env.path = "/roadmap" then "Roadmap"
                  else if env.path = "/signup" then "Signup"
                  else if env.path = "/guestbook" then "Guestbook"
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
                Http_code.serve_public_body env.reqd ~title body;
                ()
              end
              else begin
                respond_handler_value ~session ~reqd:env.reqd ~path:env.path
                  ~fragment value;
                ()
              end
          | Session.Handler_unbound -> on_unbound ~session ~handler:handler_name
          | Session.Handler_error msg ->
              if fragment then
                fragment_response env.reqd ~status:`Internal_server_error
                  ("handler " ^ handler_name ^ ": " ^ msg)
              else
                html_response env.reqd ~session ~status:`Internal_server_error
                  (chrome ~session ~title:"Route handler error"
                     [ HTML.h1 [] [ txt "Route handler error" ];
                       HTML.p [] [ txt "handler %s" handler_name ];
                       HTML.pre [] [ txt "%s" msg ] ]))

(* Public-face user-route arm: [serve] answers both "no route bound"
   and "bound handler unbound" — the pre-refactor fallthrough for the
   welcome-family faces; the catch-all entry instead renders the plain
   404 / Call_graph unbound page. *)
let user_route_or (env : route_env) (serve : unit -> unit) : unit =
  invoke_user_route env
    ~fallback:(fun ~session:_ -> serve ())
    ~on_unbound:(fun ~session:_ ~handler:_ -> serve ())

(* The plain no-route 404 (fragment-aware). Shared by the user-route
   catch-all fallback and the final any-method entry (the two arms
   rendered identical text). The final entry renders through launch so
   chrome comes from the SELECTED session — was the shared boot session
   outside the gate (logged-in users saw user=anonymous nav; render
   raced concurrent attaches on Session.ui_nav_html). *)
let not_found_page (env : route_env) (session : Session.t) : unit =
  if wants_fragment ~path:env.path ~headers:env.headers then
    fragment_response env.reqd ~status:`Not_found ("no partial for " ^ env.path)
  else
    html_response env.reqd ~session ~status:`Not_found
      (chrome ~session ~title:"Not found" [ HTML.p [] [ txt "no route for %s" env.path ] ])

(* Catch-all unbound-handler page: names the dead handler, offers
   create-stub/inspect, lists static Call_graph callers. *)
let unbound_handler_page (env : route_env) (session : Session.t)
    ~(handler : string) : unit =
  if wants_fragment ~path:env.path ~headers:env.headers then begin
    fragment_response env.reqd ~status:`Internal_server_error
      ("handler " ^ handler ^ " unbound");
    ()
  end
  else
    let snap = Call_graph.snapshot_of_session session in
    let callers = Call_graph.callers snap handler in
    let caller_nodes =
      if callers = [] then [ txt "(none yet)" ]
      else
        List.concat_map
          (fun n ->
            [ HTML.a [ HTML.href "%s" (Packages.binding_href n) ] [ txt "%s" n ];
              txt " " ])
          callers
    in
    html_response env.reqd ~session ~status:`Not_found
      (chrome ~session ~title:"Handler unbound"
         [ HTML.div [ HTML.class_ "card" ]
             [ HTML.h1 [] [ txt "Handler unbound" ];
               HTML.p []
                 [ txt "Route ";
                   HTML.code [] [ txt "%s" env.path ];
                   txt " points at ";
                   HTML.code [] [ txt "%s" handler ];
                   txt " which is not bound." ];
               HTML.p []
                 [ HTML.a
                     [ HTML.href "%s" (Packages.edit_href handler);
                       HTML.class_ "btn" ]
                     [ txt "Create stub %s" handler ];
                   txt " ";
                   HTML.a
                     [ HTML.href "%s" (Packages.binding_href handler);
                       HTML.class_ "btn secondary" ]
                     [ txt "Inspect" ] ];
               HTML.p []
                 ([ HTML.span [ HTML.class_ "muted" ] [ txt "static callers: " ] ]
                 @ caller_nodes) ] ])

(* Kernel 400 pages for malformed path shapes (bad /data segments, bad
   /inspect/row segments, empty /edit name). Rendered through launch so
   chrome comes from the SELECTED session — was the shared boot session
   outside the gate. *)
let bad_path_page (env : route_env) ~(title : string) (msg : string) : unit =
  launch env (fun session ->
      html_response env.reqd ~session ~status:`Bad_request
        (chrome ~session ~title [ HTML.p [] [ txt "%s" msg ] ]))

(* /api/analyze/<name> and /api/explain/<name> share one name extraction
   (was two verbatim copies). path never carries a query —
   request_handler splits it off at entry — so the per-copy '?' trim is
   gone. *)
let analyze_name (prefix : string) (env : route_env) : string =
  Packages.name_decode (prefix_rest prefix env.path)

(* The router table: ONE dispatch surface. Order is load-bearing —
   first match wins; exact/narrow prefixes must precede broader ones
   (assets/cm before assets, book/chapter|code|course before book/,
   /media/upload before /media/, exact /data //media//page//edit
   spellings before their prefixes) and the catch-alls are last: GET
   and POST fall to the user-route entry, every other method to the
   not-found entry. /health, /.well-known/agent.json and /agent.txt
   deliberately do NOT go through launch: image.http-robustness
   invariant 5 keeps liveness probes + the public agent bootstrap
   servable when the page-work queue is full — they render no session
   chrome, so the shared-boot-session hazard does not apply. *)
let routes : route list =
  [
    (* ---- auth / system pages (through launch: gate + selected session) ---- *)
    rt "login-form" [ `GET ] (Exact "/login")
      (fun env -> launch env (fun _ -> login_form_page env.reqd));
    rt "login-post" [ `POST ] (Exact "/login")
      (fun env ->
        launch env (fun session -> post_login session ~source_ip:env.source_ip env.reqd));
    rt "logout" [ `POST ] (Exact "/logout")
      (fun env -> launch env (fun session -> post_logout session env.reqd));
    rt "admin-users" [ `GET ] (Exact "/admin/users")
      (fun env -> launch env (fun session -> admin_users_page session env.reqd));
    rt "admin-users-post" [ `POST ] (Exact "/admin/users")
      (fun env -> launch env (fun session -> post_admin_users session env.reqd));
    rt "settings-keys" [ `GET ] (Exact "/settings/keys")
      (fun env -> launch env (fun session -> settings_keys_page session env.reqd));
    rt "settings-keys-post" [ `POST ] (Exact "/settings/keys")
      (fun env -> launch env (fun session -> post_settings_keys session env.reqd));
    rt "settings-ssh" [ `GET ] (Exact "/settings/ssh")
      (fun env -> launch env (fun session -> settings_ssh_page session env.reqd));
    rt "settings-ssh-post" [ `POST ] (Exact "/settings/ssh")
      (fun env -> launch env (fun session -> post_settings_ssh session env.reqd));
    rt "analytics" [ `GET ] (Exact "/analytics")
      (fun env -> launch env (fun session -> analytics_page session env.reqd));
    rt "home" [ `GET ] (Exact "/")
      (fun env -> launch env (fun session -> home_page session env.reqd));
    (* ---- public-face user routes (Lisp handler if bound; else static) ---- *)
    rt "welcome" [ `GET ] (Exact "/welcome")
      (fun env -> user_route_or env (fun () -> Http_code.fallback_welcome env.reqd));
    rt "background" [ `GET ] (Exact "/background")
      (fun env ->
        user_route_or env (fun () ->
            Http_code.serve_public_body env.reqd ~title:"Background"
              "<p><a href=\"/welcome\">welcome</a> · author background page not seeded yet.</p>"));
    rt "roadmap" [ `GET ] (Exact "/roadmap")
      (fun env ->
        user_route_or env (fun () ->
            Http_code.serve_public_body env.reqd ~title:"Roadmap"
              "<p><a href=\"/welcome\">welcome</a> · roadmap page not seeded yet.</p>"));
    rt "signup" [ `GET; `POST ] (Exact "/signup")
      (fun env ->
        user_route_or env (fun () ->
            Http_code.serve_public_body env.reqd ~title:"Signup"
              "<p><a href=\"/welcome\">welcome</a> · signup page not seeded yet.</p>"));
    rt "guestbook" [ `GET; `POST ] (Exact "/guestbook")
      (fun env ->
        user_route_or env (fun () ->
            Http_code.serve_public_body env.reqd ~title:"Guestbook"
              "<p><a href=\"/welcome\">welcome</a> · guestbook page not seeded yet.</p>"));
    (* ---- habitat-book (kernel-dispatched, no route pins) ---- *)
    rt "book-index" [ `GET ] (Exact "/book")
      (fun env ->
        (* habitat-book.web-renderer: the book package's merged index (borg
           chapters + package spec docs) when book/page is bound; else the
           /book route pin (spec/book-page, habitat-workflow.book-view);
           else the static release face. A handler ERROR is surfaced (500),
           never masked by falling through. *)
        launch env (fun session ->
            let request : Session.request =
              { method_ = `GET
              ; path = env.path
              ; params = parse_query env.target
              ; body = ""
              ; headers = env.headers
              }
            in
            let serve_handler handler_name =
              match Session.call_handler session ~request handler_name with
              | Session.Handler_ok value ->
                  respond_handler_value ~session ~reqd:env.reqd ~path:env.path
                    ~fragment:false value;
                  true
              | Session.Handler_unbound -> false
              | Session.Handler_error msg ->
                  text_response env.reqd ~status:`Internal_server_error
                    ~content_type:"text/plain" msg;
                  true
            in
            let served =
              serve_handler "book/page"
              ||
              (match
                 (match Request_context.get () with
                  | Some ctx -> Request_context.lookup_route ctx env.path
                  | None -> Session.lookup_route session env.path)
               with
               | Some handler_name -> serve_handler handler_name
               | None -> false)
            in
            if not served then Http_code.fallback_book env.reqd;
            ()));
    rt "book-chapter" [ `GET ] (Prefix_nonempty "/book/chapter/")
      (fun env ->
        (* habitat-book.web-renderer: borg chapter pages over the seeded
           tree rows (scripts/load-book.sh). Prefix is 14 bytes; >14 means
           a nonempty label follows. *)
        launch_book_handler env "book/chapter-page");
    rt "book-code" [ `GET ] (Prefix_nonempty "/book/code/")
      (fun env ->
        (* habitat-book.web-renderer: one code-chapter listing. *)
        launch_book_handler env "book/code-page");
    rt "book-course" [ `GET ] (Prefix_nonempty "/book/course/")
      (fun env ->
        (* habitat-book.course-documents: notes/outline course docs (slug
           course/<suffix>, kind course) assembled into an ordered reading
           list over the seeded chapter rows — compose, never copy. *)
        launch_book_handler env "book/course-page");
    rt "book-eval-example" [ `POST ] (Exact "/book/eval-example")
      (fun env -> launch env (fun session -> post_book_eval_example env session));
    rt "book-verify" [ `GET ] (Exact "/book/verify")
      (fun env ->
        (* habitat-book.eval-examples: (book/verify) as the book's
           self-test — re-evals every nopales example row with an
           `expect` trailer; mismatch/refusal renders as a hole row
           (drift-as-rendering). ?chapter=<slug> scopes to one chapter. *)
        launch_book_handler env "book/verify-page");
    rt "book-spec" [ `GET ] (Prefix_nonempty "/book/")
      (fun env ->
        (* habitat-workflow.book-view: /book/<pkg> → (spec/book-page).
           request-path carries the full path so Nopales can extract pkg.
           habitat-book.web-renderer keeps /book/pkg/<pkg> as an alias:
           the effective path strips the pkg/ segment. *)
        let path =
          if String.length env.path > 10
             && String.starts_with ~prefix:"/book/pkg/" env.path
          then "/book/" ^ prefix_rest "/book/pkg/" env.path
          else env.path
        in
        launch env (fun session ->
            let request : Session.request =
              { method_ = `GET
              ; path
              ; params = parse_query env.target
              ; body = ""
              ; headers = env.headers
              }
            in
            match Session.call_handler session ~request "spec/book-page" with
            | Session.Handler_ok value ->
                respond_handler_value ~session ~reqd:env.reqd ~path
                  ~fragment:false value
            | Session.Handler_unbound ->
                text_response env.reqd ~status:`Not_found
                  ~content_type:"text/plain"
                  "spec/book-page unbound — (load-library \"spec\")"
            | Session.Handler_error msg ->
                text_response env.reqd ~status:`Internal_server_error
                  ~content_type:"text/plain" msg));
    (* ---- public release face (hard-public file serving) ---- *)
    rt "book-pdf" [ `GET ] (Exact "/book.pdf")
      (fun env ->
        launch env (fun _ -> Http_code.handle_public_artifact env.reqd ~name:"book.pdf"));
    rt "src-tgz" [ `GET ] (Exact "/src.tgz")
      (fun env ->
        launch env (fun _ -> Http_code.handle_public_artifact env.reqd ~name:"src.tgz"));
    rt "code" [ `GET ] (Exact "/code")
      (fun env ->
        launch env (fun _ -> Http_code.handle_code_page env.reqd ~target:env.target));
    rt "code-raw" [ `GET ] (Prefix "/code/raw/")
      (fun env ->
        let rel = prefix_rest "/code/raw/" env.path in
        launch env (fun _ -> Http_code.handle_raw env.reqd ~rel));
    rt "assets-cm" [ `GET ] (Prefix "/assets/cm/")
      (fun env ->
        (* Back-compat alias: /assets/cm/* → static/codemirror/* *)
        let rel = "codemirror/" ^ prefix_rest "/assets/cm/" env.path in
        launch env (fun _ -> Http_code.handle_asset env.reqd ~rel));
    rt "assets" [ `GET ] (Prefix "/assets/")
      (fun env ->
        let rel = prefix_rest "/assets/" env.path in
        launch env (fun _ -> Http_code.handle_asset env.reqd ~rel));
    (* ---- doc pages ---- *)
    rt "about" [ `GET ] (Exact "/about")
      (fun env -> launch env (fun session -> about_page session env.reqd));
    rt "pages" [ `GET ] (Exact "/pages")
      (fun env -> launch env (fun session -> pages_list_page session env.reqd));
    (* ---- recipe post buttons ---- *)
    rt "recipe-hello" [ `POST ] (Exact "/api/recipe/hello")
      (fun env ->
        launch env (fun session ->
            let res = Session.recipe_hello session in
            match res with
            | Ok msg ->
                let body =
                  chrome ~session ~title:"Hello recipe"
                    [ HTML.div [ HTML.class_ "card" ]
                        [ HTML.p [ HTML.class_ "ok" ] [ txt "%s" msg ];
                          HTML.p []
                            [ HTML.a [ HTML.href "/hello" ] [ txt "Open /hello" ];
                              txt " · ";
                              HTML.a [ HTML.href "/edit/hi-page" ] [ txt "Edit hi-page" ];
                              txt " · ";
                              HTML.a [ HTML.href "/pages" ] [ txt "Pages" ];
                              txt " · ";
                              HTML.a [ HTML.href "/" ] [ txt "Home" ] ] ] ]
                in
                html_response env.reqd ~session body;
                ()
            | Error e ->
                let body =
                  chrome ~session ~title:"Hello recipe failed"
                    [ HTML.p [ HTML.class_ "err" ] [ txt "%s" e ];
                      HTML.p [] [ HTML.a [ HTML.href "/" ] [ txt "Home" ] ] ]
                in
                html_response env.reqd ~session ~status:`Internal_server_error body;
                ()));
    rt "recipe-todo" [ `POST ] (Exact "/api/recipe/todo")
      (fun env ->
        launch env (fun session ->
            let res = Session.recipe_todo session in
            match res with
            | Ok msg ->
                let body =
                  chrome ~session ~title:"Todo library"
                    [ HTML.div [ HTML.class_ "card" ]
                        [ HTML.p [ HTML.class_ "ok" ] [ txt "%s" msg ];
                          HTML.p []
                            [ HTML.a [ HTML.href "/todos" ] [ txt "Open /todos" ];
                              txt " · ";
                              HTML.a [ HTML.href "/bindings?pkg=todo" ] [ txt "Package todo" ];
                              txt " · ";
                              HTML.a [ HTML.href "/pages" ] [ txt "Pages" ] ] ] ]
                in
                html_response env.reqd ~session body;
                ()
            | Error e ->
                let body =
                  chrome ~session ~title:"Todo recipe failed"
                    [ HTML.p [ HTML.class_ "err" ] [ txt "%s" e ];
                      HTML.p [] [ HTML.a [ HTML.href "/" ] [ txt "Home" ] ] ]
                in
                html_response env.reqd ~session ~status:`Internal_server_error body;
                ()));
    rt "recipe-notes" [ `POST ] (Exact "/api/recipe/notes")
      (fun env ->
        launch env (fun session ->
            let res = Session.recipe_notes session in
            match res with
            | Ok msg ->
                let body =
                  chrome ~session ~title:"Notes library"
                    [ HTML.div [ HTML.class_ "card" ]
                        [ HTML.p [ HTML.class_ "ok" ] [ txt "%s" msg ];
                          HTML.p []
                            [ HTML.a [ HTML.href "/notes" ] [ txt "Open /notes" ];
                              txt " · ";
                              HTML.a [ HTML.href "/bindings?pkg=notes" ] [ txt "Package notes" ];
                              txt " · ";
                              HTML.a [ HTML.href "/bindings?pkg=outline" ] [ txt "outline" ] ];
                          HTML.p [ HTML.class_ "muted" ]
                            [ txt "Outline-first notes (org-shaped). Content lives in data_rows." ] ] ]
                in
                html_response env.reqd ~session body;
                ()
            | Error e ->
                let body =
                  chrome ~session ~title:"Notes recipe failed"
                    [ HTML.p [ HTML.class_ "err" ] [ txt "%s" e ];
                      HTML.p [] [ HTML.a [ HTML.href "/" ] [ txt "Home" ] ] ]
                in
                html_response env.reqd ~session ~status:`Internal_server_error body;
                ()));
    rt "recipe-dabar" [ `POST ] (Exact "/api/recipe/dabar")
      (fun env ->
        launch env (fun session ->
            let res = Session.recipe_dabar session in
            match res with
            | Ok msg ->
                let body =
                  chrome ~session ~title:"Dabar library"
                    [ HTML.div [ HTML.class_ "card" ]
                        [ HTML.p [ HTML.class_ "ok" ] [ txt "%s" msg ];
                          HTML.p []
                            [ HTML.a [ HTML.href "/dabar?book=John&chapter=3" ]
                                [ txt "Open /dabar" ];
                              txt " · ";
                              HTML.a [ HTML.href "/dabar?book=John&chapter=3&verse=16" ]
                                [ txt "Study John 3:16" ];
                              txt " · ";
                              HTML.a [ HTML.href "/strongs?n=G2316" ] [ txt "Strong's G2316" ] ];
                          HTML.p [ HTML.class_ "muted" ]
                            [ txt "Seeds: scripts/seed_bible_bsb.py · seed_strongs.py · seed_interlinear_dogfood.py" ] ] ]
                in
                html_response env.reqd ~session body;
                ()
            | Error e ->
                let body =
                  chrome ~session ~title:"Dabar recipe failed"
                    [ HTML.p [ HTML.class_ "err" ] [ txt "%s" e ];
                      HTML.p [] [ HTML.a [ HTML.href "/" ] [ txt "Home" ] ] ]
                in
                html_response env.reqd ~session ~status:`Internal_server_error body;
                ()));
    (* ---- liveness probe: DIRECT (no launch, no gate, no queue) ---- *)
    rt "health" [ `GET ] (Exact "/health") (fun env ->
        (* image.http-robustness invariant 7: /health must probe the same
           store path auth/prims use. Memory-only ok:true hid Eio mutex
           poison and locked out all logins while watchdog saw green.
           Invariant 5: /health must never queue behind page work. *)
        let branch = Session.branch_name env.session in
        let uptime = Session.uptime_seconds env.session in
        let boot_epoch = Session.boot_epoch env.session in
        let check = Store.health_check (Session.store env.session) in
        (* fd gauge (2026-10-02 fd-exhaustion incident): surface open fds
           vs the soft limit so a slow climb is visible before accept
           fails. Cheap fstat sweep, bounded by the limit. *)
        let open_fds = Fd_gauge.count () in
        let fd_limit = Lazy.force Fd_gauge.limit in
        let h, status =
          match check with
          | Ok () ->
              ( { Pricklypear_common.Protocol.ok = true;
                  branch;
                  uptime_seconds = uptime;
                  boot_epoch;
                  open_fds;
                  fd_limit;
                  error = None },
                `OK )
          | Error msg ->
              ( { Pricklypear_common.Protocol.ok = false;
                  branch;
                  uptime_seconds = uptime;
                  boot_epoch;
                  open_fds;
                  fd_limit;
                  error = Some msg },
                `Service_unavailable )
        in
        let j = Pricklypear_common.Protocol.health_response_to_json h in
        text_response env.reqd ~status ~content_type:"application/json"
          (Yojson.Safe.pretty_to_string j));
    (* ---- public agent bootstrap — no auth, no secrets; DIRECT (no
       launch): discovery must answer under load. /agent stays private. ---- *)
    rt "agent-json" [ `GET ] (Exact "/.well-known/agent.json")
      (fun env -> Agent_discovery.serve_agent_json env.reqd ~headers:env.headers);
    rt "agent-txt" [ `GET ] (Exact "/agent.txt")
      (fun env -> Agent_discovery.serve_agent_txt env.reqd ~headers:env.headers);
    (* ---- branches / bindings / types / timing ---- *)
    rt "branches" [ `GET ] (Exact "/branches")
      (fun env -> launch env (fun session -> branches_page session env.reqd));
    rt "changes" [ `GET ] (Exact "/changes")
      (fun env -> launch env (fun session -> changes_page session env.reqd));
    rt "branches-checkout" [ `POST ] (Exact "/branches/checkout")
      (fun env ->
        launch env (fun session -> branches_checkout session env.reqd));
    rt "branches-fork" [ `POST ] (Exact "/branches/fork")
      (fun env -> launch env (fun session -> branches_fork session env.reqd));
    rt "branches-unoverride" [ `POST ] (Exact "/branches/unoverride")
      (fun env ->
        launch env (fun session -> branches_unoverride session env.reqd));
    rt "branches-promote" [ `POST ] (Exact "/branches/promote")
      (fun env -> launch env (fun session -> branches_promote session env.reqd));
    rt "bindings" [ `GET ] (Exact "/bindings")
      (fun env -> launch env (fun session -> list_bindings_page session env.reqd));
    rt "diff" [ `GET ] (Exact "/diff")
      (fun env -> launch env (fun session -> diff_page session env.reqd));
    rt "types" [ `GET ] (Exact "/types")
      (fun env -> launch env (fun session -> list_types_page session env.reqd));
    rt "timing" [ `GET ] (Exact "/timing")
      (fun env -> launch env (fun session -> timing_page session env.reqd));
    (* ---- db panel (image.db-panel; admin; fixed read SQL) ---- *)
    rt "db-panel" [ `GET ] (Exact "/db")
      (fun env -> launch env (fun session -> db_panel_page session env.reqd));
    (* ---- data_rows browser ---- *)
    rt "data-index" [ `GET ] (Exact_any [ "/data"; "/data/" ])
      (fun env -> launch env (fun session -> data_index_page session env.reqd));
    rt "data-get" [ `GET ] (Prefix "/data/")
      (fun env ->
        let rest = prefix_rest "/data/" env.path in
        let segs = String.split_on_char '/' rest |> List.filter ((<>) "") in
        match segs with
        | [ ty; "new" ] ->
            launch env (fun session -> data_new_page session ty env.reqd)
        | [ ty; id; "delete" ] ->
            (* GET delete not supported — point at detail *)
            launch env (fun session -> data_detail_page session ty id env.reqd)
        | [ ty; id ] ->
            launch env (fun session -> data_detail_page session ty id env.reqd)
        | [ ty ] -> launch env (fun session -> data_list_page session ty env.reqd)
        | _ ->
            bad_path_page env ~title:"Bad data path"
              "use /data, /data/<type>, /data/<type>/new, /data/<type>/<id>");
    rt "data-post" [ `POST ] (Prefix "/data/")
      (fun env ->
        let rest = prefix_rest "/data/" env.path in
        let segs = String.split_on_char '/' rest |> List.filter ((<>) "") in
        match segs with
        | [ ty; id; "delete" ] ->
            launch env (fun session -> data_delete session ty id env.reqd)
        | [ ty; id ] ->
            launch env (fun session -> data_update session ty id env.reqd)
        | [ ty ] -> launch env (fun session -> data_create session ty env.reqd)
        | _ ->
            bad_path_page env ~title:"Bad data path"
              "POST /data/<type> | /data/<type>/<id> | …/delete");
    (* ---- tree browser ---- *)
    rt "tree" [ `GET ] (Exact "/tree")
      (fun env -> launch env (fun session -> Http_tree.tree_page session env.reqd));
    rt "tree-partial" [ `GET ] (Exact "/partials/tree")
      (fun env ->
        launch env (fun session -> Http_tree.tree_partial session env.reqd));
    rt "tree-edit" [ `POST ] (Exact "/tree/edit")
      (fun env -> launch env (fun session -> Http_tree.tree_edit session env.reqd));
    (* ---- repl + eval API ---- *)
    rt "repl" [ `GET ] (Exact "/repl")
      (fun env -> launch env (fun session -> repl_page session env.reqd));
    rt "api-eval" [ `POST ] (Exact "/api/eval")
      (fun env -> launch env (fun session -> api_eval session env.reqd));
    rt "api-prim" [ `POST ] (Exact "/api/prim")
      (fun env -> launch env (fun session -> api_prim session env.reqd));
    (* ---- media ---- *)
    rt "api-media-post" [ `POST ] (Exact "/api/media")
      (fun env -> launch env (fun session -> api_media_post session env.reqd));
    rt "api-media-list" [ `GET ] (Exact "/api/media")
      (fun env ->
        launch env (fun session -> api_media_list session env.reqd ~target:env.target));
    rt "api-media-by-sha-hex" [ `GET ] (Prefix_nonempty "/api/media/by-sha256/")
      (fun env ->
        let sha256 = prefix_rest "/api/media/by-sha256/" env.path in
        launch env (fun session -> api_media_by_sha session env.reqd ~sha256));
    rt "api-media-delete" [ `DELETE ] (Prefix_nonempty "/api/media/")
      (fun env ->
        let id = prefix_rest "/api/media/" env.path in
        launch env (fun session -> api_media_delete session env.reqd ~id));
    rt "media-page" [ `GET ] (Exact_any [ "/media"; "/media/" ])
      (fun env ->
        launch env (fun session -> media_page session env.reqd ~target:env.target));
    rt "media-upload" [ `POST ] (Exact "/media/upload")
      (fun env -> launch env (fun session -> media_upload_post session env.reqd));
    rtg "media-delete" [ `POST ] (Prefix_nonempty "/media/")
      (fun p -> String.ends_with ~suffix:"/delete" p)
      (fun env ->
        let rest = prefix_rest "/media/" env.path in
        (* strip trailing /delete *)
        let id =
          if String.length rest > 7 then
            String.sub rest 0 (String.length rest - 7)
          else rest
        in
        launch env (fun session -> media_delete_post session env.reqd ~id));
    rt "media-get" [ `GET ] (Prefix_nonempty "/media/")
      (fun env ->
        let id = prefix_rest "/media/" env.path in
        launch env (fun session -> media_get session env.reqd ~id));
    (* ---- agent chat / llm ---- *)
    rt "api-agent" [ `POST ] (Exact "/api/agent")
      (fun env -> launch env (fun session -> api_agent session env.reqd));
    rt "api-agent-task" [ `POST ] (Exact "/api/agent-task")
      (fun env -> launch env (fun session -> api_agent_task session env.reqd));
    rt "repl-llm" [ `POST ] (Exact "/repl-v2/llm")
      (fun env -> launch env (fun session -> repl_llm session env.reqd));
    rt "ai-propose" [ `POST ] (Exact "/api/ai/propose")
      (fun env -> launch env (fun session -> ai_propose session env.reqd));
    rt "agent-page" [ `GET ] (Exact "/agent")
      (fun env -> launch env (fun session -> agent_page session env.reqd));
    (* ---- editor ---- *)
    rt "api-edit-create" [ `POST ] (Exact "/api/edit/create")
      (fun env -> launch env (fun session -> api_edit_create session env.reqd));
    (* ---- analyze / explain (one name extraction, two endpoints) ---- *)
    rt "api-analyze" [ `POST ] (Prefix "/api/analyze/")
      (fun env ->
        let name = analyze_name "/api/analyze/" env in
        launch env (fun session -> Http_analyze.post_analyze session name env.reqd));
    rt "api-explain" [ `POST ] (Prefix "/api/explain/")
      (fun env ->
        let name = analyze_name "/api/explain/" env in
        launch env (fun session -> Http_analyze.post_explain session name env.reqd));
    (* ---- page / binding / row inspectors ---- *)
    rt "page-inspect" [ `GET ] (Prefix "/page/")
      (fun env ->
        let seg = prefix_rest "/page/" env.path in
        launch env (fun session -> page_inspector session seg env.reqd));
    rt "page-index" [ `GET ] (Exact_any [ "/page"; "/page/" ])
      (fun env ->
        launch env (fun session -> page_inspector session "" env.reqd));
    rt "inspect-binding" [ `GET ] (Prefix "/inspect/binding/")
      (fun env ->
        let name = Packages.name_decode (prefix_rest "/inspect/binding/" env.path) in
        launch env (fun session -> inspect_binding_page session name env.reqd));
    rt "inspect-row" [ `GET ] (Prefix "/inspect/row/")
      (fun env ->
        let rest = prefix_rest "/inspect/row/" env.path in
        match String.split_on_char '/' rest with
        | [ ty; id ] ->
            launch env (fun session -> inspect_row_page session ty id env.reqd)
        | _ ->
            bad_path_page env ~title:"Bad inspect path"
              "use /inspect/row/<type>/<id>");
    rt "binding" [ `GET ] (Prefix "/binding/")
      (fun env ->
        let name = Packages.name_decode (prefix_rest "/binding/" env.path) in
        launch env (fun session -> show_binding_page session name env.reqd));
    rt "editor-new" [ `GET ] (Exact_any [ "/edit"; "/edit/" ])
      (fun env ->
        (* package + local query form from Functions browser *)
        launch env (fun session ->
            let pkg = Option.value (query_param env.target "package") ~default:"" in
            let local = Option.value (query_param env.target "local") ~default:"" in
            let name = Packages.join_name ~package:pkg ~local in
            if name = "" then
              html_response env.reqd ~session
                (chrome ~session ~title:"New function"
                   [ HTML.p [] [ txt "Provide package and/or name (see Functions)." ];
                     HTML.p [] [ HTML.a [ HTML.href "/bindings" ] [ txt "Functions" ] ] ])
            else editor_page session name env.reqd));
    rt "editor-get" [ `GET ] (Prefix "/edit/")
      (fun env ->
        let name = Packages.name_decode (prefix_rest "/edit/" env.path) in
        launch env (fun session -> editor_page session name env.reqd));
    rt "edit-save" [ `POST ] (Prefix "/edit/")
      (fun env ->
        let name = Packages.name_decode (prefix_rest "/edit/" env.path) in
        launch env (fun session -> save_edit session name env.reqd));
    (* ---- CATCH-ALLS (must stay last) ---- *)
    (* User routes: GET + POST (it33). Same path → same handler;
       request-* prims expose method/params for branching. *)
    rt "user-route" [ `GET; `POST ] (Prefix "")
      (fun env ->
        invoke_user_route env
          ~fallback:(fun ~session -> not_found_page env session)
          ~on_unbound:(fun ~session ~handler ->
            unbound_handler_page env session ~handler));
    (* Any other method (PUT/DELETE/HEAD/…): the plain 404, through
       launch so chrome uses the selected session. *)
    rt "not-found" [] (Prefix "")
      (fun env -> launch env (fun session -> not_found_page env session));
  ]

(* First match wins — table order is load-bearing (see [routes]). *)
let find_route (m : Httpun.Method.t) (path : string) : route option =
  List.find_opt
    (fun (r : route) -> meth_matches r.meths m && path_matches r.pattern path && r.guard path)
    routes

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
  let env =
    { sw; registry; session; reqd; req; target; path; meth; headers; source_ip; sockaddr }
  in
  match find_route meth path with
  | Some r -> r.handler env
  | None ->
      (* Unreachable: the table ends with catch-all entries (GET/POST
         user-route + any-method not-found). Belt and braces. *)
      launch env (fun session -> not_found_page env session)

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
      (HTML.html [ HTML.lang "en" ] [
         HTML.head [] [ HTML.title [] "Pricklypear Error" ];
         HTML.body [] [ HTML.h1 [] [ txt "Error" ]; HTML.p [] [ txt "%s" msg ] ];
       ])
  in
  let headers =
    Httpun.Headers.of_list
      [ ("content-type", "text/html");
        ("content-length", string_of_int (String.length body_str)) ]
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
