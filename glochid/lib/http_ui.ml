(** glochid HTML primary surface (glochid/lib; borge/glochid.borg surfaces).

    A local HTTP server (Httpun_eio) bound to 127.0.0.1 (default port
    18090, --port to override; a busy port falls back to random). The
    chat + code browser pages are NOPALES-AUTHORED htmx
    fragments on the glochid route table (rendered by Nopales via
    libs/html), dispatched in-process through [Runtime.dispatch_route].
    A chat turn streams as multipart/mixed htmx fragments (one
    part per (html, HX-Target, HX-Swap)), emitted by the SHARED
    emitter (hostkit Multipart_emitter) through the SHARED turn engine
    (hostkit Chat_stream, i-1788671304-d — the surface plumbing +
    queue-drain loop are ONE home for glochid + pp-remote now); the
    fragment templates are Nopales (chat/user-bubble, chat/turn-open,
    chat/think-open, chat/think-token, chat/tool-open, chat/tool-line,
    chat/tool-done, chat/token, chat/error), so the surface is
    redefinable live. Each emitted part is
    also collected as data so a consumed turn replays through
    [hyper/stream] (surfaces smoke).

    Bootstrap/asset plumbing stays OCaml (per the surfaces scope): the
    vendored htmx + hx-multipart assets are baked into the binary
    ([Assets]) and served at /assets/*.js. This file keeps only the
    glochid-specific glue: fixed-port bind + prim registration, the
    settings dial, and the turn_mutex serialization. *)

open Pricklypear_lisp

(* ---- shared history / per-request serialization ---- *)

type server_state = {
  history : Provider.message list ref;
  complete :
    env:Eio_unix.Stdenv.base ->
    sw:Eio.Switch.t ->
    cfg:Provider.cfg ->
    messages:Provider.message list ->
    tools:Provider.tool_def list ->
    on_delta:(Provider.delta -> unit) ->
    (Provider.reply, string) Result.t;
  rt : Runtime.t;
  cfg : Provider.cfg ref;
  models : string list ref;
  turn_mutex : Eio.Mutex.t;
  (* sessions-persist: the CURRENT session transcript handle. Rotation
     (/new, /resume) re-points this ref; the [on_msg] sink below stays
     fixed, so all later turns stream into whatever session is current. *)
  sess : Sessions.handle option ref;
  on_msg : Provider.message -> unit;
}

(** filmstrip-shell host seam #3 (borge/glochid.borg): thread the client's
    request headers (HX-Request is the one that matters) into the route
    table's thin request context, so routes serve pane fragments in
    partial mode — the image's route-gate discipline, glochid-backed. *)
let dispatch_headers (reqd : Httpun.Reqd.t) : (string * string) list =
  match
    Httpun.Headers.get (Httpun.Reqd.request reqd).Httpun.Request.headers
      "hx-request"
  with
  | Some v -> [ ("hx-request", v) ]
  | None -> []

(* ---- GET route dispatch through the glochid route table ---- *)

let dispatch_get (st : server_state) (reqd : Httpun.Reqd.t) (target : string)
    : unit =
  let path, query =
    match String.index_opt target '?' with
    | Some i ->
        ( String.sub target 0 i,
          String.sub target (i + 1) (String.length target - i - 1) )
    | None -> (target, "")
  in
  let params = Http_kit.parse_query query in
  match Runtime.lookup_route ~rt:st.rt path with
  | None ->
      Http_kit.respond_html reqd ~status:`Not_found ~relay:[]
        ~body:(Http_kit.status_page "404"
                 (Printf.sprintf "<p>no route %s</p>" (Http_kit.esc path)))
  | Some handler ->
      let v =
        Runtime.dispatch_route ~rt:st.rt ~handler ~method_:"GET" ~path
          ~params ~headers:(dispatch_headers reqd) ~body:""
      in
      let html =
        match v with Ast.VStr s -> s | other -> Pricklypear_lisp.Reader.pp_value other
      in
      Http_kit.respond_html reqd ~status:`OK ~relay:[] ~body:html

(* ---- static assets (vendored, baked into the binary) ---- *)

let serve_asset (reqd : Httpun.Reqd.t) (name : string) : unit =
  Http_kit.serve_asset reqd ~assets:
    [
      ("htmx.min.js", Assets.htmx_min, "application/javascript; charset=utf-8");
      ("htmx-2-compat.min.js", Assets.htmx_2_compat, "application/javascript; charset=utf-8");
      ("hx-multipart.js", Assets.hx_multipart, "application/javascript; charset=utf-8");
      (* CodeMirror baked for the /code file arm (BORGE-1789581611) — same
         assets the habitat's public /code face serves from disk. *)
      ("codemirror.min.js", Assets.codemirror_min, "application/javascript; charset=utf-8");
      ("codemirror.min.css", Assets.codemirror_css, "text/css; charset=utf-8");
      ("cm/mllike.min.js", Assets.cm_mllike, "application/javascript; charset=utf-8");
      ("cm/markdown.min.js", Assets.cm_markdown, "application/javascript; charset=utf-8");
      ("cm/scheme.min.js", Assets.cm_scheme, "application/javascript; charset=utf-8");
      ("pp-cm.js", Assets.pp_cm, "application/javascript; charset=utf-8");
    ]
    name

(* ---- habitat-pane forwarder gate hook (glochid.borg habitat-panes):
   allowlisted prefixes (registered by the Nopales boot layer via
   habitat/fwd-prefix) forward TO THE HABITAT with the operator's
   session — the gate carries the browser's method + query + body
   through and relays status + HX-* reply headers + body back. The
   address-bar htmx-ness passes through (fragment vs full page is the
   caller's decision, byte-piped). In-process pane dispatch (filmstrip/
   open) does NOT come through here — it rides the Nopales fwd faces
   over the habitat/fwd prim. ---- *)

let dispatch_fwd (env : Eio_unix.Stdenv.base) (reqd : Httpun.Reqd.t)
    (method_ : string) (path : string) (query : string) (body : string)
    ~(hx : bool) =
  match
    Habitat.forward_response ~env ~meth:method_ ~path ~query ~body ~hx
  with
  | Error e ->
      Http_kit.respond_html reqd ~status:`Internal_server_error ~relay:[]
        ~body:
          (String.concat ""
             [ "<div class=\"lb-card\"><p class=\"muted\">habitat pane: "
             ; Http_kit.esc e
             ; "</p></div>" ])
  | Ok (status, hs, b) ->
      let relay =
        List.filter
          (fun (k, _) ->
             String.starts_with ~prefix:"hx-" k || k = "location")
          hs
      in
      let status =
        if status >= 500 then `Internal_server_error
        else if status >= 400 then `Not_found
        else if status >= 300 then `See_other
        else `OK
      in
      Http_kit.respond_html reqd ~status ~relay ~body:b

let handle_chat (env : Eio_unix.Stdenv.base) (sw : Eio.Switch.t)
    (st : server_state) (reqd : Httpun.Reqd.t) : unit =
  let chat : Chat_stream.t =
    {
      Chat_stream.seam = st.rt.seam;
      history = st.history;
      complete = st.complete;
      (* The model dial is a ref (POST /settings retargets the running
         loop); read at turn start, inside the producer fiber. *)
      cfg = (fun () -> !(st.cfg));
        (* REPL rule: a prompt that reads as exactly one complete Nopales
           form AND whose operator the env can call evaluates in the shared
           env, no model call (Chat_stream.repl_prompt). Prose that merely
           opens with a paren, or that lexes as one list around ordinary
           words ("(I can't reproduce this...)"), is chat. Render interp
           errors like the stdin REPL does, the OCaml default printer shows
           the opaque Interp_error(_). *)
      (* turn cap: env-overridable (GLOCHID_MAX_TURNS; legacy AGENT_MAX_TURNS), engine default 40 *)
      max_turns = Config.max_turns ();
      on_msg = Some st.on_msg;
      eval_repl =
        Some
          (fun src ->
            try Ok (Runtime.eval_string ~rt:st.rt src)
            with
            | Ast.Interp_error ie -> Error (Ast.pp_interp_error ie)
            | e -> Error (Printexc.to_string e));
        (* card 610071dc: the gate also needs the ENV, not just the reader.
           A kernel special form resolves without a binding; anything else
           must be a prim or defined name before a chat body is evaluated. *)
        repl_known =
          Some
            (fun name ->
              Eval.is_special_form name
              || Env.bound_raw st.rt.seam.env name);
    }
  in
  (* Serialize turns: a long turn must not interleave with the next
     (the queue-drain engine itself is single-turn). *)
  Eio.Mutex.lock st.turn_mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock st.turn_mutex)
    (fun () -> Chat_stream.run ~env chat reqd)

(* ---- POST /settings: retarget the running turn loop (borge/glochid.borg
   llm-config). Blank fields keep their current values (the api_key
   password input round-trips blank on every render — a blank POST body
   field is KEEP, not clear). Persists to ~/.config/glochid/config.json so
   the choice survives restarts; model is a per-surface dial (never
   replicated). htmx hx-posts (chat-header dropdown, hx-swap="none") get
   a bare 200; a browser form gets a 303 back to /settings. *)
let handle_settings (env : Eio_unix.Stdenv.base) (sw : Eio.Switch.t)
    (st : server_state) (reqd : Httpun.Reqd.t) : unit =
  let body = Http_kit.read_body reqd in
  let field n = String.trim (Http_kit.form_field body n) in
  let c = !(st.cfg) in
  let c : Provider.cfg =
    { c with
      model = (match field "model" with "" -> c.model | m -> m);
      base_url = (match field "base_url" with "" -> c.base_url | b -> b);
      api_key = (match field "api_key" with "" -> c.api_key | k -> k) }
  in
  st.cfg := c;
  (* refresh the model catalog against the new key/base so the next
     settings/chat render reflects it; a FAILED fetch keeps the
     current-model-only default, never the stale catalog *)
  st.models := [];
  (if c.api_key <> "" then
     match Provider.Openai.list_models ~env ~sw ~cfg:c () with
     | Ok ms -> st.models := ms
     | Error _ -> ());
  Config.save
    { Config.api_key = (if c.api_key = "" then None else Some c.api_key);
      base_url = Some c.base_url;
      model = Some c.model };
  let hx =
    match Httpun.Headers.get (Httpun.Reqd.request reqd).Httpun.Request.headers "hx-request" with
    | Some "true" -> true
    | _ -> false
  in
  if hx then Http_kit.respond_text reqd ~status:`OK ~relay:[] ~content_type:"text/plain" ~body:"ok"
  else begin
    let headers =
      Httpun.Headers.of_list [ ("location", "/settings"); ("content-length", "0") ]
    in
    ignore (Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers `See_other) "");
    ()
  end

(* ---- serve: bind 127.0.0.1 (default 18090), print URL, accept loop ---- *)

let serve ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t)
    ~(rt : Runtime.t) ~(cfg : Provider.cfg)
    ~(complete :
        env:Eio_unix.Stdenv.base ->
        sw:Eio.Switch.t ->
        cfg:Provider.cfg ->
        messages:Provider.message list ->
        tools:Provider.tool_def list ->
        on_delta:(Provider.delta -> unit) ->
        (Provider.reply, string) Result.t)
    ?(port = 18090) ?(open_browser = false)
    ?(persist_sessions = true)
    ?(shutdown : unit Eio.Promise.t option) (print : string -> unit) : unit =

  (* sessions-persist: NO transcript at boot. The sink creates the
     session LAZILY on the first appended message (the first user
     prompt) — page opens and harness restarts that never send a
     prompt leave nothing in the session store (the lazy [create]
     trigger lives in Sessions.on_msg). Disabled for mock providers so
     tests/smokes never write the store. A chat turn appends
     user/assistant/tool messages via [on_msg] as the loop produces
     them. *)
  let history = ref [] in
  let sess = ref None in
  let cfg_ref = ref cfg in
  let on_msg =
    Sessions.on_msg ~create:persist_sessions
      ~model:(fun () -> (!cfg_ref).Provider.model) sess
  in
  let st =
    {
      history;
      complete;
      rt;
      cfg = cfg_ref;
      models = ref [];
      turn_mutex = Eio.Mutex.create ();
      sess;
      on_msg;
    }
  in
  (* llm-config host prims for the Nopales settings/chat surfaces
     (borge/glochid.borg llm-config). glochid/models is CACHED — the
     provider ABI (/models) is hit once lazily and refetched on POST
     /settings, so a chat-page render stays local. *)
  Runtime.add_prim st.rt "glochid/models"
    (fun _ ->
      (match !(st.models) with
       | [] when !(st.cfg).Provider.api_key <> "" ->
           (match Provider.Openai.list_models ~env ~sw ~cfg:!(st.cfg) () with
            | Ok ms -> st.models := ms
            | Error _ -> ())
       | _ -> ());
      let models =
        match !(st.models) with
        | [] -> [ !(st.cfg).Provider.model ]
        | ms ->
            if List.mem !(st.cfg).Provider.model ms then ms
            else !(st.cfg).Provider.model :: ms
      in
      Ast.VList (List.map (fun m -> Ast.VStr m) models));
  Runtime.add_prim st.rt "glochid/model"
    (fun _ -> Ast.VStr !(st.cfg).Provider.model);
  Runtime.add_prim st.rt "glochid/base-url"
    (fun _ -> Ast.VStr !(st.cfg).Provider.base_url);
  Runtime.add_prim st.rt "glochid/key-status"
    (fun _ ->
      match !(st.cfg).Provider.api_key with
      | "" -> Ast.VStr "not set"
      | k -> Ast.VStr ("set \194\183 last4=" ^ Config.last4 k));
  (* Live session prims (borge/glochid.borg sessions-persist) over the
     surface's history + current-session ref; /new /resume /sessions
     REPL commands ride the same seam (hashtable registration). *)
  Sessions.register ~rt ~history:st.history ~sess:st.sess
    ~model:(fun () -> !(st.cfg).Provider.model);
  let request_handler (sa : Eio.Net.Sockaddr.stream)
      (g : Httpun.Reqd.t Gluten.reqd) : unit =
    ignore sa;
    let reqd = g.Gluten.reqd in
    let req = Httpun.Reqd.request reqd in
    let target = req.Httpun.Request.target in
    let path, query =
      match String.index_opt target '?' with
      | Some i ->
          ( String.sub target 0 i,
            Some (String.sub target (i + 1) (String.length target - i - 1)) )
      | None -> (target, None)
    in
    let query_params =
      match query with Some q -> Http_kit.parse_query q | None -> []
    in
    let fork_work (f : unit -> unit) : unit =
      Eio.Fiber.fork ~sw (fun () ->
          (try f () with
           | Eio.Cancel.Cancelled _ as e -> raise e
           | exn -> Printf.eprintf "[glochid-ui] handler error: %s\n%!" (Printexc.to_string exn)))
    in
    match req.Httpun.Request.meth with
    | `GET -> (
        if String.length path >= 8 && String.sub path 0 8 = "/assets/" then
          fork_work (fun () ->
              serve_asset reqd (String.sub path 8 (String.length path - 8)))
        else if Habitat.fwd_matches_prefix path <> None then
          fork_work (fun () ->
              let body = Http_kit.read_body reqd in
              dispatch_fwd env reqd "GET" path
                (match query with Some q -> q | None -> "")
                body
                ~hx:(dispatch_headers reqd <> []))
        else fork_work (fun () -> dispatch_get st reqd target))
    | `POST -> (
        match path with
        | "/chat" -> fork_work (fun () -> handle_chat env sw st reqd)
        | "/settings" -> fork_work (fun () -> handle_settings env sw st reqd)
        | _ -> (
            (* habitat-pane forwarder rides BEFORE the route table: a
               registered prefix (/todo /notes /calendar) forwards to the
               habitat with the operator's session (glochid.borg
               habitat-panes). *)
            if Habitat.fwd_matches_prefix path <> None then
              fork_work (fun () ->
                  let body = Http_kit.read_body reqd in
                  dispatch_fwd env reqd "POST" path
                    (match query with Some q -> q | None -> "")
                    body
                    ~hx:(dispatch_headers reqd <> []))
            else
            (* filmstrip-shell host seam #3: POSTs to unknown paths fall
               through to the route table (the /filmstrip/* shell routes
               live there). hx-vals ride the form-encoded body; params
               = query ++ body so either channel reaches request-param. *)
            match Runtime.lookup_route ~rt:st.rt path with
            | None ->
                Http_kit.respond_html reqd ~status:`Not_found ~relay:[]
                  ~body:(Http_kit.status_page "404"
                           (Printf.sprintf "<p>no route %s</p>" (Http_kit.esc path)))
            | Some handler ->
                fork_work (fun () ->
                    let body = Http_kit.read_body reqd in
                    let params =
                      query_params @ Http_kit.parse_form_body body
                    in
                    let v =
                      Runtime.dispatch_route ~rt:st.rt ~handler ~method_:"POST"
                        ~path ~params ~headers:(dispatch_headers reqd) ~body
                    in
                    let html =
                      match v with
                      | Ast.VStr s -> s
                      | other -> Pricklypear_lisp.Reader.pp_value other
                    in
                    Http_kit.respond_html reqd ~status:`OK ~relay:[] ~body:html)))
    | _ -> Http_kit.respond_html reqd ~status:`Not_found ~relay:[]
             ~body:(Http_kit.status_page "404" "<p>method not supported</p>")
  in
  let error_handler (_sa : Eio.Net.Sockaddr.stream)
      ?(request : Httpun.Request.t option)
      (err : Httpun.Server_connection.error)
      (respond : Httpun.Headers.t -> Httpun.Body.Writer.t) : unit =
    ignore request;
    ignore err;
    let body = "error" in
    let body_w =
      respond
        (Httpun.Headers.of_list
           [ ("content-type", "text/plain"); ("content-length", string_of_int (String.length body)) ])
    in
    Httpun.Body.Writer.write_string body_w body;
    Httpun.Body.Writer.close body_w
  in
  let conn_handler =
    Httpun_eio.Server.create_connection_handler ~request_handler
      ~error_handler ~sw
  in
  let net = Eio.Stdenv.net env in
  (* Fixed default port so the URL is stable across runs; a busy port
     falls back to a random one (the old --serve behavior). *)
  let bind p =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:false ~backlog:64 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, p))
  in
  let socket =
    try bind port
    with _ ->
      print (Printf.sprintf "port %d busy; using a random port\n" port);
      bind 0
  in
  let bound_addr = Eio.Net.listening_addr socket in
  let port =
    match bound_addr with `Tcp (_ip, p) -> p | _ -> 0
  in
  let url = Printf.sprintf "http://127.0.0.1:%d/" port in
  print (Printf.sprintf "glochid serving %s\n" url);
  if open_browser then (
    try
      let _ = Sys.command (Printf.sprintf "xdg-open '%s' >/dev/null 2>&1 &" url) in
      ()
    with _ -> ());
  let on_error ex = ignore ex in
  (* Run the accept loop under a CHILD switch raced against [shutdown].
     Using Fiber.first in a fresh Switch.run lets the return path
     cancel the still-blocked accept loop — Switch.run waits for both
     fibers to return, so without the child switch the calling fiber
     would end on shutdown while the accept fiber remains blocked in
     the outer switch, deadlocking the process. *)
  let accept_loop () =
    while true do
      Eio.Net.accept_fork socket ~sw ~on_error (fun client_sock client_addr ->
          try conn_handler client_addr client_sock
          with exn -> ignore exn)
    done
  in
  (match shutdown with
   | None -> accept_loop ()
   | Some p ->
       (* Fiber.first cancels the loser. The accept loop runs in its
          own switch scope so its cancellation is contained — when
          [shutdown] resolves we return from [serve] cleanly. *)
       Eio.Switch.run @@ fun _loop_sw ->
       Eio.Fiber.first
         (fun () -> Eio.Switch.run (fun _ -> accept_loop ()))
         (fun () -> Eio.Promise.await p));
  print "glochid: shutting down (signal)\n"
