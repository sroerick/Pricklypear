(** pp-remote HTML surface server (borge/local-first.borg surfaces +
    agent-loop).

    A local HTTP server (Httpun_eio) bound to 127.0.0.1 on a random
    port. The pages are NOPALES-AUTHORED htmx fragments on the client
    route table (rendered by Nopales via [libs/html]), dispatched
    in-process through [Remote.dispatch_route]. The eval POST handler
    is OCaml (it calls [Remote.eval] and renders the result via the
    Nopales [eval/result] template).

    The CHAT TURN is the shared agent loop running on THIS host
    (borge/local-first.borg agent-loop): provider call local (the API
    key is a value from the environment), tool bodies eval under the
    proxy rim (DB prims cross to /api/prim), streaming as
    multipart/mixed htmx fragments — the same emitter + Nopales
    fragment templates the agent serves (chat/user-bubble,
    chat/turn-open, chat/think-open, chat/think-token, chat/tool-open,
    chat/tool-line, chat/tool-done, chat/token, chat/error). The turn
    engine + emitter + HTTP plumbing are the SHARED hostkit modules
    (Chat_stream / Multipart_emitter / Http_kit, i-1788671304-d) — this
    file keeps only the client-specific glue: the /eval handler, the
    whole-surface req_mutex, and the random-port accept loop. *)

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
  rt : Remote.t;
  cfg : Provider.cfg;
}

(* ---- GET route dispatch through the client route table ---- *)

let dispatch_get (rt : Remote.t) (reqd : Httpun.Reqd.t) (target : string)
    : unit =
  let path, query =
    match String.index_opt target '?' with
    | Some i ->
        ( String.sub target 0 i,
          String.sub target (i + 1) (String.length target - i - 1) )
    | None -> (target, "")
  in
  let params = Http_kit.parse_query query in
  match Remote.lookup_route ~rt path with
  | None ->
      Http_kit.respond_html reqd ~status:`Not_found ~relay:[]
        ~body:(Http_kit.status_page "404"
                 (Printf.sprintf "<p>no route %s</p>" (Http_kit.esc path)))
  | Some handler ->
      (try
         let v =
           Remote.dispatch_route ~rt ~handler ~method_:"GET" ~path ~params
             ~headers:[] ~body:""
         in
         let html =
           match v with Ast.VStr s -> s | other -> Pricklypear_lisp.Reader.pp_value other
         in
         Http_kit.respond_html reqd ~status:`OK ~relay:[] ~body:html
       with exn ->
         Http_kit.respond_html reqd ~status:`Internal_server_error ~relay:[]
           ~body:(Http_kit.status_page "error"
                    (Printf.sprintf "<p>dispatch error: %s</p>"
                       (Http_kit.esc (Printexc.to_string exn)))))

(* ---- static assets (vendored, baked into the binary) ---- *)

let serve_asset (reqd : Httpun.Reqd.t) (name : string) : unit =
  Http_kit.serve_asset reqd ~assets:
    [
      ("htmx.min.js", Assets.htmx_min, "application/javascript; charset=utf-8");
      ("htmx-2-compat.min.js", Assets.htmx_2_compat, "application/javascript; charset=utf-8");
      ("hx-multipart.js", Assets.hx_multipart, "application/javascript; charset=utf-8");
    ]
    name

(* ---- POST /eval: OCaml handler -> Remote.eval -> Nopales template ---- *)

let handle_eval (rt : Remote.t) (reqd : Httpun.Reqd.t) : unit =
  let body_raw = Http_kit.read_body reqd in
  let expr = Http_kit.form_field body_raw "expr" in
  let result =
    if String.trim expr = "" then "(no input)"
    else
      try
        let v = Remote.eval rt expr in
        let base = Pricklypear_lisp.Reader.pp_value v in
        (* bumpers M2 (borge/bumpers.borg notices-envelope): lens local
           define notices rendered muted under the result. *)
        let ns =
          List.map
            (fun n -> ";; notice: " ^ Wire_codec.notice_to_line n)
            (Remote.take_local_notices rt)
        in
        (match ns with
         | [] -> base
         | _ -> String.concat "\n" (base :: ns))
      with
      | Ast.Interp_error ie -> Ast.pp_interp_error ie
      | Reader.Reader_error (msg, ln, col) ->
          Printf.sprintf "read error %d:%d %s" ln col msg
      | exn -> Printf.sprintf "error: %s" (Printexc.to_string exn)
  in
  (* Render the result fragment via the Nopales template so it is
     redefinable live; fall back to a plain <pre> if the template is
     unbound or errors. *)
  let html =
    match Remote.call_template ~rt "eval/result" [ Ast.VStr expr; Ast.VStr result ] with
    | Some h -> h
    | None ->
        Printf.sprintf "<div class=\"expr\">$ %s</div><pre>%s</pre>"
          (Http_kit.esc expr) (Http_kit.esc result)
  in
  Http_kit.respond_html reqd ~status:`OK ~relay:[] ~body:html

(* ---- chat turn: the shared Chat_stream engine (the shared agent loop
   on this host, multipart/mixed) ---- *)

let handle_chat (env : Eio_unix.Stdenv.base) (sw : Eio.Switch.t)
    (st : server_state) (reqd : Httpun.Reqd.t) : unit =
  ignore sw;
  let chat : Chat_stream.t =
    {
      Chat_stream.seam = st.rt.seam;
      history = st.history;
      complete = st.complete;
      cfg = (fun () -> st.cfg);
      (* No REPL branch on the client: every prompt is a model turn
         (the client has no shared local env to eval into — the agent's
         REPL-mode prompt rule is host-specific). *)
      eval_repl = None;
      max_turns = Loop.max_turns_default;
      (* pp-remote has no local session store (sessions live in the
         agent's .agent store); turns are not persisted here. *)
      on_msg = None;
    }
  in
  Chat_stream.run ~env chat reqd

(* ---- serve: bind 127.0.0.1:0, print URL, accept loop ---- *)

let serve ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t)
    ~(rt : Remote.t) ~(cfg : Provider.cfg)
    ~(complete :
        env:Eio_unix.Stdenv.base ->
        sw:Eio.Switch.t ->
        cfg:Provider.cfg ->
        messages:Provider.message list ->
        tools:Provider.tool_def list ->
        on_delta:(Provider.delta -> unit) ->
        (Provider.reply, string) Result.t)
    ?(open_browser = false) (print : string -> unit) : unit =
  let st = { history = ref []; complete; rt; cfg } in
  (* Serialize all HTTP requests through one mutex: the client env is
     single-threaded state (lazy def cache, route table, current_request
     ref). [Remote.dispatch_route] and [Remote.call_template] run under
     the proxy rim; the mutex keeps concurrent requests from interleaving
     on the env / transport. A chat turn holds the mutex for its whole
     lifetime — a long turn parks other tabs until it finishes (v0
     single-user posture; the agent interleaves via its per-eval lock,
     the client has no eval mutex yet). *)
  let req_mutex = Eio.Mutex.create () in
  let with_mutex f =
    Eio.Mutex.lock req_mutex;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock req_mutex) f
  in
  let request_handler (sa : Eio.Net.Sockaddr.stream)
      (g : Httpun.Reqd.t Gluten.reqd) : unit =
    ignore sa;
    let reqd = g.Gluten.reqd in
    let req = Httpun.Reqd.request reqd in
    let target = req.Httpun.Request.target in
    let path, _query =
      match String.index_opt target '?' with
      | Some i ->
          ( String.sub target 0 i,
            Some (String.sub target (i + 1) (String.length target - i - 1)) )
      | None -> (target, None)
    in
    let fork_work (f : unit -> unit) : unit =
      Eio.Fiber.fork ~sw (fun () ->
          (try f () with
           | Eio.Cancel.Cancelled _ as e -> raise e
           | exn -> Printf.eprintf "[pp-remote-ui] handler error: %s\n%!" (Printexc.to_string exn)))
    in
    match req.Httpun.Request.meth with
    | `GET -> (
        if String.length path >= 8 && String.sub path 0 8 = "/assets/" then
          fork_work (fun () ->
              serve_asset reqd (String.sub path 8 (String.length path - 8)))
        else
          fork_work (fun () -> with_mutex (fun () -> dispatch_get rt reqd target)))
    | `POST -> (
        match path with
        | "/eval" -> fork_work (fun () -> with_mutex (fun () -> handle_eval rt reqd))
        | "/chat" ->
            fork_work (fun () -> with_mutex (fun () -> handle_chat env sw st reqd))
        | _ ->
            Http_kit.respond_html reqd ~status:`Not_found ~relay:[]
              ~body:(Http_kit.status_page "404"
                       (Printf.sprintf "<p>no route %s</p>" (Http_kit.esc path))))
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
  let listen_addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:false ~backlog:64 ~sw net
      listen_addr
  in
  let bound_addr = Eio.Net.listening_addr socket in
  let port =
    match bound_addr with `Tcp (_ip, p) -> p | _ -> 0
  in
  let url = Printf.sprintf "http://127.0.0.1:%d/" port in
  print (Printf.sprintf "pp-remote serving %s\n" url);
  if open_browser then (
    try
      let _ = Sys.command (Printf.sprintf "xdg-open '%s' >/dev/null 2>&1 &" url) in
      ()
    with _ -> ());
  let on_error ex = ignore ex in
  while true do
    Eio.Net.accept_fork socket ~sw ~on_error (fun client_sock client_addr ->
        try conn_handler client_addr client_sock
        with exn -> ignore exn)
  done
