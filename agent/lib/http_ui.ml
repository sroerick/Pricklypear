(** Agent HTML primary surface (borge/agent.borg surfaces).

    A local HTTP server (Httpun_eio) bound to 127.0.0.1 on a random
    port. The chat + code browser pages are NOPALES-AUTHORED htmx
    fragments on the agent route table (rendered by Nopales via
    libs/html), dispatched in-process through [Runtime.dispatch_route].
    A chat/agent turn streams as multipart/mixed htmx fragments (one
    part per (html, HX-Target, HX-Swap)), emitted by OCaml; the fragment
    templates are Nopales (chat/user-bubble, chat/turn-open,
    chat/think-open, chat/think-token, chat/tool-open, chat/tool-line,
    chat/tool-done, chat/token, chat/error), so the surface is
    redefinable live. Each emitted part is
    also collected as data so a consumed turn replays through
    [hyper/stream] (surfaces smoke).

    Bootstrap/asset plumbing stays OCaml (per the surfaces scope): the
    vendored htmx + hx-multipart assets are baked into the binary
    ([Assets]) and served at /assets/*.js. *)

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
  cfg : Provider.cfg;
  turn_mutex : Eio.Mutex.t;
}

(* ---- HTML escaping (status/error pages only; surfaces are Nopales) ---- *)

let esc (s : string) : string =
  let buf = Buffer.create (String.length s + 16) in
  String.iter
    (fun c ->
      match c with
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let status_page (title : string) (body : string) : string =
  Printf.sprintf
    {|<!doctype html><html><head><meta charset="utf-8"><title>%s</title>
<style>body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,monospace;padding:1.5rem}a{color:#7aa2f7}</style>
</head><body>%s</body></html>|}
    (esc title) body

(* ---- HTTP plumbing ---- *)

let read_body (reqd : Httpun.Reqd.t) : string =
  let body = Httpun.Reqd.request_body reqd in
  let promise, resolver = Eio.Promise.create () in
  let buf = Buffer.create 1024 in
  let rec sched () =
    Httpun.Body.Reader.schedule_read body
      ~on_eof:(fun () -> Eio.Promise.resolve resolver (Buffer.contents buf))
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
        sched ())
  in
  sched ();
  Eio.Promise.await promise

let respond_text (reqd : Httpun.Reqd.t) ~(status : Httpun.Status.t)
    ~(content_type : string) ~(body : string) : unit =
  let headers =
    Httpun.Headers.of_list
      [
        ("content-type", content_type);
        ("content-length", string_of_int (String.length body));
      ]
  in
  let _body_w =
    Httpun.Reqd.respond_with_string reqd
      (Httpun.Response.create ~headers status)
      body
  in
  ()

let respond_html (reqd : Httpun.Reqd.t) ~(status : Httpun.Status.t)
    ~(body : string) : unit =
  respond_text reqd ~status ~content_type:"text/html; charset=utf-8" ~body

let is_closed_writer_exn = function
  | Failure msg ->
      String.equal msg "cannot write to closed writer"
      || String.starts_with ~prefix:"cannot write to closed writer" msg
  | _ -> false

let close_writer_quiet (body : Httpun.Body.Writer.t) : unit =
  (try Httpun.Body.Writer.close body
   with exn when is_closed_writer_exn exn -> ())

(* Bail out of the multipart drain loop when the client disconnects. *)
exception Peer_gone

(* ---- multipart/mixed streaming (ported from image http_agent.ml) ---- *)

(* One multipart part frame + flush. Mirrors the image's mp_write_part:
   --boundary + Content-Type: text/html + optional HX-Target/HX-Swap +
   blank + html. Returns `Peer_gone on closed writer so the drain loop
   stops cleanly. *)
let mp_write_part (body : Httpun.Body.Writer.t) ~boundary
    ?(target : string option) ?(swap : string option) (html : string)
  : [ `Ok | `Peer_gone ] =
  let buf = Buffer.create 128 in
  Buffer.add_string buf ("--" ^ boundary ^ "\r\n");
  Buffer.add_string buf "Content-Type: text/html; charset=utf-8\r\n";
  (match target with Some t -> Buffer.add_string buf ("HX-Target: " ^ t ^ "\r\n") | None -> ());
  (match swap with Some s -> Buffer.add_string buf ("HX-Swap: " ^ s ^ "\r\n") | None -> ());
  Buffer.add_string buf "\r\n";
  Buffer.add_string buf html;
  Buffer.add_string buf "\r\n";
  let chunk = Buffer.contents buf in
  try
    Httpun.Body.Writer.write_string body chunk;
    let promise, resolver = Eio.Promise.create () in
    let wake () =
      if not (Eio.Promise.is_resolved promise) then Eio.Promise.resolve resolver ()
    in
    (try Httpun.Body.Writer.flush body (fun () -> wake ())
     with exn when is_closed_writer_exn exn -> wake ());
    Eio.Promise.await promise;
    `Ok
  with exn when is_closed_writer_exn exn ->
    `Peer_gone

let mp_close (body : Httpun.Body.Writer.t) ~boundary : unit =
  let chunk = "--" ^ boundary ^ "--\r\n" in
  (try Httpun.Body.Writer.write_string body chunk
   with exn when is_closed_writer_exn exn -> ());
  close_writer_quiet body

(* ---- query-string parsing (route params for GET dispatch) ---- *)

let url_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec go i =
    if i >= len then Buffer.contents buf
    else if i + 2 < len && s.[i] = '%' then (
      (try
         Buffer.add_char buf
           (Char.chr (int_of_string ("0x" ^ String.sub s (i + 1) 2)))
       with _ -> ());
      go (i + 3))
    else (
      Buffer.add_char buf (if s.[i] = '+' then ' ' else s.[i]);
      go (i + 1))
  in
  go 0

let parse_query (q : string) : (string * string) list =
  if q = "" then []
  else
    List.filter_map
      (fun kv ->
        match String.index_opt kv '=' with
        | Some i ->
            let k = String.sub kv 0 i in
            let v = String.sub kv (i + 1) (String.length kv - i - 1) in
            Some (url_decode k, url_decode v)
        | None -> Some (url_decode kv, ""))
      (String.split_on_char '&' q)

(* ---- GET route dispatch through the agent route table ---- *)

let dispatch_get (st : server_state) (reqd : Httpun.Reqd.t) (target : string)
    : unit =
  let path, query =
    match String.index_opt target '?' with
    | Some i ->
        ( String.sub target 0 i,
          String.sub target (i + 1) (String.length target - i - 1) )
    | None -> (target, "")
  in
  let params = parse_query query in
  match Runtime.lookup_route ~rt:st.rt path with
  | None ->
      respond_html reqd ~status:`Not_found
        ~body:(status_page "404" (Printf.sprintf "<p>no route %s</p>" (esc path)))
  | Some handler ->
      let v =
        Runtime.dispatch_route ~rt:st.rt ~handler ~method_:"GET" ~path
          ~params ~headers:[] ~body:""
      in
      let html =
        match v with Ast.VStr s -> s | other -> Pricklypear_lisp.Reader.pp_value other
      in
      respond_html reqd ~status:`OK ~body:html

(* ---- static assets (vendored, baked into the binary) ---- *)

let serve_asset (reqd : Httpun.Reqd.t) (name : string) : unit =
  let body, ct =
    match name with
    | "htmx.min.js" -> (Assets.htmx_min, "application/javascript; charset=utf-8")
    | "htmx-2-compat.min.js" -> (Assets.htmx_2_compat, "application/javascript; charset=utf-8")
    | "hx-multipart.js" -> (Assets.hx_multipart, "application/javascript; charset=utf-8")
    | _ -> ("", "application/octet-stream")
  in
  if body = "" then
    respond_html reqd ~status:`Not_found
      ~body:(status_page "404" (Printf.sprintf "<p>no asset %s</p>" (esc name)))
  else respond_text reqd ~status:`OK ~content_type:ct ~body

(* ---- chat turn: multipart/mixed stream + part collection ---- *)

(* A captured part for hyper/stream replay, in the alist shape
   soup/apply-parts! consumes: (("html" . frag) ("target" . sel) ("swap" . strat)). *)
let part_alist ~html ~target ~swap : Ast.value =
  Ast.VList
    [ Ast.VPair (Ast.VStr "html", Ast.VStr html)
    ; Ast.VPair (Ast.VStr "target", Ast.VStr target)
    ; Ast.VPair (Ast.VStr "swap", Ast.VStr swap)
    ]

(* url-decode a form field value (htmx posts application/x-www-form-urlencoded). *)
let form_field (body : string) (name : string) : string =
  let prefix = name ^ "=" in
  let rec find i =
    if i >= String.length body then None
    else if i = 0 || body.[i - 1] = '&' then
      if String.length body - i >= String.length prefix
         && String.sub body i (String.length prefix) = prefix
      then
        let rest_start = i + String.length prefix in
        let rest_end =
          match String.index_from_opt body rest_start '&' with
          | Some j -> j | None -> String.length body
        in
        Some (String.sub body rest_start (rest_end - rest_start))
      else find (i + 1)
    else find (i + 1)
  in
  match find 0 with None -> "" | Some v -> url_decode v

let handle_chat (env : Eio_unix.Stdenv.base) (sw : Eio.Switch.t)
    (st : server_state) (reqd : Httpun.Reqd.t) : unit =
  let body_raw = read_body reqd in
  let prompt = form_field body_raw "msg" in
  let boundary = Printf.sprintf "pp%x%x" (Random.bits ()) (Random.bits ()) in
  let rid = Printf.sprintf "llm-%d" (Random.bits () land 0xFFFFFF) in
  let headers =
    Httpun.Headers.of_list
      [
        ("content-type", "multipart/mixed; boundary=" ^ boundary);
        ("cache-control", "no-cache");
        ("x-accel-buffering", "no");
        (* Chunked framing is MANDATORY for a streamed response: without an
           explicit framing header httpun emits the body close-delimited,
           and on a keep-alive HTTP/1.1 connection the client can never
           detect end-of-body (fetch().text() never resolves). *)
        ("transfer-encoding", "chunked");
      ]
  in
  let body_w =
    Httpun.Reqd.respond_with_streaming ~flush_headers_immediately:true
      reqd (Httpun.Response.create ~headers `OK)
  in
  (* Captured parts for hyper/stream replay (data). *)
  let parts = ref [] in
  let emit ?target ?swap html =
    let target = match target with Some t -> t | None -> "#log" in
    let swap = match swap with Some s -> s | None -> "beforeend" in
    parts := part_alist ~html ~target ~swap :: !parts;
    match mp_write_part body_w ~boundary ~target ~swap html with
    | `Ok -> ()
    | `Peer_gone -> raise Peer_gone
  in
  let run () =
    (* Openers: user echo bubble + empty llm bubble (rendered by Nopales
       templates so they are redefinable live). *)
    (match Runtime.call_template ~rt:st.rt "chat/user-bubble" [ Ast.VStr prompt ]
     with Some html -> emit html | None -> emit (Printf.sprintf "<div class=\"bubble user\">%s</div>" (esc prompt)));
    (* Turn skeleton: one container with TWO swap targets -- #<rid>-think
       inside the collapsible <details> (reasoning spans + tool traces)
       and #<rid> response bubble (content tokens). *)
    (match Runtime.call_template ~rt:st.rt "chat/turn-open" [ Ast.VStr rid ]
     with Some html -> emit html
      | None ->
          emit
            (Printf.sprintf
               "<div class=\"turn\"><details class=\"think\"><summary>thinking + tools</summary><div id=\"%s-think\"></div></details><div class=\"bubble llm\" id=\"%s\"></div></div>"
               rid rid));
    (* Producer fiber: run the agent turn, pushing events to a queue. *)
    let queue : Loop.turn_event Queue.t = Queue.create () in
    let qmutex = Eio.Mutex.create () in
    let qcond = Eio.Condition.create () in
    let finished = ref false in
    let push (e : Loop.turn_event) : unit =
      Eio.Mutex.lock qmutex;
      Queue.add e queue;
      Eio.Condition.broadcast qcond;
      Eio.Mutex.unlock qmutex
    in
    let on_event = function
      | Loop.Reasoning_text s -> push (Loop.Reasoning_text s)
      | Loop.Assistant_text s -> push (Loop.Assistant_text s)
      | Loop.Tool_begin (n, i) -> push (Loop.Tool_begin (n, i))
      | Loop.Tool_end (n, o) -> push (Loop.Tool_end (n, o))
      | Loop.Turn_error msg -> push (Loop.Turn_error msg)
    in
    (* Nested collapsibles INSIDE the turn's "thinking + tools" zone
       (#<rid>-think): each contiguous reasoning run gets its own
       <details> (chat/think-open) and each tool call its own
       (chat/tool-open, summary marked done by chat/tool-done).
       cur_think / open_tools track which inner div deltas and lines
       stream into. *)
    let think_zone = "#" ^ rid ^ "-think" in
    let think_n = ref 0 in
    let cur_think : string option ref = ref None in
    let tool_n = ref 0 in
    let open_tools : string list ref = ref [] in
    let open_think_block () : string =
      incr think_n;
      let tid = Printf.sprintf "%s-tk-%d" rid !think_n in
      (match Runtime.call_template ~rt:st.rt "chat/think-open" [ Ast.VStr tid ] with
       | Some html -> emit ~target:think_zone html
       | None ->
           emit ~target:think_zone
             (Printf.sprintf
                "<details class=\"tk\"><summary>thinking</summary><div id=\"%s\"></div></details>"
                tid));
      cur_think := Some tid;
      tid
    in
    let emit_tool_line tid label text =
      match Runtime.call_template ~rt:st.rt "chat/tool-line"
              [ Ast.VStr label; Ast.VStr text ] with
      | Some html -> emit ~target:("#" ^ tid) html
      | None ->
          emit ~target:("#" ^ tid)
            (Printf.sprintf "<div class=\"trace\">%s %s</div>"
               (esc label) (esc text))
    in
    Eio.Switch.run @@ fun lsw ->
    Eio.Fiber.fork ~sw:lsw (fun () ->
        (match
           Loop.run ~env ~sw:lsw ~rt:st.rt ~cfg:st.cfg ~complete:st.complete
             ~on_event st.history prompt
         with
         | Ok () -> ()
         | Error e ->
             (* A failed turn must be VISIBLE in the stream: closing the
                response without an error part reads as a dead UI. *)
             push (Loop.Turn_error e));
        Eio.Mutex.lock qmutex;
        finished := true;
        Eio.Condition.broadcast qcond;
        Eio.Mutex.unlock qmutex);
    let rec drain () =
      Eio.Mutex.lock qmutex;
      while Queue.is_empty queue && not !finished do
        Eio.Condition.await qcond qmutex
      done;
      let items = List.of_seq (Queue.to_seq queue) in
      Queue.clear queue;
      let fin = !finished && Queue.is_empty queue in
      Eio.Mutex.unlock qmutex;
      List.iter
        (fun e ->
          (match e with
           | Loop.Reasoning_text s ->
               let tid =
                 match !cur_think with
                 | Some t -> t
                 | None -> open_think_block ()
               in
               (match Runtime.call_template ~rt:st.rt "chat/think-token" [ Ast.VStr s ]
                with Some html -> emit ~target:("#" ^ tid) html
                 | None ->
                     emit ~target:("#" ^ tid)
                       (Printf.sprintf "<span class=\"reason\">%s</span>" (esc s)))
           | Loop.Assistant_text s ->
               cur_think := None;
               (match Runtime.call_template ~rt:st.rt "chat/token" [ Ast.VStr s ]
                with Some html -> emit ~target:("#" ^ rid) html
                 | None -> emit ~target:("#" ^ rid) (Printf.sprintf "<span>%s</span>" (esc s)))
           | Loop.Tool_begin (n, i) ->
               cur_think := None;
               incr tool_n;
               let tid = Printf.sprintf "%s-tool-%d" rid !tool_n in
               open_tools := tid :: !open_tools;
               (match Runtime.call_template ~rt:st.rt "chat/tool-open"
                        [ Ast.VStr tid; Ast.VStr n ]
                with
                | Some html -> emit ~target:think_zone html
                | None ->
                    emit ~target:think_zone
                      (Printf.sprintf
                         "<details class=\"tool\"><summary id=\"%s-sum\">tool: %s</summary><div id=\"%s\"></div></details>"
                         tid (esc n) tid));
               emit_tool_line tid "in" i
           | Loop.Tool_end (n, o) ->
               let tid =
                 match !open_tools with
                 | t :: rest -> open_tools := rest; t
                 | [] -> rid ^ "-think"
               in
               emit_tool_line tid "out" o;
               if tid <> rid ^ "-think" then
                 (match Runtime.call_template ~rt:st.rt "chat/tool-done" [ Ast.VStr n ]
                  with
                  | Some html ->
                      emit ~target:("#" ^ tid ^ "-sum") ~swap:"innerHTML" html
                  | None ->
                      emit ~target:("#" ^ tid ^ "-sum") ~swap:"innerHTML"
                        (Printf.sprintf "tool: %s &#10003;" (esc n)))
           | Loop.Turn_error msg ->
               (match Runtime.call_template ~rt:st.rt "chat/error"
                  [ Ast.VStr msg ]
                with
                | Some html -> emit ~target:("#" ^ rid) html
                | None ->
                    emit ~target:("#" ^ rid)
                      (Printf.sprintf
                         "<div class=\"trace\">[turn error: %s]</div>"
                         (esc msg)))))
        items;
      if not fin then drain ()
    in
    drain ()
  in
  Eio.Mutex.lock st.turn_mutex;
  Fun.protect
    ~finally:(fun () ->
      Eio.Mutex.unlock st.turn_mutex;
      (* Record the captured parts for hyper/stream replay. *)
      st.rt.Runtime.last_turn_parts <- Ast.VList (List.rev !parts);
      (try mp_close body_w ~boundary with Peer_gone -> ()))
    (fun () -> (try run () with Peer_gone -> ()))

(* ---- serve: bind 127.0.0.1:0, print URL, accept loop ---- *)

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
    ?(open_browser = false) (print : string -> unit) : unit =
  let st =
    {
      history = ref [];
      complete;
      rt;
      cfg;
      turn_mutex = Eio.Mutex.create ();
    }
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
           | exn -> Printf.eprintf "[agent-ui] handler error: %s\n%!" (Printexc.to_string exn)))
    in
    match req.Httpun.Request.meth with
    | `GET -> (
        if String.length path >= 8 && String.sub path 0 8 = "/assets/" then
          fork_work (fun () ->
              serve_asset reqd (String.sub path 8 (String.length path - 8)))
        else
          fork_work (fun () -> dispatch_get st reqd target))
    | `POST -> (
        match path with
        | "/chat" -> fork_work (fun () -> handle_chat env sw st reqd)
        | _ ->
            respond_html reqd ~status:`Not_found
              ~body:(status_page "404" (Printf.sprintf "<p>no route %s</p>" (esc path))))
    | _ -> respond_html reqd ~status:`Not_found ~body:(status_page "404" "<p>method not supported</p>")
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
  print (Printf.sprintf "agent serving %s\n" url);
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
