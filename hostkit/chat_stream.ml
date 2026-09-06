(** Chat_stream — the chat/agent turn as a multipart/mixed htmx
    fragment stream, ONE engine for both local hosts (agent + pp-remote,
    unified by i-1788671304-d out of two ~200-line mirrored handle_chat
    bodies). A turn: user bubble opener (Nopales chat/user-bubble
    template, redefinable live), then either the REPL branch (a
    paren-leading prompt evaluated locally — agent only) or the shared
    agent loop (llm/lib/loop.ml) on a producer fiber whose events drain
    through a queue into nested thinking/tool collapsibles + token
    bubbles. Every fragment template (chat/turn-open, chat/think-open,
    chat/think-token, chat/tool-open, chat/tool-line, chat/tool-done,
    chat/token, chat/error) renders through the host seam, so the
    surface is redefinable live; each emitted part is also captured so a
    consumed turn replays through [hyper/stream] (surfaces smoke).

    DRIFT FIX (why the merge is behavior-changing on pp-remote): the
    remote copy's [tpl] helper emitted template-rendered fragments with
    DEFAULT target/swap (#log/beforeend) for every event part, while the
    agent copy targeted each part correctly (think-open → #<rid>-think,
    tool-line/think-token → #<tid>, token → #<rid>, tool-done →
    #<tid>-sum with innerHTML). pp-remote binds the same chat/*
    templates, so its streamed turns assembled into the wrong slots.
    This shared engine carries the agent's targeting. *)

open Pricklypear_lisp

(** A captured part for hyper/stream replay, in the alist shape
   soup/apply-parts! consumes: (("html" . frag) ("target" . sel) ("swap" . strat)). *)
let part_alist ~html ~target ~swap : Ast.value =
  Ast.VList
    [ Ast.VPair (Ast.VStr "html", Ast.VStr html)
    ; Ast.VPair (Ast.VStr "target", Ast.VStr target)
    ; Ast.VPair (Ast.VStr "swap", Ast.VStr swap)
    ]

(** One chat-host record: the host seam (route/template/tool backing)
    plus the turn knobs. [history] is the shared conversation;
    [complete] the provider-completion function; [cfg] is read at turn
    start (the agent's model dial is a ref that POST /settings may have
    just retargeted); [eval_repl] enables the REPL branch (a prompt
    whose first non-space char is "(" evaluates as Nopales in the
    shared env — no model call). *)
type t = {
  seam : Host_seam.t;
  history : Provider.message list ref;
  complete :
    env:Eio_unix.Stdenv.base ->
    sw:Eio.Switch.t ->
    cfg:Provider.cfg ->
    messages:Provider.message list ->
    tools:Provider.tool_def list ->
    on_delta:(Provider.delta -> unit) ->
    (Provider.reply, string) Result.t;
  cfg : unit -> Provider.cfg;
  eval_repl : (string -> (Ast.value, string) Result.t) option;
}

(** Run one streamed turn for [reqd] (POST /chat, form field "msg").
    Serialization is the CALLER's concern: the agent holds its
    turn_mutex across [run]; pp-remote holds its whole-surface
    req_mutex. *)
let run ~(env : Eio_unix.Stdenv.base) (h : t) (reqd : Httpun.Reqd.t) : unit =
  let seam = h.seam in
  let body_raw = Http_kit.read_body reqd in
  let prompt = Http_kit.form_field body_raw "msg" in
  (* REPL rule (same as the stdin REPL): a prompt whose first non-space
     char is ( evaluates as Nopales in the shared env — no model call. *)
  let trimmed = String.trim prompt in
  let repl_expr = String.length trimmed > 0 && trimmed.[0] = '(' in
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
    match Multipart_emitter.mp_write_part ~env body_w ~boundary ~target ~swap html with
    | `Ok -> ()
    | `Peer_gone -> raise Multipart_emitter.Peer_gone
  in
  (* Render a Nopales fragment template and emit it as a part targeting
     the template's slot; false if the template is unbound (caller emits
     its OCaml fallback). The TARGETS are part of the emitter contract —
     a template's html lands in the slot its OCaml fallback would. *)
  let tpl ?target ?swap name args =
    match Host_seam.call_template ~rt:seam name (List.map (fun s -> Ast.VStr s) args) with
    | Some html -> emit ?target ?swap html; true
    | None -> false
  in
  let run_turn () =
    (* Opener: user echo bubble (Nopales template, redefinable live;
       OCaml fallback if unbound). *)
    if not (tpl "chat/user-bubble" [ prompt ]) then
      emit (Printf.sprintf "<div class=\"bubble user\">%s</div>" (Http_kit.esc prompt));
    match h.eval_repl with
    | Some eval when repl_expr -> begin
        (* REPL mode: evaluate locally (eval lock + rim, same env as
           plugins/stdin-REPL/surfaces), render the last value REPL-style.
           Errors land in the chat/error template, not a model turn. The
           closure contains Interp_error/other exns into Error itself. *)
        (match eval trimmed with
         | Ok v ->
             let s = Ast.pp_value_short v in
             if not (tpl "chat/repl-bubble" [ s ]) then
               emit (Printf.sprintf "<div class=\"bubble repl\">%s</div>" (Http_kit.esc s))
         | Error msg ->
             if not (tpl "chat/error" [ msg ]) then
               emit (Printf.sprintf "<div class=\"trace\">[repl error: %s]</div>" (Http_kit.esc msg)))
      end
    | _ ->
    (* Turn skeleton: one container with TWO swap targets -- #<rid>-think
       inside the collapsible <details> (reasoning spans + tool traces)
       and #<rid> response bubble (content tokens). *)
    (if not (tpl "chat/turn-open" [ rid ]) then
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
      if not (tpl ~target:think_zone "chat/think-open" [ tid ]) then
        emit ~target:think_zone
          (Printf.sprintf
             "<details class=\"tk\"><summary>thinking</summary><div id=\"%s\"></div></details>"
             tid);
      cur_think := Some tid;
      tid
    in
    let emit_tool_line tid label text =
      if not (tpl ~target:("#" ^ tid) "chat/tool-line" [ label; text ]) then
        emit ~target:("#" ^ tid)
          (Printf.sprintf "<div class=\"trace\">%s %s</div>"
             (Http_kit.esc label) (Http_kit.esc text))
    in
    Eio.Switch.run @@ fun lsw ->
    Eio.Fiber.fork ~sw:lsw (fun () ->
        (match
           Loop.run ~env ~sw:lsw ~host:(Host_seam.loop_host seam)
             ~cfg:(h.cfg ()) ~complete:h.complete
             ~on_event h.history prompt
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
               if not (tpl ~target:("#" ^ tid) "chat/think-token" [ s ]) then
                 emit ~target:("#" ^ tid)
                   (Printf.sprintf "<span class=\"reason\">%s</span>" (Http_kit.esc s))
           | Loop.Assistant_text s ->
               cur_think := None;
               if not (tpl ~target:("#" ^ rid) "chat/token" [ s ]) then
                 emit ~target:("#" ^ rid)
                   (Printf.sprintf "<span>%s</span>" (Http_kit.esc s))
           | Loop.Tool_begin (n, i) ->
               cur_think := None;
               incr tool_n;
               let tid = Printf.sprintf "%s-tool-%d" rid !tool_n in
               open_tools := tid :: !open_tools;
               if not (tpl ~target:think_zone "chat/tool-open" [ tid; n ]) then
                 emit ~target:think_zone
                   (Printf.sprintf
                      "<details class=\"tool\"><summary id=\"%s-sum\">tool: %s</summary><div id=\"%s\"></div></details>"
                      tid (Http_kit.esc n) tid);
               emit_tool_line tid "in" i
           | Loop.Tool_end (n, o) ->
               let tid =
                 match !open_tools with
                 | t :: rest -> open_tools := rest; t
                 | [] -> rid ^ "-think"
               in
               emit_tool_line tid "out" o;
               if tid <> rid ^ "-think" then
                 if not (tpl ~target:("#" ^ tid ^ "-sum") ~swap:"innerHTML"
                           "chat/tool-done" [ n ]) then
                   emit ~target:("#" ^ tid ^ "-sum") ~swap:"innerHTML"
                     (Printf.sprintf "tool: %s &#10003;" (Http_kit.esc n))
           | Loop.Turn_error msg ->
               if not (tpl ~target:("#" ^ rid) "chat/error" [ msg ]) then
                 emit ~target:("#" ^ rid)
                   (Printf.sprintf
                      "<div class=\"trace\">[turn error: %s]</div>"
                      (Http_kit.esc msg))))
        items;
      if not fin then drain ()
    in
    drain ()
  in
  (try run_turn () with Multipart_emitter.Peer_gone -> ());
  (* Record the captured parts for hyper/stream replay. *)
  seam.last_turn_parts <- Ast.VList (List.rev !parts);
  (try Multipart_emitter.mp_close body_w ~boundary
   with Multipart_emitter.Peer_gone -> ())
