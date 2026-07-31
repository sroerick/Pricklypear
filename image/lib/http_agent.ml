(** Agent portal + AI propose HTTP handlers. *)
open Pure_html
open Http_core

(* Client disconnect mid-SSE closes the httpaf body writer. Writing after
   that raises Failure("cannot write to closed writer") and — if uncaught —
   kills the whole single-image process. Treat peer-gone as a normal end. *)
let is_closed_writer_exn = function
  | Failure msg ->
      String.equal msg "cannot write to closed writer"
      || String.starts_with ~prefix:"cannot write to closed writer" msg
  | _ -> false

(* [is_closed_writer_exn] now shadows Http_core's identical helper; kept as a
   local alias so existing call sites stay unchanged. Body writes route
   through Http_core.safe_close_writer / safe_respond_with_string. *)

let sse_write (body : Httpun.Body.Writer.t) (event_json : string)
  : [ `Ok | `Peer_gone ] =
  let chunk = "data: " ^ event_json ^ "\n\n" in
  try
    Httpun.Body.Writer.write_string body chunk;
    let promise, resolver = Eio.Promise.create () in
    let wake () =
      if not (Eio.Promise.is_resolved promise) then
        Eio.Promise.resolve resolver ()
    in
    (try Httpun.Body.Writer.flush body (fun () -> wake ()) with
     | exn when is_closed_writer_exn exn -> wake ());
    (* Await flush ack (Httpun callback). *)
    Eio.Promise.await promise;
    `Ok
  with exn when is_closed_writer_exn exn ->
    ignore (Ops_log.info ~component:"sse" "peer gone (closed writer)");
    `Peer_gone

let close_writer_quiet (body : Httpun.Body.Writer.t) : unit =
  safe_close_writer body

let default_http_agent_seed =
  (* Short fallback when no session seed; tools sessions should prefer
     Session.default_agent_seed (dogfood + register_route rules). *)
  "You are the co-pilot for a Pricklypear multi-user Lisp OS image " ^
  "(software, not a cactus). Prefer CONTEXT; if unknown say so. " ^
  "Use structured tools only (never <tool_call> XML). " ^
  "install_function for lambdas; register_route for HTTP paths; " ^
  "data_rows via create/rows (not outline/*); inspect todo/page first."

(** POST /api/agent — durable sessions.
    Body JSON:
      {prompt, seed?, session_id?, tools?, max_steps?}
    Creates a session when session_id omitted; always returns session_id.
    Progress SSE when Accept prefers event-stream or stream:true; default
    is non-stream JSON (token streaming deferred / flaky on Synthetic). *)
(** Pull the human line out of the HTTP agent prompt wrapper for titles/UI. *)
let agent_user_visible_text (s : string) : string =
  let s = String.trim s in
  let key = "\nUser:\n" in
  let rec find_sub hay needle i =
    let nlen = String.length needle in
    let hlen = String.length hay in
    if i + nlen > hlen then None
    else if String.sub hay i nlen = needle then Some i
    else find_sub hay needle (i + 1)
  in
  match find_sub s key 0 with
  | Some i ->
      String.trim
        (String.sub s (i + String.length key)
           (String.length s - i - String.length key))
  | None ->
      if String.length s >= 20
         && String.sub s 0 20 = "=== IMAGE CONTEXT ===" then
        let n = min 80 (String.length s) in
        String.sub s 0 n ^ (if String.length s > 80 then "…" else "")
      else s

let agent_session_label (s : Ai_store.session) : string =
  let short = String.sub s.id 0 (min 8 (String.length s.id)) in
  match s.title with
  | Some t when String.trim t <> "" ->
      let t = agent_user_visible_text t in
      (* Old rows stored the full IMAGE CONTEXT wrapper as title (truncated
         before User:). Don't show that dump in the chat list. *)
      if String.trim t = ""
         || (String.length t >= 20
             && String.sub t 0 20 = "=== IMAGE CONTEXT ===")
         || String.starts_with ~prefix:"=== IMAGE CONTEXT" t then
        "chat " ^ short
      else if String.length t > 72 then String.sub t 0 72 ^ "…"
      else t
  | _ -> "chat " ^ short

let api_agent (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let seed, prompt, session_id_opt, tools, max_steps, want_stream =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          let seed =
            match List.assoc_opt "seed" items with
            | Some (`String s) -> s
            | _ -> default_http_agent_seed
          in
          let prompt =
            match List.assoc_opt "prompt" items with
            | Some (`String s) -> s
            | _ ->
                (match List.assoc_opt "q" items with
                 | Some (`String s) -> s
                 | _ -> "")
          in
          let sid =
            match List.assoc_opt "session_id" items with
            | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          in
          let tools =
            match List.assoc_opt "tools" items with
            | Some (`Bool b) -> b
            | Some (`String "0") | Some (`String "false") -> false
            | _ -> true
          in
          let max_steps =
            match List.assoc_opt "max_steps" items with
            | Some (`Int n) -> n
            | Some (`String s) ->
                (match int_of_string_opt s with Some n -> n | None -> 16)
            | _ -> 16
          in
          let stream =
            match List.assoc_opt "stream" items with
            | Some (`Bool b) -> b
            | Some (`String "1") | Some (`String "true") -> true
            | Some (`String "0") | Some (`String "false") -> false
            | _ -> false
          in
          (seed, prompt, sid, tools, max_steps, stream)
      | _ -> (default_http_agent_seed, "", None, true, 16, false)
    with _ ->
      let seed =
        Option.value (form_field body "seed") ~default:default_http_agent_seed
      in
      let prompt =
        match form_field body "prompt" with
        | Some p -> p
        | None -> Option.value (form_field body "q") ~default:""
      in
      let sid = form_field body "session_id" in
      (seed, prompt, sid, true, 16, false)
  in
  if String.trim prompt = "" then begin
    json_response reqd
      (`Assoc [ ("ok", `Bool false); ("error", `String "need prompt") ]);
    ()
  end else
    let sid_r =
      match session_id_opt with
      | Some sid -> (Ok sid)
      | None ->
          let title_opt =
            let p = agent_user_visible_text prompt in
            if String.trim p = "" then None
            else
              Some
                (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
          in
          let v =
            Session.agent_session_new session ~seed ~tools ?title:title_opt ()
          in
          (match v with
           | Pricklypear_lisp.Ast.VStr s
             when not (String.starts_with ~prefix:"agent-session-new error" s) ->
               (Ok s)
           | Pricklypear_lisp.Ast.VStr e -> (Error e)
           | other ->
               (Error (Pricklypear_lisp.Reader.pp_value other)))
    in
    match sid_r with
    | Error e ->
        json_response reqd
          (`Assoc [ ("ok", `Bool false); ("error", `String e) ]);
        ()
    | Ok sid ->
        if not want_stream then begin
          let v =
            Session.agent_say session ~session_id:sid ~prompt ~max_steps ()
          in
          let text =
            match v with
            | Pricklypear_lisp.Ast.VStr s -> s
            | _ -> Pricklypear_lisp.Reader.pp_value v
          in
          json_response reqd
            (`Assoc
               [ ("ok", `Bool true)
               ; ("session_id", `String sid)
               ; ("text", `String text)
               ]);
          ()
        end else begin
          (* Progress SSE (tool/token events), not provider token stream. *)
          let headers =
            Httpun.Headers.of_list
              [ ("content-type", "text/event-stream; charset=utf-8")
              ; ("cache-control", "no-cache")
              ; ("connection", "keep-alive")
              ; ("x-accel-buffering", "no")
              ]
          in
          let resp = Httpun.Response.create ~headers `OK in
          (* Peer may already be gone before the stream starts; on
             closed-writer, bail instead of letting it escape to the
             launch catch (image.http-robustness invariant 1). *)
          let body_w =
            try
              Httpun.Reqd.respond_with_streaming ~flush_headers_immediately:true
                reqd resp
            with exn when is_closed_writer_exn exn ->
              ignore (Ops_log.info ~component:"sse"
                        "peer gone before stream start");
              raise exn
          in
          let queue : string Queue.t = Queue.create () in
          let mutex = Eio.Mutex.create () in
          let cond = Eio.Condition.create () in
          let finished = ref false in
          let push_json (j : string) : unit =
            Eio.Mutex.lock mutex;
            Queue.add j queue;
            Eio.Condition.broadcast cond;
            Eio.Mutex.unlock mutex
          in
          push_json
            (Yojson.Safe.to_string
               (`Assoc
                  [ ("t", `String "session")
                  ; ("session_id", `String sid)
                  ]));
          let on_event = function
            | Llm_client.Token tok ->
                push_json
                  (Yojson.Safe.to_string
                     (`Assoc [ ("t", `String "token"); ("s", `String tok) ]))
            | Llm_client.Done ->
                push_json
                  (Yojson.Safe.to_string (`Assoc [ ("t", `String "done") ]));
                finished := true;
                Eio.Mutex.lock mutex;
                Eio.Condition.broadcast cond;
                Eio.Mutex.unlock mutex
            | Llm_client.Fail e ->
                push_json
                  (Yojson.Safe.to_string
                     (`Assoc [ ("t", `String "error"); ("s", `String e) ]));
                finished := true;
                Eio.Mutex.lock mutex;
                Eio.Condition.broadcast cond;
                Eio.Mutex.unlock mutex
          in
          (* Producer + drain under a local switch so tokens stream. *)
          Eio.Switch.run @@ fun sw ->
          Eio.Fiber.fork ~sw (fun () ->
              let v =
                Session.agent_say session ~session_id:sid ~prompt ~max_steps
                  ~on_event ()
              in
              if not !finished then begin
                (match v with
                 | Pricklypear_lisp.Ast.VStr s ->
                     push_json
                       (Yojson.Safe.to_string
                          (`Assoc
                             [ ("t", `String "done")
                             ; ("text", `String s)
                             ; ("session_id", `String sid)
                             ]))
                 | _ ->
                     push_json
                       (Yojson.Safe.to_string
                          (`Assoc [ ("t", `String "done") ])));
                finished := true;
                Eio.Mutex.lock mutex;
                Eio.Condition.broadcast cond;
                Eio.Mutex.unlock mutex
              end);
          let rec drain () =
            Eio.Mutex.lock mutex;
            if not (Queue.is_empty queue) then begin
              let j = Queue.take queue in
              Eio.Mutex.unlock mutex;
              match sse_write body_w j with
              | `Peer_gone ->
                  finished := true;
                  close_writer_quiet body_w
              | `Ok -> drain ()
            end else if !finished then begin
              Eio.Mutex.unlock mutex;
              close_writer_quiet body_w
            end else begin
              Eio.Condition.await cond mutex;
              Eio.Mutex.unlock mutex;
              drain ()
            end
          in
          drain ()
        end

let api_agent_task (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  (* Alias: create tools session + say (same as /api/agent with tools). *)
  let body = read_body reqd in
  let prompt =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          (match List.assoc_opt "prompt" items with
           | Some (`String s) -> s
           | _ ->
               (match List.assoc_opt "task" items with
                | Some (`String s) -> s
                | _ -> ""))
      | _ -> ""
    with _ ->
      Option.value (form_field body "prompt")
        ~default:(Option.value (form_field body "task") ~default:"")
  in
  if String.trim prompt = "" then begin
    json_response reqd
      (`Assoc [ ("ok", `Bool false); ("error", `String "need prompt/task") ]);
    ()
  end else
    let v = Session.agent_task session ~prompt () in
    let text =
      match v with
      | Pricklypear_lisp.Ast.VStr s -> s
      | _ -> Pricklypear_lisp.Reader.pp_value v
    in
    json_response reqd
      (`Assoc [ ("ok", `Bool true); ("text", `String text) ]);
    ()

let agent_page (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  (* Durable chats: list + open with ?session=id (agent-portal UI). *)
  let target = (Httpun.Reqd.request reqd).Httpun.Request.target in
  let open_sid =
    match query_param target "session" with
    | Some s when String.trim s <> "" -> Some (String.trim s)
    | _ -> None
  in
  let ctx =
    Printf.sprintf "user=%s branch=%s"
      (Session.username session) (Session.branch_name session)
  in
  let default_seed = Session.default_agent_seed session in
  let sessions =
    Ai_store.list_sessions (Session.store session)
      ~owner:(Session.username session) ~limit:40 ()
  in
  let open_sess, history =
    match open_sid with
    | None -> (None, [])
    | Some id ->
        let s_opt =
          Ai_store.get_session (Session.store session)
            ~owner:(Session.username session) ~id
        in
        (match s_opt with
         | None -> (None, [])
         | Some s ->
             let pairs =
               Ai_store.history_summaries (Session.store session) ~session_id:id
             in
             (Some s, pairs))
  in
  let seed_value =
    match open_sess with
    | Some s when String.trim s.seed <> "" -> s.seed
    | _ -> default_seed
  in
  let tools_checked =
    match open_sess with Some s -> s.tools | None -> true
  in
  let sid_display =
    match open_sess with Some s -> s.id | None -> "(new on send)"
  in
  let history_nodes =
    List.map
      (fun (role, content) ->
        let role_label =
          match role with
          | "user" -> "you"
          | "assistant" -> "agent"
          | other -> other
        in
        let shown =
          if role = "user" then agent_user_visible_text content else content
        in
        let shown =
          if String.length shown > 4000 then
            String.sub shown 0 4000 ^ "\n…(truncated)"
          else shown
        in
        HTML.div
          [ HTML.class_ "card"; HTML.style_ "margin:.4rem 0" ]
          [ HTML.b [] [txt "%s" role_label]
          ; HTML.pre
              [ HTML.style_ "white-space:pre-wrap;margin:.35rem 0 0" ]
              [txt "%s" shown]
          ])
      history
  in
  let session_list_nodes =
    if sessions = [] then
      [ HTML.p [HTML.class_ "muted"] [txt "No chats yet — send a message."] ]
    else
      List.map
        (fun (s : Ai_store.session) ->
          let active =
            match open_sess with
            | Some o when o.id = s.id -> true
            | _ -> false
          in
          let label = agent_session_label s in
          let href = "/agent?session=" ^ s.id in
          HTML.li
            [ HTML.style_
                (if active then "font-weight:700;margin:.25rem 0"
                 else "margin:.25rem 0")
            ]
            [ HTML.a [HTML.href "%s" href] [txt "%s" label]
            ; HTML.span [HTML.class_ "muted"]
                [txt "%s" (" · " ^ s.updated_at)]
            ])
        sessions
  in
  let initial_sid_js =
    match open_sess with
    | Some s -> Yojson.Safe.to_string (`String s.id)
    | None -> "null"
  in
  let tools_input =
    if tools_checked then
      HTML.input
        [ HTML.type_ "checkbox"
        ; HTML.name "tools"
        ; HTML.id "tools"
        ; HTML.value "1"
        ; HTML.checked
        ]
    else
      HTML.input
        [ HTML.type_ "checkbox"
        ; HTML.name "tools"
        ; HTML.id "tools"
        ; HTML.value "1"
        ]
  in
  let js =
    "const log=document.getElementById('log');" ^
    "const msg=document.getElementById('msg');" ^
    "const send=document.getElementById('send');" ^
    "const neu=document.getElementById('newchat');" ^
    "const sidEl=document.getElementById('sid');" ^
    "const toolsEl=document.getElementById('tools');" ^
    "const seedEl=document.getElementById('seed');" ^
    "const baseCtx=" ^ Yojson.Safe.to_string (`String ctx) ^ ";" ^
    "let sid=" ^ initial_sid_js ^ ";" ^
    "function add(role,text){" ^
    "const d=document.createElement('div');" ^
    "d.className='card';" ^
    "d.style.margin='.4rem 0';" ^
    "d.innerHTML='<b>'+role+'</b><pre style=\"white-space:pre-wrap;margin:.35rem 0 0\">'+" ^
    "String(text).replace(/[&<>]/g,c=>({\"&\":\"&amp;\",\"<\":\"&lt;\",\">\":\"&gt;\"}[c]))+" ^
    "'</pre>';log.appendChild(d);log.scrollTop=log.scrollHeight;}" ^
    "function setSid(s){sid=s||null;sidEl.textContent=sid?sid:'(new on send)';" ^
    "if(sid){const u=new URL(window.location.href);u.searchParams.set('session',sid);" ^
    "history.replaceState({},'',u.pathname+u.search);}" ^
    "else{history.replaceState({},'','/agent');}}" ^
    "setSid(sid);" ^
    "neu.onclick=()=>{window.location.href='/agent';};" ^
    "send.onclick=async()=>{" ^
    "const q=msg.value.trim();if(!q)return;" ^
    "const seed=seedEl.value;" ^
    "const tools=toolsEl.checked;" ^
    "const prompt='=== IMAGE CONTEXT ===\\n'+" ^
    "'system: Pricklypear multi-user Lisp OS image (software)\\n'+" ^
    "baseCtx+'\\n=== END CONTEXT ===\\n\\nUser:\\n'+q;" ^
    "add('you',q);msg.value='';send.disabled=true;" ^
    "try{" ^
    "const body={seed,prompt,tools,stream:false};" ^
    "if(sid)body.session_id=sid;" ^
    "const r=await fetch('/api/agent',{method:'POST'," ^
    "headers:{'content-type':'application/json'}," ^
    "body:JSON.stringify(body)});" ^
    "const j=await r.json();" ^
    "if(j.session_id)setSid(j.session_id);" ^
    "add('agent',j.text||j.error||JSON.stringify(j));" ^
    "if(j.session_id){" ^
    "const href='/agent?session='+encodeURIComponent(j.session_id);" ^
    "if(!document.querySelector('#chat-list a[href=\"'+href+'\"]'))" ^
    " setTimeout(function(){window.location.href=href;},350);}" ^
    "}catch(e){add('error',String(e));}" ^
    "finally{send.disabled=false;msg.focus();}" ^
    "};" ^
    "msg.addEventListener('keydown',e=>{if(e.key==='Enter'&&!e.shiftKey){e.preventDefault();send.click();}});"
  in
  let body =
    chrome ~session ~title:"Agent" [
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Chats"];
        HTML.p [HTML.class_ "muted"] [txt
          "Durable threads in Postgres. Open one to resume; New chat starts empty."];
        HTML.p [] [
          HTML.a [HTML.href "/agent"; HTML.class_ "btn"] [txt "New chat"];
        ];
        HTML.ul [HTML.id "chat-list"] session_list_nodes;
      ];
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Conversation"];
        HTML.p [] [
          txt "session: ";
          HTML.code [HTML.id "sid"] [txt "%s" sid_display];
        ];
        HTML.p [] [
          HTML.label [HTML.for_ "tools"] [
            tools_input;
            txt " tools (inspect / install)"
          ];
        ];
        HTML.details [] [
          HTML.summary [] [txt "seed (system persona)"];
          HTML.textarea [HTML.name "seed"; HTML.id "seed"] "%s" seed_value;
        ];
        HTML.div
          [ HTML.id "log"
          ; HTML.style_
              "max-height:28rem;overflow:auto;border:1px solid #ccc;padding:.5rem;margin:.5rem 0"
          ]
          history_nodes;
        HTML.p [] [
          HTML.label [HTML.for_ "msg"] [txt "message"];
        ];
        HTML.textarea [HTML.name "msg"; HTML.id "msg"]
          "%s" "What branch am I on?";
        HTML.p [] [
          HTML.button [HTML.type_ "button"; HTML.id "send"] [txt "Send"];
          txt " ";
          HTML.button [HTML.type_ "button"; HTML.id "newchat"] [txt "New chat"];
        ];
      ];
      HTML.script [] "%s" js;
      HTML.div [HTML.class_ "card"] [
        HTML.h2 [] [txt "Write a function"];
        HTML.p [HTML.class_ "muted"] [txt
          "One-shot authoring (not chat). Same as (ai/write name spec)."];
        HTML.p [] [
          HTML.label [HTML.for_ "fn_name"] [txt "name "];
          HTML.input [HTML.type_ "text"; HTML.name "fn_name"; HTML.id "fn_name";
                      HTML.value "demo/double"; HTML.placeholder "pkg/name"];
        ];
        HTML.p [] [
          HTML.label [HTML.for_ "fn_spec"] [txt "spec"];
        ];
        HTML.textarea [HTML.name "fn_spec"; HTML.id "fn_spec"]
          "%s" "A pure lambda of one int x that returns (* x 2).";
        HTML.p [] [
          HTML.button [HTML.type_ "button"; HTML.id "write_fn"] [txt "Generate & install"];
        ];
        HTML.pre [HTML.id "write_out"; HTML.class_ "card"] [txt ""];
      ];
      HTML.script [] "%s" (
        "const wout=document.getElementById('write_out');" ^
        "const wbtn=document.getElementById('write_fn');" ^
        "wbtn.onclick=async()=>{" ^
        "const name=document.getElementById('fn_name').value.trim();" ^
        "const spec=document.getElementById('fn_spec').value;" ^
        "wout.textContent='…'; wbtn.disabled=true;" ^
        "try{" ^
        "const r=await fetch('/api/ai/propose',{method:'POST'," ^
        "headers:{'content-type':'application/json'}," ^
        "body:JSON.stringify({name,spec})});" ^
        "const j=await r.json();" ^
        "wout.textContent=j.result||j.error||JSON.stringify(j);" ^
        "}catch(e){wout.textContent=String(e);}" ^
        "finally{wbtn.disabled=false;}" ^
        "};"
      );
    ]
  in
  html_response reqd ~session body;
  ()

let ai_propose (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  let body = read_body reqd in
  let name_opt, spec_opt =
    try
      match Yojson.Safe.from_string body with
      | `Assoc items ->
          let name =
            match List.assoc_opt "name" items with
            | Some (`String s) -> Some s
            | _ -> None
          in
          let spec =
            match List.assoc_opt "spec" items with
            | Some (`String s) -> Some s
            | _ -> None
          in
          (name, spec)
      | _ -> (None, None)
    with _ ->
      let pkg = Option.value (form_field body "package") ~default:"" in
      let local = Option.value (form_field body "local") ~default:"" in
      let name =
        match form_field body "name" with
        | Some n when String.trim n <> "" -> Some (String.trim n)
        | _ ->
            let j = Packages.join_name ~package:pkg ~local in
            if j = "" then None else Some j
      in
      (name, form_field body "spec")
  in
  match name_opt, spec_opt with
  | Some name, Some spec when name <> "" && spec <> "" ->
      let v = Session.propose_function session ~name ~spec in
      let rendered = Pricklypear_lisp.Reader.pp_value v in
      let wants_html =
        match Httpun.Headers.get (Httpun.Reqd.request reqd).headers "content-type" with
        | Some ct when String.length ct >= 33
            && String.sub ct 0 33 = "application/x-www-form-urlencoded" -> true
        | _ ->
            (match form_field body "name" with Some _ -> true | None -> false)
      in
      if wants_html then
        let body_node =
          chrome ~session ~title:("AI propose " ^ name) [
            HTML.div [HTML.class_ "card"] [
              HTML.pre [] [txt "%s" rendered];
              HTML.p [] [
                HTML.a [HTML.href "%s" ("/edit/" ^ name)] [txt "open editor"];
                txt " · ";
                HTML.a [HTML.href "%s" (Packages.inspect_href name)] [txt "inspect"];
                txt " · ";
                HTML.a [HTML.href "/bindings"] [txt "functions"];
              ];
            ];
          ]
        in
        html_response reqd ~session body_node;
        ()
      else begin
        json_response reqd
          (`Assoc [ ("ok", `Bool true)
                  ; ("result", `String rendered)
                  ; ("name", `String name) ]);
        ()
      end
  | _ ->
      json_response reqd ~status:`Bad_request
        (`Assoc [("ok", `Bool false); ("error", `String "name and spec required")]);
      ()

