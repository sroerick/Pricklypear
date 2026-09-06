(** Tests for borge/remote-eval.borg agent-loop: the SHARED LLM turn
    loop (llm/) running over the CLIENT host seam. No network — the
    provider is a canned script (mock complete) and the transport is a
    mock. Verifies the thesis end-to-end: the provider call runs on
    this process, a Nopales plugin tool's body evals under the proxy
    rim (its DB prim crosses to /api/prim exactly once), builtin tool
    bodies stay LOCAL (zero wire), and (on "tool_call" …) hooks block
    identically to the agent host. *)

open Pricklypear_lisp
open Ast

(** A mock transport: records (name, args) calls and answers from a
    table (same shape as remote_test.ml's). *)
let mock_transport
    ?(fallback : (string -> value list -> (value, exn) Stdlib.result) option)
    ()
    : Proxy_rim.transport * (string * value list) list ref
      * (string, value list -> (value, exn) Stdlib.result) Hashtbl.t =
  let calls = ref [] in
  let table = Hashtbl.create 8 in
  let fn name args =
    calls := !calls @ [ (name, args) ];
    match Hashtbl.find_opt table name with
    | Some f -> f args
    | None -> (
        match fallback with
        | Some g -> g name args
        | None ->
            Error
              (Interp_error
                 (Prim_error (name, "not in mock table", "prim_error"))))
  in
  (fn, calls, table)

let cfg : Provider.cfg =
  { base_url = "http://127.0.0.1:1"; api_key = ""; model = "mock"; system = "" }

(** A mock complete that emits one tool call then a final text. *)
let complete_with_tool_call name input =
  let script = ref 0 in
  fun ~env:_ ~sw:_ ~cfg:_ ~messages:_ ~tools:_ ~on_delta:_ ->
    let n = !script in
    incr script;
    if n = 0 then
      Ok
        {
          Provider.text = "";
          calls = [ { id = "t1"; name; input } ];
          stop_reason = "tool_use";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "") ];
        }
    else
      Ok
        {
          Provider.text = "done";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "done") ];
        }

(** The thesis test: a Nopales plugin tool registered with
    (register-tool …) whose body calls a DB prim (whoami). The loop runs
    over [Remote.loop_host]; the tool body evals under the proxy rim so
    whoami crosses to the transport EXACTLY ONCE; the tool result is
    appended to history and the loop makes its follow-up call. *)
let test_plugin_tool_proxies_db_prim ~env ~sw () =
  let transport, calls, table = mock_transport () in
  Hashtbl.replace table "whoami" (fun _args -> Ok (VStr "admin"));
  let t = Remote.make ~transport () in
  ignore
    (Remote.eval t
       {|(register-tool "who" (list) (lambda (args) (whoami)))|});
  Alcotest.(check bool) "tool registered in order" true
    (List.mem "who" t.Remote.tool_order);
  let events = ref [] in
  let history = ref [] in
  (match
     Loop.run ~env ~sw ~host:(Remote.loop_host t) ~cfg
       ~complete:(complete_with_tool_call "who" `Null)
       ~on_event:(fun e -> events := e :: !events)
       history "who am I"
   with
  | Error e -> Alcotest.fail e
  | Ok () -> ());
  (* the DB prim crossed the wire exactly once *)
  let whoami_calls = List.filter (fun (n, _) -> n = "whoami") !calls in
  Alcotest.(check int) "whoami crossed the wire exactly once" 1
    (List.length whoami_calls);
  (* the tool result reached the model as a tool-role message *)
  Alcotest.(check int) "history has 4 entries" 4 (List.length !history);
  (match List.nth !history 2 with
  | { Provider.role = "tool"; content = `String out; tool_call_id = Some "t1"; _ } ->
      Alcotest.(check string) "tool result is the proxied row" "admin" out
  | m ->
      Alcotest.fail
        ("bad tool message: " ^ m.Provider.role ^ " content="
         ^ (match m.Provider.content with
            | `String s -> s
            | c -> Yojson.Safe.to_string c)));
  (* Tool_end event carried the tool output to the stream *)
  let saw_tool_end =
    List.exists
      (fun e -> match e with Loop.Tool_end (_, o) -> o = "admin" | _ -> false)
      !events
  in
  Alcotest.(check bool) "Tool_end event carried output" true saw_tool_end

(** Builtin tools (read/bash) run as LOCAL prims through the proxy
    rim's local table — a bash round trip makes ZERO wire crossings. *)
let test_builtin_tool_stays_local ~env ~sw () =
  let transport, calls, _ = mock_transport () in
  let t = Remote.make ~transport () in
  Tools.install ~env ~resolve_path:Remote.resolve_path
    ~register:(fun name run -> Remote.register_builtin_tool t name ~run);
  let history = ref [] in
  (match
     Loop.run ~env ~sw ~host:(Remote.loop_host t) ~cfg
       ~complete:
         (complete_with_tool_call "bash"
            (`Assoc [ ("command", `String "echo hello") ]))
       ~on_event:(fun _ -> ()) history "run echo"
   with
  | Error e -> Alcotest.fail e
  | Ok () -> ());
  Alcotest.(check int) "builtin bash made zero wire crossings" 0
    (List.length !calls);
  (match List.nth !history 2 with
  | { role = "tool"; content = `String out; _ } ->
      (* {"exit":0,"out":"hello\n",...} *)
      Alcotest.(check bool) "bash output captured" true
        (try ignore (Str.search_forward (Str.regexp_string "hello") out 0); true
         with Not_found -> false)
  | _ -> Alcotest.fail "missing tool message")

(** A (on "tool_call" …) hook blocking a call reaches the model as a
    blocked tool result — same hook semantics as the agent host. *)
let test_tool_call_hook_blocks ~env ~sw () =
  let transport, _calls, _table = mock_transport () in
  let t = Remote.make ~transport () in
  Tools.install ~env ~resolve_path:Remote.resolve_path
    ~register:(fun name run -> Remote.register_builtin_tool t name ~run);
  ignore
    (Remote.eval t
       {|(on "tool_call" (lambda (ev)
            (if (string-contains? (dict-get ev "input") "rm -rf")
                (dict-set (dict-set "{}" "block" "true") "reason" "nope")
                "{}")))|});
  let saw_block = ref false in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages ~tools:_ ~on_delta:_ =
    if not !saw_block then (
      saw_block :=
        List.exists
          (fun (m : Provider.message) ->
            match m.content with
            | `String s ->
                (try
                   ignore
                     (Str.search_forward (Str.regexp_string "blocked: nope") s 0);
                   true
                 with Not_found -> false)
            | _ -> false)
          messages;
      Ok
        {
          Provider.text = "";
          calls =
            [ { id = "t1"; name = "bash";
                input = `Assoc [ ("command", `String "rm -rf /") ] } ];
          stop_reason = "tool_use";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "") ];
        })
    else
      Ok
        {
          Provider.text = "ok";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "ok") ];
        }
  in
  (match
     Loop.run ~env ~sw ~host:(Remote.loop_host t) ~cfg ~complete
       ~on_event:(fun _ -> ()) (ref []) "delete everything"
   with
  | Error e -> Alcotest.fail e
  | Ok () -> Alcotest.(check bool) "block reason reached model" true !saw_block)

(** The chat surface is Nopales-authored on the client route table:
    chat/page renders the hx-post /chat form, the chat/* fragment
    templates render, and / serves chat while /eval serves the eval
    box. *)
let test_chat_surface_nopales ~rt () =
  Surface.load ~rt;
  let render handler =
    match
      Remote.dispatch_route ~rt ~handler ~method_:"GET" ~path:"/"
        ~params:[] ~headers:[] ~body:""
    with
    | VStr s -> s
    | other -> Alcotest.fail ("bad render: " ^ Reader.pp_value other)
  in
  let chat = render "chat/page" in
  Alcotest.(check bool) "chat page has hx-post /chat" true
    (try ignore (Str.search_forward (Str.regexp_string "hx-post=\"/chat\"") chat 0); true
     with Not_found -> false);
  Alcotest.(check bool) "chat page loads hx-multipart asset" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "/assets/hx-multipart.js") chat 0);
       true
     with Not_found -> false);
  (match Remote.call_template ~rt "chat/user-bubble" [ VStr "hi" ] with
  | Some html ->
      Alcotest.(check bool) "user bubble template renders" true
        (try ignore (Str.search_forward (Str.regexp_string "bubble user") html 0); true
         with Not_found -> false)
  | None -> Alcotest.fail "chat/user-bubble unbound");
  (match Remote.call_template ~rt "chat/turn-open" [ VStr "t1" ] with
  | Some html ->
      Alcotest.(check bool) "turn-open template renders" true
        (try ignore (Str.search_forward (Str.regexp_string "id=\"t1\"") html 0); true
         with Not_found -> false)
  | None -> Alcotest.fail "chat/turn-open unbound");
  (* /eval still serves the eval box *)
  (match
    Remote.dispatch_route ~rt ~handler:"eval/page" ~method_:"GET"
      ~path:"/eval" ~params:[] ~headers:[] ~body:""
  with
  | VStr s ->
      Alcotest.(check bool) "eval box moved to /eval" true
        (try
           ignore (Str.search_forward (Str.regexp_string "hx-post=\"/eval\"") s 0);
           true
         with Not_found -> false)
  | other -> Alcotest.fail ("eval/page: " ^ Reader.pp_value other))

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stransport, _, _ = mock_transport () in
  let surface_rt = Remote.make ~transport:stransport () in
  Alcotest.run "remote-loop"
    [
      ( "agent-loop",
        [
          Alcotest.test_case "plugin-tool-proxies-db-prim" `Quick
            (test_plugin_tool_proxies_db_prim ~env ~sw);
          Alcotest.test_case "builtin-tool-stays-local" `Quick
            (test_builtin_tool_stays_local ~env ~sw);
          Alcotest.test_case "tool-call-hook-blocks" `Quick
            (test_tool_call_hook_blocks ~env ~sw);
        ] );
      ( "surfaces",
        [ Alcotest.test_case "chat-surface-nopales" `Quick
            (test_chat_surface_nopales ~rt:surface_rt) ] );
    ]
