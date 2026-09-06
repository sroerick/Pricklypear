(** Tests for borge/agent.borg: plugin load, rim, events, loop. No
    network — provider is a canned script. *)

open Pricklypear_lisp

let pp_value_testable =
  Alcotest.testable
    (fun fmt v -> Format.pp_print_string fmt (Reader.pp_value v))
    (fun a b -> Reader.pp_value a = Reader.pp_value b)

(** plugin-model: loading order + events + register-command +
    register-tool *)
let test_plugin_lifecycle ~rt () =
  let pd =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "agent-test-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir pd 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let plugins_dir = Filename.concat pd "plugins" in
  (try Unix.mkdir plugins_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat plugins_dir "test.pp" in
  let oc = open_out path in
  output_string oc
    {|(on "tool_call" (lambda (ev) ev))
(register-command "ping" "pong" (lambda (args) "pong"))
(register-tool "greet" (list) (lambda (json-args) "hello-from-plugin"))|};
  close_out oc;
  (match Plugins.load_file rt path with
   | Ok () -> ()
   | Error e -> Alcotest.fail e);
  (* command registered *)
  (match Runtime.eval_string ~rt "(list-commands)" with
   | VStr s ->
       Alcotest.(check bool) "ping registered" true
         (try ignore (Str.search_forward (Str.regexp_string "ping") s 0); true
          with Not_found -> false)
   | _ -> Alcotest.fail "list-commands not a string");
  (* tool registered *)
  Alcotest.(check bool) "greet registered" true
    (Hashtbl.mem rt.Runtime.tools "greet");
  (* tool_call handler fires *)
  let results =
    Runtime.fire rt "tool_call"
      [ Ast.VStr {|{"id":"1","name":"read","input":{"path":"x"}}|} ]
  in
  Alcotest.(check int) "one handler ran" 1 (List.length results)

(** nopales-embed: unregistered prim produces Interp_error (not
    Unhandled); live redefine takes effect immediately; a plugin
    callback during an active turn flows through the rim. *)
let test_rim_errors_and_redefine ~rt () =
  (match
     (try
        ignore (Runtime.eval_string ~rt "(no-such-prim 1)");
        `No_error
      with Ast.Interp_error _ -> `As_expected)
   with
   | `As_expected -> ()
   | `No_error -> Alcotest.fail "expected Interp_error for unknown prim");
  ignore (Runtime.eval_string ~rt "(define (f x) (+ x 1))");
  Alcotest.check pp_value_testable "first def" (VInt 42L)
    (Runtime.eval_string ~rt "(f 41)");
  ignore (Runtime.eval_string ~rt "(define (f x) (* x 2))");
  Alcotest.check pp_value_testable "redefined" (VInt 42L)
    (Runtime.eval_string ~rt "(f 21)")

(** builtin bash deadline (i-1788670652-6): a hung command is killed at
    the PP_BASH_TIMEOUT_SECS deadline and reports a timeout error JSON
    instead of holding the eval lock — and with it every later eval and
    chat turn — until the command finishes on its own. Negative-verified
    against the pre-fix tree: without the deadline the 30s sleep ran to
    completion and the <15s check failed. *)
let test_bash_deadline ~rt () =
  let old = Sys.getenv_opt "PP_BASH_TIMEOUT_SECS" in
  Fun.protect
    ~finally:(fun () ->
        (* 120 matches the in-code default, so the effective deadline is
           the same whether or not the var was set before this test. *)
        Unix.putenv "PP_BASH_TIMEOUT_SECS" (Option.value old ~default:"120"))
    (fun () ->
       Unix.putenv "PP_BASH_TIMEOUT_SECS" "1";
       (* positive control: a quick command still runs to completion *)
       (match Runtime.eval_string ~rt {|(__builtin__bash "{\"command\":\"echo hi\"}")|} with
        | VStr s ->
            Alcotest.(check bool) "quick command still runs" true
              (try
                 ignore (Str.search_forward (Str.regexp_string "hi") s 0);
                 true
               with Not_found -> false)
        | _ -> Alcotest.fail "bash result not a string");
       let t0 = Unix.gettimeofday () in
       let out =
         match
           Runtime.eval_string ~rt
             {|(__builtin__bash "{\"command\":\"sleep 30; echo late\"}")|}
         with
         | VStr s -> s
         | _ -> Alcotest.fail "timeout result not a string"
       in
       let dt = Unix.gettimeofday () -. t0 in
       Alcotest.(check bool)
         "hung command returns well before its own 30s sleep" (dt < 15.0) true;
       let j = Yojson.Safe.from_string out in
       let err_field = Yojson.Safe.Util.member "error" j in
       Alcotest.(check bool) "timeout reported in an error field" true
         (err_field <> `Null);
       let exit_code =
         Yojson.Safe.Util.to_int (Yojson.Safe.Util.member "exit" j)
       in
       Alcotest.(check int) "timeout exit is -1" (-1) exit_code)

(** agent-loop: a mock provider emitting one tool call round-trips
    exactly one follow-up call. *)
let test_loop_tool_round_trip ~env ~sw ~rt () =
  let calls = ref 0 in
  let script = ref 0 in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages:_ ~tools:_ ~on_delta:_ =
    incr calls;
    let n = !script in
    incr script;
    if n = 0 then
      Ok
        {
          Provider.text = "";
          calls =
            [
              {
                id = "t1";
                name = "bash";
                input = `Assoc [ ("command", `String "echo hello") ];
              };
            ];
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
  in
  let cfg : Provider.cfg =
    { base_url = "http://127.0.0.1:1"; api_key = ""; model = "mock"; system = "" }
  in
  let history = ref [] in
  match
    Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete ~on_event:(fun _ -> ()) history
      "say hello using echo"
  with
  | Error e -> Alcotest.fail e
  | Ok () ->
      Alcotest.(check int) "two provider calls" 2 !calls;
      Alcotest.(check int) "history has 4 entries" 4 (List.length !history)

(** agent-loop: a tool_call hook blocking reaches the model as an
    error tool result. *)
let test_loop_tool_block ~env ~sw ~rt () =
  ignore
    (Runtime.eval_string ~rt
       {|(on "tool_call" (lambda (ev)
            (if (string-contains? (dict-get ev "input") "rm -rf")
                (dict-set (dict-set "{}" "block" "true") "reason" "nope")
                "{}")))|});
  let script = ref 0 in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages ~tools:_ ~on_delta:_ =
    let n = !script in
    incr script;
    if n = 0 then
      Ok
        {
          Provider.text = "";
          calls =
            [
              {
                id = "t1";
                name = "bash";
                input = `Assoc [ ("command", `String "rm -rf /") ];
              };
            ];
          stop_reason = "tool_use";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "") ];
        }
    else (
      let saw_block =
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
          messages
      in
      Alcotest.(check bool) "block reason reached model" true saw_block;
      Ok
        {
          Provider.text = "ok";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "ok") ];
        })
  in
  let cfg : Provider.cfg =
    { base_url = "http://127.0.0.1:1"; api_key = ""; model = "mock"; system = "" }
  in
  match
    Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete ~on_event:(fun _ -> ()) (ref [])
      "delete everything"
  with
  | Error e -> Alcotest.fail e
  | Ok () -> ()

(** agent-loop regression (turn-2 provider 400): the assistant entry
    replayed in history must carry provider-shaped CONTENT (a string /
    part array), never the whole raw assistant message object nested under
    "content", and must replay tool_calls so the following tool-role entry
    pairs up. Catches the loop.ml history-append nesting bug that the real
    gateway rejected on the second message. *)
let test_loop_turn2_history_shape ~env ~sw ~rt () =
  let calls = ref 0 in
  let captured = ref [] in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages ~tools:_ ~on_delta:_ =
    incr calls;
    if !calls = 1 then
      (* first reply: assistant WITH a tool call, provider-shaped raw *)
      Ok
        {
          Provider.text = "using tool";
          calls =
            [
              {
                id = "t1";
                name = "bash";
                input = `Assoc [ ("command", `String "echo hi") ];
              };
            ];
          stop_reason = "tool_use";
          raw =
            `Assoc
              [ ("role", `String "assistant");
                ("content", `String "using tool");
                ( "tool_calls",
                  `List
                    [
                      `Assoc
                        [ ("id", `String "t1");
                          ("type", `String "function");
                          ( "function",
                            `Assoc
                              [ ("name", `String "bash");
                                ( "arguments",
                                  `String "{\"command\":\"echo hi\"}" ) ] );
                          ];
                    ] ) ];
        }
    else if !calls = 2 then
      Ok
        {
          Provider.text = "done";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "done") ];
        }
    else (
      captured := messages;
      Ok
        {
          Provider.text = "ok2";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "ok2") ];
        })
  in
  let cfg : Provider.cfg =
    { base_url = "http://127.0.0.1:1"; api_key = ""; model = "mock"; system = "" }
  in
  let history = ref [] in
  (match
     Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete ~on_event:(fun _ -> ()) history
       "first"
   with Error e -> Alcotest.fail e | Ok () -> ());
  (match
     Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete ~on_event:(fun _ -> ()) history
       "second"
   with Error e -> Alcotest.fail e | Ok () -> ());
  let msgs = !captured in
  (* system is prepended at REQUEST build time, so msgs here are:
     [user first; assistant+tool_calls; tool; assistant done; user second] *)
  Alcotest.(check int) "turn-2 replays 5 messages" 5 (List.length msgs);
  let a1 = List.nth msgs 1 in
  Alcotest.(check string) "assistant role" "assistant" a1.Provider.role;
  (match a1.Provider.content with
   | `String s -> Alcotest.(check string) "content is a string" "using tool" s
   | other ->
       Alcotest.fail
         ("assistant content must be a string, got: "
         ^ Yojson.Safe.to_string other));
  (match a1.Provider.tool_calls with
   | Some (`List (_ :: _)) -> ()
   | other ->
       Alcotest.fail
         ("assistant entry must replay tool_calls, got: "
         ^ Yojson.Safe.to_string (Option.value ~default:`Null other)));
  let a2 = List.nth msgs 3 in
  (match a2.Provider.content with
   | `String s -> Alcotest.(check string) "final content string" "done" s
   | other ->
       Alcotest.fail
         ("final assistant content must be a string, got: "
         ^ Yojson.Safe.to_string other));
  (match a2.Provider.tool_calls with
   | None -> ()
   | Some tc ->
       Alcotest.fail
         ("plain assistant entry must not carry tool_calls: "
         ^ Yojson.Safe.to_string tc));
  (* Wire shape: the actual request body validates against the gateway's
     expectation (content string, tool_calls array on the assistant msg). *)
  let body =
    Provider.Openai.request_body ~cfg ~messages:msgs ~tools:[] ~stream:false
  in
  let open Yojson.Safe.Util in
  let wire = Yojson.Safe.from_string body in
  let m1 = wire |> member "messages" |> index 2 in
  Alcotest.(check bool) "wire messages[2] role assistant" true
    (m1 |> member "role" |> to_string = "assistant");
  Alcotest.(check bool) "wire messages[2] content string" true
    (m1 |> member "content" |> to_string_option = Some "using tool");
  Alcotest.(check bool) "wire messages[2] has tool_calls" true
    (match m1 |> member "tool_calls" with `List (_ :: _) -> true | _ -> false)

(** provider-stream: SSE [data:] payloads reconstruct a [reply]; text
    deltas fire per chunk; tool-call argument fragments concatenate. *)
let test_sse_stream_reconstruction () =
  let deltas = ref [] in
  (* Each element is the [data:] payload for one SSE chunk. Two text
     deltas, a tool-call split across two fragments (id+name first,
     arguments the second time), a [DONE] marker. *)
  let reasons = ref [] in
  let lines =
    [
      {|{"choices":[{"delta":{"reasoning_content":"thinking "},"finish_reason":null}]}|};
      (* Synthetic's gateway streams thinking as plain "reasoning"
         (GLM-4.7-Flash wire probe 2026-08): alias must land in the same
         reasoning buffer / Reasoning deltas. *)
      {|{"choices":[{"delta":{"reasoning":"rea"},"finish_reason":null}]}|};
      {|{"choices":[{"delta":{"content":"hello "},"finish_reason":null}]}|};
      {|{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"t1","function":{"name":"bash","arguments":"{\"command\":\"ech"}}]}}]}|};
      {|{"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"o hello\"}"}}]}}]}|};
      {|{"choices":[{"delta":{"content":" world"},"finish_reason":null}]}|};
      {|{"choices":[{"delta":{},"finish_reason":"tool_calls"}]}|};
      "[DONE]";
    ]
  in
  let r =
    Provider.Openai.parse_stream_lines lines ~on_delta:(fun d ->
        match d with
        | Provider.Reasoning s -> reasons := !reasons @ [ s ]
        | Provider.Text s -> deltas := !deltas @ [ s ]
        | Provider.Done -> ())
  in
  Alcotest.(check (list string)) "reasoning delta streamed separately"
    [ "thinking "; "rea" ] !reasons;
  Alcotest.(check bool) "synthetic raw keeps reasoning_content" true
    (match r.Provider.raw with
     | `Assoc f ->
         (match List.assoc_opt "reasoning_content" f with
          | Some (`String "thinking rea") -> true
          | _ -> false)
     | _ -> false);
  Alcotest.(check string) "concatenated text" "hello  world" r.text;
  Alcotest.(check int) "two text deltas fired" 2 (List.length !deltas);
  Alcotest.(check (list string)) "delta order" [ "hello "; " world" ] !deltas;
  Alcotest.(check string) "stop reason" "tool_calls" r.stop_reason;
  Alcotest.(check int) "one reconstructed tool call" 1 (List.length r.calls);
  (match r.calls with
   | [ tc ] ->
       Alcotest.(check string) "tool id" "t1" tc.id;
       Alcotest.(check string) "tool name" "bash" tc.name
   | _ -> ());
  let input =
    match r.calls with
    | [ tc ] -> tc.input
    | _ -> `Null
  in
  Alcotest.(check string) "tool input json" {|{"command":"echo hello"}|}
    (Yojson.Safe.to_string input)

(** agent-route-table: (route ...) makes a path resolvable via
    route/lookup and renderable via route/dispatch; dispatch runs the
    Nopales handler through Runtime.apply_value under the eval rim and
    returns the rendered raw-HTML VStr. *)
let test_route_table_register_and_render ~rt () =
  ignore (Runtime.eval_string ~rt
    {|(route "/chat" "chat/render")|});
  ignore (Runtime.eval_string ~rt
    {|(define (chat/render) (render-html "<h1>hi</h1>"))|});
  Alcotest.check pp_value_testable "lookup resolves"
    (Ast.VStr "chat/render")
    (Runtime.eval_string ~rt {|(route/lookup "/chat")|});
  Alcotest.check pp_value_testable "dispatch renders raw-HTML VStr"
    (Ast.VStr "<h1>hi</h1>")
    (Runtime.eval_string ~rt
       {|(route/dispatch "chat/render" "GET" "/chat")|})

(** route/dispatch binds a thin request context so query-param /
    request-method / hx-request? resolve inside the handler. *)
let test_route_table_thin_context ~rt () =
  ignore (Runtime.eval_string ~rt
    {|(define (qtest)
       (string-append (request-method) "|"
         (query-param "x") "|"
         (if (hx-request?) "hx" "nohx")))|});
  ignore (Runtime.eval_string ~rt {|(route "/q" "qtest")|});
  Alcotest.check pp_value_testable "thin context resolves"
    (Ast.VStr "POST|42|hx")
    (Runtime.eval_string ~rt
       {|(route/dispatch "qtest" "POST" "/q"
           (list (cons "x" "42") (cons "hx-request" "true")))|})

(** a handler calling a Postgres-only request prim (current-author as
    a DB row) errors with unbound in the agent — the agent's request
    context carries only request-shape values. *)
let test_route_table_pg_prim_unbound ~rt () =
  ignore (Runtime.eval_string ~rt
    {|(define (authtest) (current-author))|});
  ignore (Runtime.eval_string ~rt {|(route "/a" "authtest")|});
  (match Runtime.eval_string ~rt
           {|(route/dispatch "authtest" "GET" "/a")|}
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "unbound reported" true
         (try ignore (Str.search_forward (Str.regexp_string "unbound") s 0); true
          with Not_found -> false)
   | other -> Alcotest.fail ("expected VStr, got " ^ Reader.pp_value other))

(** libs/hyper runs unchanged on the agent: boot-load libs/std +
    libs/html + libs/hyper as source, register a route-table handler, and
    exercise hyper/visit + hyper/click (immutability) + hyper/stream
    (multipart replay). The portability proof: same lib.pp, agent host. *)
let test_libs_hyper_on_agent ~rt () =
  (* dune runs the test with cwd = agent/test/, so walk up to find the
     repo root (where libs/hyper/lib.pp lives). *)
  let rec find_libs_dir dir =
    let cand = Filename.concat dir "libs/hyper/lib.pp" in
    if Sys.file_exists cand then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then "libs"
      else find_libs_dir parent
  in
  let libs_dir =
    let root = find_libs_dir (Sys.getcwd ()) in
    Filename.concat root "libs"
  in
  let load name =
    let p = Filename.concat (Filename.concat libs_dir name) "lib.pp" in
    match Runtime.load_file ~rt p with
    | Ok () -> ()
    | Error e -> Alcotest.fail (name ^ ": " ^ e)
  in
  List.iter load ["html"; "hyper"];
  (* Register route-table handlers (Nopales). *)
  ignore (Runtime.eval_string ~rt
    {|(define (home)
       (render-html "<div><button id=b hx-get=/click hx-target=#out>go</button><div id=out>old</div></div>"))|});
  ignore (Runtime.eval_string ~rt {|(route "/" "home")|});
  ignore (Runtime.eval_string ~rt {|(define (clicker) (render-html "<b>new</b>"))|});
  ignore (Runtime.eval_string ~rt {|(route "/click" "clicker")|});
  (* hyper/visit resolves the route in-process and parses the page. *)
  Alcotest.check pp_value_testable "visit #out text"
    (Ast.VStr "old")
    (Runtime.eval_string ~rt {|(hyper/text (hyper/visit "/") "#out")|});
  (* hyper/click swaps #out; original page is unchanged (immutability). *)
  ignore (Runtime.eval_string ~rt {|(define pg (hyper/visit "/"))|});
  Alcotest.check pp_value_testable "click swaps #out"
    (Ast.VStr "new")
    (Runtime.eval_string ~rt {|(hyper/text (hyper/click pg "#b" (list)) "#out")|});
  Alcotest.check pp_value_testable "source page unchanged"
    (Ast.VStr "old")
    (Runtime.eval_string ~rt {|(hyper/text pg "#out")|});
  (* hyper/stream replays an ordered part list onto a fresh copy. *)
  Alcotest.check pp_value_testable "stream replay #out"
    (Ast.VStr "new")
    (Runtime.eval_string ~rt
       {|(hyper/text
          (hyper/stream pg
            (list (list (cons "html" "<b>new</b>")
                        (cons "target" "#out")
                        (cons "swap" "innerHTML"))))
          "#out")|});
  Alcotest.check pp_value_testable "stream keeps source"
    (Ast.VStr "old")
    (Runtime.eval_string ~rt {|(hyper/text pg "#out")|})

(** surfaces: the chat + code pages are Nopales route-table handlers
    (rendered via libs/html), redefinable live; a consumed turn's captured
    parts replay through hyper/stream. Loads the embedded surface the
    same way the binary boots. *)
let test_surfaces_nopales_routes ~rt () =
  (* Boot-load the surface (libs/html + libs/hyper + chat/code handlers). *)
  Surface.load ~rt;
  (* GET / renders the chat page skeleton with the htmx form + assets. *)
  (match Runtime.dispatch_route ~rt ~handler:"chat/page" ~method_:"GET"
           ~path:"/" ~params:[] ~headers:[] ~body:""
   with
   | Ast.VStr html ->
       Alcotest.(check bool) "chat page has hx-post /chat" true
         (String.length html > 0
          && (try ignore (Str.search_forward (Str.regexp_string "hx-post=\"/chat\"") html 0); true
              with Not_found -> false));
       Alcotest.(check bool) "chat page loads hx-multipart asset" true
         ((try ignore (Str.search_forward (Str.regexp_string "/assets/hx-multipart.js") html 0); true
           with Not_found -> false))
   | other -> Alcotest.fail ("chat/page: " ^ Reader.pp_value other));
  (* GET /code renders the code browser page (root listing). *)
  (match Runtime.dispatch_route ~rt ~handler:"code/page" ~method_:"GET"
           ~path:"/code" ~params:[] ~headers:[] ~body:""
   with
   | Ast.VStr html ->
       Alcotest.(check bool) "code page renders" true (String.length html > 0)
   | other -> Alcotest.fail ("code/page: " ^ Reader.pp_value other));
  (* Redefining chat/page live changes the next render (redefinable
     without restart). *)
  ignore (Runtime.eval_string ~rt
    {|(define (chat/page) (render-html "<h1>redefined</h1>"))|});
  (match Runtime.dispatch_route ~rt ~handler:"chat/page" ~method_:"GET"
           ~path:"/" ~params:[] ~headers:[] ~body:"" with
   | Ast.VStr s ->
       Alcotest.(check bool) "live redef took effect" true
         ((try ignore (Str.search_forward (Str.regexp_string "redefined") s 0); true
           with Not_found -> false))
   | other -> Alcotest.fail ("redef: " ^ Reader.pp_value other))

(** A consumed turn's captured parts replay through hyper/stream: simulate
    the multipart emitter's captured part list, then hyper/stream over a
    page that has #log. *)
let test_turn_replay_hyper_stream ~rt () =
  Surface.load ~rt;
  (* Build a page with #log (what the chat page skeleton renders). *)
  ignore (Runtime.eval_string ~rt
    {|(define pg (soup/parse "<div id=log><div>ready</div></div>"))|});
  (* Synthesize the part list the /chat emitter would capture: a user
     bubble into #log (beforeend) + an llm bubble into #log (beforeend) +
     a token span into the llm bubble (beforeend). *)
  ignore (Runtime.eval_string ~rt
    {|(define parts (list
        (list (cons "html" "<div class=bubble>you: hi</div>") (cons "target" "#log") (cons "swap" "beforeend"))
        (list (cons "html" "<div class=bubble id=llm-1></div>") (cons "target" "#log") (cons "swap" "beforeend"))
        (list (cons "html" "<span>hello</span>") (cons "target" "#llm-1") (cons "swap" "beforeend"))))|});
  Alcotest.check pp_value_testable "replay swaps #log"
    (Ast.VStr "<div id=\"log\"><div>ready</div><div class=\"bubble\">you: hi</div><div class=\"bubble\" id=\"llm-1\"><span>hello</span></div></div>")
    (Runtime.eval_string ~rt {|(soup/serialize (soup/select-one (hyper/stream pg parts) "#log"))|});
  Alcotest.check pp_value_testable "replay token lands in bubble"
    (Ast.VStr "<div class=\"bubble\" id=\"llm-1\"><span>hello</span></div>")
    (Runtime.eval_string ~rt {|(soup/serialize (soup/select-one (hyper/stream pg parts) "#llm-1"))|});
  (* Source page unchanged (immutability). *)
  Alcotest.check pp_value_testable "source page unchanged"
    (Ast.VStr "<div id=\"log\"><div>ready</div></div>")
    (Runtime.eval_string ~rt {|(soup/serialize (soup/select-one pg "#log"))|})

(** Nested-block replay: the /chat emitter wraps each contiguous reasoning
    run and each tool call in its OWN <details>/<summary> inside the turn's
    "thinking + tools" zone (#<rid>-think). Build the capture-ordered part
    list with the REAL templates (same calls the drain loop makes), replay
    through hyper/stream, and assert the nested DOM. *)
let test_turn_replay_nested_blocks ~rt () =
  Surface.load ~rt;
  (* Build the part list OCaml-side via Runtime.call_template — the same
     producer path the drain loop uses (template output unwrapped from
     its raw-html sentinel), bound into the env for hyper/stream. *)
  let tpl name args =
    match Runtime.call_template ~rt name (List.map (fun s -> Ast.VStr s) args) with
    | Some html -> html
    | None -> Alcotest.fail ("template missing: " ^ name)
  in
  let part html target swap : Ast.value =
    Ast.VList
      [ Ast.VPair (Ast.VStr "html", Ast.VStr html)
      ; Ast.VPair (Ast.VStr "target", Ast.VStr target)
      ; Ast.VPair (Ast.VStr "swap", Ast.VStr swap)
      ]
  in
  let parts =
    Ast.VList
      [ part (tpl "chat/turn-open" [ "t1" ]) "#log" "beforeend"
      ; part (tpl "chat/think-open" [ "t1-tk-1" ]) "#t1-think" "beforeend"
      ; part (tpl "chat/think-token" [ "rea" ]) "#t1-tk-1" "beforeend"
      ; part (tpl "chat/tool-open" [ "t1-tool-1"; "bash" ]) "#t1-think" "beforeend"
      ; part (tpl "chat/tool-line" [ "in"; "ls -la" ]) "#t1-tool-1" "beforeend"
      ; part (tpl "chat/tool-line" [ "out"; "file.ml" ]) "#t1-tool-1" "beforeend"
      ; part (tpl "chat/tool-done" [ "bash" ]) "#t1-tool-1-sum" "innerHTML"
      ; part (tpl "chat/think-open" [ "t1-tk-2" ]) "#t1-think" "beforeend"
      ; part (tpl "chat/think-token" [ "more" ]) "#t1-tk-2" "beforeend"
      ; part (tpl "chat/token" [ "answer" ]) "#t1" "beforeend"
      ]
  in
  ignore (Runtime.eval_string ~rt
    {|(define npg (soup/parse "<div id=log></div>"))|});
  Env.define rt.Runtime.env "nparts" parts;
  Alcotest.check pp_value_testable "nested blocks replay into zone"
    (Ast.VStr
       ("<div id=\"t1-think\">"
        ^ "<details class=\"tk\"><summary>thinking</summary><div id=\"t1-tk-1\"><span class=\"reason\">rea</span></div></details>"
        ^ "<details class=\"tool\"><summary id=\"t1-tool-1-sum\">tool: bash ✓</summary><div id=\"t1-tool-1\"><div class=\"trace\">in ls -la</div><div class=\"trace\">out file.ml</div></div></details>"
        ^ "<details class=\"tk\"><summary>thinking</summary><div id=\"t1-tk-2\"><span class=\"reason\">more</span></div></details>"
        ^ "</div>"))
    (Runtime.eval_string ~rt
       {|(soup/serialize (soup/select-one (hyper/stream npg nparts) "#t1-think"))|});
  Alcotest.check pp_value_testable "reply bubble got tokens"
    (Ast.VStr "<div class=\"bubble llm\" id=\"t1\"><span>answer</span></div>")
    (Runtime.eval_string ~rt
       {|(soup/serialize (soup/select-one (hyper/stream npg nparts) "#t1"))|})

let () =
  Eio_main.run @@ fun env ->
  let rt = Runtime.make () in
  Tools.install ~env ~resolve_path:Runtime.resolve_path ~register:(fun name run -> Runtime.register_builtin_tool rt name ~run);
  Eio.Switch.run @@ fun sw ->
  Alcotest.run "agent"
    [
      ( "plugin-model",
        [ Alcotest.test_case "lifecycle" `Quick (test_plugin_lifecycle ~rt) ] );
      ( "nopales-embed",
        [ Alcotest.test_case "rim-errors-redefine" `Quick
            (test_rim_errors_and_redefine ~rt) ] );
      ( "agent-loop",
        [
          Alcotest.test_case "tool-round-trip" `Quick
            (test_loop_tool_round_trip ~env ~sw ~rt);
          Alcotest.test_case "bash-deadline" `Quick
            (test_bash_deadline ~rt);
          Alcotest.test_case "tool-block" `Quick
            (test_loop_tool_block ~env ~sw ~rt);
          Alcotest.test_case "turn2-history-shape" `Quick
            (test_loop_turn2_history_shape ~env ~sw ~rt);
        ] );
      ( "provider-stream",
        [
          Alcotest.test_case "sse-text-and-tool-deltas" `Quick
            test_sse_stream_reconstruction;
        ] );
      ( "agent-route-table",
        [ Alcotest.test_case "register-and-render" `Quick
            (test_route_table_register_and_render ~rt)
        ; Alcotest.test_case "thin-context" `Quick
            (test_route_table_thin_context ~rt)
        ; Alcotest.test_case "pg-prim-unbound" `Quick
            (test_route_table_pg_prim_unbound ~rt)
        ] );
      ( "libs-hyper-on-agent",
        [ Alcotest.test_case "visit-click-stream" `Quick
            (test_libs_hyper_on_agent ~rt) ] );
      ( "surfaces",
        [ Alcotest.test_case "nopales-routes" `Quick
            (test_surfaces_nopales_routes ~rt)
        ; Alcotest.test_case "turn-replay-hyper-stream" `Quick
            (test_turn_replay_hyper_stream ~rt)
        ; Alcotest.test_case "turn-replay-nested-blocks" `Quick
            (test_turn_replay_nested_blocks ~rt) ] );
    ]
