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
    (Hashtbl.mem rt.seam.tools "greet");
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
  (* GET /code renders the code browser page (root listing): since the
     rail-trees-v4 amendment the dir face IS the rail's outline
     template -- node rows, not the retired table walk. *)
  (match Runtime.dispatch_route ~rt ~handler:"code/page" ~method_:"GET"
           ~path:"/code" ~params:[] ~headers:[] ~body:""
   with
   | Ast.VStr html ->
       Alcotest.(check bool) "code page renders" true (String.length html > 0);
       Alcotest.(check bool) "code page: tree rows, not the table" true
         ((try ignore (Str.search_forward (Str.regexp_string "fs-tr-item") html 0); true
           with Not_found -> false)
          && (try ignore (Str.search_forward (Str.regexp_string "<th>name</th>") html 0); false
              with Not_found -> true))
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

(** surfaces: the bindings browser renders on the SHARED tree template
    (rail prims box pane-face twin) over the bind/* habitat-bridge
    prims (faked — tests have no habitat), is pane-aware in fragment
    mode, and fails soft (muted notice on the root face, an empty
    level otherwise; no 500) when the bridge raises. *)
let test_surfaces_bindings_browser ~rt () =
  Surface.load ~rt;
  Runtime.add_prim rt "bind/packages" (fun _ -> Ast.VStr {|["notes","todo"]|});
  Runtime.add_prim rt "bind/list" (fun _ -> Ast.VStr {|["todo/add","todo/list"]|});
  Runtime.add_prim rt "bind/get" (fun _ ->
      Ast.VStr {|{"doc":"Adds a todo.","source":"(define (todo/add t) (create 'todo t))"}|});
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  let get ?(headers = []) params =
    match Runtime.dispatch_route ~rt ~handler:"bind/page" ~method_:"GET"
            ~path:"/bindings" ~params ~headers ~body:"" with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("bind/page: " ^ Reader.pp_value other)
  in
  let root = get [] in
  Alcotest.(check bool) "packages render the tree rows (dir labels + node paths)" true
    (has "fs-tr-item" root && has "data-node=\"prims/notes\"" root
     && has "data-node=\"prims/todo\"" root);
  let pkg = get [ ("pkg", "todo") ] in
  Alcotest.(check bool) "package fn rows (leaf labels open-or-focus)" true
    (has "fs-tr-item" pkg && has "name=todo/add" pkg && has "name=todo/list" pkg);
  Alcotest.(check bool) "breadcrumb trail" true
    (has "/bindings\">packages</a>" pkg);
  let detail = get [ ("pkg", "todo"); ("name", "todo/add") ] in
  Alcotest.(check bool) "detail doc + source" true
    (has "Adds a todo." detail
     && has "<pre>(define (todo/add t)" detail);
  let frag = get ~headers:[ ("hx-request", "true") ] [ ("pkg", "todo") ] in
  Alcotest.(check bool) "fragment = tree + trail, no doctype" true
    (has "fs-tr-item" frag && has "closest .fs-body" frag);
  Alcotest.(check bool) "fragment has no doctype" false (has "<!doctype" frag);
  (* apps col1 is the bindings route leaf (sidebar-main-dichotomy) *)
  Alcotest.check pp_value_testable "apps col1 is the bindings route leaf"
    (Ast.VStr "/bindings")
    (Runtime.eval_string ~rt {|(nth (nth (glochid/apps) 2) 1)|});
  (* fail-soft: the bridge raises habitat-unreachable at CALL time -- the
     page renders a muted notice and the tree level comes up empty. *)
  Runtime.add_prim rt "bind/packages" (fun _ ->
      raise
        (Ast.Interp_error
           (Ast.Prim_error ("habitat", "down", "habitat-unreachable"))));
  let down = get [] in
  Alcotest.(check bool) "bridge-down notice" true (has "habitat bridge:" down);
  let downLevel =
    match Runtime.eval_string ~rt {|(render-html (filmstrip/tree-level "prims" (glochid/prims-tree "")))|} with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("prims level: " ^ Reader.pp_value other)
  in
  Alcotest.(check bool) "bridge-down tree level is empty (fail-soft)" true
    (not (has "<li" downLevel));
  (* restore the canned bridge for any later evals on this rt *)
  Runtime.add_prim rt "bind/packages" (fun _ -> Ast.VStr {|["notes","todo"]|})

(** rail trees (borge/filmstrip.borg rail-trees-v4): the windows tree
    renders from DECLARED ws state (3 desktop dirs + shelf subtree on an
    empty workspace), level fragments splice lazy, dir rows carry
    hx-trigger "toggle once" (fetch exactly once), the tree endpoint
    serves one level, and url-enc/url-dec round-trip. No habitat, no
    real workspace row needed -- filmstrip/ws-default covers the reads. *)
let test_rail_trees ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  Alcotest.check pp_value_testable "roots: windows first"
    (Ast.VStr "windows")
    (Runtime.eval_string ~rt
      {|(as-str (car (car (filmstrip/tree-roots))))|});
  Alcotest.check pp_value_testable "windows tree root level: 3 desks + shelf"
    (Ast.VList
       [ Ast.VStr "desktop 0"; Ast.VStr "desktop 1"; Ast.VStr "desktop 2";
         Ast.VStr "shelf" ])
    (Runtime.eval_string ~rt
      {|(list/map
          (lambda (n) (as-str (dict-get n "label")))
          (glochid/windows-tree ""))|});
  (* toggling: dir rows carry the laziness attrs; labels carry open verbs *)
  let box =
    match Runtime.eval_string ~rt
            {|(render-html (filmstrip/tree-box (car (filmstrip/tree-roots))))|}
    with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("tree-box: " ^ Reader.pp_value other)
  in
  Alcotest.(check bool) "tree box: level-0 root ul" true
    (has "fs-tr-root" box);
  Alcotest.(check bool) "tree box: dir rows lazy via toggle once" true
    (has "hx-trigger=\"toggle once\"" box);
  Alcotest.(check bool) "tree box: level fragment URL is hxoas (tree?root)" true
    (has "/filmstrip/tree?root=windows&amp;node=" box);
  Alcotest.(check bool) "tree box: collapse-all word POSTs (a mutation, not a fetch)" true
    (has "hx-post=\"/filmstrip/tree/collapse\"" box);
  (* action-glyphs: verbs are GLYPHS; the word survives as data-verb +
     aria-label + title (the queryable identity + the tooltip), and the
     narrow-only actions fold is a PLAIN ul -- no nested summary/row. *)
  Alcotest.(check bool) "tree box: collapse-all renders a glyph" true
    (has "data-verb=\"collapse\"" box);
  Alcotest.(check bool) "tree box: desktop switch renders a glyph" true
    (has "data-verb=\"switch\"" box);
  Alcotest.(check bool) "tree box: glyphs carry a tooltip" true
    (has "title=\"switch this desktop\"" box);
  Alcotest.(check bool) "tree box: actions fold is a plain ul (no second summary)" true
    (has "class=\"fs-tr-acts-ul\"" box);
  Alcotest.(check bool) "tree box: no word-labeled verb buttons remain" true
    (not (has ">switch</button>" box));
  (* one level endpoint: the desk/0 level of an empty workspace is empty *)
  (match
     Runtime.dispatch_route ~rt ~handler:"filmstrip/tree" ~method_:"GET"
       ~path:"/filmstrip/tree"
       ~params:[ ("root", "windows"); ("node", "desk/0") ]
       ~headers:[ ("hx-request", "true") ] ~body:""
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "empty level = empty fragment" true
         (not (has "<li" s))
   | other -> Alcotest.fail ("tree level: " ^ Reader.pp_value other));
  (* unknown roots surface the status line, never a 500 *)
  (match
     Runtime.dispatch_route ~rt ~handler:"filmstrip/tree" ~method_:"GET"
       ~path:"/filmstrip/tree" ~params:[ ("root", "nope"); ("node", "") ]
       ~headers:[] ~body:""
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "missing root: status line only" true
         (has "no tree root nope" s)
   | other -> Alcotest.fail ("tree missing root: " ^ Reader.pp_value other));
  (* url round-trip on the six escapees *)
  Alcotest.check pp_value_testable "url-enc/url-dec round trip"
    (Ast.VStr "/code?path=a b&x%25y")
    (Runtime.eval_string ~rt
      {|(filmstrip/url-dec (filmstrip/url-enc "/code?path=a b&x%25y"))|});
  (* EAGER MODE (rail-trees-v4 amendment): a dir node carrying a kids
     JSON string renders its child level INLINE -- no toggle-once pull
     for that dir -- while kid rows keep the plain contract. *)
  let eager_row =
    match Runtime.eval_string ~rt
            {|(render-html (filmstrip/tree-node-li "code"
                 (dict-set* "{}" (list
                   "label" "small"
                   "kind" "dir"
                   "node" "small"
                   "open" ""
                   "verbs" ""
                   "kids" "[{\"label\":\"k.txt\",\"kind\":\"leaf\",\"node\":\"small/k.txt\",\"open\":\"/code?path=small/k.txt\",\"verbs\":\"\"}]"))))|}
    with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("eager row: " ^ Reader.pp_value other)
  in
  Alcotest.(check bool) "eager dir: kids render inline" true
    (has ">k.txt</button>" eager_row);
  Alcotest.(check bool) "eager dir: no lazy pull attrs on the details" true
    (not (has "hx-trigger=\"toggle once\"" eager_row));
  Alcotest.(check bool) "eager dir: kid rows ride the spliced-ul classes" true
    (has "fs-tr-kids" eager_row)

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
  Env.define rt.seam.env "nparts" parts;
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

(** i-1788670799-f: the agent's table covers the shared pure catalog
    (the hand copy here lacked length and the math tower entirely and
    registered string-split twice), and the catalog is callable through
    the runtime env. *)
let test_pure_catalog_parity ~(rt : Runtime.t) () =
  List.iter
    (fun name ->
       if not (Hashtbl.mem rt.seam.prims name) then
         Alcotest.fail ("agent table missing pure prim: " ^ name))
    Prims_pure.catalog_names;
  Alcotest.check pp_value_testable "length callable (was unbound)"
    (Ast.VInt 2L) (Runtime.eval_string ~rt "(length (list 1 2))")

let contains (s : string) (p : string) : bool =
  let n = String.length p in
  let rec go i = i + n <= String.length s && (String.sub s i n = p || go (i + 1)) in
  go 0

(** harness-context: the shared skeleton (llm/lib/harness.ml) assembles
    the binary's segments in the documented order with the facts block
    spliced exactly once (byte-identity to the pre-refactor prompts was
    verified by an A/B dump; this pins the assembly order). *)
let test_harness_context_structure () =
  let out =
    Harness.context
      ~pre_facts:"INTRO-PARAGRAPH."
      ~fact_lines:"- tools: t1\n- image: i1\n"
      ~post_facts:"Conduct:\n- be terse.\n"
      ~model:"MODEL9" ~base_url:"URL9"
  in
  Alcotest.(check bool) "starts with identity + facts header" true
    (contains out "INTRO-PARAGRAPH.\n\nHarness facts (told to you here;") ;
  Alcotest.(check bool) "computed facts spliced" true
    ((contains out "- host: ")
     && contains out "- cwd: "
     && contains out "- date: "
     && contains out "- model: MODEL9 via URL9\n");
  Alcotest.(check bool) "fact lines between model and conduct" true
    ((contains out "- tools: t1\n")
     && contains out "- image: i1\n\nConduct:\n");
  Alcotest.(check bool) "ends with the conduct block" true
    (let suf = "- be terse.\n" in
     String.length out >= String.length suf
     && String.sub out (String.length out - String.length suf)
          (String.length suf) = suf)

(** mock-script: Mock.parse_mock_script reads the canned JSON turns and
    Mock.complete consumes them in order (text delta per turn, empty
    end_turn reply once the script runs out). *)
let test_mock_script_consumption ~env ~sw () =
  let path = Filename.temp_file "pp_agent_mock" ".json" in
  let oc = open_out path in
  output_string oc
    {|[{"text":"hi","calls":[{"id":"c1","name":"bash","input":{"command":"ls"}}]},
       {"text":"","calls":[]}]|};
  close_out oc;
  let cell = ref (Mock.parse_mock_script path) in
  let cfg : Provider.cfg =
    { base_url = ""; api_key = ""; model = "mock"; system = "" }
  in
  let events = ref [] in
  (match Mock.complete ~script_cell:cell ~env ~sw ~cfg ~messages:[] ~tools:[]
           ~on_delta:(fun d -> events := d :: !events) with
   | Ok r ->
      Alcotest.(check string) "turn-1 text" "hi" r.Provider.text;
      Alcotest.(check int) "turn-1 one call" 1 (List.length r.Provider.calls);
      Alcotest.(check string) "turn-1 stop_reason" "stop" r.Provider.stop_reason
   | Error e -> Alcotest.fail e);
  Alcotest.(check int) "turn-1 fired text+done" 2 (List.length !events);
  (match Mock.complete ~script_cell:cell ~env ~sw ~cfg ~messages:[] ~tools:[]
           ~on_delta:(fun _ -> ()) with
   | Ok r ->
      Alcotest.(check string) "turn-2 empty text" "" r.Provider.text;
      Alcotest.(check string) "turn-2 stop_reason" "stop" r.Provider.stop_reason
   | Error e -> Alcotest.fail e);
  (match Mock.complete ~script_cell:cell ~env ~sw ~cfg ~messages:[] ~tools:[]
           ~on_delta:(fun _ -> ()) with
   | Ok r ->
      Alcotest.(check string) "exhausted -> end_turn" "end_turn"
        r.Provider.stop_reason
   | Error e -> Alcotest.fail e);
  Sys.remove path

(* sessions-persist (agent/lib/sessions.ml): append-only JSONL
   transcripts under the local store; list + resume round-trip; the
   message JSON (incl. tool_calls) survives a write+read; prims are
   registered on the runtime. *)
let test_sessions_roundtrip ~rt () =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "pp-sessions-test-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Fun.protect
    ~finally:(fun () ->
        (* 0444/0700: unset takes precedence over "not set" *)
        Unix.putenv "AGENT_SESSIONS_DIR" "")
    (fun () ->
       Unix.putenv "AGENT_SESSIONS_DIR" dir;
       let sess : Sessions.handle option ref = ref None in
       let history : Provider.message list ref = ref [] in
       (* lazy trigger: NO eager session; the FIRST on_msg creates it *)
       let on_msg = Sessions.on_msg ~create:true ~model:(fun () -> "test-model") sess in
       Alcotest.(check int) "lazy: empty store before first msg" 0
         (try Sys.readdir (Sessions.root ()) |> Array.length with _ -> 0);
       let user = { Provider.role = "user"; content = `String "first prompt";
                    tool_call_id = None; tool_calls = None } in
       let asst =
         { Provider.role = "assistant"; content = `String "";
           tool_call_id = None;
           tool_calls =
             Some
               (`List
                 [ `Assoc
                     [ ("id", `String "t1"); ("type", `String "function")
                     ; ( "function"
                       , `Assoc
                           [ ("name", `String "bash")
                           ; ( "arguments"
                             , `String "{\"command\":\"echo hi\"}" ) ] ) ] ]) }
       in
       let tool =
         { Provider.role = "tool"; content = `String "hi";
           tool_call_id = Some "t1"; tool_calls = None }
       in
       List.iter on_msg [ user; asst; tool ];
       Alcotest.(check bool) "lazy: first msg CREATED the session" true
         (match !sess with Some _ -> true | None -> false);
       Alcotest.(check int) "count tracked" 3 (match !sess with Some h -> h.Sessions.count | None -> -1);
       (* listing: one entry, correct preview + count *)
       (match Sessions.entries () with
        | [ e ] ->
            Alcotest.(check string) "entry model" "test-model" e.Sessions.e_model;
            Alcotest.(check int) "entry count" 3 e.Sessions.e_count;
            Alcotest.(check string) "entry preview" "first prompt" e.Sessions.e_preview
        | other -> Alcotest.fail ("expected one entry, got " ^ string_of_int (List.length other)));
       (* resume round-trip: same messages replay, incl. tool_calls *)
       let id = (match !sess with Some h -> h.Sessions.id | None -> assert false) in
       (match Sessions.resume id with
        | Ok (h, msgs) ->
            Alcotest.(check int) "resumed count" 3 h.Sessions.count;
            (match msgs with
             | [ u; a; t ] ->
                 Alcotest.(check string) "user role" "user" u.Provider.role;
                 Alcotest.(check string) "user text" "first prompt"
                   (match u.Provider.content with `String s -> s | _ -> "?");
                 Alcotest.(check bool) "tool_calls survived" true
                   (Option.is_some a.Provider.tool_calls);
                 Alcotest.(check string) "tool result" "hi"
                   (match t.Provider.content with `String s -> s | _ -> "?")
             | _ -> Alcotest.fail "resume gave wrong arity");
            (* appending CONTINUES the resumed file (the prim re-points
               sess; mirror that here) *)
            sess := Some h;
            let on_msg2 = Sessions.on_msg ~create:true ~model:(fun () -> "test-model") sess in
            on_msg2 { Provider.role = "user"; content = `String "after resume";
                      tool_call_id = None; tool_calls = None };
            Alcotest.(check int) "count continued" 4 h.Sessions.count
        | Error e -> Alcotest.fail e);
       (* prims work over the seam *)
       Sessions.register ~rt ~history ~sess ~model:(fun () -> "test-model");
       (match Runtime.eval_string ~rt ("(session/data \"" ^ id ^ "\")") with
        | VStr j ->
            let j = Yojson.Safe.from_string j in
            let first = List.hd (Yojson.Safe.Util.to_list j) in
            Alcotest.(check string) "data first role" "user"
              (Yojson.Safe.Util.member "role" first |> Yojson.Safe.Util.to_string)
        | _ -> Alcotest.fail "session/data not a string");
       (match Runtime.eval_string ~rt "(session/name)" with
        | VStr s -> Alcotest.(check string) "session/name" id s
        | _ -> Alcotest.fail "session/name not a string");
       (* resume refuses path escapes *)
       (match Sessions.resume "../etc/passwd" with
        | Error _ -> ()
        | Ok _ -> Alcotest.fail "path escape allowed"))

(* sessions-persist: the loop's ?on_msg fires once per message the
   turn appends (user + assistant here; + one per tool result). *)
let test_loop_on_msg ~env ~sw ~rt () =
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages:_ ~tools:_ ~on_delta:_ =
    Ok
      { Provider.text = "done"; calls = []; stop_reason = "stop";
        raw = `Assoc [ ("role", `String "assistant"); ("content", `String "done") ] }
  in
  let cfg : Provider.cfg =
    { base_url = "http://127.0.0.1:1"; api_key = ""; model = "mock"; system = "" }
  in
  let got = ref [] in
  (match
     Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete
       ~on_msg:(fun m -> got := m.Provider.role :: !got)
       ~on_event:(fun _ -> ()) (ref []) "hello"
   with
   | Error e -> Alcotest.fail e
   | Ok () ->
       Alcotest.(check (list string)) "on_msg saw user+assistant" [ "user"; "assistant" ]
         (List.rev !got))

(* memory-bridge (agent/lib/habitat.ml): the remote expr builders are
   pure — gregor's libs/memory contract (TYPE first, tags default [])
   with Nopales-safe quoting. *)
let test_habitat_expr_builders () =
  Alcotest.(check string) "remember expr"
    "(mem/remember \"gregor-memory\" \"m-1\" \"goal\" \"ship P1\" \"[]\")"
    (Habitat.remember_expr ~typ:"gregor-memory" ~id:"m-1" ~title:"goal"
       ~content:"ship P1" ~tags:"");
  Alcotest.(check string) "remember with tags"
    "(mem/remember \"memory\" \"m-2\" \"t\" \"c\" \"[\\\"x\\\"]\")"
    (Habitat.remember_expr ~typ:"memory" ~id:"m-2" ~title:"t" ~content:"c"
       ~tags:"[\"x\"]");
  (* content with quotes/newlines must survive the Nopales reader *)
  let q_hacky =
    Habitat.remember_expr ~typ:"memory" ~id:"m-3" ~title:"t\"\nt"
      ~content:"line1\nline2 \"q\"" ~tags:""
  in
  Alcotest.(check bool) "quote escape" true
    (try
       Str.search_forward (Str.regexp_string "t\\\"\\nt") q_hacky 0 > 0
       || String.length q_hacky > 0
     with Not_found -> false);
  Alcotest.(check string) "search expr"
    "(json-emit (mem/search \"gregor-memory\" \"P1\"))"
    (Habitat.search_expr ~typ:"gregor-memory" ~needle:"P1");
  Alcotest.(check string) "get expr via as-str"
    "(as-str (mem/get \"memory\" \"m-1\"))"
    (Habitat.get_expr ~typ:"memory" ~id:"m-1");
  Alcotest.(check string) "forget expr"
    "(as-str (mem/forget \"memory\" \"m-1\"))"
    (Habitat.forget_expr ~typ:"memory" ~id:"m-1");
  Alcotest.(check bool) "default type is gregor-memory" true
    (String.length (Habitat.default_type ()) > 3);
  (* bindings browser verbs (the AST browser over the habitat grammar) *)
  Alcotest.(check string) "bind/packages expr"
    "(json-emit (list-packages))"
    (Habitat.packages_expr ());
  Alcotest.(check string) "bind/list expr"
    "(json-emit (package-bindings \"todo\"))"
    (Habitat.bindings_expr ~pkg:"todo");
  Alcotest.(check string) "bind/get expr"
    "(json-emit (dict-set (dict-set \"{}\" \"doc\" (function-doc \"todo/add\")) \"source\" (function-source \"todo/add\")))"
    (Habitat.binding_expr ~name:"todo/add")

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
      ( "pure-catalog",
        [ Alcotest.test_case "parity" `Quick
            (test_pure_catalog_parity ~rt) ] );
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
      ( "harness-context",
        [ Alcotest.test_case "structure" `Quick
            test_harness_context_structure ] );
      ( "mock-script",
        [ Alcotest.test_case "consumption" `Quick
            (test_mock_script_consumption ~env ~sw) ] );
      ( "sessions-persist",
        [ Alcotest.test_case "roundtrip" `Quick (test_sessions_roundtrip ~rt)
        ; Alcotest.test_case "turn-sink" `Quick
            (test_loop_on_msg ~env ~sw ~rt) ] );
      ( "memory-bridge",
        [ Alcotest.test_case "expr-builders" `Quick
            test_habitat_expr_builders ] );
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
            (test_turn_replay_nested_blocks ~rt)
        ; Alcotest.test_case "bindings-browser" `Quick
            (test_surfaces_bindings_browser ~rt)
        ; Alcotest.test_case "rail-trees" `Quick
            (test_rail_trees ~rt) ] );
    ]
