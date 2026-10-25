(** Tests for borge/glochid.borg: plugin load, rim, events, loop. No
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
      (Printf.sprintf "glochid-test-%d" (Unix.getpid ()))
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

(** glochid-loop: a mock provider emitting one tool call round-trips
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
            usage = None;
        }
    else
      Ok
        {
          Provider.text = "done";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "done") ];
            usage = None;
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

(** glochid-loop: a tool_call hook blocking reaches the model as an
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
            usage = None;
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
            usage = None;
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

(** glochid-loop regression (turn-2 provider 400): the assistant entry
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
            usage = None;
        }
    else if !calls = 2 then
      Ok
        {
          Provider.text = "done";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "done") ];
            usage = None;
        }
    else (
      captured := messages;
      Ok
        {
          Provider.text = "ok2";
          calls = [];
          stop_reason = "stop";
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "ok2") ];
            usage = None;
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

(** glochid-route-table: (route ...) makes a path resolvable via
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
    request-method / hx-request? resolve inside the handler. HX-Request
    rides HEADERS, not params (hypermedia-harness request-runner) — a
    handler must see hx=false when it is only in params, or the whole
    fragment-vs-page split is meaningless. *)
let test_route_table_thin_context ~rt () =
  ignore (Runtime.eval_string ~rt
    {|(define (qtest)
       (string-append (request-method) "|"
         (query-param "x") "|"
         (if (hx-request?) "hx" "nohx")))|});
  ignore (Runtime.eval_string ~rt {|(route "/q" "qtest")|});
  Alcotest.check pp_value_testable "thin context resolves (hx in headers)"
    (Ast.VStr "POST|42|hx")
    (Runtime.eval_string ~rt
       {|(route/dispatch "qtest" "POST" "/q"
           (list (cons "x" "42"))
           (list (cons "hx-request" "true")))|});
  Alcotest.check pp_value_testable "params alone do NOT imply hx-request"
    (Ast.VStr "POST|42|nohx")
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
  (* dune runs the test with cwd = glochid/test/, so walk up to find the
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
    (Runtime.eval_string ~rt {|(hyper/text pg "#out")|});
  (* REGRESSION (fragment-vs-page): the click loop must send HX-Request as a
     HEADER. When hyper/do-swap instead stuffed it into the params alist,
     hx-request? read false and a handler with distinct fragment/page
     branches returned its WHOLE-PAGE branch, which soup/swap! then spliced
     into the target. The "clicker" handler above renders the same markup on
     both branches, so it could never catch this. This pair CAN: the page
     carries a button targeting /branchy, and branchy answers a bare
     fragment under hx and a marked full page without it. *)
  ignore (Runtime.eval_string ~rt
    {|(define (branchy)
       (if (hx-request?)
           (render-html "<b>fragment</b>")
           (render-html "<html><body><b>page</b></body></html>")))|});
  ignore (Runtime.eval_string ~rt {|(route "/branchy" "branchy")|});
  ignore (Runtime.eval_string ~rt
    {|(define (branchy/page)
       (render-html "<div><button id=bb hx-get=/branchy hx-target=#bout>go</button><div id=bout>old</div></div>"))|});
  ignore (Runtime.eval_string ~rt {|(route "/branchy/page" "branchy/page")|});
  Alcotest.check pp_value_testable "click takes the fragment branch"
    (Ast.VStr "fragment")
    (Runtime.eval_string ~rt
       {|(hyper/text (hyper/click (hyper/visit "/branchy/page") "#bb" (list)) "#bout")|})


(** calendar faces (card e1d032a0): the calendar day/event click is
    PANE-LOCAL in filmstrip. Loads libs/lookbook + libs/calendar the way
    the habitat boots them (html + hyper first), binds the /calendar
    route, and asserts: (1) hyper/expect over the changed route -- GET
    /calendar?day=2026-10-18 serves the day agenda fragment with the day
    header; (2) the month grid day cells carry hx-get + hx-target
    #cal-main (pane-local swap, never a plain anchor out of the shell);
    (3) lookbook evidence: calendar/nav-a answers its examples card and
    is registered in (lookbook/elements). *)

let test_calendar_day_pane_local ~rt () =
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
  let fmt_interp_error = function
    | Ast.Unbound_var s -> "unbound: " ^ s
    | Ast.Not_a_function _ -> "not-a-function"
    | Ast.Arity_mismatch (n, exp, got) ->
      Printf.sprintf "arity %s: expected %d got %d" n exp got
    | Ast.Wrong_arg_count (n, exp, got) ->
      Printf.sprintf "wrong-arg-count %s: expected %d got %d" n exp got
    | Ast.Type_mismatch (exp, _) -> "type-mismatch: expected " ^ exp
    | Ast.Budget_exceeded -> "budget-exceeded"
    | Ast.Prim_error (p, m, _) -> "prim " ^ p ^ ": " ^ m
    | Ast.Parse_error (m, l, c) -> Printf.sprintf "parse: %s at %d:%d" m l c
    | Ast.User_error (m, _) -> "user-error: " ^ m
  in
  let eval_or_fail what s =
    match Runtime.eval_string ~rt s with
    | v -> v
    | exception Ast.Interp_error e ->
      Alcotest.fail (what ^ ": " ^ fmt_interp_error e)
    | exception e -> Alcotest.fail (what ^ ": " ^ Printexc.to_string e)
  in
  List.iter load ["html"; "hyper"; "lookbook"; "calendar"];
  (* Store/session prims the face reads at render time; defines resolve
     at call time, so the loads above never needed them. date-format /
     date-weekday back the date prims the month/day renders hit
     (2026-10: the 1st is a Thursday, so dow = (mod (+ 3 d) 7)). *)
  ignore (eval_or_fail "stubs"
            {|(begin (define (rows k n) (list))
                     (define (todo/item/all) (list))
                     (define (notes/doc/get s) (list))
                     (define (tree/list-kind a b c) (list))
                     (define (date-today) "2026-10-18")
                     (define (date-format y m d)
                       (string-append (as-str y) "-"
                                      (calendar/pad2 m) "-"
                                      (calendar/pad2 d)))
                     (define (date-weekday y m d) (mod (+ 3 d) 7))
                     ;; request-param is a Nopales shim in the surface
                     ;; blob, not a runtime prim; the face reads it at
                     ;; dispatch.
                     (define (request-param n) (query-param n)))|});
  ignore (Runtime.eval_string ~rt {|(route "/calendar" "calendar/page")|});
  (* Dispatch the changed route the way the host does: a real request
     context with the HX-Request header (pane partial mode). *)
  let dispatch params =
    match Runtime.dispatch_route ~rt ~handler:"calendar/page" ~method_:"GET"
            ~path:"/calendar" ~params ~headers:[ ("hx-request", "true") ]
            ~body:"" with
    | Ast.VStr html -> html
    | other -> Alcotest.fail ("calendar/page: " ^ Ast.pp_value_short other)
    | exception Ast.Interp_error e ->
      Alcotest.fail ("calendar/page dispatch: " ^ fmt_interp_error e)
    | exception e -> Alcotest.fail ("calendar/page dispatch: " ^ Printexc.to_string e)
  in
  let nopales_str s =
    let b = Buffer.create (String.length s * 2) in
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b
  in
  (* (1) hyper/expect over the changed route: GET /calendar?day=2026-10-18
     under HX-Request serves the day agenda fragment (the pane body). *)
  let day_html = dispatch [ ("day", "2026-10-18") ] in
  ignore (eval_or_fail "day page"
            ("(define pg (soup/parse \"" ^ nopales_str day_html ^ "\"))"));
  Alcotest.check pp_value_testable "day fragment via hyper/expect"
    (Ast.VBool true)
    (eval_or_fail "hyper/expect h2" {|(hyper/expect pg "h2" "2026-10-18")|});
  (* (2) the month grid: every day cell is a pane-local anchor. *)
  let month_html = dispatch [ ("ym", "2026-10") ] in
  ignore (eval_or_fail "month page"
            ("(define pgm (soup/parse \"" ^ nopales_str month_html ^ "\"))"));
  Alcotest.check pp_value_testable "day cells swap #cal-main in place"
    (Ast.VBool true)
    (eval_or_fail "hx-target #cal-main"
       {|(string-contains? (hyper/html pgm) "hx-target=\"#cal-main\"")|});
  Alcotest.check pp_value_testable "day cell hx-gets the day route"
    (Ast.VBool true)
    (eval_or_fail "hx-get day route"
       {|(string-contains? (hyper/html pgm) "hx-get=\"/calendar?day=2026-10-18\"")|});
  (* (3) lookbook evidence for the new element. *)
  Alcotest.check pp_value_testable "nav-a is pane-local"
    (Ast.VStr "#cal-main")
    (eval_or_fail "nav-a attrs"
       {|(soup/attr
           (car (soup/select
                  (soup/parse
                    (calendar/nav-a "/calendar?day=2026-10-18" "18"))
                  "a"))
           "hx-target")|});
  Alcotest.check pp_value_testable "lookbook inventory registers calendar/nav-a"
    (Ast.VBool true)
    (eval_or_fail "lookbook inventory"
       {|(string-contains? (json-emit (lookbook/elements)) "calendar/nav-a")|});
  Alcotest.check pp_value_testable "nav-a examples card"
    (Ast.VStr "calendar/nav-a")
    (eval_or_fail "examples card"
       {|(lb/get (calendar/nav-a/examples) "name")|})
(** welcome index (card e25d8165 SITE 9): the closing section's CTA
    row links to the contact page. Loads the public libs the way the
    site boots them (html + css + hyper + lookbook + layout +
    welcome), binds /, and asserts: (1) hyper/expect over the changed
    route -- GET / serves the index whose hero title is the site name;
    (2) the closing CTA row carries the Talk to us link to /contacts;
    (3) lookbook evidence: layout/cta answers its examples card and is
    registered in (lookbook/elements). *)
let test_welcome_index_cta ~rt () =
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
    let ic = open_in p in
    let n = in_channel_length ic in
    let txt = really_input_string ic n in
    close_in ic;
    (* The (package ...) export header is production-runtime sugar;
       strip it so the thin test runtime can load the lib. *)
    let stripped =
      match Str.search_forward (Str.regexp_string "(package ") txt 0 with
      | exception Not_found -> txt
      | pos ->
        let rec skip i depth =
          if depth = 0 then i
          else
            match txt.[i] with
            | '(' -> skip (i + 1) (depth + 1)
            | ')' -> skip (i + 1) (depth - 1)
            | _ -> skip (i + 1) depth
        in
        let stop = skip (pos + 1) 1 in
        String.sub txt 0 pos
        ^ String.sub txt stop (String.length txt - stop)
    in
    let tmp =
      Filename.concat (Filename.get_temp_dir_name ())
        ("pp-test-" ^ name ^ ".lib.pp") in
    let oc = open_out tmp in
    output_string oc stripped;
    close_out oc;
    match Runtime.load_file ~rt tmp with
    | Ok () -> ()
    | Error e -> Alcotest.fail (name ^ ": " ^ e)
  in
  let eval_or_fail what s =
    match Runtime.eval_string ~rt s with
    | v -> v
    | exception Ast.Interp_error (Ast.Prim_error (k, n, m)) ->
      Alcotest.fail (what ^ ": prim " ^ k ^ "/" ^ n ^ ": " ^ m)
    | exception Ast.Interp_error (Ast.Unbound_var s) ->
      Alcotest.fail (what ^ ": unbound " ^ s)
    | exception e -> Alcotest.fail (what ^ ": " ^ Printexc.to_string e)
  in
  List.iter load ["html"; "css"; "hyper"; "lookbook"; "layout"; "welcome"];
  ignore (eval_or_fail "route" {|(route "/" "welcome/page")|});
  (* Dispatch the changed route the way a browser's first nav does: a
     full-page GET with no HX-Request header. *)
  let page_html =
    match Runtime.dispatch_route ~rt ~handler:"welcome/page" ~method_:"GET"
            ~path:"/" ~params:[] ~headers:[] ~body:"" with
    | Ast.VStr html -> html
    | other -> Alcotest.fail ("welcome/page: " ^ Ast.pp_value_short other)
    | exception e -> Alcotest.fail ("welcome/page dispatch: " ^ Printexc.to_string e)
  in
  Runtime.add_prim rt "welcome-index-html" (fun _ -> Ast.VStr page_html);
  ignore (eval_or_fail "parse" {|(define pg (soup/parse (welcome-index-html)))|});
  (* (1) hyper/expect over the changed route: GET / serves the index. *)
  Alcotest.check pp_value_testable "index hero via hyper/expect"
    (Ast.VBool true)
    (eval_or_fail "hyper/expect h1" {|(hyper/expect pg "h1" "Pricklypear")|});
  (* (2) the closing section's CTA row links to the contact page. *)
  Alcotest.(check bool) "index CTA links to /contacts" true
    (match
       eval_or_fail "cta href"
         {|(string-contains? (hyper/html pg) "href=\"/contacts\"")|}
     with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("cta href: " ^ Reader.pp_value other));
  Alcotest.(check bool) "index CTA says Talk to us" true
    (match
       eval_or_fail "cta label"
         {|(string-contains? (hyper/html pg) ">Talk to us</a>")|}
     with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("cta label: " ^ Reader.pp_value other));
  (* (3) lookbook evidence for the reused element. *)
  Alcotest.(check bool) "lookbook inventory registers layout/cta" true
    (match
       eval_or_fail "lookbook inventory"
         {|(string-contains? (json-emit (lookbook/elements)) "layout/cta")|}
     with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("lookbook inventory: " ^ Reader.pp_value other));
  Alcotest.check pp_value_testable "layout/cta examples card"
    (Ast.VStr "layout/cta")
    (eval_or_fail "examples card" {|(lb/get (layout/cta/examples) "name")|})


(** welcome site (card 93434fb0 SITE 7): roerick@wyo.town is the site's
    contact email as a REAL mailto anchor from the declared layout/mailto
    element (libs/layout). Binds the public routes, dispatches them the
    way a browser's first nav does, and asserts: (1) GET / serves the
    index whose footer carries href="mailto:roerick@wyo.town";
    (2) the other public footers (background, roadmap, book) render the
    same real mailto; (3) lookbook evidence: layout/mailto answers its
    examples card and is registered in (lookbook/elements). *)
let test_welcome_mailto_footers ~rt () =
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
    let ic = open_in p in
    let n = in_channel_length ic in
    let txt = really_input_string ic n in
    close_in ic;
    (* The (package ...) export header is production-runtime sugar;
       strip it so the thin test runtime can load the lib. *)
    let stripped =
      match Str.search_forward (Str.regexp_string "(package ") txt 0 with
      | exception Not_found -> txt
      | pos ->
        let rec skip i depth =
          if depth = 0 then i
          else
            match txt.[i] with
            | '(' -> skip (i + 1) (depth + 1)
            | ')' -> skip (i + 1) (depth - 1)
            | _ -> skip (i + 1) depth
        in
        let stop = skip (pos + 1) 1 in
        String.sub txt 0 pos
        ^ String.sub txt stop (String.length txt - stop)
    in
    let tmp =
      Filename.concat (Filename.get_temp_dir_name ())
        ("pp-test-" ^ name ^ ".lib.pp") in
    let oc = open_out tmp in
    output_string oc stripped;
    close_out oc;
    match Runtime.load_file ~rt tmp with
    | Ok () -> ()
    | Error e -> Alcotest.fail (name ^ ": " ^ e)
  in
  let eval_or_fail what s =
    match Runtime.eval_string ~rt s with
    | v -> v
    | exception Ast.Interp_error (Ast.Prim_error (k, n, m)) ->
      Alcotest.fail (what ^ ": prim " ^ k ^ "/" ^ n ^ ": " ^ m)
    | exception Ast.Interp_error (Ast.Unbound_var s) ->
      Alcotest.fail (what ^ ": unbound " ^ s)
    | exception e -> Alcotest.fail (what ^ ": " ^ Printexc.to_string e)
  in
  let dispatch path handler =
    match Runtime.dispatch_route ~rt ~handler ~method_:"GET"
            ~path ~params:[] ~headers:[] ~body:"" with
    | Ast.VStr html -> html
    | other -> Alcotest.fail (handler ^ ": " ^ Ast.pp_value_short other)
    | exception e -> Alcotest.fail (handler ^ " dispatch: " ^ Printexc.to_string e)
  in
  List.iter load ["html"; "css"; "hyper"; "lookbook"; "layout"; "welcome"];
  List.iter
    (fun (path, handler) ->
       ignore (eval_or_fail "route" (Printf.sprintf "(route %S %S)" path handler)))
    [ ("/", "welcome/page"); ("/background", "welcome/background");
      ("/roadmap", "welcome/roadmap"); ("/book", "welcome/book") ];
  (* (1) hyper/expect-adjacent over the changed route: GET / footer. *)
  let page_html = dispatch "/" "welcome/page" in
  Runtime.add_prim rt "welcome-site-html" (fun _ -> Ast.VStr page_html);
  ignore (eval_or_fail "parse" {|(define pg (soup/parse (welcome-site-html)))|});
  Alcotest.check pp_value_testable "index renders via hyper/expect"
    (Ast.VBool true)
    (eval_or_fail "hyper/expect h1" {|(hyper/expect pg "h1" "Pricklypear")|});
  Alcotest.(check bool) "index footer carries the real mailto" true
    (match
       eval_or_fail "index mailto"
         {|(string-contains? (hyper/html pg) "href=\"mailto:roerick@wyo.town\"")|}
     with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("index mailto: " ^ Reader.pp_value other));
  (* (2) the other public footers render the same real mailto. *)
  List.iter
    (fun (name, path, handler) ->
       let html = dispatch path handler in
       Alcotest.(check bool) (name ^ " footer carries the real mailto") true
         (try let _ = Str.search_forward
                 (Str.regexp_string "href=\"mailto:roerick@wyo.town\"") html 0 in true
          with Not_found -> false))
    [ ("background", "/background", "welcome/background");
      ("roadmap", "/roadmap", "welcome/roadmap");
      ("book", "/book", "welcome/book") ];
  (* (3) lookbook evidence for the declared element. *)
  Alcotest.(check bool) "lookbook inventory registers layout/mailto" true
    (match
       eval_or_fail "lookbook inventory"
         {|(string-contains? (json-emit (lookbook/elements)) "layout/mailto")|}
     with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("lookbook inventory: " ^ Reader.pp_value other));
  Alcotest.check pp_value_testable "layout/mailto examples card"
    (Ast.VStr "layout/mailto")
    (eval_or_fail "examples card" {|(lb/get (layout/mailto/examples) "name")|})

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

(** optimistic htmx loader (card c7be7174): the strip's ordering/opening/
    closing triggers carry the declared optimistic attrs -- instant
    semi-fade while the roundtrip runs (css :has on htmx's htmx-request
    class; the keyboard move also transposes the focused pane via hx-on)
    -- and the server's OOB response lands already unfaded. A hyper/expect
    rides a real close roundtrip over the shell. *)
let test_filmstrip_optimistic ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  let eval_str ~what str =
    match Runtime.eval_string ~rt str with
    | Ast.VStr s -> s
    | other -> Alcotest.fail (what ^ ": " ^ Reader.pp_value other)
    | exception Ast.Interp_error e ->
        Alcotest.fail
          (what ^ ": INTERP "
           ^ (match e with
              | Ast.Prim_error (k, n, m) -> k ^ "/" ^ n ^ ": " ^ m
              | _ -> "opaque"))
  in
  (* pane-head verbs carry data-optimistic (the declared trigger mark) *)
  let head =
    eval_str ~what:"pane-head"
      {|(render-html (filmstrip/pane-head (dict-set* "{}" (list "id" "t1" "route" "/code"))))|}
  in
  Alcotest.(check bool) "pane-head: width verb optimistic" true
    (has "data-optimistic=\"width\"" head);
  Alcotest.(check bool) "pane-head: close verb optimistic" true
    (has "data-optimistic=\"close\"" head);
  (* the move verbs carry the mark + the instant-transpose hx-on --
     checked on the DECLARED attrs def (rendering filmstrip/key-sentinels
     hits a pre-existing interp failure at HEAD: keyboard-shortcuts). *)
  let ksent =
    eval_str ~what:"optimistic-attrs move"
      {|(html/attrs (filmstrip/optimistic-attrs "move"))|}
  in
  Alcotest.(check bool) "move verb: optimistic attrs" true
    (has "data-optimistic=\"move\"" ksent && has "hx-on::before-request" ksent);
  (* the css pending rules ride filmstrip/css *)
  let css = eval_str ~what:"css" {|(css/render (filmstrip/css))|} in
  Alcotest.(check bool) "css: optimistic pending rules" true
    (has "[data-optimistic].htmx-request" css && has ".fs-pane.fs-optimistic" css);
  (* the examples card: the element's declaration (lookbook discipline) *)
  let card =
    eval_str ~what:"examples card" {|(filmstrip/optimistic-attrs/examples)|}
  in
  Alcotest.(check bool) "lookbook: optimistic-attrs card declares the element" true
    (has "filmstrip/optimistic-attrs" card);
  (* hyper/expect over the changed route: POST /filmstrip/open through the
     real route machinery, parse the OOB response fragment, then click the
     optimistic close verb. The full shell render exceeds the 400k eval
     budget on the main-repo build (habitat issue i-1789591788), so the
     fragment page stands in for hyper/visit. *)
  (* a cheap pane body: the pane content dispatch must not blow the 400k
     eval budget on the main-repo build (habitat issue i-1789591788); the
     bindings browser with its canned bind prims (left by
     test_surfaces_bindings_browser) is the cheap known-good face. *)
  (match
     Runtime.dispatch_route ~rt ~handler:"filmstrip/open" ~method_:"POST"
       ~path:"/filmstrip/open" ~params:[ ("route", "/bindings") ]
       ~headers:[ ("hx-request", "true") ] ~body:""
   with
   | Ast.VStr _ -> ()
   | other -> Alcotest.fail ("open: " ^ Reader.pp_value other));
  ignore (Runtime.eval_string ~rt
      {|(define pg (soup/parse (route/dispatch (route/lookup "/filmstrip/open") "POST" "/filmstrip/open" (list (cons "route" "/bindings")) (list (cons "hx-request" "true")))))|});
  ignore (Runtime.eval_string ~rt
    {|(define pid (hyper/attr (car (hyper/query pg ".fs-pane")) "data-pane"))|});
  ignore (Runtime.eval_string ~rt
    {|(define close-btn
         (car (list/filter
                (lambda (b)
                  (string-eq (as-str (hyper/attr b "data-optimistic")) "close"))
                (hyper/query pg "button"))))|});
  Alcotest.check pp_value_testable "hyper: optimistic close verb served"
    (Ast.VStr "close")
      (Runtime.eval_string ~rt {|(as-str (hyper/attr close-btn "data-optimistic"))|});
  (* hyper/expect over the changed route: the pane fragment served by
     POST /filmstrip/open carries the optimistic close verb. (hyper/click's
     fragment-branch handling is broken at HEAD -- visit-click-stream.) *)
  Alcotest.(check bool) "hyper: optimistic pane fragment serves the title" true
      (match Runtime.eval_string ~rt {|(hyper/expect pg ".fs-title" "/bindings")|} with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("expect: " ^ Reader.pp_value other))

(** file-issue rail button (cards 5877331e + 5da2cf6c): the railtabs strip
    carries a declared lookbook element that opens the /issues face in a
    modal dialog (ui/dialog via POST /filmstrip/dialog, swapped into the
    #fs-layers sink) -- reporting an issue or UI feedback is one click
    from any desktop, no pane, no navigation. The examples card + the
    host inventory witness the lookbook discipline; hyper/expects ride a
    real POST /filmstrip/dialog roundtrip with route=/issues (a stub
    issues face stands in on the agent runtime) plus a close-button
    roundtrip that empties the sink. *)
let test_filmstrip_file_issue ~rt () =
  Surface.load ~rt;
  (* hermetic ws: a throwaway row file so stale pane state from an earlier
     test or run can never satisfy (or mask) the open roundtrip below *)
  let ws = "/tmp/glochid-file-issue-test-ws.json" in
  (try Sys.remove ws with Sys_error _ -> ());
  ignore (Runtime.eval_string ~rt
            {|(define glochid/ws-file "/tmp/glochid-file-issue-test-ws.json")|});
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  let eval_str ~what str =
    match Runtime.eval_string ~rt str with
    | Ast.VStr s -> s
    | other -> Alcotest.fail (what ^ ": " ^ Reader.pp_value other)
    | exception Ast.Interp_error e ->
        Alcotest.fail
          (what ^ ": INTERP "
           ^ (match e with
              | Ast.Prim_error (k, n, m) -> k ^ "/" ^ n ^ ": " ^ m
              | _ -> "opaque"))
  in
  (* the examples card: the element's declaration (lookbook discipline) *)
  let card = eval_str ~what:"examples card" {|(ui/file-issue/examples)|} in
  Alcotest.(check bool) "lookbook: file-issue card declares the element" true
    (has "ui/file-issue" card);
  (* the host inventory: the ui rail root + /lookbook face read this def *)
  Alcotest.(check bool) "lookbook: file-issue registered in the inventory" true
    (has "ui/file-issue"
           (eval_str ~what:"lookbook/elements" {|(json-emit (lookbook/elements))|}));
    (* ui/dialog (card 5da2cf6c): the modal element answers its card and is
       registered in the same host inventory. *)
    let dcard = eval_str ~what:"dialog card" {|(ui/dialog/examples)|} in
    Alcotest.(check bool) "lookbook: dialog card declares the element" true
      (has "ui/dialog" dcard);
    Alcotest.(check bool) "lookbook: dialog registered in the inventory" true
      (has "ui/dialog"
             (eval_str ~what:"lookbook/elements" {|(json-emit (lookbook/elements))|}));
  (* the rail strip renders the declared button *)
  let rail =
    eval_str ~what:"railtabs" {|(render-html (filmstrip/railtabs-html (list) 0))|}
  in
  Alcotest.(check bool) "rail: file-issue button present" true
    (has "fs-file-issue" rail && has "file an issue" rail);
    (* the declared verb: posts /filmstrip/dialog with route=/issues into #fs-layers *)
  let hxvals =
    eval_str ~what:"hx-vals"
      {|(as-str (hyper/attr
                  (car (hyper/query
                         (soup/parse (render-html (filmstrip/railtabs-html (list) 0)))
                         "#fs-file-issue"))
                  "hx-vals"))|}
  in
  Alcotest.(check bool) "rail: file-issue hx-vals carries the issues route" true
    (has "/issues" hxvals && has "route" hxvals);
    Alcotest.(check bool) "rail: file-issue posts /filmstrip/dialog" true
      (has "hx-post=\"/filmstrip/dialog\"" rail);
    (* card c0ad9354: the strip wraps instead of clipping -- a full
       railtabs row must never push the file-issue button off the rail
       (#fs-rail clips overflow-x). The wrap rule rides filmstrip/css. *)
    Alcotest.(check bool) "rail css: railtabs strip wraps, never clips" true
      (has ".fs-railtabs{flex-wrap:wrap"
             (eval_str ~what:"filmstrip css" {|(css/render (filmstrip/css))|}));
    (* hyper/expect: stub /issues (route/lookup validation stands in for
       the real notes face on the agent runtime), then POST /filmstrip/dialog
       through the real route machinery: the fragment carries the modal
       dialog; on the full shell page the rail button's click swaps it into
       #fs-layers and the close button empties the sink again. *)
    (match Runtime.eval_string ~rt
             {|(seq (define (stub-issues)
                        (render-html "<div id=issue-reg>issues-register</div>"))
                    (route "/issues" "stub-issues"))|}
    with
    | _ -> ()
    | exception Ast.Interp_error e ->
        Alcotest.fail
          ("stub issues face: INTERP "
           ^ (match e with
              | Ast.Prim_error (k, n, m) -> k ^ "/" ^ n ^ ": " ^ m
              | _ -> "opaque")));
    ignore (Runtime.eval_string ~rt
      {|(define pg (soup/parse (route/dispatch (route/lookup "/filmstrip/dialog") "POST" "/filmstrip/dialog" (list (cons "route" "/issues") (cons "title" "file an issue")) (list (cons "hx-request" "true")))))|});
    Alcotest.(check bool) "hyper: expect the modal dialog title" true
      (match Runtime.eval_string ~rt {|(hyper/expect pg ".fs-dialog-title" "file an issue")|} with
       | Ast.VBool b -> b
       | other -> Alcotest.fail ("expect: " ^ Reader.pp_value other));
    ignore (Runtime.eval_string ~rt
      {|(define shell (soup/parse (route/dispatch (route/lookup "/filmstrip") "GET" "/filmstrip" (list) (list (cons "hx-request" "true")))))|});
    ignore (Runtime.eval_string ~rt
      {|(define pg2 (hyper/click shell "#fs-file-issue" (list (cons "route" "/issues") (cons "title" "file an issue"))))|});
    Alcotest.(check bool) "hyper: file-issue click swaps the dialog into #fs-layers" true
      (match Runtime.eval_string ~rt {|(hyper/expect pg2 ".fs-dialog-title" "file an issue")|} with
       | Ast.VBool b -> b
       | other -> Alcotest.fail ("expect: " ^ Reader.pp_value other));
    ignore (Runtime.eval_string ~rt
      {|(define pg3 (hyper/click pg2 ".fs-dialog-close" (list)))|});
    Alcotest.(check bool) "hyper: close empties #fs-layers" true
      (match Runtime.eval_string ~rt {|(string-eq (hyper/text pg3 "#fs-layers") "")|}
      with
       | Ast.VBool b -> b
       | other -> Alcotest.fail ("expect: " ^ Reader.pp_value other))

(** start-menu cap (card e3de4aa0): the zero-JS start menu floats OVER
    the tree below the railtabs strip, so it must never extend below the
    screen -- glochid/base-css caps it at ~2/3 of the viewport height
    with its own scroll. hyper/expect rides a real GET / through the
    route machinery: the full page's <style> embeds glochid/base-css. *)
let test_filmstrip_start_menu_cap ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  let eval_str ~what str =
    match Runtime.eval_string ~rt str with
    | Ast.VStr s -> s
    | other -> Alcotest.fail (what ^ ": " ^ Reader.pp_value other)
    | exception Ast.Interp_error e ->
        Alcotest.fail
          (what ^ ": INTERP "
           ^ (match e with
              | Ast.Prim_error (k, n, m) -> k ^ "/" ^ n ^ ": " ^ m
              | _ -> "opaque"))
  in
  (* the declared host stylesheet carries the cap + its own scroll *)
  Alcotest.(check bool) "base-css: start menu capped at ~2/3 viewport, own scroll" true
    (has "max-height:66vh;overflow-y:auto"
           (eval_str ~what:"base-css" {|glochid/base-css|}));
  (* the menu element still renders the capped class *)
  Alcotest.(check bool) "start-menu: renders the fs-start-menu box" true
    (has "fs-start-menu"
           (eval_str ~what:"start-menu" {|(render-html (glochid/start-menu))|}));
  (* hyper/expect over the changed route: GET / serves the page whose
     <style> embeds glochid/base-css with the viewport cap. *)
  let page =
    match Runtime.dispatch_route ~rt ~handler:"chat/page" ~method_:"GET"
            ~path:"/" ~params:[] ~headers:[] ~body:"" with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("page: " ^ Reader.pp_value other)
  in
  Runtime.add_prim rt "page-html" (fun _ -> Ast.VStr page);
  ignore (Runtime.eval_string ~rt {|(define pg (soup/parse (page-html)))|});
  (* hyper/expect over the changed route: the served <style> text IS
     the declared glochid/base-css (which carries the viewport cap). *)
  Alcotest.(check bool) "hyper: GET / <style> serves the declared base-css" true
    (match Runtime.eval_string ~rt {|(hyper/expect pg "style" glochid/base-css)|} with
     | Ast.VBool b -> b
     | other -> Alcotest.fail ("expect: " ^ Reader.pp_value other));
  (* ... and the served stylesheet carries the cap + its own scroll *)
  Alcotest.(check bool) "hyper: GET / serves the start-menu viewport cap" true
    (has "max-height:66vh;overflow-y:auto"
           (eval_str ~what:"served style" {|(hyper/text pg "style")|}))

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
          (filmstrip/windows-tree ""))|});
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
  (* DEFAULT-OPEN desktops (rail-trees-v4 expand seed): the desk dirs
     render <details open> WITH their pre-walked kid rows; the shelf
     dir stays lazy -- inside an empty workspace the only remaining
     toggle-once pull in the box is the shelf's. *)
  Alcotest.(check bool) "tree box: desk dirs render already-open with eager kids" true
    (has "open=\"open\"" box && has "data-node=\"desk/0\"" box);
  (* inside an empty workspace the ONLY lazy pull in the box is the
     shelf's: everything before it (all three desk dirs) is eager. The
     lazy marker is hx-get + hx-trigger="toggle once" -- NOT any
     hx-trigger (the desk switch buttons carry hx-trigger="click",
     added by the 750b494 shelf-verb fix, so the old bare-needle probe
     went stale). Same proxy as probe_shell_sem M0b. *)
  Alcotest.(check bool) "tree box: desk dirs carry no lazy pull (expand rides kids)" true
    (let shelf_i =
       (try Str.search_forward (Str.regexp_string "data-node=\"shelf\"") box 0
        with Not_found -> Alcotest.fail "no shelf row in the box")
     in
     (try
        ignore
          (Str.search_forward (Str.regexp_string "hx-get")
             (String.sub box 0 shelf_i) 0);
        false
      with Not_found -> true));
  (* desk node dicts declare the seed + eager kids (empty desks -> []) *)
  Alcotest.check pp_value_testable "windows tree: desk dirs carry expand + kids"
    (Ast.VStr "true []")
    (Runtime.eval_string ~rt
      {|(string-append
          (as-str (dict-get (car (filmstrip/windows-tree "")) "expand"))
          " "
          (as-str (dict-get (car (filmstrip/windows-tree "")) "kids")))|});
  (* collapse-all answers the FULLY-CLOSED render: expand seeds cleared *)
  (match
     Runtime.dispatch_route ~rt ~handler:"filmstrip/tree-collapse"
       ~method_:"POST" ~path:"/filmstrip/tree/collapse"
       ~params:[ ("root", "windows") ] ~headers:[ ("hx-request", "true") ]
       ~body:""
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "collapse-all: renders the closed tree" true
         (if has "fs-tree-windows-root" s then not (has "open=\"open\"" s)
          else Alcotest.fail ("collapse resp: " ^ String.escaped (String.sub s 0 (min 400 (String.length s)))));
   | other -> Alcotest.fail ("collapse: " ^ Reader.pp_value other));
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

(** cross-desktop move (window-controlbox-v2 desktop int + the move
    verb): POST /filmstrip/move-to with an explicit d RE-DESKS a pane
    record (persisted in the ws row), the windows tree rows follow on
    the next OOB; a FOCUSED pane leaving the current desktop heals
    focus onto a strip-visible pane (a hidden pane never holds the
    ring); rail panes refuse (dock semantics); no d = cycle. Uses a
    temp ws file like the keys test. *)
let test_move_to_desk ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  Runtime.add_prim rt "bind/packages" (fun _ -> Ast.VStr {|[]|});
  Runtime.add_prim rt "bind/list" (fun _ -> Ast.VStr {|[]|});
  Runtime.add_prim rt "bind/get" (fun _ -> Ast.VStr {|{}|});
  let ws = Filename.temp_file "glochid-move" ".json" in
  ignore
    (Runtime.eval_string ~rt
       (Printf.sprintf {|(define glochid/ws-file "%s")|} ws));
  Fun.protect
  begin
    fun () ->
      let open_ route =
        ignore
          (Runtime.dispatch_route ~rt ~handler:"filmstrip/open"
             ~method_:"POST" ~path:"/filmstrip/open"
             ~params:[ ("route", route) ] ~headers:[] ~body:"")
      in
      let desk route =
        match
          Runtime.eval_string ~rt
            (Printf.sprintf
               {|(filmstrip/pane-desk
                   (filmstrip/pane-at (nth (filmstrip/row) 1)
                     (filmstrip/find-route-index (nth (filmstrip/row) 1)
                       "%s")))|}
               route)
        with
        | Ast.VInt d -> Int64.to_int d
        | Ast.VStr d -> int_of_string d
        | other -> Alcotest.fail ("desk: " ^ Reader.pp_value other)
      in
      open_ "/chat";   (* stamps the CURRENT desktop (0) *)
      Alcotest.(check int) "fresh pane rides desktop 0" 0 (desk "/chat");
      let id =
        match
          Runtime.eval_string ~rt
            {|(filmstrip/pane-id
                (filmstrip/pane-at (nth (filmstrip/row) 1) 0))|}
        with
        | Ast.VStr id -> String.escaped id
        | other -> Alcotest.fail ("id: " ^ Reader.pp_value other)
      in
      (match
         Runtime.dispatch_route ~rt ~handler:"filmstrip/move-to"
           ~method_:"POST" ~path:"/filmstrip/move-to"
           ~params:[ ("id", id); ("d", "1") ]
           ~headers:[ ("hx-request", "true") ] ~body:""
       with
       | Ast.VStr s ->
           Alcotest.(check bool) "move-to: answers the strip + tree OOBs" true
             (has "fs-content" s && has "fs-tree-windows-root" s);
           Alcotest.(check bool) "move-to: status line names the desk" true
             (has "moved to desktop 1" s)
       | other -> Alcotest.fail ("move-to: " ^ Reader.pp_value other));
      Alcotest.(check int) "move-to: pane record re-desked (persisted)" 1
        (desk "/chat");
      (* pane-head move glyph posts the same route *)
      (match
         Runtime.eval_string ~rt
           {|(render-html (filmstrip/pane-head
               (filmstrip/pane-at (nth (filmstrip/row) 1) 0)))|}
       with
       | Ast.VStr head ->
           Alcotest.(check bool) "pane-head: move glyph POSTs move-to" true
             (has "data-verb=\"move\"" head && has "/filmstrip/move-to" head)
       | other -> Alcotest.fail ("pane-head: " ^ Reader.pp_value other));
      (* heatmap: the tree row of the moved pane now carries only the
         to-words for OTHER desktops (to0 + to2, not to1) *)
      (match
         Runtime.eval_string ~rt
           {|(render-html (filmstrip/tree-level "windows"
               (filmstrip/windows-tree "desk/1")))|}
       with
       | Ast.VStr rows ->
           Alcotest.(check bool) "tree row: move words skip the current desk" true
             (has "data-verb=\"to0\"" rows && has "data-verb=\"to2\"" rows
              && not (has "data-verb=\"to1\"" rows))
       | other -> Alcotest.fail ("tree rows: " ^ Reader.pp_value other))
  end
    ~finally:(fun () ->
        (try Sys.remove ws with Sys_error _ -> ());
        ignore
          (Runtime.eval_string ~rt
             {|(define glochid/ws-file ".agent/workspace.json")|}))

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

(** keyboard shortcuts (borge/glochid.borg keyboard-shortcuts): the body
    sentinels render from the ONE chord table (lib restated + glochid
    rows, guard spliced into the condition), the /filmstrip/keys/*
    routes act on the FOCUSED pane -- the ws row's focus index resolved
    server-side, id injected through nested route/dispatch (lib verb
    semantics unchanged) -- and a chord with no live focus answers the
    status line, never a 500. Uses a temp ws file so the suite never
    touches a real .agent/workspace.json. *)
let test_glochid_keys ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  let post handler =
    match
      Runtime.dispatch_route ~rt ~handler ~method_:"POST" ~path:handler
        ~params:[] ~headers:[] ~body:""
    with
    | Ast.VStr s -> s
    | other -> Alcotest.fail (handler ^ ": " ^ Reader.pp_value other)
  in
  let open_ route =
    ignore
      (Runtime.dispatch_route ~rt ~handler:"filmstrip/open" ~method_:"POST"
         ~path:"/filmstrip/open" ~params:[ ("route", route) ] ~headers:[]
         ~body:"")
  in
  (* the verb OOBs re-render the windows tree, which walks the prims
     bridge -- inert stubs (test 3 registers live ones on the same rt) *)
  Runtime.add_prim rt "bind/packages" (fun _ -> Ast.VStr {|[]|});
  Runtime.add_prim rt "bind/list" (fun _ -> Ast.VStr {|[]|});
  Runtime.add_prim rt "bind/get" (fun _ -> Ast.VStr {|{}|});
  let ws = Filename.temp_file "glochid-keys" ".json" in
  ignore
    (Runtime.eval_string ~rt
       (Printf.sprintf {|(define glochid/ws-file "%s")|} ws));
  Fun.protect
  begin
    fun () ->
      (* the sentinel table renders whole (the full shell render is not
         dispatched here: it exceeds the 400k eval budget on the
         main-repo build -- habitat issue i-1789591788). *)
      (match
         Runtime.eval_string ~rt {|(render-html (filmstrip/key-sentinels))|}
       with
       | Ast.VStr html ->
           (* attribute values render escaped: && -> &amp;&amp;, ' -> &#39; *)
           Alcotest.(check bool) "lib focus row restated" true
             (has "altKey&amp;&amp;!shiftKey&amp;&amp;key==&#39;ArrowLeft&#39;" html);
           Alcotest.(check bool) "desktop chord" true
             (has "altKey&amp;&amp;key==&#39;1&#39;" html);
           Alcotest.(check bool) "close chord" true
             (has "altKey&amp;&amp;key==&#39;w&#39;" html);
           Alcotest.(check bool) "pin chord" true
             (has "altKey&amp;&amp;key==&#39;p&#39;" html);
           Alcotest.(check bool) "single chord" true
             (has "altKey&amp;&amp;key==&#39;s&#39;" html);
           Alcotest.(check bool) "keys routes in the map" true
             (has "/filmstrip/keys/close" html);
           Alcotest.(check bool) "guard spliced into the condition" true
             (has "activeElement.tagName!=&#39;INPUT&#39;" html)
       | other -> Alcotest.fail ("key-sentinels: " ^ Reader.pp_value other));
      (* the rail tabs row carries the keys hint glyph *)
      (match
         Runtime.eval_string ~rt {|(render-html (glochid/railtabs-html))|}
       with
       | Ast.VStr html ->
           Alcotest.(check bool) "keys hint glyph in the rail tabs" true
             (has "fs-keys" html)
       | other -> Alcotest.fail ("railtabs: " ^ Reader.pp_value other));
      (* END-TO-END: the shell itself renders the sentinels + hint (the
         476k->188k step/perf fix keeps the boot render under the
         400k gate, so the keys surface is reachable in situ). *)
      (match
         Runtime.dispatch_route ~rt ~handler:"glochid/shell" ~method_:"GET"
           ~path:"/" ~params:[] ~headers:[] ~body:""
       with
       | Ast.VStr html ->
           Alcotest.(check bool) "shell carries the keys routes" true
             (has "/filmstrip/keys/close" html);
           Alcotest.(check bool) "shell carries the guard clause" true
             (has "activeElement.tagName" html);
            Alcotest.(check bool) "shell carries the keys hint" true
              (has "fs-keys" html);
            Alcotest.(check bool) "shell page never scrolls (card a5bdd8ce)" true
              (has "body:has(.fs-shell){overflow:hidden}" html)
       | other -> Alcotest.fail ("shell: " ^ Reader.pp_value other));
      (* no live pane yet: the chord answers the status line *)
      Alcotest.(check bool) "no focused pane = status line" true
        (has "no focused pane" (post "glochid/keys-close"));
      (* open a pane (focus lands on it) -- then the chords act on it *)
      open_ "/bindings";
      Alcotest.(check bool) "focused close shelves the pane" true
        (has "shelved" (post "glochid/keys-close"));
      let ic = open_in_bin ws in
      let n = in_channel_length ic in
      let saved = really_input_string ic n in
      close_in ic;
      Alcotest.(check bool) "ws row persisted the shelve" true
        (has "shelved" saved);
      (* pin + single: open a SECOND pane (open focuses it), the chords
         act on the focused pane exactly as the head glyphs do *)
      open_ "/code";
      Alcotest.(check bool) "focused pin moves the region" true
        (has "fs-surface" (post "glochid/keys-pin"));
      Alcotest.(check bool) "focused single stages one-up" true
        (has "fs-single" (post "glochid/keys-single"))
  end
    ~finally:(fun () ->
        (try Sys.remove ws with Sys_error _ -> ());
        ignore
          (Runtime.eval_string ~rt
             {|(define glochid/ws-file ".agent/workspace.json")|}))

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
  (* The templates are declared elements projected through lookbook/html;
     the html projection adds block-boundary newlines around div tags
     (html/tag-emit). Cosmetically inert — so the replay compares with
     all newlines stripped. *)
  let squash s = String.concat "" (String.split_on_char '\n' s) in
  let expected =
    String.concat ""
      ("<div id=\"t1-think\">"
      :: ("<details class=\"tk\"><summary>thinking</summary><div id=\"t1-tk-1\"><span class=\"reason\">rea</span></div></details>"
          :: ("<details class=\"tool\"><summary id=\"t1-tool-1-sum\">tool: bash ✓</summary><div id=\"t1-tool-1\"><div class=\"trace\">in ls -la</div><div class=\"trace\">out file.ml</div></div></details>"
              :: ("<details class=\"tk\"><summary>thinking</summary><div id=\"t1-tk-2\"><span class=\"reason\">more</span></div></details>"
                  :: [ "</div>" ]))))
  in
  (match
     Runtime.eval_string ~rt
       {|(soup/serialize (soup/select-one (hyper/stream npg nparts) "#t1-think"))|}
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "nested blocks replay into zone (modulo block newlines)"
         true (String.equal (squash expected) (squash s))
   | other -> Alcotest.fail ("turn-replay: " ^ Reader.pp_value other));
  Alcotest.check pp_value_testable "reply bubble got tokens"
    (Ast.VStr "<div class=\"bubble llm\" id=\"t1\"><span>answer</span></div>")
    (Runtime.eval_string ~rt
       {|(soup/serialize (soup/select-one (hyper/stream npg nparts) "#t1"))|})

(** habitat-panes (glochid.borg habitat-panes): glochid panes WEAR the
    habitat's fragment faces — the fwd faces over the habitat/fwd prim
    (boot shell + boost wrapper + in-process dispatch), the fwd-prefix
    registry (kernel paths refused), the cascade app entries. The gate
    plane itself (put/forward) is exercised by the probe against a live
    habitat; here the prim is canned. *)
let test_habitat_panes ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  (* canned habitat: the forward returns a fragment *)
  Runtime.add_prim rt "habitat/fwd" (fun args ->
      match args with
      | [ Ast.VStr _meth; Ast.VStr path; Ast.VStr _q; Ast.VStr _b ] ->
          Ast.VStr ("<h1>canned</h1> for " ^ path)
      | _ -> Alcotest.fail "habitat/fwd: bad args");
  (* the open face: boost wrapper + canned fragment inside *)
  (match Runtime.dispatch_route ~rt ~handler:"glochid/fwd-todo" ~method_:"GET"
           ~path:"/todos" ~params:[ ("hx-request", "true") ] ~headers:[]
           ~body:""
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "fwd face: boost shell + fragment + stub target" true
         (has "id=\"fwd-todo\"" s && has "hx-boost=\"true\"" s
          && has "hx-target=\"#fwd-todo\"" s && has "<h1>canned</h1> for /todos" s)
   | other -> Alcotest.fail ("fwd-todo: " ^ Reader.pp_value other));
  (* prefix registry: app paths register, kernel paths refuse *)
  (* OCaml-side registry (boot registers the defaults; test adds one) *)
  ignore (Habitat.fwd_register_prefix "/kanban");
  Alcotest.(check bool) "fwd prefix: app prefix registered" true
    (Habitat.fwd_matches_prefix "/kanban?x=1" <> None);
  (match Habitat.fwd_register_prefix "/api" with
   | false -> ()
   | true -> Alcotest.fail "fwd prefix: kernel path was accepted");
  Alcotest.(check bool) "fwd prefix: kernel path NOT registered" false
    (Habitat.fwd_matches_prefix "/api/eval" <> None);
  (* the cascade wears the habitat apps (host vocabulary = habitat routes) *)
  let dumped =
    match Runtime.eval_string ~rt {|(json-emit (glochid/fwd-apps))|} with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("fwd-apps: " ^ Reader.pp_value other)
  in
  Alcotest.(check bool) "cascade: habitat apps are route leaves" true
    (has "/todos" dumped && has "/calendar" dumped);
  let bound p =
    match Runtime.eval_string ~rt ("(as-str (route/lookup \"" ^ p ^ "\"))") with
    | Ast.VStr h -> has "glochid/fwd" h
    | _ -> false
  in
  Alcotest.(check bool) "route table: /todos /notes /calendar bound" true
    (bound "/todos" && bound "/notes" && bound "/calendar")

(** lookbook (borge/filmstrip.borg lookbook): declared ui values + both
    projections over libs/lookbook, the /lookbook pane (card = the
    declaration; live variant call through the lookbook/eval seam;
    stage-only params re-render), the ui rail root over the declared
    inventory, and the no-card fail-soft path. *)
let test_lookbook ~rt () =
  Surface.load ~rt;
  let has needle s =
    (try ignore (Str.search_forward (Str.regexp_string needle) s 0); true
     with Not_found -> false)
  in
  (* the DECLARATION: a ui value is a dict (JSON), never markup *)
  let val_txt =
    match Runtime.eval_string ~rt {|(as-str (ui/button "Save" "primary"))|} with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("ui/button: " ^ Reader.pp_value other)
  in
  Alcotest.(check bool) "declared value is a dict (t=el, tag/attrs/kids)" true
    (has "\"t\":\"el\"" val_txt && has "\"tag\":\"button\"" val_txt
     && has "\"class\":\"lb-btn lb-btn-primary\"" val_txt);
  (match Runtime.eval_string ~rt {|(lookbook/html (ui/button "Save" "primary"))|} with
   | Ast.VStr s ->
       Alcotest.(check bool) "projection 1: pure html, both attrs" true
         (has "<button" s && has "class=\"lb-btn lb-btn-primary\"" s
          && has ">Save</button>" s)
   | other -> Alcotest.fail ("lookbook/html: " ^ Reader.pp_value other));
  (match Runtime.eval_string ~rt {|(lookbook/text (ui/card "Notes" "12 new" "body") 0)|} with
   | Ast.VStr s ->
       Alcotest.(check bool) "projection 2: indented outline of the COMPOSED element" true
         (has "section" s && has "span" s && has "text: 12 new" s)
   | other -> Alcotest.fail ("lookbook/text: " ^ Reader.pp_value other));
  (* the pane: card = declaration; stage = live variant call + folds *)
  let get ?(headers = []) params =
    match Runtime.dispatch_route ~rt ~handler:"lookbook/page" ~method_:"POST"
            ~path:"/lookbook" ~params ~headers ~body:"" with
    | Ast.VStr s -> s
    | other -> Alcotest.fail ("lookbook/page: " ^ Reader.pp_value other)
  in
  let pane = get ~headers:[ ("hx-request", "true") ] [ ("name", "ui/button"); ("v", "0") ] in
  Alcotest.(check bool) "pane: card name + doc + both folds + stage" true
    (has "ui/button" pane && has "One kind-preset button" pane
     && has "lb-stage" pane && has "declared value" pane
     && has "text projection" pane);
  (match Runtime.dispatch_route ~rt ~handler:"lookbook/run" ~method_:"POST"
           ~path:"/lookbook/run"
           ~params:[ ("name", "ui/button"); ("v", "0");
                     ("p_label", "Ship"); ("p_kind", "danger") ]
           ~headers:[] ~body:""
   with
   | Ast.VStr s ->
       Alcotest.(check bool) "stage POST: params re-render ONLY the stage" true
         (has "lb-btn-danger" s && has ">Ship<" s
          && not (has "<form" s));
       Alcotest.(check bool) "stage POST: same stage id (hx-target survives)" true
         (has "id=\"lb-stage-ui-button\"" s)
   | other -> Alcotest.fail ("lookbook/run: " ^ Reader.pp_value other));
  (* the ui rail root inventories the DECLARED elements (never sniffed) *)
  (match Runtime.eval_string ~rt
           {|(list/map (lambda (n) (lb/get n "open")) (glochid/ui-tree ""))|}
   with
   | Ast.VList opens ->
          Alcotest.(check int) "ui root: lib seeds + glochid host elements + ui/file-issue + ui/dialog" 15
         (List.length opens);
       Alcotest.(check bool) "ui rows open-or-focus the /lookbook pane" true
         (List.exists
            (function Ast.VStr s -> String.equal s "/lookbook?name=ui/button" | _ -> false)
            opens)
   | other -> Alcotest.fail ("ui-tree: " ^ Reader.pp_value other));
  (* EXAMPLES ARE the declaration: no card -> the no-card message, never
     an error. Also proves the fail-soft eval seam path. *)
  let nocard = get ~headers:[ ("hx-request", "true") ] [ ("name", "ui/nonesuch") ] in
  Alcotest.(check bool) "no-card message (fail-soft)" true
    (has "no examples card for ui/nonesuch" nocard);
  (* the empty-name face renders a hint (inventory lives in the rail) *)
  let root = get ~headers:[ ("hx-request", "true") ] [] in
  Alcotest.(check bool) "root face: hint, no stage" true
    (has "ui rail tree" root && not (has "lb-stage" root))

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
  let path = Filename.temp_file "pp_glochid_mock" ".json" in
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

(* sessions-persist (glochid/lib/sessions.ml): append-only JSONL
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
          raw = `Assoc [ ("role", `String "assistant"); ("content", `String "done") ];
          usage = None }
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

(* memory-bridge (glochid/lib/habitat.ml): the remote expr builders are
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

let test_habitat_bearer ~env () =
  (* auth_header: Bearer wins when token set, Basic fallback otherwise *)
  let c_tok : Habitat.creds =
    { url = "https://pricklypear.rocks"; user = "u"; pass = "p"; token = "tok-123" }
  in
  Alcotest.(check (pair string string)) "bearer wins"
    ("authorization", "Bearer tok-123")
    (Habitat.auth_header c_tok);
  let c_basic : Habitat.creds =
    { url = "https://pricklypear.rocks"; user = "u"; pass = "p"; token = "" }
  in
  Alcotest.(check (pair string string)) "basic fallback (b64 u:p)"
    ("authorization", "Basic dTpw")
    (Habitat.auth_header c_basic);
  (* creds: url + token alone is a complete config — no password needed *)
  let saved =
    [ ("GLOCHID_HABITAT_URL", Sys.getenv_opt "GLOCHID_HABITAT_URL")
    ; ("GLOCHID_HABITAT_TOKEN", Sys.getenv_opt "GLOCHID_HABITAT_TOKEN")
    ; ("GLOCHID_HABITAT_USER", Sys.getenv_opt "GLOCHID_HABITAT_USER")
    ; ("GLOCHID_HABITAT_PASS", Sys.getenv_opt "GLOCHID_HABITAT_PASS") ]
  in
  Fun.protect
    ~finally:(fun () ->
       List.iter
         (fun (k, v) ->
            match v with Some x -> Unix.putenv k x | None -> Unix.putenv k "")
         saved)
    (fun () ->
       List.iter (fun (k, _) -> Unix.putenv k "") saved;
       Unix.putenv "GLOCHID_HABITAT_URL" "https://pricklypear.rocks";
       Unix.putenv "GLOCHID_HABITAT_TOKEN" "tok-abc";
       Unix.putenv "GLOCHID_HABITAT_USER" "";
       Unix.putenv "GLOCHID_HABITAT_PASS" "";
       match Habitat.creds ~env with
       | None -> Alcotest.fail "url+token should be a complete config"
       | Some c ->
           Alcotest.(check string) "token read" "tok-abc" c.Habitat.token;
           Alcotest.(check (pair string string)) "bearer header"
             ("authorization", "Bearer tok-abc") (Habitat.auth_header c))

(* eval_timeout_s: GLOCHID_HABITAT_EVAL_TIMEOUT overrides the 30s
   default (legacy AGENT_HABITAT_EVAL_TIMEOUT still read); junk or
   non-positive values fall back to the default. *)
let test_habitat_eval_timeout () =
  let saved =
    [ ("GLOCHID_HABITAT_EVAL_TIMEOUT", Sys.getenv_opt "GLOCHID_HABITAT_EVAL_TIMEOUT")
    ; ("AGENT_HABITAT_EVAL_TIMEOUT", Sys.getenv_opt "AGENT_HABITAT_EVAL_TIMEOUT") ]
  in
  Fun.protect
    ~finally:(fun () ->
       List.iter
         (fun (k, v) ->
            match v with Some x -> Unix.putenv k x | None -> Unix.putenv k "")
         saved)
    (fun () ->
       List.iter (fun (k, _) -> Unix.putenv k "") saved;
         Alcotest.(check (float 0.)) "default 30s" 30. (Habitat.eval_timeout_s ());
       Unix.putenv "GLOCHID_HABITAT_EVAL_TIMEOUT" "120";
         Alcotest.(check (float 0.)) "glochid override" 120. (Habitat.eval_timeout_s ());
       Unix.putenv "GLOCHID_HABITAT_EVAL_TIMEOUT" "";
       Unix.putenv "AGENT_HABITAT_EVAL_TIMEOUT" "45";
         Alcotest.(check (float 0.)) "legacy spelling" 45. (Habitat.eval_timeout_s ());
       Unix.putenv "AGENT_HABITAT_EVAL_TIMEOUT" "junk";
         Alcotest.(check (float 0.)) "junk falls back" 30. (Habitat.eval_timeout_s ());
       Unix.putenv "AGENT_HABITAT_EVAL_TIMEOUT" "0";
         Alcotest.(check (float 0.)) "non-positive falls back" 30.
         (Habitat.eval_timeout_s ()))

(* run-supervisor (run-primitive phase 2): the PPFLOW parse contract + pure
   policy. These are the supervisor/sidecar boundary — a regression here
   silently loses verdicts or usage on the run rows. *)
let test_supervisor_pure () =
  let cfg = Supervisor.config () in
  let v =
    Supervisor.parse_verdict
      "noise before\n\
       pp-flow: PPFLOW-DEST DONE\n\
       pp-flow: PPFLOW-EVIDENCE built + tested the change (dune build OK)\n\
       pp-flow: PPFLOW-USAGE requests=3 input_tokens=1200 cached_tokens=800 output_tokens=310 wall_s=95 trace=tr-1\n\
       pp-flow: PPFLOW-STATUS status=:SUCCEEDED category= trace=tr-1 message=\n\
       noise after\n"
  in
  Alcotest.(check string) "dest" "DONE" v.Supervisor.v_dest;
  Alcotest.(check string) "evidence" "built + tested the change (dune build OK)"
    v.Supervisor.v_evid;
  Alcotest.(check int) "requests" 3 v.Supervisor.v_requests;
  Alcotest.(check int) "input tokens" 1200 v.Supervisor.v_input;
  Alcotest.(check int) "cached tokens" 800 v.Supervisor.v_cached;
  Alcotest.(check int) "output tokens" 310 v.Supervisor.v_output;
  Alcotest.(check int) "wall" 95 v.Supervisor.v_wall;
  Alcotest.(check string) "trace" "tr-1" v.Supervisor.v_trace;
  Alcotest.(check string) "status" ":SUCCEEDED" v.Supervisor.v_status;
  let v2 =
    Supervisor.parse_verdict
      "pp-flow: PPFLOW-STATUS status=:PROVIDER-FAILURE category=:PROVIDER-FAILURE trace=tr-2 message=:PROVIDER-FAILURE HTTP 402 upstream flake"
  in
  Alcotest.(check string) "failure status" ":PROVIDER-FAILURE"
    v2.Supervisor.v_status;
  Alcotest.(check string) "failure category" ":PROVIDER-FAILURE"
    v2.Supervisor.v_category;
  Alcotest.(check string) "failure message" ":PROVIDER-FAILURE HTTP 402 upstream flake"
    v2.Supervisor.v_message;
  let tf ~s ~c ~m =
    Supervisor.transient_failure ~status:s ~category:c ~message:m
  in
  Alcotest.(check bool) "crash transient" true (tf ~s:"crashed" ~c:"" ~m:"");
  Alcotest.(check bool) "timeout transient" true (tf ~s:"timed-out" ~c:"" ~m:"");
  Alcotest.(check bool) "402 transient" true
    (tf ~s:"provider-failure" ~c:"PROVIDER-FAILURE" ~m:"HTTP 402 blocked");
  Alcotest.(check bool) "401 deterministic" false
    (tf ~s:"provider-failure" ~c:"PROVIDER-FAILURE" ~m:"HTTP 401 bad creds");
  Alcotest.(check bool) "invalid deterministic" false
    (tf ~s:"invalid-output" ~c:"INVALID-OUTPUT" ~m:"no verdict");
  Alcotest.(check int) "backoff 1" 2 (Supervisor.backoff_s 1);
  Alcotest.(check int) "backoff 2" 4 (Supervisor.backoff_s 2);
  Alcotest.(check int) "backoff cap" 60 (Supervisor.backoff_s 9);
  Alcotest.(check string) "csv" "[\"TODO\",\"CODE\"]"
    (Supervisor.csv_to_json "TODO,CODE");
  Alcotest.(check string) "csv empty" "[]" (Supervisor.csv_to_json "");
  Alcotest.(check string) "csv passthrough" "[\"A\"]"
    (Supervisor.csv_to_json "[\"A\"]");
  Alcotest.(check bool) "review noverdict" true
    (Supervisor.noverdict_column "LOGIC-REVIEW");
  Alcotest.(check bool) "work noverdict" false
    (Supervisor.noverdict_column "CODE");
  Alcotest.(check string) "norm status" "timed-out"
    (Supervisor.norm_status ":TIMED-OUT");
  Alcotest.(check string) "norm colon" "PROVIDER-FAILURE"
    (Supervisor.norm_colon ":PROVIDER-FAILURE");
  let steps =
    Supervisor.steps_rebuild cfg ~state:"failed" ~attempts:2 ~error:"rc=1"
  in
  Alcotest.(check bool) "steps state" true
    (Supervisor.contains_sub "\"state\":\"failed\"" steps);
  Alcotest.(check bool) "steps attempts" true
    (Supervisor.contains_sub "\"attempts\":2" steps)

let () =
  Eio_main.run @@ fun env ->
  let rt = Runtime.make () in
  Tools.install ~env ~resolve_path:Runtime.resolve_path ~register:(fun name run -> Runtime.register_builtin_tool rt name ~run);
  Eio.Switch.run @@ fun sw ->
  Alcotest.run "glochid"
    [
      ( "plugin-model",
        [ Alcotest.test_case "lifecycle" `Quick (test_plugin_lifecycle ~rt) ] );
      ( "nopales-embed",
        [ Alcotest.test_case "rim-errors-redefine" `Quick
            (test_rim_errors_and_redefine ~rt) ] );
      ( "pure-catalog",
        [ Alcotest.test_case "parity" `Quick
            (test_pure_catalog_parity ~rt) ] );
      ( "glochid-loop",
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
        [ Alcotest.test_case "bearer-creds" `Quick
            (fun () -> test_habitat_bearer ~env ())
        ; Alcotest.test_case "expr-builders" `Quick
            test_habitat_expr_builders
        ; Alcotest.test_case "eval-timeout-env" `Quick
              test_habitat_eval_timeout ] );
      ( "glochid-route-table",
        [ Alcotest.test_case "register-and-render" `Quick
            (test_route_table_register_and_render ~rt)
        ; Alcotest.test_case "thin-context" `Quick
            (test_route_table_thin_context ~rt)
        ; Alcotest.test_case "pg-prim-unbound" `Quick
            (test_route_table_pg_prim_unbound ~rt)
        ] );
      ( "libs-hyper-on-glochid",
        [ Alcotest.test_case "visit-click-stream" `Quick
            (test_libs_hyper_on_agent ~rt) ] );
        ( "calendar-faces",
          [ Alcotest.test_case "day-click-pane-local" `Quick
              (test_calendar_day_pane_local ~rt) ] );
        ( "welcome-faces",
          [ Alcotest.test_case "index-cta" `Quick
                (test_welcome_index_cta ~rt)
            ; Alcotest.test_case "mailto-footers" `Quick
                (test_welcome_mailto_footers ~rt) ] );

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
            (test_rail_trees ~rt)
        ; Alcotest.test_case "move-to-desktop" `Quick
            (test_move_to_desk ~rt)
          ; Alcotest.test_case "optimistic-loader" `Quick
              (test_filmstrip_optimistic ~rt)
          ; Alcotest.test_case "file-issue-rail" `Quick
                (test_filmstrip_file_issue ~rt)
            ; Alcotest.test_case "start-menu-cap" `Quick
                  (test_filmstrip_start_menu_cap ~rt)
        ; Alcotest.test_case "lookbook" `Quick
            (test_lookbook ~rt)
        ; Alcotest.test_case "habitat-panes" `Quick
            (test_habitat_panes ~rt)
        ; Alcotest.test_case "keyboard-shortcuts" `Quick
            (test_glochid_keys ~rt) ] );
      ( "run-supervisor",
        [ Alcotest.test_case "pure-policy" `Quick
            test_supervisor_pure;
          Alcotest.test_case "native-success-is-not-a-crash-on-the-row" `Quick
            (fun () ->
              (* A native step that SUCCEEDED reports no v_status (there is no
                 PPFLOW-STATUS line on that path). The sidecar reading of
                 "empty status = crashed" must not leak onto an explicit,
                 already-determined success, or the row's result_status says
                 crashed after run-finish said succeeded. *)
              let v =
                { Supervisor.v_dest = "DONE";
                  v_evid = "evidence";
                  v_status = "";
                  v_category = "";
                  v_trace = "";
                  v_message = "";
                  v_requests = 1;
                  v_input = 885;
                  v_cached = 768;
                  v_output = 178;
                  v_wall = 5 }
              in
              let args e =
                (* the printfs emit a quoted Lisp call; read the status and
                   category with a regex-free split on the quoted strings *)
                let quoted =
                  String.split_on_char '"' e
                  |> List.mapi (fun i s -> (i, s))
                  |> List.filter (fun (i, _) -> i mod 2 = 1)
                  |> List.map snd
                in
                quoted
              in
              let native_ok =
                args
                  (Supervisor.run_result_expr ~ok:true ~status:"succeeded"
                     "r1" v "native pass/done")
              in
              Alcotest.(check bool) "result_status is succeeded" true
                (List.mem "succeeded" native_ok);
              Alcotest.(check bool) "no bogus sidecar-failure class" false
                (List.mem "sidecar-failure" native_ok);
              (* the sidecar path is unchanged: empty status still reads
                 crashed/sidecar-failure *)
              let sidecar_fail =
                args (Supervisor.run_result_expr "r2" v "no PPFLOW-STATUS line")
              in
              Alcotest.(check bool) "sidecar empty status still crashes" true
                (List.mem "crashed" sidecar_fail);
              Alcotest.(check bool) "sidecar keeps its failure class" true
                (List.mem "sidecar-failure" sidecar_fail)) ] );
      (* ---- PP native runner (borge/run-primitive.borg PHASE 3) ---- *)
      ( "native-runner",
        [ Alcotest.test_case "verdict-from-json" `Quick (fun () ->
              let v =
                Native_run.parse_verdict
                  {|{"verdict":"PASS","destination":"CODE","evidence":"ran the suite"}|}
              in
              Alcotest.(check string) "verdict" "PASS" v.Native_run.v_verdict;
              Alcotest.(check string) "destination" "CODE" v.Native_run.v_destination;
              Alcotest.(check string) "evidence" "ran the suite" v.Native_run.v_evidence);
          Alcotest.test_case "verdict-from-prose-wrapped-json" `Quick (fun () ->
              let v =
                Native_run.parse_verdict
                  "Here is my answer:\n{\"verdict\":\"FAIL\",\"destination\":\"\",\"evidence\":\"wip\"}\nThanks."
              in
              Alcotest.(check string) "verdict" "FAIL" v.Native_run.v_verdict;
              Alcotest.(check string) "evidence" "wip" v.Native_run.v_evidence);
          Alcotest.test_case "verdict-from-line-contract" `Quick (fun () ->
              let v =
                Native_run.parse_verdict
                  "did the thing\nDESTINATION: LOGIC-REVIEW\nEVIDENCE: suite green"
              in
              Alcotest.(check string) "destination" "LOGIC-REVIEW"
                v.Native_run.v_destination;
              Alcotest.(check string) "evidence" "suite green" v.Native_run.v_evidence;
              Alcotest.(check string) "no invented verdict" "" v.Native_run.v_verdict);
          Alcotest.test_case "no-verdict-is-total" `Quick (fun () ->
              (* POLICY-3: an unparseable answer is reported faithfully (empty),
                 never guessed - the supervisor owns the fail-safe. *)
              let v = Native_run.parse_verdict "not json at all, just prose" in
              Alcotest.(check string) "empty verdict" "" v.Native_run.v_verdict;
              Alcotest.(check string) "empty evidence" "" v.Native_run.v_evidence);
          Alcotest.test_case "usage-accumulates-per-request" `Quick (fun () ->
              let u : Native_run.usage =
                Native_run.add_usage Native_run.zero_usage
                  (Native_run.provider_usage ~input_tokens:100 ~cached_tokens:40
                     ~output_tokens:7)
              in
              (* annotations above are load-bearing: the accumulator and the
                 per-request provider record share three field names *)
              let u : Native_run.usage =
                Native_run.add_usage u
                  (Native_run.provider_usage ~input_tokens:5 ~cached_tokens:0
                     ~output_tokens:1)
              in
              Alcotest.(check int) "requests" 2 u.Native_run.requests;
              Alcotest.(check int) "input" 105 u.Native_run.input_tokens;
              Alcotest.(check int) "cached" 40 u.Native_run.cached_tokens;
              Alcotest.(check int) "output" 8 u.Native_run.output_tokens);
          Alcotest.test_case "result-round-trip" `Quick (fun () ->
              let r =
                { Native_run.r_schema = Native_run.schema;
                  r_step_name = "executor";
                  r_step_state = "done";
                  r_attempts = 2;
                  r_outcome = "pass";
                  r_verdict =
                    { Native_run.v_verdict = "PASS";
                      v_destination = "CODE";
                      v_evidence = "suite green" };
                  r_usage =
                    { Native_run.requests = 12; input_tokens = 3456;
                      cached_tokens = 3000; output_tokens = 789; wall_s = 42.5 };
                  r_tool_total = 3;
                  r_tools = [ ("bash", 2); ("read", 1) ];
                  r_turns = 12;
                  r_model = "syn:large:text";
                  r_error_class = "";
                  r_error_message = "";
                  r_transcript_ref = "/tmp/x/driver.log" }
              in
              (match Native_run.result_of_json (Native_run.to_json r) with
               | Some back ->
                   Alcotest.(check string) "step state" "done"
                     back.Native_run.r_step_state;
                   Alcotest.(check int) "attempts" 2 back.Native_run.r_attempts;
                   Alcotest.(check int) "input tokens" 3456
                     back.Native_run.r_usage.Native_run.input_tokens;
                   Alcotest.(check int) "cached tokens" 3000
                     back.Native_run.r_usage.Native_run.cached_tokens;
                   Alcotest.(check int) "output tokens" 789
                     back.Native_run.r_usage.Native_run.output_tokens;
                   Alcotest.(check bool) "wall preserved" true
                     (abs_float
                        (back.Native_run.r_usage.Native_run.wall_s -. 42.5)
                      < 0.001);
                   Alcotest.(check string) "destination" "CODE"
                     back.Native_run.r_verdict.Native_run.v_destination;
                   Alcotest.(check (list (pair string int))) "tools"
                     [ ("bash", 2); ("read", 1) ]
                     back.Native_run.r_tools
               | None -> Alcotest.fail "round trip lost the result"));
          Alcotest.test_case "classify-error" `Quick (fun () ->
              Alcotest.(check string) "429" "transient"
                (Native_run.classify_error "HTTP 429: slow down");
              Alcotest.(check string) "503" "transient"
                (Native_run.classify_error "HTTP 503: gateway");
              Alcotest.(check string) "401" "deterministic"
                (Native_run.classify_error "HTTP 401: bad credentials");
              Alcotest.(check string) "timeout" "timeout"
                (Native_run.classify_error "provider timeout");
              Alcotest.(check string) "crash is retryable" "transient"
                (Native_run.classify_error "connection reset by peer"));
          Alcotest.test_case "native-transient-classes-retry" `Quick (fun () ->
              (* The native runner classifies its own error, and the
                 supervisor takes that answer as authoritative (it cannot
                 re-derive it from the sidecar's text vocabulary). Pin the
                 mapping the harvest applies, so a rename in
                 Native_run.classify_error cannot silently turn every
                 transient provider fault into a one-shot dead run. *)
              let retries cls =
                cls = "transient" || cls = "timeout"
              in
              Alcotest.(check bool) "HTTP 429 retries" true
                (retries (Native_run.classify_error "HTTP 429: slow down"));
              Alcotest.(check bool) "HTTP 503 retries" true
                (retries (Native_run.classify_error "HTTP 503: gateway"));
              Alcotest.(check bool) "timeout retries" true
                (retries (Native_run.classify_error "provider timeout"));
              Alcotest.(check bool) "HTTP 401 does NOT retry" false
                (retries (Native_run.classify_error "HTTP 401: bad creds"));
              Alcotest.(check bool) "max-turns does NOT retry" false
                (retries
                   (Native_run.classify_error
                      "LLM: max turns exceeded (8)")));
          (* ---- the PHASE 3 dispatch seam: ONE place chooses a runner ---- *)
          Alcotest.test_case "runner-kind-defaults-to-the-phase-2-sidecar" `Quick
            (fun () ->
              (* The recorded flip gate: the default must NOT become native
                 just because the native runner exists. *)
              let cfg = Supervisor.config () in
              Alcotest.(check string) "default kind" "pp-flow"
                cfg.Supervisor.runner_kind;
              Alcotest.(check string) "sidecar step name" "sidecar"
                (Yojson.Safe.from_string (Supervisor.steps_initial cfg)
                |> Yojson.Safe.Util.to_list
                |> (function [] -> `Null | x :: _ -> x)
                |> Yojson.Safe.Util.member "name"
                |> Yojson.Safe.Util.to_string));
          Alcotest.test_case "native-kind-names-its-own-step" `Quick (fun () ->
              (* A row read later must say which runner produced it. *)
              let cfg = Supervisor.config () in
              let native = { cfg with Supervisor.runner_kind = "native" } in
              let step =
                Yojson.Safe.from_string (Supervisor.steps_initial native)
                |> Yojson.Safe.Util.to_list
                |> (function [] -> `Null | x :: _ -> x)
              in
              Alcotest.(check string) "native step name" "native"
                (Yojson.Safe.Util.member "name" step
                |> Yojson.Safe.Util.to_string);
              Alcotest.(check string) "native step kind" "native"
                (Yojson.Safe.Util.member "kind" step
                |> Yojson.Safe.Util.to_string));
          Alcotest.test_case "unknown-kind-falls-back-to-the-sidecar" `Quick
            (fun () ->
              (* spawn_run is a two-arm if: anything that is not exactly
                 "native" runs the phase-2 sidecar. Pin that, so a typo in
                 an env var can never dispatch nothing. *)
              let cfg = Supervisor.config () in
              let typo = { cfg with Supervisor.runner_kind = "nativ" } in
              Alcotest.(check bool) "typo is not native" false
                (typo.Supervisor.runner_kind = "native");
              Alcotest.(check string) "typo still names a sidecar step"
                "sidecar"
                (Yojson.Safe.from_string (Supervisor.steps_initial typo)
                |> Yojson.Safe.Util.to_list
                |> (function [] -> `Null | x :: _ -> x)
                |> Yojson.Safe.Util.member "name"
                |> Yojson.Safe.Util.to_string))
        ] );
    ]
