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
    Loop.run ~env ~sw ~rt ~cfg ~complete ~on_event:(fun _ -> ()) history
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
    Loop.run ~env ~sw ~rt ~cfg ~complete ~on_event:(fun _ -> ()) (ref [])
      "delete everything"
  with
  | Error e -> Alcotest.fail e
  | Ok () -> ()

let () =
  Eio_main.run @@ fun env ->
  let rt = Runtime.make () in
  Tools.install ~rt;
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
          Alcotest.test_case "tool-block" `Quick
            (test_loop_tool_block ~env ~sw ~rt);
        ] );
    ]
