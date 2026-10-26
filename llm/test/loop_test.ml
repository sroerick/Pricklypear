(** Tests for the interpreter-free default host over the shared agent turn
    loop (borge/glochid.borg agent-loop). No Nopales anywhere: the provider
    is a canned script and the only tools are the builtin read/bash bodies.
    Covers the three behaviours the loop owns: one tool call then final text
    with exactly one follow-up model call, the max-turns budget reporting
    instead of spinning, and a tool_call hook blocking a call with the
    reason reaching the model as the tool result. *)

let cfg : Provider.cfg =
  { base_url = "http://127.0.0.1:1"; api_key = ""; model = "mock"; system = "" }

let reply ?(text = "") ?(calls = []) ?(stop_reason = "stop") () :
    Provider.reply =
  {
    Provider.text;
    calls;
    stop_reason;
    raw = `Assoc [ ("role", `String "assistant"); ("content", `String text) ];
    usage = None;
  }

(* Substring search (keeps the suite free of a str dependency). *)
let contains haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  let rec go i =
    i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
  in
  nl = 0 || go 0

let bash_call id command : Provider.tool_call =
  { id; name = "bash"; input = `Assoc [ ("command", `String command) ] }

let test_one_follow_up_call ~env ~sw () =
  let host = Default_host.create ~env () in
  let calls = ref 0 in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages:_ ~tools:_ ~on_delta:_ =
    incr calls;
    if !calls = 1 then
      Ok
        (reply ~stop_reason:"tool_use"
           ~calls:[ bash_call "t1" "echo hello" ] ())
    else Ok (reply ~text:"done" ())
  in
  let history = ref [] in
  (match
     Loop.run ~env ~sw ~host:(Default_host.host host) ~cfg ~complete
       ~on_event:(fun _ -> ()) history "say hello using echo"
   with
  | Error e -> Alcotest.fail e
  | Ok () -> ());
  Alcotest.(check int) "exactly one follow-up model call" 2 !calls;
  Alcotest.(check int) "history: user, assistant, tool, assistant" 4
    (List.length !history);
  match List.nth !history 2 with
  | {
   Provider.role = "tool";
   content = `String out;
   tool_call_id = Some "t1";
   _;
  } ->
      Alcotest.(check bool) "bash output is the tool result" true
        (contains out "hello")
  | m -> Alcotest.fail ("bad tool message: " ^ m.Provider.role)

let test_max_turns_reports ~env ~sw () =
  let host = Default_host.create ~env () in
  let calls = ref 0 in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages:_ ~tools:_ ~on_delta:_ =
    incr calls;
    Ok (reply ~stop_reason:"tool_use" ~calls:[ bash_call "t1" "echo loop" ] ())
  in
  let history = ref [] in
  match
    Loop.run ~env ~sw ~host:(Default_host.host host) ~cfg ~complete
      ~max_turns:2 ~on_event:(fun _ -> ()) history "loop forever"
  with
  | Ok () -> Alcotest.fail "the loop must stop at the turn budget"
  | Error e ->
      Alcotest.(check bool) "reports the budget" true
        (contains (String.lowercase_ascii e) "max turns exceeded");
      Alcotest.(check int) "two model calls then stop" 2 !calls

let test_tool_call_hook_blocks ~env ~sw () =
  let host = Default_host.create ~env () in
  Default_host.on host "tool_call" (fun payload ->
      let command =
        match Yojson.Safe.Util.member "input" payload with
        | `Assoc kvs -> (
            match List.assoc_opt "command" kvs with
            | Some (`String c) -> c
            | _ -> "")
        | _ -> ""
      in
      if contains command "rm -rf" then
        `Assoc [ ("block", `Bool true); ("reason", `String "nope") ]
      else `Assoc []);
  let saw_block = ref false in
  let complete ~env:_ ~sw:_ ~cfg:_ ~messages ~tools:_ ~on_delta:_ =
    if not !saw_block then begin
      saw_block :=
        List.exists
          (fun (m : Provider.message) ->
            match m.content with
            | `String s -> contains s "blocked: nope"
            | _ -> false)
          messages;
      Ok (reply ~stop_reason:"tool_use" ~calls:[ bash_call "t1" "rm -rf /" ] ())
    end
    else Ok (reply ~text:"ok" ())
  in
  let history = ref [] in
  (match
     Loop.run ~env ~sw ~host:(Default_host.host host) ~cfg ~complete
       ~on_event:(fun _ -> ()) history "delete everything"
   with
  | Error e -> Alcotest.fail e
  | Ok () -> ());
  Alcotest.(check bool) "block reason reached the model" true !saw_block;
  match List.nth !history 2 with
  | { Provider.role = "tool"; content = `String out; _ } ->
      Alcotest.(check bool) "tool result carries the block" true
        (contains out "blocked: nope")
  | _ -> Alcotest.fail "missing tool message"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Alcotest.run "llm-loop"
    [
      ( "agent-loop",
        [
          Alcotest.test_case "one-tool-call-then-text" `Quick
            (test_one_follow_up_call ~env ~sw);
          Alcotest.test_case "max-turns-reports" `Quick
            (test_max_turns_reports ~env ~sw);
          Alcotest.test_case "tool-call-hook-blocks" `Quick
            (test_tool_call_hook_blocks ~env ~sw);
        ] );
    ]