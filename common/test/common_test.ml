(** Tests for the Pricklypear common library — pure, no services. *)
open Pricklypear_common

let branch_tests = [
  ("branch: main is detected", `Quick, fun () ->
    let b = Branch.make
      ~id:"b1" ~parent_id:None ~owner:"system" ~name:"main"
      ~created_at:"2026-07-05T00:00:00Z" () in
    Alcotest.(check bool) "is_main" true (Branch.is_main b);
    Alcotest.(check string) "qualified" "main" (Branch.qualified_name b));
  ("branch: user branch qualified name", `Quick, fun () ->
    let b = Branch.make
      ~id:"b2" ~parent_id:(Some "b1") ~owner:"alice" ~name:"main"
      ~created_at:"2026-07-05T00:00:00Z" () in
    Alcotest.(check bool) "not main" false (Branch.is_main b);
    Alcotest.(check string) "qualified" "alice/main" (Branch.qualified_name b));
  ("branch: JSON round-trip", `Quick, fun () ->
    let b = Branch.make
      ~id:"b3" ~parent_id:(Some "b1") ~owner:"alice" ~name:"experiment"
      ~created_at:"2026-07-05T01:00:00Z" ~last_synced:(Some "2026-07-05T02:00:00Z") () in
    let j = Branch.to_json b in
    (match Branch.of_json j with
     | Ok b' ->
        Alcotest.(check string) "id" b.id b'.id;
        Alcotest.(check string) "owner" "alice" b'.owner;
        Alcotest.(check string) "name" "experiment" b'.name;
        Alcotest.(check string) "last_synced" "2026-07-05T02:00:00Z"
                                  (match b'.last_synced with Some s -> s | None -> "")
     | Error e -> Alcotest.failf "round-trip failed: %s" e));
]

let manifest_tests = [
  ("manifest: JSON round-trip with default config", `Quick, fun () ->
    let m = Manifest.make
      ~user_id:"u1" ~username:"alice" ~branch_id:"b2"
      ~branch_name:"alice/main" ~boot_mode:Manifest.Boot_on_connect
      ~image_version:"v1" () in
    let j = Manifest.to_json m in
    (match Manifest.of_json j with
     | Ok m' ->
        Alcotest.(check string) "user_id" "u1" m'.user_id;
        Alcotest.(check string) "username" "alice" m'.username;
        Alcotest.(check string) "branch_name" "alice/main" m'.branch_name;
        Alcotest.(check int) "step_budget" 100_000 m'.config.step_budget_limit
     | Error e -> Alcotest.failf "round-trip failed: %s" e));
  ("manifest: boot_mode parse", `Quick, fun () ->
    Alcotest.(check bool) "on-connect parses"
      true (Manifest.Boot_on_connect =
              (match Manifest.boot_mode_of_string "on-connect" with
               | Some m -> m | None -> Manifest.Boot_always_on)));
]

let auth_types_tests = [
  ("auth: token string round-trip", `Quick, fun () ->
    let t = { Auth_types.opaque_id = "AAAA"; signature = "BBBB" } in
    let s = Auth_types.token_to_string t in
    (match Auth_types.token_of_string s with
     | Ok t' ->
        Alcotest.(check string) "opaque" "AAAA" t'.opaque_id;
        Alcotest.(check string) "sig" "BBBB" t'.signature
     | Error e -> Alcotest.failf "token parse failed: %s" e));
  ("auth: malformed token rejected", `Quick, fun () ->
    (match Auth_types.token_of_string "noseparator" with
     | Ok _ -> Alcotest.fail "should have failed"
     | Error _ -> ());
    Alcotest.(check bool) "rejected" true true);
  ("auth: verify_ssh_key_resp round-trip Ok", `Quick, fun () ->
    let r = Auth_types.Ok "u1" in
    let j = Auth_types.verify_ssh_key_resp_to_json r in
    (match Auth_types.verify_ssh_key_resp_of_json j with
     | Ok (Auth_types.Ok uid) -> Alcotest.(check string) "uid" "u1" uid
     | Ok Auth_types.Denied -> Alcotest.fail "expected Ok"
     | Error e -> Alcotest.failf "parse failed: %s" e));
]

let protocol_tests = [
  ("protocol: health_response round-trip ok", `Quick, fun () ->
    let h = { Protocol.ok = true; branch = "alice/main";
              uptime_seconds = 42; error = None } in
    let j = Protocol.health_response_to_json h in
    (match Protocol.health_response_of_json j with
     | Ok h' ->
        Alcotest.(check bool) "ok" true h'.ok;
        Alcotest.(check string) "branch" "alice/main" h'.branch;
        Alcotest.(check int) "uptime" 42 h'.uptime_seconds
     | Error e -> Alcotest.failf "parse failed: %s" e));
  ("protocol: health_status conversions", `Quick, fun () ->
    Alcotest.(check string) "healthy" "healthy"
      (Protocol.health_status_to_string Protocol.H_healthy);
    Alcotest.(check bool) "round-trip" true
      (Protocol.H_unhealthy = Protocol.health_status_of_string "unhealthy"));
]

let llm_codec_tests = [
  ("llm-codec: tool_def_to_json shape", `Quick, fun () ->
    let j = Llm_codec.tool_def_to_json
      { name = "read"; description = "read a file";
        parameters = `Assoc [ ("type", `String "object") ] }
    in
    let open Yojson.Safe.Util in
    Alcotest.(check string) "type" "function" (j |> member "type" |> to_string);
    Alcotest.(check string) "name" "read"
      (j |> member "function" |> member "name" |> to_string));
  ("llm-codec: parse_tool_calls_from_msg API field", `Quick, fun () ->
    let msg = Yojson.Safe.from_string
      {|{"tool_calls":[{"id":"call_1","function":{"name":"bash","arguments":"{\"command\":\"echo hi\"}"}}]}|}
    in
    let tcs = Llm_codec.parse_tool_calls_from_msg msg in
    Alcotest.(check int) "one call" 1 (List.length tcs);
    (match tcs with
     | [tc] ->
        Alcotest.(check string) "id" "call_1" tc.Llm_codec.id;
        Alcotest.(check string) "name" "bash" tc.Llm_codec.name;
        Alcotest.(check string) "args" "{\"command\":\"echo hi\"}" tc.Llm_codec.arguments
     | _ -> Alcotest.fail "expected one tool call"));
  ("llm-codec: parse_tool_calls_from_msg empty", `Quick, fun () ->
    let msg = `Assoc [ ("content", `String "hello") ] in
    Alcotest.(check int) "none" 0
      (List.length (Llm_codec.parse_tool_calls_from_msg msg)));
  ("llm-codec: parse_xml_tool_calls fallback", `Quick, fun () ->
    (* Build the XML-ish tool-call block by concatenation so this
       source contains no literal tool-call tag boundaries. *)
    let block =
      ("<" ^ "tool_call>") ^ " install_function "
      ^ ("<" ^ "arg_key>name" ^ "</" ^ "arg_key>")
      ^ ("<" ^ "arg_value>foo" ^ "</" ^ "arg_value>")
      ^ ("<" ^ "arg_key>body" ^ "</" ^ "arg_key>")
      ^ ("<" ^ "arg_value>(lambda () 1)" ^ "</" ^ "arg_value>")
      ^ ("</" ^ "tool_call>")
    in
    let tcs = Llm_codec.parse_xml_tool_calls block in
    Alcotest.(check int) "one xml call" 1 (List.length tcs);
    (match tcs with
     | [tc] ->
        Alcotest.(check string) "name" "install_function" tc.Llm_codec.name;
        (match Yojson.Safe.from_string tc.Llm_codec.arguments with
         | `Assoc kvs ->
            (match List.assoc_opt "name" kvs, List.assoc_opt "body" kvs with
             | Some (`String n), Some (`String b) ->
                Alcotest.(check string) "arg name" "foo" n;
                Alcotest.(check string) "arg body" "(lambda () 1)" b
             | _ -> Alcotest.fail "missing args")
         | _ -> Alcotest.fail "arguments not an object")
     | _ -> Alcotest.fail "expected one xml tool call"));
  ("llm-codec: parse_turn_response Final", `Quick, fun () ->
    let body =
      {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"hello there"}}]}|}
    in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Final { content; _ } ->
        Alcotest.(check string) "content" "hello there" content
     | other -> Alcotest.fail ("expected Final, got other")));
  ("llm-codec: parse_turn_response Need_tools via API field", `Quick, fun () ->
    let body =
      {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"t9","function":{"name":"read","arguments":"{\"path\":\"a.txt\"}"}}]}}]}|}
    in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Need_tools { tool_calls; _ } ->
        Alcotest.(check int) "one tool" 1 (List.length tool_calls);
        (match tool_calls with
         | [tc] -> Alcotest.(check string) "name" "read" tc.Llm_codec.name
         | _ -> Alcotest.fail "expected one")
     | other -> Alcotest.fail "expected Need_tools, got other"));
  ("llm-codec: parse_turn_response Need_tools via XML fallback", `Quick, fun () ->
    let xml_block =
      ("<" ^ "tool_call>") ^ " bash "
      ^ ("<" ^ "arg_key>command" ^ "</" ^ "arg_key>")
      ^ ("<" ^ "arg_value>echo x" ^ "</" ^ "arg_value>")
      ^ ("</" ^ "tool_call>")
    in
    let body =
      Printf.sprintf
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":%s}}]}|}
        (Yojson.Safe.to_string (`String xml_block))
    in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Need_tools { tool_calls; _ } ->
        Alcotest.(check int) "one xml tool" 1 (List.length tool_calls);
        (match tool_calls with
         | [tc] -> Alcotest.(check string) "name" "bash" tc.Llm_codec.name
         | _ -> Alcotest.fail "expected one")
     | other -> Alcotest.fail "expected Need_tools via XML"));
  ("llm-codec: parse_turn_response reasoning_content fallback", `Quick, fun () ->
    let body =
      {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":null,"reasoning_content":"the real answer"}}]}|}
    in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Final { content; _ } ->
        Alcotest.(check string) "content" "the real answer" content
     | other -> Alcotest.fail "expected Final from reasoning_content"));
  ("llm-codec: extract_stream_delta", `Quick, fun () ->
    let chunk = {|{"choices":[{"delta":{"content":"tok"}}]}|} in
    Alcotest.(check string) "delta" "tok"
      (Option.value (Llm_codec.extract_stream_delta chunk) ~default:"");
    Alcotest.(check bool) "done is none" true
      (Option.is_none (Llm_codec.extract_stream_delta "[DONE]")));
  ("llm-codec: extract_content picks complete-json content", `Quick, fun () ->
    let body =
      {|{"choices":[{"finish_reason":"stop","message":{"content":"{\"x\":1}","reasoning_content":"essay"}}]}|}
    in
    (match Llm_codec.extract_content body with
     | Ok c -> Alcotest.(check string) "complete json wins" "{\"x\":1}" c
     | Error e -> Alcotest.fail e));
]

let () =
  Alcotest.run "pricklypear-common" [
    "branch", branch_tests;
    "manifest", manifest_tests;
    "auth-types", auth_types_tests;
    "protocol", protocol_tests;
    "llm-codec", llm_codec_tests;
  ]
