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
  ("manifest: JSON round-trip preserves delegated actor", `Quick, fun () ->
    let m = Manifest.make
      ~user_id:"u1" ~username:"admin" ~branch_id:"b2"
      ~branch_name:"admin/personal" ~boot_mode:Manifest.Boot_on_connect
      ~image_version:"v1" ~actor:(Some "gregor") () in
    let j = Manifest.to_json m in
    (match Manifest.of_json j with
     | Ok m' ->
        Alcotest.(check bool) "actor survives round-trip" true
          (m'.actor = Some "gregor")
     | Error e -> Alcotest.failf "round-trip failed: %s" e));
  ("manifest: loads manifest without actor field as self (back-compat)", `Quick, fun () ->
    let m = Manifest.make
      ~user_id:"u1" ~username:"alice" ~branch_id:"b2"
      ~branch_name:"alice/main" ~boot_mode:Manifest.Boot_on_connect
      ~image_version:"v1" () in
    (* strip any actor key: a payload from a pre-delegation image *)
    let stripped = match Manifest.to_json m with
      | `Assoc kv -> `Assoc (List.filter (fun (k, _) -> k <> "actor") kv)
      | j -> j in
    (match Manifest.of_json stripped with
     | Ok m' -> Alcotest.(check bool) "no actor = self" true (m'.actor = None)
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
              uptime_seconds = 42; boot_epoch = 1756900000;
              open_fds = 37; fd_limit = 1024;
              blocked_backends = 0; idle_in_tx = 0; error = None } in
    let j = Protocol.health_response_to_json h in
    (match Protocol.health_response_of_json j with
     | Ok h' ->
        Alcotest.(check bool) "ok" true h'.ok;
        Alcotest.(check string) "branch" "alice/main" h'.branch;
        Alcotest.(check int) "uptime" 42 h'.uptime_seconds;
        Alcotest.(check int) "boot_epoch" 1756900000 h'.boot_epoch;
        Alcotest.(check int) "open_fds" 37 h'.open_fds;
        Alcotest.(check int) "fd_limit" 1024 h'.fd_limit;
        Alcotest.(check int) "blocked_backends" 0 h'.blocked_backends;
        Alcotest.(check int) "idle_in_tx" 0 h'.idle_in_tx
     | Error e -> Alcotest.failf "parse failed: %s" e));
  ("protocol: health_response contention fields round-trip", `Quick, fun () ->
    let h = { Protocol.ok = false; branch = "system/main";
              uptime_seconds = 42; boot_epoch = 1756900000;
              open_fds = 37; fd_limit = 1024;
              blocked_backends = 3; idle_in_tx = 1;
              error = Some "lock pileup: 3 backend(s) blocked" } in
    let j = Protocol.health_response_to_json h in
    (match Protocol.health_response_of_json j with
     | Ok h' ->
        Alcotest.(check bool) "ok" false h'.ok;
        Alcotest.(check int) "blocked_backends" 3 h'.blocked_backends;
        Alcotest.(check int) "idle_in_tx" 1 h'.idle_in_tx;
        Alcotest.(check (option string)) "error"
          (Some "lock pileup: 3 backend(s) blocked") h'.error
     | Error e -> Alcotest.failf "parse failed: %s" e));
  ("protocol: health_response of_json tolerates missing boot_epoch (old image)",
   `Quick, fun () ->
    let j : Yojson.Safe.t = `Assoc [ ("ok", `Bool true); ("branch", `String "system/main");
                                     ("uptime_seconds", `Int 7); ("error", `Null) ] in
    (match Protocol.health_response_of_json j with
     | Ok h' ->
        Alcotest.(check int) "boot_epoch defaults to 0" 0 h'.boot_epoch;
        Alcotest.(check int) "open_fds defaults to 0" 0 h'.open_fds;
        Alcotest.(check int) "fd_limit defaults to 0" 0 h'.fd_limit;
        Alcotest.(check int) "blocked_backends defaults to 0" 0 h'.blocked_backends;
        Alcotest.(check int) "idle_in_tx defaults to 0" 0 h'.idle_in_tx
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
  ("llm-codec: parse_turn_response surfaces provider error body", `Quick, fun () ->
    (* The wedge shape: an OpenAI error object has NO choices. The old
       path died as Type_error("Expected array, got null") — the
       undiagnosable string the wake loop retried forever. *)
    let body =
      {|{"error":{"message":"Invalid API key provided","type":"invalid_request_error","code":"invalid_api_key"}}|}
    in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Turn_error e ->
        Alcotest.(check string) "provider message surfaced"
          "provider error: Invalid API key provided (invalid_request_error)" e
     | other -> Alcotest.fail ("expected Turn_error, got other")));
  ("llm-codec: parse_turn_response null choices without error body", `Quick, fun () ->
    let body = {|{"id":"x","object":"chat.completion","choices":null}|} in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Turn_error e ->
        Alcotest.(check string) "no-choices shape" "LLM: no choices in response" e
     | other -> Alcotest.fail "expected Turn_error, got other"));
  ("llm-codec: extract_content surfaces provider error body", `Quick, fun () ->
    let body = {|{"error":{"message":"rate limited"}}|} in
    (match Llm_codec.extract_content body with
     | Error e -> Alcotest.(check string) "extract error" "provider error: rate limited" e
     | Ok _ -> Alcotest.fail "expected Error"));
  ("llm-codec: parse_turn_response error with bare string error", `Quick, fun () ->
    let body = {|{"error":"upstream timeout"}|} in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Turn_error e ->
        Alcotest.(check string) "bare string error" "provider error: upstream timeout" e
     | other -> Alcotest.fail "expected Turn_error, got other"));
  ("llm-codec: parse_turn_response empty choices still empty-error", `Quick, fun () ->
    let body = {|{"choices":[]}|} in
    (match Llm_codec.parse_turn_response body with
     | Llm_codec.Turn_error e ->
        Alcotest.(check string) "empty choices" "LLM: empty choices" e
     | other -> Alcotest.fail "expected Turn_error, got other"));
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
  ("llm-codec: sse_data_payload parses data lines", `Quick, fun () ->
    Alcotest.(check (option string)) "spaced" (Some "{}")
      (Llm_codec.sse_data_payload "data: {}");
    Alcotest.(check (option string)) "unspaced" (Some "x")
      (Llm_codec.sse_data_payload "data:x");
    Alcotest.(check (option string)) "done marker" (Some "[DONE]")
      (Llm_codec.sse_data_payload "data: [DONE]");
    Alcotest.(check (option string)) "empty payload" None
      (Llm_codec.sse_data_payload "data:   ");
    Alcotest.(check (option string)) "comment line" None
      (Llm_codec.sse_data_payload ": keepalive"));
  ("llm-codec: reassemble_stream rebuilds text+reasoning+tool fragments",
   `Quick, fun () ->
    let deltas = ref [] and reasons = ref [] in
    let lines =
      [ {|{"choices":[{"delta":{"reasoning_content":"thinking "},"finish_reason":null}]}|};
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
      Llm_codec.reassemble_stream
        ~on_text:(fun s -> deltas := !deltas @ [ s ])
        ~on_reasoning:(fun s -> reasons := !reasons @ [ s ])
        lines
    in
    Alcotest.(check string) "concatenated text" "hello  world" r.text;
    Alcotest.(check string) "concatenated reasoning" "thinking rea" r.reasoning;
    Alcotest.(check (list string)) "text deltas in stream order"
      [ "hello "; " world" ] !deltas;
    Alcotest.(check (list string)) "reasoning deltas incl. plain-reasoning alias"
      [ "thinking "; "rea" ] !reasons;
    Alcotest.(check string) "finish captured" "tool_calls" r.finish;
    Alcotest.(check int) "one reassembled tool call" 1 (List.length r.tool_calls);
    (match r.tool_calls with
     | [ tc ] ->
        Alcotest.(check string) "tool id" "t1" tc.Llm_codec.id;
        Alcotest.(check string) "tool name" "bash" tc.Llm_codec.name;
        Alcotest.(check string) "arguments reassembled"
          {|{"command":"echo hello"}|} tc.Llm_codec.arguments
     | _ -> Alcotest.fail "expected one tool call");
    (* The synthetic response parses through the shared response parser
       and keeps the reassembled tool call + text. *)
    (match Llm_codec.parse_turn_response_json r.response with
     | Llm_codec.Need_tools { content = Some "hello  world"; tool_calls = [ tc ]; _ } ->
        Alcotest.(check string) "parsed arguments"
          {|{"command":"echo hello"}|} tc.Llm_codec.arguments
     | other ->
        Alcotest.fail
          ("expected Need_tools with content, got raw: "
           ^ Yojson.Safe.to_string (match other with
              | Llm_codec.Need_tools { raw; _ } -> raw
              | Llm_codec.Final { raw; _ } -> raw
              | Llm_codec.Turn_error _ -> `String "turn_error"))));
  ("llm-codec: reassemble_stream skips malformed lines", `Quick, fun () ->
    let r = Llm_codec.reassemble_stream [ "not json"; "{}" ] in
    Alcotest.(check string) "no text from garbage" "" r.text;
    Alcotest.(check string) "default finish" "stop" r.finish;
    Alcotest.(check int) "no tool calls" 0 (List.length r.tool_calls));
  ( "llm-codec: a null usage detail does not eat a content chunk",
    `Quick,
    fun () ->
      (* REGRESSION (2026-10-18): Synthetic sends usage with
         "prompt_tokens_details": null AND omits it entirely on the first
         chunk. [member "cached_tokens"] on a null RAISES, and the
         per-chunk guard used to swallow that - so the chunk's CONTENT was
         discarded and every streamed reply came back empty
         ("LLM: empty content (finish=stop)"), which broke --once and the
         whole phase-3 native runner. The usage read must be total. *)
      let lines =
        [ {|{"choices":[{"index":0,"delta":{"role":"assistant","content":""},"finish_reason":null}],"usage":{"prompt_tokens":858,"completion_tokens":0}}|};
          {|{"choices":[{"index":0,"delta":{"content":"HELLO"},"finish_reason":"stop"}],"usage":{"prompt_tokens":858,"completion_tokens":5}}|};
          {|{"choices":[],"usage":{"prompt_tokens":858,"completion_tokens":5,"prompt_tokens_details":null}}|} ]
      in
      let r = Llm_codec.reassemble_stream lines in
      Alcotest.(check string) "content survived" "HELLO" r.text;
      Alcotest.(check string) "finish" "stop" r.finish;
      (match r.usage with
       | Some u ->
           Alcotest.(check int) "input" 858 u.Llm_codec.input_tokens;
           Alcotest.(check int) "cached defaults to 0, not a raise" 0
             u.Llm_codec.cached_tokens
       | None -> Alcotest.fail "usage dropped"));
  ( "llm-codec: usage tolerates a missing details object entirely",
    `Quick,
    fun () ->
      let u =
        Llm_codec.usage_of_response_json
          (Yojson.Safe.from_string
             {|{"usage":{"prompt_tokens":7,"completion_tokens":3}}|})
      in
      match u with
      | Some u ->
          Alcotest.(check int) "input" 7 u.Llm_codec.input_tokens;
          Alcotest.(check int) "cached" 0 u.Llm_codec.cached_tokens
      | None -> Alcotest.fail "expected usage");
  (* ---- provider-structured usage (borge/run-primitive.borg
     budget-accounting; the phase-3 seam) ---- *)
  ("llm-codec: usage_of_response_json reads the provider counts", `Quick, fun () ->
    let body =
      Yojson.Safe.from_string
        {|{"choices":[{"message":{"role":"assistant","content":"hi"}}],
           "usage":{"prompt_tokens":1200,"completion_tokens":340,
                    "prompt_tokens_details":{"cached_tokens":900}}}|}
    in
    (match Llm_codec.usage_of_response_json body with
     | Some u ->
         Alcotest.(check int) "input" 1200 u.Llm_codec.input_tokens;
         Alcotest.(check int) "cached" 900 u.Llm_codec.cached_tokens;
         Alcotest.(check int) "output" 340 u.Llm_codec.output_tokens
     | None -> Alcotest.fail "expected a usage object"));
  ("llm-codec: no usage object reports None, not zeros", `Quick, fun () ->
    Alcotest.(check bool) "absent" true
      (Llm_codec.usage_of_response_json
         (Yojson.Safe.from_string {|{"choices":[]}|})
       = None));
  ("llm-codec: a reassembled stream keeps the final usage chunk", `Quick, fun () ->
    (* stream_options.include_usage puts usage in the LAST chunk, which carries
       NO choices at all - the shape that used to be dropped on the floor. *)
    let lines =
      [ {|{"choices":[{"index":0,"delta":{"content":"ok"},"finish_reason":null}]}|};
        {|{"choices":[],"usage":{"prompt_tokens":10,"completion_tokens":2,
             "prompt_tokens_details":{"cached_tokens":5}}}|} ]
    in
    let r = Llm_codec.reassemble_stream lines in
    (match r.Llm_codec.usage with
     | Some u ->
         Alcotest.(check int) "stream input" 10 u.Llm_codec.input_tokens;
         Alcotest.(check int) "stream cached" 5 u.Llm_codec.cached_tokens;
         Alcotest.(check int) "stream output" 2 u.Llm_codec.output_tokens
     | None -> Alcotest.fail "stream usage dropped");
    (* the synthetic response must carry it too: that is how parse_reply (and
       every other host parser) reads usage with no special case *)
    (match Llm_codec.usage_of_response_json r.Llm_codec.response with
     | Some u -> Alcotest.(check int) "response input" 10 u.Llm_codec.input_tokens
     | None -> Alcotest.fail "synthesized response lost usage"));
]

let () =
  Alcotest.run "pricklypear-common" [
    "branch", branch_tests;
    "manifest", manifest_tests;
    "auth-types", auth_types_tests;
    "protocol", protocol_tests;
    "llm-codec", llm_codec_tests;
  ]
