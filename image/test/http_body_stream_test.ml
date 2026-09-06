(* Tests for the two shared HTTP plumbing helpers factored out of the
   duplicated per-handler copies (i-1788671558-f):

     - Http_core.parse_body: the ONE "JSON object -> fields / parsed
       non-object -> defaults / not JSON -> form fields" decision that
       api_eval, api_prim, api_agent, api_agent_task, repl_llm and
       ai_propose used to hand-roll six times (Body_other must NOT be
       conflated with Body_form: a JSON array like ["prompt=x"] must
       never be sniffed as a urlencoded form).
     - Http_agent.stream_events: the ONE queue-mutex-condition drain
       rig behind api_agent's SSE progress stream and repl_llm's
       multipart/mixed token stream (was ~70 lines duplicated twice,
       differing only in the frame writer).

   No DB, no live server. *)

open Pricklypear_image_lib

let shape_t : Http_core.body_shape Alcotest.testable =
  let pp fmt = function
    | Http_core.Body_fields fs ->
        Format.fprintf fmt "Body_fields(%d)" (List.length fs)
    | Http_core.Body_other -> Format.pp_print_string fmt "Body_other"
    | Http_core.Body_form -> Format.pp_print_string fmt "Body_form"
  in
  let eq a b =
    match (a, b) with
    | Http_core.Body_fields a, Http_core.Body_fields b ->
        List.length a = List.length b
    | Body_other, Body_other | Body_form, Body_form -> true
    | _ -> false
  in
  Alcotest.testable pp eq

let test_parse_body () =
  let module H = Http_core in
  (* object -> fields *)
  Alcotest.check shape_t "json object" (H.Body_fields [ ("expr", `String "(+ 1 2)") ])
    (H.parse_body {|{"expr":"(+ 1 2)"}|});
  (* bare JSON string / array / number parse but have NO fields *)
  Alcotest.check shape_t "bare string" H.Body_other (H.parse_body "\"hi\"");
  Alcotest.check shape_t "array" H.Body_other (H.parse_body {|["prompt=x"]|});
  Alcotest.check shape_t "number" H.Body_other (H.parse_body "42");
  (* not JSON -> form *)
  Alcotest.check shape_t "form body" H.Body_form (H.parse_body "prompt=x&q=y");
  Alcotest.check shape_t "empty body" H.Body_form (H.parse_body "");
  Alcotest.check shape_t "garbage" H.Body_form (H.parse_body "{{{")
  [@@warning "-26-27"]

let test_json_field_str () =
  let module H = Http_core in
  let fields = [ ("prompt", `String "hello"); ("n", `Int 5) ] in
  Alcotest.(check string) "present" "hello" (H.json_field_str fields "prompt");
  Alcotest.(check string) "missing -> default" "" (H.json_field_str fields "nope");
  Alcotest.(check string) "missing -> given default" "d"
    (H.json_field_str fields "nope" ~default:"d");
  (* non-string values fall back; they stay at call sites via assoc_opt *)
  Alcotest.(check string) "non-string -> default" "" (H.json_field_str fields "n")

(* stream_events: drive the rig with pure-OCaml frame writers (no
   Httpun) and observe drain order / peer-gone stop / close contract. *)

let test_stream_drains_in_order () =
  let written = ref [] in
  let closes = ref [] in
  let produce push finish =
    push "a";
    push "b";
    push "c";
    finish ()
  in
  Http_agent.stream_events
    ~write_frame:(fun f -> written := f :: !written; `Ok)
    ~close_stream:(fun c -> closes := c :: !closes)
    ~produce;
  Alcotest.(check (list string)) "all frames in order"
    [ "a"; "b"; "c" ] (List.rev !written);
  Alcotest.check Alcotest.int "exactly one clean close" 1 (List.length !closes);
  Alcotest.check Alcotest.int "close reason is Finish"
    0 (match !closes with [ `Finish ] -> 0 | _ -> 1)

let test_stream_peer_gone_stops_drain () =
  let written = ref [] in
  let closes = ref [] in
  let first = ref true in
  let produce push finish =
    push "a";
    push "b";
    push "c";
    finish ()
  in
  Http_agent.stream_events
    ~write_frame:(fun f ->
      if !first then begin
        first := false;
        written := f :: !written;
        `Ok
      end
      else `Peer_gone)
    ~close_stream:(fun c -> closes := c :: !closes)
    ~produce;
  (* drain stops at the first `Peer_gone: remaining queue untouched *)
  Alcotest.(check (list string)) "only frames before peer-gone" [ "a" ]
    (List.rev !written);
  Alcotest.check Alcotest.int "exactly one close" 1 (List.length !closes);
  Alcotest.check Alcotest.int "close reason is Peer_gone"
    0 (match !closes with [ `Peer_gone ] -> 0 | _ -> 1)

let test_stream_empty () =
  let written = ref [] in
  let closes = ref [] in
  Http_agent.stream_events
    ~write_frame:(fun _ -> written := "x" :: !written; `Ok)
    ~close_stream:(fun c -> closes := c :: !closes)
    ~produce:(fun _push finish -> finish ());
  Alcotest.(check (list string)) "no frames" [] !written;
  Alcotest.check Alcotest.int "clean close" 0
    (match !closes with [ `Finish ] -> 0 | _ -> 1)

let test_stream_joins_producer () =
  (* stream_events returns only after the producer fiber completed
     (switch join) — the handler fiber must not race ahead of it. *)
  let done_flag = Atomic.make false in
  Http_agent.stream_events
    ~write_frame:(fun _ -> `Ok)
    ~close_stream:(fun _ -> ())
    ~produce:(fun push finish ->
      push "late";
      finish ();
      Atomic.set done_flag true);
  Alcotest.check Alcotest.bool "producer completed before return" true
    (Atomic.get done_flag)

let () =
  Eio_main.run @@ fun _env ->
  let open Alcotest in
  run "http_body_stream"
    [ ( "parse_body"
      , [ test_case "shape" `Quick test_parse_body
        ; test_case "json_field_str" `Quick test_json_field_str ] )
    ; ( "stream_events"
      , [ test_case "drains in order then Finish" `Quick
            test_stream_drains_in_order
        ; test_case "peer-gone stops the drain" `Quick
            test_stream_peer_gone_stops_drain
        ; test_case "empty stream closes clean" `Quick test_stream_empty
        ; test_case "joins producer fiber" `Quick test_stream_joins_producer
        ] )
    ]
