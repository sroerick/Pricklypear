(** Round-trip tests for ai_store (needs Postgres from scripts/dev.sh).

    Skips cleanly when PP_DB is unreachable so unit CI without PG still
    builds. Live smoke is the primary verification. *)

let env_or name dft =
  match Sys.getenv_opt name with Some s when s <> "" -> s | _ -> dft

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = Pricklypear_image_lib.Store.config_from_env () in
      let store =
        Pricklypear_image_lib.Store.make ~cfg
          ~branch_id:"00000000-0000-0000-0000-000000000001" ()
      in
      Pricklypear_image_lib.Store.connect store;
      match Pricklypear_image_lib.Store.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let test_round_trip () =
  with_store (fun store ->
    let open Pricklypear_image_lib in
    let owner = "ai_store_test_" ^ string_of_int (Random.int 1_000_000) in
    let created =
      Ai_store.create_session store ~owner ~seed:"system seed" ~tools:true
        ~title:"t1" ~branch_ref:"system/main" ~model:"test-model" ()
    in
    match created with
    | Error e -> Alcotest.fail e
    | Ok sess ->
        let r1 =
          Ai_store.append_messages store ~session_id:sess.id
            [ Llm_client.Msg { role = Llm_client.User; content = "hello" }
            ; Llm_client.Msg { role = Llm_client.Assistant; content = "hi" }
            ]
        in
        (match r1 with Error e -> Alcotest.fail e | Ok () -> ());
        let tc : Llm_client.tool_call =
          { id = "call_1"; name = "list_bindings"; arguments = "{}" }
        in
        let r2 =
          Ai_store.append_messages store ~session_id:sess.id
            [ Llm_client.Assistant_tools
                { content = Some "checking"; tool_calls = [ tc ] }
            ; Llm_client.Tool_result
                { tool_call_id = "call_1"; content = "(no bindings)" }
            ]
        in
        (match r2 with Error e -> Alcotest.fail e | Ok () -> ());
        let hist = Ai_store.list_messages store ~session_id:sess.id in
        Alcotest.(check int) "4 messages" 4 (List.length hist);
        let api = Ai_store.messages_for_api sess hist in
        (match api with
         | Llm_client.Msg { role = Llm_client.System; content } :: _ ->
             Alcotest.(check string) "seed injected" "system seed" content
         | _ -> Alcotest.fail "expected system seed first");
        Alcotest.(check int) "api msgs = 1+history" 5 (List.length api);
        let got = Ai_store.get_session store ~owner ~id:sess.id in
        (match got with
         | None -> Alcotest.fail "get_session missing"
         | Some s ->
             Alcotest.(check string) "owner" owner s.owner;
             Alcotest.(check bool) "tools" true s.tools);
        let listed = Ai_store.list_sessions store ~owner () in
        Alcotest.(check bool) "listed" true (List.length listed >= 1);
        let del = Ai_store.delete_session store ~owner ~id:sess.id in
        Alcotest.(check bool) "deleted" true del;
        let gone = Ai_store.get_session store ~owner ~id:sess.id in
        Alcotest.(check bool) "gone" true (gone = None);
        ())

let () =
  Random.self_init ();
  Alcotest.run "ai_store"
    [ ( "round-trip"
      , [ Alcotest.test_case "create/append/list/delete" `Quick test_round_trip
        ] )
    ]
