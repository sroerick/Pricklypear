(** code-history tests (needs Postgres from scripts/dev.sh).

    Spec: branching.code-history
    Skips cleanly when PP_DB is unreachable. Apply migrations via
    scripts/dev.sh so code_history exists. *)

module S = Pricklypear_image_lib.Store

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    {
      sw;
      net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r);
    }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some conn ->
          let has_table =
            try
              let _ = S.exec conn "SELECT 1 FROM code_history LIMIT 0" in
              true
            with _ -> false
          in
          if not has_table then
            Printf.printf
              "SKIP: code_history missing (restart scripts/dev.sh to \
               apply migration 012)\n%!"
          else f store)

let uniq suffix =
  Printf.sprintf "hist_test_%s_%d" suffix (Random.int 1_000_000)

let expr_of_int n =
  Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt (Int64.of_int n))

(* i-1788671083-c: the store persists raw jsonb bodies. *)
let body_json_of_int n =
  Pricklypear_lisp.Ast.expr_to_body_json (expr_of_int n)

let test_redefine_records_old_body () =
  with_store (fun store ->
      let name = uniq "fn" in
      S.persist_function store name (body_json_of_int 1) ~doc:(Some "v1") "tester";
      S.persist_function store name (body_json_of_int 2) ~doc:(Some "v2") "tester";
      let hist =
        S.list_code_history store ~branch_id:(S.current_branch_id store) ~name
      in
      Alcotest.(check bool) "has history" true (List.length hist >= 1);
      let newest = List.hd hist in
      Alcotest.(check string) "rel" "function_defs" newest.rel;
      Alcotest.(check string) "op" "UPDATE" newest.op;
      (match newest.body with
       | None -> Alcotest.fail "expected body"
       | Some body ->
           Alcotest.(check bool) "old body present" true
             (String.length body > 0));
      let _ =
        S.delete_function_on_branch store
          ~branch_id:(S.current_branch_id store) ~name
      in
      ())

let test_merge_clobber_records_history () =
  with_store (fun store ->
      let suffix = string_of_int (Random.int 1_000_000) in
      let child_name = "hist_child_" ^ suffix in
      let name = "hist_merge_fn_" ^ suffix in
      store.branch_id <- main_id;
      S.persist_function store name (body_json_of_int 10) ~doc:(Some "main") "tester";
      match
        S.fork_branch store ~parent_id:main_id ~owner:"hist_test" ~child_name
      with
      | None -> Alcotest.fail "fork failed"
      | Some child_id ->
          store.branch_id <- child_id;
          S.persist_function store name (body_json_of_int 99) ~doc:(Some "child")
            "tester";
          let n =
            S.merge_branch store ~source_id:child_id ~target_id:main_id
          in
          Alcotest.(check bool) "merged" true
            (match n with Some _ -> true | None -> false);
          let hist = S.list_code_history store ~branch_id:main_id ~name in
          Alcotest.(check bool) "clobber history" true (List.length hist >= 1);
          store.branch_id <- main_id;
          let _ = S.delete_function_on_branch store ~branch_id:main_id ~name in
          let _ =
            S.delete_function_on_branch store ~branch_id:child_id ~name
          in
          ())

let test_additive_insert_no_history () =
  with_store (fun store ->
      let name = uniq "add" in
      store.branch_id <- main_id;
      S.persist_function store name (body_json_of_int 7) "tester";
      let hist =
        S.list_code_history store ~branch_id:(S.current_branch_id store) ~name
      in
      Alcotest.(check int) "no history on insert" 0 (List.length hist);
      let _ =
        S.delete_function_on_branch store
          ~branch_id:(S.current_branch_id store) ~name
      in
      ())

let test_rollback_no_history () =
  with_store (fun store ->
      let name = uniq "rb" in
      store.branch_id <- main_id;
      S.persist_function store name (body_json_of_int 1) "tester";
      let before =
        List.length (S.list_code_history store ~branch_id:main_id ~name)
      in
      (try
         S.with_transaction store (fun conn_opt ->
             match conn_opt with
             | None -> ()
             | Some conn ->
                 let body_json =
                   Yojson.Safe.to_string
                     (Pricklypear_lisp.Ast.expr_to_json (expr_of_int 2))
                 in
                 let sql =
                   "UPDATE function_defs SET body = $3::jsonb, " ^
                   "updated_at = now() " ^
                   "WHERE branch_id = $1 AND name = $2"
                 in
                 let _ =
                   S.exec conn
                     ~params:
                       [ Pgx.Value.of_string main_id
                       ; Pgx.Value.of_string name
                       ; Pgx.Value.of_string body_json
                       ]
                     sql
                 in
                 failwith "force rollback")
       with Failure _ -> ());
      let after =
        List.length (S.list_code_history store ~branch_id:main_id ~name)
      in
      Alcotest.(check int) "rollback leaves no history" before after;
      let _ = S.delete_function_on_branch store ~branch_id:main_id ~name in
      ())

let test_code_diff_smoke () =
  with_store (fun store ->
      let name = uniq "diff" in
      store.branch_id <- main_id;
      S.persist_function store name (body_json_of_int 1) "tester";
      S.persist_function store name (body_json_of_int 2) "tester";
      match S.code_diff store ~branch_id:main_id ~name ~n:1 with
      | Error e -> Alcotest.fail e
      | Ok s ->
          Alcotest.(check bool) "diff non-empty markers" true
            (String.contains s '-' || String.contains s '+');
          let _ = S.delete_function_on_branch store ~branch_id:main_id ~name in
          ())

let () =
  Random.self_init ();
  Alcotest.run "code-history"
    [ ( "history"
      , [ Alcotest.test_case "redefine records old body" `Quick
            test_redefine_records_old_body
        ; Alcotest.test_case "merge clobber records history" `Quick
            test_merge_clobber_records_history
        ; Alcotest.test_case "additive insert no history" `Quick
            test_additive_insert_no_history
        ; Alcotest.test_case "rollback leaves no history" `Quick
            test_rollback_no_history
        ; Alcotest.test_case "code-diff smoke" `Quick test_code_diff_smoke
        ] )
    ]
