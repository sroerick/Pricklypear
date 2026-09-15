(** Branch generation counter tests (remote-eval.borg caching subsection).

    Needs Postgres (scripts/dev.sh, migration 017). Skips cleanly when the
    DB or the branch_generations table is unreachable.

    Acceptance (caching verify clauses):
      - a write in the client's branch ancestry bumps a generation counter;
        the client's next cache-check observes the bump
      - a checkout to a different branch is a distinct (branch, generation)
        key (branch is the namespace)
      - with no writes the cache-check is one cheap unchanged-generation query
*)

module S = Pricklypear_image_lib.Store

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

(* The generation helper returns None when pre-017 / unreachable. *)
let gen_or_fail store ~branch_id =
  match S.current_generation store ~branch_id with
  | Some g -> g
  | None ->
      Alcotest.fail
        "branch_generations missing (restart scripts/dev.sh to apply \
         migration 017)"

let int_of_g (g : int64) : int = Int64.to_int g

let uniq p = Printf.sprintf "gen_%s_%d" p (Random.int 1_000_000)

let write_fn store ~branch_id ~name =
  S.with_branch store ~branch_id (fun () ->
      S.persist_function store name
        (Pricklypear_lisp.Ast.expr_to_body_json
           (Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt 1L)))
        ~doc:(Some "gen-test") "tester")

let test_write_in_ancestry_bumps () =
  with_store (fun store ->
      let _ = gen_or_fail store ~branch_id:main_id in
      let before = int_of_g (gen_or_fail store ~branch_id:main_id) in
      (* write on main = ancestor of every branch *)
      let name = uniq "fn" in
      write_fn store ~branch_id:main_id ~name;
      let after = int_of_g (gen_or_fail store ~branch_id:main_id) in
      Alcotest.(check bool) "main generation bumps on write" true (after > before))

let test_checkout_different_branch_is_distinct_key () =
  with_store (fun store ->
      let main_g = int_of_g (gen_or_fail store ~branch_id:main_id) in
      let child = uniq "b" in
      match
        S.fork_branch store ~parent_id:main_id ~owner:"gen_test" ~child_name:child
      with
      | None -> Alcotest.fail "fork_branch failed"
      | Some child_id ->
          let child_g = int_of_g (gen_or_fail store ~branch_id:child_id) in
          (* a write on the child must not change the parent's key *)
          let before_main = int_of_g (gen_or_fail store ~branch_id:main_id) in
          let name = uniq "cn" in
          write_fn store ~branch_id:child_id ~name;
          let after_main = int_of_g (gen_or_fail store ~branch_id:main_id) in
          Alcotest.(check int) "parent key unchanged by child write"
            before_main after_main;
          (* child observed the write *)
          let child_after = int_of_g (gen_or_fail store ~branch_id:child_id) in
          Alcotest.(check bool) "child generation bumped" true
            (child_after > child_g);
          (* checkout key differs: branch is the namespace *)
          Alcotest.(check bool)
            "checkout key differs (branch is the namespace)" true
            (main_g <> child_after))

let test_no_write_unchanged_cheap () =
  with_store (fun store ->
      let _ = gen_or_fail store ~branch_id:main_id in
      let g1 = S.current_generation store ~branch_id:main_id in
      let g2 = S.current_generation store ~branch_id:main_id in
      Alcotest.(check bool) "unchanged generation with no writes" true
        (g1 <> None && g1 = g2))

let test_type_and_route_write_bump () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let before = int_of_g (gen_or_fail store ~branch_id:main_id) in
          let tname = uniq "ty" in
          let _ =
            S.exec conn
              ~params:
                [ Pgx.Value.of_string main_id
                ; Pgx.Value.of_string tname
                ; Pgx.Value.of_string "[]"
                ; Pgx.Value.of_string "tester"
                ]
              "INSERT INTO type_defs (branch_id,name,fields,author) VALUES \
               ($1,$2,$3::jsonb,$4) ON CONFLICT (branch_id,name) DO UPDATE \
               SET fields=EXCLUDED.fields"
          in
          let after_type = int_of_g (gen_or_fail store ~branch_id:main_id) in
          Alcotest.(check bool) "type write bumps" true (after_type > before);
          let rname = "/gen_" ^ uniq "r" in
          let _ =
            S.exec conn
              ~params:
                [ Pgx.Value.of_string main_id
                ; Pgx.Value.of_string rname
                ; Pgx.Value.of_string "gen_handler"
                ]
              "INSERT INTO view_bindings (branch_id,route,handler) VALUES \
               ($1,$2,$3) ON CONFLICT (branch_id,route) DO UPDATE SET \
               handler=EXCLUDED.handler"
          in
          let after_route = int_of_g (gen_or_fail store ~branch_id:main_id) in
          Alcotest.(check bool) "route write bumps" true
            (after_route > after_type))

let test_data_row_write_bumps_root () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let before = int_of_g (gen_or_fail store ~branch_id:main_id) in
          let tn = uniq "dt" in
          let _ =
            S.exec conn
              ~params:
                [ Pgx.Value.of_string tn
                ; Pgx.Value.of_string "{}"
                ; Pgx.Value.of_string "tester"
                ]
              "INSERT INTO data_rows (type_name,fields,author) VALUES \
               ($1,$2::jsonb,$3)"
          in
          let after = int_of_g (gen_or_fail store ~branch_id:main_id) in
          Alcotest.(check bool) "shared data write bumps root" true
            (after > before))

let () =
  Alcotest.run "branch_generation"
    [
      ( "generation",
        [
          Alcotest.test_case "write in ancestry bumps" `Quick
            test_write_in_ancestry_bumps;
          Alcotest.test_case "checkout to another branch is a distinct key"
            `Quick test_checkout_different_branch_is_distinct_key;
          Alcotest.test_case "no writes -> unchanged cheap check" `Quick
            test_no_write_unchanged_cheap;
          Alcotest.test_case "type + route writes bump" `Quick
            test_type_and_route_write_bump;
          Alcotest.test_case "shared data write bumps root" `Quick
            test_data_row_write_bumps_root;
        ] );
    ]
