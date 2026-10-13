(* checkpoints (branching.checkpoints): named points on the gen stream.
   DB cases need Postgres via scripts/dev.sh and skip if unreachable.

   Verifies:
   - cp captures the branch's OWN post-commit head gen (== the branch
     generation counter read just before the cp; a later cp lands a strictly
     higher gen);
   - cp-list is newest-first; duplicate names refuse; bad names refuse;
   - cp-diff classifies the windowed decision table: changed / added-since /
     removed-since, unchanged pins filtered;
   - cp-restore lands the captured bodies as NEW WRITES: the changed fn's
     restore fires exactly ONE code_history row (the UPDATE before-image), a
     removed pin is re-created, bodies match verbatim, gen moves forward,
     base_gen is untouched;
   - branch/log is newest-first with create/update/delete ops and honors the
     cp window (gen > cp.gen). *)

module S = Pricklypear_image_lib.Store

let main_id = "00000000-0000-0000-0000-000000000001"

let lit n =
  Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt (Int64.of_int n))

let body_json e = Pricklypear_lisp.Ast.expr_to_body_json e

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
      Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env in
    Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
        let cfg = S.config_from_env () in
        let store = S.make ~cfg ~branch_id:main_id () in
        S.connect store;
        match S.conn_of store with
        | None ->
            Printf.printf
              "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
        | Some _ -> f store)

let cleanup_branch store ~branch_id =
  let conn = Option.get (S.conn_of store) in
  let del sql =
    ignore (S.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql) in
  del "DELETE FROM branch_checkpoints WHERE branch_id = $1";
  del "DELETE FROM view_bindings WHERE branch_id = $1";
  del "DELETE FROM type_defs WHERE branch_id = $1";
  del "DELETE FROM function_defs WHERE branch_id = $1";
  del "DELETE FROM branches WHERE id = $1"

let scalar1 store sql ~branch_id =
  match S.conn_of store with
  | Some conn -> (
      let rows = S.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql in
      match rows with
      | [ [ g ] ] -> (match Pgx.Value.to_int64 g with Some v -> v | None -> 0L)
      | _ -> 0L)
  | None -> 0L

let branch_gen store ~branch_id =
  scalar1 store
    "SELECT COALESCE((SELECT generation FROM branch_generations WHERE branch_id = $1::uuid), 0)::bigint"
    ~branch_id

let max_version_gen store ~branch_id =
  scalar1 store
    "SELECT COALESCE(max(gen), 0)::bigint FROM tree_versions WHERE branch_id = $1::uuid"
    ~branch_id

let base_gen store ~branch_id =
  scalar1 store "SELECT base_gen::bigint FROM branches WHERE id = $1::uuid"
    ~branch_id

let code_history_count store ~branch_id ~name =
  match S.conn_of store with
  | Some conn -> (
      let rows =
        S.exec conn
          ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
          "SELECT count(*)::bigint FROM code_history WHERE branch_id = $1::uuid AND name = $2"
      in
      match rows with
      | [ [ c ] ] -> (match Pgx.Value.to_int64 c with Some v -> v | None -> 0L)
      | _ -> 0L)
  | None -> 0L

let own_body store ~branch_id ~name =
  match S.lookup_function_on_branch store ~branch_id ~name with
  | Some (b, _, _, _) -> Some b
  | None -> None

(* ---- pure: name grammar ---- *)

let test_name_grammar () =
  Alcotest.(check bool) "ok simple" true (S.cp_name_ok "before-lens");
  Alcotest.(check bool) "ok digits" true (S.cp_name_ok "v2");
  Alcotest.(check bool) "refuse upper" false (S.cp_name_ok "Before");
  Alcotest.(check bool) "refuse underscore" false (S.cp_name_ok "a_b");
  Alcotest.(check bool) "refuse empty" false (S.cp_name_ok "")

(* ---- db ---- *)

let test_cp_captures_post_commit_gen () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          let src =
            S.fork_branch store ~parent_id:main_id
              ~owner:"cp_test" ~child_name:("cp_src_" ^ sfx) in
          (match src with
           | Some src_id ->
               store.branch_id <- src_id;
               S.persist_function store "alpha" (body_json (lit 1)) "tester";
               S.persist_function store "beta" (body_json (lit 2)) "tester";
               let g_before = branch_gen store ~branch_id:src_id in
               (match
                  S.create_checkpoint store ~branch_id:src_id ~name:"before"
                    ~message:None ~author:"tester"
                with
                | Error why -> Alcotest.fail ("cp failed: " ^ why)
                | Ok cp ->
                    Alcotest.(check bool) "cp gen == post-commit counter" true
                      (Int64.equal cp.S.cp_gen g_before);
                    Alcotest.(check bool) "cp gen == version trail head" true
                      (Int64.equal cp.S.cp_gen
                         (max_version_gen store ~branch_id:src_id));
                    S.persist_function store "gamma" (body_json (lit 3))
                      "tester";
                    (match
                       S.create_checkpoint store ~branch_id:src_id ~name:"after"
                         ~message:(Some "post gamma") ~author:"tester"
                     with
                     | Error why -> Alcotest.fail ("cp2 failed: " ^ why)
                     | Ok cp2 ->
                         Alcotest.(check bool) "later cp strictly higher" true
                           (Int64.compare cp2.S.cp_gen cp.S.cp_gen > 0);
                         let names =
                           List.map
                             (fun (c : S.cp_row) -> c.S.cp_name)
                             (S.list_checkpoints store ~branch_id:src_id) in
                         Alcotest.(check (list string)) "cp-list newest first"
                           [ "after"; "before" ] names;
                         (match
                            S.create_checkpoint store ~branch_id:src_id
                              ~name:"before" ~message:None ~author:"tester"
                          with
                          | Error _ -> ()
                          | Ok _ -> Alcotest.fail "duplicate cp must refuse");
                         (match
                            S.create_checkpoint store ~branch_id:src_id
                              ~name:"Bad_Name" ~message:None ~author:"tester"
                          with
                          | Error _ -> ()
                            | Ok _ -> Alcotest.fail "bad name must refuse")));
               cleanup_branch store ~branch_id:src_id
           | None -> Alcotest.fail "fork failed"))

let test_cp_diff_and_restore () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          let src =
            S.fork_branch store ~parent_id:main_id
              ~owner:"cp_test" ~child_name:("cp_diff_" ^ sfx) in
          (match src with
           | Some src_id ->
               store.branch_id <- src_id;
               S.persist_function store "alpha" (body_json (lit 1)) "tester";
               S.persist_function store "beta" (body_json (lit 2)) "tester";
               S.persist_function store "gamma" (body_json (lit 3)) "tester";
               let bg0 = base_gen store ~branch_id:src_id in
               (match
                  S.create_checkpoint store ~branch_id:src_id ~name:"before"
                    ~message:None ~author:"tester"
                with
                | Error why -> Alcotest.fail ("cp failed: " ^ why)
                | Ok cp ->
                    (* mutate: alpha changed, beta deleted, delta added *)
                    S.persist_function store "alpha" (body_json (lit 11))
                      "tester";
                    ignore
                      (S.delete_function_on_branch store ~branch_id:src_id
                         ~name:"beta");
                    S.persist_function store "delta" (body_json (lit 4))
                      "tester";
                    let rows =
                      S.checkpoint_diff store ~branch_id:src_id
                        ~cp_gen:cp.S.cp_gen in
                    let op_of n =
                      match
                        List.find_opt
                          (fun (r : S.cp_diff_row) -> String.equal r.S.d_name n)
                          rows
                      with
                      | Some r -> Some (S.cp_diff_op_s r.S.d_op)
                      | None -> None in
                    Alcotest.(check (option string)) "alpha changed"
                      (Some "changed") (op_of "alpha");
                    Alcotest.(check (option string)) "beta removed"
                      (Some "removed-since") (op_of "beta");
                    Alcotest.(check (option string)) "delta added"
                      (Some "added-since") (op_of "delta");
                    Alcotest.(check (option string)) "gamma unchanged (filtered)"
                      None (op_of "gamma");
                    (* restore default set = changed + removed (captured bodies) *)
                    let gen_before_restore = branch_gen store ~branch_id:src_id in
                    let ch_before =
                      code_history_count store ~branch_id:src_id ~name:"alpha" in
                    let outcomes =
                      S.restore_checkpoint store ~branch_id:src_id
                        ~rows:
                          (List.filter
                             (fun (r : S.cp_diff_row) -> r.S.d_op <> S.CpAdded)
                             rows)
                        ~author:"restorer" in
                    let failed =
                      List.filter
                        (fun (o : S.cp_restore_outcome) -> o.S.ro_error <> None)
                        outcomes in
                    Alcotest.(check int) "restore all succeeded" 0
                      (List.length failed);
                    Alcotest.(check bool) "alpha body verbatim" true
                      (own_body store ~branch_id:src_id ~name:"alpha"
                       = Some (body_json (lit 1)));
                    Alcotest.(check bool) "beta re-created verbatim" true
                      (own_body store ~branch_id:src_id ~name:"beta"
                       = Some (body_json (lit 2)));
                    Alcotest.(check bool) "delta untouched by restore" true
                      (own_body store ~branch_id:src_id ~name:"delta"
                       = Some (body_json (lit 4)));
                    (* alpha: redefine UPDATE + restore UPDATE -> exactly one
                       more code_history row from the restore *)
                    Alcotest.(check bool) "alpha had one redefine row" true
                      (Int64.equal ch_before 1L);
                    Alcotest.(check int) "alpha code_history delta == 1" 1
                      (Int64.to_int
                         (Int64.sub
                            (code_history_count store ~branch_id:src_id
                               ~name:"alpha")
                            ch_before));
                    (* gen moved forward; base_gen untouched *)
                    Alcotest.(check bool) "gen moved forward" true
                      (Int64.compare
                         (branch_gen store ~branch_id:src_id)
                         gen_before_restore > 0);
                    Alcotest.(check bool) "base_gen untouched" true
                        (Int64.equal bg0 (base_gen store ~branch_id:src_id)));
                    cleanup_branch store ~branch_id:src_id
           | None -> Alcotest.fail "fork failed"))

let test_branch_log () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          let src =
            S.fork_branch store ~parent_id:main_id
              ~owner:"cp_test" ~child_name:("cp_log_" ^ sfx) in
          (match src with
           | Some src_id ->
               store.branch_id <- src_id;
               S.persist_function store "solo" (body_json (lit 1)) "tester";
               (match
                  S.create_checkpoint store ~branch_id:src_id ~name:"w"
                    ~message:None ~author:"tester"
                with
                | Error why -> Alcotest.fail ("cp failed: " ^ why)
                | Ok cp ->
                    S.persist_function store "solo" (body_json (lit 2))
                      "tester";
                    ignore
                      (S.delete_function_on_branch store ~branch_id:src_id
                         ~name:"solo");
                    let log =
                      S.checkpoint_log store ~branch_id:src_id ~since_gen:None in
                    let ops = List.map (fun (e : S.cp_log_row) -> e.S.l_op) log in
                    Alcotest.(check (list string)) "newest-first ops"
                      [ "delete"; "update"; "create" ] ops;
                    let windowed =
                      S.checkpoint_log store ~branch_id:src_id
                        ~since_gen:(Some cp.S.cp_gen) in
                    let windowed_ops =
                      List.map (fun (e : S.cp_log_row) -> e.S.l_op) windowed in
                    Alcotest.(check (list string)) "window excludes pre-cp"
                      [ "delete"; "update" ] windowed_ops;
                    Alcotest.(check bool) "window gens post-cp" true
                      (List.for_all
                         (fun (e : S.cp_log_row) ->
                           Int64.compare e.S.l_gen cp.S.cp_gen > 0)
                           windowed));
                    cleanup_branch store ~branch_id:src_id
           | None -> Alcotest.fail "fork failed"))

let () =
  Random.self_init ();
  Alcotest.run "checkpoints"
    [ ( "pure"
      , [ Alcotest.test_case "name grammar" `Quick test_name_grammar ] )
    ; ( "db"
      , [
          Alcotest.test_case "cp captures post-commit head gen + list + refusals"
            `Quick test_cp_captures_post_commit_gen;
          Alcotest.test_case "cp-diff windowed table + restore as new writes"
            `Quick test_cp_diff_and_restore;
          Alcotest.test_case "branch/log ops + cp window" `Quick test_branch_log;
        ] ) ]
