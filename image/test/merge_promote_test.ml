(** merge conflict report + promote routes-with-handler
    Spec: branching.merge, branching.promote
    Needs Postgres (scripts/dev.sh). Skips if unreachable. *)

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
      | Some _ -> f store)

let expr_of_int n =
  Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt (Int64.of_int n))

let uniq p = Printf.sprintf "%s_%d" p (Random.int 1_000_000)

let count_types ~conn ~branch_id =
  match
    S.exec conn
      ~params:[ Pgx.Value.of_string branch_id ]
      "SELECT COUNT(*)::text FROM type_defs WHERE branch_id = $1"
  with
  | [ [ c ] ] ->
      (match Pgx.Value.to_string c with
       | Some s -> int_of_string_opt s |> Option.value ~default:0
       | None -> 0)
  | _ -> 0

let count_routes ~conn ~branch_id =
  match
    S.exec conn
      ~params:[ Pgx.Value.of_string branch_id ]
      "SELECT COUNT(*)::text FROM view_bindings WHERE branch_id = $1"
  with
  | [ [ c ] ] ->
      (match Pgx.Value.to_string c with
       | Some s -> int_of_string_opt s |> Option.value ~default:0
       | None -> 0)
  | _ -> 0

let lookup_route ~conn ~branch_id ~route =
  match
    S.exec conn
      ~params:
        [ Pgx.Value.of_string branch_id; Pgx.Value.of_string route ]
      "SELECT handler, COALESCE(visibility,'private') FROM view_bindings \
       WHERE branch_id = $1 AND route = $2"
  with
  | [ [ h; v ] ] ->
      (match Pgx.Value.to_string h, Pgx.Value.to_string v with
       | Some hh, Some vv -> Some (hh, vv)
       | _ -> None)
  | _ -> None

(** Target moved newer than source → conflict named in result string. *)
let test_merge_conflict_report () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "mrg_child_" ^ suffix in
          let fname = "mrg_conflict_" ^ suffix in
          store.branch_id <- main_id;
          (* seed on main *)
          S.persist_function store fname (expr_of_int 1) "tester";
          match
            S.fork_branch store ~parent_id:main_id ~owner:"mrg_test"
              ~child_name
          with
          | None -> Alcotest.fail "fork failed"
          | Some child_id ->
              (* child pins older-ish body first *)
              store.branch_id <- child_id;
              S.persist_function store fname (expr_of_int 2) "tester";
              (* bump main (target) AFTER child so target.updated_at > source *)
              store.branch_id <- main_id;
              Unix.sleepf 0.05;
              S.persist_function store fname (expr_of_int 3) "tester";
              let r =
                S.merge_branch store ~source_id:child_id ~target_id:main_id
              in
              (match r with
               | None -> Alcotest.fail "merge failed"
               | Some res ->
                   let msg = S.format_merge_result res in
                   Alcotest.(check bool) "has conflict name" true
                     (List.mem fname res.conflicts);
                   Alcotest.(check bool) "msg shape" true
                     (String.contains msg '('
                      && String.contains msg ':'));
              let _ =
                S.delete_function_on_branch store ~branch_id:main_id
                  ~name:fname
              in
              let _ =
                S.delete_function_on_branch store ~branch_id:child_id
                  ~name:fname
              in
              ignore conn)

(** Merge must not copy type_defs or view_bindings. *)
let test_merge_scope_no_types_or_routes () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "mrg_scope_" ^ suffix in
          let fname = "mrg_scope_fn_" ^ suffix in
          let tname = "mrg_scope_ty_" ^ suffix in
          let route = "/mrg-scope-" ^ suffix in
          store.branch_id <- main_id;
          let main_types_before = count_types ~conn ~branch_id:main_id in
          let main_routes_before = count_routes ~conn ~branch_id:main_id in
          match
            S.fork_branch store ~parent_id:main_id ~owner:"mrg_test"
              ~child_name
          with
          | None -> Alcotest.fail "fork failed"
          | Some child_id ->
              store.branch_id <- child_id;
              S.persist_function store fname (expr_of_int 1) "tester";
              S.persist_type store ~name:tname ~fields:[ "a"; "b" ] ~author:"tester";
              S.register_route store ~route ~handler:fname
                ~visibility:"public" ();
              let _ =
                S.merge_branch store ~source_id:child_id ~target_id:main_id
              in
              let main_types_after = count_types ~conn ~branch_id:main_id in
              let main_routes_after = count_routes ~conn ~branch_id:main_id in
              Alcotest.(check int) "no type_defs copied" main_types_before
                main_types_after;
              Alcotest.(check int) "no view_bindings copied" main_routes_before
                main_routes_after;
              (* function should have landed *)
              (match
                 S.lookup_function_on_branch store ~branch_id:main_id
                   ~name:fname
               with
               | None -> Alcotest.fail "fn not merged"
               | Some _ -> ());
              let _ =
                S.delete_function_on_branch store ~branch_id:main_id
                  ~name:fname
              in
              let _ =
                S.delete_function_on_branch store ~branch_id:child_id
                  ~name:fname
              in
              ())

(** Promote handler also copies its route; unrelated route stays put. *)
let test_promote_routes_with_handler () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "prm_child_" ^ suffix in
          let h_yes = "prm_yes_" ^ suffix in
          let h_no = "prm_no_" ^ suffix in
          let r_yes = "/prm-yes-" ^ suffix in
          let r_no = "/prm-no-" ^ suffix in
          match
            S.fork_branch store ~parent_id:main_id ~owner:"prm_test"
              ~child_name
          with
          | None -> Alcotest.fail "fork failed"
          | Some child_id ->
              store.branch_id <- child_id;
              S.persist_function store h_yes (expr_of_int 1) "tester";
              S.persist_function store h_no (expr_of_int 2) "tester";
              S.register_route store ~route:r_yes ~handler:h_yes
                ~visibility:"public" ();
              S.register_route store ~route:r_no ~handler:h_no
                ~visibility:"private" ();
              let n =
                S.promote_functions store ~source_id:child_id
                  ~target_id:main_id ~names:[ h_yes ]
              in
              Alcotest.(check int) "promoted 1" 1 n;
              (match lookup_route ~conn ~branch_id:main_id ~route:r_yes with
               | None -> Alcotest.fail "route for promoted handler missing"
               | Some (h, vis) ->
                   Alcotest.(check string) "handler" h_yes h;
                   Alcotest.(check string) "visibility preserved" "public" vis);
              (match lookup_route ~conn ~branch_id:main_id ~route:r_no with
               | Some _ -> Alcotest.fail "unrelated route should stay put"
               | None -> ());
              (* cleanup *)
              let _ =
                S.delete_function_on_branch store ~branch_id:main_id
                  ~name:h_yes
              in
              let _ =
                S.exec conn
                  ~params:
                    [ Pgx.Value.of_string main_id; Pgx.Value.of_string r_yes ]
                  "DELETE FROM view_bindings WHERE branch_id = $1 AND route = $2"
              in
              ())

(** Promote is all-or-nothing (branching.promote atomicity): an injected
    failure on one name must roll back EVERYTHING — zero rows on target.
    Regression guard for the with_transaction wrap: pre-transaction, the
    names before the failing one landed (autocommit per statement). *)
let test_promote_atomic () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let drop_injection () =
            let (_ : Pgx.row list) =
              S.exec conn
                "DROP TRIGGER IF EXISTS pp_test_boom_trg ON function_defs"
            in
            let (_ : Pgx.row list) =
              S.exec conn "DROP FUNCTION IF EXISTS pp_test_boom()"
            in
            ()
          in
          Fun.protect
            (fun () ->
              let suffix = string_of_int (Random.int 1_000_000) in
              let child_name = "prm_atom_" ^ suffix in
              let ok1 = "prm_ok1_" ^ suffix in
              let boom = "boom_" ^ suffix in
              let ok2 = "prm_ok2_" ^ suffix in
              match
                S.fork_branch store ~parent_id:main_id ~owner:"prm_test"
                  ~child_name
              with
              | None -> Alcotest.fail "fork failed"
              | Some child_id ->
                  (* Seed all three BEFORE the rogue trigger exists, so
                     seeding itself is unaffected. *)
                  store.branch_id <- child_id;
                  S.persist_function store ok1 (expr_of_int 1) "tester";
                  S.persist_function store boom (expr_of_int 2) "tester";
                  S.persist_function store ok2 (expr_of_int 3) "tester";
                  (* Rogue trigger: any INSERT/UPDATE whose name starts with
                     'boom' raises. Fires on the target upsert mid-fold. *)
                  let (_ : Pgx.row list) =
                    S.exec conn
                      "CREATE OR REPLACE FUNCTION pp_test_boom() RETURNS trigger AS $FN$ BEGIN RAISE EXCEPTION 'pp_test_boom: injected'; END $FN$ LANGUAGE plpgsql"
                  in
                  let (_ : Pgx.row list) =
                    S.exec conn
                      "CREATE TRIGGER pp_test_boom_trg BEFORE INSERT OR UPDATE ON function_defs FOR EACH ROW WHEN (NEW.name LIKE 'boom%') EXECUTE FUNCTION pp_test_boom()"
                  in
                  let n =
                    S.promote_functions store ~source_id:child_id
                      ~target_id:main_id ~names:[ ok1; boom; ok2 ]
                  in
                  (* promote swallows the failure and reports 0. *)
                  Alcotest.(check int) "promoted 0 (rolled back)" 0 n;
                  (match
                     S.lookup_function_on_branch store ~branch_id:main_id
                       ~name:ok1
                   with
                  | Some _ ->
                      Alcotest.fail
                        "ok1 landed despite failure — promote is NOT atomic"
                  | None -> ());
                  (match
                     S.lookup_function_on_branch store ~branch_id:main_id
                       ~name:ok2
                   with
                  | Some _ ->
                      Alcotest.fail
                        "ok2 landed despite failure — promote is NOT atomic"
                  | None -> ());
                  (* cleanup child rows *)
                  let (_ : Pgx.row list) =
                    S.exec conn
                      ~params:
                        [ Pgx.Value.of_string child_id
                        ; Pgx.Value.of_string ok1
                        ; Pgx.Value.of_string boom
                        ; Pgx.Value.of_string ok2 ]
                      "DELETE FROM function_defs WHERE branch_id = $1 AND name IN ($2,$3,$4)"
                  in
                  ())
            ~finally:drop_injection)

(** branching.merge S3 (2026-09 decision): diverged names are NEVER
    auto-applied — they come back as conflicts, the target keeps its own
    body, and post-merge (base advanced) the source's pin reclassifies
    ahead (its own change vs the new base) until a human resolves.
    ahead-new names still apply normally. *)
let test_merge_diverged_not_applied () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "mrg_div_" ^ suffix in
          let div_fn = "mrg_divfn_" ^ suffix in
          let new_fn = "mrg_newfn_" ^ suffix in
          store.branch_id <- main_id;
          S.persist_function store div_fn (expr_of_int 1) "tester";
          match
            S.fork_branch store ~parent_id:main_id ~owner:"mrg_test"
              ~child_name
          with
          | None -> Alcotest.fail "fork failed"
          | Some child_id ->
              (* child moves its copy; parent ALSO moves its copy since
                 the fork base → diverged. child adds a fresh fn →
                 ahead-new. *)
              store.branch_id <- child_id;
              S.persist_function store div_fn (expr_of_int 2) "tester";
              S.persist_function store new_fn (expr_of_int 5) "tester";
              store.branch_id <- main_id;
              S.persist_function store div_fn (expr_of_int 3) "tester";
              (match
                 S.merge_branch store ~source_id:child_id ~target_id:main_id
               with
              | None -> Alcotest.fail "merge failed"
              | Some res ->
                  Alcotest.(check bool) "diverged name in conflicts" true
                    (List.mem div_fn res.conflicts);
                  Alcotest.(check int) "only ahead-new applied" 1 res.count;
                  (* target kept its own body — NOT clobbered by the
                     child's 2. *)
                  (match
                     S.lookup_function_on_branch store ~branch_id:main_id
                       ~name:div_fn
                   with
                  | Some (body, _, _, _) ->
                      Alcotest.(check bool) "target body not overwritten" true
                        (body = expr_of_int 3)
                  | None -> Alcotest.fail "target row vanished");
                  (* ahead-new landed *)
                  (match
                     S.lookup_function_on_branch store ~branch_id:main_id
                       ~name:new_fn
                   with
                  | Some (body, _, _, _) ->
                      Alcotest.(check bool) "ahead-new applied" true
                        (body = expr_of_int 5)
                  | None -> Alcotest.fail "ahead-new not merged"));
              (* post-merge: base advanced; the child's pin is its own
                 change vs the new base → ahead, NOT diverged, NOT synced. *)
              let st =
                S.branch_status store ~branch_id:child_id
                |> List.find_opt (fun r -> r.S.st_name = div_fn)
              in
              (match st with
              | Some r ->
                  Alcotest.(check bool) "post-merge reclassifies ahead" true
                    (r.S.st_status = S.SAhead)
              | None -> Alcotest.fail "status row missing post-merge");
              (* cleanup *)
              let _ =
                S.delete_function_on_branch store ~branch_id:main_id
                  ~name:div_fn
              in
              let _ =
                S.delete_function_on_branch store ~branch_id:main_id
                  ~name:new_fn
              in
              let _ =
                S.delete_function_on_branch store ~branch_id:child_id
                  ~name:div_fn
              in
              let _ =
                S.delete_function_on_branch store ~branch_id:child_id
                  ~name:new_fn
              in
              ())

let test_format_merge_result () =
  Alcotest.(check string) "no conflicts" "merged 3 rows"
    (S.format_merge_result { count = 3; conflicts = [] });
  Alcotest.(check string) "with conflicts"
    "merged 5 rows (2 conflicts: a, b)"
    (S.format_merge_result { count = 5; conflicts = [ "a"; "b" ] })

let () =
  Random.self_init ();
  Alcotest.run "merge-promote"
    [ ( "format"
      , [ Alcotest.test_case "format_merge_result" `Quick
            test_format_merge_result
        ] )
    ; ( "db"
      , [ Alcotest.test_case "conflict report" `Quick test_merge_conflict_report
        ; Alcotest.test_case "scope no types/routes" `Quick
            test_merge_scope_no_types_or_routes
        ; Alcotest.test_case "promote routes with handler" `Quick
            test_promote_routes_with_handler
        ; Alcotest.test_case "promote atomic" `Quick test_promote_atomic
        ; Alcotest.test_case "merge diverged not applied" `Quick
            test_merge_diverged_not_applied
        ] )
    ]
