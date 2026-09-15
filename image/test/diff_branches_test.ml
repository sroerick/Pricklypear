(** Branch diff classification (branching.diff): Store.diff_branches.
    Needs Postgres (scripts/dev.sh). Skips if unreachable.

    Verifies: modified fn carries parsed from/to exprs; added/removed
    across two empty-lens children of main; shared unchanged defs are
    NOT reported; types classified on fields; routes added. *)

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

let expr_of_int n =
  Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt (Int64.of_int n))

(* i-1788671083-c: the store persists raw jsonb bodies. *)
let body_json_of_int n =
  Pricklypear_lisp.Ast.expr_to_body_json (expr_of_int n)

let uniq p = Printf.sprintf "%s_%d" p (Random.int 1_000_000)

let find rows ~name =
  List.find_opt (fun r -> r.S.diff_name = name) rows

let kind_of (r : S.branch_diff_row) = r.S.diff_kind
let status_of (r : S.branch_diff_row) = r.S.diff_status

(** Two empty-lens children of main: A and B. Both inherit main's same
    shared defs (unchanged → not reported). Each owns divergent rows. *)
let test_diff_classification () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let a_name = "dfa_child_" ^ suffix in
          let b_name = "dfb_child_" ^ suffix in
          let fn_shared = "dfn_shared_" ^ suffix in
          let fn_mod = "dfn_mod_" ^ suffix in
          let fn_a_add = "dfn_a_" ^ suffix in
          let fn_b_add = "dfn_b_" ^ suffix in
          let ty_shared = "dty_shared_" ^ suffix in
          let route_b = "/drt-" ^ suffix in
          let handler_b = "dfn_b_" ^ suffix in
          store.branch_id <- main_id;
          (* shared on main: identical effective def on both children *)
          S.persist_function store fn_shared (body_json_of_int 1) "tester";
          S.persist_type store ~name:ty_shared ~fields:[ "a" ] ~author:"tester";
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"dfa_test"
               ~child_name:a_name
           with
           | None -> Alcotest.fail "fork A failed"
           | Some a_id ->
               (match
                  S.fork_branch store ~parent_id:main_id ~owner:"dfa_test"
                    ~child_name:b_name
                with
                | None -> Alcotest.fail "fork B failed"
                | Some b_id ->
                    (* child A: own fn_a_add + fn_mod (int 1) *)
                    store.branch_id <- a_id;
                    S.persist_function store fn_a_add (body_json_of_int 5) "tester";
                    S.persist_function store fn_mod (body_json_of_int 1) "tester";
                    (* child B: own fn_b_add + fn_mod (int 2) + type override
                       + a route *)
                    store.branch_id <- b_id;
                    S.persist_function store fn_b_add (body_json_of_int 6) "tester";
                    S.persist_function store fn_mod (body_json_of_int 2) "tester";
                    S.persist_type store ~name:ty_shared
                      ~fields:[ "a"; "b" ] ~author:"tester";
                    S.register_route store ~route:route_b ~handler:handler_b
                      ~visibility:"public" ();
                    let rows = S.diff_branches store ~from_id:a_id ~to_id:b_id in
                    (* unchanged shared fn must NOT be reported *)
                    Alcotest.(check bool) "shared unchanged fn not in diff" true
                      (find rows ~name:fn_shared = None);
                    (* fn_mod modified (from int1 on A → int2 on B) *)
                    (match find rows ~name:fn_mod with
                     | None -> Alcotest.fail "fn_mod missing from diff"
                     | Some r ->
                         Alcotest.(check bool) "fn_mod is fn" true
                           (kind_of r = S.DiffFn);
                         Alcotest.(check bool) "fn_mod modified" true
                           (status_of r = S.DiffModified);
                         (* i-1788671083-c: rows carry RAW jsonb bodies; the
                            AST parses at the boundary. *)
                         Alcotest.(check bool) "fn_mod from_body parses" true
                           (Option.bind r.S.from_body
                              Pricklypear_lisp.Ast.expr_of_body_json
                            <> None);
                         Alcotest.(check bool) "fn_mod to_body parses" true
                           (Option.bind r.S.to_body
                              Pricklypear_lisp.Ast.expr_of_body_json
                            <> None));
                    (* fn_a_add removed (in A, not B) *)
                    (match find rows ~name:fn_a_add with
                     | None -> Alcotest.fail "fn_a_add missing"
                     | Some r ->
                         Alcotest.(check bool) "fn_a_add removed" true
                           (status_of r = S.DiffRemoved);
                         Alcotest.(check bool) "fn_a_add from_body present" true
                           (r.S.from_body <> None);
                         Alcotest.(check bool) "fn_a_add to_body absent" true
                           (r.S.to_body = None));
                    (* fn_b_add added (in B, not A) *)
                    (match find rows ~name:fn_b_add with
                     | None -> Alcotest.fail "fn_b_add missing"
                     | Some r ->
                         Alcotest.(check bool) "fn_b_add added" true
                           (status_of r = S.DiffAdded);
                         Alcotest.(check bool) "fn_b_add from_body absent" true
                           (r.S.from_body = None);
                         Alcotest.(check bool) "fn_b_add to_body present" true
                           (r.S.to_body <> None));
                    (* type override modified (fields a → a,b) *)
                    (match find rows ~name:ty_shared with
                     | None -> Alcotest.fail "ty_shared missing"
                     | Some r ->
                         Alcotest.(check bool) "ty_shared is type" true
                           (kind_of r = S.DiffType);
                         Alcotest.(check bool) "ty_shared modified" true
                           (status_of r = S.DiffModified));
                    (* route added on B *)
                    (match find rows ~name:route_b with
                     | None -> Alcotest.fail "route_b missing"
                     | Some r ->
                         Alcotest.(check bool) "route_b is route" true
                           (kind_of r = S.DiffRoute);
                         Alcotest.(check bool) "route_b added" true
                           (status_of r = S.DiffAdded));
                    (* cleanup *)
                    ignore (S.delete_function_on_branch store ~branch_id:main_id
                              ~name:fn_shared);
                    ignore (S.delete_function_on_branch store ~branch_id:a_id
                              ~name:fn_a_add);
                    ignore (S.delete_function_on_branch store ~branch_id:b_id
                              ~name:fn_b_add);
                    ignore (S.delete_function_on_branch store ~branch_id:a_id
                              ~name:fn_mod);
                    ignore (S.delete_function_on_branch store ~branch_id:b_id
                              ~name:fn_mod);
                    ignore
                      (S.exec conn
                         ~params:
                           [ Pgx.Value.of_string a_id
                           ; Pgx.Value.of_string b_id ]
                         "DELETE FROM type_defs WHERE branch_id IN ($1,$2) AND \
                          name = 'dummy-no-op'");
                    ignore
                      (S.exec conn
                         ~params:
                           [ Pgx.Value.of_string main_id
                           ; Pgx.Value.of_string ty_shared ]
                         "DELETE FROM type_defs WHERE branch_id = $1 AND name = \
                          $2");
                    ignore
                      (S.exec conn
                         ~params:
                           [ Pgx.Value.of_string b_id
                           ; Pgx.Value.of_string route_b ]
                         "DELETE FROM view_bindings WHERE branch_id = $1 AND \
                          route = $2");
                    ())))

let () =
  Random.self_init ();
  Alcotest.run "diff-branches"
    [ ( "db"
      , [ Alcotest.test_case "classification" `Quick test_diff_classification ]
      ) ]
