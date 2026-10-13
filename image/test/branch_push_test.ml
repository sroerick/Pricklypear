(* branch/push (branching.borg branch-ops push row): Call_graph.push_closure
   (pure) + Branch_push (the store oracle; DB cases need Postgres via
   scripts/dev.sh and skip if unreachable).

   Verifies (closure, pure):
   - target-visible / kernel / special-form refs are free (not holes);
   - an unbound ref defined on the source auto-includes in discovery order
     (mutual recursion safe); one defined nowhere holes its carrier;
   - holes refuse the push all-or-nothing with a complete report;
   - a requested name with no source body is refused;
   - a dep also requested by name lands in [pushed], never [auto].

   Verifies (store, PG):
   - a pierced set (entry + broken->absent) refuses WHOLE — the target
     stays untouched (the 2026-10-05 pulse-row-from class: no partial
     landing);
   - a clean named push lands entry + auto deps with verbatim bodies; a
     name inherited by both sides stays a chain VIEW on the target, never
     an own row;
   - the default set = depth-0 own code (name order);
   - an inherited-only name is refused fork-first (promote_functions'
     own-row lookup would silently skip it);
   - self-push refuses.

   The PRIM-level surface (identity classes, allow_agent_promote gate,
   rehydrate-when-on-target, JSON shape) is smoke-covered by the branch-ops
   row; this file pins the store oracle + the pure analytic it reuses. *)

module S = Pricklypear_image_lib.Store
module CG = Pricklypear_image_lib.Call_graph
module BP = Pricklypear_image_lib.Branch_push

let main_id = "00000000-0000-0000-0000-000000000001"

let lit n =
  Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt (Int64.of_int n))

let call f args = Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var f, args)

let body_json e = Pricklypear_lisp.Ast.expr_to_body_json e

(* entry -> shared (target-visible) + helper; helper -> shared2; shared2 ->
   lit; broken -> absent (defined nowhere); kfn -> if + rows (special form +
   kernel); m1 <-> m2 mutual recursion. *)
let src_fixture () : (string * Pricklypear_lisp.Ast.expr) list =
  [ ("entry", call "shared" [ call "helper" [ lit 1 ] ] )
  ; ("helper", call "shared2" [ lit 2 ])
  ; ("shared2", lit 3)
  ; ("broken", call "absent" [ lit 4 ])
  ; ("kfn", call "if" [ call "rows" [ lit 5 ]; lit 6 ] )
  ; ("m1", call "m2" [ lit 7 ])
  ; ("m2", call "m1" [ lit 8 ])
  ]

let result_s (r : (string list * string list, (string * string list) list) result)
    : string =
  match r with
  | Ok (p, a) ->
      Printf.sprintf "Ok pushed=[%s] auto=[%s]"
        (String.concat "," p) (String.concat "," a)
  | Error hs -> "holes: " ^ BP.holes_msg hs

let closure_failed r = Alcotest.fail ("push_closure: " ^ result_s r)

let expect_ok what ~pushed ~auto r =
  match r with
  | Ok (p, a) ->
      Alcotest.(check (list string)) (what ^ " pushed") pushed p;
      Alcotest.(check (list string)) (what ^ " auto") auto a
  | _ -> closure_failed r

let expect_holes what holes r =
  match r with
  | Error hs ->
      Alcotest.(check (list (pair string (list string)))) what holes hs
  | _ -> closure_failed r

(* ---- pure closure ---- *)

let test_closure_lands_auto_deps () =
  expect_ok "auto include" ~pushed:[ "entry" ] ~auto:[ "helper"; "shared2" ]
    (CG.push_closure ~requested:[ "entry" ] ~src_defs:(src_fixture ())
       ~tgt_visible:[ "shared" ] ~kernel:[])

let test_closure_kernel_special_bound () =
  expect_ok "kernel+special bound" ~pushed:[ "kfn" ] ~auto:[]
    (CG.push_closure ~requested:[ "kfn" ] ~src_defs:(src_fixture ())
       ~tgt_visible:[] ~kernel:[ "rows" ])

let test_closure_hole_refused_whole () =
  expect_holes "exact report" [ ("broken", [ "absent" ]) ]
    (CG.push_closure ~requested:[ "entry"; "broken" ]
       ~src_defs:(src_fixture ()) ~tgt_visible:[ "shared" ] ~kernel:[])

let test_closure_requested_missing_body () =
  expect_holes "requested-no-body" [ ("ghost", []) ]
    (CG.push_closure ~requested:[ "ghost" ] ~src_defs:(src_fixture ())
       ~tgt_visible:[] ~kernel:[])

let test_closure_mutual_recursion () =
  expect_ok "mutual" ~pushed:[ "m1" ] ~auto:[ "m2" ]
    (CG.push_closure ~requested:[ "m1" ] ~src_defs:(src_fixture ())
       ~tgt_visible:[] ~kernel:[])

let test_closure_requested_overlap () =
  expect_ok "overlap" ~pushed:[ "helper"; "entry" ] ~auto:[ "shared2" ]
    (CG.push_closure ~requested:[ "helper"; "entry" ]
       ~src_defs:(src_fixture ()) ~tgt_visible:[ "shared" ] ~kernel:[])

(* ---- store oracle (needs PG; skips if unreachable) ---- *)

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

let cleanup_fn store ~branch_id ~name =
  ignore (S.delete_function_on_branch store ~branch_id ~name)

let cleanup_branch store ~branch_id =
  ignore (
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id ]
      "DELETE FROM view_bindings WHERE branch_id = $1");
  ignore (
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id ]
      "DELETE FROM type_defs WHERE branch_id = $1");
  ignore (
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id ]
      "DELETE FROM function_defs WHERE branch_id = $1");
  ignore (
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id ]
      "DELETE FROM branches WHERE id = $1")

let own_body store ~branch_id ~name : string option =
  match S.lookup_function_on_branch store ~branch_id ~name with
  | Some (b, _, _, _) -> Some b
  | None -> None

(* Refuses whole, then lands verbatim, inherited-only fence, default holes. *)
let test_store_push_refuses_then_lands () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          let shr = "bp_shr_" ^ sfx in
          store.branch_id <- main_id;
          S.persist_function store shr (body_json (lit 1)) "tester";
          let src =
            S.fork_branch store ~parent_id:main_id
              ~owner:"bp_test" ~child_name:("bp_src_" ^ sfx)
          in
          let tgt =
            S.fork_branch store ~parent_id:main_id
              ~owner:"bp_test" ~child_name:("bp_tgt_" ^ sfx)
          in
          (match (src, tgt) with
           | Some src_id, Some tgt_id ->
               store.branch_id <- src_id;
               let entry_body = call "shared" [ call "helper" [ lit 1 ] ] in
               let helper_body = call "shared2" [ lit 2 ] in
               S.persist_function store "entry" (body_json entry_body)
                 "tester";
               S.persist_function store "helper" (body_json helper_body)
                 "tester";
               S.persist_function store "shared2" (body_json (lit 3))
                 "tester";
               S.persist_function store "broken"
                 (body_json (call "absent" [ lit 4 ]))
                 "tester";
               (* self-push refuses *)
               (match
                  BP.push store ~source_id:src_id ~target_id:src_id
                    ~names:[ "entry" ] ~kernel:[]
                with
                | BP.Push_refused _ -> ()
                | BP.Push_ok _ -> Alcotest.fail "self-push must refuse"
                | BP.Push_holes hs ->
                    Alcotest.fail ("self-push holed: " ^ BP.holes_msg hs));
               (* pierced set: refuses WHOLE, target untouched *)
               (match
                  BP.push store ~source_id:src_id ~target_id:tgt_id
                    ~names:[ "entry"; "broken" ] ~kernel:[]
                with
                | BP.Push_holes hs ->
                    Alcotest.(check (list (pair string (list string))))
                      "hole report exact"
                      [ ("broken", [ "absent" ]) ] hs
                | BP.Push_ok _ -> Alcotest.fail "pierced push must hole"
                | BP.Push_refused why ->
                    Alcotest.fail ("pierced push refused: " ^ why));
               Alcotest.(check bool) "target untouched by refused push" true
                 (own_body store ~branch_id:tgt_id ~name:"entry" = None);
               (* clean named push lands entry + closure *)
               (match
                  BP.push store ~source_id:src_id ~target_id:tgt_id
                    ~names:[ "entry" ] ~kernel:[]
                with
                | BP.Push_ok { pushed; auto } ->
                    Alcotest.(check (list string)) "pushed" [ "entry" ] pushed;
                    Alcotest.(check (list string)) "auto"
                      [ "helper"; "shared2" ] auto
                | BP.Push_holes hs ->
                    Alcotest.fail ("clean push holed: " ^ BP.holes_msg hs)
                | BP.Push_refused why ->
                    Alcotest.fail ("clean push refused: " ^ why));
               (* verbatim bodies on the target (raw jsonb text preserved) *)
               Alcotest.(check bool) "entry verbatim" true
                 (own_body store ~branch_id:tgt_id ~name:"entry"
                  = Some (body_json entry_body));
               Alcotest.(check bool) "helper verbatim" true
                 (own_body store ~branch_id:tgt_id ~name:"helper"
                  = Some (body_json helper_body));
               Alcotest.(check bool) "shared2 verbatim" true
                 (own_body store ~branch_id:tgt_id ~name:"shared2"
                  = Some (body_json (lit 3)));
               Alcotest.(check bool) "broken never lands" true
                 (own_body store ~branch_id:tgt_id ~name:"broken" = None);
               (* inherited name: a chain VIEW on the target, not an own row *)
               Alcotest.(check bool) "shared stays a view" true
                 (own_body store ~branch_id:tgt_id ~name:"shared" = None);
               (match S.lookup_function_at store ~branch_id:tgt_id "shared" with
                | Some _ -> ()
                | None -> Alcotest.fail "shared must be chain-visible");
               (* inherited-only name refuses fork-first (no silent skip) *)
               (match
                  BP.push store ~source_id:src_id ~target_id:tgt_id
                    ~names:[ shr ] ~kernel:[]
                with
                | BP.Push_refused why ->
                    Alcotest.(check bool) "fork-first message" true
                      (String.length why > 0)
                | BP.Push_ok _ ->
                    Alcotest.fail "inherited-only push must refuse"
                | BP.Push_holes hs ->
                    Alcotest.fail ("inherited-only holed: " ^ BP.holes_msg hs));
               (* default set = own code (incl. broken) -> holes exactly *)
               (match
                  BP.push store ~source_id:src_id ~target_id:tgt_id
                    ~names:[] ~kernel:[]
                with
                | BP.Push_holes hs ->
                    Alcotest.(check (list (pair string (list string))))
                      "default holes exactly broken"
                      [ ("broken", [ "absent" ]) ] hs
                | BP.Push_ok _ ->
                    Alcotest.fail "default push must hole (broken is own)"
                | BP.Push_refused why ->
                    Alcotest.fail ("default push refused: " ^ why));
               (* cleanup *)
               store.branch_id <- main_id;
               cleanup_fn store ~branch_id:main_id ~name:shr;
               cleanup_branch store ~branch_id:src_id;
               cleanup_branch store ~branch_id:tgt_id
           | _ -> Alcotest.fail "fork failed"))

(* Clean default set: depth-0 own code, name order, auto empty. *)
let test_store_default_push_own_code () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          let shr = "bp_shr2_" ^ sfx in
          store.branch_id <- main_id;
          S.persist_function store shr (body_json (lit 1)) "tester";
          let src =
            S.fork_branch store ~parent_id:main_id
              ~owner:"bp_test" ~child_name:("bp_def_src_" ^ sfx)
          in
          let tgt =
            S.fork_branch store ~parent_id:main_id
              ~owner:"bp_test" ~child_name:("bp_def_tgt_" ^ sfx)
          in
          (match (src, tgt) with
           | Some src_id, Some tgt_id ->
               store.branch_id <- src_id;
               S.persist_function store "entry"
                 (body_json (call "shared" [ call "helper" [ lit 1 ] ]))
                 "tester";
               S.persist_function store "helper"
                 (body_json (call "shared2" [ lit 2 ]))
                 "tester";
               S.persist_function store "shared2" (body_json (lit 3))
                 "tester";
               (match
                  BP.push store ~source_id:src_id ~target_id:tgt_id
                    ~names:[] ~kernel:[]
                with
                | BP.Push_ok { pushed; auto } ->
                    Alcotest.(check (list string)) "default = own code name-ASC"
                      [ "entry"; "helper"; "shared2" ] pushed;
                    Alcotest.(check (list string)) "auto empty (all requested)"
                      [] auto
                | BP.Push_holes hs ->
                    Alcotest.fail ("default push holed: " ^ BP.holes_msg hs)
                | BP.Push_refused why ->
                    Alcotest.fail ("default push refused: " ^ why));
               Alcotest.(check bool) "entry landed" true
                 (own_body store ~branch_id:tgt_id ~name:"entry" <> None);
               (* cleanup *)
               store.branch_id <- main_id;
               cleanup_fn store ~branch_id:main_id ~name:shr;
               cleanup_branch store ~branch_id:src_id;
               cleanup_branch store ~branch_id:tgt_id
           | _ -> Alcotest.fail "fork failed"))

let () =
  Random.self_init ();
  Alcotest.run "branch-push"
    [ ( "closure"
      , [ Alcotest.test_case "auto deps include in discovery order" `Quick
            test_closure_lands_auto_deps
        ; Alcotest.test_case "kernel + special forms are bound" `Quick
            test_closure_kernel_special_bound
        ; Alcotest.test_case "unbound ref refused whole, exact report" `Quick
            test_closure_hole_refused_whole
        ; Alcotest.test_case "requested with no body refused" `Quick
            test_closure_requested_missing_body
        ; Alcotest.test_case "mutual recursion closes" `Quick
            test_closure_mutual_recursion
        ; Alcotest.test_case "requested overlap stays in pushed" `Quick
            test_closure_requested_overlap ] )
    ; ( "db"
      , [ Alcotest.test_case
            "refused whole -> verbatim landing -> view fence -> default holes"
            `Quick test_store_push_refuses_then_lands
        ; Alcotest.test_case "default push = depth-0 own code" `Quick
            test_store_default_push_own_code ] ) ]
