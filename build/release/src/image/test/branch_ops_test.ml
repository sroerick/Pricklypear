(* Branch ops prims' store layer (branching-ast-ergonomics branch-ops
   status/pull rows): Store.status_report (the branch/status fold) and
   Store.pull_behind_pins (the branch/pull mutation).
   Needs Postgres (scripts/dev.sh). Skips if unreachable.

   Verifies:
   - status_report agrees with Store.branch_status on a fixture holding
     ALL SIX statuses (naive recount oracle; counts are the store's own
     classification folded, never re-derived; diverged worklist exact;
     pins = own ∨ non-clean; the clean non-own bulk stays out).
   - pull on a mixed branch: pulled/skipped EXACT; the stale behind PIN
     is UNBOUND (post-pull row own=false ∧ b=p — a body copy would leave
     own=true ∧ synced, the pin-hygiene violation this test kills);
     behind-without-pin rows are never adopted (no pin fabricated);
     diverged pins untouched; ahead/ahead-new/synced/clean untouched;
     a second pull is a no-op (pull set empty).
   - pull per kind (fn + type + route): an all-behind branch returns to
     inheriting the parent's live versions on every kind (own row gone
     from the scaffolding table; resolved hash = parent's). *)

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

let body_json_of_int n =
  Pricklypear_lisp.Ast.expr_to_body_json (expr_of_int n)

let status_of rows ~kind ~name =
  match
    List.find_opt
      (fun (r : S.branch_status_row) ->
        r.S.st_kind = kind && r.S.st_name = name)
      rows
  with
  | Some r -> Some r
  | None -> None

let cleanup_fn store ~branch_id ~name =
  ignore (S.delete_function_on_branch store ~branch_id ~name)

(* tree is truth: deleting the node fires the 009 reverse mirror, which
   cleans the scaffolding row (same shape as Tree_store.delete_path). *)
let cleanup_typed store ~branch_id ~path =
  ignore (
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string path ]
      "DELETE FROM tree_nodes WHERE path = $1::ltree")

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

(* Own-row existence per kind, straight from the scaffolding tables —
   the pin, independent of the status read. Empty result = no pin; a DB
   error propagates (a silent false would fake a pass). *)
let fn_pin_exists store ~branch_id ~name =
  match
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
      "SELECT 1 FROM function_defs WHERE branch_id = $1 AND name = $2"
  with
  | [] -> false
  | _ -> true

let route_pin_exists store ~branch_id ~route =
  match
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_string route ]
      "SELECT 1 FROM view_bindings WHERE branch_id = $1 AND route = $2"
  with
  | [] -> false
  | _ -> true

let type_pin_exists store ~branch_id ~name =
  match
    S.exec
      (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
      "SELECT 1 FROM type_defs WHERE branch_id = $1 AND name = $2"
  with
  | [] -> false
  | _ -> true

(** The branch/status fold: Store.status_report must AGREE with the
    store's own classification. Fixture holds all six statuses at once
    (the branch_status_test lifecycle shapes): clean (inherited, parent
    unmoved), ahead-new (fresh child fn), ahead (parent-defined fn the
    child moved), synced (both sides equal), behind-with-pin (child pins
    the OLD base while the parent moved), behind-no-pin (parent moved,
    child inherits live), diverged (both moved). *)
let test_status_report_matches_store () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "bops_rep_child_" ^ suffix in
          let clean_fn = "bops_rep_clean_" ^ suffix in
          let new_fn = "bops_rep_new_" ^ suffix in
          let ahead_fn = "bops_rep_ahead_" ^ suffix in
          let sync_fn = "bops_rep_sync_" ^ suffix in
          let behindp_fn = "bops_rep_bhp_" ^ suffix in
          let behindnp_fn = "bops_rep_bnp_" ^ suffix in
          let div_fn = "bops_rep_div_" ^ suffix in
          store.branch_id <- main_id;
          (* parent-side defs BEFORE the fork become the fork base *)
          S.persist_function store clean_fn (body_json_of_int 1) "tester";
          S.persist_function store ahead_fn (body_json_of_int 1) "tester";
          S.persist_function store sync_fn (body_json_of_int 1) "tester";
          S.persist_function store behindp_fn (body_json_of_int 1) "tester";
          S.persist_function store behindnp_fn (body_json_of_int 1) "tester";
          S.persist_function store div_fn (body_json_of_int 1) "tester";
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"bops_test"
               ~child_name
           with
           | None -> Alcotest.fail "fork failed"
           | Some child_id ->
               (* child moves first: ahead (differs from base), synced
                  (parent will catch up), diverged *)
               store.branch_id <- child_id;
               S.persist_function store ahead_fn (body_json_of_int 7) "tester";
               S.persist_function store sync_fn (body_json_of_int 7) "tester";
               S.persist_function store div_fn (body_json_of_int 7) "tester";
               S.persist_function store new_fn (body_json_of_int 9) "tester";
               (* parent moves after the fork: synced catch-up, behinds,
                  diverged counter-move *)
               store.branch_id <- main_id;
               Unix.sleepf 0.02;
               S.persist_function store sync_fn (body_json_of_int 7) "tester";
               S.persist_function store behindp_fn (body_json_of_int 2)
                 "tester";
               S.persist_function store behindnp_fn (body_json_of_int 2)
                 "tester";
               S.persist_function store div_fn (body_json_of_int 2) "tester";
               (* NOW the child pins the OLD base of behindp_fn: the
                  inherited body is the parent's NEW one (2), so the
                  base-body define DIFFERS from it and writes a real pin
                  (pin hygiene suppresses identical defines) — b = a,
                  p moved ⇒ the behind PIN. Pinning it before the parent
                  move would be suppressed (no-pin behind instead). *)
               store.branch_id <- child_id;
               S.persist_function store behindp_fn (body_json_of_int 1)
                 "tester";
               let rows = S.branch_status store ~branch_id:child_id in
               let rep = S.status_report rows in
               (* every row classified; counts = a naive independent
                  recount of the SAME rows (the store is the ONE
                  classifier; the report only folds) *)
               let naive s =
                 List.length
                   (List.filter
                      (fun (r : S.branch_status_row) ->
                        String.equal (S.st_status_s r.S.st_status) s)
                      rows)
               in
               Alcotest.(check int) "six statuses counted"
                 (List.length rep.S.counts) 6;
               List.iter
                 (fun (s, n) ->
                   Alcotest.(check int) ("count " ^ s) n (naive s))
                 rep.S.counts;
               Alcotest.(check int) "counts sum = row count"
                 (List.fold_left (fun acc (_, n) -> acc + n) 0 rep.S.counts)
                 (List.length rows);
               (* per-status fixture pins: all six really present *)
               let st_of name =
                 match status_of rows ~kind:S.DiffFn ~name with
                 | Some r -> S.st_status_s r.S.st_status
                 | None -> "MISSING"
               in
               Alcotest.(check string) "clean fixture" (st_of clean_fn) "clean";
               Alcotest.(check string) "ahead-new fixture" (st_of new_fn)
                 "ahead-new";
               Alcotest.(check string) "ahead fixture" (st_of ahead_fn) "ahead";
               Alcotest.(check string) "synced fixture" (st_of sync_fn) "synced";
               Alcotest.(check string) "behind-pin fixture" (st_of behindp_fn)
                 "behind";
               (match status_of rows ~kind:S.DiffFn ~name:behindp_fn with
               | Some r ->
                   Alcotest.(check bool)
                     "behind-pin fixture is an OWN pin" true r.S.st_own
               | None -> Alcotest.fail "behindp row missing");
               Alcotest.(check string) "behind-no-pin fixture"
                 (st_of behindnp_fn) "behind";
               Alcotest.(check string) "diverged fixture" (st_of div_fn)
                 "diverged";
               (* worklist = exactly the diverged rows, store order *)
               Alcotest.(check string) "worklist exact"
                 (String.concat ","
                    (List.map
                       (fun (r : S.branch_status_row) ->
                         S.st_status_s r.S.st_status ^ ":" ^ r.S.st_name)
                       rep.S.worklist))
                 (String.concat ","
                    (List.map
                       (fun (r : S.branch_status_row) ->
                         S.st_status_s r.S.st_status ^ ":" ^ r.S.st_name)
                       (List.filter
                          (fun (r : S.branch_status_row) ->
                            r.S.st_status = S.SDiverged)
                          rows)));
               (* pins = own ∨ non-clean; the clean non-own bulk stays out *)
               let pin_names =
                 List.map (fun (r : S.branch_status_row) -> r.S.st_name)
                   rep.S.pins
               in
               List.iter
                 (fun (r : S.branch_status_row) ->
                   if r.S.st_own || r.S.st_status <> S.SClean then
                     Alcotest.(check bool) "own/non-clean row in pins" true
                       (List.mem r.S.st_name pin_names)
                   else
                     Alcotest.(check bool) "clean non-own row NOT in pins"
                       false (List.mem r.S.st_name pin_names))
                 rows;
               (* the diverged name is own AND in the worklist *)
               Alcotest.(check bool) "diverged in worklist" true
                 (List.mem div_fn
                    (List.map
                       (fun (r : S.branch_status_row) -> r.S.st_name)
                       rep.S.worklist));
               (* cleanup: parent fns + the child branch *)
               store.branch_id <- main_id;
               List.iter
                 (fun n -> cleanup_fn store ~branch_id:main_id ~name:n)
                 [ clean_fn; ahead_fn; sync_fn; behindp_fn; behindnp_fn;
                   div_fn ];
               cleanup_branch store ~branch_id:child_id;
               ()))

(** The branch/pull mutation on a mixed branch: pulled/skipped exact;
    the stale behind pin is UNBOUND (own=false ∧ b=p after — a body copy
    would leave own=true ∧ synced: pin hygiene); behind-no-pin never
    adopted (no pin fabricated); diverged untouched; a second pull is a
    no-op. Post-pull rows classify behind-no-pin (base_gen is NOT
    advanced — advancing it would reclassify diverged pins as ahead and
    manufacture auto-applyable merges); the adoption proof is own=false
    ∧ b=p, i.e. the name now resolves to the parent's live version. *)
let test_pull_mixed_branch () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "bops_pull_child_" ^ suffix in
          let stale_fn = "bops_pull_stale_" ^ suffix in
          let behindnp_fn = "bops_pull_bnp_" ^ suffix in
          let div_fn = "bops_pull_div_" ^ suffix in
          let ahead_fn = "bops_pull_ahead_" ^ suffix in
          store.branch_id <- main_id;
          S.persist_function store stale_fn (body_json_of_int 1) "tester";
          S.persist_function store behindnp_fn (body_json_of_int 1) "tester";
          S.persist_function store div_fn (body_json_of_int 1) "tester";
          S.persist_function store ahead_fn (body_json_of_int 1) "tester";
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"bops_test"
               ~child_name
           with
           | None -> Alcotest.fail "fork failed"
           | Some child_id ->
               store.branch_id <- child_id;
               (* real diffs: diverged + ahead pins (parent still at the
                  base, so these are ahead; the parent's counter-move on
                  div_fn below makes it diverged) *)
               S.persist_function store div_fn (body_json_of_int 7) "tester";
               S.persist_function store ahead_fn (body_json_of_int 7) "tester";
               store.branch_id <- main_id;
               Unix.sleepf 0.02;
               S.persist_function store stale_fn (body_json_of_int 2)
                 "tester";
               S.persist_function store behindnp_fn (body_json_of_int 2)
                 "tester";
               S.persist_function store div_fn (body_json_of_int 2) "tester";
               (* NOW the child pins the stale base of stale_fn: the
                  inherited body is the parent's NEW one (2), so the
                  base-body define differs from it → a real pin with
                  b = a (behind OWN pin). Pinning before the parent move
                  would be suppressed by pin hygiene (no-pin behind). *)
               store.branch_id <- child_id;
               S.persist_function store stale_fn (body_json_of_int 1)
                 "tester";
               let rows = S.branch_status store ~branch_id:child_id in
               (match
                  ( status_of rows ~kind:S.DiffFn ~name:stale_fn
                  , status_of rows ~kind:S.DiffFn ~name:behindnp_fn
                  , status_of rows ~kind:S.DiffFn ~name:div_fn )
               with
               | ( Some stale, Some bnp, Some dv ) ->
                   Alcotest.(check bool) "fixture: stale pin is own-behind"
                     true (stale.S.st_own && stale.S.st_status = S.SBehind);
                   Alcotest.(check bool) "fixture: bnp is no-pin behind" true
                     ((not bnp.S.st_own) && bnp.S.st_status = S.SBehind);
                   Alcotest.(check bool) "fixture: diverged own" true
                     (dv.S.st_own && dv.S.st_status = S.SDiverged)
               | _ -> Alcotest.fail "fixture rows missing");
               (* THE PULL *)
               let pulled, skipped =
                 S.pull_behind_pins store ~branch_id:child_id
               in
               Alcotest.(check bool) "pulled = exactly the stale pin" true
                 (pulled = [ ("fn", stale_fn) ]);
               Alcotest.(check bool) "skipped = exactly the diverged pin" true
                 (skipped = [ ("fn", div_fn) ]);
               (* unbind discriminator: pin GONE (own=false), name now
                  resolves to the parent's live version (b = p) *)
               let rows = S.branch_status store ~branch_id:child_id in
               (match status_of rows ~kind:S.DiffFn ~name:stale_fn with
               | Some r ->
                   Alcotest.(check bool) "pulled pin unbound (own=false)" true
                     (not r.S.st_own);
                   Alcotest.(check bool) "pulled name adopts parent (b=p)"
                     true (r.S.st_branch_hash = r.S.st_parent_hash);
                   Alcotest.(check bool) "no fn row left on child" false
                     (fn_pin_exists store ~branch_id:child_id
                        ~name:stale_fn)
               | None -> Alcotest.fail "pulled row vanished");
               (* behind-no-pin: untouched, no pin fabricated (a naive
                  copy-the-parent adopter would have pinned it) *)
               (match status_of rows ~kind:S.DiffFn ~name:behindnp_fn with
               | Some r ->
                   Alcotest.(check bool) "no-pin behind stays pinless" true
                     (not r.S.st_own);
                   Alcotest.(check bool) "no-pin behind still behind" true
                     (r.S.st_status = S.SBehind)
               | None -> Alcotest.fail "bnp row vanished");
               Alcotest.(check bool) "no-pin behind: no row on child" false
                 (fn_pin_exists store ~branch_id:child_id ~name:behindnp_fn);
               (* diverged untouched: pin intact, own body unchanged *)
               (match status_of rows ~kind:S.DiffFn ~name:div_fn with
               | Some r ->
                   Alcotest.(check bool) "diverged still own" true r.S.st_own;
                   Alcotest.(check bool) "diverged still diverged" true
                     (r.S.st_status = S.SDiverged)
               | None -> Alcotest.fail "diverged row vanished");
               Alcotest.(check bool) "diverged body untouched" true
                 (fn_pin_exists store ~branch_id:child_id ~name:div_fn);
               (* ahead untouched *)
               (match status_of rows ~kind:S.DiffFn ~name:ahead_fn with
               | Some r ->
                   Alcotest.(check bool) "ahead still own+ahead" true
                     (r.S.st_own && r.S.st_status = S.SAhead)
               | None -> Alcotest.fail "ahead row vanished");
               (* idempotent: second pull is a no-op *)
               let pulled2, skipped2 =
                 S.pull_behind_pins store ~branch_id:child_id
               in
               Alcotest.(check bool) "second pull: nothing to pull" true
                 (pulled2 = []);
               Alcotest.(check bool) "second pull: diverged still skipped" true
                 (skipped2 = [ ("fn", div_fn) ]);
               (* cleanup *)
               store.branch_id <- main_id;
               List.iter
                 (fun n -> cleanup_fn store ~branch_id:main_id ~name:n)
                 [ stale_fn; behindnp_fn; div_fn; ahead_fn ];
               cleanup_branch store ~branch_id:child_id;
               ()))

(** Pull per kind: fn + type + route behind pins on one all-behind
    branch — every kind adopts the parent's live version via the unbind
    path (own row GONE from the scaffolding table, b = p). *)
let test_pull_per_kind () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "bops_kind_child_" ^ suffix in
          let fn_name = "bops_kind_fn_" ^ suffix in
          let ty_name = "bops_kind_ty_" ^ suffix in
          let rt = "/bops-kind-rt-" ^ suffix in
          store.branch_id <- main_id;
          S.persist_function store fn_name (body_json_of_int 1) "tester";
          S.persist_type store ~name:ty_name ~fields:[ "a" ] ~author:"tester";
          S.register_route store ~route:rt ~handler:"h1" ();
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"bops_test"
               ~child_name
           with
           | None -> Alcotest.fail "fork failed"
           | Some child_id ->
               (* parent moves all three kinds FIRST; then the child pins
                  each kind at the BASE value — inherited (parent's new
                  body) differs from the base body, so every pin is real
                  (fn pin hygiene would suppress a pre-move identical
                  define), and b = a ⇒ behind. *)
               store.branch_id <- main_id;
               Unix.sleepf 0.02;
               S.persist_function store fn_name (body_json_of_int 2) "tester";
               S.persist_type store ~name:ty_name
                 ~fields:[ "a"; "b" ] ~author:"tester";
               S.register_route store ~route:rt ~handler:"h2" ();
               store.branch_id <- child_id;
               S.persist_function store fn_name (body_json_of_int 1) "tester";
               S.persist_type store ~name:ty_name ~fields:[ "a" ]
                 ~author:"tester";
               S.register_route store ~route:rt ~handler:"h1" ();
               (* fixture: all three own-behind on the child *)
               store.branch_id <- child_id;
               let rows = S.branch_status store ~branch_id:child_id in
               (match
                  ( status_of rows ~kind:S.DiffFn ~name:fn_name
                  , status_of rows ~kind:S.DiffType ~name:ty_name
                  , status_of rows ~kind:S.DiffRoute ~name:rt )
               with
               | ( Some fr, Some tr, Some rr ) ->
                   Alcotest.(check bool) "fixture: fn own-behind" true
                     (fr.S.st_own && fr.S.st_status = S.SBehind);
                   Alcotest.(check bool) "fixture: type own-behind" true
                     (tr.S.st_own && tr.S.st_status = S.SBehind);
                   Alcotest.(check bool) "fixture: route own-behind" true
                     (rr.S.st_own && rr.S.st_status = S.SBehind)
               | _ -> Alcotest.fail "fixture rows missing");
               Alcotest.(check bool) "fixture: route pin exists" true
                 (route_pin_exists store ~branch_id:child_id ~route:rt);
               let pulled, skipped =
                 S.pull_behind_pins store ~branch_id:child_id
               in
               Alcotest.(check bool) "skipped empty (all behind, none diverged)"
                 true (skipped = []);
               Alcotest.(check bool) "pulled = all three kinds" true
                 (List.sort compare pulled =
                  [ ("fn", fn_name); ("route", rt); ("type", ty_name) ]);
               (* post-pull: every kind unbound — own row gone from the
                  scaffolding/table, status row pinless, b = p *)
               let rows = S.branch_status store ~branch_id:child_id in
               (match status_of rows ~kind:S.DiffFn ~name:fn_name with
               | Some r ->
                   Alcotest.(check bool) "fn: unbound" true (not r.S.st_own);
                   Alcotest.(check bool) "fn: b=p" true
                     (r.S.st_branch_hash = r.S.st_parent_hash)
               | None -> Alcotest.fail "fn row vanished");
               (match status_of rows ~kind:S.DiffType ~name:ty_name with
               | Some r ->
                   Alcotest.(check bool) "type: unbound" true (not r.S.st_own);
                   Alcotest.(check bool) "type: b=p" true
                     (r.S.st_branch_hash = r.S.st_parent_hash)
               | None -> Alcotest.fail "type row vanished");
               Alcotest.(check bool) "type: scaffolding row gone" false
                 (type_pin_exists store ~branch_id:child_id ~name:ty_name);
               (match status_of rows ~kind:S.DiffRoute ~name:rt with
               | Some r ->
                   Alcotest.(check bool) "route: unbound" true (not r.S.st_own);
                   Alcotest.(check bool) "route: b=p" true
                     (r.S.st_branch_hash = r.S.st_parent_hash)
               | None -> Alcotest.fail "route row vanished");
               Alcotest.(check bool) "route: view_binding row gone" false
                 (route_pin_exists store ~branch_id:child_id ~route:rt);
               (* cleanup *)
               store.branch_id <- main_id;
               cleanup_fn store ~branch_id:main_id ~name:fn_name;
               cleanup_typed store ~branch_id:main_id
                 ~path:(Pricklypear_image_lib.Tree_store.type_path ~branch_id:main_id
                          ~name:ty_name);
               cleanup_typed store ~branch_id:main_id
                 ~path:(Pricklypear_image_lib.Tree_store.route_path ~branch_id:main_id
                          ~route:rt);
               cleanup_branch store ~branch_id:child_id;
               ()))

let () =
  Random.self_init ();
  Alcotest.run "branch-ops"
    [ ( "db"
      , [ Alcotest.test_case "status report matches store" `Quick
            test_status_report_matches_store
        ; Alcotest.test_case "pull mixed branch" `Quick test_pull_mixed_branch
        ; Alcotest.test_case "pull per kind" `Quick test_pull_per_kind
        ] )
    ]
