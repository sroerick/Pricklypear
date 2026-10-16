(* T5 ast-merge lifecycle: diverged fn rows through the injected merge3.

   Spec: branching ast-merge (borge/branching.borg, ralph/spec-branching-
   rework) + the plan's lifecycle gate:
     - fork -> both sides edit DISJOINT subtrees of the same define ->
       merge applies BOTH -> the divergence resolves (target holds the
       merged body; the source's pin is unbound, revealing the merged
       body — the pull/unbind lifecycle; post-advance the row classifies
       clean, and a re-merge is a no-op);
     - both sides edit the SAME subtree differently -> conflict reported
       with the exact subtree path, target row unchanged;
     - no hook injected -> pre-T5 behavior (bare-name conflicts).

   The disjoint case doubles as the base-at-base_gen pin: merged =
   app(+,[y;2]) proves the merge base was the FORK body — a head-base
   (main's post-move body) would have produced ours verbatim.

   Needs Postgres (scripts/dev.sh). Skips if unreachable. *)

module S = Pricklypear_image_lib.Store
module A = Pricklypear_lisp.Ast
module AD = Pricklypear_lisp.Ast_diff
module SCP = Pricklypear_image_lib.Session_control_prims

let main_id = "00000000-0000-0000-0000-000000000001"

(* Fresh seed per run: the fixture suffixes below must not collide with
   leftovers from a previous run that crashed before cleanup (a fixed
   default seed would deterministically re-generate the same names). *)
let () = Random.self_init ()

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

(* AST builders. base body = app(+, [x; 1]) — a fixed slot (app.fn) and
   a list child (app.arg[1]) that the reviewed merge3 granularity
   treats as disjoint: fixed-slot edits of a node compose with the
   same node's list-frame edits (loop-#7 fixed-slot exemption pin).
   NOTE the v1 granularity (OPEN SPEC ISSUES ast-merge (a)): edits to
   DIFFERENT ELEMENTS of the SAME LIST (arg[0] vs arg[1]) conflict at
   the container's parent node — list-level diff3 is deferred to v2.
   That granularity is pinned by test_same_list_different_elements. *)
let app f args = A.App (f, args)
let vr s = A.Var s
let li n = A.Lit (A.VInt (Int64.of_int n))

let base_body () = app (vr "+") [ vr "x"; li 1 ]
(* ours edits app.fn only; theirs edits app.arg[1] only — disjoint at
   the shipped granularity. *)
let ours_disjoint () = app (vr "*") [ vr "x"; li 1 ]
let theirs_disjoint () = app (vr "+") [ vr "x"; li 2 ]
let merged_disjoint () = app (vr "*") [ vr "x"; li 2 ]
(* conflict: both sides edit arg[0] (same path, different targets). *)
let ours_conflict () = app (vr "+") [ vr "y"; li 1 ]
let theirs_conflict () = app (vr "+") [ vr "z"; li 1 ]

let body_json e = A.expr_to_body_json e
let hash_of e = AD.ast_hash e

let need_conn store =
  match S.conn_of store with
  | Some conn -> conn
  | None -> Alcotest.fail "no conn"

let fetch_fn_body conn ~branch_id ~name =
  match
    S.exec conn
      ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
      "SELECT body::text FROM function_defs WHERE branch_id = $1 AND name = $2"
  with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some s -> (
          match A.expr_of_body_json s with
          | Some e -> Some e
          | None -> Alcotest.failf "stored body does not parse: %s" s)
      | None -> None)
  | _ -> None

(* Seed base on main, fork child, pin ours on the child, then move main
   to theirs (the diverged fixture: own pin b=ours, parent p=theirs,
   base a=base — all three distinct). Returns (child_id, fname). *)
let setup_diverged (store : S.t) ~prefix ~base_e ~ours_e ~theirs_e =
  let suffix = string_of_int (Random.int 1_000_000) in
  let child_name = prefix ^ "_child_" ^ suffix in
  let fname = prefix ^ "_fn_" ^ suffix in
  store.branch_id <- main_id;
  S.persist_function store fname (body_json base_e) "tester";
  match S.fork_branch store ~parent_id:main_id ~owner:"amg_test" ~child_name with
  | None -> Alcotest.fail "fork failed"
  | Some child_id ->
      store.branch_id <- child_id;
      S.persist_function store fname (body_json ours_e) "tester";
      store.branch_id <- main_id;
      Unix.sleepf 0.05;
      S.persist_function store fname (body_json theirs_e) "tester";
      (child_id, fname)

let cleanup (store : S.t) ~(child_id : string) ~(fname : string) =
  let _ = S.delete_function_on_branch store ~branch_id:main_id ~name:fname in
  let _ =
    S.delete_function_on_branch store ~branch_id:child_id ~name:fname
  in
  ()

let contains_sub s sub =
  let ls = String.length s and ln = String.length sub in
  let rec go i = i + ln <= ls && (String.sub s i ln = sub || go (i + 1)) in
  go 0

let find_status_row rows fname =
  List.find_opt (fun r -> r.S.st_name = fname) rows

(* ---- 1. disjoint subtree edits: merge applies BOTH ---- *)
let test_disjoint_merge_applies_both () =
  with_store (fun store ->
      let conn = need_conn store in
      let child_id, fname =
        setup_diverged store ~prefix:"amg_d" ~base_e:(base_body ())
          ~ours_e:(ours_disjoint ()) ~theirs_e:(theirs_disjoint ())
      in
      let r =
        S.merge_branch store ~source_id:child_id ~target_id:main_id
          ~merge3:SCP.merge3_hook
      in
      let res = match r with Some res -> res | None -> Alcotest.fail "merge failed" in
      if res.count <> 1 || res.conflicts <> [] then
        Alcotest.failf "disjoint merge did not apply cleanly: count=%d conflicts=[%s]"
          res.count (String.concat " | " res.conflicts);
      Alcotest.(check int) "diverged row merged cleanly (count=1)" 1 res.count;
      Alcotest.(check bool) "no conflicts" true (res.conflicts = []);
      (* Target holds BOTH edits. Since merged != ours, the base provably
         was the fork body — not main's head. *)
      let target_body =
        match fetch_fn_body conn ~branch_id:main_id ~name:fname with
        | Some b -> b
        | None -> Alcotest.fail "target body missing"
      in
      Alcotest.(check bool) "target = merged (both edits)" true
        (hash_of target_body = hash_of (merged_disjoint ()));
      (* Source: pin unbound — the resolved body subsumes the pin (pull/
         unbind lifecycle; a kept pin would re-classify ahead post-
         advance and a re-merge would clobber theirs' subtree). *)
      Alcotest.(check bool) "source pin unbound" true
        (fetch_fn_body conn ~branch_id:child_id ~name:fname = None);
      let rows = S.branch_status store ~branch_id:child_id in
      let row =
        match find_status_row rows fname with
        | Some row -> row
        | None -> Alcotest.fail "status row missing"
      in
      Alcotest.(check bool)
        "post-merge row resolves clean (pin subsumed, base advanced)" true
        (S.st_status_s row.S.st_status = "clean" && not row.S.st_own);
      (* Re-merge is a no-op. *)
      let res2 =
        match
          S.merge_branch store ~source_id:child_id ~target_id:main_id
            ~merge3:SCP.merge3_hook
        with
        | Some res2 -> res2
        | None -> Alcotest.fail "re-merge failed"
      in
      Alcotest.(check int) "re-merge applies nothing" 0 res2.count;
      Alcotest.(check bool) "re-merge no conflicts" true (res2.conflicts = []);
      cleanup store ~child_id ~fname)

(* ---- 2. same subtree, different edits: conflict at the exact path ---- *)
let test_same_subtree_conflicts () =
  with_store (fun store ->
      let conn = need_conn store in
      let child_id, fname =
        setup_diverged store ~prefix:"amg_c" ~base_e:(base_body ())
          ~ours_e:(ours_conflict ()) ~theirs_e:(theirs_conflict ())
      in
      let res =
        match
          S.merge_branch store ~source_id:child_id ~target_id:main_id
            ~merge3:SCP.merge3_hook
        with
        | Some res -> res
        | None -> Alcotest.fail "merge failed"
      in
      Alcotest.(check int) "conflicted diverged row not applied" 0 res.count;
      Alcotest.(check int) "exactly one conflict line" 1
        (List.length res.conflicts);
      let c = List.hd res.conflicts in
      Alcotest.(check bool) "conflict names the define" true
        (String.starts_with ~prefix:(fname ^ ": CONFLICT at ") c);
      Alcotest.(check bool) "conflict path is the exact arg slot" true
        (contains_sub c "app.arg[0]");
      (* Target row untouched BY THE MERGE: it still holds the parent's
         own post-fork move (theirs) — the merge applied nothing. *)
      let target_body =
        match fetch_fn_body conn ~branch_id:main_id ~name:fname with
        | Some b -> b
        | None -> Alcotest.fail "target body missing"
      in
      Alcotest.(check bool) "target body unchanged" true
        (hash_of target_body = hash_of (theirs_conflict ()));
        (* Source still holds ours' body (own pin intact). 6998a14e: an
           UNRESOLVED merge must NOT advance the fork-point base - the row
           STAYS diverged, so the next merge re-reports the conflict instead
           of silently landing the source body through the ahead-copy loop
           (jj/git semantics: the merge base moves only on a resolved
           merge). *)
        let src_body =
          match fetch_fn_body conn ~branch_id:child_id ~name:fname with
          | Some b -> b
          | None -> Alcotest.fail "source body missing"
        in
        Alcotest.(check bool) "source keeps ours' body" true
          (hash_of src_body = hash_of (ours_conflict ()));
        let rows = S.branch_status store ~branch_id:child_id in
        let row =
          match find_status_row rows fname with
          | Some row -> row
          | None -> Alcotest.fail "status row missing"
        in
        Alcotest.(check bool) "row stays diverged (base NOT advanced)" true
          (S.st_status_s row.S.st_status = "diverged" && row.S.st_own);
        (* The card's two-merge repro: re-merge with NO new edits must
           re-report the conflict, not silently land the source body. *)
        let res2 =
          match
            S.merge_branch store ~source_id:child_id ~target_id:main_id
              ~merge3:SCP.merge3_hook
          with
          | Some res2 -> res2
          | None -> Alcotest.fail "re-merge failed"
        in
        Alcotest.(check int) "re-merge applies nothing" 0 res2.count;
        Alcotest.(check int) "re-merge re-reports the conflict" 1
          (List.length res2.conflicts);
        let target_body2 =
          match fetch_fn_body conn ~branch_id:main_id ~name:fname with
          | Some b -> b
          | None -> Alcotest.fail "target body missing"
        in
        Alcotest.(check bool) "target body still theirs (no silent land)" true
          (hash_of target_body2 = hash_of (theirs_conflict ()));
        cleanup store ~child_id ~fname)

(* ---- 2b. same LIST, different elements: v1 conflicts (granularity pin) ----
   arg[0] vs arg[1] are different elements of the same args list; the
   reviewed merge3 (LIST FRAME RULE, list-level diff3 deferred) conflicts
   both-sides-edited-same-list at the container's parent node. Pinned so
   a future list-diff3 upgrade flips this test DELIBERATELY. *)
let test_same_list_different_elements () =
  with_store (fun store ->
      let child_id, fname =
        setup_diverged store ~prefix:"amg_sl" ~base_e:(base_body ())
          ~ours_e:(app (vr "+") [ vr "y"; li 1 ])
          ~theirs_e:(theirs_disjoint ())
      in
      let res =
        match
          S.merge_branch store ~source_id:child_id ~target_id:main_id
            ~merge3:SCP.merge3_hook
        with
        | Some res -> res
        | None -> Alcotest.fail "merge failed"
      in
      Alcotest.(check int) "same-list row not applied" 0 res.count;
      Alcotest.(check bool) "exactly one conflict line" true
        (List.length res.conflicts = 1);
      let c = List.hd res.conflicts in
      (* reported at the container's parent NODE path (empty steps = root
         node), owning the args list children. *)
      Alcotest.(check bool) "conflict at the container's parent node" true
        (String.starts_with ~prefix:(fname ^ ": CONFLICT at : ") c);
      cleanup store ~child_id ~fname)

(* ---- 3. no hook injected: pre-T5 behavior (bare-name conflicts) ---- *)
let test_no_hook_keeps_name_conflicts () =
  with_store (fun store ->
      let conn = need_conn store in
      let child_id, fname =
        setup_diverged store ~prefix:"amg_l" ~base_e:(base_body ())
          ~ours_e:(ours_conflict ()) ~theirs_e:(theirs_conflict ())
      in
      let res =
        match S.merge_branch store ~source_id:child_id ~target_id:main_id with
        | Some res -> res
        | None -> Alcotest.fail "merge failed"
      in
      Alcotest.(check int) "legacy: diverged not applied" 0 res.count;
      Alcotest.(check bool) "legacy: bare-name conflict" true
        (res.conflicts = [ fname ]);
      let target_body =
        match fetch_fn_body conn ~branch_id:main_id ~name:fname with
        | Some b -> b
        | None -> Alcotest.fail "target body missing"
      in
      Alcotest.(check bool) "legacy: target untouched by merge" true
        (hash_of target_body = hash_of (theirs_conflict ()));
      cleanup store ~child_id ~fname)

let () =
  let open Alcotest in
  run "ast_merge_store"
    [
      ( "lifecycle",
        [
          test_case "disjoint subtree edits merge cleanly" `Quick
            test_disjoint_merge_applies_both;
          test_case "same-subtree conflict at the exact path" `Quick
            test_same_subtree_conflicts;
          test_case "same-list different elements conflict (v1 granularity)" `Quick
            test_same_list_different_elements;
          test_case "no hook keeps legacy name conflicts" `Quick
            test_no_hook_keeps_name_conflicts;
        ] );
    ]
