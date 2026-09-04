(** Branch status (branching.branch-status): git-style
    clean/ahead-new/ahead/behind/diverged/synced classification over the
    tree-time substrate (tree_versions) keyed on base_gen.
    Needs Postgres (scripts/dev.sh). Skips if unreachable.

    Verifies: fork records base_gen = parent head gen; empty fork reads
    clean; own new fn reads ahead-new; post-promote pin reads synced;
    parent-only move reads behind; both-sides move reads diverged; merge
    upserts only ahead/ahead-new and ADVANCES base_gen (diverged is
    conflict-only — never auto-applied — and reclassifies ahead vs the
    advanced base); status agrees with diff_branches head-vs-head on the
    diverged fixture. *)

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

let status_of rows ~name =
  match
    List.find_opt
      (fun (r : S.branch_status_row) ->
        r.S.st_kind = S.DiffFn && r.S.st_name = name)
      rows
  with
  | Some r -> Some r.S.st_status
  | None -> None

let base_gen_of store ~branch_id =
  match
    S.exec (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id ]
      "SELECT COALESCE(base_gen, 0)::text FROM branches WHERE id = $1"
  with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some s -> Int64.of_string s
      | None -> 0L)
  | _ -> 0L

let parent_gen_of store ~branch_id =
  match
    S.exec (Option.get (S.conn_of store))
      ~params:[ Pgx.Value.of_string branch_id ]
      "SELECT COALESCE((SELECT generation FROM branch_generations \
                        WHERE branch_id = parent_id), 0)::text \
       FROM branches WHERE id = $1"
  with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some s -> Int64.of_string s
      | None -> 0L)
  | _ -> 0L

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

(** The full lifecycle: fork -> ahead-new -> diverged -> merge advances
    base_gen -> the diverged name reclassifies ahead (conflict-only
    merge: diverged is never auto-applied; the child keeps its own
    change vs the advanced base until a human resolves). Also covers
    clean (empty fork), behind (parent moves, child does not),
    synced-by-promote, and the diff_branches equivalence on the
    diverged fixture. *)
let test_status_lifecycle () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "bst_child_" ^ suffix in
          let shared = "bst_shared_" ^ suffix in
          let newfn = "bst_new_" ^ suffix in
          store.branch_id <- main_id;
          (* main holds a shared def BEFORE the fork *)
          S.persist_function store shared (expr_of_int 1) "tester";
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"bst_test"
               ~child_name
           with
           | None -> Alcotest.fail "fork failed"
           | Some child_id ->
               (* S1: fork records base_gen = parent head gen *)
               Alcotest.(check bool) "fork sets base_gen = parent head gen"
                 true
                 (Int64.compare (base_gen_of store ~branch_id:child_id)
                    (parent_gen_of store ~branch_id:child_id)
                 = 0);
               (* clean: empty fork, shared def inherited, parent unmoved *)
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "empty fork: shared fn clean" true
                 (status_of rows ~name:shared = Some S.SClean);
               (* ahead-new: child adds a fresh fn *)
               store.branch_id <- child_id;
               S.persist_function store newfn (expr_of_int 7) "tester";
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "own new fn = ahead-new" true
                 (status_of rows ~name:newfn = Some S.SAheadNew);
               (* diverged: both sides move the shared fn *)
               S.persist_function store shared (expr_of_int 2) "tester";
               store.branch_id <- main_id;
               Unix.sleepf 0.02;
               S.persist_function store shared (expr_of_int 3) "tester";
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "both moved = diverged" true
                 (status_of rows ~name:shared = Some S.SDiverged);
               (* equivalence: diff_branches reports the same divergence
                   head-vs-head (shared modified on both sides) *)
               let drows =
                 S.diff_branches store ~from_id:child_id ~to_id:main_id
               in
               Alcotest.(check bool) "diff agrees: shared modified" true
                 (List.exists
                    (fun (r : S.branch_diff_row) ->
                      r.S.diff_name = shared && r.S.diff_status = S.DiffModified)
                    drows);
               (* behind: parent moves ANOTHER fn the child only inherits *)
               let behind_fn = "bst_behind_" ^ suffix in
               S.persist_function store behind_fn (expr_of_int 1) "tester";
               Unix.sleepf 0.02;
               S.persist_function store behind_fn (expr_of_int 2) "tester";
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "parent-only move = behind" true
                 (status_of rows ~name:behind_fn = Some S.SBehind);
               (* synced-by-promote: promote pins it; base_gen NOT advanced *)
               ignore (
                 S.promote_functions store ~source_id:child_id
                   ~target_id:main_id ~names:[ newfn ]);
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "post-promote pin = synced" true
                 (status_of rows ~name:newfn = Some S.SSynced);
               (* merge child -> main: only ahead/ahead-new names upsert;
                  base_gen advances; the diverged name is conflict-only
                  (never auto-applied) and reclassifies ahead vs the new
                  base — the child still holds its own change until a
                  human resolves (re-define + re-merge). *)
               let base_before = base_gen_of store ~branch_id:child_id in
               (match
                  S.merge_branch store ~source_id:child_id
                    ~target_id:main_id
                with
                | None -> Alcotest.fail "merge failed"
                | Some res ->
                    Alcotest.(check bool) "merge names diverged" true
                      (List.mem shared res.S.conflicts));
               Alcotest.(check bool) "merge advanced base_gen" true
                 (Int64.compare (base_gen_of store ~branch_id:child_id)
                    base_before
                 > 0);
               (* target kept its own body — diverged was NOT applied *)
               (match
                  S.lookup_function_on_branch store ~branch_id:main_id
                    ~name:shared
                with
               | Some (body, _, _, _) ->
                   Alcotest.(check bool) "target not overwritten" true
                     (body = expr_of_int 3)
               | None -> Alcotest.fail "target row vanished");
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "post-merge diverged = ahead" true
                 (status_of rows ~name:shared = Some S.SAhead);
               Alcotest.(check bool) "post-merge pin still synced" true
                 (status_of rows ~name:newfn = Some S.SSynced);
               (* cleanup *)
               cleanup_fn store ~branch_id:main_id ~name:shared;
               cleanup_fn store ~branch_id:main_id ~name:behind_fn;
               cleanup_branch store ~branch_id:child_id;
               ()))

(** branching.pin-hygiene: no diff => no pin. Defining the parent's
    exact body on a child writes NO row; re-defining a divergent pin
    back to the parent's body UNPINS it (parent revealed), returning
    the name to clean inheritance. system/main always writes (root). *)
let test_define_suppression () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "bst_sup_" ^ suffix in
          let fn = "bst_sup_fn_" ^ suffix in
          store.branch_id <- main_id;
          S.persist_function store fn (expr_of_int 42) "tester";
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"bst_test"
               ~child_name
           with
           | None -> Alcotest.fail "fork failed"
           | Some child_id ->
               let own_row () =
                 List.exists
                   (fun (r : S.branch_status_row) ->
                     r.S.st_name = fn && r.S.st_own)
                   (S.branch_status store ~branch_id:child_id)
               in
               (* define identical to parent: suppressed, no pin *)
               store.branch_id <- child_id;
               S.persist_function store fn (expr_of_int 42) "tester";
               Alcotest.(check bool) "equal define leaves no pin" false
                 (own_row ());
               Alcotest.(check bool) "equal define stays clean" true
                 (status_of
                    (S.branch_status store ~branch_id:child_id)
                    ~name:fn
                 = Some S.SClean);
               (* divergent define: pins *)
               S.persist_function store fn (expr_of_int 7) "tester";
               Alcotest.(check bool) "divergent define pins" true
                 (own_row ());
               (* redefine equal to parent: unpins, parent revealed *)
               S.persist_function store fn (expr_of_int 42) "tester";
               Alcotest.(check bool) "resync define unpins" false
                 (own_row ());
               Alcotest.(check bool) "resynced name is clean" true
                 (status_of
                    (S.branch_status store ~branch_id:child_id)
                    ~name:fn
                 = Some S.SClean);
               cleanup_fn store ~branch_id:main_id ~name:fn;
               cleanup_branch store ~branch_id:child_id;
               ()))

(** branching.pin-hygiene: (prune-synced). A pin that BECAME synced
    (parent caught up to the pinned value) is removed by the sweep;
    pins with a real diff survive. *)
let test_prune_synced () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let child_name = "bst_prune_" ^ suffix in
          let synced_fn = "bst_prune_synced_" ^ suffix in
          let diff_fn = "bst_prune_diff_" ^ suffix in
          store.branch_id <- main_id;
          S.persist_function store synced_fn (expr_of_int 1) "tester";
          S.persist_function store diff_fn (expr_of_int 1) "tester";
          (match
             S.fork_branch store ~parent_id:main_id ~owner:"bst_test"
               ~child_name
           with
           | None -> Alcotest.fail "fork failed"
           | Some child_id ->
               store.branch_id <- child_id;
               (* pin both: synced_fn diverges temporarily, diff_fn
                  keeps a real permanent diff *)
               S.persist_function store synced_fn (expr_of_int 2)
                 "tester";
               S.persist_function store diff_fn (expr_of_int 9)
                 "tester";
               (* parent catches up on synced_fn: the pin is now
                  contentless (synced), still own *)
               store.branch_id <- main_id;
               Unix.sleepf 0.02;
               S.persist_function store synced_fn (expr_of_int 2)
                 "tester";
               store.branch_id <- child_id;
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "caught-up pin is synced" true
                 (status_of rows ~name:synced_fn = Some S.SSynced);
               let unpinned = S.prune_synced_fn_pins store in
               Alcotest.(check bool) "prune removes synced pin" true
                 (List.mem synced_fn unpinned);
               Alcotest.(check bool) "prune keeps diff pin" false
                 (List.mem diff_fn unpinned);
               let rows = S.branch_status store ~branch_id:child_id in
               Alcotest.(check bool) "pruned name is clean" true
                 (status_of rows ~name:synced_fn = Some S.SClean);
               Alcotest.(check bool) "diff pin still ahead" true
                 (status_of rows ~name:diff_fn = Some S.SAhead);
               cleanup_fn store ~branch_id:main_id ~name:synced_fn;
               cleanup_fn store ~branch_id:main_id ~name:diff_fn;
               cleanup_branch store ~branch_id:child_id;
               ()))

let () =
  Random.self_init ();
  Alcotest.run "branch-status"
    [ ( "db"
      , [ Alcotest.test_case "lifecycle" `Quick test_status_lifecycle
        ; Alcotest.test_case "define suppression" `Quick
            test_define_suppression
        ; Alcotest.test_case "prune-synced" `Quick test_prune_synced
        ] )
    ]
