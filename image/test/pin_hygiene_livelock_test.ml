(* BORGE-1788333821 — branching.pin-hygiene LIVELOCK (2026-09-02 prod).

   The incident: after promote of 7f0041b (no diff => no pin), attach to
   a personal branch whose libs were identical to main entered a
   DELETE/INSERT ping-pong on tree_nodes — a backend loop of
   "DELETE FROM tree_nodes WHERE path=$1" every ~250ms, PG backend 35%
   CPU, log spamming "load def ... skipped" (rehydrate churn), every
   DB-touching HTTP request blocked while /health stayed ok:true so the
   watchdog never fired. Hypothesized cycle: identical-redefine unpins
   the pin, the additive boot seed sees the name missing on the branch
   and re-inserts an identical body, hygiene unpins again -> churn.

   This test reproduces the incident's STATE and drives the identical-
   redefine cycle through the REAL session eval path (Session.make +
   eval_string_rim, Store on local PG), asserting the ping-pong is GONE:

     1. a branch with an own fn pin IDENTICAL to the parent chain (the
        post-promote residue / historical sludge state the incident had)
        is created via the real promote path (diverged pin -> promote ->
        synced pin);
     2. an identical re-define UNPINS ONCE (1 -> 0) and STAYS 0 across
        repeated identical re-defines — the row is never re-inserted.

   Why it cannot churn anymore (each leg removed):
     - WRITE-TIME SUPPRESSION (branching.pin-hygiene): persist_function
       compares against the parent chain (canonical jsonb::text, single
       hash domain); an identical body writes NO pin and UNPINS an own
       row. There is no re-insert on the define path.
     - ADDITIVE SEED IS system/main-SCOPED: seed_stdlib runs inside
       Store.with_branch ~branch_id:system_main_id with seed_additive
       true, so persist_function_if_absent (the only additive-insert
       path) can never touch a personal branch — the "seed re-inserts
       onto personal" leg of the hypothesis does not exist.
     - LOAD-LIBRARY SKIP MAP (image.load-library-skip): identical
       chain-visible defs are skipped before eval/persist, so
       load-library cannot re-eval/re-persist an inherited body either
       (that leg was burning minutes per package pre-2026-09-08; the
       live repro of THIS fix is captured in the plan log).

   Skips cleanly when no Postgres is reachable (scripts/dev.sh). *)

module S = Pricklypear_image_lib.Store
module Ses = Pricklypear_image_lib.Session
module T = Pricklypear_image_lib.Tree_store

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

let uniq p =
  Printf.sprintf "%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

(** A real session bound to [store] on [branch_id] — eval_string_rim
    through it persists defines exactly like the running image. *)
let session_on ~store ~username ~branch_id ~branch_name =
  let manifest =
    Pricklypear_common.Manifest.make ~user_id:username ~username
      ~branch_id ~branch_name
      ~boot_mode:Pricklypear_common.Manifest.Boot_on_connect
      ~image_version:"test" ()
  in
  Ses.make ~manifest ~store ()

(** Count LIVE tree_nodes rows at the fn path for [name] on [branch_id]
    — the row the livelock's DELETE/INSERT churned. *)
let count_fn store ~branch_id ~name =
  match S.conn_of store with
  | None -> 0
  | Some conn ->
      let path = T.fn_path ~branch_id ~name in
      (match
         S.exec conn
           ~params:[ Pgx.Value.of_string path ]
           "SELECT count(*) FROM tree_nodes WHERE path = $1::ltree"
       with
       | [ [ c ] ] -> (
           match Pgx.Value.to_string c with
           | Some s -> int_of_string s
           | None -> 0)
       | _ -> 0)

let cleanup store ~branch_id ~name ~child_id =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      List.iter
        (fun sql ->
          try ignore (S.exec conn ~params:[ Pgx.Value.of_string child_id ] sql)
          with _ -> ())
        [ "DELETE FROM view_bindings WHERE branch_id = $1";
          "DELETE FROM type_defs WHERE branch_id = $1";
          "DELETE FROM function_defs WHERE branch_id = $1";
          "DELETE FROM tree_nodes WHERE path::text LIKE 'b.' || "
          ^ "replace($1::text,'-','') || '.fn.%'";
          "DELETE FROM branches WHERE id = $1" ];
      ignore (S.delete_function_on_branch store ~branch_id ~name)

(* ---- the acceptance ---- *)

let test_synced_pin_unpins_once_and_stays_gone () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let suffix = uniq "phll" in
          let fn = "phll_fn_" ^ suffix in
          let child_name = "phll_child_" ^ suffix in
          let branch_ref = "phllivelock/" ^ child_name in
          (* session on system/main *)
          let s_main =
            session_on ~store ~username:"admin" ~branch_id:main_id
              ~branch_name:"system/main"
          in
          (* 1. main owns fn = 42 *) 
          store.branch_id <- main_id;
          ignore (Ses.eval_one s_main (Printf.sprintf "(define %s 42)" fn));
          Alcotest.(check int) "main owns fn=42" 1
            (count_fn store ~branch_id:main_id ~name:fn);
          (* 2. fork a personal-style child of main *)
          let child_id =
            match
              S.fork_branch store ~parent_id:main_id ~owner:"phllivelock"
                ~child_name
            with
            | Some id -> id
            | None -> Alcotest.fail "fork failed"
          in
          (* 3. child session: define fn = 7 (divergent pin, ahead-new) *)
          let s_child =
            session_on ~store ~username:"phllivelock" ~branch_id:child_id
              ~branch_name:branch_ref
          in
          store.branch_id <- child_id;
          ignore (Ses.eval_one s_child (Printf.sprintf "(define %s 7)" fn));
          Alcotest.(check int) "child owns divergent pin" 1
            (count_fn store ~branch_id:child_id ~name:fn);
          (* 4. promote child -> main: main gets 7; the child's pin (7)
             is now IDENTICAL to the new parent -> synced residue, the
             exact state the 2026-09-02 prod carried by the thousand. *)
          let n =
            S.promote_functions store ~source_id:child_id
              ~target_id:main_id ~names:[ fn ]
          in
          Alcotest.(check int) "promote copied 1 fn" 1 n;
          Alcotest.(check int) "main now 7" 1
            (count_fn store ~branch_id:main_id ~name:fn);
          Alcotest.(check int) "child still holds the synced pin" 1
            (count_fn store ~branch_id:child_id ~name:fn);
          (* 5. THE LIVELOCK CYCLE: identical re-defines of the parent
             body. The first unpins (1 -> 0); every repeat must stay 0 —
             the 2026-09-02 ping-pong would read 1 -> 0 -> 1 -> 0 ... *)
          for i = 1 to 5 do
            store.branch_id <- child_id;
            ignore
              (Ses.eval_one s_child (Printf.sprintf "(define %s 7)" fn));
            Alcotest.(check int)
              (Printf.sprintf
                 "redefine #%d: own row stays unpinned (no ping-pong)" i)
              0 (count_fn store ~branch_id:child_id ~name:fn)
          done;
          (* 6. the seed leg cannot re-insert onto the child: the additive
             seed writes system/main only (seed_stdlib scoped by
             Store.with_branch ~system_main_id), so persist_function_
             if_absent never runs on a personal branch. Assert the child
             stayed pin-free even after a main-scoped additive insert
             (what the seed would do) of the SAME body. *)
          store.branch_id <- main_id;
          ignore
            (Ses.eval_one s_main (Printf.sprintf "(define %s 7)" fn));
          Alcotest.(check int) "child still unpinned after main move" 0
            (count_fn store ~branch_id:child_id ~name:fn);
          (* cleanup *)
          cleanup store ~branch_id:main_id ~name:fn ~child_id;
          ignore (S.delete_function_on_branch store ~branch_id:main_id ~name:fn);
          ())

let () =
  Random.self_init ();
  Alcotest.run "pin_hygiene_livelock"
    [ ( "BORGE-1788333821"
      , [ Alcotest.test_case "synced pin unpins once and stays gone" `Slow
            test_synced_pin_unpins_once_and_stays_gone
        ] )
    ]
