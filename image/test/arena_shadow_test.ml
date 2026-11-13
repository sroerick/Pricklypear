(** Arena shadow-mode contract — the mechanical rank.

    The ledger records engine + card + gate + tokens per run; the shadow
    dispatch is N independent worktrees from ONE identical base commit and
    nothing merges. Pass iff gate = ok AND outcome = ok. *)

open Pricklypear_image_lib

let base = "e5a3e56412f2"
let card = "card-67076f"

let dispatch () =
  match
    Activity_ledger.arena_dispatch ~card ~base_commit:base
      ~engines:[ "native"; "engine-2"; "engine-3" ] ~tokens:7 ()
  with
  | Error e -> Alcotest.fail e
  | Ok runs -> runs

let test_dispatch_n_rows () =
  let runs = dispatch () in
  Alcotest.(check int) "N worktrees from one card" 3 (List.length runs);
  List.iter
    (fun (r : Activity_ledger.arena_run) ->
      Alcotest.(check string) "identical base commit" base r.ga_base_commit;
      (* no merge edge: no engine targets trunk/production, and no engine's
         branch is another engine's base. *)
      Alcotest.(check bool) "beside production" true
        (r.ga_branch <> "master" && r.ga_branch <> "production"))
    runs;
  let branches =
    List.map (fun (r : Activity_ledger.arena_run) -> r.ga_branch) runs
    |> List.sort_uniq compare
  in
  let worktrees =
    List.map (fun (r : Activity_ledger.arena_run) -> r.ga_worktree) runs
    |> List.sort_uniq compare
  in
  Alcotest.(check int) "own clone per engine" 3 (List.length worktrees);
  List.iter
    (fun b ->
      Alcotest.(check bool) "no engine bases on another" true
        (not (List.mem b [ base; "master" ])))
    branches;
  (* ledger scoreboard rows: one per (engine x card), engine + tokens. *)
  let rows = Activity_ledger.arena_leaderboard runs in
  Alcotest.(check int) "N rows" 3 (List.length rows);
  List.iter
    (fun (s : Activity_ledger.arena_score) ->
      Alcotest.(check string) "row card" card s.asc_card;
      Alcotest.(check int) "row tokens" 7 s.asc_tokens;
      Alcotest.(check int) "row run" 1 s.asc_runs)
    rows

let test_leaderboard_groups () =
  let runs = dispatch () in
  let ok =
    List.map
      (fun r ->
        { r with Activity_ledger.ga_gate = "ok";
                 ga_outcome = "ok" })
      runs
  in
  let rows = Activity_ledger.arena_leaderboard (ok @ ok) in
  Alcotest.(check int) "engine x card rows" 3 (List.length rows);
  List.iter
    (fun (s : Activity_ledger.arena_score) ->
      Alcotest.(check int) "two runs grouped" 2 s.asc_runs;
      Alcotest.(check int) "two passes" 2 s.asc_passes;
      Alcotest.(check int) "tokens summed" 14 s.asc_tokens)
    rows

let test_rank_gate () =
  let runs = dispatch () in
  let incumbent = List.hd runs in
  let gate_broken =
    { incumbent with Activity_ledger.ga_gate = "fail";
                      ga_outcome = "ok" }
  in
  Alcotest.(check bool) "gate failure is not a pass" false
    (Activity_ledger.arena_pass gate_broken);
  Alcotest.(check bool) "gate ok and outcome ok is a pass" true
    (Activity_ledger.arena_pass
       { incumbent with Activity_ledger.ga_gate = "ok"; ga_outcome = "ok" });
  let rows = Activity_ledger.arena_leaderboard [ gate_broken;
                                                 { incumbent with
                                                   Activity_ledger.ga_gate = "ok" } ] in
  let ranked = Activity_ledger.arena_rank ~card rows in
  Alcotest.(check int) "one card scored" (List.length rows)
    (List.length ranked);
  Alcotest.(check int) "broken gate scores zero passes" 0
    (List.fold_left
       (fun acc (s : Activity_ledger.arena_score) ->
         if s.asc_engine = incumbent.ga_engine then s.asc_passes else acc)
       0 ranked)

let test_sealed_brief () =
  Alcotest.(check bool) "own brief only" true
    (Activity_ledger.arena_brief_sealed ~card ~brief_cards:[ card ]);
  Alcotest.(check bool) "sibling card is contamination" false
    (Activity_ledger.arena_brief_sealed ~card
       ~brief_cards:[ card; "card-67077" ])

let test_scoped_identity () =
  Alcotest.(check bool) "external engine cannot write production" false
    (Activity_ledger.arena_write_allowed ~scoped:true ~production:true);
  Alcotest.(check bool) "scoped engine has no production capability" false
    (Activity_ledger.arena_write_allowed ~scoped:true ~production:false);
  Alcotest.(check bool) "internal production scope writes" true
    (Activity_ledger.arena_write_allowed ~scoped:false ~production:true)

let test_matched_pairs () =
  let scores =
    [ { Activity_ledger.asc_card = card; asc_engine = "native"; asc_runs = 1;
        asc_passes = 1; asc_tokens = 7 };
      { Activity_ledger.asc_card = card; asc_engine = "engine-2"; asc_runs = 1;
        asc_passes = 0; asc_tokens = 7 } ]
  in
  match Activity_ledger.arena_pairs ~incumbent:"native" scores with
  | [ (c, inc, eng) ] ->
      Alcotest.(check string) "pair card" card c;
      Alcotest.(check string) "incumbent" "native" inc.asc_engine;
      Alcotest.(check string) "engine #2" "engine-2" eng.asc_engine
  | xs -> Alcotest.(check int) "one matched pair" 1 (List.length xs)

let () =
  Alcotest.run "arena_shadow"
    [ ( "dispatch",
        [ Alcotest.test_case "n-rows-no-merge" `Quick test_dispatch_n_rows ] );
      ( "leaderboard",
        [ Alcotest.test_case "engine-x-card" `Quick test_leaderboard_groups ] );
      ( "rank",
        [ Alcotest.test_case "gate-and-outcome" `Quick test_rank_gate ] );
      ("sealed", [ Alcotest.test_case "own-brief-only" `Quick test_sealed_brief ]);
      ( "identity",
        [ Alcotest.test_case "scoped-no-production" `Quick test_scoped_identity ] );
      ( "pairs",
        [ Alcotest.test_case "matched-pairs" `Quick test_matched_pairs ] );
      ("selftest",
       [ Alcotest.test_case "arena-selftest" `Quick (fun () ->
             List.iter
               (fun (name, ok) -> Alcotest.(check bool) name true ok)
               (Activity_ledger.arena_selftest ())) ]) ]
