(* upsets bench — alcotest suite.
   The load-bearing test is poison-the-future (L-2): corrupt every
   row with ts > T and the t <= T signal log and fills must be
   BYTE-IDENTICAL. Plus: stats/cooc/fill units, and holdout-once
   enforcement (L-4/L-5). *)

open Upsets_bench

let hour = 3600
let t0 = 1_700_000_000

let ord_key (o : Fills.order) =
  Printf.sprintf "%d|%s|%s|%.9f|%.4f" o.Fills.ord_fill_ts o.Fills.ord_market
    (Types.string_of_side o.Fills.ord_side) o.Fills.ord_entry o.Fills.ord_size

let sig_key (s : Types.signal) =
  Printf.sprintf "%d|%s|%s|%s|%.9f" s.Types.sig_ts s.Types.sig_market
    (Types.string_of_kind s.Types.sig_kind)
    (Types.string_of_side s.Types.sig_side)
    s.Types.sig_score

(* ---- synthetic snapshot covering all three detectors ----
   A: velocity case — flat 0.10 with tiny noise, jumps 0.30 at +24h,
      0.60 at +25h, 0.95 after; resolves YES at +48h.
   E: sharp-wallet feeder — W2 buys YES at 0.10 at +5h; resolves YES
      at +10h (gives W2 realized pnl).
   D: sharp-entry target — W2 (now proven) buys at +12h; resolves NO
      at +50h.
   B,C: ladder case — W1 sweeps both at +20h; cluster "c1"; resolve
      YES at +60h. *)

let noise i = float_of_int (i mod 3) *. 0.001

let mk_series m_id base jump =
  List.init 61 (fun i ->
      let ts = t0 + (i * hour) in
      let p =
        if i < 24 then base +. noise i
        else if i = 24 then jump
        else if i = 25 then jump +. 0.30
        else 0.95
      in
      { Types.o_market = m_id; o_ts = ts; o_p = p })

let flat_series m_id base =
  List.init 61 (fun i ->
      { Types.o_market = m_id; o_ts = t0 + (i * hour); o_p = base +. noise i })

let flow m w side usd ts =
  {
    Types.f_market = m; f_wallet = w; f_side = side; f_usd = usd; f_n = 5;
    f_first_ts = ts; f_last_ts = ts;
  }

let base_snapshot () : Types.snapshot =
  let markets =
    [
      { Types.m_id = "A"; m_question = "A"; m_slug = "a"; m_condition_id = "ca";
        m_cluster = None; m_vol = 50_000.0; m_resolved_ts = t0 + (48 * hour);
        m_outcome_won = Types.Yes };
      { Types.m_id = "E"; m_question = "E"; m_slug = "e"; m_condition_id = "ce";
        m_cluster = None; m_vol = 20_000.0; m_resolved_ts = t0 + (10 * hour);
        m_outcome_won = Types.Yes };
      { Types.m_id = "D"; m_question = "D"; m_slug = "d"; m_condition_id = "cd";
        m_cluster = None; m_vol = 20_000.0; m_resolved_ts = t0 + (50 * hour);
        m_outcome_won = Types.No };
      { Types.m_id = "B"; m_question = "B"; m_slug = "b"; m_condition_id = "cb";
        m_cluster = Some "c1"; m_vol = 90_000.0; m_resolved_ts = t0 + (60 * hour);
        m_outcome_won = Types.Yes };
      { Types.m_id = "C"; m_question = "C"; m_slug = "c"; m_condition_id = "cc";
        m_cluster = Some "c1"; m_vol = 90_000.0; m_resolved_ts = t0 + (60 * hour);
        m_outcome_won = Types.Yes };
    ]
  in
  let odds =
    mk_series "A" 0.10 0.30 @ flat_series "E" 0.10 @ flat_series "D" 0.20
    @ flat_series "B" 0.30 @ flat_series "C" 0.30
  in
  let flows =
    [ flow "E" "W2" Types.Yes 1000.0 (t0 + (5 * hour));
      flow "D" "W2" Types.Yes 1000.0 (t0 + (12 * hour));
      flow "B" "W1" Types.Yes 1000.0 (t0 + (20 * hour));
      flow "C" "W1" Types.Yes 1000.0 (t0 + (20 * hour));
    ]
  in
  {
    Types.s_name = "synthetic";
    s_window_kind = "train";
    s_from = t0;
    s_to = t0 + (61 * hour);
    s_markets = markets;
    s_odds = odds;
    s_flows = flows;
  }

let test_rule () : Types.rule =
  {
    Types.r_velocity =
      Some { v_window = 24 * hour; v_z = 2.0; v_min_odds = 0.05; v_max_odds = 0.45; v_min_move = 0.05 };
    r_ladder = Some { l_window = 48 * hour; l_min_markets = 2; l_min_usd = 500.0; l_max_odds = 0.70 };
    r_sharp = Some { s_min_pnl = 1000.0; s_min_usd = 300.0; s_max_odds = 0.50 };
    r_sizing =
      { sz_frac = 0.02; sz_bankroll = 10_000.0; sz_cap = 500.0; sz_slip_coef = 0.25;
        sz_max_slip = 0.15; sz_fees_bps = 0.0; sz_cooldown = 24 * hour };
  }

let tmpdir () =
  let d =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "upsets-test-%d-%d" (Unix.getpid ()) (Random.int 1_000_000))
  in
  Unix.mkdir d 0o755;
  d

(* ---- unit tests ---- *)

let test_stats () =
  let a = [| 1.0; 2.0; 3.0; 4.0 |] in
  let m = Stats.mean a in
  Alcotest.(check bool "mean" true (Float.abs (m -. 2.5) < 1e-12));
  let sd = Stats.stdev a ~mean:m in
  Alcotest.(check bool "stdev" true (Float.abs (sd -. 1.29099) < 1e-4));
  (* trailing window: strictly (t-w, t] — boundary t-w excluded *)
  let pts = [| (0, 1.0); (5, 2.0); (10, 3.0); (15, 4.0) |] in
  let vals = Stats.trailing_values pts 10 5 in
  Alcotest.(check int "trailing count" 1 (Array.length vals));
  Alcotest.(check bool "trailing vals" true (Array.for_all (fun v -> v = 3.0) vals))

let test_cooc () =
  (* W1 sweeps B and C; W2 only on B -> cluster {B,C} *)
  let flows =
    [ flow "B" "W1" Types.Yes 1000.0 t0;
      flow "C" "W1" Types.Yes 1000.0 t0;
      flow "B" "W2" Types.Yes 1000.0 t0;
    ]
  in
  let cls = Cooc.clusters flows ~min_usd:100.0 in
  Alcotest.(check int "one cluster" 1 (List.length cls));
  let (_, ms) = List.hd cls in
  Alcotest.(check (list string) "cluster members" [ "B"; "C" ] (List.sort compare ms));
  let ladders = Cooc.ladder_wallets flows ~window_s:3600 ~min_markets:2 ~min_usd:500.0 (t0 + 60) in
  Alcotest.(check int "ladder wallets" 1 (List.length ladders))

let test_fills () =
  let sizing = (test_rule ()).Types.r_sizing in
  (* next print strictly after t *)
  let arr = [| (0, 0.10); (10, 0.20); (20, 0.30) |] in
  Alcotest.(check (option (pair int (float 1e-9))) "next after 5" (Some (10, 0.20))
              (Fills.next_print arr 1 5 Types.Yes));
  Alcotest.(check (option (pair int (float 1e-9))) "exhausted" None
              (Fills.next_print arr 3 5 Types.Yes));
  (* slippage capped *)
  let px = Fills.fill_price ~p_next:0.10 ~size:10_000.0 ~depth_usd:100.0 sizing in
  Alcotest.(check bool "slip capped at max" true (Float.abs (px -. 0.115) < 1e-9));
  (* settle: win at entry 0.50 size 200 -> +200 - fee *)
  let pnl = Fills.settle ~entry:0.50 ~size:200.0 ~won:true sizing in
  Alcotest.(check bool "settle win" true (Float.abs (pnl -. 200.0) < 1e-9))

(* ---- the poison test (L-2) ---- *)

let test_poison () =
  let clean = base_snapshot () in
  let rule = test_rule () in
  let d1 = tmpdir () in
  let r1 = Replay.run ~snapshot:clean ~rule ~manifests_dir:d1 |> Result.get_ok in
  (* sanity: all three detectors fired *)
  let has m k =
    List.exists
      (fun s -> s.Types.sig_market = m && s.Types.sig_kind = k)
      r1.Replay.rr_signals
  in
  Alcotest.(check bool "velocity fired on A" true (has "A" Types.Velocity));
  Alcotest.(check bool "ladder fired on B" true (has "B" Types.Ladder));
  Alcotest.(check bool "sharp fired on D" true (has "D" Types.Sharp));
  (* poison: corrupt every row with ts > T = t0 + 30h *)
  let t_pois = t0 + (30 * hour) in
  let poisoned =
    {
      clean with
      Types.s_markets =
        List.map
          (fun m ->
            if m.Types.m_id = "A" then { m with m_outcome_won = Types.No } (* resolution > T *)
            else m)
          clean.Types.s_markets;
      s_odds =
        List.map
          (fun o ->
            if o.Types.o_market = "D" && o.Types.o_ts = t0 + (36 * hour) then
              { o with o_p = 0.50 } (* future print moved *)
            else if o.Types.o_market = "C" && o.Types.o_ts = t0 + (50 * hour) then
              { o with o_p = 0.01 } (* future print perturbed *)
            else o)
          clean.Types.s_odds;
      s_flows = clean.Types.s_flows @ [ flow "B" "W9" Types.Yes 900.0 (t0 + (40 * hour)) ];
    }
  in
  let d2 = tmpdir () in
  let r2 = Replay.run ~snapshot:poisoned ~rule ~manifests_dir:d2 |> Result.get_ok in
  let cut_l l = List.filter (fun s -> s.Types.sig_ts <= t_pois) l |> List.map sig_key in
  let cut_o l =
    List.filter (fun o -> o.Fills.ord_fill_ts <= t_pois) l |> List.map ord_key
  in
  Alcotest.(check bool "poisoned run still has signals <=T" true (List.length (cut_l r2.Replay.rr_signals) > 0));
  Alcotest.(check (list string) "signals <=T identical" (cut_l r1.Replay.rr_signals)
              (cut_l r2.Replay.rr_signals));
  Alcotest.(check (list string) "fills <=T identical" (cut_o r1.Replay.rr_orders)
              (cut_o r2.Replay.rr_orders))

(* ---- holdout-once (L-4/L-5) ---- *)

let test_holdout_once () =
  let snap = { (base_snapshot ()) with Types.s_window_kind = "holdout" } in
  let rule = test_rule () in
  let d = tmpdir () in
  (* first run: allowed *)
  Result.get_ok (Replay.run ~snapshot:snap ~rule ~manifests_dir:d) |> ignore;
  (* second run, same dataset + rule: refused *)
  match Replay.run ~snapshot:snap ~rule ~manifests_dir:d with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "holdout ran twice without refusal"

(* ---- L-1 base-rate tax present in manifest + sanity gate fires on
   winner-heavy corpora (the Clacton lesson, mechanized) ---- *)

let test_tax () =
  let snap = base_snapshot () in
  let rule = test_rule () in
  let d = tmpdir () in
  let r = Replay.run ~snapshot:snap ~rule ~manifests_dir:d |> Result.get_ok in
  (* D resolved NO: its YES-side points (band 0.20) are lost observations *)
  let band20 =
    List.find_opt (fun (b, _, _) -> Float.abs (b -. 0.20) < 1e-9) r.Replay.rr_metrics.Replay.mt_tax_all
  in
  (match band20 with
   | Some (_, lost, _) -> Alcotest.(check bool "band 0.20 has losses" true (lost > 0))
   | None -> Alcotest.fail "band 0.20 missing from tax table");
  (* the synthetic corpus is 4-of-5 YES-resolving -> sanity gate MUST fire *)
  let suspicious =
    match Yojson.Basic.Util.member "dataset_suspicious" r.Replay.rr_manifest with
    | `Bool b -> b
    | _ -> false
  in
  Alcotest.(check bool "winner-heavy corpus flagged" true suspicious)

let () =
  Random.self_init ();
  let open Alcotest in
  run "upsets"
    [
      ("stats", [ test_case "mean/stdev/trailing" `Quick test_stats ]);
      ("cooc", [ test_case "clusters + ladder wallets" `Quick test_cooc ]);
      ("fills", [ test_case "next-print + slippage + settle" `Quick test_fills ]);
      ("simulator", [ test_case "poison-the-future (L-2)" `Quick test_poison ]);
      ("discipline", [ test_case "holdout touched once" `Quick test_holdout_once ]);
      ("tax", [ test_case "base-rate tax table (L-1)" `Quick test_tax ]);
    ]
