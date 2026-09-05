(* upsets.analytics-bench — the event-replay simulator.
   L-2 NO LOOKAHEAD: the sweep presents detectors an as-of view
   (all rows ts <= t). Fills materialize at the next actual print
   strictly AFTER the signal tick. The poison test in upsets_test
   guards this property mechanically. *)

open Types

type settled = { st_order : Fills.order; st_won : bool; st_pnl : float }

type metrics = {
  mt_n_signals : int;
  mt_n_filled : int;
  mt_n_unfilled : int;
  mt_n_open : int;
  mt_hit_rate : float;
  mt_avg_entry : float;
  mt_expectancy : float; (* pnl per settled order *)
  mt_total_pnl : float;
  mt_max_dd : float;
  (* (band_low, lost_obs, won_obs) over ALL odds points, 0.05 bands *)
  mt_tax_all : (float * int * int) list;
  (* (band_low, tax, n) for bands touched by actual entries *)
  mt_entry_tax : (float * float * int) list;
}

type run_result = {
  rr_metrics : metrics;
  rr_signals : Types.signal list;
  rr_orders : Fills.order list;
  rr_settled : settled list;
  rr_equity : (int * float) list; (* (ts, cumulative pnl) at settle events *)
  rr_manifest : Yojson.Basic.t;
}

let sha256_hex (s : string) : string =
  Digestif.SHA256.to_hex (Digestif.SHA256.digest_string s)

let dataset_hash (snap : Types.snapshot) : string =
  sha256_hex (Yojson.Basic.to_string (Types.snapshot_to_json snap))

(* L-4/L-5 machinery: the holdout window may be touched exactly once
   per (dataset, rule). Prior manifests are the audit trail. *)
let check_holdout ~(manifests_dir : string) ~(dataset : string) ~(rule : string)
    ~(window : string) : (unit, string) result =
  if window <> "holdout" then Ok ()
  else
    let forbidden = ref None in
    (try
       Array.iter
         (fun f ->
           if Filename.check_suffix f ".json" then
             let j = Yojson.Basic.from_file (Filename.concat manifests_dir f) in
             let m = Yojson.Basic.Util.member in
             if
               m "dataset_hash" j = `String dataset
               && m "rule_hash" j = `String rule
               && m "window_kind" j = `String "holdout"
             then forbidden := Some f)
         (Sys.readdir manifests_dir)
     with Sys_error _ -> ());
    match !forbidden with
    | Some f ->
        Error
          (Printf.sprintf
             "holdout already contacted by this rule on this dataset (manifest %s). \
              Rule iteration after holdout contact makes it validation: change the \
              rule (new rule-hash) or accept that this window is burned."
             f)
    | None -> Ok ()

let band_of (p : float) : float =
  let b = Float.of_int (int_of_float (p /. 0.05)) *. 0.05 in
  Float.max 0.0 (Float.min 0.95 b)

let fst3 (x, _, _) = x

(* last YES price at or before ts *)
let price_at_or_before (arr : (int * float) array) (ts : int) : float option =
  let n = Array.length arr in
  if n = 0 || fst arr.(0) > ts then None
  else
    let lo = ref 0 and hi = ref (n - 1) in
    while !hi - !lo > 1 do
      let mid = (!lo + !hi) / 2 in
      if fst arr.(mid) <= ts then lo := mid else hi := mid
    done;
    let idx = if fst arr.(!hi) <= ts then !hi else !lo in
    Some (snd arr.(idx))

(* the sweep *)
let run ~(snapshot : Types.snapshot) ~(rule : Types.rule) ~(manifests_dir : string) :
    (run_result, string) result =
  let window_kind = snapshot.Types.s_window_kind in
  let ds_hash = dataset_hash snapshot in
  let r_hash = Rules.rule_hash rule in
  match check_holdout ~manifests_dir ~dataset:ds_hash ~rule:r_hash ~window:window_kind with
  | Error e -> Error e
  | Ok () ->
      (* ---- index the snapshot ---- *)
      let markets_tbl = Hashtbl.create 64 in
      List.iter (fun m -> Hashtbl.replace markets_tbl m.Types.m_id m) snapshot.Types.s_markets;
      let series_lists = Hashtbl.create 64 in
      List.iter
        (fun o ->
          let cur =
            match Hashtbl.find_opt series_lists o.Types.o_market with
            | Some l -> l
            | None -> []
          in
          Hashtbl.replace series_lists o.Types.o_market
            ((o.Types.o_ts, o.Types.o_p) :: cur))
        snapshot.Types.s_odds;
      let series = Hashtbl.create 64 in
      Hashtbl.iter
        (fun mid l ->
          Hashtbl.replace series mid
            (Array.of_list (List.sort (fun a b -> compare (fst a) (fst b)) l)))
        series_lists;
      let flows_arr =
        Array.of_list
          (List.sort
             (fun a b -> compare a.Types.f_last_ts b.Types.f_last_ts)
             snapshot.Types.s_flows)
      in
      let ev_set = Hashtbl.create 1024 in
      let add_ev ts =
        if ts >= snapshot.Types.s_from && ts <= snapshot.Types.s_to then
          Hashtbl.replace ev_set ts ()
      in
      List.iter (fun o -> add_ev o.Types.o_ts) snapshot.Types.s_odds;
      Array.iter (fun f -> add_ev f.Types.f_last_ts) flows_arr;
      List.iter (fun m -> add_ev m.Types.m_resolved_ts) snapshot.Types.s_markets;
      let events =
        Array.of_list (List.sort compare (Hashtbl.fold (fun k () acc -> k :: acc) ev_set []))
      in
      (* ---- sweep state ---- *)
      let len = Hashtbl.create 64 in (* market -> points consumed (ts <= t) *)
      let vis = ref [] in (* visible flows, newest first *)
      let fi = ref 0 in
      let wallet_pnl = Hashtbl.create 64 in
      let settled_markets = Hashtbl.create 64 in
      let signals_acc = ref [] in
      let orders_acc = ref [] in
      let settled_acc = ref [] in
      let unfilled = ref 0 in
      let cooldown = Hashtbl.create 64 in
      let open_orders = ref [] in
      let equity = ref [] in
      let cum = ref 0.0 in
      let depth_est m_id t =
        List.fold_left
          (fun a (f : Types.flow) -> if f.f_market = m_id then a +. f.f_usd else a)
          0.0
          (Flow.window !vis t (24 * 3600))
      in
      (* settle every market resolved at or before t (once) *)
      let settle_market (m : Types.market) t =
        if m.Types.m_resolved_ts <= t && not (Hashtbl.mem settled_markets m.Types.m_id) then begin
          Hashtbl.replace settled_markets m.Types.m_id ();
          let remaining = ref [] in
          List.iter
            (fun (o : Fills.order) ->
              if o.ord_market = m.Types.m_id then
                if o.ord_fill_ts <= t then begin
                  let won = o.ord_side = m.Types.m_outcome_won in
                  let pnl =
                    Fills.settle ~entry:o.ord_entry ~size:o.ord_size ~won rule.Types.r_sizing
                  in
                  cum := !cum +. pnl;
                  equity := (t, !cum) :: !equity;
                  settled_acc := { st_order = o; st_won = won; st_pnl = pnl } :: !settled_acc
                end
                else remaining := o :: !remaining (* fill lands after resolution: stays open *)
              else remaining := o :: !remaining)
            !open_orders;
          open_orders := List.rev !remaining;
          (* sharp-detector fuel: realized wallet pnl from this market's flows *)
          List.iter
            (fun (f : Types.flow) ->
              if f.f_market = m.Types.m_id then
                match
                  ( Hashtbl.find_opt series m.Types.m_id,
                    Hashtbl.find_opt len m.Types.m_id )
                with
                | Some arr, Some l when l > 0 ->
                    let cutoff = min f.Types.f_last_ts (fst arr.(l - 1)) in
                    (match price_at_or_before arr cutoff with
                    | Some p_yes ->
                        let p =
                          if f.Types.f_side = Types.Yes then p_yes else 1.0 -. p_yes
                        in
                        if p > 0.01 then begin
                          let cur =
                            match Hashtbl.find_opt wallet_pnl f.Types.f_wallet with
                            | Some v -> v
                            | None -> 0.0
                          in
                          let d =
                            if f.Types.f_side = m.Types.m_outcome_won then
                              f.Types.f_usd *. ((1.0 /. p) -. 1.0)
                            else -.f.Types.f_usd
                          in
                          Hashtbl.replace wallet_pnl f.Types.f_wallet (cur +. d)
                        end
                    | None -> ())
                | _ -> ())
            !vis
        end
      in
      (* ---- the sweep ---- *)
      Array.iter
        (fun t ->
          (* advance odds pointers *)
          Hashtbl.iter
            (fun mid arr ->
              let cur = match Hashtbl.find_opt len mid with Some l -> l | None -> 0 in
              let rec go i = if i < Array.length arr && fst arr.(i) <= t then go (i + 1) else i in
              Hashtbl.replace len mid (go cur))
            series;
          (* advance flow visibility *)
          while
            !fi < Array.length flows_arr
            && flows_arr.(!fi).Types.f_last_ts <= t
          do
            vis := flows_arr.(!fi) :: !vis;
            incr fi
          done;
          (* settle resolutions at/before t *)
          List.iter (fun m -> settle_market m t) snapshot.Types.s_markets;
          (* detect on the as-of view *)
          let view =
            {
              Rules.v_t = t;
              v_open =
                List.filter (fun m -> m.Types.m_resolved_ts > t) snapshot.Types.s_markets;
              v_series = series;
              v_len = len;
              v_flows = !vis;
              v_wallet_pnl = wallet_pnl;
              v_market = markets_tbl;
            }
          in
          let sigs = Rules.detect view rule in
          (* deterministic order: score desc, then market/kind/side asc *)
          let sigs =
            List.sort
              (fun a b ->
                compare
                  ( -.(a.Types.sig_score), a.Types.sig_market, Types.string_of_kind a.Types.sig_kind, Types.string_of_side a.Types.sig_side )
                  ( -.(b.Types.sig_score), b.Types.sig_market, Types.string_of_kind b.Types.sig_kind, Types.string_of_side b.Types.sig_side ))
              sigs
          in
          List.iter
            (fun (s : Types.signal) ->
              let key = (s.Types.sig_market, s.Types.sig_side, s.Types.sig_kind) in
              let cd = rule.Types.r_sizing.Types.sz_cooldown in
              let ok =
                match Hashtbl.find_opt cooldown key with
                | None -> true
                | Some last -> t - last >= cd
              in
              if ok then begin
                Hashtbl.replace cooldown key t;
                signals_acc := s :: !signals_acc;
                match
                  ( Hashtbl.find_opt series s.Types.sig_market,
                    Hashtbl.find_opt len s.Types.sig_market )
                with
                | Some arr, Some idx -> (
                    match Fills.next_print arr idx t s.Types.sig_side with
                    | Some (ts_f, p_next) ->
                        let size = Fills.size_usd rule.Types.r_sizing in
                        let entry =
                          Fills.fill_price ~p_next ~size
                            ~depth_usd:(depth_est s.Types.sig_market t)
                            rule.Types.r_sizing
                        in
                        orders_acc :=
                          {
                            Fills.ord_market = s.Types.sig_market;
                            ord_side = s.Types.sig_side;
                            ord_kind = s.Types.sig_kind;
                            ord_signal_ts = t;
                            ord_fill_ts = ts_f;
                            ord_entry = entry;
                            ord_size = size;
                          }
                          :: !orders_acc;
                        open_orders := List.hd !orders_acc :: !open_orders
                    | None -> incr unfilled)
                | _ -> incr unfilled
              end)
            sigs)
        events;
      (* ---- metrics ---- *)
      let signals = List.rev !signals_acc in
      let orders = List.rev !orders_acc in
      let settled = List.rev !settled_acc in
      let equity_sorted =
        List.sort (fun a b -> compare (fst a) (fst b)) !equity
      in
      let n_settled = List.length settled in
      let wins = List.length (List.filter (fun s -> s.st_won) settled) in
      let total_pnl = List.fold_left (fun a s -> a +. s.st_pnl) 0.0 settled in
      let max_dd =
        let _, dd =
          List.fold_left
            (fun (peak, dd) (_, v) ->
              let peak = Float.max peak v in
              (peak, Float.max dd (peak -. v)))
            (0.0, 0.0)
            equity_sorted
        in
        dd
      in
      (* base-rate tax over all odds points (L-1) *)
      let tax = Hashtbl.create 32 in (* band -> (lost, won) *)
      List.iter
        (fun o ->
          match Hashtbl.find_opt markets_tbl o.Types.o_market with
          | None -> ()
          | Some m ->
              if o.Types.o_ts < m.Types.m_resolved_ts then begin
                let bump band won =
                  let l, w =
                    match Hashtbl.find_opt tax band with
                    | Some (l, w) -> (l, w)
                    | None -> (0, 0)
                  in
                  Hashtbl.replace tax band (if won then (l, w + 1) else (l + 1, w))
                in
                bump (band_of o.Types.o_p) (m.Types.m_outcome_won = Types.Yes);
                bump (band_of (1.0 -. o.Types.o_p)) (m.Types.m_outcome_won = Types.No)
              end)
        snapshot.Types.s_odds;
      let tax_all =
        Hashtbl.fold (fun band (l, w) acc -> (band, l, w) :: acc) tax []
        |> List.sort (fun a b -> compare (fst3 a) (fst3 b))
      in
      let entry_bands =
        List.filter_map
          (fun (o : Fills.order) ->
            let b = band_of o.ord_entry in
            match Hashtbl.find_opt tax b with
            | Some (l, w) when l + w > 0 ->
                Some (b, float_of_int l /. float_of_int (l + w), l + w)
            | _ -> Some (b, 0.0, 0))
          orders
        |> List.sort_uniq compare
      in
      (* dataset sanity gate (L-1): in an unbiased corpus the realized
         loss rate at odds p should approximate (1 - p). Bands that are
         suspiciously PROFITABLE mean the snapshot is winner-heavy
         (selection artifact) and any expectancy measured on it is
         contaminated — this caught the first Clacton fixture's false
         alpha: hand-picked upset markets made RANDOM entries look
         golden. *)
      let sanity_suspicious =
        Hashtbl.fold
          (fun band (l, w) acc ->
            let n = l + w in
            if n >= 30 then
              let realized = float_of_int l /. float_of_int n in
              let prior = 1.0 -. (band +. 0.025) in
              if realized < prior -. 0.15 then
                (band,
                 `Assoc
                   [ ("band", `Float band);
                     ("realized_loss", `Float realized);
                     ("prior_loss", `Float prior);
                     ("n", `Int n);
                   ])
                :: acc
              else acc
            else acc)
          tax []
        |> List.sort (fun a b -> compare (fst a) (fst b))
        |> List.map snd
      in
      let metrics =
        {
          mt_n_signals = List.length signals;
          mt_n_filled = List.length orders;
          mt_n_unfilled = !unfilled;
          mt_n_open = List.length orders - n_settled;
          mt_hit_rate = (if n_settled = 0 then 0.0 else float_of_int wins /. float_of_int n_settled);
          mt_avg_entry =
            (if List.length orders = 0 then 0.0
             else
               List.fold_left (fun a o -> a +. o.Fills.ord_entry) 0.0 orders
               /. float_of_int (List.length orders));
          mt_expectancy = (if n_settled = 0 then 0.0 else total_pnl /. float_of_int n_settled);
          mt_total_pnl = total_pnl;
          mt_max_dd = max_dd;
          mt_tax_all = tax_all;
          mt_entry_tax = entry_bands;
        }
      in
      let tax_json =
        `List
          (List.map
             (fun (b, l, w) -> `Assoc [ ("band", `Float b); ("lost", `Int l); ("won", `Int w) ])
             tax_all)
      in
      let entry_tax_json =
        `List
          (List.map
             (fun (b, tx, n) ->
               `Assoc [ ("band", `Float b); ("tax", `Float tx); ("n", `Int n) ])
             entry_bands)
      in
      let manifest =
        `Assoc
          [ ("snapshot", `String snapshot.Types.s_name);
            ("dataset_hash", `String ds_hash);
            ("rule_hash", `String r_hash);
            ("window_kind", `String window_kind);
            ("from", `Int snapshot.Types.s_from);
            ("to", `Int snapshot.Types.s_to);
            ("created", `Float (Unix.gettimeofday ()));
            ( "metrics",
              `Assoc
                [ ("n_signals", `Int metrics.mt_n_signals);
                  ("n_filled", `Int metrics.mt_n_filled);
                  ("n_unfilled", `Int metrics.mt_n_unfilled);
                  ("n_open", `Int metrics.mt_n_open);
                  ("hit_rate", `Float metrics.mt_hit_rate);
                  ("avg_entry", `Float metrics.mt_avg_entry);
                  ("expectancy", `Float metrics.mt_expectancy);
                  ("total_pnl", `Float metrics.mt_total_pnl);
                  ("max_drawdown", `Float metrics.mt_max_dd);
                ] );
            ("tax_all", tax_json);
            ("entry_tax", entry_tax_json);
            ("sanity_suspicious_bands", `List sanity_suspicious);
            ( "dataset_suspicious",
              `Bool (List.length sanity_suspicious > 0) );
            ( "equity",
              `List
                (List.map (fun (ts, v) -> `Assoc [ ("ts", `Int ts); ("pnl", `Float v) ]) equity_sorted)
            );
          ]
      in
      (* manifest trail — the audit surface for holdout-once *)
      (try if not (Sys.is_directory manifests_dir) then Unix.mkdir manifests_dir 0o755
       with Sys_error _ -> Unix.mkdir manifests_dir 0o755);
      let mfile =
        Filename.concat manifests_dir
          (Printf.sprintf "%s-%s-%s.json" (String.sub ds_hash 0 8) (String.sub r_hash 0 8)
             window_kind)
      in
      Yojson.Basic.to_file mfile manifest;
      Ok { rr_metrics = metrics; rr_signals = signals; rr_orders = orders; rr_settled = settled; rr_equity = equity_sorted; rr_manifest = manifest }
