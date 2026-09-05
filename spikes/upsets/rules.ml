(* upsets.analytics-bench — the rule record's canonical hash (L-5),
   and the three detectors. Detection uses ONLY the as-of view passed
   in (L-2): nothing here may look past v_t. *)

open Types

let sha256_hex (s : string) : string =
  Digestif.SHA256.to_hex (Digestif.SHA256.digest_string s)

let rule_hash (r : Types.rule) : string =
  sha256_hex (Yojson.Basic.to_string (Types.rule_to_json r))

(* The as-of view handed to detectors by the replay sweep. *)
type view = {
  v_t : int;
  v_open : Types.market list; (* resolved_ts > t *)
  v_series : (string, (int * float) array) Hashtbl.t; (* full YES series, ts ascending *)
  v_len : (string, int) Hashtbl.t; (* points consumed: series[0 .. len-1] have ts <= t *)
  v_flows : Types.flow list; (* visible: f_last_ts <= t *)
  v_wallet_pnl : (string, float) Hashtbl.t; (* realized, from settled markets *)
  v_market : (string, Types.market) Hashtbl.t; (* all markets incl. resolved *)
}

let latest_p view m_id side =
  match (Hashtbl.find_opt view.v_series m_id, Hashtbl.find_opt view.v_len m_id) with
  | Some arr, Some len when len > 0 ->
      let _, p = arr.(len - 1) in
      let q = if side = Types.Yes then p else 1.0 -. p in
      Some q
  | _ -> None

(* ---- VELOCITY: longshot odds rising N sigma above their own trailing
   baseline (baseline EXCLUDES the current point), minimum absolute
   move. Both sides checked on their own price (No = 1 - p). ---- *)
let detect_velocity view (vp : Types.velocity_p) : Types.signal list =
  List.filter_map
    (fun (m : Types.market) ->
      match (Hashtbl.find_opt view.v_series m.m_id, Hashtbl.find_opt view.v_len m.m_id) with
      | Some arr, Some len when len >= 6 ->
          let last_ts, p_now = arr.(len - 1) in
          let baseline =
            Array.of_list
              (List.filter_map
                 (fun (ts, p) ->
                   if ts > view.v_t - vp.v_window && ts < last_ts then Some p else None)
                 (Array.to_list arr))
          in
          if Array.length baseline < 5 then None
          else begin
            let check ~side ~price ~base =
              let z = Stats.zscore base price in
              let first = base.(0) in
              let move = if side = Types.Yes then price -. first else (1.0 -. price) -. (1.0 -. first) in
              if
                price >= vp.v_min_odds && price <= vp.v_max_odds
                && z >= vp.v_z && move >= vp.v_min_move
              then
                Some
                  ( {
                      Types.sig_market = m.m_id;
                      sig_side = side;
                      sig_kind = Types.Velocity;
                      sig_score = z;
                      sig_ts = view.v_t;
                      sig_evidence =
                        Yojson.Basic.to_string
                          (`Assoc
                            [ ("z", `Float z);
                              ("odds", `Float price);
                              ("move", `Float move);
                            ]);
                    },
                    z )
              else None
            in
            let base_yes = baseline in
            let base_no = Array.map (fun p -> 1.0 -. p) baseline in
            let sy = check ~side:Types.Yes ~price:p_now ~base:base_yes in
            let sn = check ~side:Types.No ~price:(1.0 -. p_now) ~base:base_no in
            (* the two z's oppose each other; take the one that fired *)
            match (sy, sn) with
            | Some (s, _), _ -> Some s
            | None, Some (s, _) -> Some s
            | None, None -> None
          end
      | _ -> None)
    view.v_open

(* ---- LADDER-SWEEP: one wallet buying >= min_markets distinct markets
   within the window with combined usd >= min_usd; cluster-tagged
   markets must all share a tag. Emits one signal per swept market at
   still-cheap odds. ---- *)
let detect_ladder view (lp : Types.ladder_p) : Types.signal list =
  let wflows = Flow.window view.v_flows view.v_t lp.l_window in
  (* per (wallet, market, side) usd within window *)
  let per = Hashtbl.create 32 in
  List.iter
    (fun f ->
      let key = (f.f_wallet, f.f_market, f.f_side) in
      let cur = match Hashtbl.find_opt per key with Some u -> u | None -> 0.0 in
      Hashtbl.replace per key (cur +. f.f_usd))
    wflows;
  let sweeps =
    Cooc.ladder_wallets wflows ~window_s:lp.l_window ~min_markets:lp.l_min_markets
      ~min_usd:lp.l_min_usd view.v_t
  in
  List.concat_map
    (fun (wallet, markets, usd_total) ->
      let tags =
        List.filter_map
          (fun m_id ->
            match Hashtbl.find_opt view.v_market m_id with
            | Some m -> m.Types.m_cluster
            | None -> None)
          markets
      in
      let tag_ok =
        match List.sort_uniq compare tags with
        | [] -> true (* no tags known: no constraint *)
        | [ _ ] -> true
        | _ -> false (* mixed tags: not one ladder *)
      in
      if not tag_ok then []
      else
        List.filter_map
          (fun m_id ->
            (* dominant side for this wallet on this market *)
            let side_usd =
              Hashtbl.fold
                (fun (w, m, s) u acc ->
                  if w = wallet && m = m_id then
                    let cur = match List.assoc_opt s acc with Some v -> v | None -> 0.0 in
                    (s, cur +. u) :: List.remove_assoc s acc
                  else acc)
                per []
            in
            match List.sort (fun a b -> compare (snd b) (snd a)) side_usd with
            | (side, _) :: _ -> (
                match latest_p view m_id side with
                | Some price when price <= lp.l_max_odds ->
                    Some
                      {
                        Types.sig_market = m_id;
                        sig_side = side;
                        sig_kind = Types.Ladder;
                        sig_score = usd_total;
                        sig_ts = view.v_t;
                        sig_evidence =
                          Yojson.Basic.to_string
                            (`Assoc [ ("wallet", `String wallet); ("usd", `Float usd_total) ]);
                      }
                | _ -> None)
            | [] -> None)
          markets)
    sweeps

(* ---- SHARP-ENTRY: a wallet with demonstrated realized pnl opens a
   fresh position (first buy within the last hour) on a longshot. ---- *)
let detect_sharp view (sp : Types.sharp_p) : Types.signal list =
  let fresh =
    List.filter (fun f -> f.Types.f_first_ts > view.v_t - 3600) view.v_flows
  in
  let per = Hashtbl.create 32 in
  List.iter
    (fun f ->
      let key = (f.f_wallet, f.f_market, f.f_side) in
      let cur = match Hashtbl.find_opt per key with Some u -> u | None -> 0.0 in
      Hashtbl.replace per key (cur +. f.f_usd))
    fresh;
  Hashtbl.fold
    (fun (wallet, m_id, side) usd acc ->
      let pnl = match Hashtbl.find_opt view.v_wallet_pnl wallet with Some p -> p | None -> 0.0 in
      if pnl >= sp.s_min_pnl && usd >= sp.s_min_usd then
        match latest_p view m_id side with
        | Some price when price <= sp.s_max_odds ->
            {
              Types.sig_market = m_id;
              sig_side = side;
              sig_kind = Types.Sharp;
              sig_score = pnl;
              sig_ts = view.v_t;
              sig_evidence =
                Yojson.Basic.to_string
                  (`Assoc
                    [ ("wallet", `String wallet);
                      ("pnl", `Float pnl);
                      ("fresh_usd", `Float usd);
                    ]);
            }
            :: acc
        | _ -> acc
      else acc)
    per []

let detect view (rule : Types.rule) : Types.signal list =
  let sigs = [] in
  let sigs =
    match rule.r_velocity with Some vp -> detect_velocity view vp @ sigs | None -> sigs
  in
  let sigs = match rule.r_ladder with Some lp -> detect_ladder view lp @ sigs | None -> sigs in
  let sigs = match rule.r_sharp with Some sp -> detect_sharp view sp @ sigs | None -> sigs in
  sigs
