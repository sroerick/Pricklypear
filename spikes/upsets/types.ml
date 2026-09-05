(* spikes/upsets — borge/upsets.borg analytics-bench: domain types.
   Mirrors the data-model subsection: pm-market, pm-odds, wallet-flow,
   plus the rule record (L-5: rules are data — canonical JSON + hash).
   Snapshot JSON is the boundary format between the habitat export and
   this bench; the bench never talks to Postgres. *)

type side = Yes | No

let string_of_side = function Yes -> "YES" | No -> "NO"

let side_of_string = function
  | "YES" | "Yes" | "yes" -> Yes
  | "NO" | "No" | "no" -> No
  | s -> invalid_arg ("side_of_string: " ^ s)

let other = function Yes -> No | No -> Yes

(* pm-market row. outcome_won is the resolved side; resolved_ts is the
   settlement time used by replay to settle open orders. *)
type market = {
  m_id : string;
  m_question : string;
  m_slug : string;
  m_condition_id : string;
  m_cluster : string option; (* correlated bucket-ladder tag, if known *)
  m_vol : float;
  m_resolved_ts : int;
  m_outcome_won : side;
}

(* pm-odds row: YES-token price at hourly fidelity. NO price = 1 - p. *)
type odds_point = { o_market : string; o_ts : int; o_p : float }

(* wallet-flow row — INGEST-TIME AGGREGATE (chapter law: aggregate,
   don't hoard). f_side is the OUTCOME side the wallet BOUGHT (sell
   prints are dropped at export time). A row is visible to the replay
   sweep only from f_last_ts (conservative: fully-formed aggregates). *)
type flow = {
  f_market : string;
  f_wallet : string;
  f_side : side;
  f_usd : float;
  f_n : int;
  f_first_ts : int;
  f_last_ts : int;
}

type snapshot = {
  s_name : string;
  s_window_kind : string; (* train | validate | holdout *)
  s_from : int;
  s_to : int;
  s_markets : market list;
  s_odds : odds_point list; (* YES price; NO derivable *)
  s_flows : flow list;
}

type signal_kind = Velocity | Ladder | Sharp

let string_of_kind = function Velocity -> "VELOCITY" | Ladder -> "LADDER-SWEEP" | Sharp -> "SHARP-ENTRY"

type signal = {
  sig_market : string;
  sig_side : side;
  sig_kind : signal_kind;
  sig_score : float;
  sig_ts : int;
  sig_evidence : string; (* JSON string; wallet ids, z, etc. *)
}

(* L-5: the rule record. Slippage + fees are NAMED PARAMS so both
   engines (OCaml bench, Nopales replay) apply identical fill math —
   see upsets.parity-contract. *)
type velocity_p = {
  v_window : int; (* seconds *)
  v_z : float;
  v_min_odds : float;
  v_max_odds : float;
  v_min_move : float;
}

type ladder_p = {
  l_window : int;
  l_min_markets : int;
  l_min_usd : float;
  l_max_odds : float;
}

type sharp_p = { s_min_pnl : float; s_min_usd : float; s_max_odds : float }

type sizing_p = {
  sz_frac : float; (* fixed fraction of bankroll per signal *)
  sz_bankroll : float;
  sz_cap : float; (* max usd per order *)
  sz_slip_coef : float;
  sz_max_slip : float;
  sz_fees_bps : float;
  sz_cooldown : int; (* min seconds between signals on (market, side, kind) *)
}

type rule = {
  r_velocity : velocity_p option;
  r_ladder : ladder_p option;
  r_sharp : sharp_p option;
  r_sizing : sizing_p;
}

(* ---- JSON (Yojson.Basic) ---- *)

let member = Yojson.Basic.Util.member

(* tolerant numeric readers: emit int where float expected and vice versa *)
let to_float_j = function
  | `Float f -> f
  | `Int i -> float_of_int i
  | j -> Yojson.Basic.Util.to_float j

let to_int_j = function
  | `Int i -> i
  | `Float f -> int_of_float f
  | j -> Yojson.Basic.Util.to_int j

let to_string_j = Yojson.Basic.Util.to_string
let to_list_j = Yojson.Basic.Util.to_list

let opt_string j k =
  match member k j with `Null -> None | v -> Some (to_string_j v)

let opt_float j k =
  match member k j with `Null -> None | v -> Some (to_float_j v)

let market_of_json j =
  {
    m_id = to_string_j (member "id" j);
    m_question = to_string_j (member "question" j);
    m_slug = to_string_j (member "slug" j);
    m_condition_id = to_string_j (member "condition_id" j);
    m_cluster = opt_string j "cluster";
    m_vol = to_float_j (member "vol" j);
    m_resolved_ts = to_int_j (member "resolved_ts" j);
    m_outcome_won = side_of_string (to_string_j (member "outcome_won" j));
  }

let odds_of_json j =
  {
    o_market = to_string_j (member "market_id" j);
    o_ts = to_int_j (member "ts" j);
    o_p = to_float_j (member "p" j);
  }

let flow_of_json j =
  {
    f_market = to_string_j (member "market_id" j);
    f_wallet = to_string_j (member "wallet" j);
    f_side = side_of_string (to_string_j (member "side" j));
    f_usd = to_float_j (member "usd" j);
    f_n = to_int_j (member "n" j);
    f_first_ts = to_int_j (member "first_ts" j);
    f_last_ts = to_int_j (member "last_ts" j);
  }

let snapshot_of_json (j : Yojson.Basic.t) : snapshot =
  let markets = List.map market_of_json (to_list_j (member "markets" j)) in
  let odds = List.map odds_of_json (to_list_j (member "odds" j)) in
  let flows = List.map flow_of_json (to_list_j (member "flows" j)) in
  let name = match opt_string j "name" with Some n -> n | None -> "snapshot" in
  let window_kind =
    match opt_string j "window_kind" with Some w -> w | None -> "train"
  in
  let from_ts =
    match opt_float j "from" with
    | Some f -> int_of_float f
    | None ->
        List.fold_left (fun a o -> min a o.o_ts) max_int odds
  in
  let to_ts =
    match opt_float j "to" with
    | Some f -> int_of_float f
    | None ->
        List.fold_left (fun a m -> max a m.m_resolved_ts) 0 markets
  in
  { s_name = name; s_window_kind = window_kind; s_from = from_ts; s_to = to_ts; s_markets = markets; s_odds = odds; s_flows = flows }

(* canonical field order — both engines must serialize identically
   (parity-contract); yojson float repr is deterministic. *)
let json_of_side = function Yes -> `String "YES" | No -> `String "NO"

let velocity_to_json v =
  `Assoc
    [ ("window", `Int v.v_window);
      ("z", `Float v.v_z);
      ("min_odds", `Float v.v_min_odds);
      ("max_odds", `Float v.v_max_odds);
      ("min_move", `Float v.v_min_move);
    ]

let ladder_to_json l =
  `Assoc
    [ ("window", `Int l.l_window);
      ("min_markets", `Int l.l_min_markets);
      ("min_usd", `Float l.l_min_usd);
      ("max_odds", `Float l.l_max_odds);
    ]

let sharp_to_json s =
  `Assoc
    [ ("min_pnl", `Float s.s_min_pnl);
      ("min_usd", `Float s.s_min_usd);
      ("max_odds", `Float s.s_max_odds);
    ]

let sizing_to_json s =
  `Assoc
    [ ("frac", `Float s.sz_frac);
      ("bankroll", `Float s.sz_bankroll);
      ("cap", `Float s.sz_cap);
      ("slip_coef", `Float s.sz_slip_coef);
      ("max_slip", `Float s.sz_max_slip);
      ("fees_bps", `Float s.sz_fees_bps);
      ("cooldown", `Int s.sz_cooldown);
    ]

let sizing_of_json j =
  {
    sz_frac = to_float_j (member "frac" j);
    sz_bankroll = to_float_j (member "bankroll" j);
    sz_cap = to_float_j (member "cap" j);
    sz_slip_coef = to_float_j (member "slip_coef" j);
    sz_max_slip = to_float_j (member "max_slip" j);
    sz_fees_bps = to_float_j (member "fees_bps" j);
    sz_cooldown = match opt_float j "cooldown" with Some f -> int_of_float f | None -> 86400;
  }

let rule_to_json (r : rule) : Yojson.Basic.t =
  let opt name enc = function None -> (name, `Null) | Some v -> (name, enc v) in
  `Assoc
    [ opt "velocity" velocity_to_json r.r_velocity;
      opt "ladder" ladder_to_json r.r_ladder;
      opt "sharp" sharp_to_json r.r_sharp;
      ("sizing", sizing_to_json r.r_sizing);
    ]

let velocity_of_json j =
  {
    v_window = to_int_j (member "window" j);
    v_z = to_float_j (member "z" j);
    v_min_odds = to_float_j (member "min_odds" j);
    v_max_odds = to_float_j (member "max_odds" j);
    v_min_move = to_float_j (member "min_move" j);
  }

let ladder_of_json j =
  {
    l_window = to_int_j (member "window" j);
    l_min_markets = to_int_j (member "min_markets" j);
    l_min_usd = to_float_j (member "min_usd" j);
    l_max_odds = to_float_j (member "max_odds" j);
  }

let sharp_of_json j =
  {
    s_min_pnl = to_float_j (member "min_pnl" j);
    s_min_usd = to_float_j (member "min_usd" j);
    s_max_odds = to_float_j (member "max_odds" j);
  }

let rule_of_json (j : Yojson.Basic.t) : rule =
  let sub k = match member k j with `Null -> None | sj -> Some sj in
  {
    r_velocity = Option.map velocity_of_json (sub "velocity");
    r_ladder = Option.map ladder_of_json (sub "ladder");
    r_sharp = Option.map sharp_of_json (sub "sharp");
    r_sizing = sizing_of_json (member "sizing" j);
  }

(* canonical snapshot JSON — the dataset hash domain *)
let snapshot_to_json (s : snapshot) : Yojson.Basic.t =
  `Assoc
    [ ("name", `String s.s_name);
      ("window_kind", `String s.s_window_kind);
      ("from", `Int s.s_from);
      ("to", `Int s.s_to);
      ( "markets",
        `List
          (List.map
             (fun m ->
               `Assoc
                 [ ("id", `String m.m_id);
                   ("question", `String m.m_question);
                   ("slug", `String m.m_slug);
                   ("condition_id", `String m.m_condition_id);
                   ( "cluster",
                     match m.m_cluster with Some c -> `String c | None -> `Null );
                   ("vol", `Float m.m_vol);
                   ("resolved_ts", `Int m.m_resolved_ts);
                   ("outcome_won", json_of_side m.m_outcome_won);
                 ])
             s.s_markets) );
      ( "odds",
        `List
          (List.map
             (fun o ->
               `Assoc
                 [ ("market_id", `String o.o_market);
                   ("ts", `Int o.o_ts);
                   ("p", `Float o.o_p);
                 ])
             s.s_odds) );
      ( "flows",
        `List
          (List.map
             (fun f ->
               `Assoc
                 [ ("market_id", `String f.f_market);
                   ("wallet", `String f.f_wallet);
                   ("side", json_of_side f.f_side);
                   ("usd", `Float f.f_usd);
                   ("n", `Int f.f_n);
                   ("first_ts", `Int f.f_first_ts);
                   ("last_ts", `Int f.f_last_ts);
                 ])
             s.s_flows) );
    ]
