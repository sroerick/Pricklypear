(* upsets.analytics-bench — the fill model. A signal at t fills at the
   NEXT ACTUAL PRINT on that token after t (never the signal tick's
   price — fill-discipline verify), adjusted by a depth haircut whose
   formula is fully named in the rule record (parity-contract). *)

open Types

type order = {
  ord_market : string;
  ord_side : Types.side;
  ord_kind : Types.signal_kind;
  ord_signal_ts : int;
  ord_fill_ts : int;
  ord_entry : float;
  ord_size : float;
}

(* next print strictly after t, priced for `side` *)
let next_print (series : (int * float) array) (idx : int) (t : int)
    (side : Types.side) : (int * float) option
  =
  if idx < Array.length series then
    let ts, p = series.(idx) in
    if ts > t then
      let price = if side = Types.Yes then p else 1.0 -. p in
      if price > 0.001 && price < 0.999 then Some (ts, price) else None
    else None (* pointer invariant broken — should not happen *)
  else None

let size_usd (sizing : Types.sizing_p) : float =
  Float.min (sizing.Types.sz_frac *. sizing.Types.sz_bankroll) sizing.Types.sz_cap

(* depth_est: trailing-24h visible flow usd on the market (floor 500) *)
let fill_price ~(p_next : float) ~(size : float) ~(depth_usd : float)
    (sizing : Types.sizing_p) : float
  =
  let depth = Float.max depth_usd 500.0 in
  let slip =
    Float.min sizing.Types.sz_max_slip
      (sizing.Types.sz_slip_coef *. size /. depth)
  in
  Float.min 0.98 (p_next *. (1.0 +. slip))

(* pnl of a settled order; fees on entry notional *)
let settle ~(entry : float) ~(size : float) ~(won : bool) (sizing : Types.sizing_p) : float
  =
  let fee = sizing.Types.sz_fees_bps /. 1e4 *. size in
  if won then ((size /. entry) -. size) -. fee else -.size -. fee
