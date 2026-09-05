(* upsets.analytics-bench — wallet-flow aggregation.
   Visibility law: a flow row enters the as-of view only when
   f_last_ts <= t (the aggregate is fully formed). *)

open Types

module Misc = struct
  let rec take n = function
    | [] -> []
    | x :: rest -> if n <= 0 then [] else x :: take (n - 1) rest
end

type wagg = { wa_usd : float; wa_markets : string list; wa_sides : Types.side list }

let visible (flows : Types.flow list) (t : int) : Types.flow list =
  List.filter (fun f -> f.f_last_ts <= t) flows

let window (flows : Types.flow list) (t : int) (w : int) : Types.flow list =
  List.filter (fun f -> f.f_last_ts <= t && f.f_last_ts > t - w) flows

(* per-wallet aggregation over a flow subset *)
let by_wallet (flows : Types.flow list) : (string, wagg) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun f ->
      let w =
        match Hashtbl.find_opt tbl f.f_wallet with
        | Some w -> w
        | None -> { wa_usd = 0.0; wa_markets = []; wa_sides = [] }
      in
      Hashtbl.replace tbl f.f_wallet
        {
          wa_usd = w.wa_usd +. f.f_usd;
          wa_markets = f.f_market :: w.wa_markets;
          wa_sides = f.f_side :: w.wa_sides;
        })
    flows;
  tbl

let distinct ls = List.sort_uniq compare ls

(* top-k share of usd across flows (concentration metric) *)
let topk_share (flows : Types.flow list) (k : int) : float =
  let usds = List.map (fun f -> f.f_usd) flows |> List.sort (fun a b -> compare b a) in
  let total = List.fold_left ( +. ) 0.0 usds in
  if total <= 0.0 then 0.0
  else
    let top = Misc.take k usds in
    List.fold_left ( +. ) 0.0 top /. total
