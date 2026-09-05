(* upsets.analytics-bench — wallet x market co-occurrence.
   Clusters = connected components over markets that share wallets
   (edge weight = number of shared qualifying wallets). Feeds the
   LADDER-SWEEP detector and the /upsets/wallets view. *)

open Types

module SSet = Set.Make (String)

type uf = { parent : (string, string) Hashtbl.t }

let uf_find uf x =
  let rec go x =
    match Hashtbl.find_opt uf.parent x with
    | None -> x
    | Some p -> if p = x then x else let r = go p in Hashtbl.replace uf.parent x r; r
  in
  go x

let uf_union uf a b =
  let ra, rb = (uf_find uf a, uf_find uf b) in
  if ra <> rb then Hashtbl.replace uf.parent ra rb

(* Markets clustered by shared wallets whose aggregated usd on the
   shared market pair >= min_usd. Returns (root, markets) components
   with >= 2 markets. *)
let clusters (flows : Types.flow list) ~(min_usd : float) :
    (string * string list) list
  =
  (* per (wallet, market) usd *)
  let wm = Hashtbl.create 64 in
  List.iter
    (fun f ->
      let key = (f.f_wallet, f.f_market) in
      let cur = match Hashtbl.find_opt wm key with Some u -> u | None -> 0.0 in
      Hashtbl.replace wm key (cur +. f.f_usd))
    flows;
  let wallet_markets = Hashtbl.create 64 in
  Hashtbl.iter
    (fun (w, m) usd ->
      if usd >= min_usd then
        let ms =
          match Hashtbl.find_opt wallet_markets w with
          | Some s -> s
          | None -> SSet.empty
        in
        Hashtbl.replace wallet_markets w (SSet.add m ms))
    wm;
  let uf = { parent = Hashtbl.create 64 } in
  Hashtbl.iter
    (fun _ ms ->
      let ms = SSet.elements ms in
      match ms with
      | _ :: _ :: _ -> List.iter (uf_union uf (List.hd ms)) ms
      | _ -> ())
    wallet_markets;
  let comps = Hashtbl.create 32 in
  Hashtbl.iter (fun (w, m) _ -> if Hashtbl.mem wallet_markets w then
      let r = uf_find uf m in
      let cur = match Hashtbl.find_opt comps r with Some l -> l | None -> [] in
      Hashtbl.replace comps r (m :: cur)) wm;
  Hashtbl.fold
    (fun root ms acc ->
      let ms = List.sort_uniq compare ms in
      if List.length ms >= 2 then (root, ms) :: acc else acc)
    comps []

(* LADDER-SWEEP: wallets buying >= min_markets DISTINCT markets within
   the trailing window with combined usd >= min_usd. If markets carry
   explicit cluster tags, all swept markets must share one tag. *)
let ladder_wallets (flows : Types.flow list) ~(window_s : int) ~(min_markets : int)
    ~(min_usd : float) (t : int) : (string * string list * float) list
  (* (wallet, markets, usd) *)
  =
  let wflows = Flow.window flows t window_s in
  let per_wallet = Hashtbl.create 32 in
  List.iter
    (fun f ->
      let cur =
        match Hashtbl.find_opt per_wallet f.f_wallet with
        | Some (ms, usd) -> (SSet.add f.f_market ms, usd +. f.f_usd)
        | None -> (SSet.singleton f.f_market, f.f_usd)
      in
      Hashtbl.replace per_wallet f.f_wallet cur)
    wflows;
  Hashtbl.fold
    (fun w (ms, usd) acc ->
      let ms = SSet.elements ms in
      if List.length ms >= min_markets && usd >= min_usd then (w, ms, usd) :: acc
      else acc)
    per_wallet []
