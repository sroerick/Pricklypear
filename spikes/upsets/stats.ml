(* upsets.analytics-bench — statistics over point-in-time windows.
   Pure: arrays in, floats out. Points are (ts, value) sorted ascending. *)

let mean (a : float array) : float =
  if Array.length a = 0 then 0.0
  else Array.fold_left ( +. ) 0.0 a /. float_of_int (Array.length a)

let stdev (a : float array) ~(mean : float) : float =
  let n = Array.length a in
  if n < 2 then 0.0
  else
    let s =
      Array.fold_left (fun acc x -> acc +. ((x -. mean) *. (x -. mean))) 0.0 a
    in
    sqrt (s /. float_of_int (n - 1))

(* values with ts in (t - w, t] — strictly trailing, point at t included *)
let trailing_values (pts : (int * float) array) (t : int) (w : int) : float array =
  let lo_bound = t - w in
  let acc = ref [] in
  Array.iter
    (fun (ts, v) -> if ts > lo_bound && ts <= t then acc := v :: !acc)
    pts;
  Array.of_list (List.rev !acc)

let zscore (values : float array) (x : float) : float =
  let m = mean values in
  let s = stdev values ~mean:m in
  if s < 1e-9 then 0.0 else (x -. m) /. s

let clamp lo hi x = max lo (min hi x)
