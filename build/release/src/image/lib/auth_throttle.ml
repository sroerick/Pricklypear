(** Pricklypear Image — per-username login throttling (pure, in-memory).

    Brute-force resistance for password login (i-1787970741): a sliding
    window of failed attempts per (lowercased) username with exponential
    backoff once the window fills. Pure — no DB: state is a bounded
    Hashtbl and the caller supplies the clock ([now] as a float), so
    behavior is deterministic and testable.

    Invariant: throttling engages ONLY after recorded failures. Fewer
    than [max_failures] failures inside the window always admits —
    verify_password runs on every Basic-auth request, so correct logins
    are never blocked. A success clears the key entirely.

    Single-domain: the image runs one Eio domain and these functions
    contain no effect points, so each call is atomic; no lock needed. *)

let env_positive_int ~default name =
  match Sys.getenv_opt name with
  | None -> default
  | Some s ->
      (match int_of_string_opt s with
       | Some n when n > 0 -> n
       | _ -> default)

let env_positive_float ~default name =
  match Sys.getenv_opt name with
  | None -> default
  | Some s ->
      (match float_of_string_opt s with
       | Some f when f > 0.0 -> f
       | _ -> default)

(** Failed logins allowed inside the window before backoff engages. *)
let max_failures () = env_positive_int ~default:10 "PP_LOGIN_MAX_FAILURES"

(** Sliding-window width in seconds. *)
let window_secs () = env_positive_float ~default:900.0 "PP_LOGIN_WINDOW_SECS"

(** First lockout once the threshold is hit; doubles per extra failure. *)
let backoff_base () =
  env_positive_float ~default:30.0 "PP_LOGIN_BACKOFF_BASE_SECS"

(** Hard ceiling on any single lockout. *)
let backoff_cap = 900.0

(** Table bound: a username flood evicts expired entries, then the
    least-recently-failed key, instead of growing memory unboundedly. *)
let max_entries = 4096

type state = {
  mutable failures : float list;  (* descending: newest first *)
}

let table : (string, state) Hashtbl.t = Hashtbl.create 64

(** Per-IP table (i-1787970741 second half): a credential-stuffing sweep
    across MANY usernames defeats per-username throttling — each key stays
    under max_failures while the source IP hammers. Same machinery, same
    bounds, own bucket space keyed by the request source IP. Both gates
    must admit (username AND ip); a success clears both. *)
let ip_table : (string, state) Hashtbl.t = Hashtbl.create 64

(** Wall clock in seconds; exposed so callers share one time source. *)
let now () : float = Unix.gettimeofday ()

let size () : int = Hashtbl.length table

(* Keep the failures still inside the window. *)
let prune ~window now failures =
  let cutoff = now -. window in
  List.filter (fun t -> t > cutoff) failures

(* base * 2^excess clamped at backoff_cap. Doubling loop rather than
   [**] so an absurd failure count cannot overflow to infinity. *)
let backoff_for (base : float) (excess : int) : float =
  let rec go acc e =
    if e <= 0 then acc
    else if acc >= backoff_cap then backoff_cap
    else go (acc *. 2.0) (e - 1)
  in
  min backoff_cap (go base excess)

(* Make room for a new key: drop entries whose failures have all slid
   out of the window first; if still at capacity, evict the entry whose
   most recent failure is oldest. *)
let evict now =
  let window = window_secs () in
  let dead =
    Hashtbl.fold
      (fun k st acc ->
        let kept = prune ~window now st.failures in
        st.failures <- kept;
        if kept = [] then k :: acc else acc)
      table []
  in
  List.iter (Hashtbl.remove table) dead;
  if Hashtbl.length table >= max_entries then begin
    let oldest =
      Hashtbl.fold
        (fun k st acc ->
          let t = match st.failures with t :: _ -> t | [] -> 0.0 in
          match acc with
          | None -> Some (k, t)
          | Some (_, t0) -> if t < t0 then Some (k, t) else acc)
        table None
    in
    match oldest with Some (k, _) -> Hashtbl.remove table k | None -> ()
  end

let state_of ~now key =
  match Hashtbl.find_opt table key with
  | Some st -> st
  | None ->
      if Hashtbl.length table >= max_entries then evict now;
      let st = { failures = [] } in
      Hashtbl.add table key st;
      st

(** Record a failed login for [key] at time [now]. *)
let record_failure (key : string) (now : float) : unit =
  let window = window_secs () in
  let st = state_of ~now key in
  st.failures <- now :: prune ~window now st.failures

(** May [key] attempt a login at time [now]? True unless the window
    already holds >= max_failures failures AND the backoff those
    failures bought (doubled per failure past the threshold, capped)
    has not yet elapsed since the most recent one. *)
let admit (key : string) (now : float) : bool =
  match Hashtbl.find_opt table key with
  | None -> true
  | Some st ->
      let window = window_secs () in
      let kept = prune ~window now st.failures in
      st.failures <- kept;
      let n = List.length kept in
      let maxf = max_failures () in
      if n < maxf then true
      else
        let backoff = backoff_for (backoff_base ()) (n - maxf) in
        now >= (List.hd kept) +. backoff

(** Forget [key] entirely — a successful login clears the throttle. *)
let clear (key : string) : unit = Hashtbl.remove table key

(* --- per-IP variants: the SAME machinery over [ip_table]. The core
   admit/record/evict logic is duplicated by parameterization below —
   extracting it into a [tbl -> ...] core kept the public surface of the
   module unchanged for the existing callers. *)

let evict_on (tbl : (string, state) Hashtbl.t) now =
  let window = window_secs () in
  let dead =
    Hashtbl.fold
      (fun k st acc ->
        let kept = prune ~window now st.failures in
        st.failures <- kept;
        if kept = [] then k :: acc else acc)
      tbl []
  in
  List.iter (Hashtbl.remove tbl) dead;
  if Hashtbl.length tbl >= max_entries then begin
    let oldest =
      Hashtbl.fold
        (fun k st acc ->
          let t = match st.failures with t :: _ -> t | [] -> 0.0 in
          match acc with
          | None -> Some (k, t)
          | Some (_, t0) -> if t < t0 then Some (k, t) else acc)
        tbl None
    in
    match oldest with Some (k, _) -> Hashtbl.remove tbl k | None -> ()
  end

let state_of_on (tbl : (string, state) Hashtbl.t) ~now key =
  match Hashtbl.find_opt tbl key with
  | Some st -> st
  | None ->
      if Hashtbl.length tbl >= max_entries then evict_on tbl now;
      let st = { failures = [] } in
      Hashtbl.add tbl key st;
      st

(** Record a failed login from source ip [key] at time [now]. *)
let record_failure_ip (key : string) (now : float) : unit =
  let window = window_secs () in
  let st = state_of_on ip_table ~now key in
  st.failures <- now :: prune ~window now st.failures

(** May source ip [key] attempt a login at time [now]? Same window +
    escalating-backoff rule as the username gate. Empty/absent ip keys
    are ignored by the caller (no bucket). *)
let admit_ip (key : string) (now : float) : bool =
  match Hashtbl.find_opt ip_table key with
  | None -> true
  | Some st ->
      let window = window_secs () in
      let kept = prune ~window now st.failures in
      st.failures <- kept;
      let n = List.length kept in
      let maxf = max_failures () in
      if n < maxf then true
      else
        let backoff = backoff_for (backoff_base ()) (n - maxf) in
        now >= (List.hd kept) +. backoff

(** Forget source ip [key] — a successful login clears the IP bucket too. *)
let clear_ip (key : string) : unit = Hashtbl.remove ip_table key
