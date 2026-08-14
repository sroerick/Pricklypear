(** Per-route page render timing (borge/image.borg page-timing).

    Captures server-side render cost for each handled user route:
    wall time (total_ms), accumulated DB wall time (db_ms) and DB-hit
    count (db_hits) from the eval budget, plus the wall clock (at).

    The sample set is an in-process bounded ring per route name — no
    schema, no persistence in v1. Aggregated by the /timing system page
    and surfaced per-request via the X-Timing-Ms header. *)
open Pricklypear_lisp.Ast

type sample = {
  route : string;
  total_ms : float;   (* wall time of the handler body *)
  db_ms : float;      (* accumulated DB prim wall time *)
  db_hits : int;      (* DB-touching prim call count *)
  at : int64;         (* epoch ms when the sample was taken *)
}

(* Per-route ring of recent samples. *)
let max_samples_per_route = 100
let max_total_samples = 1000

let samples : (string, sample Queue.t) Hashtbl.t = Hashtbl.create 32
let total_count : int ref = ref 0

let now_ms () : int64 =
  Int64.of_float (Unix.gettimeofday () *. 1000.0)

(** Read the current eval budget's DB accumulators (defaults when no
    budget is active). *)
let budget_db () : float * int =
  match !Pricklypear_lisp.Eval.last_budget with
  | Some b -> (b.db_ms, b.db_hits)
  | None -> (0., 0)

(** Record a sample for [route], dropping oldest once the ring exceeds
    its bound. [total_ms] is passed in (measured by the caller around
    the handler); db_ms/db_hits are read from the just-run eval budget
    as a delta against their pre-call snapshot. *)
let record ~route ~total_ms ~db0 ~hits0 : unit =
  let db_ms, db_hits = budget_db () in
  let db_ms = max 0. (db_ms -. db0)
  and db_hits = max 0 (db_hits - hits0) in
  let q =
    match Hashtbl.find_opt samples route with
    | Some q -> q
    | None ->
        let q = Queue.create () in
        Hashtbl.replace samples route q;
        q
  in
  Queue.add { route; total_ms; db_ms; db_hits; at = now_ms () } q;
  incr total_count;
  (* Trim to bounds. *)
  while Queue.length q > max_samples_per_route do
    ignore (Queue.pop q)
  done;
  if !total_count > max_total_samples then begin
    (* Drop one oldest from the longest ring to keep overall bound. *)
    let longest = ref "" and len = ref 0 in
    Hashtbl.iter
      (fun r q ->
        let n = Queue.length q in
        if n > !len then begin len := n; longest := r end)
      samples;
    if !longest <> "" then
      match Hashtbl.find_opt samples !longest with
      | Some q when not (Queue.is_empty q) ->
          ignore (Queue.pop q);
          decr total_count
      | _ -> ()
  end

(* Request-scoped last measured total_ms, consumed by the HTTP response
   writers to attach the X-Timing-Ms header on handled-route responses. *)
let last_total_ms : float option ref = ref None

let set_last_total_ms (ms : float) : unit = last_total_ms := Some ms

(** Measure [f] (a handler invocation) for [route]: time it, record a
    sample (even on failure), and return the result + total_ms. *)
let measure ~route (f : unit -> 'a) : 'a * float =
  let db0, hits0 = budget_db () in
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let total_ms = (Unix.gettimeofday () -. t0) *. 1000. in
  record ~route ~total_ms ~db0 ~hits0;
  set_last_total_ms total_ms;
  (result, total_ms)

type agg = {
  route : string;
  count : int;
  last_ms : float;
  avg_ms : float;
  max_ms : float;
  db_ms : float;   (* avg db_ms across samples *)
  db_hits : float; (* avg db_hits across samples *)
}

(** Aggregate the current sample set, slowest-first by max_ms. *)
let aggregates () : agg list =
  let out = ref [] in
  Hashtbl.iter
    (fun route q ->
      let count = Queue.length q in
      if count > 0 then begin
        let last_ms = ref 0. and max_ms = ref 0.
        and sum = ref 0. and dbsum = ref 0. and hsum = ref 0. in
        Queue.iter
          (fun s ->
            last_ms := s.total_ms;
            max_ms := max !max_ms s.total_ms;
            sum := !sum +. s.total_ms;
            dbsum := !dbsum +. s.db_ms;
            hsum := !hsum +. float_of_int s.db_hits)
          q;
        out :=
          { route
          ; count
          ; last_ms = !last_ms
          ; avg_ms = !sum /. float_of_int count
          ; max_ms = !max_ms
          ; db_ms = !dbsum /. float_of_int count
          ; db_hits = !hsum /. float_of_int count
          }
          :: !out
      end)
    samples;
  List.sort (fun a b -> compare b.max_ms a.max_ms) !out

(** Drop all samples (used by tests / future clear affordance). *)
let clear () : unit =
  Hashtbl.clear samples;
  total_count := 0

(** Return the X-Timing-Ms header (as a [name,value] pair) if a route
    was measured for this request, consuming it so a later response in
    the same request does not double-report. Empty when none. *)
let timing_header () : (string * string) list =
  match !last_total_ms with
  | None -> []
  | Some ms ->
      last_total_ms := None;
      [("x-timing-ms", Printf.sprintf "%.1f" ms)]
