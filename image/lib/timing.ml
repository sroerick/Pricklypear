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
  steps : int;        (* eval steps billed to the handler's own budget *)
  prim_db : (string * int * float) list; (* db prims: name, calls, ms *)
  at : int64;         (* epoch ms when the sample was taken *)
}

(* Per-route ring of recent samples. *)
let max_samples_per_route = 100
let max_total_samples = 1000

let samples : (string, sample Queue.t) Hashtbl.t = Hashtbl.create 32
let total_count : int ref = ref 0

let now_ms () : int64 =
  Int64.of_float (Unix.gettimeofday () *. 1000.0)

(** {2 Fiber-keyed budget slot (i-1788670103-5)}

    [measure] wraps a handler invocation whose eval creates its OWN
    budget deep inside [Session.eval_string_rim] — the caller cannot
    thread the budget in, and the old pre/post delta against the
    [Eval.last_budget] global was a cross-request race: while this
    request's DB prim awaited, another request's eval entry re-set
    [last_budget], so [record] read a foreign budget's accumulators.

    Instead, [measure] binds a fiber-local slot (Eio fiber key, the
    same pattern as Request_context); the session eval paths publish
    their budget into it via [note_eval_budget], and the post-run read
    sees exactly THIS fiber's eval budget — race-free by construction.
    Outside any Eio fiber (tests, lwt rollback) the key read degrades
    to [None] and samples report zero DB cost. *)

let budget_key : Pricklypear_lisp.Eval.budget option ref Eio.Fiber.key =
  Eio.Fiber.create_key ()

(** Read the fiber-keyed slot; [None] outside any Eio fiber (the key
    read performs Eio's Get_context effect, which raises Unhandled
    there) or when no [measure] window is active. *)
let fiber_slot () : Pricklypear_lisp.Eval.budget option ref option =
  match Eio.Fiber.get budget_key with
  | slot -> slot
  | exception Effect.Unhandled _ -> None

(** Publish [b] as the budget of the eval running inside the active
    [measure] window on this fiber. No-op outside [measure]. *)
let note_eval_budget (b : Pricklypear_lisp.Eval.budget) : unit =
  match fiber_slot () with
  | Some slot -> slot := Some b
  | None -> ()

(** Record a sample for [route], dropping oldest once the ring exceeds
    its bound. [total_ms] is passed in (measured by the caller around
    the handler); db_ms/db_hits/steps are the ABSOLUTE accumulators of
    this request's own eval budget (fresh per eval), read from the
    fiber-keyed slot — no cross-request delta. Per image.budget-ledger,
    steps are what the ledger ranks by. *)
let record ~route ~total_ms ~db_ms ~db_hits ~steps ~prim_db : unit =
  let db_ms = max 0. db_ms and db_hits = max 0 db_hits in
  let q =
    match Hashtbl.find_opt samples route with
    | Some q -> q
    | None ->
        let q = Queue.create () in
        Hashtbl.replace samples route q;
        q
  in
  Queue.add
    { route; total_ms; db_ms; db_hits; steps; prim_db; at = now_ms () }
    q;
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
    sample (even on failure), and return the result + total_ms. The
    fiber-keyed slot is bound for [f]'s extent; the session eval paths
    fill it via [note_eval_budget]. *)
let measure ~route (f : unit -> 'a) : 'a * float =
  let slot = ref None in
  Eio.Fiber.with_binding budget_key slot (fun () ->
      let t0 = Unix.gettimeofday () in
      let result = f () in
      let total_ms = (Unix.gettimeofday () -. t0) *. 1000. in
      let db_ms, db_hits, steps, prim_db =
        match !slot with
        | Some b ->
          let l =
            Hashtbl.fold (fun k (c, t) acc -> (k, c, t) :: acc) b.prim_db []
            |> List.sort (fun (_, c1, _) (_, c2, _) -> compare c2 c1)
            |> fun l -> if List.length l > 12 then
              let rec take n = function
                | x :: rest when n > 0 -> x :: take (n - 1) rest
                | _ -> []
              in
              take 12 l
            else l
          in
          (b.db_ms, b.db_hits, b.steps, l)
        | None -> (0., 0, 0, [])
      in
      record ~route ~total_ms ~db_ms ~db_hits ~steps ~prim_db;
      set_last_total_ms total_ms;
      (result, total_ms))

type agg = {
  route : string;
  count : int;
  last_ms : float;
  avg_ms : float;
  max_ms : float;
  db_ms : float;   (* avg db_ms across samples *)
  db_hits : float; (* avg db_hits across samples *)
  steps : float;   (* avg eval steps across samples (image.budget-ledger) *)
  prim_db_agg : (string * int * float) list; (* per db prim: summed calls
    + db ms across samples, calls-descending (image.db-panel) *)
}

(** Aggregate the current sample set, slowest-first by max_ms. *)
let aggregates () : agg list =
  let out = ref [] in
  Hashtbl.iter
    (fun route q ->
      let count = Queue.length q in
      if count > 0 then begin
        let last_ms = ref 0. and max_ms = ref 0.
        and sum = ref 0. and dbsum = ref 0. and hsum = ref 0.
        and stepsum = ref 0. in
        let psum : (string, int * float) Hashtbl.t = Hashtbl.create 8 in
        Queue.iter
          (fun s ->
            last_ms := s.total_ms;
            max_ms := max !max_ms s.total_ms;
            sum := !sum +. s.total_ms;
            dbsum := !dbsum +. s.db_ms;
            hsum := !hsum +. float_of_int s.db_hits;
            stepsum := !stepsum +. float_of_int s.steps;
            List.iter
              (fun (k, c, t) ->
                let pc, pt = try Hashtbl.find psum k with Not_found -> (0, 0.) in
                Hashtbl.replace psum k (pc + c, pt +. t))
              s.prim_db)
          q;
        out :=
          { route
          ; count
          ; last_ms = !last_ms
          ; avg_ms = !sum /. float_of_int count
          ; max_ms = !max_ms
          ; db_ms = !dbsum /. float_of_int count
          ; db_hits = !hsum /. float_of_int count
          ; steps = !stepsum /. float_of_int count
          ; prim_db_agg =
            Hashtbl.fold (fun k (c, t) acc -> (k, c, t) :: acc) psum []
            |> List.sort (fun (_, c1, _) (_, c2, _) -> compare c2 c1)
          }
          :: !out
      end)
    samples;
  List.sort (fun a b -> compare b.max_ms a.max_ms) !out

(** Return the X-Timing-Ms header (as a [name,value] pair) if a route
    was measured for this request, consuming it so a later response in
    the same request does not double-report. Empty when none. *)
let timing_header () : (string * string) list =
  match !last_total_ms with
  | None -> []
  | Some ms ->
      last_total_ms := None;
      [("x-timing-ms", Printf.sprintf "%.1f" ms)]

(** PERF-5 (roerick 10-22): current per-route render aggregates as JSON,
    for /debug/stats (the /timing page is admin-session-only). *)
let aggregates_json () : string =
  let agg_json (a : agg) =
    let prim =
      List.map
        (fun (k, c, ms) ->
          Printf.sprintf "{\"prim\":\"%s\",\"calls\":%d,\"ms\":%.0f}" k c ms)
        a.prim_db_agg
      |> String.concat ","
    in
    Printf.sprintf
      "{\"route\":\"%s\",\"count\":%d,\"last_ms\":%.0f,\"avg_ms\":%.0f,\"max_ms\":%.0f,\"steps\":%.0f,\"db_hits\":%.0f,\"db_ms\":%.0f,\"prim_db\": [%s]}"
      a.route a.count a.last_ms a.avg_ms a.max_ms a.steps a.db_hits a.db_ms
      prim
  in
  "[" ^ String.concat "," (List.map agg_json (aggregates ())) ^ "]"
