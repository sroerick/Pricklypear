(** tmp/eval-fast-path: process-global def-write journal for incremental
    rehydrate.

    The image is the single writer of definition rows (persist_function /
    upsert_route / unbind / merge / promote / reseed all funnel through a
    handful of store-layer sites). Every fn/route def write appends a
    journal entry; sessions drain entries newer than their own cursor
    and rebind exactly those defs — instead of re-fetching and
    re-parsing the whole visible chain (~2800 bodies, ~280-520ms) on
    every staleness.

    Correctness contract (with image.per-identity-sessions STALENESS):
    the cheap def_gen stamp check stays the BACKSTOP. Journal coverage
    is in-process only; an out-of-band write (manual psql, another image
    process on the same DB) bumps the stamp without a journal entry, and
    the session falls back to a full load. A stamp change with journal
    entries for the session's chain is served entirely from the journal.

    Per-cursor overflow: a cursor older than the ring's earliest
    retained entry means that session may have missed writes — reported
    as overflow for THAT caller only (full load resyncs). *)

type op =
  | Upsert of string  (* branch_id *)
  | Remove of string  (* branch_id *)
  | Routes of string  (* branch_id: route table dirty *)

type entry = { seq : int; op : op; name : string }

type delta = {
  ups : (string * string) list;   (* (branch_id, name), latest wins *)
  rems : (string * string) list;  (* (branch_id, name) *)
  routes_dirty : bool;            (* any route write on any branch *)
  overflow : bool;                (* caller's cursor predates the ring *)
  epoch : int;                    (* current journal seq *)
}

let cap = 4096

let mu = Mutex.create ()
let ring : entry Queue.t = Queue.create ()
let counter = Atomic.make 0
(* seq of the oldest retained entry (valid when the ring is non-empty) *)
let min_retained = Atomic.make 0

let push (op : op) (name : string) : unit =
  Mutex.lock mu;
  let seq = Atomic.fetch_and_add counter 1 in
  Queue.add { seq; op; name } ring;
  while Queue.length ring > cap do
    let dropped = Queue.pop ring in
    Atomic.set min_retained (dropped.seq + 1)
  done;
  Mutex.unlock mu

let record_upsert ~(branch_id : string) ~(name : string) : unit =
  push (Upsert branch_id) name

let record_remove ~(branch_id : string) ~(name : string) : unit =
  push (Remove branch_id) name

let record_routes ~(branch_id : string) : unit =
  push (Routes branch_id) "routes"

(** Entries newer than [after_seq], folded so the LAST event per
    (branch, name) wins. overflow=true when [after_seq] predates the
    ring's retention window (caller must full-load). *)
let since (after_seq : int) : delta =
  Mutex.lock mu;
  let entries = List.rev (Queue.fold (fun acc (e : entry) -> e :: acc) [] ring) in
  let epoch = Atomic.get counter in
  let min_seq = Atomic.get min_retained in
  Mutex.unlock mu;
  let overflow = after_seq < min_seq - 1 in
  (* fold latest-wins per (branch, name); routes collapse to a flag *)
  let latest : (string * string, string * bool) Hashtbl.t =
    Hashtbl.create 16 in
  let routes_dirty =
    List.fold_left
      (fun dirty (e : entry) ->
        match e.op with
        | Upsert b ->
            Hashtbl.replace latest (b, e.name) (b, false);
            dirty
        | Remove b ->
            Hashtbl.replace latest (b, e.name) (b, true);
            true
        | Routes _ -> true)
      false entries in
  let ups, rems =
    Hashtbl.fold
      (fun (b, name) (_, is_remove) (ups, rems) ->
        if is_remove then (ups, (b, name) :: rems)
        else ((b, name) :: ups, rems))
      latest ([], [])
  in
  { ups; rems; routes_dirty; overflow; epoch }

(* Cheap epoch probe: any entry newer than [after_seq]? *)
let changed_since (after_seq : int) : bool =
  Mutex.lock mu;
  let any =
    Queue.fold (fun acc (e : entry) -> acc || e.seq > after_seq) false ring in
  Mutex.unlock mu;
  any

let current_epoch () : int = Atomic.get counter
