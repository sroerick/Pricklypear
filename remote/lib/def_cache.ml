(** remote/lib/def_cache.ml (borge/remote-eval.borg caching).

    The client-side half of the definition cache, keyed
    (branch, generation). The client materializes defs lazily into its
    env (client subsection, [Remote.load_def]); this module is the
    INVALIDATION half: "is my (branch, generation) still current?" is a
    single cheap round trip (two single-counter read prims) — NOT an
    N-way hash diff over every def.

    KEY. The cache holds (def-name -> (value, source-generation)) and a
    pinned (branch, generation) key. Generation is migration-017
    branch_generations — the SAME coordinate tree-time reads as-of and
    the (gen-now) prim read (the campaign-7.0 dependency pin): one
    counter, one namespace, no independent version clock to drift.

    INVALIDATION ([check] returns the names to evict from the client
    env so the next reference re-fetches from the writer — the server is
    the source of truth, the cache never is):
      - Branch is the namespace: a checkout to a different branch
        invalidates the WHOLE cache (every cached name is returned for
        eviction).
      - A generation bump (a write anywhere in the branch's ancestry —
        a promote into main, a define on personal, a data write) marks
        every def loaded before the new generation STALE and returns
        their names for eviction.
      - No writes -> the check round trip returns unchanged-generation
        and returns [] (nothing touched).
    A def-free / pure-compute session never pins a coordinate, so
    [is_active] is false and the round trip is skipped entirely (zero
    HTTP on pure compute).

    TRANSPORT-DRIVEN (proxy rim discipline). This module does zero
    interpretation itself. It reads the two counter prims (current-branch
    + gen-now, both proxyable reads registered on the image) through the
    same [Proxy_rim.transport] everything else uses, so it is unit-testable
    with a mock (no live server). It never writes. It is a cache +
    interpreter, never a source of truth. *)

open Pricklypear_lisp
open Ast

type transport = Proxy_rim.transport

(** A materialized def: the value [load_def] produced under the env and
    the generation counter it was fetched at (so a bump can drop it). *)
type entry = {
  value : value;
  source_gen : int64; (* branch_generations at fetch time *)
}

type t = {
  mutable branch : string; (* branch namespace *)
  mutable generation : int64; (* cached generation *)
  mutable initialized : bool; (* branch + gen pinned yet? *)
  mutable table : (string, entry) Hashtbl.t;
  transport : transport;
}

let create ~(transport : transport) () : t =
  {
    branch = "";
    generation = 0L;
    initialized = false;
    table = Hashtbl.create 16;
    transport;
  }

(** Whether the cache is live (a (branch, gen) coordinate has been
    pinned). A pure-compute session that never materialized a def stays
    [!is_active], so [Remote] skips the check round trip entirely. *)
let is_active (t : t) : bool = t.initialized

(** One cheap round trip: current-branch + gen-now, both proxyable read
    prims (single counters). Returns (branch, generation). Either prim
    failing resolves conservatively to ("" , 0) — a caller treating an
    unknown coordinate as "recheck later / at head" stays safe. *)
let read_key (t : t) : (string * int64) option =
  (* The transport may RAISE on connection failure (the Eio HTTP
     transport raises [Eio.Io], it does not return [Error]); catch
     both so an offline client conservatively treats the coordinate as
     unknown rather than crashing — the [check] docstring's "transport
     failure conservatively returns []" contract, made honest. *)
  let branch =
    match (try t.transport "current-branch" [] with _ -> Error (Failure "offline")) with
    | Ok (VStr b) -> b
    | _ -> ""
  in
  let gen =
    match (try t.transport "gen-now" [] with _ -> Error (Failure "offline")) with
    | Ok (VInt g) -> g
    | _ -> 0L
  in
  Some (branch, gen)

(** The cache-check round trip. Returns the list of def names whose ENV
    binding should be evicted (for the client to re-fetch on next use).
    Idempotent; [] (no evictions) on the unchanged path.

    - First call: pin (branch, generation) and return [].
    - Branch change: the namespace moved — clear the WHOLE cache and
      return every cached name (evict all from the env).
    - Generation bump: drop every def loaded before the new generation
      and return their names (evict only stale defs from the env).
    - Otherwise: unchanged — a single cheap HEAD-ish round trip, [].
    A transport failure (none of the reads answered) conservatively
    returns [] (treat as "recheck later"; never evict on uncertainty). *)
let check (t : t) : string list =
  match read_key t with
  | Some (branch, gen) ->
      if not t.initialized then (
        t.branch <- branch;
        t.generation <- gen;
        t.initialized <- true;
        [])
      else if branch <> t.branch then (
        (* branch is the namespace — a checkout invalidates everything *)
        let stale = Hashtbl.fold (fun n _ acc -> n :: acc) t.table [] in
        Hashtbl.clear t.table;
        t.branch <- branch;
        t.generation <- gen;
        stale)
      else if Int64.compare gen t.generation <> 0 then (
        (* generation bumped: reload affected defs (stale = fetched
           before this gen; keep any fetched at-or-after it) *)
        let ng = gen in
        let stale = ref [] in
        Hashtbl.filter_map_inplace
          (fun n e ->
            if Int64.compare e.source_gen ng < 0 then (
              stale := n :: !stale;
              None)
            else Some e)
          t.table;
        t.generation <- ng;
        !stale)
      else []
  | None -> []

(** Look up a materialized def by name (nil on miss). *)
let lookup (t : t) (name : string) : value option =
  match Hashtbl.find_opt t.table name with
  | Some e -> Some e.value
  | None -> None

(** Record a def as materialized under the current generation. *)
let store (t : t) (name : string) (v : value) : unit =
  Hashtbl.replace t.table name { value = v; source_gen = t.generation }

(** Drop a single def (used when a fetch fails / the name is genuinely
    unbound, so it is not cached as present). *)
let remove (t : t) (name : string) : unit =
  Hashtbl.remove t.table name

let branch (t : t) : string = t.branch
let generation (t : t) : int64 = t.generation

(** Number of defs currently cached (testing / introspection). *)
let size (t : t) : int = Hashtbl.length t.table
