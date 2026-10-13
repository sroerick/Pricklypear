(* Branch algebra — the pure, executable spec of Pricklypear branching.

   Per borge/branching.borg (branch-status decision table, override walk,
   pin-hygiene) and borge/formal-verification.borg (first verification
   island, 2026-09): the SEMANTICS of branches as pure set/map logic, no
   I/O, no SQL — so they can be property-tested against naive reference
   implementations (image/test/branch_algebra_test.ml) and reasoned about
   independent of the store machinery.

   What lives here:
     - forest/defs: the data model (parent pointers + per-branch pins)
     - resolve: OVERRIDE resolution (closest own definition wins along
       the parent chain) — storage.no-inheritance
     - pin_needed: pin hygiene — a pin exists iff the own body DIFFERS
       from the nearest inherited visible body (identical re-define
       unpins); write paths suppress contentless pins
     - classify: the git-style status decision table (clean / ahead-new /
       ahead / behind / diverged / synced) as one total function over
       (own, b, p, a) hashes with ABSENT as its own value. store.ml's
       classify_status DELEGATES here — one decision table, two callers.
     - classify_names: rename detection over removed+added (name, hash)
       sets (branching-ast-ergonomics branch-ops GRANULARITY) — a
       hash-equal body under a new name is RENAMED(from,to), reported
       separately from genuine adds/removes.
     - status_row / pull_rows / pull_set: the ops-table pull row —
       exactly the OWN pins classify renders Behind (what branch/pull
       adopt-parents via the unbind path); one computation, callers
       never re-derive.

   What deliberately does NOT live here: SQL triggers, the GUC-guarded
   mirror, generations bookkeeping — the unverifiable residue (see the
   formal-verification chapter's scope note). *)

type branch = string

(* The branch forest: (branch, parent option). Roots have None. *)
type forest = (branch * branch option) list

(* Own definitions per branch: branch -> (name -> body). *)
type defs = (branch * (string * string) list) list

let parent_of (f : forest) (b : branch) : branch option =
  match List.assoc_opt b f with Some p -> p | None -> None

(* [chain f b] = [b; parent; grandparent; ...] up to a root. Cycle-safe:
   a malformed forest (re-parenting loop) terminates and returns the
   walked prefix rather than diverging. *)
let chain (f : forest) (b : branch) : branch list =
  let rec go cur seen acc =
    if List.mem cur seen then List.rev acc
    else
      match parent_of f cur with
      | None -> List.rev (cur :: acc)
      | Some p -> go p (cur :: seen) (cur :: acc)
  in
  go b [] []

(* OVERRIDE resolution (storage.no-inheritance): the nearest own
   definition of [name] along the chain wins; absent everywhere = None.
   Reference shape: linear scan of the chain, first hit. *)
let resolve (f : forest) (defs : defs) (b : branch) (name : string) :
    string option =
  let rec go = function
    | [] -> None
    | c :: rest -> (
        match List.assoc_opt c defs with
        | Some own -> (
            match List.assoc_opt name own with
            | Some body -> Some body
            | None -> go rest)
        | None -> go rest)
  in
  go (chain f b)

(* PIN HYGIENE (branching.pin-hygiene): a pin on [b] for [name] is
   warranted iff an own body exists AND differs from the nearest
   inherited visible body. Own = inherited (or no own row) => contentless
   => never write it; an identical re-define unpins. *)
let pin_needed ~(inherited : string option) ~(own : string option) : bool =
  match (own, inherited) with
  | None, _ -> false
  | Some o, Some i -> not (String.equal o i)
  | Some _, None -> true

(* Status vocabulary. Mirrors Store.st_status 1:1; store.ml maps. *)
type status = Clean | AheadNew | Ahead | Behind | Diverged | Synced

let status_to_string = function
  | Clean -> "clean"
  | AheadNew -> "ahead-new"
  | Ahead -> "ahead"
  | Behind -> "behind"
  | Diverged -> "diverged"
  | Synced -> "synced"

(* Hash equality with ABSENT (None) as its own value — the b/p/a
   convention from branching.branch-status. *)
let hash_eq (x : string option) (y : string option) : bool =
  match (x, y) with
  | None, None -> true
  | Some a, Some b -> String.equal a b
  | _ -> false

(* The spec's decision table, as one total function:
     - no own pin:  Clean if the parent matches the base, else Behind
       (the parent moved since the fork — the child inherits it as-is)
     - own pin, b = p:                Synced (cherry-picked identical)
     - own pin, p = a, a absent:      AheadNew (own new name)
     - own pin, p = a, a present:     Ahead (own change, parent unmoved)
     - own pin, p != a, b = a:        Behind (pin holds the OLD base; the
                                      parent moved — pin is stale)
     - own pin, p != a, b != a:       Diverged (both sides moved) *)
let classify ~(own : bool) ~(b : string option) ~(p : string option)
    ~(a : string option) : status =
  if not own then (if hash_eq p a then Clean else Behind)
  else if hash_eq b p then Synced
  else if hash_eq p a then (
    match a with None -> AheadNew | Some _ -> Ahead)
  else if hash_eq b a then Behind
  else Diverged

(* MERGE APPLICATION SET (branching.merge S3, 2026-09 decision): from a
   name's status, is it auto-applied into the parent? Only ahead-new and
   ahead. Diverged names are NEVER auto-applied — they come back as
   conflicts and resolve by explicit re-define + re-merge (obituary
   rule: no silent reversals). Behind/synced/clean are no-ops. *)
let merge_applies (s : status) : bool =
  match s with AheadNew | Ahead -> true | _ -> false

(* ------------------------------------------------------------------ *)
(* RENAME DETECTION (branching-ast-ergonomics, branch-ops GRANULARITY):
   the AST makes RENAME/MOVE detectable — a hash-equal body under a new
   name is a rename, not delete+add.

   Pure and policy-free: the caller hands us the removed+added (name,
   hash) sets between whichever two snapshots it is diffing (e.g. the
   branch's own pin set before/after, or branch-own vs parent-own);
   the algebra only pairs by hash equality and reports the three event
   kinds separately.

   CANONICAL PAIRING (deterministic): removed names are processed in
   lexicographic order; each claims the FIRST unclaimed added name with
   an equal hash and a DIFFERENT name. Within one hash class the
   (from,to) pairing is canonical but arbitrary — the stable result is
   the event multiset (which names are renamed/removed/added).
   Output order: renames (by from-name), then removed (sorted), then
   added (sorted). A name present in BOTH sets violates the caller's
   disjointness precondition; it is never renamed — it degrades to
   Removed + Added. *)

type name_event =
  | Renamed of string * string  (* from = removed side, to = added side *)
  | Added of string
  | Removed of string

let classify_names ~(removed : (string * string) list)
    ~(added : (string * string) list) : name_event list =
  let by_name (x : string * string) (y : string * string) =
    String.compare (fst x) (fst y)
  in
  let rsorted = List.sort by_name removed in
  let ren = ref [] and rem = ref [] in
  let remaining = ref (List.sort by_name added) in
  List.iter
    (fun (rn, rh) ->
       (* first unclaimed added name with equal hash, different name;
         claimed entries leave the pool, blocked (equal-name) heads
         stay for later removed names. *)
       let rec claim = function
         | [] -> None
         | (an, ah) :: rest when String.equal rh ah && not (String.equal rn an)
             -> Some (an, rest)
         | cand :: rest -> (
             match claim rest with
             | Some (hit, rest') -> Some (hit, cand :: rest')
             | None -> None)
       in
       match claim !remaining with
       | Some (an, rest') ->
           remaining := rest';
           ren := (rn, an) :: !ren
       | None -> rem := rn :: !rem)
    rsorted;
  List.map (fun (f, t) -> Renamed (f, t)) (List.rev !ren)
  @ List.rev_map (fun n -> Removed n) !rem
  @ List.map (fun (an, _) -> Added an) !remaining

(* ------------------------------------------------------------------ *)
(* THE PULL SET (branching-ast-ergonomics ops table, pull row):
   "every BEHIND pin adopts the parent's version (bulk unoverride)".

   One name as the store's branch_status read renders it (what
   Store.branch_status_row projects onto the algebra): RESOLVED hashes
   (b = the branch's own pin if any, else the nearest ancestor; p = the
   parent-resolved hash; a = the at-fork base hash) plus the own-pin
   flag. r_kind rides along so the unbind consumer (branch/pull) can
   route fn/type/route without re-deriving anything; the algebra is
   kind-blind.

   [pull_rows] = exactly the OWN pins the ONE classify table renders
   Behind (own pin holds the old base while the parent moved — adopting
   = unbinding the pin, never a body copy: pin hygiene). Rows WITHOUT
   an own pin classify Behind too when the parent moved since the fork,
   but there is no pin to unbind — the branch already inherits the
   parent's body live — so they are never adopted;
   diverged/ahead/ahead-new/synced/clean are never adopted. This is THE
   one pull-set computation: branch/pull must call it, never re-derive. *)

type status_row = {
  r_kind : string; (* "fn" | "type" | "route" — opaque to the algebra *)
  r_name : string;
  r_own : bool; (* the branch has its own live row (a pin) *)
  r_b : string option; (* branch-resolved live hash *)
  r_p : string option; (* parent-resolved live hash *)
  r_a : string option; (* base (at-fork) resolved hash *)
}

let pull_rows (rows : status_row list) : status_row list =
  List.filter
    (fun r ->
       r.r_own && classify ~own:r.r_own ~b:r.r_b ~p:r.r_p ~a:r.r_a = Behind)
    rows

(* [pull_set rows] = the names of [pull_rows rows], in row order (a name
   pinning in two kinds appears once per kind; consumers wanting names
   dedupe, the branch/pull prim consumes [pull_rows] instead). *)
let pull_set (rows : status_row list) : string list =
  List.map (fun r -> r.r_name) (pull_rows rows)
