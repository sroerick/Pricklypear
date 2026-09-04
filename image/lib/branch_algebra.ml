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
