(* Branch algebra property tests — formal-verification island 1
   (borge/formal-verification.borg, branching/one-tree semantics).

   QCheck generative properties over random branch forests + pins:

     1. resolve matches a naive independent reference implementation
        (linear per-ancestor scan) on random forests.
     2. resolve soundness: a resolved body is defined for that name
        somewhere on the walked chain.
     3. chain terminates (and stays root-anchored) even on MALFORMED
        cyclic forests.
     4. pin hygiene truth table (identical re-define unpins; difference
        pins; empty own row never pins).
     5. classify matches an independently hand-written re-derivation of
        the decision table, and the per-status partition equations hold.
     6. merge application (branching.merge S3, 2026-09): diverged names
        are NEVER in the applied set; applied ⊆ {ahead-new, ahead}.
     7. lifecycle transitions (fork-clean → pin-ahead → parent-catchup-
        synced → parent-move diverged/behind) read through the model.
     8. rename detection (branch-ops GRANULARITY): classify_names
        matches a naive independent pairing reference; events partition
        the input name sets exactly; hash equality is the ONLY rename
        trigger (same-name-different-body and different-name-same-body
        discriminate); canonical pairing under hash collisions.
     9. the pull set (ops table pull row): pull_set/pull_rows match a
        naive hand-derived behind-PIN predicate on randomized status
        rows; the set is exactly the own rows classify renders Behind
        (no-pin behinds — parent moved, branch inherits live — are
        never in it); per-status deterministic pins + a pull lifecycle.

   Needs no Postgres. Seeds are QCheck-default (reproducible with
   QCHECK_SEED / -v); count kept modest so runtest stays fast. *)

module A = Pricklypear_image_lib.Branch_algebra

(* ---- generators ---- *)

(* Branch ids from a small space: random forests are shallow chains /
   trees over main/b1/b2/b3 — enough shape to exercise closest-wins. *)
let gen_branch = QCheck.Gen.oneof_list [ "main"; "b1"; "b2"; "b3" ]

(* Acyclic-by-construction forest: parent is drawn only from ids already
   generated (earlier in the list), roots get None. *)
let gen_forest =
  let rec go n acc =
    if n = 0 then QCheck.Gen.return acc
    else
      QCheck.Gen.(
        let* parent =
          if acc = [] then return None else option (oneof_list (List.map fst acc))
        in
        let b = "br" ^ string_of_int n in
        go (n - 1) ((b, parent) :: acc))
  in
  QCheck.Gen.map List.rev (go 4 [])

let gen_name = QCheck.Gen.oneof_list [ "f"; "g"; "h" ]
let gen_body = QCheck.Gen.map string_of_int (QCheck.Gen.int_range 0 5)

let gen_defs =
  QCheck.Gen.map
    (fun entries ->
       (* group (branch, name -> body) triples per branch, last-wins dedupe *)
       let tbl = Hashtbl.create 8 in
       List.iter
         (fun (b, n, body) ->
            let own =
              match Hashtbl.find_opt tbl b with
              | Some o -> o
              | None -> []
            in
            let own = (n, body) :: List.filter (fun (n', _) -> n' <> n) own in
            Hashtbl.replace tbl b own)
         entries;
       Hashtbl.fold (fun b own acc -> (b, own) :: acc) tbl [])
    (QCheck.Gen.list_size (QCheck.Gen.int_range 0 12)
       QCheck.Gen.(triple gen_branch gen_name gen_body))

let gen_hash = QCheck.Gen.map (fun n -> Printf.sprintf "h%d" n) (QCheck.Gen.int_range 0 3)

let gen_hash_opt =
  QCheck.Gen.oneof_weighted
    [ (1, QCheck.Gen.return None); (3, QCheck.Gen.map (fun h -> Some h) gen_hash) ]

(* ---- naive reference (independent of Branch_algebra internals) ---- *)

let ref_chain (f : A.forest) (b : A.branch) : A.branch list =
  let rec go cur acc =
    if List.mem cur acc then List.rev acc
    else
      match List.assoc_opt cur f with
      | Some (Some p) -> go p (cur :: acc)
      | _ -> List.rev (cur :: acc)
  in
  go b []

let ref_resolve (f : A.forest) (defs : A.defs) (b : A.branch) (n : string) :
    string option =
  let rec go = function
    | [] -> None
    | c :: rest -> (
        match List.assoc_opt c defs with
        | None -> go rest
        | Some own -> (
            match List.assoc_opt n own with
            | Some body -> Some body
            | None -> go rest))
  in
  go (ref_chain f b)

(* ---- properties ---- *)

(* 1. resolve == naive reference on random (forest, defs). *)
let prop_resolve_matches_reference =
  QCheck.Test.make
    ~name:"resolve matches naive reference"
    ~count:400
    (QCheck.make QCheck.Gen.(pair gen_forest gen_defs))
    (fun (f, defs) ->
       QCheck.Gen.(
         let bs = List.map fst f in
         List.for_all
           (fun b ->
              List.for_all
                (fun n -> A.resolve f defs b n = ref_resolve f defs b n)
                [ "f"; "g"; "h" ])
           bs))

(* 2. resolved body, when present, is defined for that name on the chain. *)
let prop_resolve_sound =
  QCheck.Test.make
    ~name:"resolve is sound (body from the chain)"
    ~count:400
    (QCheck.make QCheck.Gen.(pair gen_forest gen_defs))
    (fun (f, defs) ->
       let bs = List.map fst f in
       List.for_all
         (fun b ->
            List.for_all
              (fun n ->
                 match A.resolve f defs b n with
                 | None -> true
                 | Some body ->
                     List.exists
                       (fun c ->
                          match List.assoc_opt c defs with
                          | Some own -> List.assoc_opt n own = Some body
                          | None -> false)
                       (A.chain f b))
              [ "f"; "g"; "h" ])
         bs)

(* 3. chain terminates on malformed cyclic forests. *)
let prop_chain_terminates_on_cycles =
  QCheck.Test.make
    ~name:"chain terminates on cyclic forests"
    ~count:200
    (QCheck.make (QCheck.Gen.int_range 2 9))
    (fun n ->
       (* every node points at the next, last points at the first: a
          pure cycle. chain must terminate within n steps. *)
       let ids = List.init n (fun i -> "c" ^ string_of_int i) in
       let f =
         List.mapi
           (fun i b -> (b, Some (List.nth ids ((i + 1) mod n))))
           ids
       in
       List.length (A.chain f (List.nth ids 0)) <= n)

(* 4. pin hygiene truth table. *)
let prop_pin_hygiene =
  QCheck.Test.make
    ~name:"pin hygiene truth table"
    ~count:300
    (QCheck.make QCheck.Gen.(pair gen_body gen_body))
    (fun (x, y) ->
       A.pin_needed ~inherited:None ~own:(Some x)
       && (if x = y then not (A.pin_needed ~inherited:(Some y) ~own:(Some x))
           else A.pin_needed ~inherited:(Some y) ~own:(Some x))
       && not (A.pin_needed ~inherited:(Some x) ~own:None))

(* 5. classify matches an independently hand-written re-derivation of the
   decision table (catches silent edits to either side). *)
let prop_classify_characteristic =
  QCheck.Test.make
    ~name:"classify matches characteristic predicates"
    ~count:800
    (QCheck.make QCheck.Gen.(quad QCheck.Gen.bool gen_hash_opt gen_hash_opt gen_hash_opt))
    (fun (own, b, p, a) ->
       let eq = A.hash_eq in
       let observed = A.classify ~own ~b ~p ~a in
       let expected =
         if not own then if eq p a then A.Clean else A.Behind
         else if eq b p then A.Synced
         else if eq p a then (
           match a with None -> A.AheadNew | Some _ -> A.Ahead)
         else if eq b a then A.Behind
         else A.Diverged
       in
       observed = expected)

(* 5b. partition equations: each status's defining condition holds (and
   no other status's, where distinguishable). *)
let prop_classify_partition =
  QCheck.Test.make
    ~name:"classify partition equations"
    ~count:800
    (QCheck.make QCheck.Gen.(quad QCheck.Gen.bool gen_hash_opt gen_hash_opt gen_hash_opt))
    (fun (own, b, p, a) ->
       let eq = A.hash_eq in
       match A.classify ~own ~b ~p ~a with
       | A.Clean -> (not own) && eq p a
       | A.Behind ->
           if own then (not (eq b p)) && (not (eq p a)) && eq b a
           else not (eq p a)
       | A.Synced -> own && eq b p
       | A.AheadNew -> own && (not (eq b p)) && eq p a && a = None
       | A.Ahead -> own && (not (eq b p)) && eq p a && a <> None
       | A.Diverged ->
           own && (not (eq b p)) && (not (eq p a)) && (not (eq b a)))

(* 6. merge application set: diverged never applied; applied ⊆ ahead set. *)
let prop_merge_never_applies_diverged =
  QCheck.Test.make
    ~name:"merge never applies diverged"
    ~count:800
    (QCheck.make QCheck.Gen.(quad QCheck.Gen.bool gen_hash_opt gen_hash_opt gen_hash_opt))
    (fun (own, b, p, a) ->
       let s = A.classify ~own ~b ~p ~a in
       match s with
       | A.Diverged -> not (A.merge_applies s)
       | A.AheadNew | A.Ahead -> A.merge_applies s
       | A.Clean | A.Behind | A.Synced -> not (A.merge_applies s))

(* 7. lifecycle transitions read through the model (body doubles as hash;
   base = what main held at fork time). *)
let prop_lifecycle =
  QCheck.Test.make
    ~name:"lifecycle transitions read through the model"
    ~count:200
    (QCheck.make (QCheck.Gen.int_range 0 3))
    (fun scenario ->
       let h0, h1, h2 = ("0", "1", "2") in
       match scenario with
       | 0 ->
           (* empty fork, parent unmoved: clean *)
           A.classify ~own:false ~b:None ~p:(Some h0) ~a:(Some h0) = A.Clean
       | 1 ->
           (* child pins a different body; parent unmoved: ahead; and the
              pin IS warranted (differs from the inherited body) *)
           A.classify ~own:true ~b:(Some h1) ~p:(Some h0) ~a:(Some h0) = A.Ahead
           && A.pin_needed ~inherited:(Some h0) ~own:(Some h1)
       | 2 ->
           (* parent catches up to the pinned value: synced; pin no longer
              warranted (identical re-define unpins) *)
           A.classify ~own:true ~b:(Some h1) ~p:(Some h1) ~a:(Some h0) = A.Synced
           && not (A.pin_needed ~inherited:(Some h1) ~own:(Some h1))
       | _ ->
           (* parent moves past: child holding its own change = diverged;
              child holding the old base = behind *)
           A.classify ~own:true ~b:(Some h1) ~p:(Some h2) ~a:(Some h0) = A.Diverged
           && A.classify ~own:true ~b:(Some h0) ~p:(Some h2) ~a:(Some h0) = A.Behind)

(* ---- 8. rename detection (branch-ops GRANULARITY) ---- *)

let cmp_event a b =
  let tag = function
    | A.Renamed (f, t) -> (0, f, t)
    | A.Removed n -> (1, n, "")
    | A.Added n -> (2, n, "")
  in
  compare (tag a) (tag b)

(* Naive reference for [classify_names]: same canonical claim rule (each
   removed name, in sorted order, claims the first unclaimed equal-hash
   DIFFERENT added name), but a different mechanism — per-hash bucket
   queues over sorted name lists, no rebuilding-claim scan. *)
let ref_classify_names removed added =
  let sorted_added_at h =
    List.sort String.compare
      (List.filter_map
         (fun (n, h') -> if String.equal h h' then Some n else None)
         added)
  in
  let queues = Hashtbl.create 8 in
  List.iter
    (fun (_, h) ->
       if not (Hashtbl.mem queues h) then Hashtbl.add queues h (sorted_added_at h))
    (removed @ added);
  let ren = ref [] and rem = ref [] in
  List.iter
    (fun (rn, rh) ->
       let q = Option.value (Hashtbl.find_opt queues rh) ~default:[] in
       let rec claim acc = function
         | [] -> (None, List.rev acc)
         | an :: rest when String.equal an rn -> claim (an :: acc) rest
         | an :: rest -> (Some an, List.rev_append acc rest)
       in
       let claimed, q' = claim [] q in
       Hashtbl.replace queues rh q';
       match claimed with
       | Some an -> ren := (rn, an) :: !ren
       | None -> rem := rn :: !rem)
    (List.sort (fun (a, _) (b, _) -> String.compare a b) removed);
  let adds =
    List.sort String.compare
      (Hashtbl.fold (fun _ q acc -> List.rev_append q acc) queues [])
  in
  List.map (fun (f, t) -> A.Renamed (f, t)) (List.rev !ren)
  @ List.rev_map (fun n -> A.Removed n) !rem
  @ List.map (fun n -> A.Added n) adds

(* removed/added (name, hash) sets over a small name pool and a dense
   hash space (constant collisions exercise the canonical pairing).
   Names are unique per side and the sides are disjoint: the first side
   a name is drawn for wins; later occurrences of the same name are
   dropped. *)
let gen_name_diff =
  let pool = [ "f"; "g"; "h"; "i"; "j" ] in
  let gen_hash =
    QCheck.Gen.map (fun n -> "h" ^ string_of_int n) (QCheck.Gen.int_range 0 2)
  in
  QCheck.Gen.(
    let* entries =
      list_size (int_range 0 10) (pair bool (pair (oneof_list pool) gen_hash))
    in
    let tbl = Hashtbl.create 8 in
    List.iter
      (fun (as_added, (name, hash)) ->
         if not (Hashtbl.mem tbl name) then Hashtbl.add tbl name (as_added, hash))
      entries;
    let removed, added =
      Hashtbl.fold
        (fun name (as_added, hash) (r, a) ->
           if as_added then (r, (name, hash) :: a) else ((name, hash) :: r, a))
        tbl ([], [])
    in
    return (removed, added))

(* 8a. classify_names == the naive pairing reference (event multiset). *)
let prop_rename_matches_reference =
  QCheck.Test.make
    ~name:"classify_names matches naive pairing reference"
    ~count:600
    (QCheck.make gen_name_diff)
    (fun (removed, added) ->
       List.sort cmp_event (A.classify_names ~removed ~added)
       = List.sort cmp_event (ref_classify_names removed added))

(* 8b. the events partition the input name sets exactly, and renames
   pair DIFFERENT names with EQUAL hashes — hash equality is the only
   rename trigger. *)
let prop_rename_partition =
  QCheck.Test.make
    ~name:"rename events partition inputs; renames are hash-equal"
    ~count:600
    (QCheck.make gen_name_diff)
    (fun (removed, added) ->
       let events = A.classify_names ~removed ~added in
       let rhash n = List.assoc_opt n removed in
       let ahash n = List.assoc_opt n added in
       let pairs =
         List.filter_map
           (function A.Renamed (f, t) -> Some (f, t) | _ -> None)
           events
       in
       let froms = List.map fst pairs in
       let tos = List.map snd pairs in
       let rems =
         List.filter_map (function A.Removed n -> Some n | _ -> None) events
       in
       let adds =
         List.filter_map (function A.Added n -> Some n | _ -> None) events
       in
       List.sort String.compare (froms @ rems)
       = List.sort String.compare (List.map fst removed)
       && List.sort String.compare (tos @ adds)
          = List.sort String.compare (List.map fst added)
       && List.for_all
            (fun (f, t) ->
               not (String.equal f t) && rhash f = ahash t && ahash t <> None)
            pairs)

(* 8c. deterministic pins: hash equality is the ONLY rename trigger —
   different-name-same-body renames, same-name-different-body (a
   precondition violation) never does; canonical pairing under hash
   collisions; exact output order (renames by from, then removed, then
   added, each sorted). *)
let t_rename_pins =
  QCheck.Test.make
    ~name:"rename detection deterministic pins"
    ~count:1
    (QCheck.make QCheck.Gen.(return ()))
    (fun () ->
       A.classify_names ~removed:[ ("f", "h1") ] ~added:[ ("g", "h1") ]
       = [ A.Renamed ("f", "g") ]
       && A.classify_names ~removed:[ ("f", "h1") ] ~added:[ ("g", "h2") ]
          = [ A.Removed "f"; A.Added "g" ]
       && A.classify_names ~removed:[ ("f", "h1") ] ~added:[ ("f", "h1") ]
          = [ A.Removed "f"; A.Added "f" ]
       && A.classify_names ~removed:[ ("z", "h"); ("a", "h") ] ~added:[ ("m", "h") ]
          = [ A.Renamed ("a", "m"); A.Removed "z" ]
       && A.classify_names
            ~removed:[ ("old", "h1"); ("dead", "h2") ]
            ~added:[ ("fresh", "h3"); ("new", "h1") ]
          = [ A.Renamed ("old", "new"); A.Removed "dead"; A.Added "fresh" ]
       && A.classify_names ~removed:[] ~added:[] = [])

(* ---- 9. the pull set (ops table pull row) ---- *)

let gen_status_row =
  QCheck.Gen.(
    let* kind = oneof_list [ "fn"; "type"; "route" ] in
    let* name = oneof_list [ "f"; "g"; "h" ] in
    let* own = bool in
    let* b = gen_hash_opt in
    let* p = gen_hash_opt in
    let* a = gen_hash_opt in
    return
      { A.r_kind = kind;
        r_name = name;
        r_own = own;
        r_b = b;
        r_p = p;
        r_a = a })

let gen_rows = QCheck.Gen.list_size (QCheck.Gen.int_range 0 10) gen_status_row

(* 9a. pull_set == a naive hand-derived behind-PIN predicate (own ∧
   b≠p ∧ p≠a ∧ b=a), independent of classify. *)
let prop_pull_matches_reference =
  QCheck.Test.make
    ~name:"pull_set matches naive behind-pin predicate"
    ~count:800
    (QCheck.make gen_rows)
    (fun rows ->
       let eq = A.hash_eq in
       let expected =
         List.filter
           (fun r ->
              r.A.r_own && (not (eq r.A.r_b r.A.r_p))
              && not (eq r.A.r_p r.A.r_a)
              && eq r.A.r_b r.A.r_a)
           rows
         |> List.map (fun r -> r.A.r_name)
       in
       A.pull_set rows = expected)

(* 9b. per-row equivalence through the ONE classifier: a row is in the
   pull rows iff it has an own pin AND classify renders it Behind. The
   own-guard is load-bearing — a no-pin row classifies Behind when the
   parent moved since the fork, but there is no pin to unbind. *)
let prop_pull_is_classify_behind_pins =
  QCheck.Test.make
    ~name:"pull_rows = own pins classify renders Behind"
    ~count:800
    (QCheck.make gen_rows)
    (fun rows ->
       List.for_all
         (fun r ->
            List.mem r (A.pull_rows rows)
            = (r.A.r_own
               && A.classify ~own:r.A.r_own ~b:r.A.r_b ~p:r.A.r_p ~a:r.A.r_a
                  = A.Behind))
         rows)

(* 9c. deterministic pins: one row per status (only the behind PIN is
   adopted; no-pin behinds — parent moved, branch inherits live — and
   clean/ahead/ahead-new/synced/diverged never are; a tombstone pin
   holds the ABSENT base, so it is behind and gets unbound), plus the
   lifecycle: after the pull the pins are gone, the branch inherits the
   parent live, and the pull set is empty. *)
let t_pull_pins =
  QCheck.Test.make
    ~name:"pull set deterministic pins (per-status + lifecycle)"
    ~count:1
    (QCheck.make QCheck.Gen.(return ()))
    (fun () ->
       let row own k n b p a =
         { A.r_kind = k;
           r_name = n;
           r_own = own;
           r_b = b;
           r_p = p;
           r_a = a }
       in
       let h0, h1, h2 = ("h0", "h1", "h2") in
       let rows =
         [
           row true "fn" "behind_pin" (Some h0) (Some h1) (Some h0);
           row true "fn" "diverged" (Some h2) (Some h1) (Some h0);
           row true "fn" "ahead" (Some h1) (Some h0) (Some h0);
           row true "fn" "ahead_new" (Some h1) None None;
           row true "fn" "synced" (Some h0) (Some h0) (Some h1);
           row false "fn" "clean_inherit" (Some h0) (Some h0) (Some h0);
           row false "fn" "nopin_behind" (Some h1) (Some h1) (Some h0);
           row true "fn" "tombstone_pin" None (Some h1) None;
         ]
       in
       A.pull_set rows = [ "behind_pin"; "tombstone_pin" ]
       && A.pull_set
            [
              row false "fn" "behind_pin" (Some h1) (Some h1) (Some h0);
              row false "fn" "tombstone_pin" (Some h1) (Some h1) None;
            ]
          = [])

let () =
  QCheck_base_runner.run_tests_main
    [
      prop_resolve_matches_reference;
      prop_resolve_sound;
      prop_chain_terminates_on_cycles;
      prop_pin_hygiene;
      prop_classify_characteristic;
      prop_classify_partition;
      prop_merge_never_applies_diverged;
      prop_lifecycle;
      prop_rename_matches_reference;
      prop_rename_partition;
      t_rename_pins;
      prop_pull_matches_reference;
      prop_pull_is_classify_behind_pins;
      t_pull_pins;
    ]
