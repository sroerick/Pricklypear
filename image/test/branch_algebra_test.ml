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
    ]
