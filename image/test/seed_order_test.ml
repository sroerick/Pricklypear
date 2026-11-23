(** Seed-order resolution: (package … (requires …)) is the dependency edge.

    The boot seed keeps its preferred list as a tiebreak, but a package's
    declared requirements must come first. Missing requirements and cycles
    are excluded loudly rather than half-seeded. *)

let counter = ref 0

let with_root f =
  incr counter;
  let tmp =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "pp_seed_order_%d_%d" (Unix.getpid ()) !counter)
  in
  (if Sys.file_exists tmp then
     ignore (Sys.command ("rm -rf " ^ Filename.quote tmp)));
  Unix.mkdir tmp 0o755;
  Unix.putenv "PP_LIBS_DIR" tmp;
  Unix.putenv "PP_LIBS_PATH" tmp;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command ("rm -rf " ^ Filename.quote tmp)))
    (fun () -> f tmp)

let write_pkg root name body =
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o755;
  let oc = open_out (Filename.concat dir "lib.pp") in
  output_string oc body;
  close_out oc

let contains hay needle =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  m = 0 || go 0

let has_failure (plan : Pricklypear_image_lib.Boot_seed.seed_plan) name frag =
  List.exists (fun (n, msg) -> n = name && contains msg frag) plan.failures

let test_dependency_order () =
  with_root (fun root ->
      write_pkg root "a"
        "(package a (requires b) (export a/x))\n(define a/x 1)\n";
      write_pkg root "b" "(package b (export b/y))\n(define b/y 2)\n";
      let plan =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "a"; "b" ]
      in
      let why =
        String.concat " | " (List.map (fun (n, m) -> n ^ ": " ^ m) plan.failures)
      in
      Alcotest.(check (list string)) ("b before a (" ^ why ^ ")")
        [ "b"; "a" ] plan.order;
      Alcotest.(check int) "no failures" 0 (List.length plan.failures))

let test_candidate_tiebreak () =
  with_root (fun root ->
      write_pkg root "a" "(package a)\n";
      write_pkg root "b" "(package b)\n";
      let plan =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "a"; "b" ]
      in
      Alcotest.(check (list string)) "candidate order preserved"
        [ "a"; "b" ] plan.order;
      let rev =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "b"; "a" ]
      in
      Alcotest.(check (list string)) "candidate order reversed"
        [ "b"; "a" ] rev.order)

let test_versioned_requirement () =
  with_root (fun root ->
      write_pkg root "a" "(package a (requires (b \">=1.0\")))\n";
      write_pkg root "b" "(package b (version \"1.2.0\"))\n";
      let plan =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "a"; "b" ]
      in
      Alcotest.(check (list string)) "versioned edge" [ "b"; "a" ] plan.order;
      Alcotest.(check int) "no failures" 0 (List.length plan.failures))

let test_missing_and_dependent () =
  with_root (fun root ->
      write_pkg root "a" "(package a (requires nope))\n";
      write_pkg root "b" "(package b)\n";
      write_pkg root "c" "(package c (requires a))\n";
      let plan =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "a"; "b"; "c" ]
      in
      Alcotest.(check (list string)) "only b seeds" [ "b" ] plan.order;
      Alcotest.(check bool) "a missing dep" true
        (has_failure plan "a" "requires unavailable package nope");
      Alcotest.(check bool) "c depends on excluded a" true
        (has_failure plan "c" "requires unavailable package a"))

let test_cycle () =
  with_root (fun root ->
      write_pkg root "a" "(package a (requires b))\n";
      write_pkg root "b" "(package b (requires a))\n";
      let plan =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "a"; "b" ]
      in
      Alcotest.(check (list string)) "cycle seeds nothing" [] plan.order;
      Alcotest.(check bool) "a cycle failure" true
        (has_failure plan "a" "dependency cycle");
      Alcotest.(check bool) "b cycle failure" true
        (has_failure plan "b" "dependency cycle"))

let test_no_manifest () =
  with_root (fun root ->
      write_pkg root "a" "(define a/x 1)\n";
      write_pkg root "b" "(define b/y 2)\n";
      let plan =
        Pricklypear_image_lib.Boot_seed.resolve_seed_order [ "a"; "b" ]
      in
      Alcotest.(check (list string)) "no manifest = leaf"
        [ "a"; "b" ] plan.order;
      Alcotest.(check int) "no failures" 0 (List.length plan.failures))

let rec find_up dir rel =
  if Sys.file_exists (Filename.concat dir rel) then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_up parent rel

(* Dune's test sandbox runs with cwd set to a temporary directory; the
   real corpus is not copied there unless it is a declared dependency.
   Walk up from cwd first (pp/ or the wyo root), then from the executable
   path (which is still under pp/_build). *)
let pp_root_for_corpus () =
  let cwd = Sys.getcwd () in
  match find_up cwd "libs/std/lib.pp" with
  | Some d -> d
  | None -> (
      match find_up cwd "pp/libs/std/lib.pp" with
      | Some d -> Filename.concat d "pp"
      | None -> (
          match
            find_up (Filename.dirname Sys.executable_name) "libs/std/lib.pp"
          with
          | Some d -> d
          | None -> failwith "seed_order_test: cannot find libs/std/lib.pp"))

(* The real tree: every discovered package resolves, and pp-borge's real
   manifest parses. Run from pp/ (the CI command) or the wyo root. *)
let test_real_corpus () =
  let pp_root = pp_root_for_corpus () in
  let libs = Filename.concat pp_root "libs" in
  let plugins = Filename.concat (Filename.dirname pp_root) "plugins" in
  let path =
    if Sys.file_exists plugins then libs ^ ":" ^ plugins else libs
  in
  Unix.putenv "PP_LIBS_DIR" libs;
  Unix.putenv "PP_LIBS_PATH" path;
  let candidates = Pricklypear_image_lib.Boot_seed.full_seed_packages () in
  let plan = Pricklypear_image_lib.Boot_seed.resolve_seed_order candidates in
  Alcotest.(check int) "real corpus manifest failures" 0
    (List.length plan.failures);
  Alcotest.(check int) "all discovered packages ordered"
    (List.length candidates) (List.length plan.order);
  if Sys.file_exists (Filename.concat plugins "pp-borge/lib.pp") then
    match Pricklypear_image_lib.Boot_seed.manifest_for "pp-borge" with
    | Ok (m : Pricklypear_lisp.Package_manifest.t) ->
        Alcotest.(check string) "pp-borge manifest" "pp-borge" m.name
    | Error e -> Alcotest.fail e

let () =
  Alcotest.run "nopales-seed-order"
    [ ( "seed-order"
      , [ ("dependency-order", `Quick, test_dependency_order)
        ; ("candidate-tiebreak", `Quick, test_candidate_tiebreak)
        ; ("versioned-requirement", `Quick, test_versioned_requirement)
        ; ("missing-and-dependent", `Quick, test_missing_and_dependent)
        ; ("cycle", `Quick, test_cycle)
        ; ("no-manifest", `Quick, test_no_manifest)
        ; ("real-corpus", `Quick, test_real_corpus) ] ) ]
