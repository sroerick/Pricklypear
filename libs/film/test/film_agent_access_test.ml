(* Film AGENT-ACCESS pins (borge/film-analysis.borg, subsection
   agent-access).

   lib.pp is SEED SOURCE, not an OCaml module (the film_shape_test /
   film_ingest_test / film_chiasm_test / film_cut_test precedent): the
   suite reads the source and pins the pp_eval API an agent calls.

     A1. film/report stays the FULL report tree (center, shape, pairs,
         declared, orphans, tightness) and is exported, so
         `pp_eval (film/report ...)` hands an agent the whole tree.
     A2. scene/beats answers ONE scene's beats keyed on the scene's
         CURATED slug; scene/briefs hands the same brief shape a report
         row carries.
     A3. film/cut-beats walks ONE cut's rows (film/cut-scenes) pairing
         each with scene/beats, so "scene/beats queries work per film and
         cut" is one call.
     A4. film/agent-report folds report + scenes + cut into ONE dict an
         agent diffs across cuts.
     A5. the family is PURE: it creates no rows and writes no branch.

   Run: `dune runtest --root=pp libs/film`. *)

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Whitespace runs collapse to one space, so a needle pins the FORM and not
   lib.pp's indentation (the film_chiasm_test helper). *)
let collapse s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\n' | '\t' | '\r' -> Buffer.add_char b ' '
      | ' ' when Buffer.length b > 0
                 && Buffer.nth b (Buffer.length b - 1) = ' ' -> ()
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let contains ~needle hay =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let checks = ref 0

let check name cond =
  incr checks;
  Alcotest.(check bool) name true cond
let has needle hay = contains ~needle hay

let slice_from hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then failwith (Printf.sprintf "slice start %S not found" needle)
    else if String.sub hay i n = needle then i
    else go (i + 1)
  in
  let i = go 0 in
  String.sub hay i (h - i)

(* The body of one form: from [needle] up to the NEXT (define ..., so a pin
   can say "this definition does not create a row" without tripping on the
   neighbour that does. *)
let region src needle =
  let rest = slice_from src needle in
  let n = String.length "(define " and h = String.length rest in
  let rec go i =
    if i + n > h then h
    else if String.sub rest i n = "(define " then i
    else go (i + 1)
  in
  let stop = go 1 in
  String.sub rest 0 stop

let load () =
  let path =
    if Sys.file_exists "lib.pp" then "lib.pp"
    else if Sys.file_exists "../lib.pp" then "../lib.pp"
    else failwith "film_agent_access_test: lib.pp not found"
  in
  collapse (read path)

let () =
  let src = load () in
  let exports = region src "(export " in
  let report = region src "(define film/report" in
  let beats = region src "(define scene/beats" in
  let briefs = region src "(define scene/briefs" in
  let cut_beats = region src "(define film/cut-beats" in
  let agent = region src "(define film/agent-report" in

  (* ---- A1: the exported FULL report tree ---- *)
  check "export film/report" (has "film/report" exports);
  check "report carries center" (has "\"center\"" report);
  check "report carries shape" (has "\"shape\"" report);
  check "report carries pairs" (has "\"pairs\"" report);
  check "report carries declared" (has "\"declared\"" report);
  check "report carries orphans" (has "\"orphans\"" report);
  check "report carries tightness" (has "\"tightness\"" report);
  check "report is one dict tree" (has "(dict-set* \"{}\"" report);

  (* ---- A2: per-scene beats, keyed on the curated slug ---- *)
  check "export scene/beats" (has "scene/beats" exports);
  check "scene beats read by scene slug"
    (has "(chiasm/beats-of-scene beats (chiasm/scene-slug scene))" beats);
  check "briefs reuse the report brief shape"
    (has "(list/map chiasm/beat-brief beats)" briefs);

  (* ---- A3: per film and cut, one call ---- *)
  check "export film/cut-beats" (has "film/cut-beats" exports);
  check "cut beats walk one cut's rows"
    (has "(film/cut-scenes scenes cut)" cut_beats);
  check "cut beats pair each scene with its beats"
    (has "(scene/briefs (scene/beats s beats))" cut_beats);
  check "cut beats carry the scene brief"
    (has "\"scene\"" cut_beats && has "\"beats\"" cut_beats);

  (* ---- A4: one agent-facing dict an agent diffs ---- *)
  check "export film/agent-report" (has "film/agent-report" exports);
  check "agent report carries the cut label" (has "\"cut\"" agent);
  check "agent report folds film/cut-report"
    (has "(film/cut-report scenes beats cut)" agent);
  check "agent report folds the cut's scenes"
    (has "(film/cut-beats scenes beats cut)" agent);

  (* ---- A5: pure -- nothing created, no branch written ---- *)
  check "agent report creates no rows" (not (has "(create " agent));
  check "cut beats create no rows" (not (has "(create " cut_beats));
  check "scene beats create no rows" (not (has "(create " beats));
  check "family writes no branch"
    (not (has "branch-create" agent) && not (has "branch-create" cut_beats));

  print_endline "film_agent_access_test: agent access pinned";
  Printf.printf "Test Successful in 0.000s. %d tests run.\n" !checks
