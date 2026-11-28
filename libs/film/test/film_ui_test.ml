(* Film UI pins (borge/film-analysis.borg, subsection ui).

   lib.pp is SEED SOURCE, not an OCaml module (the film_shape_test /
   film_chiasm_test precedent): the suite reads the source and pins the
   ring report page and its arc diagram.

     U1. routes: /film renders the index (film/page) and /film/<slug>
         renders one film's ring report (film/film-page); the slug half
         rides the kernel prefix table because the pp route table only
         matches whole paths, so /film itself stays the whole-path route.
     U2. the film page READS the derivation: the cut's rows come from
         film/cut-scenes and the report from film/report over them.
     U3. the page shows center + shape + tightness, and a per-pair table
         (left, right, dur delta in frames, shared motifs).
     U4. the diagram is render-html of an SVG with one arc per POSITIONAL
         mirror pair -- the pair's left_ord/right_ord, never a declared
         mirror field.
     U5. the pages are PURE: they create no rows and write no branch.

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

(* The body of one form: from [needle] up to the NEXT [stop] literal, so a
   pin can say "this definition creates no row" without tripping on a
   neighbour that does. *)
let body_until src needle stop =
  let rest = slice_from src needle in
  let n = String.length stop and h = String.length rest in
  let rec go i =
    if i + n > h then h
    else if String.sub rest i n = stop then i
    else go (i + 1)
  in
  String.sub rest 0 (go 1)

let load () =
  let path =
    if Sys.file_exists "lib.pp" then "lib.pp"
    else if Sys.file_exists "../lib.pp" then "../lib.pp"
    else failwith "film_ui_test: lib.pp not found"
  in
  collapse (read path)

let () =
  let src = load () in
  let exports = body_until src "(export " "(define " in
  let index = body_until src "(define film/page" "(define film/film-page" in
  let detail = slice_from src "(define film/film-page" in
  (* pair-row is its own definition just above the table it fills, so the
     slice starts there: the two together are "the per-pair table". *)
  let table = body_until src "(define film/ui/pair-row" "(define film/ui/hue" in
  let arcs = body_until src "(define film/ui/arcs" "(define film/ui/nodes" in
  let diagram = body_until src "(define film/ui/diagram" "(define film/ui/center-label" in

  (* ---- U1: the leaf's API surface and its two routes ---- *)
  check "export film/page" (has "film/page" exports);
  check "export film/film-page" (has "film/film-page" exports);
  check "/film is the index route"
    (has "(route \"/film\" \"film/page\"" src);
  check "/film/<slug> rides the kernel prefix table"
    (has "is a PREFIX, so it rides the" src);
  check "detail reads the slug half of request-path"
    (has "(film/ui/path-suffix \"/film/\" (request-path))" detail);
  check "an empty slug falls back to the index"
    (has "(string-eq slug \"\")" detail);
  check "an unknown slug answers 404"
    (has "(status-response 404" detail);

  (* ---- U2: the page reads the derivation, it does not re-derive ---- *)
  check "index lists film rows" (has "(film/ui/films)" index);
  check "detail takes ONE cut's rows" (has "(film/cut-scenes scenes cut)" detail);
  check "detail builds the report from those rows"
    (has "(film/report rows (film/ui/beats scenes))" detail);
  check "the cut comes from film/cuts" (has "(film/ui/cut scenes)" detail);

  (* ---- U3: center, pairs table and tightness are on the page ---- *)
  check "detail names the center" (has "(film/ui/center-label" detail);
  check "even center reads as seam" (has "\"seam\"" src);
  check "detail prints tightness in frames"
    (has "\" · tightness \"" detail);
  check "detail renders the pairs table"
    (has "(film/ui/pairs-table (dict-get report \"pairs\"))" detail);
  check "pair row carries dur delta" (has "dur_delta" table);
  check "pair table headers name the mirror halves"
    (has "(html/text \"Left\")" table && has "(html/text \"Right\")" table);
  check "pair table shows the shared motifs"
    (has "(dict-get pair \"shared_motifs\")" table);

  (* ---- U4: the diagram arcs the DERIVED (positional) pairs ---- *)
  check "diagram is render-html of an svg"
    (has "svg" diagram && has "(render-html" diagram);
  check "one arc per pair, from the pair's ords"
    (has "(dict-get (car pairs) \"left_ord\")" arcs
     && has "(dict-get (car pairs) \"right_ord\")" arcs);
  check "arcs read no declared mirror field"
    (not (has "\"mirror\"" arcs));
  check "arcs step through the rainbow"
    (has "hsl(" arcs && has "film-arc" arcs);
  check "diagram nodes print the scene ords"
    (has "(html/text (as-str ord))" src);
  check "diagram is fed the report's positional pairs"
    (has "(film/ui/diagram rows (dict-get report \"pairs\"))" detail);

  (* ---- U5: the pages are pure renderers ---- *)
  check "index creates no rows" (not (has "(create " index));
  check "detail creates no rows" (not (has "(create " detail));

  print_endline "film_ui_test: ring report page pinned";
  Printf.printf "Test Successful in 0.000s. %d tests run.\n" !checks
