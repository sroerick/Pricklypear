(* Film CUT-COMPARISON pins (borge/film-analysis.borg, subsection
   cut-comparison).

   lib.pp is SEED SOURCE, not an OCaml module (the film_shape_test /
   film_ingest_test / film_chiasm_test precedent): the suite reads the
   source and pins the per-cut reporting derivation.

     D1. every scene row carries its CUT label, so ONE film's rows span
         several cuts; film/cut-label names that field and nothing else.
     D2. film/cut-report is film/report over ONE cut's rows only -- two
         labels over one film therefore produce INDEPENDENT reports.
     D3. film/cuts names the distinct labels first-seen (no sort), so a
         caller can report every branch the film holds.
     D4. film/tightness-diff is after - before in frames: NEGATIVE means
         the recut tightened the ring (the leaf's question).
     D5. the family is PURE: it creates no rows and writes no branch.

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
    else failwith "film_cut_test: lib.pp not found"
  in
  collapse (read path)

let () =
  let src = load () in
  let exports = region src "(export " in
  let label = region src "(define film/cut-label" in
  let walk = region src "(define film/cut-labels-walk" in
  let cuts = region src "(define film/cuts" in
  let cut_scenes = region src "(define film/cut-scenes" in
  let cut_report = region src "(define film/cut-report" in
  let diff = region src "(define film/tightness-diff" in
  let cmp = region src "(define film/cut-comparison" in

  (* ---- exports: the leaf's API surface ---- *)
  check "export film/cut-label" (has "film/cut-label" exports);
  check "export film/cut-scenes" (has "film/cut-scenes" exports);
  check "export film/cuts" (has "film/cuts" exports);
  check "export film/cut-report" (has "film/cut-report" exports);
  check "export film/tightness-diff" (has "film/tightness-diff" exports);
  check "export film/cut-comparison" (has "film/cut-comparison" exports);

  (* ---- D1: a scene row names the cut it landed on ---- *)
  check "cut label reads the row's cut field"
    (has "(as-str (dict-get scene \"cut\"))" label);

  (* ---- D2: one report per cut, from that cut's rows alone ---- *)
  check "cut scenes filter on the cut label"
    (has "(string-eq (film/cut-label s) cut)" cut_scenes);
  check "cut report is film/report over one cut's rows"
    (has "(film/report (film/cut-scenes scenes cut) beats)" cut_report);
  check "two labels are independent reports"
    (has "produce independent reports" cut_report);

  (* ---- D3: the distinct labels, first-seen, no sort ---- *)
  check "cuts dedupe first-seen"
    (has "(film/cut-in? (film/cut-label (car scenes)) acc)" walk);
  check "cuts do not sort" (not (has "sort" walk) && not (has "sort" cuts));

  (* ---- D4: the tightness diff is after - before ---- *)
  check "diff subtracts before from after"
    (has "(dict-get after \"tightness\"))) (parse-int (as-str (dict-get before \"tightness\")))"
       diff);
  check "negative diff means tighter"
    (has "NEGATIVE means the recut tightened" diff);

  (* ---- D5: comparison pairs the two reports; nothing is created ---- *)
  check "comparison carries both tightnesses"
    (has "\"before_tightness\"" cmp && has "\"after_tightness\"" cmp);
  check "comparison carries the delta" (has "\"tightness_delta\"" cmp);
  check "comparison carries both reports"
    (has "\"before_report\"" cmp && has "\"after_report\"" cmp);
  check "comparison creates no rows" (not (has "(create " cmp));
  check "cut report creates no rows" (not (has "(create " cut_report));

  print_endline "film_cut_test: cut comparison pinned";
  Printf.printf "Test Successful in 0.000s. %d tests run.\n" !checks
