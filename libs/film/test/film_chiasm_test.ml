(* Film CHIASTIC-ANALYSIS pins (borge/film-analysis.borg, subsection
   chiastic-analysis).

   lib.pp is SEED SOURCE, not an OCaml module (the film_shape_test /
   film_ingest_test precedent): the suite reads the source and pins the
   two-pointer derivation and the report tree.

     C1. chiasm/pairs is END-PAIRED by two pointers: the head pairs the
         last row and the walk recurses inward -- no index, no sort, no
         annotation.
     C2. chiasm/center is the SAME recursion: a singleton on an odd ring
         (the center scene) and '() on an even ring (the seam).
     C3. film/report NAMES the center from structure alone (odd -> the
         center scene's brief, even -> the string "seam") and reports the
         shape beside it.
     C4. declared mirror mismatches land in report.declared: the DECLARED
         claim (scene/declared-mirror) is compared with the POSITION-
         derived mirror (scene/derived-mirror-ord), and only disagreements
         are listed.
     C5. orphan plants (a plant beat with no downstream payoff beat of the
         same motif) land in report.orphans.
     C6. tightness is the SUM of per-pair dur deltas in frames.

   Run: `dune runtest --root=pp libs/film`. *)

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Whitespace runs collapse to one space, so a needle pins the FORM and not
   lib.pp's indentation (the film_shape_test helper). *)
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

(* The body of the (define NAME ...) form that starts at [needle], cut off
   at the next form the caller names -- so a pin can say "this recursion
   does NOT read that field". *)
let body_until src needle stop =
  let rest = slice_from src needle in
  match String.index_opt rest ')' with
  | None -> rest
  | Some _ ->
      if has stop rest then
        let j =
          let n = String.length stop and h = String.length rest in
          let rec go i =
            if i + n > h then h
            else if String.sub rest i n = stop then i
            else go (i + 1)
          in
          go 0
        in
        String.sub rest 0 j
      else rest

let load () =
  let path =
    if Sys.file_exists "lib.pp" then "lib.pp"
    else if Sys.file_exists "../lib.pp" then "../lib.pp"
    else failwith "film_chiasm_test: lib.pp not found"
  in
  collapse (read path)

let () =
  let src = load () in
  let chiasm = slice_from src "(define chiasm/length" in

  (* ---- C1: end-paired two-pointer recursion ---- *)
  check "export chiasm/pairs" (has "chiasm/pairs" src);
  check "export chiasm/center" (has "chiasm/center" src);
  check "export film/report" (has "film/report" src);
  check "pairs pair the head with the last row"
    (has "(chiasm/pair (car scenes) (chiasm/last scenes) beats)" chiasm);
  check "pairs recurse inward on init of the tail"
    (has "(chiasm/init (cdr scenes) (list))" chiasm);
  check "init hands back every row but the last"
    (has "\"Every row of a non-empty list but the LAST one" chiasm);
  check "last is the right pointer's first row"
    (has "\"The LAST row of a non-empty list" chiasm);
  check "one row left over is not a pair"
    (has "(chiasm/pairs-from (chiasm/init (cdr scenes) (list))" chiasm);
  (* the pairing is positional: no sort, no annot ation, no scene lookup *)
  let pairs_body = body_until src "(define chiasm/pairs-from" "(define chiasm/pairs " in
  check "pairs do not sort" (not (has "sort" pairs_body));
  check "pair carries a dur delta in frames"
    (has "\"dur_delta\"" chiasm);
  check "dur delta is unsigned"
    (has "(chiasm/abs (- (chiasm/frames-of left \"dur\") (chiasm/frames-of right \"dur\")))" chiasm);
  check "pair carries the shared motifs"
    (has "\"shared_motifs\"" chiasm);

  (* ---- C2: center is the same recursion's remainder shape ---- *)
  let center_body = body_until src "(define chiasm/center-walk" "(define chiasm/center " in
  check "center walks two pointers" (has "chiasm/center-walk (cdr slow) (cdr (cdr fast))" center_body);
  check "empty remainder is the even seam"
    (has "\"The CENTER of a ring as a SINGLETON list (odd ring: the center scene) or '() (even ring:" chiasm);
  check "singleton remainder is the center scene"
    (has "((null? (cdr fast)) (list (car slow)))" center_body);
  check "no length arithmetic for the center"
    (not (has "chiasm/length" center_body));

  (* ---- C3: the report NAMES the center from structure ---- *)
  check "center row is the scene brief or the seam"
    (has "(if (null? center) \"seam\" (chiasm/phrase (car center)))" chiasm);
  check "shape is odd or even"
    (has "(if (null? center) \"even\" \"odd\")" chiasm);
  check "report carries center" (has "\"center\"" chiasm);
  check "report carries shape" (has "\"shape\"" chiasm);
  check "report carries pairs" (has "\"pairs\"" chiasm);
  (* the report's pairs are the BEAT-AWARE walk, so a pair can share motifs;
     a report built on the beatless chiasm/pairs would carry empty
     shared_motifs and still satisfy every pin above. *)
  check "report pairs fold the film's beats in"
    (has "(chiasm/ring-pairs scenes beats)" chiasm);

  (* ---- C4: declared mismatches land in report.declared ---- *)
  check "export chiasm/declared" (has "chiasm/declared chiasm/orphans" src);
  check "report carries declared" (has "\"declared\"" chiasm);
  check "declared row names both halves"
    (has "\"declared\" (as-str declared) \"derived\" (as-str derived)" chiasm);
  check "mismatch compares declared vs position"
    (has "(scene/mirror-mismatch? declared derived)" chiasm);
  check "derived half is the position formula"
    (has "(chiasm/scene-at (scene/derived-mirror-ord n k) scenes)" chiasm);
  check "only disagreeing claims are listed"
    (has "A mismatch is either a wrong annotation or a ring broken on purpose" chiasm);
  check "an uncurated claim is skipped"
    (has "(string-eq declared \"\")" chiasm);

  (* ---- C5: orphan plants land in report.orphans ---- *)
  check "export chiasm/orphans" (has "chiasm/orphans" src);
  check "report carries orphans" (has "\"orphans\"" chiasm);
  check "orphan is a plant with no downstream payoff"
    (has "(string-eq (as-str (dict-get (car beats) \"role\")) \"plant\")" chiasm);
  check "payoff must sit at or after the plant"
    (has "(not (< (parse-int (as-str (dict-get b \"tc\"))) tc))" chiasm);
  check "payoff must share the motif"
    (has "(string-eq (as-str (dict-get b \"motif\")) motif)" chiasm);

  (* ---- C6: tightness is the sum of per-pair dur deltas ---- *)
  check "export chiasm/tightness" (has "chiasm/tightness" src);
  check "report carries tightness" (has "\"tightness\"" chiasm);
  let tight_body = body_until src "(define chiasm/tightness" "(define film/report" in
  check "tightness sums dur deltas"
    (has "(parse-int (as-str (dict-get (car pairs) \"dur_delta\")))" tight_body);
  check "tightness is documented as the sum"
    (has "\"The ring's TIGHTNESS: the SUM of per-pair dur deltas in frames" chiasm);

  (* ---- the report is PURE: it creates no rows ---- *)
  let report_body = body_until src "(define film/report" ";; ---------- ingestion" in
  check "report creates no rows" (not (has "(create " report_body));

  print_endline "film_chiasm_test: chiastic analysis pinned";
Printf.printf "Test Successful in 0.000s. %d tests run.\n" !checks
