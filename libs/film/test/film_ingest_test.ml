(* Film INGESTION pins (borge/film-analysis.borg, subsection ingestion).

   lib.pp is SEED SOURCE, not an OCaml module (the film_shape_test
   precedent): the suite reads the source and pins the emitter the laptop
   pipes to pp_eval.

     I1. an scdet log is read by its `lavfi.scd.time:` marker, in log
         order, as the detector's SECOND strings -- the only place a
         second count exists.
     I2. seconds->frames converts ONCE at the film's fps with round, so a
         row's tc_in/dur are INTEGER frame counts and seconds stay
         render-only.
     I3. ingest emits ORDERED `(scene-create ...)` source: ord starts at
         1 and increments, the slug is CUT-sORD, and mirror is the EMPTY
         declaration (the ring is curated on a fork, never by the
         detector).
     I4. the emitter is PURE: nothing is created and no branch is written,
         so a re-ingest at a new threshold takes a NEW cut label
         (film/auto-cut -> auto-t<N>) and the old -auto branch is
         untouched.

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

let check name cond = Alcotest.(check bool) name true cond
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

let load () =
  let path =
    if Sys.file_exists "lib.pp" then "lib.pp"
    else if Sys.file_exists "../lib.pp" then "../lib.pp"
    else failwith "film_ingest_test: lib.pp not found"
  in
  collapse (read path)

let () =
  let src = load () in
  let ingest = slice_from src "(define film/scd/time-marker" in

  (* I1: the marker ffmpeg's scdet writes, and the marker scan. *)
  check "export film/scdet-times" (has "film/scdet-times" src);
  check "scd time marker" (has "lavfi.scd.time:" ingest);
  check "marker-pos scans the line" (has "\"Index of the scdet time marker in one log line, or -1.\"" ingest);

  (* I2: one conversion, at the film's fps, rounded to an integer frame. *)
  check "export film/seconds->frames" (has "film/seconds->frames" src);
  check "seconds convert once at fps"
    (has "(round (* (parse-float (as-str fps)) (parse-float (as-str secs))))" ingest);
  check "frames ride film/frames" (has "(film/frames tc)" ingest);
  check "dur rides film/frames" (has "(film/frames dur)" ingest);

  (* I3: ordered scene-create source with a cut-scoped machine slug and no
     declared mirror. *)
  check "emits scene-create forms" (has "(scene-create " ingest);
  check "slug names its own cut"
    (has "(string-append (as-str cut) \"-s\" (as-str ord))" ingest);
  check "mirror declared empty" (has "(film/scd/q \"\")" ingest);
  check "ingest starts at ord 1" (has "(film/scd/forms film-id cut" ingest);
  check "first scene begins at frame 0" (has "(if (= (car frames) 0) frames (cons 0 frames))" ingest);

  (* I4: the emitter is pure; a new threshold is a new cut label. *)
  check "export film/auto-cut" (has "film/auto-cut" src);
  check "auto cut label names its threshold"
    (has "(string-append \"auto-t\" (as-str (parse-int (as-str threshold))))" ingest);
  check "emitter creates no rows" (not (has "(create" ingest));
  check "emitter is documented pure" (has "PURE: nothing is created and no branch is written here." ingest);
  check "re-ingest takes a new label" (has "a re-ingest at a NEW label" ingest
        || has "takes a NEW label" ingest);

  print_endline "film_ingest_test: ingestion pinned"
