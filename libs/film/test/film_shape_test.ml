(* Film model shape pins (borge/film-analysis.borg, subsection model).

   lib.pp is SEED SOURCE, not an OCaml module: the image seeds it at boot
   and `(load-library "film")` installs the film/... bindings. So these
   pins read the SOURCE (the seed_sources_test / pp_clean_build_pins
   precedent) and assert the four things the borge leaf names:

     T1. film, scene, motif and beat are declared with this section's
         fields (and fps lives on FILM alone).
     T2. ord / tc_in / dur / a beat's tc are frame INTEGER counts: every
         one is wrapped in film/frames, which parses to an int so no
         float or second count can ride into a row.
     T3. scene-create stores cut/ord/tc_in/dur and leaves mirror a
         DECLARATION (as-str, never a frame conversion).
     T4. derivation comes from ord: scene/derived-mirror-ord is pure ord
         arithmetic (n-k+1) and never reads the stored mirror field.

   Run: `dune runtest libs/film` from pp/. *)

let failures = ref 0
let checks = ref 0

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let contains ~needle hay =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

(* Whitespace runs collapse to one space, so a needle pins the FORM and
   not lib.pp's indentation. *)
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

let fail name msg =
  incr failures;
  Printf.eprintf "FAIL %s: %s\n" name msg

let check_has name hay needle =
  incr checks;
  if not (contains ~needle hay) then
    fail name (Printf.sprintf "missing %S" needle)

let check_lacks name hay needle =
  incr checks;
  if contains ~needle hay then fail name (Printf.sprintf "found %S" needle)

(* ---- deftype field extraction ---- *)

let find_sub hay needle start =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then None
    else if String.sub hay i n = needle then Some i
    else go (i + 1)
  in
  go start

(* The balanced (deftype ...) form whose first (quote NAME) is [name], as
   its list of quoted symbols: the type name first, then its fields. *)
let deftype_names src name =
  let h = String.length src in
  let rec scan j depth =
    if j >= h then j
    else
      match src.[j] with
      | '(' -> scan (j + 1) (depth + 1)
      | ')' -> if depth = 1 then j + 1 else scan (j + 1) (depth - 1)
      | _ -> scan (j + 1) depth
  in
  let rec quotes sl from acc =
    match find_sub sl "(quote " from with
    | None -> List.rev acc
    | Some j ->
        let k = j + String.length "(quote " in
        let e =
          try String.index_from sl k ')' with Not_found -> String.length sl
        in
        let nm = String.trim (String.sub sl k (e - k)) in
        quotes sl (e + 1) (nm :: acc)
  in
  let rec go from =
    match find_sub src "(deftype" from with
    | None -> None
    | Some i ->
        let stop = scan i 0 in
        let sl = String.sub src i (stop - i) in
        (match quotes sl 0 [] with
         | t :: fields when t = name -> Some fields
         | _ -> go stop)
  in
  go 0

let check_fields src name expected =
  incr checks;
  match deftype_names src name with
  | None -> fail ("deftype " ^ name) "no (deftype (quote ...) ...) form found"
  | Some fields ->
      if fields <> expected then
        fail ("deftype " ^ name)
          (Printf.sprintf "fields %s, want %s"
             (String.concat " " fields)
             (String.concat " " expected))

let () =
  let root_lib =
    (* dune runs the test in _build; lib.pp is a dep of the cwd. *)
    if Sys.file_exists "lib.pp" then "lib.pp"
    else if Sys.file_exists "../lib.pp" then "../lib.pp"
    else failwith "film_shape_test: lib.pp not found"
  in
  let src = collapse (read root_lib) in

  (* T1: the four types carry THIS section's fields. *)
  check_fields src "film" [ "slug"; "title"; "year"; "fps"; "notes" ];
  check_fields src "scene"
    [ "film"; "cut"; "ord"; "slug"; "summary"; "tc_in"; "dur"; "mirror" ];
  check_fields src "motif" [ "film"; "name"; "kind" ];
  check_fields src "beat" [ "scene"; "motif"; "role"; "tc" ];
  (* T1b: each deftype rides an IDEMPOTENT value define -- the seed lint
     only allows define/route/deftype/... at top level, and a re-seed must
     leave an existing declaration alone instead of re-declaring it. *)
  List.iter
    (fun name ->
      check_has ("guard " ^ name) src
        (Printf.sprintf "(if (null? (type-fields (quote %s)))" name);
      check_has ("guard fallback " ^ name) src
        (Printf.sprintf "\"%s ok\"" name))
    [ "film"; "scene"; "motif"; "beat" ];
  (* T1c: the header names the DIRECTORY package -- load-library strips the
     (package NAME (export ...)) header by name, so a mismatch silently
     drops the film/... bindings this leaf exists to provide. *)
  check_has "package header" src "(package film";
  check_has "exports film/create" src "film/create";
  check_has "exports scene-create" src "scene-create";
  check_has "exports motif-create" src "motif-create";
  check_has "exports beat-create" src "beat-create";
  (* fps lives on the film row alone; no type stores a rate or a second. *)
  List.iter
    (fun name ->
      match deftype_names src name with
      | None -> fail ("deftype " ^ name) "missing"
      | Some fields ->
          List.iter
            (fun bad ->
              if List.mem bad fields then
                fail ("deftype " ^ name) (Printf.sprintf "unexpected field %S" bad))
            [ "fps"; "sec"; "seconds"; "frame"; "clock" ])
    [ "scene"; "motif"; "beat" ];

  (* T2: integer frames. film/frames parses to an int, and each stored
     timecode rides it. *)
  check_has "film/frames integer"
    src "\"A frame count as its canonical integer string. Every timecode this library stores (ord, tc_in, dur, beat tc) is an INTEGER FRAME COUNT";
  check_has "film/frames parses int" src "(as-str (parse-int (as-str n)))";
  check_has "ord is frames" src "\"ord\" (film/frames ord)";
  check_has "tc_in is frames" src "\"tc_in\" (film/frames tc-in)";
  check_has "dur is frames" src "\"dur\" (film/frames dur)";
  check_has "beat tc is frames" src "\"tc\" (film/frames tc)";
  (* fps is on the film row, so seconds stay render-only. *)
  check_has "fps on film/create" src "\"fps\" (as-str fps)";

  (* T3: scene-create stores cut/ord/tc_in/dur; mirror is a DECLARATION. *)
  check_has "scene-create stores cut" src "\"cut\" (as-str cut)";
  check_has "scene-create stores mirror as declaration"
    src "\"mirror\" (as-str mirror)";
  check_lacks "scene-create does not convert mirror to frames"
    src "\"mirror\" (film/frames";
  check_has "mirror declared field doc"
    src "mirror is the DECLARED mirror scene slug (empty until curated)";

  (* T3b: the other two constructors store THIS section's fields as well --
     motif-create (film/name/kind) and beat-create (scene/motif/role) -- so
     every declared field has a writer, not just a shape pin. *)
  check_has "motif-create stores film" src "\"film\" (as-str film-id)";
  check_has "motif-create stores name" src "\"name\" (as-str name)";
  check_has "motif-create stores kind" src "\"kind\" (as-str kind)";
  check_has "beat-create stores scene" src "\"scene\" (as-str scene-id)";
  check_has "beat-create stores motif" src "\"motif\" (as-str motif)";
  check_has "beat-create stores role" src "\"role\" (as-str role)";

  (* T4: derivation comes from ord alone -- n-k+1, no row lookup. *)
  check_has "derived mirror is ord arithmetic" src "(- (+ n 1) k)";
  check_has "derived mirror answer documented"
    src "Derived mirror position: in a ring of n scenes, scene k mirrors scene n-k+1";
  (match deftype_names src "scene" with
   | Some _ -> ()
   | None -> ());
  check_has "mismatch compares declared vs derived"
    src "scene/mirror-mismatch?";
  (* the DECLARED half must read the STORED claim (dict-get on "mirror"):
     the linter needs both halves -- claim and position -- to report where
     they disagree. *)
  (match find_sub src "(define scene/declared-mirror" 0 with
   | None -> fail "scene/declared-mirror" "define not found"
   | Some i ->
       let rest = String.sub src i (String.length src - i) in
       let body_len =
         match find_sub rest "(define scene/derived-mirror-ord" 0 with
         | Some j -> j
         | None -> String.length rest
       in
       let body = String.sub rest 0 body_len in
       check_has "declared mirror reads the stored claim" body "dict-get";
       check_has "declared mirror key is mirror" body "mirror");
  (* the derivation section must not reach for a stored mirror value *)
  (match find_sub src "(define scene/derived-mirror-ord" 0 with
   | None -> fail "scene/derived-mirror-ord" "define not found"
   | Some i ->
       let rest = String.sub src i (String.length src - i) in
       let body_len =
         match find_sub rest "(define scene/mirror-mismatch?" 0 with
         | Some j -> j
         | None -> String.length rest
       in
       let body = String.sub rest 0 body_len in
       check_lacks "derived mirror reads no stored field" body "dict-get";
       check_lacks "derived mirror reads no stored mirror" body "\"mirror\"");

  if !failures > 0 then begin
    Printf.eprintf "film_shape_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "film_shape_test: model shapes pinned";
Printf.printf "Test Successful in 0.000s. %d tests run.\n" !checks
