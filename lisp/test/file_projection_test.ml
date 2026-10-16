(* file-bridge P1 (branching.file-bridge, spec commit 3f4d6d1, 2026-10-16):
   the pure projection format — a local file is a manifest of defs + raw
   spans. Round-trip properties:
     materialize (parse f) = f                       (byte-identical)
     parse (materialize p) = p                       (spans preserved)
     of_defs files carry no spans beyond the generated manifest
   Pure tests: no Postgres, no interpreter state. *)

module FP = Pricklypear_lisp.File_projection

let sample =
  {|;;; branch: admin/dev   base-gen: 412   projection: v1
;;; a leading comment block the parser owns

;; todos: parse-todo
(define (parse-todo row)
  (get row "title"))

; a stray top-level form is a raw span, never a def
(not-a-define 1 2)

(define (render-todo t)
  (str "todo: " t))
|}

let parse_ok file =
  match FP.parse file with
  | Ok p -> p
  | Error e -> Alcotest.fail ("parse failed: " ^ e)

(* ---- 1. byte-identical round-trip ---- *)

let test_roundtrip () =
  let p = parse_ok sample in
  Alcotest.(check string) "materialize (parse f) = f" sample
    (FP.materialize p);
  let p2 = parse_ok (FP.materialize p) in
  Alcotest.(check bool) "parse (materialize p) = p" true (p = p2)

(* ---- 2. def extraction + manifest header ---- *)

let test_names_and_header () =
  let p = parse_ok sample in
  Alcotest.(check (list string)) "def names in order"
    [ "parse-todo"; "render-todo" ] (FP.def_names p);
  let branch, gen, version = FP.header p in
  Alcotest.(check string) "manifest branch" "admin/dev" branch;
  Alcotest.(check int) "manifest base-gen" 412 (Int64.to_int gen);
  Alcotest.(check string) "manifest projection version" "v1" version;
  (* the stray form and comments did NOT become defs *)
  Alcotest.(check bool) "stray form not a def" true
    (not (List.mem "not-a-define" (FP.def_names p)))

(* ---- 3. of_defs: a branch def set becomes a projection with no spans ----

   Semantic round-trip: parse re-attaches the generated manifest to the
   first def's slot (that is where a file's leading raw lives), so compare
   names + bodies + header fields, not the raw record. *)

let test_of_defs () =
  let body = "(define (parse-todo row)\n  (get row \"title\"))" in
  let p =
    FP.of_defs ~branch:"admin/dev" ~base_gen:412L
      [ ("parse-todo", body); ("render-todo", "(define (render-todo t) t)") ]
  in
  let p2 = parse_ok (FP.materialize p) in
  Alcotest.(check (list string)) "names round-trip" (FP.def_names p)
    (FP.def_names p2);
  Alcotest.(check bool) "bodies byte-exact" true
    (List.map (fun (d : FP.def_slot) -> d.FP.d_body) p2.FP.p_defs
    = [ body; "(define (render-todo t) t)" ]);
  Alcotest.(check bool) "header fields round-trip" true
    (FP.header p2 = FP.header p);
  (* spans empty beyond the generated manifest: no def carries a before *)
  Alcotest.(check bool) "no raw spans" true
    (List.for_all (fun (d : FP.def_slot) -> d.FP.d_before = "") p2.FP.p_defs);
  (* of_defs materialize is byte-stable too *)
  Alcotest.(check string) "materialize (parse m) = m" (FP.materialize p)
    (FP.materialize p2)

(* ---- 4. unbalanced parens refuse at the file level ---- *)

let test_unbalanced () =
  Alcotest.(check bool) "truncated file = Error, never a raw span" true
    (match FP.parse "(define (f x) (+ x 1)" with
     | Error _ -> true
     | Ok _ -> false)

(* ---- 5. raw-span edits survive materialize; defs ride along ---- *)

let test_span_edit_survives () =
  let p = parse_ok sample in
  let p2 =
    match p.FP.p_defs with
    | d :: rest -> { p with FP.p_defs = { d with FP.d_before = ";; new comment\n" } :: rest }
    | [] -> p
  in
  let p3 = parse_ok (FP.materialize p2) in
    Alcotest.(check bool) "span edit carried into the file's leading raw" true
      (p3.FP.p_header = p.FP.p_header ^ ";; new comment\n");
    Alcotest.(check bool) "defs ride along untouched" true
      (FP.def_names p3 = FP.def_names p
       && List.map (fun (d : FP.def_slot) -> d.FP.d_body) p3.FP.p_defs
          = List.map (fun (d : FP.def_slot) -> d.FP.d_body) p.FP.p_defs);
  Alcotest.(check string) "full round-trip of the edited file"
    (FP.materialize p2) (FP.materialize p3)

let () =
  Alcotest.run "file-projection"
    [
      ( "round-trip"
      , [
          Alcotest.test_case "materialize (parse f) = f, byte-identical" `Quick
            test_roundtrip;
          Alcotest.test_case "def names + manifest header" `Quick
            test_names_and_header;
          Alcotest.test_case "of_defs: no spans, semantic round-trip" `Quick
            test_of_defs;
          Alcotest.test_case "unbalanced parens refuse" `Quick test_unbalanced;
          Alcotest.test_case "raw-span edits survive materialize" `Quick
            test_span_edit_survives;
        ] )
    ]
