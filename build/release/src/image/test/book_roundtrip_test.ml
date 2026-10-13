(* Book pipeline round-trip (i-1788671698-b, no DB).

   The habitat-book pipeline (libs/book, book-chapter rows, book/verify)
   had ZERO tests — only the prod-facing scripts/smoke_book.py — and
   tests/fixtures/sample-book-export.json was referenced by nothing.
   These tests wire the fixture and pin the pipeline's two halves:

   B1. The export-JSON contract (borge book export →
       scripts/load_book.py): root/mode/chapters/code-chapters, chapter
       slug/title/status/nodes, example lang/src/expected, code-chapter
       path/lang/source.
   B2. Round-trip: the exported nopales example evaluates through the
       restricted Example_eval engine to exactly its expect trailer —
       the same comparison book/verify-one makes.
   B3. The SHIPPED book verify engine (book/f, book/row-path,
       book/verify-skip?, book/guard-msg, book/verify-one,
       book/verify-holes from libs/book/lib.pp) over synthesized
       book-chapter rows: a passing example → nil, a drifted expect →
       a hole record (path src expected got), ocaml and no-expect rows
       skip. The example-eval prim is wired over the REAL engine the
       same way session.ml registers it.
   B4. The exported code chapter parses (its defines are loadable
       Nopales). *)

open Pricklypear_lisp.Ast

(* substring containment (contains_seq is not in this stdlib) *)
let contains_seq (hay : string) (needle : string) : bool =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  if m = 0 then true else go 0

let pp = Pricklypear_lisp.Reader.pp_value

(* --- fixtures on disk --------------------------------------------- *)

(* Walk up from the dune cwd until [rel] exists (repo root), like
   seed_sources_test's find_libs_dir. *)
let find_up (rel : string) : string =
  let rec up n dir =
    if n > 10 then failwith ("book_roundtrip_test: not found: " ^ rel)
    else
      let p = Filename.concat dir rel in
      if Sys.file_exists p then dir else up (n + 1) (Filename.dirname dir)
  in
  up 0 (Sys.getcwd ())

let fixture_path () =
  Filename.concat (find_up "tests/fixtures/sample-book-export.json")
    "tests/fixtures/sample-book-export.json"

let real_libs_dir () =
  Filename.concat (find_up "libs/std/lib.pp") "libs"

let read (path : string) : string =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* --- eval harness (example_eval_test precedent, no DB) ------------- *)

let prim_table = ref (Pricklypear_image_lib.Primitives.make_with_defaults ())

let env_of () =
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ -> Pricklypear_lisp.Env.define env name (VPrim name))
    !prim_table;
  env

(* Eval one AST form under the image rim. *)
let eval_form env e =
  Pricklypear_image_lib.Control_rim.run ~prims:!prim_table (fun () ->
      Pricklypear_lisp.Eval.eval ~budget:(Pricklypear_lisp.Eval.make_budget ())
        ~prims:Pricklypear_lisp.Eval.empty_prims env e)

let eval_call env fn args =
  eval_form env (App (Var fn, List.map (fun a -> Lit a) args))

(* Wire the example-eval prim over the REAL restricted engine, the same
   registration session.ml does (rendered via pp_value; refusals become
   User_error the guard in book/verify-one can hole). *)
let install_example_eval env =
  let table = !prim_table in
  Pricklypear_image_lib.Primitives.register table "example-eval" (fun args ->
      match args with
      | [ VStr src ] | [ VSymbol src ] ->
          (try
             VStr
               (Pricklypear_lisp.Reader.pp_value
                  (Pricklypear_image_lib.Example_eval.eval_source
                     ~env ~full_prims:table ~known_fn:(fun _ -> None) src))
           with
           | Pricklypear_image_lib.Example_eval.Refused reason ->
               raise
                 (Interp_error
                    (User_error ("example-eval: refused: " ^ reason, "denied"))))
      | _ ->
          raise
            (Interp_error
               (Prim_error ("example-eval", "need 1 source string", "bad-args"))));
  (* the session auto-binds every prim name into the env as VPrim —
     bind AFTER registering so tests are order-independent *)
  Pricklypear_lisp.Env.define env "example-eval" (VPrim "example-eval")

(* Top-level (define <name> …) / (define (<name> …) …) names in src. *)
let top_level_binds (src : string) : string list =
  let forms =
    Pricklypear_lisp.Reader.read_all
      (Pricklypear_image_lib.Loader.strip_line_comments src)
  in
  List.filter_map
    (fun form ->
       match form with
       | App (Var ("define" | "defmacro"), target :: _) -> (
           match target with
           | Var n -> Some n
           | App (Var n, _) -> Some n
           | _ -> None)
       | _ -> None)
    forms

let find_define src name =
  let forms =
    Pricklypear_lisp.Reader.read_all
      (Pricklypear_image_lib.Loader.strip_line_comments src)
  in
  match
    List.find_opt
      (fun form ->
         match form with
         | App (Var "define", Var n :: _) -> n = name
         | _ -> false)
      forms
  with
  | Some d -> d
  | None -> failwith ("book_roundtrip_test: no (define " ^ name ^ " …) found")

(* B1: the export contract scripts/load_book.py depends on. *)
let get_str fields k =
  match List.assoc_opt k fields with Some (`String s) -> Some s | _ -> None

let get_list fields k =
  match List.assoc_opt k fields with Some (`List l) -> Some l | _ -> None

let obj v = match v with `Assoc f -> f | _ -> Alcotest.fail "expected JSON object"

let test_export_contract () =
  let fields =
    obj (Yojson.Safe.from_string (read (fixture_path ())))
  in
  Alcotest.(check bool) "root present" true (Option.is_some (get_str fields "root"));
  Alcotest.(check string) "mode" "workshop"
    (Option.value ~default:"?" (get_str fields "mode"));
  let chapters =
    match get_list fields "chapters" with
    | Some l when l <> [] -> l
    | _ -> Alcotest.fail "chapters not a nonempty list"
  in
  let ch = obj (List.hd chapters) in
  Alcotest.(check string) "chapter slug" "sample-chapter"
    (Option.value ~default:"?" (get_str ch "slug"));
  Alcotest.(check string) "chapter status" "implemented"
    (Option.value ~default:"?" (get_str ch "status"));
  (match get_list ch "nodes" with
   | Some ns -> Alcotest.(check bool) "chapter has nodes" true (ns <> [])
   | None -> Alcotest.fail "chapter nodes missing");
  let examples =
    match get_list ch "examples" with
    | Some l when List.length l = 2 -> l
    | _ -> Alcotest.fail "chapter examples not a 2-list"
  in
  let ex0 = obj (List.nth examples 0) in
  Alcotest.(check string) "example lang" "nopales"
    (Option.value ~default:"?" (get_str ex0 "lang"));
  Alcotest.(check string) "example src" "(+ 1 2)\n"
    (Option.value ~default:"?" (get_str ex0 "src"));
  Alcotest.(check string) "example expected" "3"
    (Option.value ~default:"?" (get_str ex0 "expected"));
  let ex1 = obj (List.nth examples 1) in
  Alcotest.(check string) "ocaml lang" "ocaml"
    (Option.value ~default:"?" (get_str ex1 "lang"));
  (match List.assoc_opt "expected" ex1 with
   | Some `Null -> ()
   | _ -> Alcotest.fail "ocaml expected is not null");
  let ccs =
    match get_list fields "code-chapters" with
    | Some l when List.length l = 1 -> l
    | _ -> Alcotest.fail "code-chapters not a 1-list"
  in
  let cc = obj (List.hd ccs) in
  Alcotest.(check string) "code-chapter path" "sample-code.pp"
    (Option.value ~default:"?" (get_str cc "path"));
  Alcotest.(check string) "code-chapter lang" "pp"
    (Option.value ~default:"?" (get_str cc "lang"));
  (match get_str cc "source" with
   | Some s ->
       Alcotest.(check bool) "code-chapter source names sample/greet" true
         (contains_seq s "sample/greet")
   | None -> Alcotest.fail "code-chapter source missing")

(* B2: the exported nopales example round-trips through the restricted
   engine to exactly its expect trailer. *)
let test_fixture_example_evals () =
  let env = env_of () in
  let expected = "3" in
  match
    Pricklypear_image_lib.Example_eval.eval_source
      ~env ~full_prims:!prim_table ~known_fn:(fun _ -> None)
      "(+ 1 2)\n"
  with
  | v ->
      Alcotest.(check string) "expect trailer holds" expected
        (String.trim (pp v))
  | exception Pricklypear_image_lib.Example_eval.Refused reason ->
      Alcotest.fail ("example refused: " ^ reason)

(* B3: the SHIPPED book verify engine over synthesized book-chapter
   rows (row JSON = {path, body}; body = JSON string with the example
   fields — the shape tree/get returns). *)
let row_json path body_fields =
  let body = `Assoc body_fields |> Yojson.Safe.to_string in
  `Assoc [ ("path", `String path); ("body", `String body) ]
  |> Yojson.Safe.to_string

let book_env () =
  Unix.putenv "PP_LIBS_DIR" (real_libs_dir ());
  let src =
    match
      Pricklypear_image_lib.Loader.read_file
        (Pricklypear_image_lib.Loader.library_path "book")
    with
    | Ok s -> s
    | Error e -> failwith ("book_roundtrip_test: " ^ e)
  in
  let env = env_of () in
  (* the verify path and its helpers, from the SHIPPED source *)
  List.iter
    (fun name -> eval_form env (find_define src name) |> ignore)
    [ "book/f"; "book/row-path"; "book/verify-skip?"; "book/guard-msg"
    ; "book/verify-one"; "book/verify-holes" ];
  install_example_eval env;
  (env, src)

let verify_one env node =
  eval_call env "book/verify-one" [ VStr node ]

let test_verify_pass_is_nil () =
  let env, _ = book_env () in
  let node =
    row_json "data.book.ch.fix.s1"
      [ ("node", `String "example")
      ; ("lang", `String "nopales")
      ; ("src", `String "(+ 1 2)\n")
      ; ("expected", `String "3") ]
  in
  Alcotest.(check string) "passing example verifies to nil" "()"
    (pp (verify_one env node))

let test_verify_drift_is_a_hole () =
  let env, _ = book_env () in
  let path = "data.book.ch.fix.s2" in
  let node =
    row_json path
      [ ("node", `String "example")
      ; ("lang", `String "nopales")
      ; ("src", `String "(+ 1 2)\n")
      ; ("expected", `String "4") ]
  in
  match verify_one env node with
  | VList [ VStr p; VStr s; VStr e; VStr g ] ->
      Alcotest.(check string) "hole path" path p;
      Alcotest.(check string) "hole src" "(+ 1 2)\n" s;
      Alcotest.(check string) "hole expected" "4" e;
      Alcotest.(check string) "hole got (rendered)" "3" g
  | v -> Alcotest.fail ("expected a 4-slot hole record, got: " ^ pp v)

let test_verify_skips () =
  let env, _ = book_env () in
  (* ocaml blocks are captured, not live — skipped even with an expect *)
  let ocaml =
    row_json "data.book.ch.fix.s3"
      [ ("node", `String "example")
      ; ("lang", `String "ocaml")
      ; ("src", `String "let double x = x * 2")
      ; ("expected", `String "captured") ]
  in
  Alcotest.(check string) "ocaml skipped" "()" (pp (verify_one env ocaml));
  (* no expect trailer — skipped *)
  let noexpect =
    row_json "data.book.ch.fix.s4"
      [ ("node", `String "example")
      ; ("lang", `String "nopales")
      ; ("src", `String "(+ 1 2)") ]
  in
  Alcotest.(check string) "no-expect skipped" "()" (pp (verify_one env noexpect))

let test_verify_holes_over_rowset () =
  let env, _ = book_env () in
  let pass =
    row_json "data.book.ch.fix.s1"
      [ ("node", `String "example")
      ; ("lang", `String "nopales")
      ; ("src", `String "(+ 1 2)\n")
      ; ("expected", `String "3") ]
  in
  let drift =
    row_json "data.book.ch.fix.s2"
      [ ("node", `String "example")
      ; ("lang", `String "nopales")
      ; ("src", `String "(+ 1 2)\n")
      ; ("expected", `String "4") ]
  in
  let ocaml =
    row_json "data.book.ch.fix.s3"
      [ ("node", `String "example")
      ; ("lang", `String "ocaml")
      ; ("src", `String "let double x = x * 2")
      ; ("expected", `String "captured") ]
  in
  let rows = VList [ VStr pass; VStr drift; VStr ocaml ] in
  match eval_call env "book/verify-holes" [ rows ] with
  | VList [ hole ] -> (
      match hole with
      | VList [ VStr p; _; VStr e; VStr _ ] ->
          Alcotest.(check string) "only the drift holes" "data.book.ch.fix.s2" p;
          Alcotest.(check string) "drift expected" "4" e
      | _ -> Alcotest.fail "hole is not a 4-list")
  | v -> Alcotest.fail ("expected exactly one hole, got: " ^ pp v)

(* B4: the exported code chapter parses as Nopales (loadable source). *)
let test_code_chapter_parses () =
  let fields =
    obj (Yojson.Safe.from_string (read (fixture_path ())))
  in
  let ccs =
    match get_list fields "code-chapters" with
    | Some l when List.length l = 1 -> l
    | _ -> Alcotest.fail "code-chapters not a 1-list"
  in
  match get_str (obj (List.hd ccs)) "source" with
  | Some src ->
      let binds = top_level_binds src in
      Alcotest.(check bool) "sample/greet defined" true
        (List.mem "sample/greet" binds)
  | None -> Alcotest.fail "code-chapter source missing"

let () =
  Alcotest.run "pricklypear-book-roundtrip"
    [ ( "book-roundtrip"
      , [ ("export-contract", `Quick, test_export_contract)
        ; ("fixture-example-evals", `Quick, test_fixture_example_evals)
        ; ("verify-pass-is-nil", `Quick, test_verify_pass_is_nil)
        ; ("verify-drift-is-a-hole", `Quick, test_verify_drift_is_a_hole)
        ; ("verify-skips", `Quick, test_verify_skips)
        ; ("verify-holes-over-rowset", `Quick, test_verify_holes_over_rowset)
        ; ("code-chapter-parses", `Quick, test_code_chapter_parses) ] ) ]
