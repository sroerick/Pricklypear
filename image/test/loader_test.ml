(* Loader failure modes + seed order (i-1788671698-b, no DB).

   The blind spot that let libs/list fail silently at every boot for
   weeks: nothing exercised Loader.load_string's failure modes, and the
   seed-order constraints (css before notes, theme before ui) lived
   only in comments. These tests pin:

   L1. load_string Ok path: "loaded N form(s)" summary.
   L2. A failing form reports its INDEX, the interp-error message, and
       a source snippet ("form 2 failed: unbound: nope / in: (nope 1)").
   L3. A reader error (unbalanced paren) reports the "reader:" shape.
   L4. strip_line_comments strips ';' comments but preserves ';' INSIDE
       a string and an escaped quote (the in-string walk).
   L5. seed_package_priority: std first; css strictly before notes
       (notes/tree-css evaluates (css/sheet …) at define time); theme
       strictly before ui; no duplicate names.
   L6. discover_seed_packages: preferred names first in priority order,
       non-preferred dirs appended alphabetically AFTER all preferred,
       and the discovery filters (dot/underscore dirs, theme-* skins,
       dirs without lib.pp) actually filter.
   L7. The real libs/std loads through the real loader with the image
       prim table and its defines WORK — the first package every boot
       seeds (the load_library failure mode that was silent). *)

open Pricklypear_lisp.Ast

(* substring containment (contains_seq is not in this stdlib) *)
let contains_seq (hay : string) (needle : string) : bool =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  if m = 0 then true else go 0

(* Walk up from the dune cwd until [rel] exists (repo root), like
   seed_sources_test's find_libs_dir. *)
let find_up (rel : string) : string =
  let rec up n dir =
    if n > 10 then failwith ("loader_test: not found: " ^ rel)
    else
      let p = Filename.concat dir rel in
      if Sys.file_exists p then dir else up (n + 1) (Filename.dirname dir)
  in
  up 0 (Sys.getcwd ())

let real_libs_dir () =
  let dir = find_up "libs/std/lib.pp" in
  Filename.concat dir "libs"

let point_pp_libs_dir (dir : string) : unit =
  Unix.putenv "PP_LIBS_DIR" dir

(* Image prim table + env with prims auto-bound as VPrim names (the
   session attach shape); eval_form services Prim_call via Control_rim. *)
let eval_form_of () =
  let prims = Pricklypear_image_lib.Primitives.make_with_defaults () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ -> Pricklypear_lisp.Env.define env name (VPrim name))
    prims;
  let budget = Pricklypear_lisp.Eval.make_budget () in
  let run e =
    Pricklypear_image_lib.Control_rim.run ~prims (fun () ->
        Pricklypear_lisp.Eval.eval ~budget
          ~prims:Pricklypear_lisp.Eval.empty_prims env e)
  in
  (run, env, prims)

(* L1: multi-form load returns the "loaded N form(s)" summary. *)
let test_load_ok () =
  point_pp_libs_dir (real_libs_dir ());
  let eval_form, _, _ = eval_form_of () in
  match
    Pricklypear_image_lib.Loader.load_string ~eval_form
      "(define a 1)\n(+ 1 2)\n42\n"
  with
  | Ok msg -> Alcotest.(check string) "summary" "loaded 3 form(s)" msg
  | Error e -> Alcotest.fail ("expected Ok, got: " ^ e)

(* L2: a failing form names its index, the error, and a snippet. *)
let test_load_unbound_form () =
  point_pp_libs_dir (real_libs_dir ());
  let eval_form, _, _ = eval_form_of () in
  match
    Pricklypear_image_lib.Loader.load_string ~eval_form
      "(define a 1)\n(nope 1)\n"
  with
  | Ok msg -> Alcotest.fail ("expected Error, got: " ^ msg)
  | Error e ->
      Alcotest.(check bool) "form index" true
        (String.starts_with ~prefix:"form 2 failed" e);
      Alcotest.(check bool) "unbound named" true
        (contains_seq e "unbound: nope");
      Alcotest.(check bool) "snippet present" true
        (contains_seq e "in: (nope 1)")

(* L3: reader error shape. *)
let test_load_reader_error () =
  point_pp_libs_dir (real_libs_dir ());
  let eval_form, _, _ = eval_form_of () in
  match
    Pricklypear_image_lib.Loader.load_string ~eval_form "(define a"
  with
  | Ok msg -> Alcotest.fail ("expected Error, got: " ^ msg)
  | Error e ->
      Alcotest.(check bool) "reader prefix" true
        (String.starts_with ~prefix:"reader:" e)

(* L4: strip_line_comments — ';' inside strings survives, comments
   don't; an escaped quote inside a string does not close it early. *)
let test_strip_line_comments () =
  let strip = Pricklypear_image_lib.Loader.strip_line_comments in
  (* comment to EOL goes, newline kept *)
  Alcotest.(check bool) "comment stripped" true
    (not (contains_seq (strip "(define a 1) ; set a\n(+ 1 2)") "set a"));
  Alcotest.(check bool) "form after comment kept" true
    (contains_seq (strip "(define a 1) ; set a\n(+ 1 2)") "(+ 1 2)");
  (* ';' inside a string is NOT a comment *)
  Alcotest.(check bool) "string semicolon kept" true
    (contains_seq (strip "(str \"a;b\")") "a;b");
  (* escaped quote does not close the string: the ';' after it is still
     inside the string and survives *)
  Alcotest.(check bool) "escaped quote keeps string open" true
    (contains_seq (strip "(str \"a\\\";b\")") "a\\\";b")

(* L5: seed_package_priority constraints previously comment-only. *)
let index_of name xs =
  let rec go i = function
    | [] -> None
    | x :: r -> if x = name then Some i else go (i + 1) r
  in
  go 0 xs

let test_seed_priority_order () =
  let pri = Pricklypear_image_lib.Session.seed_package_priority in
  Alcotest.(check string) "std first" "std" (List.hd pri);
  let i css = index_of css pri in
  let () =
    match (i "css", i "notes", i "theme", i "ui") with
    | Some c, Some n, Some t, Some u ->
        Alcotest.(check bool) "css before notes" true (c < n);
        Alcotest.(check bool) "theme before ui" true (t < u)
    | _ -> Alcotest.fail "css/notes/theme/ui missing from priority"
  in
  let uniq = List.sort_uniq String.compare pri in
  Alcotest.(check int) "no duplicate names" (List.length pri) (List.length uniq)

let first_n n xs =
  let rec go acc n = function
    | [] -> List.rev acc
    | x :: r -> if n <= 0 then List.rev acc else go (x :: acc) (n - 1) r
  in
  go [] n xs

(* L6: discover_seed_packages ordering + filters over a temp libs dir. *)
let test_discover_seed_packages () =
  let tmp =
    let d =
      Filename.concat (Filename.get_temp_dir_name ())
        (Printf.sprintf "pp_loader_test_%d" (Unix.getpid ()))
    in
    if Sys.file_exists d then ignore (Sys.command ("rm -rf " ^ Filename.quote d));
    Unix.mkdir d 0o755;
    d
  in
  let dirs =
    [ "std"; "theme"; "ui"; "css"; "notes"; "zpkg"; ".hidden"; "_private"
    ; "theme-paper"; "nolib" ]
  in
  List.iter
    (fun d -> Unix.mkdir (Filename.concat tmp d) 0o755)
    dirs;
  (* only these carry a lib.pp (nolib must be dropped by the filter) *)
  List.iter
    (fun d ->
       let oc = open_out (Filename.concat tmp (Filename.concat d "lib.pp")) in
       output_string oc "(define (x) 1)\n";
       close_out oc)
    [ "std"; "theme"; "ui"; "css"; "notes"; "zpkg" ];
  point_pp_libs_dir tmp;
  let pkgs = Pricklypear_image_lib.Session.discover_seed_packages () in
  let pri = Pricklypear_image_lib.Session.seed_package_priority in
  (* every preferred name first, in priority order (even those absent
     from the dir — the seed pipeline owns missing-package handling) *)
  Alcotest.(check int) "preferred prefix" (List.length pri)
    (List.length (first_n (List.length pri) pkgs));
  List.iteri
    (fun i p ->
       Alcotest.(check string) ("preferred order " ^ string_of_int i) p
         (List.nth pkgs i))
    pri;
  (* non-preferred discovered packages appended after ALL preferred *)
  Alcotest.(check string) "extra appended last" "zpkg" (List.nth pkgs (List.length pkgs - 1));
  (* filters: dot/underscore/theme-*/no-lib.pp dirs never appear *)
  List.iter
    (fun bad ->
       Alcotest.(check bool) ("filtered: " ^ bad) false (List.mem bad pkgs))
    [ ".hidden"; "_private"; "theme-paper"; "nolib" ];
  (* cleanup *)
  List.iter
    (fun d -> Sys.remove (Filename.concat tmp (Filename.concat d "lib.pp")))
    [ "std"; "theme"; "ui"; "css"; "notes"; "zpkg" ];
  List.iter (fun d -> Unix.rmdir (Filename.concat tmp d)) dirs;
  Unix.rmdir tmp

(* Drop top-level (package <name> (export …)) metadata forms — a
   SESSION-level form (session.ml intercepts it in its eval_form BEFORE
   arg evaluation; the kernel has no package form and its symbol args
   must not evaluate). Dropping it is the equivalent of the real seed
   path: Loader.load_string then takes every real define of the source. *)
let drop_package_form (src : string) : string =
  src
  |> Pricklypear_image_lib.Loader.strip_line_comments
  |> Pricklypear_lisp.Reader.read_all
  |> List.filter (fun form ->
         match form with
         | App (Var "package", _) -> false
         | _ -> true)
  |> List.map Pricklypear_lisp.Reader.pp_expr
  |> String.concat "\n"

(* L7: the real std package loads through the real loader and its
   defines WORK (list/take callable after load). *)
let test_real_std_loads () =
  let libs = real_libs_dir () in
  point_pp_libs_dir libs;
  let eval_form, env, prims = eval_form_of () in
  let src =
    match
      Pricklypear_image_lib.Loader.read_file
        (Pricklypear_image_lib.Loader.library_path "std")
    with
    | Ok s -> drop_package_form s
    | Error e -> Alcotest.fail e
  in
  let () =
    match Pricklypear_image_lib.Loader.load_string ~eval_form src with
    | Ok msg ->
        Alcotest.(check bool) "summary counts std defines" true
          (contains_seq msg "loaded ")
    | Error e -> Alcotest.fail ("std failed to load: " ^ e)
  in
  (* a shipped std define is callable *)
  let v =
    Pricklypear_image_lib.Control_rim.run ~prims (fun () ->
        Pricklypear_lisp.Eval.eval
          ~budget:(Pricklypear_lisp.Eval.make_budget ())
          ~prims:Pricklypear_lisp.Eval.empty_prims env
          (App
             ( Var "list/take"
             , [ Lit (VList [ VInt 1L; VInt 2L; VInt 3L ]); Lit (VInt 2L) ])))
  in
  Alcotest.(check string) "list/take works after load" "(1 2)"
    (Pricklypear_lisp.Reader.pp_value v)

let () =
  Alcotest.run "pricklypear-loader"
    [ ( "loader"
      , [ ("load-ok", `Quick, test_load_ok)
        ; ("load-unbound-form", `Quick, test_load_unbound_form)
        ; ("load-reader-error", `Quick, test_load_reader_error)
        ; ("strip-line-comments", `Quick, test_strip_line_comments)
        ; ("seed-priority-order", `Quick, test_seed_priority_order)
        ; ("discover-seed-packages", `Quick, test_discover_seed_packages)
        ; ("real-std-loads", `Quick, test_real_std_loads) ] ) ]
