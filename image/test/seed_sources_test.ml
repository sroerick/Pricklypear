(** Seed-source static checks (no DB) — i-1788670156.

    The libs/list package defined list/filter, list/foldl, list/map —
    all kernel special forms — so assert_bindable_name refused them and
    the package failed to seed at EVERY boot, swallowed to one stderr
    line; its list/take never landed on a fresh DB and calendar/upcoming
    (which calls list/take) broke on clean environments.

    These tests pin the bug class at the seed-source level:

    S1. No seedable libs/<pkg>/lib.pp may bind a kernel special-form
        name with top-level define/defmacro — such a define is refused
        by assert_bindable_name and aborts that package's seed mid-file.
    S2. libs/std (the first seed package) defines list/take, which
        calendar/upcoming calls — it must exist on every clean env. *)

let find_libs_dir () =
  (* Dune runs tests under _build; the seed sources live at the repo
     root. Walk up until libs/std/lib.pp exists, then point Loader at
     it the same way the image does (PP_LIBS_DIR). *)
  let rec up n dir =
    if n > 8 then failwith "seed_sources_test: libs/ not found from cwd"
    else
      let candidate = Filename.concat dir "libs" in
      if Sys.file_exists (Filename.concat candidate "std/lib.pp") then
        candidate
      else up (n + 1) (Filename.dirname dir)
  in
  let dir = up 0 (Sys.getcwd ()) in
  Unix.putenv "PP_LIBS_DIR" dir;
  dir

(* Seedable package names, mirroring Session.discover_seed_packages'
   filters: skip dot/underscore dirs and theme-* alternate skins. *)
let seedable_packages (root : string) : string list =
  (try Array.to_list (Sys.readdir root) with _ -> [])
  |> List.filter (fun name ->
         name <> ""
         && name.[0] <> '_'
         && name.[0] <> '.'
         && not (String.starts_with ~prefix:"theme-" name))
  |> List.sort String.compare

(* Names bound by top-level (define name ...) / (define (f x) ...) /
   defmacro forms, parsed the way Loader.load_string does. *)
let top_level_binds (src : string) : string list =
  let open Pricklypear_lisp.Ast in
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

let load_package_src (root : string) (pkg : string) : string =
  match Pricklypear_image_lib.Loader.read_file (Pricklypear_image_lib.Loader.library_path pkg) with
  | Ok src -> src
  | Error e -> failwith ("seed_sources_test: " ^ e)

(* S1: no seedable package binds a kernel special-form name. *)
let test_no_kernel_form_defines () =
  let root = find_libs_dir () in
  let kernel = Pricklypear_lisp.Eval.kernel_special_names in
  let offenders =
    seedable_packages root
    |> List.map (fun pkg ->
           let binds = top_level_binds (load_package_src root pkg) in
           (pkg, List.filter (fun n -> List.mem n kernel) binds))
    |> List.filter (fun (_, bad) -> bad <> [])
  in
  match offenders with
  | [] -> ()
  | xs ->
      failwith
        ( Printf.sprintf
            "%d package(s) define kernel special forms (seed aborts): " (List.length xs)
        ^ String.concat "; "
            (List.map
               (fun (pkg, bad) -> pkg ^ ": " ^ String.concat ", " bad)
               xs) )

(* S2: std provides list/take (calendar/upcoming depends on it). *)
let test_std_defines_list_take () =
  let root = find_libs_dir () in
  let binds = top_level_binds (load_package_src root "std") in
  Alcotest.(check bool) "std binds list/take" true
    (List.mem "list/take" binds);
  Alcotest.(check bool) "std binds list/range" true
    (List.mem "list/range" binds)

(* S3: the deleted libs/list package must not come back. *)
let test_list_package_deleted () =
  let root = find_libs_dir () in
  Alcotest.(check bool) "libs/list does not exist" false
    (Sys.file_exists (Filename.concat root "list/lib.pp"))

let () =
  Alcotest.run "pricklypear-seed-sources"
    [ ( "seed-sources"
      , [ ("no-kernel-form-defines", `Quick, test_no_kernel_form_defines)
        ; ("std-defines-list-take", `Quick, test_std_defines_list_take)
        ; ("list-package-deleted", `Quick, test_list_package_deleted)
        ] ) ]
