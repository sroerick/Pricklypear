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
        calendar/upcoming calls — it must exist on every clean env.
    S3. The deleted libs/list package must not come back.
    S4. Exactly ONE json escaper: std defines json-escape; no package
        binds any other *json-esc*/*json-escape* name (the four
        diverged per-package copies are deleted — i-1788670711).
    S5. No *escape-xml* bind: edit/escape-xml was deleted for the
        kernel html-escape prim (i-1788670711).
    S6. Behavioral: the SHIPPED std json-escape define, evaluated
        against the image prim table, escapes the full set (backslash
        quote newline CR tab) and its output parses as valid JSON and
        round-trips — the book/json-esc newline corruption class.
    S7. No bare top-level forms (i-1788685367, bi/wiki class): an
        UNESCAPED inner double quote inside a top-level docstring string
        closes the string early and leaks the rest as bare top-level
        forms (bi: a Var `published`; wiki: a Var `{}`) that the seed
        eval refuses with unbound-var, aborting the package mid-file.
        Every top-level form must be a define/defmacro/route/deftype/
        package/export/public-route call or a docstring string literal
        (the established libs convention: docstrings follow the define
        as top-level strings). A reader-level test — parse errors and
        corrupted shapes cannot hide behind a swallowed seed failure.
    S8. No define-time calls to file-local bindings (i-1788685367, rb
        class): under the additive boot-seed skip (session.ml seed_skip)
        a define whose name already exists on the branch is NOT
        evaluated and NOT bound into the seed env, so a NEW value define
        whose body calls that sibling raises unbound at boot (rb's
        (define rb/plan-config (plan 4 …)) on warm DBs; every rb recipe
        define likewise). Value defines must be data-only (quote/lambdas
       /kernel prims fine); DSL data that needs sibling helpers ships as
        a zero-arg function the consumers call. *)

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

(* Binds whose name contains "json-esc" (any package prefix). *)
let json_escaper_binds binds =
  List.filter
    (fun n ->
      let rec go i =
        i + 8 <= String.length n
        && (String.sub n i 8 = "json-esc" || go (i + 1))
      in
      go 0)
    binds

(* S4: ONE json escaper — std's json-escape; per-package copies gone. *)
let test_one_json_escaper () =
  let root = find_libs_dir () in
  let offenders =
    seedable_packages root
    |> List.map (fun pkg ->
           (pkg, json_escaper_binds (top_level_binds (load_package_src root pkg))))
    |> List.filter (fun (_, bad) -> bad <> [])
  in
  match offenders with
  | [ ("std", [ "json-escape" ]) ] -> ()
  | xs ->
      failwith
        ( Printf.sprintf
            "expected exactly one json escaper (std: json-escape), got: "
        ^ String.concat "; "
            (List.map
               (fun (pkg, bad) -> pkg ^ ": " ^ String.concat ", " bad)
               xs) )

(* S5: escape-xml is gone — kernel html-escape is the one XML/HTML
   escaper. *)
let test_no_escape_xml () =
  let root = find_libs_dir () in
  let offenders =
    seedable_packages root
    |> List.map (fun pkg ->
           let binds = top_level_binds (load_package_src root pkg) in
           (pkg, List.filter (fun n -> String.ends_with ~suffix:"escape-xml" n) binds))
    |> List.filter (fun (_, bad) -> bad <> [])
  in
  match offenders with
  | [] -> ()
  | xs ->
      failwith
        ( "escape-xml defines must use kernel html-escape instead: "
        ^ String.concat "; "
            (List.map
               (fun (pkg, bad) -> pkg ^ ": " ^ String.concat ", " bad)
               xs) )

(* --- S6: behavioral, still no DB ---

   Eval the SHIPPED std json-escape define (and book's array builder,
   which now calls it) against the image prim table
   [Primitives.make_with_defaults] — proving the full escape set and
   that produced JSON parses and round-trips. *)

(* Top-level (define <name> ...) form named [name], from stripped src. *)
let find_define src name =
  let forms =
    Pricklypear_lisp.Reader.read_all
      (Pricklypear_image_lib.Loader.strip_line_comments src)
  in
  let open Pricklypear_lisp.Ast in
  match
    List.find_opt
      (fun form ->
        match form with
        | App (Var "define", Var n :: _) -> n = name
        | _ -> false)
      forms
  with
  | Some d -> d
  | None -> failwith ("seed_sources_test: no (define " ^ name ^ " …) found")

(* Env with the shipped std json-escape (and, optionally, book's
   json-array helpers) evaluated into it. *)
let json_escape_env ?(with_book_array = false) () =
  let root = find_libs_dir () in
  let prims = Pricklypear_image_lib.Primitives.make_with_defaults () in
  let env = Pricklypear_lisp.Env.create () in
  (* Auto-bind prims into the env as VPrim names — same as the image's
     session attach loop (session.ml); eval dispatches VPrim via the
     prim table. *)
  Hashtbl.iter
    (fun name _ ->
      Pricklypear_lisp.Env.define env name
        (Pricklypear_lisp.Ast.VPrim name))
    prims;
  let budget = Pricklypear_lisp.Eval.make_budget () in
  (* Direct-style eval runs under the sync rim (lisp/test eval_test
     precedent): Prim_call effects are serviced from the prim table. *)
  let run_expr expr =
    Pricklypear_lisp.Control.run_sync ~prims (fun () ->
        Pricklypear_lisp.Eval.eval ~budget ~prims env expr)
  in
  run_expr (find_define (load_package_src root "std") "json-escape")
    |> ignore;
  if with_book_array then begin
    (* book/json-array-join calls std json-escape at runtime; both
       live in one env like they do on the seeded branch. *)
    run_expr (find_define (load_package_src root "book") "book/json-array-join")
    |> ignore;
    run_expr (find_define (load_package_src root "book") "book/json-array")
    |> ignore
  end;
  (env, budget, prims, run_expr)

let eval_call run_expr fn args =
  let open Pricklypear_lisp.Ast in
  run_expr (App (Var fn, List.map (fun a -> Lit a) args))

let json_escape_str s =
  let env, _, _, run_expr = json_escape_env () in
  let open Pricklypear_lisp.Ast in
  match eval_call run_expr "json-escape" [ VStr s ] with
  | VStr out -> out
  | v -> failwith ("json-escape returned non-string: " ^ Pricklypear_lisp.Reader.pp_value v)
  | exception Pricklypear_lisp.Ast.Interp_error e ->
      failwith ("json-escape call failed: " ^ Pricklypear_lisp.Ast.pp_interp_error e)

let test_json_escape_full_set () =
  (* full set: backslash quote newline CR tab — all escaped *)
  Alcotest.(check string) "full set"
    "a\\\"b\\\\c\\nd\\re\\tf"
    (json_escape_str "a\"b\\c\nd\re\tf");
  (* backslash FIRST: an escaped quote (backslash before quote) must
     not double-escape the inserted backslash — output is exactly the
     JSON encoding of the input (\\ → \\\\, then quote): *)
  Alcotest.(check string) "backslash before quote"
    "\\\\\\\"" (json_escape_str "\\\"");
  (* plain text passes through *)
  Alcotest.(check string) "plain" "hello" (json_escape_str "hello");
  (* coercion: numbers coerce like the old per-package (as-str …) sites *)
  let _, _, _, run_expr = json_escape_env () in
  let open Pricklypear_lisp.Ast in
  (match eval_call run_expr "json-escape" [ VInt 42L ] with
   | VStr "42" -> ()
   | v -> failwith ("json-escape 42 -> " ^ Pricklypear_lisp.Reader.pp_value v))

(* The reported corruption: book/json-array stored on section rows must
   be VALID JSON even when values contain newlines/quotes/backslashes. *)
let test_book_json_array_roundtrip () =
  let _, _, _, run_expr = json_escape_env ~with_book_array:true () in
  let a = "resolved\nline \"two\"" in
  let b = "back\\slash\ttab" in
  let open Pricklypear_lisp.Ast in
  match
    eval_call run_expr "book/json-array" [ VList [ VStr a; VStr b ] ]
  with
  | VStr arr -> (
      match Yojson.Safe.from_string arr with
      | `List [ `String ga; `String gb ] ->
          Alcotest.(check string) "elem 0 round-trip" a ga;
          Alcotest.(check string) "elem 1 round-trip" b gb
      | j -> failwith ("unexpected JSON: " ^ Yojson.Safe.to_string j)
      | exception e ->
          failwith ("book/json-array produced invalid JSON: " ^ arr ^ " (" ^ Printexc.to_string e ^ ")"))
  | v -> failwith ("book/json-array returned non-string: " ^ Pricklypear_lisp.Reader.pp_value v)

(* --- S7/S8: the i-1788685367 seed-abort classes ---

   Both work over the real reader's top-level forms; neither needs a
   DB. They complement the runtime banner (which a swallowed seed
   failure turns into one stderr line) with a parse-level tripwire. *)

(* Heads a seedable lib.pp may call at top level, mirroring the
   boot-seed-skip safety scan in session.ml. Plus string literals:
   the libs docstring convention places the doc AFTER the define as a
   top-level string. *)
let top_level_allowed_heads =
  [ "define"; "defmacro"; "route"; "deftype"; "package"; "export"; "public-route" ]

(* S7: no bare top-level forms — an unescaped inner quote in a top-level
   docstring leaks Var forms (`published`, `{}`) that abort the seed. *)
let test_no_bare_top_level_forms () =
  let root = find_libs_dir () in
  let offenders =
    seedable_packages root
    |> List.map (fun pkg ->
           let src = load_package_src root pkg in
           let forms =
             Pricklypear_lisp.Reader.read_all
               (Pricklypear_image_lib.Loader.strip_line_comments src)
           in
           let bad =
             List.filter
               (fun form ->
                 match form with
                 | Pricklypear_lisp.Ast.App
                     (Pricklypear_lisp.Ast.Var head, _) ->
                     not (List.mem head top_level_allowed_heads)
                 | Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VStr _) ->
                     false (* docstring-after-define convention *)
                 | _ -> true)
               forms
           in
           (pkg, bad))
    |> List.filter (fun (_, bad) -> bad <> [])
  in
  match offenders with
  | [] -> ()
  | xs ->
      failwith
        ( Printf.sprintf
            "%d package(s) have bare top-level forms (docstring quote \
             corruption class — escape the inner quotes): "
            (List.length xs)
        ^ String.concat "; "
            (List.map
               (fun (pkg, bad) ->
                 pkg ^ ": "
                 ^ String.concat ", "
                     (List.map Pricklypear_lisp.Reader.pp_expr bad))
               xs) )

(* References a name inside [binds] found outside nested lambda/quote
   bodies — i.e. something a VALUE define would evaluate at define time.
   The reader produces plain App nodes for special forms (they are
   recognized at EVAL time, reader.ml header), so the call-time
   boundaries are App (Var "lambda") / (Var "quote") / (Var "delay")
   here, and let/letrec/if/seq are transparent (their bodies DO run at
   define time when the define is a value define). *)
let rec define_time_refs ~(binds : string list) ~(owner : string) (acc : string list)
    (expr : Pricklypear_lisp.Ast.expr) : string list =
  let open Pricklypear_lisp.Ast in
  let go acc e = define_time_refs ~binds ~owner acc e in
  match expr with
  | Var v when v <> owner && List.mem v binds -> v :: acc
  | App (Var ("lambda" | "quote" | "delay"), _) ->
      acc (* call-time / data / thunk body: not evaluated at define time *)
  | App (Var ("define" | "defmacro"), _) ->
      acc (* nested/local binding, not a top-level sibling *)
  | App (Var v, args) when v <> owner && List.mem v binds ->
      List.fold_left go (v :: acc) args
  | App (f, args) -> List.fold_left go acc (f :: args)
  | Lambda (_, body) -> acc
  | Quote _ -> acc
  | Lit _ -> acc
  | Let (_, e, body) -> go (go acc e) body
  | Letrec (bs, body) -> List.fold_left go (go acc body) (List.map snd bs)
  | If (c, t, e) -> go (go (go acc c) t) e
  | Prim (_, args) -> List.fold_left go acc args
  | Seq (a, b) -> go (go acc a) b
  | Define _ -> acc (* nested/local binding, not a top-level sibling *)
  | Var _ -> acc (* a file-local ref is handled by the guarded clause *)

(* S8: value defines may not call file-local siblings at define time —
   the additive boot-seed skip leaves an existing sibling unbound in the
   seed env, so a new define calling it raises unbound at every warm
   boot (rb/plan-config → plan). Function-style defines (define (f x) …)
   evaluate nothing at define time and are exempt. *)
let test_no_define_time_sibling_calls () =
  let root = find_libs_dir () in
  let offenders =
    seedable_packages root
    |> List.map (fun pkg ->
           let src = load_package_src root pkg in
           let forms =
             Pricklypear_lisp.Reader.read_all
               (Pricklypear_image_lib.Loader.strip_line_comments src)
           in
           let binds = top_level_binds src in
           let bad =
             List.filter_map
               (fun form ->
                 match form with
                 | Pricklypear_lisp.Ast.App
                     (Pricklypear_lisp.Ast.Var "define", target :: body)
                   when not (List.is_empty body) -> (
                     match target with
                     | Pricklypear_lisp.Ast.Var name ->
                         let refs =
                           List.fold_left
                             (define_time_refs ~binds ~owner:name)
                             [] body
                         in
                         if refs = [] then None else Some (name, refs)
                     | _ -> None (* (define (f x) …): body is call-time *) )
                 | _ -> None)
               forms
           in
           (pkg, bad))
    |> List.filter (fun (_, bad) -> bad <> [])
  in
  match offenders with
  | [] -> ()
  | xs ->
      failwith
        ( Printf.sprintf
            "%d package(s) define data by calling file-local bindings at \
             define time (unbound under the additive boot-seed skip on \
             warm DBs — make them zero-arg functions): "
            (List.length xs)
        ^ String.concat "; "
            (List.map
               (fun (pkg, bad) ->
                 pkg ^ ": "
                 ^ String.concat ", "
                     (List.map
                        (fun (name, refs) ->
                          name ^ " -> " ^ String.concat ", " refs)
                        bad))
               xs) )

let () =
  Alcotest.run "pricklypear-seed-sources"
    [ ( "seed-sources"
      , [ ("no-kernel-form-defines", `Quick, test_no_kernel_form_defines)
        ; ("std-defines-list-take", `Quick, test_std_defines_list_take)
        ; ("list-package-deleted", `Quick, test_list_package_deleted)
        ; ("one-json-escaper", `Quick, test_one_json_escaper)
        ; ("no-escape-xml", `Quick, test_no_escape_xml)
        ; ("json-escape-full-set", `Quick, test_json_escape_full_set)
        ; ("book-json-array-roundtrip", `Quick, test_book_json_array_roundtrip)
        ; ("no-bare-top-level-forms", `Quick, test_no_bare_top_level_forms)
        ; ("no-define-time-sibling-calls", `Quick, test_no_define_time_sibling_calls)
        ] ) ]
