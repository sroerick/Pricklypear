(** Spec-tree migration guard (card 4eacdf4c).

    Project specs live in the authoritative borge tree: one chapter file per
    project under borge/<name>.borg, inlined from pricklypear.borg. These
    static checks (no DB) pin the migration so a spec cannot quietly drift
    back into specs/ or keep pointing code at a retired location:

    S1. One borge chapter per migrated project spec.
    S2. The chapter carries the spec text: the original sections and
        acceptance headings survive the move.
    S3. The old specs/ location holds no spec file — at most a signpost
        stub that names the borge path.
    S4. Inline registration: pricklypear.borg inlines every migrated chapter.
    S5. No code or living doc points at the retired specs/ path; the borge
        path is the one named.

    The needles are built by concatenation so this file never matches its
    own S5 search. borge/ and specs/ are excluded from that walk: a chapter
    may legitimately record where its text came from. *)

(* Migrated spec name (basename without extension) + a heading that must
   survive the move (the section/acceptance text is preserved verbatim). *)
let migrated =
  [
    ("authz-matrix", "## 4. Consolidation and gaps");
    ("row-type-public-visibility", "## Required acceptance for the mechanism");
    ("session-freshness", "## Acceptance");
    ("workout-ksf-cjc", "## Tests — the first render-assert in the house");
  ]

let rec repo_root n dir =
  if n > 8 then failwith "spec_tree_test: repo root not found from cwd"
  else if Sys.file_exists (Filename.concat dir "pricklypear.borg") then dir
  else repo_root (n + 1) (Filename.dirname dir)

let root = lazy (repo_root 0 (Sys.getcwd ()))

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let exists path = Sys.file_exists path

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

let old_path name = "specs/" ^ name ^ ".md"
let borge_path name = "borge/" ^ name ^ ".borg"

(* S1: every migrated spec has its borge chapter. *)
let test_chapters_exist () =
  let dir = Lazy.force root in
  List.iter
    (fun (name, _) ->
      Alcotest.(check bool)
        (borge_path name ^ " exists") true
        (exists (Filename.concat dir (borge_path name))))
    migrated

(* S2: the chapter carries the original spec text, headings included. *)
let test_chapters_carry_spec_text () =
  let dir = Lazy.force root in
  List.iter
    (fun (name, heading) ->
      let path = Filename.concat dir (borge_path name) in
      let body = if exists path then read_file path else "" in
      Alcotest.(check bool)
        (borge_path name ^ " is non-trivial") true
        (String.length body > 1000);
      Alcotest.(check bool)
        (Printf.sprintf "%s keeps %S" (borge_path name) heading)
        true (contains body heading);
      Alcotest.(check bool)
        (borge_path name ^ " is a project form") true
        (String.starts_with ~prefix:"(project " body))
    migrated

(* S3: the old location holds no spec file; a stub may only signpost. *)
let test_old_location_is_not_a_spec () =
  let dir = Lazy.force root in
  List.iter
    (fun (name, _) ->
      let path = Filename.concat dir (old_path name) in
      if exists path then
        let body = read_file path in
        Alcotest.(check bool)
          (Printf.sprintf "%s is a signpost, not a spec" (old_path name))
          true
          (contains body "Moved:" && contains body (borge_path name)))
    migrated

(* S4: pricklypear.borg inlines every migrated chapter. *)
let test_root_inlines_chapters () =
  let dir = Lazy.force root in
  let book = read_file (Filename.concat dir "pricklypear.borg") in
  List.iter
    (fun (name, _) ->
      Alcotest.(check bool)
        (Printf.sprintf "pricklypear.borg inlines %s" (borge_path name))
        true
        (contains book ("(inline " ^ borge_path name ^ ")")))
    migrated

(* S5: code and living docs name the borge path, never the retired one. *)
let text_exts = [ ".ml"; ".md"; ".pp"; ".sh"; ".py" ]

let rec walk dir acc =
  let entries = try Array.to_list (Sys.readdir dir) with _ -> [] in
  List.fold_left
    (fun acc e ->
      if e = "" || e.[0] = '.' || e = "_build" then acc
      else
        let p = Filename.concat dir e in
        if Sys.is_directory p then (if Sys.file_exists p then walk p acc else acc)
        else if List.mem (Filename.extension p) text_exts then p :: acc
        else acc)
    acc entries

let test_no_stale_spec_paths () =
  let dir = Lazy.force root in
  let files =
    List.concat_map
      (fun d ->
        let p = Filename.concat dir d in
        if exists p then walk p [] else [])
      [ "image"; "libs"; "docs"; "scripts"; "common"; "remote"; "cli" ]
  in
  List.iter
    (fun (name, _) ->
      let needle = old_path name in
      let offenders =
        List.filter
          (fun f -> try contains (read_file f) needle with _ -> false)
          files
      in
      match offenders with
      | [] -> ()
      | xs ->
          failwith
            (Printf.sprintf
               "%d file(s) still point at the retired %s (name the borge \
                tree instead): %s"
               (List.length xs) needle
               (String.concat ", "
                  (List.map (fun f -> Filename.basename (Filename.dirname f) ^ "/" ^ Filename.basename f) xs))))
    migrated

let () =
  Alcotest.run "spec-tree"
    [
      ( "chapters",
        [
          Alcotest.test_case "S1 chapters exist" `Quick test_chapters_exist;
          Alcotest.test_case "S2 spec text preserved" `Quick
            test_chapters_carry_spec_text;
          Alcotest.test_case "S4 root inlines chapters" `Quick
            test_root_inlines_chapters;
        ] );
      ( "single-copy",
        [
          Alcotest.test_case "S3 old location is not a spec" `Quick
            test_old_location_is_not_a_spec;
          Alcotest.test_case "S5 no stale specs/ paths" `Quick
            test_no_stale_spec_paths;
        ] );
    ]
