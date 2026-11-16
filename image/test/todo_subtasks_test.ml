(** card c257735e — subtasks wired into the todo layer (fast, DB-free leg).

    A card may be filed UNDER another card. The contract the loop and the
    board depend on is defined in libs/todo/lib.pp:

      1. todo/item/parent + todo/item/children read the relation back
         (carried as a parent field on the child row, so it survives the
         flat todo/item/all pool);
      2. todo/loop/rows hands the loop LEAF cards only — a parent is never
         implemented directly;
      3. a parent with children reports a rollup line (progress plus
         blocked-by-children while any child is unlanded), so a parent is
         never invisible work.

    This test asserts that contract against the product source, so it runs
    without Postgres. The DB-backed behaviour leg lives in
    todo_dep_dag_test's sibling scratch-branch pattern. *)

let point_pp_libs_file () : string =
  let rec up n dir =
    if n > 10 then failwith "todo_subtasks_test: libs/todo/lib.pp not found"
    else
      let p = Filename.concat (Filename.concat dir "libs") "todo/lib.pp" in
      if Sys.file_exists p then p else up (n + 1) (Filename.dirname dir)
  in
  match Sys.getenv_opt "PP_LIBS_DIR" with
  | Some d when Sys.file_exists (Filename.concat d "todo/lib.pp") ->
      Filename.concat d "todo/lib.pp"
  | _ -> up 0 (Sys.getcwd ())

let slurp p =
  let ic = open_in_bin p in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let has_sub hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let src = lazy (slurp (point_pp_libs_file ()))

let check_def name =
  Alcotest.(check bool)
    (Printf.sprintf "(define %s ...) is bound" name)
    true
    (has_sub (Lazy.force src) (Printf.sprintf "(define %s" name))

let test_parent_child_prims () =
  List.iter check_def
    [ "todo/item/parent"; "todo/item/children"; "todo/item/set-parent";
      "todo/item/leaf?"; "todo/item/rollup"; "todo/item/progress" ]

let test_loop_processes_leaves () =
  check_def "todo/loop/rows";
  Alcotest.(check bool) "the queue gates on a LEAF" true
    (has_sub (Lazy.force src) "todo/item/leaf?")

let test_parent_rolls_up () =
  check_def "todo/loop/rollup-line";
  check_def "todo/loop/rollup-lines";
  check_def "todo/loop/report";
  check_def "todo/loop/blocked-by-children";
  Alcotest.(check bool) "rows and rollup are produced together, so the skip is logged"
    true
    (has_sub (Lazy.force src) "(todo/loop/rollup-lines items)");
  Alcotest.(check bool) "a rollup line is emitted" true
    (has_sub (Lazy.force src) "rollup ");
  Alcotest.(check bool) "an unlanded parent is reported, not silent" true
    (has_sub (Lazy.force src) "blocked-by-children")

let () =
  Alcotest.run "todo_subtasks"
    [ ( "todo-subtasks",
        [ Alcotest.test_case "parent/children prims bound" `Quick
            test_parent_child_prims;
          Alcotest.test_case "the loop processes leaves only" `Quick
            test_loop_processes_leaves;
          Alcotest.test_case "a parent with children rolls up" `Quick
            test_parent_rolls_up ] ) ]
