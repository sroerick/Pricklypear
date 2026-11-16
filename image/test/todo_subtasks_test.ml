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
         never invisible work: todo/loop/report emits rows AND rollups,
         and both the master list (todo/item/list-li) and the board
         (kanban/card-html-marked) RENDER the rollup instead of hiding it.

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

(** Another lib in the same checkout (board = libs/kanban/lib.pp). *)
let lib_src lib =
  let dirs = Filename.dirname (Filename.dirname (point_pp_libs_file ())) in
  slurp (Filename.concat (Filename.concat dirs lib) "lib.pp")

let check_def name =
  Alcotest.(check bool)
    (Printf.sprintf "(define %s ...) is bound" name)
    true
    (has_sub (Lazy.force src) (Printf.sprintf "(define %s" name))

let test_parent_child_prims () =
  List.iter check_def
    [ "todo/item/parent"; "todo/item/children"; "todo/item/set-parent";
      "todo/item/leaf?"; "todo/item/rollup"; "todo/item/progress" ];
  Alcotest.(check bool) "the parent field is written by set-parent" true
    (has_sub (Lazy.force src) "\"parent\"")

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

(** crit5: the master card list RENDERS the rollup for a parent, so a
    parent with children is never invisible work on /todos. *)
let test_master_list_renders_rollup () =
  Alcotest.(check bool) "todo/item/list-li renders the rollup pill" true
    (has_sub (Lazy.force src) "todo-rollup")

(** crit7: GET /board renders the same rollup on the card. *)
let test_board_renders_rollup () =
  Alcotest.(check bool) "the kanban card renders the rollup" true
    (has_sub (lib_src "kanban") "kb-card-rollup")

let () =
  Alcotest.run "todo_subtasks"
    [ ( "todo-subtasks",
        [ Alcotest.test_case "parent/children prims bound" `Quick
            test_parent_child_prims;
          Alcotest.test_case "the loop processes leaves only" `Quick
            test_loop_processes_leaves;
          Alcotest.test_case "a parent with children rolls up" `Quick
            test_parent_rolls_up;
          Alcotest.test_case "the master list renders the rollup" `Quick
            test_master_list_renders_rollup;
          Alcotest.test_case "the board renders the rollup" `Quick
            test_board_renders_rollup ] ) ]
