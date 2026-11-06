(** pp-perf-lint: static R1 perf guard over libs/<pkg>/lib.pp.

    Exit codes: 0 clean, 1 RED findings, 2 unreadable/unparsable input. *)

let usage =
  "usage: pp-perf-lint [--json] [PATHS...]\n\
   \n\
   Walks libs/*/lib.pp (or the given files/directories) and reports DB prim\n\
   calls that sit inside a per-row loop lambda.\n\
   \n\
   \  RED    prim in the loop lambda body (one statement per row)\n\
   \  AMBER  prim in a helper the loop lambda calls\n\
   \n\
   Exit 0 clean, 1 RED found, 2 unreadable input.\n"

let () =
  let json = ref false in
  let paths = ref [] in
  let rec parse = function
    | [] -> ()
    | "--json" :: rest ->
        json := true;
        parse rest
    | ("--help" | "-h") :: _ ->
        print_string usage;
        exit 0
    | a :: rest ->
        paths := a :: !paths;
        parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  exit (Perf_lint.main ~json:!json ~paths:(List.rev !paths))
