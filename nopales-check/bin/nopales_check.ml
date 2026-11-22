(** nopales-check: static checking for a language with no compile phase.

    usage: nopales-check [--json] [--strict] [--effects] [--txn] [--baseline FILE] [PATHS…]

    Default path is libs/ (recursively, *.pp). Exit 0 clean, 1 any ERROR
    (or any WARN with --strict), 2 nothing readable. *)

let usage =
  "usage: nopales-check [--json] [--strict] [--effects] [--txn] [--baseline FILE] [PATHS...]\n\
   \n\
   Walks the Nopales corpus with the interpreter's own reader and reports:\n\
   \n\
   \  ERROR  reader error; arity mismatch on a known define; unbound\n\
   \         package-qualified name (contains '/')\n\
   \  WARN   unknown qualified callable; unbound bare name; perflint R1 AMBER\n\
   \n\
   Perflint R1 (DB prim inside a per-row loop) is folded in: RED is an ERROR,\n\
   AMBER a WARN. --no-perf disables that half. Bare unbound names are reported\n\
   (macros are expanded first); --no-unbound-bare suppresses them.\n\
   \n\
   --effects adds the capability pass: per-define effect classes (pure, db,\n\
   route, tx, egress, unknown) and the unresolved qualified names behind an\n\
   unknown. JSON gains an \"effects\" array; text mode prints one EFFECT line\n\
   per define plus a class histogram. --txn adds the advisory transactional\n\
   hints (multi-write or select-for-update outside with-transaction).\n\
   \n\
   --baseline FILE suppresses findings already in FILE (written by\n\
   --save-baseline FILE), so a repo can adopt the gate while it burns down the\n\
   existing list; identical findings are counted, so a new call site is new.\n\
   \n\
   Default path: libs/ (recursive, *.pp).\n\
   Exit 0 clean, 1 any ERROR (or any WARN with --strict), 2 nothing readable.\n"

let () =
  let json = ref false and strict = ref false and perf = ref true
  and unbound_bare = ref true and effects = ref false and txn = ref false
  and baseline = ref "" and save_baseline = ref "" and paths = ref [] in
  let rec parse = function
    | [] -> ()
    | "--json" :: rest -> json := true; parse rest
    | "--strict" :: rest -> strict := true; parse rest
    | "--no-perf" :: rest -> perf := false; parse rest
    | "--unbound-bare" :: rest -> unbound_bare := true; parse rest
    | "--no-unbound-bare" :: rest -> unbound_bare := false; parse rest
    | "--effects" :: rest -> effects := true; parse rest
    | "--txn" :: rest -> txn := true; parse rest
    | "--baseline" :: v :: rest -> baseline := v; parse rest
    | "--save-baseline" :: v :: rest -> save_baseline := v; parse rest
    | (("--baseline" | "--save-baseline") as opt) :: [] ->
        Printf.eprintf "option %s needs a file\n\n%s" opt usage;
        exit 2
    | ("--help" | "-h") :: _ -> print_string usage; exit 0
    | a :: rest ->
        if String.length a > 0 && a.[0] = '-' then begin
          Printf.eprintf "unknown option %s\n\n%s" a usage;
          exit 2
        end;
        paths := a :: !paths;
        parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  if !baseline <> "" && !save_baseline <> "" then begin
    Printf.eprintf "choose --baseline (gate) or --save-baseline (record), not both\n";
    exit 2
  end;
  let paths = List.rev !paths in
  try
    let r = Pp_nopales_check.Check.run ~json:!json ~strict:!strict ~perf:!perf
              ~unbound_bare:!unbound_bare ~effects:!effects ~txn:!txn
              ~baseline:!baseline ~save_baseline:!save_baseline paths in
    let code = Pp_nopales_check.Check.exit_code ~strict:!strict r in
    (* recording a baseline is maintenance, not a gate: the findings it just
       recorded must not fail the command that recorded them. *)
    exit (if !save_baseline <> "" && code = 1 then 0 else code)
  with
  | Sys_error msg | Failure msg ->
      Printf.eprintf "nopales-check: %s\n" msg;
      exit 2
  | Yojson.Json_error msg ->
      Printf.eprintf "nopales-check: baseline is not valid JSON: %s\n" msg;
      exit 2
