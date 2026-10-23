(** nopales/bin/main.ml — the standalone Nopales CLI.

    Usage:
      nopales script.pp [args…]
      nopales -e '(println (+ 1 2))'
      nopales            # REPL on a tty, script on piped stdin
      nopales - < script.pp

    Pure interpreter + pure prim catalog + file/argv/exit/print/load.
    No Postgres, no image, no network. See nopales/lib/host.ml. *)

open Pricklypear_lisp

let version = "nopales 0.1.0"

type mode =
  | File of string * string list
  | Eval of string * string list
  | Repl of string list

type parsed = {
  mode : mode option;
  std : string option;
  no_std : bool;
  quiet : bool;
  lens : string option;
  lens_owner : string option;
}

let empty =
  {
    mode = None;
    std = None;
    no_std = false;
    quiet = false;
    lens = None;
    lens_owner = None;
  }

let usage =
  Printf.sprintf
    "%s\n\
     \n\
     Usage:\n\
     \  nopales [options] FILE [ARGS…]\n\
     \  nopales [options] -e EXPR [ARGS…]\n\
     \  nopales [options]            REPL on a tty, script on piped stdin\n\
     \  nopales [options] -          script from stdin\n\
     \n\
     Options:\n\
     \  -e, --eval EXPR   evaluate EXPR and print the last value\n\
     \  -i, --repl        force the interactive REPL\n\
     \  -q, --quiet       with -e, do not print the result\n\
     \      --std PATH    stdlib seed (default: NOPALES_STD, installed\n\
     \                    share/pricklypear-nopales/std.pp, or libs/std/lib.pp)\n\
     \      --no-std      do not load the stdlib seed\n\
     \      --lens DIR    local lens root: offline (get), (tree/get),\n\
     \                    and (rows) over cloned data (default: NOPALES_LENS\n\
     \                    or PP_LENS)\n\
     \      --lens-owner NAME  owner namespace when it cannot be inferred\n\
     \                    from the lens's data/u/<label> directory\n\
     \  -h, --help        this help\n\
     \      --version     version\n\
     \n\
     The stdlib seed supplies let*/when/unless (defmacros in libs/std).\n\
     DB, identity, route, and branch prims are intentionally absent."
    version

let die msg =
  prerr_endline ("nopales: " ^ msg);
  prerr_endline "try: nopales --help";
  exit 2

let parse (args : string list) : parsed =
  let rec go (p : parsed) = function
    | [] -> p
    | ("-h" | "--help") :: _ ->
        print_endline usage;
        exit 0
    | "--version" :: _ ->
        print_endline version;
        exit 0
    | ("-q" | "--quiet") :: rest -> go { p with quiet = true } rest
    | "--no-std" :: rest ->
        if p.std <> None then die "--std and --no-std are mutually exclusive";
        go { p with no_std = true } rest
    | "--std" :: path :: rest ->
        if p.no_std then die "--std and --no-std are mutually exclusive";
        go { p with std = Some path } rest
    | "--std" :: [] -> die "--std needs a path"
    | "--lens" :: dir :: rest -> go { p with lens = Some dir } rest
    | "--lens" :: [] -> die "--lens needs a directory"
    | "--lens-owner" :: name :: rest ->
        go { p with lens_owner = Some name } rest
    | "--lens-owner" :: [] -> die "--lens-owner needs a name"
    | ("-e" | "--eval") :: expr :: rest ->
        if p.mode <> None then die "only one mode may be given";
        go { p with mode = Some (Eval (expr, rest)) } []
    | ("-e" | "--eval") :: [] -> die "--eval needs an expression"
    | ("-i" | "--repl") :: rest ->
        if p.mode <> None then die "only one mode may be given";
        go { p with mode = Some (Repl rest) } []
    | "--" :: rest -> (
        match rest with
        | [] -> p
        | file :: argv ->
            if p.mode <> None then die "only one mode may be given";
            go { p with mode = Some (File (file, argv)) } [])
    | "-" :: rest ->
        if p.mode <> None then die "only one mode may be given";
        go { p with mode = Some (File ("-", rest)) } []
    | opt :: _ when String.length opt > 1 && opt.[0] = '-' ->
        die ("unknown option " ^ opt)
    | file :: argv ->
        if p.mode <> None then die "only one mode may be given";
        go { p with mode = Some (File (file, argv)) } []
  in
  go empty args

(** Walk up from [dir] until [rel] exists. *)
let rec walk_up dir rel =
  let candidate = Filename.concat dir rel in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else walk_up parent rel

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let read_stdin () =
  let b = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string b (input_line stdin);
       Buffer.add_char b '\n'
     done
   with End_of_file -> ());
  Buffer.contents b

(** Resolve the stdlib seed. Explicit intent ([--std] / [NOPALES_STD])
    must exist; auto-discovery falls back to installed share, then the
    source tree ([libs/std/lib.pp]). *)
let find_std (p : parsed) : string option =
  let explicit =
    match p.std with
    | Some s -> Some ("--std", s)
    | None -> (
        match Sys.getenv_opt "NOPALES_STD" with
        | Some s when String.trim s <> "" -> Some ("NOPALES_STD", s)
        | _ -> None)
  in
  match explicit with
  | Some (origin, path) ->
      if Sys.file_exists path then Some path
      else die (Printf.sprintf "%s points at a missing file: %s" origin path)
  | None ->
      let exe_dir = Filename.dirname (absolute Sys.executable_name) in
      let candidates =
        [ Filename.concat exe_dir "../share/pricklypear-nopales/std.pp";
          Filename.concat exe_dir "../share/nopales/std.pp" ]
        @ List.filter_map
            (fun base -> walk_up base "libs/std/lib.pp")
            [ Sys.getcwd (); exe_dir ]
      in
      List.find_opt Sys.file_exists candidates

let repl_value = function
  | Ast.VNil | Ast.VLambda _ | Ast.VMacro _ -> false
  | _ -> true

(** Lens root precedence: [--lens], NOPALES_LENS, PP_LENS. Explicit
    intent must exist; [--lens-owner] without a lens is a usage error. *)
let find_lens (p : parsed) : string option =
  let explicit =
    match p.lens with
    | Some d -> Some ("--lens", d)
    | None -> (
        match Sys.getenv_opt "NOPALES_LENS" with
        | Some d when String.trim d <> "" -> Some ("NOPALES_LENS", d)
        | _ -> (
            match Sys.getenv_opt "PP_LENS" with
            | Some d when String.trim d <> "" -> Some ("PP_LENS", d)
            | _ -> None))
  in
  match explicit with
  | Some (origin, dir) ->
      if Sys.file_exists dir then Some dir
      else
        die
          (Printf.sprintf "%s points at a missing lens root: %s" origin
             dir)
  | None ->
      if p.lens_owner <> None then die "--lens-owner requires --lens";
      None

let repl (h : Host.t) : int =
  print_endline (version ^ " — Ctrl-D to quit");
  let buf = Buffer.create 256 in
  let rec loop () =
    let prompt =
      if Buffer.length buf = 0 then "nopales> " else "       ... "
    in
    print_string prompt;
    flush stdout;
    match input_line stdin with
    | exception End_of_file ->
        print_newline ();
        0
    | line ->
        if Buffer.length buf = 0 && (line = ":quit" || line = ":q") then 0
        else begin
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          if Reader.is_complete (Buffer.contents buf) then begin
            let src = Buffer.contents buf in
            Buffer.clear buf;
            (match Host.eval_string h src with
             | Ok v -> if repl_value v then print_endline (Reader.pp_value v)
             | Error f -> prerr_endline (Host.format_failure f))
          end;
          loop ()
        end
  in
  loop ()

let () =
  let opts = parse (List.tl (Array.to_list Sys.argv)) in
  let mode =
    match opts.mode with
    | Some m -> m
    | None ->
        if Unix.isatty Unix.stdin then Repl []
        else File ("-", [])
  in
  (* argv is the ARGS after FILE / EXPR; the binary and script are not
     included (same convention as most script runtimes). *)
  let argv =
    match mode with
    | File (_, args) | Eval (_, args) | Repl args -> args
  in
  let lens = find_lens opts in
  let h =
    Host.create ~argv ?lens_root:lens ?lens_owner:opts.lens_owner ()
  in
  (if not opts.no_std then
     match find_std opts with
     | Some path -> (
         match Host.load_std h path with
         | Ok () -> ()
         | Error f ->
             prerr_endline (Host.format_failure f);
             exit 1)
     | None ->
         prerr_endline
           "nopales: no std library found; let*/when/unless are unavailable \
            (pass --std PATH or set NOPALES_STD)");
  let finish (r : (Ast.value, Host.failure) result) ~print_result : int =
    match r with
    | Ok v ->
        if print_result && not opts.quiet && repl_value v then
          print_endline (Reader.pp_value v);
        0
    | Error f ->
        prerr_endline (Host.format_failure f);
        1
  in
  let code =
    try
      match mode with
      | File ("-", _) ->
          let src = read_stdin () in
          finish (Host.eval_string h src) ~print_result:false
      | File (path, _) -> finish (Host.load_file h path) ~print_result:false
      | Eval (expr, _) -> finish (Host.eval_string h expr) ~print_result:true
      | Repl _ -> repl h
    with Host.Exit n -> n
  in
  exit code
