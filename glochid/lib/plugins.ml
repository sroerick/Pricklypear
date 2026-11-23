(** Plugin loading (borge/glochid.borg plugin-model).

    Global plugins from ~/.config/glochid/plugins/*.pp, then project
    plugins from ./.glochid/plugins/*.pp (filename order; legacy
    ~/.config/agent + ./.agent plugin dirs stay readable as read-only
    fallbacks while the glochid dirs are absent). Each file's
    forms are evaluated in order into the one shared runtime env;
    errors are contained and reported, loading continues. *)

open Pricklypear_lisp

let pp_files_of dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".pp")
    |> List.sort String.compare
    |> List.map (Filename.concat dir)

let global_dir () =
  (* Glochid plugin dirs win when they exist; the legacy agent dirs
     remain READ-ONLY fallbacks (never created by glochid). *)
  let dir app =
    match Sys.getenv_opt "HOME" with
    | Some h ->
        Filename.concat (Filename.concat h ".config")
          (Filename.concat app "plugins")
    | None -> Filename.concat ".config" (Filename.concat app "plugins")
  in
  let glochid = dir "glochid" in
  if Sys.file_exists glochid && Sys.is_directory glochid then glochid
  else dir "agent"

let project_dir () =
  if Sys.file_exists ".glochid/plugins" && Sys.is_directory ".glochid/plugins"
  then ".glochid/plugins"
  else if Sys.file_exists ".agent/plugins" && Sys.is_directory ".agent/plugins"
  then ".agent/plugins"
  else ".glochid/plugins"

(* ---- project trust (borge/glochid.borg plugin-model SAFETY, U1) ----
   Global plugins are the operator's own and load unconditionally.
   PROJECT plugins are skipped unless the operator has explicitly trusted
   this canonical project path. Trust is path-keyed (not content-keyed):
   inspectable, and the documented limitation is that re-cloning at the
   same path inherits the decision. --no-plugins disables every dir. *)
let trust_file () : string =
  Filename.concat (Config.config_home ()) "trusted-projects.txt"

let project_root () : string =
  try Unix.realpath (Sys.getcwd ()) with _ -> Sys.getcwd ()

let trusted_projects () : string list =
  try
    let ic = open_in_bin (trust_file ()) in
    let s = In_channel.input_all ic in
    close_in ic;
    String.split_on_char '\n' s
    |> List.map String.trim
    |> List.filter (fun l -> l <> "")
  with Sys_error _ -> []

let is_trusted () : bool = List.mem (project_root ()) (trusted_projects ())

(** Record the canonical cwd as trusted and return it. Idempotent. *)
let trust_project () : string =
  let root = project_root () in
  if not (is_trusted ()) then begin
    Config.mkdir_p (Filename.dirname (trust_file ()));
    let oc =
      open_out_gen [ Open_append; Open_creat ] 0o600 (trust_file ())
    in
    output_string oc (root ^ "\n");
    close_out oc
  end;
  root

let slurp path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Evaluate the toplevel forms of [src] one by one in [rt]'s env. *)
let eval_src (rt : Runtime.t) (src : string) : unit =
  let forms = Reader.read_all src in
  List.iter
    (fun form ->
      let rendered = Reader.pp_expr form in
      (* go through Runtime.eval_string so the rim + mutex apply *)
      ignore (Runtime.eval_string ~rt rendered))
    forms

let load_file (rt : Runtime.t) (path : string) : (unit, string) Result.t =
  try
    let src = slurp path in
    eval_src rt src;
    Ok ()
  with
  | Sys_error e -> Error e
  | Reader.Reader_error (msg, ln, col) ->
      Error (Printf.sprintf "read error %d:%d: %s" ln col msg)
  | Ast.Interp_error ie -> Error (Ast.pp_interp_error ie)

(** Load a directory's plugins; print a one-line report per file to stderr. *)
let load_dir (rt : Runtime.t) (dir : string) : unit =
  List.iter
    (fun path ->
      match load_file rt path with
      | Ok () -> Printf.eprintf "[plugins] %s ✓\n%!" path
      | Error e -> Printf.eprintf "[plugins] %s ✗ %s\n%!" path e)
    (pp_files_of dir)

(** Load all plugins. GLOBAL plugins always load; PROJECT plugins load only
    when the canonical project is trusted (see [trust_project]). *)
let load_all (rt : Runtime.t) : unit =
  load_dir rt (global_dir ());
  let pdir = project_dir () in
  let pfiles = pp_files_of pdir in
  if pfiles = [] then ()
  else if is_trusted () then load_dir rt pdir
  else
    Printf.eprintf
      "[plugins] skipped %d project plugin(s): untrusted project %s\n\
       [plugins] trust this project explicitly with: glochid --trust-project\n%!"
      (List.length pfiles) (project_root ())
