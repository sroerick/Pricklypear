(** Plugin loading (borge/glochid.borg plugin-model).

    Global plugins from ~/.config/agent/plugins/*.pp, then project
    plugins from ./.agent/plugins/*.pp (filename order). Each file's
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
  match Sys.getenv_opt "HOME" with
  | Some h -> Filename.concat h ".config/agent/plugins"
  | None -> ".config/agent/plugins"

let project_dir () = ".agent/plugins"

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

(** Load all plugins; print a one-line report per file to stderr. *)
let load_all (rt : Runtime.t) : unit =
  let dirs = [ global_dir (); project_dir () ] in
  List.iter
    (fun dir ->
      List.iter
        (fun path ->
          match load_file rt path with
          | Ok () -> Printf.eprintf "[plugins] %s ✓\n%!" path
          | Error e -> Printf.eprintf "[plugins] %s ✗ %s\n%!" path e)
        (pp_files_of dir))
    dirs
