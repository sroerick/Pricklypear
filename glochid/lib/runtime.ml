(** Agent runtime: the shared Nopales env + rim host.

    One runtime per process. All evaluation is serialized through
    [eval_mutex] (per borge/glochid.borg nopales-embed); host prims are
    synchronous and block the calling fiber under Eio.

    The host seam (request shape, route table + dispatch, template
    calls, event/tool registry, [Loop.host], boot stdlib macro) lives in
    hostkit/host_seam.ml — ONE home shared with pp-remote
    (i-1788671304-d). This module is the agent's [run_in_rim] (the eval
    mutex + local host-prim rim) plus the agent-only prims: fs, ui,
    register-command/list-commands, identity. The [Runtime.*] API is
    kept as delegating aliases so plugins/surface/bin/tests are
    unchanged. *)

open Pricklypear_lisp
open Ast

(** Tool/command registry entry — alias of the shared loop's record
    (llm/lib/loop.ml owns the definition now that agent + pp-remote
    share the loop). *)
type registry_entry = Ast.value Loop.registry_entry

type t = {
  seam : Host_seam.t;
  eval_mutex : Eio.Mutex.t;
  (** The agent-only command registry (register-command /
      list-commands). Tools + events live in the shared seam. *)
  commands : (string, registry_entry) Hashtbl.t;
}

(** Current working directory as seen by path prims. Immutable per
   process: agent never chdir-s. *)
let cwd = ref (Sys.getcwd ())

(** Parse a plugin-produced value back to host JSON. VStr is treated
    as raw JSON first (dict prims produce JSON strings), else a plain
    string. The codec lives in hostkit/host_seam.ml (the loop seam itself
    is Yojson-only); re-exported here for host callers. *)
let json_of_value = Host_seam.json_of_value

let str_arg = Host_seam.str_arg

let emit_error = Host_seam.emit_error

(** Register a host prim: dispatch table PLUS a VPrim binding in the
    shared env (Nopales resolves prims via env, like the image's
    auto-bind loop in session.ml). All registrations go through the
    shared seam. *)
let add_prim (t : t) (name : string) (fn : Ast.value list -> Ast.value)
    : unit =
  Host_seam.add_prim t.seam name fn

(** Path resolution: relative paths resolve under [cwd]; ".." escapes
    are refused (v0 sandbox posture: refuse, not normalize). Shared
    logic in Host_seam. *)
let resolve_path (p : string) : string =
  Host_seam.resolve_path ~cwd p

(** Serialize Nopales evals. Manual lock/unlock (not use_rw):
    Interp_error is a normal outcome of eval and must not poison the
    mutex for later evals. *)
let with_eval_lock (t : t) (f : unit -> 'a) : 'a =
  Eio.Mutex.lock t.eval_mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.eval_mutex) f

(** The agent rim: eval lock + the local host-prim rim. This is the
    ONE agent-specific seam decision (Host_seam.run_in_rim). *)
let run_in_rim (t : t) (f : unit -> 'a) : 'a =
  with_eval_lock t (fun () -> Control.run_with_rim ~prims:t.seam.prims f)

let eval_string ~(rt : t) (src : string) : Ast.value =
  rt.seam.run_in_rim (fun () ->
      Eval.eval_string_direct ~prims:rt.seam.prims rt.seam.env
        ~budget:(Eval.make_budget ()) src)

(** Evaluate all top-level forms in [src] into the shared env, under
    the eval lock + rim. Used to boot-load Nopales packages (libs/std,
    libs/html, libs/hyper) as SOURCE — the same env as plugins/REPL, so
    the surface is redefinable live (borge/glochid.borg surfaces). *)
let load_source ~(rt : t) (src : string) : unit =
  ignore
    (rt.seam.run_in_rim (fun () ->
         let v =
           try
             Eval.read_eval_all_direct ~prims:rt.seam.prims rt.seam.env src
           with
           | Ast.Interp_error ie ->
               Printf.eprintf "boot-load: %s\n%!" (Ast.pp_interp_error ie);
               raise (Ast.Interp_error ie)
           | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
               Printf.eprintf "boot-load: reader error line %d col %d: %s\n%!" l c m;
               raise (Pricklypear_lisp.Reader.Reader_error (m, l, c))
         in
         ignore v;
         Ast.VNil))

(** Load a .pp file from [path] into the shared env. *)
let load_file ~(rt : t) (path : string) : (unit, string) result =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    load_source ~rt s;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

(** Apply a Nopales function value to host-side value args under the
    eval lock + rim (Host_seam.apply_value). Callers inside an eval go
    through Host_seam.apply_value_raw (no re-lock). *)
let apply_value (t : t) (fn : Ast.value) (args : Ast.value list) : Ast.value =
  Host_seam.apply_value t.seam fn args

(* ---- Host prims (agent-only) ---- *)

let register_fs_prims (t : t) : unit =
  let read args =
    match args with
    | [ path ] -> (
        let p = resolve_path (str_arg "fs/read" path) in
        try
          let ic = open_in_bin p in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          VStr s
        with Sys_error e -> emit_error "fs/read" e)
    | _ -> emit_error "fs/read" "usage: (fs/read path)"
  in
  add_prim t "fs/read" read;
  let write args =
    match args with
    | [ path; content ] -> (
        let p = resolve_path (str_arg "fs/write" path) in
        let s = str_arg "fs/write" content in
        (* create the parent dir when missing (e.g. .agent/workspace.json
           in a project without .agent/ yet) *)
        (let dir = Filename.dirname p in
         if not (Sys.file_exists dir) then ignore (Unix.mkdir dir 0o755));
        try
          let oc = open_out_bin p in
          output_string oc s;
          close_out oc;
          VBool true
        with Sys_error e -> emit_error "fs/write" e)
    | _ -> emit_error "fs/write" "usage: (fs/write path content)"
  in
  add_prim t "fs/write" write;
  let exists args =
    match args with
    | [ path ] ->
        VBool (Sys.file_exists (resolve_path (str_arg "fs/exists?" path)))
    | _ -> emit_error "fs/exists?" "usage: (fs/exists? path)"
  in
  add_prim t "fs/exists?" exists;
  (* (fs/ls path) -> JSON array of {"name","dir"} sorted by name *)
  let ls args =
    match args with
    | [ path ] -> (
        let p = resolve_path (str_arg "fs/ls" path) in
        try
          let entries =
            Sys.readdir p |> Array.to_list
            |> List.map (fun e ->
                   let full = Filename.concat p e in
                   `Assoc
                     [
                       ("name", `String e);
                       ("dir", `Bool (try Sys.is_directory full with _ -> false));
                     ])
          in
          VStr (Yojson.Safe.to_string (`List (List.sort compare entries)))
        with Sys_error e -> emit_error "fs/ls" e)
    | _ -> emit_error "fs/ls" "usage: (fs/ls path)"
  in
  add_prim t "fs/ls" ls;
  (* (fs/dir? path) -> VBool. True if path resolves to a directory. *)
  let is_dir args =
    match args with
    | [ path ] ->
        let p = resolve_path (str_arg "fs/dir?" path) in
        VBool (try Sys.is_directory p with Sys_error _ -> false)
    | _ -> emit_error "fs/dir?" "usage: (fs/dir? path)"
  in
  add_prim t "fs/dir?" is_dir

let register_ui_prims (t : t) : unit =
  let notify args =
    match args with
    | [ msg ] ->
        Printf.printf "\027[2m[notify] %s\027[0m\n%!" (str_arg "ui/notify" msg);
        VNil
    | _ -> emit_error "ui/notify" "usage: (ui/notify msg)"
  in
  add_prim t "ui/notify" notify;
  let confirm args =
    match args with
    | [ msg ] ->
        Printf.printf "%s [y/N] %!" (str_arg "ui/confirm" msg);
        let line = try input_line stdin with End_of_file -> "n" in
        VBool
          (match String.lowercase_ascii (String.trim line) with
          | "y" | "yes" -> true
          | _ -> false)
    | _ -> emit_error "ui/confirm" "usage: (ui/confirm msg)"
  in
  add_prim t "ui/confirm" confirm

(** The agent-only command registry (register-command /
    list-commands); the shared on/register-tool pair lives in
    Host_seam.register_event_prims. *)
let register_command_prims (t : t) : unit =
  let reg_cmd args =
    match args with
    | [ name; doc; fn ] ->
        let name = str_arg "register-command" name in
        Hashtbl.replace t.commands name
          { doc = str_arg "register-command" doc; run = fn };
        VBool true
    | [ name; fn ] ->
        let name = str_arg "register-command" name in
        Hashtbl.replace t.commands name { doc = ""; run = fn };
        VBool true
    | _ ->
        emit_error "register-command"
          "usage: (register-command name [doc] fn)"
  in
  add_prim t "register-command" reg_cmd;
  let list_cmds _args =
    let names =
      Hashtbl.fold (fun k _ acc -> `String k :: acc) t.commands []
    in
    VStr (Yojson.Safe.to_string (`List (List.sort compare names)))
  in
  add_prim t "list-commands" list_cmds

(* The route table + request-context prims (route / route/lookup /
   route/dispatch / query-param / request-method / hx-request? /
   session/last-turn-parts) live in Host_seam.register_route_prims —
   THE HOST SEAM, identical Nopales signatures to the image's route
   prims so libs/hyper runs unchanged. *)
let register_route_prims (t : t) : unit =
  Host_seam.register_route_prims t.seam

let register_pure_prims (t : t) : unit =
  Host_seam.register_pure_prims t.seam

let register_soup_prims (t : t) : unit =
  Host_seam.register_soup_prims t.seam

(** Identity prims — host-local backing. The image's whoami/current-author
    read the HTTP session identity. In the agent: whoami is the OPERATOR
    (one unix process on their machine); current-author stays INTENTIONALLY
    UNBOUND — authorship is a DB-row concept (agent-route-table.pg-prim-
    unbound pins it: routes calling it must report unbound), and the lens
    has no author column. Same name, host-local backing — the route/
    dispatch seam philosophy (borge/glochid.borg, borge/local-first.borg). *)
let operator_name () : string =
  try Unix.getlogin ()
  with _ -> (
    try Sys.getenv "USER"
    with _ -> (
      try Sys.getenv "LOGNAME" with _ -> "glochid"))

let register_identity_prims (t : t) : unit =
  add_prim t "whoami" (fun _args -> Ast.VStr (operator_name ()))

let make () : t =
  let eval_mutex = Eio.Mutex.create () in
  let prims = Eval.make_prim_table () in
  let run_in_rim f =
    Eio.Mutex.lock eval_mutex;
    Fun.protect
      ~finally:(fun () -> Eio.Mutex.unlock eval_mutex)
      (fun () -> Control.run_with_rim ~prims f)
  in
  let t =
    {
      seam = Host_seam.make ~prims ~run_in_rim ~cwd ();
      eval_mutex;
      commands = Hashtbl.create 8;
    }
  in
  register_pure_prims t;
  register_soup_prims t;
  register_fs_prims t;
  Host_seam.register_event_prims t.seam;
  register_command_prims t;
  register_ui_prims t;
  register_route_prims t;
  register_identity_prims t;
  (* Host library layer: the minimal stdlib (let* macro) so libs/hyper
     and the Nopales surface run unchanged. *)
  load_source ~rt:t Host_seam.boot_stdlib;
  t

let dispatch_route ~(rt : t) ~handler ~method_ ~path ~params ~headers
    ~body : Ast.value =
  Host_seam.dispatch_route ~rt:rt.seam ~handler ~method_ ~path ~params
    ~headers ~body

let lookup_route ~(rt : t) (path : string) : string option =
  Host_seam.lookup_route ~rt:rt.seam path

let call_template ~(rt : t) (name : string) (args : Ast.value list)
    : string option =
  Host_seam.call_template ~rt:rt.seam name args

let fire (t : t) (ev : string) (payload : Ast.value list) : Ast.value list =
  Host_seam.fire t.seam ev payload

let eval_tool_body (t : t) (name : string) (args_json : string) : Ast.value =
  Host_seam.eval_tool_body t.seam name args_json

let register_builtin_tool (t : t) (name : string) ~(run : Yojson.Safe.t -> string) : unit =
  Host_seam.register_builtin_tool t.seam name ~run

let is_builtin (t : t) (name : string) : bool =
  match Hashtbl.find_opt t.seam.Host_seam.tools name with
  | Some { Loop.run = VPrim p; _ } ->
      String.length p > 10 && String.sub p 0 11 = "__builtin__"
  | _ -> false

(** Host adapter for the shared agent turn loop (llm/lib/loop.ml): the
    small [Loop.host] seam — live tool order, registry lookup, event
    fire, tool-body eval, and serialized prim dispatch through the eval
    lock + the agent's local host rim. This is the ONLY coupling the
    loop has to this host; pp-remote supplies the same record over the
    proxy rim (borge/local-first.borg agent-loop). *)
let loop_host (t : t) : Loop.host = Host_seam.loop_host t.seam
