(** nopales/lib/host.ml — the standalone Nopales script host.

    The language core ([lisp/]) is I/O-free and the pure prim catalog
    ([pure/prims_pure.ml]) has no DB/net dependency. This module is the
    missing host: it installs the pure catalog plus the small set of
    process prims a script runtime needs (stdout, files, argv, exit,
    load), evaluates whole files form by form, and renders failures as
    [file:line:col: message].

    It is deliberately NOT the image: no Postgres, no identity, no
    routes, no branches. DB prims stay unbound, so a function that
    calls them fails at call time with a normal [Unbound_var] instead of
    pretending. The habitat image remains the full host.

    Stdlib policy: core Nopales has no [let*]/[when]/[unless] — those
    are [defmacro]s in [libs/std/lib.pp]. The CLI loads that file before
    the script unless [--no-std] is given. *)

open Pricklypear_lisp
open Ast

(** Raised by [(exit N)]; the CLI turns it into the process status. *)
exception Exit of int

type failure = {
  file : string option;
  line : int;
  col : int;
  head : string;  (** enclosing top-level form, e.g. "define foo" *)
  message : string;
}

type t = {
  prims : Eval.prim_table;
  env : env;
  argv : string list;
  (* Directory stack for resolving relative [(load …)] paths. Innermost
     first; the top-level script's directory sits above the process cwd. *)
  dirs : string list ref;
  (* Active load chain, for cycle detection (a.pp -> b.pp -> a.pp). *)
  active : string list ref;
  out : string -> unit;  (** [(print …)] sink (no implicit newline) *)
  mutable std_loaded : bool;
}

let read_file (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let n = in_channel_length ic in
      really_input_string ic n)

let emit_error name msg =
  raise (Interp_error (Prim_error (name, msg, "script")))

let str_arg name = function
  | VStr s | VSymbol s -> s
  | v ->
      raise
        (Interp_error
           (Type_mismatch ("string for " ^ name, v)))

let int_arg name = function
  | VInt n -> Int64.to_int n
  | v -> raise (Interp_error (Type_mismatch ("integer for " ^ name, v)))

(** Human-readable rendering of a top-level form for error context. *)
let head_of : expr -> string = function
  | App (Var "define", Var n :: _) -> "define " ^ n
  | App (Var "define", App (Var n, _) :: _) -> "define " ^ n
  | App (Var n, _) -> n
  | Var n -> n
  | Lit _ -> "<literal>"
  | _ -> "<form>"

let format_failure (f : failure) : string =
  let loc =
    match f.file with
    | Some p -> Printf.sprintf "%s:%d:%d" p f.line f.col
    | None -> Printf.sprintf "%d:%d" f.line f.col
  in
  let form = if f.head = "" then "" else Printf.sprintf " in (%s …)" f.head in
  Printf.sprintf "%s: %s%s" loc f.message form

(** Top-level metadata forms a Nopales package file carries that a script
    host must ignore rather than evaluate. [(package …)] is the only one
    today: it is a deploy/export directive, not a language form. *)
let deploy_form : expr -> bool = function
  | App (Var "package", _) -> true
  | _ -> false

let resolve (t : t) (path : string) : string =
  if Filename.is_relative path then
    Filename.concat (List.hd !(t.dirs)) path
  else path

let write_file ~(path : string) (contents : string) : unit =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then ignore (Unix.mkdir dir 0o755);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

(** Evaluate every top-level form in [src], stop at the first failure.
    Returns the last form's value (VNil for an empty source). Reader
    errors and interpreter errors both come back as [failure], so the
    caller decides how to print them. [budget] is shared with the caller
    when [(load …)] is used, so a runaway include chain still exhausts
    one step budget. *)
let eval_source ?file ?budget (t : t) (src : string) :
    (value, failure) result =
  let budget = match budget with Some b -> b | None -> Eval.make_budget () in
  (* A process may host more than one [t] (tests, embeddings); make this
     eval dispatch through its own table. *)
  Control.direct_prims := Some t.prims;
  match Reader.read_all_pos src with
  | exception Reader.Reader_error (msg, line, col) ->
      Error { file; line; col; head = ""; message = "parse error: " ^ msg }
  | forms ->
      Eval.with_scope (Some budget) (fun () ->
          let rec loop last = function
            | [] -> Ok last
            | (form, (line, col)) :: rest ->
                if deploy_form form then loop last rest
                else
                  (try
                     let v = Eval.eval ~budget ~prims:t.prims t.env form in
                     loop v rest
                   with
                   | Interp_error e ->
                       Error
                         { file; line; col; head = head_of form;
                           message = Ast.pp_interp_error e }
                   | Reader.Reader_error (msg, l, c) ->
                       Error
                         { file; line = l; col = c; head = head_of form;
                           message = "parse error: " ^ msg }
                   | Exit n -> raise (Exit n)
                   | exn ->
                       Error
                         { file; line; col; head = head_of form;
                           message = Printexc.to_string exn })
          in
          loop VNil forms)

(** Load and evaluate a file. The file's directory becomes the base for
    relative [(load …)] calls made while its forms evaluate. *)
let load_file (t : t) (path : string) : (value, failure) result =
  match read_file path with
  | exception Sys_error msg ->
      Error { file = Some path; line = 1; col = 1; head = ""; message = msg }
  | src ->
      let saved = !(t.dirs) in
      t.dirs := Filename.dirname path :: saved;
      Fun.protect
        ~finally:(fun () -> t.dirs := saved)
        (fun () -> eval_source ~file:path t src)

let eval_string (t : t) (src : string) : (value, failure) result =
  eval_source t src

(** Load the stdlib seed. Marks the host as seeded only on success. *)
let load_std (t : t) (path : string) : (unit, failure) result =
  match load_file t path with
  | Ok _ -> t.std_loaded <- true; Ok ()
  | Error e -> Error e

let std_loaded (t : t) : bool = t.std_loaded

(** ---- host prims ---- *)

let display = function VStr s -> s | v -> Reader.pp_value v

let add_prim (t : t) (name : string) (fn : value list -> value) : unit =
  Eval.register_prim t.prims name fn;
  Env.define t.env name (VPrim name)

let install_host_prims (t : t) : unit =
  let add = add_prim t in
  let print args =
    t.out (String.concat " " (List.map display args));
    VNil
  in
  add "print" print;
  add "println" (fun args ->
      t.out (String.concat " " (List.map display args) ^ "\n");
      VNil);
  add "fs/read" (fun args ->
      match args with
      | [ path ] -> (
          let p = resolve t (str_arg "fs/read" path) in
          try VStr (read_file p)
          with Sys_error msg -> emit_error "fs/read" msg)
      | _ -> emit_error "fs/read" "usage: (fs/read path)");
  add "fs/write" (fun args ->
      match args with
      | [ path; content ] -> (
          let p = resolve t (str_arg "fs/write" path) in
          try write_file ~path:p (str_arg "fs/write" content); VBool true
          with Sys_error msg -> emit_error "fs/write" msg)
      | _ -> emit_error "fs/write" "usage: (fs/write path content)");
  add "fs/exists?" (fun args ->
      match args with
      | [ path ] ->
          VBool (Sys.file_exists (resolve t (str_arg "fs/exists?" path)))
      | _ -> emit_error "fs/exists?" "usage: (fs/exists? path)");
  add "argv" (fun args ->
      match args with
      | [] -> VList (List.map (fun s -> VStr s) t.argv)
      | _ -> emit_error "argv" "usage: (argv)");
  add "exit" (fun args ->
      match args with
      | [] -> raise (Exit 0)
      | [ code ] -> raise (Exit (int_arg "exit" code))
      | _ -> emit_error "exit" "usage: (exit [code])");
  add "load" (fun args ->
      match args with
      | [ path ] ->
          let p = resolve t (str_arg "load" path) in
          if List.mem p !(t.active) then
            emit_error "load" ("cycle loading " ^ p);
          t.active := p :: !(t.active);
          Fun.protect
            ~finally:(fun () ->
              match !(t.active) with
              | _ :: rest -> t.active := rest
              | [] -> ())
            (fun () ->
              match
                (* Share the caller's step budget so includes cannot
                   multiply the per-eval limit. *)
                (try
                   eval_source ~file:p
                     ~budget:(Eval.current_or_fresh ()) t (read_file p)
                 with Sys_error msg -> emit_error "load" msg)
              with
              | Ok v -> v
              | Error f ->
                  raise
                    (Interp_error
                       (User_error (format_failure f, "load"))))
      | _ -> emit_error "load" "usage: (load path)")

(** ---- local lens reads (offline data) ----

    With [--lens DIR] the host reads the cloned rows the lens already
    holds: [(get 'type id)] and [(tree/get path)] are path-addressed
    reads, and [(rows 'type [limit] [sort])] scans the type's directory.
    Nothing here crosses a wire — a miss is [()] / an empty list, which
    is the offline working-set semantics pp-remote uses for [get] and
    [tree/get]. Indexed queries on the image remain server-side; the
    local rows scan is the script-host substitute for small vocabularies
    (default limit 100, same as the image's [rows]). *)

type sort_col = Sort_created | Sort_updated | Sort_id
type sort_dir = Asc | Desc

(** Decode a lens path label (base64 with [_/-] substitutions and the
    pad count as a trailing 0..2 digit) back to the original name. Used
    to infer the lens owner from [data/u/<label>]. *)
let decode_label (s : string) : string option =
  let n = String.length s in
  if n < 2 then None
  else
    let pad = Char.code s.[n - 1] - Char.code '0' in
    if pad < 0 || pad > 2 then None
    else
      let core = String.sub s 0 (n - 1) in
      let core =
        String.map (function '_' -> '+' | '-' -> '/' | c -> c) core
      in
      match Base64.decode (core ^ String.make pad '=') with
      | Ok v -> Some v
      | Error _ -> None

(** Owner precedence: the single [data/u/<label>] directory in the lens,
    then NOPALES_USER, then PP_USER, then "admin". *)
let infer_owner (root : string) : string =
  let dir = Filename.concat root "data/u" in
  match (try Sys.readdir dir with Sys_error _ -> [||]) with
  | [| only |] -> Option.value (decode_label only) ~default:"admin"
  | _ -> (
      match Sys.getenv_opt "NOPALES_USER" with
      | Some u when String.trim u <> "" -> u
      | _ -> (
          match Sys.getenv_opt "PP_USER" with
          | Some u when String.trim u <> "" -> u
          | _ -> "admin"))

let lens_of ?lens_owner (root : string) : Lens_store.lens =
  let owner =
    match lens_owner with Some o -> o | None -> infer_owner root
  in
  {
    Lens_store.root;
    owner;
    device = "script";
    branch = "script";
    generation = 0L;
  }

let lens_type_dir (lens : Lens_store.lens) (type_name : string) : string =
  List.fold_left Filename.concat lens.root
    [ "data"; "u"; Lens_store.enc_label lens.owner;
      Lens_store.enc_label type_name ]

let parse_sort (s : string) : sort_col * sort_dir =
  let s = String.trim s in
  let upper = String.uppercase_ascii s in
  let dir, base =
    if Filename.check_suffix upper " DESC" then
      (Desc, String.sub s 0 (String.length s - 5))
    else if Filename.check_suffix upper " ASC" then
      (Asc, String.sub s 0 (String.length s - 4))
    else (Asc, s)
  in
  let col =
    match String.lowercase_ascii (String.trim base) with
    | "created_at" -> Sort_created
    | "updated_at" -> Sort_updated
    | "id" -> Sort_id
    | other ->
        emit_error "rows"
          (Printf.sprintf
             "unsupported local sort %S (created_at|updated_at|id, optional \
              ASC|DESC)"
             other)
  in
  (col, dir)

(** Dash a 32-char uuid label (the path segment) into the canonical
    8-4-4-4-12 form the image's [(rows …)] returns. The envelope's [id]
    field is the TREE NODE uuid, which is not the datum row id — the row
    id is the last path segment. *)
let dash_uuid (s : string) : string =
  if String.length s <> 32 then s
  else
    Printf.sprintf "%s-%s-%s-%s-%s"
      (String.sub s 0 8)
      (String.sub s 8 4)
      (String.sub s 12 4)
      (String.sub s 16 4)
      (String.sub s 20 12)

(** Scan [data/u/<owner>/<type>/*.json] and return the image's [(rows …)]
    shape: [(id fields-json)] pairs, default created_at ASC, limit 100.
    The row id is the datum path label (dashed uuid), matching the
    server; the envelope [id] is a tree-node uuid and is deliberately
    ignored. *)
let local_rows (lens : Lens_store.lens) (type_name : string) ~(limit : int)
    ~(sort : string option) : value =
  let col, dir =
    match sort with Some s -> parse_sort s | None -> (Sort_created, Asc)
  in
  let dirpath = lens_type_dir lens type_name in
  let names =
    match Sys.readdir dirpath with
    | a -> Array.to_list a
    | exception Sys_error _ -> []
  in
  let rows =
    List.filter_map
      (fun name ->
        if not (Filename.check_suffix name ".json") then None
        else
          let file = Filename.concat dirpath name in
          match read_file file with
          | exception Sys_error _ -> None
          | json -> (
              match Yojson.Safe.from_string json with
              | `Assoc kv ->
                  let str k =
                    match List.assoc_opt k kv with
                    | Some (`String s) -> Some s
                    | _ -> None
                  in
                  let id =
                    dash_uuid (Filename.chop_suffix name ".json")
                  in
                  (match List.assoc_opt "body" kv with
                   | Some body ->
                       Some
                         ( id,
                           Option.value (str "created_at") ~default:"",
                           Option.value (str "updated_at") ~default:"",
                           Yojson.Safe.to_string body )
                   | None -> None)
              | _ -> None))
      names
  in
  let key =
    match col with
    | Sort_created -> (fun (_, c, _, _) -> c)
    | Sort_updated -> (fun (_, _, u, _) -> u)
    | Sort_id -> (fun (i, _, _, _) -> i)
  in
  let order = match dir with Asc -> 1 | Desc -> -1 in
  let rows =
    List.sort
      (fun a b -> order * String.compare (key a) (key b))
      rows
  in
  let rec take n = function
    | [] -> []
    | _ when n <= 0 -> []
    | x :: rest -> x :: take (n - 1) rest
  in
  VList
    (List.map
       (fun (id, _, _, body) -> VPair (VStr id, VStr body))
       (take limit rows))

let install_lens_reads (t : t) (lens : Lens_store.lens) : unit =
  let coerce = function VSymbol s -> VStr s | v -> v in
  add_prim t "get" (fun args ->
      match List.map coerce args with
      | [ VStr type_name; VStr id ] -> (
          let path =
            Lens_store.datum_path ~user:lens.owner ~type_name ~id ()
          in
          match Lens_store.get ~lens path with
          | Some env -> (
              match Lens_store.datum_body_of_envelope env with
              | Some body -> VStr body
              | None -> VNil)
          | None -> VNil)
      | _ -> emit_error "get" "usage: (get 'type id)");
  add_prim t "tree/get" (fun args ->
      match List.map coerce args with
      | [ VStr path ] -> (
          match Lens_store.get ~lens path with
          | Some env -> VStr env
          | None -> VNil)
      | _ -> emit_error "tree/get" "usage: (tree/get path)");
  add_prim t "rows" (fun args ->
      match List.map coerce args with
      | VStr type_name :: rest ->
          let limit, sort =
            match rest with
            | [] -> (100, None)
            | [ VInt n ] -> (Int64.to_int n, None)
            | [ VInt n; VStr s ] -> (Int64.to_int n, Some s)
            | _ -> emit_error "rows" "usage: (rows 'type [limit] [sort])"
          in
          local_rows lens type_name ~limit ~sort
      | _ -> emit_error "rows" "usage: (rows 'type [limit] [sort])")

(** Build a host with the pure catalog installed and bound. Pure compute
    needs no image; DB/identity prims stay unbound on purpose. With
    [lens_root] the path-addressed data reads ([get], [tree/get], [rows])
    resolve against the local lens instead. *)
let create ?(out : (string -> unit) = print_string) ?(argv : string list = [])
    ?(cwd : string option) ?lens_root ?lens_owner () : t =
  let prims = Eval.make_prim_table () in
  Prims_pure.install prims;
  (* Pure hosts own the Prim_call effect through the direct table (same
     posture as the browser client). Without this, [Eval.eval] would
     escalate an effect nobody handles. *)
  Control.direct_prims := Some prims;
  let env = Env.create () in
  Hashtbl.iter (fun name _ -> Env.define env name (VPrim name)) prims;
  let t =
    {
      prims;
      env;
      argv;
      dirs = ref [ Option.value cwd ~default:(Sys.getcwd ()) ];
      active = ref [];
      out;
      std_loaded = false;
    }
  in
  install_host_prims t;
  (match lens_root with
   | Some root ->
       if not (Sys.file_exists root) then
         invalid_arg ("lens root does not exist: " ^ root);
       install_lens_reads t (lens_of ?lens_owner root)
   | None -> ());
  t
