(** Pure library loader: Lisp source text → define / route / deftype.

    Same source files live in git (libs/<name>/lib.pp) and load into the
    live branch. Evaluates AST forms directly (no pp_expr round-trip). *)
open Pricklypear_lisp.Ast

let libs_dir () : string =
  match Sys.getenv_opt "PP_LIBS_DIR" with
  | Some d when d <> "" -> d
  | _ ->
      if Sys.file_exists "libs" then "libs"
      else if Sys.file_exists "../libs" then "../libs"
      else "libs"

let library_path (name : string) : string =
  let root = libs_dir () in
  let a = Filename.concat root (Filename.concat name "lib.pp") in
  let b = Filename.concat root (name ^ ".pp") in
  if Sys.file_exists a then a
  else if Sys.file_exists b then b
  else a

(* i-1788670098: load-library takes a PACKAGE name, not a path.
   Filename.concat passes ".." components and absolute names through
   verbatim, so an unvalidated name turned (load-library ...) into an
   arbitrary-file-read + arbitrary-code-eval: any <name>.pp or
   <name>/lib.pp on disk would be parsed and EVALUATED as lisp in the
   caller's session, and failing forms echo source snippets back.
   Package names everywhere (libs/ dirs, discover_seed_packages,
   disabled-package prefixes "pkg/name") are flat [a-z0-9_-]; enforce
   exactly that before any path is computed. Validating here, the only
   entry with untrusted input, covers both the (load-library) prim and
   the admin (reseed-package) path behind it. *)
let valid_library_name (name : string) : bool =
  let len = String.length name in
  len > 0 && len <= 64
  && String.for_all
       (fun c ->
          (c >= 'a' && c <= 'z')
          || (c >= '0' && c <= '9')
          || c = '-' || c = '_')
       name

let read_file (path : string) : (string, string) result =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    Ok s
  with exn -> Error (path ^ ": " ^ Printexc.to_string exn)

let strip_line_comments (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i in_str =
    if i >= len then Buffer.contents buf
    else
      let c = s.[i] in
      if in_str then begin
        Buffer.add_char buf c;
        if c = '\\' && i + 1 < len then begin
          Buffer.add_char buf s.[i + 1];
          loop (i + 2) true
        end else if c = '"' then loop (i + 1) false
        else loop (i + 1) true
      end else if c = '"' then begin
        Buffer.add_char buf c;
        loop (i + 1) true
      end else if c = ';' then
        let rec skip j =
          if j >= len || s.[j] = '\n' then j else skip (j + 1)
        in
        let j = skip i in
        if j < len then Buffer.add_char buf '\n';
        loop (if j < len then j + 1 else j) false
      else begin
        Buffer.add_char buf c;
        loop (i + 1) false
      end
  in
  loop 0 false

(** card 2cd8042e: names of the top-level defines a form introduces — the
    define itself, define sugar, or defines on a begin/seq spine (the
    pp-flow/seed load shapes). Load errors name them: the head-80 snippet
    of a begin-wrapped load only shows the FIRST define, so a failing
    define deep in the spine was unfindable. Capped: the error needs
    pointers, not an inventory. *)
let rec define_names_in (e : expr) : string list =
  match e with
  | Define (name, _) -> [ name ]
  | App (Var "define", Var name :: _) -> [ name ]
  | App (Var "define", App (Var name, _) :: _) -> [ name ]
  | App (Var ("begin" | "seq"), args) -> List.concat_map define_names_in args
  | Seq (a, b) -> define_names_in a @ define_names_in b
  | _ -> []
let load_string
    ~(eval_form : expr -> value)
    (src : string)
    : (string, string) result =
  try
    let trace = Sys.getenv_opt "PP_TRACE_LOAD" <> None in
    let t0 = Unix.gettimeofday () in
    let src = strip_line_comments src in
    let forms = Pricklypear_lisp.Reader.read_all src in
    let () =
      if trace then
        Printf.eprintf "[loader] parse: %d forms, %.1f ms\n%!"
          (List.length forms)
          ((Unix.gettimeofday () -. t0) *. 1000.0)
    in
    let rec go i = function
      | [] ->
          (Ok (Printf.sprintf "loaded %d form(s)" i))
      | e :: rest ->
          (try
      let tf = Unix.gettimeofday () in
      let _ = eval_form e in
              let () =
                if trace then
                  let ms = (Unix.gettimeofday () -. tf) *. 1000.0 in
                  if ms > 10.0 then
                    Printf.eprintf "[loader] form %d: %.1f ms\n%!" (i + 1) ms
              in
              go (i + 1) rest
    with exn ->
      let msg =
                match exn with
                | Interp_error err ->
                    (match err with
                     | User_error (m, _) -> m
                     | Parse_error (m, l, c) ->
                         Printf.sprintf "parse: %s (%d:%d)" m l c
                     | Unbound_var s -> "unbound: " ^ s
                     | _ -> Printexc.to_string exn)
                | _ -> Printexc.to_string exn
              in
                let snippet_src =
                  try Pricklypear_lisp.Reader.pp_expr e with _ -> "?"
                in
                let snippet =
                  String.sub snippet_src 0 (min 80 (String.length snippet_src))
                in
                (* card 2cd8042e: name what the failing form defines, so a
                   define-shape failure in a big load is findable. *)
                let names = define_names_in e in
                let define_line =
                  match names with
                  | [] -> ""
                  | [ n ] -> Printf.sprintf "\n  define: %s" n
                  | ns ->
                      Printf.sprintf "\n  defines: %s"
                        (String.concat ", "
                           (if List.length ns > 8 then
                              List.filteri (fun i _ -> i < 8) ns @ [ "…" ]
                            else ns))
                in
                (Error
                   (Printf.sprintf "form %d failed: %s\n  in: %s%s" (i + 1) msg
                      snippet define_line)))
    in
    go 0 forms
  with
  | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
      (Error (Printf.sprintf "reader: %s (%d:%d)" m l c))
  | exn -> (Error (Printexc.to_string exn))

let load_library
    ~(eval_form : expr -> value)
    (name : string)
    : (string, string) result =
  if not (valid_library_name name) then
    (Error
       (Printf.sprintf "invalid package name %S (want [a-z0-9_-]{1,64})"
          name))
  else
    let path = library_path name in
    match read_file path with
    | Error e -> (Error e)
    | Ok src ->
        let r = load_string ~eval_form src in
        (match r with
         | Ok msg -> (Ok (Printf.sprintf "%s from %s" msg path))
         | Error e -> (Error e))
