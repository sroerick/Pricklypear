(** Pure library loader: Lisp source text → define / route / deftype.

    Same source files live in git (libs/<name>/lib.pp) and load into the
    live branch. No application logic here — only read + eval_one. *)
open Lwt.Syntax

let libs_dir () : string =
  match Sys.getenv_opt "PP_LIBS_DIR" with
  | Some d when d <> "" -> d
  | _ ->
      (* Release CWD is release root; dev may run from repo root. *)
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

let read_file (path : string) : (string, string) result =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    Ok s
  with exn -> Error (path ^ ": " ^ Printexc.to_string exn)

(** Strip `;` line comments outside strings so file headers do not confuse load. *)
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
        (* skip to end of line *)
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

(** Load SRC: each top-level form is pretty-printed and passed to
    [eval_one] so (define) persists and (route)/(deftype) run as prims. *)
let load_string
    ~(eval_one : string -> Pricklypear_lisp.Ast.value Lwt.t)
    (src : string)
    : (string, string) result Lwt.t =
  try
    let src = strip_line_comments src in
    let forms = Pricklypear_lisp.Reader.read_all src in
    let rec go i = function
      | [] ->
          Lwt.return
            (Ok (Printf.sprintf "loaded %d form(s)" i))
      | e :: rest ->
          let form_src = Pricklypear_lisp.Reader.pp_expr e in
          Lwt.catch
            (fun () ->
              let* _ = eval_one form_src in
              go (i + 1) rest)
            (fun exn ->
              let msg =
                match exn with
                | Pricklypear_lisp.Ast.Interp_error err ->
                    (match err with
                     | Pricklypear_lisp.Ast.User_error m -> m
                     | Pricklypear_lisp.Ast.Parse_error (m, l, c) ->
                         Printf.sprintf "parse: %s (%d:%d)" m l c
                     | Pricklypear_lisp.Ast.Unbound_var s -> "unbound: " ^ s
                     | _ -> Printexc.to_string exn)
                | _ -> Printexc.to_string exn
              in
              Lwt.return
                (Error
                   (Printf.sprintf "form %d failed: %s\n  in: %s" (i + 1) msg
                      form_src)))
    in
    go 0 forms
  with
  | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
      Lwt.return
        (Error (Printf.sprintf "reader: %s (%d:%d)" m l c))
  | exn -> Lwt.return (Error (Printexc.to_string exn))

let load_library
    ~(eval_one : string -> Pricklypear_lisp.Ast.value Lwt.t)
    (name : string)
    : (string, string) result Lwt.t =
  let path = library_path name in
  match read_file path with
  | Error e -> Lwt.return (Error e)
  | Ok src ->
      let* r = load_string ~eval_one src in
      (match r with
       | Ok msg ->
           Lwt.return (Ok (Printf.sprintf "%s from %s" msg path))
       | Error e -> Lwt.return (Error e))
