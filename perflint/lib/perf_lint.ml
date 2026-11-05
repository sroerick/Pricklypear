(** R1 static perf guard: DB prims inside per-row loop lambdas.

    Walks every libs/<pkg>/lib.pp through the repo's own reader
    ([Pricklypear_lisp.Reader]) and the DB prim set from [Db_cost], and
    reports each DB prim call that sits lexically inside a lambda handed to
    a row-loop HOF (list/foldl, list/map, list/filter, ...).

    Severities:
      RED    the prim is in the loop lambda's own body: one statement per row.
      AMBER  the prim is in a helper the loop lambda calls: a lambda bound in
             the same define (let/letrec/define), or a define in the same
             lib.pp file.

    Positions come from [read_node], a position wrapper that reuses the
    reader's own token functions (peek/advance/skip/read_atom/read_string).
    Every file is cross-checked against [Reader.read_all], so the wrapper
    cannot silently drift from the AST the interpreter loads: a mismatch is
    a loud error, not a quiet wrong line number. *)

module Ast = Pricklypear_lisp.Ast
module Reader = Pricklypear_lisp.Reader
module Db_cost = Pricklypear_image_lib.Db_cost

type severity = Red | Amber

let severity_name = function Red -> "RED" | Amber -> "AMBER"

(** A parsed expression plus its source position. [kids] is the
    source-order element list of a paren form (head first), or [] for an
    atom. *)
type node = {
  expr : Ast.expr;
  line : int;
  col : int;
  is_list : bool;
  kids : node list;
}

type finding = {
  file : string;
  line : int;
  col : int;
  severity : severity;
  define_name : string;
  prim : string;
  loop : string;
  via : string option;
  depth : int;  (** loop nesting depth, used only to deduplicate *)
}

exception Lint_error of string

(* ------------------------------------------------------------------ *)
(* Positions                                                          *)
(* ------------------------------------------------------------------ *)

let rec read_node (st : Reader.state) : node =
  Reader.skip_ws_and_comments st;
  let line = st.Reader.line and col = st.Reader.col in
  match Reader.peek st with
  | None -> raise (Lint_error "unexpected end of input")
  | Some '(' ->
      ignore (Reader.advance st);
      let rec elems acc =
        Reader.skip_ws_and_comments st;
        match Reader.peek st with
        | None -> raise (Lint_error "unterminated list")
        | Some ')' ->
            ignore (Reader.advance st);
            List.rev acc
        | Some _ -> elems (read_node st :: acc)
      in
      let kids = elems [] in
      let expr =
        match kids with
        | [] -> Ast.Lit Ast.VNil
        | op :: args -> Ast.App (op.expr, List.map (fun k -> k.expr) args)
      in
      { expr; line; col; is_list = true; kids }
  | Some '\'' ->
      ignore (Reader.advance st);
      let k = read_node st in
      { expr = Ast.App (Ast.Var "quote", [ k.expr ]); line; col; is_list = false; kids = [ k ] }
  | Some '`' ->
      ignore (Reader.advance st);
      let k = read_node st in
      { expr = Ast.App (Ast.Var "quasiquote", [ k.expr ]); line; col; is_list = false; kids = [ k ] }
  | Some ',' ->
      ignore (Reader.advance st);
      let name =
        match Reader.peek st with
        | Some '@' ->
            ignore (Reader.advance st);
            "unquote-splicing"
        | _ -> "unquote"
      in
      let k = read_node st in
      { expr = Ast.App (Ast.Var name, [ k.expr ]); line; col; is_list = false; kids = [ k ] }
  | Some '"' ->
      let s = Reader.read_string st in
      { expr = Ast.Lit (Ast.VStr s); line; col; is_list = false; kids = [] }
  | Some c when Reader.is_token_start c ->
      { expr = Reader.read_atom st; line; col; is_list = false; kids = [] }
  | Some c -> raise (Lint_error (Printf.sprintf "unexpected character %C" c))

(** Parse [src] into positioned nodes, then prove the result is the AST
    [Reader.read_all] produces for the same text. *)
let read_nodes ~(file : string) (src : string) : node list =
  let st = Reader.make_state src in
  let rec loop acc =
    Reader.skip_ws_and_comments st;
    match Reader.peek st with
    | None -> List.rev acc
    | Some _ -> loop (read_node st :: acc)
  in
  let nodes = loop [] in
  let real = List.map Reader.pp_expr (Reader.read_all src) in
  let mine = List.map (fun n -> Reader.pp_expr n.expr) nodes in
  if real <> mine then
    raise
      (Lint_error
         (file ^ ": position reader disagreed with Reader.read_all"));
  nodes

(* ------------------------------------------------------------------ *)
(* DB prim set                                                        *)
(* ------------------------------------------------------------------ *)

(** Store prims [Db_cost.static_cost] does not tier yet but the store
    exposes the same way (tree rows and per-item datum rows). *)
let extra_db_prims = [ "tree/list-kind"; "datum/rows-for-item" ]

let db_prim_tbl : (string, unit) Hashtbl.t =
  let t = Hashtbl.create 64 in
  List.iter (fun p -> Hashtbl.replace t p ()) Db_cost.known_prims;
  List.iter (fun p -> Hashtbl.replace t p ()) extra_db_prims;
  t

let is_db_prim (p : string) : bool = Hashtbl.mem db_prim_tbl p

(* ------------------------------------------------------------------ *)
(* Row loops                                                          *)
(* ------------------------------------------------------------------ *)

(** The row-loop heads the interpreter implements (eval.ml
    special_form_names) plus for-each spellings. *)
let core_loop_heads =
  [ "list/foldl"; "list/foldr"; "list/map"; "list/filter"; "for-each";
    "list/for-each"; "each" ]

(** Package-local HOF wrappers (rb/map, filmstrip/map-indexed, ...) are row
    loops too. Recognised by the tail of the name; the lambda-argument test
    below keeps non-HOF calls out. *)
let loop_name_tails =
  [ "map"; "map-indexed"; "filter"; "foldl"; "foldr"; "for-each"; "each" ]

let name_tail (name : string) : string =
  match String.rindex_opt name '/' with
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)
  | None -> name

let is_loop_head (name : string) : bool =
  List.mem name core_loop_heads || List.mem (name_tail name) loop_name_tails

(* ------------------------------------------------------------------ *)
(* Tree helpers                                                       *)
(* ------------------------------------------------------------------ *)

(** Source-order elements of a paren form; [n] itself for an atom. *)
let elements (n : node) : node list = if n.is_list then n.kids else [ n ]

let is_lambda (n : node) : bool =
  match n.expr with
  | Ast.App (Ast.Var "lambda", _) | Ast.Lambda _ -> true
  | _ -> false

(** Body forms of a lambda node (params and any docstring skipped by the
    caller's walk; the docstring is a literal and harmless). *)
let lambda_body (n : node) : node list =
  match n.expr with
  | Ast.App (Ast.Var "lambda", _) -> (
      match n.kids with _ :: _params :: bodies -> bodies | _ -> [])
  | _ -> [ n ]

let prim_call (n : node) : string option =
  match n.expr with
  | Ast.App (Ast.Var p, _) | Ast.Prim (p, _) when is_db_prim p -> Some p
  | _ -> None

(** [name, body forms] of a define node, sugar or plain. *)
let define_parts (n : node) : (string * node list) option =
  match n.expr with
  | Ast.App (Ast.Var "define", _) -> (
      match n.kids with
      | _ :: name_node :: rest -> (
          match name_node.expr with
          | Ast.Var nm -> Some (nm, rest)
          | Ast.App (Ast.Var nm, _) -> Some (nm, rest)
          | _ -> None)
      | _ -> None)
  | _ -> None

let define_label (n : node) : string =
  match define_parts n with
  | Some (nm, _) -> nm
  | None -> (
      match n.expr with
      | Ast.App (Ast.Var "defmacro", _) -> (
          match n.kids with
          | _ :: name :: _ -> (
              match name.expr with Ast.Var nm -> nm | _ -> "<defmacro>")
          | _ -> "<defmacro>")
      | Ast.App (Ast.Var "route", _) -> (
          match n.kids with
          | _ :: p :: _ -> (
              match p.expr with
              | Ast.Lit (Ast.VStr path) -> "route " ^ path
              | _ -> "<route>")
          | _ -> "<route>")
      | Ast.App (Ast.Var "package", _) -> (
          match n.kids with
          | _ :: p :: _ -> (
              match p.expr with Ast.Var nm -> "package " ^ nm | _ -> "<package>")
          | _ -> "<package>")
      | Ast.App (Ast.Var h, _) -> "<" ^ h ^ ">"
      | _ -> "<top-level>")

(* ------------------------------------------------------------------ *)
(* Prim collection                                                    *)
(* ------------------------------------------------------------------ *)

let rec prims_in (acc : (node * string) list) (n : node) :
    (node * string) list =
  let acc = match prim_call n with Some p -> (n, p) :: acc | None -> acc in
  match n.expr with
  | Ast.App (Ast.Var "quote", _) -> acc (* data, never a call *)
  | _ -> List.fold_left prims_in acc n.kids

(** Prims lexically inside a loop lambda. Nested loop lambdas are their own
    finding (reported when that loop is scanned), so their bodies are
    skipped here; a prim in their collection argument still runs per outer
    row and is kept. *)
let rec prims_in_loop_body (acc : (node * string) list) (n : node) :
    (node * string) list =
  let acc = match prim_call n with Some p -> (n, p) :: acc | None -> acc in
  match n.expr with
  | Ast.App (Ast.Var "quote", _) -> acc
  | Ast.App (Ast.Var h, _) when is_loop_head h && List.exists is_lambda n.kids ->
      List.fold_left
        (fun a k -> if is_lambda k then a else prims_in_loop_body a k)
        acc n.kids
  | _ -> List.fold_left prims_in_loop_body acc n.kids

let rec callee_names (acc : string list) (n : node) : string list =
  let acc =
    match n.expr with Ast.App (Ast.Var h, _) -> h :: acc | _ -> acc
  in
  List.fold_left callee_names acc n.kids

(* ------------------------------------------------------------------ *)
(* Local function bindings                                            *)
(* ------------------------------------------------------------------ *)

(** Function bindings visible in a define body: let/let*/letrec lambdas and
    nested defines. *)
let collect_locals (body : node list) : (string, node) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  let rec go (n : node) : unit =
    (match n.expr with
    | Ast.App (Ast.Var ("let" | "let*" | "letrec" | "let-values"), _) -> (
        match n.kids with
        | _ :: bindings :: _ ->
            List.iter
              (fun pair ->
                match elements pair with
                | name_node :: value :: _ -> (
                    match name_node.expr with
                    | Ast.Var nm when is_lambda value ->
                        Hashtbl.replace tbl nm value
                    | _ -> ())
                | _ -> ())
              (elements bindings)
        | _ -> ())
    | Ast.App (Ast.Var "define", _) -> (
        match define_parts n with
        | Some (nm, b) -> (
            match List.find_opt is_lambda b with
            | Some lam -> Hashtbl.replace tbl nm lam
            | None -> ())
        | None -> ())
    | _ -> ());
    List.iter go n.kids
  in
  List.iter go body;
  tbl

(** Top-level (and begin/seq-spine) defines of one file, name -> body node. *)
let collect_tops (nodes : node list) : (string, node) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  let rec go (n : node) : unit =
    (match define_parts n with
    | Some (nm, body) -> (
        match List.find_opt is_lambda body with
        | Some lam -> Hashtbl.replace tbl nm lam
        | None -> (
            match body with v :: _ -> Hashtbl.replace tbl nm v | [] -> ()))
    | None -> ());
    match n.expr with
    | Ast.App (Ast.Var ("begin" | "seq"), _) -> (
        match n.kids with _ :: rest -> List.iter go rest | [] -> ())
    | _ -> ()
  in
  List.iter go nodes;
  tbl

(* ------------------------------------------------------------------ *)
(* Scan                                                               *)
(* ------------------------------------------------------------------ *)

let scan_body ~(file : string) ~(tops : (string, node) Hashtbl.t)
    ~(define_name : string) ~(locals : (string, node) Hashtbl.t)
    (emit : finding -> unit) (body : node list) : unit =
  let emit_prim ~severity ~via ~loop ~depth (pn : node) (prim : string) =
    emit
      {
        file;
        line = pn.line;
        col = pn.col;
        severity;
        define_name;
        prim;
        loop;
        via;
        depth;
      }
  in
  let amber_from_helper ~via ~loop ~depth =
    if via = define_name then ()
    else
      let helper =
        match Hashtbl.find_opt locals via with
        | Some n -> Some n
        | None -> Hashtbl.find_opt tops via
      in
      match helper with
      | None -> ()
      | Some h ->
          List.iter
            (fun (pn, prim) ->
              emit_prim ~severity:Amber ~via:(Some via) ~loop ~depth pn prim)
            (List.rev (prims_in [] h))
  in
  let rec walk ~(define_name : string) ~(locals : (string, node) Hashtbl.t)
      ~(depth : int) (n : node) : unit =
    match n.expr with
    | Ast.App (Ast.Var "define", _) -> (
        (* nested define: attribute its body to its own name *)
        match define_parts n with
        | Some (nm, dbody) ->
            let locals' = collect_locals dbody in
            List.iter
              (walk ~define_name:nm ~locals:locals' ~depth:(depth + 1))
              dbody
        | None -> List.iter (walk ~define_name ~locals ~depth:(depth + 1)) n.kids)
    | Ast.App (Ast.Var h, _) when is_loop_head h -> (
        (match n.kids with
        | _head :: f :: _ ->
            if is_lambda f then begin
              let body = lambda_body f in
              List.iter
                (fun b ->
                  List.iter
                    (fun (pn, prim) ->
                      emit_prim ~severity:Red ~via:None ~loop:h ~depth pn prim)
                    (List.rev (prims_in_loop_body [] b)))
                body;
              let callees =
                List.sort_uniq compare
                  (List.concat_map (callee_names []) body)
              in
              List.iter
                (fun c -> amber_from_helper ~via:c ~loop:h ~depth)
                callees
            end
            else (
              match f.expr with
              | Ast.Var c -> amber_from_helper ~via:c ~loop:h ~depth
              | _ -> ())
        | _ -> ());
        List.iter (walk ~define_name ~locals ~depth:(depth + 1)) n.kids)
    | _ -> List.iter (walk ~define_name ~locals ~depth:(depth + 1)) n.kids
  in
  List.iter (walk ~define_name ~locals ~depth:0) body

let finding_key (f : finding) : string =
  Printf.sprintf "%s:%d:%d:%s:%s:%s" f.file f.line f.col f.prim
    (severity_name f.severity)
    (match f.via with None -> "-" | Some v -> v)

let dedupe (fs : finding list) : finding list =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun f ->
      let k = finding_key f in
      match Hashtbl.find_opt tbl k with
      | Some prev when prev.depth >= f.depth -> ()
      | _ -> Hashtbl.replace tbl k f)
    fs;
  Hashtbl.fold (fun _ f acc -> f :: acc) tbl []
  |> List.sort (fun a b ->
         let c = compare a.file b.file in
         if c <> 0 then c
         else
           let c = compare a.line b.line in
           if c <> 0 then c
           else
             let c = compare a.col b.col in
             if c <> 0 then c else compare a.prim b.prim)

let scan_source ~(file : string) (src : string) : finding list =
  let nodes = read_nodes ~file src in
  let tops = collect_tops nodes in
  let out = ref [] in
  let emit f = out := f :: !out in
  let rec top (n : node) : unit =
    match n.expr with
    | Ast.App (Ast.Var ("begin" | "seq"), _) -> (
        match n.kids with _ :: rest -> List.iter top rest | [] -> ())
    | _ -> (
        match define_parts n with
        | Some (name, body) ->
            let locals = collect_locals body in
            scan_body ~file ~tops ~define_name:name ~locals emit body
        | None ->
            let body = elements n in
            let name = define_label n in
            let locals = collect_locals body in
            scan_body ~file ~tops ~define_name:name ~locals emit body)
  in
  List.iter top nodes;
  dedupe (List.rev !out)

let scan_file (path : string) : finding list =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let src = really_input_string ic len in
  close_in ic;
  scan_source ~file:path src

(* ------------------------------------------------------------------ *)
(* Reporting                                                          *)
(* ------------------------------------------------------------------ *)

let format_line (f : finding) : string =
  Printf.sprintf "%s:%d:%d %-5s %s  in %s  loop=%s%s" f.file f.line f.col
    (severity_name f.severity)
    f.prim f.define_name f.loop
    (match f.via with None -> "" | Some v -> " via=" ^ v)

let count_sev (sev : severity) (fs : finding list) : int =
  List.length (List.filter (fun f -> f.severity = sev) fs)

let summary_line ~(files : int) (fs : finding list) : string =
  Printf.sprintf "perf-lint R1: %d lib.pp files, %d findings (%d RED, %d AMBER)"
    files
    (List.length fs)
    (count_sev Red fs)
    (count_sev Amber fs)

(** 0 when no RED finding exists, 1 otherwise: the code CI and the loop
    gate on. Unreadable input is 2, decided by [main]. *)
let exit_code (fs : finding list) : int =
  if count_sev Red fs > 0 then 1 else 0

let finding_json (f : finding) : Yojson.Safe.t =
  `Assoc
    [
      ("file", `String f.file);
      ("line", `Int f.line);
      ("col", `Int f.col);
      ("severity", `String (severity_name f.severity));
      ("define", `String f.define_name);
      ("prim", `String f.prim);
      ("loop", `String f.loop);
      ("via", match f.via with None -> `Null | Some v -> `String v);
    ]

let report_json ~(files : int) (fs : finding list) : Yojson.Safe.t =
  `Assoc
    [
      ("findings", `List (List.map finding_json fs));
      ( "summary",
        `Assoc
          [
            ("files", `Int files);
            ("findings", `Int (List.length fs));
            ("red", `Int (count_sev Red fs));
            ("amber", `Int (count_sev Amber fs));
          ] );
    ]

(* ------------------------------------------------------------------ *)
(* CLI                                                                *)
(* ------------------------------------------------------------------ *)

let pp_files_under (root : string) : string list =
  let rec walk (dir : string) (acc : string list) : string list =
    if not (Sys.file_exists dir) then acc
    else if not (Sys.is_directory dir) then
      if Filename.check_suffix dir ".pp" then dir :: acc else acc
    else
      let entries = try Array.to_list (Sys.readdir dir) with _ -> [] in
      List.fold_left
        (fun a e ->
          if e = "_build" || e = ".git" then a
          else walk (Filename.concat dir e) a)
        acc (List.sort compare entries)
  in
  List.rev (walk root [])

let expand_path (p : string) : string list =
  if Sys.file_exists p && Sys.is_directory p then pp_files_under p
  else if Sys.file_exists p then [ p ]
  else []

let main ~(json : bool) ~(paths : string list) : int =
  let paths = if paths = [] then [ "libs" ] else paths in
  let files =
    List.sort_uniq compare (List.concat_map expand_path paths)
  in
  if files = [] then begin
    Printf.eprintf "perf-lint: no .pp files under %s\n%!"
      (String.concat ", " paths);
    2
  end
  else begin
    let errored = ref false in
    let findings =
      List.concat_map
        (fun f ->
          try scan_file f
          with
          | Lint_error msg ->
              errored := true;
              Printf.eprintf "%s: ERROR %s\n%!" f msg;
              []
          | Reader.Reader_error (msg, line, col) ->
              errored := true;
              Printf.eprintf "%s:%d:%d: ERROR %s\n%!" f line col msg;
              []
          | Sys_error msg ->
              errored := true;
              Printf.eprintf "%s: ERROR %s\n%!" f msg;
              [])
        files
    in
    if json then
      print_endline (Yojson.Safe.to_string (report_json ~files:(List.length files) findings))
    else begin
      List.iter (fun f -> print_endline (format_line f)) findings;
      print_endline (summary_line ~files:(List.length files) findings)
    end;
    if !errored then 2 else exit_code findings
  end
