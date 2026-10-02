(** Comment-preserving vault merge (card 8dff7c9e, borge/branching.borg
    vault-authority).

    The vault (libs/<pkg>/lib.pp) is LITERATE SOURCE: comment blocks
    explain non-obvious code and live ONLY in git — the store keeps the
    AST, and (export-package …) emits code with NO interior comments
    (filmstrip: 146 comment lines in git, 2 in the live export). A
    snapshot that wrote generator output over the file would delete that
    documentation permanently.

    This module merges the freshly exported text into the EXISTING file:
    for every top-level form whose canonical AST is unchanged, the old
    text is emitted VERBATIM (comments, formatting, order all kept). Only
    forms whose AST actually changed are replaced by the new text. Every
    form is matched by NAME, so reordering is harmless.

    Honest limit: a CHANGED form takes the new text, so its interior
    comments can be lost. That is reported ({changed_with_comments}) so
    the author can re-attach them — the merge never silently drops a
    comment it noticed. *)

(* ------------------------------------------------------------------ *)
(* Top-level form scanner.                                            *)
(* ------------------------------------------------------------------ *)

type chunk = { prefix : string; body : string }

(* Split [text] into (prefix, body) chunks. [prefix] is the whitespace
   and line comments before each top-level form; [body] is one balanced
   s-expression, string literal, or atom. Returns (chunks, trailing),
   where [trailing] is the whitespace/comments after the last form. *)
let scan_chunks (text : string) : chunk list * string =
  let n = String.length text in
  let i = ref 0 in
  let chunks = ref [] in
  let trailing = ref "" in
  (* Consume whitespace + ';' line comments; return that prefix. *)
  let consume_prefix () =
    let start = !i in
    let go = ref true in
    while !go && !i < n do
      let c = text.[!i] in
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
      else if c = ';' then
        while !i < n && text.[!i] <> '\n' do incr i done
      else go := false
    done;
    String.sub text start (!i - start)
  in
  let scan_form () =
    let start = !i in
    if !i < n && text.[!i] = '(' then begin
      let depth = ref 0 in
      let in_str = ref false and in_cmt = ref false in
      let done_ = ref false in
      while (not !done_) && !i < n do
        let c = text.[!i] in
        if !in_cmt then begin
          if c = '\n' then in_cmt := false;
          incr i
        end
        else if !in_str then begin
          if c = '\\' then i := min n (!i + 2)
          else begin
            if c = '"' then in_str := false;
            incr i
          end
        end
        else begin
          (match c with
           | ';' -> in_cmt := true
           | '"' -> in_str := true
           | '(' -> incr depth
           | ')' ->
               decr depth;
               if !depth = 0 then done_ := true
           | _ -> ());
          incr i
        end
      done
    end
    else if !i < n && text.[!i] = '"' then begin
      let in_str = ref true in
      incr i;
      while !in_str && !i < n do
        let c = text.[!i] in
        if c = '\\' then i := min n (!i + 2)
        else begin
          if c = '"' then in_str := false;
          incr i
        end
      done
    end
    else
      while
        !i < n
        &&
        let c = text.[!i] in
        c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r' && c <> ';'
        && c <> '(' && c <> ')'
      do
        incr i
      done;
    String.sub text start (!i - start)
  in
  while !i < n do
    let pre = consume_prefix () in
    if !i >= n then trailing := pre
    else chunks := { prefix = pre; body = scan_form () } :: !chunks
  done;
  (List.rev !chunks, !trailing)

(* ------------------------------------------------------------------ *)
(* Form identity + canonical form.                                    *)
(* ------------------------------------------------------------------ *)

let read_first (body : string) : Pricklypear_lisp.Ast.expr option =
  match Pricklypear_lisp.Reader.read_all body with
  | e :: _ -> Some e
  | [] -> None

(* A stable key for a top-level form: the define name, or "@pkg" for a
   (package …) form. None for an unnamed form (bare docstring/atom). *)
let form_key (body : string) : string option =
  match read_first body with
  | None -> None
  | Some e -> (
      match Pricklypear_lisp.Eval.parse_top_define e with
      | Some { Pricklypear_lisp.Eval.name; _ } -> Some name
      | None -> (
          match e with
          | Pricklypear_lisp.Ast.App
              (Pricklypear_lisp.Ast.Var "package", name_expr :: _) ->
              let s =
                match name_expr with
                | Pricklypear_lisp.Ast.Var s
                | Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VStr s) ->
                    s
                | _ -> "?"
              in
              Some ("@package:" ^ s)
          | _ -> None))

(* Canonical JSON of the first form — comments/whitespace-insensitive,
   so two texts with the same code compare equal. For a define we use
   [parse_top_define]'s BODY (leading-lambda docstring stripped), so a
   docstring moved between the lambda-internal and trailing-define
   positions does not count as a body change. *)
let canon (body : string) : string option =
  match read_first body with
  | None -> None
  | Some e ->
      let e' =
        match Pricklypear_lisp.Eval.parse_top_define e with
        | Some { Pricklypear_lisp.Eval.body; _ } -> body
        | None -> e
      in
      Some (Yojson.Safe.to_string (Pricklypear_lisp.Ast.expr_to_json e'))

let has_comment (body : string) : bool =
  let re = Re.Perl.compile_pat {|;[^;\n]|} in
  Re.execp re body

(* Comments inside [body] (interior, i.e. not in the leading prefix). *)
let count_comments (body : string) : int =
  let re = Re.Perl.compile_pat {|;[^;\n]|} in
  List.length (Re.all re body)

(* ------------------------------------------------------------------ *)
(* Merge.                                                             *)
(* ------------------------------------------------------------------ *)

type report = {
  changed : string list;             (* AST differs — took new text *)
  changed_with_comments : string list;  (* …and the OLD text had comments *)
  removed : string list;             (* in old, absent from new *)
  added : string list;               (* new names not previously present *)
  reused : int;                      (* unchanged forms kept verbatim *)
}

let empty_report =
  { changed = []; changed_with_comments = []; removed = []; added = [];
    reused = 0 }

(* True for export-package output (freshly generated, safe to merge). *)
let is_generated (text : string) : bool =
  let rec take n l =
    if n = 0 then []
    else match l with [] -> [] | x :: tl -> x :: take (n - 1) tl
  in
  let head = String.concat "\n" (take 4 (String.split_on_char '\n' text)) in
  let re = Re.Perl.compile_pat {|exported from live branch|} in
  Re.execp re head

(* Merge [new_text] (generator output) into [old_text] (the existing
   vault file). Returns (merged_text, report). *)
let merge ~(old_text : string) ~(new_text : string) : string * report =
  let old_chunks, _ = scan_chunks old_text in
  let new_chunks, new_trailing = scan_chunks new_text in
  let old_map : (string, chunk) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun c ->
      match form_key c.body with
      | Some k when not (Hashtbl.mem old_map k) -> Hashtbl.replace old_map k c
      | _ -> ())
    old_chunks;
  let new_keys = Hashtbl.create 256 in
  let changed = ref [] and changed_c = ref [] and added = ref [] in
  let reused = ref 0 in
  let buf = Buffer.create (String.length new_text + 8192) in
  List.iteri
    (fun idx (nc : chunk) ->
      match form_key nc.body with
      | None ->
          (* unnamed form: emit new as-is *)
          Buffer.add_string buf nc.prefix;
          Buffer.add_string buf nc.body
      | Some k ->
          Hashtbl.replace new_keys k ();
          (match Hashtbl.find_opt old_map k with
           | Some oc when canon oc.body = canon nc.body ->
               (* unchanged: keep the OLD text verbatim (comments + fmt).
                  For the first chunk use the NEW prefix so the branch
                  header stays fresh; later chunks keep their section
                  comments from the old prefix. *)
               (if idx = 0 then Buffer.add_string buf nc.prefix
                else Buffer.add_string buf oc.prefix);
               Buffer.add_string buf oc.body;
               incr reused
           | Some oc ->
               (* changed: take new body; re-attach the old leading
                  prefix (usually a section comment) but the body's own
                  interior comments are gone — report it. *)
               Buffer.add_string buf oc.prefix;
               Buffer.add_string buf nc.body;
               changed := k :: !changed;
               if has_comment oc.body then changed_c := k :: !changed_c
           | None ->
               Buffer.add_string buf nc.prefix;
               Buffer.add_string buf nc.body;
               added := k :: !added))
    new_chunks;
  Buffer.add_string buf new_trailing;
  let removed =
    Hashtbl.fold
      (fun k _ acc ->
        if Hashtbl.mem new_keys k then acc
        else if String.length k > 8 && String.sub k 0 8 = "@package" then acc
        else k :: acc)
      old_map []
  in
  ( Buffer.contents buf,
    { changed = List.rev !changed
    ; changed_with_comments = List.rev !changed_c
    ; removed = List.rev removed
    ; added = List.rev !added
    ; reused = !reused } )
