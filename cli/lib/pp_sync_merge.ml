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

(* Read the first form. NEVER RAISES: [read_all] can fail on partial
   input and [parse_top_define] inside callers can reject malformed
   defines; both degrade to None so a single weird HAND form cannot
   crash a whole snapshot run. *)
let read_first (body : string) : Pricklypear_lisp.Ast.expr option =
  match (try Pricklypear_lisp.Reader.read_all body with _ -> []) with
  | e :: _ -> (try Some e with _ -> None)
  | [] -> None

(* A stable key for a top-level form: the define name, or "@pkg" for a
   (package …) form. None for an unnamed form (bare docstring/atom). *)
let form_key (body : string) : string option =
  try
    match read_first body with
    | None -> None
    | Some e ->
      (match Pricklypear_lisp.Eval.parse_top_define e with
       | Some { Pricklypear_lisp.Eval.name; _ } -> Some name
       | None ->
         (match e with
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
  with _ -> None

(* Canonical JSON of the first form — comments/whitespace-insensitive,
   so two texts with the same code compare equal. For a define we use
   [parse_top_define]'s BODY (leading-lambda docstring stripped), so a
   docstring moved between the lambda-internal and trailing-define
   positions does not count as a body change. *)
let canon (body : string) : string option =
  match read_first body with
  | None -> None
  | Some e ->
      (* [parse_top_define] can RAISE on malformed defines; degrade to
         the whole-form canon (no match against a valid old define) rather
         than crash. *)
      let e' =
        (match
           Pricklypear_lisp.Eval.parse_top_define e
         with
         | Some { Pricklypear_lisp.Eval.body; _ } -> body
         | None -> e
         | exception _ -> e)
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
   vault file). Returns (merged_text, report).

   FIRST-PREFIX RULE: for the first chunk the NEW prefix is used when
   the old file is GENERATED (keeps the branch stamp fresh), but a
   NON-generated (hand-authored) file keeps its OLD prefix — a custom
   header (std's "Standard Lisp helpers…") is content, and losing it
   would silently convert curated source into generator output. *)
let merge ~(old_text : string) ~(new_text : string) : string * report =
  let old_chunks, _ = scan_chunks old_text in
  let new_chunks, new_trailing = scan_chunks new_text in
  let old_map : (string, chunk) Hashtbl.t = Hashtbl.create 256 in
  let hand_header = old_text <> "" && not (is_generated old_text) in
  Hashtbl.reset old_map;
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
                  FIRST-PREFIX RULE (idx = 0): generated files take the
                  NEW prefix (branch stamp stays fresh); a hand-authored
                  file keeps its OLD prefix — the custom header IS
                  content. Later chunks keep their section comments
                  from the old prefix. *)
               (if idx = 0 then begin
                  if hand_header then Buffer.add_string buf oc.prefix
                  else Buffer.add_string buf nc.prefix
                end
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

(* ------------------------------------------------------------------ *)
(* The ONE vault-write decision (snapshot / pull / land all go here).  *)
(* ------------------------------------------------------------------ *)

(* Top-level define count by the REAL reader, not a regex: an interior
   comment or string mentioning "(define " miscounts (std has 116
   comment lines) and the guards key off these numbers. [None] = the
   text does not parse, which callers treat as grounded. *)
let count_defines (text : string) : int option =
  match Pp_sync_diff.parse_bodies text with
  | Some bs -> Some (List.length bs)
  | None -> None

(* Names in [old_text] but absent from [new_text] — what a merge would
   DROP from the file. Pure: both texts scanned + form-keyed. *)
let dropped_names ~(old_text : string) ~(new_text : string) : string list =
  let keys text =
    List.filter_map (fun c -> form_key c.body) (fst (scan_chunks text))
  in
  let memo = Hashtbl.create 64 in
  List.iter (fun k -> Hashtbl.replace memo k ()) (keys new_text);
  List.filter (fun k -> not (Hashtbl.mem memo k)) (keys old_text)
  |> List.sort_uniq String.compare

type plan =
  | Refused of string  (* refuse with reason (--force to override) *)
  | Write of
      { text : string   (* what hits disk *)
      ; report : report (* zero-report for a raw write *)
      ; raw : bool     (* generator output verbatim ("force" / new file) *)
      ; hand : bool     (* old file was hand-authored (custom header kept) *)
      }

(* Decide how [new_text] (fresh export-package output) may replace the
   current vault file [old_text] ("" = no file yet). ONE decision point
   for every vault write path - snapshot, pull, and land - so the guards
   cannot differ per verb:
   - old file absent       -> raw write (first capture).
   - DROPPED-NAME ACCOUNTING (2026-10-02 review): the file may drop a
     name only if live main still binds it SOMEWHERE (package re-homing
     - ui/badge moving lookbook->ui - is a legitimate mirror edit).
     Main is APPEND-ONLY (no primitive deletes a define from the
     inheritance root), so a vault name bound nowhere on live main is
     an ANOMALY (export/membership bug, wrong branch) - refused by
     NAME. This replaces reliance on the bare count guard, which
     blocked exactly those legitimate re-homing merges (chat, pads,
     todo, rb all failed on counts while their dropped names live on).
   - old file hand-authored
     - live export has NO defines for the package -> REFUSE (vault-only
       skin / curated source; the generator would erase it).
     - otherwise -> MERGE, old first prefix (custom header) kept
       (the 2026-10-02 case A; std is the real case: curated header +
       real bodies on live main).
   - old file generated -> MERGE (comment-preserving); unlimited
     accounted shrink (the old count guard is replaced by accounting).
   - old file does not PARSE -> REFUSE (never merge into text we
     cannot understand).
   - [~force] -> raw write of the new text, all guards off.
   [~live_names]: the full binding-name set of the compared branch
   ([] = cannot verify -> every drop counts as unaccounted ->
   conservative refusal). The [dry] choice is the caller's: the plan
   is computed first, the caller writes or reports. *)
let plan_write ~(old_text : string) ~(new_text : string) ~live_names
    ~force : plan =
  let human names =
    let visible =
      List.filter (fun n -> String.length n > 0 && n.[0] <> '@') names
    in
    (match visible with
     | [] -> ""
     | _ ->
         Printf.sprintf "%s%s"
           (String.concat ", " (List.filteri (fun n _ -> n < 8) visible))
           (if List.length visible > 8 then ", ..." else ""))
  in
  if String.trim new_text = "" then Refused "empty export"
  else if old_text = "" || String.trim old_text = "" then
    Write { text = new_text; report = empty_report; raw = true; hand = false }
  else if force then
    Write { text = new_text; report = empty_report; raw = true;
            hand = not (is_generated old_text) }
  else
    match count_defines old_text with
    | None ->
        Refused
          "the existing vault file does not parse; will not merge into text \
           I cannot fully read. Fix it by hand or pass --force to overwrite \
           deliberately."
    | Some old_defs ->
      let hand = not (is_generated old_text) in
      match count_defines new_text with
      | None -> Refused "the live export does not parse (partial eval reply?)"
      | Some new_defs ->
      (* gate order = most specific refusal first:
         1. hand-authored file + the live export holds NO defines for
            the package (skins / dead source): the whole package is
            absent live; the drop wording below would mislead.
         2. any name the file would drop that live binds NOWHERE: main
            is append-only (no delete primitive), so this is an
            anomaly, not a mirror edit. Re-homed names (bound under
            another package) pass and merge.
         3. (the historical count guard is GONE — accounting supersedes it)
         4. otherwise: comment-preserving merge (hand files keep their
            custom first prefix). *)
      if hand && new_defs = 0 then
        Refused
          (Printf.sprintf
             "hand-authored vault file (custom header, no 'exported \
              from live branch' stamp) and the live export has ZERO \
              defines for this package (%d on disk) - the generator \
              would erase it. On-demand skins and dead sources stay \
              hand-curated; --force to overwrite deliberately."
             old_defs)
      else
        let dropped = dropped_names ~old_text ~new_text in
        let live_set = Hashtbl.create 512 in
        List.iter (fun k -> Hashtbl.replace live_set k ()) live_names;
        let unaccounted =
          List.filter
            (fun n ->
              String.length n > 0 && n.[0] <> '@' && not (Hashtbl.mem live_set n))
            dropped
        in
        if unaccounted <> [] then
          Refused
            (Printf.sprintf
               "the file would DROP %d name(s) that live main binds \
                NOWHERE - and main is append-only (nothing is ever \
                deleted from it), so this is an anomaly (export/membership \
                bug? wrong branch?), not a mirror edit: %s. Review with \
                `pp-sync status <pkg>`; promote the names if they should \
                ship, or --force AFTER a deliberate deletion."
               (List.length unaccounted) (human unaccounted))
        else begin
          (* 2026-10-02 review: the old count ("gut") guard is GONE. Per-name
             accounting replaced it: a legit cross-family re-homing
             (todo/browse* moving to the filmstrip package) shrinks the
             source file while every drop stays bound live — refused
             under the count rule, an honest mirror edit under
             accounting. The broken/empty-export gut (std/theme 69→0
             when the name-prefix exporter was live) is still caught:
             every dropped name lands in the unaccounted set, because no
             export anywhere carries std's names when the exporter is
             wrong. *)
          let merged, rep = merge ~old_text ~new_text in
          Write { text = merged; report = rep; raw = false;
                  hand = hand && new_defs > 0 }
        end

(* One line of ordinary text summary for a Write plan (the shared
   phrasing of pull / snapshot / land output) — [dry] lines are
   composed by the caller around the same report. *)
let describe_write (w : plan) : string =
  match w with
  | Refused m -> m
  | Write wr ->
      (* raw writes also drop removal knowledge per name — the caller's
         note lines cover the details; keep this a plain summary *)
      Printf.sprintf "%d form(s) kept verbatim, %d changed, %d added"
        wr.report.reused (List.length wr.report.changed)
        (List.length wr.report.added)

(* Total ';' comment lines in a text — the warned-loss counter for raw
   (--force / new-file) overwrites, which have no per-form knowledge. *)
let count_comment_lines (text : string) : int =
  String.split_on_char '\n' text
  |> List.filter (fun l -> String.trim l <> "" && String.contains l ';')
  |> List.length
