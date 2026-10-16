(** Files as PROJECTIONS of the def store (branching.file-bridge, spec commit
    3f4d6d1, P1): a local file is a manifest of defs + raw spans. Editing the
    file and eval'ing in-image are the same op stream — this module owns the
    pure file side: parse a projection file into def slots + raw spans,
    materialize back BYTE-IDENTICALLY, and build a projection from a branch
    def set (no spans).

    FORMAT: a leading header-manifest comment block (the parser owns it),
    then per def in manifest order the def's source text (byte-exact as
    parsed) with the raw span(s) preceding it (comments, reader gunk,
    non-def top-level forms) attached to that def's slot. Raw spans are
    OWNED BY THE FILE: the image never generates them, the projection never
    drops them.

    ROUND-TRIP PROPERTIES (property-tested in file_projection_test.ml):
      materialize (parse f) = f                      (byte-identical)
      parse (materialize p) = p                      (spans preserved)
      parse (materialize (of_defs defs)) = of_defs defs

    CLASSIFICATION: a top-level unit is a DEF when the reader parses it as
    (define ...); everything else — comments, whitespace, bare atoms,
    stray forms, unparsable units — is a raw span. Unbalanced parens are
    a file-level parse Error, never a silent raw span.

    Pure: no Postgres, no interpreter state — Reader + Ast only. The
    ingest server call (P2) lives in the image lib; this module is the
    client-side format the pp-local transport reads and writes. *)

type def_slot = {
  d_name : string;    (* defined name *)
  d_body : string;    (* the def's source text, byte-exact as parsed *)
  d_before : string;  (* raw span(s) between the previous def and this one *)
}

type projection = {
  p_branch : string;     (* manifest header: branch ref *)
  p_base_gen : Int64.t;  (* manifest header: the sync coordinate *)
  p_version : string;    (* manifest header: projection format version *)
  p_header : string;     (* raw before the first def, incl. the manifest *)
  p_defs : def_slot list;
  p_trailing : string;   (* raw after the last def *)
}

let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(* The matching ')' for the '(' at [open_i] — string-literal and line-comment
   aware (mirrors reader.ml's syntax: no block comments, no char literals). *)
let find_close (s : string) (open_i : int) : (int, string) result =
  let len = String.length s in
  let depth = ref 0 in
  let i = ref open_i in
  let rec skip_string j =
    if j >= len then Error "unterminated string literal"
    else
      match s.[j] with
      | '"' -> Ok (j + 1)
      | '\\' -> if j + 1 >= len then Error "unterminated escape" else skip_string (j + 2)
      | _ -> skip_string (j + 1)
  in
  let rec loop () =
    if !i >= len then Error "unbalanced parens: no matching close"
    else
      match s.[!i] with
      | '"' ->
          (match skip_string (!i + 1) with
           | Ok j ->
               i := j;
               loop ()
           | Error e -> Error e)
      | ';' ->
          while !i < len && s.[!i] <> '\n' do incr i done;
          loop ()
      | '(' ->
          incr depth;
          incr i;
          loop ()
      | ')' ->
          decr depth;
          if !depth = 0 then Ok !i
          else begin
            incr i;
            loop ()
          end
      | _ ->
          incr i;
          loop ()
  in
  loop ()

(* Scan-level structural failure: unbalanced parens / unterminated string.
   A file-level parse Error — never silently demoted to a raw span. *)
exception Scan_error of string
(* Split [s] into top-level units: (raw_before, form) in order, plus the
   trailing raw. [form] is the byte-exact text of one balanced unit. *)
let scan_units (s : string) : (string * string) list * string =
  let len = String.length s in
  let raw = Buffer.create 256 in
  let units = ref [] in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    if c = ';' then begin
      (* line comment: consume through the newline into the raw span *)
      let j = ref !i in
      while !j < len && s.[!j] <> '\n' do incr j done;
      if !j < len then incr j;
      Buffer.add_substring raw s !i (!j - !i);
      i := !j
    end
    else if is_ws c then begin
      Buffer.add_char raw c;
      incr i
    end
    else if c = '(' then begin
      match find_close s !i with
      | Ok close ->
          let unit = String.sub s !i (close - !i + 1) in
          units := (Buffer.contents raw, unit) :: !units;
          Buffer.clear raw;
          i := close + 1
        | Error why -> raise (Scan_error why)
    end
    else begin
      (* top-level bare atom: never a define — raw span *)
      let j = ref !i in
      while !j < len && not (is_ws s.[!j]) && s.[!j] <> ';' && s.[!j] <> '(' do
        incr j
      done;
      Buffer.add_substring raw s !i (!j - !i);
      i := !j
    end
  done;
  (List.rev !units, Buffer.contents raw)

(* The name a top-level unit defines, if it is a define at all. Unparsable
   units are raw spans, never errors (the opaque-body decode-guard policy). *)
let define_name (unit_text : string) : string option =
  match Reader.read_one unit_text with
  | Ast.Define (name, _) -> Some name
  (* The READER produces plain App forms — Ast.Define is the STORE's body-json
     codec. Classify defines structurally: (define name ...) and
     (define (name params...) ...), plus defmacro (the pp-sync blind spot). *)
  | Ast.App
      (Ast.Var ("define" | "defmacro"), Ast.App (Ast.Var name, _) :: _) ->
      Some name
  | Ast.App (Ast.Var ("define" | "defmacro"), Ast.Var name :: _) -> Some name
  | _ -> None
  | exception Reader.Reader_error _ -> None

(* "branch:", "base-gen:", "projection:" from a raw header block. *)
let grab_key (key : string) (raw : string) : string option =
  let klen = String.length key in
  let rlen = String.length raw in
  let rec go i =
    if i + klen >= rlen then None
    else if String.sub raw i klen = key && raw.[i + klen] = ':' then begin
      let j = ref (i + klen + 1) in
      while !j < rlen && raw.[!j] = ' ' do incr j done;
      let k = ref !j in
      while !k < rlen && not (is_ws raw.[!k]) do incr k done;
      if !k > !j then Some (String.sub raw !j (!k - !j)) else None
    end
    else go (i + 1)
  in
  go 0

let parse (s : string) : (projection, string) result =
  match scan_units s with
    | exception Scan_error why -> Error why
  | exception Reader.Reader_error (msg, _, _) ->
      Error ("scan failed: " ^ msg)
  | units, trailing ->
      let defs, before_acc =
        List.fold_left
          (fun (acc, raw_acc) (raw_before, unit) ->
             match define_name unit with
             | Some name ->
                 ( { d_name = name;
                     d_body = unit;
                     d_before = raw_acc ^ raw_before }
                   :: acc,
                   "" )
             | None ->
                 (* raw span: comment block, atom, or stray form *)
                 (acc, raw_acc ^ raw_before ^ unit))
          ([], "") units
      in
      let defs = List.rev defs in
      (match defs with
       | [] ->
           Ok
             {
               p_branch = "";
               p_base_gen = 0L;
               p_version = "";
               p_header = before_acc ^ trailing;
               p_defs = [];
               p_trailing = "";
             }
       | first :: _ ->
           Ok
             {
               p_branch = Option.value (grab_key "branch" first.d_before) ~default:"";
               p_base_gen =
                 Option.value
                   (Option.bind (grab_key "base-gen" first.d_before)
                      Int64.of_string_opt)
                   ~default:0L;
               p_version =
                 Option.value (grab_key "projection" first.d_before) ~default:"";
               p_header = first.d_before;
                p_defs =
                  (match defs with
                   | f :: rest -> { f with d_before = "" } :: rest
                   | [] -> []);
                p_trailing = before_acc ^ trailing;
             })

(* materialize reconstructs the file byte-identically. *)
let materialize (p : projection) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf p.p_header;
  List.iter
    (fun (d : def_slot) ->
       Buffer.add_string buf d.d_before;
       Buffer.add_string buf d.d_body)
    p.p_defs;
  Buffer.add_string buf p.p_trailing;
  Buffer.contents buf

let def_names (p : projection) : string list =
  List.map (fun (d : def_slot) -> d.d_name) p.p_defs

(* Build a projection from a branch def set (name, canonical body text):
   the generated manifest is the ONLY leading raw; no spans. *)
let of_defs ~(branch : string) ~(base_gen : Int64.t)
    (defs : (string * string) list) : projection =
  let version = "v1" in
  let header =
    Printf.sprintf ";;; branch: %s   base-gen: %Ld   projection: %s\n\n" branch
      base_gen version in
  {
    p_branch = branch;
    p_base_gen = base_gen;
    p_version = version;
    p_header = header;
    p_defs =
      List.map (fun (n, b) -> { d_name = n; d_body = b; d_before = "" }) defs;
    p_trailing = "\n";
  }

let header (p : projection) : (string * Int64.t * string) =
  (p.p_branch, p.p_base_gen, p.p_version)
