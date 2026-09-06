(* HTML/hypermedia primitives: render-html/status/redirect sentinels,
   html escape/decode, wiki-link + bible rendering, prose/render
   tokens, xml-parse, hx attribute helpers. Split from prims_pure.ml
   (i-1788671384). *)
open Pricklypear_lisp.Ast
open Prims_common


(* (render-html html-str) -> VStr tagged-for-raw-rendering.
   Marks the returned VStr as raw HTML so http.ml renders it
   unescaped (instead of pretty-printing the value inside <pre>).
   The tag is a private prefix sentinel; http.ml checks for it. *)
let html_sentinel = "__html__:"
let redirect_sentinel = "__redirect__:"
let status_sentinel = "__status__:"

let prim_render_html args =
  match args with
  | [ VStr s ] -> VStr (html_sentinel ^ s)
  | _ -> raise (Interp_error (User_error ("render-html needs 1 string", "bad-args")))

(* Expose the sentinel so http.ml can import it (single source of
   truth for the marker). *)
let is_raw_html (v : value) : string option =
  match v with
  | VStr s when String.length s >= String.length html_sentinel
              && String.sub s 0 (String.length html_sentinel) = html_sentinel ->
      Some (String.sub s (String.length html_sentinel)
              (String.length s - String.length html_sentinel))
  | _ -> None

let redirect_value (path : string) : value =
  (* Single funnel for redirect targets (the redirect prim is its only
     caller). Reject CR/LF: httpun does not validate header values, so a
     newline in the target would let a handler inject response headers. *)
  if String.contains path '\r' || String.contains path '\n' then
    raise
      (Interp_error
         (User_error ("redirect target must not contain CR or LF", "bad-args")))
  else VStr (redirect_sentinel ^ path)

let is_redirect (v : value) : string option =
  match v with
  | VStr s when String.length s >= String.length redirect_sentinel
              && String.sub s 0 (String.length redirect_sentinel)
                 = redirect_sentinel ->
      Some (String.sub s (String.length redirect_sentinel)
              (String.length s - String.length redirect_sentinel))
  | _ -> None

(* A handler can signal a non-200 response by returning a status-tagged
   value: __status__:<code>\n<body>. The OCaml layer splits on the first
   newline and responds with that status + body. Used by op-seq rejection
   (409) and other handler-level error responses. Spec: structural-editing.
   client-eval OP-SEQ. *)
let status_response ~(code : int) ~(body : string) : value =
  VStr (status_sentinel ^ string_of_int code ^ "\n" ^ body)

(* (status-response code body) -> VStr tagged for non-200 response.
   Nopales-facing prim wrapper around status_response. *)
let prim_status_response args =
  match args with
  | [ VInt code; VStr body ] ->
      status_response ~code:(Int64.to_int code) ~body
  | [ VInt code; body ] ->
      status_response ~code:(Int64.to_int code)
        ~body:(coerce_str ~ctx:"status-response" body)
  | _ -> raise (Interp_error (User_error ("status-response needs code + body", "bad-args")))

let is_status (v : value) : (int * string) option =
  match v with
  | VStr s when String.length s >= String.length status_sentinel
              && String.sub s 0 (String.length status_sentinel)
                 = status_sentinel ->
      let rest =
        String.sub s (String.length status_sentinel)
          (String.length s - String.length status_sentinel)
      in
      (try
         let nl = String.index rest '\n' in
         let code = int_of_string (String.sub rest 0 nl) in
         let body = String.sub rest (nl + 1) (String.length rest - nl - 1) in
         Some (code, body)
       with _ -> None)
  | _ -> None

(* (html-escape s) -> VStr. Escape amp lt gt quote apos for HTML text/attrs. *)
let rec prim_html_escape args =
  match args with
  | [ VStr s ] ->
      let buf = Buffer.create (String.length s + 8) in
      String.iter
        (function
          | '&' -> Buffer.add_string buf "&amp;"
          | '<' -> Buffer.add_string buf "&lt;"
          | '>' -> Buffer.add_string buf "&gt;"
          | '"' -> Buffer.add_string buf "&quot;"
          | '\'' -> Buffer.add_string buf "&#39;"
          | c -> Buffer.add_char buf c)
        s;
      VStr (Buffer.contents buf)
  | [ v ] ->
      (* coerce then escape *)
      let s =
        match v with
        | VStr s -> s
        | VInt n -> Int64.to_string n
        | VBool true -> "true"
        | VBool false -> "false"
        | VNil -> ""
        | _ -> Pricklypear_lisp.Reader.pp_value v
      in
      prim_html_escape [ VStr s ]
  | _ -> raise (Interp_error (User_error ("html-escape needs 1 value", "bad-args")))

(* (hx-get url) -> VStr tagged attribute helper (lightweight; authors may
   also embed Hx attrs inside render-html). *)
let prim_hx_get args =
  match args with
  | [ VStr url ] -> VStr ("hx-get:" ^ url)
  | _ -> raise (Interp_error (User_error ("hx-get needs a string url", "bad-args")))

let prim_hx_post args =
  match args with
  | [ VStr url ] -> VStr ("hx-post:" ^ url)
  | _ -> raise (Interp_error (User_error ("hx-post needs a string url", "bad-args")))

(* ---- extract-links: OCaml wiki-link scanner (budget P1) ----

   (extract-links body-string) -> list of (slug label) pairs.
   Parses [[slug]] and [[slug|label]] left-to-right.
   Empty targets ([[]]) are skipped. Unclosed [[ is ignored (no hang).
   Media syntax [[media:uuid|alt]] is included (caller may filter);
   roam/insert-collected already skips media: via roam/media-slug?.

   Char-by-char Lisp recursion was ~4-6 steps/char and blew the 100k
   budget on ~2-4k bodies. This prim is O(n) in OCaml = 1 budget tick. *)

let extract_links_from_string (s : string) : (string * string) list =
  let len = String.length s in
  let rec find_close i =
    if i >= len - 1 then None
    else if s.[i] = ']' && s.[i + 1] = ']' then Some i
    else find_close (i + 1)
  in
  let split_label inner =
    let inner = String.trim inner in
    match String.index_opt inner '|' with
    | None -> (inner, inner)
    | Some p ->
        let slug = String.trim (String.sub inner 0 p) in
        let lab =
          String.trim
            (String.sub inner (p + 1) (String.length inner - p - 1))
        in
        (slug, lab)
  in
  let rec loop i acc =
    if i >= len - 3 then List.rev acc
    else if s.[i] = '[' && s.[i + 1] = '[' then
      match find_close (i + 2) with
      | None -> loop (i + 1) acc
      | Some end_i ->
          let inner = String.sub s (i + 2) (end_i - (i + 2)) in
          let slug, lab = split_label inner in
          if slug = "" then loop (end_i + 2) acc
          else loop (end_i + 2) ((slug, lab) :: acc)
    else loop (i + 1) acc
  in
  loop 0 []

let prim_extract_links args =
  let of_body body =
    let pairs = extract_links_from_string body in
    VList
      (List.map
         (fun (slug, lab) -> VList [ VStr slug; VStr lab ])
         pairs)
  in
  match args with
  | [ VStr body ] -> of_body body
  | [ VNil ] -> VList []
  | [ other ] ->
      let body =
        match other with
        | VStr s -> s
        | VInt n -> Int64.to_string n
        | VFloat f -> string_of_float f
        | VBool true -> "true"
        | VBool false -> "false"
        | VNil -> ""
        | _ ->
            raise
              (Interp_error
                 (User_error ("extract-links needs a string body", "bad-args")))
      in
      of_body body
  | _ ->
      raise
        (Interp_error (User_error ("extract-links needs one string arg", "bad-args")))

(* ---- render-links: OCaml wiki/media body → HTML (budget view path) ----

   (render-links body-string) -> HTML string matching roam/render-body:
   plain text html-escaped; [[slug]]/[[slug|label]] → notes anchors;
   [[media:uuid|alt]] → linked img + caption. Unclosed [[ escapes one
   char and continues. Empty [[]] emits escaped brackets. *)

let html_escape_to_buffer buf s =
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | c -> Buffer.add_char buf c)
    s

let html_escape_str s =
  let buf = Buffer.create (String.length s + 8) in
  html_escape_to_buffer buf s;
  Buffer.contents buf

let media_prefix = "media:"

let is_media_slug slug =
  let n = String.length media_prefix in
  String.length slug >= n && String.sub slug 0 n = media_prefix

let media_id_of slug =
  let n = String.length media_prefix in
  String.sub slug n (String.length slug - n)

(* Parse (bible BOOK ch [verse] [verse-to] ["label"]) starting at '('. Returns
   Some (end_exclusive, book, chapter, verse_opt, label). verse_opt is "" if
   chapter-only. label defaults to a human ref. *)
let parse_bible_sexp_at (s : string) (start : int) :
    (int * string * string * string * string) option =
  let len = String.length s in
  if start >= len || s.[start] <> '(' then None
  else
    let skip_ws i =
      let rec go i =
        if i < len && (s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n' || s.[i] = '\r')
        then go (i + 1)
        else i
      in
      go i
    in
    let i0 = skip_ws (start + 1) in
    if i0 + 5 > len then None
    else if String.lowercase_ascii (String.sub s i0 5) <> "bible" then None
    else
      let after_kw = i0 + 5 in
      let ok_kw_end =
        after_kw >= len
        ||
        let c = s.[after_kw] in
        c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '"' || c = ')'
        || (c >= '0' && c <= '9')
      in
      if not ok_kw_end then None
      else
        let parse_token i =
          let i = skip_ws i in
          if i >= len then None
          else if s.[i] = ')' then Some (`Close, i + 1)
          else if s.[i] = '"' then
            let rec find_q j =
              if j >= len then None
              else if s.[j] = '\\' && j + 1 < len then find_q (j + 2)
              else if s.[j] = '"' then Some j
              else find_q (j + 1)
            in
            (match find_q (i + 1) with
             | None -> None
             | Some q ->
                 let raw = String.sub s (i + 1) (q - i - 1) in
                 Some (`Str raw, q + 1))
          else
            let rec end_atom j =
              if j >= len then j
              else
                match s.[j] with
                | ' ' | '\t' | '\n' | '\r' | ')' | '"' -> j
                | _ -> end_atom (j + 1)
            in
            let j = end_atom i in
            if j = i then None
            else Some (`Atom (String.sub s i (j - i)), j)
        in
        let rec gather i acc =
          match parse_token i with
          | None -> None
          | Some (`Close, i') -> Some (List.rev acc, i')
          | Some (`Str v, i') | Some (`Atom v, i') -> gather i' (v :: acc)
        in
        match gather after_kw [] with
        | None -> None
        | Some (args, end_i) ->
            (match args with
             | book :: ch :: rest when book <> "" && ch <> "" ->
                 let verse, label =
                   match rest with
                   | [] -> ("", book ^ " " ^ ch)
                   | [ v ] ->
                       (* verse or label-only? prefer verse if numeric *)
                       let numeric =
                         String.length v > 0
                         && String.for_all (function '0' .. '9' -> true | _ -> false) v
                       in
                       if numeric then (v, book ^ " " ^ ch ^ ":" ^ v)
                       else ("", v)
                   | v :: lab :: _ ->
                       let numeric =
                         String.length v > 0
                         && String.for_all (function '0' .. '9' -> true | _ -> false) v
                       in
                       if numeric then
                         let lab =
                           if lab = "" then book ^ " " ^ ch ^ ":" ^ v else lab
                         in
                         (v, lab)
                       else ("", v)
                 in
                 Some (end_i, book, ch, verse, label)
             | _ -> None)

let render_links_from_string (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create (len + 64) in
  let rec find_close i =
    if i >= len - 1 then None
    else if s.[i] = ']' && s.[i + 1] = ']' then Some i
    else find_close (i + 1)
  in
  let split_label inner =
    let inner = String.trim inner in
    match String.index_opt inner '|' with
    | None -> (inner, inner)
    | Some p ->
        let slug = String.trim (String.sub inner 0 p) in
        let lab =
          String.trim
            (String.sub inner (p + 1) (String.length inner - p - 1))
        in
        (slug, lab)
  in
  let emit_wiki slug lab =
    Buffer.add_string buf "<a href=\"/notes?slug=";
    html_escape_to_buffer buf slug;
    Buffer.add_string buf "\">";
    html_escape_to_buffer buf lab;
    Buffer.add_string buf "</a>"
  in
  let emit_bible book ch verse lab =
    Buffer.add_string buf "<a class=\"bible-ref\" href=\"/dabar?book=";
    html_escape_to_buffer buf book;
    Buffer.add_string buf "&chapter=";
    html_escape_to_buffer buf ch;
    if verse <> "" then begin
      Buffer.add_string buf "&verse=";
      html_escape_to_buffer buf verse
    end;
    Buffer.add_string buf "\">";
    html_escape_to_buffer buf lab;
    Buffer.add_string buf "</a>"
  in
  let emit_media id alt =
    let href = "/media/" ^ id in
    let label = if alt = "" then id else alt in
    Buffer.add_string buf "<a href=\"";
    html_escape_to_buffer buf href;
    Buffer.add_string buf "\"><img src=\"";
    html_escape_to_buffer buf href;
    Buffer.add_string buf "\" alt=\"";
    html_escape_to_buffer buf label;
    Buffer.add_string buf "\" class=\"note-media\"></a>";
    Buffer.add_string buf "<p class=\"muted note-media-cap\"><a href=\"";
    html_escape_to_buffer buf href;
    Buffer.add_string buf "\">";
    html_escape_to_buffer buf label;
    Buffer.add_string buf "</a></p>"
  in
  let at_open i =
    i < len - 1 && s.[i] = '[' && s.[i + 1] = '['
  in
  let rec loop i =
    if i >= len then ()
    else if at_open i then
      match find_close (i + 2) with
      | None ->
          html_escape_to_buffer buf (String.sub s i 1);
          loop (i + 1)
      | Some end_i ->
          let inner = String.sub s (i + 2) (end_i - (i + 2)) in
          let slug, lab = split_label inner in
          if slug = "" then begin
            html_escape_to_buffer buf
              (String.sub s i (end_i + 2 - i));
            loop (end_i + 2)
          end else if is_media_slug slug then begin
            emit_media (media_id_of slug) lab;
            loop (end_i + 2)
          end else begin
            emit_wiki slug lab;
            loop (end_i + 2)
          end
    else
      match parse_bible_sexp_at s i with
      | Some (end_i, book, ch, verse, lab) ->
          emit_bible book ch verse lab;
          loop end_i
      | None ->
          let j = ref i in
          while !j < len && not (at_open !j) && s.[!j] <> '(' do
            incr j
          done;
          (* If we stopped on '(', try bible; else emit through j. If '(' but
             not bible, emit the paren and continue. *)
          if !j > i then begin
            html_escape_to_buffer buf (String.sub s i (!j - i));
            loop !j
          end else if s.[i] = '(' then begin
            html_escape_to_buffer buf "(";
            loop (i + 1)
          end else
            loop (i + 1)
  in
  loop 0;
  Buffer.contents buf

let prim_render_links args =
  let of_body body = VStr (render_links_from_string body) in
  match args with
  | [ VStr body ] -> of_body body
  | [ VNil ] -> VStr ""
  | [ other ] ->
      let body =
        match other with
        | VStr s -> s
        | VInt n -> Int64.to_string n
        | VFloat f -> string_of_float f
        | VBool true -> "true"
        | VBool false -> "false"
        | VNil -> ""
        | _ ->
            raise
              (Interp_error
                 (User_error ("render-links needs a string body", "bad-args")))
      in
      of_body body
  | _ ->
      raise
        (Interp_error (User_error ("render-links needs one string arg", "bad-args")))

(** habitat-book web-renderer: prose tokenization for binding-token
    links. Both prims are ONE OCaml scan over the prose (Buffer + direct
    char tests, zero interpreter steps) — the interpreted per-word
    pipeline they replace (book/split-piece + spec/token-binding? per
    word in libs/book) cost ~300 steps per word, so a 12.7k-word chapter
    needed ~4M steps against a 100k page budget. Resolution stays in
    Nopales (spec/binding-resolves? via function-source) but at
    O(unique tokens), not O(words).

    (prose-tokens s) -> list of UNIQUE token-shaped cores in first-
    occurrence order. Whitespace runs (space/tab/newline) separate
    words; at most one leading char from ( [ ' , and one trailing char
    from ) ] , . ; : is stripped as edge punctuation (the retired
    book/split-piece sets, minus \n which is now a separator). The
    token-shape test MIRRORS spec/token-binding? (len>2, contains "/",
    not "->", not "/", no space, not leading "/") — spec/ keeps the
    authoritative definition; keep the two in lockstep (parity-tested
    in image/test/primitives_test.ml). *)
let token_shape (core : string) : bool =
  String.length core > 2
  && String.contains core '/'
  && core <> "->"
  && core <> "/"
  && not (String.contains core ' ')
  && core.[0] <> '/'

let prose_token_edges (s : string) : string * string * string =
  let is_lead = function '(' | '[' | '\'' | ',' -> true | _ -> false in
  let is_trail = function ')' | ']' | ',' | '.' | ';' | ':' -> true | _ -> false in
  let n = String.length s in
  let lead = if n > 0 && is_lead s.[0] then 1 else 0 in
  let s1 = if lead = 1 then String.sub s 1 (n - 1) else s in
  let n1 = String.length s1 in
  let trail = if n1 > 0 && is_trail s1.[n1 - 1] then 1 else 0 in
  (String.sub s 0 lead,
   (if n1 - trail > 0 then String.sub s1 0 (n1 - trail) else ""),
   (if trail = 1 then String.sub s1 (n1 - 1) 1 else ""))

let is_word_char = function
  | ' ' | '\t' | '\n' | '\r' -> false
  | _ -> true

let prim_prose_tokens args =
  let s = coerce_str ~ctx:"prose-tokens" (match args with [ v ] -> v | _ -> raise (Interp_error (User_error ("prose-tokens needs one string", "bad-args")))) in
  let seen = Hashtbl.create 64 in
  let out = ref [] in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if is_word_char s.[!i] then begin
      let j = ref !i in
      while !j < n && is_word_char s.[!j] do incr j done;
      let _, core, _ = prose_token_edges (String.sub s !i (!j - !i)) in
      if core <> "" && token_shape core && not (Hashtbl.mem seen core) then begin
        Hashtbl.add seen core ();
        out := VStr core :: !out
      end;
      i := !j
    end else incr i
  done;
  VList (List.rev !out)

(** (render-tokens s resolved unresolved) -> HTML string for one prose
    paragraph body. Same word scan as prose-tokens; each word renders as
    escape(lead) ++ core-html ++ escape(trail) where core-html is:
      - core in `resolved`   -> <a href="/binding/CORE">CORE</a>
      - core in `unresolved` -> <span class="book-hole">CORE&#8709;</span>
      - otherwise (incl. empty core / non-token-shaped word) -> the whole
        word escaped verbatim
    Inter-word whitespace is preserved verbatim. Presentation constants
    (/binding/ href, book-hole class) mirror render-links' kernel-side
    anchors (/notes?slug=); the book package owns the .book-hole CSS. *)
let prim_render_tokens args =
  match args with
  | [ s_v; resolved_v; unresolved_v ] ->
      let s = coerce_str ~ctx:"render-tokens" s_v in
      let strset vals =
        let h = Hashtbl.create 64 in
        List.iter (fun v -> Hashtbl.replace h (coerce_str ~ctx:"render-tokens" v) ()) vals;
        h
      in
      let resolved = strset (list_of_value resolved_v) in
      let unresolved = strset (list_of_value unresolved_v) in
      let buf = Buffer.create (String.length s + 16) in
      let n = String.length s in
      let i = ref 0 in
      while !i < n do
        if is_word_char s.[!i] then begin
          let j = ref !i in
          while !j < n && is_word_char s.[!j] do incr j done;
          let word = String.sub s !i (!j - !i) in
          let lead, core, trail = prose_token_edges word in
          if core <> "" && token_shape core && Hashtbl.mem resolved core then begin
            html_escape_to_buffer buf lead;
            Buffer.add_string buf "<a href=\"/binding/";
            html_escape_to_buffer buf core;
            Buffer.add_string buf "\">";
            html_escape_to_buffer buf core;
            Buffer.add_string buf "</a>";
            html_escape_to_buffer buf trail
          end
          else if core <> "" && token_shape core && Hashtbl.mem unresolved core then begin
            html_escape_to_buffer buf lead;
            Buffer.add_string buf "<span class=\"book-hole\">";
            html_escape_to_buffer buf core;
            Buffer.add_string buf "&#8709;</span>";
            html_escape_to_buffer buf trail
          end
          else html_escape_to_buffer buf word;
          i := !j
        end else begin
          Buffer.add_char buf s.[!i];
          incr i
        end
      done;
      VStr (Buffer.contents buf)
  | _ ->
      raise (Interp_error (User_error ("render-tokens needs (s resolved unresolved)", "bad-args")))

(* ---- html-decode (entity decode) ----

   Handles &amp; &lt; &gt; &quot; &apos; &#NN; &#xHH; and common named entities.
   Best-effort; unknown entities are left as-is. *)

(* All replacement values are UTF-8 byte sequences: Latin-1 range
   entities (U+00A0..U+00BB) are two-byte C2-lead sequences, the rest
   are the standard three-byte forms. Single-byte \169/\240 etc. are
   invalid UTF-8 and once leaked into rendered pages. *)
let named_entities =
  [ "amp", "&"; "lt", "<"; "gt", ">"; "quot", "\""; "apos", "'";
    "nbsp", "\194\160"; "copy", "\194\169"; "reg", "\194\174";
    "mdash", "\226\128\148"; "ndash", "\226\128\147";
    "hellip", "\226\128\166"; "rsquo", "\226\128\153"; "lsquo", "\226\128\152";
    "ldquo", "\226\128\156"; "rdquo", "\226\128\157"; "euro", "\226\130\172";
    "trade", "\226\132\162"; "deg", "\194\176"; "plusmn", "\194\177";
    "times", "\195\151"; "divide", "\195\183"; "bull", "\226\128\162";
    "middot", "\194\183"; "laquo", "\194\171"; "raquo", "\194\187" ]

let prim_html_decode args =
  let decode s =
    let len = String.length s in
    let buf = Buffer.create len in
    let lookup_named name =
      try Some (List.assoc name named_entities) with Not_found -> None
    in
    let rec loop i =
      if i >= len then ()
      else if s.[i] <> '&' then begin Buffer.add_char buf s.[i]; loop (i + 1) end
      else begin
        (* find ';' within a reasonable window *)
        let rec find_semi j =
          if j >= len || j > i + 12 then None
          else if s.[j] = ';' then Some j else find_semi (j + 1)
        in
        match find_semi (i + 1) with
        | None -> Buffer.add_char buf '&'; loop (i + 1)
        | Some e ->
            let body = String.sub s (i + 1) (e - i - 1) in
            let decoded =
              if String.length body >= 1 && body.[0] = '#' then
                let num = String.sub body 1 (String.length body - 1) in
                let n =
                  if String.length num >= 1 && (num.[0] = 'x' || num.[0] = 'X')
                  then
                    (try Some (int_of_string ("0x" ^ String.sub num 1 (String.length num - 1)))
                     with _ -> None)
                  else (try Some (int_of_string num) with _ -> None)
                in
                (match n with
                 | Some n when n >= 0 && n < 0x110000 ->
                     (* encode as UTF-8 *)
                     let rec utf8 n =
                       if n < 0x80 then String.make 1 (Char.chr n)
                       else if n < 0x800 then
                         String.make 1 (Char.chr (0xC0 lor (n lsr 6)))
                         ^ String.make 1 (Char.chr (0x80 lor (n land 0x3F)))
                       else if n < 0x10000 then
                         String.make 1 (Char.chr (0xE0 lor (n lsr 12)))
                         ^ String.make 1 (Char.chr (0x80 lor ((n lsr 6) land 0x3F)))
                         ^ String.make 1 (Char.chr (0x80 lor (n land 0x3F)))
                       else
                         String.make 1 (Char.chr (0xF0 lor (n lsr 18)))
                         ^ String.make 1 (Char.chr (0x80 lor ((n lsr 12) land 0x3F)))
                         ^ String.make 1 (Char.chr (0x80 lor ((n lsr 6) land 0x3F)))
                         ^ String.make 1 (Char.chr (0x80 lor (n land 0x3F)))
                     in
                     Some (utf8 n)
                 | _ -> None)
              else lookup_named body
            in
            (match decoded with
             | Some txt -> Buffer.add_string buf txt
             | None -> Buffer.add_string buf ("&" ^ body ^ ";"));
            loop (e + 1)
      end
    in
    loop 0;
    Buffer.contents buf
  in
  match args with
  | [ v ] -> VStr (decode (coerce_str ~ctx:"html-decode" v))
  | _ -> raise (Interp_error (User_error ("html-decode needs 1 string", "bad-args")))

(* ---- xml-parse (markup) ----

   (xml-parse text) -> Lisp tree. Node shape (per note):
     ("tag" (("k" . "v") ...) child1 child2 ...)
   text node = bare VStr (already entity-decoded). comments/PI dropped.
   Namespaces: raw prefixed names preserved verbatim. Tag names are strings
   so string-eq/car walks are safe. On hard failure returns
   ("error" message). *)

(* Coerce a Markup.name (namespace_uri, local_name) to a single string.
   For lenient RSS/Atom with no namespace declaration, the raw prefix is
   preserved in the local part, so snd alone is correct. When a namespace URI
   is present we still return the local name (Lisp-side normalizes prefixes). *)
let xml_name_to_string (_ns : string) (local : string) : string = local

let rec xml_tree_to_value = function
  | `Text s -> VStr s
  | `Elem (name, attrs, children) ->
      let tag = xml_name_to_string (fst name) (snd name) in
      let attr_pairs =
        List.map
          (fun (k, v) ->
            VPair (VStr (xml_name_to_string (fst k) (snd k)), VStr v))
          attrs
      in
      VList (VStr tag :: VList attr_pairs :: List.map xml_tree_to_value children)

let prim_xml_parse args =
  let parse s =
    let open Markup in
    try
      let parsed =
        s |> string |> parse_xml |> signals
        |> tree
            ~text:(fun ss -> `Text (String.concat "" ss))
            ~element:(fun name attrs children ->
              `Elem (name, attrs, children))
      in
      match parsed with
      | None -> VList [ VStr "error"; VStr "empty document" ]
      | Some t -> xml_tree_to_value t
    with exn ->
      VList [ VStr "error"; VStr (Printexc.to_string exn) ]
  in
  match args with
  | [ v ] -> parse (coerce_str ~ctx:"xml-parse" v)
  | _ -> raise (Interp_error (User_error ("xml-parse needs 1 string", "bad-args")))
