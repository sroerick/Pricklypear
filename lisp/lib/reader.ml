(** Pricklypear Lisp — S-expression reader.

    Parses surface s-expression syntax -> Ast.expr. The AST IS the
    source (homoiconicity): a function stored as JSONB is its AST, and
    the s-expr syntax is one of several possible editors (along with
    the htmx tree-editor that edits the AST directly).

    Per spec [lisp-interpreter.reader]:
      - Standard s-expressions: (foo bar baz), 'symbol, "string", 123,
        #t, #f, ()
      - String escapes: named (n/t/r/backslash/quote/apostrophe),
        three-digit decimal byte (\\ddd), unicode scalar (\\uXXXX,
        UTF-8 encoded; lone surrogates rejected); other backslash-X
        is forgiving (literal X)
      - Comments: ; to end of line
      - Docstrings: a leading string in a body position (deferred; reader
        just produces the AST, docstring extraction is eval-time)
      - Special forms [lambda/let/letrec/if/define/quote] are recognized
        at EVAL time, not read time — the reader produces plain App
        nodes; eval dispatches them.

    The reader tracks line/col for error messages. *)
open Ast

(** A parse error: (message, line, col). *)
type reader_error = string * int * int
exception Reader_error of reader_error

(** True for whitespace (space, tab, newline, carriage return). *)
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** True for ASCII digit characters. *)
let is_digit c = c >= '0' && c <= '9'

(** True for characters that may begin a symbol: anything that is not a
    delimiter (paren, quote, double-quote, semicolon) and not whitespace. *)
let is_symbol_start c =
  (c <> '(' && c <> ')' && c <> '\'' && c <> '"'
  && c <> ';' && not (is_space c))

(** True for characters that may continue a symbol (start chars, digits, '#'). *)
let is_symbol_cont c = is_symbol_start c || is_digit c || c = '#'

(** True at a token start. '#' may begin a token for #t/#f (and later
    #\char etc). *)
let is_token_start c = c = '#' || is_symbol_start c

(** True for characters that may continue a token. *)
let is_token_cont c = is_symbol_cont c

(** Reader cursor state: the remaining input, current position, and the
    line/col of the current position (for error messages). *)
type state = {
  mutable input : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

(** [make_state s] builds an initial cursor over [s] at position (1,1). *)
let make_state (s : string) : state =
  { input = s; pos = 0; line = 1; col = 1 }

(** [peek st] returns the next character without consuming it, or [None]
    at end of input. *)
let peek (st : state) : char option =
  if st.pos >= String.length st.input then None
  else Some st.input.[st.pos]

(** [advance st] consumes and returns the next character (or [None] at
    end of input), tracking line/col across newlines. *)
let advance (st : state) : char option =
  match peek st with
  | None -> None
  | Some c ->
      st.pos <- st.pos + 1;
      if c = '\n' then (st.line <- st.line + 1; st.col <- 1)
      else st.col <- st.col + 1;
      Some c

(** [skip_ws_and_comments st] skips whitespace and ';' line comments,
    leaving the cursor at the next significant character (or EOF). *)
let skip_ws_and_comments (st : state) : unit =
  let rec loop () =
    match peek st with
    | None -> ()
    | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
        ignore (advance st); loop ()
    | Some ';' ->
        (* line comment: skip to end of line or EOF, then keep looping
           so the trailing newline (and any further ws/comments) is
           consumed. The old code re-peeked the same ';' without
           advancing, so it only consumed the ';' itself and left the
           rest of the line to be read as a symbol — e.g. a
           `;; package ...` header lexed as the symbol `package`. *)
        (let rec skip_to_eol () =
           match peek st with
           | Some '\n' | None -> ()
           | _ -> ignore (advance st); skip_to_eol ()
         in skip_to_eol (); loop ())
    | _ -> ()
  in
  loop ()

(** True for ASCII digit characters. *)
let is_digit c = c >= '0' && c <= '9'

(** Numeric value of an ASCII digit. *)
let digit_val c = Char.code c - Char.code '0'

(** True for hexadecimal digit characters. *)
let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(** Numeric value of a hexadecimal digit character. *)
let hex_val c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then 10 + Char.code c - Char.code 'a'
  else 10 + Char.code c - Char.code 'A'

(** UTF-8 encode a Unicode scalar value into buf (1–4 bytes).
    Caller must have rejected lone surrogates (D800..DFFF). *)
let utf8_add_scalar (buf : Buffer.t) (n : int) : unit =
  if n < 0x80 then Buffer.add_char buf (Char.chr n)
  else if n < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (n lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (n land 0x3F)))
  end else if n < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (n lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((n lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (n land 0x3F)))
  end else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (n lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((n lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((n lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (n land 0x3F)))
  end

(** Escape a string for Nopales source. Keep UTF-8 bytes as-is so
    round-trip through the reader preserves non-ASCII text. Only quote,
    backslash, and a few controls get named escapes; other controls use
    exactly-three-digit decimal [\\ddd] (OCaml-compatible).
    \\uXXXX is a READ-side escape only — the printer never emits it;
    UTF-8 bytes go out raw and the reader accepts them on the way back. *)
let escape_string (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\%03d" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(** [read_string st] reads a string literal. Assumes the current char is
    the opening quote (which is consumed). Handles named escapes
    (n/t/r/backslash/quote/apostrophe), three-digit decimal bytes
    [\\ddd], and unicode [\\uXXXX] (lone surrogates rejected); any other
    backslash-X is treated as literal X. Raises [Reader_error] on
    unterminated strings or bad escapes. *)
let read_string (st : state) : string =
  (* assumes current char is the opening quote *)
  ignore (advance st);  (* consume the opening quote *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek st with
    | None ->
        raise (Reader_error ("unterminated string", st.line, st.col))
    | Some '"' ->
        ignore (advance st);
        Buffer.contents buf
    | Some '\\' ->
        (* Escape sequences in string literals.
           Named: n t r backslash double-quote single-quote.
           Exactly three decimal digits [\\ddd] → byte (OCaml String.escaped
           form; needed so export/pull vaults with non-ASCII still load).
           Any other backslash-X is treated as literal X (forgiving), so
           editor over-escapes and paste noise do not hard-fail. *)
        ignore (advance st);  (* consume the backslash we peeked *)
        (match peek st with
         | None ->
             raise (Reader_error ("unterminated escape", st.line, st.col))
         | Some 'n' -> ignore (advance st); Buffer.add_char buf '\n'
         | Some 't' -> ignore (advance st); Buffer.add_char buf '\t'
         | Some 'r' -> ignore (advance st); Buffer.add_char buf '\r'
         | Some '\\' -> ignore (advance st); Buffer.add_char buf '\\'
         | Some '"' -> ignore (advance st); Buffer.add_char buf '"'
         | Some '\'' -> ignore (advance st); Buffer.add_char buf '\''
         | Some 'u' ->
             (* \uXXXX unicode scalar (exactly 4 hex digits, UTF-8
                encoded). Lone surrogates are rejected: v1 does not
                combine surrogate pairs. *)
             ignore (advance st);  (* consume the 'u' *)
             let hex4 () =
               match peek st with
               | Some c when is_hex c -> ignore (advance st); hex_val c
               | _ ->
                   raise
                     (Reader_error
                        ("bad unicode escape \\u (need 4 hex digits)",
                         st.line, st.col))
             in
             (* Bind left-to-right: OCaml evaluates + operands right-to-left,
                which would reverse nibble significance (203e → e302). *)
             let h1 = hex4 () in
             let h2 = hex4 () in
             let h3 = hex4 () in
             let h4 = hex4 () in
             let n = (h1 * 4096) + (h2 * 256) + (h3 * 16) + h4 in
             if n >= 0xD800 && n <= 0xDFFF then
               raise
                 (Reader_error
                    (Printf.sprintf "bad unicode escape \\u%04X (lone surrogate)" n,
                     st.line, st.col))
             else utf8_add_scalar buf n
         | Some d1 when is_digit d1 ->
             ignore (advance st);
             (match peek st with
              | Some d2 when is_digit d2 ->
                  ignore (advance st);
                  (match peek st with
                   | Some d3 when is_digit d3 ->
                       ignore (advance st);
                       let n =
                         (digit_val d1 * 100) + (digit_val d2 * 10) + digit_val d3
                       in
                       if n > 255 then
                         raise
                           (Reader_error
                              ( "bad decimal escape \\"
                                ^ String.make 1 d1
                                ^ String.make 1 d2
                                ^ String.make 1 d3
                              , st.line
                              , st.col ));
                       Buffer.add_char buf (Char.chr n)
                   | _ ->
                       (* two digits only: forgiving literal digits *)
                       Buffer.add_char buf d1;
                       Buffer.add_char buf d2)
              | _ ->
                  (* one digit only: forgiving literal digit *)
                  Buffer.add_char buf d1)
         | Some c ->
             ignore (advance st);
             Buffer.add_char buf c);
        loop ()
    | Some c ->
        Buffer.add_char buf c;
        ignore (advance st);
        loop ()
  in
  loop ()

(** [read_atom st] reads a non-list, non-quote atom starting at the cursor
    (the caller has ensured the char is a token start). Returns [Var] for
    bare symbols (resolved by eval), [Lit] for numbers, booleans, and
    (already-read) strings. Rejects a lone '.' (dotted-pair notation) with
    a clear error. *)
let read_atom (st : state) : expr =
  (* reads a non-list, non-quote atom. Returns an [expr]: [Var] for bare
     symbols (which eval will look up), [Lit] for numbers, booleans,
     and (already-read) strings. *)
  let buf = Buffer.create 8 in
  let rec loop () =
    match peek st with
    | Some c when is_symbol_cont c ->
        Buffer.add_char buf c;
        ignore (advance st);
        loop ()
    | _ -> ()
  in
  loop ();
  let s = Buffer.contents buf in
  if s = "" then
    raise (Reader_error ("empty token", st.line, st.col))
  else if s = "#t" then Lit (VBool true)
  else if s = "#f" then Lit (VBool false)
  else if s = "." then
    (* A lone '.' is Scheme dotted-pair notation. Improper lists are
       not supported; reject at read time with a clear error instead
       of silently reading it as a symbol named "." (which would
       corrupt list length and trap macro authors). Float literals
       like .5 still parse below because they are not exactly ".". *)
    raise
      (Reader_error
         ("dotted-pair notation (.) is not supported", st.line, st.col))
  else
    (* Numbers: int first; if token has '.' or exponent, try float.
       Bare symbols otherwise (Var). *)
    let has_dot_or_exp =
      String.contains s '.'
      || String.contains s 'e'
      || String.contains s 'E'
    in
    if has_dot_or_exp then
      (try Lit (VFloat (float_of_string s))
       with _ -> Var s)
    else
      (try Lit (VInt (Int64.of_string s))
       with _ -> Var s)

(** [read_list st] reads a parenthesized list. Assumes the current char is
    '('. Returns the element list (empty for '()'); the caller turns it
    into an [expr]. Raises [Reader_error] on an unterminated list. *)
let rec read_list (st : state) : expr list =
  (* current char is '('; consume it *)
  ignore (advance st);
  let rec loop acc =
    skip_ws_and_comments st;
    match peek st with
    | None ->
        raise (Reader_error ("unterminated list", st.line, st.col))
    | Some ')' ->
        ignore (advance st);
        List.rev acc
    | Some _ ->
        let e = read_expr st in
        loop (e :: acc)
  in
  loop []

(** [read_expr st] reads one expression (the full reader core). Lists
    become [App] nodes with the first element as operator (or [Lit VNil]
    for () — special forms are dispatched at eval), quote/backtick/comma
    become the corresponding quote sugar, strings become [Lit (VStr …)],
    and other tokens become atoms via [read_atom]. Raises [Reader_error]
    on unexpected end of input or characters. *)
and read_expr (st : state) : expr =
  skip_ws_and_comments st;
  match peek st with
  | None -> raise (Reader_error ("unexpected end of input", st.line, st.col))
  | Some '(' ->
      (* list -> App or special form (handled at eval; reader produces App
         with the first element as operator, or Lit for empty list) *)
      let items = read_list st in
      (match items with
       | [] -> Lit VNil  (* () is nil *)
       | op :: args -> App (op, args))
  | Some '\'' ->
      (* quote sugar: 'x -> (quote x) *)
      ignore (advance st);
      let e = read_expr st in
      App (Var "quote", [ e ])
  | Some '`' ->
      (* quasiquote sugar: `x -> (quasiquote x) *)
      ignore (advance st);
      let e = read_expr st in
      App (Var "quasiquote", [ e ])
  | Some ',' ->
      (* unquote / unquote-splicing: ,x or ,@x *)
      ignore (advance st);
      (match peek st with
       | Some '@' ->
           ignore (advance st);
           let e = read_expr st in
           App (Var "unquote-splicing", [ e ])
       | _ ->
           let e = read_expr st in
           App (Var "unquote", [ e ]))
  | Some '"' -> Lit (VStr (read_string st))
  | Some c when is_token_start c ->
      (* atom: symbol (-> Var), number, or #t/#f (-> Lit) *)
      read_atom st
  | Some c ->
      raise (Reader_error
               ("unexpected character " ^ String.make 1 c, st.line, st.col))

(** [read_one s] reads exactly one s-expression from [s]. Raises
    [Reader_error] on parse error or if there's trailing junk after the
    expression. *)
let read_one (s : string) : expr =
  let st = make_state s in
  let e = read_expr st in
  skip_ws_and_comments st;
  (match peek st with
   | None -> ()
   | Some _ ->
       raise (Reader_error ("trailing input after expression", st.line, st.col)));
  e

(** [read_all s] reads as many top-level expressions as are in [s]. *)
let read_all (s : string) : expr list =
  let st = make_state s in
  let rec loop acc =
    skip_ws_and_comments st;
    match peek st with
    | None -> List.rev acc
    | Some _ ->
        let e = read_expr st in
        loop (e :: acc)
  in
  loop []

(** [is_complete s] returns true if [s] has balanced parens and closed
    strings, i.e. the REPL can evaluate it. Used by the REPL to know
    whether to prompt for more input vs. evaluate now. *)
let is_complete (s : string) : bool =
  let st = make_state s in
  let depth = ref 0 in
  let in_string = ref false in
  let in_comment = ref false in
  try
    while peek st |> Option.is_some do
      let c = Option.get (peek st) in
      if !in_comment then begin
        if c = '\n' then in_comment := false;
        ignore (advance st)
      end else if !in_string then begin
        (match c with
         | '"' -> in_string := false
         | _ -> ());
        ignore (advance st)
      end else begin
        (match c with
         | ';' -> in_comment := true
         | '"' -> in_string := true
         | '(' -> incr depth
         | ')' -> decr depth
         | _ -> ());
        ignore (advance st)
      end
    done;
    not !in_string && !depth = 0
    (* in_comment at EOF is fine: a line comment is implicitly
       closed by end-of-input. Only an unclosed string or
       unbalanced parens means "need more input". *)
  with _ -> false

(** Pretty-print a value for the REPL. *)
let rec pp_value (v : value) : string =
  match v with
  | VInt n -> Int64.to_string n
  | VFloat f ->
      (* Prefer a readable form; ensure a decimal so re-read is float. *)
      let s = Printf.sprintf "%.12g" f in
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then s
      else s ^ ".0"
  | VStr s -> "\"" ^ escape_string s ^ "\""
  | VSymbol s ->
      (* Bare token; quote odd names as |…| if needed later. *)
      s
  | VBool b -> if b then "#t" else "#f"
  | VNil -> "()"
  | VPair (a, b) ->
      (* Conventional list printer: a chain of VPairs ending in VNil
         prints as (a b c ...); an improper tail prints as
         (a b . tail). A VList tail is treated as the proper
         continuation (VList is just sugar for nested pairs). *)
      let rec collect acc = function
        | VPair (x, rest) -> collect (x :: acc) rest
        | VNil -> Some (List.rev acc, None)
        | VList items -> Some (List.rev acc @ items, None)
        | tail -> Some (List.rev acc, Some tail)
      in
      (match collect [] (VPair (a, b)) with
       | Some (items, None) ->
           "(" ^ String.concat " " (List.map pp_value items) ^ ")"
       | Some (items, Some tail) ->
           "(" ^ String.concat " " (List.map pp_value items)
           ^ " . " ^ pp_value tail ^ ")"
       | None -> assert false)
  | VLambda _ -> "#<lambda>"
  | VMacro _ -> "#<macro>"
  | VPrim name -> "#<prim:" ^ name ^ ">"
  | VRef (t, i) -> "#<ref:" ^ t ^ ":" ^ String.sub i 0 (min 8 (String.length i)) ^ ">"
  | VList vs -> "(" ^ String.concat " " (List.map pp_value vs) ^ ")"

(** Flat one-line print (for length checks / compact leaves). *)
let rec pp_expr_flat (e : expr) : string =
  match e with
  | Var s -> s
  | Lit v -> pp_value v
  | App (f, args) ->
      "(" ^ pp_expr_flat f
      ^ (if args = [] then ""
         else " " ^ String.concat " " (List.map pp_expr_flat args))
      ^ ")"
  | Let (n, v, b) ->
      "(let ((" ^ n ^ " " ^ pp_expr_flat v ^ ")) " ^ pp_expr_flat b ^ ")"
  | Letrec (bs, b) ->
      "(letrec ("
      ^ String.concat " "
          (List.map (fun (n, e) -> "(" ^ n ^ " " ^ pp_expr_flat e ^ ")") bs)
      ^ ") " ^ pp_expr_flat b ^ ")"
  | If (t, th, el) ->
      "(if " ^ pp_expr_flat t ^ " " ^ pp_expr_flat th ^ " " ^ pp_expr_flat el
      ^ ")"
  | Lambda (ps, b) ->
      "(lambda (" ^ String.concat " " ps ^ ") " ^ pp_expr_flat b ^ ")"
  | Prim (n, args) ->
      "(prim " ^ n
      ^ (if args = [] then ""
         else " " ^ String.concat " " (List.map pp_expr_flat args))
      ^ ")"
  | Seq (a, b) -> "(seq " ^ pp_expr_flat a ^ " " ^ pp_expr_flat b ^ ")"
  | Quote e -> "'" ^ pp_expr_flat e
  | Define (n, e) -> "(define " ^ n ^ " " ^ pp_expr_flat e ^ ")"

(** [pp_indent depth] returns a string of [depth * 2] spaces, used as the
    indentation for each nesting level in [pp_expr_at]. *)
let pp_indent (depth : int) : string = String.make (depth * 2) ' '

(** Prefer one line when short and shallow; otherwise break. *)
let rec expr_is_compact (e : expr) : bool =
  let flat = pp_expr_flat e in
  if String.length flat <= 56 then
    match e with
    | Var _ | Lit _ -> true
    | Quote e' -> expr_is_compact e'
    | App (f, args) ->
        List.length args <= 5
        && expr_is_compact f
        && List.for_all expr_is_compact args
    | Lambda (ps, b) -> List.length ps <= 4 && expr_is_compact b
    | Prim (_, args) -> List.for_all expr_is_compact args
    | If (t, th, el) ->
        expr_is_compact t && expr_is_compact th && expr_is_compact el
    | Seq (a, b) -> expr_is_compact a && expr_is_compact b
    | Let (_, v, b) -> expr_is_compact v && expr_is_compact b
    | Letrec (bs, b) ->
        List.length bs <= 2
        && List.for_all (fun (_, e) -> expr_is_compact e) bs
        && expr_is_compact b
    | Define (_, e) -> expr_is_compact e
  else false

(** Pretty-print an expr with indentation for the Functions browser / editor. *)
let rec pp_expr_at (depth : int) (e : expr) : string =
  if depth > 0 && expr_is_compact e then pp_expr_flat e
  else
    let ind1 = pp_indent (depth + 1) in
    match e with
    | Var s -> s
    | Lit v -> pp_value v
    | Quote e' ->
        if expr_is_compact e' then "'" ^ pp_expr_flat e'
        else "(quote\n" ^ ind1 ^ pp_expr_at (depth + 1) e' ^ ")"
    | App (f, []) -> "(" ^ pp_expr_at depth f ^ ")"
    | App (f, args) ->
        if expr_is_compact e then pp_expr_flat e
        else
          let head = pp_expr_at (depth + 1) f in
          let body =
            List.map
              (fun a -> ind1 ^ pp_expr_at (depth + 1) a)
              args
          in
          "(" ^ head ^ "\n" ^ String.concat "\n" body ^ ")"
    | Lambda (ps, b) ->
        if expr_is_compact e then pp_expr_flat e
        else
          "(lambda (" ^ String.concat " " ps ^ ")\n"
          ^ ind1 ^ pp_expr_at (depth + 1) b ^ ")"
    | If (t, th, el) ->
        if expr_is_compact e then pp_expr_flat e
        else
          "(if\n"
          ^ ind1 ^ pp_expr_at (depth + 1) t ^ "\n"
          ^ ind1 ^ pp_expr_at (depth + 1) th ^ "\n"
          ^ ind1 ^ pp_expr_at (depth + 1) el ^ ")"
    | Let (n, v, b) ->
        if expr_is_compact e then pp_expr_flat e
        else
          "(let ((" ^ n ^ "\n"
          ^ ind1 ^ "  " ^ pp_expr_at (depth + 2) v ^ "))\n"
          ^ ind1 ^ pp_expr_at (depth + 1) b ^ ")"
    | Letrec (bs, b) ->
        if expr_is_compact e then pp_expr_flat e
        else
          let binds =
            List.map
              (fun (n, e') ->
                ind1 ^ "(" ^ n ^ "\n"
                ^ pp_indent (depth + 2) ^ pp_expr_at (depth + 2) e' ^ ")")
              bs
          in
          "(letrec (\n"
          ^ String.concat "\n" binds ^ ")\n"
          ^ ind1 ^ pp_expr_at (depth + 1) b ^ ")"
    | Seq (a, b) ->
        if expr_is_compact e then pp_expr_flat e
        else
          "(seq\n"
          ^ ind1 ^ pp_expr_at (depth + 1) a ^ "\n"
          ^ ind1 ^ pp_expr_at (depth + 1) b ^ ")"
    | Prim (n, args) ->
        if expr_is_compact e then pp_expr_flat e
        else if args = [] then "(prim " ^ n ^ ")"
        else
          "(prim " ^ n ^ "\n"
          ^ String.concat "\n"
              (List.map
                 (fun a -> ind1 ^ pp_expr_at (depth + 1) a)
                 args)
          ^ ")"
    | Define (n, body) ->
        if expr_is_compact e then pp_expr_flat e
        else
          "(define " ^ n ^ "\n"
          ^ ind1 ^ pp_expr_at (depth + 1) body ^ ")"

(** [pp_expr e] pretty-prints [e] with indentation, starting at depth 0. *)
let pp_expr (e : expr) : string = pp_expr_at 0 e
