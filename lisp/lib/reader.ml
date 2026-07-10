(** Pricklypear Lisp — S-expression reader.

    Parses surface s-expression syntax -> Ast.expr. The AST IS the
    source (homoiconicity): a function stored as JSONB is its AST, and
    the s-expr syntax is one of several possible editors (along with
    the htmx tree-editor that edits the AST directly).

    Per spec [lisp-interpreter.reader]:
      - Standard s-expressions: (foo bar baz), 'symbol, "string", 123,
        #t, #f, ()
      - Comments: ; to end of line
      - Docstrings: a leading string in a body position (deferred; reader
        just produces the AST, docstring extraction is eval-time)
      - Special forms [lambda/let/letrec/if/define/quote] are recognized
        at EVAL time, not read time — the reader produces plain App
        nodes; eval dispatches them.

    The reader tracks line/col for error messages. *)
open Ast

type reader_error = string * int * int  (* (message, line, col) *)
exception Reader_error of reader_error

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let is_digit c = c >= '0' && c <= '9'
let is_symbol_start c =
  (c <> '(' && c <> ')' && c <> '\'' && c <> '"'
  && c <> ';' && not (is_space c))
let is_symbol_cont c = is_symbol_start c || is_digit c || c = '#'

(* # is allowed as a token start for #t/#f (and later #\char etc). *)
let is_token_start c = c = '#' || is_symbol_start c

let is_token_cont c = is_symbol_cont c

type state = {
  mutable input : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

let make_state (s : string) : state =
  { input = s; pos = 0; line = 1; col = 1 }

let peek (st : state) : char option =
  if st.pos >= String.length st.input then None
  else Some st.input.[st.pos]

let advance (st : state) : char option =
  match peek st with
  | None -> None
  | Some c ->
      st.pos <- st.pos + 1;
      if c = '\n' then (st.line <- st.line + 1; st.col <- 1)
      else st.col <- st.col + 1;
      Some c

let skip_ws_and_comments (st : state) : unit =
  let rec loop () =
    match peek st with
    | None -> ()
    | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
        ignore (advance st); loop ()
    | Some ';' ->
        (* line comment *)
        (match peek st with
         | Some '\n' | None -> ()
         | _ -> ignore (advance st); loop ())
    | _ -> ()
  in
  loop ()

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
        (* Escape: consume the backslash, then the next char is the
           escape letter. Two-step: advance past '\\', then peek+advance
           the escape char (NOT (match advance st with ...) — that
           would match the '\\' we just peeked, not the next char). *)
        ignore (advance st);  (* consume '\\' (we peeked it) *)
        (match peek st with
         | None ->
             raise (Reader_error ("unterminated escape", st.line, st.col))
         | Some 'n' -> ignore (advance st); Buffer.add_char buf '\n'
         | Some 't' -> ignore (advance st); Buffer.add_char buf '\t'
         | Some '\\' -> ignore (advance st); Buffer.add_char buf '\\'
         | Some '"' -> ignore (advance st); Buffer.add_char buf '"'
         | Some c ->
             raise (Reader_error ("bad escape \\" ^ String.make 1 c, st.line, st.col)));
        loop ()
    | Some c ->
        Buffer.add_char buf c;
        ignore (advance st);
        loop ()
  in
  loop ()

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
  else
    (* try int; otherwise it's a bare symbol -> a Var reference *)
    (try Lit (VInt (Int64.of_string s))
     with _ -> Var s)

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
  | VStr s -> "\"" ^ String.escaped s ^ "\""
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
  | VPrim name -> "#<prim:" ^ name ^ ">"
  | VRef (t, i) -> "#<ref:" ^ t ^ ":" ^ String.sub i 0 (min 8 (String.length i)) ^ ">"
  | VList vs -> "(" ^ String.concat " " (List.map pp_value vs) ^ ")"

(** Pretty-print an expr (the AST). Mostly for debugging; the REPL
    pretty-prints values, not exprs. *)
let rec pp_expr (e : expr) : string =
  match e with
  | Var s -> s
  | Lit v -> pp_value v
  | App (f, args) ->
      "(" ^ pp_expr f
      ^ (if args = [] then "" else " " ^ String.concat " " (List.map pp_expr args))
      ^ ")"
  | Let (n, v, b) -> "(let (" ^ n ^ " " ^ pp_expr v ^ ") " ^ pp_expr b ^ ")"
  | Letrec (bs, b) ->
      "(letrec ("
      ^ String.concat " " (List.map (fun (n, e) -> "(" ^ n ^ " " ^ pp_expr e ^ ")") bs)
      ^ ") " ^ pp_expr b ^ ")"
  | If (t, th, el) ->
      "(if " ^ pp_expr t ^ " " ^ pp_expr th ^ " " ^ pp_expr el ^ ")"
  | Lambda (ps, b) ->
      "(lambda (" ^ String.concat " " ps ^ ") " ^ pp_expr b ^ ")"
  | Prim (n, args) ->
      "(prim " ^ n ^ " " ^ String.concat " " (List.map pp_expr args) ^ ")"
  | Seq (a, b) -> "(seq " ^ pp_expr a ^ " " ^ pp_expr b ^ ")"
  | Quote e -> "(quote " ^ pp_expr e ^ ")"
  | Define (n, e) -> "(define " ^ n ^ " " ^ pp_expr e ^ ")"
