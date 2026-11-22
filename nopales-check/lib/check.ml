(** Nopales static check: the compile phase the language does not have.

    The language has no build step. A definition is read, stored, and hot-edited;
    nothing checks it against its callers, so arity, unbound names and unknown
    callables surface only when a call happens to run. This module walks the
    corpus with the interpreter's own reader and reports those classes statically.

    Design notes, because the shape is the argument:

    - The reader produces raw [App] trees (special forms are dispatched at eval,
      not by the reader). It does not macroexpand. A checker therefore has to
      model the binding forms the corpus actually uses or it drowns in false
      positives. Measured 2026-11-21 over libs/*/lib.pp: [let*] 493 uses,
      [guard] 35, [cond] 8, and no [case]/[match]/[do]. Those are handled
      structurally; everything else in [Eval.special_form_names] is walked as
      expressions.
    - A list in the corpus is not always a call. An unknown head may be a user
      macro whose arguments are syntax, so the walker reports it and does NOT
      descend into its arguments. That is the difference between a checker and a
      false-positive generator.
    - Positions are exact: [read_node] keeps (line, col) for every node, so a
      finding points at the call, not at the define that contains it. The
      positioned tree is cross-checked against [Reader.read_all] so the two
      cannot drift (the tripwire perflint already uses).
    - R1 (DB prim inside a per-row loop) is folded in as one severity class:
      perflint's own scan, RED -> ERROR, AMBER -> WARN.

    Severities:
      ERROR  reader error, arity mismatch, unbound package-qualified name,
             perflint RED.
      WARN   unknown qualified callable, perflint AMBER.
    Exit: 0 clean, 1 any ERROR (or any WARN under [--strict]), 2 unreadable. *)

module Ast = Pricklypear_lisp.Ast
module Reader = Pricklypear_lisp.Reader
module Eval = Pricklypear_lisp.Eval
module Env = Pricklypear_lisp.Env
module Control = Pricklypear_lisp.Control
module Effect = Pricklypear_lisp.Effect
module Db_cost = Pricklypear_image_lib.Db_cost

type severity = Error | Warn

type finding = {
  file : string;
  line : int;
  severity : severity;
  code : string;     (* reader | arity | unbound | unknown-callable | perf-r1 *)
  name : string;
  message : string;
}

type defn = {
  arity : int option;
  variadic : bool;
  value_only : bool;
  file : string;
  line : int;
  mutable ambiguous : bool;
}

(** One define's inferred capability footprint. [de_classes] is the coarse
    vocabulary the change-set gates use (docs/change-set.md §9); [de_effects]
    keeps the interpreter's finer names so a reader can see read-db vs write-db.
    A qualified call to a name neither the corpus nor [Effect] classifies turns
    into ["unknown"] — the factory's fail-closed class. *)
type define_effect = {
  de_name : string;
  de_file : string;
  de_line : int;
  de_effects : string list;
  de_classes : string list;
  de_unknown : string list;  (* unresolved qualified heads, when any *)
}

type report = {
  files : int;
  defines : int;
  findings : finding list;   (* sorted by file, then line *)
  effects : define_effect list;  (* computed only with [--effects] *)
  errors : int;
  warns : int;
}

let severity_name = function Error -> "ERROR" | Warn -> "WARN"

type ctx = {
  defs : (string, defn) Hashtbl.t;
  specials : (string, unit) Hashtbl.t;
  prims : (string, unit) Hashtbl.t;
  mutable findings : finding list;
  seen : (string, unit) Hashtbl.t;
  mutable file : string;
  mutable at : string;   (* enclosing define, for unbound-bare messages *)
  mutable def_line : int;  (* the enclosing define's line; 0 at top level *)
  unbound_bare : bool;
}

(* ------------------------------------------------------------------ *)
(* positioned tree                                                    *)
(* ------------------------------------------------------------------ *)

(** A node with its source position. [kids] is the source-order element list of
    a parenthesized form (head first), or [] for an atom. Quote sugar becomes a
    two-element form [quote <node>] so one walk handles every list. *)
type node = {
  expr : Ast.expr;
  line : int;
  col : int;
  is_list : bool;
  kids : node list;
}

let atom ~line ~col expr = { expr; line; col; is_list = false; kids = [] }

let rec read_node (st : Reader.state) : node =
  Reader.skip_ws_and_comments st;
  let line = st.Reader.line and col = st.Reader.col in
  match Reader.peek st with
  | None ->
      raise (Reader.Reader_error ("unexpected end of input", line, col))
  | Some '(' ->
      ignore (Reader.advance st);
      let rec elems acc =
        Reader.skip_ws_and_comments st;
        match Reader.peek st with
        | None ->
            raise (Reader.Reader_error ("unterminated list", line, col))
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
  | Some ('\'' as _q) ->
      ignore (Reader.advance st);
      let k = read_node st in
      let head = atom ~line ~col (Ast.Var "quote") in
      { expr = Ast.App (Ast.Var "quote", [ k.expr ]); line; col;
        is_list = true; kids = [ head; k ] }
  | Some '`' ->
      ignore (Reader.advance st);
      let k = read_node st in
      let head = atom ~line ~col (Ast.Var "quasiquote") in
      { expr = Ast.App (Ast.Var "quasiquote", [ k.expr ]); line; col;
        is_list = true; kids = [ head; k ] }
  | Some ',' ->
      ignore (Reader.advance st);
      let name =
        match Reader.peek st with
        | Some '@' -> ignore (Reader.advance st); "unquote-splicing"
        | _ -> "unquote"
      in
      let k = read_node st in
      let head = atom ~line ~col (Ast.Var name) in
      { expr = Ast.App (Ast.Var name, [ k.expr ]); line; col;
        is_list = true; kids = [ head; k ] }
  | Some '"' ->
      let s = Reader.read_string st in
      atom ~line ~col (Ast.Lit (Ast.VStr s))
  | Some c when Reader.is_token_start c ->
      atom ~line ~col (Reader.read_atom st)
  | Some c ->
      raise (Reader.Reader_error
               (Printf.sprintf "unexpected character %C" c, line, col))

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
      (Reader.Reader_error
         (file ^ ": positioned reader disagreed with Reader.read_all", 1, 1));
  nodes

(* ------------------------------------------------------------------ *)
(* small node helpers                                                 *)
(* ------------------------------------------------------------------ *)

let is_var (n : node) name =
  match n.expr with Ast.Var s -> s = name | _ -> false

let head_var (n : node) =
  match n.kids with
  | h :: _ -> (match h.expr with Ast.Var s -> Some s | _ -> None)
  | [] -> None

let args_of (n : node) = match n.kids with _ :: rest -> rest | [] -> []

let sym (n : node) = match n.expr with Ast.Var s -> Some s | _ -> None

let params_of (n : node) =
  List.filter_map sym n.kids

let is_lambda (n : node) =
  match n.kids with h :: _ -> is_var h "lambda" | [] -> false

let arity_of_params ps =
  let is_rest p =
    p = "." || p = "&" || p = "&rest"
    || (String.length p > 0 && p.[0] = '&')
  in
  let rec go n = function
    | [] -> (Some n, false)
    | p :: rest -> if is_rest p then (Some n, true) else go (n + 1) rest
  in
  go 0 ps

(* ------------------------------------------------------------------ *)
(* findings                                                           *)
(* ------------------------------------------------------------------ *)

let add ctx ~line ~severity ~code ~name ~message =
  (* One unknown callable per file is enough to act on; arity, unbound and perf
     findings are per site, because each site is a separate fix. *)
  let key =
    if code = "unknown-callable"
    then Printf.sprintf "%s|%s|%s" ctx.file code name
    else if code = "txn"
    then Printf.sprintf "%s|%s|%s|%d|%s" ctx.file code name line message
    else Printf.sprintf "%s|%s|%s|%d" ctx.file code name line
  in
  if not (Hashtbl.mem ctx.seen key) then begin
    Hashtbl.add ctx.seen key ();
    ctx.findings <-
      { file = ctx.file; line; severity; code; name; message } :: ctx.findings
  end

(* ------------------------------------------------------------------ *)
(* pass 1: the definition index                                       *)
(* ------------------------------------------------------------------ *)

let record_def ctx ~line name arity variadic value_only =
  let d = { arity; variadic; value_only; file = ctx.file; line;
            ambiguous = false } in
  match Hashtbl.find_opt ctx.defs name with
  | None -> Hashtbl.add ctx.defs name d
  | Some old ->
      if old.arity <> arity || old.variadic <> variadic
         || old.value_only <> value_only then begin
        old.ambiguous <- true;
        d.ambiguous <- true;
        Hashtbl.replace ctx.defs name d
      end

let collect_define ctx (n : node) =
  match n.kids with
  | kw :: target :: body ->
      let macro = is_var kw "defmacro" in
      let name, params =
        match target.expr with
        | Ast.Var f ->
            (match body with
             | p :: _ when macro -> (Some f, Some (params_of p))
             | lam :: _ when is_lambda lam ->
                 (match lam.kids with
                  | _ :: p :: _ -> (Some f, Some (params_of p))
                  | _ -> (Some f, None))
             | _ -> (Some f, None))
        | _ ->
            (match params_of target with
             | vhead :: ps -> (Some vhead, Some ps)
             | [] -> (None, None))
      in
      (match name with
       | Some name ->
           let arity, variadic =
             match params with
             | Some ps -> arity_of_params ps
             | None -> (None, false)
           in
           record_def ctx ~line:n.line name arity variadic (params = None)
       | None -> ())
  | _ -> ()

let rec collect_top ctx (n : node) =
  match n.kids with
  | h :: _ when is_var h "define" || is_var h "defmacro" -> collect_define ctx n
  | h :: args when is_var h "begin" || is_var h "seq" ->
      List.iter (collect_top ctx) args
  | _ -> ()

(* ------------------------------------------------------------------ *)
(* pass 2: macro expansion + the scope walk                           *)
(* ------------------------------------------------------------------ *)

let bound ctx scope x =
  (String.length x > 0 && x.[0] = ':')   (* keyword atoms self-evaluate *)
  || List.exists (fun frame -> List.mem x frame) scope
  || Hashtbl.mem ctx.defs x
  || Hashtbl.mem ctx.specials x
  || Hashtbl.mem ctx.prims x
  || x = "else"

(* --- expression helpers (expansion works on Ast.expr, not nodes) --- *)

(** A surface list as its element expressions.

    The reader represents a list as [App (head, args)] where [head] is the
    FIRST ELEMENT (which may itself be a list), not necessarily a symbol: a
    list of binding pairs [(h (g x))] is [App (App (h, [g x]), [])]. Only call
    this where the form is known to be a list. *)
let list_elems_expr = function
  | Ast.Lit Ast.VNil -> Some []
  | Ast.App (h, rest) -> Some (h :: rest)
  | _ -> None

let params_of_expr e =
  match list_elems_expr e with
  | Some elems ->
      let names =
        List.filter_map (function Ast.Var s -> Some s | _ -> None) elems
      in
      if List.length names = List.length elems then Some names else None
  | None -> None

let bindings_of_expr e =
  match list_elems_expr e with
  | None -> []
  | Some pairs ->
      List.filter_map
        (function
          | Ast.App (Ast.Var n, [ v ]) -> Some (n, v)
          | _ -> None)
        pairs

(** A define's body and the params it binds, for both surface shapes:
    [(define (f a) …)], [(define f (lambda (a) …))], and the defmacro forms. *)
let define_parts_expr e =
  match e with
  | Ast.App (Ast.Var kw, target :: body) when kw = "define" || kw = "defmacro" ->
      let macro = kw = "defmacro" in
      (match target with
       | Ast.Var _ ->
           (match body with
            | p :: rest when macro ->
                (match params_of_expr p with Some ps -> (ps, rest) | None -> ([], rest))
            | _ -> ([], body))
       | _ ->
           (match params_of_expr target with
            | Some (_name :: ps) -> (ps, body)
            | _ -> ([], body)))
  | _ -> ([], [])

let rec walk ctx scope ~line_of e =
  match e with
  | Ast.Var x ->
      (* A bare Var's printed form is not unique in a file, so the position map
         cannot say which occurrence this is. Report the enclosing define's
         line (the message names the define), never a wrong first occurrence. *)
      let vline = if ctx.def_line > 0 then ctx.def_line else line_of e in
      if not (bound ctx scope x) then begin
        if String.contains x '/' then
          add ctx ~line:vline ~severity:Error ~code:"unbound" ~name:x
            ~message:(Printf.sprintf "%s is not defined anywhere in the corpus" x)
        else if ctx.unbound_bare then begin
          if Sys.getenv_opt "NOPALES_CHECK_TRACE" = Some "1" then
            Printf.eprintf "UNBOUND %s at=%s defline=%d\n%!" x ctx.at ctx.def_line;
          add ctx ~line:vline ~severity:Warn ~code:"unbound" ~name:x
            ~message:(Printf.sprintf
                        "%s is not bound in %s (a real gap, or a host prim used \
                         as a value)" x (if ctx.at = "" then "the form" else ctx.at))
        end
      end
  | Ast.Lit _ | Ast.Quote _ | Ast.Prim _ -> ()
  | Ast.App (Ast.Var ("quote" | "quasiquote" | "route" | "deftype" | "package"), _) -> ()
  | Ast.App (Ast.Var kw, target :: body) when kw = "define" || kw = "defmacro" ->
      let ps, body = define_parts_expr e in
      let scope = if ps = [] then scope else ps :: scope in
      let prev = ctx.at and prev_line = ctx.def_line in
      ctx.at <-
        (match target with
         | Ast.Var n -> n
         | Ast.App (Ast.Var n, _) -> n
         | _ -> prev);
      ctx.def_line <- line_of e;
      if Sys.getenv_opt "NOPALES_CHECK_TRACE" = Some "1" then
        Printf.eprintf "ENTER %s @%d\n%!" ctx.at ctx.def_line;
      Fun.protect ~finally:(fun () -> ctx.at <- prev; ctx.def_line <- prev_line)
        (fun () -> List.iter (walk ctx scope ~line_of) body)
  | Ast.App (Ast.Var "lambda", p :: body) ->
      let ps = match params_of_expr p with Some ps -> ps | None -> [] in
      List.iter (walk ctx (ps :: scope) ~line_of) body
  | Ast.App (Ast.Var ("let" | "let*" as kw), binds :: body) ->
      let pairs = bindings_of_expr binds in
      if kw = "let*" then begin
        let rec seq scope = function
          | [] -> List.iter (walk ctx scope ~line_of) body
          | (name, v) :: rest ->
              walk ctx scope ~line_of v;
              seq ([ name ] :: scope) rest
        in
        seq scope pairs
      end else begin
        List.iter (fun (_, v) -> walk ctx scope ~line_of v) pairs;
        List.iter (walk ctx (List.map fst pairs :: scope) ~line_of) body
      end
  | Ast.App (Ast.Var "letrec", binds :: body) ->
      let pairs = bindings_of_expr binds in
      let names = List.map fst pairs in
      List.iter (fun (_, v) -> walk ctx (names :: scope) ~line_of v) pairs;
      List.iter (walk ctx (names :: scope) ~line_of) body
  | Ast.App (Ast.Var "prim", _name :: args) ->
      List.iter (walk ctx scope ~line_of) args
  | Ast.App (Ast.Var name, args) ->
      if List.exists (fun frame -> List.mem name frame) scope then
        List.iter (walk ctx scope ~line_of) args
      else
        (match Hashtbl.find_opt ctx.defs name with
         | Some d when not d.ambiguous && not d.value_only ->
             (match d.arity with
              | Some want when d.variadic && List.length args < want ->
                  add ctx ~line:(line_of e) ~severity:Error ~code:"arity" ~name
                    ~message:(Printf.sprintf
                                "%s takes at least %d argument(s), called with %d"
                                name want (List.length args))
              | Some want when (not d.variadic) && List.length args <> want ->
                  add ctx ~line:(line_of e) ~severity:Error ~code:"arity" ~name
                    ~message:(Printf.sprintf
                                "%s takes %d argument(s), called with %d"
                                name want (List.length args))
              | _ -> ());
             List.iter (walk ctx scope ~line_of) args
         | Some _ -> List.iter (walk ctx scope ~line_of) args
         | None ->
             if Hashtbl.mem ctx.specials name || Hashtbl.mem ctx.prims name then
               List.iter (walk ctx scope ~line_of) args
             else if String.contains name '/' then
               (* A qualified head claims a corpus define. A bare one is
                  usually an image prim with no static catalogue here. *)
               add ctx ~line:(line_of e) ~severity:Warn ~code:"unknown-callable"
                 ~name
                 ~message:(Printf.sprintf
                             "%s is not a define, a special form, or a known prim"
                             name))
  | Ast.App (f, args) ->
      walk ctx scope ~line_of f;
      List.iter (walk ctx scope ~line_of) args
  | Ast.Lambda (ps, b) -> walk ctx (ps :: scope) ~line_of b
  | Ast.Let (name, v, b) ->
      walk ctx scope ~line_of v;
      walk ctx ([ name ] :: scope) ~line_of b
  | Ast.Letrec (bs, b) ->
      let names = List.map fst bs in
      List.iter (fun (_, v) -> walk ctx (names :: scope) ~line_of v) bs;
      walk ctx (names :: scope) ~line_of b
  | Ast.If (c, t, f) ->
      walk ctx scope ~line_of c; walk ctx scope ~line_of t;
      walk ctx scope ~line_of f
  | Ast.Seq (a, b) -> walk ctx scope ~line_of a; walk ctx scope ~line_of b
  | Ast.Define (_, v) -> walk ctx scope ~line_of v

let pp_expansion_error = function
  | Ast.Interp_error (Ast.Unbound_var x) -> "unbound var " ^ x
  | Ast.Interp_error (Ast.Arity_mismatch (name, want, got)) ->
      Printf.sprintf "%s takes %d argument(s), called with %d" name want got
  | Ast.Interp_error (Ast.Wrong_arg_count (name, want, got)) ->
      Printf.sprintf "%s takes %d argument(s), called with %d" name want got
  | Ast.Interp_error (Ast.User_error (msg, _)) -> msg
  | Ast.Interp_error (Ast.Prim_error (name, msg, _)) -> name ^ ": " ^ msg
  | Ast.Interp_error Ast.Budget_exceeded -> "budget exceeded"
  | Ast.Interp_error (Ast.Not_a_function _) -> "macro expanded to a non-callable"
  | Ast.Interp_error (Ast.Type_mismatch (want, _)) -> "type mismatch: " ^ want
  | Ast.Interp_error (Ast.Parse_error (msg, l, c)) ->
      Printf.sprintf "parse: %s at %d:%d" msg l c
  | e -> Printexc.to_string e

(* --- macro environment --- *)

(** Bind every corpus `defmacro` into one env. Macro bodies are templates;
    evaluating the definition binds a VMacro without running the body, and the
    bodies use only pure prims in practice. A macro that cannot be defined is
    skipped, not fatal: expansion then falls back to the unexpanded form. *)
let macro_env parsed =
  let env = Env.create () in
  let prims = Eval.make_prim_table () in
  Prims_pure.install prims;
  (* The evaluator resolves call heads through the ENV, not the prim table
     (App -> Env.lookup -> VPrim). A macro body that calls (list …) or
     (null? …) needs those names bound, or expansion fails on the template. *)
  List.iter (fun n -> Env.define env n (Ast.VPrim n)) Prims_pure.catalog_names;
  List.iter
    (fun (_file, nodes) ->
      let rec go (n : node) =
        match n.kids with
        | h :: _ when is_var h "defmacro" ->
            (try
               ignore
                 (Control.run_with_rim ~prims (fun () ->
                      Eval.eval ~budget:(Eval.make_budget ()) ~prims env n.expr))
             with _ -> ())
        | h :: args when is_var h "begin" || is_var h "seq" ->
            List.iter go args
        | _ -> ()
      in
      List.iter go nodes)
    parsed;
  (env, prims)

(** Position lookup for an expanded tree: user-written sub-expressions match
    their original nodes by printed form; macro-introduced ones fall back to
    the enclosing form's line. *)
let positions_of_nodes nodes =
  let t = Hashtbl.create 1024 in
  let rec go (n : node) =
    let key = Reader.pp_expr n.expr in
    if not (Hashtbl.mem t key) then Hashtbl.add t key (n.line, n.col);
    List.iter go n.kids
  in
  List.iter go nodes;
  t

(* ------------------------------------------------------------------ *)
(* pass 3: effects                                                     *)
(* ------------------------------------------------------------------ *)

(** The interpreter's own classification is the authority. Using it here (and
    in the callable table below) is what keeps [unknown-callable] about names
    that really are unknown: [tree/leaf] and [soup/parse] are classified prims,
    [plot/chart] is not. *)
let classified_prim_names =
  Effect.pure_prims @ Effect.read_db_prims @ Effect.write_db_prims
  @ Effect.branch_prims @ Effect.publish_prims @ Effect.html_prims
  @ Effect.net_prims @ Effect.ai_prims
  @ [ "with-transaction"; "__with_transaction__" ]

(** Coarse class for the change-set gates (docs/change-set.md §9). [Html] is
    a rendering capability, not an I/O one: a define that only builds markup
    needs no live target, so it stays [pure] for gate selection. [Branch] is
    store I/O, so it joins [db]; [Ai] leaves the process, so it joins [egress]. *)
let class_of_eff = function
  | Effect.ReadDb | Effect.WriteDb | Effect.Branch -> "db"
  | Effect.Txn -> "tx"
  | Effect.Net | Effect.Ai -> "egress"
  | Effect.Publish -> "route"
  | Effect.Html | Effect.Pure -> "pure"

let classes_of_effects fx =
  let cs =
    List.filter_map
      (fun e -> match class_of_eff e with "pure" -> None | c -> Some c)
      (Effect.EffSet.to_list fx)
  in
  if cs = [] then [ "pure" ] else List.sort_uniq String.compare cs

(** Qualified applied heads in [e] that neither the corpus nor [Effect]
    classifies. Bare names are not claims (they are parameters or host prims
    the image registers); a qualified one claims a namespace, and if the
    checker cannot resolve it the effect is not knowable. *)
let unclassified_qualified defs e =
  let out = ref [] in
  let special n =
    List.mem n Effect.special_form_heads
    || List.mem n Eval.special_form_names
    || Hashtbl.mem defs n
  in
  let note n =
    if String.contains n '/'
       && not (special n)
       && not (Effect.is_classified n)
    then out := n :: !out
  in
  let rec go = function
    | Ast.App (Ast.Var n, args) -> note n; List.iter go args
    | Ast.Prim (n, args) -> note n; List.iter go args
    | Ast.App (f, args) -> go f; List.iter go args
    | Ast.Lambda (_, b) | Ast.Define (_, b) -> go b
    | Ast.Let (_, v, b) | Ast.Seq (v, b) -> go v; go b
    | Ast.Letrec (bs, b) ->
        List.iter (fun (_, v) -> go v) bs;
        go b
    | Ast.If (a, b, c) -> go a; go b; go c
    | Ast.Var _ | Ast.Lit _ | Ast.Quote _ -> ()
  in
  go e;
  List.sort_uniq String.compare !out

(** Infer one effect row per define. [known_fn] resolves corpus calls, so a
    pure wrapper that calls a DB writer is [db]. The transactional hints are
    advisory and ride the same inference (a second write site, or a row lock,
    outside [with-transaction]). *)
let effect_pass ctx raw =
  let bindings = List.map (fun (name, body, _, _) -> (name, body)) raw in
  let known_fn = Effect.known_fn_of_bindings bindings in
  List.map
    (fun (name, body, file, line) ->
      let fx = Effect.infer ~known_fn body in
      let unknown = unclassified_qualified ctx.defs body in
      let base = classes_of_effects fx in
      let classes =
        if unknown = [] then base
        else
          List.sort_uniq String.compare
            ("unknown" :: List.filter (fun c -> c <> "pure") base)
      in
      { de_name = name;
        de_file = file;
        de_line = line;
        de_effects =
          List.map Effect.eff_to_string (Effect.EffSet.to_list fx);
        de_classes = classes;
        de_unknown = unknown })
    (List.rev raw)

(** Advisory transactional hints (multi-write / row lock outside
    [with-transaction]). Kept out of the default finding set: the interpreter
    classifies them "soft", and several corpus writers open a transaction in
    the caller, which this static pass cannot see. [--txn] asks for them. *)
let txn_pass ctx raw =
  let bindings = List.map (fun (name, body, _, _) -> (name, body)) raw in
  let known_fn = Effect.known_fn_of_bindings bindings in
  List.iter
    (fun (name, body, file, line) ->
      List.iter
        (fun hint ->
          ctx.file <- file;
          add ctx ~line ~severity:Warn ~code:"txn" ~name ~message:hint)
        (Effect.txn_lint ~known_fn body))
    raw

(* ------------------------------------------------------------------ *)
(* files                                                              *)
(* ------------------------------------------------------------------ *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let rec pp_files path =
  if not (Sys.file_exists path) then []
  else if Sys.is_directory path then
    Sys.readdir path |> Array.to_list
    |> List.sort String.compare
    |> List.concat_map (fun name -> pp_files (Filename.concat path name))
  else if Filename.check_suffix path ".pp" then [ path ]
  else []

(* ------------------------------------------------------------------ *)
(* perflint R1, folded in as one severity class                       *)
(* ------------------------------------------------------------------ *)

let perf_findings files =
  List.concat_map
    (fun file ->
      try
        List.map
          (fun (pf : Perf_lint.finding) ->
            { file = pf.file;
              line = pf.line;
              severity = (match pf.severity with
                          | Perf_lint.Red -> Error | Perf_lint.Amber -> Warn);
              code = "perf-r1";
              name = pf.prim;
              message =
                Printf.sprintf "DB prim %s inside %s%s" pf.prim pf.loop
                  (match pf.via with Some v -> " via " ^ v | None -> "") })
          (Perf_lint.scan_file file)
      with _ -> [])
    files

(* ------------------------------------------------------------------ *)
(* run                                                                *)
(* ------------------------------------------------------------------ *)

let run ?(json = false) ?(strict = false) ?(perf = true)
    ?(unbound_bare = true) ?(effects = false) ?(txn = false)
    (paths : string list) : report =
  let ctx =
    { defs = Hashtbl.create 4096;
      specials = Hashtbl.create 64;
      prims = Hashtbl.create 256;
      findings = [];
      seen = Hashtbl.create 1024;
      file = "";
      at = "";
      def_line = 0;
      unbound_bare }
  in
  List.iter (fun n -> Hashtbl.replace ctx.specials n ()) Eval.special_form_names;
  List.iter (fun n -> Hashtbl.replace ctx.prims n ()) Prims_pure.catalog_names;
  List.iter (fun n -> Hashtbl.replace ctx.prims n ()) Db_cost.known_prims;
  List.iter (fun n -> Hashtbl.replace ctx.prims n ()) classified_prim_names;

  let files =
    List.concat_map pp_files (if paths = [] then [ "libs" ] else paths)
  in
  (* pass 0: read + parse; exact positions, cross-checked against read_all *)
  let parsed =
    List.filter_map
      (fun file ->
        ctx.file <- file;
        try Some (file, read_nodes ~file (read_file file))
        with
        | Reader.Reader_error (msg, line, col) ->
            add ctx ~line ~severity:Error ~code:"reader"
              ~name:(Filename.basename file)
              ~message:(Printf.sprintf "reader: %s at %d:%d" msg line col);
            None
        | Sys_error msg ->
            add ctx ~line:1 ~severity:Error ~code:"reader"
              ~name:(Filename.basename file) ~message:msg;
            None)
      files
  in
  (* pass 1: the index, over every file *)
  List.iter
    (fun (file, nodes) ->
      ctx.file <- file;
      List.iter (collect_top ctx) nodes)
    parsed;
  (* pass 2: macroexpand each top-level form, then walk it. Positions come
     from the unexpanded tree, so a finding inside user-written code is exact;
     a form the expansion introduced takes the enclosing form's line. *)
  let menv, mprims = macro_env parsed in
  let raw_defs = ref [] in
  List.iter
    (fun (file, nodes) ->
      ctx.file <- file;
      let pos = positions_of_nodes nodes in
      List.iter
        (fun (n : node) ->
          ctx.def_line <- 0;
          match n.kids with
          | h :: _ when is_var h "defmacro" -> ()
          | _ ->
              let line_of e =
                match Hashtbl.find_opt pos (Reader.pp_expr e) with
                | Some (l, _) -> l
                | None -> n.line
              in
              let expanded =
                try
                  Control.run_with_rim ~prims:mprims (fun () ->
                      Eval.macroexpand_expr ~budget:(Eval.make_budget ())
                        ~prims:mprims menv n.expr)
                with e ->
                  add ctx ~line:n.line ~severity:Warn ~code:"macro-expand"
                    ~name:"(expansion)"
                    ~message:(Printf.sprintf "macro expansion failed: %s"
                                (pp_expansion_error e));
                  n.expr
              in
              (match (try Eval.parse_top_define expanded with _ -> None) with
               | Some ds ->
                   raw_defs :=
                     (ds.Eval.name, ds.Eval.body, file, line_of expanded)
                     :: !raw_defs
               | None -> ());
              walk ctx [] ~line_of expanded)
        nodes)
    parsed;
  let effects_report = if effects then effect_pass ctx !raw_defs else [] in
  if txn then txn_pass ctx !raw_defs;
  let ours = List.rev ctx.findings in
  let perf_fs = if perf then perf_findings (List.map fst parsed) else [] in
  let findings =
    List.sort
      (fun (a : finding) (b : finding) ->
        let c = String.compare a.file b.file in
        if c <> 0 then c else compare a.line b.line)
      (ours @ perf_fs)
  in
  let errors = List.length (List.filter (fun (f : finding) ->
      f.severity = Error) findings) in
  let warns = List.length findings - errors in
  let report =
    { files = List.length parsed; defines = Hashtbl.length ctx.defs;
      findings; effects = effects_report; errors; warns }
  in
  if json then begin
    let base =
      [ ("files", `Int report.files);
        ("defines", `Int report.defines);
        ("errors", `Int errors);
        ("warns", `Int warns);
        ( "findings",
          `List
            (List.map
               (fun (f : finding) ->
                 `Assoc
                   [ ("file", `String f.file);
                     ("line", `Int f.line);
                     ("severity", `String (severity_name f.severity));
                     ("code", `String f.code);
                     ("name", `String f.name);
                     ("message", `String f.message) ])
               findings) ) ]
    in
    let base =
      if effects then
        ("effects",
         `List
           (List.map
              (fun (e : define_effect) ->
                `Assoc
                  [ ("name", `String e.de_name);
                    ("file", `String e.de_file);
                    ("line", `Int e.de_line);
                    ("classes", `List (List.map (fun c -> `String c) e.de_classes));
                    ("effects", `List (List.map (fun x -> `String x) e.de_effects));
                    ("unknown", `List (List.map (fun x -> `String x) e.de_unknown)) ])
              effects_report))
        :: base
      else base
    in
    print_endline (Yojson.Safe.pretty_to_string (`Assoc base))
  end else begin
    List.iter
      (fun (f : finding) ->
        Printf.printf "%s:%d: %s %s %s\n" f.file f.line
          (severity_name f.severity) f.code f.message)
      findings;
    if effects then begin
      List.iter
        (fun (e : define_effect) ->
          Printf.printf "%s:%d: EFFECT %s classes=%s effects=%s%s\n"
            e.de_file e.de_line e.de_name
            (String.concat "+" e.de_classes)
            (if e.de_effects = [] then "none"
             else String.concat "+" e.de_effects)
            (if e.de_unknown = [] then ""
             else " unknown=" ^ String.concat "+" e.de_unknown))
        effects_report;
      let hist = Hashtbl.create 8 in
      List.iter
        (fun (e : define_effect) ->
          List.iter
            (fun c ->
              Hashtbl.replace hist c
                (1 + (match Hashtbl.find_opt hist c with Some n -> n | None -> 0)))
            e.de_classes)
        effects_report;
      let pairs =
        Hashtbl.fold (fun c n acc -> (c, n) :: acc) hist []
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      Printf.printf "effects: %s\n"
        (String.concat ", "
           (List.map (fun (c, n) -> Printf.sprintf "%s=%d" c n) pairs))
    end;
    Printf.printf "%d file(s), %d define(s), %d error(s), %d warning(s)\n"
      report.files report.defines errors warns
  end;
  report

let exit_code ?(strict = false) (r : report) =
  if r.errors > 0 then 1
  else if strict && r.warns > 0 then 1
  else if r.files = 0 then 2
  else 0
