(** Pricklypear Image — Primitive registry.

    Primitives are OCaml functions exposed to Lisp. They live in the image
    (not the lisp/ package) because they're I/O-shaped: read from store,
    emit htmx, persist, etc. The lisp/ package is pure I/O-free.

    Spec [lisp-interpreter.primitives] lists the full ~20; for the minimal
    image, ~5 are sufficient to demo the loop. *)
open Lwt
open Pricklypear_lisp.Ast
(* A prim fn takes evaluated args, returns a value (or raises) wrapped in
   a promise. Pure prims register via [register_sync] which wraps the sync
   prim in [Lwt.return]; DB-touching prims register via [register] and
   return their promise directly. *)
type prim_fn = value list -> value Lwt.t

(* The sync form: pure prims return [value] immediately. They get wrapped
   in [Lwt.return] when registered via [register_sync]. *)
type prim_fn_sync = value list -> value

type t = (string, prim_fn) Hashtbl.t

let make () : t = Hashtbl.create 16

let register (t : t) (name : string) (fn : prim_fn) : unit =
  Hashtbl.replace t name fn

let register_sync (t : t) (name : string) (fn : prim_fn_sync) : unit =
  Hashtbl.replace t name (fun args -> Lwt.return (fn args))

let lookup (t : t) (name : string) : prim_fn option =
  Hashtbl.find_opt t name

(* "+" sums two-or-more numeric VInt args. Variadic. *)
let prim_add (args : value list) : value =
  let sum =
    List.fold_left
      (fun acc v ->
        match v with
        | VInt n -> Int64.add acc n
        | _ -> raise (Interp_error (User_error "+ needs VInt args")))
      0L args
  in
  VInt sum

(* "-" binary: (- a b) = a - b; unary: (- a) = -a. *)
let prim_sub args =
  match args with
  | [ VInt a ] -> VInt (Int64.neg a)
  | [ VInt a; VInt b ] -> VInt (Int64.sub a b)
  | _ -> raise (Interp_error (User_error "- needs one or two VInt args"))

(* "*". For now, two-or-more VInt args; bignum deferred. *)
let prim_mul (args : value list) : value =
  let prod =
    List.fold_left
      (fun acc v ->
        match v with
        | VInt n -> Int64.mul acc n
        | _ -> raise (Interp_error (User_error "* needs VInt args")))
      1L args
  in
  VInt prod

(* "=" numeric equality for two VInt args. Returns VBool. *)
let prim_num_eq args =
  match args with
  | [ VInt a; VInt b ] -> VBool (Int64.equal a b)
  | _ -> raise (Interp_error (User_error "= needs two VInt args"))

(* "<" numeric less-than for two VInt args *)
let prim_lt args =
  match args with
  | [ VInt a; VInt b ] -> VBool (Int64.compare a b < 0)
  | _ -> raise (Interp_error (User_error "< needs two VInt args"))

(* ">" numeric greater-than for two VInt args *)
let prim_gt args =
  match args with
  | [ VInt a; VInt b ] -> VBool (Int64.compare a b > 0)
  | _ -> raise (Interp_error (User_error "> needs two VInt args"))

(* "<=" ">=" numeric ordering for two VInt args *)
let prim_le args =
  match args with
  | [ VInt a; VInt b ] -> VBool (Int64.compare a b <= 0)
  | _ -> raise (Interp_error (User_error "<= needs two VInt args"))
let prim_ge args =
  match args with
  | [ VInt a; VInt b ] -> VBool (Int64.compare a b >= 0)
  | _ -> raise (Interp_error (User_error ">= needs two VInt args"))

(* "not" boolean negation *)
let prim_not args =
  match args with
  | [ VBool b ] -> VBool (not b)
  | [ VNil ] -> VBool true
  | [ _ ] -> VBool false   (* any non-nil non-false is truthy *)
  | _ -> raise (Interp_error (User_error "not needs one arg"))

(* (list a b c ...) -> VList. Variadic. *)
let prim_list args = VList args

(* (car xs): head of a VList or VPair. Errors on empty / atoms. *)
let prim_car args =
  match args with
  | [ VList (h :: _) ] -> h
  | [ VList [] ] -> VNil
  | [ VPair (a, _) ] -> a
  | _ -> raise (Interp_error (User_error "car needs a list or pair"))

(* (cdr xs): tail of a VList or VPair. *)
let prim_cdr args =
  match args with
  | [ VList (_ :: rest) ] -> VList rest
  | [ VList [] ] -> VNil
  | [ VPair (_, b) ] -> b
  | _ -> raise (Interp_error (User_error "cdr needs a list or pair"))

(* (cons a b) -> VPair. Classic Lisp cons cell. *)
let prim_cons args =
  match args with
  | [ a; b ] -> VPair (a, b)
  | _ -> raise (Interp_error (User_error "cons needs two args"))

(* (null? x) -> VBool. True if VNil. *)
let prim_null_p args =
  match args with
  | [ VNil ] -> VBool true
  | [ VList [] ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error "null? needs one arg"))

(* (pair? x) -> VBool. True if VPair or non-empty VList. *)
let prim_pair_p args =
  match args with
  | [ VPair _ ] -> VBool true
  | [ VList (_ :: _) ] -> VBool true
  | _ -> VBool false

(* (string-append s1 s2 ...) -> VStr. Variadic over VStr. *)
let prim_string_append args =
  let buf = Buffer.create 32 in
  List.iter (fun v ->
    match v with
    | VStr s -> Buffer.add_string buf s
    | _ -> raise (Interp_error (User_error "string-append needs VStr args")))
    args;
  VStr (Buffer.contents buf)

(* (type-of x) -> VStr. Returns a type tag for runtime reflection. *)
let prim_type_of args =
  match args with
  | [ VInt _ ] -> VStr "int"
  | [ VStr _ ] -> VStr "str"
  | [ VBool _ ] -> VStr "bool"
  | [ VNil ] -> VStr "nil"
  | [ VPair _ ] -> VStr "pair"
  | [ VList _ ] -> VStr "list"
  | [ VLambda _ ] -> VStr "lambda"
  | [ VPrim name ] -> VStr ("prim:" ^ name)
  | [ VRef (t, _) ] -> VStr ("ref:" ^ t)
  | _ -> raise (Interp_error (User_error "type-of needs one arg"))

(* (str x) -> VStr. Coerces any value to its pretty-printed form. *)
let prim_str args =
  match args with
  | [ v ] -> VStr (Pricklypear_lisp.Reader.pp_value v)
  | _ -> raise (Interp_error (User_error "str needs one arg"))

(* (as-str x) -> VStr without Lisp quotes (for path/ref building). *)
let prim_as_str args =
  match args with
  | [ VStr s ] -> VStr s
  | [ VInt n ] -> VStr (Int64.to_string n)
  | [ VBool true ] -> VStr "true"
  | [ VBool false ] -> VStr "false"
  | [ VNil ] -> VStr ""
  | [ v ] -> VStr (Pricklypear_lisp.Reader.pp_value v)
  | _ -> raise (Interp_error (User_error "as-str needs one arg"))

(* (div a b) integer division *)
let prim_div args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.div a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error "div by zero"))
  | _ -> raise (Interp_error (User_error "div needs two VInt args"))

let prim_mod args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.rem a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error "mod by zero"))
  | _ -> raise (Interp_error (User_error "mod needs two VInt args"))

let prim_abs args =
  match args with
  | [ VInt a ] -> VInt (Int64.abs a)
  | _ -> raise (Interp_error (User_error "abs needs one VInt"))

let prim_min args =
  match args with
  | [] -> raise (Interp_error (User_error "min needs at least one VInt"))
  | VInt h :: rest ->
      let m =
        List.fold_left
          (fun acc v ->
            match v with
            | VInt n -> if Int64.compare n acc < 0 then n else acc
            | _ -> raise (Interp_error (User_error "min needs VInt args")))
          h rest
      in
      VInt m
  | _ -> raise (Interp_error (User_error "min needs VInt args"))

let prim_max args =
  match args with
  | [] -> raise (Interp_error (User_error "max needs at least one VInt"))
  | VInt h :: rest ->
      let m =
        List.fold_left
          (fun acc v ->
            match v with
            | VInt n -> if Int64.compare n acc > 0 then n else acc
            | _ -> raise (Interp_error (User_error "max needs VInt args")))
          h rest
      in
      VInt m
  | _ -> raise (Interp_error (User_error "max needs VInt args"))

let prim_string_length args =
  match args with
  | [ VStr s ] -> VInt (Int64.of_int (String.length s))
  | _ -> raise (Interp_error (User_error "string-length needs one VStr"))

let prim_substring args =
  match args with
  | [ VStr s; VInt start; VInt len ] ->
      let st = Int64.to_int start in
      let n = Int64.to_int len in
      let sl = String.length s in
      if st < 0 || n < 0 || st > sl then
        raise (Interp_error (User_error "substring out of range"))
      else
        let n = min n (sl - st) in
        VStr (String.sub s st n)
  | [ VStr s; VInt start ] ->
      let st = Int64.to_int start in
      let sl = String.length s in
      if st < 0 || st > sl then
        raise (Interp_error (User_error "substring out of range"))
      else VStr (String.sub s st (sl - st))
  | _ ->
      raise
        (Interp_error
           (User_error "substring needs string start [len]"))

let prim_string_contains args =
  match args with
  | [ VStr hay; VStr needle ] ->
      let rec loop i =
        if needle = "" then true
        else if i + String.length needle > String.length hay then false
        else if String.sub hay i (String.length needle) = needle then true
        else loop (i + 1)
      in
      VBool (loop 0)
  | _ ->
      raise
        (Interp_error (User_error "string-contains? needs two VStr args"))

let list_of_value = function
  | VList xs -> xs
  | VNil -> []
  | VPair (a, b) ->
      let rec unwind p acc =
        match p with
        | VPair (x, y) -> unwind y (x :: acc)
        | VNil | VList [] -> List.rev acc
        | VList xs -> List.rev_append acc xs
        | other -> List.rev (other :: acc)
      in
      unwind (VPair (a, b)) []
  | _ -> raise (Interp_error (User_error "expected a list"))

let prim_append args =
  match args with
  | [ a; b ] -> VList (list_of_value a @ list_of_value b)
  | xs ->
      VList
        (List.concat_map
           (function
             | VList l -> l
             | VNil -> []
             | _ -> raise (Interp_error (User_error "append needs lists")))
           xs)

let prim_reverse args =
  match args with
  | [ xs ] -> VList (List.rev (list_of_value xs))
  | _ -> raise (Interp_error (User_error "reverse needs one list"))

let prim_nth args =
  match args with
  | [ xs; VInt i ] ->
      let n = Int64.to_int i in
      let lst = list_of_value xs in
      if n < 0 || n >= List.length lst then VNil
      else List.nth lst n
  | _ -> raise (Interp_error (User_error "nth needs list and VInt"))

(* ---- dict primitives over VStr JSON -----------------------------

   Iteration-14 decision: don't introduce a new VDict value type yet
   (that would ripple through ast.ml / reader.ml / eval.ml / value
   JSON round-trip). Instead, (get 'type id) returns a VStr holding
   the fields JSON; these prims operate on VStr JSON directly so Lisp
   code can do:
       (define u (get 'user id))
       (dict-get u "name")
       (dict-set u "active" "false")
   The primitive returns VStr JSON (or VNil if the input wasn't a valid
   object, or key wasn't found). The underlying parse-and-emit is
   Yojson; the canonical round-trip preserves well-formed objects. *)

(* (dict-get json-str key) -> VStr value | VNil.
   Looks up [key] in the JSON object at [json-str]. Returns the value
   as a VStr (numbers and booleans stringified via Yojson); VNil if
   the key is missing or the input isn't a JSON object. *)
let prim_dict_get args =
  match args with
  | [ VStr json; VStr key ] ->
      (try
        let j = Yojson.Safe.from_string json in
        (match j with
         | `Assoc kvs ->
             (match List.assoc_opt key kvs with
              | Some (`String s) -> VStr s
              | Some (`Int n) -> VStr (string_of_int n)
              | Some (`Intlit s) -> VStr s
              | Some (`Float f) -> VStr (string_of_float f)
              | Some (`Bool b) -> VStr (string_of_bool b)
              | Some (`Null) -> VNil
              | Some (#Yojson.Safe.t as v) ->
                  VStr (Yojson.Safe.to_string v)
              | None -> VNil)
         | _ -> VNil)
      with _ -> VNil)
  | _ -> raise (Interp_error (User_error "dict-get needs 2 strings"))

(* (dict-set json-str key value) -> VStr new-json.
   Builds a JSON object with [key] = [value] (a string; numbers
   detected and emitted as JSON numeric where parseable). Returns the
   updated JSON as a VStr. If [json-str] isn't a JSON object, starts
   a fresh {} and adds the new key. *)
let prim_dict_set args =
  match args with
  | [ VStr json; VStr key; VStr value ] ->
      let base =
        try
          (match Yojson.Safe.from_string json with
           | `Assoc kvs -> kvs
           | _ -> [])
        with _ -> []
      in
      let v_json =
        (* Try parsing the value as JSON (so callers can pass nested
           objects/arrays); if it doesn't parse, emit as a JSON string. *)
        try Yojson.Safe.from_string value
        with _ -> `String value
      in
      let updated =
        (key, v_json) ::
        (List.filter (fun (k, _) -> k <> key) base)
      in
      VStr (Yojson.Safe.to_string (`Assoc updated))
  | _ -> raise (Interp_error (User_error "dict-set needs 3 strings"))

(* (dict-keys json-str) -> VList of VStr keys.
   Returns the keys of the JSON object at [json-str] (insertion order
   preserved by Yojson's `Assoc). Empty list if input isn't an object. *)
let prim_dict_keys args =
  match args with
  | [ VStr json ] ->
      (try
        (match Yojson.Safe.from_string json with
         | `Assoc kvs ->
             VList (List.map (fun (k, _) -> VStr k) kvs)
         | _ -> VList [])
      with _ -> VList [])
  | _ -> raise (Interp_error (User_error "dict-keys needs 1 string"))

(* (render-html html-str) -> VStr tagged-for-raw-rendering.
   Marks the returned VStr as raw HTML so http.ml renders it
   unescaped (instead of pretty-printing the value inside <pre>).
   The tag is a private prefix sentinel; http.ml checks for it. *)
let html_sentinel = "__html__:"
let redirect_sentinel = "__redirect__:"

let prim_render_html args =
  match args with
  | [ VStr s ] -> VStr (html_sentinel ^ s)
  | _ -> raise (Interp_error (User_error "render-html needs 1 string"))

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
  VStr (redirect_sentinel ^ path)

let is_redirect (v : value) : string option =
  match v with
  | VStr s when String.length s >= String.length redirect_sentinel
              && String.sub s 0 (String.length redirect_sentinel)
                 = redirect_sentinel ->
      Some (String.sub s (String.length redirect_sentinel)
              (String.length s - String.length redirect_sentinel))
  | _ -> None

(* (string-eq a b) -> VBool. String equality for request-method checks etc. *)
let prim_string_eq args =
  match args with
  | [ VStr a; VStr b ] -> VBool (String.equal a b)
  | _ -> raise (Interp_error (User_error "string-eq needs 2 strings"))

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
  | _ -> raise (Interp_error (User_error "html-escape needs 1 value"))

(* (hx-get url) -> VStr tagged attribute helper (lightweight; authors may
   also embed Hx attrs inside render-html). *)
let prim_hx_get args =
  match args with
  | [ VStr url ] -> VStr ("hx-get:" ^ url)
  | _ -> raise (Interp_error (User_error "hx-get needs a string url"))

let prim_hx_post args =
  match args with
  | [ VStr url ] -> VStr ("hx-post:" ^ url)
  | _ -> raise (Interp_error (User_error "hx-post needs a string url"))

(* (count list) -> int. Demonstrates a prim that walks the evaluator's
   own value type. *)
let prim_count args =
  match args with
  | [ VList items ] -> VInt (Int64.of_int (List.length items))
  | [ VNil ] -> VInt 0L
  | _ -> raise (Interp_error (User_error "count needs a list"))

(* --- Session-aware primitives ---

   These need access to the running session (env, manifest). They're
   registered as closures at session-make time via
   [register_session_prims]. The signature stays prim_fn (value list
   -> value) — the closure captures the session. *)

(* (whoami) -> VStr. The current session's username (from the
   manifest asserted by the supervisor). *)
let prim_whoami_fn ~(username : string) (_ : value list) : value =
  VStr username

(* (current-author) -> VStr. Same as whoami for now; reserved for
   the multi-user permission model where (current-author) consults
   the binding being executed rather than the session. *)
let prim_current_author_fn ~(username : string) (_ : value list) : value =
  VStr username

(* (list-bindings) -> VList of binding-name VStrs. Lists all names
   currently visible in the session env. *)
let prim_list_bindings_fn ~(env : Pricklypear_lisp.Ast.env) (_ : value list) : value =
  let names =
    Pricklypear_lisp.Env.fold env
      (fun name _ acc -> name :: acc) []
  in
  VList (List.map (fun s -> VStr s) names)

(* Register session-aware primitives into [t]. Called at session-make
   time with manifest.username and env. *)
let register_session_prims
    (t : t)
    ~(username : string)
    ~(env : Pricklypear_lisp.Ast.env)
    : unit =
  register_sync t "whoami" (prim_whoami_fn ~username);
  register_sync t "current-author" (prim_current_author_fn ~username);
  register_sync t "list-bindings" (prim_list_bindings_fn ~env)


(* Register the minimal set. Returns the populated table. *)
let make_with_defaults () : t =
  let t = make () in
  register_sync t "+" prim_add;
  register_sync t "-" prim_sub;
  register_sync t "*" prim_mul;
  register_sync t "div" prim_div;
  register_sync t "mod" prim_mod;
  register_sync t "abs" prim_abs;
  register_sync t "min" prim_min;
  register_sync t "max" prim_max;
  register_sync t "=" prim_num_eq;
  register_sync t "<" prim_lt;
  register_sync t ">" prim_gt;
  register_sync t "<=" prim_le;
  register_sync t ">=" prim_ge;
  register_sync t "not" prim_not;
  register_sync t "list" prim_list;
  register_sync t "car" prim_car;
  register_sync t "cdr" prim_cdr;
  register_sync t "cons" prim_cons;
  register_sync t "null?" prim_null_p;
  register_sync t "pair?" prim_pair_p;
  register_sync t "string-append" prim_string_append;
  register_sync t "string-length" prim_string_length;
  register_sync t "substring" prim_substring;
  register_sync t "string-contains?" prim_string_contains;
  register_sync t "string-eq" prim_string_eq;
  register_sync t "html-escape" prim_html_escape;
  register_sync t "type-of" prim_type_of;
  register_sync t "str" prim_str;
  register_sync t "as-str" prim_as_str;
  register_sync t "append" prim_append;
  register_sync t "reverse" prim_reverse;
  register_sync t "nth" prim_nth;
  (* list-branches is registered as a session DB prim in session.ml *)
  register_sync t "hx-get" prim_hx_get;
  register_sync t "hx-post" prim_hx_post;
  register_sync t "count" prim_count;
  register_sync t "dict-get" prim_dict_get;
  register_sync t "dict-set" prim_dict_set;
  register_sync t "dict-keys" prim_dict_keys;
  register_sync t "render-html" prim_render_html;
  t

(* Short docs for the Functions browser (kernel package). *)
let prim_summary (name : string) : string =
  match name with
  | "+" -> "add numbers"
  | "-" -> "subtract / negate"
  | "*" -> "multiply"
  | "div" -> "integer divide"
  | "mod" -> "integer remainder"
  | "abs" -> "absolute value"
  | "min" -> "minimum of VInts"
  | "max" -> "maximum of VInts"
  | "=" -> "numeric equality"
  | "<" | ">" | "<=" | ">=" -> "numeric compare"
  | "not" -> "boolean not"
  | "list" -> "build a list"
  | "car" -> "first of pair/list"
  | "cdr" -> "rest of pair/list"
  | "cons" -> "cons cell"
  | "null?" -> "empty list?"
  | "pair?" -> "is pair?"
  | "count" -> "length of list"
  | "string-append" -> "concat strings"
  | "string-length" -> "length of string"
  | "substring" -> "substring start [len]"
  | "string-contains?" -> "haystack contains needle?"
  | "string-eq" -> "string equality"
  | "html-escape" -> "escape text for HTML"
  | "request-method" -> "HTTP method of active request"
  | "request-path" -> "path of active request"
  | "request-param" -> "form/query param of active request"
  | "request-header" -> "HTTP header of active request"
  | "hx-request?" -> "true if HX-Request header present"
  | "request-params" -> "all params as JSON object"
  | "request-body" -> "raw body of active request"
  | "redirect" -> "HTTP 303 redirect response"
  | "append" -> "concatenate lists"
  | "reverse" -> "reverse list"
  | "nth" -> "nth element of list (0-based)"
  | "str" -> "value to string"
  | "as-str" -> "plain string coerce (no quotes)"
  | "type-of" -> "runtime type tag"
  | "dict-get" -> "JSON object field get"
  | "dict-set" -> "JSON object field set"
  | "dict-keys" -> "JSON object keys"
  | "render-html" -> "mark string as raw HTML"
  | "hx-get" | "hx-post" -> "htmx attribute helper"
  | "whoami" | "current-author" -> "session identity"
  | "list-bindings" -> "names in env"
  | "list-branches" -> "list branch refs"
  | "current-branch" -> "current branch name"
  | "fork" -> "fork a branch"
  | "merge" -> "merge branches"
  | "checkout" -> "switch branch"
  | "route" -> "bind URL to function"
  | "deftype" -> "declare a data type"
  | "get" -> "read one data row"
  | "create" -> "insert data row → id"
  | "update" -> "replace row fields JSON"
  | "update-field" -> "set one JSON field"
  | "delete" -> "delete data row"
  | "rows" -> "list data rows"
  | "find-row" -> "first row by fields->>key"
  | "find-rows" -> "rows filtered by fields"
  | "find-text" -> "substring search on a field"
  | "select-for-update" -> "lock row in transaction"
  | "agent" -> "AI portal: (agent seed prompt) -> text"
  | "agent-task" -> "multi-turn tool agent; may install many functions"
  | "propose-function" -> "AI draft+install function on branch"
  | "install-function" -> "install AI body string as named function"
  | "llm-inner" | "llm-outer" | "llm-rewrite" | "llm-seed" ->
      "AI rewrite / growth tool"
  | "__with_transaction__" -> "internal transaction helper"
  | "load-string" -> "load Lisp source text (define/route/deftype)"
  | "load-library" -> "load libs/<name>/lib.pp into current branch"
  | _ -> "built-in primitive"

(** All registered prim names with one-line summaries, sorted. *)
let catalog (t : t) : (string * string) list =
  Hashtbl.fold
    (fun name _ acc -> (name, prim_summary name) :: acc)
    t []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let list_names (t : t) : string list =
  List.map fst (catalog t)

(* Convert the table into the shape Pricklypear_lisp.Eval.prim_table
   expects. Delegating to a separate function so the integration with
   the interpreter is explicit. *)
let to_eval_table (t : t) : Pricklypear_lisp.Eval.prim_table =
  let module E = Pricklypear_lisp.Eval in
  let tbl = E.make_prim_table () in
  Hashtbl.iter (fun name fn -> E.register_prim tbl name fn) t;
  tbl
