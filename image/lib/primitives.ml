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

(* Numeric helpers: VInt | VFloat with promotion (int+float → float). *)
type num = NInt of int64 | NFloat of float

let num_of_value = function
  | VInt n -> NInt n
  | VFloat f -> NFloat f
  | v ->
      raise
        (Interp_error
           (User_error ("numeric op needs int or float, got " ^
                       (match v with
                        | VStr _ -> "string"
                        | VBool _ -> "bool"
                        | VNil -> "nil"
                        | _ -> "other"))))

let float_of_num = function
  | NInt n -> Int64.to_float n
  | NFloat f -> f

let value_of_num = function
  | NInt n -> VInt n
  | NFloat f -> VFloat f

let num_add a b =
  match a, b with
  | NInt x, NInt y -> NInt (Int64.add x y)
  | _ -> NFloat (float_of_num a +. float_of_num b)

let num_sub a b =
  match a, b with
  | NInt x, NInt y -> NInt (Int64.sub x y)
  | _ -> NFloat (float_of_num a -. float_of_num b)

let num_mul a b =
  match a, b with
  | NInt x, NInt y -> NInt (Int64.mul x y)
  | _ -> NFloat (float_of_num a *. float_of_num b)

let num_div a b =
  let bf = float_of_num b in
  if bf = 0. then
    raise (Interp_error (User_error "div by zero"))
  else
    match a, b with
    | NInt x, NInt y -> NInt (Int64.div x y)
    | _ -> NFloat (float_of_num a /. bf)

let num_neg = function
  | NInt n -> NInt (Int64.neg n)
  | NFloat f -> NFloat (~-. f)

let num_compare a b =
  match a, b with
  | NInt x, NInt y -> Int64.compare x y
  | _ -> compare (float_of_num a) (float_of_num b)

(* "+" sums two-or-more numeric args. Variadic. *)
let prim_add (args : value list) : value =
  if args = [] then
    raise (Interp_error (User_error "+ needs at least one arg"))
  else
    value_of_num
      (List.fold_left
         (fun acc v -> num_add acc (num_of_value v))
         (num_of_value (List.hd args))
         (List.tl args))

(* "-" binary: (- a b) = a - b; unary: (- a) = -a. *)
let prim_sub args =
  match args with
  | [ a ] -> value_of_num (num_neg (num_of_value a))
  | [ a; b ] -> value_of_num (num_sub (num_of_value a) (num_of_value b))
  | _ -> raise (Interp_error (User_error "- needs one or two numeric args"))

(* "*". Two-or-more numeric args. *)
let prim_mul (args : value list) : value =
  if args = [] then
    raise (Interp_error (User_error "* needs at least one arg"))
  else
    value_of_num
      (List.fold_left
         (fun acc v -> num_mul acc (num_of_value v))
         (num_of_value (List.hd args))
         (List.tl args))

(* "=" numeric equality (int/float). *)
let prim_num_eq args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) = 0)
  | _ -> raise (Interp_error (User_error "= needs two numeric args"))

let prim_lt args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) < 0)
  | _ -> raise (Interp_error (User_error "< needs two numeric args"))

let prim_gt args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) > 0)
  | _ -> raise (Interp_error (User_error "> needs two numeric args"))

let prim_le args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) <= 0)
  | _ -> raise (Interp_error (User_error "<= needs two numeric args"))

let prim_ge args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) >= 0)
  | _ -> raise (Interp_error (User_error ">= needs two numeric args"))

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
  | [ VFloat _ ] -> VStr "float"
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
  | [ VFloat f ] -> VStr (Printf.sprintf "%.12g" f)
  | [ VBool true ] -> VStr "true"
  | [ VBool false ] -> VStr "false"
  | [ VNil ] -> VStr ""
  | [ v ] -> VStr (Pricklypear_lisp.Reader.pp_value v)
  | _ -> raise (Interp_error (User_error "as-str needs one arg"))

(* (parse-float s|n) -> VFloat *)
let prim_parse_float args =
  match args with
  | [ VFloat f ] -> VFloat f
  | [ VInt n ] -> VFloat (Int64.to_float n)
  | [ VStr s ] ->
      (try VFloat (float_of_string (String.trim s))
       with _ -> VFloat 0.)
  | _ -> raise (Interp_error (User_error "parse-float needs string or number"))

(* (floor x) -> VInt *)
let prim_floor args =
  match args with
  | [ VInt n ] -> VInt n
  | [ VFloat f ] -> VInt (Int64.of_float (floor f))
  | _ -> raise (Interp_error (User_error "floor needs one numeric arg"))

(* (round x) -> VInt *)
let prim_round args =
  match args with
  | [ VInt n ] -> VInt n
  | [ VFloat f ] -> VInt (Int64.of_float (Float.round f))
  | _ -> raise (Interp_error (User_error "round needs one numeric arg"))

(* (div a b) — integer div when both int; else float. *)
let prim_div args =
  match args with
  | [ a; b ] -> value_of_num (num_div (num_of_value a) (num_of_value b))
  | _ -> raise (Interp_error (User_error "div needs two numeric args"))

let prim_mod args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.rem a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error "mod by zero"))
  | [ a; b ] ->
      let bf = float_of_num (num_of_value b) in
      if bf = 0. then raise (Interp_error (User_error "mod by zero"))
      else VFloat (mod_float (float_of_num (num_of_value a)) bf)
  | _ -> raise (Interp_error (User_error "mod needs two numeric args"))

let prim_abs args =
  match args with
  | [ VInt a ] -> VInt (Int64.abs a)
  | [ VFloat f ] -> VFloat (abs_float f)
  | _ -> raise (Interp_error (User_error "abs needs one numeric arg"))

let prim_min args =
  match args with
  | [] -> raise (Interp_error (User_error "min needs at least one numeric arg"))
  | h :: rest ->
      value_of_num
        (List.fold_left
           (fun acc v ->
             let n = num_of_value v in
             if num_compare n acc < 0 then n else acc)
           (num_of_value h) rest)

let prim_max args =
  match args with
  | [] -> raise (Interp_error (User_error "max needs at least one numeric arg"))
  | h :: rest ->
      value_of_num
        (List.fold_left
           (fun acc v ->
             let n = num_of_value v in
             if num_compare n acc > 0 then n else acc)
           (num_of_value h) rest)

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

(* (string-trim s) -> VStr. Trim leading/trailing whitespace. *)
let prim_string_trim args =
  match args with
  | [ VStr s ] -> VStr (String.trim s)
  | _ -> raise (Interp_error (User_error "string-trim needs 1 string"))

(* JSON <-> Lisp values for outline trees and generic data.
   Objects become VStr of JSON object (use dict-get); arrays -> VList. *)
let rec json_to_value (j : Yojson.Safe.t) : value =
  match j with
  | `Null -> VNil
  | `Bool b -> VBool b
  | `Int n -> VInt (Int64.of_int n)
  | `Intlit s -> (try VInt (Int64.of_string s) with _ -> VStr s)
  | `Float f -> VStr (string_of_float f)
  | `String s -> VStr s
  | `List xs -> VList (List.map json_to_value xs)
  | `Assoc _ as o -> VStr (Yojson.Safe.to_string o)

let rec value_to_json (v : value) : Yojson.Safe.t =
  match v with
  | VNil -> `Null
  | VBool b -> `Bool b
  | VInt n -> `Intlit (Int64.to_string n)
  | VStr s ->
      (* Nested JSON object/array strings stay structured when emitted. *)
      let len = String.length s in
      if len >= 2
         && ((s.[0] = '{' && s.[len - 1] = '}')
             || (s.[0] = '[' && s.[len - 1] = ']'))
      then (try Yojson.Safe.from_string s with _ -> `String s)
      else `String s
  | VList xs -> `List (List.map value_to_json xs)
  | VPair (a, b) ->
      let rec unwind acc = function
        | VPair (x, y) -> unwind (x :: acc) y
        | VNil -> List.rev acc
        | VList ys -> List.rev_append acc ys
        | tail -> List.rev (tail :: acc)
      in
      `List (List.map value_to_json (unwind [] (VPair (a, b))))
  | other -> `String (Pricklypear_lisp.Reader.pp_value other)

let prim_json_parse args =
  match args with
  | [ VStr s ] ->
      (try json_to_value (Yojson.Safe.from_string s)
       with _ -> raise (Interp_error (User_error "json-parse: invalid JSON")))
  | _ -> raise (Interp_error (User_error "json-parse needs 1 string"))

let prim_json_emit args =
  match args with
  | [ v ] -> VStr (Yojson.Safe.to_string (value_to_json v))
  | _ -> raise (Interp_error (User_error "json-emit needs 1 value"))

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

(* (whoami) -> VStr. Current session username. [username_fn] is
   re-read on each call so cotenant multi-user can switch identity
   per HTTP request without re-registering prims. *)
let prim_whoami_fn ~(username_fn : unit -> string) (_ : value list) : value =
  VStr (username_fn ())

(* (current-author) -> VStr. Same as whoami for now; reserved for
   the multi-user permission model where (current-author) consults
   the binding being executed rather than the session. *)
let prim_current_author_fn ~(username_fn : unit -> string)
    (_ : value list) : value =
  VStr (username_fn ())

(* (list-bindings) -> VList of binding-name VStrs. Lists all names
   currently visible in the session env. *)
let prim_list_bindings_fn ~(env : Pricklypear_lisp.Ast.env) (_ : value list) : value =
  let names =
    Pricklypear_lisp.Env.fold env
      (fun name _ acc -> name :: acc) []
  in
  VList (List.map (fun s -> VStr s) names)

(* Register session-aware primitives into [t]. Called at session-make
   time with a dynamic username getter + env. *)
let register_session_prims
    (t : t)
    ~(username_fn : unit -> string)
    ~(env : Pricklypear_lisp.Ast.env)
    : unit =
  register_sync t "whoami" (prim_whoami_fn ~username_fn);
  register_sync t "current-author" (prim_current_author_fn ~username_fn);
  register_sync t "list-bindings" (prim_list_bindings_fn ~env)


(* (date-today) -> VStr "YYYY-MM-DD" in local time. *)
let prim_date_today (_args : value list) : value =
  let tm = Unix.localtime (Unix.time ()) in
  VStr
    (Printf.sprintf "%04d-%02d-%02d"
       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday)

(* (parse-int s|n) -> VInt. Soft-fail to 0 on bad string. *)
let prim_parse_int (args : value list) : value =
  match args with
  | [ VInt n ] -> VInt n
  | [ VStr s ] ->
      (try VInt (Int64.of_int (int_of_string (String.trim s)))
       with _ -> VInt 0L)
  | _ -> raise (Interp_error (User_error "parse-int needs string or int"))

(* (date-weekday y m d) -> VInt 0=Sunday .. 6=Saturday (local). *)
let prim_date_weekday (args : value list) : value =
  match args with
  | [ VInt y; VInt m; VInt d ] ->
      let y = Int64.to_int y and m = Int64.to_int m and d = Int64.to_int d in
      let tm =
        { Unix.tm_sec = 0
        ; tm_min = 0
        ; tm_hour = 12
        ; tm_mday = d
        ; tm_mon = m - 1
        ; tm_year = y - 1900
        ; tm_wday = 0
        ; tm_yday = 0
        ; tm_isdst = false
        }
      in
      let _, tm' = Unix.mktime tm in
      VInt (Int64.of_int tm'.Unix.tm_wday)
  | _ ->
      raise (Interp_error (User_error "date-weekday needs year month day ints"))

(* (date-format y m d) -> VStr "YYYY-MM-DD". *)
let prim_date_format (args : value list) : value =
  match args with
  | [ VInt y; VInt m; VInt d ] ->
      VStr
        (Printf.sprintf "%04d-%02d-%02d"
           (Int64.to_int y) (Int64.to_int m) (Int64.to_int d))
  | _ -> raise (Interp_error (User_error "date-format needs year month day"))

(** (date-add-days "YYYY-MM-DD" n) | (date-add-days n) with today.
    Local calendar day arithmetic via Unix.mktime (flashcards SRS). *)
let prim_date_add_days (args : value list) : value =
  let add_from_ymd y m d n =
    let tm =
      { Unix.tm_sec = 0
      ; tm_min = 0
      ; tm_hour = 12
      ; tm_mday = d + n
      ; tm_mon = m - 1
      ; tm_year = y - 1900
      ; tm_wday = 0
      ; tm_yday = 0
      ; tm_isdst = false
      }
    in
    let _, tm' = Unix.mktime tm in
    VStr
      (Printf.sprintf "%04d-%02d-%02d"
         (tm'.Unix.tm_year + 1900)
         (tm'.Unix.tm_mon + 1)
         tm'.Unix.tm_mday)
  in
  let parse_ymd s =
    let s = String.trim s in
    if String.length s < 10 then None
    else
      try
        let y = int_of_string (String.sub s 0 4) in
        let m = int_of_string (String.sub s 5 2) in
        let d = int_of_string (String.sub s 8 2) in
        Some (y, m, d)
      with _ -> None
  in
  match args with
  | [ VStr day; VInt n ] ->
      (match parse_ymd day with
       | Some (y, m, d) -> add_from_ymd y m d (Int64.to_int n)
       | None ->
           raise (Interp_error (User_error "date-add-days: bad date string")))
  | [ VInt n ] ->
      let tm = Unix.localtime (Unix.time ()) in
      add_from_ymd
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        (Int64.to_int n)
  | [ VStr day; VStr n ] ->
      (match parse_ymd day, int_of_string_opt (String.trim n) with
       | Some (y, m, d), Some n -> add_from_ymd y m d n
       | _ ->
           raise (Interp_error (User_error "date-add-days: bad args")))
  | _ ->
      raise
        (Interp_error
           (User_error "date-add-days needs (day n) or (n) for today+n"))

(** (date-diff-days a b) -> VInt days (b - a), local calendar.
    Args are YYYY-MM-DD strings. *)
let prim_date_diff_days (args : value list) : value =
  let parse s =
    let s = String.trim s in
    if String.length s < 10 then None
    else
      try
        Some
          ( int_of_string (String.sub s 0 4)
          , int_of_string (String.sub s 5 2)
          , int_of_string (String.sub s 8 2) )
      with _ -> None
  in
  let epoch_days y m d =
    let tm =
      { Unix.tm_sec = 0; tm_min = 0; tm_hour = 12; tm_mday = d
      ; tm_mon = m - 1; tm_year = y - 1900; tm_wday = 0; tm_yday = 0
      ; tm_isdst = false }
    in
    let t, _ = Unix.mktime tm in
    t /. 86400.
  in
  match args with
  | [ VStr a; VStr b ] ->
      (match parse a, parse b with
       | Some (y1, m1, d1), Some (y2, m2, d2) ->
           let n =
             int_of_float
               (Float.round (epoch_days y2 m2 d2 -. epoch_days y1 m1 d1))
           in
           VInt (Int64.of_int n)
       | _ ->
           raise (Interp_error (User_error "date-diff-days: bad date")))
  | _ ->
      raise
        (Interp_error (User_error "date-diff-days needs two YYYY-MM-DD strings"))

(** FSRS-4.5 schedule step (open-spaced-repetition defaults).

    (fsrs-schedule stability difficulty elapsed_days rating)
      stability/difficulty: float or int; use 0 0 for a new card
      elapsed_days: days since last review (0 for first)
      rating: 1..4 or again|hard|good|easy

    Returns (list S' D' interval_days) where interval_days is int >= 0
    (0 means relearn today). request_retention fixed at 0.9. *)
let fsrs_w : float array =
  [| 0.40255; 1.18385; 3.173; 15.69105; 7.1949; 0.5345; 1.4604; 0.0046;
     1.54575; 0.1192; 1.01925; 1.9395; 0.11; 0.29605; 2.2698; 0.2315;
     2.9898; 0.51655; 0.6621 |]

let fsrs_clamp_d (d : float) : float = max 1. (min 10. d)

let fsrs_init_stability (grade : int) : float =
  (* w[0..3] for Again..Easy *)
  max 0.1 fsrs_w.(grade - 1)

let fsrs_init_difficulty (grade : int) : float =
  fsrs_clamp_d (fsrs_w.(4) -. exp (fsrs_w.(5) *. float_of_int (grade - 1)) +. 1.)

let fsrs_next_difficulty (d : float) (grade : int) : float =
  let delta = -. fsrs_w.(6) *. float_of_int (grade - 3) in
  let d' = d +. delta *. (10. -. d) /. 9. in
  let d0_good = fsrs_init_difficulty 3 in
  fsrs_clamp_d (fsrs_w.(7) *. d0_good +. (1. -. fsrs_w.(7)) *. d')

let fsrs_retrievability ~(s : float) ~(elapsed : float) : float =
  if s <= 0. then 0.
  else (1. +. elapsed /. (9. *. s)) ** (-1.)

let fsrs_next_stability_success ~(s : float) ~(d : float) ~(r : float)
    ~(grade : int) : float =
  let hard_pen = if grade = 2 then fsrs_w.(15) else 1. in
  let easy_bon = if grade = 4 then fsrs_w.(16) else 1. in
  let s' =
    s
    *. (exp fsrs_w.(8)
        *. (11. -. d)
        *. (s ** (-. fsrs_w.(9)))
        *. (exp (fsrs_w.(10) *. (1. -. r)) -. 1.)
        *. hard_pen
        *. easy_bon
        +. 1.)
  in
  max s' s  (* stability should not decrease on success in v1 *)

let fsrs_next_stability_fail ~(s : float) ~(d : float) ~(r : float) : float =
  let s' =
    fsrs_w.(11)
    *. (d ** (-. fsrs_w.(12)))
    *. (((s +. 1.) ** fsrs_w.(13)) -. 1.)
    *. exp (fsrs_w.(14) *. (1. -. r))
  in
  min s (max 0.1 s')

let fsrs_interval_days ~(s : float) : int =
  (* For R=0.9, t ≈ S (FSRS-4 power formula). *)
  max 0 (int_of_float (Float.round (max 0. s)))

let fsrs_parse_grade = function
  | VInt n ->
      let g = Int64.to_int n in
      if g < 1 || g > 4 then None else Some g
  | VStr s ->
      (match String.lowercase_ascii (String.trim s) with
       | "again" | "1" -> Some 1
       | "hard" | "2" -> Some 2
       | "good" | "3" -> Some 3
       | "easy" | "4" -> Some 4
       | _ -> None)
  | _ -> None

let float_arg v =
  match num_of_value v with
  | NInt n -> Int64.to_float n
  | NFloat f -> f

let prim_fsrs_schedule (args : value list) : value =
  match args with
  | [ s_v; d_v; elapsed_v; grade_v ] ->
      (match fsrs_parse_grade grade_v with
       | None ->
           raise
             (Interp_error
                (User_error
                   "fsrs-schedule: rating must be 1..4 or again|hard|good|easy"))
       | Some grade ->
           let s0 = float_arg s_v in
           let d0 = float_arg d_v in
           let elapsed = max 0. (float_arg elapsed_v) in
           let is_new = s0 <= 0. || d0 <= 0. in
           let s, d =
             if is_new then
               (fsrs_init_stability grade, fsrs_init_difficulty grade)
             else
               let r = fsrs_retrievability ~s:s0 ~elapsed in
               let d' = fsrs_next_difficulty d0 grade in
               let s' =
                 if grade = 1 then
                   fsrs_next_stability_fail ~s:s0 ~d:d0 ~r
                 else
                   fsrs_next_stability_success ~s:s0 ~d:d0 ~r ~grade
               in
               (s', d')
           in
           let interval =
             if grade = 1 then 0
             else if is_new && grade = 2 then 1
             else if is_new && grade = 3 then max 1 (fsrs_interval_days ~s)
             else if is_new && grade = 4 then max 2 (fsrs_interval_days ~s)
             else max 1 (fsrs_interval_days ~s)
           in
           VList [ VFloat s; VFloat d; VInt (Int64.of_int interval) ])
  | _ ->
      raise
        (Interp_error
           (User_error
              "fsrs-schedule needs stability difficulty elapsed rating"))

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
  register_sync t "string-trim" prim_string_trim;
  register_sync t "html-escape" prim_html_escape;
  register_sync t "json-parse" prim_json_parse;
  register_sync t "json-emit" prim_json_emit;
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
  register_sync t "date-today" prim_date_today;
  register_sync t "parse-int" prim_parse_int;
  register_sync t "parse-float" prim_parse_float;
  register_sync t "floor" prim_floor;
  register_sync t "round" prim_round;
  register_sync t "date-weekday" prim_date_weekday;
  register_sync t "date-format" prim_date_format;
  register_sync t "date-add-days" prim_date_add_days;
  register_sync t "date-diff-days" prim_date_diff_days;
  register_sync t "fsrs-schedule" prim_fsrs_schedule;
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
  | "date-today" -> "local date YYYY-MM-DD"
  | "parse-int" -> "string or int to int"
  | "parse-float" -> "string or number to float"
  | "floor" | "round" -> "float to int"
  | "date-weekday" -> "weekday 0=Sun for y m d"
  | "date-format" -> "format y m d as YYYY-MM-DD"
  | "date-add-days" -> "add n days to YYYY-MM-DD (or today)"
  | "date-diff-days" -> "days between two YYYY-MM-DD strings"
  | "fsrs-schedule" -> "FSRS-4.5: (S D elapsed rating) -> (S' D' interval)"

  | "string-append" -> "concat strings"
  | "string-length" -> "length of string"
  | "substring" -> "substring start [len]"
  | "string-contains?" -> "haystack contains needle?"
  | "string-eq" -> "string equality"
  | "string-trim" -> "trim whitespace from string"
  | "html-escape" -> "escape text for HTML"
  | "json-parse" -> "parse JSON string to Lisp value"
  | "json-emit" -> "emit Lisp value as JSON string"
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
  | "find-random" -> "one random data row of a type"
  | "select-for-update" -> "lock row in transaction"
  | "agent-session-new" -> "create durable AI session: (agent-session-new seed [tools?])"
  | "agent-say" -> "append user msg + run tool loop: (agent-say sid prompt)"
  | "agent-history" -> "list messages in AI session"
  | "agent-sessions" -> "list recent AI sessions for whoami"
  | "agent-session-delete" -> "delete AI session and messages"
  | "agent" -> "alias: new session + one say (prefer agent-session-new/say)"
  | "agent-task" -> "alias: new tools session + one say"
  | "propose-function" -> "AI draft+install function on branch"
  | "install-function" -> "install AI body string as named function"
  | "function-source" -> "pretty-print stored function body"
  | "package-bindings" -> "list binding names in a package"
  | "export-package" -> "export package as lib.pp text"
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
