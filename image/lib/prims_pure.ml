(** Pure Lisp primitives (no DB, no Session.t).
    Registered by [Primitives.make_with_defaults]. *)
open Pricklypear_lisp.Ast

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

(* "+" sums zero-or-more numeric args. Variadic; (+) = 0 (additive identity). *)
let prim_add (args : value list) : value =
  value_of_num
    (List.fold_left
       (fun acc v -> num_add acc (num_of_value v))
       (NInt 0L)
       args)

(* "-" binary: (- a b) = a - b; unary: (- a) = -a. *)
let prim_sub args =
  match args with
  | [ a ] -> value_of_num (num_neg (num_of_value a))
  | [ a; b ] -> value_of_num (num_sub (num_of_value a) (num_of_value b))
  | _ -> raise (Interp_error (User_error "- needs one or two numeric args"))

(* "*". Zero-or-more numeric args. Variadic; empty product is 1 (multiplicative identity). *)
let prim_mul (args : value list) : value =
  value_of_num
    (List.fold_left
       (fun acc v -> num_mul acc (num_of_value v))
       (NInt 1L)
       args)

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

(** Unwrap macro string tags produced by expand_and_eval_macro:
    (__str__ "foo") is a VList that must behave like VStr for string prims. *)
let rec coerce_str ?(ctx = "string") (v : value) : string =
  match v with
  | VStr s -> s
  | VList [ VStr "__str__"; VStr s ] -> s
  | VInt n -> Int64.to_string n
  | VFloat f -> Printf.sprintf "%.12g" f
  | VBool true -> "true"
  | VBool false -> "false"
  | VNil -> ""
  | _ ->
      raise
        (Interp_error
           (User_error (ctx ^ " needs a string (got " ^
                        Pricklypear_lisp.Reader.pp_value v ^ ")")))

(* (string-append s1 s2 ...) -> VStr. Variadic over VStr / __str__ tags. *)
let prim_string_append args =
  let buf = Buffer.create 32 in
  List.iter
    (fun v -> Buffer.add_string buf (coerce_str ~ctx:"string-append" v))
    args;
  VStr (Buffer.contents buf)

(* (type-of x) -> VStr. Returns a type tag for runtime reflection. *)
let prim_type_of args =
  match args with
  | [ VInt _ ] -> VStr "int"
  | [ VFloat _ ] -> VStr "float"
  | [ VStr _ ] -> VStr "str"
  | [ VList [ VStr "__str__"; VStr _ ] ] -> VStr "str"
  | [ VBool _ ] -> VStr "bool"
  | [ VNil ] -> VStr "nil"
  | [ VPair _ ] -> VStr "pair"
  | [ VList _ ] -> VStr "list"
  | [ VLambda _ ] -> VStr "lambda"
  | [ VMacro _ ] -> VStr "macro"
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
  | [ v ] -> VStr (coerce_str ~ctx:"as-str" v)
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
  | [ v ] ->
      let s = coerce_str ~ctx:"string-length" v in
      VInt (Int64.of_int (String.length s))
  | _ -> raise (Interp_error (User_error "string-length needs one VStr"))

let prim_substring args =
  match args with
  | [ sv; VInt start; VInt len ] ->
      let s = coerce_str ~ctx:"substring" sv in
      let st = Int64.to_int start in
      let n = Int64.to_int len in
      let sl = String.length s in
      if st < 0 || n < 0 || st > sl then
        raise (Interp_error (User_error "substring out of range"))
      else
        let n = min n (sl - st) in
        VStr (String.sub s st n)
  | [ sv; VInt start ] ->
      let s = coerce_str ~ctx:"substring" sv in
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
  | [ hay_v; needle_v ] ->
      let hay = coerce_str ~ctx:"string-contains?" hay_v in
      let needle = coerce_str ~ctx:"string-contains?" needle_v in
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
  | [ a; b ] ->
      VBool
        (String.equal
           (coerce_str ~ctx:"string-eq" a)
           (coerce_str ~ctx:"string-eq" b))
  | _ -> raise (Interp_error (User_error "string-eq needs 2 strings"))

(* (string-trim s) -> VStr. Trim leading/trailing whitespace. *)
let prim_string_trim args =
  match args with
  | [ v ] -> VStr (String.trim (coerce_str ~ctx:"string-trim" v))
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

(* (read-string s) -> quoted value tree. Same reader as load-string / the
   REPL, then quote_to_value — NO eval. Pair with (function-ast name):
   both yield walkable s-expr data. Clojure-style twin of load-string.
   (read-all-string s) -> VList of forms (multi top-level). *)
let prim_read_string args =
  match args with
  | [ VStr s ] ->
      (try
         let e = Pricklypear_lisp.Reader.read_one s in
         Pricklypear_lisp.Eval.quote_to_value e
       with
       | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
           raise
             (Interp_error
                (User_error
                   (Printf.sprintf "read-string: %s (%d:%d)" msg line col)))
       | exn ->
           raise
             (Interp_error
                (User_error
                   ("read-string: " ^ Printexc.to_string exn))))
  | _ -> raise (Interp_error (User_error "read-string needs 1 string"))

let prim_read_all_string args =
  match args with
  | [ VStr s ] ->
      (try
         let es = Pricklypear_lisp.Reader.read_all s in
         VList (List.map Pricklypear_lisp.Eval.quote_to_value es)
       with
       | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
           raise
             (Interp_error
                (User_error
                   (Printf.sprintf "read-all-string: %s (%d:%d)" msg line col)))
       | exn ->
           raise
             (Interp_error
                (User_error
                   ("read-all-string: " ^ Printexc.to_string exn))))
  | _ -> raise (Interp_error (User_error "read-all-string needs 1 string"))

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
                 (User_error "extract-links needs a string body"))
      in
      of_body body
  | _ ->
      raise
        (Interp_error (User_error "extract-links needs one string arg"))

(* (budget-remaining) -> VInt steps left in current eval call. *)
let prim_budget_remaining _args =
  VInt (Int64.of_int (Pricklypear_lisp.Eval.budget_remaining_steps ()))

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
      let j = ref i in
      while !j < len && not (at_open !j) do
        incr j
      done;
      html_escape_to_buffer buf (String.sub s i (!j - i));
      loop !j
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
                 (User_error "render-links needs a string body"))
      in
      of_body body
  | _ ->
      raise
        (Interp_error (User_error "render-links needs one string arg"))

