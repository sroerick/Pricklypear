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
  | VSymbol s -> s (* product HTML/paths; symbols print as names *)
  | VList [ VStr "__str__"; VStr s ] -> s
  | VList [ VSymbol "__str__"; VStr s ] -> s
  | VInt n -> Int64.to_string n
  | VFloat f -> Printf.sprintf "%.12g" f
  | VBool true -> "true"
  | VBool false -> "false"
  | VNil -> ""
  (* Empty list is null? in Lisp and used as empty html attrs: (html/p* (list) body).
     Treat like nil so as-str/html/tag do not blow up product home widgets. *)
  | VList [] -> ""
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
  | [ VSymbol _ ] -> VStr "symbol"
  | [ VList [ VStr "__str__"; VStr _ ] ] -> VStr "str"
  | [ VList [ VSymbol "__str__"; VStr _ ] ] -> VStr "str"
  | [ VBool _ ] -> VStr "bool"
  | [ VNil ] -> VStr "nil"
  | [ VPair _ ] -> VStr "pair"
  | [ VList _ ] -> VStr "list"
  | [ VLambda _ ] -> VStr "lambda"
  | [ VMacro _ ] -> VStr "macro"
  | [ VPrim name ] -> VStr ("prim:" ^ name)
  | [ VRef (t, _) ] -> VStr ("ref:" ^ t)
  | _ -> raise (Interp_error (User_error "type-of needs one arg"))

let prim_symbol_p = function
  | [ VSymbol _ ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error "symbol? needs one arg"))

let prim_string_p = function
  | [ VStr _ ] -> VBool true
  | [ VList [ VStr "__str__"; VStr _ ] ]
  | [ VList [ VSymbol "__str__"; VStr _ ] ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error "string? needs one arg"))

let prim_symbol_eq = function
  | [ VSymbol a; VSymbol b ] -> VBool (String.equal a b)
  | [ _; _ ] -> VBool false
  | _ -> raise (Interp_error (User_error "symbol=? needs two args"))

let prim_symbol_to_string = function
  | [ VSymbol s ] -> VStr s
  | [ VStr s ] -> VStr s (* soft: already a string *)
  | _ -> raise (Interp_error (User_error "symbol->string needs a symbol"))

let prim_string_to_symbol = function
  | [ VStr s ] -> VSymbol s
  | [ VSymbol s ] -> VSymbol s
  | _ -> raise (Interp_error (User_error "string->symbol needs a string"))

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

(* Scheme aliases of the int paths of div/mod — reject floats. *)
let prim_quotient args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.div a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error "quotient by zero"))
  | [ VInt _; _ ] | [ _; VInt _ ] | [ _; _ ] ->
      raise (Interp_error (User_error "quotient needs two int args"))
  | _ -> raise (Interp_error (User_error "quotient needs two int args"))

let prim_remainder args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.rem a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error "remainder by zero"))
  | [ _; _ ] ->
      raise (Interp_error (User_error "remainder needs two int args"))
  | _ -> raise (Interp_error (User_error "remainder needs two int args"))

(* ---- math tower (lisp-interpreter.math) ---- *)

let int_pow (base : int64) (exp : int64) : int64 =
  (* Non-negative exponent only; Int64 overflow wraps like Int64.mul.
     Exponentiation by squaring — O(log e), not O(e). *)
  let rec loop acc b e =
    if e = 0L then acc
    else if Int64.logand e 1L = 1L then
      loop (Int64.mul acc b) (Int64.mul b b) (Int64.shift_right_logical e 1)
    else
      loop acc (Int64.mul b b) (Int64.shift_right_logical e 1)
  in
  if exp < 0L then
    raise (Interp_error (User_error "expt int path needs non-negative exponent"))
  else loop 1L base exp

let prim_expt args =
  match args with
  | [ a; b ] ->
      (match num_of_value a, num_of_value b with
       | NInt base, NInt e when e >= 0L -> VInt (int_pow base e)
       | ba, bb ->
           VFloat (Float.pow (float_of_num ba) (float_of_num bb)))
  | _ -> raise (Interp_error (User_error "expt needs two numeric args"))

let prim_sqrt args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x < 0. then
        raise (Interp_error (User_error "sqrt of negative"))
      else VFloat (sqrt x)
  | _ -> raise (Interp_error (User_error "sqrt needs one numeric arg"))

let prim_exp args =
  match args with
  | [ a ] -> VFloat (exp (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error "exp needs one numeric arg"))

let prim_log args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x <= 0. then
        raise (Interp_error (User_error "log of non-positive"))
      else VFloat (log x)
  | [ a; b ] ->
      let x = float_of_num (num_of_value a) in
      let base = float_of_num (num_of_value b) in
      if x <= 0. then
        raise (Interp_error (User_error "log of non-positive"))
      else if base <= 0. || base = 1. then
        raise (Interp_error (User_error "log: invalid base"))
      else VFloat (log x /. log base)
  | _ -> raise (Interp_error (User_error "log needs one or two numeric args"))

let prim_sin args =
  match args with
  | [ a ] -> VFloat (sin (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error "sin needs one numeric arg"))

let prim_cos args =
  match args with
  | [ a ] -> VFloat (cos (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error "cos needs one numeric arg"))

let prim_tan args =
  match args with
  | [ a ] -> VFloat (tan (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error "tan needs one numeric arg"))

let prim_asin args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x < -1. || x > 1. then
        raise (Interp_error (User_error "asin domain [-1,1]"))
      else VFloat (asin x)
  | _ -> raise (Interp_error (User_error "asin needs one numeric arg"))

let prim_acos args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x < -1. || x > 1. then
        raise (Interp_error (User_error "acos domain [-1,1]"))
      else VFloat (acos x)
  | _ -> raise (Interp_error (User_error "acos needs one numeric arg"))

let prim_atan args =
  match args with
  | [ a ] -> VFloat (atan (float_of_num (num_of_value a)))
  | [ a; b ] ->
      (* Two-arg form → atan2(y, x) with y first (Scheme order). *)
      let y = float_of_num (num_of_value a) in
      let x = float_of_num (num_of_value b) in
      VFloat (atan2 y x)
  | _ -> raise (Interp_error (User_error "atan needs one or two numeric args"))

let prim_number_p args =
  match args with
  | [ VInt _ ] | [ VFloat _ ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error "number? needs one arg"))

let prim_integer_p args =
  match args with
  | [ VInt _ ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error "integer? needs one arg"))

let prim_zero_p args =
  match args with
  | [ a ] -> VBool (num_compare (num_of_value a) (NInt 0L) = 0)
  | _ -> raise (Interp_error (User_error "zero? needs one numeric arg"))

let prim_positive_p args =
  match args with
  | [ a ] -> VBool (num_compare (num_of_value a) (NInt 0L) > 0)
  | _ -> raise (Interp_error (User_error "positive? needs one numeric arg"))

let prim_negative_p args =
  match args with
  | [ a ] -> VBool (num_compare (num_of_value a) (NInt 0L) < 0)
  | _ -> raise (Interp_error (User_error "negative? needs one numeric arg"))

let as_integral_int64 = function
  | VInt n -> n
  | VFloat f ->
      if Float.is_integer f then Int64.of_float f
      else raise (Interp_error (User_error "even?/odd? needs an integer"))
  | _ -> raise (Interp_error (User_error "even?/odd? needs an integer"))

let prim_even_p args =
  match args with
  | [ a ] ->
      let n = as_integral_int64 a in
      VBool (Int64.logand n 1L = 0L)
  | _ -> raise (Interp_error (User_error "even? needs one arg"))

let prim_odd_p args =
  match args with
  | [ a ] ->
      let n = as_integral_int64 a in
      VBool (Int64.logand n 1L <> 0L)
  | _ -> raise (Interp_error (User_error "odd? needs one arg"))

(** (random) -> float [0,1); (random n) -> int [0,n) for VInt n > 0. *)
let prim_random args =
  match args with
  | [] -> VFloat (Random.float 1.0)
  | [ VInt n ] when n > 0L ->
      (* Random.int takes int; clamp to positive native int range. *)
      let n_int =
        if n > Int64.of_int max_int then max_int else Int64.to_int n
      in
      VInt (Int64.of_int (Random.int n_int))
  | [ VInt _ ] ->
      raise (Interp_error (User_error "random: n must be positive int"))
  | _ -> raise (Interp_error (User_error "random needs 0 args or one positive int"))

(** (random-seed s) reseeds process PRNG; returns nil. *)
let prim_random_seed args =
  match args with
  | [ VInt s ] ->
      let seed =
        if s > Int64.of_int max_int then max_int
        else if s < Int64.of_int min_int then min_int
        else Int64.to_int s
      in
      Random.init seed;
      VNil
  | _ -> raise (Interp_error (User_error "random-seed needs one int"))

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
let status_sentinel = "__status__:"

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
  | _ -> raise (Interp_error (User_error "status-response needs code + body"))

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

(* ------------------------------------------------------------------
   multipart/mixed values (multipart.mime-framing)

   Structured as VLists with symbolic tags (not string sentinels) so
   parts keep headers as data:
     part  = ( __pp_part__  body content-type target-or-() swap-or-() id-or-() )
     multi = ( __pp_multipart__  env-target env-swap  part… )
   ------------------------------------------------------------------ *)

let part_tag = "__pp_part__"
let multipart_tag = "__pp_multipart__"

type mime_part =
  { body : string
  ; content_type : string
  ; target : string option
  ; swap : string option
  ; id : string option
  }

type mime_multipart =
  { envelope_target : string option
  ; envelope_swap : string option
  ; parts : mime_part list
  }

let opt_str = function
  | VNil | VList [] -> None
  | VStr s -> Some s
  | VSymbol s -> Some s
  | other -> Some (Pricklypear_lisp.Reader.pp_value other)

let str_or_raw = function
  | VStr s ->
      (match is_raw_html (VStr s) with Some raw -> raw | None -> s)
  | VSymbol s -> s
  | other -> Pricklypear_lisp.Reader.pp_value other

let kw_name = function
  | VSymbol s when String.length s > 1 && s.[0] = ':' ->
      Some (String.sub s 1 (String.length s - 1))
  | VStr s when String.length s > 1 && s.[0] = ':' ->
      Some (String.sub s 1 (String.length s - 1))
  | _ -> None

(** Split args into (kwargs, positionals).
    Supports leading kwargs, trailing kwargs, or positionals-then-kwargs:
      (part html :target "#x")  (multipart :target "#f" p1 p2) *)
let split_kwargs (args : value list) : (string * value) list * value list =
  let rec go kws pos = function
    | k :: v :: rest ->
        (match kw_name k with
         | Some name when String.length name > 0 ->
             go ((name, v) :: kws) pos rest
         | _ -> go kws (k :: pos) (v :: rest))
    | [ last ] -> go kws (last :: pos) []
    | [] -> (List.rev kws, List.rev pos)
  in
  go [] [] args

let kw_get (kws : (string * value) list) (name : string) : string option =
  match List.assoc_opt name kws with
  | None -> None
  | Some v -> opt_str v

let make_part_value ~(body : string) ~(content_type : string)
    ~(target : string option) ~(swap : string option) ~(id : string option)
  : value =
  let o = function None -> VNil | Some s -> VStr s in
  VList
    [ VSymbol part_tag
    ; VStr body
    ; VStr content_type
    ; o target
    ; o swap
    ; o id
    ]

let part_of_value = function
  | VList
      [ VSymbol t; VStr body; VStr ct; target; swap; id ]
    when t = part_tag ->
      Some
        { body
        ; content_type = ct
        ; target = opt_str target
        ; swap = opt_str swap
        ; id = opt_str id
        }
  | _ -> None

let is_part (v : value) : mime_part option = part_of_value v

let is_multipart (v : value) : mime_multipart option =
  match v with
  | VList (VSymbol t :: env_target :: env_swap :: rest) when t = multipart_tag
    ->
      let parts =
        List.filter_map part_of_value rest
      in
      if parts = [] && rest <> [] then None
      else
        Some
          { envelope_target = opt_str env_target
          ; envelope_swap = opt_str env_swap
          ; parts
          }
  | _ -> None

(* (part html [:target t] [:swap s] [:id i]) *)
let prim_part args =
  let kws, pos = split_kwargs args in
  match pos with
  | [ body ] ->
      make_part_value
        ~body:(str_or_raw body)
        ~content_type:"text/html; charset=utf-8"
        ~target:(kw_get kws "target")
        ~swap:(kw_get kws "swap")
        ~id:(kw_get kws "id")
  | _ ->
      raise
        (Interp_error
           (User_error
              "part needs (part html [:target t] [:swap s] [:id i])"))

(* (json-part json-string [:id i]) *)
let prim_json_part args =
  let kws, pos = split_kwargs args in
  match pos with
  | [ body ] ->
      make_part_value
        ~body:(str_or_raw body)
        ~content_type:"application/json"
        ~target:(kw_get kws "target")
        ~swap:(kw_get kws "swap")
        ~id:(kw_get kws "id")
  | _ ->
      raise
        (Interp_error
           (User_error "json-part needs (json-part json [:id i])"))

(* (multipart [:target t] [:swap s] part…)
   Also accepts a single list of parts: (multipart :target "#x" parts-list) *)
let prim_multipart args =
  let kws, pos = split_kwargs args in
  let pos =
    match pos with
    | [ VList items ] when items <> [] && List.for_all (fun v -> part_of_value v <> None) items
      -> items
    | [ VList items ] when items <> [] -> items
    | _ -> pos
  in
  let parts =
    List.map
      (fun v ->
        match part_of_value v with
        | Some _ -> v
        | None ->
            (* Bare string / raw-html becomes an html part. *)
            make_part_value
              ~body:(str_or_raw v)
              ~content_type:"text/html; charset=utf-8"
              ~target:None ~swap:None ~id:None)
      pos
  in
  let o = function None -> VNil | Some s -> VStr s in
  VList
    ( VSymbol multipart_tag
      :: o (kw_get kws "target")
      :: o (kw_get kws "swap")
      :: parts )

(* (string-eq a b) -> VBool. String equality for request-method checks etc. *)
let prim_string_eq args =
  match args with
  | [ a; b ] ->
      VBool
        (String.equal
           (coerce_str ~ctx:"string-eq" a)
           (coerce_str ~ctx:"string-eq" b))
  | _ -> raise (Interp_error (User_error "string-eq needs 2 strings"))

(* (string<? a b), (string<=? a b), (string>? a b), (string>=? a b) -> VBool.
   Byte-wise lexicographic comparison (NOT numeric). Correct ordering for
   ISO dates (YYYY-MM-DD) and other fixed-width formats. *)
let prim_string_cmp name op args =
  match args with
  | [ a; b ] ->
      VBool
        (op
           (String.compare
              (coerce_str ~ctx:name a)
              (coerce_str ~ctx:name b))
           0)
  | _ ->
      raise
        (Interp_error
           (User_error (Printf.sprintf "%s needs 2 strings" name)))

let prim_string_lt args = prim_string_cmp "string<?" ( < ) args
let prim_string_le args = prim_string_cmp "string<=?" ( <= ) args
let prim_string_gt args = prim_string_cmp "string>?" ( > ) args
let prim_string_ge args = prim_string_cmp "string>=?" ( >= ) args

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

(* (length xs) -> VInt. Like count but accepts VList, VNil, and proper
   VPair chains (the conventional Lisp `length`). Frozen std name. *)
let prim_length args =
  match args with
  | [ xs ] -> VInt (Int64.of_int (List.length (list_of_value xs)))
  | _ -> raise (Interp_error (User_error "length needs one list"))

(* Structural deep equality (equal?): recurses VList/VPair/VStr/VInt/
   VFloat/VBool/VSymbol/VNil. VList and VPair chains are compared
   element-wise as sequences, so `(list 1 2 3)` is equal? to the
   equivalent cons-chain; a dotted/improper pair is NOT equal? to the
   same proper list (the atom tail falls through to a type mismatch).
   VNil and the empty VList are equal (both the empty list). Different
   value types are not equal? (1 is not 1.0). *)
let rec value_equal (a : value) (b : value) : bool =
  match a, b with
  | VInt x, VInt y -> Int64.equal x y
  | VFloat x, VFloat y -> x = y
  | VBool x, VBool y -> x = y
  | VStr x, VStr y -> String.equal x y
  | VSymbol x, VSymbol y -> String.equal x y
  | VNil, VNil -> true
  | VList [], VNil | VNil, VList [] | VList [], VList [] -> true
  | VList (h1 :: t1), VList (h2 :: t2) ->
      value_equal h1 h2 && value_equal (VList t1) (VList t2)
  | VList (h1 :: t1), VPair (h2, t2) ->
      value_equal h1 h2 && value_equal (VList t1) t2
  | VPair (h1, t1), VList (h2 :: t2) ->
      value_equal h1 h2 && value_equal t1 (VList t2)
  | VPair (h1, t1), VPair (h2, t2) ->
      value_equal h1 h2 && value_equal t1 t2
  | _ -> false

let prim_equal_p args =
  match args with
  | [ a; b ] -> VBool (value_equal a b)
  | _ -> raise (Interp_error (User_error "equal? needs two args"))

(* c*r compositions: reuse car/cdr so error messages + VPair/VList
   handling stay consistent. cadr/caddr/caar/cddr are bread-and-butter
   for code-as-data walking. *)
let prim_cadr args =
  match args with
  | [ v ] -> prim_car [ prim_cdr [ v ] ]
  | _ -> raise (Interp_error (User_error "cadr needs one arg"))

let prim_caar args =
  match args with
  | [ v ] -> prim_car [ prim_car [ v ] ]
  | _ -> raise (Interp_error (User_error "caar needs one arg"))

let prim_caddr args =
  match args with
  | [ v ] -> prim_car [ prim_cdr [ prim_cdr [ v ] ] ]
  | _ -> raise (Interp_error (User_error "caddr needs one arg"))

let prim_cddr args =
  match args with
  | [ v ] -> prim_cdr [ prim_cdr [ v ] ]
  | _ -> raise (Interp_error (User_error "cddr needs one arg"))

(* (assoc key alist) -> the first entry (pair or list) whose car is
   equal? to key, or VNil if absent. Entries may be (k . v) pairs or
   (k v) two-element lists. *)
let prim_assoc args =
  match args with
  | [ key; alist ] ->
      let rec search = function
      | VNil | VList [] -> VNil
      | VList (entry :: rest) ->
          (match entry with
           | VPair (k, _) when value_equal k key -> entry
           | VList (k :: _) when value_equal k key -> entry
           | _ -> search (VList rest))
      | VPair (entry, rest) ->
          (match entry with
           | VPair (k, _) when value_equal k key -> entry
           | VList (k :: _) when value_equal k key -> entry
           | _ -> search rest)
      | _ -> VNil
      in
      search alist
  | _ -> raise (Interp_error (User_error "assoc needs key and alist"))

(* (member x list) -> the sublist starting at the first element
   equal? to x (truthy), or #f if absent. *)
let prim_member args =
  match args with
  | [ x; lst ] ->
      let rec search = function
      | VNil | VList [] -> VBool false
      | VList (h :: t) ->
          if value_equal h x then VList (h :: t) else search (VList t)
      | VPair (h, t) ->
          if value_equal h x then VPair (h, t) else search t
      | _ -> VBool false
      in
      search lst
  | _ -> raise (Interp_error (User_error "member needs x and list"))
(* (parse-int s|n) -> VInt. Soft-fail to 0 on bad string. *)
let prim_parse_int (args : value list) : value =
  match args with
  | [ VInt n ] -> VInt n
  | [ VStr s ] ->
      (try VInt (Int64.of_int (int_of_string (String.trim s)))
       with _ -> VInt 0L)
  | _ -> raise (Interp_error (User_error "parse-int needs string or int"))

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
                 (User_error "render-links needs a string body"))
      in
      of_body body
  | _ ->
      raise
        (Interp_error (User_error "render-links needs one string arg"))

(* ---- string helpers (P2 from kernel-io-primitives note) ---- *)

let prim_string_replace args =
  match args with
  | [ VStr s; VStr pat; VStr repl ] ->
      let plen = String.length pat in
      if plen = 0 then VStr s
      else
        let buf = Buffer.create (String.length s) in
        let rec loop i =
          if i + plen > String.length s then begin
            Buffer.add_substring buf s i (String.length s - i);
            Buffer.contents buf
          end else if String.sub s i plen = pat then begin
            Buffer.add_string buf repl;
            loop (i + plen)
          end else begin
            Buffer.add_char buf s.[i];
            loop (i + 1)
          end
        in
        VStr (loop 0)
  | _ -> raise (Interp_error (User_error "string-replace needs (s pat repl)"))

let prim_string_downcase args =
  match args with
  | [ v ] -> VStr (String.lowercase_ascii (coerce_str ~ctx:"string-downcase" v))
  | _ -> raise (Interp_error (User_error "string-downcase needs 1 string"))

let prim_string_upcase args =
  match args with
  | [ v ] -> VStr (String.uppercase_ascii (coerce_str ~ctx:"string-upcase" v))
  | _ -> raise (Interp_error (User_error "string-upcase needs 1 string"))

let prim_string_starts_with_p args =
  match args with
  | [ VStr s; VStr pfx ] -> VBool (String.starts_with ~prefix:pfx s)
  | [ a; b ] ->
      VBool (String.starts_with
               ~prefix:(coerce_str ~ctx:"string-starts-with?" b)
               (coerce_str ~ctx:"string-starts-with?" a))
  | _ -> raise (Interp_error (User_error "string-starts-with? needs 2 strings"))

let prim_string_ends_with_p args =
  match args with
  | [ VStr s; VStr sfx ] -> VBool (String.ends_with ~suffix:sfx s)
  | [ a; b ] ->
      VBool (String.ends_with
               ~suffix:(coerce_str ~ctx:"string-ends-with?" b)
               (coerce_str ~ctx:"string-ends-with?" a))
  | _ -> raise (Interp_error (User_error "string-ends-with? needs 2 strings"))

(* ---- url-encode / url-decode (percent-encoding) ---- *)

let prim_url_encode args =
  let encode s =
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '_' | '~' ->
            Buffer.add_char buf c
        | _ ->
            Buffer.add_string buf
              (Printf.sprintf "%%%02X" (Char.code c)))
      s;
    Buffer.contents buf
  in
  match args with
  | [ v ] -> VStr (encode (coerce_str ~ctx:"url-encode" v))
  | _ -> raise (Interp_error (User_error "url-encode needs 1 string"))

let prim_url_decode args =
  let decode s =
    let len = String.length s in
    let buf = Buffer.create len in
    let rec loop i =
      if i >= len then ()
      else if s.[i] = '%' && i + 2 < len then
        (try
           let n = int_of_string ("0x" ^ String.sub s (i + 1) 2) in
           Buffer.add_char buf (Char.chr n);
           loop (i + 3)
         with _ -> Buffer.add_char buf s.[i]; loop (i + 1))
      else if s.[i] = '+' then begin Buffer.add_char buf ' '; loop (i + 1) end
      else begin Buffer.add_char buf s.[i]; loop (i + 1) end
    in
    loop 0;
    Buffer.contents buf
  in
  match args with
  | [ v ] -> VStr (decode (coerce_str ~ctx:"url-decode" v))
  | _ -> raise (Interp_error (User_error "url-decode needs 1 string"))

(* ---- html-decode (entity decode) ----

   Handles &amp; &lt; &gt; &quot; &apos; &#NN; &#xHH; and common named entities.
   Best-effort; unknown entities are left as-is. *)

let named_entities =
  [ "amp", "&"; "lt", "<"; "gt", ">"; "quot", "\""; "apos", "'";
    "nbsp", "\240"; "copy", "\169"; "reg", "\174";
    "mdash", "\226\128\148"; "ndash", "\226\128\147";
    "hellip", "\226\128\166"; "rsquo", "\226\128\153"; "lsquo", "\226\128\152";
    "ldquo", "\226\128\156"; "rdquo", "\226\128\157"; "euro", "\226\130\172";
    "trade", "\226\132\162"; "deg", "\176"; "plusmn", "\191"; (* actual ± U+00B1 *)
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
  | _ -> raise (Interp_error (User_error "html-decode needs 1 string"))

(* ---- parse-date -> UTC epoch int (VInt) ----

   Accepts RFC-822, ISO-8601, and bare YYYY-MM-DD forms. Returns seconds
   since the Unix epoch (UTC) as a VInt, or VInt 0 on failure so Lisp can
   fall back to document order. UTC-anchored via a days-from-civil formula
   (no timezone drift). *)

let days_from_civil ~year ~month ~day =
  (* Howard Hinnant's algorithm; works for proleptic Gregorian, year >= 0. *)
  let y = if month <= 2 then year - 1 else year in
  let era = if y >= 0 then y / 400 else (y - 399) / 400 in
  let yoe = y - era * 400 in
  let m = month in
  let doy = (153 * (if m > 2 then m - 3 else m + 9) + 2) / 5 + day - 1 in
  let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy in
  era * 146097 + doe - 719468

let parse_int_sub s off len =
  try Some (int_of_string (String.sub s off len)) with _ -> None

let prim_parse_date args =
  let parse s =
    let s = String.trim s in
    let len = String.length s in
    (* strip optional leading day-of-week: "Tue, " or "Tue " *)
    let s =
      match String.index_opt s ',' with
      | Some i -> String.trim (String.sub s (i + 1) (len - i - 1))
      | None -> s
    in
    let len = String.length s in
    let months =
      [| "Jan", 1; "Feb", 2; "Mar", 3; "Apr", 4; "May", 5; "Jun", 6;
         "Jul", 7; "Aug", 8; "Sep", 9; "Oct", 10; "Nov", 11; "Dec", 12 |]
    in
    let month_of_name nm =
      let rec i = ref 0 in
      let found = ref None in
      while !i < 12 && !found = None do
        let (n, m) = months.(!i) in
        if String.equal (String.lowercase_ascii n) (String.lowercase_ascii nm)
        then found := Some m;
        incr i
      done;
      !found
    in
    (* ISO-8601: YYYY-MM-DDTHH:MM:SS[.f][Z|+HH:MM] or YYYY-MM-DD HH:MM:SS *)
    let try_iso () =
      if len >= 10
         && String.length s >= 10
         && String.sub s 0 4 |> String.for_all (fun c -> c >= '0' && c <= '9')
         && (s.[4] = '-' || s.[4] = '/')
      then
        let y = parse_int_sub s 0 4 in
        let mo = parse_int_sub s 5 2 in
        let d = parse_int_sub s 8 2 in
        let h, mi, sec =
          if len >= 19 && (s.[10] = 'T' || s.[10] = 't' || s.[10] = ' ') then
            (parse_int_sub s 11 2, parse_int_sub s 14 2, parse_int_sub s 17 2)
          else (Some 0, Some 0, Some 0)
        in
        match y, mo, d, h, mi, sec with
        | Some y, Some mo, Some d, Some h, Some mi, Some sec ->
            let days = days_from_civil ~year:y ~month:mo ~day:d in
            Some (Int64.of_int (days * 86400 + h * 3600 + mi * 60 + sec))
        | _ -> None
      else None
    in
    (* RFC-822: "28 Jul 2026 02:56:21 GMT" (day-of-week already stripped) *)
    let try_rfc822 () =
      let tokens = String.split_on_char ' ' s in
      let strip_tz = function
        | [ d; mo; y; t; _tz ] -> [ d; mo; y; t ]
        | other -> other
      in
      match strip_tz tokens with
      | [ d_str; mon_str; y_str; time_str ] ->
          (match parse_int_sub d_str 0 (String.length d_str),
                 month_of_name mon_str,
                 parse_int_sub y_str 0 (String.length y_str) with
           | Some d, Some mo, Some y ->
               let parts = String.split_on_char ':' time_str in
               (match parts with
                | [ hh; mm; ss ] ->
                    (match parse_int_sub hh 0 2, parse_int_sub mm 0 2,
                            parse_int_sub ss 0 2 with
                     | Some h, Some mi, Some se ->
                         let days = days_from_civil ~year:y ~month:mo ~day:d in
                         Some (Int64.of_int (days * 86400 + h * 3600 + mi * 60 + se))
                     | _ -> None)
                | [ hh; mm ] ->
                    (match parse_int_sub hh 0 2, parse_int_sub mm 0 2 with
                     | Some h, Some mi ->
                         let days = days_from_civil ~year:y ~month:mo ~day:d in
                         Some (Int64.of_int (days * 86400 + h * 3600 + mi * 60))
                     | _ -> None)
                | _ -> None)
           | _ -> None)
      | _ -> None
    in
    (* bare YYYY-MM-DD *)
    let try_bare () =
      if len >= 10 && (s.[4] = '-' || s.[4] = '/') then
        match parse_int_sub s 0 4, parse_int_sub s 5 2, parse_int_sub s 8 2 with
        | Some y, Some mo, Some d ->
            let days = days_from_civil ~year:y ~month:mo ~day:d in
            Some (Int64.of_int (days * 86400))
        | _ -> None
      else None
    in
    try_iso () |> function Some e -> Some e | None ->
    try_rfc822 () |> function Some e -> Some e | None ->
    try_bare ()
  in
  match args with
  | [ v ] ->
      (match parse (coerce_str ~ctx:"parse-date" v) with
       | Some e -> VInt e
       | None -> VInt 0L)
  | _ -> raise (Interp_error (User_error "parse-date needs 1 string"))

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
  | _ -> raise (Interp_error (User_error "xml-parse needs 1 string"))

(* ---- http response header parsing (helper for http-get prim) ----

   curl -D writes headers like:
     HTTP/2 200\r\n  (or "HTTP/1.1 200 OK")
     content-type: text/html\r\n
   Returns a JSON object string with lowercased keys. *)
(** (random-uuid) -> VStr canonical uuid. Uses stdlib Random (seeded at process start).
    Distinct from (random)/(random-seed) — those are the numeric PRNG surface. *)
let prim_random_uuid (_args : value list) : value =
  let b = Bytes.create 16 in
  for i = 0 to 15 do Bytes.set b i (Char.chr (Random.int 256)) done;
  (* RFC 4122 version 4 + variant 10xx *)
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let hex_byte i = Printf.sprintf "%02x" (Char.code (Bytes.get b i)) in
  VStr
    (String.concat ""
       [ hex_byte 0; hex_byte 1; hex_byte 2; hex_byte 3; "-"
       ; hex_byte 4; hex_byte 5; "-"
       ; hex_byte 6; hex_byte 7; "-"
       ; hex_byte 8; hex_byte 9; "-"
       ; hex_byte 10; hex_byte 11; hex_byte 12; hex_byte 13; hex_byte 14;
         hex_byte 15
       ])

let http_parse_headers (blob : string) : string =
  let lines = String.split_on_char '\n' blob in
  let acc = ref [] in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line = "" then ()
      else
        (match String.index_opt line ':' with
         | Some i ->
             let k = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
             let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
             acc := (k, v) :: !acc
         | None ->
             (* status line; ignore — we capture status separately *)
             ()))
    lines;
  Yojson.Safe.to_string (`Assoc (List.map (fun (k, v) -> (k, `String v)) (List.rev !acc)))

