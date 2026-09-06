(* Value layer: the shared coercion policies every prim module
   builds on (i-1788671384 split of prims_pure.ml).

   - numeric coercion ([num_of_value] family: int/float only)
   - string coercion ([coerce_str]: VStr/__str__ tags/symbols/numbers/
     bool/nil/empty-list — THE string policy, one implementation)
   - list unwrapping ([list_of_value] = the kernel's Eval.value_to_list:
     VList/VNil/proper VPair chains flatten, improper tails error — THE
     list policy, one implementation)
   - value reflection prims (type-of, symbol?/string?/symbol=?,
     symbol->string/string->symbol, str/as-str)
   - (budget-remaining) *)
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
                        | _ -> "other"), "bad-args")))

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
    raise (Interp_error (User_error ("div by zero", "bad-args")))
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
                        Pricklypear_lisp.Reader.pp_value v ^ ")", "bad-args")))

(* THE list-unwrapping policy (i-1788671384): ONE implementation, the
   kernel's own Eval.value_to_list. VList / VNil / proper VPair chains
   flatten; improper (atom) tails are an Interp_error — the same decision
   apply and the args evaluator make, and standard Lisp semantics
   ((length '(1 . 2)) is an error). The former five copies disagreed:
   length/append/reverse/nth/string-join/json-emit silently accepted an
   atom tail as a final element; render-tokens rejected VPair chains
   outright. All now share this one. *)
let list_of_value (v : value) : value list =
  Pricklypear_lisp.Eval.value_to_list v

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
  | _ -> raise (Interp_error (User_error ("type-of needs one arg", "bad-args")))

let prim_symbol_p = function
  | [ VSymbol _ ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error ("symbol? needs one arg", "bad-args")))

let prim_string_p = function
  | [ VStr _ ] -> VBool true
  | [ VList [ VStr "__str__"; VStr _ ] ]
  | [ VList [ VSymbol "__str__"; VStr _ ] ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error ("string? needs one arg", "bad-args")))

let prim_symbol_eq = function
  | [ VSymbol a; VSymbol b ] -> VBool (String.equal a b)
  | [ _; _ ] -> VBool false
  | _ -> raise (Interp_error (User_error ("symbol=? needs two args", "bad-args")))

let prim_symbol_to_string = function
  | [ VSymbol s ] -> VStr s
  | [ VStr s ] -> VStr s (* soft: already a string *)
  | _ -> raise (Interp_error (User_error ("symbol->string needs a symbol", "bad-args")))

let prim_string_to_symbol = function
  | [ VStr s ] -> VSymbol s
  | [ VSymbol s ] -> VSymbol s
  | _ -> raise (Interp_error (User_error ("string->symbol needs a string", "bad-args")))

(* (str x) -> VStr. Coerces any value to its pretty-printed form. *)
let prim_str args =
  match args with
  | [ v ] -> VStr (Pricklypear_lisp.Reader.pp_value v)
  | _ -> raise (Interp_error (User_error ("str needs one arg", "bad-args")))

(* (as-str x) -> VStr without Lisp quotes (for path/ref building). *)
let prim_as_str args =
  match args with
  | [ v ] -> VStr (coerce_str ~ctx:"as-str" v)
  | _ -> raise (Interp_error (User_error ("as-str needs one arg", "bad-args")))

(* (budget-remaining) -> VInt steps left in current eval call. *)
let prim_budget_remaining _args =
  VInt (Int64.of_int (Pricklypear_lisp.Eval.budget_remaining_steps ()))
