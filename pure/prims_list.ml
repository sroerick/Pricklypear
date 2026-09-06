(* List/pair primitives: construction, destructuring, HOF-free list
   ops, structural equality, alist lookup. All list traversal goes
   through Prims_common.list_of_value (the ONE improper-tail policy,
   i-1788671384). Split from prims_pure.ml. *)
open Pricklypear_lisp.Ast
open Prims_common


(* (list a b c ...) -> VList. Variadic. *)
let prim_list args = VList args

(* (car xs): head of a VList or VPair. Errors on empty / atoms. *)
let prim_car args =
  match args with
  | [ VList (h :: _) ] -> h
  | [ VList [] ] -> VNil
  | [ VPair (a, _) ] -> a
  | _ -> raise (Interp_error (User_error ("car needs a list or pair", "bad-args")))

(* (cdr xs): tail of a VList or VPair. *)
let prim_cdr args =
  match args with
  | [ VList (_ :: rest) ] -> VList rest
  | [ VList [] ] -> VNil
  | [ VPair (_, b) ] -> b
  | _ -> raise (Interp_error (User_error ("cdr needs a list or pair", "bad-args")))

(* (cons a b) -> VPair. Classic Lisp cons cell. *)
let prim_cons args =
  match args with
  | [ a; b ] -> VPair (a, b)
  | _ -> raise (Interp_error (User_error ("cons needs two args", "bad-args")))

(* (null? x) -> VBool. True if VNil. *)
let prim_null_p args =
  match args with
  | [ VNil ] -> VBool true
  | [ VList [] ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error ("null? needs one arg", "bad-args")))

(* (pair? x) -> VBool. True if VPair or non-empty VList. *)
let prim_pair_p args =
  match args with
  | [ VPair _ ] -> VBool true
  | [ VList (_ :: _) ] -> VBool true
  | _ -> VBool false

(** Unwrap macro string tags produced by expand_and_eval_macro:
    (__str__ "foo") is a VList that must behave like VStr for string prims. *)

(* (sort lst) -> stable ascending list. All-number lists (int/float
   mixed) sort numerically; all-string lists lexicographically; mixed
   kinds are a User_error. Spec: image.borg stdlib-round-2
   (BORGE-1788622799). *)
let prim_sort args =
  match args with
  | [ VList items ] ->
      let kind = function
        | VInt _ | VFloat _ -> `Num
        | VStr _ -> `Str
        | _ -> `Other
      in
      let kinds = List.map kind items in
      let all k = List.for_all (fun k' -> k' = k) kinds in
      if all `Num then
        VList
          (List.stable_sort
             (fun a b ->
               compare
                 (float_of_num (num_of_value a))
                 (float_of_num (num_of_value b)))
             items)
      else if all `Str then VList (List.stable_sort compare items)
      else
        raise
          (Interp_error
             (User_error
                ( "sort: mixed types (need all numbers or all strings)"
                , "bad-args" )))
  | _ ->
      raise
        (Interp_error (User_error ("sort needs one list", "bad-args")))

(* prim_string_split is DEFINED LATER in this file (agent section, ~line
   1830) — it is now ALSO a kernel image prim (habitat-book web-renderer:
   the interpreted libs/std char-walk cost ~interpreter steps per input
   byte, so one 3KB prose paragraph blew the 100k-step page budget).
   libs/std no longer defines it; keep agent semantics in one place. *)

let prim_append args =
  match args with
  | [ a; b ] -> VList (list_of_value a @ list_of_value b)
  | xs -> VList (List.concat_map list_of_value xs)

let prim_reverse args =
  match args with
  | [ xs ] -> VList (List.rev (list_of_value xs))
  | _ -> raise (Interp_error (User_error ("reverse needs one list", "bad-args")))

let prim_nth args =
  match args with
  | [ xs; VInt i ] ->
      let n = Int64.to_int i in
      let lst = list_of_value xs in
      if n < 0 || n >= List.length lst then VNil
      else List.nth lst n
  | _ -> raise (Interp_error (User_error ("nth needs list and VInt", "bad-args")))

(* (err/shape msg kind) -> VList (error msg kind).
   Constructs the guard-handler payload shape as data so Lisp code can
   build/inspect error envelopes without raising. Mirrors the VList the
   guard special form delivers to its handler lambda. See design note
   error-shape-design §3,§x-2a. *)
let prim_err_shape args =
  match args with
  | [ VStr msg; VStr kind ] ->
      VList [ VSymbol "error"; VStr msg; VSymbol kind ]
  | [ msg; kind ] ->
      (* coerce: symbols accepted as kind/msg for ergonomics *)
      let s = function VStr s -> s | VSymbol s -> s | v -> Pricklypear_lisp.Reader.pp_value v in
      VList [ VSymbol "error"; VStr (s msg); VSymbol (s kind) ]
  | _ -> raise (Interp_error (User_error ("err/shape needs msg and kind", "bad-args")))

(* (count list) -> int. Demonstrates a prim that walks the evaluator's
   own value type. *)
let prim_count args =
  match args with
  | [ VList items ] -> VInt (Int64.of_int (List.length items))
  | [ VNil ] -> VInt 0L
  | _ -> raise (Interp_error (User_error ("count needs a list", "bad-args")))

(* (length xs) -> VInt. Like count but accepts VList, VNil, and proper
   VPair chains (the conventional Lisp `length`). Frozen std name. *)
let prim_length args =
  match args with
  | [ xs ] -> VInt (Int64.of_int (List.length (list_of_value xs)))
  | _ -> raise (Interp_error (User_error ("length needs one list", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("equal? needs two args", "bad-args")))

(* c*r compositions: reuse car/cdr so error messages + VPair/VList
   handling stay consistent. cadr/caddr/caar/cddr are bread-and-butter
   for code-as-data walking. *)
let prim_cadr args =
  match args with
  | [ v ] -> prim_car [ prim_cdr [ v ] ]
  | _ -> raise (Interp_error (User_error ("cadr needs one arg", "bad-args")))

let prim_caar args =
  match args with
  | [ v ] -> prim_car [ prim_car [ v ] ]
  | _ -> raise (Interp_error (User_error ("caar needs one arg", "bad-args")))

let prim_caddr args =
  match args with
  | [ v ] -> prim_car [ prim_cdr [ prim_cdr [ v ] ] ]
  | _ -> raise (Interp_error (User_error ("caddr needs one arg", "bad-args")))

let prim_cddr args =
  match args with
  | [ v ] -> prim_cdr [ prim_cdr [ v ] ]
  | _ -> raise (Interp_error (User_error ("cddr needs one arg", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("assoc needs key and alist", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("member needs x and list", "bad-args")))
