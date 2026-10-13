(** Heuristic big-O classifier for Nopales ASTs.

    Honest [Unknown] default. Pattern-matches common shapes only.
    Pure: no I/O. *)
open Ast
open Recursion

(** Asymptotic complexity class of a Nopales AST. [Unknown] when
    no recognized shape matches. *)
type complexity =
  | O_constant
  | O_log_n
  | O_linear
  | O_n_log_n
  | O_quadratic
  | O_exponential
  | Unknown

(** Human-readable badge for a complexity class (e.g. ["O(n²)"]). *)
let badge = function
  | O_constant -> "O(1)"
  | O_log_n -> "O(log n)"
  | O_linear -> "O(n)"
  | O_n_log_n -> "O(n log n)"
  | O_quadratic -> "O(n²)"
  | O_exponential -> "O(2ⁿ)"
  | Unknown -> "?"

(** The more expensive of two complexity classes (by rank). *)
let max_cx a b =
  let rank = function
    | O_constant -> 0
    | O_log_n -> 1
    | O_linear -> 2
    | O_n_log_n -> 3
    | O_quadratic -> 4
    | O_exponential -> 5
    | Unknown -> 6
  in
  if rank a >= rank b then a else b

(** Is [n] the name of a list-iteration primitive ([map], [foldl], …)? *)
let is_list_iter_prim = function
  | "list/map" | "list/filter" | "list/foldl" | "map" | "filter" | "foldl"
  | "for-each" -> true
  | _ -> false

(** Is [n] the name of a DB-scan primitive ([rows], [find-rows], …)? *)
let is_db_scan_prim = function
  | "rows" | "find-rows" | "find-text" -> true
  | _ -> false

(** Does [e] reach any DB-scan primitive (or call a function that does)? *)
let rec mentions_db_scan (e : expr) : bool =
  match e with
  | Prim (n, args) -> is_db_scan_prim n || List.exists mentions_db_scan args
  | App (Var n, args) when is_db_scan_prim n -> true
  | App (f, args) -> mentions_db_scan f || List.exists mentions_db_scan args
  | Let (_, a, b) | Seq (a, b) -> mentions_db_scan a || mentions_db_scan b
  | Letrec (bs, body) ->
      List.exists (fun (_, e) -> mentions_db_scan e) bs || mentions_db_scan body
  | If (a, b, c) ->
      mentions_db_scan a || mentions_db_scan b || mentions_db_scan c
  | Lambda (_, body) | Define (_, body) -> mentions_db_scan body
  | Var _ | Lit _ | Quote _ -> false

(** Does [e] contain any list-iteration primitive? *)
let rec has_list_iter (e : expr) : bool =
  match e with
  | Prim (n, args) -> is_list_iter_prim n || List.exists has_list_iter args
  | App (Var n, args) when is_list_iter_prim n -> true
  | App (f, args) -> has_list_iter f || List.exists has_list_iter args
  | Let (_, a, b) | Seq (a, b) -> has_list_iter a || has_list_iter b
  | Letrec (bs, body) ->
      List.exists (fun (_, e) -> has_list_iter e) bs || has_list_iter body
  | If (a, b, c) -> has_list_iter a || has_list_iter b || has_list_iter c
  | Lambda (_, body) | Define (_, body) -> has_list_iter body
  | Var _ | Lit _ | Quote _ -> false

(** Nested list-iter or list-iter over a DB scan → quadratic-ish. *)
let rec has_nested_iter (e : expr) : bool =
  let iter_body_has_iter = function
    | Prim (n, args) when is_list_iter_prim n ->
        List.exists has_list_iter args || List.exists mentions_db_scan args
    | App (Var n, args) when is_list_iter_prim n ->
        List.exists has_list_iter args || List.exists mentions_db_scan args
    | _ -> false
  in
  match e with
  | e when iter_body_has_iter e -> true
  | App (f, args) -> has_nested_iter f || List.exists has_nested_iter args
  | Prim (_, args) -> List.exists has_nested_iter args
  | Let (_, a, b) | Seq (a, b) -> has_nested_iter a || has_nested_iter b
  | Letrec (bs, body) ->
      List.exists (fun (_, e) -> has_nested_iter e) bs || has_nested_iter body
  | If (a, b, c) ->
      has_nested_iter a || has_nested_iter b || has_nested_iter c
  | Lambda (_, body) | Define (_, body) -> has_nested_iter body
  | Var _ | Lit _ | Quote _ -> false

(** Does [e] look like a shrinking argument ([cdr] or decrement by 1)? *)
let looks_like_cdr_arg (e : expr) : bool =
  match e with
  | App (Var "cdr", [ _ ]) | Prim ("cdr", [ _ ]) -> true
  | App (Var "-", [ _; Lit (VInt 1L) ]) | Prim ("-", [ _; Lit (VInt 1L) ])
    -> true
  | _ -> false

(** Does [e] look like a halving argument (divide by 2)? *)
let looks_like_halve (e : expr) : bool =
  match e with
  | App (Var op, [ _; Lit (VInt 2L) ]) when op = "/" || op = "div" || op = "halve"
    -> true
  | Prim (op, [ _; Lit (VInt 2L) ]) when op = "/" || op = "div" -> true
  | App (Var "cdr", _) -> false
  | _ -> false

(** Number of self-recursive call sites of [fn_name] within [e]. *)
let count_self_calls ~(fn_name : string) (e : expr) : int =
  let info = analyze_self ~fn_name e in
  List.length info.self_sites

(** Does [fn_name] recurse on arguments that shrink linearly (cdr / -1)? *)
let self_args_look_linear ~(fn_name : string) (e : expr) : bool =
  let rec find acc = function
    | App (Var n, args) when n = fn_name ->
        List.exists looks_like_cdr_arg args || acc
    | App (f, args) -> List.fold_left find (find acc f) args
    | Prim (_, args) -> List.fold_left find acc args
    | Let (_, a, b) | Seq (a, b) -> find (find acc a) b
    | Letrec (bs, body) ->
        let acc = List.fold_left (fun a (_, e) -> find a e) acc bs in
        find acc body
    | If (a, b, c) -> find (find (find acc a) b) c
    | Lambda (_, body) | Define (_, body) -> find acc body
    | Var _ | Lit _ | Quote _ -> acc
  in
  find false e

(** Does [fn_name] recurse on arguments that shrink by halving (/ 2)? *)
let self_args_look_divide ~(fn_name : string) (e : expr) : bool =
  let rec find acc = function
    | App (Var n, args) when n = fn_name ->
        List.exists looks_like_halve args || acc
    | App (f, args) -> List.fold_left find (find acc f) args
    | Prim (_, args) -> List.fold_left find acc args
    | Let (_, a, b) | Seq (a, b) -> find (find acc a) b
    | Letrec (bs, body) ->
        let acc = List.fold_left (fun a (_, e) -> find a e) acc bs in
        find acc body
    | If (a, b, c) -> find (find (find acc a) b) c
    | Lambda (_, body) | Define (_, body) -> find acc body
    | Var _ | Lit _ | Quote _ -> acc
  in
  find false e

(** Classify the big-O complexity of the function [fn_name] with body [e].
    Pure pattern matching over recursion shape, list iters and DB scans.*)
let classify ~(fn_name : string) (e : expr) : complexity =
  let body = unwrap_fn_body e in
  let n_self = count_self_calls ~fn_name body in
  if has_nested_iter body then O_quadratic
  else if n_self >= 2 && self_args_look_linear ~fn_name body then O_exponential
  else if n_self >= 2 && self_args_look_divide ~fn_name body then O_n_log_n
  else if n_self = 1 && self_args_look_linear ~fn_name body then O_linear
  else if n_self = 1 && self_args_look_divide ~fn_name body then O_log_n
  else if has_list_iter body || mentions_db_scan body then O_linear
  else if n_self = 0 && not (has_list_iter body) && not (mentions_db_scan body)
  then O_constant
  else if n_self > 0 then O_linear (* recursive but shape unclear — mild guess *)
  else Unknown
