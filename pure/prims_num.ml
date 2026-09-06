(* Numeric primitives: arithmetic, comparison, predicates, the
   lisp-interpreter.math tower, number parsing, RNG, uuid, FSRS.
   Split from prims_pure.ml (i-1788671384). *)
open Pricklypear_lisp.Ast
open Prims_common


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
  | _ -> raise (Interp_error (User_error ("- needs one or two numeric args", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("= needs two numeric args", "bad-args")))

let prim_lt args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) < 0)
  | _ -> raise (Interp_error (User_error ("< needs two numeric args", "bad-args")))

let prim_gt args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) > 0)
  | _ -> raise (Interp_error (User_error ("> needs two numeric args", "bad-args")))

let prim_le args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) <= 0)
  | _ -> raise (Interp_error (User_error ("<= needs two numeric args", "bad-args")))

let prim_ge args =
  match args with
  | [ a; b ] -> VBool (num_compare (num_of_value a) (num_of_value b) >= 0)
  | _ -> raise (Interp_error (User_error (">= needs two numeric args", "bad-args")))

(* "not" boolean negation *)
let prim_not args =
  match args with
  | [ VBool b ] -> VBool (not b)
  | [ VNil ] -> VBool true
  | [ _ ] -> VBool false   (* any non-nil non-false is truthy *)
  | _ -> raise (Interp_error (User_error ("not needs one arg", "bad-args")))

(* (parse-float s|n) -> VFloat *)
let prim_parse_float args =
  match args with
  | [ VFloat f ] -> VFloat f
  | [ VInt n ] -> VFloat (Int64.to_float n)
  | [ VStr s ] ->
      (try VFloat (float_of_string (String.trim s))
       with _ -> VFloat 0.)
  | _ -> raise (Interp_error (User_error ("parse-float needs string or number", "bad-args")))

(* (floor x) -> VInt *)
let prim_floor args =
  match args with
  | [ VInt n ] -> VInt n
  | [ VFloat f ] -> VInt (Int64.of_float (floor f))
  | _ -> raise (Interp_error (User_error ("floor needs one numeric arg", "bad-args")))

(* (round x) -> VInt *)
let prim_round args =
  match args with
  | [ VInt n ] -> VInt n
  | [ VFloat f ] -> VInt (Int64.of_float (Float.round f))
  | _ -> raise (Interp_error (User_error ("round needs one numeric arg", "bad-args")))

(* (div a b) — integer div when both int; else float. *)
let prim_div args =
  match args with
  | [ a; b ] -> value_of_num (num_div (num_of_value a) (num_of_value b))
  | _ -> raise (Interp_error (User_error ("div needs two numeric args", "bad-args")))

let prim_mod args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.rem a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error ("mod by zero", "bad-args")))
  | [ a; b ] ->
      let bf = float_of_num (num_of_value b) in
      if bf = 0. then raise (Interp_error (User_error ("mod by zero", "bad-args")))
      else VFloat (mod_float (float_of_num (num_of_value a)) bf)
  | _ -> raise (Interp_error (User_error ("mod needs two numeric args", "bad-args")))

(* Scheme aliases of the int paths of div/mod — reject floats. *)
let prim_quotient args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.div a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error ("quotient by zero", "bad-args")))
  | _ ->
      raise (Interp_error (User_error ("quotient needs two int args", "bad-args")))

let prim_remainder args =
  match args with
  | [ VInt a; VInt b ] when b <> 0L -> VInt (Int64.rem a b)
  | [ VInt _; VInt 0L ] ->
      raise (Interp_error (User_error ("remainder by zero", "bad-args")))
  | _ ->
      raise (Interp_error (User_error ("remainder needs two int args", "bad-args")))

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
    raise (Interp_error (User_error ("expt int path needs non-negative exponent", "bad-args")))
  else loop 1L base exp

let prim_expt args =
  match args with
  | [ a; b ] ->
      (match num_of_value a, num_of_value b with
       | NInt base, NInt e when e >= 0L -> VInt (int_pow base e)
       | ba, bb ->
           VFloat (Float.pow (float_of_num ba) (float_of_num bb)))
  | _ -> raise (Interp_error (User_error ("expt needs two numeric args", "bad-args")))

let prim_sqrt args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x < 0. then
        raise (Interp_error (User_error ("sqrt of negative", "bad-args")))
      else VFloat (sqrt x)
  | _ -> raise (Interp_error (User_error ("sqrt needs one numeric arg", "bad-args")))

let prim_exp args =
  match args with
  | [ a ] -> VFloat (exp (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error ("exp needs one numeric arg", "bad-args")))

let prim_log args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x <= 0. then
        raise (Interp_error (User_error ("log of non-positive", "bad-args")))
      else VFloat (log x)
  | [ a; b ] ->
      let x = float_of_num (num_of_value a) in
      let base = float_of_num (num_of_value b) in
      if x <= 0. then
        raise (Interp_error (User_error ("log of non-positive", "bad-args")))
      else if base <= 0. || base = 1. then
        raise (Interp_error (User_error ("log: invalid base", "bad-args")))
      else VFloat (log x /. log base)
  | _ -> raise (Interp_error (User_error ("log needs one or two numeric args", "bad-args")))

let prim_sin args =
  match args with
  | [ a ] -> VFloat (sin (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error ("sin needs one numeric arg", "bad-args")))

let prim_cos args =
  match args with
  | [ a ] -> VFloat (cos (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error ("cos needs one numeric arg", "bad-args")))

let prim_tan args =
  match args with
  | [ a ] -> VFloat (tan (float_of_num (num_of_value a)))
  | _ -> raise (Interp_error (User_error ("tan needs one numeric arg", "bad-args")))

let prim_asin args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x < -1. || x > 1. then
        raise (Interp_error (User_error ("asin domain [-1,1]", "bad-args")))
      else VFloat (asin x)
  | _ -> raise (Interp_error (User_error ("asin needs one numeric arg", "bad-args")))

let prim_acos args =
  match args with
  | [ a ] ->
      let x = float_of_num (num_of_value a) in
      if x < -1. || x > 1. then
        raise (Interp_error (User_error ("acos domain [-1,1]", "bad-args")))
      else VFloat (acos x)
  | _ -> raise (Interp_error (User_error ("acos needs one numeric arg", "bad-args")))

let prim_atan args =
  match args with
  | [ a ] -> VFloat (atan (float_of_num (num_of_value a)))
  | [ a; b ] ->
      (* Two-arg form → atan2(y, x) with y first (Scheme order). *)
      let y = float_of_num (num_of_value a) in
      let x = float_of_num (num_of_value b) in
      VFloat (atan2 y x)
  | _ -> raise (Interp_error (User_error ("atan needs one or two numeric args", "bad-args")))

let prim_number_p args =
  match args with
  | [ VInt _ ] | [ VFloat _ ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error ("number? needs one arg", "bad-args")))

let prim_integer_p args =
  match args with
  | [ VInt _ ] -> VBool true
  | [ _ ] -> VBool false
  | _ -> raise (Interp_error (User_error ("integer? needs one arg", "bad-args")))

let prim_zero_p args =
  match args with
  | [ a ] -> VBool (num_compare (num_of_value a) (NInt 0L) = 0)
  | _ -> raise (Interp_error (User_error ("zero? needs one numeric arg", "bad-args")))

let prim_positive_p args =
  match args with
  | [ a ] -> VBool (num_compare (num_of_value a) (NInt 0L) > 0)
  | _ -> raise (Interp_error (User_error ("positive? needs one numeric arg", "bad-args")))

let prim_negative_p args =
  match args with
  | [ a ] -> VBool (num_compare (num_of_value a) (NInt 0L) < 0)
  | _ -> raise (Interp_error (User_error ("negative? needs one numeric arg", "bad-args")))

let as_integral_int64 = function
  | VInt n -> n
  | VFloat f ->
      if Float.is_integer f then Int64.of_float f
      else raise (Interp_error (User_error ("even?/odd? needs an integer", "bad-args")))
  | _ -> raise (Interp_error (User_error ("even?/odd? needs an integer", "bad-args")))

let prim_even_p args =
  match args with
  | [ a ] ->
      let n = as_integral_int64 a in
      VBool (Int64.logand n 1L = 0L)
  | _ -> raise (Interp_error (User_error ("even? needs one arg", "bad-args")))

let prim_odd_p args =
  match args with
  | [ a ] ->
      let n = as_integral_int64 a in
      VBool (Int64.logand n 1L <> 0L)
  | _ -> raise (Interp_error (User_error ("odd? needs one arg", "bad-args")))

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
      raise (Interp_error (User_error ("random: n must be positive int", "bad-args")))
  | _ -> raise (Interp_error (User_error ("random needs 0 args or one positive int", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("random-seed needs one int", "bad-args")))

let prim_abs args =
  match args with
  | [ VInt a ] -> VInt (Int64.abs a)
  | [ VFloat f ] -> VFloat (abs_float f)
  | _ -> raise (Interp_error (User_error ("abs needs one numeric arg", "bad-args")))

let prim_min args =
  match args with
  | [] -> raise (Interp_error (User_error ("min needs at least one numeric arg", "bad-args")))
  | h :: rest ->
      value_of_num
        (List.fold_left
           (fun acc v ->
             let n = num_of_value v in
             if num_compare n acc < 0 then n else acc)
           (num_of_value h) rest)

let prim_max args =
  match args with
  | [] -> raise (Interp_error (User_error ("max needs at least one numeric arg", "bad-args")))
  | h :: rest ->
      value_of_num
        (List.fold_left
           (fun acc v ->
             let n = num_of_value v in
             if num_compare n acc > 0 then n else acc)
           (num_of_value h) rest)

(* (parse-int s|n) -> VInt. Soft-fail to 0 on bad string. *)
let prim_parse_int (args : value list) : value =
  match args with
  | [ VInt n ] -> VInt n
  | [ VStr s ] ->
      (try VInt (Int64.of_int (int_of_string (String.trim s)))
       with _ -> VInt 0L)
  | _ -> raise (Interp_error (User_error ("parse-int needs string or int", "bad-args")))

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
                   ("fsrs-schedule: rating must be 1..4 or again|hard|good|easy", "bad-args")))
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
              ("fsrs-schedule needs stability difficulty elapsed rating", "bad-args")))

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
