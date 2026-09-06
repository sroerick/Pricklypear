(* String primitives: concat/join/slice/search/replace/case/compare/
   split, percent-encoding, date parsing. Every operand goes through
   Prims_common.coerce_str — the ONE string-coercion policy
   (i-1788671384); no prim demands a bare VStr any more. *)
open Pricklypear_lisp.Ast
open Prims_common


(* (string-append s1 s2 ...) -> VStr. Variadic over VStr / __str__ tags. *)
let prim_string_append args =
  let buf = Buffer.create 32 in
  List.iter
    (fun v -> Buffer.add_string buf (coerce_str ~ctx:"string-append" v))
    args;
  VStr (Buffer.contents buf)

(* (string-join strs [sep]) -> VStr. Concatenate a list of strings in a single
   O(n) pass (Buffer add), optionally separated by sep (default ""). Unlike a
   fold over string-append (O(n²) accumulator-copy), this is linear. List items
   are coerced like string-append (string/__str__/symbol/int/float/bool/nil). *)
let prim_string_join args =
  let join items sep =
    let buf = Buffer.create 32 in
    (match items with
     | [] -> ()
     | x :: rest ->
         Buffer.add_string buf (coerce_str ~ctx:"string-join" x);
         List.iter
           (fun v -> Buffer.add_string buf sep; Buffer.add_string buf (coerce_str ~ctx:"string-join" v))
           rest);
    VStr (Buffer.contents buf)
  in
  match args with
  | [ lst ] -> join (list_of_value lst) ""
  | [ lst; sep_v ] -> join (list_of_value lst) (coerce_str ~ctx:"string-join" sep_v)
  | _ -> raise (Interp_error (User_error ("string-join needs a list [sep]", "bad-args")))

let prim_string_length args =
  match args with
  | [ v ] ->
      let s = coerce_str ~ctx:"string-length" v in
      VInt (Int64.of_int (String.length s))
  | _ -> raise (Interp_error (User_error ("string-length needs one VStr", "bad-args")))

let prim_substring args =
  match args with
  | [ sv; VInt start; VInt len ] ->
      let s = coerce_str ~ctx:"substring" sv in
      let st = Int64.to_int start in
      let n = Int64.to_int len in
      let sl = String.length s in
      if st < 0 || n < 0 || st > sl then
        raise (Interp_error (User_error ("substring out of range", "bad-args")))
      else
        let n = min n (sl - st) in
        VStr (String.sub s st n)
  | [ sv; VInt start ] ->
      let s = coerce_str ~ctx:"substring" sv in
      let st = Int64.to_int start in
      let sl = String.length s in
      if st < 0 || st > sl then
        raise (Interp_error (User_error ("substring out of range", "bad-args")))
      else VStr (String.sub s st (sl - st))
  | _ ->
      raise
        (Interp_error
           (User_error ("substring needs string start [len]", "bad-args")))

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
        (Interp_error (User_error ("string-contains? needs two VStr args", "bad-args")))

(* (string-eq a b) -> VBool. String equality for request-method checks etc. *)
let prim_string_eq args =
  match args with
  | [ a; b ] ->
      VBool
        (String.equal
           (coerce_str ~ctx:"string-eq" a)
           (coerce_str ~ctx:"string-eq" b))
  | _ -> raise (Interp_error (User_error ("string-eq needs 2 strings", "bad-args")))

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
           (User_error (Printf.sprintf "%s needs 2 strings" name, "bad-args")))

let prim_string_lt args = prim_string_cmp "string<?" ( < ) args
let prim_string_le args = prim_string_cmp "string<=?" ( <= ) args
let prim_string_gt args = prim_string_cmp "string>?" ( > ) args
let prim_string_ge args = prim_string_cmp "string>=?" ( >= ) args

(* (string-trim s) -> VStr. Trim leading/trailing whitespace. *)
let prim_string_trim args =
  match args with
  | [ v ] -> VStr (String.trim (coerce_str ~ctx:"string-trim" v))
  | _ -> raise (Interp_error (User_error ("string-trim needs 1 string", "bad-args")))

(* JSON <-> Lisp values for outline trees and generic data.
   Objects become VStr of JSON object (use dict-get); arrays -> VList. *)

(* ---- string helpers (P2 from kernel-io-primitives note) ---- *)

let prim_string_replace args =
  match args with
  | [ s_v; pat_v; repl_v ] ->
      (* Shared string-coercion policy (i-1788671384): like string-trim/
         string-split, operands go through coerce_str (__str__ tags,
         symbols, numbers coerce; non-stringy values error). *)
      let s = coerce_str ~ctx:"string-replace" s_v in
      let pat = coerce_str ~ctx:"string-replace" pat_v in
      let repl = coerce_str ~ctx:"string-replace" repl_v in
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
  | _ -> raise (Interp_error (User_error ("string-replace needs (s pat repl)", "bad-args")))

let prim_string_downcase args =
  match args with
  | [ v ] -> VStr (String.lowercase_ascii (coerce_str ~ctx:"string-downcase" v))
  | _ -> raise (Interp_error (User_error ("string-downcase needs 1 string", "bad-args")))

let prim_string_upcase args =
  match args with
  | [ v ] -> VStr (String.uppercase_ascii (coerce_str ~ctx:"string-upcase" v))
  | _ -> raise (Interp_error (User_error ("string-upcase needs 1 string", "bad-args")))

let prim_string_starts_with_p args =
  match args with
  | [ VStr s; VStr pfx ] -> VBool (String.starts_with ~prefix:pfx s)
  | [ a; b ] ->
      VBool (String.starts_with
               ~prefix:(coerce_str ~ctx:"string-starts-with?" b)
               (coerce_str ~ctx:"string-starts-with?" a))
  | _ -> raise (Interp_error (User_error ("string-starts-with? needs 2 strings", "bad-args")))

let prim_string_ends_with_p args =
  match args with
  | [ VStr s; VStr sfx ] -> VBool (String.ends_with ~suffix:sfx s)
  | [ a; b ] ->
      VBool (String.ends_with
               ~suffix:(coerce_str ~ctx:"string-ends-with?" b)
               (coerce_str ~ctx:"string-ends-with?" a))
  | _ -> raise (Interp_error (User_error ("string-ends-with? needs 2 strings", "bad-args")))

(* (string-split s delim) -> VList of VStr.
   Matches the libs/std Nopales string-split semantics so libs/hyper runs
   unchanged on hosts that don't load libs/std (the agent): empty/no-match
   -> single-element list of the whole string; otherwise splits on each
   non-overlapping occurrence of delim, preserving empty trailing
   segments. KERNEL PRIM as of 2026-08-30 (habitat-book web-renderer):
   the interpreted libs/std definition cost ~O(n) interpreter steps per
   input byte — one 3KB prose paragraph blew the 100k-step page budget —
   so std no longer defines it and the image registers this prim
   (string-split moves to the reserved catalog). Empty delimiter returns
   the whole string (the retired std version looped forever there). *)
let prim_string_split args =
  match args with
  | [ s_v; d_v ] ->
      let s = coerce_str ~ctx:"string-split" s_v in
      let d = coerce_str ~ctx:"string-split" d_v in
      let slen = String.length s in
      let dlen = String.length d in
      if slen = 0 || dlen = 0 then VList [ VStr s ]
      else begin
        (* search for delim like string-contains? (byte compare) *)
        let find_from i =
          let rec go j =
            if j + dlen > slen then None
            else if String.sub s j dlen = d then Some j
            else go (j + 1)
          in
          go i
        in
        let rec loop acc i =
          match find_from i with
          | None -> List.rev (VStr (String.sub s i (slen - i)) :: acc)
          | Some p -> loop (VStr (String.sub s i (p - i)) :: acc) (p + dlen)
        in
        VList (loop [] 0)
      end
  | _ -> raise (Interp_error (User_error ("string-split needs (s delim)", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("url-encode needs 1 string", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("url-decode needs 1 string", "bad-args")))

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
  | _ -> raise (Interp_error (User_error ("parse-date needs 1 string", "bad-args")))
