(** Wall-clock / calendar primitives.

    These read the system clock ([Unix]) so they are NOT portable to the
    browser client and live in the image only. Split out of
    [Prims_pure] so the pure-prims library stays Unix-free and can be
    shared with the js_of_ocaml client target. Spec:
    borge/structural-editing.borg client-eval (PURE PRIM TABLE ONLY). *)
open Pricklypear_lisp.Ast

(* (date-today) -> VStr "YYYY-MM-DD" in local time. *)
let prim_date_today (_args : value list) : value =
  let tm = Unix.localtime (Unix.time ()) in
  VStr
    (Printf.sprintf "%04d-%02d-%02d"
       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday)

(** (now) -> VInt unix epoch seconds (UTC). Wall clock for apps;
    pairs with date-today for calendar day. BORGE-1785211069. *)
let prim_now (_args : value list) : value =
  VInt (Int64.of_float (Unix.gettimeofday ()))

(** (now-iso) -> VStr "YYYY-MM-DDTHH:MM:SSZ" (UTC, ISO 8601).
    The app-facing timestamp stamp: uniform 20-char shape so plain
    string compare IS chronological order (same convention as PG
    timestamptz text; workstreams/date-prefix and
    todo/datum/insert-by-ts already rely on it). Before this prim the
    chat lib stamped created_at "" — blank DM timestamps.
    BORGE-i-<filed>, see borge issue register. *)
let prim_now_iso (_args : value list) : value =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  VStr
    (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
       (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
       tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)

(** (now-ms) -> VInt unix epoch milliseconds. *)
let prim_now_ms (_args : value list) : value =
  VInt (Int64.of_float (Unix.gettimeofday () *. 1000.0))

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
      raise (Interp_error (User_error ("date-weekday needs year month day ints", "bad-args")))

(* (date-format y m d) -> VStr "YYYY-MM-DD". *)
let prim_date_format (args : value list) : value =
  match args with
  | [ VInt y; VInt m; VInt d ] ->
      VStr
        (Printf.sprintf "%04d-%02d-%02d"
           (Int64.to_int y) (Int64.to_int m) (Int64.to_int d))
  | _ -> raise (Interp_error (User_error ("date-format needs year month day", "bad-args")))

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
           raise (Interp_error (User_error ("date-add-days: bad date string", "bad-args"))))
  | [ VInt n ] ->
      let tm = Unix.localtime (Unix.time ()) in
      add_from_ymd
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        (Int64.to_int n)
  | [ VStr day; VStr n ] ->
      (match parse_ymd day, int_of_string_opt (String.trim n) with
       | Some (y, m, d), Some n -> add_from_ymd y m d n
       | _ ->
           raise (Interp_error (User_error ("date-add-days: bad args", "bad-args"))))
  | _ ->
      raise
        (Interp_error
           (User_error ("date-add-days needs (day n) or (n) for today+n", "bad-args")))

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
           raise (Interp_error (User_error ("date-diff-days: bad date", "bad-args"))))
  | _ ->
      raise
        (Interp_error
           (User_error ("date-diff-days needs two YYYY-MM-DD strings", "bad-args")))
