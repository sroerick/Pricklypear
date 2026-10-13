(* auth_throttle_test — pure alcotest for Auth_throttle (i-1787970741).

   Pins the three env knobs so window/max/backoff are deterministic
   regardless of ambient env. The clock is injected (a float), so
   nothing here sleeps. Each case uses its own key prefix: the table is
   module-global. *)

module T = Pricklypear_image_lib.Auth_throttle

let pin_env () =
  Unix.putenv "PP_LOGIN_MAX_FAILURES" "10";
  Unix.putenv "PP_LOGIN_WINDOW_SECS" "900";
  Unix.putenv "PP_LOGIN_BACKOFF_BASE_SECS" "30"

let maxf = 10
let base = 30.0
let cap = 900.0

let check_bool name expected actual =
  Alcotest.(check bool) name expected actual

(* Record [n] failures for [key] at times 0.0 .. n-1. *)
let record_n key n =
  for i = 0 to n - 1 do
    T.record_failure key (float_of_int i)
  done

let clean_key_admits () =
  check_bool "clean key admits" true (T.admit "t-clean" 100.0)

let below_threshold_never_blocks () =
  let key = "t-below" in
  record_n key (maxf - 1);
  (* 9 failures inside the window: still admitted on every attempt. *)
  check_bool "9 failures admit" true (T.admit key 1000.0);
  check_bool "still admits later" true (T.admit key 1500.0)

let threshold_engages_backoff () =
  let key = "t-threshold" in
  record_n key maxf;
  (* 10 failures, last at t=9, first lockout = 30s -> locked until 39. *)
  check_bool "locked right after" false (T.admit key 20.0);
  check_bool "locked just before deadline" false
    (T.admit key 38.999);
  check_bool "admitted at deadline" true (T.admit key 39.0)

let backoff_doubles () =
  let key = "t-double" in
  record_n key (maxf + 1);
  (* 11th failure -> 60s from t=10. *)
  check_bool "11th failure locks 60s" false (T.admit key 69.0);
  check_bool "admitted at 70" true (T.admit key 70.0);
  (* A further failure now (t=70) -> 12 in window -> 120s from t=70. *)
  T.record_failure key 70.0;
  check_bool "12th failure locks 120s" false (T.admit key 189.0);
  check_bool "admitted at 190" true (T.admit key 190.0)

let backoff_capped () =
  let key = "t-cap" in
  record_n key 30;
  (* 20 failures past the threshold: backoff is capped at 900s from
     t=29 (deadline 929). At t=500 the window still holds the whole
     burst, so the cap is what keeps the key locked. *)
  check_bool "capped lockout holds" false (T.admit key 500.0);
  check_bool "capped lockout still holds near deadline" false
    (T.admit key 900.0);
  (* Once the burst slides out of the 900s window the key frees. *)
  check_bool "admitted after window clears" true
    (T.admit key (29.0 +. cap))

let sliding_window_expires () =
  let key = "t-window" in
  (* 9 failures at t=0 and one at t=895: nominal lockout until 925, but
     by t=920 the nine old failures slid out of the 900s window. *)
  for _ = 1 to maxf - 1 do
    T.record_failure key 0.0
  done;
  T.record_failure key 895.0;
  check_bool "locked inside window" false (T.admit key 500.0);
  check_bool "expired failures free the key" true
    (T.admit key 920.0)

let clear_resets () =
  let key = "t-clear" in
  record_n key maxf;
  check_bool "locked before clear" false (T.admit key 20.0);
  T.clear key;
  check_bool "admitted after clear" true (T.admit key 20.0);
  (* A fresh single failure does not re-lock. *)
  T.record_failure key 21.0;
  check_bool "single failure admits" true (T.admit key 22.0)

let table_bounded () =
  (* Flood well past the bound; the table must evict, not grow. *)
  for i = 0 to 4199 do
    T.record_failure (Printf.sprintf "t-flood-%d" i) 1000.0
  done;
  Alcotest.(check int) "table capped at 4096" T.max_entries
    (T.size ());
  (* Evicted keys are forgotten; fresh keys still admit. *)
  check_bool "evicted key admits" true (T.admit "t-flood-0" 1001.0);
  check_bool "fresh key admits" true (T.admit "t-flood-fresh" 1001.0)

(* --- per-IP gate (i-1787970741 second half): same machinery, own bucket
   space keyed by source IP, independent of the username table. *)

let record_n_ip key n =
  for i = 0 to n - 1 do
    T.record_failure_ip key (1.0 +. float_of_int i)
  done

let ip_gate_engages_backoff () =
  let key = "ip-threshold" in
  check_bool "clean ip admits" true (T.admit_ip key 0.0);
  record_n_ip key maxf;
  check_bool "locked ip at threshold" false (T.admit_ip key 20.0);
  check_bool "other ips unaffected" true (T.admit_ip "ip-other" 20.0);
  check_bool "username gate independent of ip lock" true
    (T.admit "some-user" 20.0)

let ip_backoff_doubles_and_expires () =
  let key = "ip-double" in
  record_n_ip key (maxf + 1);
  (* 11 failures, last at t=11 -> first lockout doubles base: 60s -> 71. *)
  check_bool "1 past threshold: doubled base backoff" false (T.admit_ip key 61.0);
  check_bool "admitted at 71" true (T.admit_ip key 71.0);
  (* A further failure now (t=71) -> 12 in window -> 120s from t=71. *)
  T.record_failure_ip key 71.0;
  check_bool "12th failure locks 120s" false (T.admit_ip key 190.0);
  (* Once the burst slides out of the 900s window the key frees. *)
  check_bool "expired ip failures free the key" true (T.admit_ip key 975.0)

let ip_clear_resets () =
  let key = "ip-clear" in
  record_n_ip key maxf;
  check_bool "locked before clear" false (T.admit_ip key 20.0);
  T.clear_ip key;
  check_bool "admitted after clear" true (T.admit_ip key 20.0)

let both_gates_compose () =
  (* The verify_password policy: admit requires BOTH gates. Simulate the
     composition the caller does: a username under its threshold but an
     IP past its own = the attempt is denied by the ip half. *)
  let user = "compose-user" and ip = "compose-ip" in
  T.record_failure user 1.0;
  record_n_ip ip maxf;
  let both_admit = T.admit user 20.0 && T.admit_ip ip 20.0 in
  check_bool "ip gate denies despite clean username" false both_admit;
  T.clear_ip ip;
  check_bool "both admit after ip clear" true
    (T.admit user 20.0 && T.admit_ip ip 20.0)

let () =
  pin_env ();
  Alcotest.run "auth_throttle"
    [ ( "throttle",
        [ ("clean key admits", `Quick, clean_key_admits);
          ("below threshold never blocks", `Quick, below_threshold_never_blocks);
          ("threshold engages backoff", `Quick, threshold_engages_backoff);
          ("backoff doubles", `Quick, backoff_doubles);
          ("backoff capped at 900s", `Quick, backoff_capped);
          ("sliding window expires", `Quick, sliding_window_expires);
          ("clear resets", `Quick, clear_resets);
          ("table bounded at 4096", `Quick, table_bounded);
          ("ip gate engages backoff", `Quick, ip_gate_engages_backoff);
          ("ip backoff doubles and expires", `Quick, ip_backoff_doubles_and_expires);
          ("ip clear resets", `Quick, ip_clear_resets);
          ("both gates compose", `Quick, both_gates_compose) ]) ]
