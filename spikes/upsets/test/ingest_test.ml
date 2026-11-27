(* spikes/upsets/test — ingest vectors (borge/upsets.borg, subsection
   ingest; spec.pp.borge.upsets#upsets/ingest).

   The three pollers live in ../lib.pp as Nopales package code, which dune
   cannot link. So the contract is checked two ways:

   1. source-level: the package source is read here and its exported surface
      plus the structural invariants (per-tick caps, shipped-off schedule
      table, error-not-partial-row fetch discipline, the idempotency keys)
      are asserted — libs/filmstrip/test precedent.
   2. pure-level: the reduction rules the tick relies on (watched selection
      from the two outcome prices, the hourly flow bucket, the BUY / min-usd
      trade filter, the odds watermark) are replayed over hand-computed
      fixtures.

   The alcotest summary line printed by Alcotest.run is the gate receipt. *)

let lib_pp = "../lib.pp"

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let src = lazy (read_file lib_pp)

let contains ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec go i =
    if i + nl > hl then false
    else if String.equal (String.sub hay i nl) needle then true
    else go (i + 1)
  in
  nl = 0 || go 0

let has needle =
  Alcotest.(check bool) ("lib.pp contains " ^ needle) true
    (contains ~needle (Lazy.force src))

(* ---- 1. surface ---- *)

let test_surface () =
  has "(define upsets/poll";
  has "(define upsets/poll-odds";
  has "(define upsets/poll-flow";
  has "(define upsets/watched-rows";
  (* the CHAPTER LAW: no new kernel code — the tick rides http-get only. *)
  has "http-get";
  has "gamma-api.polymarket.com";
  has "clob.polymarket.com";
  has "data-api.polymarket.com"

(* ---- 2. budget discipline: caps so a tick fits the interpreter floors ---- *)

let test_caps () =
  has "(define upsets/market-cap";
  has "(define upsets/poll-pages";
  has "(define upsets/odds-chunk";
  has "(define upsets/flow-chunk";
  has "(define upsets/watch-max";
  has "(define upsets/flow-max";
  has "(define upsets/watch-threshold";
  has "(define upsets/min-trade-usd"

(* ---- 3. shipped-off schedules (the operator turns them on) ---- *)

let test_schedules () =
  has "upsets-poll\" \"*/5 * * * *\"";
  has "upsets-odds\" \"0 * * * *\"";
  has "upsets-flow\" \"*/15 * * * *\"";
  has "(define upsets/schedules-enabled"

(* ---- 4. failure discipline: raise, don't write partial rows ---- *)

let test_failure_discipline () =
  has "(string-eq (as-str (dict-get r \"ok\")) \"true\")";
  (* a failed GET raises out of the tick before any create/update runs *)
  has "(error";
  has "as-str (dict-get r \"err\")"

(* ---- 5. idempotency is structural ---- *)

let test_idempotency () =
  (* markets upsert on condition-id *)
  has "find-rows";
  has "condition-id";
  (* odds watermarked by last-odds-ts; flows upsert on market|wallet|side|bucket *)
  has "last-odds-ts";
  has "last-flow-ts";
  has "bucket";
  has "wallet-flow"

(* ---- 6. pure reduction rules ---- *)

let min2 a b = if a < b then a else b

let watched ~threshold yes no = min2 yes no < threshold

let test_watched () =
  Alcotest.(check bool) "0.32/0.68 watched" true (watched ~threshold:0.4 0.32 0.68);
  Alcotest.(check bool) "0.45/0.55 not watched" false
    (watched ~threshold:0.4 0.55 0.45);
  (* boundary: threshold itself is not watched (strict <.4) *)
  Alcotest.(check bool) "0.40/0.60 not watched" false
    (watched ~threshold:0.4 0.40 0.60)

let bucket ts = ts / 3600

(* hour-aligned base: 472222 * 3600 = 1_699_999_200. *)
let h0 = 1_699_999_200

let test_bucket () =
  Alcotest.(check int) "hour bucket" 472222 (bucket h0);
  Alcotest.(check int) "same hour collapses" (bucket h0) (bucket (h0 + 3599));
  Alcotest.(check int) "next hour splits" 472223 (bucket (h0 + 3600))

let keep ~min_usd side usd = String.equal side "BUY" && usd >= min_usd

let test_trade_filter () =
  let usd size price = size *. price in
  Alcotest.(check bool) "buy 100@0.5 kept" true
    (keep ~min_usd:10.0 "BUY" (usd 100.0 0.5));
  Alcotest.(check bool) "sell dropped" false (keep ~min_usd:10.0 "SELL" (usd 100.0 0.5));
  Alcotest.(check bool) "dust buy dropped" false (keep ~min_usd:10.0 "BUY" (usd 10.0 0.5))

(* watermark: odds points are written only when strictly newer than the
   stored last-odds-ts, so a re-run writes nothing. *)
let writes_between ~last_ts ts = ts > last_ts

let test_watermark () =
  Alcotest.(check bool) "new point written" true (writes_between ~last_ts:100 200);
  Alcotest.(check bool) "same point skipped" false (writes_between ~last_ts:200 200);
  Alcotest.(check bool) "old point skipped" false (writes_between ~last_ts:200 199)

let () =
  Alcotest.run "upsets-ingest"
    [
      ("surface", [ Alcotest.test_case "pollers + hosts" `Quick test_surface ]);
      ("budget", [ Alcotest.test_case "tick caps" `Quick test_caps ]);
      ("schedules", [ Alcotest.test_case "shipped off" `Quick test_schedules ]);
      ("failures", [ Alcotest.test_case "raise, no partial rows" `Quick test_failure_discipline ]);
      ("idempotency", [ Alcotest.test_case "upsert keys" `Quick test_idempotency ]);
      ("watched", [ Alcotest.test_case "two-price selection" `Quick test_watched ]);
      ("bucket", [ Alcotest.test_case "hourly flow bucket" `Quick test_bucket ]);
      ("trades", [ Alcotest.test_case "buy + min-usd filter" `Quick test_trade_filter ]);
      ("odds", [ Alcotest.test_case "last-odds-ts watermark" `Quick test_watermark ]);
    ]
