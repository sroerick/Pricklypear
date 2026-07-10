(** Pricklypear Auth — pure tests, no services.

    Tests of the pure parts of the auth library that don't need a
    running PG. The DB-backed functions (verify_ssh_key, mint,
    verify_token which require store connection) are integration-tested
    against a real DB in the iteration-8 smoke test, not here. *)
open Pricklypear_auth_lib

let () =
  (try Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
   with _ -> ())

let crypto_tests = [
  ("tokens: HMAC round-trips deterministically", `Quick, fun () ->
    (* Verify HMAC of the same input with the same key is stable. *)
    let key = Cstruct.of_string "test-key-32-bytes-long-enough!" in
    let msg = Cstruct.of_string "opaque-id-bytes-32-long----" in
    let sig1 = Tokens.(sign ~key msg) in
    let sig2 = Tokens.(sign ~key msg) in
    let s1 = Hex.to_string (Hex.of_cstruct sig1) in
    let s2 = Hex.to_string (Hex.of_cstruct sig2) in
    Alcotest.(check string) "hmac stable" s1 s2);
  ("tokens: random_opaque_id is 32 bytes and distinct", `Quick, fun () ->
    let a = Tokens.random_opaque_id () in
    let b = Tokens.random_opaque_id () in
    Alcotest.(check int) "opaque len a" 32 (Cstruct.length a);
    Alcotest.(check int) "opaque len b" 32 (Cstruct.length b);
    (* Distinct (rare collision would be a bug; this is a sane check) *)
    let sa = Hex.to_string (Hex.of_cstruct a) in
    let sb = Hex.to_string (Hex.of_cstruct b) in
    let distinct = not (String.equal sa sb) in
    Alcotest.(check bool) "random distinct" true distinct);
  ("tokens: to_string/of_string round-trip", `Quick, fun () ->
    let t = {
      Pricklypear_common.Auth_types.opaque_id = "aabbccdd";
      signature = "00112233";
    } in
    let s = Tokens.to_string t in
    (match Tokens.of_string s with
     | Ok t' ->
        Alcotest.(check string) "opaque" "aabbccdd" t'.opaque_id;
        Alcotest.(check string) "sig" "00112233" t'.signature
     | Error e -> Alcotest.failf "of_string failed: %s" e));
  ("tokens: malformed token rejected", `Quick, fun () ->
    (match Tokens.of_string "noseparator" with
     | Ok _ -> Alcotest.fail "should have failed"
     | Error _ -> Alcotest.(check bool) "rejected" true true));
]

let () =
  Alcotest.run "pricklypear-auth" [
    "crypto", crypto_tests;
  ]
