(** Unit tests for password hashing (argon2id champion + legacy interim
    sha256 upgrade path) + token hash. *)

let () = Mirage_crypto_rng_unix.use_default ()

open Pricklypear_image_lib.Auth_password

let hash_roundtrip () =
  let h = hash_password "s3cret" in
  Alcotest.(check bool) "argon2id prefix" true
    (String.starts_with ~prefix:"$argon2id$" h);
  Alcotest.(check bool) "verify ok" true
    (verify_password ~stored:h "s3cret");
  Alcotest.(check bool) "verify bad" false
    (verify_password ~stored:h "wrong");
  Alcotest.(check bool) "fresh hash needs no upgrade" false
    (needs_upgrade h)

let distinct_salts () =
  let a = hash_password "same" in
  let b = hash_password "same" in
  Alcotest.(check bool) "different hashes" true (a <> b);
  Alcotest.(check bool) "both verify" true
    (verify_password ~stored:a "same"
     && verify_password ~stored:b "same")

let legacy_legacy_still_verifies () =
  (* Row produced by the interim sha256 scheme (pre-argon2id on the
     switch). Must still verify after the argon2id cutover — and be
     flagged for the transparent upgrade. *)
  let salt_hex = hex_encode (Mirage_crypto_rng.generate 16) in
  let digest = sha256_hex (salt_hex ^ ":" ^ "interim-pass") in
  let stored = Printf.sprintf "sha256$%s$%s" salt_hex digest in
  Alcotest.(check bool) "legacy verifies" true
    (verify_password ~stored "interim-pass");
  Alcotest.(check bool) "legacy wrong-password rejected" false
    (verify_password ~stored "nope");
  Alcotest.(check bool) "legacy needs upgrade" true
    (needs_upgrade stored);
  let full = verify_full ~stored "interim-pass" in
  Alcotest.(check bool) "full: ok + needs_upgrade" true
    (full.ok && full.needs_upgrade)

let malformed_reject () =
  Alcotest.(check bool) "empty" false (verify_password ~stored:"" "x");
  Alcotest.(check bool) "garbage" false
    (verify_password ~stored:"not-a-hash" "x");
  Alcotest.(check bool) "partial sha256" false
    (verify_password ~stored:"sha256$ab" "x");
  Alcotest.(check bool) "corrupt argon2id" false
    (verify_password ~stored:"$argon2id$garbage" "x");
  Alcotest.(check bool) "corrupt needs no upgrade flag" false
    (needs_upgrade "$argon2id$garbage")

let secure_equal_tests () =
  Alcotest.(check bool) "eq" true (secure_equal "abc" "abc");
  Alcotest.(check bool) "neq" false (secure_equal "abc" "abd");
  Alcotest.(check bool) "len" false (secure_equal "ab" "abc")

let token_hash_deterministic () =
  let h1 = sha256_hex "tok" in
  let h2 = sha256_hex "tok" in
  Alcotest.(check string) "det" h1 h2;
  Alcotest.(check bool) "diff" true (h1 <> sha256_hex "tok2")

let () =
  Alcotest.run "auth_password"
    [ ("hash",
       [ ("roundtrip argon2id", `Quick, hash_roundtrip);
         ("distinct salts", `Quick, distinct_salts);
         ("legacy still verifies + upgrades", `Quick,
          legacy_legacy_still_verifies);
         ("malformed reject", `Quick, malformed_reject);
         ("secure_equal", `Quick, secure_equal_tests);
         ("token hash det", `Quick, token_hash_deterministic) ]) ]
