(** Pure unit tests for interim password hashing + token hash. *)

let () = Mirage_crypto_rng_unix.use_default ()

let hash_roundtrip () =
  let h = Pricklypear_image_lib.Auth_password.hash_password "s3cret" in
  Alcotest.(check bool) "prefix" true
    (String.length h > 10
     && String.sub h 0 7 = "sha256$");
  Alcotest.(check bool) "verify ok" true
    (Pricklypear_image_lib.Auth_password.verify_password ~stored:h "s3cret");
  Alcotest.(check bool) "verify bad" false
    (Pricklypear_image_lib.Auth_password.verify_password ~stored:h "wrong")

let distinct_salts () =
  let a = Pricklypear_image_lib.Auth_password.hash_password "same" in
  let b = Pricklypear_image_lib.Auth_password.hash_password "same" in
  Alcotest.(check bool) "different hashes" true (a <> b);
  Alcotest.(check bool) "both verify" true
    (Pricklypear_image_lib.Auth_password.verify_password ~stored:a "same"
     && Pricklypear_image_lib.Auth_password.verify_password ~stored:b "same")

let malformed_reject () =
  Alcotest.(check bool) "empty" false
    (Pricklypear_image_lib.Auth_password.verify_password ~stored:"" "x");
  Alcotest.(check bool) "garbage" false
    (Pricklypear_image_lib.Auth_password.verify_password ~stored:"not-a-hash" "x");
  Alcotest.(check bool) "partial" false
    (Pricklypear_image_lib.Auth_password.verify_password
       ~stored:"sha256$ab" "x")

let secure_equal_tests () =
  Alcotest.(check bool) "eq" true
    (Pricklypear_image_lib.Auth_password.secure_equal "abc" "abc");
  Alcotest.(check bool) "neq" false
    (Pricklypear_image_lib.Auth_password.secure_equal "abc" "abd");
  Alcotest.(check bool) "len" false
    (Pricklypear_image_lib.Auth_password.secure_equal "ab" "abc")

let token_hash_deterministic () =
  let h1 = Pricklypear_image_lib.Auth_password.sha256_hex "tok" in
  let h2 = Pricklypear_image_lib.Auth_password.sha256_hex "tok" in
  Alcotest.(check string) "det" h1 h2;
  Alcotest.(check bool) "diff" true
    (h1 <> Pricklypear_image_lib.Auth_password.sha256_hex "tok2")

let () =
  Alcotest.run "auth_password"
    [ ("hash",
       [ ("roundtrip", `Quick, hash_roundtrip);
         ("distinct salts", `Quick, distinct_salts);
         ("malformed reject", `Quick, malformed_reject);
         ("secure_equal", `Quick, secure_equal_tests);
         ("token hash det", `Quick, token_hash_deterministic) ]) ]
