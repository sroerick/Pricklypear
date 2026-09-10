(** Envelope tests (i-1788670058) - no DB.

    Roundtrip + identity binding + tamper rejection for the AES-256-GCM
    envelope, plus the Auth_store secret-blob reader's dual-shape
    behavior: envelope blobs open under the RIGHT user_id only, legacy
    plaintext {"api_key":...} blobs still read (the boot migration's
    input), and a degraded-mode write is never silently "sealed". *)

module E = Pricklypear_image_lib.Envelope
module A = Pricklypear_image_lib.Auth_store

let key_hex =
  "3f2a1b0c9d8e7f6a5b4c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3f2a"

let with_env_key key f =
  Unix.putenv "PP_LLM_ENVELOPE_KEY" key;
  let r = f () in
  (* empty value reads as unset everywhere in Envelope *)
  Unix.putenv "PP_LLM_ENVELOPE_KEY" "";
  r

let is_error = function Error _ -> true | Ok _ -> false

let contains_sub (hay : string) (needle : string) : bool =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (String.sub hay i m = needle || go (i + 1)) in
  m = 0 || go 0


(* no master key -> encryption refuses (degraded mode must be LOUD, and
   the caller's plaintext fallback is visible, not silent) *)
let test_no_key_encrypt_errors () =
  with_env_key "" (fun () ->
      Alcotest.(check bool) "encrypt without key errors" true
        (is_error (E.encrypt ~user_id:"u1" "sk-abc")))

(* roundtrip: seal, blob leaks nothing, open with the same identity *)
let test_roundtrip () =
  with_env_key key_hex (fun () ->
      match E.encrypt ~user_id:"u1" "sk-test-1234567890" with
      | Error e -> Alcotest.fail ("encrypt failed: " ^ e)
      | Ok blob ->
          Alcotest.(check bool) "blob is envelope" true
            (E.is_envelope_blob blob);
          Alcotest.(check bool) "blob hides plaintext" true
            (not (contains_sub blob "sk-test-1234567890"));
          (match E.decrypt ~user_id:"u1" blob with
           | Some k ->
               Alcotest.(check string) "roundtrip" "sk-test-1234567890" k
           | None -> Alcotest.fail "decrypt failed for right identity"))

(* index of the first char of the "ct" value in an envelope blob *)
let ct_value_start (blob : string) : int =
  let marker = "\"ct\":" in
  let rec find i =
    if i + String.length marker > String.length blob then
      failwith "no ct member in blob"
    else if String.sub blob i (String.length marker) = marker then
      i + String.length marker
    else find (i + 1)
  in
  let rec skip_space j = if blob.[j] = ' ' then skip_space (j + 1) else j in
  (skip_space (find 0)) + 1 (* past the opening quote *)

(* identity binding: the AAD binds ciphertext to the row's user_id *)
let test_wrong_user_fails () =
  with_env_key key_hex (fun () ->
      match E.encrypt ~user_id:"u1" "sk-test-1234567890" with
      | Error e -> Alcotest.fail e
      | Ok blob ->
          Alcotest.(check bool) "other user gets None" true
            (E.decrypt ~user_id:"u2" blob = None))

(* rekey: a blob sealed under one master key does not open under another *)
let test_wrong_key_fails () =
  with_env_key key_hex (fun () ->
      match E.encrypt ~user_id:"u1" "sk-test-1234567890" with
      | Error e -> Alcotest.fail e
      | Ok blob ->
          Unix.putenv "PP_LLM_ENVELOPE_KEY"
            "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef";
          let r = E.decrypt ~user_id:"u1" blob in
          Unix.putenv "PP_LLM_ENVELOPE_KEY" "";
          Alcotest.(check bool) "other key gets None" true (r = None))

(* tamper: flipping one ciphertext character kills the GCM tag *)
let test_tamper_fails () =
  with_env_key key_hex (fun () ->
      match E.encrypt ~user_id:"u1" "sk-test-1234567890" with
      | Error e -> Alcotest.fail e
      | Ok blob ->
          let i = ct_value_start blob in
          let c0 = blob.[i] in
          let c1 = if c0 = 'A' then 'B' else 'A' in
          let tampered =
            String.concat ""
              [ String.sub blob 0 i
              ; String.make 1 c1
              ; String.sub blob (i + 1) (String.length blob - i - 1) ]
          in
          Alcotest.(check bool) "tampered blob gets None" true
            (E.decrypt ~user_id:"u1" tampered = None))

(* the Auth_store reader: legacy plaintext still reads (migration input),
   envelope opens for the right identity, other identity is None *)
let test_reader_dual_shape () =
  with_env_key key_hex (fun () ->
      Alcotest.(check (option string)) "legacy plaintext reads" (Some "sk-legacy")
        (A.llm_key_of_secret ~user_id:"u1" "{\"api_key\": \"sk-legacy\"}");
      match E.encrypt ~user_id:"u1" "sk-sealed" with
      | Error e -> Alcotest.fail e
      | Ok blob ->
          Alcotest.(check (option string)) "envelope reads for owner"
            (Some "sk-sealed")
            (A.llm_key_of_secret ~user_id:"u1" blob);
          Alcotest.(check bool) "envelope None for other identity" true
            (A.llm_key_of_secret ~user_id:"u2" blob = None))

(* non-hex passphrase secrets stretch deterministically and roundtrip *)
let test_passphrase_secret_roundtrip () =
  with_env_key "a memorable passphrase with spaces" (fun () ->
      match E.encrypt ~user_id:"u1" "sk-pass" with
      | Error e -> Alcotest.fail e
      | Ok blob ->
          Alcotest.(check (option string)) "passphrase roundtrip" (Some "sk-pass")
            (E.decrypt ~user_id:"u1" blob))

let () =
  (* same seeding the live image does in Boot.prepare; [generate] for the
     nonce raises without a default generator *)
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run "pricklypear-envelope"
    [ ( "envelope"
      , [ ("no-key-encrypt-errors", `Quick, test_no_key_encrypt_errors)
        ; ("roundtrip", `Quick, test_roundtrip)
        ; ("wrong-user-fails", `Quick, test_wrong_user_fails)
        ; ("wrong-key-fails", `Quick, test_wrong_key_fails)
        ; ("tamper-fails", `Quick, test_tamper_fails)
        ; ("reader-dual-shape", `Quick, test_reader_dual_shape)
        ; ( "passphrase-secret-roundtrip"
          , `Quick
          , test_passphrase_secret_roundtrip )
        ] ) ]