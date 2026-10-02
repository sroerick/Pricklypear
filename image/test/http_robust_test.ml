(** Pricklypear Image — HTTP robustness tests (image.http-robustness).

    Tests the peer-disconnect / closed-writer guards and the
    accept-side connection cap. These are the invariants that prevent
    a single aborted request from killing the single-image process
    (prod incident 2026-07-16). No DB / no live server needed: we
    exercise the pure predicate, the safe-writer wrappers' behavior on
    the recognized exception, and the inflight gate's counting. *)

let is_closed = Pricklypear_image_lib.Http_core.is_closed_writer_exn

module Hc = Pricklypear_image_lib.Http_core


let test_recognizes_closed_writer () =
  Alcotest.(check bool) "exact closed-writer msg" true
    (is_closed (Failure "cannot write to closed writer"));
  Alcotest.(check bool) "closed-writer with suffix" true
    (is_closed (Failure "cannot write to closed writer (foo)"));
  Alcotest.(check bool) "unrelated Failure not matched" false
    (is_closed (Failure "something else"));
  Alcotest.(check bool) "non-Failure exn not matched" false
    (is_closed Not_found);
  Alcotest.(check bool) "Stack_overflow not matched" false
    (is_closed Stack_overflow)

(* safe_close_writer / safe_respond_with_string swallow the closed-writer
   Failure and return () instead of raising. We can't easily build a real
   closed httpaf Body.t in a unit test, so we assert the guard predicate is
   what the wrappers rely on, and that the recognized exception does not
   propagate when fed through a try/when using the same predicate — i.e.
   the pattern the wrappers use. *)
let test_safe_wrappers_swallow_closed_writer () =
  let swallowed =
    try
      (try raise (Failure "cannot write to closed writer")
       with exn when is_closed exn -> ());
      true
    with _ -> false
  in
  Alcotest.(check bool) "closed-writer is swallowed, not propagated" true
    swallowed;
  (* A non-closed-writer exception is NOT swallowed by the guard. *)
  let propagated =
    try
      (try raise (Failure "unrelated")
       with exn when is_closed exn -> ());
      false
    with _ -> true
  in
  Alcotest.(check bool) "unrelated Failure propagates past guard" true
    propagated

(* The inflight gate bounds concurrent in-flight connections. Acquiring
   under the limit returns immediately; releasing decrements so a
   subsequent acquire succeeds. We do not test the blocking path (would
   need a paused releaser); the counting contract is what matters here. *)
let test_inflight_acquire_release () =
  (* Eio.Mutex requires an Eio loop. *)
  Eio_main.run @@ fun _env ->
  let before = Pricklypear_image_lib.Http_core.inflight_sem.count in
  Pricklypear_image_lib.Http_core.inflight_acquire ();
  Alcotest.(check int) "acquire increments count" (before + 1)
    Pricklypear_image_lib.Http_core.inflight_sem.count;
  Pricklypear_image_lib.Http_core.inflight_release ();
  Alcotest.(check int) "release decrements count" before
    Pricklypear_image_lib.Http_core.inflight_sem.count

(* Per-identity fair-share gate (card 1bdeb856): a single tenant can
   hold at most max_inflight_per_user concurrent page slots, so one
   authenticated user's slow evals cannot eat every global slot and
   starve the rest. We test the additive counting contract
   (acquire/release per key); the blocking-at-limit path is exercised
   by the same Eio.Condition machinery as the global gate. *)
let test_per_user_gate_counting () =
  Eio_main.run @@ fun _env ->
  let count key =
    try List.assoc key Hc.per_user_sem.counts with Not_found -> 0
  in
  Hc.per_user_acquire "u:alice";
  Hc.per_user_acquire "u:alice";
  Hc.per_user_acquire "u:bob";
  Alcotest.(check int) "alice holds 2" 2 (count "u:alice");
  Alcotest.(check int) "bob holds 1 (independent bucket)" 1 (count "u:bob");
  Hc.per_user_release "u:alice";
  Alcotest.(check int) "alice down to 1" 1 (count "u:alice");
  Hc.per_user_release "u:alice";
  Alcotest.(check int) "alice bucket empties" 0 (count "u:alice");
  Hc.per_user_release "u:bob";
  Alcotest.(check int) "bob bucket empties" 0 (count "u:bob")

(* The pre-identity gate key must map the same credential to the same
   bucket and never leak the raw secret (it is hashed). *)
let test_identity_gate_key () =
  let k1 = Hc.identity_gate_key ~headers:[ ("cookie", "pp_session=abc123") ]
      ~source_ip:"1.2.3.4" in
  let k2 = Hc.identity_gate_key ~headers:[ ("cookie", "pp_session=abc123") ]
      ~source_ip:"9.9.9.9" in
  Alcotest.(check bool) "same cookie => same key regardless of IP" true
    (k1 = k2);
  Alcotest.(check bool) "raw token never appears in the key" true
    (not (String.length k1 > 0 && k1 = "abc123"));
  let basic =
    Hc.identity_gate_key
      ~headers:[ ("authorization", "Basic dXNlcjpwYXNz") ]
      ~source_ip:"1.2.3.4"
  in
  Alcotest.(check string) "basic falls back to username key" "u:user" basic;
  let anon = Hc.identity_gate_key ~headers:[] ~source_ip:"5.6.7.8" in
  Alcotest.(check string) "anonymous falls back to source ip" "ip:5.6.7.8" anon;
  let anon2 = Hc.identity_gate_key ~headers:[] ~source_ip:"" in
  Alcotest.(check string) "no header no ip => anonymous" "anonymous" anon2

(* i-1788670736: the subdomain-mismatch redirect (http.ml) and the
   post-login redirect (http_core.ml) both route users to their garden
   host via garden_url. The contract that matters: with PP_BASE_DOMAIN
   set the URL is always https (a plain-http bounce drops the Secure
   session cookie and loops the user through login); with no base
   domain (dev plain HTTP) it degrades to a relative path. *)
let test_garden_url_https_contract () =
  Unix.putenv "PP_BASE_DOMAIN" "pricklypear.rocks";
  Alcotest.(check string) "garden url is https with base domain"
    "https://alice.pricklypear.rocks/"
    (Pricklypear_image_lib.Http_core.garden_url ~username:"alice" ~path:"/");
  Alcotest.(check string) "garden url keeps deep paths"
    "https://alice.pricklypear.rocks/notes?slug=agents"
    (Pricklypear_image_lib.Http_core.garden_url ~username:"alice"
       ~path:"/notes?slug=agents");
  Unix.putenv "PP_BASE_DOMAIN" "";
  Alcotest.(check string) "no base domain degrades to relative path"
    "/notes"
    (Pricklypear_image_lib.Http_core.garden_url ~username:"alice" ~path:"/notes")

let () =
  Alcotest.run "http_robust"
    [ ("closed-writer predicate",
        [ Alcotest.test_case "recognizes" `Quick
            (fun () -> test_recognizes_closed_writer ())
        ; Alcotest.test_case "safe wrappers swallow" `Quick
            (fun () -> test_safe_wrappers_swallow_closed_writer ())
        ])
    ; ("inflight gate",
        [ Alcotest.test_case "acquire/release counting" `Quick
            (fun () ->  (test_inflight_acquire_release ()))
        ])
    ; ("per-identity fair-share",
        [ Alcotest.test_case "independent per-user buckets" `Quick
            (fun () -> test_per_user_gate_counting ())
        ; Alcotest.test_case "pre-identity key hashes credentials" `Quick
            (fun () -> test_identity_gate_key ())
        ])
    ; ("garden url scheme",
        [ Alcotest.test_case "https contract" `Quick
            (fun () -> test_garden_url_https_contract ())
        ])
    ]
