(** Pricklypear Image — session-cookie Secure + HSTS policy tests
    (i-1787970928 follow-up, 2026-10-01).

    The card's residual: cf3938e emitted "; Secure" ONLY when
    PP_BASE_DOMAIN was set, but prod serves https with no base domain —
    so prod handed out a cookie with no Secure flag (verified live:
    /login Set-Cookie had none). These tests pin the new resolution
    order and the HSTS default.

    No DB / no server: pure env + string inspection. Env vars are
    mutated per-case and restored by the runner (single process). *)

open Pricklypear_image_lib

(* ---- env helpers (process-global; restore after each case) ---- *)

let get k = Sys.getenv_opt k

let with_env k v f =
  let old = get k in
  (match v with
   | Some x -> Unix.putenv k x
   | None -> Unix.putenv k "");
  let restore () =
    match old with
    | Some x -> Unix.putenv k x
    | None -> Unix.putenv k ""   (* OCaml has no unsetenv; "" ~ unset for us *)
  in
  Fun.protect ~finally:restore f

let contains ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec loop i =
    if i + nl > hl then false
    else if String.sub hay i nl = needle then true
    else loop (i + 1)
  in
  loop 0

let check_bool msg expected actual = Alcotest.(check bool) msg expected actual
let check_str msg expected actual = Alcotest.(check string) msg expected actual

(* ---- cookie_secure resolution order ---- *)

let test_secure_flag_over_base_domain () =
  (* Original heuristic preserved: base domain set -> Secure. *)
  with_env "PP_BASE_DOMAIN" (Some "example.test") (fun () ->
      with_env "PP_COOKIE_SECURE" (Some "") (fun () ->
          check_bool "base domain alone yields Secure" true
            (Http_core.cookie_secure ())))

let test_secure_force_on_no_base () =
  (* THE PROD FIX: PP_COOKIE_SECURE=1 works with no base domain. *)
  with_env "PP_BASE_DOMAIN" (Some "") (fun () ->
      with_env "PP_COOKIE_SECURE" (Some "1") (fun () ->
          check_bool "PP_COOKIE_SECURE=1 forces Secure without a base domain"
            true (Http_core.cookie_secure ())))

let test_secure_force_off () =
  with_env "PP_BASE_DOMAIN" (Some "example.test") (fun () ->
      with_env "PP_COOKIE_SECURE" (Some "0") (fun () ->
          check_bool "PP_COOKIE_SECURE=0 forces Secure OFF (http dev)" false
            (Http_core.cookie_secure ())))

let test_secure_default_off () =
  with_env "PP_BASE_DOMAIN" (Some "") (fun () ->
      with_env "PP_COOKIE_SECURE" (Some "") (fun () ->
          check_bool "no config -> not Secure (unchanged default)" false
            (Http_core.cookie_secure ())))

(* ---- emitted header shape ---- *)

let test_set_cookie_header_emits_secure () =
  with_env "PP_BASE_DOMAIN" (Some "") (fun () ->
      with_env "PP_COOKIE_SECURE" (Some "1") (fun () ->
          let h = Http_core.set_cookie_header ~token:"tok" () in
          check_bool "Set-Cookie carries '; Secure'" true
            (contains ~needle:"; Secure" h);
          check_bool "Set-Cookie is HttpOnly" true
            (contains ~needle:"HttpOnly" h)))

let test_set_cookie_force_secure_param () =
  (* The live per-request path (X-Forwarded-Proto: https) forces Secure
     even with no env config at all. *)
  with_env "PP_BASE_DOMAIN" (Some "") (fun () ->
      with_env "PP_COOKIE_SECURE" (Some "") (fun () ->
          let h = Http_core.set_cookie_header ~force_secure:true ~token:"t" () in
          check_bool "force_secure emits '; Secure'" true
            (contains ~needle:"; Secure" h);
          let h2 = Http_core.set_cookie_header ~force_secure:false ~token:"t" () in
          check_bool "no force + no config -> no Secure" false
            (contains ~needle:"; Secure" h2)))

(* ---- X-Forwarded-Proto parsing ---- *)

let test_request_is_https () =
  let hs = [ ("x-forwarded-proto", "https") ] in
  check_bool "https detected" true (Http_core.request_is_https hs);
  check_bool "http not https" false
    (Http_core.request_is_https [ ("x-forwarded-proto", "http") ]);
  check_bool "first token of a list wins" true
    (Http_core.request_is_https [ ("x-forwarded-proto", "https, http") ]);
  check_bool "absent header" false (Http_core.request_is_https [])

(* ---- HSTS ---- *)

let test_hsts_default_on () =
  with_env "PP_HSTS_MAX_AGE" (Some "") (fun () ->
      let hs = Http_core.security_headers () in
      check_bool "HSTS present by default" true
        (List.mem_assoc "strict-transport-security" hs))

let test_hsts_disabled () =
  with_env "PP_HSTS_MAX_AGE" (Some "0") (fun () ->
      let hs = Http_core.security_headers () in
      check_bool "PP_HSTS_MAX_AGE=0 removes HSTS" false
        (List.mem_assoc "strict-transport-security" hs))

let test_hsts_value () =
  with_env "PP_HSTS_MAX_AGE" (Some "31536000") (fun () ->
      let hs = Http_core.security_headers () in
      check_str "HSTS max-age mirrors config" "max-age=31536000"
        (List.assoc "strict-transport-security" hs))

let () =
  Alcotest.run "http_secure_cookie"
    [ ( "cookie-secure",
        [ Alcotest.test_case "base-domain heuristic" `Quick
            test_secure_flag_over_base_domain;
          Alcotest.test_case "force on, no base domain (PROD FIX)" `Quick
            test_secure_force_on_no_base;
          Alcotest.test_case "force off" `Quick test_secure_force_off;
          Alcotest.test_case "default off" `Quick test_secure_default_off ] );
      ( "set-cookie",
        [ Alcotest.test_case "emits Secure" `Quick
            test_set_cookie_header_emits_secure;
          Alcotest.test_case "force_secure param" `Quick
            test_set_cookie_force_secure_param ] );
      ( "x-forwarded-proto",
        [ Alcotest.test_case "parses" `Quick test_request_is_https ] );
      ( "hsts",
        [ Alcotest.test_case "default on" `Quick test_hsts_default_on;
          Alcotest.test_case "disabled" `Quick test_hsts_disabled;
          Alcotest.test_case "value" `Quick test_hsts_value ] );
    ]
