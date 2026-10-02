(** Pricklypear Image — PP_AUTH_DISABLED prod-shape guard tests.

    security.borg LAW 1 (structure over judgment): the dev-only
    PP_AUTH_DISABLED escape must not be able to boot a prod-shaped
    image with every gate off and a green /health. Boot.prepare now
    refuses that combination before any store work.

    Pure env tests: set the vars, call the guard, restore. *)

open Pricklypear_image_lib

let get k = Sys.getenv_opt k

let with_env k v f =
  let old = get k in
  (match v with Some x -> Unix.putenv k x | None -> Unix.putenv k "");
  let restore () =
    match old with Some x -> Unix.putenv k x | None -> Unix.putenv k ""
  in
  Fun.protect ~finally:restore f

let clear_prod_shape () =
  Unix.putenv "PP_PROFILE" "";
  Unix.putenv "PP_BASE_DOMAIN" "";
  Unix.putenv "PP_AUTH_DISABLED" "";
  Unix.putenv "PP_AUTH_DISABLED_ALLOW_PROD" ""

let refuses f =
  match f () with
  | () -> false
  | exception Failure _ -> true
  | exception _ -> false

let test_dev_ok () =
  clear_prod_shape ();
  with_env "PP_AUTH_DISABLED" (Some "1") (fun () ->
      Alcotest.(check bool) "dev (no prod shape) boots" false
        (refuses Boot.auth_disabled_prod_guard))

let test_unset_ok () =
  clear_prod_shape ();
  Alcotest.(check bool) "flag unset boots" false
    (refuses Boot.auth_disabled_prod_guard)

let test_refuses_with_profile () =
  clear_prod_shape ();
  with_env "PP_AUTH_DISABLED" (Some "1") (fun () ->
      with_env "PP_PROFILE" (Some "prod") (fun () ->
          Alcotest.(check bool) "disabled + PP_PROFILE refuses" true
            (refuses Boot.auth_disabled_prod_guard)))

let test_refuses_with_base_domain () =
  clear_prod_shape ();
  with_env "PP_AUTH_DISABLED" (Some "1") (fun () ->
      with_env "PP_BASE_DOMAIN" (Some "pricklypear.rocks") (fun () ->
          Alcotest.(check bool) "disabled + PP_BASE_DOMAIN refuses" true
            (refuses Boot.auth_disabled_prod_guard)))

let test_explicit_override () =
  clear_prod_shape ();
  with_env "PP_AUTH_DISABLED" (Some "1") (fun () ->
      with_env "PP_BASE_DOMAIN" (Some "pricklypear.rocks") (fun () ->
          with_env "PP_AUTH_DISABLED_ALLOW_PROD" (Some "1") (fun () ->
              Alcotest.(check bool) "explicit override boots" false
                (refuses Boot.auth_disabled_prod_guard))))

let () =
  Alcotest.run "auth_disabled_guard"
    [ ( "pp-auth-disabled",
        [ Alcotest.test_case "dev ok" `Quick test_dev_ok
        ; Alcotest.test_case "unset ok" `Quick test_unset_ok
        ; Alcotest.test_case "refuses with profile" `Quick
            test_refuses_with_profile
        ; Alcotest.test_case "refuses with base domain" `Quick
            test_refuses_with_base_domain
        ; Alcotest.test_case "explicit override" `Quick test_explicit_override
        ] )
    ]
