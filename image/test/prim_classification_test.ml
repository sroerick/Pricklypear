(** Boot-time prim-classification assertion (i-1788669715 / i-1788670318).

    Session.make asserts every REGISTERED prim appears in exactly one
    Effect capability bucket. This test constructs a full session over a
    no-DB store so the same registration path runs here: any prim the
    classification sweep missed fails construction with a Failure naming
    it. Also spot-checks the strict gate: unclassified names infer as
    EffSet.top (denied by every allowlist). *)

module S = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest
module E = Pricklypear_lisp.Effect

let main_id = "00000000-0000-0000-0000-000000000001"

let test_full_catalog_classified () =
  let manifest =
    M.make ~user_id:"test-user" ~username:"test" ~branch_id:main_id
      ~branch_name:"main"
      ~boot_mode:M.Boot_on_connect ~image_version:"test" ()
  in
  let store =
    Pricklypear_image_lib.Store.make
      ~cfg:(Pricklypear_image_lib.Store.config_from_env ())
      ~branch_id:main_id ()
  in
  (* Session.make registers the FULL prim catalog and then asserts the
     classification; a raise here is the test failing loudly. *)
  let _t = S.make ~manifest ~store () in
  Alcotest.(check bool) "session make passed classification" true true

let test_strict_fails_closed () =
  (* 2026-09-11: unclassified names RAISE Unresolved_name (the gate names
     the offender); catching it and denying is the fail-closed path —
     every raise site previously inferred EffSet.top and was denied. *)
  (match E.strict_prim_effects "definitely-not-a-prim-xyz" with
   | fx ->
       Alcotest.failf "expected Unresolved_name, got effects %s"
         (E.EffSet.pp fx)
   | exception E.Unresolved_name n ->
       Alcotest.(check string) "names the offender" n
         "definitely-not-a-prim-xyz");
  let fx2 = E.strict_prim_effects "eval-string" in
  Alcotest.(check bool) "eval-string is WriteDb (meta-eval)" true
    (E.EffSet.mem E.WriteDb fx2);
  Alcotest.(check bool) "eval-string not public-allowed" false
    (E.policy_allows ~allow:(E.public_route_allow ()) fx2)

let () =
  let open Alcotest in
  run "prim_classification"
    [
      ( "boot-assertion"
      , [ test_case "full catalog classified (Session.make)" `Quick
            test_full_catalog_classified
        ; test_case "strict lookup fails closed" `Quick test_strict_fails_closed
        ] )
    ]
