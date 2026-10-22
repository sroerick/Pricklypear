(** authz.ml step-1 tests (specs/authz-matrix.md): identity classes and
    the capability table over no-DB sessions (prim_gates_test harness
    shape). The table is inert until families migrate onto it (steps
    2-6); these pins travel with it so the matrix cannot drift
    silently. *)

module S = Pricklypear_image_lib.Session
module A = Pricklypear_image_lib.Authz
module M = Pricklypear_common.Manifest

let main_id = "00000000-0000-0000-0000-000000000001"

let make_session ~username ?(admin = false) ?(delegated = false) () =
  let manifest =
    M.make ~user_id:("u-" ^ username) ~username ~branch_id:main_id
      ~branch_name:"main" ~boot_mode:M.Boot_on_connect ~image_version:"test" ()
  in
  let store =
    Pricklypear_image_lib.Store.make
      ~cfg:(Pricklypear_image_lib.Store.config_from_env ())
      ~branch_id:main_id ()
  in
  let t = S.make ~manifest ~store () in
  t.is_admin <- admin;
  t.delegated <- delegated;
  t

let check_class label expected t =
  Alcotest.(check string) label
    (A.string_of_identity_class expected)
    (A.string_of_identity_class (A.identity_class_of_session t))

let check_auth label ~expect_ok t ~capability ?owner () =
  match (A.authorize t ~capability ?owner (), expect_ok) with
  | Ok (), true -> ()
  | Error _, false -> ()
  | Ok (), false -> Alcotest.fail (label ^ ": expected denial, got Ok")
  | Error e, true -> Alcotest.fail (label ^ ": expected Ok, got: " ^ e)

let test_classes () =
  check_class "anonymous" A.Anonymous (make_session ~username:"anonymous" ());
  check_class "self-human" A.Self_human (make_session ~username:"alice" ());
  check_class "self-agent" A.Self_agent
    (make_session ~username:"alice/agent" ());
  check_class "delegated" A.Delegated
    (make_session ~username:"alice" ~delegated:true ());
  check_class "admin" A.Admin (make_session ~username:"bob" ~admin:true ());
  (* spec_town_profile: PP_PROFILE flips every non-admin session to
     tenant; restore the env afterwards. *)
  Unix.putenv "PP_PROFILE" "town";
  check_class "tenant" A.Tenant (make_session ~username:"alice" ());
  Unix.putenv "PP_PROFILE" ""

let test_reseed_main () =
  check_auth "reseed-main admin ok" ~expect_ok:true
    (make_session ~username:"bob" ~admin:true ()) ~capability:A.Reseed_main ();
  check_auth "reseed-main self denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.Reseed_main ();
  check_auth "reseed-main delegated denied" ~expect_ok:false
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Reseed_main ()

let test_reseed_branch () =
  check_auth "reseed-branch self ok" ~expect_ok:true
    (make_session ~username:"alice" ()) ~capability:A.Reseed_branch ();
  check_auth "reseed-branch admin ok" ~expect_ok:true
    (make_session ~username:"bob" ~admin:true ())
    ~capability:A.Reseed_branch ();
  check_auth "reseed-branch delegated own ok" ~expect_ok:true
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Reseed_branch ~owner:"alice" ();
  check_auth "reseed-branch delegated stranger denied" ~expect_ok:false
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Reseed_branch ~owner:"hollis" ();
  check_auth "reseed-branch anonymous denied" ~expect_ok:false
    (make_session ~username:"anonymous" ()) ~capability:A.Reseed_branch ()

let test_reseed_branch_tenant () =
  Unix.putenv "PP_PROFILE" "town";
  check_auth "reseed-branch tenant own ok" ~expect_ok:true
    (make_session ~username:"alice" ())
    ~capability:A.Reseed_branch ~owner:"alice" ();
  check_auth "reseed-branch tenant stranger denied" ~expect_ok:false
    (make_session ~username:"alice" ())
    ~capability:A.Reseed_branch ~owner:"hollis" ();
  Unix.putenv "PP_PROFILE" ""

let test_branch_move () =
  check_auth "branch-move self ok" ~expect_ok:true
    (make_session ~username:"alice" ()) ~capability:A.Branch_move ();
  check_auth "branch-move delegated denied" ~expect_ok:false
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Branch_move ();
  check_auth "branch-move anonymous denied" ~expect_ok:false
    (make_session ~username:"anonymous" ()) ~capability:A.Branch_move ()

let test_code_persist () =
  check_auth "code-persist self ok" ~expect_ok:true
    (make_session ~username:"alice" ()) ~capability:A.Code_persist ();
  check_auth "code-persist delegated denied" ~expect_ok:false
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Code_persist ();
  check_auth "code-persist anonymous denied" ~expect_ok:false
    (make_session ~username:"anonymous" ()) ~capability:A.Code_persist ();
  Unix.putenv "PP_PROFILE" "town";
  check_auth "code-persist tenant denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.Code_persist ();
  Unix.putenv "PP_PROFILE" ""

let test_deftype () =
  check_auth "deftype self ok" ~expect_ok:true
    (make_session ~username:"alice" ()) ~capability:A.Deftype ();
  Unix.putenv "PP_PROFILE" "town";
  check_auth "deftype tenant denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.Deftype ();
  Unix.putenv "PP_PROFILE" ""

let test_tokens () =
  check_auth "token-own delegated ok" ~expect_ok:true
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Token_own ();
  check_auth "token-own anonymous denied" ~expect_ok:false
    (make_session ~username:"anonymous" ()) ~capability:A.Token_own ();
  check_auth "token-other admin ok" ~expect_ok:true
    (make_session ~username:"bob" ~admin:true ()) ~capability:A.Token_other ();
  check_auth "token-other self denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.Token_other ()

let test_admin_caps () =
  check_auth "user-admin admin ok" ~expect_ok:true
    (make_session ~username:"bob" ~admin:true ()) ~capability:A.User_admin ();
  check_auth "user-admin self denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.User_admin ();
  check_auth "retention-admin admin ok" ~expect_ok:true
    (make_session ~username:"bob" ~admin:true ())
    ~capability:A.Retention_admin ();
  check_auth "retention-admin self denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.Retention_admin ()

let test_data_and_promote () =
  check_auth "data-write self ok" ~expect_ok:true
    (make_session ~username:"alice" ()) ~capability:A.Data_write ();
  check_auth "data-write anonymous denied" ~expect_ok:false
    (make_session ~username:"anonymous" ()) ~capability:A.Data_write ();
  check_auth "data-read anonymous ok" ~expect_ok:true
    (make_session ~username:"anonymous" ()) ~capability:A.Data_read ();
  check_auth "promote-own self-agent ok" ~expect_ok:true
    (make_session ~username:"alice/agent" ()) ~capability:A.Promote_own ();
  check_auth "promote-own self-human denied" ~expect_ok:false
    (make_session ~username:"alice" ()) ~capability:A.Promote_own ()

let test_load_library_and_namespace () =
  check_auth "load-library delegated ok" ~expect_ok:true
    (make_session ~username:"alice" ~delegated:true ())
    ~capability:A.Load_library ();
  let t = make_session ~username:"alice" () in
  Alcotest.(check bool) "namespace own" true (A.owner_namespace_ok t "alice");
  Alcotest.(check bool) "namespace stranger" false
    (A.owner_namespace_ok t "hollis")

let () =
  Alcotest.run "authz"
    [ ( "authz-matrix step 1",
        [ Alcotest.test_case "identity classes" `Quick test_classes;
          Alcotest.test_case "reseed-main" `Quick test_reseed_main;
          Alcotest.test_case "reseed-branch" `Quick test_reseed_branch;
          Alcotest.test_case "reseed-branch tenant" `Quick
            test_reseed_branch_tenant;
          Alcotest.test_case "branch-move" `Quick test_branch_move;
          Alcotest.test_case "code-persist" `Quick test_code_persist;
          Alcotest.test_case "deftype" `Quick test_deftype;
          Alcotest.test_case "tokens" `Quick test_tokens;
          Alcotest.test_case "admin caps" `Quick test_admin_caps;
          Alcotest.test_case "data + promote" `Quick test_data_and_promote;
          Alcotest.test_case "load-library + namespace" `Quick
            test_load_library_and_namespace ] ) ]