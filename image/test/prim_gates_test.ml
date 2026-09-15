(** Write-surface gates (i-1788669791 / i-1788669842).

    undo joins the anonymous gate the rest of the write surface has;
    code-restore gains the same anon gate PLUS the branch-owner rule on
    its target ref (admin-only for system/*, own tenant otherwise)
    BEFORE the branch lookup — closing the promote bypass (RLS does not
    cover non-datum kinds, migration 013, so fn/type/route rows had no
    floor at all).

    Drives the REAL registered prim closures over no-DB sessions (same
    construction as prim_classification_test): denials fire before any
    store call; the authorized-ref path reaches the branch lookup,
    which answers "no such branch" on a no-DB store (lookup returns
    None without a connection). *)

open Pricklypear_lisp.Ast

module S = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest

let main_id = "00000000-0000-0000-0000-000000000001"

(* Store/session plumbing rides the Eio ctx (Io_eio), so every test
   body runs inside the same harness code_restore_test uses. *)
let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx f

let make_session ~username =
  let manifest =
    M.make ~user_id:("u-" ^ username) ~username ~branch_id:main_id
      ~branch_name:"main"
      ~boot_mode:M.Boot_on_connect ~image_version:"test" ()
  in
  let store =
    Pricklypear_image_lib.Store.make
      ~cfg:(Pricklypear_image_lib.Store.config_from_env ())
      ~branch_id:main_id ()
  in
  S.make ~manifest ~store ()

(* Call a registered prim by name through the session's rim + env:
   loader_test's driving shape, but on the SESSION's prim table and env
   (the real registration path ran in S.make). *)
let call t name (args : value list) =
  let prims = t.S.eval_state.prims in
  let env = t.S.eval_state.env in
  Pricklypear_image_lib.Control_rim.run ~prims (fun () ->
      Pricklypear_lisp.Eval.eval
        ~budget:(Pricklypear_lisp.Eval.make_budget ())
        ~prims:Pricklypear_lisp.Eval.empty_prims env
        (App (Var name, List.map (fun v -> Lit v) args)))

let pp v = Pricklypear_lisp.Reader.pp_value v

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

(* undo: anonymous -> User_error denial (raises: a returned string is
   dropped by (seq ...) and could partial-succeed). *)
let test_undo_anon_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"anonymous" in
      match call t "undo" [ VStr "data:x/y" ] with
      | v -> Alcotest.fail ("expected anon denial, got: " ^ pp v)
      | exception Interp_error (User_error (m, _)) ->
          Alcotest.(check string) "undo anon gate"
            "undo: denied for anonymous" m
      | exception e ->
          Alcotest.fail ("unexpected exn: " ^ Printexc.to_string e))

(* undo: authenticated passes the gate and reaches the store; on a
   no-DB store that is the Prim_error no-DB raise, NOT a denial. *)
let test_undo_authed_reaches_store () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "undo" [ VStr "data:x/y" ] with
      | v -> Alcotest.fail ("no-DB undo should not succeed: " ^ pp v)
      | exception Interp_error (Prim_error (n, m, _)) ->
          Alcotest.(check bool) "undo passed gate to store" true
            (n = "undo" && m = "failed: no DB")
      | exception e ->
          Alcotest.fail ("unexpected exn: " ^ Printexc.to_string e))

(* code-restore: anonymous -> User_error denial, before any lookup. *)
let test_code_restore_anon_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"anonymous" in
      match call t "code-restore" [ VStr "fn" ] with
      | v -> Alcotest.fail ("expected anon denial, got: " ^ pp v)
      | exception Interp_error (User_error (m, _)) ->
          Alcotest.(check string) "code-restore anon gate"
            "code-restore: denied for anonymous" m
      | exception e ->
          Alcotest.fail ("unexpected exn: " ^ Printexc.to_string e))

(* code-restore: non-admin targeting system/* (bare name defaults to
   system/<name> too) -> VStr denial, owner named, before the lookup. *)
let test_code_restore_system_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      let check_one ~label ref =
        match call t "code-restore" [ VStr "fn"; VStr ref ] with
        | VStr s ->
            Alcotest.(check bool) label true
              (starts_with ~prefix:"denied: branch " s)
        | v -> Alcotest.fail (label ^ ": expected VStr denial, got: " ^ pp v)
      in
      check_one ~label:"system/main denied" "system/main";
      check_one ~label:"bare name = system/<name> denied" "main")

(* code-restore: non-admin targeting ANOTHER tenant -> VStr denial. *)
let test_code_restore_other_tenant_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "code-restore" [ VStr "fn"; VStr "somebody/work" ] with
      | VStr s ->
          Alcotest.(check bool) "other-tenant denied" true
            (s = "denied: branch somebody/work is owned by somebody")
      | v -> Alcotest.fail ("expected VStr denial, got: " ^ pp v))

(* code-restore: OWN tenant passes the gate and reaches the branch
   lookup; no DB -> the clean "no such branch" answer (proves the ref
   went through, unlike a denial). *)
let test_code_restore_own_tenant_passes () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "code-restore" [ VStr "fn"; VStr "tester/missing" ] with
      | VStr s ->
          Alcotest.(check string) "own tenant reaches lookup"
            "code-restore: no such branch: tester/missing" s
      | v -> Alcotest.fail ("expected no-such-branch, got: " ^ pp v))

(* code-restore: admin keeps system/* access (username admin per the
   branch_owner_authorized admin rule). *)
let test_code_restore_admin_system_passes () =
  with_eio (fun () ->
      let t = make_session ~username:"admin" in
      match call t "code-restore" [ VStr "fn"; VStr "system/main" ] with
      | VStr s ->
          Alcotest.(check string) "admin reaches system/main lookup"
            "code-restore: no such branch: system/main" s
      | v -> Alcotest.fail ("expected no-such-branch, got: " ^ pp v))

(* fork: parent owned by ANOTHER tenant -> VStr denial before the
   lookup (i-1787970640 residue: fork copies the parent's code into the
   caller's readable branch — same exfil shape the merge-source gate
   closes). *)
let test_fork_other_tenant_parent_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "fork" [ VStr "somebody/work"; VStr "tester/copy" ] with
      | VStr s ->
          Alcotest.(check string) "other-tenant fork parent denied"
            "denied: branch somebody/work is owned by somebody" s
      | v -> Alcotest.fail ("expected VStr denial, got: " ^ pp v))

(* fork: system/* parent + own-tenant child passes both gates and
   reaches the lookup; no DB -> the clean "no such parent" answer
   (proves the gates let it through, unlike a denial). *)
let test_fork_system_parent_own_child_passes () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "fork" [ VStr "system/main"; VStr "tester/garden" ] with
      | VStr s ->
          Alcotest.(check string) "system parent fork reaches lookup"
            "no such parent: system/main" s
      | v -> Alcotest.fail ("expected no-such-parent, got: " ^ pp v))

(* fork: child owned by ANOTHER tenant still denied (write rule, kept
   from 511ebdf). *)
let test_fork_other_tenant_child_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "fork" [ VStr "system/main"; VStr "somebody/garden" ] with
      | VStr s ->
          Alcotest.(check string) "other-tenant fork child denied"
            "denied: branch somebody/garden is owned by somebody" s
      | v -> Alcotest.fail ("expected VStr denial, got: " ^ pp v))

(* merge: source owned by ANOTHER tenant -> VStr denial (kept from
   511ebdf; blocks reading others' code via merge results). *)
let test_merge_other_tenant_source_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "merge" [ VStr "somebody/work"; VStr "tester/main" ] with
      | VStr s ->
          Alcotest.(check string) "other-tenant merge source denied"
            "denied: branch somebody/work is owned by somebody" s
      | v -> Alcotest.fail ("expected VStr denial, got: " ^ pp v))

(* merge: system/* SOURCE into own tenant now PASSES (spec
   branching.merge: "per-user can merge into their own branches
   freely" — the 511ebdf source gate over-denied this); no DB -> the
   "source or target branch not found" answer, not a denial. *)
let test_merge_system_source_own_target_passes () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "merge" [ VStr "system/main"; VStr "tester/garden" ] with
      | VStr s ->
          Alcotest.(check string) "system source merge reaches lookup"
            "source or target branch not found" s
      | v -> Alcotest.fail ("expected not-found, got: " ^ pp v))

(* merge: target system/* stays admin-only for non-admins (kept). *)
let test_merge_system_target_denied () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      match call t "merge" [ VStr "tester/work"; VStr "system/main" ] with
      | VStr s ->
          Alcotest.(check string) "merge to system denied"
            "denied: admin-only merge to system branch" s
      | v -> Alcotest.fail ("expected VStr denial, got: " ^ pp v))

let () =
  Alcotest.run "pricklypear-prim-gates"
    [
      ( "gates"
      , [ ("undo-anon-denied", `Quick, test_undo_anon_denied)
        ; ("undo-authed-reaches-store", `Quick, test_undo_authed_reaches_store)
        ; ("code-restore-anon-denied", `Quick, test_code_restore_anon_denied)
        ; ("code-restore-system-denied", `Quick, test_code_restore_system_denied)
        ; ( "code-restore-other-tenant-denied"
          , `Quick
          , test_code_restore_other_tenant_denied )
        ; ( "code-restore-own-tenant-passes"
          , `Quick
          , test_code_restore_own_tenant_passes )
          ; ( "code-restore-admin-system-passes"
            , `Quick
            , test_code_restore_admin_system_passes )
          ; ( "fork-other-tenant-parent-denied"
            , `Quick
            , test_fork_other_tenant_parent_denied )
          ; ( "fork-system-parent-own-child-passes"
            , `Quick
            , test_fork_system_parent_own_child_passes )
          ; ( "fork-other-tenant-child-denied"
            , `Quick
            , test_fork_other_tenant_child_denied )
          ; ( "merge-other-tenant-source-denied"
            , `Quick
            , test_merge_other_tenant_source_denied )
          ; ( "merge-system-source-own-target-passes"
            , `Quick
            , test_merge_system_source_own_target_passes )
          ; ( "merge-system-target-denied"
            , `Quick
            , test_merge_system_target_denied )
        ] )
    ]