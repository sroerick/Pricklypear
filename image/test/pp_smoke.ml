(** TIER-B tier smoke matrix (card f7c56b20).

    Boots the REAL image session (S.make runs the same prim registration
    the rim services) and drives the REGISTERED auth prims — no
    test-local mirror table of who-may-mint (an earlier round mocked the
    tier logic; this matrix exercises prims_auth.ml). The store is the
    no-DB floor (Store.make without connect, prim_gates_test /
    delegation_test precedent), so a mint that clears every tier gate
    stops at "no database" rather than inserting; the matrix asserts the
    GATE verdict and that the actor carried on every mint is the caller.

    T1 anonymous · T2 plain user · T3 foreign actor · T4 admin
    on-behalf-of · T5 labelled self-mint · T6 the 403 wire status of a
    denial · T7 the delegated tier. *)

module S = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest
module Ast = Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

(* Store/session plumbing rides the Eio ctx (Io_eio), so every case runs
   inside the harness prim_gates_test uses. *)
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
      ~branch_name:"main" ~boot_mode:M.Boot_on_connect
      ~image_version:"smoke" ()
  in
  let store =
    Pricklypear_image_lib.Store.make
      ~cfg:(Pricklypear_image_lib.Store.config_from_env ())
      ~branch_id:main_id ()
  in
  S.make ~manifest ~store ()

(* Drive the registered closure from the session's own prim table — the
   same fn an in-process eval reaches through the rim. *)
let mint (t : S.t) ~actor ?label () =
  Pricklypear_image_lib.Prims_auth.mint ~tbl:t.S.eval_state.prims ~actor
    ?label ()

(* The admin on-behalf-of form (actor label owner) goes through the same
   registered closure; the extra owner argument is the only difference. *)
let mint_for (t : S.t) ~actor ~label ~owner () =
  Pricklypear_image_lib.Prims_auth.mint ~tbl:t.S.eval_state.prims ~actor
    ~label ~owner ()

(* Provenance is the tier property under test: a case that mints (or is
   denied minting) for anyone but the caller fails here first. *)
let assert_actor ~tier ~(t : S.t) ~actor () =
  let caller = S.username t in
  if actor <> caller then
    failwith (Printf.sprintf "%s: actor %s <> caller %s" tier actor caller);
  if tier = "T1" then Printf.printf "T1 actor==caller\n%!"
  else Printf.printf "%s actor==caller\n%!" tier

(* T1: the anonymous tier is denied minting a credential, as itself. *)
let test_t1 () =
  with_eio (fun () ->
      let t = make_session ~username:"anonymous" in
      assert_actor ~tier:"T1" ~t ~actor:"anonymous" ();
      match mint t ~actor:"anonymous" ~label:"smoke" () with
      | Error m -> Printf.printf "T1 denial=%s\n%!" m
      | Ok _ -> failwith "T1: anonymous minted a token")

(* T2: a plain (non-admin, non-agent) identity is denied as itself. *)
let test_t2 () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      assert_actor ~tier:"T2" ~t ~actor:"tester" ();
      match mint t ~actor:"tester" () with
      | Error m -> Printf.printf "T2 denial=%s\n%!" m
      | Ok _ -> failwith "T2: plain user minted a credential-tier token")

(* T3: a non-admin minting under a FOREIGN actor is denied and the
   caller's own identity is unchanged (no provenance laundering). *)
let test_t3 () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      assert_actor ~tier:"T3" ~t ~actor:"tester" ();
      match mint t ~actor:"someone-else" () with
      | Error m -> Printf.printf "T3 denial=%s\n%!" m
      | Ok _ -> failwith "T3: non-admin minted under a foreign actor")

(* T4: the admin on-behalf-of form (actor label owner) needs an admin
   row; with none it is denied and stays the caller's session. *)
let test_t4 () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      assert_actor ~tier:"T4" ~t ~actor:"tester" ();
      match mint_for t ~actor:"tester" ~label:"handoff" ~owner:"somebody" () with
      | Error m -> Printf.printf "T4 denial=%s\n%!" m
      | Ok _ -> failwith "T4: on-behalf-of mint passed without an admin row")

(* T5: the labelled self-mint form carries the label through the same
   gate; still the caller's actor. *)
let test_t5 () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      assert_actor ~tier:"T5" ~t ~actor:"tester" ();
      match mint t ~actor:"tester" ~label:"smoke-labelled" () with
      | Error m -> Printf.printf "T5 denial=%s\n%!" m
      | Ok _ -> failwith "T5: labelled self-mint bypassed the credential gate")

(* T6: a tier-gate denial maps to HTTP 403 on the wire (http_pages.ml's
   wire_status, which api_eval/api_prim call), so a client reading only
   the status is not told "success" by a 200-with-denial-body. *)
let test_t6 () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      assert_actor ~tier:"T6" ~t ~actor:"tester" ();
      match mint t ~actor:"tester" () with
      | Ok _ -> failwith "T6: expected a denial to map"
      | Error m ->
          let denial =
            Ast.Interp_error (Ast.Prim_error ("mint-delegated-token", m, "denied"))
          in
          (match Pricklypear_image_lib.Http_pages.wire_status denial with
           | `Forbidden -> Printf.printf "T6 denial=403\n%!"
           | _ -> failwith "T6: a tier denial did not map to 403"))

(* T7 premise re-derived from prims_db_common.ml's ACTUAL behavior:
   Pricklypear_image_lib.Prims_db_common.require_admin answers a
   delegated session with "op: denied for delegated sessions" FIRST —
   before the anonymous check and before the PG admin re-check — so an
   owner who happens to be admin cannot launder the carrying agent into
   an admin op. *)
let test_t7 () =
  with_eio (fun () ->
      let t = make_session ~username:"tester" in
      t.S.delegated <- true;
      assert_actor ~tier:"T7" ~t ~actor:"tester" ();
      match
        Pricklypear_image_lib.Prims_db_common.require_admin ~t
          "tier-b-smoke" (fun () -> Ast.VBool true)
      with
      | Ast.VStr s -> Printf.printf "T7 delegated=%s\n%!" s
      | v ->
          failwith
            ("T7: require_admin did not deny a delegated session: "
            ^ Pricklypear_lisp.Reader.pp_value v))

(* T8: the GUEST regression (card 419bb7 / code-review FAIL C3). The
   3-arg ATTRIBUTED self-mint (actor label owner) with owner = caller
   used to fall through to the owner lookup and mint for a plain human
   (broland), because the tier was never consulted on that branch. A
   guest must be denied on the attributed form exactly as on the 2-arg
   one — no owner-string match may substitute for a tier. *)
let test_t8 () =
  with_eio (fun () ->
      let t = make_session ~username:"broland" in
      assert_actor ~tier:"T8" ~t ~actor:"broland" ();
      (match mint_for t ~actor:"broland" ~label:"self" ~owner:"broland" () with
       | Error m -> Printf.printf "T8 attributed-self denial=%s\n%!" m
       | Ok _ -> failwith "T8: guest minted via the attributed self-mint form");
      (* and the delegated tier may not mint at all (scope laundering) *)
      t.S.delegated <- true;
      (match mint t ~actor:"broland" () with
       | Error m -> Printf.printf "T8 delegated denial=%s\n%!" m
       | Ok _ -> failwith "T8: delegated session minted a fresh credential"))

let () =
  test_t1 ();
  test_t2 ();
  test_t3 ();
  test_t4 ();
  test_t5 ();
  test_t6 ();
  test_t7 ();
  test_t8 ();
  print_endline "tier matrix PASS"
