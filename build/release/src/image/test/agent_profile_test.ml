(** Bumpers M6 (borge/bumpers.borg notices-envelope quiet tiers): the
    DURABLE per-identity quiet preference served by (agent-profile
    'quiet) — tier (c) of the three-tier quiet mode (request flag >
    session taper > durable datum), stored as a user datum of type
    "profile" (author = the identity; mirrored data_rows path
    data.u.<user>.profile.<id>).

    Against a real Postgres (scripts/dev.sh; skips when unreachable), the
    profile must:
      - start false for a fresh identity (nothing stored);
      - persist: a NEW session of the same identity reads back what an
        EARLIER session set, and after unset a THIRD session reads false;
      - anonymous/public (shared system/main — no personal garden) stay
        strictly in-memory: set true in session A, a fresh anonymous
        session still loads false.

    pp.user is bound explicitly (Store.with_pp_user) around set/load, the
    way the HTTP request context binds it in production — the datum path
    is data.u.<user>.* and the RLS floor/WITH CHECK is confined to that
    identity. *)

module S = Pricklypear_image_lib.Store
module Ses = Pricklypear_image_lib.Session
module M = Pricklypear_common.Manifest

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let st = S.make ~cfg ~branch_id:main_id () in
      S.connect st;
      match S.conn_of st with
      | Some _ -> f st
      | _ ->
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!")

let make_session st ~username ~user_id =
  let manifest =
    M.make ~user_id ~username ~branch_id:main_id ~branch_name:"system/main"
      ~boot_mode:M.Boot_always_on ~image_version:"test" ()
  in
  Ses.make ~manifest ~store:st ()

let test_durable_quiet_persists_across_sessions () =
  with_store (fun st ->
      (* fresh identity: default false, nothing stored yet *)
      let s1 = make_session st ~username:"alice" ~user_id:"u-alice" in
      Alcotest.(check bool) "fresh session starts quiet=false" false
        (Ses.durable_quiet s1);
      (* set quiet true (persists a profile datum) *)
      S.with_pp_user "alice" (fun () -> Ses.set_agent_profile_quiet s1 ~quiet:true);
      Alcotest.(check bool) "set updates the in-memory flag" true
        (Ses.durable_quiet s1);
      (* a NEW session of the same identity reads the durable pref back *)
      let s2 = make_session st ~username:"alice" ~user_id:"u-alice" in
      S.with_pp_user "alice" (fun () -> Ses.load_agent_profile s2);
      Alcotest.(check bool) "durable quiet survives a new session" true
        (Ses.durable_quiet s2);
      (* unset -> a THIRD session reads false *)
      S.with_pp_user "alice" (fun () -> Ses.set_agent_profile_quiet s2 ~quiet:false);
      let s3 = make_session st ~username:"alice" ~user_id:"u-alice" in
      S.with_pp_user "alice" (fun () -> Ses.load_agent_profile s3);
      Alcotest.(check bool) "durable quiet unset survives a new session" false
        (Ses.durable_quiet s3))

let test_anonymous_stays_in_memory () =
  with_store (fun st ->
      (* anonymous has no personal branch: set is in-memory only *)
      let s1 = make_session st ~username:"anonymous" ~user_id:"anonymous" in
      S.with_pp_user "anonymous" (fun () -> Ses.set_agent_profile_quiet s1 ~quiet:true);
      Alcotest.(check bool) "anon in-memory quiet is true" true
        (Ses.durable_quiet s1);
      (* a fresh anonymous session must NOT inherit it (system/main shared) *)
      let s2 = make_session st ~username:"anonymous" ~user_id:"anonymous" in
      S.with_pp_user "anonymous" (fun () -> Ses.load_agent_profile s2);
      Alcotest.(check bool) "anon quiet does not persist" false
        (Ses.durable_quiet s2))

let () =
  Alcotest.run "agent-profile"
    [ ( "durable-quiet"
      , [ ("persists-across-sessions", `Quick, test_durable_quiet_persists_across_sessions)
        ; ("anonymous-in-memory-only", `Quick, test_anonymous_stays_in_memory)
        ] ) ]
