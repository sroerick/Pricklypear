(** Auth_store user-type flip (auth.user-management admin surface; the
    agent-native chapter's pending type flip).

    DB-backed tests for Auth_store.set_user_type — the store function
    behind POST /admin/users action=set_user_type and the admin-only
    (set-user-type username "agent"|"human") prim. Needs Postgres
    (scripts/dev.sh); skips cleanly when unreachable.

    Covers:
      - human -> agent with parent: user_type + parent_user_id set
      - agent -> human: parent cleared
      - invalid user_type rejected
      - agent flip with a missing parent rejected
      - agent-parent guard: an agent can never parent another agent
      - unknown user rejected
*)

module S = Pricklypear_image_lib.Store
module A = Pricklypear_image_lib.Auth_store

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p = Printf.sprintf "asut-%s-%d" p (Random.int 1_000_000)

let () = Random.self_init ()

(* Sweep leftovers from aborted runs (one shot by prefix). *)
let cleanup_all (store : S.t) : unit =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      let _ =
        S.exec conn ~params:[]
          "DELETE FROM users WHERE username LIKE 'asut-%'"
      in
      ()

let with_store f =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ ->
          cleanup_all store;
          f store)

(* Raw DELETE of the temp users (no credentials/sessions are created). *)
let cleanup_user (store : S.t) ~(username : string) : unit =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      let _ =
        S.exec conn
          ~params:[ Pgx.Value.of_string username ]
          "DELETE FROM users WHERE username = $1"
      in
      ()

let user_shape (u : A.user) = (u.A.user_type, u.A.parent_user_id)

let test_flip_to_agent_with_parent () =
  with_store @@ fun store ->
  let parent_name = uniq "parent" in
  let agent_name = uniq "bot" in
  let p =
    match A.create_user store ~username:parent_name ~is_admin:true with
    | Ok u -> u
    | Error e -> Alcotest.failf "create parent: %s" e
  in
  let b =
    match A.create_user store ~username:agent_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create agent: %s" e
  in
  (match A.set_user_type store ~user_id:b.id ~user_type:"agent"
           ~parent_user_id:(Some p.id) with
  | Error e -> Alcotest.failf "flip to agent: %s" e
  | Ok () ->
      let after =
        match A.get_user_by_username store agent_name with
        | Some u -> u
        | None -> Alcotest.fail "agent user vanished"
      in
      Alcotest.(check (pair string (option string)))
        "agent shape" ("agent", Some p.id) (user_shape after));
  cleanup_user store ~username:agent_name;
  cleanup_user store ~username:parent_name

let test_flip_back_to_human_clears_parent () =
  with_store @@ fun store ->
  let parent_name = uniq "parent" in
  let agent_name = uniq "bot" in
  let p =
    match A.create_user store ~username:parent_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create parent: %s" e
  in
  let b =
    match A.create_user store ~username:agent_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create agent: %s" e
  in
  let () =
    match A.set_user_type store ~user_id:b.id ~user_type:"agent"
            ~parent_user_id:(Some p.id) with
    | Ok () -> ()
    | Error e -> Alcotest.failf "flip to agent: %s" e
  in
  (match A.set_user_type store ~user_id:b.id ~user_type:"human"
           ~parent_user_id:None with
  | Error e -> Alcotest.failf "flip back to human: %s" e
  | Ok () ->
      let after =
        match A.get_user_by_username store agent_name with
        | Some u -> u
        | None -> Alcotest.fail "user vanished"
      in
      Alcotest.(check (pair string (option string)))
        "human shape" ("human", None) (user_shape after));
  cleanup_user store ~username:agent_name;
  cleanup_user store ~username:parent_name

let test_rejects_bad_type () =
  with_store @@ fun store ->
  let name = uniq "u" in
  let u =
    match A.create_user store ~username:name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create user: %s" e
  in
  (match A.set_user_type store ~user_id:u.id ~user_type:"robot"
           ~parent_user_id:None with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "bad user_type accepted");
  cleanup_user store ~username:name

let test_rejects_missing_parent () =
  with_store @@ fun store ->
  let name = uniq "u" in
  let u =
    match A.create_user store ~username:name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create user: %s" e
  in
  (match A.set_user_type store ~user_id:u.id ~user_type:"agent"
           ~parent_user_id:
             (Some "00000000-0000-0000-0000-00000000dead") with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "agent flip with missing parent accepted");
  cleanup_user store ~username:name

let test_agent_cannot_parent_agent () =
  with_store @@ fun store ->
  let parent_name = uniq "parent" in
  let bot1_name = uniq "bot1" in
  let bot2_name = uniq "bot2" in
  let p =
    match A.create_user store ~username:parent_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create parent: %s" e
  in
  let b1 =
    match A.create_user store ~username:bot1_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create bot1: %s" e
  in
  let b2 =
    match A.create_user store ~username:bot2_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create bot2: %s" e
  in
  let () =
    match A.set_user_type store ~user_id:b1.id ~user_type:"agent"
            ~parent_user_id:(Some p.id) with
    | Ok () -> ()
    | Error e -> Alcotest.failf "flip bot1: %s" e
  in
  (match A.set_user_type store ~user_id:b2.id ~user_type:"agent"
           ~parent_user_id:(Some b1.id) with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "agent parent accepted");
  cleanup_user store ~username:bot2_name;
  cleanup_user store ~username:bot1_name;
  cleanup_user store ~username:parent_name

let test_unknown_user_rejected () =
  with_store @@ fun store ->
  match A.set_user_type store
          ~user_id:"00000000-0000-0000-0000-00000000dead"
          ~user_type:"agent" ~parent_user_id:None with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "unknown user accepted"

(* ---- delete_user (admin-surface gap card 0d20dc58) ---- *)

let test_delete_user_removes_row () =
  with_store @@ fun store ->
  let name = uniq "del" in
  let u =
    match A.create_user store ~username:name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create user: %s" e
  in
  (match A.delete_user store ~user_id:u.id with
   | Error e -> Alcotest.failf "delete user: %s" e
   | Ok n -> Alcotest.(check int) "one row deleted" 1 n);
  (match A.get_user_by_username store name with
   | None -> ()
   | Some _ -> Alcotest.fail "user still present after delete")

let test_delete_user_cascades_credentials () =
  with_store @@ fun store ->
  let name = uniq "delcred" in
  let u =
    match A.create_user store ~username:name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create user: %s" e
  in
  (match A.set_password store ~user_id:u.id ~password:"pw-123456" with
   | Ok () -> ()
   | Error e -> Alcotest.failf "set password: %s" e);
  (match A.delete_user store ~user_id:u.id with
   | Error e -> Alcotest.failf "delete user: %s" e
   | Ok _ -> ());
  (* The password credential row cascaded with the user. *)
  Alcotest.(check (option string)) "no password hash after delete" None
    (Option.map snd (A.password_hash_for_user store ~username:name))

let test_delete_user_reparents_agent () =
  with_store @@ fun store ->
  let parent_name = uniq "delparent" in
  let agent_name = uniq "delagent" in
  let p =
    match A.create_user store ~username:parent_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create parent: %s" e
  in
  let a =
    match A.create_user store ~username:agent_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create agent: %s" e
  in
  (match A.set_user_type store ~user_id:a.id ~user_type:"agent"
           ~parent_user_id:(Some p.id) with
   | Ok () -> ()
   | Error e -> Alcotest.failf "flip to agent: %s" e);
  (* Deleting the parent must not fail on the FK; the agent is reparented. *)
  (match A.delete_user store ~user_id:p.id with
   | Error e -> Alcotest.failf "delete parent: %s" e
   | Ok _ -> ());
  let after =
    match A.get_user_by_username store agent_name with
    | Some u -> u
    | None -> Alcotest.fail "agent vanished with parent"
  in
  Alcotest.(check (option string)) "agent reparented to NULL" None
    after.A.parent_user_id;
  cleanup_user store ~username:agent_name

let test_delete_unknown_user_zero () =
  with_store @@ fun store ->
  match A.delete_user store
          ~user_id:"00000000-0000-0000-0000-00000000dead" with
  | Error e -> Alcotest.failf "delete unknown: %s" e
  | Ok n -> Alcotest.(check int) "no rows deleted" 0 n

let () =
  let open Alcotest in
  run "auth_store_set_user_type"
    [ ( "set_user_type"
      , [ test_case "human -> agent with parent sets type + parent" `Quick
            test_flip_to_agent_with_parent
        ; test_case "agent -> human clears parent" `Quick
            test_flip_back_to_human_clears_parent
        ; test_case "invalid user_type rejected" `Quick
            test_rejects_bad_type
        ; test_case "agent with missing parent rejected" `Quick
            test_rejects_missing_parent
        ; test_case "agent cannot parent another agent" `Quick
            test_agent_cannot_parent_agent
        ; test_case "unknown user rejected" `Quick
            test_unknown_user_rejected ] )
    ; ( "delete_user"
      , [ test_case "removes the user row" `Quick
            test_delete_user_removes_row
        ; test_case "cascades credentials" `Quick
            test_delete_user_cascades_credentials
        ; test_case "reparents child agents" `Quick
            test_delete_user_reparents_agent
        ; test_case "unknown id deletes zero rows" `Quick
            test_delete_unknown_user_zero ] ) ]
