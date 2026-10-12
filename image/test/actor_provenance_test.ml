(* Actor provenance (migration 039; PP review card 55584f0a).

   FED3 needs two things pinned:

   1. The CRUD stamp surface has no inputs: Crud.session_actor () reads
      ONLY the fiber-bound Request_context snapshot (Pricklypear_common
      .Manifest.actor of the request manifest). A caller cannot name an
      actor from a request body, header, or eval payload.
   2. Manifest.actor has exactly one residence-based producer:
      Http_core.resolve_identity -> Auth_store.verify_delegated_token,
      which resolves the actor from the SERVER-MINTED credentials row
      (kind='delegated_token', public_data->>'actor') and DENIES unless
      that actor names an extant user. A caller-crafted token that was
      never minted, or a tampered row naming a nonexistent user,
      resolves to None — nothing can be claimed by the caller.

   Suite session_actor needs no DB. Suite actor_provenance_db needs the
   dev PG (scripts/dev.sh) and SKIPS cleanly when it is unreachable
   (auth_store_test precedent). *)

module C = Pricklypear_image_lib.Crud
module Rc = Pricklypear_image_lib.Request_context
module M = Pricklypear_common.Manifest
module S = Pricklypear_image_lib.Store
module A = Pricklypear_image_lib.Auth_store
module Hc = Pricklypear_image_lib.Http_core

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p = Printf.sprintf "apx-%s-%d" p (Random.int 1_000_000)

let () = Random.self_init ()

(* -- session_actor contract (no DB) -- *)

let mk_manifest ?(actor : string option = None) ?(username = "apx-user") () =
  M.make ~user_id:"apx-user-id" ~username ~branch_id:main_id
    ~branch_name:"apx-user/personal"
    ~boot_mode:M.Boot_on_connect ~image_version:"test" ~actor ()

let with_bound_ctx ~(actor : string option) (f : unit -> unit) : unit =
  Eio_main.run (fun _env ->
      let ctx =
        Rc.make ~is_admin:false ~manifest:(mk_manifest ~actor ())
          ~env:(Pricklypear_lisp.Env.create ())
          ~routes:(Hashtbl.create 4) ~source_ip:None
      in
      Rc.with_current ctx f)

let same_opt (a : string option) (b : string option) : bool = a = b

let check_actor ~expect f =
  match f () with
  | got when same_opt got expect -> ()
  | got ->
      let show = function
        | Some a -> "Some " ^ a
        | None -> "None"
      in
      Alcotest.failf "session_actor: expected %s, got %s" (show expect)
        (show got)

let test_no_context () =
  Eio_main.run (fun _env -> check_actor ~expect:None C.session_actor)

let test_owner_session () =
  with_bound_ctx ~actor:None (fun () ->
      check_actor ~expect:None C.session_actor)

let test_delegated_session () =
  with_bound_ctx ~actor:(Some "apx-actor") (fun () ->
      check_actor ~expect:(Some "apx-actor") C.session_actor)

let test_binding_released () =
  with_bound_ctx ~actor:(Some "apx-actor") (fun () ->
      check_actor ~expect:(Some "apx-actor") C.session_actor);
  Eio_main.run (fun _env -> check_actor ~expect:None C.session_actor)

(* -- provenance at the resolve seam (needs the dev PG) -- *)

let cleanup_all (store : S.t) : unit =
  match S.conn_of store with
  | None -> ()
  | Some conn -> (
      try
        let _ =
          S.exec conn ~params:[]
            "DELETE FROM credentials WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'apx-%')"
        in
        let _ =
          S.exec conn ~params:[] "DELETE FROM users WHERE username LIKE 'apx-%'"
        in
        ()
      with _ -> ())

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

let test_delegated_provenance (store : S.t) : unit =
  let owner_name = uniq "owner" and actor_name = uniq "actor" in
  let owner =
    match A.create_user store ~username:owner_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create owner: %s" e
  in
  let _actor_user =
    match A.create_user store ~username:actor_name ~is_admin:false with
    | Ok u -> u
    | Error e -> Alcotest.failf "create actor user: %s" e
  in
  (* mint: the only producer of an actor stamp *)
  let raw =
    match
      A.mint_delegated_token store ~owner_user_id:owner.A.id ~actor:actor_name ()
    with
    | Ok (raw, _) -> raw
    | Error e -> Alcotest.failf "mint delegated: %s" e
  in
  (* a minted token resolves to (owner, actor) *)
  (match A.verify_delegated_token store ~raw_token:raw () with
   | Some (u, a) ->
       Alcotest.(check string) "owner id" owner.A.id u.A.id;
       Alcotest.(check string) "actor" actor_name a
   | None -> Alcotest.fail "minted delegated token failed to resolve");
  let sess =
    Pricklypear_image_lib.Session.make
      ~manifest:(mk_manifest ~username:owner_name ())
      ~store ()
  in
  (* resolve_identity threads the actor out of the token verify *)
  (match
     Hc.resolve_identity sess
       ~headers:[ ("authorization", "Bearer " ^ raw) ]
       ()
   with
   | Some (_, a) ->
       Alcotest.(check (option string)) "resolve_identity actor"
         (Some actor_name) a
   | None -> Alcotest.fail "resolve_identity missed delegated token");
  (* NEGATIVE 1: a caller-crafted token that was never minted is IGNORED *)
  (match A.verify_delegated_token store ~raw_token:"apx-forged" () with
   | Some _ -> Alcotest.fail "caller-crafted token must not resolve"
   | None -> ());
  (match
     Hc.resolve_identity sess
       ~headers:[ ("authorization", "Bearer apx-forged") ]
       ()
   with
   | Some _ -> Alcotest.fail "forged bearer must not yield identity"
   | None -> ());
  (* NEGATIVE 2: an actor that names no extant user is DENIED, not stamped *)
  let raw2, cid =
    match
      A.mint_delegated_token store ~owner_user_id:owner.A.id ~actor:actor_name ()
    with
    | Ok (r, c) -> (r, c)
    | Error e -> Alcotest.failf "mint delegated 2: %s" e
  in
  (match S.conn_of store with
   | Some conn ->
       let _ =
         S.exec conn
           ~params:
             [ Pgx.Value.of_string cid; Pgx.Value.of_string "apx-satin" ]
           "UPDATE credentials SET public_data = jsonb_set(public_data, '{actor}', to_jsonb($2::text)) WHERE id = $1::uuid"
       in
       (match A.verify_delegated_token store ~raw_token:raw2 () with
        | Some _ -> Alcotest.fail "nonexistent-user actor must be denied"
        | None -> ())
   | None -> ())

let () =
  Alcotest.run "actor_provenance"
    [
      ( "session_actor",
        [ Alcotest.test_case "no request context binds no actor" `Quick
            test_no_context;
          Alcotest.test_case "owner session binds no actor" `Quick
            test_owner_session;
          Alcotest.test_case "delegated context binds its actor" `Quick
            test_delegated_session;
          Alcotest.test_case "actor binding is fiber-scoped" `Quick
            test_binding_released ] );
      ( "actor_provenance_db",
          [ Alcotest.test_case "server-set provenance only" `Slow
              (fun () -> with_store test_delegated_provenance) ] );
    ]