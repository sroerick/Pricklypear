(** product-experience.borg §activity-ledger READ (operator console) —
    the filmstrip strip's data plane.

    The console face itself is Nopales (libs/ops/lib.pp, route /ops); its
    kernel seam is [Presence] (live sessions) plus
    [Activity_ledger.rollup_rows] (per-identity spend). These tests pin
    the seam, not the HTML:

      1. [Presence.touch/snapshot] records an authenticated identity with
         its branch/admin/last-seen and DROPS anonymous/public/empty (a
         session row is an identity act, not a pageview);
      2. stale rows age out of [snapshot] (a crashed connection cannot
         linger in the operator's "on now");
      3. [Presence.forget] removes an identity (logout / revoke);
      4. [rollup_rows] returns per-actor sums from the forever-kept day
         rollups and is DB-backed (skips cleanly with no PG).

    No DB is needed for 1-3; 4 needs Postgres (scripts/dev.sh). *)

module S = Pricklypear_image_lib.Store
module Al = Pricklypear_image_lib.Activity_ledger
module P = Pricklypear_image_lib.Presence
module Db = Pricklypear_image_lib.Store_db

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p =
  Printf.sprintf "pres_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

let with_store f =
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
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let cleanup store ~actor =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      List.iter
        (fun sql ->
          try ignore (Db.exec conn ~params:[ Pgx.Value.of_string actor ] sql)
          with _ -> ())
        [ "DELETE FROM activity_ledger WHERE actor = $1";
          "DELETE FROM activity_rollup_day WHERE actor = $1" ]

(* ---- live presence (pure, in-process) ---- *)

let test_touch_records_and_excludes_anon () =
  P.clear ();
  P.touch ~username:"alice" ~user_id:"uid-alice" ~branch_id:"b1"
    ~branch_name:"alice/personal" ~is_admin:false ();
  P.touch ~username:"anonymous" ~user_id:"uid-anon" ~branch_id:"b2"
    ~branch_name:"system/main" ~is_admin:false ();
  P.touch ~username:"public" ~user_id:"uid-pub" ~branch_id:"b2"
    ~branch_name:"system/main" ~is_admin:false ();
  P.touch ~username:"" ~user_id:"uid-empty" ~branch_id:"b2"
    ~branch_name:"system/main" ~is_admin:false ();
  let rows = P.snapshot ~window:900.0 () in
  Alcotest.(check int) "only the authenticated identity is present" 1
    (List.length rows);
  let alice = List.hd rows in
  Alcotest.(check string) "username" "alice" alice.P.username;
  Alcotest.(check string) "branch" "alice/personal" alice.P.branch_name;
  Alcotest.(check bool) "not admin" false alice.P.is_admin;
  P.clear ()

let test_touch_updates_and_ages_out () =
  P.clear ();
  P.touch ~username:"bob" ~user_id:"uid-bob" ~branch_id:"b1"
    ~branch_name:"bob/personal" ~is_admin:true ();
  (* a second touch replaces (one row per identity), branch updated *)
  P.touch ~username:"bob" ~user_id:"uid-bob" ~branch_id:"b2"
    ~branch_name:"system/main" ~is_admin:true ();
  let rows = P.snapshot ~window:900.0 () in
  Alcotest.(check int) "one row per identity" 1 (List.length rows);
  Alcotest.(check string) "latest branch wins" "system/main"
    (List.hd rows).P.branch_name;
  (* a zero-second window ages the row out on the next snapshot *)
  let aged = P.snapshot ~window:0.0 () in
  Alcotest.(check int) "stale row aged out" 0 (List.length aged);
  P.clear ()

let test_forget_removes () =
  P.clear ();
  P.touch ~username:"carol" ~user_id:"uid-carol" ~branch_id:"b1"
    ~branch_name:"carol/personal" ~is_admin:false ();
  P.forget ~username:"carol" ();
  Alcotest.(check int) "forgotten identity gone" 0
    (List.length (P.snapshot ~window:900.0 ()));
  P.clear ()

(* ---- spend rollups (DB-backed read face) ---- *)

let test_rollup_rows () =
  with_store (fun store ->
      let actor = uniq "spend" in
      cleanup store ~actor;
      Fun.protect ~finally:(fun () -> cleanup store ~actor) (fun () ->
          ignore
            (Al.push ~actor ~cls:"api.eval" ~target:(Some "b.fn.x")
               ~outcome:"ok"
               ~cost_json:(Al.budget_cost_json ~steps:10 ~prim_calls:2
                             ~db_hits:3 ~db_ms:5.0) ());
          ignore
            (Al.push ~actor ~cls:"route.hit" ~target:(Some "/x")
               ~outcome:"ok" ());
          Al.flush ~store;
          let rows =
            Al.rollup_rows ~store ()
            |> List.filter_map (fun s ->
                   try Some (Yojson.Safe.from_string s) with _ -> None)
            |> List.filter (fun j ->
                   Yojson.Safe.Util.member "actor" j
                   |> Yojson.Safe.Util.to_string = actor)
          in
          Alcotest.(check int) "one rollup row for the actor" 1
            (List.length rows);
          let r = List.hd rows in
          let open Yojson.Safe.Util in
          Alcotest.(check int) "steps summed" 10 (member "steps" r |> to_int);
          Alcotest.(check int) "db_hits summed" 3 (member "db_hits" r |> to_int);
          Alcotest.(check int) "events counted" 2 (member "events" r |> to_int)))

let () =
  Alcotest.run "operator_console"
    [ ( "operator-console",
        [ Alcotest.test_case "presence records and excludes anon" `Quick
            test_touch_records_and_excludes_anon;
          Alcotest.test_case "presence updates and ages out" `Quick
            test_touch_updates_and_ages_out;
          Alcotest.test_case "presence forget removes" `Quick
            test_forget_removes;
          Alcotest.test_case "rollup rows sum per actor" `Quick
            test_rollup_rows;
        ] )
    ]
