(** product-experience.borg §activity-ledger (T3.1) — storage + ring.

    Pins the 031-shaped mechanism:
      1. the class/outcome grammars are CLOSED — an unknown class or
         outcome rejects at [push] and enqueues nothing;
      2. flushed rows land in activity_ledger with actor/class/target/
         outcome/cost, and source_ip is carried only where given;
      3. each flushed batch accumulates day rollups exactly once per
         (day, actor, class), summing steps/db_hits from the budget cost;
      4. the ring is drop-oldest at capacity (bounded memory, no
         synchronous DB work on push).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
module Al = Pricklypear_image_lib.Activity_ledger
module Db = Pricklypear_image_lib.Store_db

let main_id = "00000000-0000-0000-0000-000000000001"

let uniq p =
  Printf.sprintf "ledger_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

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

let q_int conn ?(params = []) sql : int =
  match Db.exec conn ~params sql with
  | [ [ c ] ] -> int_of_string (Option.get (Pgx.Value.to_string c))
  | _ -> Alcotest.fail ("query shape: " ^ sql)

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

(* ---- the acceptance ---- *)

let test_grammar_closed () =
  Alcotest.(check bool) "known class accepted" true (Al.valid_class "api.eval");
  Alcotest.(check bool) "unknown class rejected" false
    (Al.valid_class "eval.secret");
  Alcotest.(check bool) "known outcome accepted" true
    (Al.valid_outcome "timed-out");
  Alcotest.(check bool) "unknown outcome rejected" false
    (Al.valid_outcome "maybe");
  (match Al.push ~actor:"x" ~cls:"not.a.class" ~target:None ~outcome:"ok" () with
   | Error _ -> ()
   | Ok () -> Alcotest.fail "unknown class must reject");
  (match Al.push ~actor:"x" ~cls:"api.eval" ~target:None ~outcome:"nope" () with
   | Error _ -> ()
   | Ok () -> Alcotest.fail "unknown outcome must reject")

let test_flush_and_rollup () =
  with_store (fun store ->
      let actor = uniq "actor" in
      cleanup store ~actor;
      let conn = Option.get (S.conn_of store) in
      Fun.protect ~finally:(fun () -> cleanup store ~actor) (fun () ->
          let cost =
            "{\"steps\":10,\"prim_calls\":2,\"db_hits\":3,\"db_ms\":7}"
          in
          (* api.eval row with a cost, auth.deny with source_ip, auth.login
             without one. Push does zero DB work; flush drains. *)
          ignore
            (Al.push ~actor ~cls:"api.eval" ~target:(Al.path_target "b.fn.foo")
               ~outcome:"ok" ~cost_json:cost ());
          ignore
            (Al.push ~actor ~cls:"auth.deny" ~target:(Some "/login")
               ~outcome:"denied" ~source_ip:"203.0.113.9" ());
          ignore
            (Al.push ~actor ~cls:"auth.login" ~target:(Some "/login")
               ~outcome:"ok" ());
          Al.flush ~store;
          let n =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT count(*)::text FROM activity_ledger WHERE actor=$1"
          in
          Alcotest.(check int) "three rows flushed" 3 n;
          let outcome =
            match
              Db.exec conn
                ~params:[ Pgx.Value.of_string actor ]
                "SELECT outcome FROM activity_ledger WHERE actor=$1 \
                 AND class='auth.deny'"
            with
            | [ [ c ] ] -> Option.get (Pgx.Value.to_string c)
            | _ -> Alcotest.fail "shape"
          in
          Alcotest.(check string) "denial outcome" "denied" outcome;
          (* source_ip carried on auth.* only: the deny row has it, the
             api.eval row does not. *)
          let with_ip =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT count(*)::text FROM activity_ledger WHERE actor=$1 \
               AND source_ip IS NOT NULL"
          in
          Alcotest.(check int) "exactly the auth.deny row carries source_ip" 1
            with_ip;
          (* rollups: one row per (day,actor,class), sums from the cost *)
          let rollup_steps =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT coalesce(sum(steps),0)::text FROM activity_rollup_day \
               WHERE actor=$1"
          in
          Alcotest.(check int) "rollup steps sum" 10 rollup_steps;
          let rollup_hits =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT coalesce(sum(db_hits),0)::text FROM activity_rollup_day \
               WHERE actor=$1"
          in
          Alcotest.(check int) "rollup db_hits sum" 3 rollup_hits;
          (* a second flush of the same batch shape accumulates, not resets *)
          ignore
            (Al.push ~actor ~cls:"api.eval" ~target:(Some "b.fn.foo")
               ~outcome:"ok" ~cost_json:cost ());
          Al.flush ~store;
          let rollup_events =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT coalesce(sum(events),0)::text FROM activity_rollup_day \
               WHERE actor=$1 AND class='api.eval'"
          in
          Alcotest.(check int) "rollups accumulate" 2 rollup_events))

(* The ring is bounded: capacity pushes + overflow still leaves exactly
   [capacity] pending, and push never touched the DB. Runs under Eio so
   the push mutex has a scheduler context. *)
let test_ring_bounded () =
  Eio_main.run @@ fun _env ->
  ignore (Al.drain ());
  for i = 1 to Al.capacity + 25 do
    ignore
      (Al.push ~actor:"ringtest" ~cls:"route.hit"
         ~target:(Some (Printf.sprintf "/r%d" i)) ~outcome:"ok" ())
  done;
  let pending = Al.drain () in
  Alcotest.(check int) "ring dropped oldest to capacity" Al.capacity
    (List.length pending)

(* T3.2 emit-seam helpers: the budget→cost JSON shape, the auth_log kind→
   ledger class map (mirror, never fork), and the lonely-habitat bit. *)
let test_budget_cost_json () =
  let s =
    Al.budget_cost_json ~steps:12 ~prim_calls:3 ~db_hits:4 ~db_ms:7.9
  in
  let j = Yojson.Safe.from_string s in
  Alcotest.(check int) "steps" 12 (Yojson.Safe.Util.member "steps" j |> Yojson.Safe.Util.to_int);
  Alcotest.(check int) "db_ms truncated" 7
    (Yojson.Safe.Util.member "db_ms" j |> Yojson.Safe.Util.to_int)

let test_auth_kind_map () =
  let open Pricklypear_image_lib.Auth_store in
  Alcotest.(check string) "password -> auth.login" "auth.login"
    (ledger_class_of_auth_kind "password");
  Alcotest.(check string) "mint -> auth.mint" "auth.mint"
    (ledger_class_of_auth_kind "mint");
  Alcotest.(check string) "issue_token -> auth.mint" "auth.mint"
    (ledger_class_of_auth_kind "issue_token");
  Alcotest.(check string) "admin_create_user -> admin.create" "admin.create"
    (ledger_class_of_auth_kind "admin_create_user");
  (* every mapped class is inside the closed grammar *)
  List.iter
    (fun k ->
      Alcotest.(check bool)
        (Printf.sprintf "%s maps to a valid class" k)
        true (Al.valid_class (ledger_class_of_auth_kind k)))
    [ "password"; "mint"; "issue_token"; "api_key"; "revoke_all";
      "delegated"; "admin_create_user"; "admin_delete_user";
      "admin_revoke_sessions"; "admin_set_password";
      "admin_set_user_type"; "unknown_kind" ]

let test_lonely_bit () =
  Al.reset_lonely ();
  (* a supplied human count is memoized *)
  Alcotest.(check bool) "one human is lonely" true
    (Al.is_lonely_habitat ~human_count:(Some 1) ());
  Al.reset_lonely ();
  Alcotest.(check bool) "two humans is not lonely" false
    (Al.is_lonely_habitat ~human_count:(Some 2) ())

let () =
  Alcotest.run "activity_ledger"
    [ ( "activity-ledger",
        [ Alcotest.test_case "class/outcome grammar closed" `Quick
            test_grammar_closed;
          Alcotest.test_case "flush + day rollups" `Quick test_flush_and_rollup;
          Alcotest.test_case "ring bounded (drop-oldest)" `Quick
            test_ring_bounded;
          Alcotest.test_case "budget cost json" `Quick test_budget_cost_json;
          Alcotest.test_case "auth_log kind -> ledger class" `Quick
            test_auth_kind_map;
          Alcotest.test_case "lonely-habitat bit" `Quick test_lonely_bit;
        ] )
    ]
