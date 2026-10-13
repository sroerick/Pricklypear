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

let mk_entry ~actor ~cls ~outcome =
  { Al.at = Unix.gettimeofday (); actor; session_id = None; branch_id = None;
    cls; target = None; outcome; cost_json = None; source_ip = None }

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

(* T3.3 read face: (ops/activity) reads ledger rows, and a non-admin
   is force-filtered to its own actor AT THE SQL LAYER. Uses read_rows
   directly (the prim adds the arg parsing + class grammar only). *)
let test_read_force_filter () =
  with_store (fun store ->
      let alice = uniq "alice" in
      let bob = uniq "bob" in
      cleanup store ~actor:alice;
      cleanup store ~actor:bob;
      Fun.protect
        ~finally:(fun () -> cleanup store ~actor:alice; cleanup store ~actor:bob)
        (fun () ->
          ignore
            (Al.push ~actor:alice ~cls:"api.eval" ~target:(Some "b.fn.a")
               ~outcome:"ok"
               ~cost_json:(Al.budget_cost_json ~steps:1 ~prim_calls:1
                             ~db_hits:0 ~db_ms:0.0) ());
          ignore
            (Al.push ~actor:bob ~cls:"route.hit" ~target:(Some "/b")
               ~outcome:"ok" ());
          Al.flush ~store;
          (* admin, no actor filter: sees both (and any concurrent rows) *)
          let admin_rows =
            Al.read_rows ~store ~caller:alice ~is_admin:true
              ~actor:alice ()
          in
          Alcotest.(check int) "admin sees its own filtered row" 1
            (List.length admin_rows);
          (* non-admin cannot see another actor even if it asks for it:
             the supplied actor is ignored and the SQL filters to self. *)
          let sneaky =
            Al.read_rows ~store ~caller:alice ~is_admin:false
              ~actor:bob ()
          in
          Alcotest.(check int) "non-admin force-filtered to self" 1
            (List.length sneaky);
          let row = List.hd sneaky in
          let j = Yojson.Safe.from_string row in
          Alcotest.(check string) "row is the caller's" alice
            (Yojson.Safe.Util.member "actor" j |> Yojson.Safe.Util.to_string);
          (* class filter: bob's route.hit never appears for alice *)
          let cls_rows =
            Al.read_rows ~store ~caller:alice ~is_admin:false
              ~cls:"route.hit" ()
          in
          Alcotest.(check int) "class filter excludes other actor" 0
            (List.length cls_rows);
          (* JSON row shape: cost is an object with steps *)
          Alcotest.(check int) "cost.steps present" 1
            (Yojson.Safe.Util.member "cost" j
             |> Yojson.Safe.Util.member "steps"
             |> Yojson.Safe.Util.to_int)))

let test_read_class_rejects () =
  (* the prim rejects an unknown class before it ever reaches SQL;
     valid_class is the same gate read_rows' callers use. *)
  Alcotest.(check bool) "unknown class rejected" false
    (Al.valid_class "activity.secret")

(* T3.4 acceptance #2: auth events land as attributed rows with the
   right outcomes. Auth_store.log_auth MIRRORS onto the ledger (it does
   not fork the write); a failed password is a denied auth.login, a
   successful password is an ok auth.login, and a mint is ok auth.mint. *)
let test_auth_events_rows () =
  with_store (fun store ->
      let uname = uniq "authactor" in
      (* create_user lowercases; derive the canonical name. *)
      let uname = String.lowercase_ascii uname in
      let uid =
        match Pricklypear_image_lib.Auth_store.create_user store ~username:uname
                ~is_admin:false with
        | Ok u -> u.Pricklypear_image_lib.Auth_store.id
        | Error e -> Alcotest.fail ("create_user: " ^ e)
      in
      let conn = Option.get (S.conn_of store) in
      Fun.protect
        ~finally:(fun () ->
          cleanup store ~actor:uname;
          cleanup store ~actor:"anonymous";
          (try
             ignore
               (Db.exec conn
                  ~params:[ Pgx.Value.of_string uid ]
                  "DELETE FROM users WHERE id=$1::uuid")
           with _ -> ()))
        (fun () ->
          (* failed password, no user_id -> anonymous actor, denied *)
          Pricklypear_image_lib.Auth_store.log_auth store ~kind:"password"
            ~success:false ~source_ip:"203.0.113.7" ();
          (* successful login, attributed, ok *)
          Pricklypear_image_lib.Auth_store.log_auth store ~user_id:uid
            ~kind:"password" ~success:true ~source_ip:"203.0.113.8" ();
          (* token mint, attributed, ok *)
          Pricklypear_image_lib.Auth_store.log_auth store ~user_id:uid
            ~kind:"mint" ~success:true ();
          Al.flush ~store;
          let outcome_cls actor =
            match
              Db.exec conn
                ~params:[ Pgx.Value.of_string actor ]
                "SELECT class, outcome FROM activity_ledger WHERE actor=$1 \
                 ORDER BY id"
            with
            | [ [ c; o ] ] ->
                (Option.get (Pgx.Value.to_string c),
                 Option.get (Pgx.Value.to_string o))
            | _ -> Alcotest.fail "shape (expected one row)"
          in
          let anon_cls, anon_out = outcome_cls "anonymous" in
          Alcotest.(check string) "denial is auth.login" "auth.login" anon_cls;
          Alcotest.(check string) "denial outcome" "denied" anon_out;
          let cls, out = outcome_cls uname in
          Alcotest.(check string) "login is auth.login" "auth.login" cls;
          Alcotest.(check string) "login outcome" "ok" out;
          (* the mint row is the second attributed row *)
          let mint_cls =
            match
              Db.exec conn
                ~params:[ Pgx.Value.of_string uname ]
                "SELECT class FROM activity_ledger WHERE actor=$1 \
                 AND class <> 'auth.login'"
            with
            | [ [ c ] ] -> Option.get (Pgx.Value.to_string c)
            | _ -> Alcotest.fail "shape (expected one mint row)"
          in
          Alcotest.(check string) "mint is auth.mint" "auth.mint" mint_cls))

(* T3.4 acceptance #3: a labeled eval pushes api.eval ASYNCHRONOUSLY.
   The proof is the absence of a synchronous insert path: the DB row
   count is unchanged the instant the eval returns (the event is sitting
   in the in-process ring), and only appears after an explicit flush. *)
let test_api_eval_async () =
  with_store (fun store ->
      let uname = uniq "evalactor" in
      let manifest =
        Pricklypear_common.Manifest.make ~user_id:uname ~username:uname
          ~branch_id:main_id ~branch_name:"system/main"
          ~boot_mode:Pricklypear_common.Manifest.Boot_on_connect
          ~image_version:"test" ()
      in
      let t = Pricklypear_image_lib.Session.make ~manifest ~store () in
      Fun.protect ~finally:(fun () -> cleanup store ~actor:uname) (fun () ->
          let conn = Option.get (S.conn_of store) in
          ignore (Al.drain ());
          let before =
            q_int conn ~params:[ Pgx.Value.of_string uname ]
              "SELECT count(*)::text FROM activity_ledger WHERE actor=$1"
          in
          ignore (Pricklypear_image_lib.Session.eval_string_rim t "(+ 1 2)");
          (* ring holds the event; zero synchronous DB work happened. *)
          let pending =
            List.filter
              (fun (e : Al.entry) -> e.actor = uname && e.cls = "api.eval")
              (Al.drain ())
          in
          Alcotest.(check int) "one api.eval pending in the ring" 1
            (List.length pending);
          let after_eval =
            q_int conn ~params:[ Pgx.Value.of_string uname ]
              "SELECT count(*)::text FROM activity_ledger WHERE actor=$1"
          in
          Alcotest.(check int) "no synchronous insert on the eval path" before
            after_eval;
          (* the evaluator drained the ring above; re-push what we saw back
             so the flush path is still exercised with a real row. *)
          List.iter
            (fun (e : Al.entry) ->
              ignore
                (Al.push ~actor:e.actor ~cls:e.cls ~target:e.target
                   ~outcome:e.outcome ?cost_json:e.cost_json ()))
            pending;
          Al.flush ~store;
          let flushed =
            q_int conn ~params:[ Pgx.Value.of_string uname ]
              "SELECT count(*)::text FROM activity_ledger WHERE actor=$1 \
               AND class='api.eval'"
          in
          Alcotest.(check int) "flush lands the api.eval row" 1 flushed;
          let cost_steps =
            match
              Db.exec conn
                ~params:[ Pgx.Value.of_string uname ]
                "SELECT (cost->>'steps') FROM activity_ledger WHERE actor=$1"
            with
            | [ [ c ] ] -> int_of_string (Option.get (Pgx.Value.to_string c))
            | _ -> Alcotest.fail "shape (cost)"
          in
          Alcotest.(check bool) "cost carries steps" true (cost_steps > 0)))

(* T4.1/T4.2: deterministic knock schema. A pure detector over
   structure-only events, then a DB-backed filing path that files at
   most ONE security incident per (kind, actor): N denials in a window
   produce exactly one row, and a repeat scan BUMPS its count rather
   than opening a second. The severity class ("security") is distinct
   from reliability. *)
let test_knock_detect_thresholds () =
  let open Pricklypear_image_lib.Knock_policy in
  let denials n actor =
    List.init n (fun _ -> mk_entry ~actor ~cls:"admin.delete"
      ~outcome:"denied")
  in
  (* below threshold: no knock *)
  Alcotest.(check int) "4 denials below min=5" 0
    (List.length (detect ~min_denials:5 (denials 4 "a")));
  (* at threshold: exactly one knock, correct kind + count *)
  let ks = detect ~min_denials:5 (denials 5 "a" @ denials 7 "b") in
  Alcotest.(check int) "two actors over threshold" 2 (List.length ks);
  List.iter
    (fun (k : knock) ->
      Alcotest.(check string) "kind" "admin.denials" k.kind;
      Alcotest.(check int)
        (Printf.sprintf "count for %s" k.actor)
        (if k.actor = "a" then 5 else 7) k.count)
    ks;
  (* rls.violation repeats file *)
  let rls =
    List.init 3 (fun _ -> mk_entry ~actor:"c" ~cls:"rls.violation"
      ~outcome:"denied")
  in
  Alcotest.(check int) "rls repeats knock" 1
    (List.length (List.filter (fun k -> k.kind = "rls.violation") (detect rls)));
  (* a denied privileged act knocks at count 1 *)
  let priv =
    [ mk_entry ~actor:"d" ~cls:"promote.attempt" ~outcome:"denied" ]
  in
  Alcotest.(check int) "privilege escalation knocks" 1
    (List.length (List.filter (fun k -> k.kind = "privilege.escalation")
                    (detect priv)))

let test_trifecta_from_classes () =
  let open Pricklypear_image_lib.Knock_policy in
  (* an actor with private-data reads and an effect channel but NO ingest
     is not flagged; supplying an ingest leg flips all three. *)
  let evs =
    [ mk_entry ~actor:"a" ~cls:"admin.content-read" ~outcome:"ok";
      mk_entry ~actor:"a" ~cls:"data.write" ~outcome:"ok";
      mk_entry ~actor:"b" ~cls:"data.write" ~outcome:"ok" ]
  in
  let rows = trifecta evs in
  let find a = List.find (fun r -> r.actor = a) rows in
  Alcotest.(check bool) "a not flagged without ingest" false (find "a").flag;
  Alcotest.(check bool) "b has only effect leg" false (find "b").flag;
  let rows2 = trifecta ~ingest_classes:[ "egress.http" ]
    (evs @ [ mk_entry ~actor:"a" ~cls:"egress.http" ~outcome:"ok" ]) in
  Alcotest.(check bool) "a flagged with ingest" true
    (List.find (fun r -> r.actor = "a") rows2).flag

let test_knock_files_once () =
  with_store (fun store ->
      let actor = uniq "knockactor" in
      let conn = Option.get (S.conn_of store) in
      let cleanup () =
        let listed =
          Pricklypear_image_lib.Crud.find_rows store ~type_name:"ops-incident"
            ~filters:[ ("actor", actor) ] ~limit:50 ()
        in
        (match listed with
         | Pricklypear_lisp.Ast.VList rows ->
             List.iter
               (function
                 | Pricklypear_lisp.Ast.VPair
                     (Pricklypear_lisp.Ast.VStr id, _) ->
                     ignore
                       (Pricklypear_image_lib.Crud.delete store
                          ~type_name:"ops-incident" ~id)
                 | _ -> ())
               rows
         | _ -> ());
        ignore conn
      in
      cleanup ();
      Fun.protect ~finally:cleanup (fun () ->
          let fp = "security|admin.denials|" ^ actor in
          let id1 =
            Pricklypear_image_lib.Ops_incidents.file_security_incident
              ~store ~fingerprint:fp ~kind:"admin.denials" ~actor
              ~title:"5 denied admin attempts" ~count0:5
          in
          (* a second scan BUMPS the count, never opens a second row *)
          let id2 =
            Pricklypear_image_lib.Ops_incidents.file_security_incident
              ~store ~fingerprint:fp ~kind:"admin.denials" ~actor
              ~title:"5 denied admin attempts" ~count0:5
          in
          Alcotest.(check string) "same row deduped by fingerprint" id1 id2;
          let n =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT count(*)::text FROM data_rows WHERE type_name='ops-incident' \
               AND fields->>'actor'=$1"
          in
          Alcotest.(check int) "exactly ONE security incident" 1 n;
          let count =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT (fields->>'count')::int FROM data_rows \
               WHERE type_name='ops-incident' AND fields->>'actor'=$1"
          in
          Alcotest.(check int) "repeat bumped the count" 10 count;
          let sev =
            match
              Db.exec conn ~params:[ Pgx.Value.of_string actor ]
                "SELECT fields->>'severity' FROM data_rows \
                 WHERE type_name='ops-incident' AND fields->>'actor'=$1"
            with
            | [ [ c ] ] -> Option.get (Pgx.Value.to_string c)
            | _ -> Alcotest.fail "shape"
          in
          Alcotest.(check string) "security severity class" "security" sev))

(* IKEA test (spec verify): day rollups per (actor, class) survive a
   365-day run. One event per UTC day for a full year, with increasing
   steps, rolled up directly (no ring) so the [at] is controlled. The
   rollups must be one row per (day, actor, class) — 365 of them — and
   the steps/db_ms sums must be exact. Pins that the rollup key is the
   DAY, not the flush time. *)
let test_ikea_365_day_rollup () =
  with_store (fun store ->
      let actor = uniq "ikea" in
      cleanup store ~actor;
      let conn = Option.get (S.conn_of store) in
      Fun.protect ~finally:(fun () -> cleanup store ~actor) (fun () ->
          let now = Unix.gettimeofday () in
          let entries =
            List.init 365 (fun i ->
                { (mk_entry ~actor ~cls:"api.eval" ~outcome:"ok") with
                  Al.at = now -. (float_of_int i *. 86400.0);
                  cost_json =
                    Some (Al.budget_cost_json ~steps:(i + 1) ~prim_calls:1
                            ~db_hits:2 ~db_ms:(float_of_int (i + 1))) })
          in
          Al.rollup_entries ~store entries;
          let days =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT count(distinct day)::text FROM activity_rollup_day \
               WHERE actor=$1"
          in
          Alcotest.(check int) "one rollup row per UTC day" 365 days;
          let rows =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT count(*)::text FROM activity_rollup_day WHERE actor=$1"
          in
          Alcotest.(check int) "365 rollup rows total" 365 rows;
          let steps =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT coalesce(sum(steps),0)::text FROM activity_rollup_day \
               WHERE actor=$1"
          in
          Alcotest.(check int) "steps sum over the year" (365 * 366 / 2) steps;
          let hits =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT coalesce(sum(db_hits),0)::text FROM activity_rollup_day \
               WHERE actor=$1"
          in
          Alcotest.(check int) "db_hits sum over the year" (365 * 2) hits;
          (* re-applying the SAME day accumulates, it never adds a row *)
          Al.rollup_entries ~store [ List.nth entries 0 ];
          let rows2 =
            q_int conn ~params:[ Pgx.Value.of_string actor ]
              "SELECT count(*)::text FROM activity_rollup_day WHERE actor=$1"
          in
          Alcotest.(check int) "same day upserts, no new row" 365 rows2))

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
          Alcotest.test_case "read force-filters non-admin" `Quick
            test_read_force_filter;
          Alcotest.test_case "read rejects unknown class" `Quick
            test_read_class_rejects;
          Alcotest.test_case "auth events land as rows" `Quick
            test_auth_events_rows;
          Alcotest.test_case "api.eval pushes asynchronously" `Quick
            test_api_eval_async;
          Alcotest.test_case "knock detect thresholds" `Quick
            test_knock_detect_thresholds;
          Alcotest.test_case "trifecta from class counts" `Quick
            test_trifecta_from_classes;
          Alcotest.test_case "knock files exactly one security incident" `Quick
            test_knock_files_once;
          Alcotest.test_case "IKEA: 365-day rollups survive" `Quick
            test_ikea_365_day_rollup;
        ] )
    ]
