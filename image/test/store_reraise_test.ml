(* i-1788671060-2 — swallow-and-default retirement.

   store.ml (43 with-exn blocks) and auth_store.ml (27) each used to
   eprintf a DB failure and return a default: lookup_function -> None,
   count_functions_on_branch -> 0, get_user_by_username -> None,
   get_allow_agent_promote -> TRUE (fail-open). A DB outage, missing
   migration, or bad row therefore presented exactly like legitimately
   empty data — "unbound variable" for a function that visibly exists,
   count-0 branches, bad-credentials logins, silent admin re-bootstrap.
   The fix: catch-arms log-and-PROPAGATE via Store_db.reraise; the
   session/HTTP boundaries (/api/eval error JSON, HTTP 500 + incident,
   REPL Lwt.catch) already convert. Remaining swallows are genuine
   optional probes (visible_stamp stay-on-memory, log_auth, the
   last_used_at touch) or honest (Error msg) result contracts.

   Pinned here:
     - the helper: reraise propagates the ORIGINAL exception (callers
       can classify by SQLSTATE) and is usable as any type.
     - no-DB arms are unchanged: a Store.t with no connection still
       returns the "not configured" defaults (a distinct degraded mode,
       not an error to mask).
     - against a REAL Postgres, a store on the always-present `postgres`
       database (zero pricklypear tables — the "missing migration"
       shape) RAISES PostgreSQL_Error 42P01 from reads/counts/lookups
       instead of returning None/0/[]/true, while auth WRITE actions
       keep returning (Error msg) — the honest result contract.
     - visible_stamp keeps its probe semantics ("").
     - the unique-violation classifier matches a REAL 23505 (temp-table
       double insert) and rejects other classes (real 22012 divide-by-
       zero).

   Skips cleanly when no Postgres is reachable (scripts/dev.sh). *)

module S = Pricklypear_image_lib.Store
module A = Pricklypear_image_lib.Auth_store
module Branch = Pricklypear_common.Branch

let main_id = "00000000-0000-0000-0000-000000000001"

let exn_t : exn Alcotest.testable =
  Alcotest.testable
    (fun ppf e -> Format.pp_print_string ppf (Printexc.to_string e))
    (fun a b -> Printexc.to_string a = Printexc.to_string b)

(** PG error SQLSTATE, when the exception is a PostgreSQL_Error. *)
let pg_code (exn : exn) : string option =
  match exn with
  | Pgx.PostgreSQL_Error (_, fields) -> Some fields.Pgx.Error_response.code
  | _ -> None

(* ---- helper contract (no services) ---- *)

let test_reraise_propagates_original () =
  let raised : exn option =
    try
      ignore (S.reraise ~op:"unit-probe" (Failure "boom-42"));
      None
    with e -> Some e
  in
  Alcotest.(check (option exn_t)) "reraise re-raises the SAME exn"
    (Some (Failure "boom-42")) raised

let test_no_db_arms_unchanged () =
  let cfg = S.config_from_env () in
  let t = S.make ~cfg ~branch_id:main_id () in
  (* Never connected: the distinct "not configured" degraded mode. *)
  Alcotest.(check bool) "no-DB lookup_function is None" true
    (S.lookup_function t "nope" = None);
  Alcotest.(check int) "no-DB count_functions_on_branch is 0" 0
    (S.count_functions_on_branch t ~branch_id:main_id);
  Alcotest.(check bool) "no-DB list_branches is []" true (S.list_branches t = []);
  Alcotest.(check bool) "no-DB visible_stamp is \"\"" true
    (S.visible_stamp t = "");
  Alcotest.(check bool) "no-DB get_user_by_username is None" true
    (A.get_user_by_username t "nope" = None)

(* ---- real-PG group (skips when unreachable) ----

   The dev cluster (scripts/dev.sh) superuser is [postgres] with trust
   auth; a prod-shaped env uses PP_DB_USER. Try the env user first, then
   the dev.sh default — and SKIP EXPLICITLY (Alcotest.skip) when no
   connect succeeds, never a vacuous pass. *)

let with_store ~database f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let base = { (S.config_from_env ()) with database } in
      (* Drop any stale conn from a candidate before trying the next. *)
      let rec try_cfg = function
        | [] -> None
        | cfg :: rest ->
            let t = S.make ~cfg ~branch_id:main_id () in
            S.connect t;
            (match S.conn_of t with Some _ -> Some t | None -> try_cfg rest)
      in
      let users =
        if base.user = "postgres" then [ base ]
        else [ base; { base with user = "postgres"; password = "" } ]
      in
      (match try_cfg users with
       | Some t -> f t
       | None ->
           (* No Alcotest.skip in this version; the SKIP line is the loud
             marker (rls_floor precedent). A real-PG claim requires the
             tests to actually run — grep the output, never assume. *)
           Printf.printf "SKIP: no postgres reachable (set PP_DB_* / run scripts/dev.sh)\n%!";
           ()))

(* A missing-table (42P01) OR missing-migration-function (42883) DB —
   both are "the schema isn't there"; what matters is RAISE, not the
   exact SQLSTATE. *)
let expect_raises ~what (f : unit -> 'a) =
  match (try Ok (f ()) with e -> Error e) with
  | Error e -> (
      match pg_code e with
      | Some code when code = "42P01" || code = "42883" ->
          Alcotest.(check bool) what true true
      | Some code ->
          Alcotest.fail
            (Printf.sprintf "%s: expected 42P01/42883, got %s" what code)
      | None ->
          Alcotest.fail
            (Printf.sprintf "%s: expected a PG error, got %s" what
               (Printexc.to_string e)))
  | Ok _ ->
      Alcotest.fail
        (Printf.sprintf "%s: returned a default instead of raising" what)

let test_bare_database_raises () =
  with_store ~database:"postgres" @@ fun bare ->
  (* The "missing migration" shape: connected, but no pricklypear tables.
     Reads/counts/lookups must RAISE, never present as empty. *)
  expect_raises ~what:"count_functions_on_branch raises 42P01" (fun () ->
      S.count_functions_on_branch bare ~branch_id:main_id);
  expect_raises ~what:"lookup_function raises 42P01" (fun () ->
      S.lookup_function bare "whatever");
  expect_raises ~what:"list_branches raises 42P01" (fun () ->
      S.list_branches bare);
  expect_raises ~what:"current_generation raises 42P01" (fun () ->
      S.current_generation bare ~branch_id:main_id);
  expect_raises ~what:"auth get_user_by_username raises 42P01" (fun () ->
      A.get_user_by_username bare "admin");
  (* was FAIL-OPEN: a DB error used to default to true *)
  expect_raises ~what:"auth get_allow_agent_promote raises 42P01" (fun () ->
      A.get_allow_agent_promote bare
        ~user_id:"00000000-0000-0000-0000-000000000000");
  (* was 0 on error: an empty-looking user table re-triggers admin
     bootstrap with the default password *)
  expect_raises ~what:"auth count_users raises 42P01" (fun () ->
      A.count_users bare);
  (* The honest (Error msg) write contracts are KEPT — a failure is a
     result the admin surface can render, not a swallow. *)
  (match
     A.create_user bare ~username:"reraise-probe-x" ~is_admin:false
   with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "create_user on the bare DB should Error");
  (* The one deliberate read probe: stamp failure = stay-on-memory. *)
  Alcotest.(check string) "visible_stamp probe returns empty on failure" ""
    (S.visible_stamp bare)

let test_unique_violation_classifier () =
  with_store ~database:"postgres" @@ fun bare ->
  let conn = Option.get (S.conn_of bare) in
  ignore (S.exec conn "CREATE TEMP TABLE reraise_probe (x int PRIMARY KEY)");
  ignore (S.exec conn "INSERT INTO reraise_probe VALUES (1)");
  let dup =
    try
      ignore (S.exec conn "INSERT INTO reraise_probe VALUES (1)");
      None
    with e -> Some e
  in
  (match dup with
   | Some e ->
       Alcotest.(check bool) "23505 classifies as unique violation" true
         (S.is_unique_violation e)
   | None -> Alcotest.fail "duplicate insert should have raised");
  let div0 =
    try
      ignore (S.exec conn "SELECT 1/0");
      None
    with e -> Some e
  in
  (match div0 with
   | Some e ->
       Alcotest.(check bool) "22012 is NOT a unique violation" true
         (not (S.is_unique_violation e));
       Alcotest.(check bool) "22012 is NOT a serialization failure" true
         (not (S.is_serialization_failure e))
   | None -> Alcotest.fail "1/0 should have raised")

let test_real_database_still_returns_values () =
  (* Non-regression: on the REAL configured DB the happy path still
     returns values (the fix changed failure semantics only). *)
  with_store ~database:(S.config_from_env ()).database @@ fun t ->
  let n = S.count_functions_on_branch t ~branch_id:main_id in
  Alcotest.(check bool) "real DB count is a non-negative int" true (n >= 0);
  let stamp = S.visible_stamp t in
  Alcotest.(check bool) "real DB stamp is non-empty (tables exist)" true
    (stamp <> "");
  Alcotest.(check bool) "real DB list_branches includes system/main" true
    (List.exists (fun (b : Branch.t) -> b.Branch.id = main_id)
       (S.list_branches t))


(* ---- i-job-tx-wedge: nested transactions RIDE the ambient tx ---- *)

(* Inside an open transaction (a Lisp (with-transaction …) body, a job
   body, or an outer with_transaction) the crud prims used to open a
   SECOND BEGIN: pgx raised "begin_work: cannot transact while in
   another transaction", and the raised exception aborted the OUTER tx
   ("current transaction is aborted, commands ignored until end of
   transaction block") — every rows/find-rows/get under a
   with-transaction or job body wedged with an opaque error. The fix
   makes Store_db.with_transaction / with_dynamic_transaction JOIN the
   ambient transaction (Crud.with_tree_tx's old exclusive trick, now the
   shared rule). Pinned against a REAL pg:
     - a nested with_transaction sees the outer tx's UNCOMMITTED write
       (same snapshot — proof it joined, not opened a second tx),
     - a nested with_dynamic_transaction (the (with-transaction …) prim
       shape) rides too — it used to raise "nested transactions not
       supported" here,
     - the whole stack commits exactly once (marker visible after). *)
let probe_ride (t : S.t) : string option =
  match S.current_conn t with
  | Some c -> (
      match S.exec c "SELECT k FROM ride_probe" with
      | [ [ k ] ] -> Pgx.Value.to_string k
      | _ -> None)
  | None -> None

let test_nested_transactions_ride () =
  with_store ~database:(S.config_from_env ()).database @@ fun t ->
  let marker = "ride-" ^ string_of_int (Unix.getpid ()) in
  let inner_saw = ref None in
  let dyn_saw = ref None in
  S.with_transaction t (fun _ ->
      (match S.current_conn t with
       | Some c ->
           let _ = S.exec c "CREATE TEMP TABLE ride_probe(k text)" in
           let _ =
             S.exec c ~params:[ Pgx.Value.of_string marker ]
               "INSERT INTO ride_probe(k) VALUES ($1)"
           in
           ()
       | None -> Alcotest.fail "no ambient conn inside with_transaction");
      (* 1. nested with_transaction (the crud-prim shape) must ride. *)
      inner_saw := S.with_transaction t (fun _ -> probe_ride t);
      (* 2. nested with_dynamic_transaction (the (with-transaction …)
         prim shape) must ride — no "nested transactions not supported". *)
      dyn_saw :=
        S.with_dynamic_transaction t (fun () ->
            probe_ride t));
  Alcotest.(check (option string))
    "inner with_transaction saw the outer uncommitted write" (Some marker)
    !inner_saw;
  Alcotest.(check (option string))
    "nested with_dynamic_transaction rides (no raise)" (Some marker) !dyn_saw;
  (* 3. after the outer tx committed, the marker is visible — the ride
     did not swallow or double-commit the outer work. (current_tx is
     already None again: the finally cleared it inside the lock.) *)
  let after = probe_ride t in
  Alcotest.(check (option string))
    "outer tx committed exactly once (marker survives)" (Some marker) after

let () =
  Alcotest.run "store reraise policy (i-1788671060-2)"
    [
      ("helper+no-db",
       [
         Alcotest.test_case "reraise propagates original exn" `Quick
           test_reraise_propagates_original;
         Alcotest.test_case "no-DB arms unchanged" `Quick
           test_no_db_arms_unchanged;
       ]);
      ("real-pg",
       [
         Alcotest.test_case "bare database raises instead of empty defaults"
           `Slow test_bare_database_raises;
         Alcotest.test_case "unique-violation classifier" `Slow
           test_unique_violation_classifier;
         Alcotest.test_case "real DB happy path unchanged" `Slow
           test_real_database_still_returns_values;
         Alcotest.test_case "nested transactions ride the ambient tx" `Slow
           test_nested_transactions_ride;
       ]);
    ]
