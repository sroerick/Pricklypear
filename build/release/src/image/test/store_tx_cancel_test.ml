(* BORGE-1790987435 — transaction leak on the cancellation / disconnect
   path in Store_db.with_transaction.

   The 2026-10-02 prod lock pileup's leaked transaction was believed to
   come from this code path: a pgx transaction (BEGIN ... COMMIT/ROLLBACK)
   whose body was interrupted by Eio cancellation (client disconnect /
   nginx 499 / request teardown), skipping pgx's rollback and releasing
   the connection from the store mutex with the server-side transaction
   still open. The next request reuses the same pgx connection and
   inherits an open tx (begin_work then raises "cannot transact while in
   another transaction"; writes silently join the leaked tx).

   Current state (2026-10-02 loop): with_eio_conn_lock wraps the WHOLE
   pgx with_transaction in Eio.Cancel.protect. Eio's protect runs its
   body in a protected (non-cancellable) child context, so a parent
   cancellation is DEFERRED: the tx body runs to completion and pgx's
   catch-arm runs rollback before the cancellation can land. The leak is
   therefore already prevented by the protect boundary.

   This test PROVES that (it does not assume it):
     - test_cancel_mid_tx_leaves_store_clean cancels a fiber in the
       middle of a with_transaction body (the body signals "mid-tx" and
       blocks on a cancellable op, then the test cancels its context)
       and asserts the store is clean for reuse afterward: no store-managed
       tx, no server-side open tx (pg_stat_activity state, observed from
       a SEPARATE connection so the probe does not change the observed
       state), and a fresh with_transaction on the same connection opens,
       runs, and commits.
     - test_reuse_guard_rolls_back_stale_tx exercises the NEW defense-in-
       depth reuse guard (Store_db.rollback_stale, added in this fix):
       it forces the exact leak shape (a server-side tx opened via direct
       pgx begin_work, leaving pgx's in_transaction flag set while
       Store.current_tx is None) and asserts the next with_transaction
       ROLLS IT BACK, then opens a fresh tx, runs, and commits.

   Skips cleanly when no Postgres is reachable (scripts/dev.sh). *)

module S = Pricklypear_image_lib.Store
module IE = Pricklypear_image_lib.Io_eio

let main_id = "00000000-0000-0000-0000-000000000001"

(* Real-PG group (skips when unreachable) — same shape as
   store_reraise_test: try the env user first, then the dev.sh default
   (postgres, trust auth), and skip loudly when nothing connects. *)
let with_store ~database f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : IE.Ctx.t = IE.Ctx.of_stdenv ~sw env in
  IE.with_ctx ctx (fun () ->
      let base = { (S.config_from_env ()) with database } in
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
      match try_cfg users with
      | Some t -> f t
      | None ->
          Printf.printf
            "SKIP: no postgres reachable (set PP_DB_* / run scripts/dev.sh)\n%!";
          ())

(** A second store to the same database, used ONLY to observe the primary
    backend's pg_stat_activity.state. A same-connection probe cannot work:
    while it runs, the backend's state is 'active', masking 'idle in
    transaction'. (xact_start is no good either: it is set even for an
    autocommit statement's implicit transaction.) *)
let observer (t : S.t) : S.t =
  let cfg = S.config t in
  let o = S.make ~cfg ~branch_id:main_id () in
  S.connect o;
  o

(** The primary store's backend state as seen by [obs]:
    'idle' (no open tx), 'idle in transaction', 'active', etc. *)
let backend_state (t : S.t) (obs : S.t) : string =
  let pid =
    match S.conn_of t with
    | Some (pg, mu) -> (
        match S.exec (pg, mu) "SELECT pg_backend_pid()" with
        | [ [ p ] ] -> (
            match Pgx.Value.to_string p with
            | Some s -> s
            | None -> failwith "pg_backend_pid returned no string")
        | _ -> failwith "pg_backend_pid returned unexpected rows")
    | None -> failwith "primary store has no conn"
  in
  match S.conn_of obs with
  | Some (pg, mu) -> (
      match
        S.exec (pg, mu)
          ~params:[ Pgx.Value.of_string pid ]
          "SELECT state FROM pg_stat_activity WHERE pid = $1::int"
      with
      | [ [ s ] ] -> (
          match Pgx.Value.to_string s with
          | Some st -> st
          | None -> failwith "state returned no string")
      | _ -> failwith "state probe returned unexpected rows")
  | None -> failwith "observer has no conn"

(** Assert the primary store's backend is NOT inside a server-side
    transaction (state is 'idle'). *)
let assert_server_idle (t : S.t) (obs : S.t) ~what () =
  let st = backend_state t obs in
  Printf.printf "  [%s] backend state: %s\n%!" what st;
  Alcotest.(check bool) what true (String.equal st "idle")

(* ---- A. cancellation mid-transaction must not leak ---- *)

let test_cancel_mid_tx_leaves_store_clean () =
  with_store ~database:(S.config_from_env ()).database @@ fun t ->
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let obs = observer t in
  let mid_tx = Eio.Stream.create 1 in
  let resume = Eio.Stream.create 1 in
  let cancel_ctx = ref None in
  let outcome = ref "none" in
  let w =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Cancel.sub (fun ctx ->
            cancel_ctx := Some ctx;
            try
              S.with_transaction t (fun _ ->
                  (* real DB work: a genuine server BEGIN has happened *)
                  (match S.current_conn t with
                   | Some c ->
                       ignore
                         (S.exec c
                            "CREATE TEMP TABLE tx_cancel_probe(k text)");
                       ignore
                         (S.exec c
                            ~params:[ Pgx.Value.of_string "mid" ]
                            "INSERT INTO tx_cancel_probe(k) VALUES ($1)")
                   | None -> failwith "no conn inside with_transaction");
                  (* signal main: we are mid-transaction and blocked *)
                  Eio.Stream.add mid_tx "mid";
                  (* block on a cancellable op. If the tx body were NOT
                     protected by Eio.Cancel.protect (with_eio_conn_lock),
                     this is exactly where a cancellation would interrupt
                     the tx and — per the issue — risk skipping pgx's
                     rollback. With the protect, the cancellation is
                     deferred and the body runs to completion. *)
                  ignore (Eio.Stream.take resume));
              outcome := "completed"
            with
            | Eio.Cancel.Cancelled _ -> outcome := "cancelled"
            | e -> outcome := "exn:" ^ Printexc.to_string e))
  in
  (* main: wait until the worker is mid-transaction, then cancel it *)
  ignore (Eio.Stream.take mid_tx);
  Eio.Cancel.cancel (Option.get !cancel_ctx) (Failure "test-cancel-mid-tx");
  (* feed the resume stream so the (protected) body can complete *)
  Eio.Stream.add resume "go";
  (match Eio.Promise.await w with
   | Ok () -> ()
   | Error e -> outcome := "worker-error:" ^ Printexc.to_string e);
  Printf.printf "  [cancel] worker outcome: %s\n%!" !outcome;
  (* INVARIANT: nothing was left open on this store *)
  Alcotest.(check bool) "no store-managed tx left open" false
    (S.in_transaction t);
  assert_server_idle t obs ~what:"server-side tx closed (state idle)" ();
  (* REUSE: a fresh with_transaction on the SAME connection must open a
     clean tx, run, and commit. If a leaked tx remained, begin_work
     raises "cannot transact while in another transaction". *)
  let marker = "reuse-" ^ string_of_int (Unix.getpid ()) in
  S.with_transaction t (fun _ ->
      match S.current_conn t with
      | Some c ->
          ignore
            (S.exec c ~params:[ Pgx.Value.of_string marker ]
               "INSERT INTO tx_cancel_probe(k) VALUES ($1)")
      | None -> Alcotest.fail "no conn in reuse tx");
  assert_server_idle t obs ~what:"reuse tx committed (state idle)" ()

(* ---- B. the reuse guard rolls back a stale server-side tx ---- *)

let test_reuse_guard_rolls_back_stale_tx () =
  with_store ~database:(S.config_from_env ()).database @@ fun t ->
  Eio_main.run @@ fun _env ->
  let obs = observer t in
  (* Force the exact leak shape: a server-side tx opened OUTSIDE the
     store's bookkeeping (direct pgx begin_work), so pgx's in_transaction
     flag is set while Store.current_tx stays None — the released-with-
     open-tx state BORGE-1790987435 describes. The next with_transaction
     must ROLL IT BACK (Store_db.rollback_stale) before opening a fresh
     one; without the guard, begin_work raises "cannot transact while in
     another transaction" and the write joins the stale tx. *)
  let (pg, _mu) =
    match S.conn_of t with
    | Some c -> c
    | None -> Alcotest.fail "store has no conn"
  in
  ignore (IE.Pg.begin_work pg);
  (* before the guard runs, the backend is observably in a transaction *)
  let st_before = backend_state t obs in
  Printf.printf "  [guard] state after forced BEGIN: %s\n%!" st_before;
  Alcotest.(check bool) "stale tx is open before the guard runs" true
    (String.equal st_before "idle in transaction");
  let marker = "guard-" ^ string_of_int (Unix.getpid ()) in
  S.with_transaction t (fun _ ->
      match S.current_conn t with
      | Some c ->
          ignore (S.exec c "CREATE TEMP TABLE tx_guard_probe(k text)");
          ignore
            (S.exec c ~params:[ Pgx.Value.of_string marker ]
               "INSERT INTO tx_guard_probe(k) VALUES ($1)")
      | None -> Alcotest.fail "no conn in guarded tx");
  assert_server_idle t obs ~what:"stale tx rolled back (state idle)" ();
  Alcotest.(check bool) "no store-managed tx left open" false
    (S.in_transaction t)

let () =
  Alcotest.run "store tx cancellation / reuse (BORGE-1790987435)"
    [
      ( "real-pg",
        [
          Alcotest.test_case "cancel mid-tx leaves store clean" `Slow
            test_cancel_mid_tx_leaves_store_clean;
          Alcotest.test_case "reuse guard rolls back a stale tx" `Slow
            test_reuse_guard_rolls_back_stale_tx;
        ] );
    ]
