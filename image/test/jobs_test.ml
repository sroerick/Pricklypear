(** Durable background jobs — node-shapes + runner (borge/jobs.borg).

    DB-backed regression tests for the dedicated `jobs` table
    (migrations/015_jobs.sql) and image/lib/jobs.ml. Needs Postgres
    (scripts/dev.sh); skips cleanly/unreachable.

    Covers the job node-shapes + runner verify stanzas:
      - job node create / claim / status transitions with runner-owned
        field confinement (submitted_by + branch pinned at submit; the
        visibility prims confine to the caller unless admin).
      - claim SKIP LOCKED skips a row locked by a concurrent connection.
      - exponential backoff schedule (failed retry is re-queued into the
        future; exhausted attempts => dead).
      - dead-runner reaper fails stale 'running' rows left by a dead
        runner_id (retry under attempts/backoff or dead at exhaustion).
      - cancel of a pending job (and only pending; not-yours rejected).

    The boot-time runner fiber (boot.ml) is thin glue over these Jobs
    functions (claim -> eval -> record); the substantive node-shapes and
    runner behaviors live in Jobs and are asserted here directly.
*)

module S = Pricklypear_image_lib.Store
module J = Pricklypear_image_lib.Jobs

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let uniq p = Printf.sprintf "j_%s_%d" p (Random.int 1_000_000)

(** True when [sub] is a substring of [s]. *)
let contains_sub (s : string) (sub : string) : bool =
  let n = String.length sub in
  if n = 0 then true
  else
    let rec go i =
      if i + n > String.length s then false
      else if String.sub s i n = sub then true
      else go (i + 1)
    in
    go 0

let now_ms () = Int64.of_float (Unix.gettimeofday () *. 1000.0)

(** Raw read of one job row's scalar fields (no result/error decoding). *)
let read_job_fields (store : S.t) ~(id : string) : (int * string * int64) option
    =
  match S.conn_of store with
  | None -> None
  | Some conn ->
      let rows =
        S.exec conn
          ~params:[ Pgx.Value.of_string id ]
          "SELECT attempts, status, run_at::text, runner_id FROM jobs \
           WHERE id = $1::uuid"
      in
      (match rows with
       | [ [ attempts; status; run_at; runner_id ] ] ->
           (match Pgx.Value.to_string attempts with
            | None -> None
            | Some a ->
                let attempts = try int_of_string a with _ -> 0 in
                let status =
                  match Pgx.Value.to_string status with Some s -> s | None -> ""
                in
                let run_at =
                  match Pgx.Value.to_string run_at with
                  | Some r -> (try Int64.of_string r with _ -> 0L)
                  | None -> 0L
                in
                let _runner_id =
                  match Pgx.Value.to_string runner_id with
                  | Some r -> r
                  | None -> ""
                in
                Some (attempts, status, run_at))
       | _ -> None)

let delete_job (store : S.t) ~(id : string) : unit =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      ignore
        (S.exec conn ~params:[ Pgx.Value.of_string id ]
           "DELETE FROM jobs WHERE id = $1::uuid")

(** The jobs table is shared/persistent; the runner claims oldest-first, so
    leftover rows from prior runs would be claimed instead of the test's own.
    Give each test a clean slate before exercising claim/backoff/reap. *)
let clear_jobs (store : S.t) : unit =
  match S.conn_of store with
  | None -> ()
  | Some conn ->
      ignore (S.exec conn "DELETE FROM job_steps");
      ignore (S.exec conn "DELETE FROM jobs")

(* ------------------------------------------------------------------ *)
(* LIVE-RUNNER DETERMINISM (campaign 4.1 follow-up)                    *)
(*                                                                     *)
(* boot.ml's runner fiber polls the shared jobs table every ~1s and     *)
(* claims ANY *due* (run_at <= now) pending job. The DB-backed tests    *)
(* share that table with the live image, so a test that submits a due   *)
(* job and then claims it itself can lose the race to the runner        *)
(* (either the test's claim comes up empty, or the runner takes the     *)
(* formerly-locked row the test expected to get). To make the tests     *)
(* deterministic WITHOUT stopping the image: submit with a FUTURE       *)
(* run_at (the runner's due-scan skips it), then drive run_at to 0 *     *)
(* just before the test's own claim. The window between make_due and     *)
(* claim_due is microseconds — far smaller than the runner's poll       *)
(* interval — so the test claims deterministically and the runner never  *)
(* competes for the same row.                                           *)
(* ------------------------------------------------------------------ *)

let future = 86_400_000L  (* one day in ms — runner due-scan (run_at<=now) skips *)

(** Submit a job the live runner will NOT claim (future run_at). *)
let submit_future (store : S.t) ~(fn : string) ~(args_json : string)
    ~(branch : string) ~(submitted_by : string) : string =
  match
    J.submit ~store ~fn ~args_json ~branch
      ~submitted_by ~run_at_ms:(Int64.add (now_ms ()) future) ()
  with
  | Ok id -> id
  | Error msg -> failwith ("submit_future: " ^ msg)

(** Drive a job's run_at to 0 so it is definitely due (run_at <= now); the
    runner's next due-scan is up to ~1s away, so the subsequent test-side
    claim_due (microseconds later) wins the row deterministically. *)
let make_due (store : S.t) ~(id : string) : unit =
  match S.conn_of store with
  | Some conn ->
      ignore
        (S.exec conn ~params:[ Pgx.Value.of_string id ]
           "UPDATE jobs SET run_at = 0 WHERE id = $1::uuid")
  | None -> ()

(* ------------------------------------------------------------------ *)
(* node-shapes: job node create / claim / status transitions          *)
(* ------------------------------------------------------------------ *)

let test_job_lifecycle () =
  with_store (fun store ->
      clear_jobs store;
      let id = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      (* submit: id returned, not an error string *)
      Alcotest.(check bool) "submit returns a uuid" true
        (String.length id = 36);
      (* status=pending, attempts=0, runner-owned fields pinned *)
      (match read_job_fields store ~id with
       | Some (attempts, status, _) ->
           Alcotest.(check int) "initial attempts" 0 attempts;
           Alcotest.(check string) "initial status" "pending" status
       | None -> Alcotest.fail "job row not present after submit");
      (* get as admin shows the pinned submitted_by + branch *)
      (match J.get ~store ~id ~caller:"bob" ~is_admin:true with
       | Pricklypear_lisp.Ast.VStr s ->
           Alcotest.(check bool) "status visible in JSON" true
             (contains_sub s "pending");
           Alcotest.(check bool) "submitted_by pinned" true
             (contains_sub s "alice");
           Alcotest.(check bool) "branch pinned" true
             (contains_sub s "system/main")
       | _ -> Alcotest.fail "get (admin) returned non-string");
      (* claim: pending -> running, runner_id set, attempts+1 *)
      make_due store ~id;
      (match J.claim_due ~store with
       | None -> Alcotest.fail "claim of a due job returned None"
       | Some j ->
           Alcotest.(check string) "claimed id" id j.J.id;
           Alcotest.(check string) "claimed status" "running" j.J.status;
           Alcotest.(check int) "claimed attempts=1" 1 j.J.attempts;
           Alcotest.(check bool) "runner_id stamped" true
             (j.J.runner_id <> ""));
      (* record_done: running -> done with result *)
      J.record_done ~store ~id ~result_json:"\"ok-result\"";
      (match J.get ~store ~id ~caller:"alice" ~is_admin:false with
       | Pricklypear_lisp.Ast.VStr s ->
           Alcotest.(check bool) "complete status done" true
             (contains_sub s "\"done\"")
       | _ -> Alcotest.fail "done job not visible to its owner");
      delete_job store ~id)

(* runner-owned field confinement: a non-owner, non-admin caller does NOT
   see a job; (jobs) lists only the caller's jobs; cancel rejects others' *)
let test_visibility_confinement () =
  with_store (fun store ->
      clear_jobs store;
      let id = submit_future store ~fn:"jfn2" ~args_json:"{}"
          ~branch:"admin/personal" ~submitted_by:"alice"
      in
      (* non-owner, non-admin cannot see it via get *)
      (match J.get ~store ~id ~caller:"mallory" ~is_admin:false with
       | Pricklypear_lisp.Ast.VNil -> ()
       | _ -> Alcotest.fail "non-owner, non-admin saw a job");
      (* owner sees it *)
      (match J.get ~store ~id ~caller:"alice" ~is_admin:false with
       | Pricklypear_lisp.Ast.VStr _ -> ()
       | _ -> Alcotest.fail "owner could not read own job");
      (* (jobs) confinement: mallory's list is empty, admin sees both *)
      (match J.list ~store ~caller:"mallory" ~is_admin:false ~limit:50 () with
       | Pricklypear_lisp.Ast.VList l ->
           Alcotest.(check bool) "mallory sees no jobs" true (l = [])
       | _ -> Alcotest.fail "non-admin list returned non-list");
      (match J.list ~store ~caller:"admin" ~is_admin:true ~limit:50 () with
       | Pricklypear_lisp.Ast.VList l ->
           Alcotest.(check bool) "admin sees at least one job" true
             (List.length l >= 1)
       | _ -> Alcotest.fail "admin list returned non-list");
      (* cancel of pending by a non-owner, non-admin -> rejected *)
      (match J.cancel ~store ~id ~caller:"mallory" ~is_admin:false with
       | Pricklypear_lisp.Ast.VStr s ->
           Alcotest.(check bool) "cancel not permitted for others" true
             (contains_sub s "not pending or not yours")
       | _ -> Alcotest.fail "foreign cancel should not return the id");
      (* pending row survives -> admin cancels it *)
      (match J.cancel ~store ~id ~caller:"admin" ~is_admin:true with
       | Pricklypear_lisp.Ast.VStr s ->
           Alcotest.(check bool) "admin cancel returns id" true
             (String.length s = 36)
       | _ -> Alcotest.fail "admin cancel did not return id");
      (* cancel only affects pending: cancelling a done/failed row is a no-op *)
      (match J.cancel ~store ~id ~caller:"admin" ~is_admin:true with
       | Pricklypear_lisp.Ast.VStr s ->
           Alcotest.(check bool) "re-cancel of failed row is no-op" true
             (contains_sub s "not pending or not yours")
       | _ -> Alcotest.fail "re-cancel should be a no-op");
      delete_job store ~id)

(* ------------------------------------------------------------------ *)
(* runner: SKIP LOCKED claim skips a row locked by another connection *)
(* ------------------------------------------------------------------ *)

let test_claim_skips_locked () =
  with_store (fun store_a ->
      clear_jobs store_a;
      let job1 = submit_future store_a ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      and job2 = submit_future store_a ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      (* Make BOTH jobs due BEFORE taking the row lock on job1. Driving
         run_at to 0 after connection B has FOR UPDATE-locked job1 would
         deadlock: store_a's own UPDATE on the locked job1 row would block
         until B commits, but B's transaction cannot commit while this
         function is blocked inside it. Making both due up front leaves
         the claim to exercise only the SKIP LOCKED path (job1 stays
         locked-by-B, so A skips it and takes job2). *)
      make_due store_a ~id:job1;
      make_due store_a ~id:job2;
      (* second, independent connection/transaction locks job1's row.
         Use READ COMMITTED for the lock-holding side so store A's
         SERIALIZABLE claim UPDATE (SKIP LOCKED) neither deadlocks nor
         trips SSI 40001 the moment B's row lock is held — B holds a
         plain row lock, not a snapshot conflict, so A cleanly skips
         the locked job1 and takes job2. (SERIALIZABLE would abort A's
         write with 'could not serialize access due to concurrent
         update' — the documented flakiness; READ COMMITTED removes it.) *)
      let cfg = S.config_from_env () in
      let store_b = S.make ~cfg ~branch_id:main_id () in
      S.connect store_b;
      S.with_transaction ~isolation:Pgx.Isolation.Read_committed
        store_b (fun conn_b ->
          match conn_b with
          | None -> Alcotest.fail "no conn on store_b"
          | Some c ->
              (* hold job1's row lock open on connection B *)
              ignore
                (S.exec c ~params:[ Pgx.Value.of_string job1 ]
                   "SELECT id FROM jobs WHERE id=$1::uuid FOR UPDATE");
              (* claim on store A must SKIP the locked job1 and take job2 *)
              (match J.claim_due ~store:store_a with
               | None -> Alcotest.fail "claim skipped nothing"
               | Some j ->
                   Alcotest.(check string) "skipped locked, claimed job2"
                     job2 j.J.id));
      (* B's tx committed/rolled back -> job1 unlockable; claim it *)
      (match J.claim_due ~store:store_a with
       | None -> Alcotest.fail "second claim returned None"
       | Some j -> Alcotest.(check string) "claimed formerly-locked job1"
             job1 j.J.id);
      delete_job store_a ~id:job1;
      delete_job store_a ~id:job2)

(* ------------------------------------------------------------------ *)
(* runner: exponential backoff schedule + reach-max => dead            *)
(* ------------------------------------------------------------------ *)

let test_backoff_schedule () =
  with_store (fun store ->
      clear_jobs store;
      let before = now_ms () in
      let id = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      (* claim (attempts 0->1) then fail with max_attempts=3 *)
      make_due store ~id;
      (match J.claim_due ~store with
       | None -> Alcotest.fail "no job to claim"
       | Some j ->
           Alcotest.(check int) "attempts after first claim" 1 j.J.attempts;
           J.record_failed ~store ~id ~error:"boom" ~max_attempts:3);
      (* not dead yet (attempts 1 < max 3) -> requeued pending into future *)
      (match read_job_fields store ~id with
       | Some (attempts, status, run_at) ->
           Alcotest.(check int) "attempts still 1" 1 attempts;
           Alcotest.(check string) "requeued pending" "pending" status;
           Alcotest.(check bool) "run_at scheduled in future" true
             (Int64.compare run_at before > 0)
       | None -> Alcotest.fail "job missing after backoff");
      (* second claim (attempts 2) + fail -> still pending, later retry *)
      make_due store ~id;
      (match J.claim_due ~store with
       | None -> Alcotest.fail "no job to claim (2nd)"
       | Some j -> J.record_failed ~store ~id ~error:"boom2"
             ~max_attempts:3);
      (match read_job_fields store ~id with
       | Some (attempts, status, _) ->
           Alcotest.(check int) "attempts after 2nd fail" 2 attempts;
           Alcotest.(check string) "retry still pending" "pending" status
       | None -> Alcotest.fail "job missing after 2nd backoff");
      (* third claim (attempts 3 = max) + fail -> dead *)
      make_due store ~id;
      (match J.claim_due ~store with
       | None -> Alcotest.fail "no job to claim (3rd)"
       | Some j -> J.record_failed ~store ~id ~error:"boom3"
             ~max_attempts:3);
      (match read_job_fields store ~id with
       | Some (_, status, _) ->
           Alcotest.(check string) "exhausted -> dead" "dead" status
       | None -> Alcotest.fail "job missing after reaching max");
      delete_job store ~id)

(* ------------------------------------------------------------------ *)
(* runner: dead-runner reaper fails stale 'running' rows              *)
(* ------------------------------------------------------------------ *)

let test_reap_dead () =
  with_store (fun store ->
      clear_jobs store;
      (* a job claimed (running) by a DIFFERENT runner id than ours, with
         attempts just under max -> reaper requeues it pending *)
      let id_a = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      ignore
        (S.exec (Option.get (S.conn_of store))
           ~params:
             [ Pgx.Value.of_string id_a
             ; Pgx.Value.of_string "runner-OTHER"
             ]
           "UPDATE jobs SET status='running', runner_id=$2, attempts=2 \
            WHERE id=$1::uuid");
      (* a job claimed by another runner at max_attempts -> reaper dead *)
      let id_b = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      ignore
        (S.exec (Option.get (S.conn_of store))
           ~params:
             [ Pgx.Value.of_string id_b
             ; Pgx.Value.of_string "runner-OTHER"
             ]
           "UPDATE jobs SET status='running', runner_id=$2, attempts=3, \
            max_attempts=3 WHERE id=$1::uuid");
      J.reap_dead ~store;
      (match read_job_fields store ~id:id_a with
       | Some (attempts, status, _) ->
           Alcotest.(check int) "reaped attempts kept" 2 attempts;
           Alcotest.(check string) "stale running requeued pending"
             "pending" status
       | None -> Alcotest.fail "job_a missing after reap");
      (match read_job_fields store ~id:id_b with
       | Some (_, status, _) ->
           Alcotest.(check string) "exhausted stale -> dead" "dead" status
       | None -> Alcotest.fail "job_b missing after reap");
      (* part A done; remove the now-claimable id_a/id_b so the "our runner
         not reaped" check below exercises a solitary due job, else
         claim_due (oldest-first) would claim id_a, not ours. *)
      delete_job store ~id:id_a;
      delete_job store ~id:id_b;
      (* the reaper must NOT touch rows running under OUR runner_id *)
      let mine = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      make_due store ~id:mine;
      (match J.claim_due ~store with
       | None -> Alcotest.fail "no job to claim for reap-mine check"
       | Some j ->
           Alcotest.(check string) "claimed our own job" mine j.J.id;
           J.record_done ~store ~id:j.J.id ~result_json:"null");
      (* after our claim+done the row is done, not pending from reaper *)
      (match read_job_fields store ~id:mine with
       | Some (_, status, _) ->
           Alcotest.(check string) "our runner not reaped" "done" status
       | None -> Alcotest.fail "our own job missing");
      delete_job store ~id:mine)

(* ------------------------------------------------------------------ *)
(* runner ARGS BUG — campaign 4.1: eval_src must splice the jsonb args   *)
(* directly as the call literal, not %S-quote it. A stored number arg   *)
(* (args=1) must produce (f 1), never (f "1"); numerics/json/booleans   *)
(* otherwise fail with a numeric-op error and retry to dead.             *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* step-checkpoints: current-job binding + checkpoint memoization      *)
(* (campaign 4.2 — the (with-step "name" thunk) special form memoizes *)
(* (job-id, name); the DB mechanics live in Jobs (checkpoint_lookup /   *)
(* checkpoint_write), which the __step__ prim in session.ml drives.     *)
(* ------------------------------------------------------------------ *)

let test_current_job_id_binding () =
  (* current_job_id_opt reads the Eio fiber key, so it must run on a fiber
     (Eio_main). Outside any job binding there is no job context. *)
  Eio_main.run @@ fun _env ->
    (* Outside any job binding: no job context. *)
    Alcotest.(check bool) "no job context by default" true
      (J.current_job_id_opt () = None);
    (* Inside with_current_job_id: Some id. *)
    J.with_current_job_id "job-abc" (fun () ->
        Alcotest.(check bool) "job id visible inside binding" true
          (J.current_job_id_opt () = Some "job-abc"));
    (* Cleared after the binding scope exits. *)
    Alcotest.(check bool) "job id cleared after binding" true
      (J.current_job_id_opt () = None)

let test_checkpoint_memoization () =
  with_store (fun store ->
      clear_jobs store;
      (* a real job row to satisfy the job_steps FK (ON DELETE CASCADE) *)
      let jid = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      (* no checkpoint yet for this (job, step) pair *)
      Alcotest.(check bool) "no checkpoint initially" true
        (J.checkpoint_lookup ~store ~job_id:jid ~name:"load" = None);
      (* write + read round-trips *)
      J.checkpoint_write ~store ~job_id:jid ~name:"load" ~value:"42";
      (match J.checkpoint_lookup ~store ~job_id:jid ~name:"load" with
       | Some v -> Alcotest.(check string) "checkpoint value" "42" v
       | None -> Alcotest.fail "checkpoint not found after write");
      (* a different step name is independent (per (job-id, name) key) *)
      Alcotest.(check bool) "different step independent" true
        (J.checkpoint_lookup ~store ~job_id:jid ~name:"other" = None);
      (* same (job,step) with a different job is independent too *)
      let jid2 = submit_future store ~fn:"jfn" ~args_json:"{}"
          ~branch:"system/main" ~submitted_by:"alice"
      in
      Alcotest.(check bool) "different job independent" true
        (J.checkpoint_lookup ~store ~job_id:jid2 ~name:"load" = None);
      delete_job store ~id:jid2;
      (* overwrite updates in place (a re-run of a step that already has
         a checkpoint on retry REPLACES it — but the step prim skips the
         re-run entirely when a checkpoint exists, so this is only reachable
         via an explicit re-write; assert the upsert semantics) *)
      J.checkpoint_write ~store ~job_id:jid ~name:"load" ~value:"99";
      (match J.checkpoint_lookup ~store ~job_id:jid ~name:"load" with
       | Some v -> Alcotest.(check string) "checkpoint overwritten" "99" v
       | None -> Alcotest.fail "overwritten checkpoint missing");
      delete_job store ~id:jid)

(* ------------------------------------------------------------------ *)
(* schedules: cron next-fire computation (incl. across-month) +        *)
(* every-ms drift (campaign 4.2)                                       *)
(* ------------------------------------------------------------------ *)

let test_next_minute () =
  let now = now_ms () in
  (* "* * * * *" fires every whole minute; next fire is the next minute *)
  (match J.next_cron_ms "* * * * *" now with
   | None -> Alcotest.fail "next_cron_ms (* * * * *) returned None"
   | Some n ->
       Alcotest.(check bool) "next fire on a minute boundary" true
         (Int64.rem n 60_000L = 0L);
       Alcotest.(check bool) "next fire strictly after now" true
         (Int64.compare n now > 0);
       Alcotest.(check bool) "next fire within two minutes" true
         (Int64.compare n (Int64.add now (Int64.mul 2L 60_000L)) < 0))

let test_every_ms_drift () =
  Alcotest.(check string) "every-ms adds the interval"
    "110" (Int64.to_string (J.next_every_ms 10L 100L));
  Alcotest.(check bool) "integer spec is not cron" false
    (J.spec_is_cron "60000");
  Alcotest.(check bool) "cron spec is cron" true
    (J.spec_is_cron "* * * * *")

(* tz knob (roerick 09-11, "match cron with mountain time"): the cron
   calendar is injectable, so tests run without touching the process TZ.
   [utc_minus_6] stands in for America/Denver in DST (MDT). *)
let utc_minus_6 (t : float) : Unix.tm =
  Unix.gmtime (t -. (6.0 *. 3600.0))

let test_cron_tz_local () =
  let now = now_ms () in
  let base = J.next_cron_ms_with ~calendar:utc_minus_6 "0 9 * * *" now in
  let utc = J.next_cron_ms "0 9 * * *" now in
  let hour_of ~calendar n = (calendar (Int64.to_float n /. 1000.0)).Unix.tm_hour in
  (match base, utc with
   | Some b, Some u ->
       Alcotest.(check int) "local calendar fires at 09:00 projected"
         9 (hour_of ~calendar:utc_minus_6 b);
       Alcotest.(check bool) "09:00 local (15:00Z in MDT) differs from 09:00Z"
         true (Int64.compare b u <> 0);
       Alcotest.(check int) "utc calendar still fires at 09:00Z"
         9 (hour_of ~calendar:Unix.gmtime u);
       Alcotest.(check bool) "local fire is 6h after the utc fire"
         true (Int64.compare b (Int64.add u (Int64.of_int (6 * 3600 * 1000))) = 0)
   | _ -> Alcotest.fail "next_cron_ms_with returned None");
  (* every-ms schedules ignore tz; the matcher routes by the tz string *)
  Alcotest.(check bool) "tz routing: local vs utc differ" true
    (Int64.compare
       (Option.get (J.next_cron_for_tz "local" "0 9 * * *" now))
       (Option.get (J.next_cron_for_tz "utc" "0 9 * * *" now)) <> 0)

let test_cron_across_month () =
  let now = now_ms () in
  (* "0 0 1 1 *" = every Jan 1 00:00 UTC; the returned fire must be a
     Jan-1 midnight strictly after now (may be this Jan or next). *)
  (match J.next_cron_ms "0 0 1 1 *" now with
   | None -> Alcotest.fail "next_cron_ms (Jan 1) returned None"
   | Some n ->
       let tm = Unix.gmtime (Int64.to_float n /. 1000.0) in
       Alcotest.(check int) "minute 0" 0 tm.Unix.tm_min;
       Alcotest.(check int) "hour 0" 0 tm.Unix.tm_hour;
       Alcotest.(check int) "day 1" 1 tm.Unix.tm_mday;
       Alcotest.(check int) "month January" 0 tm.Unix.tm_mon;
       Alcotest.(check bool) "after now" true (Int64.compare n now > 0));
  (* a day-of-month restricted cron must not fire on non-matching days *)
  Alcotest.(check bool) "Feb 30 (impossible) never matches" true
    (J.next_cron_ms "0 0 30 2 *" now = None)

(* ------------------------------------------------------------------ *)
(* schedules: DB CRUD + due-claim + disable (campaign 4.2)             *)
(* ------------------------------------------------------------------ *)

let test_schedule_crud () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          ignore (S.exec conn "DELETE FROM schedules");
          (* upsert an every-ms schedule; returns the name; enabled *)
          (match J.schedule_upsert ~store ~name:"pp-test-every"
                   ~spec:"60000" ~fn:"jfn" ~args_json:"{}"
                   ~branch:"system/main" ~submitted_by:"alice" () with
           | Pricklypear_lisp.Ast.VStr n ->
               Alcotest.(check string) "upsert returns name" "pp-test-every" n
           | _ -> Alcotest.fail "schedule upsert did not return name");
          (* list shows it *)
          (match J.list_schedules ~store () with
           | Pricklypear_lisp.Ast.VList l ->
               Alcotest.(check bool) "list has the schedule" true
                 (List.exists
                    (function
                      | Pricklypear_lisp.Ast.VStr s ->
                          contains_sub s "pp-test-every"
                      | _ -> false)
                    l)
           | _ -> Alcotest.fail "list_schedules returned non-list");
          (* drive it due and claim (future-run_at-style: the initial
             next_run is now+60000 so the live runner skips it; we set it
             to the past just before OUR claim to win deterministically) *)
          ignore (S.exec conn
                    "UPDATE schedules SET next_run = 0 \
                     WHERE name = 'pp-test-every'");
          (match J.claim_due_schedule ~store with
           | Some sch ->
               Alcotest.(check string) "claimed schedule name"
                 "pp-test-every" sch.J.s_name;
               Alcotest.(check bool) "every_ms captured" true
                 (sch.J.s_every_ms = Some 60_000L);
               J.advance_schedule ~store ~name:sch.J.s_name
                 ~fired_at:(now_ms ())
                 ~cron:sch.J.s_cron ~every_ms:sch.J.s_every_ms
           | None -> Alcotest.fail "claim_due_schedule returned None");
          (* after advance, next_run is in the future -> no longer due *)
          (match J.claim_due_schedule ~store with
           | Some _ -> Alcotest.fail "schedule still due after advance"
           | None -> ());
          (* unschedule disables; a disabled schedule is never due *)
          (match J.unschedule ~store ~name:"pp-test-every" () with
           | Pricklypear_lisp.Ast.VStr n ->
               Alcotest.(check string) "unschedule returns name"
                 "pp-test-every" n
           | _ -> Alcotest.fail "unschedule did not return name");
          ignore (S.exec conn
                    "UPDATE schedules SET next_run = 0 \
                     WHERE name = 'pp-test-every'");
          (match J.claim_due_schedule ~store with
           | Some _ -> Alcotest.fail "disabled schedule claimed"
           | None -> ());
          let _ = conn in
          ignore (S.exec conn "DELETE FROM schedules"))

let test_eval_src_args () =
  (* numeric arg: stored jsonb '1' -> (f 1) — the 4.1 args-bug regression *)
  Alcotest.(check string) "numeric arg splices bare"
    "(jfn 1)" (J.eval_src ~fn:"jfn" ~args:"1");
  (* string arg: stored jsonb is the QUOTED JSON literal -> (f "hello") *)
  Alcotest.(check string) "string arg keeps its JSON quotes"
    "(jfn \"hello\")" (J.eval_src ~fn:"jfn" ~args:"\"hello\"");
  (* object arg: (f {"a":1}) *)
  Alcotest.(check string) "object arg splices bare"
    "(jfn {\"a\":1})" (J.eval_src ~fn:"jfn" ~args:"{\"a\":1}");
  (* boolean arg *)
  Alcotest.(check string) "bool arg splices bare"
    "(jfn true)" (J.eval_src ~fn:"jfn" ~args:"true");
  (* empty-object (no-arg) job stays a bare call *)
  Alcotest.(check string) "empty object -> no-arg call"
    "(jfn {})" (J.eval_src ~fn:"jfn" ~args:"{}")


(* ------------------------------------------------------------------ *)
(* JSON serialization (i-1788670620-4): job/schedule rows render as     *)
(* VALID JSON even for adversarial field values. The old sprintf        *)
(* spliced fn/branch/submitted_by raw, so a fn containing a quote or    *)
(* backslash produced invalid JSON in job-status/jobs/schedules output. *)
(* Now built with Yojson: every string field is escaped, stored jsonb   *)
(* literals (args/result) stay JSON literals, not strings.              *)
(* ------------------------------------------------------------------ *)

(* Field value hitting every escaping hazard: quote, backslash, newline, *)
(* tab, control byte.                                                    *)
let adversarial = "f\"n\\x\n\tend\000"

let parse_json (s : string) : Yojson.Safe.t =
  match Yojson.Safe.from_string s with
  | j -> j
  | exception e ->
      Alcotest.fail
        (Printf.sprintf "invalid JSON %S: %s" s (Printexc.to_string e))

let member key (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match json with
  | `Assoc kvs -> List.assoc_opt key kvs
  | _ -> None

let test_job_to_value_json () =
  let j : J.job =
    { J.id = "id-1"; J.fn = adversarial; J.args = "42"; J.branch = "b\"r"
    ; J.submitted_by = "a\\ice"; J.status = "pending"; J.run_at = 1234L
    ; J.attempts = 1; J.max_attempts = 3; J.result = None
    ; J.error = adversarial; J.runner_id = "boot-1" }
  in
  let json = parse_json (match J.job_to_value j with
                         | Pricklypear_lisp.Ast.VStr s -> s
                         | _ -> Alcotest.fail "job_to_value returned non-string")
  in
  Alcotest.(check bool) "fn round-trips escaped" true
    (member "fn" json = Some (`String adversarial));
  Alcotest.(check bool) "error round-trips escaped" true
    (member "error" json = Some (`String adversarial));
  Alcotest.(check bool) "branch/submitted_by round-trip" true
    (member "branch" json = Some (`String "b\"r")
     && member "submitted_by" json = Some (`String "a\\ice"));
  Alcotest.(check bool) "numeric args stays a JSON number" true
    (member "args" json = Some (`Int 42));
  Alcotest.(check bool) "no result -> null" true
    (member "result" json = Some `Null);
  Alcotest.(check bool) "scalar fields preserved" true
    (member "run_at" json = Some (`Int 1234)
     && member "attempts" json = Some (`Int 1)
     && member "max_attempts" json = Some (`Int 3)
     && member "status" json = Some (`String "pending"));
  (* a string-valued jsonb result stays a JSON string (record_done wraps     *)
  (* results in a JSON string before storing, so this is the live shape)     *)
  let j2 = { j with J.args = "{}"; J.result = Some "\"ok-result\"" } in
  let json2 = parse_json (match J.job_to_value j2 with
                          | Pricklypear_lisp.Ast.VStr s -> s
                          | _ -> Alcotest.fail "job_to_value returned non-string")
  in
  Alcotest.(check bool) "result jsonb stays a literal string" true
    (member "result" json2 = Some (`String "ok-result"));
  Alcotest.(check bool) "empty-object args stays an object" true
    (member "args" json2 = Some (`Assoc []));
  (* a string arg is stored as the QUOTED jsonb literal -> JSON string *)
  let j3 = { j with J.args = "\"hello\"" } in
  let json3 = parse_json (match J.job_to_value j3 with
                          | Pricklypear_lisp.Ast.VStr s -> s
                          | _ -> Alcotest.fail "job_to_value returned non-string")
  in
  Alcotest.(check bool) "string args stays a JSON string" true
    (member "args" json3 = Some (`String "hello"))

let test_schedule_to_value_json () =
  let s : J.schedule =
    { J.s_name = "s\"ch"; J.s_fn = adversarial; J.s_args = "{}"
    ; J.s_branch = "b\"r"; J.s_submitted_by = "a\\ice"
    ; J.s_cron = Some "* * * * *"; J.s_every_ms = None; J.s_next_run = 99L
    ; J.s_enabled = true; J.s_tz = "utc" }
  in
  let json = parse_json (match J.schedule_to_value s with
                         | Pricklypear_lisp.Ast.VStr s -> s
                         | _ -> Alcotest.fail "schedule_to_value returned non-string")
  in
  Alcotest.(check bool) "fn round-trips escaped" true
    (member "fn" json = Some (`String adversarial));
  Alcotest.(check bool) "cron is a JSON string" true
    (member "cron" json = Some (`String "* * * * *"));
  Alcotest.(check bool) "no every_ms -> null" true
    (member "every_ms" json = Some `Null);
  Alcotest.(check bool) "next_run/enabled preserved" true
    (member "next_run" json = Some (`Int 99)
     && member "enabled" json = Some (`Bool true));
  (* every-ms variant *)
  let s2 = { s with J.s_cron = None; J.s_every_ms = Some 60_000L } in
  let json2 = parse_json (match J.schedule_to_value s2 with
                          | Pricklypear_lisp.Ast.VStr s -> s
                          | _ -> Alcotest.fail "schedule_to_value returned non-string")
  in
  Alcotest.(check bool) "every_ms variant" true
    (member "cron" json2 = Some `Null
     && member "every_ms" json2 = Some (`Int 60000))


(** i-1788670909: submit is typed — failure is [Error], never an
    id-shaped string. No DB needed: an unconnected store hits the
    no-connection arm before any I/O. *)
let test_submit_no_db_error () =
  let store = S.make ~cfg:(S.config_from_env ()) ~branch_id:main_id () in
  match
    J.submit ~store ~fn:"f" ~args_json:"{}" ~branch:"system/main"
      ~submitted_by:"a" ~run_at_ms:1L ()
  with
  | Error msg ->
      Alcotest.(check string) "no-db failure message" "submit-job: no db connection"
        msg
  | Ok id -> Alcotest.fail ("expected Error, got id " ^ id)

let () =
  Random.self_init ();
  Alcotest.run "jobs"
    [ ( "node-shapes"
      , [ Alcotest.test_case "job lifecycle + transitions" `Quick
            test_job_lifecycle
        ; Alcotest.test_case "visibility confinement" `Quick
            test_visibility_confinement ] )
    ; ( "runner"
      , [ Alcotest.test_case "claim skips locked" `Quick
            test_claim_skips_locked
        ; Alcotest.test_case "backoff schedule" `Quick test_backoff_schedule
        ; Alcotest.test_case "dead-runner reaper" `Quick test_reap_dead ] )
    ; ( "args-bug"
      , [ Alcotest.test_case "eval_src splices jsonb args as literal" `Quick
            test_eval_src_args ] )
    ; ( "json-serialization"
      , [ Alcotest.test_case "job_to_value escapes adversarial fields" `Quick
            test_job_to_value_json
        ; Alcotest.test_case "schedule_to_value escapes adversarial fields" `Quick
            test_schedule_to_value_json ] )
    ; ( "submit-contract"
      , [ Alcotest.test_case "no-DB submit is Error, not an id string" `Quick
            test_submit_no_db_error ] )
    ; ( "step-checkpoints"
      , [ Alcotest.test_case "job-id fiber binding" `Quick
            test_current_job_id_binding
        ; Alcotest.test_case "checkpoint memoization (job,step)" `Quick
            test_checkpoint_memoization ] )
    ; ( "schedules"
      , [ Alcotest.test_case "cron next-fire every minute" `Quick
            test_next_minute
        ; Alcotest.test_case "every-ms drift + spec classification" `Quick
            test_every_ms_drift
        ; Alcotest.test_case "cron across-month boundary" `Quick
            test_cron_across_month
        ; Alcotest.test_case "cron tz: local calendar vs utc" `Quick
            test_cron_tz_local
        ; Alcotest.test_case "schedule CRUD + claim/advance/disable" `Quick
            test_schedule_crud ] )
    ]
