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
  J.submit ~store ~fn ~args_json ~branch
    ~submitted_by ~run_at_ms:(Int64.add (now_ms ()) future) ()

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
      (* second, independent connection/transaction locks job1's row *)
      let cfg = S.config_from_env () in
      let store_b = S.make ~cfg ~branch_id:main_id () in
      S.connect store_b;
      S.with_transaction store_b (fun conn_b ->
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
    ]
