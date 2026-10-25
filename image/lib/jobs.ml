(** Durable background jobs — borge/jobs.borg.

    Jobs are dedicated rows (not data_rows): the runner's SKIP LOCKED claim
    needs real status/run_at columns. The runner is a boot-time Eio fiber
    with its own store+session so it doesn't race HTTP on the shared
    session's branch. Guests see rows via prims; the host owns concurrency.

    v1: job table + runner + core prims (submit-job, job-status, jobs,
    cancel-job). Campaign 4.2 adds step-checkpoints (the (step "name"
    thunk) special form -> __step__ prim) and schedules (schedule/
    unschedule/schedules prims + the runner's due-schedule fire path).
    Tables exist in migration 015. *)

open Pricklypear_lisp.Ast

(* ---- runner identity — one fresh id per boot, host-scoped ---- *)

(* The id is `<host>#<pid>`: the host prefix is what lets a booting image
   recognise leases left by a PREVIOUS process on the SAME machine (see
   [preempt_dead_host_leases]) without ever touching another machine's live
   worker. The pid is what makes that recognition exact — a same-host owner
   whose pid is gone is provably dead, so its rows can be recovered in
   milliseconds instead of waiting out the lease TTL. *)
let host_tag : string =
  let h =
    match Sys.getenv_opt "PP_HOST_TAG" with
    | Some h when String.trim h <> "" -> String.trim h
    | _ -> ( try Unix.gethostname () with _ -> "unknown")
  in
  (* keep the tag free of the separator and path-ish characters *)
  String.map
    (fun c -> if c = '#' || c = '/' || c = ' ' || c = '%' then '_' else c)
    h

let runner_id : string = Printf.sprintf "boot-%s#%d" host_tag (Unix.getpid ())

(** Host prefix shared by every runner id this machine issues. *)
let host_prefix : string = Printf.sprintf "boot-%s#" host_tag

(** Prefix for workers that are NOT this image process — pp-lens and any
    other external claimant (borge/jobs.borg, THE FLEET QUESTION).

    Deliberately DISJOINT from [host_prefix]. [preempt_dead_host_leases]
    scans `boot-<host>#%` and decides death by probing /proc for that pid ON
    THIS MACHINE. A worker on another host would fail that probe — its pid is
    simply not in our process table — so including it in the same namespace
    would let a booting image declare a perfectly live foreign worker dead and
    steal its rows. Keeping external owners in their own prefix means the
    preempt path cannot see them at all, and an external owner recovers on the
    lease TTL alone: extra latency, never a stolen live claim. *)
let external_prefix : string = "ext-"

(** Normalize an external worker's self-declared owner id.

    The worker names itself (`(claim-job "aurora")` on a laptop, say) because
    only it knows what it is; the image must not guess from its own hostname.
    Sanitized to be punctuation-free so the value is safe inside a SQL LIKE
    pattern and readable in an error column, then forced under [external_prefix]
    so a caller cannot smuggle itself into the boot namespace by passing a
    `boot-…` string. *)
let external_owner (raw : string) : string =
  let cleaned =
    String.map
      (fun c ->
        if c = '#' || c = '/' || c = ' ' || c = '%' || c = '\'' then '_'
        else c)
      (String.trim raw)
  in
  if cleaned = "" then external_prefix ^ "unnamed"
  else if
    String.length cleaned >= String.length external_prefix
    && String.sub cleaned 0 (String.length external_prefix) = external_prefix
  then cleaned
  else external_prefix ^ cleaned

(* ---- lease ---- *)

(** Lease TTL. A claim stamps `lease_deadline = now + ttl` and the runner
    heartbeats it while the job evaluates; a row is recoverable only once
    that deadline has PASSED. Mirrors glochid's ratified claim TTL shape
    (supervisor.ml `ttl_s`) so there is one lease idiom in the codebase.
    The floor is generous on purpose: recovery latency is bounded by it, and
    cost of being late is one stuck row, while the cost of being early is
    two live workers on one job. *)
let lease_ttl_ms () : int64 =
  let secs =
    match Sys.getenv_opt "PP_JOB_LEASE_SECS" with
    | Some s -> ( match int_of_string_opt (String.trim s) with Some n -> n | None -> 600)
    | None -> 600
  in
  Int64.mul (Int64.of_int (max 30 secs)) 1000L

(* row-cell accessors live in Row (was a local copy here, "ai_store pattern"). *)

(* ---- job row type ---- *)

type job = {
  id : string;
  fn : string;
  args : string;        (* JSON string *)
  branch : string;
  submitted_by : string;
  status : string;
  run_at : int64;
  attempts : int;
  max_attempts : int;
  result : string option;
  error : string;
  runner_id : string;
  lease_heartbeat_at : int64;
  lease_deadline : int64;
}

let select_cols =
  "id, fn, args::text, branch, submitted_by, status, run_at, attempts, \
   max_attempts, result::text, error, runner_id, lease_heartbeat_at, \
   lease_deadline"

(* Build the call source the runner evals: (fn <args>).
   [args] is the jsonb column rendered ::text — i.e. the JSON literal for the
   single argument (a number, a quoted string, an object, ...). It is spliced
   DIRECTLY (never %S-quoted) so a numeric arg stays a number in the call:
   (submit-job "f" 1) must eval (f 1), not (f "1"). %S-quoting the already-
   JSON text turned every non-string arg into a string literal and broke
   numerics/json — see the runner agent-note in borge/jobs.borg (4.1). *)
let eval_src ~(fn : string) ~(args : string) : string =
  (* Zero-arg convention: args stored as JSON "[]"/"null"/"" fire as a
     0-arity call. Anything else is spliced positionally (must therefore
     be Nopales-readable, e.g. "[1 2]" is NOT — pass "1 2"-style JSON
     scalars or a JSON string). *)
  let a = String.trim args in
  if a = "" || a = "[]" || a = "null" then Printf.sprintf "(%s)" fn
  else Printf.sprintf "(%s %s)" fn args

let row_to_job row : job option =
  try
    let id = Row.cell_string row 0
    and fn = Row.cell_string row 1
    and args = Row.cell_string row 2
    and branch = Row.cell_string row 3
    and submitted_by = Row.cell_string row 4
    and status = Row.cell_string row 5
    and run_at = Row.cell_int64 row 6
    and attempts = Row.cell_int row 7
    and max_attempts = Row.cell_int row 8
    and result = Row.cell_string row 9
    and error = Row.cell_string row 10
    and runner_id = Row.cell_string row 11
    and lease_heartbeat_at = Row.cell_int64 row 12
    and lease_deadline = Row.cell_int64 row 13
    in
    match id, fn, branch, submitted_by, status with
    | Some id, Some fn, Some branch, Some submitted_by, Some status ->
        Some {
          id; fn;
          args = (match args with Some s -> s | None -> "{}");
          branch; submitted_by; status;
          run_at = (match run_at with Some r -> r | None -> 0L);
          attempts = (match attempts with Some a -> a | None -> 0);
          max_attempts = (match max_attempts with Some a -> a | None -> 3);
          result; error = (match error with Some e -> e | None -> "");
          runner_id = (match runner_id with Some r -> r | None -> "");
          lease_heartbeat_at =
            (match lease_heartbeat_at with Some t -> t | None -> 0L);
          lease_deadline = (match lease_deadline with Some t -> t | None -> 0L);
        }
    | _ -> None
  with _ -> None

let pg_conn (store : Store.t) : Io_db.Db.conn option =
  Io_db.Db.current_conn store

(* ---- value serialization ---- *)

(* Embed a stored jsonb column rendered ::text as a JSON value. PG only
   ever hands back valid jsonb text, but a parse failure here must not
   yield invalid output — fall back to a JSON string of the raw text. *)
let json_of_stored_literal (s : string) : Yojson.Safe.t =
  try Yojson.Safe.from_string s with _ -> `String s

let job_to_value (j : job) : value =
  VStr
    (Yojson.Safe.to_string
       (`Assoc
          [ ("id", `String j.id)
          ; ("fn", `String j.fn)
          ; ("args", json_of_stored_literal j.args)
          ; ("branch", `String j.branch)
          ; ("submitted_by", `String j.submitted_by)
          ; ("status", `String j.status)
          ; ("run_at", `Intlit (Int64.to_string j.run_at))
          ; ("attempts", `Int j.attempts)
          ; ("max_attempts", `Int j.max_attempts)
          ; ("result",
             match j.result with
             | Some r -> json_of_stored_literal r
             | None -> `Null)
          ; ("error", `String j.error)
          ; ("runner_id", `String j.runner_id)
          ; ("lease_heartbeat_at", `Intlit (Int64.to_string j.lease_heartbeat_at))
          ; ("lease_deadline", `Intlit (Int64.to_string j.lease_deadline)) ]))


(* ---- submit ---- *)

(** [submit] -> Ok id | Error message (i-1788670909: was id-or-error-string,
    so a caller could not tell a real job id from "submit-job: ..." failure
    text without prefix-sniffing; the prims keep their VStr value contract
    by rendering [Error] as the value, byte-identically). *)
let submit ~(store : Store.t) ~(fn : string) ~(args_json : string)
    ~(branch : string) ~(submitted_by : string) ~(run_at_ms : int64) ()
    : (string, string) result =
  match pg_conn store with
  | None -> Error "submit-job: no db connection"
  | Some conn ->
    (try
      let sql =
        Printf.sprintf
          "INSERT INTO jobs (fn, args, branch, submitted_by, run_at) \
           VALUES ($1, $2::jsonb, $3, $4, $5) RETURNING id::text"
      in
      let rows =
        Store_db.exec conn
          ~params:
            [ Pgx.Value.of_string fn
            ; Pgx.Value.of_string args_json
            ; Pgx.Value.of_string branch
            ; Pgx.Value.of_string submitted_by
            ; Pgx.Value.of_int64 run_at_ms
            ]
          sql
      in
      (match rows with
       | row :: _ ->
           (match Row.cell_string row 0 with
            | Some id -> Ok id
            | None -> Error "submit-job: no id returned")
       | [] -> Error "submit-job: insert returned no row")
    with exn ->
      Error ("submit-job: " ^ Printexc.to_string exn))

(* ---- get (by id, confined to caller unless admin) ---- *)

let get ~(store : Store.t) ~(id : string) ~(caller : string) ~(is_admin : bool)
    : value =
  match pg_conn store with
  | None -> VNil
  | Some conn ->
    let sql =
      Printf.sprintf "SELECT %s FROM jobs WHERE id = $1::uuid" select_cols
    in
    let rows =
      Store_db.exec conn ~params:[ Pgx.Value.of_string id ] sql
    in
    (match rows with
     | row :: _ ->
         (match row_to_job row with
          | Some j when is_admin || j.submitted_by = caller ->
              job_to_value j
          | _ -> VNil)
     | [] -> VNil)

(* ---- list (caller's jobs, recent first) ---- *)

let list ~(store : Store.t) ~(caller : string) ~(is_admin : bool) ~(limit : int) ()
    : value =
  match pg_conn store with
  | None -> VList []
  | Some conn ->
    let lim = limit in
    let where =
      if is_admin then "" else "WHERE submitted_by = $2 "
    in
    let params =
      if is_admin then [ Pgx.Value.of_int lim ]
      else [ Pgx.Value.of_int lim; Pgx.Value.of_string caller ]
    in
    let sql =
      Printf.sprintf "SELECT %s FROM jobs %sORDER BY created_at DESC LIMIT $1"
        select_cols where
    in
    let rows = Store_db.exec conn ~params sql in
    let vals =
      List.filter_map
        (fun r ->
          match row_to_job r with
          | Some j -> Some (job_to_value j)
          | None -> None)
        rows
    in
    VList vals

(* ---- cancel (pending → failed/cancelled) ---- *)

let cancel ~(store : Store.t) ~(id : string) ~(caller : string) ~(is_admin : bool)
    : value =
  match pg_conn store with
  | None -> VStr "cancel-job: no db connection"
  | Some conn ->
    let confinement =
      if is_admin then "" else "AND submitted_by = $2 "
    in
    let params =
      if is_admin then [ Pgx.Value.of_string id ]
      else [ Pgx.Value.of_string id; Pgx.Value.of_string caller ]
    in
    let sql =
      Printf.sprintf
        "UPDATE jobs SET status = 'failed', error = 'cancelled' \
         WHERE id = $1::uuid AND status = 'pending' %sRETURNING id::text"
        confinement
    in
    let rows = Store_db.exec conn ~params sql in
    (match rows with
     | row :: _ ->
         (match Row.cell_string row 0 with
          | Some id -> VStr id
          | None -> VStr "cancel-job: no id returned")
     | [] -> VStr "cancel-job: not pending or not yours")

(* ---- claim: SKIP LOCKED, mark running ---- *)

(** Claim one due job.

    [eligible_for] is the TENANT SCOPE of the claim, and it is the only place
    scope has to be decided: claiming is the single step that CHOOSES a row.
    [None] = unrestricted (the in-image runner, and verified admins);
    [Some uname] = only rows [submitted_by] that user.

    Note this needs no new column: [submitted_by] is pinned at submit
    (Jobs.submit) from the authenticated caller, so it already IS the owner of
    the work. Self-claim is not an escalation — submitting a job already means
    "eval this fn on my branch as me" — while claiming ANOTHER user's job
    would be, which is why that stays admin-only. *)
let claim_due ~(store : Store.t) ?(owner : string = runner_id)
    ?(eligible_for = (None : string option))
    ?(branch = (None : string option)) () : job option =
  match pg_conn store with
  | None -> None
  | Some _ ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let lease_until = Int64.add now_ms (lease_ttl_ms ()) in
    (* Claim one due pending job, oldest first, with SKIP LOCKED.
       Runs in its own transaction so the claim commits before eval.

       The claim is a LEASE (045): the same statement that flips the row to
       running stamps the owner, the heartbeat, and the deadline, so there is
       never a 'running' row without a liveness proof. *)
    Store.with_dynamic_transaction store (fun () ->
      match pg_conn store with
      | None -> None
      | Some conn ->
        (* The eligibility predicate lives INSIDE the subquery, not on the
           UPDATE: filtering after the LIMIT 1 would let an ineligible row
           occupy the single slot and starve the claimant forever. *)
        (* SCOPE PREDICATES, NUMBERED DYNAMICALLY.

           Eligibility and branch are independent optional narrowings, so the
           placeholder numbers depend on WHICH are present: hardcoding $4/$5
           leaves an unreferenced parameter when only one applies, and
           PostgreSQL rejects that at prepare time (42P18, "could not
           determine data type of parameter $4"). Build the fragments and
           their params together, numbering from 4 — the first free slot after
           owner/heartbeat/deadline. *)
        let numbered =
          List.filter_map
            (fun v -> v)
            [ Option.map (fun uname -> ("submitted_by", uname)) eligible_for
            ; Option.map (fun b -> ("branch", b)) branch ]
        in
        let scope_sql =
          String.concat ""
            (List.mapi
               (fun i (col, _) ->
                 Printf.sprintf " AND %s = $%d" col (i + 4))
               numbered)
        in
        let scope_params =
          List.map (fun (_, v) -> Pgx.Value.of_string v) numbered
        in
        let sql =
          Printf.sprintf
            "UPDATE jobs SET status = 'running', runner_id = $1, \
             attempts = attempts + 1, lease_heartbeat_at = $2, \
             lease_deadline = $3 \
             WHERE id = (SELECT id FROM jobs WHERE status = 'pending' \
                         AND run_at <= $2 %s ORDER BY run_at, id \
                         FOR UPDATE SKIP LOCKED LIMIT 1) \
             RETURNING %s"
            scope_sql select_cols
        in
        let rows =
          Store_db.exec conn
            ~params:
              ([ Pgx.Value.of_string owner
               ; Pgx.Value.of_int64 now_ms
               ; Pgx.Value.of_int64 lease_until
               ]
              @ scope_params)
            sql
        in
        (match rows with
         | row :: _ -> row_to_job row
         | [] -> None))

(* exponential backoff: 2^attempts seconds, capped at 60s *)
let rec expt_backoff (store : Store.t) (id : string) : int =
  match pg_conn store with
  | None -> 2
  | Some conn ->
    let rows =
      Store_db.exec conn ~params:[ Pgx.Value.of_string id ]
        "SELECT attempts FROM jobs WHERE id = $1::uuid"
    in
    (match rows with
     | row :: _ ->
         (match Row.cell_int row 0 with
          | Some a ->
              let secs = 1 lsl (min a 6) in
              min secs 60
          | None -> 2)
     | [] -> 2)


(* ---- record results ---- *)

(* Terminal transitions RELEASE the lease (045): deadline 0 means "not a
   live claim", so nothing has to special-case status here and a leftover
   deadline can never make a finished row look recoverable. *)

(* The trailing [()] makes the optional [owner] erasable: without it the
   optional can never be resolved, because every other argument is labelled
   (warning 16 as error under this dune profile).

   Returns [true] iff the CAS actually landed. The in-image runner ignores
   this (it already tracks lease loss through [heartbeat]), but an EXTERNAL
   claimant cannot: it learns it lost the row no other way, and a worker that
   assumed success would silently drop a result. *)
let record_done ~(store : Store.t) ?(owner : string option)
    ~(id : string) ~(result_json : string) () : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
    (* With [owner], the write is a CAS on the lease: only the runner that
       still holds the row may finish it. Without it a worker whose lease
       lapsed could stamp 'done' over a result another runner already
       produced after reclaiming the row. *)
    let guard, extra =
      match owner with
      | Some rid ->
          ( "AND status = 'running' AND runner_id = $3",
            [ Pgx.Value.of_string rid ] )
      | None -> ("", [])
    in
    let sql =
      Printf.sprintf
        "UPDATE jobs SET status = 'done', result = $2::jsonb, error = '', \
         lease_deadline = 0 \
         WHERE id = $1::uuid %s \
         RETURNING id::text"
        guard
    in
    (* Wrap the result in a JSON string value so it's always valid jsonb. *)
    let wrapped = Printf.sprintf "%s" (Yojson.Safe.to_string (`String result_json)) in
    let rows =
      Store_db.exec conn
        ~params:
          ([ Pgx.Value.of_string id; Pgx.Value.of_string wrapped ] @ extra)
        sql
    in
    rows <> []

(* [max_attempts] is OPTIONAL ON PURPOSE (2026-10 external-claimant pass).
   Callers used to pass it, and boot.ml only ever echoed back the value it
   read off the row in the claim's RETURNING — so the parameter added no
   information while giving any caller the power to inflate the retry budget
   by passing a bigger number. Now an external worker (claim-job / job-finish)
   cannot touch it: omitted, the UPDATE compares against the row's own
   max_attempts column. The in-image runner may still pass it explicitly,
   which is byte-equivalent to what it passed before. *)
let record_failed ~(store : Store.t) ?(owner : string option)
    ?(max_attempts : int option) ~(id : string) ~(error : string)
    () : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
    (* If attempts >= max_attempts → dead; else pending with backoff. *)
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let run_at =
      Int64.add now_ms (Int64.of_int (expt_backoff store id))
    in
    (* PARAMETER NUMBERING depends on whether a budget was supplied, because
       the row-derived form does not reference $3 at all. Numbering by hand
       (six literals) instead of by position is what keeps SQL and params in
       lockstep: leaving a $3 in the text with no param — or passing one the
       text never mentions — is PostgreSQL 42P18 "could not determine data
       type of parameter $3", a hard error at prepare time. *)
    let budget_sql, run_at_idx, next =
      match max_attempts with
      | Some _ -> ("$3", 4, 5)
      | None -> ("max_attempts", 3, 4)
    in
    let guard, guard_params =
      match owner with
      | Some rid ->
          ( Printf.sprintf "AND status = 'running' AND runner_id = $%d" next,
            [ Pgx.Value.of_string rid ] )
      | None -> ("", [])
    in
    let sql =
      Printf.sprintf
        "UPDATE jobs SET status = CASE WHEN attempts >= %s THEN 'dead' \
                                  ELSE 'pending' END, \
         error = $2, lease_deadline = 0, \
         run_at = CASE WHEN attempts >= %s THEN run_at \
                                    ELSE $%d END \
         WHERE id = $1::uuid %s \
         RETURNING status"
        budget_sql budget_sql run_at_idx guard
    in
    let params =
      [ Pgx.Value.of_string id; Pgx.Value.of_string error ]
      @ (match max_attempts with
         | Some n -> [ Pgx.Value.of_int n ]
         | None -> [])
      @ [ Pgx.Value.of_int64 run_at ]
      @ guard_params
    in
    (* RETURNING makes the CAS verdict observable in the same statement: an
       empty result means the guard did not match, i.e. we no longer own the
       row. An external claimant needs this — losing the lease is otherwise
       invisible to it. *)
    Store_db.exec conn ~params sql <> []

(* ---- lease heartbeat ---- *)

(** Renew the lease on a job this runner holds. Called by the runner's
    heartbeat fiber while the job evaluates.

    Returns [true] when the row was still ours and is renewed, [false] when
    the lease is GONE — either the row finished, or (worse) the deadline
    passed unnoticed and another runner reclaimed the row. The runner uses
    the [false] to stop treating the eval as authoritative; it does NOT abort
    the fiber (v1 has no mid-eval cancellation, jobs.borg hard edges), but the
    stopped-renews path is logged so a duplicate eval is never silent. *)
let heartbeat ~(store : Store.t) ~(id : string)
    ?(owner : string = runner_id) () : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let deadline = Int64.add now_ms (lease_ttl_ms ()) in
    let rows =
      Store_db.exec conn
        ~params:
          [ Pgx.Value.of_string id
          ; Pgx.Value.of_int64 now_ms
          ; Pgx.Value.of_int64 deadline
          ; Pgx.Value.of_string owner
          ]
        "UPDATE jobs SET lease_heartbeat_at = $2, lease_deadline = $3 \
         WHERE id = $1::uuid AND status = 'running' AND runner_id = $4 \
         RETURNING id::text"
    in
    rows <> []

(* ---- release: hand a claim back UNRUN ---- *)

(** Return a claimed row to pending without counting an attempt.

    This is not the same as [record_failed]. A worker that discovers it
    cannot faithfully run a job — the body carries effects that cannot leave
    the image, or the def does not resolve locally — has not USED an attempt:
    nothing ran, nothing failed. Without this, such a row would sit 'running'
    until the lease lapsed (10 min default) and then be requeued with
    attempts+1, so a worker that keeps picking up an unrunnable job would
    march it to 'dead' purely by declining to run it. The decrement
    (attempts = GREATEST(attempts - 1, 0)) is what makes declining free.

    Guarded by the same ownership CAS as every other terminal write: only the
    current holder can give the row back. [run_at] is untouched, so a
    released row is immediately claimable by the next eligible worker —
    which is exactly the handoff intended (an image runner takes the Txn
    job a lens worker declined). *)
let release ~(store : Store.t) ~(id : string)
    ?(owner : string = runner_id) () : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
    let rows =
      Store_db.exec conn
        ~params:[ Pgx.Value.of_string id; Pgx.Value.of_string owner ]
        "UPDATE jobs SET status = 'pending', runner_id = '', \
         lease_deadline = 0, lease_heartbeat_at = 0, \
         attempts = GREATEST(attempts - 1, 0) \
         WHERE id = $1::uuid AND status = 'running' AND runner_id = $2 \
         RETURNING id::text"
    in
    rows <> []

(* ---- dead-runner recovery: expire LAPSED leases only ---- *)

(** Recover rows whose lease deadline has passed. This is the ONLY reclaim
    path, and it never looks at [runner_id] for ownership: a live worker's
    lease is not stolen no matter which process is asking (glochid's ratified
    rule — "a live claim is NEVER stolen"). A crashed image simply stops
    heartbeating and its rows come back here once the TTL lapses.

    Requeue is IMMEDIATE (no backoff): [attempts] was already incremented by
    the claim, so the retry budget — not a timer — bounds a job that keeps
    killing its runner; making the user wait out both a TTL and a backoff for
    an image restart is pure latency. [run_at] is therefore pulled to now, not
    left where it was: only a row that WAS claimed can be here, so this can
    never resurrect a job that was legitimately scheduled for later. *)
let reap_dead ~(store : Store.t) : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let sql =
      "UPDATE jobs SET status = CASE WHEN attempts >= max_attempts THEN 'dead' \
                                      ELSE 'pending' END, \
       error = 'runner lease expired: ' || runner_id, \
       lease_deadline = 0, lease_heartbeat_at = 0, \
       run_at = CASE WHEN attempts >= max_attempts THEN run_at ELSE $1 END \
       WHERE status = 'running' AND lease_deadline <= $1"
    in
    ignore
      (Store_db.exec conn
         ~params:[ Pgx.Value.of_int64 now_ms ]
         sql)

(* ---- same-host boot preempt: recover our OWN dead processes at once ---- *)

(** Parse the pid out of an id this module issues (`boot-<host>#<pid>`). *)
let pid_of_runner_id (rid : string) : int option =
  match String.rindex_opt rid '#' with
  | None -> None
  | Some i ->
    int_of_string_opt
      (String.sub rid (i + 1) (String.length rid - i - 1))

let pid_alive (pid : int) : bool =
  if pid <= 0 then false
  else if pid = Unix.getpid () then true
  else if Sys.file_exists (Printf.sprintf "/proc/%d" pid) then true
  else (try Unix.kill pid 0; true with Unix.Unix_error (Unix.ESRCH, _, _) -> false | _ -> true)

(** Recover leases held by a process on THIS host that no longer exists.

    [reap_dead] alone would make an image restart wait out the full lease TTL
    (600s by default) before re-running an interrupted job — a real latency
    regression against the old reap-everything-else behaviour. This narrows
    that: a lease whose owner is *this machine* AND whose pid is gone is
    provably dead, so its rows are recovered immediately at boot.

    Safety: the host tag must match, so another machine's live worker is
    never touched; an unknowable pid leaves the row to the TTL. Residual
    risk is pid reuse across a restart — a reused pid keeps a dead lease
    alive until the deadline, which costs latency, never correctness. *)
let preempt_dead_host_leases ~(store : Store.t) : int =
  match pg_conn store with
  | None -> 0
  | Some conn ->
    let rows =
      Store_db.exec conn
        ~params:[ Pgx.Value.of_string (host_prefix ^ "%") ]
        "SELECT DISTINCT runner_id FROM jobs \
         WHERE status = 'running' AND runner_id LIKE $1"
    in
    let dead_owners =
      List.filter_map
        (fun row ->
          match Row.cell_string row 0 with
          | None -> None
          | Some rid -> (
              match pid_of_runner_id rid with
              | Some pid when not (pid_alive pid) -> Some rid
              | _ -> None))
        rows
    in
    let total = ref 0 in
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    List.iter
      (fun rid ->
        let n =
          match
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string rid; Pgx.Value.of_int64 now_ms ]
              "UPDATE jobs SET \
               status = CASE WHEN attempts >= max_attempts THEN 'dead' \
                             ELSE 'pending' END, \
               error = 'runner died: ' || runner_id, \
               lease_deadline = 0, lease_heartbeat_at = 0, \
               run_at = CASE WHEN attempts >= max_attempts THEN run_at \
                             ELSE $2 END \
               WHERE status = 'running' AND runner_id = $1 \
               RETURNING id::text"
          with
          | rows -> List.length rows
        in
        total := !total + n)
      dead_owners;
    !total

(* The runner loop (eval_job + runner_loop) lives in boot.ml to avoid a
   dependency cycle: Jobs (DB ops) must not depend on Session, which
   depends on Prims_db, which depends on Jobs. boot.ml can depend on
   both Jobs and Session. *)

(* ================================================================== *)
(* STEP CHECKPOINTS — (with-step "name" thunk) memoized by (job-id, name). *)
(* ======================================================================== *)

(* Fiber-local id of the job currently being evaluated by the runner. The
   runner (boot.ml) binds it around eval; the __step__ prim reads it to
   scope checkpoints per (job-id, name). Outside a job fiber it is None, so
   (with-step "name" thunk) falls back to a plain thunk call. Mirrors the
   pp_user_key pattern in store_db.ml. *)
let current_job_id : string Eio.Fiber.key = Eio.Fiber.create_key ()

let with_current_job_id (job_id : string) (f : unit -> 'a) : 'a =
  Eio.Fiber.with_binding current_job_id job_id f

let current_job_id_opt () : string option = Eio.Fiber.get current_job_id

(** Look up an existing checkpoint value for (job_id, name). Returns the
    stored JSONB value as a string (the JSON literal), or None. *)
let checkpoint_lookup ~(store : Store.t) ~(job_id : string) ~(name : string)
    : string option =
  match pg_conn store with
  | None -> None
  | Some conn ->
    let rows =
      Store_db.exec conn
        ~params:
          [ Pgx.Value.of_string job_id; Pgx.Value.of_string name ]
        "SELECT value::text FROM job_steps WHERE job_id = $1::uuid \
         AND step_name = $2"
    in
    (match rows with
     | row :: _ -> Row.cell_string row 0
     | [] -> None)

(** Write a checkpoint row for (job_id, name) storing [value] (a JSON
    literal string). Upsert: a re-run overwrites. Rides the current
    connection (so inside a with-transaction it lands in the SAME tx as
    the thunk's writes — the exactly-once composition). *)
let checkpoint_write ~(store : Store.t) ~(job_id : string) ~(name : string)
    ~(value : string) : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    ignore
      (Store_db.exec conn
         ~params:
           [ Pgx.Value.of_string job_id; Pgx.Value.of_string name;
             Pgx.Value.of_string value ]
         "INSERT INTO job_steps (job_id, step_name, value) \
          VALUES ($1::uuid, $2, $3::jsonb) \
          ON CONFLICT (job_id, step_name) DO UPDATE SET value = EXCLUDED.value")

(* ================================================================== *)
(* SCHEDULES — cron / every-ms rows + next-fire math.                  *)
(* ================================================================== *)

(* 5-field cron: minute hour day-of-month month day-of-week. A field is
   either "*", "*/step", or a comma list of values ("a,b,c"). We support
   the common subset: *, */N, and single integers / comma lists. v1 does not
   implement ranges ("1-5") or names (jan, mon). *)

type cron_field =
  | Any                    (* * *)
  | Step of int * int      (* */N -> min 0, step N *)
  | List of int list       (* explicit values *)

let parse_cron_field (raw : string) (low : int) (high : int) : cron_field =
  let raw = String.trim raw in
  if raw = "*" then Any
  else if String.length raw >= 3 && String.sub raw 0 2 = "*/" then
    let n =
      try int_of_string (String.sub raw 2 (String.length raw - 2)) with _ -> 0
    in
    if n <= 0 then Any else Step (low, n)
  else
    let parts = String.split_on_char ',' raw in
    let vals =
      List.filter_map
        (fun p ->
          let p = String.trim p in
          if p = "" then None
          else
            (try
               let v = int_of_string p in
               if v >= low && v <= high then Some v else None
             with _ -> None))
        parts
    in
    if vals = [] then Any else List vals

let cron_field_matches (f : cron_field) (v : int) : bool =
  match f with
  | Any -> true
  | Step (minv, n) -> v >= minv && (v - minv) mod n = 0
  | List vs -> List.mem v vs

(** Split a 5-field cron string and parse each field with its bounds
    (minute hour day-of-month month day-of-week). Malformed width → None. *)
let parse_cron_seq (raw : string) (bounds : (int * int) list)
    : (cron_field list) option =
  let fields = String.split_on_char ' ' raw in
  if List.length fields <> 5 then None
  else
    (try
       let parsed =
         List.map2
           (fun f (low, high) -> parse_cron_field f low high)
           fields bounds
       in
       Some parsed
     with _ -> None)

(** Parse a full 5-field cron string into a tuple of fields
    (minute hour day-of-month month day-of-week). Bounds follow POSIX cron:
    minute 0-59, hour 0-23, day-of-month 1-31, month 1-12, day-of-week 0-6
    (Sunday=0). Malformed input (wrong width, out-of-range-only fields)
    yields None; the caller treats None as "no match / default to +1min". *)
let parse_cron (raw : string)
    : (cron_field * cron_field * cron_field * cron_field * cron_field) option =
  match parse_cron_seq raw [(0, 59); (0, 23); (1, 31); (1, 12); (0, 6)] with
  | Some [ m; h; dom; mon; dow ] -> Some (m, h, dom, mon, dow)
  | _ -> None

(* Standard cron: if BOTH dom and dow are restricted (non-Any), a match on
   either is sufficient; if only one is restricted, it must match. *)
let is_restricted (f : cron_field) : bool =
  match f with Any -> false | _ -> true

(** Does the given calendar time match the parsed cron? [t] is epoch
    seconds; [calendar] projects it (Unix.gmtime for UTC crons,
    Unix.localtime for process-TZ crons, i.e. America/Denver in prod).
    DST is handled by construction: the minute-by-minute search walks
    real epochs, so a nonexistent spring-forward local time never
    matches and a repeated fall-back hour fires twice, like vixie cron. *)
let cron_matches_cal ~(calendar : float -> Unix.tm) (cron : string)
    (t : float) : bool =
  match parse_cron cron with
  | None -> false
  | Some (m, h, dom, mon, dow) ->
      let tm = calendar t in
      let min = tm.Unix.tm_min
      and hour = tm.Unix.tm_hour
      and mday = tm.Unix.tm_mday
      and month = tm.Unix.tm_mon + 1
      and wday = tm.Unix.tm_wday in
      let m_ok = cron_field_matches m min
      and h_ok = cron_field_matches h hour
      and mon_ok = cron_field_matches mon month
      and dom_ok = cron_field_matches dom mday
      and dow_ok = cron_field_matches dow wday in
      (* If both dom and dow are restricted, either counting is enough. *)
      let dom_dow_ok =
        if is_restricted dom && is_restricted dow then dom_ok || dow_ok
        else dom_ok && dow_ok
      in
      m_ok && h_ok && mon_ok && dom_dow_ok

let cron_matches (cron : string) (t : float) : bool =
  cron_matches_cal ~calendar:Unix.gmtime cron t

(** Next fire time (epoch ms) strictly after [from_ms] matching the cron,
    searching minute-by-minute with a ~2-year horizon (avoids infinite
    loops). Returns None if the cron doesn't match within the horizon (e.g.
    an impossible Feb-30). *)
let next_cron_ms_with ~(calendar : float -> Unix.tm) (cron : string)
    (from_ms : int64) : int64 option =
  try
    match parse_cron cron with
    | None -> None
    | Some _ ->
        let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
        (* Start from the next whole minute strictly after from. *)
        let start_min =
          Int64.add (Int64.div from_ms 60_000L) 1L
        in
        let rec search (min64 : int64) : int64 option =
          if min64 > Int64.add (Int64.div now_ms 60_000L)
                   (Int64.of_int (365 * 2 * 24 * 60))
          then None
          else
            let t = Int64.to_float (Int64.mul min64 60_000L) /. 1000.0 in
            if cron_matches_cal ~calendar cron t then Some (Int64.mul min64 60_000L)
            else search (Int64.add min64 1L)
        in
        search start_min
  with _ -> None

let next_cron_ms (cron : string) (from_ms : int64) : int64 option =
  next_cron_ms_with ~calendar:Unix.gmtime cron from_ms

(** Cron matched against the image process's local zone (prod:
    America/Denver via the 5cec9fe TZ knob). "0 15 * * *" means 15:00
    local, DST included — retires the one-shot dst-flip class. *)
let next_cron_ms_local (cron : string) (from_ms : int64) : int64 option =
  next_cron_ms_with ~calendar:Unix.localtime cron from_ms

(** The matcher for a schedule's [tz] column ("utc" default, "local" =
    process zone). *)
let next_cron_for_tz (tz : string) (cron : string) (from_ms : int64) :
    int64 option =
  if tz = "local" then next_cron_ms_local cron from_ms
  else next_cron_ms cron from_ms

(** Next fire for an every-ms interval: from + every_ms. *)
let next_every_ms (every_ms : int64) (from_ms : int64) : int64 =
  Int64.add from_ms every_ms

(** Is [spec] an every-ms integer (as opposed to a 5-field cron)? *)
let is_every_ms (spec : string) : bool =
  match int_of_string_opt (String.trim spec) with Some _ -> true | None -> false

let spec_is_cron (spec : string) : bool = not (is_every_ms spec)

(* --- schedule DB ops --- *)

type schedule = {
  s_name : string;
  s_fn : string;
  s_args : string;
  s_branch : string;
  s_submitted_by : string;
  s_cron : string option;
  s_every_ms : int64 option;
  s_next_run : int64;
  s_enabled : bool;
  s_tz : string;   (* "utc" (default) | "local" (image process zone) *)
}

let schedule_to_value (s : schedule) : value =
  VStr
    (Yojson.Safe.to_string
       (`Assoc
          [ ("name", `String s.s_name)
          ; ("fn", `String s.s_fn)
          ; ("args", json_of_stored_literal s.s_args)
          ; ("branch", `String s.s_branch)
          ; ("submitted_by", `String s.s_submitted_by)
          ; ("cron", match s.s_cron with Some c -> `String c | None -> `Null)
          ; ("every_ms",
             match s.s_every_ms with
             | Some e -> `Intlit (Int64.to_string e)
             | None -> `Null)
           ; ("next_run", `Intlit (Int64.to_string s.s_next_run))
           ; ("tz", `String s.s_tz)
           ; ("enabled", `Bool s.s_enabled) ]))

let row_to_schedule (row : Pgx.row) : schedule option =
  try
    let name = Row.cell_string row 0
    and fn = Row.cell_string row 1
    and args = Row.cell_string row 2
    and branch = Row.cell_string row 3
    and submitted_by = Row.cell_string row 4
    and cron = Row.cell_string row 5
    and every_ms = Row.cell_int64 row 6
      and next_run = Row.cell_int64 row 7
      and enabled = Row.cell_string row 8
      and tz = Row.cell_string row 9
      in
    match name, fn, branch, submitted_by, next_run with
    | Some name, Some fn, Some branch, Some submitted_by, Some next_run ->
        Some {
          s_name = name; s_fn = fn;
          s_args = (match args with Some a -> a | None -> "{}");
          s_branch = branch; s_submitted_by = submitted_by;
          s_cron = cron;
          s_every_ms =
            (match cron, every_ms with
             | Some _c, _ -> None
             | None, Some e -> Some e
             | None, None -> None);
          s_next_run = next_run;
          s_enabled = (match enabled with Some "t" | Some "true" -> true | _ -> false);
          s_tz = (match tz with Some "local" -> "local" | _ -> "utc");
        }
    | _ -> None
  with _ -> None

(** Upsert a schedule row. [spec] is a 5-field cron string or an every-ms
    integer. [tz] is "utc" (default) or "local": cron fields matched
    against the image process zone (America/Denver in prod) instead of
    UTC. Computes the first next_run from now. *)
let schedule_upsert ~(store : Store.t) ~(name : string) ~(spec : string)
    ~(fn : string) ~(args_json : string) ~(branch : string)
    ~(submitted_by : string) ?(tz = "utc") () : value =
  match pg_conn store with
  | None -> VStr "schedule: no db connection"
  | Some conn ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let cron = if spec_is_cron spec then Some spec else None in
    let every_ms =
      match int_of_string_opt (String.trim spec) with
      | Some ms -> Some (Int64.of_int ms)
      | None -> None
    in
    if tz <> "utc" && tz <> "local" then
      VStr "schedule: tz must be \"utc\" or \"local\""
    else if cron = None && every_ms = None then
      VStr "schedule: spec must be a 5-field cron or an every-ms integer"
    else
      let next_run =
        match cron with
        | Some c ->
            (match next_cron_for_tz tz c now_ms with
             | Some n -> n
             | None -> Int64.add now_ms 60_000L)
        | None -> next_every_ms (Option.get every_ms) now_ms
      in
      (try
        let sql =
          "INSERT INTO schedules (name, fn, args, branch, submitted_by, \
           cron, every_ms, next_run, tz, enabled) \
           VALUES ($1, $2, $3::jsonb, $4, $5, $6, $7, $8, $9, true) \
           ON CONFLICT (name) DO UPDATE SET fn = EXCLUDED.fn, \
             args = EXCLUDED.args, branch = EXCLUDED.branch, \
             submitted_by = EXCLUDED.submitted_by, cron = EXCLUDED.cron, \
             every_ms = EXCLUDED.every_ms, next_run = EXCLUDED.next_run, \
             tz = EXCLUDED.tz, enabled = true \
             RETURNING name::text"
        in
        let rows =
          Store_db.exec conn
            ~params:
              [ Pgx.Value.of_string name; Pgx.Value.of_string fn;
                Pgx.Value.of_string args_json; Pgx.Value.of_string branch;
                Pgx.Value.of_string submitted_by;
                (match cron with Some c -> Pgx.Value.of_string c | None -> Pgx.Value.null);
                (match every_ms with Some e -> Pgx.Value.of_int64 e | None -> Pgx.Value.null);
                Pgx.Value.of_int64 next_run; Pgx.Value.of_string tz ]
            sql
          in
          (match rows with
           | row :: _ ->
               (match Row.cell_string row 0 with Some n -> VStr n | None -> VStr name)
           | [] -> VStr name)
        with exn -> VStr ("schedule: " ^ Printexc.to_string exn))

(** Disable a schedule. *)
let unschedule ~(store : Store.t) ~(name : string) () : value =
  match pg_conn store with
  | None -> VStr "unschedule: no db connection"
  | Some conn ->
    (try
      let rows =
        Store_db.exec conn ~params:[ Pgx.Value.of_string name ]
          "UPDATE schedules SET enabled = false WHERE name = $1 RETURNING name::text"
      in
      (match rows with
       | row :: _ ->
           (match Row.cell_string row 0 with Some n -> VStr n | None -> VStr "")
       | [] -> VStr "unschedule: no such schedule")
    with exn -> VStr ("unschedule: " ^ Printexc.to_string exn))

(** List schedules (ordered by name). *)
let list_schedules ~(store : Store.t) () : value =
  match pg_conn store with
  | None -> VList []
  | Some conn ->
    (try
        let rows =
          Store_db.exec conn
            "SELECT name, fn, args::text, branch, submitted_by, cron, every_ms, \
             next_run, enabled, tz FROM schedules ORDER BY name"
        in
      let vals =
        List.filter_map
          (fun r -> match row_to_schedule r with Some s -> Some (schedule_to_value s) | None -> None)
          rows
      in
      VList vals
     with exn ->
       (* A silent [] here hid prod's missing schedules.tz column (42703) for
          13 days (card 6ab0722c): (schedules) looked empty while every
          schedule was overdue. Keep the value contract, log loudly. *)
       Printf.eprintf "[jobs] schedules listing failed: %s\n%!"
         (Printexc.to_string exn);
       VList [])

(** Claim a single DUE enabled schedule (oldest next_run first), returning
    it (and leaving the row unlocked for the caller to advance). Used by the
    runner's schedule fire path — SKIP LOCKED so concurrent boot/recovery
    can't double-fire. *)
let claim_due_schedule ~(store : Store.t) : schedule option =
  match pg_conn store with
  | None -> None
  | Some _ ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    Store.with_dynamic_transaction store (fun () ->
      match pg_conn store with
      | None -> None
      | Some conn ->
          let sql =
            "SELECT name, fn, args::text, branch, submitted_by, cron, every_ms, \
             next_run, enabled, tz FROM schedules \
             WHERE enabled AND next_run <= $1 \
             ORDER BY next_run, name FOR UPDATE SKIP LOCKED LIMIT 1"
          in
        let rows =
          Store_db.exec conn ~params:[ Pgx.Value.of_int64 now_ms ] sql
        in
        (match rows with row :: _ -> row_to_schedule row | [] -> None))

(** Advance a schedule's next_run after it fired (cron or every-ms).
    [tz] selects the cron calendar ("utc" or "local"): callers pass the
    schedule's tz column value, "utc" when unset. An impossible cron
    (next_cron_ms = None, e.g. Feb 30) DISABLES the schedule here instead
    of rescheduling it: retrying in a minute re-fired the broken spec
    every minute forever (i-1789100668). *)
let advance_schedule ~(store : Store.t) ~(name : string) ~(fired_at : int64)
    ~(cron : string option) ~(every_ms : int64 option) ~(tz : string) : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let next =
      match cron, every_ms with
      | Some c, _ -> next_cron_for_tz tz c now_ms
      | None, Some e -> Some (next_every_ms e now_ms)
      | None, None -> Some (Int64.add now_ms 60_000L)
    in
    (match next with
     | Some n ->
       ignore
         (Store_db.exec conn
            ~params:[ Pgx.Value.of_string name; Pgx.Value.of_int64 n ]
            "UPDATE schedules SET next_run = $2 WHERE name = $1")
     | None ->
       (* No next match within the search horizon: disable once. The
          (schedules) listing shows enabled=false; re-upserting a fixed
          spec re-enables. *)
       ignore
         (Store_db.exec conn ~params:[ Pgx.Value.of_string name ]
            "UPDATE schedules SET enabled = false WHERE name = $1"))

(** Push a schedule's next_run forward unconditionally. Last-resort companion
    to [advance_schedule] for the runner's fire loop: when advancing itself
    raises, the loop must still leave the row NOT due, or claim_due_schedule
    hands the same row straight back and the fire loop spins (2026-10-12
    scheduler stall, card 6ab0722c). Best effort: false when the write itself
    fails. *)
let push_schedule_forward ~(store : Store.t) ~(name : string)
    ~(not_before : int64) : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
    (try
       Store_db.exec conn
         ~params:[ Pgx.Value.of_string name; Pgx.Value.of_int64 not_before ]
         "UPDATE schedules SET next_run = $2 WHERE name = $1"
       <> []
     with _ -> false)
(* The runner loop (eval_job + runner_loop) lives in boot.ml to avoid a
   dependency cycle: Jobs (DB ops) must not depend on Session, which
   depends on Prims_db, which depends on Jobs. boot.ml can depend on
   both Jobs and Session. *)
