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

(* ---- runner identity — one fresh id per boot ---- *)

let runner_id : string =
  Printf.sprintf "boot-%s" (string_of_int (Unix.getpid ()))

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
}

let select_cols =
  "id, fn, args::text, branch, submitted_by, status, run_at, attempts, \
   max_attempts, result::text, error, runner_id"

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
        }
    | _ -> None
  with _ -> None

let pg_conn (store : Store.t) : Io_lwt.Db.conn option =
  Io_lwt.Db.current_conn store

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
          ; ("runner_id", `String j.runner_id) ]))


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

let claim_due ~(store : Store.t) : job option =
  match pg_conn store with
  | None -> None
  | Some _ ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    (* Claim one due pending job, oldest first, with SKIP LOCKED.
       Runs in its own transaction so the claim commits before eval. *)
    Store.with_dynamic_transaction store (fun () ->
      match pg_conn store with
      | None -> None
      | Some conn ->
        let sql =
          Printf.sprintf
            "UPDATE jobs SET status = 'running', runner_id = $1, \
             attempts = attempts + 1 \
             WHERE id = (SELECT id FROM jobs WHERE status = 'pending' \
                         AND run_at <= $2 ORDER BY run_at, id \
                         FOR UPDATE SKIP LOCKED LIMIT 1) \
             RETURNING %s"
            select_cols
        in
        let rows =
          Store_db.exec conn
            ~params:
              [ Pgx.Value.of_string runner_id
              ; Pgx.Value.of_int64 now_ms
              ]
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

let record_done ~(store : Store.t) ~(id : string) ~(result_json : string) : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    let sql =
      "UPDATE jobs SET status = 'done', result = $2::jsonb, error = '' \
       WHERE id = $1::uuid"
    in
    (* Wrap the result in a JSON string value so it's always valid jsonb. *)
    let wrapped = Printf.sprintf "%s" (Yojson.Safe.to_string (`String result_json)) in
    ignore (Store_db.exec conn
              ~params:[ Pgx.Value.of_string id; Pgx.Value.of_string wrapped ]
              sql)

let record_failed ~(store : Store.t) ~(id : string) ~(error : string)
    ~(max_attempts : int) : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    (* If attempts >= max_attempts → dead; else pending with backoff. *)
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let sql =
      "UPDATE jobs SET status = CASE WHEN attempts >= $3 THEN 'dead' \
                                ELSE 'pending' END, \
       error = $2, run_at = CASE WHEN attempts >= $3 THEN run_at \
                                  ELSE $4 END \
       WHERE id = $1::uuid"
    in
    ignore (Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string id
                ; Pgx.Value.of_string error
                ; Pgx.Value.of_int max_attempts
                ; Pgx.Value.of_int64
                    (Int64.add now_ms (Int64.of_int (expt_backoff store id)))
                ]
              sql)

(* ---- dead-runner recovery: fail stale 'running' rows from dead runners ---- *)

let reap_dead ~(store : Store.t) : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    (* Any row status=running whose runner_id != our boot runner_id is
       stale (the image crashed mid-eval). Requeue with backoff or dead. *)
    let sql =
      "UPDATE jobs SET status = CASE WHEN attempts >= max_attempts THEN 'dead' \
                                      ELSE 'pending' END, \
       error = 'runner died: ' || runner_id \
       WHERE status = 'running' AND runner_id <> $1"
    in
    ignore (Store_db.exec conn ~params:[ Pgx.Value.of_string runner_id ] sql)

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
    with _ -> VList [])

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
    [tz] selects the cron calendar ("utc" default, "local" = process
    zone). Note: while the old None-fallback re-arms at now+60s, the
    honest failure for a cron that matches nothing is DISABLE — the
    every-minute refire loop is filed as i-1789100668; fixing it here
    once the semantics are agreed (disable vs park). *)
let advance_schedule ~(store : Store.t) ~(name : string) ~(fired_at : int64)
    ~(cron : string option) ~(every_ms : int64 option) ?(tz = "utc") : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
    let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
    let next =
      match cron, every_ms with
      | Some c, _ ->
          (match next_cron_for_tz tz c now_ms with
           | Some n -> n
           | None -> Int64.add now_ms 60_000L)
      | None, Some e -> next_every_ms e now_ms
      | None, None -> Int64.add now_ms 60_000L
    in
    ignore
      (Store_db.exec conn ~params:[ Pgx.Value.of_string name; Pgx.Value.of_int64 next ]
         "UPDATE schedules SET next_run = $2 WHERE name = $1")

(* The runner loop (eval_job + runner_loop) lives in boot.ml to avoid a
   dependency cycle: Jobs (DB ops) must not depend on Session, which
   depends on Prims_db, which depends on Jobs. boot.ml can depend on
   both Jobs and Session. *)
