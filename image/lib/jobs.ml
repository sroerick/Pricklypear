(** Durable background jobs — borge/jobs.borg.

    Jobs are dedicated rows (not data_rows): the runner's SKIP LOCKED claim
    needs real status/run_at columns. The runner is a boot-time Eio fiber
    with its own store+session so it doesn't race HTTP on the shared
    session's branch. Guests see rows via prims; the host owns concurrency.

    v1: job table + runner + core prims (submit-job, job-status, jobs,
    cancel-job). Step-checkpoints and schedules tables exist (migration
    015) but their prims are deferred. *)

open Pricklypear_lisp.Ast

(* ---- runner identity — one fresh id per boot ---- *)

let runner_id : string =
  Printf.sprintf "boot-%s" (string_of_int (Unix.getpid ()))

(* ---- field accessors (pgx row helpers, ai_store pattern) ---- *)

let cell_string row i =
  match List.nth_opt row i with
  | Some v -> Pgx.Value.to_string v
  | None -> None

let cell_int row i =
  match cell_string row i with
  | Some s -> (try Some (int_of_string s) with _ -> None)
  | None -> None

let cell_int64 row i =
  match cell_string row i with
  | Some s -> (try Some (Int64.of_string s) with _ -> None)
  | None -> None

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
  Printf.sprintf "(%s %s)" fn args

let row_to_job row : job option =
  try
    let id = cell_string row 0
    and fn = cell_string row 1
    and args = cell_string row 2
    and branch = cell_string row 3
    and submitted_by = cell_string row 4
    and status = cell_string row 5
    and run_at = cell_int64 row 6
    and attempts = cell_int row 7
    and max_attempts = cell_int row 8
    and result = cell_string row 9
    and error = cell_string row 10
    and runner_id = cell_string row 11
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

let job_to_value (j : job) : value =
  let fields =
    Printf.sprintf
      "{\"id\":\"%s\",\"fn\":\"%s\",\"args\":%s,\"branch\":\"%s\",\
       \"submitted_by\":\"%s\",\"status\":\"%s\",\"run_at\":%s,\
       \"attempts\":%d,\"max_attempts\":%d,\"result\":%s,\"error\":\"%s\",\
       \"runner_id\":\"%s\"}"
      j.id j.fn j.args j.branch j.submitted_by j.status
      (Int64.to_string j.run_at)
      j.attempts j.max_attempts
      (match j.result with Some r -> r | None -> "null")
      (String.concat "" (List.map (fun c -> if c = '"' then "\\\"" else String.make 1 c) (String.to_seq j.error |> List.of_seq)))
      j.runner_id
  in
  VStr fields


(* ---- submit ---- *)

let submit ~(store : Store.t) ~(fn : string) ~(args_json : string)
    ~(branch : string) ~(submitted_by : string) ~(run_at_ms : int64) () : string =
  match pg_conn store with
  | None -> "submit-job: no db connection"
  | Some conn ->
    (try
      let sql =
        Printf.sprintf
          "INSERT INTO jobs (fn, args, branch, submitted_by, run_at) \
           VALUES ($1, $2::jsonb, $3, $4, $5) RETURNING id::text"
      in
      let rows =
        Io_lwt.Db.execute conn
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
           (match cell_string row 0 with
            | Some id -> id
            | None -> "submit-job: no id returned")
       | [] -> "submit-job: insert returned no row")
    with exn ->
      "submit-job: " ^ Printexc.to_string exn)

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
      Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string id ] sql
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
    let rows = Io_lwt.Db.execute conn ~params sql in
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
    let rows = Io_lwt.Db.execute conn ~params sql in
    (match rows with
     | row :: _ ->
         (match cell_string row 0 with
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
          Io_lwt.Db.execute conn
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
      Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string id ]
        "SELECT attempts FROM jobs WHERE id = $1::uuid"
    in
    (match rows with
     | row :: _ ->
         (match cell_int row 0 with
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
    ignore (Io_lwt.Db.execute conn
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
    ignore (Io_lwt.Db.execute conn
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
    ignore (Io_lwt.Db.execute conn ~params:[ Pgx.Value.of_string runner_id ] sql)

(* The runner loop (eval_job + runner_loop) lives in boot.ml to avoid a
   dependency cycle: Jobs (DB ops) must not depend on Session, which
   depends on Prims_db, which depends on Jobs. boot.ml can depend on
   both Jobs and Session. *)
