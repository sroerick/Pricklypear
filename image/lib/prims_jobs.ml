(* Jobs + schedules prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  (* ===== Jobs prims (borge/jobs.borg) ===== *)
  (* Guest surface: submit-job, job-status, jobs, cancel-job, step,
     schedule, unschedule, schedules. The runner fiber (boot.ml) claims
     and evals; prims just read/submit. with-step is a special form (eval.ml)
     routed through the __step__ prim in session.ml. *)

  let job_caller () =
    match Request_context.get () with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  let job_is_admin () =
    let uname = job_caller () in
    if uname = "anonymous" then false
    else
      match Auth_store.get_user_by_username t.store uname with
      | Some u -> u.Auth_store.is_admin
      | None -> false
  in

  Primitives.register tbl "submit-job"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr fn;
          args_val ] ->
        let args_json =
          match args_val with
          | Pricklypear_lisp.Ast.VStr s -> s
          | _ -> Pricklypear_lisp.Reader.pp_value args_val
        in
        let caller = job_caller () in
        if caller = "anonymous" then
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job", "denied for anonymous", "denied"))))
        else
          let branch =
            match Request_context.get () with
            | Some c -> (Request_context.manifest c).branch_name
            | None -> t.manifest.branch_name
          in
          let now_ms =
            Int64.of_float (Unix.gettimeofday () *. 1000.0)
          in
          (match
            Jobs.submit ~store:t.store ~fn ~args_json ~branch
              ~submitted_by:caller ~run_at_ms:now_ms ()
          with
          | Ok id -> (Pricklypear_lisp.Ast.VStr id)
          (* prim value contract unchanged: the failure text WAS the
             returned string; only the module boundary is typed now. *)
          | Error msg -> (Pricklypear_lisp.Ast.VStr msg))
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job", "need fn-str, args", "bad-args")))));

  Primitives.register tbl "submit-job-at"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr fn;
          args_val;
          Pricklypear_lisp.Ast.VInt run_at_ms ] ->
        let args_json =
          match args_val with
          | Pricklypear_lisp.Ast.VStr s -> s
          | _ -> Pricklypear_lisp.Reader.pp_value args_val
        in
        let caller = job_caller () in
        if caller = "anonymous" then
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job-at", "denied for anonymous", "denied"))))
        else
          let branch =
            match Request_context.get () with
            | Some c -> (Request_context.manifest c).branch_name
            | None -> t.manifest.branch_name
          in
          (match
            Jobs.submit ~store:t.store ~fn ~args_json ~branch
              ~submitted_by:caller ~run_at_ms ()
          with
          | Ok id -> (Pricklypear_lisp.Ast.VStr id)
          | Error msg -> (Pricklypear_lisp.Ast.VStr msg))
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job-at", "need fn-str, args, run-at-ms", "bad-args")))));

  Primitives.register tbl "job-status"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
        Jobs.get ~store:t.store ~id
          ~caller:(job_caller ()) ~is_admin:(job_is_admin ())
      | _ -> (Pricklypear_lisp.Ast.VNil));

  Primitives.register tbl "jobs"
    (fun _ ->
      Jobs.list ~store:t.store
        ~caller:(job_caller ()) ~is_admin:(job_is_admin ()) ~limit:50 ());

  Primitives.register tbl "cancel-job"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
        Jobs.cancel ~store:t.store ~id
          ~caller:(job_caller ()) ~is_admin:(job_is_admin ())
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("cancel-job", "need job-id-str", "bad-args")))));

  (* (schedule "name" "cron-or-ms" "fn" args [tz]) -> upsert a schedule,
     caller-scoped identity + branch. spec is a 5-field cron or an
     every-ms integer. Optional [tz] is "utc" (default) or "local":
     cron fields match the image process zone (America/Denver in prod).
     Capability: WriteDb (borge/jobs.borg schedules). *)
  let do_schedule ~tz name spec fn args_val =
    let args_json =
      match args_val with
      | Pricklypear_lisp.Ast.VStr s -> s
      | _ -> Pricklypear_lisp.Reader.pp_value args_val
    in
    let caller = job_caller () in
    if caller = "anonymous" then
      (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("schedule", "denied for anonymous", "denied"))))
    else
      let branch =
        match Request_context.get () with
        | Some c -> (Request_context.manifest c).branch_name
        | None -> t.manifest.branch_name
      in
      Jobs.schedule_upsert ~store:t.store ~name ~spec ~fn ~args_json
        ~branch ~submitted_by:caller ?tz ()
  in
  Primitives.register tbl "schedule"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec;
          Pricklypear_lisp.Ast.VStr fn;
          args_val ] ->
        do_schedule ~tz:None name spec fn args_val
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec;
          Pricklypear_lisp.Ast.VStr fn;
          args_val;
          Pricklypear_lisp.Ast.VStr tz ] ->
        do_schedule ~tz:(Some tz) name spec fn args_val
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("schedule", "need name-str, spec-str, fn-str, args, [tz-str]", "bad-args")))));

  Primitives.register tbl "unschedule"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
        Jobs.unschedule ~store:t.store ~name ()
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unschedule", "need name-str", "bad-args")))));

  Primitives.register tbl "schedules"
    (fun _ -> Jobs.list_schedules ~store:t.store ());

  (* ===== EXTERNAL CLAIM SURFACE (borge/jobs.borg, THE FLEET QUESTION) =====

     Until this block existed, `claim-job` was not a thing you could call: the
     queue was multi-claimable in SCHEMA (045: SKIP LOCKED claim, lease,
     expiry-only reclaim, CAS terminal writes) but the only process that could
     exercise it was the in-image runner, whose id was baked into
     Jobs.runner_id. These three prims are the missing verb.

     GATE — SELF OR VERIFIED ADMIN. Claiming picks up work submitted by ANY
     user, and the runner then evals that job's fn on the job's branch, so
     "may claim WHOSE work" is really "may execute whose submitted code".
     That is the whole eligibility question, and it needs NO new column:
     jobs.submitted_by is pinned at submit from the authenticated caller, so
     it already names the owner of the work.

       - non-admin ⇒ eligible only for their OWN rows (Some caller).
         Self-claim is not an escalation: submitting a job already means
         "eval this fn on my branch as me". This is what lets an ordinary
         user's lens worker drain their own queue — the intended ceiling,
         not a concession.
       - verified admin ⇒ unrestricted (None), because that is exactly the
         cross-tenant authority the admin check already grants.

     is_verified_admin is not the session flag on purpose: on prod the SHARED
     BOOT session derives is_admin from username="admin", which is the
     situation it was written for. Fail closed when the row is missing.

     The scope is enforced INSIDE claim_due's SELECT (see the note there) so
     an ineligible row cannot occupy the LIMIT 1 slot and starve the worker. *)
  let worker_owner (tag : string) : string =
    (* Owner id is derived from the AUTHENTICATED caller plus the worker's own
       tag: the image must not accept a self-declared identity, or a worker
       could claim into another worker's lease namespace. Namespacing under
       ext- (Jobs.external_owner) keeps these rows invisible to
       preempt_dead_host_leases, which decides death by probing /proc on THIS
       host and would otherwise declare a remote laptop worker dead. *)
    Jobs.external_owner (Printf.sprintf "%s-%s" (job_caller ()) tag)
  in
  let require_worker ~(op : string) : unit =
    (* Anonymous denial only. Heartbeat and finish need no further gate: both
       are lease CASes keyed on the DERIVED owner id below, so a caller can
       only ever touch a row they already hold. *)
    if job_caller () = "anonymous" then Prims_db_common.deny_anon op
  in
  (* The tenant scope of ONE claim. [None] = unrestricted. This is read fresh
     per call, never cached: the DB row is what decides, so a demotion lands
     on the next claim rather than at the next restart. *)
  let claim_scope ~(op : string) : string option =
    require_worker ~op;
    if Prims_db_common.is_verified_admin ~t () then None
    else Some (job_caller ())
  in
  let as_json (v : Pricklypear_lisp.Ast.value) : string =
    match v with
    | Pricklypear_lisp.Ast.VStr s -> s
    | other -> Pricklypear_lisp.Reader.pp_value other
  in
  (* (claim-job "worker-tag") -> the claimed job row | null.
     null is NOT an error: an empty queue is the normal steady state. *)
  Primitives.register_server_only tbl "claim-job"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr tag ] ->
        let eligible_for = claim_scope ~op:"claim-job" in
        (match
           Jobs.claim_due ~store:t.store ~owner:(worker_owner tag) ~eligible_for
             ()
         with
         | Some j -> Jobs.job_to_value j
         | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
        raise
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.Prim_error
                ("claim-job", "need worker-tag-str", "bad-args"))));

  (* (job-heartbeat id "worker-tag") -> bool.

     false means the lease is GONE — the row finished, or the deadline passed
     and another worker reclaimed it. A worker must treat false as "stop, my
     work is no longer authoritative" and must NOT finish the row. *)
  Primitives.register_server_only tbl "job-heartbeat"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id; Pricklypear_lisp.Ast.VStr tag ] ->
        require_worker ~op:"job-heartbeat";
        Pricklypear_lisp.Ast.VBool
          (Jobs.heartbeat ~store:t.store ~id ~owner:(worker_owner tag) ())
      | _ ->
        raise
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.Prim_error
                ( "job-heartbeat",
                  "need job-id-str, worker-tag-str",
                  "bad-args" ))));

  (* (job-finish id "worker-tag" "done" value)    -> bool
     (job-finish id "worker-tag" "failed" message) -> bool

     Both are lease CASes: the returned bool is the verdict on OWNERSHIP, not
     merely on the write. false = this worker no longer holds the row and its
     result was discarded — a retry already owns the job. That distinction
     cannot be reconstructed by the worker, so it has to come back here.

     max_attempts is deliberately NOT a parameter: the retry budget is a
     property of the row, so a worker cannot inflate its own budget (see
     Jobs.record_failed). *)
  Primitives.register_server_only tbl "job-finish"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id;
          Pricklypear_lisp.Ast.VStr tag;
          Pricklypear_lisp.Ast.VStr outcome;
          value ] ->
        require_worker ~op:"job-finish";
        let owner = worker_owner tag in
        (match outcome with
         | "done" ->
           Pricklypear_lisp.Ast.VBool
             (Jobs.record_done ~store:t.store ?owner:(Some owner) ~id
                ~result_json:(as_json value) ())
         | "failed" ->
           Pricklypear_lisp.Ast.VBool
             (Jobs.record_failed ~store:t.store ?owner:(Some owner) ~id
                ~error:(as_json value) ())
         | other ->
           raise
             (Pricklypear_lisp.Ast.Interp_error
                (Pricklypear_lisp.Ast.Prim_error
                   ( "job-finish",
                     Printf.sprintf
                       "outcome must be \"done\" or \"failed\", got %S" other,
                     "bad-args" ))))
      | _ ->
        raise
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.Prim_error
                ( "job-finish",
                  "need job-id-str, worker-tag-str, outcome-str, value",
                  "bad-args" ))));

