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

  (* (schedule "name" "cron-or-ms" "fn" args) -> upsert a schedule,
     caller-scoped identity + branch. spec is a 5-field cron or an
     every-ms integer. Capability: WriteDb (borge/jobs.borg schedules). *)
  Primitives.register tbl "schedule"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec;
          Pricklypear_lisp.Ast.VStr fn;
          args_val ] ->
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
            ~branch ~submitted_by:caller ()
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("schedule", "need name-str, spec-str, fn-str, args", "bad-args")))));

  Primitives.register tbl "unschedule"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
        Jobs.unschedule ~store:t.store ~name ()
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unschedule", "need name-str", "bad-args")))));

  Primitives.register tbl "schedules"
    (fun _ -> Jobs.list_schedules ~store:t.store ());
