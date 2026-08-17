(** Pricklypear Image — Boot.

    Single-image entry (see [image.boot] / [single-image]):
      1. Build boot manifest from env (or defaults).
      2. Connect store, bootstrap admin if needed.
      3. Load function_defs for the branch into the session env.
      4. Start optional TCP REPL (loopback line protocol) as a fiber.
      5. Start HTTP via Httpun_eio ([start_eio]).

    Env knobs include PP_USERNAME, PP_BRANCH_ID, PP_BRANCH_NAME,
    PP_BOOT_MODE, PP_IMAGE_VERSION, PP_HTTP_PORT, PP_REPL_PORT — set by
    [scripts/dev.sh] or the host service unit. *)

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft

let manifest_from_env () : Pricklypear_common.Manifest.t =
  let boot_mode =
    match env_or "PP_BOOT_MODE" "on-connect" with
    | "always-on" -> Pricklypear_common.Manifest.Boot_always_on
    | _ -> Pricklypear_common.Manifest.Boot_on_connect
  in
  Pricklypear_common.Manifest.make
    ~user_id:(env_or "PP_USER_ID" "dev-user")
    ~username:(env_or "PP_USERNAME" "dev")
    ~branch_id:
      (env_or "PP_BRANCH_ID" "00000000-0000-0000-0000-000000000001")
    ~branch_name:(env_or "PP_BRANCH_NAME" "main")
    ~boot_mode
    ~image_version:(env_or "PP_IMAGE_VERSION" "0.1.0-dev")
    ()

(** Shared setup: RNG, store connect, session. Returns session + ports. *)
let prepare () : Session.t * int * int =
  ignore (Ops_log.info ~component:"boot" "Pricklypear image starting up");
  Printf.eprintf "[boot] Pricklypear image starting up...\n%!";
  Mirage_crypto_rng_unix.use_default ();
  Random.self_init ();
  let manifest = manifest_from_env () in
  let cfg = Store.config_from_env () in
  let store = Store.make ~cfg ~branch_id:manifest.branch_id () in
  Store.connect store;
  ignore (Auth_store.bootstrap_admin_if_empty store);
  (* Surface any drift between the OCaml seed-type mirror and the SQL
     authority (tree_seed_types) — non-fatal warning. See i-seed-list-3copies. *)
  (match Store.current_conn store with
   | Some conn -> Tree_store.verify_seed_types conn
   | None -> ());
  let session = Session.make ~manifest ~store () in
  let http_port = int_of_string (env_or "PP_HTTP_PORT" "8080") in
  let repl_port = int_of_string (env_or "PP_REPL_PORT" "7878") in
  Printf.eprintf "[boot] REPL endpoint 127.0.0.1:%d (internal)\n%!" repl_port;
  Printf.eprintf "[boot] user=%s branch=%s version=%s\n%!"
    manifest.username manifest.branch_name manifest.image_version;
  (session, http_port, repl_port)

(** Eio path: prepare + fork REPL fiber + Httpun_eio accept loop.
    Call under Eio_main + Io_eio.with_ctx + Lwt_eio.with_event_loop. *)
(** Jobs runner loop — lives here (not in Jobs.ml) to avoid a dependency
    cycle: Jobs (DB ops) must not depend on Session, which depends on
    Prims_db, which depends on Jobs. boot.ml can depend on both. *)
let jobs_runner_loop ~(session : Session.t) : unit =
  let eval_job ~job : (string * bool) =
    let t = session in
    let owner, name =
      match String.index_opt job.Jobs.branch '/' with
      | Some i ->
          (String.sub job.Jobs.branch 0 i,
           String.sub job.Jobs.branch (i + 1)
             (String.length job.Jobs.branch - i - 1))
      | None -> ("system", "main")
    in
    (try
      let branch_id =
        match Store.lookup_branch_id_by_name t.store ~owner ~name with
        | Some id -> id
        | None -> Store.system_main_id
      in
      Store.checkout_branch t.store ~new_branch_id:branch_id;
      t.manifest <-
        { t.manifest with
          Pricklypear_common.Manifest.branch_id = branch_id;
          branch_name = job.Jobs.branch;
          username = job.Jobs.submitted_by };
      Session.rehydrate t;
      let call_src = Jobs.eval_src ~fn:job.Jobs.fn ~args:job.Jobs.args in
      (* Wrap eval in a Request_context so crud's user-scoping (visible_sql
         / pp.user RLS floor) sees the job's submitted_by, not 'anonymous'.
         Without this, forge/graph-by-name (a rows scan over forge-graph
         data_rows) returns nothing because the runner looks anonymous. *)
      let ctx =
        Request_context.make
          ~manifest:t.manifest ~env:t.env ~routes:t.routes
      in
      (* Bind the current job id on this fiber so the (with-step "name"
         thunk) special form (__step__ prim) scopes its checkpoints to this
         job (borge/jobs.borg step-checkpoints). Outside this binding there
         is no job context and with-step is a plain thunk call. *)
      Jobs.with_current_job_id job.Jobs.id (fun () ->
        let v =
          Request_context.with_current ctx (fun () ->
            Session.eval_string_rim t call_src)
        in
        let result_json =
          match v with
          | Pricklypear_lisp.Ast.VStr s -> s
          | Pricklypear_lisp.Ast.VNil -> "null"
          | Pricklypear_lisp.Ast.VInt n -> Int64.to_string n
          | Pricklypear_lisp.Ast.VBool b -> if b then "true" else "false"
          | _ -> Printf.sprintf "\"%s\"" (Pricklypear_lisp.Reader.pp_value v)
        in
        (result_json, true))
    with exn ->
      (* Surface the REAL error text (unbound var / arity / prim message)
         instead of the opaque `Interp_error(_)` Printexc prints — see the
         runner agent-note in borge/jobs.borg (campaign 4.1). *)
      let msg =
        match exn with
        | Pricklypear_lisp.Ast.Interp_error e ->
            Pricklypear_lisp.Ast.pp_interp_error e
        | _ -> Printexc.to_string exn
      in
      (msg, false))
  in
  let rec loop () =
    (try Jobs.reap_dead ~store:session.store with _ -> ());
    (* Fire due schedules: submit a job per due schedule, then advance its
       next_run. Downtime collapses to one catch-up run (cron searches
       strictly after now; every-ms just adds the interval). Spec:
       borge/jobs.borg schedules. *)
    (let rec fire_schedules () =
       match Jobs.claim_due_schedule ~store:session.store with
       | None -> ()
       | Some sch ->
           let fired_at =
             Int64.of_float (Unix.gettimeofday () *. 1000.0)
           in
           (try
              ignore
                (Jobs.submit ~store:session.store
                   ~fn:sch.Jobs.s_fn ~args_json:sch.Jobs.s_args
                   ~branch:sch.Jobs.s_branch
                   ~submitted_by:sch.Jobs.s_submitted_by
                   ~run_at_ms:fired_at ());
              Jobs.advance_schedule ~store:session.store
                ~name:sch.Jobs.s_name ~fired_at
                ~cron:sch.Jobs.s_cron ~every_ms:sch.Jobs.s_every_ms
            with _ -> ());
           fire_schedules ()
     in
     try fire_schedules () with _ -> ());
    let claimed = (try Jobs.claim_due ~store:session.store with _ -> None) in
    (match claimed with
     | Some job ->
       (try
          let result, ok = eval_job ~job in
          if ok then
            Jobs.record_done ~store:session.store ~id:job.Jobs.id
              ~result_json:result
          else
            Jobs.record_failed ~store:session.store ~id:job.Jobs.id
              ~error:result ~max_attempts:job.Jobs.max_attempts
        with exn ->
          (try
             Jobs.record_failed ~store:session.store ~id:job.Jobs.id
               ~error:(Printexc.to_string exn)
               ~max_attempts:job.Jobs.max_attempts
           with _ -> ()));
       loop ()
     | None ->
       Lwt_eio.run_lwt (fun () -> Lwt_unix.sleep 1.0);
       loop ())
  in
  Printf.eprintf "[jobs] runner fiber started (runner_id=%s)\n%!" Jobs.runner_id;
  loop ()


let start_eio ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) : unit =
  let session, http_port, repl_port = prepare () in
  (* REPL still uses Lwt_unix sockets under Lwt_eio. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        Lwt_eio.run_lwt (fun () ->
            Repl_endpoint.serve ~session ~port:repl_port ())
      with exn ->
        Printf.eprintf "[boot] REPL fiber exited: %s\n%!"
          (Printexc.to_string exn));

  (** Fork the jobs runner fiber — a background loop that claims due
      jobs and evals them. It gets its own store+session so it doesn't
      race HTTP on the shared session's branch. Spec: borge/jobs.borg. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        let cfg = Store.config_from_env () in
        let runner_store = Store.make ~cfg ~branch_id:Store.system_main_id () in
        Store.connect runner_store;
        let manifest =
          Pricklypear_common.Manifest.make
            ~user_id:"jobs-runner"
            ~username:"admin"
            ~branch_id:Store.system_main_id
            ~branch_name:"system/main"
            ~boot_mode:Pricklypear_common.Manifest.Boot_always_on
            ~image_version:session.manifest.image_version
            ()
        in
        let runner_session = Session.make ~manifest ~store:runner_store () in
        jobs_runner_loop ~session:runner_session
      with exn ->
        Printf.eprintf "[boot] jobs runner fiber exited: %s\n%!"
          (Printexc.to_string exn));

  Printf.eprintf "[boot] serving http://0.0.0.0:%d/  (htmx, httpun-eio)\n%!"
    http_port;
  Http.serve_eio ~session ~env ~sw ~port:http_port ()

(** Legacy name — same as start_eio without env (not used by main). *)
let start () : unit =
  failwith "Boot.start: use Boot.start_eio under Eio_main (image/bin/main.ml)"
