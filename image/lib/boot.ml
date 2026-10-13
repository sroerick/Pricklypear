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

(* promote-window task 7 (2026-09-04): boot-phase timing. The t0 is a
   module initializer, so it fires at program start (before Eio_main.run) —
   effectively process start. [Phase.log name] prints one stderr line:
   wall-clock ms since the previous mark + cumulative ms since t0. Grep
   [boot-phase] in image.log for the boot cost table (promote-optimization
   plan, task 7). Migrations are deliberately NOT a phase here — they are
   applied by dev.sh / the promote script BEFORE the image execs, never in
   the image boot path. *)
module Phase : sig
  val log : string -> unit
  val since_boot_ms : unit -> int
end = struct
  let t0 = Unix.gettimeofday ()
  let last = ref t0
  let since_boot_ms () =
    int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0)
  let log name =
    let now = Unix.gettimeofday () in
    let delta = int_of_float ((now -. !last) *. 1000.0) in
    last := now;
    Printf.eprintf "[boot-phase] %-20s %5d ms  (%d ms from start)\n%!"
      name delta (since_boot_ms ())
end

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

(** Security.borg LAW 1 (structure over judgment): PP_AUTH_DISABLED is a
    dev-only escape, but nothing stops it leaking into a prod env file
    (copy-paste, emergency debug then forget) — the image would then
    boot with EVERY gate off and /health would still read green.

    Refuse when the flag is set together with a prod-shaped config
    (a named PP_PROFILE, a PP_BASE_DOMAIN, or a TLS edge header is
    impossible at boot, so those two are the boot-visible signals).
    The check is structural: the bad combination cannot start, so
    remembering to unset the flag is not a safety property.

    PP_AUTH_DISABLED_ALLOW_PROD=1 is the explicit, audible override
    for someone who really means it (still logs a loud warning). *)
let auth_disabled_prod_guard () : unit =
  let auth_disabled =
    match Sys.getenv_opt "PP_AUTH_DISABLED" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false
  in
  if auth_disabled then begin
    let prod_shaped =
      (match Sys.getenv_opt "PP_PROFILE" with
       | Some p when String.trim p <> "" -> Some ("PP_PROFILE=" ^ p)
       | _ ->
           (match Sys.getenv_opt "PP_BASE_DOMAIN" with
            | Some d when String.trim d <> "" ->
                Some ("PP_BASE_DOMAIN=" ^ d)
            | _ -> None))
    in
    match prod_shaped with
    | None -> ()
    | Some why ->
        let override =
          match Sys.getenv_opt "PP_AUTH_DISABLED_ALLOW_PROD" with
          | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
          | _ -> false
        in
        if override then
          Printf.eprintf
            "[boot] WARNING: PP_AUTH_DISABLED=1 with prod-shaped config \
             (%s) — ALL AUTH GATES ARE OFF (explicit override)\n%!" why
        else
          failwith
            (Printf.sprintf
               "refusing to boot: PP_AUTH_DISABLED=1 with prod-shaped \
                config (%s). Every auth gate would be off. Unset \
                PP_AUTH_DISABLED, or set PP_AUTH_DISABLED_ALLOW_PROD=1 \
                to override deliberately." why)
  end

(** Shared setup: RNG, store connect, session. Returns session + ports. *)
let prepare () : Session.t * int * int =
  ignore (Ops_log.info ~component:"boot" "Pricklypear image starting up");
  Printf.eprintf "[boot] Pricklypear image starting up...\n%!";
  auth_disabled_prod_guard ();
  Mirage_crypto_rng_unix.use_default ();
  Random.self_init ();
  let manifest = manifest_from_env () in
  let cfg = Store.config_from_env () in
  let store = Store.make ~cfg ~branch_id:manifest.branch_id () in
  Store.connect store;
  Phase.log "pg-connect";
  ignore (Auth_store.bootstrap_admin_if_empty store);
  Phase.log "bootstrap-admin";
  (* i-1788670058: re-seal legacy plaintext llm_key rows at boot when a
     master key is configured (idempotent). Without a key, keep the
     degraded mode VISIBLE at every boot - never silently "fixed". *)
  if not (Envelope.master_key_present ()) then
    Printf.eprintf
      "[boot] WARNING: PP_LLM_ENVELOPE_KEY unset - llm keys stored PLAINTEXT (i-1788670058)\n%!"
  else ignore (Auth_store.migrate_llm_keys store);
  (* Surface any drift between the OCaml seed-type mirror and the SQL
     authority (tree_seed_types) — non-fatal warning. See i-seed-list-3copies. *)
  (match Store.current_conn store with
   | Some conn -> Tree_store.verify_seed_types conn
   | None -> ());
  Phase.log "seed-types-verify";
  let session = Session.make ~manifest ~store () in
  Phase.log "session-make";
  (* BORGE-1788334666: the one-time additive libs seed +
     boot-identity personal checkout run HERE, once per process, on
     the boot session. Per-identity sessions (Session_registry) build
     cheap and attach directly, never re-seeding system/main. *)
  Session.boot_personal session;
  Phase.log "boot-personal";
  let http_port = int_of_string (env_or "PP_HTTP_PORT" "8080") in
  let repl_port = int_of_string (env_or "PP_REPL_PORT" "7878") in
  Printf.eprintf "[boot] REPL endpoint 127.0.0.1:%d (internal)\n%!" repl_port;
  Printf.eprintf "[boot] user=%s branch=%s version=%s\n%!"
    manifest.username manifest.branch_name manifest.image_version;
  (session, http_port, repl_port)

(** Eio path: prepare + fork REPL fiber + Httpun_eio accept loop.
    Call under Eio_main + Io_eio.with_ctx. *)
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
          ~manifest:t.manifest ~env:t.eval_state.env ~routes:t.routes
          ~is_admin:t.is_admin ~source_ip:None
      in
      (* Bind the current job id on this fiber so the (with-step "name"
         thunk) special form (__step__ prim) scopes its checkpoints to this
         job (borge/jobs.borg step-checkpoints). Outside this binding there
         is no job context and with-step is a plain thunk call. *)
      Jobs.with_current_job_id job.Jobs.id (fun () ->
        (* Jobs run UNBUDGETED (jobs.borg runner, 2026-09-05): the job
           eval gets a max_int-step budget. Request-path evals keep the
           default sync budget (400k); the runaway guard for jobs is
           attempts/backoff + step checkpoints + host ownership. *)
        let v =
          Request_context.with_current ctx (fun () ->
              Session.eval_string_rim_budgeted t call_src
                (Pricklypear_lisp.Eval.make_budget ~limit:max_int ()))
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
  (* tree-time.gc (5.3): periodic retention sweep ON THE JOBS RUNNER. The
     retention worker (policy-as-habitat-data + rollup + DROP PARTITION)
     runs here, gated by wall-clock time so it does not fire every 1s tick.
     Conservative by design: Store.retention_run is a no-op unless a
     whole-class policy row exists (tree_retention_policy), so on a
     policy-free habitat this is a cheap SELECT. Spec: borge/tree-time.borg
     gc + coexistence-plan PHASE 4 ("the retention worker runs on the jobs
     runner"). Interval overridable via PP_RETENTION_SECS for dev/testing. *)
  let retention_interval_s =
    let v = Sys.getenv_opt "PP_RETENTION_SECS" in
    match v with
    | Some s -> int_of_string_opt s |> Option.value ~default:3600
    | None -> 3600
  in
  let last_retention = ref 0.0 in
  let retention_sweep () =
    let now = Unix.gettimeofday () in
    if now -. !last_retention >= float_of_int retention_interval_s then (
      last_retention := now;
      try
        let rs = Store.retention_run session.store ~path_class:None () in
        List.iter
          (fun (pc, d, c) ->
            if d > 0L || c > 0L then
              Printf.eprintf
                "[jobs] retention %s: dropped %Ld window(s), checkpointed %Ld path(s)\n%!"
                pc d c)
          rs
      with exn ->
        Printf.eprintf "[jobs] retention sweep failed: %s\n%!"
          (Printexc.to_string exn))
  in
  let rec loop () =
    (try Jobs.reap_dead ~store:session.store with _ -> ());
    retention_sweep ();
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
                 ~tz:sch.Jobs.s_tz
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
       Io_eio.sleep 1.0;
       loop ())
  in
  Printf.eprintf "[jobs] runner fiber started (runner_id=%s)\n%!" Jobs.runner_id;
  loop ()


let start_eio ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) : unit =
  let session, http_port, repl_port = prepare () in
  (* REPL serves directly on Eio (Eio.Net), no runtime bridge. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        Repl_endpoint.serve ~session ~port:repl_port ()
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

  (* i-20260914-agents-note-async: the living agents-note seed runs in the
     background, once per boot, on its OWN store+session (jobs-runner
     pattern: never race the boot session / REPL env). Sync placement in
     boot_personal paid prod's 40-90s notes tree floor before /health ever
     answered — both 2026-09-14 promote gates rolled back a healthy
     release on it (20347ee 61s, ca2bc21 89s, boot-personal 78/90s).
     Identity + branch match the boot session's post-boot_personal state
     (username/personal), so the note lands exactly where it always has. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        let cfg = Store.config_from_env () in
        let store = Store.make ~cfg ~branch_id:session.manifest.branch_id () in
        Store.connect store;
        let s = Session.make ~manifest:session.manifest ~store () in
        ignore (Session.ensure_personal_branch s);
        Session.checkout_personal s;
        Session.seed_agents_note s
      with exn ->
        Printf.eprintf "[boot] agents-note seed fiber exited: %s\n%!"
          (Printexc.to_string exn));

  (* image.function-stat-ring: flush labeled-eval stats <= every 30s
     instead of a synchronous insert per /api/eval. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        Function_stats.flush_loop ~clock:(Eio.Stdenv.clock env)
          ~store:session.store
      with exn ->
        Printf.eprintf "[boot] function_stats flush fiber exited: %s\n%!"
          (Printexc.to_string exn));
  (* fd gauge (2026-10-02 fd-exhaustion incident): one open-fd sample per
     minute, mirroring /health. The gauge is what makes the next slow fd
     climb visible before accept fails. *)
  Eio.Fiber.fork ~sw (fun () ->
      try Fd_gauge.watch_loop ~clock:(Eio.Stdenv.clock env)
      with exn ->
        Printf.eprintf "[boot] fd-gauge fiber exited: %s\n%!"
          (Printexc.to_string exn));
  (* product-experience.activity-ledger: identity-attributed,
     structure-only event stream (migration 035). Same 031 shape as
     function_stats — the request path pushes to an in-process ring, this
     fiber flushes with plain INSERTs and prunes set-based. Never tree
     datums. *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        Activity_ledger.flush_loop ~clock:(Eio.Stdenv.clock env)
          ~store:session.store
      with exn ->
        Printf.eprintf "[boot] activity_ledger flush fiber exited: %s\n%!"
          (Printexc.to_string exn));
  (* security.borg §the-watched-machine: the lonely-habitat bit. Count the
     HUMAN users once at boot (OFF the request path) and memoize it; when
     there is exactly one human, the admin.content-read ceremony is
     suppressed. PP_LONELY_HABITAT overrides. Best-effort — a DB miss just
     leaves it undetermined (the ceremony stays on). *)
  Eio.Fiber.fork ~sw (fun () ->
      try
        let n =
          match Store.conn_of session.store with
          | None -> None
          | Some conn -> (
              try
                match
                  Store_db.exec conn
                    "SELECT count(*)::text FROM users WHERE user_type='human'"
                with
                | [ [ c ] ] -> (
                    match Pgx.Value.to_string c with
                    | Some s -> (try Some (int_of_string s) with _ -> None)
                    | None -> None)
                | _ -> None
              with _ -> None)
        in
        ignore (Activity_ledger.is_lonely_habitat ~human_count:n ())
      with exn ->
        Printf.eprintf "[boot] lonely-habitat probe exited: %s\n%!"
          (Printexc.to_string exn));
  (* image.per-identity-sessions: HTTP requests run on per-identity
     sessions (own env, own pgx connection); the boot session keeps the
     REPL endpoint + auth lookups. *)
  let registry = Session_registry.create ~boot:session in
  Printf.eprintf "[boot] serving http://0.0.0.0:%d/  (htmx, httpun-eio)\n%!"
    http_port;
  (* server-bind phase fires inside serve_eio the moment the listen socket
     is actually bound (task 6's retry loop can spend up to 10s there). *)
  Http.serve_eio ~session ~registry ~env ~sw ~port:http_port
    ~on_bound:(fun () -> Phase.log "server-bind") ()

(** Legacy name — same as start_eio without env (not used by main). *)
let start () : unit =
  failwith "Boot.start: use Boot.start_eio under Eio_main (image/bin/main.ml)"
