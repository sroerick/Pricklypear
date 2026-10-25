(** Pricklypear Image — session state for eval / HTTP / REPL.

    Holds the env (function defs for the current branch, plus any
    (define)'d locals), the boot manifest (default identity), auth
    attachment fields, and the store handle.

    Per spec [image.repl-experience]:
      - (define ...) persists to the store: survives image reboot.
      - REPL-local let bindings are session-local: NOT persisted.
      - Switching branch (checkout) keeps REPL locals but changes visibility. *)
include Session_types

(** Run direct-style eval under the prim rim (H1.7). *)
let eval_under_rim (t : t) (thunk : unit -> 'a) : 'a =
  Control_rim.run ~prims:t.eval_state.prims thunk

let persist_budget_stats (t : t) ~(budget : Pricklypear_lisp.Eval.budget)
    ~(label : string) : unit =
  (* The budget object is passed explicitly (i-1788670103-5): reading the
     [last_budget] global here raced across request fibers — while this
     eval yielded in a DB prim, another request's eval entry re-set the
     global and this persistence would have billed ITS counters under
     OUR label. *)
  let b = budget in
  if label = "" then ()
  else
    (try
       let fields =
         Yojson.Safe.to_string
           (`Assoc
              [
                ("fn", `String label);
                ("steps", `Int b.steps);
                ("prim_calls", `Int b.prim_calls);
                ("db_hits", `Int b.db_hits);
                ("when", `String (string_of_float (Unix.gettimeofday ())));
              ])
       in
       (* image.function-stat-ring: push, no synchronous DB write. *)
       Function_stats.push ~fn:label ~fields_json:fields
         ~author:t.manifest.username;
       (* product-experience.activity-ledger: the by-ACTOR view of the
          same event. push is a mutex + queue op only (migration 035).
          Target is the function label, a PATH, never a body; cost is the
          budget object. NOTE: never pass the raw auth_token as
          session_id — it is a bearer secret; the ledger carries no
          session id until a non-secret one exists. *)
       let () =
         ignore
           (Activity_ledger.push ~actor:t.manifest.username ~cls:"api.eval"
              ~target:(Activity_ledger.path_target label) ~outcome:"ok"
              ~cost_json:
                (Activity_ledger.budget_cost_json ~steps:b.steps
                   ~prim_calls:b.prim_calls ~db_hits:b.db_hits ~db_ms:b.db_ms)
              ~branch_id:(Store.current_branch_id t.store) ())
       in
       ()
     with _ -> ())

(* Budgeted variant (jobs.borg runner, 2026-09-05): the job runner passes
   a max_int-step budget so long-running ingest/analytics ticks (400+ DB
   ops) don't die on the 100k request budget. Same rim + label logic as
   eval_string_rim; callers that don't care use eval_string_rim. ~env
   evaluates in an explicit env — call_handler passes the per-request
   snapshot env so the handler lookup and the eval cannot diverge when a
   concurrent rehydrate swaps t.eval_state.env mid-request. *)
let eval_string_rim_budgeted (t : t) ?env (input : string)
    (budget : Pricklypear_lisp.Eval.budget) : Pricklypear_lisp.Ast.value =
  (* Publish this request's eval budget to the /timing fiber slot
     (i-1788670103-5) BEFORE the eval so a failed handler still samples
     its partial DB accumulators. *)
  Timing.note_eval_budget budget;
    let v =
      eval_under_rim t (fun () ->
          Pricklypear_lisp.Eval.eval_string_direct
            ~budget ~prims:Pricklypear_lisp.Eval.empty_prims
            (Option.value env ~default:t.eval_state.env) input)
  in
  (* Best-effort label: top-level (name) call *)
  let label =
    let s = String.trim input in
    if String.length s >= 3 && s.[0] = '(' && s.[String.length s - 1] = ')' then
      let inner = String.sub s 1 (String.length s - 2) |> String.trim in
      match String.split_on_char ' ' inner with
      | name :: _ when name <> "" && name.[0] <> '(' -> name
      | _ -> ""
    else ""
  in
  (* image.product-theme: shell evals cause ZERO DB WRITES. The shell
     refresh runs in the request gate; persisting budget stats for it
     would turn every shell re-render into a serialized Crud.create
     under the pgx sequencer. Skip stats for shell labels. *)
  if label <> "" && label <> "ui/css" && label <> "ui/nav"
     && label <> "ui/home" && label <> "ui/theme"
  then persist_budget_stats t ~budget ~label;
  v

let eval_string_rim (t : t) ?env (input : string) : Pricklypear_lisp.Ast.value =
  eval_string_rim_budgeted t ?env input (Pricklypear_lisp.Eval.make_budget ())

(* habitat-book.eval-examples: shared engine for the (example-eval …)
   prim and the POST /book/eval-example endpoint. Restriction lives in
   Example_eval (whitelisted prim table + static gate + child env +
   fresh budget); known_fn comes from the branch's visible defs — the
   same body source forge-effects uses. *)
let example_eval_engine (t : t) (src : string) : Pricklypear_lisp.Ast.value =
  let known_fn =
    Pricklypear_lisp.Effect.known_fn_of_bindings
      (List.filter_map
         (fun (n, body, _) ->
           (* Opaque-body contract (i-1788671083-c): parse at the eval
              boundary; malformed rows skipped as before. *)
           match Pricklypear_lisp.Ast.expr_of_body_json body with
           | Some e -> Some (n, e)
           | None -> None)
         (Store.list_function_names t.store))
  in
  Example_eval.eval_source ~env:t.eval_state.env ~full_prims:t.eval_state.prims ~known_fn src

let eval_expr_rim (t : t) (expr : Pricklypear_lisp.Ast.expr)
  : Pricklypear_lisp.Ast.value =
  let budget = Pricklypear_lisp.Eval.make_budget () in
  Pricklypear_lisp.Eval.last_budget := Some budget;
  Timing.note_eval_budget budget;
  eval_under_rim t (fun () ->
      (* Fiber-scope the budget (i-1788670103-5): prim bodies resolve
         THIS eval's budget through the scope, even after a yield. *)
      Pricklypear_lisp.Eval.with_scope (Some budget) (fun () ->
          Pricklypear_lisp.Eval.eval
            ~budget ~prims:Pricklypear_lisp.Eval.empty_prims t.eval_state.env expr))

(* card d189fa94 ("[ops] handler: unbound: stream/page-size"): the env a
   lazy thunk body resolves against when FORCED. image.lazy-env-load binds
   every package define as a VThunk and Env.lookup is the single auto-force
   site; call_handler runs the handler lookup + eval in the per-request
   snapshot env (image.per-request-context, cards d93d8292 / 0eaf31d5).
   The force closure itself used to evaluate the body against
   t.eval_state.env — the session's LIVE env — so a thunk-bound package
   handler forced at dispatch escaped the snapshot: a concurrent
   rehydrate / checkout swapping t.eval_state.env mid-request (shared
   anonymous session, attach-time ensure_fresh) made the body's references
   die "unbound: <def>" and auto-file the reliability incident. When a
   request context is bound, force against THAT context's snapshot env;
   otherwise (REPL, boot seed, rehydrate-time forcing) keep the live env —
   the tmp/eval-fast-path freshness semantics (carried-over thunks see
   rebound defs) are unchanged outside requests. *)
let thunk_force_env (t : t) : Pricklypear_lisp.Ast.env =
  match Request_context.get () with
  | Some ctx -> Request_context.env ctx
  | None -> t.eval_state.env

(* The force evaluator load_defs / rehydrate bind for every non-macro
   package define: fiber-scoped budget + scope (i-1788670103-5) under the
   session rim, body resolved through [thunk_force_env]. *)
let force_thunk_body (t : t) (expr : Pricklypear_lisp.Ast.expr)
    : Pricklypear_lisp.Ast.value =
  let b = Pricklypear_lisp.Eval.current_or_fresh () in
  eval_under_rim t (fun () ->
      Pricklypear_lisp.Eval.with_scope (Some b) (fun () ->
          Pricklypear_lisp.Eval.eval ~budget:b
            ~prims:Pricklypear_lisp.Eval.empty_prims (thunk_force_env t) expr))

let load_defs (t : t) : unit =
  let disabled = Store.disabled_package_names t.store in
  let disabled_set = Hashtbl.create 16 in
  List.iter (fun p -> Hashtbl.replace disabled_set p ()) disabled;
  (** Hide when package provenance is disabled, or (legacy untagged) when
      the binding name is under a disabled package prefix. *)
  let name_disabled name =
    Hashtbl.fold
      (fun d _ acc ->
        acc
        || name = d
        || String.starts_with ~prefix:(d ^ "/") name)
      disabled_set false
  in
  let binding_disabled name pkg =
    match pkg with
    | Some p -> Hashtbl.mem disabled_set p || name_disabled name
    | None -> name_disabled name
  in
  let defs = Store.list_function_names_for_load t.store in
  (* Opaque-body contract (i-1788671083-c): rows carry the RAW jsonb body;
     parse here at the interpreter boundary. A malformed row is skipped,
     exactly as the store's old per-row decode guard did. *)
  let defs =
    List.filter_map
      (fun (name, body_json, author, pkg, _doc, branch_id, updated_at) ->
        match Pricklypear_lisp.Ast.expr_of_body_json body_json with
        | Some body -> Some (name, body, author, pkg, branch_id, updated_at)
        | None -> None)
      defs
  in
  (* Kernel prim names already bound as VPrim on rehydrate. Never let a
     stale function_defs row re-poison them (BORGE-1785213238). *)
  let is_kernel_prim name =
    List.exists (fun (n, _) -> n = name) t.kernel_catalog
  in
  let _ =
    List.iter
      (fun (name, body, _author, pkg, branch_id, updated_at) ->
        if binding_disabled name pkg then ()
        else if is_kernel_prim name then begin
          Printf.eprintf
            "[session] load def %s skipped: kernel prim name (shadow blocked)\n%!"
            name;
          ()
        end else
        (* Always (re)bind — parents first, children last → override.
           image.lazy-env-load: non-macro bodies bind as VTHUNKS — the
           body is NOT evaluated at load. First lookup forces (once,
           memoized, failure too) under the session rim + active budget.
           Macros keep the eager path: VMacro construction is pure. *)
        (match body with
              | Pricklypear_lisp.Ast.App
                  (Pricklypear_lisp.Ast.Var "__macro__",
                   [ params_expr; body_expr ]) ->
                  let params =
                    match params_expr with
                    | Pricklypear_lisp.Ast.Lit Pricklypear_lisp.Ast.VNil -> []
                    | Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var p, rest) ->
                        p
                        :: List.filter_map
                             (function
                               | Pricklypear_lisp.Ast.Var s -> Some s
                               | _ -> None)
                             rest
                    | _ -> []
                  in
                  Pricklypear_lisp.Env.define t.eval_state.env name
                    (Pricklypear_lisp.Ast.VMacro (params, body_expr, t.eval_state.env));
              | _ ->
                    let force _env' expr' =
                      (* tmp/eval-fast-path + card d189fa94: the body resolves
                         through force_thunk_body — the per-request snapshot
                         env when one is bound (the same env call_handler
                         looks the handler up and evals in; a mid-request
                         rehydrate swap of t.eval_state.env used to escape
                         here and die "unbound: <def>"), else the session's
                         CURRENT env at force time, not the bind-time
                         capture: top-level def bodies are dynamically
                         scoped to the session env, so a thunk forced after
                         an incremental rehydrate sees the rebound defs
                         (full rehydrate gave the same guarantee by
                         re-thunking everything; this keeps unchanged
                         thunks fresh without rebinding them). The budget
                         is read through the fiber scope (i-1788670103-5):
                         the forcing eval's budget pays, never a
                         concurrently-published request's. *)
                      force_thunk_body t expr'
                    in
                  Pricklypear_lisp.Env.define_thunk t.eval_state.env name ~force body;
                  Hashtbl.replace t.eval_state.thunk_bodies name body))
      defs
  in
  (* Also load view_bindings into the in-memory routes table. The
     recursive-CTE returns ancestors first, so a route defined in both
     the child and parent: child's wins (Hashtbl.replace on the
     first-seen only — skip further matches). Skip routes whose handler
     belongs to a disabled package. *)
  let pkg_of_fn = Store.function_package_map t.store in
  let routes = Store.list_routes t.store in
  let seen_r = Hashtbl.create 16 in
  List.iter
    (fun (route, handler, visibility) ->
      let handler_pkg = Hashtbl.find_opt pkg_of_fn handler in
      if binding_disabled handler handler_pkg then ()
      else if not (Hashtbl.mem seen_r route) then begin
        Hashtbl.replace seen_r route ();
        Hashtbl.replace t.routes route (handler, visibility)
      end)
    routes;
  ()

let load_defs_async (t : t) : unit =
  load_defs t;
  (* image.per-identity-sessions: stamp AFTER a successful load so the
     attach-time staleness check has a baseline. *)
  t.eval_state.load_stamp <- Store.visible_stamp t.store

(** Clear env + routes and reload from the store for the current branch.
    Re-binds kernel prims from [t.kernel_catalog] first so checkout does
    not lose +/define/load-library/etc. *)
let rehydrate (t : t) : unit =
  (* image.http-robustness invariant 4a: build a FRESH env record and
     SWAP t.eval_state.env to it. Do NOT Hashtbl.clear the old env's bindings —
     VLambda closures capture `env` by reference, so clearing in place
     would empty the binding table an in-flight page (holding the old
     env via its captured closures) is still reading. The old record
     stays alive for any closure that captured it; new requests see
     the fresh env. This makes concurrent rehydrate safe to run
     alongside lock-free page work (invariant 4b). *)
  let fresh =
    match t.eval_state.env.parent with
    | None -> Pricklypear_lisp.Env.create ()
    | Some p -> Pricklypear_lisp.Env.create ~parent:(Some p) ()
  in
  t.eval_state.env <- fresh;
  Hashtbl.clear t.routes;
  List.iter
    (fun (name, _) ->
      Pricklypear_lisp.Env.define t.eval_state.env name
        (Pricklypear_lisp.Ast.VPrim name))
    t.kernel_catalog;
  (* First-class HOF special forms (i-1790560913) — same env-only bind as
     session make; checkout rehydrate must not lose them. *)
  List.iter
    (fun n ->
      Pricklypear_lisp.Env.define t.eval_state.env n
        (Pricklypear_lisp.Ast.VPrim n))
    [ "apply"; "list/map"; "list/filter"; "list/foldl"; "list/foldr" ];
  Pricklypear_lisp.Env.define t.eval_state.env "load-string"
    (Pricklypear_lisp.Ast.VPrim "load-string");
  Pricklypear_lisp.Env.define t.eval_state.env "eval-string"
    (Pricklypear_lisp.Ast.VPrim "eval-string");
  Pricklypear_lisp.Env.define t.eval_state.env "load-library"
    (Pricklypear_lisp.Ast.VPrim "load-library");
  (* lisp-interpreter.math: pi is a bound value, not a zero-arg prim.
     Rebind on every rehydrate so /api/eval sessions see it. *)
  Pricklypear_lisp.Env.define t.eval_state.env "pi"
    (Pricklypear_lisp.Ast.VFloat Float.pi);
  load_defs t;
  t.eval_state.load_stamp <- Store.visible_stamp t.store

(** tmp/eval-fast-path: incremental rehydrate — drain the process-global
    Def_journal (all in-process def writes funnel through it) and rebind
    exactly the defs whose effective row changed since this session's
    last check, instead of re-fetching + re-parsing the whole visible
    chain (~2800 bodies, ~280-520ms measured).

    Contract identical to [rehydrate]: fresh env record SWAPPED in
    (invariant 4a — the old record is never mutated; in-flight closures
    keep their captured env), routes rebuilt when dirty, stamp
    refreshed. Unchanged defs are carried over by SHALLOW-COPYING the
    old binding table (values shared). VThunk bodies resolve against the
    session's CURRENT env at force time (dynamic, see load_defs), so
    carried-over thunks see rebound defs and never go stale. Macros are
    re-fetched + re-bound whenever they change, capturing the fresh env.

    The def_gen stamp is the BACKSTOP: journal coverage is in-process
    only. Stamp changed + journal has entries for this chain → journal
    applies. Stamp changed + journal overflow / no chain entries
    (out-of-band write, ring overflow, another image on the DB) → full
    [rehydrate], which is always correct. *)
let rehydrate_incremental (t : t) : unit =
  let full () =
    let () = rehydrate t in
    t.eval_state.def_journal_seq <- Def_journal.current_epoch ()
  in
  let delta = Def_journal.since t.eval_state.def_journal_seq in
  if delta.Def_journal.overflow then full ()
  else
    (try
       let chain = Store.branch_chain t.store ~branch_id:t.store.Store_db.branch_id in
       let in_chain b = List.mem b chain in
       let ups =
         List.filter (fun (b, _) -> in_chain b) delta.Def_journal.ups in
       let rems =
         List.filter (fun (b, _) -> in_chain b) delta.Def_journal.rems in
       let routes_dirty = delta.Def_journal.routes_dirty in
       (* nothing for this chain: advance the cursor, keep the env *)
       if ups = [] && rems = [] && not routes_dirty then
         t.eval_state.def_journal_seq <- delta.Def_journal.epoch
       else begin
         let old = t.eval_state.env in
         let fresh =
           { Pricklypear_lisp.Ast.bindings = Hashtbl.copy old.bindings;
             parent = old.parent } in
         List.iter
           (fun (_b, name) ->
             Hashtbl.remove fresh.bindings name;
             Hashtbl.remove t.eval_state.thunk_bodies name)
           rems;
         if ups <> [] then begin
           let disabled_set = Hashtbl.create 16 in
           List.iter
             (fun p -> Hashtbl.replace disabled_set p ())
             (Store.disabled_package_names t.store);
           let name_disabled name =
             Hashtbl.fold
               (fun d _ acc ->
                 acc || name = d
                 || String.starts_with ~prefix:(d ^ "/") name)
               disabled_set false in
           let is_kernel_prim name =
             List.exists (fun (n, _) -> n = name) t.kernel_catalog in
           let bodies =
             Store.list_function_bodies_for t.store ~pairs:ups in
           (* Opaque-body contract (i-1788671083-c): parse at the eval
              boundary; malformed rows skipped as the store's old decode
              guard did. *)
           let bodies =
             List.filter_map
               (fun (name, body_json, author, pkg) ->
                 match Pricklypear_lisp.Ast.expr_of_body_json body_json with
                 | Some body -> Some (name, body, author, pkg)
                 | None -> None)
               bodies
           in
           (* a row may legitimately be gone already (removed between
              events) — only bail when we got NONE of what we asked for
              and asked for several *)
           if bodies = [] && List.length ups > 1 then
             failwith "incr: body fetch empty"
           else
             List.iter
               (fun (name, body, _author, pkg) ->
                 let disabled =
                   match pkg with
                   | Some p ->
                       Hashtbl.mem disabled_set p || name_disabled name
                   | None -> name_disabled name in
                 if disabled then Hashtbl.remove fresh.bindings name
                 else if is_kernel_prim name then
                   Printf.eprintf
                     "[session] incr def %s skipped: kernel prim name\n%!"
                     name
                 else
                   match body with
                   | Pricklypear_lisp.Ast.App
                       (Pricklypear_lisp.Ast.Var "__macro__",
                        [ params_expr; body_expr ]) ->
                       let params =
                         match params_expr with
                         | Pricklypear_lisp.Ast.Lit
                             Pricklypear_lisp.Ast.VNil -> []
                         | Pricklypear_lisp.Ast.App
                             (Pricklypear_lisp.Ast.Var p, rest) ->
                             p
                             :: List.filter_map
                                  (function
                                    | Pricklypear_lisp.Ast.Var s -> Some s
                                    | _ -> None)
                                  rest
                         | _ -> []
                       in
                       Pricklypear_lisp.Env.define fresh name
                         (Pricklypear_lisp.Ast.VMacro
                            (params, body_expr, fresh))
                   | _ ->
                         let force _env' expr' =
                           (* Fiber-scoped budget + scope for the nested eval
                              (i-1788670103-5; see load_defs above) + card
                              d189fa94: force resolves through
                              force_thunk_body (per-request snapshot env when
                              one is bound, else the live env). *)
                           force_thunk_body t expr'
                         in
                       Pricklypear_lisp.Env.define_thunk fresh name
                         ~force body;
                       Hashtbl.replace t.eval_state.thunk_bodies name body)
               bodies
         end;
         (* Re-thunk every carried def from the cached body registry:
              resets memoization (a forced def would otherwise keep a
              VLambda capturing the PREVIOUS env — stale deps) and gives
              incremental loads the same freshness semantics as a full
              rehydrate, without re-fetching/re-parsing from the DB. *)
         Hashtbl.iter
           (fun name body ->
             if Hashtbl.mem fresh.bindings name then
                 let force _env' expr' =
                   (* Fiber-scoped budget + scope for the nested eval
                      (i-1788670103-5; see load_defs above) + card d189fa94:
                      force resolves through force_thunk_body. *)
                   force_thunk_body t expr'
                 in
               Pricklypear_lisp.Env.define_thunk fresh name ~force body)
           t.eval_state.thunk_bodies;
         if routes_dirty then begin
           (* rebuild routes into a temp table, then swap contents in
              (no clear-then-refill DB gap for in-flight dispatch) *)
           let disabled_set = Hashtbl.create 16 in
           List.iter
             (fun p -> Hashtbl.replace disabled_set p ())
             (Store.disabled_package_names t.store);
           let name_disabled name =
             Hashtbl.fold
               (fun d _ acc ->
                 acc || name = d
                 || String.starts_with ~prefix:(d ^ "/") name)
               disabled_set false in
           let routes =
             (* handler->package filtering only matters when some package
                is disabled — skips the ~2800-row package map otherwise *)
             if Hashtbl.length disabled_set = 0 then
               Store.list_routes t.store
             else begin
               let pkg_of_fn = Store.function_package_map t.store in
               List.filter
                 (fun (_route, handler, _vis) ->
                   let hp = Hashtbl.find_opt pkg_of_fn handler in
                   not
                     (match hp with
                      | Some p ->
                          Hashtbl.mem disabled_set p || name_disabled handler
                      | None -> name_disabled handler))
                 (Store.list_routes t.store)
             end in
           let seen_r = Hashtbl.create 16 in
           let fresh_routes : (string, string * string) Hashtbl.t =
             Hashtbl.create 64 in
           List.iter
             (fun (route, handler, visibility) ->
               if not (Hashtbl.mem seen_r route) then begin
                 Hashtbl.replace seen_r route ();
                 Hashtbl.replace fresh_routes route (handler, visibility)
               end)
             routes;
           Hashtbl.reset t.routes;
           Hashtbl.iter (fun r v -> Hashtbl.replace t.routes r v) fresh_routes
         end;
         t.eval_state.env <- fresh;
         t.eval_state.def_journal_seq <- delta.Def_journal.epoch;
         t.eval_state.load_stamp <- Store.visible_stamp t.store
       end
     with exn ->
       Printf.eprintf
         "[session] incremental rehydrate failed (%s); full load\n%!"
         (Printexc.to_string exn);
       full ())

(** Rehydrate iff the visible def chain changed since the last load.
    Called by the session registry at attach time; with image.lazy-env-load
    the rehydrate itself is a CTE + thunk binds (no body evals), so the
    whole check stays in the low-ms range. *)
let ensure_fresh (t : t) : unit =
  match Store.visible_stamp t.store with
  | "" -> ()  (* no DB: stay on whatever is in memory *)
  | stamp when stamp = t.eval_state.load_stamp -> ()
  | _ ->
      (* tmp/eval-fast-path: incremental first (binds only changed defs,
         ~ms) with automatic full-load fallback; see rehydrate_incremental. *)
      rehydrate_incremental t

(** Force the next refresh_ui_shell to re-eval ui/nav + ui/css.
    image.product-theme: invalidation is DEPENDENCY-TRACKED — a define
    busts iff the defined name was in the last shell eval's recorded
    read-set (see refresh_ui_shell); no TTL remains. Checkout still
    busts unconditionally (read-sets are per-branch), as do
    load-library / reseed (bulk redefines). *)
let bust_ui_shell_cache (t : t) : unit =
  t.ui_shell.nav_html <- None;
  t.ui_shell.css <- None;
  t.ui_shell.reads <- [];
  t.ui_shell.retry_at <- 0.0

(** image.product-theme: true when [name] was read by the last shell
    eval. Conservative when no read-set exists yet (never refreshed):
    fall back to the old shell-prefix set so first-refresh still
    triggers on shell-name defines. *)
let touches_shell (t : t) (name : string) : bool =
  match t.ui_shell.reads with
  | [] -> name = "ui/nav" || name = "ui/home" || name = "ui/css"
          || name = "ui/theme"
          || String.starts_with ~prefix:"theme/" name
  | reads -> List.exists (String.equal name) reads

(** Switch store + manifest to [branch_id], then rehydrate env. *)
let checkout_to (t : t) ~(branch_id : string) ~(branch_ref : string)
  : unit =
  let () = Store.checkout_branch t.store ~new_branch_id:branch_id in
  t.manifest <-
    { t.manifest with
      Pricklypear_common.Manifest.branch_id = branch_id;
      branch_name = branch_ref };
  (* Drop shell caches — ui/nav and ui/css are branch-visible. Next
     refresh_ui_shell (request launch / load) repopulates from new env.
     Reset the invariant-3 retry/refresh timestamps too. *)
  bust_ui_shell_cache t;
  rehydrate t

(* ---- Control prims (session_control_prims.ml) ----
   The fork/merge/route/checkout/unbind/prune-synced/bench +
   with-transaction/step/with-public-write surface and the branch-ref
   helpers moved verbatim to Session_control_prims (i-1788671109-1).
   Session supplies the env-driver hooks and re-exports the helpers so
   the checkout funnel below (checkout_ref / unoverride / promote /
   fork_ref) and the Session.* API stay unchanged. *)
let split_branch_ref = Session_control_prims.split_branch_ref

let branch_owner_authorized = Session_control_prims.branch_owner_authorized

let branch_readable = Session_control_prims.branch_readable

(** The env-driver seam for the control prims: rehydrate + checkout_to
    are defined above (driver concern) and cannot move behind the
    module-cycle-free boundary. *)
let control_hooks =
  { Session_control_prims.rehydrate = rehydrate;
    Session_control_prims.checkout_to = checkout_to }

let register_session_control_prims (t : t) (tbl : Primitives.t) : unit =
  Session_control_prims.register ~hooks:control_hooks t tbl

(** If model returns a bare expression body, wrap as (lambda (x) ...). *)

(* ---- AI / agent tools (Agent_tools) — re-export for stable Session.* API ---- *)
let define_binding = Agent_tools.define_binding
let install_ai_function = Agent_tools.install_ai_function
let function_source = Agent_tools.function_source
let function_ast = Agent_tools.function_ast
let package_binding_names = Agent_tools.package_binding_names
let package_export_names = Agent_tools.package_export_names
let list_package_names = Agent_tools.list_package_names
let apropos_docs = Agent_tools.apropos_docs
let describe = Agent_tools.describe
let docs_missing = Agent_tools.docs_missing
let app_check = Agent_tools.app_check
let export_package = Agent_tools.export_package
let propose_function = Agent_tools.propose_function
let default_agent_seed = Agent_tools.default_agent_seed
let agent_session_new = Agent_tools.agent_session_new
let agent_session_new_res = Agent_tools.agent_session_new_res
let agent_say = Agent_tools.agent_say
let agent_history = Agent_tools.agent_history
let agent_sessions = Agent_tools.agent_sessions
let agent_session_delete = Agent_tools.agent_session_delete
let agent = Agent_tools.agent
let agent_task = Agent_tools.agent_task
let llm_rewrite = Agent_tools.llm_rewrite
let llm_inner = Agent_tools.llm_inner
let llm_outer = Agent_tools.llm_outer
let llm_seed = Agent_tools.llm_seed
let pp_interp_exn = Agent_tools.pp_interp_exn

let register_media_prims (t : t) (tbl : Primitives.t) : unit =
  Prims_media.register t tbl
let do_http_get = Prims_net.do_http_get

(* Late-bound thunk for route/dispatch prim. call_handler is defined later
   in this module (after `make`); this ref lets the prim registered inside
   make reference it without a forward-let. Bound right after call_handler. *)
let route_dispatch_fn :
  (t -> string -> Session_types.request -> Pricklypear_lisp.Ast.value) ref =
  ref (fun _t _handler _req ->
         Pricklypear_lisp.Ast.VStr "route/dispatch: not initialized")

(* Late-bound thunk for the bumpers M4 verify-route prim (borge/bumpers.
   borg verify-route). Defined after call_handler (which needs the store
   + env + current_request machinery that only exists past `make`); this
   ref lets the prim registered inside make reference it without a
   forward-let, mirroring route_dispatch_fn. Bound right after
   call_handler. *)
let verify_route_fn : (t -> string -> Pricklypear_lisp.Ast.value) ref =
  ref (fun _t _path ->
         Pricklypear_lisp.Ast.VStr "verify-route: not initialized")

let register_net_prims (t : t) (tbl : Primitives.t) : unit =

  Prims_net.register t tbl;
  Net_prims.register t tbl
let promote_to_personal = Prims_db.promote_to_personal
let register_crud_prims (t : t) (tbl : Primitives.t) : unit =
  Prims_db.register ~rehydrate_fn:rehydrate t tbl

let method_to_string = Prims_request.method_to_string
let require_request = Prims_request.require_request
let str_name = Prims_request.str_name
let depth_arg = Prims_request.depth_arg
let record_define_side_effects = Call_graph.record_define_side_effects
let impact_summary_text = Call_graph.impact_summary_text
(** Graph + impact + change-set + DNU prims (image.inspector / dnu-stubs). *)
let register_graph_prims (t : t) (tbl : Primitives.t) : unit =
  Prims_graph.register t tbl
(** Register request-* + redirect prims. Closures capture [t] and read
    [current_request] at call time (handler-scoped dynamic binding). *)
let register_request_prims (t : t) (tbl : Primitives.t) : unit =
  Prims_request.register t tbl
(* Evaluate a single s-expression string as one REPL turn.
   Returns either the resulting value or an error message string.

   Per spec [image.repl-experience]: (define name val) ALSO persists to
   the store: the env binding is in-memory (fast), the store binding
   survives image reboot. The store call is non-fatal on PG-down (the
   env binding still happened).

   Lwt ABI (post-refactor): fork/merge/route/checkout are NORMAL PRIMS
   registered via [register_session_control_prims] — they return their
   real DB result wrapped in a promise. Only [define] keeps an AST-level
   intercept: it's read-level special (the name arg is a Var that must
   not be evaluated), so it can't become a regular prim. *)
(* bumpers M1: read-and-clear the per-eval notice buffer. The ONLY
   intentional drain is api_eval after a successful eval; the reset at
   the top of the next eval_one catches any leftovers. Pending-notice
   producers (M2+: define/route) append to [t.pending_notices].

   bumpers M2 (borge/bumpers.borg notice-budget): the drain caps what it
   serves at the per-response budget (first N win) — the eval response
   is the budget unit, so a begin-wrapped multi-define batch cannot
   burst past 2 notices. The cap shares [Effect.notice_budget], the one
   budget constant (lisp/lib/effect.ml). *)
let take_notices (t : t) : Wire_codec.notice list =
  let ns = t.pending_notices in
  t.pending_notices <- [];
  let rec take n acc = function
    | _ when n = 0 -> List.rev acc
    | x :: rest -> take (n - 1) (x :: acc) rest
    | [] -> List.rev acc
  in
  take Pricklypear_lisp.Effect.notice_budget [] ns

(* bumpers M2 define-notices (borge/bumpers.borg notices-envelope):
   generate + buffer the notices for one interactive top-level define.
   Gated on BULK LOAD/SEED windows via the existing
   [suppress_refresh_on_define] flag — load_string/load_library and the
   boot seed set it for their whole window, and per-define notices there
   would be pure noise (a library load is not a coaching moment). The
   per-session taper drops tip-kinds that already fired; the ≤2
   response budget is enforced at drain ([take_notices]). All of this is
   pure AST work + session state — no tree/notes reads, so nothing here
   touches the notes-tree eval path before /health serves (2026-09-14
   promote incident invariant). *)
let notice_define (t : t) ~(name : string)
    ~(body : Pricklypear_lisp.Ast.expr) ~(doc : string option) : unit =
  if t.ui_shell.suppress_refresh_on_define then ()
  else
    let candidates =
      Pricklypear_lisp.Effect.define_notices ~name ~doc body
    in
    let kept, fired =
      Pricklypear_lisp.Effect.taper_tips ~fired:t.fired_tip_kinds candidates
    in
    t.fired_tip_kinds <- fired;
    let to_wire (n : Pricklypear_lisp.Effect.define_notice) : Wire_codec.notice =
      { Wire_codec.kind = n.kind; name = Some n.name;
        effects = n.effects; text = n.text }
    in
    t.pending_notices <- t.pending_notices @ List.map to_wire kept

(* ---- bumpers M6: (agent-profile 'quiet) durable quiet preference -----
   (borge/bumpers.borg notices-envelope quiet tiers)

   QUIET MODE has three tiers, narrowest wins: (a) the per-request
   {"quiet": true} flag; (b) the automatic session taper (M2's
   once-per-tip-kind); (c) a durable per-identity preference via
   (agent-profile 'quiet), persisted as a user datum of type "profile"
   (tree datum, author = the identity; mirrored path
   data.u.<user>.profile.<id>). The session consults the durable flag in
   api_eval as the third tier: quiet = request_flag || durable. The datum
   body is a tiny JSON {"username": u, "quiet": bool}. Anonymous /
   public identities (shared system/main — no personal garden) stay
   in-memory only, nothing durable. Reads are best-effort: DB-down or no
   row degrades to false (quiet OFF is the safe default — a durable
   preference must never wedge an eval response). Invariant 7 (telemetry
   is not content): this is generated preference state owned by the
   (agent-profile 'quiet) prim, and the per-session read happens ONCE at
   session-make (registry per-identity slot), never on the per-eval
   path — no notes-tree reads at all. *)

let profile_type = "profile"

(** Find the identity's profile datum (type "profile", author = the
    session identity). Returns (id, fields-JSON) of the newest row. *)
let find_profile_datum (t : t) : (string * string) option =
  let uname = t.manifest.username in
  Io_db.Db.with_transaction_low t.store (function
    | None -> None
    | Some conn ->
        let sql =
          "SELECT id::text, fields::text FROM tree_data_rows " ^
          "WHERE type_name = $1 AND author = $2 " ^
          "ORDER BY updated_at DESC LIMIT 1"
        in
        (match
           Store_db.exec conn
             ~params:
               [ Pgx.Value.of_string profile_type; Pgx.Value.of_string uname ]
             sql
         with
         | [ [ id_c; fields_c ] ] -> (
             match
               ( Pgx.Value.to_string id_c, Pgx.Value.to_string fields_c )
             with
             | Some id, Some fields -> Some (id, fields)
             | _ -> None)
         | _ -> None))

(** Set + persist the durable quiet preference for the session identity.
    Anonymous/public stay in-memory only. The persist is best-effort (a DB-down write degrades to the in-memory value and never raises — a
    preference write must not break the eval response that carried it). *)
let set_agent_profile_quiet (t : t) ~(quiet : bool) : unit =
  t.durable_quiet <- quiet;
  let uname = t.manifest.username in
  if uname = "" || uname = "anonymous" || uname = "public" then ()
  else begin
    let fields_json =
      Yojson.Safe.to_string
        (`Assoc [ ("username", `String uname); ("quiet", `Bool quiet) ])
    in
    match find_profile_datum t with
    | Some (id, _) ->
        (try
           let (_ : Pricklypear_lisp.Ast.value) =
             Crud.update_impl t.store ~type_name:profile_type ~id
               ~fields_json ~caller:uname
           in
           ()
         with _ -> ())
    | None ->
        (try
           let (_ : Crud.datum_write) =
             Crud.create_outcome t.store ~type_name:profile_type
               ~fields_json ~author:uname
           in
           ()
         with _ -> ())
  end

(** Load the identity's durable quiet preference from the store. Called
    once at session-make (per-identity registry slot) after the
    personal/agent-branch attach; never per-eval. Best-effort: DB-down or
    no row -> false. Anonymous stays false. *)
let load_agent_profile (t : t) : unit =
  let uname = t.manifest.username in
  if uname = "" || uname = "anonymous" || uname = "public" then
    t.durable_quiet <- false
  else
    match find_profile_datum t with
    | Some (_id, fields_json) ->
        let quiet =
          try
            match Yojson.Safe.from_string fields_json with
            | `Assoc fields -> (
                match List.assoc_opt "quiet" fields with
                | Some (`Bool b) -> b
                | _ -> false)
            | _ -> false
          with _ -> false
        in
        t.durable_quiet <- quiet
    | None -> t.durable_quiet <- false

(** Accessor for api_eval's third quiet tier (tier c: durable
    preference). The request flag (tier a) takes precedence in api_eval;
    tier b (session taper) is automatic via fired_tip_kinds. *)
let durable_quiet (t : t) : bool = t.durable_quiet

(** The (agent-profile …) effective profile JSON: {"username", "quiet"}. *)
let agent_profile_json (t : t) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("username", `String t.manifest.username)
       ; ("quiet", `Bool t.durable_quiet) ])

let rec eval_one (t : t) (input : string) : Pricklypear_lisp.Ast.value =
  (* bumpers M1 (borge/bumpers.borg notices-envelope): per-eval notice
     buffer — reset so each eval_one starts fresh; api_eval drains it
     into the envelope after the value. Notices are strictly OUT OF
     BAND: nothing here ever adds to or reads from the return value, so
     define's contract is untouched. M1 is plumbing only (no producers). *)
  t.pending_notices <- [];
  try
    let expr = Pricklypear_lisp.Reader.read_one input in
    (* Pre-eval intercept: (define name <body>) is the only AST-level
       special case left, because the name argument is a Var that must
       not be evaluated. Everything else (fork/merge/route/checkout)
       dispatches through Eval.apply via the prim_table. *)
    (* package-surface: (package NAME (export ...)) is a directive, not a value. *)
    (match expr with
     | Pricklypear_lisp.Ast.App
         (Pricklypear_lisp.Ast.Var "package", args) ->
         handle_package_form t args
     | _ ->
    let v = eval_string_rim t input in
    let pkg = t.eval_state.current_package in
    let persist_fn name body_expr doc =
      (* spec_delegation: delegated sessions NEVER persist code -
         define/defmacro bind in-env only (data, never for code). *)
      if restricted t then ()
      else begin
        (* Opaque-body contract (i-1788671083-c): the store persists the raw
           jsonb body; serialization happens here at the eval boundary. *)
        let body_json = Pricklypear_lisp.Ast.expr_to_body_json body_expr in
        (* branching.seed-policy MIRROR (2026-10-22): persist always
           upserts. The boot seed (seed_boot window) mirrors system/main
           onto libs/<pkg> code; personal branches stay the override
           layer via closest-chain. *)
        Store.persist_function t.store name body_json ~doc
          ~package:pkg t.manifest.username;
        (* product-experience §activity-ledger: a code write is a
           tree.write event. Target is the b.fn.<name> PATH.
           seed_boot boot loading is not interactive activity — skip
           it (it would flood the ledger with every package define). *)
        if not t.seed_boot then
          ignore
            (Activity_ledger.push ~actor:t.manifest.username
               ~cls:"tree.write"
               ~target:(Activity_ledger.path_target ("b.fn." ^ name))
               ~outcome:"ok"
               ~branch_id:(Store.current_branch_id t.store) ())
      end
    in
    let persist_define name body_expr doc =
      let () =
        if t.manifest.username <> "anonymous" then
          persist_fn name body_expr doc
        else ()
      in
      let () =
        if not t.ui_shell.suppress_refresh_on_define && touches_shell t name
        then begin
          (* Bust only: the next request-gate refresh re-evals. Eager
             refresh here would multiply work per-define during bulk
             loads; invariant-3 retry suppression keeps a broken define
             from blocking the defining request itself. *)
          bust_ui_shell_cache t
        end else ()
      in
      let () = record_define_side_effects t ~name in
      let () = notice_define t ~name ~body:body_expr ~doc in
    v
  in
  (* Persist one top-level-shaped form (define | defmacro) — shared by
     plain top-level eval and the begin/seq spine walk below (log/issues
     "begin-wrapped defines never persist"): defines inside (begin …)
     bound in env via eval_special_form but never landed in the store
      and died with the session. eval_one has no seed/load skip windows
      (boot seed skips via load_skip_names in eval_form only), so
     the only guard here is the anonymous one, as for plain defines. *)
  let rec persist_toplevel (e : Pricklypear_lisp.Ast.expr) : unit =
    match Pricklypear_lisp.Eval.parse_top_define e with
    | Some { name; body; doc } -> ignore (persist_define name body doc)
    | None ->
        (match e with
         | Pricklypear_lisp.Ast.App
             (Pricklypear_lisp.Ast.Var "defmacro",
              [ Pricklypear_lisp.Ast.Var name; params_expr; body_expr ]) ->
             if t.manifest.username <> "anonymous" then begin
               let stored =
                 Pricklypear_lisp.Ast.App
                   (Pricklypear_lisp.Ast.Var "__macro__",
                    [ params_expr; body_expr ])
               in
               persist_fn name stored None
             end
         | Pricklypear_lisp.Ast.App
             (Pricklypear_lisp.Ast.Var ("begin" | "seq"), inner) ->
             (* raw AST args exclude the head (eval_step prepends it at
                dispatch) — inner IS the body list *)
             persist_spine inner
         | _ -> ())
  and persist_spine (forms : Pricklypear_lisp.Ast.expr list) : unit =
    List.iter persist_toplevel forms
  in
    (match Pricklypear_lisp.Eval.parse_top_define expr with
     | Some { name; body; doc } ->
         (* Persist peeled body + docstring. Env bind already ran via eval. *)
         persist_define name body doc
     | None ->
     (match expr with
     | Pricklypear_lisp.Ast.App
         (Pricklypear_lisp.Ast.Var "defmacro",
          [ Pricklypear_lisp.Ast.Var name; params_expr; body_expr ]) ->
         let () =
           if t.manifest.username <> "anonymous" then begin
             let stored =
               Pricklypear_lisp.Ast.App
                 (Pricklypear_lisp.Ast.Var "__macro__",
                  [ params_expr; body_expr ])
             in
             persist_fn name stored None
           end else ()
         in
         v
     | Pricklypear_lisp.Ast.App
         (Pricklypear_lisp.Ast.Var ("begin" | "seq"), forms) ->
         persist_spine forms;
         v
     | _ -> v)))
  with
  | Pricklypear_lisp.Ast.Interp_error e as exn -> raise exn
  | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
      raise (Pricklypear_lisp.Ast.Interp_error
                  (Pricklypear_lisp.Ast.Parse_error (msg, line, col)))
  | exn -> raise exn

(* --- Library loader: same .pp text as in git libs/ --- *)

and handle_package_form (t : t) (args : Pricklypear_lisp.Ast.expr list)
  : Pricklypear_lisp.Ast.value =
  let name_of = function
    | Pricklypear_lisp.Ast.Var s -> Some s
    | Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VStr s) -> Some s
    | _ -> None
  in
  let export_names = function
    | Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "export", xs) ->
        List.filter_map name_of xs
    | _ -> []
  in
  match args with
  | name_e :: rest ->
      (match name_of name_e with
       | None ->
           (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package", "need name symbol/string", "bad-args"))))
       | Some name ->
           let name = String.trim name in
           if name = "" then
             (Pricklypear_lisp.Ast.VStr "package: empty name")
           else if name = "kernel" then
             (Pricklypear_lisp.Ast.VStr "package: reserved name: kernel")
           else begin
             let exports =
               match rest with
               | [ exp_form ] -> export_names exp_form
               | _ ->
                   List.concat_map export_names rest
             in
             (* image.package-form-defines (2026-10-07): forms nested in
                the package body that are NOT export declarations are
                evaluated — historically they were silently dropped, so a
                lib whose defines sat inside (package …) seeded nothing
                but its header ("loaded N form(s)" counted the header)
                and every session inherited unbound handlers. Evaluating
                them keeps wrapped libs working and makes the define-
                interceptor (persist + env bind) see them. Defines at
                top level remain the convention (spec: the libs chapter). *)
             List.iter
               (fun f ->
                 match f with
                 | Pricklypear_lisp.Ast.App
                     (Pricklypear_lisp.Ast.Var "export", _) -> ()
                 | _ -> ignore (eval_form t f))
               rest;
             t.eval_state.current_package <- Some name;
              let () =
                (* spec_delegation: delegated sessions never write the
                   package registry or bindings stamps (code, not data). *)
                if t.manifest.username = "anonymous" || restricted t then ()
                else begin
                 let trace = Sys.getenv_opt "PP_TRACE_LOAD" <> None in
                 let t1 = Unix.gettimeofday () in
                 let () =
                   Store.upsert_package_registry t.store ~name ~exports
                     ~author:t.manifest.username ~enabled_preserve:true ()
                 in
                 let t2 = Unix.gettimeofday () in
                 Store.stamp_package_bindings t.store ~package:name ~exports;
                 let t3 = Unix.gettimeofday () in
                 let () =
                   if trace then
                     Printf.eprintf
                       "[package] %s: registry %.1f ms, stamp %.1f ms\n%!"
                       name ((t2 -. t1) *. 1000.0) ((t3 -. t2) *. 1000.0)
                 in
                 ()
               end
             in
             (Pricklypear_lisp.Ast.VStr
                  (Printf.sprintf "package %s (%d exports)" name
                     (List.length exports)))
           end)
  | [] ->
      (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package", "need name", "bad-args"))))

and eval_form (t : t) (expr : Pricklypear_lisp.Ast.expr)
    : Pricklypear_lisp.Ast.value =
  (* Like eval_one but takes AST — avoids pp_expr string round-trip. *)
  (match expr with
   | Pricklypear_lisp.Ast.App
       (Pricklypear_lisp.Ast.Var "package", args) ->
       handle_package_form t args
   | _ ->
    (* branching.seed-policy MIRROR (2026-10-22): the boot seed upserts
       system/main from libs/<pkg>, so a define whose name already exists
       but whose BODY changed must re-eval and re-persist — the old
       body-agnostic boot-seed-skip (name-present => skip; additive era,
       promote-window task 7 2026-09-04) is gone. Warm-boot cost is now
       carried by the IDENTICAL-body skip: during the seed window
       boot_seed.ml builds load_skip_names once (chain-visible
       fingerprints on system/main) and load_skips below skips defines
       whose normalized body + doc match — a warm boot re-evals only
       what a deploy changed. image.load-library-skip (2026-09-03) gives
       interactive load-library / reseed-package the same medicine;
       changed/new defines keep full eval.
       Safety: no libs/*/lib.pp has a top-level CALL form (scan
       2026-09-04: only define/defmacro/route/deftype/package/export/
       public-route), so a later top-level form in the same load cannot
       need a skipped binding; route/deftype/package forms are NOT
       skipped, and record_define_side_effects already no-ops under the
       seed's suppress_refresh_on_define. Name extraction is by pattern
       only — malformed defines still fall through to eval and raise
       exactly as before. *)
  (* image.load-library-skip (2026-09-03), per-form (shared by the
     top-level expr and the begin/seq spine walk below): inside
     load-library / reseed-package, skip defines whose name resolves
     (closest chain row) to an IDENTICAL normalized body + doc — the
     env binding is already the branch thunk, so eval + persist are
     pure waste. This is what made (load-library "todo") re-evaluate +
     re-persist ~1000 defines per call and blow page/eval budgets
     (budget-exceeded loops). Changed/new defines still eval + persist
      normally. *)
    (* image.load-library-skip self-heal (2026-10-18): skip only when the
       env ACTUALLY binds the name. The map is built from chain-visible
       rows, which load_defs binds — but a parked session's env can lose
       a binding silently (incremental rehydrate applies only journal
       deltas, and "nothing for this chain: keep the env" skips the full
       rebuild). Then every load-library skipped the define forever and
       the session stayed poisoned (prod 10-18: gregor sessions and the
       swarm supervisor died on 'unbound: todo/tree/redash' every tick
       while the store was fine). The check is O(1); a miss costs one
       normal eval + persist, which re-binds and heals. *)
    let load_skips (e : Pricklypear_lisp.Ast.expr) : bool =
      match t.eval_state.load_skip_names with
      | None -> false
      | Some tbl ->
        (* boot seed: env holds only the kernel prim catalog during the
           seed window (route-gate note in session_control_prims), so
           requiring env_binds would disable every boot skip and force a
           full ~2300-define re-eval per warm boot. Safe to relax: the
           post-seed rehydrate (boot_personal: seed -> checkout ->
           rehydrate) rebuilds env from the very store rows this
           fingerprint just matched. The self-heal case (2026-10-18, a
           LIVE load-library on a parked session whose env lost a
           binding) still needs the env check — there seed_boot is
           false. *)
        let env_binds name =
          t.seed_boot || Hashtbl.mem t.eval_state.env.bindings name
        in
        (match Pricklypear_lisp.Eval.parse_top_define e with
         | Some { name; body; doc } ->
             (match Hashtbl.find_opt tbl name with
              | Some stored ->
                  env_binds name
                  && stored =
                       (Pricklypear_lisp.Reader.pp_expr body ^ "\x00"
                        ^ (match doc with Some d -> d | None -> ""))
              | None -> false)
         | None ->
             (match e with
              | Pricklypear_lisp.Ast.App
                  (Pricklypear_lisp.Ast.Var "defmacro",
                   [ Pricklypear_lisp.Ast.Var name; params_expr; body_expr ]) ->
                  let stored_macro =
                    Pricklypear_lisp.Reader.pp_expr
                      (Pricklypear_lisp.Ast.App
                         (Pricklypear_lisp.Ast.Var "__macro__",
                          [ params_expr; body_expr ]))
                  in
                  (match Hashtbl.find_opt tbl name with
                   | Some stored ->
                       env_binds name && stored = stored_macro ^ "\x00"
                   | None -> false)
              | _ -> false))
  in
  let load_skip_hit = load_skips expr in
    if load_skip_hit then Pricklypear_lisp.Ast.VNil
  else
  let v = eval_expr_rim t expr in
  let pkg = t.eval_state.current_package in
  let persist_fn name body_expr doc =
    (* spec_delegation: delegated sessions NEVER persist code -
       define/defmacro bind in-env only. *)
    if restricted t then ()
    else begin
      (* Opaque-body contract (i-1788671083-c): the store persists the raw
         jsonb body; serialization happens here at the eval boundary. *)
      let body_json = Pricklypear_lisp.Ast.expr_to_body_json body_expr in
        (* branching.seed-policy MIRROR (2026-10-22): persist always
           upserts; the seed_boot window carries the rest. *)
        Store.persist_function t.store name body_json ~doc
          ~package:pkg t.manifest.username
    end
  in
  let persist_define name body_expr doc =
    let () =
      if t.manifest.username <> "anonymous" then
        persist_fn name body_expr doc
      else ()
    in
    let () =
      if not t.ui_shell.suppress_refresh_on_define && touches_shell t name
      then begin
        bust_ui_shell_cache t
      end else ()
    in
    let () = record_define_side_effects t ~name in
    let () = notice_define t ~name ~body:body_expr ~doc in
    v
  in
    (* Persist one top-level-shaped form (define | defmacro), honoring the
       load-skip window. Used by the begin/seq spine walk —
     log/issues "begin-wrapped defines never persist (bit me twice)":
     the persist hook only watched the WHOLE top-level form, so defines
     inside (begin …) bound in env but never landed in the store, and
     died with the session. eval_special_form "begin" already evaluated
     each body (env bindings exist); this walks the same spine after
     eval and persists what it finds. Nested begin/seq recurse. *)
  let rec persist_toplevel (e : Pricklypear_lisp.Ast.expr) : unit =
    match Pricklypear_lisp.Eval.parse_top_define e with
    | Some { name; body; doc } ->
          if not (load_skips e) then
          ignore (persist_define name body doc)
    | None ->
        (match e with
         | Pricklypear_lisp.Ast.App
             (Pricklypear_lisp.Ast.Var "defmacro",
              [ Pricklypear_lisp.Ast.Var name; params_expr; body_expr ]) ->
               if not (load_skips e) then begin
               if t.manifest.username <> "anonymous" then begin
                 let stored =
                   Pricklypear_lisp.Ast.App
                     (Pricklypear_lisp.Ast.Var "__macro__",
                      [ params_expr; body_expr ])
                 in
                 persist_fn name stored None
               end
             end
         | Pricklypear_lisp.Ast.App
             (Pricklypear_lisp.Ast.Var ("begin" | "seq"), inner) ->
             (* raw AST args exclude the head (eval_step prepends it at
                dispatch) — inner IS the body list *)
             persist_spine inner
         | _ -> ())
  and persist_spine (forms : Pricklypear_lisp.Ast.expr list) : unit =
    List.iter persist_toplevel forms
  in
  (match Pricklypear_lisp.Eval.parse_top_define expr with
   | Some { name; body; doc } -> persist_define name body doc
   | None ->
   (match expr with
   | Pricklypear_lisp.Ast.App
       (Pricklypear_lisp.Ast.Var "defmacro",
        [ Pricklypear_lisp.Ast.Var name; params_expr; body_expr ]) ->
       let () =
         if t.manifest.username <> "anonymous" then begin
           let stored =
             Pricklypear_lisp.Ast.App
               (Pricklypear_lisp.Ast.Var "__macro__",
                [ params_expr; body_expr ])
           in
           persist_fn name stored None
         end else ()
       in
       v
   | Pricklypear_lisp.Ast.App
       (Pricklypear_lisp.Ast.Var ("begin" | "seq"), forms) ->
       persist_spine forms;
       v
   | _ -> v)))

and load_string (t : t) (src : string) : (string, string) result =
  let prev = t.eval_state.current_package in
  let prev_suppress = t.ui_shell.suppress_refresh_on_define in
  t.eval_state.current_package <- None;
  t.ui_shell.suppress_refresh_on_define <- true;
  Fun.protect
    ~finally:(fun () ->
      t.eval_state.current_package <- prev;
      t.ui_shell.suppress_refresh_on_define <- prev_suppress)
    (fun () ->
      let r = Loader.load_string ~eval_form:(eval_form t) src in
      (* Boot seed holds suppress_refresh across many loads and does one
         explicit refresh at the end — don't thrash ui/css mid-seed. *)
      let () =
        if not prev_suppress then begin
          bust_ui_shell_cache t;
          refresh_ui_shell t
        end
      in
      r)

and load_library (t : t) (name : string) : (string, string) result =
  (* spec_delegation: delegated sessions may load SEEDED packages only -
     libs/<pkg> product code that boot seed already put on system/main.
     Anything else would upsert foreign code onto the owner's branch. *)
  if restricted t
     && not (List.mem name (Boot_seed.discover_seed_packages ()))
  then
    Error
      (Printf.sprintf
         "load-library: delegated or tenant sessions may load seeded packages only \
          (denied: %s)" name)
  else
  let prev = t.eval_state.current_package in
  let prev_suppress = t.ui_shell.suppress_refresh_on_define in
  t.eval_state.current_package <- None;
  t.ui_shell.suppress_refresh_on_define <- true;
  (* image.load-library-skip (2026-09-03): fingerprint the branch's OWN
     stored defs (pp_expr of the stripped body + doc) once per load so
     eval_form can skip re-evaluating + re-persisting unchanged defines.
     Excluded from the map, i.e. full eval path preserved: inherited
     names (persist_function's no-diff-no-pin unpin must stay reachable
     for re-defines of a parent body), kernel-prim shadow names
     (load_defs never binds those rows), and disabled-package names.
     Not built during the boot-seed window: seed_skip already covers the
     seed by name, and this map would rebuild once per seeded package.
     Note: skipped identical defines no longer trigger persist_function's
     unpin sweep on personal branches — (prune-synced) remains the
     contentless-pin sweep. Restored in finally: the skip window is
     exactly this load.

     log/issues "load-library re-persists inherited defs (~0.8s/def)"
     (2026-09-08): the map used to fingerprint only the branch's OWN
     defs, so an UNPINNED (inherited) name with an identical lib body
     still took the full path — re-eval + persist_function, whose
     no-diff-no-pin check is a parent-chain CTE per def (~0.8s each on
     prod; a notes load burned minutes). The map is now fingerprinted
     from the CHAIN-VISIBLE effective rows (list_function_names_for_load,
     ancestors-first replace-fold → closest non-hidden row wins —
     exactly what load_defs binds). Skip safety for the pin cases is
     unchanged: an own pin with a DIFFERENT body is the effective row
     and still misses the map (unpin stays reachable); an own pin with
     the identical body skipped before and still skips. What changed is
     the inherited-only case: env already binds the inherited body and
     persist would be a no-op delete of a nonexistent row — both pure
     waste, now skipped. *)
  let prev_skip = t.eval_state.load_skip_names in
  let () =
      (* boot seed: the mirror-skip map was built ONCE in boot_seed.ml
         for the whole seed window; rebuilding per package would
         re-decode the whole chain ~25x. *)
      if t.seed_boot then ()
    else begin
      let disabled_set = Hashtbl.create 16 in
      List.iter
        (fun p -> Hashtbl.replace disabled_set p ())
        (Store.disabled_package_names t.store);
      let name_disabled name =
        Hashtbl.fold
          (fun d _ acc ->
            acc || name = d || String.starts_with ~prefix:(d ^ "/") name)
          disabled_set false
      in
      let kernel_set = Hashtbl.create 64 in
      List.iter
        (fun (n, _) -> Hashtbl.replace kernel_set n ())
        t.kernel_catalog;
      let tbl = Hashtbl.create 1024 in
      (* list_function_names_for_load is depth DESC (ancestors first),
         so a plain replace-fold lands on the closest visible row —
         the same closest-wins the env binds. Hidden rows never
         clobber a visible ancestor binding; kernel-prim names stay out
         of the map (full eval path preserved). Fingerprint = pp_expr
         body + doc, same shape the own-defs map used. *)
      List.iter
        (fun (name, body_json, _author, pkg, doc, _branch, _updated) ->
          let hidden =
            match pkg with
            | Some p -> Hashtbl.mem disabled_set p || name_disabled name
            | None -> name_disabled name
          in
          match (if hidden || Hashtbl.mem kernel_set name then None
                 else Pricklypear_lisp.Ast.expr_of_body_json body_json) with
          | None -> ()  (* hidden, or malformed row: skip as before *)
          | Some body ->
            Hashtbl.replace tbl name
              (Pricklypear_lisp.Reader.pp_expr body ^ "\x00"
               ^ (match doc with Some d -> d | None -> "")))
        (Store.list_function_names_for_load t.store);
      t.eval_state.load_skip_names <- Some tbl
    end
  in
  Fun.protect
    ~finally:(fun () ->
      t.eval_state.current_package <- prev;
      t.ui_shell.suppress_refresh_on_define <- prev_suppress;
      t.eval_state.load_skip_names <- prev_skip)
    (fun () ->
      (* branching.package-updates ("load-library on non-main: warn that
         it pins the package"): the release file is eval'd ONTO the
         current branch, so a stale libs/<pkg>/lib.pp silently reverts
         in-flight branch pins — the rb dec-2 hazard, hit for real by
         repair scripts on prod (log/issues i-1790493758). Reseed and the
         boot seed do not warn: reseed runs inside with_branch
           system_main_id, and the boot seed sets seed_boot.
         Keyed on the STORE branch (current_branch_id), not the manifest,
         so the with_branch rebind is honored. *)
      let warn_nonmain =
          (not t.seed_boot)
        && Store.current_branch_id t.store <> Store.system_main_id
      in
      let () =
        if warn_nonmain then
          Printf.eprintf "[load-library] %s onto non-main branch %s (pin warning)\n%!"
            name t.manifest.branch_name
      in
      let r = Loader.load_library ~eval_form:(eval_form t) name in
      let r =
        match r with
        | Ok msg when warn_nonmain ->
            Ok
              (Printf.sprintf
                 "%s\nWARNING: loaded onto non-main branch '%s' — bodies that differ \
                  from the parent chain now PIN this branch (the release file \
                  overwrites live work). Shared changes: define then promote; \
                  git → main is (reseed-package \"%s\")."
                 msg t.manifest.branch_name name)
        | r -> r
      in
      let () =
        if not prev_suppress then begin
          bust_ui_shell_cache t;
          refresh_ui_shell t
        end
      in
      r)

    (** branching.seed-policy: upsert of libs/<pkg>/lib.pp. system/main stays
        admin-only (full clobber for names in that file; delegated sessions
        never reseed main - spec_delegation: a code write to system/main;
        data, never for code). Other branches are open to self sessions -
        agents reseed a review branch so a user can look at the code there
        (roerick 10-21: "gate system/main but allow all reseeds to all other
        branches") - but restricted sessions (delegated + tenants) are
        namespace-gated: manifest.username is the OWNER a delegated session
        acts for, so a delegated agent can never edit a stranger's branch
        (roerick 10-21, follow-up). Checked in [resolve] BEFORE the lookup,
        so denial does not leak branch existence. Does not touch personal/dev
      rows. Persist upserts (branching.seed-policy MIRROR 2026-10-22). *)
  and reseed_package (t : t) (pkg : string) (branch : string option) :
      (string, string) result =
    let pkg = String.trim pkg in
    let main_id = Store.system_main_id in
    let resolve () =
      match branch with
      | None -> Ok (main_id, "system/main", true)
      | Some b ->
          let b = String.trim b in
          if b = "system/main" then Ok (main_id, "system/main", true)
          else
            (match String.index_opt b '/' with
             | Some i when i > 0 && i < String.length b - 1 ->
                 let owner = String.sub b 0 i in
                 let name =
                   String.sub b (i + 1) (String.length b - i - 1)
                 in
                   (* roerick 10-21: a delegated session edits only the
                      branches of the user it acts for - manifest.username
                      is the OWNER under spec_delegation - never a
                      stranger's namespace. Checked before the lookup so
                      denial does not leak branch existence. *)
                   if restricted t && owner <> t.manifest.username then
                     Error
                       (Printf.sprintf
                          "reseed-package: '%s' is outside your namespace \
                           (you may only reseed branches of '%s')"
                          b t.manifest.username)
                   else
                     (match
                        Store.lookup_branch_id_by_name t.store ~owner ~name
                      with
                      | Some id when id <> main_id -> Ok (id, b, false)
                      | Some _ -> Ok (main_id, "system/main", true)
                      | None -> Error ("no such branch: " ^ b))
             | _ -> Error ("no such branch: " ^ b))
    in
    match resolve () with
    | Error e -> Error e
    | Ok (branch_id, label, is_main) ->
        if is_main then begin
          (* spec_delegation: delegated sessions never reseed main. *)
          if restricted t then
            (Error "reseed-package: denied for delegated sessions and tenants")
          else if pkg = "" then (Error "empty package name")
          else
            if (not (Prims_db_common.is_verified_admin ~t ()))
               && t.manifest.username <> "admin"
               && t.manifest.username <> "system" then
              (Error "requires admin")
            else reseed_onto t pkg branch_id label
        end
        else if pkg = "" then (Error "empty package name")
        else reseed_onto t pkg branch_id label

  and reseed_onto (t : t) (pkg : string) (branch_id : string) (label : string) :
      (string, string) result =
    let finally () =
      t.ui_shell.suppress_refresh_on_define <- false;
      ()
    in
    Fun.protect ~finally (fun () ->
        Store.with_branch t.store ~branch_id (fun () ->
          let r = load_library t pkg in
          let () = rehydrate t in
          let () = refresh_ui_shell t in
          match r with
          | Ok msg ->
              (Ok (Printf.sprintf "reseeded %s → %s: %s" pkg label msg))
          | Error e -> (Error e)))

and refresh_ui_shell (t : t) : unit =
  (* Product shell: nav HTML + theme CSS (image.product-theme).
     image.http-robustness invariant 3: this runs on every request, so
     a FAILED shell eval must not re-attempt + re-warn per request
     (ui_shell_retry_at suppresses for shell_retry_secs after a throw;
     reset on checkout / success). Emergency chrome fallback is
     unchanged: when ui_nav_html is None, http serves emergency nav.

     image.product-theme: invalidation is DEPENDENCY-TRACKED, not
     TTL-based. A successful shell eval records the binding names it
     read (ui_shell_reads, via Env.lookup_hook, armed only around these
     two evals). A define of a name in that set busts the cache (see
     touches_shell); anything else leaves it. The cache is therefore
     fresh until a real dependency changes — no 60s window serving
     stale CSS, no prefix-guess busting on unrelated defines. Checkout
     and load-library still bust unconditionally. *)
  let shell_retry_secs = 60.0 in
  let now = Unix.gettimeofday () in
  let pp_exn exn =
    match exn with
    | Pricklypear_lisp.Ast.Interp_error e ->
        Pricklypear_lisp.Ast.pp_interp_error e
    | _ -> Printexc.to_string exn
  in
  (* Refresh one shell binding: eval [eval_str], record its read-set,
     cache the rendered string via [set_cached], handle failure with
     retry suppression. *)
  let refresh name eval_str set_cached =
    (* Arm the read-set hook with CAPTURE-RESTORE (i-1788670103-5):
       restoring [None] unconditionally used to drop a CONCURRENT
       shell refresh's collector mid-eval (two identities' sessions
       refresh in parallel), under-busting its cache and serving stale
       CSS/nav after a dependency changed. Reads from OTHER fibers
       still land in this collector while it is armed (no fiber
       identity in [Env.lookup]); that direction is conservative — a
       superset read-set only busts the cache more often, never
       serves stale. *)
    let prev_hook = !Pricklypear_lisp.Env.lookup_hook in
    Pricklypear_lisp.Env.lookup_hook :=
      Some (fun n ->
        if not (List.exists (String.equal n) t.ui_shell.reads) then
          t.ui_shell.reads <- n :: t.ui_shell.reads);
    Fun.protect
      ~finally:(fun () -> Pricklypear_lisp.Env.lookup_hook := prev_hook)
      (fun () ->
        (try
           let v = eval_one t eval_str in
           (match Primitives.is_raw_html v with
            | Some s -> set_cached (Some s)
            | None ->
                (match v with
                 | Pricklypear_lisp.Ast.VStr s -> set_cached (Some s)
                 | _ ->
                     set_cached
                       (Some (Pricklypear_lisp.Reader.pp_value v))));
           t.ui_shell.retry_at <- 0.0;
           ()
         with exn ->
           (* Warn once per retry window, then go quiet. Print the
              full message so the failure is diagnosable. *)
           ignore
             (Ops_log.warn ~component:"session"
                ~context:[("fn", name)]
                ("refresh_ui_shell failed (retry in "
                 ^ string_of_float shell_retry_secs ^ "s): "
                 ^ pp_exn exn));
           set_cached None;
           t.ui_shell.retry_at <- now +. shell_retry_secs;
           ()))
  in
  let () =
    match Pricklypear_lisp.Env.lookup t.eval_state.env "ui/nav" with
    | None -> t.ui_shell.nav_html <- None
    | Some _ ->
        if t.ui_shell.nav_html <> None then
          (* Cache present: serve it (bust clears it first). *)
          ()
        else if now < t.ui_shell.retry_at then
          (* Recently failed: skip re-eval + skip warn. *)
          ()
        else
          refresh "ui/nav" "(ui/nav)" (fun v -> t.ui_shell.nav_html <- v)
  in
  match Pricklypear_lisp.Env.lookup t.eval_state.env "ui/css" with
  | None -> t.ui_shell.css <- None
  | Some _ ->
      if t.ui_shell.css <> None then
        ()
      else if now < t.ui_shell.retry_at then
        ()
      else
        refresh "ui/css" "(ui/css)" (fun v -> t.ui_shell.css <- v)

(* ---- Boot seeding + branch bootstrap (boot_seed.ml) ----
   The additive libs seed, package discovery, the agents-note seed,
   personal/agent branch bootstrap, and the first-load recipes moved
   verbatim to Boot_seed (i-1788671109-1). Session supplies the driver
   ops (load_library / eval_one / rehydrate / checkout_to, all defined
   above) and re-exports the moved names so the Session.* API —
   http.ml's recipe_* faces, Boot.boot_personal, attach_identity,
   loader_test's seed-order pins — is unchanged. *)
let boot_ops =
  { Boot_seed.load_library = (fun t name -> load_library t name);
    Boot_seed.eval_one = (fun t src -> eval_one t src);
    Boot_seed.rehydrate = rehydrate;
    Boot_seed.checkout_to = checkout_to }

let seed_one (t : t) (name : string) : string option =
  Boot_seed.seed_one ~ops:boot_ops t name

let seed_package_priority : string list = Boot_seed.seed_package_priority

let discover_seed_packages : unit -> string list =
  Boot_seed.discover_seed_packages

let seed_agents_note (t : t) : unit = Boot_seed.seed_agents_note ~ops:boot_ops t

let seed_stdlib (t : t) : unit = Boot_seed.seed_stdlib ~ops:boot_ops t

let ensure_personal_branch (t : t) : string option =
  Boot_seed.ensure_personal_branch t

let checkout_personal (t : t) : unit = Boot_seed.checkout_personal ~ops:boot_ops t

let ensure_agent_branch (t : t) ~(parent_username : string) : string option =
  Boot_seed.ensure_agent_branch t ~parent_username

let checkout_agent (t : t) ~(parent_username : string) : unit =
  Boot_seed.checkout_agent ~ops:boot_ops t ~parent_username

let recipe_hello (t : t) : (string, string) result =
  Boot_seed.recipe_hello ~ops:boot_ops t

let recipe_todo (t : t) : (string, string) result =
  Boot_seed.recipe_todo ~ops:boot_ops t

let recipe_bible (t : t) : (string, string) result =
  Boot_seed.recipe_bible ~ops:boot_ops t

let recipe_dabar (t : t) : (string, string) result =
  Boot_seed.recipe_dabar ~ops:boot_ops t

let recipe_notes (t : t) : (string, string) result =
  Boot_seed.recipe_notes ~ops:boot_ops t

let make ~(manifest : Pricklypear_common.Manifest.t) ~(store : Store.t) () : t =
  let env = Pricklypear_lisp.Env.create () in
  let prim_table = Primitives.make_with_defaults () in
  (* Pre-allocate the session record so the DB-touching control prims
     (fork/merge/route/checkout) can capture [t] as a closure (they
     need access to t.store + t.manifest) and register themselves on
     the same prim_table. *)
  let routes = Hashtbl.create 16 in
  let t =
    ref {
      manifest;
      store;
      routes;
      started_at = Unix.gettimeofday ();
      kernel_catalog = [];  (* filled after all prims register *)
      ui_shell =
        { nav_html = None; css = None; reads = []; retry_at = 0.0;
          suppress_refresh_on_define = false };
      eval_state =
        { env;
          load_stamp = "";
          prims = prim_table;  (* temporary; replaced below *)
          current_package = None;
          load_skip_names = None;
          def_journal_seq = 0;
          thunk_bodies = Hashtbl.create 512 };
      seed_boot = false;
      current_request = None;
      agent_branch_lock = false;
      public_dispatch = false;
      delegated = false;  (* spec_delegation: set true by attach_identity
                             when the request carried a delegated on-behalf-of
                             token (auth.delegation-tokens) *)
      is_admin = (manifest.username = "admin");
      auth_token = None;
      impact_queue = [];
      change_transcript = [];
      net_streams = Hashtbl.create 16;
      pending_notices = [];  (* bumpers M1: per-eval notice buffer (plumbing only) *)
      fired_tip_kinds = [];  (* bumpers M2: session taper (once per tip kind per session) *)
      standalone_routes = [];  (* bumpers M3: route annotations that silenced the fragment family *)
      durable_quiet = false;  (* bumpers M6: durable per-identity quiet preference, loaded at attach *)
    }
  in
  (* Register session-aware primitives (whoami, current-author,
     list-bindings). username_fn re-reads t.manifest so per-request
     identity attach updates (whoami) without re-registering. *)
  Primitives.register_session_prims prim_table
    ~username_fn:(fun () ->
      (match Request_context.get () with
       | Some c -> (Request_context.manifest c).username
       | None -> (!t).manifest.username)
    ) ~env;
  (* Register the DB-touching control primitives on the prim_table,
     then rebind t.eval_state.prims to the populated table. These prims capture
     (!t), so they read/write store+manifest on each invocation. *)
  register_session_control_prims !t prim_table;
  (* HTTP request-context prims (it33): request-method/path/param(s)/body +
     redirect. Dynamic scope via current_request while call_handler runs. *)
  register_request_prims !t prim_table;
  (* Call graph + impact + change-set + DNU (image.inspector combo pack). *)
  register_graph_prims !t prim_table;
  (* Function observability: (analyze 'fn) / (explain 'fn). *)
  let name_arg args =
    match args with
    | [ Pricklypear_lisp.Ast.VStr n ] -> Some n
    | [ Pricklypear_lisp.Ast.VSymbol n ] -> Some n
    | _ -> None
  in
  Primitives.register prim_table "analyze" (fun args ->
      match name_arg args with
      | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("analyze", "need function name", "bad-args")))
      | Some name ->
          let author = (!t).manifest.username in
          let user_id =
            let uid = (!t).manifest.user_id in
            if uid = "" || uid = "anonymous" || uid = "public" then None
            else Some uid
          in
          (match
             Analyze.analyze_function (!t).store ~author ~user_id ~fn_name:name
               ~force_llm:true
           with
           | Ok j -> Analyze.result_to_vstr (Ok j)
           | Error _llm_err ->
               (* Fall back to free heuristic when no key / LLM failure. *)
               Analyze.result_to_vstr
                 (Analyze.analyze_function (!t).store ~author ~user_id
                    ~fn_name:name ~force_llm:false)));
  Primitives.register prim_table "explain" (fun args ->
      match name_arg args with
      | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("explain", "need function name", "bad-args")))
      | Some name ->
          (match Analyze.explain_function (!t).store ~fn_name:name with
           | Ok s -> Pricklypear_lisp.Ast.VStr s
           | Error e -> Pricklypear_lisp.Ast.VStr e));
  (* bumpers M6 (borge/bumpers.borg notices-envelope quiet tiers): the
     (agent-profile 'quiet) durable per-identity quiet preference — tier
     (c) of the three-tier quiet mode (request flag > session taper >
     durable datum). Registered here (not prims_agent) because it
     reads/writes Session helpers below the module cycle boundary, the
     same reason analyze/explain/route-dispatch/verify-route are
     inline. Forms:
       (agent-profile)          -> VStr {"username","quiet"}
       (agent-profile 'quiet)   -> VBool current quiet (getter)
       (agent-profile 'quiet #t|#f) -> set + persist, VStr profile
     Anonymous/public identities set in-memory only (nothing durable on
     shared system/main). Classified WriteDb in lisp/lib/effect.ml (it
     CAN write a datum — conservative, like create/update). *)
  Primitives.register prim_table "agent-profile"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VSymbol "quiet" ] ->
          Pricklypear_lisp.Ast.VBool (durable_quiet !t)
      | [ Pricklypear_lisp.Ast.VSymbol "quiet"; Pricklypear_lisp.Ast.VBool b ]
        ->
          set_agent_profile_quiet !t ~quiet:b;
          Pricklypear_lisp.Ast.VStr (agent_profile_json !t)
      | [] -> Pricklypear_lisp.Ast.VStr (agent_profile_json !t)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("agent-profile",
                        "expected () | ('quiet) | ('quiet #t|#f)",
                        "bad-args")))));
  (* Register CRUD primitives (get/create/update/delete/list) over
     the shared data_rows table. Same pattern as control prims:
     they capture (!t) and return their real DB result under the Eio rim. *)
  register_crud_prims !t prim_table;
  (* Network primitives (tcp-connect, stream-read/write/close) —
     single-image.net-prims. *)
  register_net_prims !t prim_table;
  (* Media prims (media-put/meta/list/delete/url) — storage.media.
     Was defined since the media blob store landed but never
     registered (filed as habitat BUG 2026-08-31); (media-meta)
     et al were unbound on every branch. *)
  register_media_prims !t prim_table;
  (* Hypermedia harness: html/* lambdasoup prims + route/dispatch
     (headless htmx swap simulator; spec borge/hypermedia-harness.borg).
     route/dispatch is registered inline here (not in Prims_hyper) so it
     can close over Session.t without a dependency cycle — mirrors
     register_crud_prims above. *)
  Prims_hyper.register_pure_prims prim_table;
  Primitives.register prim_table "route/dispatch"
    (fun args ->
       match args with
       | Pricklypear_lisp.Ast.VStr handler_name
         :: Pricklypear_lisp.Ast.VStr meth
         :: Pricklypear_lisp.Ast.VStr path :: rest ->
         let params =
           match rest with
           | p :: _ -> Prims_hyper.assoc_of p
           | _ -> []
         in
         let headers =
           match rest with
           | _ :: h :: _ ->
             List.map
               (fun (k, v) -> (String.lowercase_ascii k, v))
               (Prims_hyper.assoc_of h)
           | _ -> []
         in
         let body =
           match rest with
           | _ :: _ :: Pricklypear_lisp.Ast.VStr b :: _ -> b
           | _ -> ""
         in
         let method_ : Session_types.http_method =
           match String.lowercase_ascii meth with
           | "post" -> `POST
           | _ -> `GET
         in
         let req : Session_types.request = {
           method_;
           path;
           params;
           body;
           headers;  (* caller controls HX-Request etc. *)
         } in
         (* Invoke call_handler via the late-bound thunk so we don't
            forward-reference it (defined later in this module). *)
         !route_dispatch_fn !t handler_name req
       | _ -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "route/dispatch needs handler method path", "bad-args"))));
  (* route/lookup: resolve a URL path to its handler name via the route
     table. Returns VNil if no route matches. Used by libs/hyper/visit. *)
  Primitives.register prim_table "route/lookup"
    (fun args ->
       match args with
       | [Pricklypear_lisp.Ast.VStr path] ->
         (match Hashtbl.find_opt (!t).routes path with
          | Some (handler, _) -> Pricklypear_lisp.Ast.VStr handler
          | None -> Pricklypear_lisp.Ast.VNil)
       | _ -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "route/lookup needs one path", "bad-args"))));
  (* bumpers M4 verify-route (borge/bumpers.borg verify-route): dry-run
     a route — (verify-route "/path") resolves the handler on the
     session's chain, reports static cost/effects/shape + warnings, then
     invokes it ONCE under the standard step budget. Budget overflow is
     a REPORT FIELD ({kind:"budget-exceeded"}), never a 500. Implemented
     via the late-bound [verify_route_fn] (bound right after
     call_handler) so the prim has no forward reference to the session
     machinery. WriteDb-classified (same executor semantics as
     route/dispatch): denies public-route handlers from probing other
     routes. *)
  Primitives.register prim_table "verify-route"
    (fun args ->
       match args with
       | [Pricklypear_lisp.Ast.VStr path] -> !verify_route_fn !t path
       | _ -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "verify-route needs one path string", "bad-args"))));
  (* AI + list-branches / current-branch registered inside
     register_crud_prims above (they capture t). *)
  (* Make primitives reachable by name in the env, so `(+)` etc.
     resolve. We define each as VPrim <name>; eval.ml's apply already
     dispatches VPrim via lookup_prim. Comes AFTER control prims are
     registered, so fork/merge/route/checkout auto-bind too. *)
  Hashtbl.iter
    (fun name _ ->
      Pricklypear_lisp.Env.define env name (Pricklypear_lisp.Ast.VPrim name))
    prim_table;
  (* First-class HOF special forms (i-1790560913): value position reifies
     them as VPrim values; Eval.apply_step dispatches the five by name.
     NOT in the prim table (head position stays the special form) — this
     is env-only, so the bulk catalog loop above misses them. *)
  List.iter
    (fun n -> Pricklypear_lisp.Env.define env n (Pricklypear_lisp.Ast.VPrim n))
    [ "apply"; "list/map"; "list/filter"; "list/foldl"; "list/foldr" ];
  (* lisp-interpreter.math: pi is a bound value, not a zero-arg prim. *)
  Pricklypear_lisp.Env.define env "pi" (Pricklypear_lisp.Ast.VFloat Float.pi);
  Primitives.register prim_table "load-string"
    (fun args ->
      if (!t).public_dispatch then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("load-string", "denied in public-route handler dispatch", "denied"))));
      match args with
      | [ Pricklypear_lisp.Ast.VStr src ] ->
            let r = load_string !t src in
          (match r with
           | Ok msg -> (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               (Pricklypear_lisp.Ast.VStr ("load-string: " ^ e)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("load-string", "need 1 string", "bad-args")))));
  (* (eval-string src) — evaluate one expression in the current env.
     Must NOT call eval_one/eval_under_rim: this prim already runs under the
     outer Control_rim (route handler /api/eval); nested match_with resumes
     the Prim_call continuation twice → Continuation_already_resumed. *)
  Primitives.register prim_table "eval-string"
    (fun args ->
      if (!t).public_dispatch then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("eval-string", "denied in public-route handler dispatch", "denied"))));
      match args with
      | [ Pricklypear_lisp.Ast.VStr src ]
      | [ Pricklypear_lisp.Ast.VSymbol src ] ->
          Pricklypear_lisp.Eval.eval_string_direct
            ~prims:Pricklypear_lisp.Eval.empty_prims !t.eval_state.env
            ~budget:(Pricklypear_lisp.Eval.make_budget ()) src
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error
                  ("eval-string: need 1 string", "bad-args"))));
  (* habitat-book.eval-examples: (example-eval src) -> VStr (rendered
     value). Parses + evals Nopales source in a RESTRICTED environment:
     whitelisted pure + read-only-DB prim table (absent prims fail
     closed), static gate on mutation special forms + capabilities,
     child of the session env (bindings resolve, nothing persists),
     fresh step budget + wall-clock cap. Read-time eval in the
     reader's session; the POST /book/eval-example endpoint shares
     [example_eval_engine]. *)
  Primitives.register prim_table "example-eval"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr src ]
      | [ Pricklypear_lisp.Ast.VSymbol src ] ->
          let v =
            try Ok (example_eval_engine !t src)
            with Example_eval.Refused reason ->
              Error ("refused: " ^ reason)
          in
          (match v with
           | Ok v -> Pricklypear_lisp.Ast.VStr (Pricklypear_lisp.Reader.pp_value v)
           | Error msg ->
               (raise
                  (Pricklypear_lisp.Ast.Interp_error
                     (Pricklypear_lisp.Ast.User_error
                        ("example-eval: " ^ msg, "denied")))))
      | _ ->
          (raise
             (Pricklypear_lisp.Ast.Interp_error
                (Pricklypear_lisp.Ast.Prim_error
                   ("example-eval", "need 1 source string", "bad-args")))));
  Primitives.register prim_table "load-library"
    (fun args ->
      if (!t).public_dispatch then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("load-library", "denied in public-route handler dispatch", "denied"))));
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          (* load_library busts shell cache + refresh_ui_shell once. *)
          let r = load_library !t name in
          (match r with
           | Ok msg -> (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               (Pricklypear_lisp.Ast.VStr ("load-library: " ^ e)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("load-library", "need 1 string", "bad-args")))));
  (* (reseed-package pkg [branch]) - upsert libs/<pkg> onto the vault.
     No branch arg or "system/main": admin-only (delegated sessions never
     reseed main). Other branches: open to self sessions - agents reseed
     a review branch so a user can look at the code there - while
     delegated and tenant sessions are namespace-gated to their owner's
     own branches (roerick 10-21). *)
  Primitives.register prim_table "reseed-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          (match reseed_package !t pkg None with
           | Ok msg -> (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               (Pricklypear_lisp.Ast.VStr ("reseed-package: " ^ e)))
      | [ Pricklypear_lisp.Ast.VStr pkg; Pricklypear_lisp.Ast.VStr branch ] ->
          (match reseed_package !t pkg (Some branch) with
           | Ok msg -> (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               (Pricklypear_lisp.Ast.VStr ("reseed-package: " ^ e)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("reseed-package", "need package-str [branch-str]", "bad-args")))));
  let final_prims = prim_table in
  let kernel_catalog = Primitives.catalog prim_table in
  (* i-1788670318: boot-time prim-classification assertion. Every
     registered prim must appear in exactly one Effect capability
     bucket. The whole fail-open class of i-1788669715 came from this
     drifting silently — a prim absent from the lists inferred as
     pure and sailed through the publish/AI gates. Fail construction
     (session make runs at boot and per identity) on any drift. *)
  (match Pricklypear_lisp.Effect.classification_errors (List.map fst kernel_catalog) with
   | [] -> ()
   | errs ->
       raise (Failure
                 ("prim classification drift (see lisp/lib/effect.ml): "
                  ^ String.concat "; " errs)));
  (* Mutate the prims field in place (rather than building a fresh
     record via { (!t) with prims = ... }). Closures captured by
     register_session_control_prims / register_crud_prims read
     t.eval_state.prims at CALL time (e.g. __with_transaction__'s Eval.apply
     ~prims:t.eval_state.prims) and need the populated table, not the temporary
     snapshot from step 3. Lands it29: this was latent until
     with-transaction's body needed to call back into the prim
     registry itself. The earlier control prims (fork/merge/route/
     checkout) didn't trip this because they only invoke Store.*
     functions, never Eval.apply against the prims table. *)
  (!t).eval_state.prims <- final_prims;
  (!t).kernel_catalog <- kernel_catalog;
  (* Bind load prims into env (registered after the bulk VPrim bind). *)
  Pricklypear_lisp.Env.define env "load-string" (Pricklypear_lisp.Ast.VPrim "load-string");
  Pricklypear_lisp.Env.define env "eval-string" (Pricklypear_lisp.Ast.VPrim "eval-string");
  Pricklypear_lisp.Env.define env "example-eval" (Pricklypear_lisp.Ast.VPrim "example-eval");
  Pricklypear_lisp.Env.define env "load-library" (Pricklypear_lisp.Ast.VPrim "load-library");
  Pricklypear_lisp.Env.define env "reseed-package" (Pricklypear_lisp.Ast.VPrim "reseed-package");
  let session : t = !t in
  (* BORGE-1788334666: [make] must be PURE CONSTRUCTION — no seed, no
     branch movement, no shell refresh. Per-identity sessions are built
     lazily by Session_registry on first attach; the boot-order steps
     below (seed_stdlib especially, a full 55-package additive libs
     pass) cost seconds-to-minutes and used to run once per identity
     while the registry mutex was held, stalling all HTTP. Boot calls
     [boot_personal] once on the boot session; per-identity sessions
     get their branch via attach_identity's checkout path instead. *)
  session

(** Process-boot personalization + the ONE-TIME additive seed.
    Runs ONCE at image boot on the boot session (Boot.prepare), never
    inside per-identity session construction. Boot order
    (personal-branch safety):
      1. ensure username/personal exists (empty child of system/main)
      2. seed_stdlib writes package defaults ONLY to system/main
      3. checkout personal + rehydrate env (inherits main via CTE)
      4. refresh ui shell from the personal-visible ui/nav
    User (define)s land on personal and are never wiped by reseed. *)
let boot_personal (t : t) : unit =
  (* promote-window task 7 (2026-09-04): per-step ms lines, indented under
     Boot.prepare's [boot-phase] marks (session.ml cannot depend on Boot,
     so these use a local clock — same stderr tag for greppability).
     seed_stdlib's own total + per-package lines are unchanged. *)
  let mark prev name =
    let now = Unix.gettimeofday () in
    Printf.eprintf "[boot-phase]   %-18s %5d ms\n%!"
      name (int_of_float ((now -. prev) *. 1000.0));
    now
  in
  let t0 = Unix.gettimeofday () in
  let _ = ensure_personal_branch t in
  let p = mark t0 "ensure-personal" in
  seed_stdlib t;
  let p = mark p "seed-stdlib" in
  checkout_personal t;
  let p = mark p "checkout-personal" in
  (* i-20260914-agents-note-async: the agents-note seed NO LONGER runs
     synchronously here. Its existence probe (even the cheap notes/doc/row
     form) pays prod's 40-90s notes tree floor, which pushed boot_personal
     to 78s+ and blew BOTH 2026-09-14 promote health gates (20347ee and
     ca2bc21 rolled back while perfectly healthy). The seed is additive,
     once per boot, and only acts when the note is missing — Boot.start_eio
     now runs it on a background fiber (own store+session, jobs-runner
     pattern) AFTER the HTTP path is about to serve. Spec: the boot-order
     contract keeps item 4 (refresh ui shell from personal-visible ui/nav)
     synchronous; the living-agents-note seed moves to the async phase. *)
  refresh_ui_shell t;
  let _ = mark p "ui-shell" in
  ()


let uptime_seconds (t : t) : int =
  int_of_float (Unix.gettimeofday () -. t.started_at)

(** Unix seconds at image boot, surfaced by /health as boot_epoch (promote
    task 4, 2026-09-03): the promote gate requires boot_epoch >= its restart
    stamp so a pre-restart orphan still holding :18080 can't false-OK the
    health gate with the OLD release. Contract: /health runs on the BOOT
    session (Http.serve_eio's [session], created in Boot.prepare at process
    start), so this session's started_at IS image boot time. Per-identity
    sessions attach later and never serve /health. *)
let boot_epoch (t : t) : int = int_of_float t.started_at

let branch_name (t : t) : string = t.manifest.branch_name
let username (t : t) : string = t.manifest.username
(** Snapshot of the attached identity/branch fields for per-request context.
    Record copy is shallow; all fields are immutable strings/ints. *)
let manifest (t : t) : Pricklypear_common.Manifest.t = t.manifest
let is_admin (t : t) : bool = t.is_admin
let store (t : t) : Store.t = t.store
let branch_id (t : t) : string = t.manifest.branch_id

(** Snapshot accessors for per-request context build (image.per-request-context).
    env is swapped by reference on rehydrate (invariant 4a — old record stays
    alive for captured closures), so holding this reference is a snapshot.
    routes is cleared in place on rehydrate, so the caller must copy it via
    Request_context.copy_routes when building the snapshot. *)
let env (t : t) : Pricklypear_lisp.Ast.env = t.eval_state.env

let routes (t : t) : (string, string * string) Hashtbl.t = t.routes
let auth_token (t : t) : string option = t.auth_token

(** Attach authenticated identity for this request (auth.web-login).
    Updates manifest user/username, is_admin, optional raw token.
    When [checkout_personal] is true, ensures + checks out
    username/personal (login / first attach). *)
let attach_identity
    (t : t)
    ~(user_id : string)
    ~(username : string)
    ~(is_admin : bool)
    ?(auth_token : string option = None)
    ?(checkout_personal_now = false)
    ?(user_type : string = "human")
    ?(parent_user_id : string option = None)
    ?(delegated : bool = false)
    ?(actor : string option = None)
    () : unit =
  let prev = t.manifest.username in
  let was_delegated = t.delegated in
  t.manifest <-
    { t.manifest with user_id; username; actor };
  (* spec_delegation (auth.delegation-tokens): a delegated session acts
     for the manifest.username (OWNER) with data grants only — never
     inherits the owner's admin, never persists code, never moves branch.
     The enforcement gates read t.delegated via Session_types.restricted;
     locking the branch here is the 'token resolve path's job' the
     session_types comment reserves for exactly this producer. A session
     returning to SELF attach clears the lock (defensive: the registry
     keys delegated sessions separately, so this transition should never
     fire in production). *)
  t.delegated <- delegated;
  t.is_admin <- (if delegated then false else is_admin);
  if delegated then t.agent_branch_lock <- true
  else if was_delegated then t.agent_branch_lock <- false;
  t.auth_token <- auth_token;
  if checkout_personal_now || prev <> username then
    if user_type = "agent" then
      (* Agent boots into <parent>/agent, not <username>/personal. *)
      (match parent_user_id with
       | Some pid ->
           let parent_opt = Auth_store.get_user_by_id t.store pid in
           (match parent_opt with
            | Some p ->
                let _ = ensure_agent_branch t ~parent_username:p.username in
                checkout_agent t ~parent_username:p.username
            | None ->
                Printf.eprintf
                  "[session] agent %s has no parent; staying on %s\n%!"
                  username t.manifest.branch_name;
                ())
       | None ->
           Printf.eprintf
             "[session] agent %s has no parent_user_id; staying on %s\n%!"
             username t.manifest.branch_name;
           ())
    else
      let _ = ensure_personal_branch t in
      checkout_personal t
  else
    ()

(** Anonymous request identity (auth.route-visibility).
    system/main only; no personal garden; no admin. *)
let attach_anonymous (t : t) : unit =
  t.manifest <-
    { t.manifest with
      user_id = "anonymous"
    ; username = "anonymous"
    ; actor = None  (* spec_delegation: anonymous is always self *)
    };
  t.is_admin <- false;
  t.delegated <- false;
  t.auth_token <- None;
  (* Prefer system/main for package defaults. *)
  let found =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  match found with
  | None -> ()
  | Some id ->
      if t.manifest.branch_id = id then ()
      else checkout_to t ~branch_id:id ~branch_ref:"system/main"

let clear_auth_token (t : t) : unit =
  t.auth_token <- None

(** Resolve owner/name ref and checkout. *)
let checkout_ref (t : t) (branch_ref : string) : (unit, string) result =
  let owner, name = split_branch_ref branch_ref in
  (* i-1787970640: same branch-owner rule as the checkout prim — this is
     the funnel behind http_branches.branches_checkout and
     branches_unoverride. *)
  if not (branch_owner_authorized t owner) then
    (Error (Printf.sprintf "denied: branch %s is owned by %s"
              branch_ref owner))
  else
    let ref' = owner ^ "/" ^ name in
    let found = Store.lookup_branch_id_by_name t.store ~owner ~name in
    match found with
    | None -> (Error "no such branch")
    | Some id ->
        let () = checkout_to t ~branch_id:id ~branch_ref:ref' in
        (Ok ())

(** Delete a function row on the current branch (unoverride). Rehydrates. *)
let unoverride (t : t) (name : string) : (bool, string) result =
  (* i-1787970640: current-branch owner must pass the branch-owner rule
     (same guard as the unbind prim; defense in depth — http_branches
     now aborts on a failed checkout_ref (i-1788670767), but the gate
     also covers every other entry into this path). *)
  let cur_owner, _cur_name = split_branch_ref t.manifest.branch_name in
  if not (branch_owner_authorized t cur_owner) then
    (Error (Printf.sprintf "denied: branch %s is owned by %s"
              t.manifest.branch_name cur_owner))
  else
    let bid = Store.current_branch_id t.store in
    let deleted =
      Store.delete_function_on_branch t.store ~branch_id:bid ~name
    in
    let () = rehydrate t in
    let () = refresh_ui_shell t in
    (Ok deleted)

(** Soft habitat-workflow lint lines for packages touched by [names].
    Best-effort: missing libs/spec → empty string. Never blocks promote.
    Uses empty branch arg so lint does not checkout away from the session. *)
let promote_soft_warnings (t : t) ~(names : string list) : string =
  let pkgs =
    names
    |> List.filter_map (fun n ->
           match String.index_opt n '/' with
           | None -> None
           | Some i when i > 0 -> Some (String.sub n 0 i)
           | Some _ -> None)
    |> List.sort_uniq String.compare
  in
  if pkgs = [] then ""
  else begin
    (* Load lint deps if present; ignore failures (soft). *)
    List.iter
      (fun lib -> ignore (load_library t lib))
      [ "outline"; "notes"; "spec" ];
    let lines =
      List.filter_map
        (fun pkg ->
          let expr =
            Printf.sprintf "(spec/lint-summary %S %S)" pkg ""
          in
          try
            match eval_one t expr with
            | Pricklypear_lisp.Ast.VStr s when String.trim s <> "" ->
                Some (String.trim s)
            | _ -> None
          with _ -> None)
        pkgs
    in
    String.concat " · " lines
  end

(** Promote selected names from source ref onto system/main (admin policy).
    Returns (count, soft_warnings). Soft warnings never block. *)
let promote_to_main (t : t) ~(source_ref : string) ~(names : string list)
  : (int * string, string) result =
  if names = [] then (Error "no names selected")
  else
    let owner, name = split_branch_ref source_ref in
    let src =
      Store.lookup_branch_id_by_name t.store ~owner ~name
    in
    match src with
    | None -> (Error "no such source branch")
    | Some source_id ->
        (* Policy: only admin username may write system/main via promote UI.
           2026-10 security pass: the flag alone is not trustworthy on
           shared-session paths (jobs runner / loopback REPL eval on the
           boot session whose t.is_admin stays boot-derived while the
           manifest flips to the submitter) — recheck users.is_admin. *)
        if (not (Prims_db_common.is_verified_admin ~t ()))
           && t.manifest.username <> "admin"
           && t.manifest.username <> "system" then
          (Error "promote to system/main requires admin")
        else
          let soft = promote_soft_warnings t ~names in
          let n =
            Store.promote_functions t.store
              ~source_id ~target_id:Store.system_main_id ~names
          in
          (* If we're on main, rehydrate so env sees new bodies. *)
          let () =
            if Store.current_branch_id t.store = Store.system_main_id then
              rehydrate t
            else ()
          in
          (Ok (n, soft))

(** Full fork: parent_ref -> owner/child_name (uses Store.fork_branch copy). *)
let fork_ref (t : t) ~(parent_ref : string) ~(child_ref : string)
  : (string, string) result =
  let p_owner, p_name = split_branch_ref parent_ref in
  let c_owner, c_name = split_branch_ref child_ref in
  (* i-1787970640: BOTH sides gate (same as the fork prim, BEFORE the
     lookup so denials don't leak existence). child_owner passes the
     branch-owner rule (blocks pre-claiming another user's garden);
     parent_owner passes the READ rule — system/* stays forkable (the
     UI default "fork from system/main"), other tenants' gardens are
     not. This is the funnel behind http_branches.branches_fork. *)
  if not (branch_owner_authorized t c_owner) then
    (Error (Printf.sprintf "denied: branch %s is owned by %s"
              child_ref c_owner))
  else if not (branch_readable t p_owner) then
    (Error (Printf.sprintf "denied: branch %s is owned by %s"
              parent_ref p_owner))
  else
    let parent_id =
      Store.lookup_branch_id_by_name t.store ~owner:p_owner ~name:p_name
    in
    match parent_id with
    | None -> (Error "no such parent branch")
    | Some pid ->
        let created =
          Store.fork_branch t.store ~parent_id:pid ~owner:c_owner
            ~child_name:c_name
        in
        (match created with
         | None -> (Error "fork failed")
         | Some id -> (Ok id))

(* Look up a route's handler name. Returns None if the route isn't
   registered on this branch. Called by http.ml's request_handler. *)
let lookup_route (t : t) (route : string) : string option =
  match Hashtbl.find_opt t.routes route with
  | Some (handler, _) -> Some handler
  | None -> None

let lookup_route_meta (t : t) (route : string)
  : (string * string) option =
  Hashtbl.find_opt t.routes route

let route_visibility (t : t) (route : string) : string option =
  match Hashtbl.find_opt t.routes route with
  | Some (_, vis) -> Some vis
  | None -> None

let is_anonymous (t : t) : bool = t.manifest.username = "anonymous"

let kernel_catalog (t : t) : (string * string) list = t.kernel_catalog

let ui_nav_html (t : t) : string option = t.ui_shell.nav_html

let set_ui_nav_html (t : t) (s : string option) : unit = t.ui_shell.nav_html <- s

let ui_css (t : t) : string option = t.ui_shell.css

let set_ui_css (t : t) (s : string option) : unit = t.ui_shell.css <- s


let list_pages (t : t) : (string * string) list =
  Hashtbl.fold
    (fun route (handler, _) acc -> (route, handler) :: acc) t.routes []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let list_pages_meta (t : t) : (string * string * string) list =
  Hashtbl.fold
    (fun route (handler, vis) acc -> (route, handler, vis) :: acc) t.routes []
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)

let set_route (t : t) ~(route : string) ~(handler : string)
    ?visibility () : unit =
  let vis =
    match visibility with
    | Some v -> Store.normalize_visibility v
    | None ->
        (match Hashtbl.find_opt t.routes route with
         | Some (_, v) -> v
         | None -> "private")
  in
  Hashtbl.replace t.routes route (handler, vis);
  let _ =
    match visibility with
    | Some v ->
        Store.register_route t.store ~route ~handler
          ~visibility:(Store.normalize_visibility v) ()
    | None -> Store.register_route t.store ~route ~handler ()
  in
  ()

(* card b851a9a0 ("[ops] handler: unbound: stream/drop-n"): a parked
   session's env can silently lose a package binding while the store still
   has it — the 2026-10-18 load_skips self-heal only runs inside
   load-library / reseed-package, and the software factory no longer wraps
   calls in load-library (cddd0a9). Every request through the affected
   handler chain then dies "unbound: <def>" and auto-files a reliability
   incident while the store is fine. Heal at dispatch: when a handler body
   dies Unbound_var for a name the env genuinely lacks, re-bind that def —
   from the cached body registry first (no DB), else one chain-row fetch —
   and retry the handler once. Names the store does not know are
   negatively cached per (branch, name): a typo stays cheap and keeps
   surfacing on every touch. *)
let heal_miss_cache : (string * string, unit) Hashtbl.t = Hashtbl.create 16

let heal_unbound_binding (t : t) (env : Pricklypear_lisp.Ast.env)
    (name : string) : bool =
  let kernel = List.exists (fun (n, _) -> n = name) t.kernel_catalog in
  let bound =
    Pricklypear_lisp.Env.bound_raw env name
    || Pricklypear_lisp.Env.bound_raw t.eval_state.env name in
  if kernel || bound then false
  else
    let rebind body =
      let force _env' expr' = force_thunk_body t expr' in
      Pricklypear_lisp.Env.define_thunk env name ~force body;
      Pricklypear_lisp.Env.define_thunk t.eval_state.env name ~force body;
      Hashtbl.replace t.eval_state.thunk_bodies name body
    in
    match Hashtbl.find_opt t.eval_state.thunk_bodies name with
    | Some body -> rebind body; true
    | None ->
        let key = (t.store.Store_db.branch_id, name) in
        if Hashtbl.mem heal_miss_cache key then false
        else
          let disabled_pkgs = Store.disabled_package_names t.store in
          let name_disabled n =
            List.exists (fun d ->
                n = d || String.starts_with ~prefix:(d ^ "/") n) disabled_pkgs in
          match Store.lookup_function_pkg t.store name with
          | Some (body_json, pkg)
            when (match pkg with
                  | Some p -> not (List.mem p disabled_pkgs)
                  | None -> true)
                 && not (name_disabled name) ->
              (match Pricklypear_lisp.Ast.expr_of_body_json body_json with
               | Some body -> rebind body; true
               | None -> Hashtbl.replace heal_miss_cache key (); false)
          | _ ->
              if Hashtbl.length heal_miss_cache > 4096 then
                Hashtbl.reset heal_miss_cache;
              Hashtbl.replace heal_miss_cache key ();
              false

(** Result of invoking a route handler. Errors keep the real message
    (e.g. "budget exceeded") so HTTP can show it — never mask as bare None. *)
type handler_call =
  | Handler_ok of Pricklypear_lisp.Ast.value
  | Handler_unbound
  | Handler_error of string

(* card 1b76bdcf: dispatch surface - a name whose binding is not callable
   (VLambda | VPrim) can only fail at eval; route it to the precise-error
   arm instead of a blind "(name)" eval. *)
let callable_value (v : Pricklypear_lisp.Ast.value) : bool =
  match v with
  | Pricklypear_lisp.Ast.VLambda _ | Pricklypear_lisp.Ast.VPrim _ -> true
  | _ -> false

(* Invoke a named handler function (looked up via env). The handler
   must be a zero-arg VLambda or a VPrim name.
   When [~request] is provided, request-* prims see that context for
   the duration of the call (cleared in finalize). *)
let call_handler (t : t) ?request (handler_name : string)
  : handler_call =
  (* Resolve the handler env against the per-request snapshot when bound
     (image.per-request-context): a concurrent attach_anonymous +
     checkout_to mid-request would otherwise rehydrate t.eval_state.env to system/main
     and this lookup would miss personal-branch handlers. Fall back to t.eval_state.env
     when no context (REPL, /api/eval outside HTTP gate, pre-gate paths). *)
  let env =
    match Request_context.get () with
    | Some ctx -> Request_context.env ctx
    | None -> t.eval_state.env
  in
  match Pricklypear_lisp.Env.lookup env handler_name with
  | None -> Handler_unbound
  | Some fn when callable_value fn ->
      (* i-1788669761 (TOCTOU): the publish-time gate is not enough —
         (define h …) AFTER (route "/x" "h" 'public) re-points the route
         with zero re-gate, and boot rehydrate loads stored routes
         unchecked. Re-run the public-route effect gate here at every
         dispatch of a handler currently bound to a public route. No
         cache: define swaps the body under the same name, so a cached
         verdict would be stale by construction. *)
      let is_public_handler () =
        Hashtbl.fold
          (fun _route (h, vis) acc ->
            acc || (vis = "public" && h = handler_name))
          t.routes false
      in
      let public_gate_result = ref (Ok ()) in
        let run () =
          let eval_handler () =
            (* Eval in the SAME env the lookup resolved against (the
               per-request snapshot, image.per-request-context): a
               concurrent rehydrate can swap t.eval_state.env to
               system/main mid-request, and evaluating the found handler
               in the live env then dies "unbound: <handler>" — the
               "[ops] handler: unbound: marked" incident class. *)
            eval_string_rim t ~env ("(" ^ handler_name ^ ")")
          in
          let handler_error exn =
            let msg = pp_interp_exn exn in
            Ops_incidents.emit_and_maybe_file t
              ~level:Ops_log.Error ~component:"handler"
              ~file:(not (Ops_incidents.is_expected_runtime_error exn))
              ~context:[("handler", handler_name)]
              msg;
            (* i-1787971008: the CLIENT-facing message must not carry raw
               exception text. Controlled surface (Interp_error / Reader
               _error — the interpreter's own error values, including
               handler-deliberate (error "...") prims) passes through so
               handler UX keeps its messages. Everything else (PG/pgx
               errors, Match_failure, Nopales exceptions from OCaml
               land) is an UNCONTROLLED exn: full detail stays in the
               ops incident above; the client gets a generic line. The
               raw-exn 500 path in http.ml is already generic — this
               closes the same leak for the Handler_error render sites. *)
            let client_msg =
              match exn with
              | Pricklypear_lisp.Ast.Interp_error _
              | Pricklypear_lisp.Reader.Reader_error _ -> msg
              | _ -> "handler error (correlated in the ops log)"
            in
            (Handler_error client_msg)
          in
          (try Handler_ok (eval_handler ())
           with
           | Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.Unbound_var miss) as exn ->
               (* card b851a9a0 ("[ops] handler: unbound: stream/drop-n"):
                  a lost package binding heals from the cached body
                  registry / the branch chain and the handler retries
                  ONCE. A name the store does not know stays on the same
                  precise error path — no loop, no masking. *)
               if heal_unbound_binding t env miss then begin
                 Ops_incidents.emit_and_maybe_file t
                   ~level:Ops_log.Warn ~component:"handler" ~file:false
                   ~context:[("handler", handler_name); ("rebound", miss)]
                   ("unbound " ^ miss ^ " re-bound from the branch chain; handler retried");
                 (try Handler_ok (eval_handler ()) with exn -> handler_error exn)
               end
               else handler_error exn
           | exn -> handler_error exn)
      in
      if is_public_handler () then begin
        public_gate_result :=
          Pricklypear_lisp.Route_gate.check_public_handler ~env handler_name;
        (match !public_gate_result with
         | Error msg -> Handler_error msg
         | Ok () ->
             (* i-1788669715: meta-eval prims are the capability reset
                escape hatch; deny them for the duration of public
                handler execution. *)
             let prev = t.public_dispatch in
             t.public_dispatch <- true;
             (* Exception-safe: an escaped non-Interp_error must not leave
                the flag stuck true for the session (fail-closed, but it
                would deny meta-eval prims for the identity's session
                lifetime). *)
               Fun.protect
                 (fun () ->
                    match request with
                    | None -> run ()
                    | Some req ->
                        (* card c58830ad: RESTORE the previous binding, never
                           clear to None. A nested dispatch (route/dispatch ->
                           call_handler) used to clobber the OUTER handler's
                           request binding; any request-* prim after the nested
                           call then raised "no active request" (notes panel
                           click -> filmstrip open/leaf -> in-process pane
                           dispatch -> outer handler requestless). *)
                        let prev_req = t.current_request in
                        t.current_request <- Some req;
                        Fun.protect
                          ~finally:(fun () -> t.current_request <- prev_req)
                          run)
                 ~finally:(fun () -> t.public_dispatch <- prev))
        end else
          (match request with
           | None -> run ()
           | Some req ->
               (* card c58830ad: restore, don't clear (see the public arm
                  above) — a nested route/dispatch must not leave the outer
                  handler requestless. *)
               let prev_req = t.current_request in
               t.current_request <- Some req;
               Fun.protect
                 ~finally:(fun () -> t.current_request <- prev_req)
                 run)

  | Some v ->
      (* card 1b76bdcf ("[ops] handler: not a function: 12"): a rehydrated
         route or a stale define can leave a handler name bound to a
         non-function (e.g. a page-size variable). Evaluating the name
         blind died "not a function: 12" - an opaque, filed reliability
         incident that never names the handler. A non-function binding is
         branch state (same class as the unbound path above): log it
         precisely and return a Handler_error naming the handler and the
         bound value; no reliability incident. *)
      let msg =
        Printf.sprintf "handler %s is bound to a non-function value (%s)"
          handler_name (Pricklypear_lisp.Reader.pp_value v)
      in
      (Ops_incidents.emit_and_maybe_file t
         ~level:Ops_log.Error ~component:"handler" ~file:false
         ~context:[ ("handler", handler_name) ] msg;
       Handler_error msg)

(* Late-bind the route/dispatch thunk now that call_handler exists. *)
let () =
  route_dispatch_fn :=
    (fun t handler req ->
      (* Bind a per-call Request_context snapshot so handler prims
         (whoami, current-branch, ui/css, author-scoped reads) see the
         session's manifest/env/routes — mirrors http.ml's snapshot_ctx.
         Without this, notes/page renders with anonymous author and
         the fixture's ol-nodes don't appear. *)
      let ctx =
        Request_context.make
          ~manifest:t.manifest
          ~env:t.eval_state.env
          ~is_admin:t.is_admin
          ~routes:(Request_context.copy_routes t.routes)
          ~source_ip:None
      in
      Request_context.with_current ctx (fun () ->
        match call_handler t ~request:req handler with
        | Handler_ok v ->
          Pricklypear_lisp.Ast.VStr (Prims_hyper.render_handler_value v)
        | Handler_unbound ->
          Pricklypear_lisp.Ast.VStr
            (Printf.sprintf "route/dispatch: unbound handler %s" handler)
        | Handler_error msg ->
          Pricklypear_lisp.Ast.VStr (Printf.sprintf "route/dispatch: %s" msg)))

(* Late-bind the bumpers M4 verify-route thunk now that call_handler
   (and the env/route/request machinery it needs) exists. The report is
   a JSON dict an agent can read before opening a browser or promoting:
   {route, handler, effects (static badge), shape (fragment|shell|
   not-html), cost {ms, prim-calls, eval-cost}, warnings []}. Budget
   overflow surfaces as {kind:"budget-exceeded"} IN the report — never
   an exception/500. Invocation binds a default GET request (empty
   params) so request-* prims inside the handler resolve instead of
   raising "no active request", mirroring route/dispatch; the handler
   runs under a FRESH standard step budget via eval_string_rim_budgeted
   (the same proven inner-rim path route/dispatch uses), so prim-calls /
   eval-cost are this run's own accumulators. No /timing ring sample and
   no X-Timing-Ms header: verify is a probe, not a real request. *) 
let () =
  verify_route_fn :=
    (fun t path ->
      let open Pricklypear_lisp in
      let env =
        match Request_context.get () with
        | Some ctx -> Request_context.env ctx
        | None -> t.eval_state.env
      in
      let report fields : Pricklypear_lisp.Ast.value =
        Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string (`Assoc fields))
      in
      match lookup_route t path with
      | None -> report [ ("route", `String path); ("error", `String "no such route") ]
      | Some handler ->
        let body = Pricklypear_lisp.Route_gate.handler_body env handler in
        let effects, shape =
          match body with
          | Some b ->
              ( (try Some (Pricklypear_lisp.Effect.EffSet.badge (Pricklypear_lisp.Effect.infer b))
                 with _ -> Some "?")
              , Pricklypear_lisp.Effect.classify_route_shape b )
          | None -> (None, Pricklypear_lisp.Effect.NotHtml)
        in
        let warnings =
          match body with
          | Some b -> Pricklypear_lisp.Effect.verify_route_warnings b
          | None -> []
        in
        let req : Session_types.request =
          { method_ = `GET; path; params = []; body = ""; headers = [] }
        in
        let budget = Pricklypear_lisp.Eval.make_budget () in
        let t0 = Unix.gettimeofday () in
          let outcome =
            (* card c58830ad: restore, don't clear — same discipline as
               call_handler; the probe may now run nested inside a real
               request without stealing its binding. *)
            let prev_req = t.current_request in
            t.current_request <- Some req;
            Fun.protect
              ~finally:(fun () -> t.current_request <- prev_req)
            (fun () ->
              try
                let _v =
                    eval_string_rim_budgeted t ~env ("(" ^ handler ^ ")") budget
                in
                `Ok
              with
              | Ast.Interp_error Ast.Budget_exceeded -> `Budget
              | exn -> `Error (pp_interp_exn exn))
        in
        let ms = (Unix.gettimeofday () -. t0) *. 1000. in
        let cost =
          `Assoc
            [ ("ms", `Float ms)
            ; ("prim-calls", `Int budget.prim_calls)
            ; ("eval-cost", `Int budget.steps) ]
        in
        let base =
          [ ("route", `String path)
          ; ("handler", `String handler)
          ; ( "effects"
            , `String (Option.value ~default:"?" effects) )
          ; ("shape", `String (Pricklypear_lisp.Effect.route_shape_string shape))
          ; ("cost", cost)
          ; ( "warnings"
            , `List (List.map (fun w -> `String w) warnings) ) ]
        in
        (match outcome with
         | `Ok -> report base
         | `Budget -> report (( "kind", `String "budget-exceeded" ) :: base)
         | `Error msg -> report (("error", `String msg) :: base)))
