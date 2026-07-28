(** Pricklypear Image — session state for eval / HTTP / REPL.

    Holds the env (function defs for the current branch, plus any
    (define)'d locals), the boot manifest (default identity), auth
    attachment fields, and the store handle.

    Per spec [image.repl-experience]:
      - (define ...) persists to the store: survives image reboot.
      - REPL-local let bindings are session-local: NOT persisted.
      - Switching branch (checkout) keeps REPL locals but changes visibility. *)
open Lwt.Syntax
include Session_types

(** Run direct-style eval under the Lwt prim rim (H1.7). *)
let eval_under_rim (t : t) (thunk : unit -> 'a) : 'a Lwt.t =
  Control_rim.run_lwt ~prims:t.prims thunk

let eval_string_rim (t : t) (input : string) : Pricklypear_lisp.Ast.value Lwt.t =
  eval_under_rim t (fun () ->
      Pricklypear_lisp.Eval.eval_string_direct
        ~prims:Pricklypear_lisp.Eval.empty_prims t.env input)

let eval_expr_rim (t : t) (expr : Pricklypear_lisp.Ast.expr)
  : Pricklypear_lisp.Ast.value Lwt.t =
  let budget = Pricklypear_lisp.Eval.make_budget () in
  eval_under_rim t (fun () ->
      Pricklypear_lisp.Eval.eval
        ~budget ~prims:Pricklypear_lisp.Eval.empty_prims t.env expr)

let load_defs (t : t) : unit Lwt.t =
  let* defs = Store.list_function_names_for_load t.store in
  let* _ =
    Lwt_list.iter_s
      (fun (name, body, _author) ->
        (* Always (re)define — parents first, children last → override. *)
        Lwt.catch
          (fun () ->
             (* Eval the stored AST directly — NEVER pp_expr round-trip.
                Complex bodies (ui/home, notes) break under print/parse.
                Macro bodies are stored as (__macro__ params body). *)
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
                  Pricklypear_lisp.Env.define t.env name
                    (Pricklypear_lisp.Ast.VMacro (params, body_expr, t.env));
                  Lwt.return_unit
              | _ ->
                  let* _ =
                    eval_expr_rim t
                      (Pricklypear_lisp.Ast.Define (name, body))
                  in
                  Lwt.return_unit))
          (fun exn ->
             Printf.eprintf "[session] load def %s failed: %s\n%!"
               name (Printexc.to_string exn);
             Lwt.return_unit))
      defs
  in
  (* Also load view_bindings into the in-memory routes table. The
     recursive-CTE returns ancestors first, so a route defined in both
     the child and parent: child's wins (Hashtbl.replace on the
     first-seen only — skip further matches). *)
  let* routes = Store.list_routes t.store in
  let seen_r = Hashtbl.create 16 in
  List.iter
    (fun (route, handler, visibility) ->
      if not (Hashtbl.mem seen_r route) then begin
        Hashtbl.replace seen_r route ();
        Hashtbl.replace t.routes route (handler, visibility)
      end)
    routes;
  Lwt.return_unit

let load_defs_async (t : t) : unit =
  Lwt.async (fun () -> load_defs t)

(** Clear env + routes and reload from the store for the current branch.
    Re-binds kernel prims from [t.kernel_catalog] first so checkout does
    not lose +/define/load-library/etc. *)
let rehydrate (t : t) : unit Lwt.t =
  (* image.http-robustness invariant 4a: build a FRESH env record and
     SWAP t.env to it. Do NOT Hashtbl.clear the old env's bindings —
     VLambda closures capture `env` by reference, so clearing in place
     would empty the binding table an in-flight page (holding the old
     env via its captured closures) is still reading. The old record
     stays alive for any closure that captured it; new requests see
     the fresh env. This makes concurrent rehydrate safe to run
     alongside lock-free page work (invariant 4b). *)
  let fresh =
    match t.env.parent with
    | None -> Pricklypear_lisp.Env.create ()
    | Some p -> Pricklypear_lisp.Env.create ~parent:(Some p) ()
  in
  t.env <- fresh;
  Hashtbl.clear t.routes;
  List.iter
    (fun (name, _) ->
      Pricklypear_lisp.Env.define t.env name
        (Pricklypear_lisp.Ast.VPrim name))
    t.kernel_catalog;
  Pricklypear_lisp.Env.define t.env "load-string"
    (Pricklypear_lisp.Ast.VPrim "load-string");
  Pricklypear_lisp.Env.define t.env "load-library"
    (Pricklypear_lisp.Ast.VPrim "load-library");
  load_defs t

(** Switch store + manifest to [branch_id], then rehydrate env. *)
let checkout_to (t : t) ~(branch_id : string) ~(branch_ref : string)
  : unit Lwt.t =
  let* () = Store.checkout_branch t.store ~new_branch_id:branch_id in
  t.manifest <-
    { t.manifest with
      Pricklypear_common.Manifest.branch_id = branch_id;
      branch_name = branch_ref };
  (* Drop shell caches — ui/nav and ui/css are branch-visible. Next
     refresh_ui_shell (request launch / load) repopulates from new env.
     Reset the invariant-3 retry/refresh timestamps too. *)
  t.ui_nav_html <- None;
  t.ui_css <- None;
  t.ui_nav_refreshed_at <- 0.0;
  t.ui_nav_retry_at <- 0.0;
  t.ui_css_refreshed_at <- 0.0;
  t.ui_css_retry_at <- 0.0;
  rehydrate t

let split_branch_ref (s : string) : string * string =
  match String.split_on_char '/' s with
  | [ o; n ] -> o, n
  | [ n ] -> "system", n  (* bare name = system/<name> *)
  | _ -> "system", s

let register_session_control_prims (t : t) (tbl : Primitives.t) : unit =
  (* (fork 'parent 'child) -> VStr new-branch-id | VStr "no such parent" *)
  Primitives.register tbl "fork"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "fork: denied for anonymous")
      else if t.agent_branch_lock then
        Lwt.return
          (Pricklypear_lisp.Ast.VStr
             "fork blocked: agent is locked to the current branch")
      else
      match args with
      | [ Pricklypear_lisp.Ast.VStr parent_name;
          Pricklypear_lisp.Ast.VStr child_name ] ->
          let parent_owner, parent_short = split_branch_ref parent_name in
          let child_owner, child_short = split_branch_ref child_name in
          let* parent_id =
            Store.lookup_branch_id_by_name
              t.store ~owner:parent_owner ~name:parent_short
          in
          (match parent_id with
           | None ->
               Lwt.return (Pricklypear_lisp.Ast.VStr
                             (Printf.sprintf "no such parent: %s" parent_name))
           | Some pid ->
               let* new_id =
                 Store.fork_branch t.store ~parent_id:pid
                   ~owner:child_owner ~child_name:child_short
               in
               (match new_id with
                | Some id ->
                    Lwt.return (Pricklypear_lisp.Ast.VStr id)
                | None ->
                    Lwt.return (Pricklypear_lisp.Ast.VStr "fork failed")))
      | _ ->
          Lwt.return (Pricklypear_lisp.Ast.VStr "fork: bad args (need 2 strings)"));

  (* (merge 'source 'target) -> VStr "merged N rows" | VStr "denied".
     POLICY (per spec [branching.merge]): merging into `system/*`
     requires the session username to be "admin"; merges into the
     caller's own `owner/*` branches are free. *)
  Primitives.register tbl "merge"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "merge: denied for anonymous")
      else if t.agent_branch_lock then
        Lwt.return
          (Pricklypear_lisp.Ast.VStr
             "merge blocked: agent is locked to the current branch")
      else
      match args with
      | [ Pricklypear_lisp.Ast.VStr source_ref;
          Pricklypear_lisp.Ast.VStr target_ref ] ->
          let src_owner, src_short = split_branch_ref source_ref in
          let tgt_owner, tgt_short = split_branch_ref target_ref in
          let authorized =
            (tgt_owner <> "system")
            || t.is_admin
            || (t.manifest.username = "admin")
          in
          if not authorized then
            Lwt.return (Pricklypear_lisp.Ast.VStr
                          "denied: admin-only merge to system branch")
          else
            let* src_id =
              Store.lookup_branch_id_by_name
                t.store ~owner:src_owner ~name:src_short
            in
            let* tgt_id =
              Store.lookup_branch_id_by_name
                t.store ~owner:tgt_owner ~name:tgt_short
            in
            (match src_id, tgt_id with
             | Some s, Some tg ->
                 let* n = Store.merge_branch t.store
                            ~source_id:s ~target_id:tg in
                 (match n with
                  | Some count ->
                      Lwt.return (Pricklypear_lisp.Ast.VStr
                                    (Printf.sprintf "merged %d rows" count))
                  | None ->
                      Lwt.return (Pricklypear_lisp.Ast.VStr "merge failed"))
             | _ ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr
                               "source or target branch not found"))
      | _ ->
          Lwt.return (Pricklypear_lisp.Ast.VStr "merge: bad args (need 2 strings)"));

  (* (route "/path" handler [visibility]) -> VStr "/path".
     Optional third arg 'public | "public" | 'private | "private".
     Handler-only re-register keeps prior visibility (store ON CONFLICT).
     Anonymous sessions may not register routes.
     H0.3: public targets must pass the public-route lattice (same as AI
     allow minus elevate): pure|read-db|write-db|html|txn. Branch/Ai/
     Publish on a public handler is denied. Missing handler body is OK
     (define-after-route still works). *)
  let parse_vis = function
    | Pricklypear_lisp.Ast.VStr s ->
        (match String.lowercase_ascii (String.trim s) with
         | "public" -> Some "public"
         | "private" -> Some "private"
         | _ -> None)
    | _ -> None
  in
  let check_public_handler (handler : string) : (unit, string) result Lwt.t =
    (* Fail-closed public-route gate (BORGE-1785212463 / 1785212603).
       Infer effects from the in-memory env (synchronous, fresh) via the
       pure [Route_gate] module. The previous store read was stale after
       the define persist race and DEFAULTED TO ALLOW on unknown handlers;
       now unknown / non-function handlers are DENIED. *)
    Lwt.return
      (Pricklypear_lisp.Route_gate.check_public_handler ~env:t.env handler)
  in
  let register_route_checked ~route_path ~handler ~visibility
      ~(pass_vis_to_store : bool) : Pricklypear_lisp.Ast.value Lwt.t =
    let* gate =
      if visibility = "public" then check_public_handler handler
      else Lwt.return (Ok ())
    in
    match gate with
    | Error msg -> Lwt.return (Pricklypear_lisp.Ast.VStr msg)
    | Ok () ->
        Hashtbl.replace t.routes route_path (handler, visibility);
        let* _ =
          if t.seed_additive then
            if pass_vis_to_store then
              Store.register_route_if_absent t.store ~route:route_path
                ~handler ~visibility ()
            else
              Store.register_route_if_absent t.store ~route:route_path
                ~handler ()
          else if pass_vis_to_store then
            Store.register_route t.store ~route:route_path ~handler
              ~visibility ()
          else
            Store.register_route t.store ~route:route_path ~handler ()
        in
        Lwt.return (Pricklypear_lisp.Ast.VStr route_path)
  in
  Primitives.register tbl "route"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "route: denied for anonymous")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler ] ->
            let vis =
              match Hashtbl.find_opt t.routes route_path with
              | Some (_, v) -> v
              | None -> "private"
            in
            (* handler-only: pass_vis_to_store=false so DB keeps visibility
               on UPDATE; memory keeps prior or private for new. *)
            register_route_checked ~route_path ~handler ~visibility:vis
              ~pass_vis_to_store:false
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler;
            vis_v ] ->
            (match parse_vis vis_v with
             | None ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr
                      "route: visibility must be public or private")
             | Some vis ->
                 register_route_checked ~route_path ~handler ~visibility:vis
                   ~pass_vis_to_store:true)
        | _ ->
            Lwt.return (Pricklypear_lisp.Ast.VStr "route: bad args"));

  (* (public-route path handler) sugar *)
  Primitives.register tbl "public-route"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "public-route: denied for anonymous")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler ] ->
            register_route_checked ~route_path ~handler ~visibility:"public"
              ~pass_vis_to_store:true
        | _ ->
            Lwt.return (Pricklypear_lisp.Ast.VStr "public-route: bad args"));

  (* (route-visibility path) -> "public" | "private" | () *)
  Primitives.register tbl "route-visibility"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr route_path ] ->
          (match Hashtbl.find_opt t.routes route_path with
           | Some (_, vis) ->
               Lwt.return (Pricklypear_lisp.Ast.VStr vis)
           | None -> Lwt.return Pricklypear_lisp.Ast.VNil)
      | _ -> Lwt.return Pricklypear_lisp.Ast.VNil);

  (* (checkout 'branch-ref) -> VStr "checked out" | "no such branch".
     Mutates t.store.branch_id + t.manifest, then re-hydrates the env
     from the new branch's function_defs (sync — so the next form sees
     the right bindings). Blocked while agent_branch_lock is set. *)
  Primitives.register tbl "checkout"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "checkout: denied for anonymous")
      else if t.agent_branch_lock then
        Lwt.return
          (Pricklypear_lisp.Ast.VStr
             "checkout blocked: agent is locked to the current branch")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr branch_ref ] ->
            let owner, name = split_branch_ref branch_ref in
            let* found =
              Store.lookup_branch_id_by_name t.store ~owner ~name
            in
            (match found with
             | None -> Lwt.return (Pricklypear_lisp.Ast.VStr "no such branch")
             | Some new_id ->
                 let* () = checkout_to t ~branch_id:new_id ~branch_ref in
                 Lwt.return (Pricklypear_lisp.Ast.VStr "checked out"))
        | _ ->
            Lwt.return (Pricklypear_lisp.Ast.VStr "checkout: bad args"));

  (* (bench expr-string [n]) -> VFloat average microseconds per iteration.
     Parses once, warms up once, runs n times (default 1000) under a max
     budget so the bench loop itself never hits Budget_exceeded. Uses the
     current session env. Prefer pure expressions (side-effecting bodies
     will run for real). *)
  let bench_run (expr_str : string) (n : int) : Pricklypear_lisp.Ast.value Lwt.t =
    let n = max 1 n in
    let e =
      try Pricklypear_lisp.Reader.read_one expr_str
      with exn ->
        raise
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.User_error
                ("bench: parse failed: " ^ Printexc.to_string exn)))
    in
    let run_once () =
      let budget =
        Pricklypear_lisp.Eval.make_budget ~limit:max_int ()
      in
      Control_rim.run_lwt ~prims:t.prims (fun () ->
          Pricklypear_lisp.Eval.eval
            ~budget ~prims:Pricklypear_lisp.Eval.empty_prims t.env e)
    in
    (* warmup (discard) *)
    let* _ = run_once () in
    let t0 = Unix.gettimeofday () in
    let rec loop i =
      if i >= n then Lwt.return_unit
      else
        let* _ = run_once () in
        loop (i + 1)
    in
    let* () = loop 0 in
    let t1 = Unix.gettimeofday () in
    let avg_us = ((t1 -. t0) *. 1_000_000.) /. float_of_int n in
    Lwt.return (Pricklypear_lisp.Ast.VFloat avg_us)
  in
  Primitives.register tbl "bench"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] -> bench_run s 1000
      | [ Pricklypear_lisp.Ast.VStr s; Pricklypear_lisp.Ast.VInt n ] ->
          bench_run s (Int64.to_int n)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "bench: need (bench expr-string [iterations])"));

  (* Private prim used by the with-transaction special form in eval.ml.
     NOT USER-VISIBLE: it's looked up under name __with_transaction__ by
     the special form handler, never `(with-transaction-call ...)` from Lisp.
     Also bound into the env as a VPrim name by the auto-bind loop, but
     a `(with-transaction-call ...)` call would just route through here.
     The prim receives one arg: a VLambda value. It wraps Eval.apply
     inside Store.with_dynamic_transaction, so prims called from the
     body of the lambda see the in-transaction pgx conn via
     Store.current_conn. Lands it29, option A from BORGE-1783438027.

     Returns the lambda body's result. Nested transactions raise a clean
     Interp_error (see with_dynamic_transaction). *)
  (* Apply a VLambda under the effect rim (H1.7). *)
  let apply_lambda_auto (lam : Pricklypear_lisp.Ast.value) : Pricklypear_lisp.Ast.value Lwt.t =
    let budget = Pricklypear_lisp.Eval.make_budget () in
    Control_rim.run_lwt ~prims:t.prims (fun () ->
        Pricklypear_lisp.Eval.apply
          ~budget ~prims:Pricklypear_lisp.Eval.empty_prims lam [])
  in
  Primitives.register tbl "__with_transaction__"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VLambda (params, body, clo_env) as lam ] ->
          (* Enforce zero-arg lambda for clarity: with-transaction bodies
             shouldn't take args. *)
          let _ = params in
          let _ = clo_env in
          (* H1.3: body re-enters apply_eff under Control.run_lwt when
             PP_EVAL_CONTROL=eff so nested prims Prim_call while seeing
             Store.current_conn from with_dynamic_transaction. *)
          Io_lwt.Db.with_dynamic_transaction t.store (fun () ->
              apply_lambda_auto lam)
      | _ ->
          Lwt.return (Pricklypear_lisp.Ast.VStr
                        "__with_transaction__: bad args (need a lambda)"));

  (* (with-public-write (lambda () body...)) — run body with write privilege
     so public route handlers can create data_rows / notes captures.
     Elevates anonymous only (logged-in sessions already write). Restores
     identity afterward. Prefer for narrow public forms (signup), not general
     unauth APIs.

     Elevates BOTH the live session manifest AND the per-request context
     manifest. CRUD prims (create/deftype) read their anon guard from
     Request_context.manifest (the gate-time snapshot, per BORGE-1784434193
     cotenant fix); without elevating the snapshot, the guard sees the stale
     "anonymous" and silently denies (create returns a VStr, seq discards it,
     the handler renders its thanks page with no row written). *)
  Primitives.register tbl "with-public-write"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VLambda (_params, _body, _clo) as lam ] ->
          let prev = t.manifest in
          let need_elevate = prev.username = "anonymous" in
          if need_elevate then
            t.manifest <-
              { prev with user_id = "public"; username = "public" };
          let run_body =
            match Lwt.get Request_context.current with
            | Some ctx when need_elevate ->
                let elevated_manifest =
                  { (Request_context.manifest ctx) with
                    user_id = "public"; username = "public" }
                in
                let elevated_ctx =
                  Request_context.make
                    ~manifest:elevated_manifest
                    ~env:(Request_context.env ctx)
                    ~routes:(Request_context.routes ctx)
                in
                Lwt.with_value Request_context.current (Some elevated_ctx)
                  (fun () -> apply_lambda_auto lam)
            | _ -> apply_lambda_auto lam
          in
          Lwt.finalize
            (fun () -> run_body)
            (fun () ->
              if need_elevate then t.manifest <- prev;
              Lwt.return_unit)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "with-public-write: bad args (need a zero-arg lambda)"))

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
let export_package = Agent_tools.export_package
let propose_function = Agent_tools.propose_function
let default_agent_seed = Agent_tools.default_agent_seed
let agent_session_new = Agent_tools.agent_session_new
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
  let deny_anon name =
    (* Raise, don't return a VStr: a returned string is silently dropped
       by (seq …), so a multi-write handler could partially succeed past
       an anonymous denial. Raising aborts the whole eval. (BORGE-1785213376) *)
    raise
      (Pricklypear_lisp.Ast.Interp_error
         (Pricklypear_lisp.Ast.User_error
            (name ^ ": denied for anonymous")))
  in
  let actor () =
    match Lwt.get Request_context.current with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  (* (media-meta id) -> VStr json | VNil *)
  Primitives.register tbl "media-meta"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
          let* m = Media.get_meta t.store ~id in
          (match m with
           | None -> Lwt.return Pricklypear_lisp.Ast.VNil
           | Some meta ->
               Lwt.return
                 (Pricklypear_lisp.Ast.VStr (Media.meta_to_json_string meta)))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "media-meta: need id"));
  (* (media-list [limit]) -> VList of meta json strings *)
  Primitives.register tbl "media-list"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "media-list"
      else
        let limit =
          match args with
          | [] -> 50
          | [ Pricklypear_lisp.Ast.VInt n ] -> Int64.to_int n
          | _ -> 50
        in
        let* rows =
          Media.list t.store ~author:(Some uname) ~limit ()
        in
        let items =
          List.map
            (fun m ->
              Pricklypear_lisp.Ast.VStr (Media.meta_to_json_string m))
            rows
        in
        Lwt.return (Pricklypear_lisp.Ast.VList items));
  (* (media-delete id) -> VStr id | VNil | denial *)
  Primitives.register tbl "media-delete"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "media-delete"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr id ] ->
            let* res =
              Media.delete t.store ~id ~actor:uname ~is_admin:t.is_admin
            in
            (match res with
             | Ok (`Ok id') ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr id')
             | Ok `Not_found -> Lwt.return Pricklypear_lisp.Ast.VNil
             | Ok `Forbidden ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr
                      "media-delete: not author or admin")
             | Error e ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr ("media-delete: " ^ e)))
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "media-delete: need id"));
  (* (media-put content-type data-b64 [filename]) -> VStr id | error
     Tiny payloads only (icons/fixtures). Real pushes: POST /api/media. *)
  Primitives.register tbl "media-put"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "media-put"
      else
        let parse =
          match args with
          | [ Pricklypear_lisp.Ast.VStr ct
            ; Pricklypear_lisp.Ast.VStr b64 ] ->
              Ok (ct, b64, None)
          | [ Pricklypear_lisp.Ast.VStr ct
            ; Pricklypear_lisp.Ast.VStr b64
            ; Pricklypear_lisp.Ast.VStr filename ] ->
              Ok (ct, b64, Some filename)
          | _ -> Error "media-put: need content-type, data-b64 [, filename]"
        in
        match parse with
        | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr e)
        | Ok (ct, b64, filename) ->
            (match Media.b64_decode_opt (String.trim b64) with
             | None ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr "media-put: invalid data-b64")
             | Some data ->
                 let* res =
                   Media.put t.store ~author:uname ~content_type:ct ~data
                     ~filename ~max_bytes:Media.max_eval_put_bytes ()
                 in
                 (match res with
                  | Ok r -> Lwt.return (Pricklypear_lisp.Ast.VStr r.id)
                  | Error e ->
                      Lwt.return
                        (Pricklypear_lisp.Ast.VStr ("media-put: " ^ e)))));
  (* (media-url id) -> VStr "/media/<id>" *)
  Primitives.register tbl "media-url"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr (Media.url_of_id id))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "media-url: need id"))

(** Agent promotes from its current branch (admin/agent) into the parent
    human's personal branch. Gated by the parent's allow_agent_promote
    toggle. Empty names = all functions defined ON the current branch
    (depth 0, not inherited). Defined here (before register_crud_prims) so
    the (promote) prim can reference it. *)
let promote_to_personal (t : t) ~(names : string list)
  : (int, string) result Lwt.t =
  let* me_opt = Auth_store.get_user_by_username t.store t.manifest.username in
  match me_opt with
  | None -> Lwt.return (Error "no such user")
  | Some me when me.user_type <> "agent" ->
      Lwt.return (Error "promote-to-personal is for agent users")
  | Some me ->
      (match me.parent_user_id with
       | None -> Lwt.return (Error "agent has no parent")
       | Some pid ->
           let* parent_opt = Auth_store.get_user_by_id t.store pid in
           (match parent_opt with
            | None -> Lwt.return (Error "parent user not found")
            | Some parent ->
                let* allowed =
                  Auth_store.get_allow_agent_promote t.store
                    ~user_id:parent.id
                in
                if not allowed then
                  Lwt.return (Error "agent promote disabled by parent")
                else
                  let* final_names =
                    if names <> [] then Lwt.return names
                    else
                      let* meta = Store.list_function_names_meta t.store in
                      let own =
                        List.filter (fun (_, _, _, d) -> d = 0) meta
                      in
                      Lwt.return (List.map (fun (n, _, _, _) -> n) own)
                  in
                  if final_names = [] then
                    Lwt.return (Error "no functions defined on this branch")
                  else
                    let source_id = Store.current_branch_id t.store in
                    let* tgt_opt =
                      Store.lookup_branch_id_by_name t.store
                        ~owner:parent.username ~name:"personal"
                    in
                    (match tgt_opt with
                     | None ->
                         Lwt.return (Error "parent personal branch not found")
                     | Some target_id ->
                         let* n =
                           Store.promote_functions t.store
                             ~source_id ~target_id ~names:final_names
                         in
                         Lwt.return (Ok n))))

let register_crud_prims (t : t) (tbl : Primitives.t) : unit =
  (* Quoted type/field names are VSymbol after Phase 1; CRUD still wants
     string payloads for SQL. Coerce symbols→strings at the CRUD boundary
     only (do not collapse in symbol=? / general prims). *)
  let coerce_sym_args (args : Pricklypear_lisp.Ast.value list) =
    List.map
      (function
        | Pricklypear_lisp.Ast.VSymbol s -> Pricklypear_lisp.Ast.VStr s
        | v -> v)
      args
  in
  let register_crud name fn =
    Primitives.register tbl name (fun args -> fn (coerce_sym_args args))
  in
  (* (deftype 'name 'field1 'field2 ...) -> VStr "defined type <name>".
     All args are quoted symbols (now VSymbol; coerced above). Persists
     the declaration into type_defs on the session's branch. *)
  let deny_anon name =
    (* Raise, don't return a VStr: a returned string is silently dropped
       by (seq …), so a multi-write handler could partially succeed past
       an anonymous denial. Raising aborts the whole eval. (BORGE-1785213376) *)
    raise
      (Pricklypear_lisp.Ast.Interp_error
         (Pricklypear_lisp.Ast.User_error
            (name ^ ": denied for anonymous")))
  in
  (* Per-request identity snapshot (avoids the cotenant race where the
     live t.manifest is mid-swap). Falls back to t.manifest outside HTTP. *)
  let actor () =
    match Lwt.get Request_context.current with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  register_crud "deftype"
    (fun args ->
      let uname =
        match Lwt.get Request_context.current with
        | Some c -> (Request_context.manifest c).username
        | None -> t.manifest.username
      in
      if uname = "anonymous" then deny_anon "deftype"
      else Crud.deftype t.store ~args ~author:uname);

  (* (get 'type id) -> VStr fields-json | VNil *)
  register_crud "get"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          Crud.get t.store ~type_name ~id
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "get: bad args (need type, id)"));

  (* (create 'type "{fields-json}") -> VStr new-id | VStr "create failed" *)
  register_crud "create"
    (fun args ->
      let uname =
        match Lwt.get Request_context.current with
        | Some c -> (Request_context.manifest c).username
        | None -> t.manifest.username
      in
      if uname = "anonymous" then deny_anon "create"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr fields_json ] ->
            (* Attribute to the per-request identity, not the live manifest
               (which can be mid-swap under the cotenant race). *)
            Crud.create t.store
              ~type_name ~fields_json
              ~author:uname
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "create: bad args (need type, fields)"));

  (* (update 'type id "{fields-json}") -> VStr id | VNil *)
  register_crud "update"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "update"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr id;
            Pricklypear_lisp.Ast.VStr fields_json ] ->
            Crud.update t.store ~type_name ~id ~fields_json
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 "update: bad args (need type, id, fields)"));

  (* (update-field 'type id "field" "value") -> VStr id | VNil.
      Ergonomic shorthand: composes get + dict-set + update in one prim.
      Lands it18 (2026-07-06); resolves the it14 "verbose round-trip syntax" TODO. *)
  register_crud "update-field"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "update-field"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr id;
            Pricklypear_lisp.Ast.VStr field;
            Pricklypear_lisp.Ast.VStr value ] ->
            Crud.update_field t.store ~type_name ~id ~field ~value
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 "update-field: bad args (need type, id, field, value)"));

  (* (delete 'type id) -> VStr id | VNil *)
  register_crud "delete"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "delete"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr id ] ->
            Crud.delete t.store ~type_name ~id
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "delete: bad args (need type, id)"));

  (* (select-for-update 'type id) -> VStr fields-json | VNil.
     Acquires an explicit SELECT ... FOR UPDATE row lock on the
     named row, for the duration of the surrounding transaction.
     Per spec [storage.transactions] (prim lands it17, 2026-07-06).
     See Crud.select_for_update doc for transactional-scoping caveat:
     outside a transaction, this is a hinted-get with no real lock. *)
  register_crud "select-for-update"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          Crud.select_for_update t.store ~type_name ~id
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "select-for-update: bad args (need type, id)"));

  (* (rows 'type [limit]) -> VList of (id . fields) pairs.
     With no limit, defaults to 100. NOTE: prim name is `rows`, NOT
     `list` — the latter is the pure VList-constructor prim. Earlier
     tentative naming collided and broke `(list 1 2 3)`; renamed to
     avoid that. *)
  register_crud "rows"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.list t.store ~type_name ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.list t.store ~type_name ~limit:(Int64.to_int n) ()
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "rows: bad args (need type[, limit])"));

  (* (find-row 'type "field" "value") -> fields JSON | () *)
  register_crud "find-row"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr value ] ->
          Crud.find_row t.store ~type_name ~field ~value
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "find-row: need type, field, value"));

  (* (find-rows 'type "f" "v" … [limit]) -> list of (id . fields) *)
  register_crud "find-rows"
    (fun args ->
      let rec parse_filters acc = function
        | [] -> Ok (List.rev acc, 500)
        | [ Pricklypear_lisp.Ast.VInt n ] ->
            Ok (List.rev acc, Int64.to_int n)
        | Pricklypear_lisp.Ast.VStr f :: Pricklypear_lisp.Ast.VStr v :: rest ->
            parse_filters ((f, v) :: acc) rest
        | _ -> Error "find-rows: bad filter args"
      in
      match args with
      | Pricklypear_lisp.Ast.VStr type_name :: rest ->
          (match parse_filters [] rest with
           | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr e)
           | Ok (filters, limit) ->
               Crud.find_rows t.store ~type_name ~filters ~limit ())
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "find-rows: need type and field/value pairs"));

  (* (find-text 'type "field" "needle" [limit]) *)
  register_crud "find-text"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr needle ] ->
          Crud.find_text t.store ~type_name ~field ~needle ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr needle;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.find_text t.store ~type_name ~field ~needle
            ~limit:(Int64.to_int n) ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "find-text: need type, field, needle [, limit]"));

  (* (find-random 'type) -> (id . fields) | () *)
  register_crud "find-random"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.find_random t.store ~type_name
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "find-random: need type"));

  (* (list-branches) -> VList of "owner/name" strings *)
  Primitives.register tbl "list-branches"
    (fun _args ->
      let* bs = Store.list_branches t.store in
      let names =
        List.map
          (fun (b : Pricklypear_common.Branch.t) ->
            Pricklypear_lisp.Ast.VStr
              (b.Pricklypear_common.Branch.owner ^ "/" ^
               b.Pricklypear_common.Branch.name))
          bs
      in
      Lwt.return (Pricklypear_lisp.Ast.VList names));

  (* storage.media Lisp surface — meta/list/delete + tiny put.
     Bulk agent pushes use POST /api/media (5 MiB), not eval. *)
  register_media_prims t tbl;

  (* (list-usernames) -> VList of VStr; any authenticated user.
     Usernames are public identifiers on the box (multi-user.chat DMs). *)
  Primitives.register tbl "list-usernames"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        Lwt.return
          (Pricklypear_lisp.Ast.VStr "list-usernames: denied for anonymous")
      else
        let* users = Auth_store.list_users t.store in
        let names =
          List.map
            (fun (u : Auth_store.user) ->
              Pricklypear_lisp.Ast.VStr u.Auth_store.username)
            users
        in
        Lwt.return (Pricklypear_lisp.Ast.VList names));

  (* Admin gating for Lisp auth surface (auth.user-management /
     auth.bootstrap-admin). Prefer the per-request snapshot username
     and re-check users.is_admin in PG so a concurrent attach cannot
     flip t.is_admin under an in-flight /api/eval. *)
  let caller_username () =
    match Lwt.get Request_context.current with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  let require_admin (op : string) (k : unit -> Pricklypear_lisp.Ast.value Lwt.t)
    : Pricklypear_lisp.Ast.value Lwt.t =
    let uname = caller_username () in
    if uname = "anonymous" then
      Lwt.return
        (Pricklypear_lisp.Ast.VStr (op ^ ": denied for anonymous"))
    else
      let* u = Auth_store.get_user_by_username t.store uname in
      match u with
      | Some u when u.Auth_store.is_admin -> k ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr (op ^ ": denied: admin only"))
  in

  (* (is-admin?) -> VBool. True iff the calling identity has users.is_admin. *)
  Primitives.register tbl "is-admin?"
    (fun _args ->
      let uname = caller_username () in
      if uname = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VBool false)
      else
        let* u = Auth_store.get_user_by_username t.store uname in
        match u with
        | Some u -> Lwt.return (Pricklypear_lisp.Ast.VBool u.Auth_store.is_admin)
        | None -> Lwt.return (Pricklypear_lisp.Ast.VBool false));

  (* (create-user username password [is-admin]) -> VStr new-user-id | error VStr.
     Admin only. Mirrors POST /admin/users action=create (create + set password
     + auth_log admin_create_user). Optional third arg: bool / truthy str/int. *)
  Primitives.register tbl "create-user"
    (fun args ->
      require_admin "create-user" (fun () ->
        let parse_is_admin = function
          | Pricklypear_lisp.Ast.VBool b -> b
          | Pricklypear_lisp.Ast.VStr s ->
              (match String.lowercase_ascii (String.trim s) with
               | "1" | "true" | "yes" | "on" -> true
               | _ -> false)
          | Pricklypear_lisp.Ast.VInt n -> n <> 0L
          | _ -> false
        in
        let run ~username ~password ~is_admin =
          if String.trim username = "" || password = "" then
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 "create-user: username and password required")
          else
            let* created =
              Auth_store.create_user t.store ~username ~is_admin
            in
            match created with
            | Error e ->
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr ("create-user: " ^ e))
            | Ok user ->
                let* pw =
                  Auth_store.set_password t.store
                    ~user_id:user.Auth_store.id ~password
                in
                (match pw with
                 | Error e ->
                     Lwt.return
                       (Pricklypear_lisp.Ast.VStr
                          ("create-user: password: " ^ e))
                 | Ok () ->
                     let* () =
                       Auth_store.log_auth t.store
                         ~user_id:user.Auth_store.id
                         ~kind:"admin_create_user"
                         ~success:true ()
                     in
                     Lwt.return
                       (Pricklypear_lisp.Ast.VStr user.Auth_store.id))
        in
        match args with
        | [ Pricklypear_lisp.Ast.VStr username;
            Pricklypear_lisp.Ast.VStr password ] ->
            run ~username ~password ~is_admin:false
        | [ Pricklypear_lisp.Ast.VStr username;
            Pricklypear_lisp.Ast.VStr password;
            flag ] ->
            run ~username ~password ~is_admin:(parse_is_admin flag)
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 "create-user: need username password [is-admin]")));

  (* (create-agent parent-username) -> VStr agent-user-id. Admin only. *)
  Primitives.register tbl "create-agent"
    (fun args ->
      require_admin "create-agent" (fun () ->
        match args with
        | [ Pricklypear_lisp.Ast.VStr parent ] ->
            let* r = Auth_store.create_agent t.store ~parent_username:parent in
            (match r with
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("create-agent: " ^ e))
             | Ok u ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr u.Auth_store.id))
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "create-agent: need parent-username")));

  (* (issue-token [label]) -> VStr raw-token (returned ONCE). Caller's own user. *)
  (* (issue-token user-id [label]) -> VStr raw token. Admin only when user-id given. *)
  Primitives.register tbl "issue-token"
    (fun args ->
      let caller = caller_username () in
      let parse_label = function
        | Pricklypear_lisp.Ast.VStr s when s <> "" -> Some s
        | _ -> None
      in
      (match args with
       | [] ->
           (* issue for the caller themselves, no label *)
           let* u = Auth_store.get_user_by_username t.store caller in
           (match u with
            | None -> Lwt.return (Pricklypear_lisp.Ast.VStr "issue-token: no such user")
            | Some user ->
                let* r = Auth_store.issue_token t.store ~user_id:user.id () in
                (match r with
                 | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("issue-token: " ^ e))
                 | Ok raw -> Lwt.return (Pricklypear_lisp.Ast.VStr raw)))
       | [ label ] ->
           (* issue for the caller themselves, with label *)
           let* u = Auth_store.get_user_by_username t.store caller in
           (match u with
            | None -> Lwt.return (Pricklypear_lisp.Ast.VStr "issue-token: no such user")
            | Some user ->
                let* r = Auth_store.issue_token t.store ~user_id:user.id ~label:(parse_label label) () in
                (match r with
                 | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("issue-token: " ^ e))
                 | Ok raw -> Lwt.return (Pricklypear_lisp.Ast.VStr raw)))
       | [ Pricklypear_lisp.Ast.VStr uid; label ] ->
           require_admin "issue-token" (fun () ->
             let* r = Auth_store.issue_token t.store ~user_id:uid ~label:(parse_label label) () in
             (match r with
              | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("issue-token: " ^ e))
              | Ok raw -> Lwt.return (Pricklypear_lisp.Ast.VStr raw)))
       | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "issue-token: need [label] (own) or user-id label (admin)")));

  (* (list-tokens [user-id]) -> VList of (id label last4 created last_used). *)
  Primitives.register tbl "list-tokens"
    (fun args ->
      let uname = caller_username () in
      let* caller_opt = Auth_store.get_user_by_username t.store uname in
      let caller_id =
        match caller_opt with Some x -> x.Auth_store.id | None -> ""
      in
      let is_admin =
        match caller_opt with Some x -> x.Auth_store.is_admin | None -> false
      in
      let target_uid =
        match args with
        | [ Pricklypear_lisp.Ast.VStr uid ] when uid <> "" -> uid
        | _ -> caller_id
      in
      if not is_admin && target_uid <> caller_id then
        Lwt.return (Pricklypear_lisp.Ast.VStr "list-tokens: denied: admin only")
      else if target_uid = "" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "list-tokens: no such user")
      else
        let* metas = Auth_store.list_tokens t.store ~user_id:target_uid in
        let rows =
          List.map
            (fun (m : Auth_store.token_meta) ->
              Pricklypear_lisp.Ast.VList
                [ Pricklypear_lisp.Ast.VStr m.id;
                  Pricklypear_lisp.Ast.VStr (Option.value m.label ~default:"");
                  Pricklypear_lisp.Ast.VStr m.token_last4;
                  Pricklypear_lisp.Ast.VStr m.created_at;
                  Pricklypear_lisp.Ast.VStr (Option.value m.last_used_at ~default:"") ])
            metas
        in
        Lwt.return (Pricklypear_lisp.Ast.VList rows));

  (* (revoke-token credential-id) -> VStr ok|error. Caller may revoke own; admin any. *)
  Primitives.register tbl "revoke-token"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr cid ] when cid <> "" ->
          let* r = Auth_store.revoke_token t.store ~credential_id:cid in
          (match r with
           | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("revoke-token: " ^ e))
           | Ok () -> Lwt.return (Pricklypear_lisp.Ast.VStr "ok"))
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "revoke-token: need credential-id"));

  (* (rotate-token credential-id [label]) -> VStr new-raw-token. Admin: rotates any;
     non-admin rotates own (credential must belong to caller). Returns new raw token ONCE. *)
  Primitives.register tbl "rotate-token"
    (fun args ->
      let uname = caller_username () in
      (match args with
       | [ Pricklypear_lisp.Ast.VStr cid ] when cid <> "" ->
           (* Verify the credential belongs to the caller (or admin). *)
           let* caller = Auth_store.get_user_by_username t.store uname in
           let caller_id = match caller with Some x -> x.Auth_store.id | None -> "" in
           let is_admin = match caller with Some x -> x.Auth_store.is_admin | None -> false in
           let* metas = Auth_store.list_tokens t.store ~user_id:caller_id in
           let owns = List.exists (fun (m : Auth_store.token_meta) -> m.id = cid) metas in
           if not is_admin && not owns then
             Lwt.return (Pricklypear_lisp.Ast.VStr "rotate-token: denied: not your token")
           else
             let* r = Auth_store.rotate_token t.store ~credential_id:cid ~user_id:caller_id () in
             (match r with
              | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("rotate-token: " ^ e))
              | Ok raw -> Lwt.return (Pricklypear_lisp.Ast.VStr raw))
       | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "rotate-token: need credential-id")));

  (* (set-allow-agent-promote bool) -> VStr ok|error. Sets the CALLER's own toggle. *)
  Primitives.register tbl "set-allow-agent-promote"
    (fun args ->
      let parse_bool = function
        | Pricklypear_lisp.Ast.VBool b -> b
        | Pricklypear_lisp.Ast.VStr s ->
            (match String.lowercase_ascii (String.trim s) with
             | "1" | "true" | "yes" | "on" -> true
             | _ -> false)
        | Pricklypear_lisp.Ast.VInt n -> n <> 0L
        | _ -> false
      in
      let caller = caller_username () in
      if caller = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "set-allow-agent-promote: denied for anonymous")
      else
        (match args with
         | [ flag ] ->
             let* u = Auth_store.get_user_by_username t.store caller in
             (match u with
              | None -> Lwt.return (Pricklypear_lisp.Ast.VStr "set-allow-agent-promote: no such user")
              | Some user ->
                  let v = parse_bool flag in
                  let* r = Auth_store.set_allow_agent_promote t.store ~user_id:user.id ~value:v in
                  (match r with
                   | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("set-allow-agent-promote: " ^ e))
                   | Ok () -> Lwt.return (Pricklypear_lisp.Ast.VStr "ok")))
         | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "set-allow-agent-promote: need bool")));

  (* (promote [name...]) -> VStr count|error. Agent: promote own defs (or named)
     into parent's personal. Human: denied (use promote-to-main admin path). *)
  Primitives.register tbl "promote"
    (fun args ->
      let names =
        List.filter_map
          (fun a -> match a with Pricklypear_lisp.Ast.VStr s -> Some s | _ -> None)
          args
      in
      let* r = promote_to_personal t ~names in
      (match r with
       | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("promote: " ^ e))
       | Ok n -> Lwt.return (Pricklypear_lisp.Ast.VStr (string_of_int n))));

  (* (current-branch) -> VStr *)
  Primitives.register_sync tbl "current-branch"
    (fun _ ->
      let branch =
        match Lwt.get Request_context.current with
        | Some c -> (Request_context.manifest c).branch_name
        | None -> t.manifest.branch_name
      in
      Pricklypear_lisp.Ast.VStr branch);

  (* Durable agent sessions (ai-authoring.agent-portal). *)
  Primitives.register tbl "agent-session-new"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed ] ->
          agent_session_new t ~seed ~tools:true ()
      | [ Pricklypear_lisp.Ast.VStr seed; Pricklypear_lisp.Ast.VBool tools ] ->
          agent_session_new t ~seed ~tools ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "agent-session-new: need seed-str [, tools-bool]"));

  Primitives.register tbl "agent-say"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt ] ->
          agent_say t ~session_id:sid ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt;
          Pricklypear_lisp.Ast.VInt n ] ->
          agent_say t ~session_id:sid ~prompt ~max_steps:(Int64.to_int n) ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "agent-say: need session-id, prompt [, max-steps]"));

  Primitives.register tbl "agent-history"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] -> agent_history t ~session_id:sid
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "agent-history: need session-id"));

  Primitives.register tbl "agent-sessions"
    (fun args ->
      match args with
      | [] -> agent_sessions t ()
      | [ Pricklypear_lisp.Ast.VInt n ] ->
          agent_sessions t ~limit:(Int64.to_int n) ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "agent-sessions: optional limit int"));

  Primitives.register tbl "agent-session-delete"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] ->
          agent_session_delete t ~session_id:sid
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "agent-session-delete: need session-id"));

  (* Temporary aliases: new session + one say. Prefer agent-session-new/say. *)
  Primitives.register tbl "agent"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed;
          Pricklypear_lisp.Ast.VStr prompt ] ->
          agent t ~seed ~prompt ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "agent: need 2 strings (seed, prompt)"));

  Primitives.register tbl "agent-task"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr prompt ] ->
          agent_task t ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr prompt; Pricklypear_lisp.Ast.VInt n ] ->
          agent_task t ~max_steps:(Int64.to_int n) ~prompt ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "agent-task: need prompt-str [, max-steps int]"));

  (* (propose-function 'name "spec text") -> VStr name | error string.
     Calls OpenAI-compat LLM, defines the function on the CURRENT branch
     with author = agent:<model> (AI provenance). Never merges to system. *)
  Primitives.register tbl "propose-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec ] ->
          propose_function t ~name ~spec
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "propose-function: bad args (need name-str, spec-str)"));

  (* (install-function name body-src) -> install AI-authored body (effect gate). *)
  Primitives.register tbl "install-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr body ] ->
          install_ai_function t ~name ~body_src:body
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "install-function: need name-str, body-str"));

  (* (function-source name) -> VStr pretty body | VNil *)
  Primitives.register tbl "function-source"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let* src = function_source t name in
          (match src with
           | Some s -> Lwt.return (Pricklypear_lisp.Ast.VStr s)
           | None -> Lwt.return Pricklypear_lisp.Ast.VNil)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "function-source: need name-str"));

  (* (function-doc name) -> VStr docstring | VNil
     Store metadata first; special-form / prim blurbs as fallback. *)
  Primitives.register tbl "function-doc"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let* doc = Store.lookup_function_doc t.store name in
          (match doc with
           | Some s -> Lwt.return (Pricklypear_lisp.Ast.VStr s)
           | None ->
               (match Pricklypear_lisp.Special_forms.find name with
                | Some e ->
                    Lwt.return (Pricklypear_lisp.Ast.VStr e.summary)
                | None ->
                    let blurb = Primitives.prim_summary name in
                    if blurb = "built-in primitive" then
                      Lwt.return Pricklypear_lisp.Ast.VNil
                    else Lwt.return (Pricklypear_lisp.Ast.VStr blurb)))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "function-doc: need name-str"));

  (* (function-ast name) -> quoted body value tree | VNil.
     Does not eval the body; reifies the stored AST via quote_to_value. *)
  Primitives.register tbl "function-ast"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let* ast = function_ast t name in
          (match ast with
           | Some v -> Lwt.return v
           | None -> Lwt.return Pricklypear_lisp.Ast.VNil)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "function-ast: need name-str"));

  (* (package-exports pkg) -> VList of public names (Phase 3 package-surface).
     Kernel/std: frozen set. Other packages: shallow names (pkg or pkg/local). *)
  let kernel_std_exports =
    [ "list/map"; "list/filter"; "list/foldl"; "reverse"; "append"; "nth"
    ; "null?"; "pair?"; "car"; "cdr"; "cons"; "list"; "count"
    ; "string-append"; "string-length"; "substring"; "string-eq"
    ; "string-contains?"; "string-trim"; "html-escape"; "as-str"; "str"
    ; "string<?"; "string<=?"; "string>?"; "string>=?"
    ; "symbol?"; "string?"; "symbol=?"; "symbol->string"; "string->symbol"
    ; "not"; "type-of"; "read-string"; "read-all-string"; "function-ast"
    ; "function-source"; "function-doc"; "apropos"; "apropos-docs"
    ; "describe"; "list-packages"; "package-bindings"; "package-exports"
    ; "prefix-of"
    ]
  in
  Primitives.register tbl "package-exports"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let pkg = String.lowercase_ascii (String.trim pkg) in
          if pkg = "std" || pkg = "kernel" || pkg = "(kernel)" then
            Lwt.return
              (Pricklypear_lisp.Ast.VList
                 (List.map
                    (fun n -> Pricklypear_lisp.Ast.VStr n)
                    kernel_std_exports))
          else
            let* names = package_export_names t pkg in
            Lwt.return
              (Pricklypear_lisp.Ast.VList
                 (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "package-exports: need package-str"));

  (* (package-bindings pkg) -> VList of name strings (prefix pkg/ any depth) *)
  Primitives.register tbl "package-bindings"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let* names = package_binding_names t pkg in
          Lwt.return
            (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "package-bindings: need package-str"));

  (* (list-packages) -> VList of top-level package prefix strings *)
  Primitives.register tbl "list-packages"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          let* names = list_package_names t in
          Lwt.return
            (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "list-packages: takes no args"));

  (* (apropos-docs pat) -> VList of (name . doc) pairs *)
  Primitives.register tbl "apropos-docs"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pat ] ->
          let* pairs = apropos_docs t pat in
          let rows =
            List.map
              (fun (n, d) ->
                Pricklypear_lisp.Ast.VList
                  [ Pricklypear_lisp.Ast.VStr n
                  ; Pricklypear_lisp.Ast.VStr d
                  ])
              pairs
          in
          Lwt.return (Pricklypear_lisp.Ast.VList rows)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "apropos-docs: need pattern-str"));

  (* (describe name-or-pkg) -> VStr selective reveal text *)
  Primitives.register tbl "describe"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          let* s = describe t needle in
          Lwt.return (Pricklypear_lisp.Ast.VStr s)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "describe: need name-or-package-str"));

  (* (export-package pkg) -> VStr lib.pp text *)
  Primitives.register tbl "export-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let* text = export_package t pkg in
          Lwt.return (Pricklypear_lisp.Ast.VStr text)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "export-package: need package-str"));

  (* SSH keys (auth.user-ssh-repl phase 1) *)
  Primitives.register tbl "add-ssh-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "add-ssh-key: denied for anonymous")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr openssh ] ->
            let* r =
              Auth_store.add_ssh_key t.store ~user_id:t.manifest.user_id
                ~openssh ()
            in
            (match r with
             | Ok m ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr m.Auth_store.fingerprint)
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("add-ssh-key: " ^ e)))
        | [ Pricklypear_lisp.Ast.VStr openssh; Pricklypear_lisp.Ast.VStr label ] ->
            let* r =
              Auth_store.add_ssh_key t.store ~user_id:t.manifest.user_id
                ~openssh ~label:(Some label) ()
            in
            (match r with
             | Ok m ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr m.Auth_store.fingerprint)
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("add-ssh-key: " ^ e)))
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "add-ssh-key: need openssh-str [, label]"));

  Primitives.register tbl "list-ssh-keys"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "list-ssh-keys: denied for anonymous")
      else
        let* keys =
          Auth_store.list_ssh_keys t.store ~user_id:t.manifest.user_id
        in
        let vs =
          List.map
            (fun (k : Auth_store.ssh_key_meta) ->
              Pricklypear_lisp.Ast.VStr k.fingerprint)
            keys
        in
        Lwt.return (Pricklypear_lisp.Ast.VList vs));

  Primitives.register tbl "revoke-ssh-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "revoke-ssh-key: denied for anonymous")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr id_or_fp ] ->
            let* r =
              Auth_store.revoke_ssh_key t.store ~user_id:t.manifest.user_id
                ~id_or_fingerprint:id_or_fp
            in
            (match r with
             | Ok () -> Lwt.return Pricklypear_lisp.Ast.VNil
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("revoke-ssh-key: " ^ e)))
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "revoke-ssh-key: need fingerprint-or-id"));

  (* Smoke/admin: resolve openssh pubkey -> username | () *)
  Primitives.register tbl "verify-ssh-key"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr openssh ] ->
          let* u = Auth_store.verify_ssh_key t.store ~openssh_or_blob:openssh () in
          (match u with
           | Some user ->
               Lwt.return (Pricklypear_lisp.Ast.VStr user.Auth_store.username)
           | None -> Lwt.return Pricklypear_lisp.Ast.VNil)
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "verify-ssh-key: need openssh-pubkey-str"));

  (* Per-user LLM keys (auth.user-llm-keys) *)
  Primitives.register tbl "set-llm-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "set-llm-key: denied for anonymous")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr key ] ->
            let* r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ()
            in
            (match r with
             | Ok m ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("set-llm-key: " ^ e)))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base ] ->
            let* r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ()
            in
            (match r with
             | Ok m ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("set-llm-key: " ^ e)))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base;
            Pricklypear_lisp.Ast.VStr model ] ->
            let* r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ~model:(Some model) ()
            in
            (match r with
             | Ok m ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 Lwt.return (Pricklypear_lisp.Ast.VStr ("set-llm-key: " ^ e)))
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 "set-llm-key: need key-str [, base] [, model]"));

  Primitives.register tbl "clear-llm-key"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "clear-llm-key: denied for anonymous")
      else
        let* r =
          Auth_store.clear_llm_key t.store ~user_id:t.manifest.user_id
        in
        match r with
        | Ok () -> Lwt.return Pricklypear_lisp.Ast.VNil
        | Error e ->
            Lwt.return (Pricklypear_lisp.Ast.VStr ("clear-llm-key: " ^ e)));

  Primitives.register tbl "llm-key-status"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        Lwt.return (Pricklypear_lisp.Ast.VStr "llm-key-status: denied for anonymous")
      else
        let* meta =
          Auth_store.get_llm_key_meta t.store ~user_id:t.manifest.user_id
        in
        match meta with
        | None ->
            let env_set = Llm_client.api_key () <> "" in
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 (if env_set then
                    "configured=false env_fallback=true"
                  else "configured=false env_fallback=false"))
        | Some m ->
            let base =
              match m.base with Some b -> " base=" ^ b | None -> ""
            in
            let model =
              match m.model with Some m -> " model=" ^ m | None -> ""
            in
            Lwt.return
              (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "configured=true last4=%s%s%s"
                    m.last4 base model)));

  (* Coding tools: freeze AST rewrites into store *)
  Primitives.register tbl "llm-inner"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          llm_inner t ~name ~concept
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "llm-inner: need name-str, concept-str"));

  Primitives.register tbl "llm-outer"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          llm_outer t ~name ~concept
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "llm-outer: need name-str, concept-str"));

  Primitives.register tbl "llm-rewrite"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          llm_rewrite t ~mode:"replace" ~name ~concept
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept;
          Pricklypear_lisp.Ast.VStr mode ] ->
          llm_rewrite t ~mode ~name ~concept
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "llm-rewrite: need name, concept [, mode replace|wrap]"));

  Primitives.register tbl "llm-seed"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr requirement ] ->
          llm_seed t ~requirement ()
      | [ Pricklypear_lisp.Ast.VStr requirement;
          Pricklypear_lisp.Ast.VInt n ] ->
          llm_seed t ~requirement ~max_steps:(Int64.to_int n) ()
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr
               "llm-seed: need requirement-str [, max-steps int]"));

  (* Ops: structured log tail + analytics summary (product-experience). *)
  Primitives.register tbl "log-tail"
    (fun args ->
      let n, level =
        match args with
        | [] -> 50, None
        | [ Pricklypear_lisp.Ast.VInt n ] -> Int64.to_int n, None
        | [ Pricklypear_lisp.Ast.VStr lv ] -> 50, Ops_log.level_of_string lv
        | [ Pricklypear_lisp.Ast.VInt n; Pricklypear_lisp.Ast.VStr lv ] ->
            Int64.to_int n, Ops_log.level_of_string lv
        | [ Pricklypear_lisp.Ast.VStr lv; Pricklypear_lisp.Ast.VInt n ] ->
            Int64.to_int n, Ops_log.level_of_string lv
        | _ -> 50, None
      in
      let evs = Ops_log.tail ~n ?level () in
      Lwt.return
        (Pricklypear_lisp.Ast.VList
           (List.map Ops_log.event_to_value evs)));

  Primitives.register tbl "ops/summary"
    (fun _args ->
      let* ok, fail = Ops_incidents.count_logins t.store in
      let j = Ops_stats.summary_yojson ~logins_ok:ok ~logins_fail:fail in
      Lwt.return (Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j)));

  Primitives.register tbl "ops/incidents"
    (fun _args ->
      let* j = Ops_incidents.list_open_incidents t.store in
      Lwt.return (Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j)))


let method_to_string (m : http_method) : string =
  match m with `GET -> "GET" | `POST -> "POST"

let require_request (t : t) : request =
  match t.current_request with
  | Some r -> r
  | None ->
      raise
        (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.User_error "no active request"))

let str_name = function
  | Pricklypear_lisp.Ast.VStr s | Pricklypear_lisp.Ast.VSymbol s -> Some s
  | _ -> None

let depth_arg = function
  | None -> 1
  | Some (Pricklypear_lisp.Ast.VInt n) -> Int64.to_int n
  | Some (Pricklypear_lisp.Ast.VStr s) ->
      (try int_of_string s with _ -> 1)
  | Some _ -> 1

let vlist_strs xs =
  Pricklypear_lisp.Ast.VList (List.map (fun s -> Pricklypear_lisp.Ast.VStr s) xs)

let impact_to_json (e : impact_event) : string =
  let open Yojson.Safe in
  to_string
    (`Assoc
       [ ("name", `String e.name)
       ; ("at", `Float e.at)
       ; ("branch", `String e.branch)
       ; ("author", `String e.author)
       ; ("dependents", `List (List.map (fun s -> `String s) e.dependents))
       ; ("routes", `List (List.map (fun s -> `String s) e.routes))
       ; ("status", `String e.status)
       ])

let change_to_json (e : change_event) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("op", `String e.op)
       ; ("name", `String e.name)
       ; ("at", `Float e.at)
       ; ("branch", `String e.branch)
       ; ("author", `String e.author)
       ])

let record_define_side_effects = Call_graph.record_define_side_effects
let impact_summary_text = Call_graph.impact_summary_text

(** Graph + impact + change-set + DNU prims (image.inspector / dnu-stubs). *)
let register_graph_prims (t : t) (tbl : Primitives.t) : unit =
  Primitives.register tbl "callees"
    (fun args ->
      match args with
      | [ n ] | [ n; _ ] as a ->
          (match str_name n with
           | None -> Lwt.return (vlist_strs [])
           | Some name ->
               let depth =
                 match a with
                 | [ _; d ] -> depth_arg (Some d)
                 | _ -> 1
               in
               let* snap = Call_graph.snapshot_of_session t in
               Lwt.return (vlist_strs (Call_graph.callees snap ~name ~depth)))
      | _ -> Lwt.return (vlist_strs []));
  Primitives.register tbl "callers"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None -> Lwt.return (vlist_strs [])
           | Some name ->
               let* snap = Call_graph.snapshot_of_session t in
               Lwt.return (vlist_strs (Call_graph.callers snap name)))
      | _ -> Lwt.return (vlist_strs []));
  Primitives.register tbl "dependents"
    (fun args ->
      match args with
      | [ n ] | [ n; _ ] as a ->
          (match str_name n with
           | None -> Lwt.return (vlist_strs [])
           | Some name ->
               let depth =
                 match a with
                 | [ _; d ] -> depth_arg (Some d)
                 | _ -> 8
               in
               let* snap = Call_graph.snapshot_of_session t in
               Lwt.return
                 (vlist_strs (Call_graph.dependents snap ~name ~depth)))
      | _ -> Lwt.return (vlist_strs []));
  Primitives.register tbl "used-by-routes"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None -> Lwt.return (vlist_strs [])
           | Some name ->
               let* snap = Call_graph.snapshot_of_session t in
               Lwt.return (vlist_strs (Call_graph.used_by_routes snap name)))
      | _ -> Lwt.return (vlist_strs []));
  Primitives.register tbl "holes"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None -> Lwt.return (vlist_strs [])
           | Some name ->
               let* snap = Call_graph.snapshot_of_session t in
               Lwt.return (vlist_strs (Call_graph.holes snap name)))
      | _ -> Lwt.return (vlist_strs []));
  Primitives.register tbl "impact-queue"
    (fun _args ->
      let xs =
        List.map
          (fun e -> Pricklypear_lisp.Ast.VStr (impact_to_json e))
          t.impact_queue
      in
      Lwt.return (Pricklypear_lisp.Ast.VList xs));
  Primitives.register tbl "impact-clear"
    (fun args ->
      (match args with
       | [] | [ Pricklypear_lisp.Ast.VStr "all" ]
       | [ Pricklypear_lisp.Ast.VSymbol "all" ] ->
           t.impact_queue <- []
       | [ n ] ->
           (match str_name n with
            | Some name ->
                t.impact_queue <-
                  List.filter
                    (fun (e : impact_event) -> e.name <> name)
                    t.impact_queue
            | None -> ())
       | _ -> ());
      Lwt.return Pricklypear_lisp.Ast.VNil);
  Primitives.register tbl "impact-mark"
    (fun args ->
      match args with
      | [ n; st ] ->
          (match str_name n, str_name st with
           | Some name, Some status ->
               t.impact_queue <-
                 List.map
                   (fun (e : impact_event) ->
                     if e.name = name then { e with status } else e)
                   t.impact_queue;
               Lwt.return (Pricklypear_lisp.Ast.VStr status)
           | _ -> Lwt.return Pricklypear_lisp.Ast.VNil)
      | _ -> Lwt.return Pricklypear_lisp.Ast.VNil);
  Primitives.register tbl "change-transcript"
    (fun _args ->
      let xs =
        List.map
          (fun e -> Pricklypear_lisp.Ast.VStr (change_to_json e))
          t.change_transcript
      in
      Lwt.return (Pricklypear_lisp.Ast.VList xs));
  Primitives.register tbl "branch-changes"
    (fun args ->
      let* branch_id =
        match args with
        | [] -> Lwt.return (Some t.manifest.branch_id)
        | [ n ] ->
            (match str_name n with
             | None -> Lwt.return None
             | Some ref_ ->
                 let owner, name = split_branch_ref ref_ in
                 Store.lookup_branch_id_by_name t.store ~owner ~name)
        | _ -> Lwt.return None
      in
      match branch_id with
      | None -> Lwt.return (Pricklypear_lisp.Ast.VList [])
      | Some bid ->
          let* own = Store.list_own_functions t.store ~branch_id:bid in
          let* routes = Store.list_own_routes t.store ~branch_id:bid in
          let* parent_id = Store.branch_parent_id t.store ~branch_id:bid in
          let* ov =
            Store.list_overrides t.store ~branch_id:bid ~parent_id
          in
          let class_of name =
            match List.find_opt (fun (n, _, _) -> n = name) ov with
            | Some (_, _, None) -> "new"
            | Some (_, _, Some _) -> "override"
            | None -> "own"
          in
          let fn_items =
            List.map
              (fun (name, author, updated) ->
                let j =
                  Yojson.Safe.to_string
                    (`Assoc
                       [ ("name", `String name)
                       ; ("kind", `String "fn")
                       ; ("author", `String author)
                       ; ("updated_at", `String updated)
                       ; ("class", `String (class_of name))
                       ])
                in
                Pricklypear_lisp.Ast.VStr j)
              own
          in
          let route_items =
            List.map
              (fun (path, handler) ->
                let j =
                  Yojson.Safe.to_string
                    (`Assoc
                       [ ("name", `String path)
                       ; ("kind", `String "route")
                       ; ("handler", `String handler)
                       ; ("class", `String "route")
                       ])
                in
                Pricklypear_lisp.Ast.VStr j)
              routes
          in
          Lwt.return (Pricklypear_lisp.Ast.VList (fn_items @ route_items)));
  Primitives.register tbl "stub-function"
    (fun args ->
      match args with
      | [ n ] ->
          (match str_name n with
           | None ->
               Lwt.return
                 (Pricklypear_lisp.Ast.VStr "stub-function: need name")
           | Some name ->
               let* existing = Store.lookup_function t.store name in
               (match existing with
                | Some _ ->
                    Lwt.return
                      (Pricklypear_lisp.Ast.VStr
                         ("stub-function: already bound: " ^ name))
                | None ->
                    let body_src =
                      Printf.sprintf
                        "(lambda ()\n  \"TODO: implement %s\")" name
                    in
                    let* r =
                      Agent_tools.define_binding t ~name ~body_src
                        ~author:t.manifest.username
                    in
                    (match r with
                     | Error e ->
                         Lwt.return
                           (Pricklypear_lisp.Ast.VStr ("stub-function: " ^ e))
                     | Ok _ ->
                         let* () = record_define_side_effects t ~name in
                         Lwt.return
                           (Pricklypear_lisp.Ast.VStr
                              ("stubbed " ^ name ^ "; "
                               ^ impact_summary_text t name)))))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "stub-function: need 1 name"))

(** Register request-* + redirect prims. Closures capture [t] and read
    [current_request] at call time (handler-scoped dynamic binding). *)
let register_request_prims (t : t) (tbl : Primitives.t) : unit =
  Primitives.register_sync tbl "request-method" (fun _args ->
      let r = require_request t in
      Pricklypear_lisp.Ast.VStr (method_to_string r.method_));
  Primitives.register_sync tbl "request-path" (fun _args ->
      let r = require_request t in
      Pricklypear_lisp.Ast.VStr r.path);
  Primitives.register_sync tbl "request-body" (fun _args ->
      let r = require_request t in
      Pricklypear_lisp.Ast.VStr r.body);
  Primitives.register_sync tbl "request-param" (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr key ] ->
          let r = require_request t in
          (match List.assoc_opt key r.params with
           | Some v -> Pricklypear_lisp.Ast.VStr v
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error "request-param needs 1 string")));
  Primitives.register_sync tbl "request-header" (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr key ] ->
          let r = require_request t in
          let k = String.lowercase_ascii key in
          (match List.assoc_opt k r.headers with
           | Some v -> Pricklypear_lisp.Ast.VStr v
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error "request-header needs 1 string")));
  Primitives.register_sync tbl "hx-request?" (fun _args ->
      let r = require_request t in
      match List.assoc_opt "hx-request" r.headers with
      | Some v when v <> "" && v <> "false" -> Pricklypear_lisp.Ast.VBool true
      | _ -> Pricklypear_lisp.Ast.VBool false);
  Primitives.register_sync tbl "request-params" (fun _args ->
      let r = require_request t in
      let j =
        `Assoc
          (List.map (fun (k, v) -> (k, `String v)) r.params)
      in
      Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j));
  Primitives.register_sync tbl "redirect" (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          if String.length path = 0 || path.[0] <> '/' then
            raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.User_error
                    "redirect path must be absolute on host (start with /)"))
          else if String.contains path ':' then
            raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.User_error
                    "redirect rejects off-site URLs"))
          else Primitives.redirect_value path
      | _ ->
          raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.User_error "redirect needs 1 string path")))

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
let rec eval_one (t : t) (input : string) : Pricklypear_lisp.Ast.value Lwt.t =
  try
    let expr = Pricklypear_lisp.Reader.read_one input in
    (* Pre-eval intercept: (define name <body>) is the only AST-level
       special case left, because the name argument is a Var that must
       not be evaluated. Everything else (fork/merge/route/checkout)
       dispatches through Eval.apply via the prim_table.
       H1: [eval_string_auto] honors PP_EVAL_CONTROL=lwt|eff (default lwt). *)
    let* v = eval_string_rim t input in
    let persist_fn name body_expr doc =
      if t.seed_additive then
        Store.persist_function_if_absent t.store name body_expr ~doc
          t.manifest.username
      else
        Store.persist_function t.store name body_expr ~doc
          t.manifest.username
    in
    let persist_define name body_expr doc =
      let* () =
        if t.manifest.username <> "anonymous" then
          persist_fn name body_expr doc
        else Lwt.return_unit
      in
      let* () =
        if not t.suppress_refresh_on_define
           && (name = "ui/nav" || name = "ui/home" || name = "ui/css"
               || name = "ui/theme"
               || String.starts_with ~prefix:"theme/" name)
        then refresh_ui_shell t
        else Lwt.return_unit
      in
      let* () = record_define_side_effects t ~name in
      Lwt.return v
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
         let* () =
           if t.manifest.username <> "anonymous" then begin
             let stored =
               Pricklypear_lisp.Ast.App
                 (Pricklypear_lisp.Ast.Var "__macro__",
                  [ params_expr; body_expr ])
             in
             persist_fn name stored None
           end else Lwt.return_unit
         in
         Lwt.return v
     | _ -> Lwt.return v))
  with
  | Pricklypear_lisp.Ast.Interp_error e as exn -> Lwt.fail exn
  | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
      Lwt.fail (Pricklypear_lisp.Ast.Interp_error
                  (Pricklypear_lisp.Ast.Parse_error (msg, line, col)))
  | exn -> Lwt.fail exn


(* --- Library loader: same .pp text as in git libs/ --- *)

and eval_form (t : t) (expr : Pricklypear_lisp.Ast.expr)
    : Pricklypear_lisp.Ast.value Lwt.t =
  (* Like eval_one but takes AST — avoids pp_expr string round-trip. *)
  let* v = eval_expr_rim t expr in
  let persist_fn name body_expr doc =
    if t.seed_additive then
      Store.persist_function_if_absent t.store name body_expr ~doc
        t.manifest.username
    else
      Store.persist_function t.store name body_expr ~doc
        t.manifest.username
  in
  let persist_define name body_expr doc =
    let* () =
      if t.manifest.username <> "anonymous" then
        persist_fn name body_expr doc
      else Lwt.return_unit
    in
    let* () =
      if not t.suppress_refresh_on_define
         && (name = "ui/nav" || name = "ui/home" || name = "ui/css"
             || name = "ui/theme"
             || String.starts_with ~prefix:"theme/" name)
      then refresh_ui_shell t
      else Lwt.return_unit
    in
    let* () = record_define_side_effects t ~name in
    Lwt.return v
  in
  (match Pricklypear_lisp.Eval.parse_top_define expr with
   | Some { name; body; doc } -> persist_define name body doc
   | None ->
   (match expr with
   | Pricklypear_lisp.Ast.App
       (Pricklypear_lisp.Ast.Var "defmacro",
        [ Pricklypear_lisp.Ast.Var name; params_expr; body_expr ]) ->
       let* () =
         if t.manifest.username <> "anonymous" then begin
           let stored =
             Pricklypear_lisp.Ast.App
               (Pricklypear_lisp.Ast.Var "__macro__",
                [ params_expr; body_expr ])
           in
           persist_fn name stored None
         end else Lwt.return_unit
       in
       Lwt.return v
   | _ -> Lwt.return v))

and load_string (t : t) (src : string) : (string, string) result Lwt.t =
  Loader.load_string ~eval_form:(eval_form t) src

and load_library (t : t) (name : string) : (string, string) result Lwt.t =
  Loader.load_library ~eval_form:(eval_form t) name

(** branching.seed-policy: admin-only upsert of libs/<pkg>/lib.pp onto
    system/main (full clobber for names in that file). Does not touch
    personal/dev rows. seed_additive stays false so persist upserts. *)
and reseed_package (t : t) (pkg : string) : (string, string) result Lwt.t =
  let pkg = String.trim pkg in
  if pkg = "" then Lwt.return (Error "empty package name")
  else if (not t.is_admin)
          && t.manifest.username <> "admin"
          && t.manifest.username <> "system" then
    Lwt.return (Error "requires admin")
  else begin
    t.seed_additive <- false;
    t.suppress_refresh_on_define <- true;
    let finally () =
      t.suppress_refresh_on_define <- false;
      t.seed_additive <- false;
      Lwt.return_unit
    in
    Lwt.finalize
      (fun () ->
        Store.with_branch t.store ~branch_id:Store.system_main_id (fun () ->
          let* r = load_library t pkg in
          let* () = rehydrate t in
          let* () = refresh_ui_shell t in
          match r with
          | Ok msg ->
              Lwt.return
                (Ok (Printf.sprintf "reseeded %s → system/main: %s" pkg msg))
          | Error e -> Lwt.return (Error e)))
      finally
  end

and refresh_ui_shell (t : t) : unit Lwt.t =
  (* Product shell: nav HTML + theme CSS (image.product-theme).
     image.http-robustness invariant 3: this runs on every request, so
     it must NOT re-eval + re-warn per request. Cache success for 60s
     (ui_nav_refreshed_at / ui_css_refreshed_at) and, on failure, suppress
     re-attempt + warn for 60s (ui_nav_retry_at / ui_css_retry_at). Both
     are reset on checkout (branch switch). Emergency chrome fallback
     is unchanged: when ui_nav_html is None, http serves emergency nav. *)
  let shell_ttl = 60.0 in
  let now = Unix.gettimeofday () in
  let pp_exn exn =
    match exn with
    | Pricklypear_lisp.Ast.Interp_error e ->
        Pricklypear_lisp.Ast.pp_interp_error e
    | _ -> Printexc.to_string exn
  in
  let* () =
    match Pricklypear_lisp.Env.lookup t.env "ui/nav" with
    | None ->
        t.ui_nav_html <- None;
        Lwt.return_unit
    | Some _ ->
        if t.ui_nav_html <> None
           && now -. t.ui_nav_refreshed_at < shell_ttl then
          (* Cache fresh: serve cached nav, skip re-eval. *)
          Lwt.return_unit
        else if t.ui_nav_html = None && now < t.ui_nav_retry_at then
          (* Recently failed on this branch: skip re-eval + skip warn. *)
          Lwt.return_unit
        else
          Lwt.catch
            (fun () ->
              let* v = eval_one t "(ui/nav)" in
              (match Primitives.is_raw_html v with
               | Some s -> t.ui_nav_html <- Some s
               | None ->
                   (match v with
                    | Pricklypear_lisp.Ast.VStr s -> t.ui_nav_html <- Some s
                    | _ ->
                        t.ui_nav_html <-
                          Some (Pricklypear_lisp.Reader.pp_value v)));
              t.ui_nav_refreshed_at <- now;
              t.ui_nav_retry_at <- 0.0;
              Lwt.return_unit)
            (fun exn ->
              (* Warn once per retry window, then go quiet. Print the
                 full message so the failure is diagnosable. *)
              ignore
                (Ops_log.warn ~component:"session"
                   ~context:[("fn", "ui/nav")]
                   ("refresh_ui_shell failed (retry in "
                    ^ string_of_float shell_ttl ^ "s): "
                    ^ pp_exn exn));
              t.ui_nav_html <- None;
              t.ui_nav_retry_at <- now +. shell_ttl;
              Lwt.return_unit)
  in
  match Pricklypear_lisp.Env.lookup t.env "ui/css" with
  | None ->
      t.ui_css <- None;
      Lwt.return_unit
  | Some _ ->
      if t.ui_css <> None
         && now -. t.ui_css_refreshed_at < shell_ttl then
        Lwt.return_unit
      else if t.ui_css = None && now < t.ui_css_retry_at then
        Lwt.return_unit
      else
        Lwt.catch
          (fun () ->
            let* v = eval_one t "(ui/css)" in
            (match Primitives.is_raw_html v with
             | Some s -> t.ui_css <- Some s
             | None ->
                 (match v with
                  | Pricklypear_lisp.Ast.VStr s -> t.ui_css <- Some s
                  | _ ->
                      t.ui_css <-
                        Some (Pricklypear_lisp.Reader.pp_value v)));
            t.ui_css_refreshed_at <- now;
            t.ui_css_retry_at <- 0.0;
            Lwt.return_unit)
          (fun exn ->
            ignore
              (Ops_log.warn ~component:"session"
               ~context:[("fn", "ui/css")]
               ("refresh_ui_shell css failed (retry in "
                ^ string_of_float shell_ttl ^ "s): "
                ^ pp_exn exn));
            t.ui_css <- None;
            t.ui_css_retry_at <- now +. shell_ttl;
            Lwt.return_unit)

and seed_one (t : t) (name : string) : unit Lwt.t =
  let* r = load_library t name in
  (match r with
   | Ok msg -> Printf.eprintf "[session] %s: %s\n%!" name msg
   | Error e -> Printf.eprintf "[session] %s load: %s\n%!" name e);
  Lwt.return_unit

(** Ensure living agents note exists on system/main after notes seed.
    Idempotent: skip if slug agents already present. *)
and seed_agents_note (t : t) : unit Lwt.t =
  let body =
    String.concat "\n"
      [ "# Agents (living AGENTS.md)"
      ; ""
      ; "This note is the habitat AGENTS.md. Edit with notes/doc/upsert."
      ; ""
      ; "## Entry path"
      ; "1. Public bootstrap (no secrets): GET /agent.txt and GET /.well-known/agent.json"
      ; "2. Those docs explain HTTP Basic auth and how to build a REPL client (POST /api/eval)."
      ; "3. Local helper: pp-eval CLI or curl with Basic auth."
      ; "4. Then explore live:"
      ; "   (whoami)"
      ; "   (list-packages)"
      ; "   (load-library \"todo\")"
      ; "   (describe \"todo\")"
      ; "   (apropos-docs \"todo/item\")"
      ; "   (function-doc \"todo/item/add\")"
      ; "   (function-source \"todo/item/add\")"
      ; ""
      ; "## Selective discovery"
      ; "- (list-packages) — top-level package prefixes"
      ; "- (package-bindings \"pkg\") — all names under pkg/ (any depth)"
      ; "- (package-exports \"pkg\") — shallow pkg or pkg/local names"
      ; "- (apropos \"pat\") / (apropos-docs \"pat\") — search names (+ docs)"
      ; "- (describe \"name-or-pkg\") — one-shot orientation"
      ; "- Prefer DATA builders from /api/eval; */page handlers need a real HTTP request."
      ; ""
      ; "## Libraries"
      ; "Load before use: std ui notes todo outline bible dabar chat calendar"
      ; "flashcards bookmarks contacts plan bi weather ref verify ai layout welcome."
      ]
  in
  (* Escape body for a Lisp string literal. *)
  let escaped =
    let b = Buffer.create (String.length body + 32) in
    String.iter
      (function
        | '\\' -> Buffer.add_string b "\\\\"
        | '"' -> Buffer.add_string b "\\\""
        | '\n' -> Buffer.add_string b "\\n"
        | c -> Buffer.add_char b c)
      body;
    Buffer.contents b
  in
  let expr =
    Printf.sprintf
      {| (if (null? (notes/doc/get "agents"))
            (notes/doc/upsert "agents" "Agents (living AGENTS.md)"
              (outline/node* "root" "Agents (living AGENTS.md)" "%s" "" (list))
              "" "note" "")
            "agents-note-exists") |} 
      escaped
  in
  Lwt.catch
    (fun () ->
      let* v = eval_one t expr in
      Printf.eprintf "[session] agents-note: %s\n%!"
        (Pricklypear_lisp.Reader.pp_value v);
      Lwt.return_unit)
    (fun exn ->
      Printf.eprintf "[session] agents-note seed: %s\n%!" (pp_interp_exn exn);
      Lwt.return_unit)

and seed_stdlib (t : t) : unit Lwt.t =
  (* Package defaults always land on system/main — NEVER on the user's
     personal branch. Personal inherits main via the recursive CTE; user
     (define)s write overrides on personal and survive reseed/deploy.

     branching.seed-policy OPTION A: boot seed is ADDITIVE —
     persist_function_if_absent / register_route_if_absent via
     t.seed_additive. Existing system/main rows (e.g. from promote)
     are not clobbered on deploy/restart. Explicit (reseed-package)
     upserts from libs when an admin asks. Interactive load-library
     still upserts the CURRENT branch.

     suppress_refresh_on_define: the eval_one define-interceptor would
     otherwise fire refresh_ui_shell when ui/nav is defined mid-seed
     (before notes/capture/nav is bound) and fail with
     `unbound: notes/capture/nav`. The boot sequence does one explicit
     refresh_ui_shell AFTER all packages are loaded. *)
  Store.with_branch t.store ~branch_id:Store.system_main_id (fun () ->
    t.suppress_refresh_on_define <- true;
    t.seed_additive <- true;
    let finally () =
      t.suppress_refresh_on_define <- false;
      t.seed_additive <- false;
      Lwt.return_unit
    in
    Lwt.finalize
      (fun () ->
        let* () = seed_one t "std" in
    let* () = seed_one t "weather" in
    let* () = seed_one t "ui" in
    let* () = seed_one t "layout" in
    let* () = seed_one t "welcome" in
    let* () = seed_one t "ai" in
    let* () = seed_one t "outline" in
    let* () = seed_one t "structed" in
    let* () = seed_one t "notes" in
    let* () = seed_agents_note t in
    let* () = seed_one t "signup" in
    let* () = seed_one t "todo" in
    let* () = seed_one t "contacts" in
    let* () = seed_one t "plan" in
    let* () = seed_one t "chat" in
    let* () = seed_one t "bookmarks" in
    let* () = seed_one t "calendar" in
    let* () = seed_one t "workout" in
    let* () = seed_one t "flashcards" in
    let* () = seed_one t "bible" in
    let* () = seed_one t "dabar" in
    let* () = seed_one t "bi" in
    let* () = seed_one t "ref" in
    let* () = seed_one t "verify" in
    Lwt.return_unit)
      finally)

(** Ensure username/personal exists as an EMPTY child of system/main.
    Inheritance (not copy) means main defaults stay visible; personal
    only stores the user's overrides. *)
and ensure_personal_branch (t : t) : string option Lwt.t =
  let owner = t.manifest.username in
  let* main_id_opt =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  let parent_id =
    match main_id_opt with
    | Some id -> id
    | None -> Store.system_main_id
  in
  Store.ensure_branch t.store ~parent_id ~owner ~name:"personal"

and checkout_personal (t : t) : unit Lwt.t =
  let owner = t.manifest.username in
  let branch_ref = owner ^ "/personal" in
  let* found =
    Store.lookup_branch_id_by_name t.store ~owner ~name:"personal"
  in
  match found with
  | None ->
      Printf.eprintf
        "[session] personal branch missing; staying on %s\n%!"
        t.manifest.branch_name;
      (* Still rehydrate whatever branch we have. *)
      rehydrate t
  | Some id ->
      Printf.eprintf "[session] checkout %s (%s)\n%!" branch_ref id;
      checkout_to t ~branch_id:id ~branch_ref

(** Agent branch: owner = parent human's username, name = "agent".
    Sibling of the parent's personal; agent boots here, not in personal. *)
and ensure_agent_branch (t : t) ~(parent_username : string)
  : string option Lwt.t =
  let* main_id_opt =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  let parent_id =
    match main_id_opt with
    | Some id -> id
    | None -> Store.system_main_id
  in
  Store.ensure_branch t.store ~parent_id ~owner:parent_username ~name:"agent"

and checkout_agent (t : t) ~(parent_username : string) : unit Lwt.t =
  let branch_ref = parent_username ^ "/agent" in
  let* found =
    Store.lookup_branch_id_by_name t.store ~owner:parent_username
      ~name:"agent"
  in
  match found with
  | None ->
      Printf.eprintf
        "[session] agent branch missing; staying on %s\n%!"
        t.manifest.branch_name;
      rehydrate t
  | Some id ->
      Printf.eprintf "[session] checkout %s (%s)\n%!" branch_ref id;
      checkout_to t ~branch_id:id ~branch_ref

and recipe_hello (t : t) : (string, string) result Lwt.t =
  load_library t "hello"

and recipe_todo (t : t) : (string, string) result Lwt.t =
  load_library t "todo"

and recipe_bible (t : t) : (string, string) result Lwt.t =
  (* Types + API + /bible only — corpus via scripts/seed_bible_bsb.py *)
  load_library t "bible"

and recipe_dabar (t : t) : (string, string) result Lwt.t =
  (* Study layer — requires bible/*; Strong's/interlinear via seed scripts *)
  let* r = load_library t "bible" in
  (match r with
   | Error e -> Lwt.return (Error e)
   | Ok _ -> load_library t "dabar")

and recipe_notes (t : t) : (string, string) result Lwt.t =
  (* Outline-first notes — outline pure lib then notes package *)
  let* r = load_library t "outline" in
  (match r with
   | Error e -> Lwt.return (Error e)
   | Ok _ -> load_library t "notes")


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
      env;
      prims = prim_table;  (* temporary; replaced below *)
      routes;
      started_at = Unix.gettimeofday ();
      kernel_catalog = [];  (* filled after all prims register *)
      ui_nav_html = None;
      ui_css = None;
      ui_nav_refreshed_at = 0.0;
      ui_nav_retry_at = 0.0;
      ui_css_refreshed_at = 0.0;
      ui_css_retry_at = 0.0;
      suppress_refresh_on_define = false;
      seed_additive = false;
      current_request = None;
      agent_branch_lock = false;
      is_admin = (manifest.username = "admin");
      auth_token = None;
      impact_queue = [];
      change_transcript = [];
    }
  in
  (* Register session-aware primitives (whoami, current-author,
     list-bindings). username_fn re-reads t.manifest so per-request
     identity attach updates (whoami) without re-registering. *)
  Primitives.register_session_prims prim_table
    ~username_fn:(fun () ->
      (match Lwt.get Request_context.current with
       | Some c -> (Request_context.manifest c).username
       | None -> (!t).manifest.username)
    ) ~env;
  (* Register the DB-touching control primitives on the prim_table,
     then rebind t.prims to the populated table. These prims capture
     (!t), so they read/write store+manifest on each invocation. *)
  register_session_control_prims !t prim_table;
  (* HTTP request-context prims (it33): request-method/path/param(s)/body +
     redirect. Dynamic scope via current_request while call_handler runs. *)
  register_request_prims !t prim_table;
  (* Call graph + impact + change-set + DNU (image.inspector combo pack). *)
  register_graph_prims !t prim_table;
  (* Register CRUD primitives (get/create/update/delete/list) over
     the shared data_rows table. Same pattern as control prims:
     they capture (!t) and return their real DB result via Lwt. *)
  register_crud_prims !t prim_table;
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
  Primitives.register prim_table "load-string"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr src ] ->
          let* r = load_string !t src in
          (match r with
           | Ok msg -> Lwt.return (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               Lwt.return (Pricklypear_lisp.Ast.VStr ("load-string: " ^ e)))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "load-string: need 1 string"));
  Primitives.register prim_table "load-library"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let* r = load_library !t name in
          let* () = refresh_ui_shell !t in
          (match r with
           | Ok msg -> Lwt.return (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               Lwt.return (Pricklypear_lisp.Ast.VStr ("load-library: " ^ e)))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "load-library: need 1 string"));
  (* (reseed-package pkg) — admin-only upsert libs/<pkg> onto system/main. *)
  Primitives.register prim_table "reseed-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let* r = reseed_package !t pkg in
          (match r with
           | Ok msg -> Lwt.return (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               Lwt.return (Pricklypear_lisp.Ast.VStr ("reseed-package: " ^ e)))
      | _ ->
          Lwt.return
            (Pricklypear_lisp.Ast.VStr "reseed-package: need package-str"));
  let final_prims = prim_table in
  let kernel_catalog = Primitives.catalog prim_table in
  (* Mutate the prims field in place (rather than building a fresh
     record via { (!t) with prims = ... }). Closures captured by
     register_session_control_prims / register_crud_prims read
     t.prims at CALL time (e.g. __with_transaction__'s Eval.apply
     ~prims:t.prims) and need the populated table, not the temporary
     snapshot from step 3. Lands it29: this was latent until
     with-transaction's body needed to call back into the prim
     registry itself. The earlier control prims (fork/merge/route/
     checkout) didn't trip this because they only invoke Store.*
     functions, never Eval.apply against the prims table. *)
  (!t).prims <- final_prims;
  (!t).kernel_catalog <- kernel_catalog;
  (* Bind load prims into env (registered after the bulk VPrim bind). *)
  Pricklypear_lisp.Env.define env "load-string" (Pricklypear_lisp.Ast.VPrim "load-string");
  Pricklypear_lisp.Env.define env "load-library" (Pricklypear_lisp.Ast.VPrim "load-library");
  Pricklypear_lisp.Env.define env "reseed-package" (Pricklypear_lisp.Ast.VPrim "reseed-package");
  let session : t = !t in
  (* Boot order (personal-branch safety):
       1. ensure username/personal exists (empty child of system/main)
       2. seed_stdlib writes package defaults ONLY to system/main
       3. checkout personal + rehydrate env (inherits main via CTE)
       4. refresh ui shell from the personal-visible ui/nav
     User (define)s land on personal and are never wiped by reseed. *)
  Lwt.async (fun () ->
    let* _ = ensure_personal_branch session in
    let* () = seed_stdlib session in
    let* () = checkout_personal session in
    let* () = refresh_ui_shell session in
    Lwt.return_unit);
  session


let uptime_seconds (t : t) : int =
  int_of_float (Unix.gettimeofday () -. t.started_at)

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
let env (t : t) : Pricklypear_lisp.Ast.env = t.env

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
    () : unit Lwt.t =
  let prev = t.manifest.username in
  t.manifest <-
    { t.manifest with user_id; username };
  t.is_admin <- is_admin;
  t.auth_token <- auth_token;
  if checkout_personal_now || prev <> username then
    if user_type = "agent" then
      (* Agent boots into <parent>/agent, not <username>/personal. *)
      (match parent_user_id with
       | Some pid ->
           let* parent_opt = Auth_store.get_user_by_id t.store pid in
           (match parent_opt with
            | Some p ->
                let* _ = ensure_agent_branch t ~parent_username:p.username in
                checkout_agent t ~parent_username:p.username
            | None ->
                Printf.eprintf
                  "[session] agent %s has no parent; staying on %s\n%!"
                  username t.manifest.branch_name;
                Lwt.return_unit)
       | None ->
           Printf.eprintf
             "[session] agent %s has no parent_user_id; staying on %s\n%!"
             username t.manifest.branch_name;
           Lwt.return_unit)
    else
      let* _ = ensure_personal_branch t in
      checkout_personal t
  else
    Lwt.return_unit

(** Anonymous request identity (auth.route-visibility).
    system/main only; no personal garden; no admin. *)
let attach_anonymous (t : t) : unit Lwt.t =
  t.manifest <-
    { t.manifest with
      user_id = "anonymous"
    ; username = "anonymous"
    };
  t.is_admin <- false;
  t.auth_token <- None;
  (* Prefer system/main for package defaults. *)
  let* found =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  match found with
  | None -> Lwt.return_unit
  | Some id ->
      if t.manifest.branch_id = id then Lwt.return_unit
      else checkout_to t ~branch_id:id ~branch_ref:"system/main"

let clear_auth_token (t : t) : unit =
  t.auth_token <- None

(** Resolve owner/name ref and checkout. *)
let checkout_ref (t : t) (branch_ref : string) : (unit, string) result Lwt.t =
  let owner, name = split_branch_ref branch_ref in
  let ref' = owner ^ "/" ^ name in
  let* found = Store.lookup_branch_id_by_name t.store ~owner ~name in
  match found with
  | None -> Lwt.return (Error "no such branch")
  | Some id ->
      let* () = checkout_to t ~branch_id:id ~branch_ref:ref' in
      Lwt.return (Ok ())

(** Delete a function row on the current branch (unoverride). Rehydrates. *)
let unoverride (t : t) (name : string) : (bool, string) result Lwt.t =
  let bid = Store.current_branch_id t.store in
  let* deleted =
    Store.delete_function_on_branch t.store ~branch_id:bid ~name
  in
  let* () = rehydrate t in
  let* () = refresh_ui_shell t in
  Lwt.return (Ok deleted)

(** Promote selected names from source ref onto system/main (admin policy). *)
let promote_to_main (t : t) ~(source_ref : string) ~(names : string list)
  : (int, string) result Lwt.t =
  if names = [] then Lwt.return (Error "no names selected")
  else
    let owner, name = split_branch_ref source_ref in
    let* src =
      Store.lookup_branch_id_by_name t.store ~owner ~name
    in
    match src with
    | None -> Lwt.return (Error "no such source branch")
    | Some source_id ->
        (* Policy: only admin username may write system/main via promote UI. *)
        if (not t.is_admin)
           && t.manifest.username <> "admin"
           && t.manifest.username <> "system" then
          Lwt.return (Error "promote to system/main requires admin")
        else
          let* n =
            Store.promote_functions t.store
              ~source_id ~target_id:Store.system_main_id ~names
          in
          (* If we're on main, rehydrate so env sees new bodies. *)
          let* () =
            if Store.current_branch_id t.store = Store.system_main_id then
              rehydrate t
            else Lwt.return_unit
          in
          Lwt.return (Ok n)

(** Agent promotes from its current branch (admin/agent) into the parent
    human's personal branch. Gated by the parent's allow_agent_promote
    toggle. Empty names = all functions defined ON the current branch
    (depth 0, not inherited). *)
(** Full fork: parent_ref -> owner/child_name (uses Store.fork_branch copy). *)
let fork_ref (t : t) ~(parent_ref : string) ~(child_ref : string)
  : (string, string) result Lwt.t =
  let p_owner, p_name = split_branch_ref parent_ref in
  let c_owner, c_name = split_branch_ref child_ref in
  let* parent_id =
    Store.lookup_branch_id_by_name t.store ~owner:p_owner ~name:p_name
  in
  match parent_id with
  | None -> Lwt.return (Error "no such parent branch")
  | Some pid ->
      let* created =
        Store.fork_branch t.store ~parent_id:pid ~owner:c_owner
          ~child_name:c_name
      in
      (match created with
       | None -> Lwt.return (Error "fork failed")
       | Some id -> Lwt.return (Ok id))

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

let ui_nav_html (t : t) : string option = t.ui_nav_html

let set_ui_nav_html (t : t) (s : string option) : unit = t.ui_nav_html <- s

let ui_css (t : t) : string option = t.ui_css

let set_ui_css (t : t) (s : string option) : unit = t.ui_css <- s


let list_pages (t : t) : (string * string) list =
  Hashtbl.fold
    (fun route (handler, _) acc -> (route, handler) :: acc) t.routes []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let list_pages_meta (t : t) : (string * string * string) list =
  Hashtbl.fold
    (fun route (handler, vis) acc -> (route, handler, vis) :: acc) t.routes []
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)

let set_route (t : t) ~(route : string) ~(handler : string)
    ?visibility () : unit Lwt.t =
  let vis =
    match visibility with
    | Some v -> Store.normalize_visibility v
    | None ->
        (match Hashtbl.find_opt t.routes route with
         | Some (_, v) -> v
         | None -> "private")
  in
  Hashtbl.replace t.routes route (handler, vis);
  let* _ =
    match visibility with
    | Some v ->
        Store.register_route t.store ~route ~handler
          ~visibility:(Store.normalize_visibility v) ()
    | None -> Store.register_route t.store ~route ~handler ()
  in
  Lwt.return_unit

(** Result of invoking a route handler. Errors keep the real message
    (e.g. "budget exceeded") so HTTP can show it — never mask as bare None. *)
type handler_call =
  | Handler_ok of Pricklypear_lisp.Ast.value
  | Handler_unbound
  | Handler_error of string

(* Invoke a named handler function (looked up via env). The handler
   must be a zero-arg VLambda or a VPrim name.
   When [~request] is provided, request-* prims see that context for
   the duration of the call (cleared in finalize). *)
let call_handler (t : t) ?request (handler_name : string)
  : handler_call Lwt.t =
  (* Resolve the handler env against the per-request snapshot when bound
     (image.per-request-context): a concurrent attach_anonymous +
     checkout_to mid-request would otherwise rehydrate t.env to system/main
     and this lookup would miss personal-branch handlers. Fall back to t.env
     when no context (REPL, /api/eval outside HTTP gate, pre-gate paths). *)
  let env =
    match Lwt.get Request_context.current with
    | Some ctx -> Request_context.env ctx
    | None -> t.env
  in
  match Pricklypear_lisp.Env.lookup env handler_name with
  | None -> Lwt.return Handler_unbound
  | Some _fn ->
      (* Evaluate (handler-name) — zero args. Re-uses eval's apply path
         so VLambda closures AND VPrim dispatch both work. *)
      let run () =
        Lwt.catch
          (fun () ->
             let* v =
               eval_string_rim t ("(" ^ handler_name ^ ")")
             in
             Lwt.return (Handler_ok v))
          (fun exn ->
             let msg = pp_interp_exn exn in
             Lwt.async (fun () ->
               Ops_incidents.emit_and_maybe_file t
                 ~level:Ops_log.Error ~component:"handler"
                 ~file:(not (Ops_incidents.is_expected_runtime_error exn))
                 ~context:[("handler", handler_name)]
                 msg);
             Lwt.return (Handler_error msg))
      in
      (match request with
       | None -> run ()
       | Some req ->
           t.current_request <- Some req;
           Lwt.finalize run (fun () ->
               t.current_request <- None;
               Lwt.return_unit))
