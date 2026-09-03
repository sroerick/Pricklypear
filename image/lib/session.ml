(** Pricklypear Image — session state for eval / HTTP / REPL.

    Holds the env (function defs for the current branch, plus any
    (define)'d locals), the boot manifest (default identity), auth
    attachment fields, and the store handle.

    Per spec [image.repl-experience]:
      - (define ...) persists to the store: survives image reboot.
      - REPL-local let bindings are session-local: NOT persisted.
      - Switching branch (checkout) keeps REPL locals but changes visibility. *)
include Session_types

(** Run direct-style eval under the Lwt prim rim (H1.7). *)
let eval_under_rim (t : t) (thunk : unit -> 'a) : 'a =
  Control_rim.run ~prims:t.prims thunk

let persist_last_budget_stats (t : t) ~(label : string) : unit =
  match !Pricklypear_lisp.Eval.last_budget with
  | None -> ()
  | Some b when label = "" -> ()
  | Some b ->
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
           ~author:t.manifest.username
       with _ -> ())

let eval_string_rim (t : t) (input : string) : Pricklypear_lisp.Ast.value =
  let v =
    eval_under_rim t (fun () ->
        Pricklypear_lisp.Eval.eval_string_direct
          ~prims:Pricklypear_lisp.Eval.empty_prims t.env input)
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
  then persist_last_budget_stats t ~label;
  v

(* habitat-book.eval-examples: shared engine for the (example-eval …)
   prim and the POST /book/eval-example endpoint. Restriction lives in
   Example_eval (whitelisted prim table + static gate + child env +
   fresh budget); known_fn comes from the branch's visible defs — the
   same body source forge-effects uses. *)
let example_eval_engine (t : t) (src : string) : Pricklypear_lisp.Ast.value =
  let known_fn =
    Pricklypear_lisp.Effect.known_fn_of_bindings
      (List.map (fun (n, e, _) -> (n, e))
         (Store.list_function_names t.store))
  in
  Example_eval.eval_source ~env:t.env ~full_prims:t.prims ~known_fn src

let eval_expr_rim (t : t) (expr : Pricklypear_lisp.Ast.expr)
  : Pricklypear_lisp.Ast.value =
  let budget = Pricklypear_lisp.Eval.make_budget () in
  Pricklypear_lisp.Eval.last_budget := Some budget;
  eval_under_rim t (fun () ->
      Pricklypear_lisp.Eval.eval
        ~budget ~prims:Pricklypear_lisp.Eval.empty_prims t.env expr)

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
  (* Kernel prim names already bound as VPrim on rehydrate. Never let a
     stale function_defs row re-poison them (BORGE-1785213238). *)
  let is_kernel_prim name =
    List.exists (fun (n, _) -> n = name) t.kernel_catalog
  in
  let _ =
    List.iter
      (fun (name, body, _author, pkg) ->
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
                  Pricklypear_lisp.Env.define t.env name
                    (Pricklypear_lisp.Ast.VMacro (params, body_expr, t.env));
                  ()
              | _ ->
                  let force env' expr' =
                    eval_under_rim t (fun () ->
                        let b =
                          match !Pricklypear_lisp.Eval.current_budget with
                          | Some b -> b
                          | None -> Pricklypear_lisp.Eval.make_budget ()
                        in
                        Pricklypear_lisp.Eval.eval ~budget:b
                          ~prims:Pricklypear_lisp.Eval.empty_prims env' expr')
                  in
                  Pricklypear_lisp.Env.define_thunk t.env name ~force body))
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
  t.load_stamp <- Store.visible_stamp t.store

(** Clear env + routes and reload from the store for the current branch.
    Re-binds kernel prims from [t.kernel_catalog] first so checkout does
    not lose +/define/load-library/etc. *)
let rehydrate (t : t) : unit =
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
  Pricklypear_lisp.Env.define t.env "eval-string"
    (Pricklypear_lisp.Ast.VPrim "eval-string");
  Pricklypear_lisp.Env.define t.env "load-library"
    (Pricklypear_lisp.Ast.VPrim "load-library");
  (* lisp-interpreter.math: pi is a bound value, not a zero-arg prim.
     Rebind on every rehydrate so /api/eval sessions see it. *)
  Pricklypear_lisp.Env.define t.env "pi"
    (Pricklypear_lisp.Ast.VFloat Float.pi);
  load_defs t;
  t.load_stamp <- Store.visible_stamp t.store

(** Rehydrate iff the visible def chain changed since the last load.
    Called by the session registry at attach time; with image.lazy-env-load
    the rehydrate itself is a CTE + thunk binds (no body evals), so the
    whole check stays in the low-ms range. *)
let ensure_fresh (t : t) : unit =
  match Store.visible_stamp t.store with
  | "" -> ()  (* no DB: stay on whatever is in memory *)
  | stamp when stamp = t.load_stamp -> ()
  | _ -> rehydrate t

(** Force the next refresh_ui_shell to re-eval ui/nav + ui/css.
    image.product-theme: invalidation is DEPENDENCY-TRACKED — a define
    busts iff the defined name was in the last shell eval's recorded
    read-set (see refresh_ui_shell); no TTL remains. Checkout still
    busts unconditionally (read-sets are per-branch), as do
    load-library / reseed (bulk redefines). *)
let bust_ui_shell_cache (t : t) : unit =
  t.ui_nav_html <- None;
  t.ui_css <- None;
  t.ui_shell_reads <- [];
  t.ui_shell_retry_at <- 0.0

(** image.product-theme: true when [name] was read by the last shell
    eval. Conservative when no read-set exists yet (never refreshed):
    fall back to the old shell-prefix set so first-refresh still
    triggers on shell-name defines. *)
let touches_shell (t : t) (name : string) : bool =
  match t.ui_shell_reads with
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

let split_branch_ref (s : string) : string * string =
  match String.split_on_char '/' s with
  | [ o; n ] -> o, n
  | [ n ] -> "system", n  (* bare name = system/<name> *)
  | _ -> "system", s

(** Branch-owner authorization (i-1787970640, spec branching.personal-
    workspace): a session with username U (admin = t.is_admin || U =
    "admin") may target a branch owner O iff O = U or the session is
    admin. Agent users are named "<parent>/agent" and work in the
    parent-owned <parent>/agent workshop branch, so their tenant is the
    parent username. *)
let branch_owner_authorized (t : t) (owner : string) : bool =
  let u = t.manifest.username in
  if t.is_admin || u = "admin" then true
  else
    let tenant =
      let suffix = "/agent" in
      let n = String.length u and m = String.length suffix in
      if n > m && String.sub u (n - m) m = suffix then
        String.sub u 0 (n - m)
      else u
    in
    owner = u || owner = tenant

let register_session_control_prims (t : t) (tbl : Primitives.t) : unit =
  (* (fork 'parent 'child) -> VStr new-branch-id | VStr "no such parent" *)
  Primitives.register_server_only tbl "fork"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("fork", "denied for anonymous", "denied"))))
      else if t.agent_branch_lock then
        (Pricklypear_lisp.Ast.VStr
             "fork blocked: agent is locked to the current branch")
      else
      match args with
      | [ Pricklypear_lisp.Ast.VStr parent_name;
          Pricklypear_lisp.Ast.VStr child_name ] ->
          let parent_owner, parent_short = split_branch_ref parent_name in
          let child_owner, child_short = split_branch_ref child_name in
          let parent_id =
            Store.lookup_branch_id_by_name
              t.store ~owner:parent_owner ~name:parent_short
          in
          (match parent_id with
           | None ->
               (Pricklypear_lisp.Ast.VStr
                             (Printf.sprintf "no such parent: %s" parent_name))
           | Some pid ->
               (* i-1787970640: child_owner must pass the branch-owner rule
                  (blocks pre-claiming another user's garden). *)
               if not (branch_owner_authorized t child_owner) then
                 (Pricklypear_lisp.Ast.VStr
                    (Printf.sprintf "denied: branch %s is owned by %s"
                       child_name child_owner))
               else
               let new_id =
                 Store.fork_branch t.store ~parent_id:pid
                   ~owner:child_owner ~child_name:child_short
               in
               (match new_id with
                | Some id ->
                    (Pricklypear_lisp.Ast.VStr id)
                | None ->
                    (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "fork failed", "failed"))))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("fork", "bad args (need 2 strings)", "bad-args")))));

  (* (merge 'source 'target) -> VStr "merged N rows" | raise (denied).
     POLICY (per spec [branching.merge]): merging into `system/*`
     requires the session username to be "admin"; merges into the
     caller's own `owner/*` branches are free. *)
  Primitives.register_server_only tbl "merge"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("merge", "denied for anonymous", "denied"))))
      else if t.agent_branch_lock then
        (Pricklypear_lisp.Ast.VStr
             "merge blocked: agent is locked to the current branch")
      else
      match args with
      | [ Pricklypear_lisp.Ast.VStr source_ref;
          Pricklypear_lisp.Ast.VStr target_ref ] ->
          let src_owner, src_short = split_branch_ref source_ref in
          let tgt_owner, tgt_short = split_branch_ref target_ref in
          (* i-1787970640: BOTH src_owner and tgt_owner must pass the
             branch-owner rule. Target system/* stays admin-only (spec
             branching.merge); the source check is new — it blocks reading
             others' code via merge results. *)
          if (tgt_owner = "system")
             && not (t.is_admin || t.manifest.username = "admin")
          then
            (Pricklypear_lisp.Ast.VStr
                          "denied: admin-only merge to system branch")
          else if not (branch_owner_authorized t src_owner) then
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "denied: branch %s is owned by %s"
                  source_ref src_owner))
          else if not (branch_owner_authorized t tgt_owner) then
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "denied: branch %s is owned by %s"
                  target_ref tgt_owner))
          else
            let src_id =
              Store.lookup_branch_id_by_name
                t.store ~owner:src_owner ~name:src_short
            in
            let tgt_id =
              Store.lookup_branch_id_by_name
                t.store ~owner:tgt_owner ~name:tgt_short
            in
            (match src_id, tgt_id with
             | Some s, Some tg ->
                 let r = Store.merge_branch t.store
                            ~source_id:s ~target_id:tg in
                 (match r with
                  | Some res ->
                      (Pricklypear_lisp.Ast.VStr
                                    (Store.format_merge_result res))
                  | None ->
                      (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "merge failed", "failed")))))
             | _ ->
                 (Pricklypear_lisp.Ast.VStr
                               "source or target branch not found"))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("merge", "bad args (need 2 strings)", "bad-args")))));

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
  let check_public_handler (handler : string) : (unit, string) result =
    (* Fail-closed public-route gate (BORGE-1785212463 / 1785212603).
       Infer effects from the in-memory env (synchronous, fresh) via the
       pure [Route_gate] module. The previous store read was stale after
       the define persist race and DEFAULTED TO ALLOW on unknown handlers;
       now unknown / non-function handlers are DENIED. *)
    (Pricklypear_lisp.Route_gate.check_public_handler ~env:t.env handler)
  in
  let register_route_checked ~route_path ~handler ~visibility
      ~(pass_vis_to_store : bool) : Pricklypear_lisp.Ast.value =
    let gate =
      if visibility = "public" then check_public_handler handler
      else (Ok ())
    in
    match gate with
    | Error msg -> (Pricklypear_lisp.Ast.VStr msg)
    | Ok () ->
        Hashtbl.replace t.routes route_path (handler, visibility);
        let _ =
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
        (Pricklypear_lisp.Ast.VStr route_path)
  in
  Primitives.register_server_only tbl "route"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("route", "denied for anonymous", "denied"))))
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
                 (Pricklypear_lisp.Ast.VStr
                      "route: visibility must be public or private")
             | Some vis ->
                 register_route_checked ~route_path ~handler ~visibility:vis
                   ~pass_vis_to_store:true)
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("route", "bad args", "bad-args")))));

  (* (public-route path handler) sugar *)
  Primitives.register tbl "public-route"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("public-route", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler ] ->
            register_route_checked ~route_path ~handler ~visibility:"public"
              ~pass_vis_to_store:true
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("public-route", "bad args", "bad-args")))));

  (* (route-visibility path) -> "public" | "private" | () *)
  Primitives.register tbl "route-visibility"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr route_path ] ->
          (match Hashtbl.find_opt t.routes route_path with
           | Some (_, vis) ->
               (Pricklypear_lisp.Ast.VStr vis)
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ -> Pricklypear_lisp.Ast.VNil);

  (* (checkout 'branch-ref) -> VStr "checked out" | "no such branch".
     Mutates t.store.branch_id + t.manifest, then re-hydrates the env
     from the new branch's function_defs (sync — so the next form sees
     the right bindings). Blocked while agent_branch_lock is set. *)
  Primitives.register_server_only tbl "checkout"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("checkout", "denied for anonymous", "denied"))))
      else if t.agent_branch_lock then
        (Pricklypear_lisp.Ast.VStr
             "checkout blocked: agent is locked to the current branch")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr branch_ref ] ->
            let owner, name = split_branch_ref branch_ref in
            (* i-1787970640: owner of the target ref must pass the
               branch-owner rule (system/* included — non-admin users
               never need main; personal branches inherit it). *)
            if not (branch_owner_authorized t owner) then
              (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "denied: branch %s is owned by %s"
                    branch_ref owner))
            else
            let found =
              Store.lookup_branch_id_by_name t.store ~owner ~name
            in
            (match found with
             | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("checkout", "no such branch", "not-found")))
             | Some new_id ->
                 let () = checkout_to t ~branch_id:new_id ~branch_ref in
                 (Pricklypear_lisp.Ast.VStr "checked out"))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("checkout", "bad args", "bad-args")))));

  (* (unbind name) -> VStr "unbound" | "nothing to unbind" | error.
     Deletes ONLY the current branch's function_defs row for name;
     the name then resolves via inheritance to the shadowed parent
     binding (or becomes unbound). Refuses on system/main and on
     anonymous sessions. See branching.unbind spec. *)
  Primitives.register tbl "unbind"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unbind", "denied for anonymous", "denied"))))
      else
        (* i-1787970640: the current branch's owner must pass the
           branch-owner rule (delete_function already refuses system/main
           and the prim refuses anonymous). *)
        let cur_owner, _cur_name =
          split_branch_ref t.manifest.branch_name in
        if not (branch_owner_authorized t cur_owner) then
          (Pricklypear_lisp.Ast.VStr
             (Printf.sprintf "denied: branch %s is owned by %s"
                t.manifest.branch_name cur_owner))
        else
        match args with
        | [ Pricklypear_lisp.Ast.VStr name ] ->
            (try
              let deleted = Store.delete_function t.store name in
              if deleted then begin
                (* Rehydrate so the env sees the revealed parent binding *)
                rehydrate t;
                (Pricklypear_lisp.Ast.VStr "unbound")
              end else
                (Pricklypear_lisp.Ast.VStr "nothing to unbind")
            with Failure msg ->
              (Pricklypear_lisp.Ast.VStr msg))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unbind", "need a name", "bad-args")))));

  (* (prune-synced) -> VStr JSON {"pruned": N, "names": [...]}.
     branching.pin-hygiene: unbinds every synced FN pin on the current
     branch (body canonically identical to the parent chain — a
     contentless pin). ahead/ahead-new/diverged pins untouched.
     Denied for anonymous; system/main is a no-op (a root pin can never
     be synced). Same branch-owner rule as unbind. *)
  Primitives.register tbl "prune-synced"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prune-synced", "denied for anonymous", "denied"))))
      else
        (match args with
         | [] ->
             let cur_owner, _cur_name =
               split_branch_ref t.manifest.branch_name in
             if not (branch_owner_authorized t cur_owner) then
               (Pricklypear_lisp.Ast.VStr
                  (Printf.sprintf "denied: branch %s is owned by %s"
                     t.manifest.branch_name cur_owner))
             else if t.store.Store.branch_id = Store.system_main_id then
               (Pricklypear_lisp.Ast.VStr
                  (Yojson.Safe.to_string
                     (`Assoc [ "pruned", `Int 0;
                               "note", `String "system/main: nothing can be synced" ])))
             else
               let names = Store.prune_synced_fn_pins t.store in
               let () = if names <> [] then rehydrate t in
               Pricklypear_lisp.Ast.VStr
                 (Yojson.Safe.to_string
                    (`Assoc
                      [ "pruned", `Int (List.length names);
                        "names",
                          `List
                            (List.map (fun n -> `String n)
                               (List.rev names)) ]))
         | _ ->
             (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prune-synced", "no args", "bad-args"))))));

  (* (bench expr-string [n]) -> VFloat average microseconds per iteration.
     Parses once, warms up once, runs n times (default 1000) under a max
     budget so the bench loop itself never hits Budget_exceeded. Uses the
     current session env. Prefer pure expressions (side-effecting bodies
     will run for real). *)
  let bench_run (expr_str : string) (n : int) : Pricklypear_lisp.Ast.value =
    let n = max 1 n in
    let e =
      try Pricklypear_lisp.Reader.read_one expr_str
      with exn ->
        raise
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.User_error
                ("bench: parse failed: " ^ Printexc.to_string exn, "bad-args")))
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
    let _ = run_once () in
    let t0 = Unix.gettimeofday () in
    let rec loop i =
      if i >= n then ()
      else
        let _ = run_once () in
        loop (i + 1)
    in
    let () = loop 0 in
    let t1 = Unix.gettimeofday () in
    let avg_us = ((t1 -. t0) *. 1_000_000.) /. float_of_int n in
    (Pricklypear_lisp.Ast.VFloat avg_us)
  in
  Primitives.register tbl "bench"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] -> bench_run s 1000
      | [ Pricklypear_lisp.Ast.VStr s; Pricklypear_lisp.Ast.VInt n ] ->
          bench_run s (Int64.to_int n)
      | _ ->
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
  let apply_lambda_auto (lam : Pricklypear_lisp.Ast.value) : Pricklypear_lisp.Ast.value =
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
          (Pricklypear_lisp.Ast.VStr
                        "__with_transaction__: bad args (need a lambda)"));

  (* Private prim used by the with-step special form in eval.ml. NOT
     USER-VISIBLE: routed to under name __step__ by eval.ml, never
     `(with-step "name" (lambda ...))` from a plain prim call.

     (__step__ "name" <lambda>) is a checkpoint boundary (borge/jobs.borg
     step-checkpoints):
       - Outside a job (Jobs.current_job_id_opt () = None, e.g. an HTTP
         request or REPL) it is a PLAIN thunk call — no checkpoint.
       - Inside a job (the runner binds the job id around eval), the
         thunk's result is memoized under (job_id, name): first execution
         runs the thunk and writes a job_steps row; re-execution (a retry)
         returns the stored value WITHOUT re-running the thunk.

     Exactly-once with (with-transaction (step …)): the checkpoint INSERT
     rides Store.current_conn — when the caller is inside
     with_transaction, the insert lands in the SAME transaction as the
     thunk's writes, so it commits or rolls back atomically with them (a
     failed/later-raised step leaves no checkpoint; a retry re-runs). If
     the THUNK ITSELF raises, no checkpoint is written (the exception
     propagates, rolling back the surrounding transaction if any). *)
  Primitives.register tbl "__step__"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VLambda (_params, _body, _clo) as lam ] ->
          (match Jobs.current_job_id_opt () with
           | None ->
               (* Outside a job: a plain thunk call with no checkpoint. *)
               apply_lambda_auto lam
           | Some job_id ->
               (match
                  Jobs.checkpoint_lookup ~store:t.store ~job_id ~name
                with
                | Some value_json ->
                    (* Memoized: return the stored value without re-running. *)
                    (try
                       Prims_pure.json_to_value
                         (Yojson.Safe.from_string value_json)
                     with _ ->
                       Pricklypear_lisp.Ast.VStr value_json)
                | None ->
                    (* First execution in this job: run the thunk, then write
                       the checkpoint. If the thunk raises, the exception
                       propagates (no checkpoint, surrounding tx rolls back). *)
                    let result = apply_lambda_auto lam in
                    let value_json =
                      Yojson.Safe.to_string (Prims_pure.value_to_json result)
                    in
                    Jobs.checkpoint_write ~store:t.store ~job_id ~name
                      ~value:value_json;
                    result))
      | _ ->
          (Pricklypear_lisp.Ast.VStr
                        "__step__: bad args (need name-str, lambda)"));

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
            match Request_context.get () with
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
                Request_context.with_current elevated_ctx
                  (fun () -> apply_lambda_auto lam)
            | _ -> apply_lambda_auto lam
          in
          (Fun.protect ~finally:(fun () -> if need_elevate then t.manifest <- prev) (fun () -> run_body))
      | _ ->
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
  Prims_media.register t tbl
let do_http_get = Prims_net.do_http_get

(* Late-bound thunk for route/dispatch prim. call_handler is defined later
   in this module (after `make`); this ref lets the prim registered inside
   make reference it without a forward-let. Bound right after call_handler. *)
let route_dispatch_fn :
  (t -> string -> Session_types.request -> Pricklypear_lisp.Ast.value) ref =
  ref (fun _t _handler _req ->
         Pricklypear_lisp.Ast.VStr "route/dispatch: not initialized")

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
let rec eval_one (t : t) (input : string) : Pricklypear_lisp.Ast.value =
  try
    let expr = Pricklypear_lisp.Reader.read_one input in
    (* Pre-eval intercept: (define name <body>) is the only AST-level
       special case left, because the name argument is a Var that must
       not be evaluated. Everything else (fork/merge/route/checkout)
       dispatches through Eval.apply via the prim_table.
       H1: [eval_string_auto] honors PP_EVAL_CONTROL=lwt|eff (default lwt). *)
    (* package-surface: (package NAME (export ...)) is a directive, not a value. *)
    (match expr with
     | Pricklypear_lisp.Ast.App
         (Pricklypear_lisp.Ast.Var "package", args) ->
         handle_package_form t args
     | _ ->
    let v = eval_string_rim t input in
    let pkg = t.current_package in
    let persist_fn name body_expr doc =
      if t.seed_additive then
        Store.persist_function_if_absent t.store name body_expr ~doc
          ~package:pkg t.manifest.username
      else
        Store.persist_function t.store name body_expr ~doc
          ~package:pkg t.manifest.username
    in
    let persist_define name body_expr doc =
      let () =
        if t.manifest.username <> "anonymous" then
          persist_fn name body_expr doc
        else ()
      in
      let () =
        if not t.suppress_refresh_on_define && touches_shell t name
        then begin
          (* Bust only: the next request-gate refresh re-evals. Eager
             refresh here would multiply work per-define during bulk
             loads; invariant-3 retry suppression keeps a broken define
             from blocking the defining request itself. *)
          bust_ui_shell_cache t
        end else ()
      in
      let () = record_define_side_effects t ~name in
      v
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
             t.current_package <- Some name;
             let () =
               if t.manifest.username = "anonymous" then ()
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
  (* image.boot-seed-skip (promote-window task 7, 2026-09-04): during the
     ADDITIVE boot seed, a top-level define/defmacro whose name already
     exists on the branch is a guaranteed no-op — persist_*_if_absent
     skips (seed_names hit) and the in-env binding is discarded by the
     post-seed rehydrate (boot_personal: seed → checkout → rehydrate).
     Skip the eval entirely; ~2300 define forms re-eval per warm boot for
     nothing. Scoped to the seed window (seed_additive + seed_names both
     active). image.load-library-skip (2026-09-03) gives interactive
     load-library / reseed-package the same medicine for IDENTICAL
     normalized body + doc via load_skip_names; changed/new defines keep
     full eval.
     Safety: no libs/*/lib.pp has a top-level CALL form (scan 2026-09-04:
     only define/defmacro/route/deftype/package/export/public-route), so
     a later top-level form in the same load cannot need a skipped
     binding; route/deftype/package forms are NOT skipped, and
     record_define_side_effects already no-ops under the seed's
     suppress_refresh_on_define. Name extraction is by pattern only —
     malformed defines still fall through to eval and raise exactly as
     before. *)
  let seed_skip =
    t.seed_additive
    && (match t.store.seed_names with Some _ -> true | None -> false)
    && (match expr with
        | Pricklypear_lisp.Ast.App
            (Pricklypear_lisp.Ast.Var "define",
             (Pricklypear_lisp.Ast.Var name :: _))
        | Pricklypear_lisp.Ast.App
            (Pricklypear_lisp.Ast.Var "define",
             Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var name, _) :: _)
        | Pricklypear_lisp.Ast.App
            (Pricklypear_lisp.Ast.Var "defmacro",
             [ Pricklypear_lisp.Ast.Var name; _; _ ]) ->
            (match t.store.seed_names with
             | Some set -> Hashtbl.mem set name
             | None -> false)
        | _ -> false)
  in
  (* image.load-library-skip (2026-09-03): inside load-library /
     reseed-package, skip defines whose name is among the branch's OWN
     stored defs with an IDENTICAL normalized body + doc — the env
     binding is already the branch thunk, so eval + persist are pure
     waste. This is what made (load-library "todo") re-evaluate +
     re-persist ~1000 defines per call and blow page/eval budgets
     (budget-exceeded loops). Changed/new defines still eval + persist
     normally. *)
  let load_skip_hit =
    match t.load_skip_names with
    | None -> false
    | Some tbl ->
      (match Pricklypear_lisp.Eval.parse_top_define expr with
       | Some { name; body; doc } ->
           (match Hashtbl.find_opt tbl name with
            | Some stored ->
                stored =
                  (Pricklypear_lisp.Reader.pp_expr body ^ "\x00"
                   ^ (match doc with Some d -> d | None -> ""))
            | None -> false)
       | None ->
           (match expr with
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
                 | Some stored -> stored = stored_macro ^ "\x00"
                 | None -> false)
            | _ -> false))
  in
  if seed_skip || load_skip_hit then Pricklypear_lisp.Ast.VNil
  else
  let v = eval_expr_rim t expr in
  let pkg = t.current_package in
  let persist_fn name body_expr doc =
    if t.seed_additive then
      Store.persist_function_if_absent t.store name body_expr ~doc
        ~package:pkg t.manifest.username
    else
      Store.persist_function t.store name body_expr ~doc
        ~package:pkg t.manifest.username
  in
  let persist_define name body_expr doc =
    let () =
      if t.manifest.username <> "anonymous" then
        persist_fn name body_expr doc
      else ()
    in
    let () =
      if not t.suppress_refresh_on_define && touches_shell t name
      then begin
        bust_ui_shell_cache t
      end else ()
    in
    let () = record_define_side_effects t ~name in
    v
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
   | _ -> v)))

and load_string (t : t) (src : string) : (string, string) result =
  let prev = t.current_package in
  let prev_suppress = t.suppress_refresh_on_define in
  t.current_package <- None;
  t.suppress_refresh_on_define <- true;
  Fun.protect
    ~finally:(fun () ->
      t.current_package <- prev;
      t.suppress_refresh_on_define <- prev_suppress)
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
  let prev = t.current_package in
  let prev_suppress = t.suppress_refresh_on_define in
  t.current_package <- None;
  t.suppress_refresh_on_define <- true;
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
     exactly this load. *)
  let prev_skip = t.load_skip_names in
  let () =
    if t.seed_additive then ()
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
      List.iter
        (fun (name, body, _author, pkg, doc) ->
          let hidden =
            match pkg with
            | Some p -> Hashtbl.mem disabled_set p || name_disabled name
            | None -> name_disabled name
          in
          if hidden || Hashtbl.mem kernel_set name then ()
          else
            Hashtbl.replace tbl name
              (Pricklypear_lisp.Reader.pp_expr body ^ "\x00"
               ^ (match doc with Some d -> d | None -> "")))
        (Store.list_own_function_defs t.store);
      t.load_skip_names <- Some tbl
    end
  in
  Fun.protect
    ~finally:(fun () ->
      t.current_package <- prev;
      t.suppress_refresh_on_define <- prev_suppress;
      t.load_skip_names <- prev_skip)
    (fun () ->
      let r = Loader.load_library ~eval_form:(eval_form t) name in
      let () =
        if not prev_suppress then begin
          bust_ui_shell_cache t;
          refresh_ui_shell t
        end
      in
      r)

(** branching.seed-policy: admin-only upsert of libs/<pkg>/lib.pp onto
    system/main (full clobber for names in that file). Does not touch
    personal/dev rows. seed_additive stays false so persist upserts. *)
and reseed_package (t : t) (pkg : string) : (string, string) result =
  let pkg = String.trim pkg in
  if pkg = "" then (Error "empty package name")
  else if (not t.is_admin)
          && t.manifest.username <> "admin"
          && t.manifest.username <> "system" then
    (Error "requires admin")
  else begin
    t.seed_additive <- false;
    t.suppress_refresh_on_define <- true;
    let finally () =
      t.suppress_refresh_on_define <- false;
      t.seed_additive <- false;
      ()
    in
    Fun.protect ~finally (fun () ->
        Store.with_branch t.store ~branch_id:Store.system_main_id (fun () ->
          let r = load_library t pkg in
          let () = rehydrate t in
          let () = refresh_ui_shell t in
          match r with
          | Ok msg ->
              (Ok (Printf.sprintf "reseeded %s → system/main: %s" pkg msg))
          | Error e -> (Error e)))
  end

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
    Pricklypear_lisp.Env.lookup_hook :=
      Some (fun n ->
        if not (List.exists (String.equal n) t.ui_shell_reads) then
          t.ui_shell_reads <- n :: t.ui_shell_reads);
    Fun.protect
      ~finally:(fun () -> Pricklypear_lisp.Env.lookup_hook := None)
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
           t.ui_shell_retry_at <- 0.0;
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
           t.ui_shell_retry_at <- now +. shell_retry_secs;
           ()))
  in
  let () =
    match Pricklypear_lisp.Env.lookup t.env "ui/nav" with
    | None -> t.ui_nav_html <- None
    | Some _ ->
        if t.ui_nav_html <> None then
          (* Cache present: serve it (bust clears it first). *)
          ()
        else if now < t.ui_shell_retry_at then
          (* Recently failed: skip re-eval + skip warn. *)
          ()
        else
          refresh "ui/nav" "(ui/nav)" (fun v -> t.ui_nav_html <- v)
  in
  match Pricklypear_lisp.Env.lookup t.env "ui/css" with
  | None -> t.ui_css <- None
  | Some _ ->
      if t.ui_css <> None then
        ()
      else if now < t.ui_shell_retry_at then
        ()
      else
        refresh "ui/css" "(ui/css)" (fun v -> t.ui_css <- v)

and seed_one (t : t) (name : string) : unit =
  (* promote-window task 7 (2026-09-04): per-package ms on the existing
     load line — the boot-phase table drills into seed_stdlib here. *)
  let t0 = Unix.gettimeofday () in
  let r = load_library t name in
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  (match r with
   | Ok msg -> Printf.eprintf "[session] %s: %s (%.0f ms)\n%!" name msg ms
   | Error e -> Printf.eprintf "[session] %s load: %s (%.0f ms)\n%!" name e ms);
  ()

(** Boot seed order (BORGE-1785211333).
    Preferred prefix preserves dependency-ish order (std/ui before notes,
    outline before notes, …). Any other [libs/<name>/lib.pp] is appended
    alphabetically so adding a package no longer requires an OCaml edit. *)
and seed_package_priority : string list =
  (* css before notes: notes/tree-css evaluates (css/sheet …) at define
     time; if css is unbound the notes load aborts mid-file and routes
     like /partials/notes-search never seed (notes.search-modal). *)
  (* theme before ui: default theme/* tokens exist before ui/css first runs. *)
  [ "std"; "theme"; "weather"; "ui"; "layout"; "welcome"; "ai"; "css"; "html"
  ; "list"; "outline"; "structed"; "notes"; "signup"; "todo"; "contacts"
  ; "plan"; "chat"; "bookmarks"; "calendar"; "workout"; "flashcards"
  ; "bible"; "dabar"; "bi"; "ref"; "verify" ]

and discover_seed_packages () : string list =
  let preferred = seed_package_priority in
  let root = Loader.libs_dir () in
  let discovered =
    try
      Sys.readdir root
      |> Array.to_list
      |> List.filter (fun name ->
             name <> ""
             && name.[0] <> '_'
             && name.[0] <> '.'
             (* Alternate skins (theme-paper, …) load on demand via
                theme/set → load-library; seeding them would only
                thrash in-memory theme/* before rehydrate. *)
             && not (String.starts_with ~prefix:"theme-" name)
             &&
             let pp =
               Filename.concat root (Filename.concat name "lib.pp")
             in
             Sys.file_exists pp)
      |> List.sort String.compare
    with _ -> []
  in
  let extras =
    List.filter (fun n -> not (List.mem n preferred)) discovered
  in
  preferred @ extras

(** Ensure living agents note exists on system/main after notes seed.
    Idempotent: skip if slug agents already present. *)
and seed_agents_note (t : t) : unit =
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
  (try
      let v = eval_one t expr in
      Printf.eprintf "[session] agents-note: %s\n%!"
        (Pricklypear_lisp.Reader.pp_value v);
      ()
    with exn ->
      Printf.eprintf "[session] agents-note seed: %s\n%!" (pp_interp_exn exn);
      ())

and seed_stdlib (t : t) : unit =
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
    (* image.boot-seed-batch: preload existing fn/route names on
       system/main once, so persist_function_if_absent /
       register_route_if_absent skip the per-name get_by_path SELECT
       (~1800 defines + routes). On a warm DB this collapses the seed
       to two queries + zero writes. *)
    Store.begin_seed_names t.store;
    let started = Unix.gettimeofday () in
    let finally () =
      let preloaded =
        match t.store.seed_names with
        | Some h -> Some (Hashtbl.length h)
        | None -> None in
      Store.end_seed_names t.store;
      t.suppress_refresh_on_define <- false;
      t.seed_additive <- false;
      let elapsed_ms =
        int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
      Printf.eprintf "[session] boot seed: %d packages, %d ms%s\n%!"
        (List.length (discover_seed_packages ())) elapsed_ms
        (match preloaded with
         | Some n -> Printf.sprintf " (%d names preloaded)" n
         | None -> "")
    in
    Fun.protect ~finally (fun () ->
        let pkgs = discover_seed_packages () in
        Printf.eprintf "[session] seed_stdlib packages (%d): %s\n%!"
          (List.length pkgs) (String.concat " " pkgs);
        let rec go = function
          | [] -> ()
          | "notes" :: rest ->
              let () = seed_one t "notes" in
              let () = seed_agents_note t in
              go rest
          | name :: rest ->
              let () = seed_one t name in
              go rest
        in
        go pkgs))

(** Ensure username/personal exists as an EMPTY child of system/main.
    Inheritance (not copy) means main defaults stay visible; personal
    only stores the user's overrides. *)
and ensure_personal_branch (t : t) : string option =
  let owner = t.manifest.username in
  let main_id_opt =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  let parent_id =
    match main_id_opt with
    | Some id -> id
    | None -> Store.system_main_id
  in
  Store.ensure_branch t.store ~parent_id ~owner ~name:"personal"

and checkout_personal (t : t) : unit =
  let owner = t.manifest.username in
  let branch_ref = owner ^ "/personal" in
  let found =
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
  : string option =
  let main_id_opt =
    Store.lookup_branch_id_by_name t.store ~owner:"system" ~name:"main"
  in
  let parent_id =
    match main_id_opt with
    | Some id -> id
    | None -> Store.system_main_id
  in
  Store.ensure_branch t.store ~parent_id ~owner:parent_username ~name:"agent"

and checkout_agent (t : t) ~(parent_username : string) : unit =
  let branch_ref = parent_username ^ "/agent" in
  let found =
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

and recipe_hello (t : t) : (string, string) result =
  load_library t "hello"

and recipe_todo (t : t) : (string, string) result =
  load_library t "todo"

and recipe_bible (t : t) : (string, string) result =
  (* Types + API + /bible only — corpus via scripts/seed_bible_bsb.py *)
  load_library t "bible"

and recipe_dabar (t : t) : (string, string) result =
  (* Study layer — requires bible/*; Strong's/interlinear via seed scripts *)
  let r = load_library t "bible" in
  (match r with
   | Error e -> (Error e)
   | Ok _ -> load_library t "dabar")

and recipe_notes (t : t) : (string, string) result =
  (* Outline-first notes — outline pure lib then notes package *)
  let r = load_library t "outline" in
  (match r with
   | Error e -> (Error e)
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
      load_stamp = "";
      prims = prim_table;  (* temporary; replaced below *)
      routes;
      started_at = Unix.gettimeofday ();
      kernel_catalog = [];  (* filled after all prims register *)
      ui_nav_html = None;
      ui_css = None;
      ui_shell_reads = [];
      ui_shell_retry_at = 0.0;
      suppress_refresh_on_define = false;
      seed_additive = false;
      current_request = None;
      agent_branch_lock = false;
      is_admin = (manifest.username = "admin");
      auth_token = None;
      impact_queue = [];
      change_transcript = [];
      current_package = None;
      load_skip_names = None;
      net_streams = Hashtbl.create 16;
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
     then rebind t.prims to the populated table. These prims capture
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
  (* Register CRUD primitives (get/create/update/delete/list) over
     the shared data_rows table. Same pattern as control prims:
     they capture (!t) and return their real DB result via Lwt. *)
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
  (* lisp-interpreter.math: pi is a bound value, not a zero-arg prim. *)
  Pricklypear_lisp.Env.define env "pi" (Pricklypear_lisp.Ast.VFloat Float.pi);
  Primitives.register prim_table "load-string"
    (fun args ->
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
      match args with
      | [ Pricklypear_lisp.Ast.VStr src ]
      | [ Pricklypear_lisp.Ast.VSymbol src ] ->
          Pricklypear_lisp.Eval.eval_string_direct
            ~prims:Pricklypear_lisp.Eval.empty_prims !t.env src
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
  (* (reseed-package pkg) — admin-only upsert libs/<pkg> onto system/main. *)
  Primitives.register prim_table "reseed-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let r = reseed_package !t pkg in
          (match r with
           | Ok msg -> (Pricklypear_lisp.Ast.VStr msg)
           | Error e ->
               (Pricklypear_lisp.Ast.VStr ("reseed-package: " ^ e)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("reseed-package", "need package-str", "bad-args")))));
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
    () : unit =
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
    };
  t.is_admin <- false;
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
     (same guard as the unbind prim; branches_unoverride ignores its
     checkout_ref result, so this is its own gate). *)
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
      [ "list"; "outline"; "notes"; "spec" ];
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
        (* Policy: only admin username may write system/main via promote UI. *)
        if (not t.is_admin)
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
  (* i-1787970640: child_owner must pass the branch-owner rule (same as
     the fork prim — blocks pre-claiming another user's garden). This is
     the funnel behind http_branches.branches_fork. *)
  if not (branch_owner_authorized t c_owner) then
    (Error (Printf.sprintf "denied: branch %s is owned by %s"
              child_ref c_owner))
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
  : handler_call =
  (* Resolve the handler env against the per-request snapshot when bound
     (image.per-request-context): a concurrent attach_anonymous +
     checkout_to mid-request would otherwise rehydrate t.env to system/main
     and this lookup would miss personal-branch handlers. Fall back to t.env
     when no context (REPL, /api/eval outside HTTP gate, pre-gate paths). *)
  let env =
    match Request_context.get () with
    | Some ctx -> Request_context.env ctx
    | None -> t.env
  in
  match Pricklypear_lisp.Env.lookup env handler_name with
  | None -> Handler_unbound
  | Some _fn ->
      (* Evaluate (handler-name) — zero args. Re-uses eval's apply path
         so VLambda closures AND VPrim dispatch both work. *)
      let run () =
        (try
      let v =
               eval_string_rim t ("(" ^ handler_name ^ ")")
             in
             (Handler_ok v)
    with exn ->
      let msg = pp_interp_exn exn in
             Ops_incidents.emit_and_maybe_file t
               ~level:Ops_log.Error ~component:"handler"
               ~file:(not (Ops_incidents.is_expected_runtime_error exn))
               ~context:[("handler", handler_name)]
               msg;
             (Handler_error msg))
      in
      (match request with
       | None -> run ()
       | Some req ->
           t.current_request <- Some req;
           Fun.protect
             ~finally:(fun () -> t.current_request <- None)
             run)

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
          ~env:t.env
          ~routes:(Request_context.copy_routes t.routes)
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
