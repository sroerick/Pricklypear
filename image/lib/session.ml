(** Pricklypear Image — REPL session.

    A per-connection session holds the env (function defs loaded for the
    session's branch, plus any (define)'d locals), the manifest (identity
    claim asserted by supervisor), and the store handle.

    Per spec [image.repl-experience]:
      - (define ...) persists to the store: survives image reboot.
      - REPL-local let bindings are session-local: NOT persisted.
      - Switching branch (checkout) keeps REPL locals but changes visibility. *)
open Lwt.Syntax
include Session_types

let load_defs (t : t) : unit Lwt.t =
  let* defs = Store.list_function_names_for_load t.store in
  let* _ =
    Lwt_list.iter_s
      (fun (name, body, _author) ->
        (* Always (re)define — parents first, children last → override. *)
        Lwt.catch
          (fun () ->
             (* Eval the stored AST directly — NEVER pp_expr round-trip.
                Complex bodies (ui/home, notes) break under print/parse. *)
             let* _ =
               Pricklypear_lisp.Eval.eval
                 ~budget:(Pricklypear_lisp.Eval.make_budget ())
                 ~prims:t.prims t.env
                 (Pricklypear_lisp.Ast.Define (name, body))
             in
             Lwt.return_unit)
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
  Hashtbl.clear t.env.bindings;
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
     refresh_ui_shell (request launch / load) repopulates from new env. *)
  t.ui_nav_html <- None;
  t.ui_css <- None;
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
     Anonymous sessions may not register routes. *)
  let parse_vis = function
    | Pricklypear_lisp.Ast.VStr s ->
        (match String.lowercase_ascii (String.trim s) with
         | "public" -> Some "public"
         | "private" -> Some "private"
         | _ -> None)
    | _ -> None
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
            (* handler-only: pass None so DB keeps visibility on UPDATE;
               memory keeps prior or private for new. *)
            Hashtbl.replace t.routes route_path (handler, vis);
            let* _ =
              Store.register_route t.store ~route:route_path ~handler ()
            in
            Lwt.return (Pricklypear_lisp.Ast.VStr route_path)
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler;
            vis_v ] ->
            (match parse_vis vis_v with
             | None ->
                 Lwt.return
                   (Pricklypear_lisp.Ast.VStr
                      "route: visibility must be public or private")
             | Some vis ->
                 Hashtbl.replace t.routes route_path (handler, vis);
                 let* _ =
                   Store.register_route t.store ~route:route_path ~handler
                     ~visibility:vis ()
                 in
                 Lwt.return (Pricklypear_lisp.Ast.VStr route_path))
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
            Hashtbl.replace t.routes route_path (handler, "public");
            let* _ =
              Store.register_route t.store ~route:route_path ~handler
                ~visibility:"public" ()
            in
            Lwt.return (Pricklypear_lisp.Ast.VStr route_path)
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
  Primitives.register tbl "__with_transaction__"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VLambda (params, body, clo_env) as lam ] ->
          (* Enforce zero-arg lambda for clarity: with-transaction bodies
             shouldn't take args. *)
          let _ = params in
          let _ = clo_env in
          Store.with_dynamic_transaction t.store
            (fun () ->
               let budget = Pricklypear_lisp.Eval.make_budget () in
               Pricklypear_lisp.Eval.apply
                 ~budget ~prims:t.prims lam [])
      | _ ->
          Lwt.return (Pricklypear_lisp.Ast.VStr
                        "__with_transaction__: bad args (need a lambda)"))

(** If model returns a bare expression body, wrap as (lambda (x) ...). *)

(* ---- AI / agent tools (Agent_tools) — re-export for stable Session.* API ---- *)
let define_binding = Agent_tools.define_binding
let install_ai_function = Agent_tools.install_ai_function
let function_source = Agent_tools.function_source
let package_binding_names = Agent_tools.package_binding_names
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

let register_crud_prims (t : t) (tbl : Primitives.t) : unit =
  (* (deftype 'name 'field1 'field2 ...) -> VStr "defined type <name>".
     All args are quoted symbols (which evaluate to VStr). Persists
     the declaration into type_defs on the session's branch. *)
  let deny_anon name =
    Lwt.return
      (Pricklypear_lisp.Ast.VStr (name ^ ": denied for anonymous"))
  in
  Primitives.register tbl "deftype"
    (fun args ->
      if t.manifest.username = "anonymous" then deny_anon "deftype"
      else Crud.deftype t.store ~args ~author:t.manifest.username);

  (* (get 'type id) -> VStr fields-json | VNil *)
  Primitives.register tbl "get"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          Crud.get t.store ~type_name ~id
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "get: bad args (need type, id)"));

  (* (create 'type "{fields-json}") -> VStr new-id | VStr "create failed" *)
  Primitives.register tbl "create"
    (fun args ->
      if t.manifest.username = "anonymous" then deny_anon "create"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr fields_json ] ->
            Crud.create t.store
              ~type_name ~fields_json
              ~author:t.manifest.username
        | _ ->
            Lwt.return
              (Pricklypear_lisp.Ast.VStr "create: bad args (need type, fields)"));

  (* (update 'type id "{fields-json}") -> VStr id | VNil *)
  Primitives.register tbl "update"
    (fun args ->
      if t.manifest.username = "anonymous" then deny_anon "update"
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
  Primitives.register tbl "update-field"
    (fun args ->
      if t.manifest.username = "anonymous" then deny_anon "update-field"
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
  Primitives.register tbl "delete"
    (fun args ->
      if t.manifest.username = "anonymous" then deny_anon "delete"
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
  Primitives.register tbl "select-for-update"
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
  Primitives.register tbl "rows"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.list t.store ~type_name ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.list t.store ~type_name ~limit:(Int64.to_int n) ()
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "rows: bad args (need type[, limit])"));

  (* (find-row 'type "field" "value") -> fields JSON | () *)
  Primitives.register tbl "find-row"
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
  Primitives.register tbl "find-rows"
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
  Primitives.register tbl "find-text"
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
  Primitives.register tbl "find-random"
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

  (* (current-branch) -> VStr *)
  Primitives.register_sync tbl "current-branch"
    (fun _ -> Pricklypear_lisp.Ast.VStr t.manifest.branch_name);

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

  (* (package-bindings pkg) -> VList of name strings *)
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
               "llm-seed: need requirement-str [, max-steps int]"))


let method_to_string (m : http_method) : string =
  match m with `GET -> "GET" | `POST -> "POST"

let require_request (t : t) : request =
  match t.current_request with
  | Some r -> r
  | None ->
      raise
        (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.User_error "no active request"))

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
       dispatches through Eval.apply via the prim_table. *)
    let* v =
      Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env input
    in
    (match expr with
     | Pricklypear_lisp.Ast.App
         (Pricklypear_lisp.Ast.Var "define",
          [ Pricklypear_lisp.Ast.Var name; body_expr ]) ->
         (* Persist the body AST to the store for reload-on-boot. The
            env binding already happened via eval's Define case; this
            DB write is non-fatal on PG-down. Anonymous may not persist. *)
         if t.manifest.username <> "anonymous" then
           Lwt.async (fun () ->
             Store.persist_function t.store name body_expr t.manifest.username);
         let* () =
           if name = "ui/nav" || name = "ui/home" || name = "ui/css"
              || name = "ui/theme"
              || String.starts_with ~prefix:"theme/" name
           then refresh_ui_shell t
           else Lwt.return_unit
         in
         Lwt.return v
     | _ -> Lwt.return v)
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
  let* v =
    Pricklypear_lisp.Eval.eval ~budget:(Pricklypear_lisp.Eval.make_budget ())
      ~prims:t.prims t.env expr
  in
  (match expr with
   | Pricklypear_lisp.Ast.App
       (Pricklypear_lisp.Ast.Var "define",
        [ Pricklypear_lisp.Ast.Var name; body_expr ]) ->
       if t.manifest.username <> "anonymous" then
         Lwt.async (fun () ->
           Store.persist_function t.store name body_expr t.manifest.username);
       let* () =
         if name = "ui/nav" || name = "ui/home" || name = "ui/css"
            || name = "ui/theme"
            || String.starts_with ~prefix:"theme/" name
         then refresh_ui_shell t
         else Lwt.return_unit
       in
       Lwt.return v
   | _ -> Lwt.return v)

and load_string (t : t) (src : string) : (string, string) result Lwt.t =
  Loader.load_string ~eval_form:(eval_form t) src

and load_library (t : t) (name : string) : (string, string) result Lwt.t =
  Loader.load_library ~eval_form:(eval_form t) name

and refresh_ui_shell (t : t) : unit Lwt.t =
  (* Product shell: nav HTML + theme CSS (image.product-theme). *)
  let* () =
    match Pricklypear_lisp.Env.lookup t.env "ui/nav" with
    | None ->
        t.ui_nav_html <- None;
        Lwt.return_unit
    | Some _ ->
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
            Lwt.return_unit)
          (fun exn ->
            Printf.eprintf "[session] refresh_ui_shell (ui/nav) failed: %s\n%!"
              (Printexc.to_string exn);
            t.ui_nav_html <- None;
            Lwt.return_unit)
  in
  match Pricklypear_lisp.Env.lookup t.env "ui/css" with
  | None ->
      t.ui_css <- None;
      Lwt.return_unit
  | Some _ ->
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
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[session] refresh_ui_shell (ui/css) failed: %s\n%!"
            (Printexc.to_string exn);
          t.ui_css <- None;
          Lwt.return_unit)

and seed_one (t : t) (name : string) : unit Lwt.t =
  let* r = load_library t name in
  (match r with
   | Ok msg -> Printf.eprintf "[session] %s: %s\n%!" name msg
   | Error e -> Printf.eprintf "[session] %s load: %s\n%!" name e);
  Lwt.return_unit

and seed_stdlib (t : t) : unit Lwt.t =
  (* Package defaults always land on system/main — NEVER on the user's
     personal branch. Personal inherits main via the recursive CTE; user
     (define)s write overrides on personal and survive reseed/deploy.

     Canonical source is libs/*.pp in git. Explicit (load-library ...) on
     the current branch still overwrites that branch (intentional). *)
  Store.with_branch t.store ~branch_id:Store.system_main_id (fun () ->
    let* () = seed_one t "std" in
    let* () = seed_one t "weather" in
    let* () = seed_one t "ui" in
    let* () = seed_one t "ai" in
    let* () = seed_one t "outline" in
    let* () = seed_one t "notes" in
    let* () = seed_one t "todo" in
    let* () = seed_one t "contacts" in
    let* () = seed_one t "bookmarks" in
    let* () = seed_one t "calendar" in
    let* () = seed_one t "flashcards" in
    let* () = seed_one t "bible" in
    let* () = seed_one t "dabar" in
    Lwt.return_unit)

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
      prims = Primitives.to_eval_table prim_table;  (* temporary; replaced below *)
      routes;
      started_at = Unix.gettimeofday ();
      kernel_catalog = [];  (* filled after all prims register *)
      ui_nav_html = None;
      ui_css = None;
      current_request = None;
      agent_branch_lock = false;
      is_admin = (manifest.username = "admin");
      auth_token = None;
    }
  in
  (* Register session-aware primitives (whoami, current-author,
     list-bindings). username_fn re-reads t.manifest so per-request
     identity attach updates (whoami) without re-registering. *)
  Primitives.register_session_prims prim_table
    ~username_fn:(fun () -> (!t).manifest.username) ~env;
  (* Register the DB-touching control primitives on the prim_table,
     then rebind t.prims to the populated table. These prims capture
     (!t), so they read/write store+manifest on each invocation. *)
  register_session_control_prims !t prim_table;
  (* HTTP request-context prims (it33): request-method/path/param(s)/body +
     redirect. Dynamic scope via current_request while call_handler runs. *)
  register_request_prims !t prim_table;
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
  let final_prims = Primitives.to_eval_table prim_table in
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
let is_admin (t : t) : bool = t.is_admin
let store (t : t) : Store.t = t.store
let branch_id (t : t) : string = t.manifest.branch_id
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
    () : unit Lwt.t =
  let prev = t.manifest.username in
  t.manifest <-
    { t.manifest with user_id; username };
  t.is_admin <- is_admin;
  t.auth_token <- auth_token;
  if checkout_personal_now || prev <> username then
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

(* Invoke a named handler function (looked up via env). The handler
   must be a zero-arg VLambda or a VPrim name. Returns its value, or
   raises Interp_error if the handler doesn't resolve.
   When [~request] is provided, request-* prims see that context for
   the duration of the call (cleared in finalize). *)
let call_handler (t : t) ?request (handler_name : string)
  : Pricklypear_lisp.Ast.value option Lwt.t =
  match Pricklypear_lisp.Env.lookup t.env handler_name with
  | None -> Lwt.return_none
  | Some _fn ->
      (* Evaluate (handler-name) — zero args. Re-uses eval's apply path
         so VLambda closures AND VPrim dispatch both work. *)
      let run () =
        Lwt.catch
          (fun () ->
             let* v =
               Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env
                 ("(" ^ handler_name ^ ")")
             in
             Lwt.return (Some v))
          (fun exn ->
             Printf.eprintf "[session] call_handler %s: %s\n%!"
               handler_name (pp_interp_exn exn);
             Lwt.return_none)
      in
      (match request with
       | None -> run ()
       | Some req ->
           t.current_request <- Some req;
           Lwt.finalize run (fun () ->
               t.current_request <- None;
               Lwt.return_unit))
