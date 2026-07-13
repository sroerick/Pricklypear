(** Pricklypear Image — REPL session.

    A per-connection session holds the env (function defs loaded for the
    session's branch, plus any (define)'d locals), the manifest (identity
    claim asserted by supervisor), and the store handle.

    Per spec [image.repl-experience]:
      - (define ...) persists to the store: survives image reboot.
      - REPL-local let bindings are session-local: NOT persisted.
      - Switching branch (checkout) keeps REPL locals but changes visibility. *)
open Lwt.Syntax

(** Per-request HTTP context for user-route handlers (it33).
    Bound only while [call_handler] runs; request-* prims read this. *)
type http_method = [ `GET | `POST ]

type request = {
  method_ : http_method;
  path : string;
  params : (string * string) list;  (** form fields win over query *)
  body : string;
  (** Lowercase header names → values (for HX-Request, etc.). *)
  headers : (string * string) list;
}

type t = {
  mutable manifest : Pricklypear_common.Manifest.t;
  store : Store.t;
  env : Pricklypear_lisp.Ast.env;
  mutable prims : Pricklypear_lisp.Eval.prim_table;
  (* In-memory route table mirroring view_bindings for the branch.
     Populated by load_defs_async at session-make time; (route ...)
     updates also live-persist to view_bindings AND update this table.
     Value is (handler, visibility) where visibility is public|private.
     http.ml reads from here on every request (no DB round-trip). *)
  routes : (string, string * string) Hashtbl.t;
  mutable started_at : float;
  (** Kernel prim catalog for Functions browser: (name, summary) list. *)
  mutable kernel_catalog : (string * string) list;
  (** Cached HTML from ui/nav when defined; None = use OCaml default nav. *)
  mutable ui_nav_html : string option;
  (** Cached CSS from (ui/css) when defined; None = emergency_css in http. *)
  mutable ui_css : string option;
  (** Active HTTP request for user-route dispatch; None in REPL. *)
  mutable current_request : request option;
  (** When true, (checkout)/(fork)/(merge) are blocked so the agent
      cannot leave the current branch mid-task. *)
  mutable agent_branch_lock : bool;
  (** Admin flag from users.is_admin (auth identity). *)
  mutable is_admin : bool;
  (** Raw pp_session cookie for the active HTTP request, if any. *)
  mutable auth_token : string option;
}

(* Load persisted function_defs + routes into the env.
   Returns when finished (caller may then seed libs/ which overwrite).

   Load order is critical: many personal pins are *values* evaluated at
   define-time (e.g. ui/menu = (list (menu/link …))), not lambdas.
   Those bodies need parent helpers (menu/link, menu/group, …) already
   bound. Store.list_function_names returns effective defs only (closest
   branch wins) but does NOT topological-order dependencies across the
   parent chain. We therefore load the full chain parents-first
   (depth DESC) and redefine so child overrides win last.

   Historical bug: depth ASC + first-wins loaded personal ui/menu BEFORE
   system menu/link → define failed → ui/menu unbound → (ui/nav) fails →
   silent fallback to OCaml default_nav_nodes ("navbar disappeared"). *)
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
let ensure_callable_body (body_src : string) : string =
  try
    let e = Pricklypear_lisp.Reader.read_one body_src in
    match e with
    | Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "lambda", _) -> body_src
    | Pricklypear_lisp.Ast.Lambda _ -> body_src
    | _ -> Printf.sprintf "(lambda (x) %s)" body_src
  with _ -> body_src

(* Save/define a binding from source body text. Used by function-editor
   HTTP and AI propose. [author] is recorded on function_defs. *)
let define_binding (t : t) ~(name : string) ~(body_src : string)
    ~(author : string)
    : (Pricklypear_lisp.Ast.value, string) result Lwt.t =
  let body_src = ensure_callable_body body_src in
  let input = Printf.sprintf "(define %s %s)" name body_src in
  Lwt.catch
    (fun () ->
      let expr = Pricklypear_lisp.Reader.read_one input in
      let* v =
        Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env input
      in
      (match expr with
       | Pricklypear_lisp.Ast.App
           (Pricklypear_lisp.Ast.Var "define",
            [ Pricklypear_lisp.Ast.Var n; body_expr ]) ->
           let* () = Store.persist_function t.store n body_expr author in
           (* Stale chrome nav was the "bible only on home" bug: home
              live-calls ui/nav; other pages used a cached string. *)
           if n = "ui/nav" || n = "ui/home" || n = "ui/css" || n = "ui/theme"
              || String.starts_with ~prefix:"theme/" n then (
             t.ui_nav_html <- None;
             t.ui_css <- None);
           Lwt.return (Ok v)
       | _ ->
           (* Still bound in env if define special-form ran *)
           Lwt.return (Ok v)))
    (fun exn ->
      let msg =
        match exn with
        | Pricklypear_lisp.Ast.Interp_error e ->
            (match e with
             | Pricklypear_lisp.Ast.Parse_error (m, l, c) ->
                 Printf.sprintf "parse error: %s (%d:%d)" m l c
             | Pricklypear_lisp.Ast.User_error m -> m
             | Pricklypear_lisp.Ast.Unbound_var s -> "unbound: " ^ s
             | Pricklypear_lisp.Ast.Budget_exceeded -> "budget exceeded"
             | _ -> Printexc.to_string exn)
        | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
            Printf.sprintf "parse error: %s (%d:%d)" m l c
        | _ -> Printexc.to_string exn
      in
      Lwt.return (Error msg))

(* First-run recipe: hello page function + route. No Lisp knowledge required. *)
(* Optional elevate: PP_AI_ALLOW_WRITE=1 allows WriteDb/Publish/etc. *)
let ai_allow_set () =
  match Sys.getenv_opt "PP_AI_ALLOW_WRITE" with
  | Some ("1" | "true" | "TRUE" | "yes") ->
      Pricklypear_lisp.Effect.EffSet.normalize
        [ Pricklypear_lisp.Effect.ReadDb
        ; Pricklypear_lisp.Effect.WriteDb
        ; Pricklypear_lisp.Effect.Html
        ; Pricklypear_lisp.Effect.Publish
        ; Pricklypear_lisp.Effect.Ai
        ]
  | _ -> Pricklypear_lisp.Effect.ai_default_allow

let check_ai_body_effects (body_src : string) : (unit, string) result =
  try
    let expr = Pricklypear_lisp.Reader.read_one body_src in
    let fx = Pricklypear_lisp.Effect.infer expr in
    if Pricklypear_lisp.Effect.policy_allows ~allow:(ai_allow_set ()) fx then
      Ok ()
    else
      Error
        (Printf.sprintf
           "effect policy denied (%s); allow pure|read-db|write-db|html by default; use register_route tool for routes (not (route ...) in body); set PP_AI_ALLOW_WRITE=1 for branch/publish elevate"
           (Pricklypear_lisp.Effect.EffSet.pp fx))
  with _ -> Ok ()  (* parse errors handled by define_binding *)

(* [agent] temporary alias defined after agent_session_new / agent_say. *)

(** Install a Lisp body as [name] with AI provenance (parse + effect gate). *)
let install_ai_function (t : t) ~(name : string) ~(body_src : string)
    : Pricklypear_lisp.Ast.value Lwt.t =
  match Llm_client.extract_lisp_sexpr body_src with
  | Error e ->
      let preview =
        let s = String.trim body_src in
        if String.length s > 240 then String.sub s 0 240 ^ "…" else s
      in
      Lwt.return
        (Pricklypear_lisp.Ast.VStr
           (Printf.sprintf "ai body invalid: %s\n---\n%s" e preview))
  | Ok body_src ->
      (match check_ai_body_effects body_src with
       | Error pe ->
           Lwt.return
             (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "ai rejected by effects: %s\n---\n%s" pe body_src))
       | Ok () ->
           let author = "agent:" ^ Llm_client.model () in
           let* def = define_binding t ~name ~body_src ~author in
           (match def with
            | Ok _ ->
                let badge =
                  try
                    let e = Pricklypear_lisp.Reader.read_one body_src in
                    Pricklypear_lisp.Effect.EffSet.badge
                      (Pricklypear_lisp.Effect.infer e)
                  with _ -> "?"
                in
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "installed %s by %s [%s]\n%s"
                        name author badge body_src))
            | Error e ->
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "ai body invalid: %s\n---\n%s" e body_src))))

(** Pretty source body for a binding (env lambda or store). *)
let function_source (t : t) (name : string) : string option Lwt.t =
  let* found = Store.lookup_function t.store name in
  match found with
  | Some (expr, _) ->
      Lwt.return (Some (Pricklypear_lisp.Reader.pp_expr expr))
  | None ->
      (match Pricklypear_lisp.Env.lookup t.env name with
       | Some (Pricklypear_lisp.Ast.VLambda (params, body, _)) ->
           let e =
             Pricklypear_lisp.Ast.Lambda (params, body)
           in
           Lwt.return (Some (Pricklypear_lisp.Reader.pp_expr e))
       | _ -> Lwt.return_none)

let package_binding_names (t : t) (pkg : string) : string list Lwt.t =
  let* defs = Store.list_function_names t.store in
  let pkg =
    if pkg = "(root)" || pkg = "root" then "" else pkg
  in
  let names =
    List.filter_map
      (fun (name, _, _) ->
        let p = Packages.package_of_name name in
        if p = pkg then Some name else None)
      defs
    |> List.sort_uniq String.compare
  in
  Lwt.return names

(** Export package as lib.pp text: (define name body)… pretty-printed. *)
let export_package (t : t) (pkg : string) : string Lwt.t =
  let* names = package_binding_names t pkg in
  let label = if pkg = "" || pkg = "(root)" then "(root)" else pkg in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (Printf.sprintf
       ";; package %s -- exported from live branch %s\n;; Sync: scripts/pp_sync.py pull|push\n\n"
       label t.manifest.branch_name);
  let rec go = function
    | [] -> Lwt.return (Buffer.contents buf)
    | name :: rest ->
        let* src = function_source t name in
        (match src with
         | None -> go rest
         | Some body ->
             Buffer.add_string buf "(define ";
             Buffer.add_string buf name;
             Buffer.add_string buf "\n  ";
             (* indent body lines by 2 spaces after first *)
             let body_indented =
               body
               |> String.split_on_char '\n'
               |> (function
                    | [] -> ""
                    | h :: t ->
                        String.concat "\n"
                          (h :: List.map (fun line -> "  " ^ line) t))
             in
             Buffer.add_string buf body_indented;
             Buffer.add_string buf ")\n\n";
             go rest)
  in
  go names

(* AI propose: draft via agent portal, then install on current branch. *)
let propose_function (t : t) ~(name : string) ~(spec : string)
    : Pricklypear_lisp.Ast.value Lwt.t =
  let seed =
    "You write Pricklypear Lisp function bodies.\n" ^
    "OUTPUT RULES (strict):\n" ^
    "1. Reply with EXACTLY one s-expression and nothing else.\n" ^
    "2. The s-expression MUST be a lambda, e.g. (lambda (x) (* x 2)).\n" ^
    "3. Do NOT write analysis, markdown, numbered lists, or prose.\n" ^
    "4. Do NOT write (define ...).\n" ^
    "5. Do NOT wrap the answer in ``` fences.\n" ^
    "Prims: + - * = < > list car cdr cons null? string-append str " ^
    "dict-get dict-set get create update delete rows render-html."
  in
  let prompt =
    Printf.sprintf
      "Name: %s\nSpec: %s\n\nYour entire reply is one lambda s-expression only."
      name spec
  in
  (* Low temperature; authoring is not chat. *)
  let* res =
    Llm_client.agent_chat ~temperature:0.1 ~max_tokens:512
      ~seed ~prompt ()
  in
  match res with
  | Error e ->
      Lwt.return (Pricklypear_lisp.Ast.VStr ("agent error: " ^ e))
  | Ok body_src ->
      install_ai_function t ~name ~body_src

(** Pretty-print a stored function body, or default stub. *)
let current_fn_pretty (t : t) (name : string) : string Lwt.t =
  let* found = Store.lookup_function t.store name in
  match found with
  | Some (expr, _) ->
      Lwt.return (Pricklypear_lisp.Reader.pp_expr expr)
  | None ->
      (match Pricklypear_lisp.Env.lookup t.env name with
       | Some (Pricklypear_lisp.Ast.VLambda (params, body, _)) ->
           let p =
             "(lambda (" ^ String.concat " " params ^ ") " ^
             Pricklypear_lisp.Reader.pp_expr body ^ ")"
           in
           Lwt.return p
       | _ -> Lwt.return "(lambda () #f)")

(** Freeze AI rewrite into store (coding tool). mode: replace|wrap *)
(* ---- Tool-using agent loop (multi-function tasks) ---- *)

let json_string_field (j : Yojson.Safe.t) (k : string) : string option =
  match j with
  | `Assoc items ->
      (match List.assoc_opt k items with
       | Some (`String s) -> Some s
       | Some (`Int i) -> Some (string_of_int i)
       | Some (`Float f) -> Some (string_of_float f)
       | Some (`Bool b) -> Some (string_of_bool b)
       | Some other -> Some (Yojson.Safe.to_string other)
       | None -> None)
  | _ -> None

let pp_value_short (v : Pricklypear_lisp.Ast.value) : string =
  let s = Pricklypear_lisp.Reader.pp_value v in
  if String.length s > 4000 then String.sub s 0 4000 ^ "…" else s

let agent_tool_defs : Llm_client.tool_def list =
  let open Llm_client in
  let obj props required =
    `Assoc
      [ ("type", `String "object")
      ; ("properties", `Assoc props)
      ; ("required", `List (List.map (fun s -> `String s) required))
      ]
  in
  [ { name = "list_bindings"
    ; description = "List function/binding names visible in this image env"
    ; parameters = obj [] []
    }
  ; { name = "list_routes"
    ; description = "List HTTP routes and their handler function names"
    ; parameters = obj [] []
    }
  ; { name = "inspect_function"
    ; description = "Show the Lisp source body of a named function if present"
    ; parameters =
        obj
          [ ("name",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "function name, e.g. demo/double or todo/page")
               ])
          ]
          [ "name" ]
    }
  ; { name = "install_function"
    ; description =
        "Install a Lisp function on the current branch. body must be a lambda s-expression only, e.g. (lambda (x) (* x 2)) or (lambda () (html/page ...)). Do not put (route ...) in the body — use register_route. Author becomes agent:<model>."
    ; parameters =
        obj
          [ ("name",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "binding name to define")
               ])
          ; ("body",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "lambda s-expression source")
               ])
          ]
          [ "name"; "body" ]
    }
  ; { name = "register_route"
    ; description =
        "Bind an HTTP path to an already-installed handler function name on this branch. Use after install_function of the page handler. Example: path=/contacts handler=contacts/page. Do NOT put (route ...) inside install_function bodies."
    ; parameters =
        obj
          [ ("path",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "URL path starting with /, e.g. /contacts")
               ])
          ; ("handler",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "function name, e.g. contacts/page")
               ])
          ]
          [ "path"; "handler" ]
    }
  ; { name = "eval_expr"
    ; description =
        "Evaluate a short Lisp expression in this image (same env as the branch). Use for smoke checks like (count (contacts/all)) or (contacts/name (cdr (car (contacts/all)))). Returns the printed value or an error. Prefer small expressions; no multi-form progn dumps."
    ; parameters =
        obj
          [ ("expr",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "one Lisp expression")
               ])
          ]
          [ "expr" ]
    }
  ; { name = "smoke_route"
    ; description =
        "Call a registered user route handler in-process (no real HTTP). method GET|POST; optional form/query params as a JSON object of strings; optional contains substring that must appear in the HTML/text result (or redirect Location). Use after register_route to verify wiring: smoke_route path=/contacts method=GET contains=Contacts. On failure, inspect_function the handler, fix with install_function, re-smoke."
    ; parameters =
        obj
          [ ("path",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "route path e.g. /contacts")
               ])
          ; ("method",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "GET or POST (default GET)")
               ])
          ; ("params",
             `Assoc
               [ ("type", `String "object")
               ; ("description", `String "form/query params as string values, e.g. {\"action\":\"create\",\"name\":\"Ada\"}")
               ])
          ; ("contains",
             `Assoc
               [ ("type", `String "string")
               ; ("description", `String "optional substring that must appear in body or redirect location")
               ])
          ]
          [ "path" ]
    }
  ; { name = "html_helpers"
    ; description =
        "Return the html/* arity cheatsheet and anti-patterns. Call this BEFORE writing page/form/list HTML with install_function. Required when smoke_route fails with unbound/arity/type errors in html/*."
    ; parameters = obj [] []
    }
  ; { name = "image_context"
    ; description = "Return user, branch, and a short summary of this image"
    ; parameters = obj [] []
    }
  ]

let json_object_string_pairs (j : Yojson.Safe.t) : (string * string) list =
  match j with
  | `Assoc items ->
      List.filter_map
        (fun (k, v) ->
          match v with
          | `String s -> Some (k, s)
          | `Int n -> Some (k, string_of_int n)
          | `Bool b -> Some (k, if b then "true" else "false")
          | `Null -> Some (k, "")
          | other -> Some (k, Yojson.Safe.to_string other))
        items
  | _ -> []

let value_smoke_kind (v : Pricklypear_lisp.Ast.value) : string * string =
  match Primitives.is_redirect v with
  | Some loc -> ("redirect", loc)
  | None ->
      (match Primitives.is_raw_html v with
       | Some html -> ("html", html)
       | None ->
           (match v with
            | Pricklypear_lisp.Ast.VStr s -> ("text", s)
            | _ -> ("value", Pricklypear_lisp.Reader.pp_value v)))

(** Human-readable message from evaluator/reader exceptions (for agent tools). *)
let pp_interp_exn (exn : exn) : string =
  match exn with
  | Pricklypear_lisp.Ast.Interp_error e ->
      (match e with
       | Pricklypear_lisp.Ast.Unbound_var s -> "unbound: " ^ s
       | Pricklypear_lisp.Ast.User_error m -> m
       | Pricklypear_lisp.Ast.Not_a_function _ -> "not a function"
       | Pricklypear_lisp.Ast.Arity_mismatch (name, exp, got) ->
           Printf.sprintf "arity mismatch: %s expected %d got %d" name exp got
       | Pricklypear_lisp.Ast.Wrong_arg_count (name, exp, got) ->
           Printf.sprintf "wrong arg count: %s expected %d got %d" name exp got
       | Pricklypear_lisp.Ast.Type_mismatch (expected, v) ->
           Printf.sprintf "type mismatch: expected %s got %s" expected
             (Pricklypear_lisp.Reader.pp_value v)
       | Pricklypear_lisp.Ast.Budget_exceeded -> "budget exceeded"
       | Pricklypear_lisp.Ast.Prim_error (prim, msg) ->
           Printf.sprintf "prim %s: %s" prim msg
       | Pricklypear_lisp.Ast.Parse_error (m, l, c) ->
           Printf.sprintf "parse error: %s (%d:%d)" m l c)
  | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
      Printf.sprintf "parse error: %s (%d:%d)" m l c
  | _ -> Printexc.to_string exn

let html_helpers_cheatsheet : string =
  "Pricklypear html/* cheatsheet (WRONG shapes are the #1 agent failure).\n" ^
  "\n" ^
  "CORE:\n" ^
  "  (html/attr \"class\" \"card\")           ; ONE key, ONE value → string\n" ^
  "  (html/attrs (list a1 a2 a3))          ; join attr strings\n" ^
  "  (html/join (list s1 s2 s3))           ; concat HTML strings\n" ^
  "  (html/tag \"div\" attrs body)           ; attrs = string, body = string\n" ^
  "  (html/void \"input\" attrs)             ; void tag\n" ^
  "  (html/page (list part1 part2))        ; full page via render-html\n" ^
  "\n" ^
  "ELEMENTS (copy these arities exactly):\n" ^
  "  (html/h1 body)  (html/h2 body)  (html/h3 body)\n" ^
  "  (html/p body)                     ; escapes text\n" ^
  "  (html/p* attrs body)              ; raw body HTML allowed\n" ^
  "  (html/div attrs body)\n" ^
  "  (html/span attrs body)\n" ^
  "  (html/ul attrs body)  (html/li attrs body)\n" ^
  "  (html/strong body)  (html/code body)  (html/pre body)\n" ^
  "  (html/label attrs body)\n" ^
  "  (html/button attrs body)\n" ^
  "  (html/a href body)                ; NOT (html/a (list attrs) body)\n" ^
  "  (html/form method action attrs body)\n" ^
  "      e.g. (html/form \"POST\" \"/bookmarks\"\n" ^
  "              (html/attrs (list (html/attr \"class\" \"card\")))\n" ^
  "              (html/join (list ...fields...)))\n" ^
  "  (html/input attrs)\n" ^
  "  (html/hidden name value)\n" ^
  "  (html/text-input name attrs)      ; name is first arg STRING\n" ^
  "  (html/textarea name attrs body)\n" ^
  "  (html/void \"br\" \"\")                ; no html/br binding — use void\n" ^
  "\n" ^
  "ANTI-PATTERNS (will crash smoke_route):\n" ^
  "  ✗ (html/form (list attrs...) (list kids...))\n" ^
  "  ✗ (html/a (list (html/attr ...)) title)\n" ^
  "  ✗ (html/text-input (list (html/attr \"name\" \"x\")))\n" ^
  "  ✗ (html/attr \"a\" \"b\" \"c\" \"d\")     ; only 2 args\n" ^
  "  ✗ (html/br)                        ; unbound\n" ^
  "  ✗ free variable `row` outside list/map\n" ^
  "\n" ^
  "GOOD list item:\n" ^
  "  (html/li (html/attr \"class\" \"card\")\n" ^
  "    (html/join (list\n" ^
  "      (html/a (dict-get fields \"url\") (html/text (dict-get fields \"title\")))\n" ^
  "      (html/form \"POST\" \"/bookmarks\" \"\"\n" ^
  "        (html/join (list (html/hidden \"action\" \"delete\")\n" ^
  "                         (html/hidden \"id\" id)\n" ^
  "                         (html/button (html/attr \"type\" \"submit\") \"Delete\")))))))\n" ^
  "\n" ^
  "Also: inspect_function html/form | html/a | contacts/form-html | todo/add-form\n"

let truncate_for_tool ~(limit : int) (s : string) : string =
  if String.length s <= limit then s
  else String.sub s 0 limit ^ "…(truncated)"

let run_smoke_route
    (t : t)
    ~(path : string)
    ~(meth : http_method)
    ~(params : (string * string) list)
    ~(contains : string option) : string Lwt.t =
  let path = String.trim path in
  let meth_s = match meth with `GET -> "GET" | `POST -> "POST" in
  match Hashtbl.find_opt t.routes path with
  | None ->
      Lwt.return
        (Printf.sprintf
           "ok: false\nerror: no route registered for %s\n(hint: register_route path=%s handler=...)"
           path path)
  | Some (handler, _vis) ->
      (match Pricklypear_lisp.Env.lookup t.env handler with
       | None ->
           Lwt.return
             (Printf.sprintf
                "ok: false\nroute: %s -> %s\nerror: handler unbound in env"
                path handler)
       | Some _ ->
           let req =
             { method_ = meth
             ; path
             ; params
             ; body = ""
             ; headers =
                 (match meth with
                  | `POST ->
                      [ ("content-type", "application/x-www-form-urlencoded") ]
                  | `GET -> [])
             }
           in
           t.current_request <- Some req;
           let* v_res =
             Lwt.finalize
               (fun () ->
                  Lwt.catch
                    (fun () ->
                       let* v =
                         Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env
                           ("(" ^ handler ^ ")")
                       in
                       Lwt.return (`Ok v))
                    (fun exn ->
                       Lwt.return
                         (`Err ("handler error: " ^ pp_interp_exn exn))))
               (fun () ->
                  t.current_request <- None;
                  Lwt.return_unit)
           in
           (match v_res with
            | `Err msg ->
                Lwt.return
                  (Printf.sprintf
                     "ok: false\nroute: %s -> %s\nmethod: %s\nerror: %s"
                     path handler meth_s msg)
            | `Ok v ->
                let kind, body = value_smoke_kind v in
                let contains_ok =
                  match contains with
                  | None | Some "" -> true
                  | Some needle ->
                      let nlen = String.length needle in
                      let blen = String.length body in
                      let rec find i =
                        if i + nlen > blen then false
                        else if String.sub body i nlen = needle then true
                        else find (i + 1)
                      in
                      find 0
                in
                let ok = contains_ok in
                let snippet = truncate_for_tool ~limit:500 body in
                let lines =
                  [ Printf.sprintf "ok: %s" (if ok then "true" else "false")
                  ; Printf.sprintf "route: %s -> %s" path handler
                  ; Printf.sprintf "method: %s" meth_s
                  ; Printf.sprintf "kind: %s" kind
                  ; Printf.sprintf "bytes: %d" (String.length body)
                  ]
                in
                let lines =
                  match contains with
                  | None | Some "" -> lines
                  | Some needle ->
                      lines
                      @ [ Printf.sprintf "contains %S: %s" needle
                            (if contains_ok then "true" else "false") ]
                in
                let lines =
                  if not ok then
                    lines
                    @ [ "error: smoke failed — inspect_function, fix, re-smoke" ]
                  else lines
                in
                Lwt.return
                  (String.concat "\n" lines ^ "\n--- snippet ---\n" ^ snippet)))

let run_agent_tool (t : t) ~(name : string) ~(arguments : string)
    : string Lwt.t =
  let args_json =
    try Yojson.Safe.from_string arguments with _ -> `Assoc []
  in
  match name with
  | "list_bindings" ->
      let names =
        try
          match Primitives.prim_list_bindings_fn ~env:t.env [] with
          | Pricklypear_lisp.Ast.VList xs ->
              List.filter_map
                (function Pricklypear_lisp.Ast.VStr s -> Some s | _ -> None) xs
              |> List.sort String.compare
          | _ -> []
        with _ -> []
      in
      Lwt.return (if names = [] then "(no bindings)" else String.concat "\n" names)
  | "list_routes" ->
      let rs =
        Hashtbl.fold
          (fun route (handler, vis) acc ->
            (Printf.sprintf "%s -> %s [%s]" route handler vis) :: acc)
          t.routes []
        |> List.sort String.compare
      in
      Lwt.return (if rs = [] then "(no routes)" else String.concat "\n" rs)
  | "inspect_function" ->
      (match json_string_field args_json "name" with
       | None -> Lwt.return "error: need name"
       | Some n ->
           let* pretty = current_fn_pretty t n in
           Lwt.return pretty)
  | "install_function" ->
      (match json_string_field args_json "name", json_string_field args_json "body" with
       | Some n, Some body ->
           let* v = install_ai_function t ~name:n ~body_src:body in
           Lwt.return (pp_value_short v)
       | _ -> Lwt.return "error: need name and body")
  | "register_route" ->
      (match json_string_field args_json "path", json_string_field args_json "handler" with
       | Some path, Some handler ->
           let path = String.trim path in
           let handler = String.trim handler in
           let bad_prefix =
             List.exists
               (fun p ->
                 path = p
                 || (String.length path > String.length p
                     && String.sub path 0 (String.length p + 1) = p ^ "/"))
               [ "/api"; "/admin"; "/login"; "/logout"; "/health"; "/agent" ]
           in
           if path = "" || path.[0] <> '/' then
             Lwt.return "error: path must start with /"
           else if bad_prefix then
             Lwt.return ("error: path reserved: " ^ path)
           else if handler = "" then
             Lwt.return "error: need handler function name"
           else begin
             let vis =
               match json_string_field args_json "visibility" with
               | Some v -> Store.normalize_visibility v
               | None -> "private"
             in
             Hashtbl.replace t.routes path (handler, vis);
             let* _ =
               Store.register_route t.store ~route:path ~handler
                 ~visibility:vis ()
             in
             Lwt.return
               (Printf.sprintf "registered route %s -> %s (%s)"
                  path handler vis)
           end
       | _ -> Lwt.return "error: need path and handler")
  | "eval_expr" ->
      (match json_string_field args_json "expr" with
       | None -> Lwt.return "error: need expr"
       | Some expr ->
           let expr = String.trim expr in
           if expr = "" then Lwt.return "error: empty expr"
           else if String.length expr > 4000 then
             Lwt.return "error: expr too long (max 4000)"
           else
             Lwt.catch
               (fun () ->
                  let* v =
                    Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env expr
                  in
                  let s = Pricklypear_lisp.Reader.pp_value v in
                  Lwt.return
                    ("ok: true\nvalue: " ^ truncate_for_tool ~limit:1500 s))
               (fun exn ->
                  Lwt.return ("ok: false\nerror: " ^ pp_interp_exn exn)))
  | "smoke_route" ->
      (match json_string_field args_json "path" with
       | None -> Lwt.return "error: need path"
       | Some path ->
           let meth =
             match json_string_field args_json "method" with
             | Some m when String.uppercase_ascii (String.trim m) = "POST" ->
                 `POST
             | _ -> `GET
           in
           let params =
             match args_json with
             | `Assoc items ->
                 (match List.assoc_opt "params" items with
                  | Some p -> json_object_string_pairs p
                  | None -> [])
             | _ -> []
           in
           let contains = json_string_field args_json "contains" in
           run_smoke_route t ~path ~meth ~params ~contains)
  | "html_helpers" ->
      Lwt.return html_helpers_cheatsheet
  | "image_context" ->
      let names =
        try
          match Primitives.prim_list_bindings_fn ~env:t.env [] with
          | Pricklypear_lisp.Ast.VList xs ->
              List.filter_map
                (function Pricklypear_lisp.Ast.VStr s -> Some s | _ -> None)
                xs
          | _ -> []
        with _ -> []
      in
      let nshow = List.length names in
      let sample = String.concat ", " (List.filteri (fun i _ -> i < 40) names) in
      Lwt.return
        (Printf.sprintf
           "user=%s\nbranch=%s\nbinding_count=%d\nbindings_sample=%s"
           t.manifest.username t.manifest.branch_name nshow sample)
  | other ->
      Lwt.return ("error: unknown tool " ^ other)

(** One durable tool-capable loop over an ai_sessions row.
    Loads history from Postgres, resubmits full messages[] each turn,
    appends user/assistant/tool rows as it goes. *)
let run_loop
    (t : t)
    ~(sess : Ai_store.session)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    () : string Lwt.t =
  t.agent_branch_lock <- true;
  let open Llm_client in
  let installs : string list ref = ref [] in
  let rec loop step =
    if step > max_steps then begin
      let summary =
        match !installs with
        | [] ->
            "agent: max steps exceeded (no installs recorded). " ^
            "Try a smaller task or raise max-steps."
        | xs ->
            "agent: max steps exceeded after installs:\n- " ^
            String.concat "\n- " (List.rev xs) ^
            "\n(Partial progress kept on this branch and in the session.)"
      in
      on_event (Fail summary);
      Lwt.return summary
    end else
      let* history = Ai_store.list_messages t.store ~session_id:sess.id in
      let msgs = Ai_store.messages_for_api sess history in
      (* Last step: no tools — force a plain-text wrap-up. *)
      let tools_now =
        if (not sess.tools) || step = max_steps then [] else agent_tool_defs
      in
      let* turn =
        chat_turn ~temperature:0.2 ~max_tokens:2048
          ~tools:tools_now ~messages:msgs ()
      in
      match turn with
      | Turn_error e ->
          on_event (Fail e);
          Lwt.return ("agent error: " ^ e)
      | Final text ->
          let* r =
            Ai_store.append_messages t.store ~session_id:sess.id
              [ Msg { role = Assistant; content = text } ]
          in
          (match r with
           | Error e ->
               on_event (Fail e);
               Lwt.return ("agent error: " ^ e)
           | Ok () ->
               on_event (Token text);
               on_event Done;
               Lwt.return text)
      | Need_tools { content; tool_calls } ->
          if tool_calls = [] then begin
            let fallback =
              match content with
              | Some c when String.trim c <> "" -> c
              | _ ->
                  "agent: model requested tools but sent none; stopping."
            in
            let* _ =
              Ai_store.append_messages t.store ~session_id:sess.id
                [ Msg { role = Assistant; content = fallback } ]
            in
            on_event (Token fallback);
            on_event Done;
            Lwt.return fallback
          end else begin
            (match content with
             | Some c when String.trim c <> "" -> on_event (Token (c ^ "\n"))
             | _ -> ());
            let* r1 =
              Ai_store.append_messages t.store ~session_id:sess.id
                [ Assistant_tools { content; tool_calls } ]
            in
            match r1 with
            | Error e ->
                on_event (Fail e);
                Lwt.return ("agent error: " ^ e)
            | Ok () ->
                let* tool_rows =
                  Lwt_list.map_s
                    (fun (tc : tool_call) ->
                      on_event (Token (Printf.sprintf "[tool %s]\n" tc.name));
                      let* result =
                        run_agent_tool t ~name:tc.name ~arguments:tc.arguments
                      in
                      if tc.name = "install_function" then begin
                        let n =
                          match
                            try
                              json_string_field
                                (Yojson.Safe.from_string tc.arguments) "name"
                            with _ -> None
                          with
                          | Some name -> name
                          | None -> "?"
                        in
                        installs := n :: !installs
                      end;
                      on_event
                        (Token
                           (Printf.sprintf "→ %s\n"
                              (if String.length result > 200 then
                                 String.sub result 0 200 ^ "…"
                               else result)));
                      Lwt.return
                        (Tool_result
                           { tool_call_id = tc.id; content = result }))
                    tool_calls
                in
                let* r2 =
                  Ai_store.append_messages t.store ~session_id:sess.id tool_rows
                in
                (match r2 with
                 | Error e ->
                     on_event (Fail e);
                     Lwt.return ("agent error: " ^ e)
                 | Ok () -> loop (step + 1))
          end
  in
  Lwt.finalize
    (fun () -> loop 1)
    (fun () ->
      t.agent_branch_lock <- false;
      Lwt.return_unit)

let default_agent_seed (t : t) : string =
  "You are the agent for a running Pricklypear multi-user Lisp OS image " ^
  "(software, not a cactus).\n" ^
  Printf.sprintf
    "LOCKED BRANCH: %s — you see only the effective bindings on this branch "
    t.manifest.branch_name ^
  "(personal overrides shadow main; one name = one function).\n" ^
  "All install_function writes go to THIS branch only. " ^
  "Do NOT attempt checkout, fork, or merge.\n" ^
  "Tools: image_context, list_bindings, list_routes, inspect_function, " ^
  "install_function, register_route, smoke_route, eval_expr, html_helpers.\n" ^
  "\n" ^
  "How to build a small app (dogfood pattern — copy todo/notes style):\n" ^
  "1) Persist with create/rows/get/update/delete on data_rows. " ^
  "Build fields JSON with (dict-set* \"{}\" (list \"name\" n \"email\" e ...)). " ^
  "Optional init: (lambda () (deftype 'contact 'name 'email 'phone 'notes)).\n" ^
  "2) install_function name=pkg/page body=(lambda () ...) using html/* " ^
  "helpers: html/page, html/form method action attrs body, html/join, " ^
  "html/text-input, html/hidden, html/button. Never invent html/nil or " ^
  "multi-arg html/attr bags like (html/attr \"a\" \"b\" \"c\" \"d\").\n" ^
  "3) Row shape from (rows 'type): each row is (id . fields-json). " ^
  "Use (car row) for id, (cdr row) for fields, (dict-get fields \"name\").\n" ^
  "4) POST handlers: branch on (request-method) and (request-param \"action\"); " ^
  "after mutate, (redirect \"/path\") or (render-html ...) for htmx.\n" ^
  "5) NEVER use outline/* as a database. outline/* is for notes trees only.\n" ^
  "6) Routes: call register_route path=/pkg handler=pkg/page. " ^
  "Do NOT (route ...) inside install_function (publish effect is blocked there).\n" ^
  "7) Inspect working apps first: inspect_function todo/page, notes/page, " ^
  "todo/add, html/form.\n" ^
  "\n" ^
  "Test loop (required for apps):\n" ^
  "8) After register_route, smoke_route path=/app method=GET contains=Title.\n" ^
  "9) Data: eval_expr (count (pkg/all)); POST via smoke_route params then GET.\n" ^
  "10) If smoke fails: call html_helpers, inspect_function, fix install_function, re-smoke until ok:true.\n" ^
  "\n" ^
  "HTML: call html_helpers before install_function of *page/*form/*list* bodies.\n" ^
  "Tool discipline:\n" ^
  "- Use ONLY the structured tool API. Never write <tool_call> XML in text.\n" ^
  "- install_function body = one lambda s-expression only, no prose/markdown.\n" ^
  "- Batch multiple tool calls in one turn when possible.\n" ^
  "- Prefer package/name names (contacts/page).\n" ^
  "- When finished, SHORT text summary and NO tool calls.\n" ^
  "- Do not invent bindings; use list_bindings / inspect_function."

let agent_session_new
    (t : t)
    ~(seed : string)
    ?(tools = true)
    ?title
    () : Pricklypear_lisp.Ast.value Lwt.t =
  let* r =
    Ai_store.create_session t.store
      ~owner:t.manifest.username
      ~seed
      ?title
      ~branch_ref:t.manifest.branch_name
      ~tools
      ~model:(Llm_client.model ())
      ()
  in
  match r with
  | Ok s -> Lwt.return (Pricklypear_lisp.Ast.VStr s.id)
  | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("agent-session-new error: " ^ e))

let agent_say
    (t : t)
    ~(session_id : string)
    ~(prompt : string)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    () : Pricklypear_lisp.Ast.value Lwt.t =
  let* sess_opt =
    Ai_store.get_session t.store
      ~owner:t.manifest.username
      ~id:session_id
  in
  match sess_opt with
  | None ->
      Lwt.return
        (Pricklypear_lisp.Ast.VStr
           ("agent-say: no session " ^ session_id ^ " for " ^ t.manifest.username))
  | Some sess ->
      let title =
        match sess.title with
        | Some _ -> None
        | None ->
            let p =
              let s = String.trim prompt in
              let key = "\nUser:\n" in
              let rec find i =
                if i + String.length key > String.length s then None
                else if String.sub s i (String.length key) = key then Some i
                else find (i + 1)
              in
              match find 0 with
              | Some i ->
                  String.trim
                    (String.sub s (i + String.length key)
                       (String.length s - i - String.length key))
              | None -> s
            in
            if String.trim p = "" then None
            else
              Some
                (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
      in
      let* () =
        Ai_store.touch_session t.store ~id:sess.id ?title
          ~branch_ref:t.manifest.branch_name ()
      in
      let* ar =
        Ai_store.append_messages t.store ~session_id:sess.id
          [ Llm_client.Msg { role = Llm_client.User; content = prompt } ]
      in
      match ar with
      | Error e ->
          Lwt.return (Pricklypear_lisp.Ast.VStr ("agent-say error: " ^ e))
      | Ok () ->
          let* text =
            run_loop t ~sess ~max_steps ~on_event ()
          in
          Lwt.return (Pricklypear_lisp.Ast.VStr text)

let agent_history (t : t) ~(session_id : string)
    : Pricklypear_lisp.Ast.value Lwt.t =
  let* sess_opt =
    Ai_store.get_session t.store
      ~owner:t.manifest.username
      ~id:session_id
  in
  match sess_opt with
  | None ->
      Lwt.return
        (Pricklypear_lisp.Ast.VStr
           ("agent-history: no session " ^ session_id))
  | Some _ ->
      let* pairs =
        Ai_store.history_summaries t.store ~session_id
      in
      let rows =
        List.map
          (fun (role, content) ->
            Pricklypear_lisp.Ast.VList
              [ Pricklypear_lisp.Ast.VStr role
              ; Pricklypear_lisp.Ast.VStr content
              ])
          pairs
      in
      Lwt.return (Pricklypear_lisp.Ast.VList rows)

let agent_sessions (t : t) ?(limit = 50) ()
    : Pricklypear_lisp.Ast.value Lwt.t =
  let* xs =
    Ai_store.list_sessions t.store
      ~owner:t.manifest.username
      ~limit
      ()
  in
  let rows =
    List.map
      (fun (s : Ai_store.session) ->
        let title = Option.value s.title ~default:"" in
        Pricklypear_lisp.Ast.VList
          [ Pricklypear_lisp.Ast.VStr s.id
          ; Pricklypear_lisp.Ast.VStr title
          ; Pricklypear_lisp.Ast.VStr s.updated_at
          ])
      xs
  in
  Lwt.return (Pricklypear_lisp.Ast.VList rows)

let agent_session_delete (t : t) ~(session_id : string)
    : Pricklypear_lisp.Ast.value Lwt.t =
  let* ok =
    Ai_store.delete_session t.store
      ~owner:t.manifest.username
      ~id:session_id
  in
  Lwt.return
    (Pricklypear_lisp.Ast.VStr
       (if ok then "deleted" else "not found"))

(** Temporary alias: new session (tools on) + one say.
    Prefer (agent-session-new) + (agent-say). [stream] ignored. *)
let agent
    (t : t)
    ?(stream = false)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    ~(seed : string)
    ~(prompt : string)
    () : Pricklypear_lisp.Ast.value Lwt.t =
  let _ = stream in
  let title_opt =
    let p = String.trim prompt in
    if p = "" then None
    else Some (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
  in
  let* created =
    agent_session_new t ~seed ~tools:true ?title:title_opt ()
  in
  match created with
  | Pricklypear_lisp.Ast.VStr sid
    when not (String.starts_with ~prefix:"agent-session-new error" sid) ->
      agent_say t ~session_id:sid ~prompt ~on_event ()
  | other -> Lwt.return other

(** Temporary alias: new tools session + one say. *)
let agent_task
    (t : t)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    ~(prompt : string)
    () : Pricklypear_lisp.Ast.value Lwt.t =
  let seed = default_agent_seed t in
  let title_opt =
    let p = String.trim prompt in
    if p = "" then None
    else Some (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
  in
  let* created =
    agent_session_new t ~seed ~tools:true ?title:title_opt ()
  in
  match created with
  | Pricklypear_lisp.Ast.VStr sid when not (String.starts_with ~prefix:"agent-session-new error" sid) ->
      agent_say t ~session_id:sid ~prompt ~max_steps ~on_event ()
  | other -> Lwt.return other


let llm_rewrite (t : t) ~(mode : string) ~(name : string) ~(concept : string)
    : Pricklypear_lisp.Ast.value Lwt.t =
  let* current = current_fn_pretty t name in
  let* res =
    Llm_client.rewrite_component_source ~mode ~name ~concept
      ~current_pretty:current ()
  in
  match res with
  | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("ai error: " ^ e))
  | Ok body_src ->
      (match check_ai_body_effects body_src with
       | Error pe ->
           Lwt.return
             (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "ai rejected by effects: %s\n---\n%s" pe body_src))
       | Ok () ->
           let author = "agent:" ^ Llm_client.model () in
           let* def = define_binding t ~name ~body_src ~author in
           (match def with
            | Ok _ ->
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "froze %s (%s) by %s\n%s"
                        name mode author body_src))
            | Error e ->
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "freeze failed: %s\n---\n%s" e body_src))))

let llm_inner t ~name ~concept =
  llm_rewrite t ~mode:"replace" ~name ~concept

let llm_outer t ~name ~concept =
  llm_rewrite t ~mode:"wrap" ~name ~concept

(** Growth-from-seed: smallest core then recursive outer wraps. *)
let llm_seed (t : t) ~(requirement : string) ?(max_steps = 5) ()
    : Pricklypear_lisp.Ast.value Lwt.t =
  let tip = "seed" in
  let* seed_res = Llm_client.seed_core_source ~requirement () in
  match seed_res with
  | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("seed error: " ^ e))
  | Ok body_src ->
      (match check_ai_body_effects body_src with
       | Error pe ->
           Lwt.return
             (Pricklypear_lisp.Ast.VStr ("seed rejected: " ^ pe ^ "\n" ^ body_src))
       | Ok () ->
           let author = "agent:" ^ Llm_client.model () in
           let* def = define_binding t ~name:tip ~body_src ~author in
           (match def with
            | Error e ->
                Lwt.return (Pricklypear_lisp.Ast.VStr ("seed freeze failed: " ^ e))
            | Ok _ ->
                let log = Buffer.create 256 in
                Buffer.add_string log (Printf.sprintf "step0 seed %s\n%s\n" tip body_src);
                let rec grow step =
                  if step > max_steps then
                    Lwt.return
                      (Pricklypear_lisp.Ast.VStr
                         (Buffer.contents log ^ "stopped: max-steps\n"))
                  else
                    let* pretty = current_fn_pretty t tip in
                    let* judge =
                      Llm_client.growth_judge ~requirement ~current_pretty:pretty
                        ~step ()
                    in
                    match judge with
                    | Error e ->
                        Lwt.return
                          (Pricklypear_lisp.Ast.VStr
                             (Buffer.contents log ^ "judge error: " ^ e))
                    | Ok line ->
                        let upper = String.uppercase_ascii (String.trim line) in
                        if String.length upper >= 4 && String.sub upper 0 4 = "DONE"
                        then
                          Lwt.return
                            (Pricklypear_lisp.Ast.VStr
                               (Buffer.contents log ^ "DONE at step " ^
                                string_of_int step ^ "\n" ^ pretty))
                        else
                          let concept =
                            if String.length line >= 5
                               && String.sub (String.lowercase_ascii line) 0 5 = "next:"
                            then String.trim (String.sub line 5 (String.length line - 5))
                            else String.trim line
                          in
                          Buffer.add_string log
                            (Printf.sprintf "step%d NEXT %s\n" step concept);
                          let* wr =
                            llm_outer t ~name:tip ~concept
                          in
                          Buffer.add_string log
                            (Pricklypear_lisp.Reader.pp_value wr ^ "\n");
                          grow (step + 1)
                in
                grow 1))

(* Register the 5 CRUD primitives (get/create/update/delete/list) over
   the shared `data_rows` table. These are NORMAL prims on the Lwt-typed
   ABI (post-iteration-12): prim_fn = value list -> value Lwt.t. They
   return their real DB result through the promise chain — e.g.
   (get 'user 42) returns the user's JSON fields as a VStr in `(str (get
   'user 42))`; `(mapcar (lambda (id) (get 'user id)) ids)` works.

   Per spec [storage.types] / migration 003: data_rows is shared across
   branches (no branch_id column), so these prims don't walk the
   recursive CTE — they hit the table directly. Author is the session's
   current username (recorded on every create). *)
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
