(** Pricklypear Image — REPL session.

    A per-connection session holds the env (function defs loaded for the
    session's branch, plus any (define)'d locals), the manifest (identity
    claim asserted by supervisor), and the store handle.

    Per spec [image.repl-experience]:
      - (define ...) persists to the store: survives image reboot.
      - REPL-local let bindings are session-local: NOT persisted.
      - Switching branch (checkout) keeps REPL locals but changes visibility. *)
open Lwt.Syntax

type t = {
  mutable manifest : Pricklypear_common.Manifest.t;
  store : Store.t;
  env : Pricklypear_lisp.Ast.env;
  mutable prims : Pricklypear_lisp.Eval.prim_table;
  (* In-memory route table mirroring view_bindings for the branch.
     Populated by load_defs_async at session-make time; (route ...)
     updates also live-persist to view_bindings AND update this table.
     http.ml reads from here on every request (no DB round-trip). *)
  routes : (string, string) Hashtbl.t;
  mutable started_at : float;
}

(* Asynchronously load all persisted function_defs for this branch
   into the env. Non-blocking — the REPL starts immediately, defs
   appear as the store responds. *)
let load_defs_async (t : t) : unit =
  Lwt.async (fun () ->
    let* defs = Store.list_function_names t.store in
    (* Iterate ALL definitions; the first occurrence (depth ASC) wins
       because the recursive CTE returns ancestors first. *)
    let seen = Hashtbl.create 16 in
    let* _ =
      Lwt_list.iter_s
        (fun (name, body, _author) ->
          if not (Hashtbl.mem seen name) then begin
            Hashtbl.replace seen name ();
            (* Re-evaluate (define name <body>) so the session env binds
               the deserialized lambda/value to [name]. *)
            Lwt.catch
              (fun () ->
                 let* _ =
                   Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env
                     (Printf.sprintf "(define %s %s)" name
                        (Pricklypear_lisp.Reader.pp_expr body))
                 in
                 Lwt.return_unit)
              (fun _exn -> Lwt.return_unit)
          end else Lwt.return_unit)
        defs
    in
    (* Also load view_bindings into the in-memory routes table. The
       recursive-CTE returns ancestors first, so a route defined in both
       the child and parent: child's wins (Hashtbl.replace on the
       first-seen only — skip further matches). *)
    let* routes = Store.list_routes t.store in
    let seen_r = Hashtbl.create 16 in
    List.iter
      (fun (route, handler) ->
        if not (Hashtbl.mem seen_r route) then begin
          Hashtbl.replace seen_r route ();
          Hashtbl.replace t.routes route handler
        end)
      routes;
    Lwt.return_unit)

let split_branch_ref (s : string) : string * string =
  match String.split_on_char '/' s with
  | [ o; n ] -> o, n
  | [ n ] -> "system", n  (* bare name = system/<name> *)
  | _ -> "system", s

let register_session_control_prims (t : t) (tbl : Primitives.t) : unit =
  (* (fork 'parent 'child) -> VStr new-branch-id | VStr "no such parent" *)
  Primitives.register tbl "fork"
    (fun args ->
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
      match args with
      | [ Pricklypear_lisp.Ast.VStr source_ref;
          Pricklypear_lisp.Ast.VStr target_ref ] ->
          let src_owner, src_short = split_branch_ref source_ref in
          let tgt_owner, tgt_short = split_branch_ref target_ref in
          let authorized =
            (tgt_owner <> "system")
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

  (* (route "/path" 'handler-name) -> VStr "/path".
     Updates the in-memory routes table immediately (so the next HTTP
     request sees it) and persists to view_bindings. *)
  Primitives.register tbl "route"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr route_path;
          Pricklypear_lisp.Ast.VStr handler ] ->
          Hashtbl.replace t.routes route_path handler;
          let* _ = Store.register_route t.store ~route:route_path ~handler in
          Lwt.return (Pricklypear_lisp.Ast.VStr route_path)
      | _ ->
          Lwt.return (Pricklypear_lisp.Ast.VStr "route: bad args"));

  (* (checkout 'branch-ref) -> VStr "checked out" | "no such branch".
     Mutates t.store.branch_id + t.manifest.branch_name, then re-hydrates
     the env from the new branch's function_defs via load_defs_async. *)
  Primitives.register tbl "checkout"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr branch_ref ] ->
          let owner, name = split_branch_ref branch_ref in
          let* found =
            Store.lookup_branch_id_by_name t.store ~owner ~name
          in
          (match found with
           | None -> Lwt.return (Pricklypear_lisp.Ast.VStr "no such branch")
           | Some new_id ->
               let* _ = Store.checkout_branch t.store ~new_branch_id:new_id in
               t.manifest <- { t.manifest with
                               Pricklypear_common.Manifest.branch_name = branch_ref };
               load_defs_async t;
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
let recipe_hello (t : t) : (string, string) result Lwt.t =
  let body_src =
    "(lambda () (render-html \"<h1>Hello</h1><p>Your first Pricklypear page. Edit me at /edit/hi-page.</p>\"))"
  in
  let* def =
    define_binding t ~name:"hi-page" ~body_src ~author:(t.manifest.username)
  in
  match def with
  | Error e -> Lwt.return (Error e)
  | Ok _ ->
      Hashtbl.replace t.routes "/hello" "hi-page";
      let* _ = Store.register_route t.store ~route:"/hello" ~handler:"hi-page" in
      Lwt.return
        (Ok "created function hi-page and route /hello — open /hello")

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
           "effect policy denied (%s); allow pure|read-db|html by default; set PP_AI_ALLOW_WRITE=1 to elevate"
           (Pricklypear_lisp.Effect.EffSet.pp fx))
  with _ -> Ok ()  (* parse errors handled by define_binding *)

(* AI propose: LLM writes body; we define with agent provenance. *)
let propose_function (t : t) ~(name : string) ~(spec : string)
    : Pricklypear_lisp.Ast.value Lwt.t =
  let* res = Llm_client.propose_function_source ~name ~spec () in
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
                let badge =
                  try
                    let e = Pricklypear_lisp.Reader.read_one body_src in
                    Pricklypear_lisp.Effect.EffSet.badge
                      (Pricklypear_lisp.Effect.infer e)
                  with _ -> "?"
                in
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "proposed %s by %s [%s]\n%s"
                        name author badge body_src))
            | Error e ->
                Lwt.return
                  (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "ai proposed invalid body: %s\n---\n%s" e body_src))))

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
  Primitives.register tbl "deftype"
    (fun args ->
      Crud.deftype t.store ~args ~author:t.manifest.username);

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
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr fields_json ] ->
          Crud.create t.store
            ~type_name ~fields_json
            ~author:t.manifest.username
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "create: bad args (need type, fields)"));

  (* (update 'type id "{fields-json}") -> VStr id | VNil *)
  Primitives.register tbl "update"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id;
          Pricklypear_lisp.Ast.VStr fields_json ] ->
          Crud.update t.store ~type_name ~id ~fields_json
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "update: bad args (need type, id, fields)"));

  (* (update-field 'type id "field" "value") -> VStr id | VNil.
      Ergonomic shorthand: composes get + dict-set + update in one prim.
      Lands it18 (2026-07-06); resolves the it14 "verbose round-trip syntax" TODO. *)
  Primitives.register tbl "update-field"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr value ] ->
          Crud.update_field t.store ~type_name ~id ~field ~value
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "update-field: bad args (need type, id, field, value)"));

  (* (delete 'type id) -> VStr id | VNil *)
  Primitives.register tbl "delete"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          Crud.delete t.store ~type_name ~id
      | _ -> Lwt.return (Pricklypear_lisp.Ast.VStr "delete: bad args (need type, id)"));

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


let make ~(manifest : Pricklypear_common.Manifest.t) ~(store : Store.t) () : t =
  let env = Pricklypear_lisp.Env.create () in
  let prim_table = Primitives.make_with_defaults () in
  (* Register session-aware primitives (whoami, current-author,
     list-bindings). These capture the manifest.username and env as
     closures. *)
  Primitives.register_session_prims prim_table
    ~username:manifest.username ~env;
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
    }
  in
  (* Register the DB-touching control primitives on the prim_table,
     then rebind t.prims to the populated table. These prims capture
     (!t), so they read/write store+manifest on each invocation. *)
  register_session_control_prims !t prim_table;
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
  let final_prims = Primitives.to_eval_table prim_table in
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
  let session : t = !t in
  (* Kick off background loading of persisted function_defs into env. *)
  load_defs_async session;
  session

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
let eval_one (t : t) (input : string) : Pricklypear_lisp.Ast.value Lwt.t =
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
            DB write is non-fatal on PG-down. *)
         Lwt.async (fun () ->
           Store.persist_function t.store name body_expr t.manifest.username);
         Lwt.return v
     | _ -> Lwt.return v)
  with
  | Pricklypear_lisp.Ast.Interp_error e as exn -> Lwt.fail exn
  | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
      Lwt.fail (Pricklypear_lisp.Ast.Interp_error
                  (Pricklypear_lisp.Ast.Parse_error (msg, line, col)))
  | exn -> Lwt.fail exn

(* Register the DB-touching control primitives (fork/merge/route/checkout)
   on a session's prim_table. These capture the session [t] so they can
   read/write the store + manifest. They are normal prim_fns returning
   'a Lwt.t — the Lwt-typed evaluator threads the promise all the way
   back to the REPL caller, so e.g. `(fork ...)` returns the new branch
   id, `(merge ...)` returns the row count. *)
let uptime_seconds (t : t) : int =
  int_of_float (Unix.gettimeofday () -. t.started_at)

let branch_name (t : t) : string = t.manifest.branch_name
let username (t : t) : string = t.manifest.username
let store (t : t) : Store.t = t.store

(* Look up a route's handler name. Returns None if the route isn't
   registered on this branch. Called by http.ml's request_handler. *)
let lookup_route (t : t) (route : string) : string option =
  Hashtbl.find_opt t.routes route

let list_pages (t : t) : (string * string) list =
  Hashtbl.fold (fun route handler acc -> (route, handler) :: acc) t.routes []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* Invoke a named handler function (looked up via env). The handler
   must be a zero-arg VLambda or a VPrim name. Returns its value, or
   raises Interp_error if the handler doesn't resolve.
   Used by http.ml to render dynamic pages. *)
let call_handler (t : t) (handler_name : string)
  : Pricklypear_lisp.Ast.value option Lwt.t =
  match Pricklypear_lisp.Env.lookup t.env handler_name with
  | None -> Lwt.return_none
  | Some _fn ->
      (* Evaluate (handler-name) — zero args. Re-uses eval's apply path
         so VLambda closures AND VPrim dispatch both work. *)
      Lwt.catch
        (fun () ->
           let* v = Pricklypear_lisp.Eval.eval_string ~prims:t.prims t.env
                      ("(" ^ handler_name ^ ")")
           in
           Lwt.return (Some v))
        (fun _exn -> Lwt.return_none)
