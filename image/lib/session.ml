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
     http.ml reads from here on every request (no DB round-trip). *)
  routes : (string, string) Hashtbl.t;
  mutable started_at : float;
  (** Kernel prim catalog for Functions browser: (name, summary) list. *)
  mutable kernel_catalog : (string * string) list;
  (** Cached HTML from ui/nav when defined; None = use OCaml default nav. *)
  mutable ui_nav_html : string option;
  (** Active HTTP request for user-route dispatch; None in REPL. *)
  mutable current_request : request option;
}

(* Load persisted function_defs + routes into the env.
   Returns when finished (caller may then seed libs/ which overwrite). *)
let load_defs (t : t) : unit Lwt.t =
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
               Lwt.return_unit)
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
     Mutates t.store.branch_id + t.manifest, then re-hydrates the env
     from the new branch's function_defs (sync — so the next form sees
     the right bindings). *)
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
           if n = "ui/nav" || n = "ui/home" then t.ui_nav_html <- None;
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
           "effect policy denied (%s); allow pure|read-db|html by default; set PP_AI_ALLOW_WRITE=1 to elevate"
           (Pricklypear_lisp.Effect.EffSet.pp fx))
  with _ -> Ok ()  (* parse errors handled by define_binding *)

(** Unified agent portal: seed + prompt -> final text.
    Progress via optional [on_event] (SSE / log). Does not define. *)
let agent
    (t : t)
    ?(stream = false)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    ~(seed : string)
    ~(prompt : string)
    () : Pricklypear_lisp.Ast.value Lwt.t =
  let _ = t in
  let* res =
    Llm_client.agent_chat ~stream ~on_event ~seed ~prompt ()
  in
  match res with
  | Ok text -> Lwt.return (Pricklypear_lisp.Ast.VStr text)
  | Error e -> Lwt.return (Pricklypear_lisp.Ast.VStr ("agent error: " ^ e))

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
        "Install a Lisp function on the current branch. body must be a lambda          s-expression only, e.g. (lambda (x) (* x 2)). Author becomes agent:<model>."
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
  ; { name = "image_context"
    ; description = "Return user, branch, and a short summary of this image"
    ; parameters = obj [] []
    }
  ]

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
          (fun route handler acc -> (route ^ " -> " ^ handler) :: acc)
          t.routes []
        |> List.sort String.compare
      in
      Lwt.return (if rs = [] then "(no routes)" else String.concat "
" rs)
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

(** Multi-turn agent with tools: can install many functions to finish a task. *)
let agent_task
    (t : t)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    ~(prompt : string)
    () : Pricklypear_lisp.Ast.value Lwt.t =
  let seed =
    "You are the agent for a running Pricklypear multi-user Lisp OS image " ^
    "(software, not a cactus).\n" ^
    "Tools: image_context, list_bindings, list_routes, inspect_function, install_function.\n" ^
    "Efficiency rules:\n" ^
    "- Batch: emit MULTIPLE tool calls in ONE assistant turn when possible " ^
    "(e.g. several install_function calls together).\n" ^
    "- Do not re-list bindings after every install unless needed.\n" ^
    "- install_function body = one lambda s-expression only, no prose.\n" ^
    "- Prefer package/name names (demo/hello).\n" ^
    "- When finished, respond with a SHORT text summary and NO tool calls.\n" ^
    "- Do not invent bindings; use list_bindings / inspect_function."
  in
  let open Llm_client in
  let msgs =
    ref
      [ Msg { role = System; content = seed }
      ; Msg { role = User; content = prompt }
      ]
  in
  let installs : string list ref = ref [] in
  let rec loop step =
    if step > max_steps then begin
      let summary =
        match !installs with
        | [] ->
            "agent-task: max steps exceeded (no installs recorded). " ^
            "Try a smaller task or (agent-task prompt 24)."
        | xs ->
            "agent-task: max steps exceeded after installs:\n- " ^
            String.concat "\n- " (List.rev xs) ^
            "\n(Partial progress kept on this branch.)"
      in
      on_event (Fail summary);
      Lwt.return (Pricklypear_lisp.Ast.VStr summary)
    end else
      (* Last step: no tools — force a plain-text wrap-up. *)
      let tools_now =
        if step = max_steps then [] else agent_tool_defs
      in
      let* turn =
        chat_turn ~temperature:0.2 ~max_tokens:2048
          ~tools:tools_now ~messages:!msgs ()
      in
      match turn with
      | Turn_error e ->
          on_event (Fail e);
          Lwt.return (Pricklypear_lisp.Ast.VStr ("agent-task error: " ^ e))
      | Final text ->
          on_event (Token text);
          on_event Done;
          Lwt.return (Pricklypear_lisp.Ast.VStr text)
      | Need_tools { content; tool_calls } ->
          (* Empty tool_calls burns steps instantly — stop that loop. *)
          if tool_calls = [] then begin
            let fallback =
              match content with
              | Some c when String.trim c <> "" -> c
              | _ ->
                  "agent-task: model requested tools but sent none; stopping."
            in
            on_event (Token fallback);
            on_event Done;
            Lwt.return (Pricklypear_lisp.Ast.VStr fallback)
          end else begin
            (match content with
             | Some c when String.trim c <> "" -> on_event (Token (c ^ "\n"))
             | _ -> ());
            msgs :=
              !msgs
              @ [ Assistant_tools { content; tool_calls } ];
            let* () =
              Lwt_list.iter_s
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
                  msgs :=
                    !msgs
                    @ [ Tool_result
                          { tool_call_id = tc.id; content = result }
                      ];
                  Lwt.return_unit)
                tool_calls
            in
            loop (step + 1)
          end
  in
  loop 1


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

  (* (agent seed prompt) -> VStr final text | error string.
     Unified AI portal. Effect Ai. Does not auto-define. *)
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

  (* (agent-task prompt) multi-turn tools; may install many functions. *)
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
            DB write is non-fatal on PG-down. *)
         Lwt.async (fun () ->
           Store.persist_function t.store name body_expr t.manifest.username);
         let* () =
           if name = "ui/nav" || name = "ui/home" then refresh_ui_shell t
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
       Lwt.async (fun () ->
         Store.persist_function t.store name body_expr t.manifest.username);
       let* () =
         if name = "ui/nav" || name = "ui/home" then refresh_ui_shell t
         else Lwt.return_unit
       in
       Lwt.return v
   | _ -> Lwt.return v)

and load_string (t : t) (src : string) : (string, string) result Lwt.t =
  Loader.load_string ~eval_form:(eval_form t) src

and load_library (t : t) (name : string) : (string, string) result Lwt.t =
  Loader.load_library ~eval_form:(eval_form t) name

and refresh_ui_shell (t : t) : unit Lwt.t =
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
        (fun _ ->
          t.ui_nav_html <- None;
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
    let* () = seed_one t "ui" in
    let* () = seed_one t "ai" in
    let* () = seed_one t "outline" in
    let* () = seed_one t "notes" in
    let* () = seed_one t "todo" in
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
      kernel_catalog = [];  (* filled after all prims register *)
      ui_nav_html = None;
      current_request = None;
    }
  in
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
let store (t : t) : Store.t = t.store
let branch_id (t : t) : string = t.manifest.branch_id

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
        if t.manifest.username <> "admin"
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
  Hashtbl.find_opt t.routes route

let kernel_catalog (t : t) : (string * string) list = t.kernel_catalog

let ui_nav_html (t : t) : string option = t.ui_nav_html

let set_ui_nav_html (t : t) (s : string option) : unit = t.ui_nav_html <- s


let list_pages (t : t) : (string * string) list =
  Hashtbl.fold (fun route handler acc -> (route, handler) :: acc) t.routes []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let set_route (t : t) ~(route : string) ~(handler : string) : unit Lwt.t =
  Hashtbl.replace t.routes route handler;
  let* _ = Store.register_route t.store ~route ~handler in
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
               handler_name (Printexc.to_string exn);
             Lwt.return_none)
      in
      (match request with
       | None -> run ()
       | Some req ->
           t.current_request <- Some req;
           Lwt.finalize run (fun () ->
               t.current_request <- None;
               Lwt.return_unit))
