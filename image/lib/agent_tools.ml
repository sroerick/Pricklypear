(** AI agent tools, propose/install, LLM rewrite/growth loops.
    Operates on [Session_types.t]; re-exported from [Session]. *)
open Session_types

(** Human-readable message from evaluator/reader exceptions (for agent tools
    and POST /api/eval). Delegates to [Ast.pp_interp_error] so every
    [interp_error] variant is covered — never emit opaque [Interp_error(_)]. *)
let pp_interp_exn (exn : exn) : string =
  match exn with
  | Pricklypear_lisp.Ast.Interp_error e ->
      Pricklypear_lisp.Ast.pp_interp_error e
  | Pricklypear_lisp.Reader.Reader_error (m, l, c) ->
      Printf.sprintf "parse error: %s (%d:%d)" m l c
  | _ -> Printexc.to_string exn

(** Resolve per-user LLM key (auth.user-llm-keys); env fallback inside client. *)
let with_user_llm (t : t) (f : unit -> 'a) : 'a =
  let uid = t.manifest.user_id in
  if uid = "" || uid = "anonymous" || uid = "public" then f ()
  else
    let secret = Auth_store.get_llm_key_secret t.store ~user_id:uid in
    match secret with
    | None -> f ()
    | Some (key, meta) ->
        let base =
          match meta.Auth_store.base with
          | Some b when b <> "" -> b
          | _ -> Llm_client.api_base ()
        in
        let model =
          match meta.Auth_store.model with
          | Some m when m <> "" -> m
          | _ -> Llm_client.model ()
        in
        let creds =
          { Llm_client.api_key = key; api_base = base; model }
        in
        Llm_client.with_creds creds f

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
    : (Pricklypear_lisp.Ast.value, string) result =
  let body_src = ensure_callable_body body_src in
  let input = Printf.sprintf "(define %s %s)" name body_src in
  (try
      let expr = Pricklypear_lisp.Reader.read_one input in
      let v =
        Control_rim.run ~prims:t.eval_state.prims (fun () ->
          Pricklypear_lisp.Eval.eval_string_direct
            ~prims:Pricklypear_lisp.Eval.empty_prims t.eval_state.env
            ~budget:(Pricklypear_lisp.Eval.make_budget ()) input)
      in
      (match Pricklypear_lisp.Eval.parse_top_define expr with
       | Some { name = n; body = body_expr; doc } ->
           let () =
             (* Opaque-body contract (i-1788671083-c): serialize at the
                eval boundary; the store persists raw jsonb. *)
             Store.persist_function t.store n
               (Pricklypear_lisp.Ast.expr_to_body_json body_expr)
               ~doc author
           in
           (* Stale chrome nav was the "bible only on home" bug: home
              live-calls ui/nav; other pages used a cached string. *)
           if n = "ui/nav" || n = "ui/home" || n = "ui/css" || n = "ui/theme"
              || String.starts_with ~prefix:"theme/" n then (
             t.ui_shell.nav_html <- None;
             t.ui_shell.css <- None);
           (* Impact + change transcript filled by Session after define when
              called via Session.eval paths; install_function appends text. *)
           (Ok v)
       | None ->
           (* Still bound in env if define special-form ran *)
           (Ok v))
    with exn ->
      let msg = pp_interp_exn exn in
      (Error msg))

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

(** Derive-on-read [known_fn] from branch-visible function_defs (H0.2). *)
let known_fn_from_store (t : t)
  : Pricklypear_lisp.Effect.known_fn_lookup =
  let rows = Store.list_function_names t.store in
  (* Opaque-body contract (i-1788671083-c): parse at the eval boundary;
     malformed rows skipped as the store's old decode guard did. *)
  let bindings =
    List.filter_map
      (fun (n, body, _) ->
        match Pricklypear_lisp.Ast.expr_of_body_json body with
        | Some e -> Some (n, e)
        | None -> None)
      rows
  in
  (Pricklypear_lisp.Effect.known_fn_of_bindings bindings)

let check_ai_body_effects ?(known_fn = fun _ -> None)
    ?env (body_src : string) : (unit, string) result =
  try
    let expr = Pricklypear_lisp.Reader.read_one body_src in
    (* Expand macros before infer when an env is available (BORGE-1785212882).
       Without env, fall back to unexpanded — install paths that have a
       session should pass ~env.
       If expansion FAILS we must NOT fall back to the unexpanded tree:
       a macro could hide a write-db effect behind an expansion that the
       gate would then miss. Surface the error so the install is denied. *)
    let expanded =
      match env with
      | None -> Ok expr
      | Some e ->
          (try Ok (Pricklypear_lisp.Eval.macroexpand_expr e expr)
           with
           | Pricklypear_lisp.Ast.Interp_error err ->
               Error
                 (Printf.sprintf "macroexpand failed: %s"
                    (Pricklypear_lisp.Ast.pp_interp_error err))
           | exn ->
               Error
                 (Printf.sprintf "macroexpand failed: %s"
                    (Printexc.to_string exn)))
    in
    match expanded with
    | Error msg -> Error msg
    | Ok expr ->
        let fx = Pricklypear_lisp.Effect.infer ~known_fn expr in
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
    : Pricklypear_lisp.Ast.value =
  match Llm_client.extract_lisp_sexpr body_src with
  | Error e ->
      let preview =
        let s = String.trim body_src in
        if String.length s > 240 then String.sub s 0 240 ^ "…" else s
      in
      (Pricklypear_lisp.Ast.VStr
           (Printf.sprintf "ai body invalid: %s\n---\n%s" e preview))
  | Ok body_src ->
      let known_fn = known_fn_from_store t in
      (match check_ai_body_effects ~known_fn ~env:t.eval_state.env body_src with
       | Error pe ->
           (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "ai rejected by effects: %s\n---\n%s" pe body_src))
       | Ok () ->
           let author = "agent:" ^ Llm_client.active_model () in
           let def = define_binding t ~name ~body_src ~author in
           (match def with
            | Ok _ ->
                let () = Call_graph.record_define_side_effects t ~name in
                let badge =
                  try
                    let e = Pricklypear_lisp.Reader.read_one body_src in
                    Pricklypear_lisp.Effect.EffSet.badge
                      (Pricklypear_lisp.Effect.infer ~known_fn e)
                  with _ -> "?"
                in
                let impact = Call_graph.impact_summary_text t name in
                (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "installed %s by %s [%s]\n%s\n%s"
                        name author badge body_src impact))
            | Error e ->
                (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "ai body invalid: %s\n---\n%s" e body_src))))

(** Pretty source body for a binding (env lambda or store).
    Special forms are not stored; return a short language card instead. *)
let function_source_at (t : t) ~(branch_id : string) (name : string)
  : string option =
  let found = Store.lookup_function_at t.store ~branch_id name in
  let is_current = branch_id = t.store.branch_id in
  (* Opaque-body contract (i-1788671083-c): parse at the eval boundary;
     a malformed body is treated like an absent function, as before. *)
  let found =
    Option.bind found (fun (b, a, d) ->
        Option.map (fun e -> (e, a, d)) (Pricklypear_lisp.Ast.expr_of_body_json b))
  in
  match found with
  | Some (expr, _, _) ->
      (Some (Pricklypear_lisp.Reader.pp_expr expr))
  | None ->
      (match if is_current then Pricklypear_lisp.Env.lookup t.eval_state.env name else None with
       | Some (Pricklypear_lisp.Ast.VLambda (params, body, _)) ->
           let e =
             Pricklypear_lisp.Ast.Lambda (params, body)
           in
           (Some (Pricklypear_lisp.Reader.pp_expr e))
       | _ ->
           (match Pricklypear_lisp.Special_forms.find name with
            | Some e ->
                let card =
                  Printf.sprintf
                    ";; special form (evaluator) — not a Lisp binding\n;; %s\n;; %s\n%s"
                    e.arity e.summary e.examples
                in
                (Some card)
            | None -> None))

let function_source (t : t) (name : string) : string option =
  function_source_at t ~branch_id:t.store.branch_id name

(** Raw function body as quoted data (no eval, no pretty-print).
    Store body (or env lambda) reified via [Eval.quote_to_value] so the
    result is a first-class Lisp value tree agents can walk/rewrite.
    Missing / special-form / non-lambda → None. *)
let function_ast_at (t : t) ~(branch_id : string) (name : string)
  : Pricklypear_lisp.Ast.value option =
  let found = Store.lookup_function_at t.store ~branch_id name in
  let is_current = branch_id = t.store.branch_id in
  (* Opaque-body contract (i-1788671083-c): parse at the eval boundary;
     a malformed body is treated like an absent function, as before. *)
  let found =
    Option.bind found (fun (b, a, d) ->
        Option.map (fun e -> (e, a, d)) (Pricklypear_lisp.Ast.expr_of_body_json b))
  in
  match found with
  | Some (expr, _, _) ->
      (Some (Pricklypear_lisp.Eval.quote_to_value expr))
  | None ->
      (match if is_current then Pricklypear_lisp.Env.lookup t.eval_state.env name else None with
       | Some (Pricklypear_lisp.Ast.VLambda (params, body, _)) ->
           let e = Pricklypear_lisp.Ast.Lambda (params, body) in
           (Some (Pricklypear_lisp.Eval.quote_to_value e))
       | Some (Pricklypear_lisp.Ast.VMacro (params, body, _)) ->
           (* Same shape as lambda; tagged head so agents can tell macros apart. *)
           let v =
             Pricklypear_lisp.Ast.VList
               [ Pricklypear_lisp.Ast.VStr "defmacro"
               ; Pricklypear_lisp.Ast.VList
                   (List.map (fun p -> Pricklypear_lisp.Ast.VStr p) params)
               ; Pricklypear_lisp.Eval.quote_to_value body
               ]
           in
           (Some v)
       | _ -> None)

let function_ast (t : t) (name : string)
  : Pricklypear_lisp.Ast.value option =
  function_ast_at t ~branch_id:t.store.branch_id name

(** Names in package [pkg].

    Match policy (agent discovery):
    - [pkg] empty / root → names with no [/]
    - else → exact [name = pkg] OR prefix [pkg/…] (any depth).

    Previous implementation used [Packages.package_of_name] (last [/]
    segment parent), so [(package-bindings "todo")] missed
    [todo/item/add] (parent [todo/item]). Prefix match is what
    agent.txt and explorers expect. *)
let package_binding_names (t : t) (pkg : string) : string list =
  let defs = Store.list_function_names t.store in
  let pkg =
    let p = String.trim pkg in
    if p = "(root)" || p = "root" then "" else p
  in
  (* MEMBERSHIP (i-vault-exporter, 2026-10-02): provenance FIRST.
     function_defs.package is the (package …) form's stamp, set at
     load time — it records which package a define was DECLARED in,
     regardless of the name's shape. Name-prefix matching is only a
     fallback: it is simply wrong for packages whose defines don't use
     the package name as a prefix (std defines list/…, html/…, css/…;
     each theme-* defines theme/…). Prefix-matching those produced an
     EMPTY export, which pp-sync snapshot then wrote over the vault —
     gutting libs/std (69 defines -> 0) and every theme-*. *)
  if pkg = "" then
    (* root: names with no [/] that carry no provenance either. *)
    List.filter_map
      (fun (name, _, _) ->
        if not (String.contains name '/') then Some name else None)
      defs
    |> List.sort_uniq String.compare
  else
    let by_prov = Store.package_names_by_provenance t.store pkg in
    if by_prov <> [] then by_prov
    else
      (* Fallback for unstamped/legacy rows: prefix + exact match. *)
      let prefix = pkg ^ "/" in
      let plen = String.length prefix in
      let matches name =
        name = pkg
        || (String.length name >= plen && String.sub name 0 plen = prefix)
      in
      List.filter_map (fun (name, _, _) -> if matches name then Some name else None) defs
      |> List.sort_uniq String.compare

(** Public export names for [pkg].
    Resolution: declared package_registry exports (if non-empty) →
    shallow heuristic ([pkg] or [pkg/local] with no further [/]).
    Kernel/std freeze is applied by the session prim wrapper. *)
let package_export_names (t : t) (pkg : string) : string list =
  let pkg = String.trim pkg in
  let pkg = if pkg = "(root)" || pkg = "root" then "" else pkg in
  let reg =
    if pkg = "" then None else Store.lookup_package t.store pkg
  in
  match reg with
  | Some r when r.exports <> [] -> r.exports
  | _ ->
      let names = package_binding_names t pkg in
      if pkg = "" then names
      else
        let prefix = pkg ^ "/" in
        let plen = String.length prefix in
        let shallow =
          List.filter
            (fun name ->
              name = pkg
              || (String.length name > plen
                  && String.sub name 0 plen = prefix
                  && not (String.contains_from name plen '/')))
            names
        in
        shallow

(** Unique top-level package prefixes from visible function_defs. *)
let list_package_names (t : t) : string list =
  let defs = Store.list_function_names t.store in
  let tbl = Hashtbl.create 32 in
  List.iter
    (fun (name, _, _) ->
      match String.index_opt name '/' with
      | None -> ()
      | Some i ->
          let p = String.sub name 0 i in
          if p <> "" then Hashtbl.replace tbl p ())
    defs;
  let pkgs =
    Hashtbl.fold (fun k _ acc -> k :: acc) tbl []
    |> List.sort String.compare
  in
  pkgs

let name_contains (name : string) (pat : string) : bool =
  if pat = "" then true
  else
    let n = String.length name and p = String.length pat in
    let rec loop i =
      if i + p > n then false
      else if String.sub name i p = pat then true
      else loop (i + 1)
    in
    loop 0

(** [(name, doc)] for visible defs whose name contains [pat].
    Doc falls back to empty string when unset (still listed). *)
let apropos_docs (t : t) (pat : string) : (string * string) list =
  let defs = Store.list_function_names t.store in
  let pat = String.trim pat in
  let names =
    List.filter_map
      (fun (name, _, _) ->
        if name_contains name pat then Some name else None)
      defs
    |> List.sort_uniq String.compare
  in
  let rec go acc = function
    | [] -> (List.rev acc)
    | name :: rest ->
        let doc_opt = Store.lookup_function_doc t.store name in
        let doc =
          match doc_opt with
          | Some s -> s
          | None ->
              (match Pricklypear_lisp.Special_forms.find name with
               | Some e -> e.summary
               | None ->
                   let b = Primitives.prim_summary name in
                   if b = "built-in primitive" then "" else b)
        in
        go ((name, doc) :: acc) rest
  in
  go [] names

(* ---- bumpers M5: discovery surfaces (borge/bumpers.borg
       discovery-surfaces) -------------------------------------------------

   The reference side of the feedback channel: effect badges in describe
   text (read-db vs write-db visible in every listing), a doc-coverage
   footer on package describe, a (docs-missing [pkg]) prim listing
   visible bindings without docstrings, and an (app-check pkg) per-route
   fragment/shell/not-html classification (reuses the M3 classifier;
   advisory only — route_gate stays legality-only, never about shape).
   The doc-coverage footer is what habitat-workflow.spec-health's
   "gap follows the agent around" line promises: describe surfaces it
   without waiting for a report run. Pure formatters are declared here so
   the no-DB agent_tools_test suite can pin their shape. *)

(** Pure: render a listing name with its arity + effect badge:
    "name (a b) [db-write]". [args] None or [] → no parens. *)
let annotate_name (name : string) (args : string list option)
    (badge : string) : string =
  let arg_str =
    match args with
    | Some ps when ps <> [] -> " (" ^ String.concat " " ps ^ ")"
    | _ -> ""
  in
  name ^ arg_str ^ " [" ^ badge ^ "]"

(** Pure: doc-coverage footer "documented X/Y — (docs-missing \"pkg\")",
    only when Y > X (undocumented bindings exist). *)
let doc_coverage_footer ~(documented : int) ~(total : int) ~(pkg : string)
  : string option =
  if total > documented then
    Some
      (Printf.sprintf "documented %d/%d — (docs-missing \"%s\")"
         documented total pkg)
  else None

(** Pure: one app-check report line. [shape] ∈ fragment|shell|not-html|unbound. *)
let app_check_line ~(path : string) ~(handler : string) ~(shape : string)
    ~(vis : string) : string =
  Printf.sprintf "  %-26s  %-24s  %-9s  [%s]" path handler shape vis

(** Is [name] missing a docstring? Mirrors the function-doc prim: store
    doc, else special-form summary, else prim blurb — function-doc ()
    means missing. *)
let binding_has_doc (t : t) (name : string) : bool =
  match Store.lookup_function_doc t.store name with
  | Some _ -> true
  | None ->
      Pricklypear_lisp.Special_forms.find name <> None
      || (let b = Primitives.prim_summary name in
          b <> "built-in primitive")

(** Effect badge + arity annotation for a visible binding [name]:
    "name (a b) [db-write]". Store functions are effect-inferred over
    their body with the precomputed branch [known_fn] (describe passes
    it) so user-callee capabilities are credited; prims/special forms
    resolve from the classified capability catalog. Unclassified names
    get "[?]" — never a false "pure". *)

(** Extract lambda param names from a stored body expr. Bodies persist in
    TWO shapes: the constructor [Lambda (ps, _)] (interactive define-sugar
    via parse_top_define) and the reader form
    [App (Var "lambda", params :: _)] which is what the boot seed and
    load-library persist — there [params] is the flat list encoding
    (first param as the App head, remaining params as the args array;
    [Lit VNil] for ()). None when the body isn't a lambda. *)
let lambda_params (e : Pricklypear_lisp.Ast.expr) : string list option =
  match e with
  | Pricklypear_lisp.Ast.Lambda (ps, _) -> Some ps
  | Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var "lambda", params_expr :: _)
    ->
      (match params_expr with
       | Pricklypear_lisp.Ast.Lit Pricklypear_lisp.Ast.VNil -> Some []
       | Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var first, rest) ->
           Some
             (first
             :: List.filter_map
                  (function
                    | Pricklypear_lisp.Ast.Var s -> Some s
                    | _ -> None)
                  rest)
       | _ -> None)
  | _ -> None

let binding_annot ?(known_fn : Pricklypear_lisp.Effect.known_fn_lookup =
  fun _ -> None) (t : t) (name : string) : string =  match Store.lookup_function t.store name with
  | Some (body, _, _) ->
      (match Pricklypear_lisp.Ast.expr_of_body_json body with
       | Some e ->
           let args = lambda_params e in
           let badge =
             Pricklypear_lisp.Effect.EffSet.badge
               (Pricklypear_lisp.Effect.infer ~known_fn e)
           in
           annotate_name name args badge
       | None -> name ^ " [?]")
  | None ->
      (* not stored: special form / prim / unknown *)
      let is_known =
        Pricklypear_lisp.Special_forms.find name <> None
        || (Primitives.prim_summary name <> "built-in primitive")
      in
      if is_known then
        annotate_name name None
          (Pricklypear_lisp.Effect.EffSet.badge
             (Pricklypear_lisp.Effect.default_prim_lookup name))
      else name ^ " [?]"

(** branching.branch-resolved-reads SCOPE grammar. Every read prim DECLARES
    the visibility of its answer, because three readers silently differing
    on "does marcus have data?" burned operator time. The closed vocabulary
    is `chain` (branch-chain-visible), `garden` (own-branch/garden-scoped
    with a prefix escape), `store` (whole-store under the admin RLS
    disjunct). Stored Lisp defs default to `chain` (they cross the branch
    chain); unknown prims get no line. A new reader may not introduce a
    fourth kind without a line in the spec. *)
let read_scope (t : t) (name : string) : string option =
  match name with
  | "find-code" | "function-source" | "function-ast" | "code-history"
  | "code-diff" | "describe" | "branch-changes" -> Some "chain"
  | "find-text" | "tree/find-by" -> Some "garden"
  | "find-rows-author" -> Some "store"
  | "ops/activity" -> Some "store"
  | "ops/presence" -> Some "store"
  | "ops/knock" -> Some "store"
  | _ ->
      (match Store.lookup_function t.store name with
       | Some _ -> Some "chain"
       | None -> None)

(** Visible bindings without docstrings (bumpers M5; function-doc () =
    missing). [pkg] None → every visible binding; Some p → package
    prefix filter (same match as package-bindings). *)
let docs_missing (t : t) ?(pkg : string option) () : string list =
  let names =
    match pkg with
    | None ->
        Store.list_function_names t.store
        |> List.map (fun (n, _, _) -> n) |> List.sort_uniq String.compare
    | Some p -> package_binding_names t p
  in
  List.filter (fun n -> not (binding_has_doc t n)) names

(** (app-check "pkg"): per-route fragment/shell/not-html classification
    report for every route whose handler lives in package [pkg] (reuses
    the M3 classifier + the gate's own handler-body walk). Advisory —
    route_gate stays legality-only. Empty pkg → all routes. *)
let app_check (t : t) ?(pkg : string option) () : string =
  let pkg = Option.value pkg ~default:"" in
  let label = if pkg = "" then "(root)" else pkg in
  let match_handler handler =
    pkg = "" || handler = pkg
    || (String.length handler > String.length pkg
        && String.sub handler 0 (String.length pkg + 1) = pkg ^ "/")
  in
  let routes =
    Store.list_routes t.store
    |> List.filter (fun (_, handler, _) -> match_handler handler)
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  if routes = [] then
    Printf.sprintf
      "app-check %s: no routes whose handler is in this package" label
  else
    let lines =
      List.map
        (fun (path, handler, vis) ->
          let shape =
            match
              Pricklypear_lisp.Route_gate.handler_body t.eval_state.env handler
            with
            | None -> "unbound"
            | Some body ->
                Pricklypear_lisp.Effect.route_shape_string
                  (Pricklypear_lisp.Effect.classify_route_shape body)
          in
          app_check_line ~path ~handler ~shape ~vis)
        routes
    in
    Printf.sprintf "app-check %s (%d routes)\n%s"
      label (List.length routes) (String.concat "\n" lines)

(** Package or binding description for selective agent reveal. *)
let describe (t : t) (needle : string) : string =
  let needle = String.trim needle in
  if needle = "" then
    "describe: need name or package string"
  else
    let known_fn = known_fn_from_store t in
    let pkgs = list_package_names t in
    let pkg_names = package_binding_names t needle in
    let is_pkg =
      List.mem needle pkgs || List.length pkg_names > 0
    in
    (* Prefer exact binding describe when name is a concrete def and not
       only a package prefix with many children — if needle equals a
       binding name, show that binding. *)
    let exact_doc = Store.lookup_function_doc t.store needle in
    let exact_src = function_source t needle in
    let has_exact = match exact_src with Some _ -> true | None -> false in
    if has_exact && not (List.length pkg_names > 1 && List.mem needle pkgs)
    then
      let doc =
        match exact_doc with
        | Some s -> s
        | None ->
            (match Pricklypear_lisp.Special_forms.find needle with
             | Some e -> e.summary
             | None ->
                 let b = Primitives.prim_summary needle in
                 if b = "built-in primitive" then "(no docstring)" else b)
      in
      let scope_line =
        match read_scope t needle with
        | Some s -> Printf.sprintf "\n  scope: %s" s
        | None -> ""
      in
      (Printf.sprintf
           "%s\n  doc: %s%s\n  next: (function-source %S) (function-ast %S)"
           (binding_annot ~known_fn t needle) doc scope_line needle needle)
    else if read_scope t needle <> None then
      (* A known read prim (not stored, not a special form): it has no body
         to describe, but the branch-resolved-reads SCOPE grammar still
         requires a line. Emit the exact-binding shape with the prim
         summary instead of a fuzzy apropos list. *)
      let summary = Primitives.prim_summary needle in
      let doc =
        if summary = "built-in primitive" then "(no docstring)" else summary
      in
      (Printf.sprintf "%s\n  doc: %s\n  scope: %s"
         (binding_annot ~known_fn t needle) doc
         (match read_scope t needle with Some s -> s | None -> "chain"))
    else if is_pkg then
      let exports = package_export_names t needle in
      let lines =
        List.map
          (fun name ->
            let n = binding_annot ~known_fn t name in
            let d = Store.lookup_function_doc t.store name in
            match d with
            | Some s when s <> "" ->
                Printf.sprintf "  - %s — %s" n s
            | _ -> Printf.sprintf "  - %s" n)
          exports
      in
      let total = List.length pkg_names in
      let documented =
        List.length
          (List.filter (fun n -> binding_has_doc t n) pkg_names)
      in
      let footer =
        match doc_coverage_footer ~documented ~total ~pkg:needle with
        | Some f -> "\n" ^ f
        | None -> ""
      in
      let body =
        Printf.sprintf
          "package %s (%d bindings, %d shallow exports)\n\
           shallow exports (package-exports):\n%s\n\
           full list: (package-bindings %S)\n\
           search docs: (apropos-docs %S)\n\
           one binding: (describe \"name\") or (function-doc \"name\")"
          needle (List.length pkg_names) (List.length exports)
          (if lines = [] then "  (none)" else String.concat "\n" lines)
          needle (needle ^ "/")
      in
      body ^ footer
    else
      (* Fallback: treat as apropos hit list *)
      let pairs = apropos_docs t needle in
      if pairs = [] then
        (Printf.sprintf
             "%s: not a binding or package. Try (apropos %S) or (list-packages)."
             needle needle)
      else
        let lines =
          List.map
            (fun (n, d) ->
              let an = binding_annot ~known_fn t n in
              if d = "" then Printf.sprintf "  - %s" an
              else Printf.sprintf "  - %s — %s" an d)
            (let rec take n xs =
               match n, xs with
               | _, [] -> []
               | 0, _ -> []
               | n, x :: xs -> x :: take (n - 1) xs
             in
             take 30 pairs)
        in
        (Printf.sprintf
             "apropos %S (%d matches, showing up to 30):\n%s\n\
              refine: (apropos-docs %S) (describe \"pkg\")"
             needle (List.length pairs) (String.concat "\n" lines) needle)

(** Export package as lib.pp text: (define name body)… pretty-printed. *)
let export_package (t : t) (pkg : string) : string =
  let names = package_binding_names t pkg in
  let label = if pkg = "" || pkg = "(root)" then "(root)" else pkg in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (Printf.sprintf
       ";; package %s -- exported from live branch %s\n;; Sync: scripts/pp_sync.py pull|push\n\n"
       label t.manifest.branch_name);
  let reg =
    if pkg = "" || pkg = "(root)" then None
    else Store.lookup_package t.store pkg
  in
  (match reg with
   | Some r when r.exports <> [] ->
       Buffer.add_string buf (Printf.sprintf "(package %s\n  (export\n" pkg);
       List.iter
         (fun n -> Buffer.add_string buf (Printf.sprintf "    %s\n" n))
         r.exports;
       Buffer.add_string buf "))\n\n"
   | _ -> ());
  let rec go = function
    | [] -> (Buffer.contents buf)
    | name :: rest ->
        let src = function_source t name in
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
             (* Re-inject the define-time docstring as a trailing
                define-doc form so the vault round-trip preserves the
                doc column. parse_top_define's 3-arg (trailing-doc)
                case reads it back into the doc column on reseed; trailing
                wins over any lambda-internal doc, and the trailing string
                is treated as metadata (not evaluated). (i-1787318456) *)
             (match Store.lookup_function_doc t.store name with
              | Some doc ->
                  Buffer.add_string buf "\n  ";
                  Buffer.add_string buf
                    (Pricklypear_lisp.Reader.pp_value
                       (Pricklypear_lisp.Ast.VStr doc))
              | None -> ());
             Buffer.add_string buf ")\n\n";
             go rest)
  in
  go names

(* AI propose: draft via agent portal, then install on current branch. *)
let propose_function (t : t) ~(name : string) ~(spec : string)
    : Pricklypear_lisp.Ast.value =
  with_user_llm t (fun () ->
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
  let res =
    Llm_client.agent_chat ~temperature:0.1 ~max_tokens:512
      ~seed ~prompt ()
  in
  match res with
  | Error e ->
      (Pricklypear_lisp.Ast.VStr ("agent error: " ^ e))
  | Ok body_src ->
      install_ai_function t ~name ~body_src)

(** Pretty-print a stored function body, or default stub. *)
let current_fn_pretty (t : t) (name : string) : string =
  let found = Store.lookup_function t.store name in
  (* Opaque-body contract (i-1788671083-c): parse at the eval boundary;
     a malformed body is treated like an absent function, as before. *)
  let found =
    Option.bind found (fun (b, a, d) ->
        Option.map (fun e -> (e, a, d)) (Pricklypear_lisp.Ast.expr_of_body_json b))
  in
  match found with
  | Some (expr, _, _) ->
      (Pricklypear_lisp.Reader.pp_expr expr)
  | None ->
      (match Pricklypear_lisp.Env.lookup t.eval_state.env name with
       | Some (Pricklypear_lisp.Ast.VLambda (params, body, _)) ->
           let p =
             "(lambda (" ^ String.concat " " params ^ ") " ^
             Pricklypear_lisp.Reader.pp_expr body ^ ")"
           in
           p
       | _ -> "(lambda () #f)")

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
        "Install a Lisp function on the current branch. body must be a lambda s-expression only, e.g. (lambda (x) (* x 2)) or (lambda () (html/page ...)). Install only parses+binds the lambda — it does NOT run the body. After install: register_route (if page), then smoke_route and/or eval_expr until ok:true. For chrome CSS (ui/css, ui/theme, layout/theme) prove with eval_expr (string-contains? (ui/css) \"...\"). Do not put (route ...) in the body — use register_route. Author becomes agent:<model>."
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
        "Evaluate a short Lisp expression in this image (same env as the branch). Use to prove non-route work actually runs: (count (contacts/all)), (type-of ui/css), (string-contains? (ui/css) \"#2d5a27\"), ((pkg/helper)). Install alone is not enough. Returns printed value or error. Prefer small expressions; no multi-form progn dumps."
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
        "REQUIRED proof that a page runs. Call a registered user route handler in-process (no real HTTP). method GET|POST; optional form/query params as a JSON object of strings; optional contains substring that must appear in the HTML/text result (or redirect Location). Always smoke after install_function+register_route of a page handler. Example: smoke_route path=/contacts method=GET contains=Contacts. On failure: html_helpers, inspect_function, fix install_function, re-smoke until ok:true. Do not claim success without a successful smoke_route for that path."
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
  "Also: inspect_function html/form | html/a | contacts/form-html | todo/ui/add-form\n"

let truncate_for_tool ~(limit : int) (s : string) : string =
  if String.length s <= limit then s
  else String.sub s 0 limit ^ "…(truncated)"

let run_smoke_route
    (t : t)
    ~(path : string)
    ~(meth : http_method)
    ~(params : (string * string) list)
    ~(contains : string option) : string =
  let path = String.trim path in
  let meth_s = match meth with `GET -> "GET" | `POST -> "POST" in
  match Hashtbl.find_opt t.routes path with
  | None ->
      (Printf.sprintf
           "ok: false\nerror: no route registered for %s\n(hint: register_route path=%s handler=...)"
           path path)
  | Some (handler, _vis) ->
      (match Pricklypear_lisp.Env.lookup t.eval_state.env handler with
       | None ->
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
           let v_res =
             (Fun.protect ~finally:(fun () -> t.current_request <- None) (fun () -> (try
      let v =
                         Control_rim.run ~prims:t.eval_state.prims (fun () ->
                             Pricklypear_lisp.Eval.eval_string_direct
                               ~prims:Pricklypear_lisp.Eval.empty_prims t.eval_state.env
                               ~budget:(Pricklypear_lisp.Eval.make_budget ())
                               ("(" ^ handler ^ ")"))
                       in
                       (`Ok v)
    with exn ->
      (`Err ("handler error: " ^ pp_interp_exn exn)))))
           in
           (match v_res with
            | `Err msg ->
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
                (String.concat "\n" lines ^ "\n--- snippet ---\n" ^ snippet)))

let run_agent_tool (t : t) ~(name : string) ~(arguments : string)
    : string =
  let args_json =
    try Yojson.Safe.from_string arguments with _ -> `Assoc []
  in
  match name with
  | "list_bindings" ->
      let names =
        try
          match Primitives.prim_list_bindings_fn ~env:t.eval_state.env [] with
          | Pricklypear_lisp.Ast.VList xs ->
              List.filter_map
                (function Pricklypear_lisp.Ast.VStr s -> Some s | _ -> None) xs
              |> List.sort String.compare
          | _ -> []
        with _ -> []
      in
      (if names = [] then "(no bindings)" else String.concat "\n" names)
  | "list_routes" ->
      let rs =
        Hashtbl.fold
          (fun route (handler, vis) acc ->
            (Printf.sprintf "%s -> %s [%s]" route handler vis) :: acc)
          t.routes []
        |> List.sort String.compare
      in
      (if rs = [] then "(no routes)" else String.concat "\n" rs)
  | "inspect_function" ->
      (match json_string_field args_json "name" with
       | None -> "error: need name"
       | Some n ->
           let pretty = current_fn_pretty t n in
           pretty)
  | "install_function" ->
      (match json_string_field args_json "name", json_string_field args_json "body" with
       | Some n, Some body ->
           let v = install_ai_function t ~name:n ~body_src:body in
           (pp_value_short v)
       | _ -> "error: need name and body")
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
             "error: path must start with /"
           else if bad_prefix then
             ("error: path reserved: " ^ path)
           else if handler = "" then
             "error: need handler function name"
           else begin
             let vis =
               match json_string_field args_json "visibility" with
               | Some v -> Store.normalize_visibility v
               | None -> "private"
             in
             (* Phase 3: same public-route effect gate as (route … 'public). *)
             let gate =
               if vis = "public" then begin
                 (* Opaque-body contract (i-1788671083-c): parse at the
                    eval boundary; malformed rows skipped as before. *)
                 let rows =
                   List.filter_map
                     (fun (n, body, a) ->
                       match Pricklypear_lisp.Ast.expr_of_body_json body with
                       | Some e -> Some (n, e, a)
                       | None -> None)
                     (Store.list_function_names t.store)
                 in
                 let known_fn =
                   Pricklypear_lisp.Effect.known_fn_of_bindings
                     (List.map (fun (n, e, _) -> (n, e)) rows)
                 in
                 let public_allow =
                   Pricklypear_lisp.Effect.public_route_allow ()
                 in
                 match List.find_opt (fun (n, _, _) -> n = handler) rows with
                 | None -> (Ok ())
                 | Some (_, body, _) ->
                     let fx =
                       Pricklypear_lisp.Effect.infer ~known_fn body
                     in
                     if Pricklypear_lisp.Effect.policy_allows
                          ~allow:public_allow fx
                     then (Ok ())
                     else
                       (Error
                            (Printf.sprintf
                               "public route denied: handler %S effects %s (allow %s)"
                               handler
                               (Pricklypear_lisp.Effect.EffSet.pp fx)
                               (Pricklypear_lisp.Effect.public_route_allow_pp ())))
               end else (Ok ())
             in
             match gate with
             | Error e -> ("error: " ^ e)
             | Ok () ->
                 Hashtbl.replace t.routes path (handler, vis);
                 let _ =
                   Store.register_route t.store ~route:path ~handler
                     ~visibility:vis ()
                 in
                 (Printf.sprintf "registered route %s -> %s (%s)"
                      path handler vis)
           end
       | _ -> "error: need path and handler")
  | "eval_expr" ->
      (match json_string_field args_json "expr" with
       | None -> "error: need expr"
       | Some expr ->
           let expr = String.trim expr in
           if expr = "" then "error: empty expr"
           else if String.length expr > 4000 then
             "error: expr too long (max 4000)"
           else begin
             (* Phase 3 agent mask: deny Branch/Publish/admin-ish prims in tool eval. *)
             let agent_eval_allow =
               Pricklypear_lisp.Effect.EffSet.normalize
                 [ Pricklypear_lisp.Effect.ReadDb
                 ; Pricklypear_lisp.Effect.WriteDb
                 ; Pricklypear_lisp.Effect.Html
                 ; Pricklypear_lisp.Effect.Txn
                 ; Pricklypear_lisp.Effect.Ai
                 ]
             in
             let denied =
               try
                 let e = Pricklypear_lisp.Reader.read_one expr in
                 let fx = Pricklypear_lisp.Effect.infer e in
                 if Pricklypear_lisp.Effect.policy_allows
                      ~allow:agent_eval_allow fx
                 then None
                 else Some (Pricklypear_lisp.Effect.EffSet.pp fx)
               with _ -> None
             in
             match denied with
             | Some fx ->
                 ("ok: false\nerror: agent effect mask denied (" ^ fx
                    ^ "; no branch/publish)")
             | None ->
                 (try
      let v =
                        Control_rim.run ~prims:t.eval_state.prims (fun () ->
                            Pricklypear_lisp.Eval.eval_string_direct
                              ~prims:Pricklypear_lisp.Eval.empty_prims t.eval_state.env
                              ~budget:(Pricklypear_lisp.Eval.make_budget ()) expr)
                      in
                      let s = Pricklypear_lisp.Reader.pp_value v in
                      ("ok: true\nvalue: "
                         ^ truncate_for_tool ~limit:1500 s)
    with exn ->
      ("ok: false\nerror: " ^ pp_interp_exn exn))
           end)
  | "smoke_route" ->
      (match json_string_field args_json "path" with
       | None -> "error: need path"
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
      html_helpers_cheatsheet
  | "image_context" ->
      let names =
        try
          match Primitives.prim_list_bindings_fn ~env:t.eval_state.env [] with
          | Pricklypear_lisp.Ast.VList xs ->
              List.filter_map
                (function Pricklypear_lisp.Ast.VStr s -> Some s | _ -> None)
                xs
          | _ -> []
        with _ -> []
      in
      let nshow = List.length names in
      let sample = String.concat ", " (List.filteri (fun i _ -> i < 40) names) in
      (Printf.sprintf
           "user=%s\nbranch=%s\nbinding_count=%d\nbindings_sample=%s"
           t.manifest.username t.manifest.branch_name nshow sample)
  | other ->
      ("error: unknown tool " ^ other)

(** One durable tool-capable loop over an ai_sessions row.
    Loads history from Postgres, resubmits full messages[] each turn,
    appends user/assistant/tool rows as it goes. *)
let run_loop
    (t : t)
    ~(sess : Ai_store.session)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    () : string =
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
      summary
    end else
      let history = Ai_store.list_messages t.store ~session_id:sess.id in
      let msgs = Ai_store.messages_for_api sess history in
      (* Last step: no tools — force a plain-text wrap-up. *)
      let tools_now =
        if (not sess.tools) || step = max_steps then [] else agent_tool_defs
      in
      let turn =
        chat_turn ~temperature:0.2 ~max_tokens:2048
          ~tools:tools_now ~messages:msgs ()
      in
      match turn with
      | Turn_error e ->
          on_event (Fail e);
          ("agent error: " ^ e)
      | Final text ->
          let r =
            Ai_store.append_messages t.store ~session_id:sess.id
              [ Msg { role = Assistant; content = text } ]
          in
          (match r with
           | Error e ->
               on_event (Fail e);
               ("agent error: " ^ e)
           | Ok () ->
               on_event (Token text);
               on_event Done;
               text)
      | Need_tools { content; tool_calls } ->
          if tool_calls = [] then begin
            let fallback =
              match content with
              | Some c when String.trim c <> "" -> c
              | _ ->
                  "agent: model requested tools but sent none; stopping."
            in
            let _ =
              Ai_store.append_messages t.store ~session_id:sess.id
                [ Msg { role = Assistant; content = fallback } ]
            in
            on_event (Token fallback);
            on_event Done;
            fallback
          end else begin
            (match content with
             | Some c when String.trim c <> "" -> on_event (Token (c ^ "\n"))
             | _ -> ());
            let r1 =
              Ai_store.append_messages t.store ~session_id:sess.id
                [ Assistant_tools { content; tool_calls } ]
            in
            match r1 with
            | Error e ->
                on_event (Fail e);
                ("agent error: " ^ e)
            | Ok () ->
                let tool_rows =
                  List.map
                    (fun (tc : tool_call) ->
                      on_event (Token (Printf.sprintf "[tool %s]\n" tc.name));
                      let result =
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
                      (Tool_result
                           { tool_call_id = tc.id; content = result }))
                    tool_calls
                in
                let r2 =
                  Ai_store.append_messages t.store ~session_id:sess.id tool_rows
                in
                (match r2 with
                 | Error e ->
                     on_event (Fail e);
                     ("agent error: " ^ e)
                 | Ok () -> loop (step + 1))
          end
  in
  (Fun.protect ~finally:(fun () -> t.agent_branch_lock <- false) (fun () -> loop 1))

let default_agent_seed (t : t) : string =
  "You are the agent for a running Pricklypear multi-user Lisp OS image " ^
  "(software, not a cactus).\n" ^
  "I will stand at my guard post and station myself on the ramparts. I will " ^
  "watch to see what He will say to me, and how I should answer when " ^
  "corrected (Hab 2:1).\n" ^
  Printf.sprintf
    "LOCKED BRANCH: %s — you see only the effective bindings on this branch "
    t.manifest.branch_name ^
  "(personal overrides shadow main; one name = one function).\n" ^
  "All install_function writes go to THIS branch only. " ^
  "Do NOT attempt checkout, fork, or merge.\n" ^
  "Tools: image_context, list_bindings, list_routes, inspect_function, " ^
  "install_function, register_route, smoke_route, eval_expr, html_helpers.\n" ^
  "\n" ^
  "CRITICAL — no compile step; install is not proof:\n" ^
  "- install_function only parses + binds a lambda. It does NOT run the body.\n" ^
  "- A green install (\"installed X [pure]\") is NEVER enough to claim success.\n" ^
  "- You MUST prove work runs with smoke_route (pages) and/or eval_expr " ^
  "(helpers, CSS, data) before finishing. If you cannot show ok:true, say so.\n" ^
  "\n" ^
  "Code-as-data / symbols (Nopales):\n" ^
  "- Bare names in source are symbols at runtime when quoted or from " ^
  "(read-string)/(function-ast). Strings use double quotes.\n" ^
  "- (quote lambda) and \"lambda\" are DIFFERENT types. " ^
  "(symbol? 'lambda) => #t, (string? \"lambda\") => #t.\n" ^
  "- Walk ASTs with (symbol=? (car form) 'lambda), NOT string-eq on car.\n" ^
  "  OK: (string-eq (symbol->string (car form)) \"lambda\") if you must.\n" ^
  "- (function-ast \"pkg/fn\") returns a list tree with symbolic heads; " ^
  "(function-source …) is pretty text only.\n" ^
  "- (read-string \"(lambda (x) x)\") parses LLM text to the same tree shape.\n" ^
  "- Prefer package-exports / package-bindings over inventing names.\n" ^
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
  "   Public routes: handler effects must be ⊆ pure|read-db|html|txn " ^
  "(no branch/ai/publish). Write-db on public needs PP_PUBLIC_ALLOW_WRITE=1 " ^
  "and prefer (with-public-write …) for the write body.\n" ^
  "7) Inspect working apps first: inspect_function todo/page, notes/page, " ^
  "todo/item/add, html/form.\n" ^
  "\n" ^
  "Definition of done (required — do not skip):\n" ^
  "8) Pages: after install_function + register_route, smoke_route path=/app " ^
  "method=GET contains=<visible title or unique marker>. Must return ok:true.\n" ^
  "9) Data / helpers: eval_expr e.g. (count (pkg/all)) or ((pkg/helper arg)) " ^
  "and confirm the value. POST via smoke_route params then GET again.\n" ^
  "10) CSS / chrome: product chrome uses (ui/css), NOT only layout/theme. " ^
  "If the task is theming or global style, install into ui/css and/or " ^
  "ui/theme (or compose layout/theme into ui/css). Prove with:\n" ^
  "    eval_expr (string-contains? (ui/css) \"<unique token>\")\n" ^
  "    and smoke_route path=/ method=GET contains=<token or class>.\n" ^
  "    Do NOT only edit layout/theme and claim the site is themed.\n" ^
  "11) If smoke/eval fails: html_helpers, inspect_function, fix " ^
  "install_function, re-smoke/re-eval until ok:true. Never finish on a " ^
  "failed smoke with a confident success message.\n" ^
  "11b) On unbound_var: read error name field; (stub-function name) or " ^
  "install_function a real body; (callers name) for who wanted it; " ^
  "then smoke used-by-routes / impact text from install result.\n" ^
  "11c) After install_function, read impact: dependents + smoke routes " ^
  "line; smoke_route each listed path when present.\n" ^
  "12) Final message must name the smoke_route / eval_expr proofs you ran " ^
  "(path + contains, or expression + result). No proofs → not done.\n" ^
  "\n" ^
  "HTML: call html_helpers before install_function of *page/*form/*list* bodies.\n" ^
  "Tool discipline:\n" ^
  "- Use ONLY the structured tool API. Never write <tool_call> XML in text.\n" ^
  "- install_function body = one lambda s-expression only, no prose/markdown.\n" ^
  "- Batch multiple tool calls in one turn when possible.\n" ^
  "- Prefer package/name names (contacts/page).\n" ^
  "- When finished, SHORT text summary listing proofs, and NO tool calls.\n" ^
  "- Do not invent bindings; use list_bindings / inspect_function."

let agent_session_new_res
    (t : t)
    ~(seed : string)
    ?(tools = true)
    ?title
    () : (string, string) result =
  let r =
    Ai_store.create_session t.store
      ~owner:t.manifest.username
      ~seed
      ?title
      ~branch_ref:t.manifest.branch_name
      ~tools
      ~model:(Llm_client.model ())
      ()
  in
  match r with Ok s -> Ok s.id | Error e -> Error e

(** Prim/value-surface wrapper (i-1788670909): Lisp callers keep the
    VStr id | VStr "agent-session-new error: ..." contract; OCaml
    callers use [agent_session_new_res] instead of sniffing the prefix. *)
let agent_session_new
    (t : t)
    ~(seed : string)
    ?(tools = true)
    ?title
    () : Pricklypear_lisp.Ast.value =
  match agent_session_new_res t ~seed ~tools ?title () with
  | Ok id -> (Pricklypear_lisp.Ast.VStr id)
  | Error e -> (Pricklypear_lisp.Ast.VStr ("agent-session-new error: " ^ e))

let agent_say
    (t : t)
    ~(session_id : string)
    ~(prompt : string)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    () : Pricklypear_lisp.Ast.value =
  let sess_opt =
    Ai_store.get_session t.store
      ~owner:t.manifest.username
      ~id:session_id
  in
  match sess_opt with
  | None ->
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
      let () =
        Ai_store.touch_session t.store ~id:sess.id ?title
          ~branch_ref:t.manifest.branch_name ()
      in
      let ar =
        Ai_store.append_messages t.store ~session_id:sess.id
          [ Llm_client.Msg { role = Llm_client.User; content = prompt } ]
      in
      match ar with
      | Error e ->
          (Pricklypear_lisp.Ast.VStr ("agent-say error: " ^ e))
      | Ok () ->
          let text =
            run_loop t ~sess ~max_steps ~on_event ()
          in
          (Pricklypear_lisp.Ast.VStr text)

let agent_history (t : t) ~(session_id : string)
    : Pricklypear_lisp.Ast.value =
  let sess_opt =
    Ai_store.get_session t.store
      ~owner:t.manifest.username
      ~id:session_id
  in
  match sess_opt with
  | None ->
      (Pricklypear_lisp.Ast.VStr
           ("agent-history: no session " ^ session_id))
  | Some _ ->
      let pairs =
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
      (Pricklypear_lisp.Ast.VList rows)

let agent_sessions (t : t) ?(limit = 50) ()
    : Pricklypear_lisp.Ast.value =
  let xs =
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
  (Pricklypear_lisp.Ast.VList rows)

let agent_session_delete (t : t) ~(session_id : string)
    : Pricklypear_lisp.Ast.value =
  let ok =
    Ai_store.delete_session t.store
      ~owner:t.manifest.username
      ~id:session_id
  in
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
    () : Pricklypear_lisp.Ast.value =
  let _ = stream in
  let title_opt =
    let p = String.trim prompt in
    if p = "" then None
    else Some (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
  in
  let created =
    agent_session_new_res t ~seed ~tools:true ?title:title_opt ()
  in
  match created with
  | Ok sid -> agent_say t ~session_id:sid ~prompt ~on_event ()
  | Error e ->
      (Pricklypear_lisp.Ast.VStr ("agent-session-new error: " ^ e))

(** Temporary alias: new tools session + one say. *)
let agent_task
    (t : t)
    ?(max_steps = 16)
    ?(on_event : Llm_client.agent_event -> unit = fun _ -> ())
    ~(prompt : string)
    () : Pricklypear_lisp.Ast.value =
  let seed = default_agent_seed t in
  let title_opt =
    let p = String.trim prompt in
    if p = "" then None
    else Some (if String.length p > 80 then String.sub p 0 80 ^ "…" else p)
  in
  let created =
    agent_session_new_res t ~seed ~tools:true ?title:title_opt ()
  in
  match created with
  | Ok sid ->
      agent_say t ~session_id:sid ~prompt ~max_steps ~on_event ()
  | Error e ->
      (Pricklypear_lisp.Ast.VStr ("agent-session-new error: " ^ e))


let llm_rewrite (t : t) ~(mode : string) ~(name : string) ~(concept : string)
    : Pricklypear_lisp.Ast.value =
  with_user_llm t (fun () ->
  let current = current_fn_pretty t name in
  let res =
    Llm_client.rewrite_component_source ~mode ~name ~concept
      ~current_pretty:current ()
  in
  match res with
  | Error e -> (Pricklypear_lisp.Ast.VStr ("ai error: " ^ e))
  | Ok body_src ->
      let known_fn = known_fn_from_store t in
      (match check_ai_body_effects ~known_fn ~env:t.eval_state.env body_src with
       | Error pe ->
           (Pricklypear_lisp.Ast.VStr
                (Printf.sprintf "ai rejected by effects: %s\n---\n%s" pe body_src))
       | Ok () ->
           let author = "agent:" ^ Llm_client.active_model () in
           let def = define_binding t ~name ~body_src ~author in
           (match def with
            | Ok _ ->
                (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "froze %s (%s) by %s\n%s"
                        name mode author body_src))
            | Error e ->
                (Pricklypear_lisp.Ast.VStr
                     (Printf.sprintf "freeze failed: %s\n---\n%s" e body_src)))))

let llm_inner t ~name ~concept =
  llm_rewrite t ~mode:"replace" ~name ~concept

let llm_outer t ~name ~concept =
  llm_rewrite t ~mode:"wrap" ~name ~concept

(** Growth-from-seed: smallest core then recursive outer wraps. *)
let llm_seed (t : t) ~(requirement : string) ?(max_steps = 5) ()
    : Pricklypear_lisp.Ast.value =
  with_user_llm t (fun () ->
  let tip = "seed" in
  let seed_res = Llm_client.seed_core_source ~requirement () in
  match seed_res with
  | Error e -> (Pricklypear_lisp.Ast.VStr ("seed error: " ^ e))
  | Ok body_src ->
      let known_fn = known_fn_from_store t in
      (match check_ai_body_effects ~known_fn ~env:t.eval_state.env body_src with
       | Error pe ->
           (Pricklypear_lisp.Ast.VStr ("seed rejected: " ^ pe ^ "\n" ^ body_src))
       | Ok () ->
           let author = "agent:" ^ Llm_client.active_model () in
           let def = define_binding t ~name:tip ~body_src ~author in
           (match def with
            | Error e ->
                raise (Pricklypear_lisp.Ast.Interp_error
                         (Pricklypear_lisp.Ast.Prim_error
                            ("seed", "freeze failed: " ^ e, "failed")))
            | Ok _ ->
                let log = Buffer.create 256 in
                Buffer.add_string log (Printf.sprintf "step0 seed %s\n%s\n" tip body_src);
                let rec grow step =
                  if step > max_steps then
                    (Pricklypear_lisp.Ast.VStr
                         (Buffer.contents log ^ "stopped: max-steps\n"))
                  else
                    let pretty = current_fn_pretty t tip in
                    let judge =
                      Llm_client.growth_judge ~requirement ~current_pretty:pretty
                        ~step ()
                    in
                    match judge with
                    | Error e ->
                        (Pricklypear_lisp.Ast.VStr
                             (Buffer.contents log ^ "judge error: " ^ e))
                    | Ok line ->
                        let upper = String.uppercase_ascii (String.trim line) in
                        if String.length upper >= 4 && String.sub upper 0 4 = "DONE"
                        then
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
                          let wr =
                            llm_outer t ~name:tip ~concept
                          in
                          Buffer.add_string log
                            (Pricklypear_lisp.Reader.pp_value wr ^ "\n");
                          grow (step + 1)
                in
                grow 1)))

(* Register the 5 CRUD primitives (get/create/update/delete/list) over
   `data_rows`. These are NORMAL prims: prim_fn = value list -> value.
   They return their real DB result — e.g. (get 'user 42) returns the
   row's JSON fields as a VStr; `(mapcar (lambda (id) (get 'user id)) ids)`
   works.

   Per [one-tree.branching-interaction] /data DECISION: rows are
   USER-SCOPED. Creates land under the session user (author column +
   data.u.<user>.* tree path). Reads see only the caller's own rows
   plus shared seed types (bible-*, strongs-entry). Not branch-versioned. *)
