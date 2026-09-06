(** Pricklypear Image — restricted read-time eval for book example
    blocks (habitat-book.eval-examples).

    `(example-eval src)` parses + evals a Nopales source string so a
    READER can run a book's example blocks in their own session:

    - RESTRICTED PRIM TABLE. The example runs under its own Control
      rim whose table is a NAME FILTER of the caller's table: whitelist
      = Effect.pure_prims + Effect.read_db_prims + a reviewed
      read-only supplement ([read_extra_prims]). Everything else is
      ABSENT, and Control.perform_prim raises "unregistered primitive"
      on absent names — fail-closed, including through function bodies
      and macro expansions (they eval under the same nested rim).
    - STATIC GATE. Before eval, each parsed form must (a) contain no
      mutation-family special form ([forbidden_special]) and (b) infer
      an effect set within {pure, read-db, html} via Effect.infer with
      known_fn from the branch's visible defs. The runtime table is
      the hard boundary; the static gate gives precise refusals and
      catches capability-carrying calls the name filter would only
      stop deeper in.
    - READER'S BRANCH ENV. Eval runs in a CHILD of the session env:
      real bindings resolve through the parent chain (thunks force
      through their own captured closures), and nothing the example
      does can leak into the session env.
    - CAPS. Fresh step budget per call (Eval.make_budget, default
      limit) — the lisp/ recursion/loop guardrail; plus a wall-clock
      cap checked between top-level forms. Blocking prims (sleep,
      http-get, llm/agent families) are not whitelisted, so nothing
      inside the example can park the fiber past the step budget. A
      runaway example's budget exhaustion converts to [Refused], NOT
      a bare Budget_exceeded: the only budget in play here is the
      nested one (the caller's scope becomes visible again on exit),
      and guard — the only catch Nopales has — deliberately re-raises
      Budget_exceeded, so (book/verify) must see a refusal to render
      the runaway as a hole instead of dying with the report.

    The nested budget is FIBER-SCOPED (i-1788670103-5): the scope
    answers [current ()] inside the window and the caller's scope
    re-emerges on exit (same shadow/restore semantics the old ref
    stash provided, invisible to concurrent requests). The ambient
    rim is saved/restored around the nested run
    (Control.run_with_rim nests by design; same pattern as
    Session.bench). *)

open Pricklypear_lisp.Ast

exception Refused of string
(** Static gate refusal. The reason names the offending form or
    capability; callers render it as a drift-hole-style error box. *)

(** Default step budget for one example eval (matches the per-page
    default; examples are snippets, not pages). *)
let default_step_limit = 100_000

(** Wall-clock cap in seconds, checked between top-level forms (a form
    cannot be interrupted mid-flight; the step budget bounds those). *)
let default_time_cap = 10.0

(** Effects an example block may use. [Html] is allowed because
    render-html is a pure value tag (wraps a VStr), not I/O. *)
let allowed_effect = function
  | Pricklypear_lisp.Effect.Pure
  | Pricklypear_lisp.Effect.ReadDb
  | Pricklypear_lisp.Effect.Html -> true
  | _ -> false

(** Mutation-family special forms refused anywhere in example source.
    define/defmacro mutate envs (and define persists in ordinary
    eval); with-transaction/with-step are write paths; prim is the
    internal escape hatch. Quoted data is inert and never reaches the
    scanner's refusal (a quoted (define …) is just data). *)
let forbidden_special = [ "define"; "defmacro"; "with-transaction"; "with-step"; "prim" ]

(** Blocked despite a pure classification: [sleep] sits in
    Effect.pure_prims but parks the fiber for its duration — exactly
    what a reader example must not be able to do (the step budget only
    counts steps, not wall time). Blocked BEFORE the allow lists so the
    classification cannot smuggle it in. Non-blocking clock reads
    (now/now-ms) stay allowed. Revisit only alongside an effect.ml
    reclassification (that would ripple to INSTALL/ROUTE gates —
    out of scope here). *)
let blocked_prims = [ "sleep" ]

(** Whitelist supplement: kernel prims that are read-only or pure but
    not yet classified in lisp/lib/effect.ml's capability lists (those
    lists predate several kernel prims and only need updating when an
    INSTALL/ROUTE gate must see the capability — the example whitelist
    must be exact, so it carries its own supplement). Each entry is
    reviewed read-only/pure; anything absent stays absent (fail-closed). *)
let read_extra_prims =
  [ (* pure string/token kernels (P2.2 budget prims + helpers) *)
    "string-split"; "prose-tokens"; "render-tokens"; "err/shape"
  ; "extract-links"; "render-links"; "budget-remaining"
  ; "status-response"; "part"; "json-part"; "multipart"
    (* one-tree READ surface + pure path helpers *)
  ; "tree/get"; "tree/list"; "tree/list-kind"; "tree/children-of"
  ; "tree/subtree"; "tree/search-text"; "tree/note-index"
  ; "tree/lens-get"; "tree/lens-children"; "tree/lens-subtree"
  ; "tree/path-label"; "tree/parent"; "tree/leaf"; "tree/segments"
  ; "tree/child"; "tree/ancestors"; "tree/garden-root"
  ; "tree/find-by"; "tree/get-uuid"; "tree/folder-index"
  ; "tree/children-html"; "tree/siblings"; "tree/next"; "tree/prev"
  ; "backlinks"; "out-links"; "node-path"; "type-fields"
    (* reference/package reflection (read-only) *)
  ; "list-packages"; "describe"; "apropos-docs"
  ; "callees"; "callers"; "dependents"; "used-by-routes"; "holes"
  ; "find-code"; "code-history"; "code-diff"; "diff"
  ; "list-data-lenses"
    (* media metadata reads *)
  ; "media-list"; "media-meta"; "media-url" ]

let whitelisted (name : string) : bool =
  not (List.mem name blocked_prims)
  && (List.mem name Pricklypear_lisp.Effect.pure_prims
      || List.mem name Pricklypear_lisp.Effect.read_db_prims
      || List.mem name read_extra_prims)

(** Build the restricted table: a NEW table holding only whitelisted
    entries of [full] (same closures — read DB prims keep the caller's
    session store/branch). Unknown names are simply absent. *)
let restricted_prims (full : Primitives.t)
    : Primitives.t =
  let t = Hashtbl.create 64 in
  Hashtbl.iter
    (fun name fn -> if whitelisted name then Hashtbl.replace t name fn)
    full;
  t

(** Reader sugar 'x parses as App (Var "quote", [e]); eval dispatches
    "quote" as a SPECIAL FORM (datum returned as-is — eval.ml), but
    Effect.infer only knows the Quote node and otherwise treats the
    shape as a call application whose args it unions — so a quoted
    datum containing e.g. (create …) would infer WriteDb and get
    refused as a capability the example never actually uses. Rewrite
    the sugar to the Quote node before gating so quoted data stays
    inert to BOTH the special-form scan and the capability inference.
    Quasiquote is deliberately NOT desugared: its unquoted parts DO
    execute, so infer unioning their effects is correct there. *)
let rec desugar_quote (e : expr) : expr =
  match e with
  | App (Var "quote", [ x ]) -> Quote (desugar_quote x)
  | App (f, args) -> App (desugar_quote f, List.map desugar_quote args)
  | Prim (n, args) -> Prim (n, List.map desugar_quote args)
  | Lambda (ps, body) -> Lambda (ps, desugar_quote body)
  | Define (n, body) -> Define (n, desugar_quote body)
  | Let (n, a, b) -> Let (n, desugar_quote a, desugar_quote b)
  | Letrec (bs, body) ->
      Letrec (List.map (fun (n, b) -> (n, desugar_quote b)) bs, desugar_quote body)
  | Seq (a, b) -> Seq (desugar_quote a, desugar_quote b)
  | If (a, b, c) -> If (desugar_quote a, desugar_quote b, desugar_quote c)
  | e -> e

(** First forbidden special form in [e], or None. Does NOT descend into
    quoted data — the reader sugar for 'x is App (Var "quote", [e]) and
    eval returns the datum as-is (desugar_quote rewrites it to the
    Quote node first) — so a quoted (define …) is inert. Soundness:
    anything quoted data can still reach execution through
    (macro expansion building code, quasiquote unquotes, eval-string)
    evals under the RESTRICTED rim, which fails closed on absent
    prims. *)
let rec find_forbidden (e : expr) : string option =
  match e with
  | Var _ | Lit _ | Quote _ -> None
  | App (Var "quote", [ _ ]) -> None
  | Prim (n, args) ->
      if List.mem n forbidden_special then Some n else scan_args args
  | App (Var name, args) ->
      if List.mem name forbidden_special then Some name else scan_args args
  | App (f, args) -> (
      match find_forbidden f with
      | Some x -> Some x
      | None -> scan_args args)
  | Lambda (_, body) -> find_forbidden body
  | Define _ -> Some "define"
      (* AST-level define (internal shape); surface text is App-form *)
  | Let (_, a, b) | Seq (a, b) -> (
      match find_forbidden a with
      | Some x -> Some x
      | None -> find_forbidden b)
  | Letrec (bs, body) -> (
      match List.find_map (fun (_, e') -> find_forbidden e') bs with
      | Some x -> Some x
      | None -> find_forbidden body)
  | If (a, b, c) -> (
      match find_forbidden a with
      | Some x -> Some x
      | None -> (
          match find_forbidden b with
          | Some x -> Some x
          | None -> find_forbidden c))

and scan_args (args : expr list) : string option =
  List.find_map find_forbidden args

(** Static gate over one parsed form: [Some reason] refuses. *)
let check_form ~(known_fn : Pricklypear_lisp.Effect.known_fn_lookup)
    (raw : expr) : string option =
  let e = desugar_quote raw in
  match find_forbidden e with
  | Some f ->
      Some
        (Printf.sprintf "special form '%s' is not allowed in examples" f)
  | None ->
      let fx = Pricklypear_lisp.Effect.infer ~known_fn e in
      match
        List.find_opt
          (fun eff -> not (allowed_effect eff))
          (Pricklypear_lisp.Effect.EffSet.to_list fx)
      with
      | Some eff ->
          Some
            (Printf.sprintf
               "example needs capability '%s' (allowed: pure, read-db, html)"
               (Pricklypear_lisp.Effect.eff_to_string eff))
      | None -> None

(** Parse + gate + eval [src] (all top-level forms, last value
    returned) in a child of [env] under a fresh budget and a restricted
    rim over [full_prims]. Raises [Refused] on gate refusals and on
    the example's own step-budget exhaustion; parse errors and eval
    errors propagate as-is. *)
let eval_source ~(env : Pricklypear_lisp.Ast.env)
    ~(full_prims : Primitives.t)
    ~(known_fn : Pricklypear_lisp.Effect.known_fn_lookup)
    ?(limit = default_step_limit) ?(time_cap = default_time_cap)
    (src : string) : Pricklypear_lisp.Ast.value =
  let forms = Pricklypear_lisp.Reader.read_all src in
  if forms = [] then
    raise (Refused "empty source");
  List.iter
    (fun e ->
      match check_form ~known_fn e with
      | Some reason -> raise (Refused reason)
      | None -> ())
    forms;
  let rprims = restricted_prims full_prims in
  let cenv = Pricklypear_lisp.Env.child env in
  let budget = Pricklypear_lisp.Eval.make_budget ~limit () in
  let t0 = Unix.gettimeofday () in
  try
    Control_rim.run ~prims:rprims (fun () ->
        (* Fiber-scope the nested budget (i-1788670103-5): inside this
           window the scope answers [current ()] with the nested budget
           and the caller's scope becomes visible again on exit — the
           same shadow/restore semantics the old ref stash provided,
           but invisible to concurrently-published requests. *)
        Pricklypear_lisp.Eval.with_scope (Some budget) (fun () ->
            let rec loop last = function
              | [] -> last
              | e :: rest ->
                  if Unix.gettimeofday () -. t0 > time_cap then
                    raise
                      (Interp_error
                         (User_error
                            ("example-eval: time cap exceeded", "timeout")));
                  let v =
                    Pricklypear_lisp.Eval.eval ~budget
                      ~prims:Pricklypear_lisp.Eval.empty_prims cenv e
                  in
                  loop v rest
            in
            loop VNil forms))
  with
    (* Inside this window the only live budget is the nested one (the
       caller's scope becomes visible again on exit). Convert so
       both consumers (POST run button, (example-eval) prim →
       book/verify's guard) render a runaway example as a refusal. *)
    Interp_error Budget_exceeded ->
      raise (Refused (Printf.sprintf "example budget exceeded (%d steps)" limit))
