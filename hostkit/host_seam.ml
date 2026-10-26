(** Host_seam — the host seam shared by agent/lib/runtime.ml and
    remote/lib/remote.ml (borge/agent.borg agent-route-table +
    borge/local-first.borg client, lifted here by i-17867304-d so the
    ~450 verbatim mirror lines live in ONE home).

    The seam is the small surface the Nopales-authored world (libs/hyper
    route table, the agent turn loop, the HTTP surfaces) programs
    against: a thin request shape, a route table with
    route/route/lookup/route/dispatch, request-context prims
    (query-param / request-method / hx-request?), Nopales template
    calls, the event/tool registry backing [Loop.host], and the boot
    stdlib macro. Identical Nopales signatures to the image's
    route prims, host-local backing (a route table + thin request
    context vs the image's Postgres routes + fat Request_context), so
    libs/hyper runs unchanged on every host.

    The ONE host-specific decision — which rim services Prim_call for
    an OCaml caller OUTSIDE an active eval — is the [run_in_rim] hook:
    the agent locks its eval mutex and runs the local host-prim rim
    (Control.run_with_rim); pp-remote runs the proxy rim
    (Proxy_rim.run), so a dispatched handler's DB prim calls cross to
    /api/prim. Everything else in this module is rim-agnostic. *)

open Pricklypear_lisp
open Ast

(** Thin request context bound during route/dispatch. Carries ONLY
    request-shape values: method, path, params, headers, body. DB/
    branch/auth request prims (current-author-as-DB-row etc.) are
    intentionally UNBOUND in both hosts — the request context provides
    only the request-shape values (agent-route-table.pg-prim-unbound
    pins this). *)
type request_shape = {
  method_ : string;
  path : string;
  params : (string * string) list;
  headers : (string * string) list;  (* lowercased keys *)
  body : string;
}

(** The shared seam state. Both host records (Runtime.t, Remote.t)
    embed one [t] as their [seam] field; host-specific state (the
    agent's eval mutex + command registry, the client's transport +
    def cache + lens) stays in the host record. *)
type t = {
  env : Ast.env;
  prims : Eval.prim_table;
  (** Route table: path -> handler-name (THE HOST SEAM,
      agent-route-table). Local-lens backing for the image's Postgres
      routes — identical Nopales signatures so libs/hyper runs
      unchanged. *)
  routes : (string, string) Hashtbl.t;
  (** Per-call request snapshot bound while a route/dispatch handler
      runs, so query-param / request-method / hx-request? resolve.
      None outside a dispatch. *)
  current_request : request_shape option ref;
  (** Captured multipart parts of the last consumed chat/agent turn,
      as a VList of part alists (("html" . frag) ("target" . sel)
      ("swap" . strat)) — the data shape [soup/apply-parts!] consumes.
      Set by the HTTP surface's stream emitter (Chat_stream) so a
      consumed turn replays deterministically through [hyper/stream]
      (surfaces smoke). *)
  mutable last_turn_parts : Ast.value;
  (** Tool registry (builtin read/bash + Nopales register-tool
      plugins) + event handlers ((on "event" …) hooks) — the backing
      of the shared LLM turn loop's [Loop.host] seam. *)
  tools : (string, Ast.value Loop.registry_entry) Hashtbl.t;
  mutable tool_order : string list;
  handlers : (string, (Ast.value list -> Ast.value) list ref) Hashtbl.t;
  (** Establish the host rim for an OCaml caller OUTSIDE an active
      eval. agent: eval lock + Control.run_with_rim over the local
      host-prim table; remote: Proxy_rim.run (prim calls cross to
      /api/prim). Every seam use is an eval (value-returning;
      load_source wraps read_eval_all_direct as VNil). *)
  run_in_rim : (unit -> Ast.value) -> Ast.value;
  (** Names registered through [add_prim] (LOCAL prims: pure catalog +
      soup + route/event/tool prims). The remote placer treats these
      as NOT crossing the wire (no prim-density count); the agent
      ignores the list. Proxy-bound prims (bind_proxy_prim) and
      server-only prims are NOT here. *)
  mutable registered : string list;
  (** Host working directory for path prims (immutable per process;
      hosts never chdir). *)
  cwd : string ref;
}

let make ~(prims : Eval.prim_table)
    ~(run_in_rim : (unit -> Ast.value) -> Ast.value)
    ~(cwd : string ref) () : t =
  {
    env = Env.create ();
    prims;
    routes = Hashtbl.create 16;
    current_request = ref None;
    last_turn_parts = Ast.VNil;
    tools = Hashtbl.create 16;
    tool_order = [];
    handlers = Hashtbl.create 8;
    run_in_rim;
    registered = [];
    cwd;
  }

let registered_local_prims (t : t) : string list = List.rev t.registered

(** Common prim-argument helpers (one copy; both hosts previously
    defined identical str_arg/emit_error). *)
let str_arg name v =
  match v with
  | Ast.VStr s -> s
  | Ast.VSymbol s -> s
  | _ ->
      raise
        (Ast.Interp_error
           (Ast.Prim_error (name, "expected string/symbol argument", "bad-args")))

let emit_error name msg =
  raise (Ast.Interp_error (Ast.Prim_error (name, msg, "error")))

(** Register a host prim: dispatch table PLUS a VPrim binding in the
    shared env (Nopales resolves prims via env, like the image's
    auto-bind loop in session.ml). All registrations go through here;
    the name lands in [registered] (see placer note above). *)
let add_prim (t : t) (name : string) (fn : Ast.value list -> Ast.value)
    : unit =
  Eval.register_prim t.prims name fn;
  Env.define t.env name (Ast.VPrim name);
  t.registered <- name :: t.registered

(** Path resolution: relative paths resolve under [cwd]; ".." escapes
    are refused (v0 sandbox posture: refuse, not normalize). Shared by
    both hosts' path-taking prims and the read/bash builtin tools. *)
let resolve_path ~(cwd : string ref) (p : string) : string =
  let p =
    if String.length p > 0 && p.[0] = '/' then p
    else Filename.concat !cwd p
  in
  let parts =
    String.split_on_char '/' p |> List.filter (fun s -> s <> "" && s <> ".")
  in
  if List.exists (fun s -> s = "..") parts then
    emit_error "fs/path" ("path escapes sandbox: " ^ p);
  "/" ^ String.concat "/" parts

(** Look up the active request shape, or error if none is bound (i.e. a
    request prim called outside a route/dispatch). Mirrors the image's
    prims_request require_request, but this context is thinner. *)
let require_request (t : t) : request_shape =
  match !(t.current_request) with
  | Some r -> r
  | None ->
      emit_error "request" "no active request (call inside route/dispatch)"

(** [apply_value_raw] — apply a Nopales function value to host-side
    value args directly, assuming the caller ALREADY has the rim active
    (route/dispatch invokes a handler from within the rim; on the agent
    the caller additionally holds the eval lock — the non-reentrant
    mutex must not be re-taken). Args NEVER go through the reader, so
    lambda values and arbitrary JSON strings are safe. *)
let apply_value_raw (t : t) (fn : Ast.value) (args : Ast.value list)
    : Ast.value =
  let fn_sym = "__host_apply_fn__" in
  Env.define t.env fn_sym fn;
  let arg_syms =
    List.mapi (fun i _ -> Printf.sprintf "__host_apply_arg_%d__" i) args
  in
  List.iter2 (Env.define t.env) arg_syms args;
  let src =
    "(" ^ fn_sym
    ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "" arg_syms
    ^ ")"
  in
  let budget = Eval.make_budget () in
  let r =
    try Ok (Eval.eval_string_direct ~prims:t.prims t.env ~budget src)
    with e -> Error e
  in
  if Sys.getenv_opt "PP_PROFILE_RENDER" <> None then begin
    (* PP_PROFILE_TREE=1 additionally dumps the debug call tree
       (budget.dbg_dump). CAVEAT: brackets ride the flat current_fn
       attribution, which is approximate across thunk-force boundaries
       — per-name totals and leaf rows are exact; a node that absorbed
       later spans shows incl ≈ its root's. Read it as
       concentration-by-segment, not as exact nesting. *)
    (match Sys.getenv_opt "PP_PROFILE_TREE" with
     | Some _ ->
         Printf.eprintf
           "[call-tree] %s\n%!"
           (Budget.dbg_dump budget)
     | None -> ());
    let merged =
      Hashtbl.fold (fun k steps acc ->
          let c =
            try Hashtbl.find budget.Budget.fn_call_counts k with Not_found -> 0
          in
          (steps, c, k) :: acc)
        budget.Budget.fn_step_counts []
    in
    let clean = String.map (fun c -> if c = '(' then '_' else c) in
    let hot =
      List.sort (fun (s1, _, _) (s2, _, _) -> compare s2 s1) merged
      |> List.filteri (fun i _ -> i < 15)
      |> List.map (fun (s, c, k) ->
             Printf.sprintf "%s=%d/%d(%.1f)" (clean k) s c
               (if c > 0 then float_of_int s /. float_of_int c else 0.))
      |> String.concat " "
    in
    let frequent =
      List.sort (fun (_, c1, _) (_, c2, _) -> compare c2 c1) merged
      |> List.filteri (fun i _ -> i < 15)
      |> List.map (fun (s, c, k) ->
             Printf.sprintf "%s=%d/%d(%.1f)" (clean k) s c
               (if c > 0 then float_of_int s /. float_of_int c else 0.))
      |> String.concat " "
    in
    Printf.eprintf "[profile] left=%d/prim=%d top-self: %s\n[profile] hot-freq: %s\n%!"
      (max 0 (budget.Budget.limit - budget.Budget.steps))
      budget.Budget.prim_calls hot frequent
  end;
  match r with
  | Ok v -> v
  | Error e -> raise e

(** Apply a Nopales function value to host-side value args. Establishes
    the rim via [run_in_rim] — for callers OUTSIDE an active eval (loop
    tool bodies, event handlers, HTTP template calls). Prims running
    inside an eval use [apply_value_raw] instead. *)
let apply_value (t : t) (fn : Ast.value) (args : Ast.value list) : Ast.value =
  t.run_in_rim (fun () -> apply_value_raw t fn args)

(** [dispatch_route_raw] — the shared route/dispatch body. Assumes the
    rim is already active (the route/dispatch prim runs inside an eval;
    the HTTP gate uses the rim-establishing [dispatch_route] wrapper).
    Returns the rendered raw-HTML VStr the browser would receive. *)
let dispatch_route_raw (t : t) ~handler ~method_ ~path ~params
    ~headers ~body : Ast.value =
  let req : request_shape =
    { method_ = String.uppercase_ascii method_; path; params; headers; body }
  in
  match Env.lookup t.env handler with
  | None -> VStr (Printf.sprintf "route/dispatch: unbound handler %s" handler)
  | Some fn ->
      t.current_request := Some req;
      Fun.protect
        ~finally:(fun () -> t.current_request := None)
        (fun () ->
          try
            let v = apply_value_raw t fn [] in
            VStr (Prims_soup.render_handler_value v)
          with Ast.Interp_error ie ->
            VStr
              (Printf.sprintf "route/dispatch: %s"
                 (Ast.pp_interp_error ie)))

(** [dispatch_route] — rim-establishing wrapper for OCaml callers
    OUTSIDE an active eval (the HTTP gate dispatches GET routes through
    the route table). On the agent this takes the eval lock + the local
    rim; on pp-remote it runs the proxy rim. *)
let dispatch_route ~(rt : t) ~handler ~method_ ~path ~params ~headers
    ~body : Ast.value =
  rt.run_in_rim (fun () ->
      dispatch_route_raw rt ~handler ~method_ ~path ~params ~headers ~body)

(** Resolve a URL path to its handler name via the route table. *)
let lookup_route ~(rt : t) (path : string) : string option =
  Hashtbl.find_opt rt.routes path

(** Call a Nopales template (zero-or-more-arg fn) by name and return the
    rendered raw-HTML string (raw-HTML-tagged VStrs unwrap). For OCaml
    callers outside an eval (the HTTP surface renders fragment
    templates). Returns [None] if the template is unbound or errors. *)
let call_template ~(rt : t) (name : string) (args : Ast.value list)
    : string option =
  match Env.lookup rt.env name with
  | None -> None
  | Some fn ->
      (try
         let v = apply_value rt fn args in
         Some (Prims_soup.render_handler_value v)
       with Ast.Interp_error _ -> None)

(** Register the route-table + request-context prims (route,
    route/lookup, route/dispatch, query-param, request-method,
    hx-request?, session/last-turn-parts). Reuses assoc_of +
    render_handler_value from the shared soup kernel. *)
let register_route_prims (t : t) : unit =
  (* (route path handler) — bind a path to a Nopales render handler name
     on the route table. The host's local-lens backing for the image's
     (route ...) over Postgres. *)
  let route args =
    match args with
    | [ path; handler ] ->
        let path = str_arg "route" path in
        let handler = str_arg "route" handler in
        Hashtbl.replace t.routes path handler;
        VBool true
    | _ -> emit_error "route" "usage: (route path handler)"
  in
  add_prim t "route" route;
  (* (route/lookup path) -> handler-name VStr | VNil. Resolve a URL path
     to its handler via the route table — exactly hyper/visit's
     resolver. *)
  let route_lookup args =
    match args with
    | [ path ] ->
        let path = str_arg "route/lookup" path in
        (match Hashtbl.find_opt t.routes path with
         | Some h -> VStr h
         | None -> VNil)
    | _ -> emit_error "route/lookup" "usage: (route/lookup path)"
  in
  add_prim t "route/lookup" route_lookup;
  (* (route/dispatch handler method path [params] [headers] [body])
     -> rendered raw-HTML VStr. Invoke the handler fn (looked up in env)
     through the active rim with a THIN REQUEST CONTEXT bound
     (query-param / request-method / hx-request? resolve; DB/branch/auth
     request prims stay unbound), and return the rendered value the
     browser would receive. *)
  let route_dispatch args =
    match args with
    | VStr handler :: VStr meth :: VStr path :: rest ->
        let params =
          match rest with p :: _ -> Prims_soup.assoc_of p | _ -> []
        in
        let headers =
          match rest with
          | _ :: h :: _ ->
              List.map
                (fun (k, v) -> (String.lowercase_ascii k, v))
                (Prims_soup.assoc_of h)
          | _ -> []
        in
        let body =
          match rest with
          | _ :: _ :: VStr b :: _ -> b
          | _ -> ""
        in
        dispatch_route_raw t ~handler ~method_:meth ~path ~params ~headers ~body
    | _ -> emit_error "route/dispatch"
              "usage: (route/dispatch handler method path [params] [headers] [body])"
  in
  add_prim t "route/dispatch" route_dispatch;
  (* ---- request-shape prims (the thin context) ----
     query-param / request-method / hx-request? — they back
     route/dispatch's thin context and libs/hyper's hyper/do-swap
     HX-Request header path. DB/branch/auth request prims are NOT
     registered here (the request context carries only request-shape
     values); calling them yields unbound-prim. *)
  let query_param args =
    match args with
    | [ key ] ->
        let r = require_request t in
        let key = str_arg "query-param" key in
        (match List.assoc_opt key r.params with
         | Some v -> VStr v
         | None -> VNil)
    | _ -> emit_error "query-param" "usage: (query-param key)"
  in
  add_prim t "query-param" query_param;
  let request_method _args =
    let r = require_request t in
    VStr r.method_
  in
  add_prim t "request-method" request_method;
  (* (request-params) -> VList of (k v) pairs — the FULL merged param
     alist (query ++ form body). Exists for hosts that pipe a request
     through to another server wholesale (the habitat-pane forwarder
     rebuilds a query string from it); it is the one prim that hands
     over the whole alist instead of one key. *)
  let request_params _args =
    let r = require_request t in
    VList
      (List.map
         (fun (k, v) -> VList [ VStr k; VStr v ])
         r.params)
  in
  add_prim t "request-params" request_params;
  let hx_request_p _args =
    let r = require_request t in
    match List.assoc_opt "hx-request" r.headers with
    | Some v when v <> "" && v <> "false" -> VBool true
    | _ -> VBool false
  in
  add_prim t "hx-request?" hx_request_p;
  (* (session/last-turn-parts) -> VList of part alists captured from the
     last consumed chat/agent turn (the data shape soup/apply-parts! /
     hyper/stream consume). Lets a consumed turn replay deterministically
     through hyper/stream (surfaces smoke). Returns VNil if no turn ran. *)
  let last_turn_parts _args = t.last_turn_parts in
  add_prim t "session/last-turn-parts" last_turn_parts

(** Register the event + plugin-tool prims both hosts share:
    (on "event" handler-fn) hooks and (register-tool name params-alist
    fn). Params are validated but v0 wire schemas are synthesized from
    param names only (loop.ml). The agent's register-command /
    list-commands stay agent-side (Runtime.t owns the command
    registry). *)
let register_event_prims (t : t) : unit =
  let on args =
    match args with
    | [ ev; fn ] ->
        let ev = str_arg "on" ev in
        let cell =
          match Hashtbl.find_opt t.handlers ev with
          | Some c -> c
          | None ->
              let c = ref [] in
              Hashtbl.replace t.handlers ev c;
              c
        in
        cell := !cell @ [ (fun vals -> apply_value t fn vals) ];
        VBool true
    | _ -> emit_error "on" "usage: (on \"event\" (lambda (ev) ...))"
  in
  add_prim t "on" on;
  let reg_tool args =
    match args with
    | [ name; _params; fn ] ->
        let name = str_arg "register-tool" name in
        Hashtbl.replace t.tools name { Loop.doc = ""; run = fn };
        if not (List.mem name t.tool_order) then
          t.tool_order <- t.tool_order @ [ name ];
        VBool true
    | _ -> emit_error "register-tool" "usage: (register-tool name params fn)"
  in
  add_prim t "register-tool" reg_tool

(** Fire an event: run handlers in registration order. Returns the last
    handler's value list (or []). Callers that care about
    short-circuiting (tool_call block) inspect per-handler values
    themselves. *)
let fire (t : t) (ev : string) (payload : Ast.value list) : Ast.value list =
  match Hashtbl.find_opt t.handlers ev with
  | None -> []
  | Some hs ->
      List.map
        (fun h ->
          (* A handler raising must not kill the host turn; surface it
             as an {"error": ...} value the caller can inspect. *)
          try h payload
          with Ast.Interp_error ie ->
            Ast.VStr
              (Yojson.Safe.to_string
                 (`Assoc
                   [
                     ("error",
                      `String
                        (Printf.sprintf "%s handler: %s" ev
                           (Ast.pp_interp_error ie)));
                   ])))
        !hs


(** Parse a host/plugin-produced value back to host JSON. VStr is treated
    as raw JSON first (dict prims produce JSON strings), else a plain
    string. Lives here rather than in llm/lib/loop.ml because the loop seam
    is Yojson-only; hosts re-export it for their callers. *)
let rec json_of_value (v : Ast.value) : Yojson.Safe.t =
  match v with
  | Ast.VStr s -> (
      try Yojson.Safe.from_string s
      with Yojson.Json_error _ -> `String s)
  | VInt i -> `Intlit (Int64.to_string i)
  | VFloat f -> `Float f
  | VBool b -> `Bool b
  | VNil -> `Null
  | VSymbol s -> `String s
  | VList vs -> `List (List.map json_of_value vs)
  | VLambda _ -> `String "<lambda>"
  | VPrim n -> `String ("<prim " ^ n ^ ">")
  | other -> `String (Reader.pp_value other)
(** Run a plugin tool's closure with JSON args, containing errors into
    a {"error": ...} JSON string. *)
let eval_tool_body (t : t) (name : string) (args_json : string) : Ast.value =
  match Hashtbl.find_opt t.tools name with
  | None ->
      VStr
        (Yojson.Safe.to_string
           (`Assoc [ ("error", `String ("no tool named " ^ name)) ]))
  | Some e ->
      (try apply_value t e.Loop.run [ VStr args_json ]
       with Ast.Interp_error ie ->
         VStr
           (Yojson.Safe.to_string
              (`Assoc
                [ ("error", `String (Ast.pp_interp_error ie)) ])))

(** Register a builtin host tool (read/bash): registry entry as a VPrim
    naming a LOCAL prim, so the loop dispatches builtins through the
    rim's local table (no wire — builtin bodies are OCaml). Plugin
    hooks (tool_call / tool_result events) still see the registry
    entry. *)
let register_builtin_tool (t : t) (name : string)
    ~(run : Yojson.Safe.t -> string) : unit =
  Hashtbl.replace t.tools name
    { Loop.doc = "builtin"; run = Ast.VPrim ("__builtin__" ^ name) };
  if not (List.mem name t.tool_order) then
    t.tool_order <- t.tool_order @ [ name ];
  (* prim that the registry entry's VPrim names; loop.ml calls
     builtins through this prim table entry instead of applying a
     lambda. *)
  add_prim t ("__builtin__" ^ name)
    (fun args ->
      match args with
      | [ Ast.VStr args_json ] -> (
          try VStr (run (Yojson.Safe.from_string args_json))
          with
          | Yojson.Json_error e ->
              VStr
                (Yojson.Safe.to_string
                   (`Assoc [ ("error", `String ("bad json: " ^ e)) ]))
          | Ast.Interp_error ie ->
              VStr
                (Yojson.Safe.to_string
                   (`Assoc
                     [ ("error", `String (Ast.pp_interp_error ie)) ])))
      | _ -> emit_error "__builtin" "expected single JSON string arg")

(** Host adapter for the shared agent turn loop (llm/lib/loop.ml): the
    small [Loop.host] seam — live tool order, registry lookup, event
    fire, tool-body eval, and serialized prim dispatch through
    [run_in_rim]. This is the ONLY coupling the loop has to a host. *)
(** Host adapter for the shared agent turn loop (llm/lib/loop.ml): the
    small [Loop.host] seam — live tool order, registry lookup, event
    fire, tool-body eval, and serialized prim dispatch through
    [run_in_rim]. Every value crossing into the loop is Yojson; the
    registry entry's own [run] payload stays host-side. This is the ONLY
    coupling the loop has to a host. *)
let loop_host (t : t) : Loop.host =
  {
    Loop.tool_order = (fun () -> t.tool_order);
    tool_entry =
      (fun name ->
        match Hashtbl.find_opt t.tools name with
        | None -> None
        | Some e -> (
            match e.Loop.run with
            | Ast.VPrim prim -> Some (Loop.Builtin prim)
            | _ -> Some Loop.Plugin));
    fire =
      (fun ev payloads ->
        List.map json_of_value
          (fire t ev
             (List.map (fun j -> Ast.VStr (Yojson.Safe.to_string j)) payloads)));
    eval_tool_body =
      (fun name args ->
        json_of_value (eval_tool_body t name (Yojson.Safe.to_string args)));
    call_prim =
      (fun name args ->
        let args_json = Yojson.Safe.to_string args in
        (* Builtin bodies are host OCaml running under the rim: contain
           their errors here so the loop only ever renders a value. *)
        try
          json_of_value
            (t.run_in_rim (fun () ->
                 (Hashtbl.find t.prims name) [ Ast.VStr args_json ]))
        with
        | Ast.Interp_error ie ->
            `Assoc [ ("error", `String (Ast.pp_interp_error ie)) ]
        | Not_found ->
            `Assoc [ ("error", `String ("no prim named " ^ name)) ]);
  }

(** Install the shared pure-prim catalog (pure/prims_pure.ml
    [Prims_pure.catalog] — the ONE hand-enumerated frozen surface,
    i-1788670799-f: the per-host hand copies had already diverged).
    add_prim registers in the table AND binds the VPrim in env. *)
let register_pure_prims (t : t) : unit =
  List.iter (fun (name, fn) -> add_prim t name fn) Prims_pure.catalog

(** Register the SHARED soup/ tree prims (Phase 0: lifted into
    pure/prims_soup.ml so every host registers the same tree prims).
    [Prims_soup.register_pure_prims] fills the prim table; each soup/
    name is then bound as a VPrim in env (matching [add_prim]'s
    env-bind half) and recorded as local (soup prims are pure — they
    never cross the wire). *)
let register_soup_prims (t : t) : unit =
  Prims_soup.register_pure_prims t.prims;
  Hashtbl.iter
    (fun name fn ->
      if String.length name >= 5 && String.sub name 0 5 = "soup/" then
        add_prim t name fn)
    t.prims

(** Minimal host stdlib — the host library layer (borge/agent.borg
    nopales-embed: "the agent adds HOST PRIMS and an optional host library
    layer in .pp"). Provides the [let*] sequential-binding macro that
    libs/hyper uses, without loading the heavy libs/std (which depends on
    image-only prims: css/*, menu/*, list-bindings, redirect, ...).
    The image gets [let*] from libs/std; the hosts get it here. Same
    expansion as the libs/std macro (written with car/cdr so it depends on
    no c*r beyond the registered set). Both hosts load this verbatim at
    make. *)
let boot_stdlib = {|
(defmacro let* (bindings body)
  (if (null? bindings)
      (list (quote let) (list) body)
      (list (quote let)
            (list (list (car (car bindings)) (car (cdr (car bindings)))))
            (list (quote let*) (cdr bindings) body))))
|}
