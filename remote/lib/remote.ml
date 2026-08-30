(** remote/lib/remote.ml (borge/remote-eval.borg client).

    The client host: a pure-lisp env + a small LOCAL table of pure
    prims (shared Prims_pure code, so semantics are identical to the
    image's) + a [Proxy_rim.transport] to the image. All evaluation
    runs under the proxy rim; pure compute stays local, everything
    else proxies.

    LAZY ENV (spec): function resolution is data (the server's CTE
    over function_defs). The client materializes defs on first
    reference instead of at boot: an [Interp_error (Unbound_var name)]
    during eval triggers a fetch of the def source (via the
    [function-source] prim, proxied) and evals it into the env under
    the proxy rim, then retries the expression once. The env is a
    CACHE of definitions — never a source of truth.

    IDENTITY: the client passes basic-auth per prim call through the
    transport. It never claims an author it does not hold; identity is
    server-asserted (whoami/current-author come back from the server). *)

open Pricklypear_lisp
open Ast

type transport = Proxy_rim.transport

type t = {
  env : Ast.env;
  local_prims : Eval.prim_table;
  transport : transport;
  (* definitions materialized locally, keyed (branch, generation): the
     def cache (caching subsection). Bounds server fetches on a miss and
     invalidates on generation change / branch switch. *)
  def_cache : Def_cache.t;
  (* PURE prim names bound locally (via [add_prim]): the placer treats
     these as NOT crossing the wire, so they do not count toward the
     prim-density heuristic. Pure compute stays local regardless. *)
  mutable pure_prims : string list;
  (* Whole-expression remote eval (POST /api/eval) for Remote
     placements. Some = hybrid placement is active; None = no remote
     eval path, so a Remote placement raises rather than silently
     running locally (which would break transaction correctness). *)
  remote_eval : (string -> value) option;
  (* OPTIONAL local lens substrate (borge/local-first.borg local-
     substrate): a materialized lens root + ancestry chain, used as the
     OFFLINE-FIRST def source. When set, [fetch_def] consults it before
     proxying [function-source], so defs in the replication unit resolve
     locally (the client evaluates against the local substrate; the
     server is only the merge/index authority). None = no lens wired,
     defs always proxy. *)
  lens_lookup : (string -> string option) option;
  (* The lens record itself, when wired. Set by the host after [make]
     (pp-remote --lens) so the lens/list + lens/read surface prims can
     browse the substrate. None = no lens; the prims return empty. *)
  mutable lens : Lens_store.lens option;
  (* ---- Route table + request context (agent-route-table host seam,
     ported to the client so the Nopales-authored surface runs on
     pp-remote unchanged — borge/remote-eval.borg surfaces). Local
     hashtbl backing for (route path handler); identical Nopales
     signatures to the image's route prims so libs/hyper runs
     unchanged. Env-only: no host-prim dependency, no rim coupling
     beyond the proxy rim the rest of the client already uses. *)
  routes : (string, string) Hashtbl.t;
  current_request : request_shape option ref;
  mutable last_turn_parts : Ast.value;
  (* ---- agent-loop host seam (borge/remote-eval.borg agent-loop) ----
     The shared LLM turn loop (llm/lib/loop.ml) drives the client
     through a small [Loop.host] record; these fields are its backing:
     a LOCAL tool registry (builtin read/bash + Nopales register-tool
     plugins) and event handlers ((on "tool_call" …) hooks). All
     env+hashtbl ops — same shape as the agent's Runtime.t surface. *)
  tools : (string, Loop.registry_entry) Hashtbl.t;
  mutable tool_order : string list;
  handlers : (string, (Ast.value list -> Ast.value) list ref) Hashtbl.t;
}

(** Thin request context bound during route/dispatch (agent-route-table
    host seam). Carries ONLY request-shape values: method, path, params,
    headers, body — the same shape the agent binds, so libs/hyper's
    query-param / request-method / hx-request? resolve identically. *)
and request_shape = {
  method_ : string;
  path : string;
  params : (string * string) list;
  headers : (string * string) list;  (* lowercased keys *)
  body : string;
}

let value_to_json = Prims_pure.value_to_json
let json_to_value = Prims_pure.json_to_value

(** Register a prim BOTH in the dispatch table and as a VPrim binding
    in the env (Nopales resolves prims via the env, like the image
    session binds its catalog). *)
let add_prim (t : t) (name : string) (fn : value list -> value) : unit =
  Eval.register_prim t.local_prims name fn;
  Env.define t.env name (VPrim name);
  (* track the pure/local prim for the placer (it stays local, so it
     must NOT count toward the prim-density threshold) *)
  t.pure_prims <- name :: t.pure_prims

(** The [Placer.cfg] for this client: the local pure prims are the
    names the placer must NOT count as proxy-round-trips. The rest —
    proxyable prims and user-function references — walk as prim calls. *) 
let placer_cfg (t : t) : Placer.cfg =
  Placer.default_cfg ~local_prims:(List.rev t.pure_prims)

(** Bind a prim as VPrim in the env WITHOUT a local handler. Calling it
    performs [Control.Prim_call], which the proxy rim forwards to
    /api/prim (it is absent from [local_prims]). This is how server
    prims (get/create/rows/…) reach the wire: they must be bound as
    VPrim so eval performs Prim_call, but must NOT be in the local
    table or they'd be serviced locally. Server-only prims
    (route/fork/merge/…) are intentionally NOT bound here — the server
    refuses them with a `server_only` error, and leaving them unbound
    surfaces that locally as Unbound_var rather than a round-trip refusal. *)
let bind_proxy_prim (t : t) (name : string) : unit =
  Env.define t.env name (VPrim name)

(** Register the frozen pure prim surface locally. Same code as the
    image links (Prims_pure), so local semantics are identical — the
    proxy rim's local table is the "pure compute stays local" half of
    hybrid placement. Options that keep a prim OUT of this table
    simply proxy it; nothing depends on the split being exhaustive. *)
let register_pure_prims (t : t) : unit =
  let open Prims_pure in
  let r n f = add_prim t n f in
  r "+" prim_add;
  r "-" prim_sub;
  r "*" prim_mul;
  r "/" prim_div;
  r "div" prim_div;
  r "mod" prim_mod;
  r "quotient" prim_quotient;
  r "remainder" prim_remainder;
  r "abs" prim_abs;
  r "min" prim_min;
  r "max" prim_max;
  r "=" prim_num_eq;
  r "<" prim_lt;
  r ">" prim_gt;
  r "<=" prim_le;
  r ">=" prim_ge;
  r "not" prim_not;
  r "list" prim_list;
  r "car" prim_car;
  r "cdr" prim_cdr;
  r "cons" prim_cons;
  r "null?" prim_null_p;
  r "pair?" prim_pair_p;
  r "append" prim_append;
  r "reverse" prim_reverse;
  r "nth" prim_nth;
  r "count" prim_count;
  r "length" prim_length;
  r "string-append" prim_string_append;
  r "string-length" prim_string_length;
  r "substring" prim_substring;
  r "string-contains?" prim_string_contains;
  r "string-split" prim_string_split;
  r "string-trim" prim_string_trim;
  r "string-replace" prim_string_replace;
  r "string-downcase" prim_string_downcase;
  r "string-upcase" prim_string_upcase;
  r "string-starts-with?" prim_string_starts_with_p;
  r "string-ends-with?" prim_string_ends_with_p;
  r "string=?" prim_string_eq;
  r "string-eq" prim_string_eq;
  r "type-of" prim_type_of;
  r "symbol?" prim_symbol_p;
  r "string?" prim_string_p;
  r "symbol->string" prim_symbol_to_string;
  r "string->symbol" prim_string_to_symbol;
  r "str" prim_str;
  r "as-str" prim_as_str;
  r "assoc" prim_assoc;
  r "member" prim_member;
  r "json-parse" prim_json_parse;
  r "json-emit" prim_json_emit;
  r "dict-get" prim_dict_get;
  r "dict-set" prim_dict_set;
  r "dict-keys" prim_dict_keys;
  r "equal?" prim_equal_p;
  r "random-uuid" prim_random_uuid;
  (* Pure rendering + string prims the Nopales surface (libs/html, the
     pp-remote surface) calls heavily. These are PURE (tag a VStr /
     split a string) — registering them LOCAL (not proxied) keeps the
     surface responsive offline and matches the agent's pure set. *)
  r "render-html" prim_render_html;
  r "html-escape" prim_html_escape;
  r "string-split" prim_string_split;
  (* c*r accessors used by libs/hyper + libs/html (parity with the
     agent's pure set). *)
  r "cadr" prim_cadr;
  r "caar" prim_caar;
  r "caddr" prim_caddr;
  r "cddr" prim_cddr

(** Bind the server-side prims a client is allowed to proxy. These are
    the proxyable DB CRUD + read/identity prims (NOT the server-only
    branch/admin prims — those stay unbound; see [bind_proxy_prim]). The
    set is deliberately a known list: it is the client's declaration of
    which server prims exist for it. A server prim not in this list is
    simply unbound on the client (Unbound_var), which is honest — the
    client doesn't pretend to know about prims it wasn't built against. *)
let register_proxy_prims (t : t) : unit =
  List.iter (bind_proxy_prim t)
    [ "get"; "create"; "update"; "delete"; "rows"
    ; "function-source"; "function-doc"; "apropos"; "apropos-docs"
    ; "find-code"
    ; "list-bindings"; "whoami"; "current-author"
    ; "node-path"; "backlinks"; "type-fields"; "list-kind"
      (* tree/* prims — the hierarchical store surface (one-tree). These
         are read-mostly (tree/get, tree/list, tree/subtree, …) plus the
         write trio tree/upsert | tree/delete | tree/move. Proxied so a
         lens client can eval defs that call them; the server stays the
         data-plane authority. *)
    ; "tree/get"; "tree/list"; "tree/children-of"; "tree/subtree"
    ; "tree/lens-get"; "tree/lens-children"; "tree/lens-subtree"
    ; "tree/upsert"; "tree/delete"; "tree/move"
    ; "tree/get-uuid"; "tree/find-by"; "tree/path-label"; "tree/garden-root"
    ; "tree/folder-index"; "tree/children-html"
    ; "package-exports"; "package-bindings"
      (* def-cache invalidation coordinates (caching subsection):
         current-branch (branch namespace) + gen-now (migration-017
         branch_generations = the tree-time gen coordinate — one
         counter, one namespace). Both are proxyable read prims. *)
    ; "current-branch"; "gen-now"
      (* local-first sync prims (borge/local-first.borg sync). sync/plan
         classifies; sync/push applies a two-way data push. Proxied so
         a lens client can drive a sync round. *)
    ; "sync/plan"; "sync/push" ]

(** Fetch the stored source for [name] by proxying the [function-source]
    read prim (registered on the image as a plain, proxyable prim — see
    image/lib/prims_db.ml). Returns Some source when the server has a
    def for [name], None when it does not. *)
let fetch_def (t : t) (name : string) : string option =
  (match t.lens_lookup with
   | None -> None
   | Some lookup -> lookup name)
  |> (function
      | Some src -> Some src
      | None -> (
          (* Proxy [function-source]; catch transport exceptions (the Eio
             HTTP transport RAISES [Eio.Io] on a refused connection, it
             does not return [Error]) so an offline client with a lens
             misses cleanly → [Unbound_var] instead of a process crash. *)
          match (try t.transport "function-source" [ VStr name ] with _ -> Error (Failure "offline")) with
          | Ok (VStr src) when String.trim src <> "" -> Some src
          | Ok (VStr _) -> None
          | Ok _ -> None
          | Error _ -> None))

(** Materialize a def into the client env: parse + eval the fetched
    source (a toplevel (define …)) under the proxy rim in [env]. The
    def body may reference prims — those proxy/run locally on call,
    not at define time.

    [function-source] returns the stored body as a bare (lambda …),
    not a (define name …) form, so eval produces the value but does
    NOT bind the name. Bind it explicitly when the src did not already
    do so (a (define …) binds during eval; a bare lambda does not) —
    this is what makes a fetched def resolvable on the retry. *)
let load_def (t : t) (name : string) (src : string) : value =
  let v =
    Proxy_rim.run ~local_prims:t.local_prims ~transport:t.transport (fun () ->
        Eval.read_eval_all_direct ~prims:t.local_prims t.env src)
  in
  (match Env.lookup t.env name with
   | None -> Env.define t.env name v
   | Some _ -> ());
  Def_cache.store t.def_cache name v;
  v

(** Lazy env miss inlined into [eval_local]: on [Unbound_var name],
    fetch the def source and materialize it, then retry the expression.
    Bounds retries per name so a genuinely unbound name fails cleanly. *)
let eval_local (t : t) (src : string) : value =
  (* ONE CHEAP CACHE-CHECK PER EVAL (caching subsection). Once the cache
     is live (a def has been materialized), confirm the (branch,
     generation) coordinate is still current — a single HEAD-ish round
     trip (current-branch + gen-now), never an N-way hash diff. If a
     generation bump or branch switch makes any loaded def stale, evict
     its ENV binding so the next reference re-fetches it (the writer is
     the source of truth; the client cache never is). A def-free /
     pure-compute session is skipped entirely (nothing cached, zero
     round trips). *)
  (if Def_cache.is_active t.def_cache then (
     let stale = Def_cache.check t.def_cache in
     List.iter (fun n -> Hashtbl.remove t.env.bindings n) stale)
   else ());
  let seen = Hashtbl.create 4 in
  let rec go () =
    try
      Proxy_rim.run ~local_prims:t.local_prims ~transport:t.transport (fun () ->
          Eval.eval_string_direct ~prims:t.local_prims t.env src)
    with
    | Interp_error (Unbound_var name) ->
        if Hashtbl.mem seen name then raise (Interp_error (Unbound_var name))
        else (
          Hashtbl.replace seen name ();
          (* one cheap cache-check round trip before re-fetching: a
             branch switch invalidates the whole cache and a generation
             bump drops stale defs, so a def that changed on the server
             is re-fetched on next use. *)
          let stale = Def_cache.check t.def_cache in
          List.iter (fun n -> Hashtbl.remove t.env.bindings n) stale;
          (match Def_cache.lookup t.def_cache name with
           | Some _ ->
               (* still materialized at the current generation: retry the
                  expression (the env already binds the name; if it does not
                  the [seen] guard below re-raises rather than looping). *)
               go ()
           | None -> (
               match fetch_def t name with
               | Some def_src ->
                   ignore (load_def t name def_src);
                   go ()
               | None ->
                   Def_cache.remove t.def_cache name;
                   raise (Interp_error (Unbound_var name)))))
  in
  go ()

(** Hybrid placement entry point (borge/remote-eval.borg placement).
    Decide once per expression whether it runs LOCAL under the proxy rim
    or is POSTed REMOTE to /api/eval wholesale. The decision is logged
    so the boundary is visible — no silent routing.

    - Local (pure, or <= threshold individually-proxied prims): eval
      under the proxy rim with lazy def materialization ([eval_local]).
    - Remote (transaction-wrapped or prim-dense): hand the WHOLE source
      to the [remote_eval] hook (a POST /api/eval). If no hook is
      configured we RAISE rather than silently run locally — a
      (with-transaction …) CANNOT run locally without corrupting the
      store (locks don't span HTTP), so pretending to is never safe. *)
let eval (t : t) (src : string) : value =
  match Placer.place ~cfg:(placer_cfg t) src with
  | Placer.Local -> eval_local t src
  | Placer.Remote (target, _reason) -> (
      match t.remote_eval with
      | Some f -> f src
      | None ->
          failwith
            (Printf.sprintf
               "pp-remote: expression must run remotely (%s) but no remote-eval \
                transport is configured — not running it locally"
               target))

(** Create a client bound to a [transport] against one image. No
    network is touched here (boot is lazy), so construction is cheap
    and pure. Pass [~remote_eval] (a whole-expression POST /api/eval) to
    enable hybrid placement; without it a Remote placement (e.g. a
    (with-transaction …)) raises rather than silently running locally. *)
let str_arg name v =
  match v with
  | VStr s -> s
  | VSymbol s -> s
  | _ ->
      raise
        (Ast.Interp_error
           (Ast.Prim_error (name, "expected string/symbol argument", "bad-args")))

let emit_error name msg =
  raise (Ast.Interp_error (Ast.Prim_error (name, msg, "error")))

(* ---- Route table + request-context prims (agent-route-table host
   seam, ported to the client). Identical Nopales signatures to the
   image's route / route/lookup / route/dispatch, host-local backing
   (client route table + thin request context vs the image's Postgres
   routes + fat Request_context). libs/hyper runs unchanged on both.

   The only adaptation from agent/runtime.ml: the rim that services
   Prim_call inside a dispatched handler is the PROXY rim (forwarding
   to /api/prim), not the agent's local host-prim table. The route /
   dispatch / template machinery itself is env-only — it has zero
   dependency on which rim services the handler's prim calls. *)

(** [apply_value_raw] — apply a Nopales function value to host-side
    value args directly, assuming a rim is ALREADY active (the caller
    holds nothing: the proxy rim is non-reentrant per fiber but the
    caller is the rim's own continuation). Used by prims that run
    inside an eval (route/dispatch invokes a handler from within the
    rim). Args NEVER go through the reader, so lambda values and
    arbitrary JSON strings are safe. *)
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
  Eval.eval_string_direct ~prims:t.local_prims t.env src

(** Apply a Nopales function value to host-side value args under the
    proxy rim — for OCaml callers OUTSIDE an active eval (the HTTP
    surface renders templates). Prims running inside an eval use
    [apply_value_raw] instead. *)
let apply_value (t : t) (fn : Ast.value) (args : Ast.value list)
    : Ast.value =
  Proxy_rim.run ~local_prims:t.local_prims ~transport:t.transport (fun () ->
      apply_value_raw t fn args)

let require_request (t : t) : request_shape =
  match !(t.current_request) with
  | Some r -> r
  | None ->
      emit_error "request" "no active request (call inside route/dispatch)"

(** [dispatch_route_raw] — the shared route/dispatch body. Assumes a
    rim is active (route/dispatch prim runs inside an eval; the HTTP
    gate uses the [dispatch_route] wrapper). Returns the rendered
    raw-HTML VStr the browser would receive. *)
let dispatch_route_raw (t : t) ~handler ~method_ ~path ~params ~headers
    ~body : Ast.value =
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
    the route table). Runs under the proxy rim so the handler's prim
    calls forward to /api/prim. *)
let dispatch_route ~(rt : t) ~handler ~method_ ~path ~params ~headers
    ~body : Ast.value =
  Proxy_rim.run ~local_prims:rt.local_prims ~transport:rt.transport (fun () ->
      dispatch_route_raw rt ~handler ~method_ ~path ~params ~headers ~body)

(** Resolve a URL path to its handler name via the client route table. *)
let lookup_route ~(rt : t) (path : string) : string option =
  Hashtbl.find_opt rt.routes path

(** Call a Nopales template (zero-or-more-arg fn) by name and return the
    rendered raw-HTML string. For OCaml callers outside an eval (the
    HTTP surface renders fragment templates). Returns [None] if the
    template is unbound or errors. *)
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
    hx-request?, session/last-turn-parts) PLUS the lens-browsing prims
    (lens/list, lens/read) so the Nopales-authored surface can register
    routes and browse the local lens. All are LOCAL prims (they operate
    on the client's env / route table / lens, never the wire), so they
    go through [add_prim] and the placer treats them as not-crossing. *)
let register_route_prims (t : t) : unit =
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
          match rest with _ :: _ :: VStr b :: _ -> b | _ -> ""
        in
        dispatch_route_raw t ~handler ~method_:meth ~path ~params
          ~headers ~body
    | _ ->
        emit_error "route/dispatch"
          "usage: (route/dispatch handler method path [params] [headers] [body])"
  in
  add_prim t "route/dispatch" route_dispatch;
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
  let hx_request_p _args =
    let r = require_request t in
    let from_headers =
      match List.assoc_opt "hx-request" r.headers with
      | Some v when v <> "" && v <> "false" -> true
      | _ -> false
    in
    let from_params =
      match List.assoc_opt "hx-request" r.params with
      | Some v when v <> "" && v <> "false" -> true
      | _ -> false
    in
    VBool (from_headers || from_params)
  in
  add_prim t "hx-request?" hx_request_p;
  let last_turn_parts _args = t.last_turn_parts in
  add_prim t "session/last-turn-parts" last_turn_parts;
  (* lens/list -> JSON array of {"path","kind","name"} over the lens
     substrate. Empty array when no lens is wired. Kind is derived from
     the tree path (b.<branch>.fn|type|route.<name> | data.<...>). *)
  let lens_list _args =
    match t.lens with
    | None -> VStr "[]"
    | Some lens ->
        let nodes = Lens_store.node_manifest ~lens in
        let kind_of_path p =
          if String.length p >= 2 && String.sub p 0 2 = "b." then begin
            let segs = String.split_on_char '.' p in
            (* b.<branch>.<kind>.<name> -> segs.(2) *)
            match segs with
            | _ :: _ :: k :: _ -> k
            | _ -> "code"
          end else if String.length p >= 5 && String.sub p 0 5 = "data."
          then "data"
          else "node"
        in
        let name_of_path p =
          let segs = String.split_on_char '.' p in
          match List.rev segs with
          | n :: _ -> n
          | [] -> p
        in
        let items =
          List.map
            (fun (p, _h) ->
              `Assoc
                [ ("path", `String p)
                ; ("kind", `String (kind_of_path p))
                ; ("name", `String (name_of_path p))
                ])
            nodes
        in
        VStr (Yojson.Safe.to_string (`List items))
  in
  add_prim t "lens/list" lens_list;
  (* lens/read path -> VStr source | VNil. For a code node returns the
     def source (code_src_of_envelope); for a data node returns the
     row's body JSON (datum_body_of_envelope); else the raw envelope. *)
  let lens_read args =
    match t.lens, args with
    | None, _ -> VNil
    | Some lens, [ path ] ->
        let p = str_arg "lens/read" path in
        (match Lens_store.get ~lens p with
         | Some env ->
             (match Lens_store.code_src_of_envelope env with
              | Some s -> VStr s
              | None ->
                  (match Lens_store.datum_body_of_envelope env with
                   | Some s -> VStr s
                   | None -> VStr env))
         | None -> VNil)
    | _ -> emit_error "lens/read" "usage: (lens/read path)"
  in
  add_prim t "lens/read" lens_read

(* ---- agent-loop host seam (borge/remote-eval.borg agent-loop) ----

   The client can run the LLM agent turn LOCALLY: the shared loop
   (llm/lib/loop.ml) + provider (llm/lib/provider.ml) are pure OCaml
   and link into pp-remote; the API key is a value read from the
   environment (same AGENT_* vars the agent reads). What stays remote
   is the DATA PLANE: a tool body's prim calls go through the proxy
   rim, so (get 'user id) inside a Nopales tool crosses to /api/prim
   exactly as a REPL eval would. The surface below mirrors the agent's
   Runtime surface (fire / eval_tool_body / register_builtin_tool) —
   same shapes, proxy rim instead of the local host-prim table. *)

(** Current working directory as seen by the read tool's path
    resolution. Immutable per process (pp-remote never chdir-s).
    Refuses ".." escapes — the same v0 sandbox posture as the agent. *)
let cwd = ref (Sys.getcwd ())

let resolve_path p =
  let p =
    if String.length p > 0 && p.[0] = '/' then p else Filename.concat !cwd p
  in
  let parts =
    String.split_on_char '/' p
    |> List.filter (fun s -> s <> "" && s <> ".")
  in
  if List.exists (fun s -> s = "..") parts then
    emit_error "fs/path" ("path escapes sandbox: " ^ p);
  "/" ^ String.concat "/" parts

(** Register the event + plugin-tool prims: (on "event" fn) hooks and
    (register-tool name params fn). Local prims over the client's own
    hashtbls — the loop's tool_call/tool_result/input events fire
    through [fire] below, and a Nopales plugin tool's closure evals
    under the proxy rim (its DB prim calls proxy). Same Nopales
    signatures as the agent's, so plugin sources transfer. *)
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
        Hashtbl.replace t.tools name { doc = ""; run = fn };
        if not (List.mem name t.tool_order) then
          t.tool_order <- t.tool_order @ [ name ];
        VBool true
    | _ -> emit_error "register-tool" "usage: (register-tool name params fn)"
  in
  add_prim t "register-tool" reg_tool

(** Fire an event: run handlers in registration order, containing a
    raising handler into an {"error": …} value (mirrors agent
    Runtime.fire). *)
let fire (t : t) (ev : string) (payload : Ast.value list) : Ast.value list =
  match Hashtbl.find_opt t.handlers ev with
  | None -> []
  | Some hs ->
      List.map
        (fun h ->
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

(** Run a plugin tool's closure with JSON args under the proxy rim,
    containing errors into a {"error": …} JSON string (mirrors agent
    Runtime.eval_tool_body). *)
let eval_tool_body (t : t) (name : string) (args_json : string) : Ast.value =
  match Hashtbl.find_opt t.tools name with
  | None ->
      VStr
        (Yojson.Safe.to_string
           (`Assoc [ ("error", `String ("no tool named " ^ name)) ]))
  | Some e ->
      (try apply_value t e.run [ VStr args_json ]
       with Ast.Interp_error ie ->
         VStr
           (Yojson.Safe.to_string
              (`Assoc
                [ ("error", `String (Ast.pp_interp_error ie)) ])))

(** Register a builtin host tool (read/bash): registry entry as a VPrim
    naming a LOCAL prim, so the loop dispatches builtins through the
    proxy rim's local table (no wire — builtin bodies are OCaml).
    Mirrors agent Runtime.register_builtin_tool. *)
let register_builtin_tool (t : t) (name : string)
    ~(run : Yojson.Safe.t -> string) : unit =
  Hashtbl.replace t.tools name
    { doc = "builtin"; run = VPrim ("__builtin__" ^ name) };
  if not (List.mem name t.tool_order) then
    t.tool_order <- t.tool_order @ [ name ];
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
    client's [Loop.host] — local tool registry, event fire, tool-body
    eval under the proxy rim, and builtin prim dispatch through the
    proxy rim (builtin bodies run as local prims; a plugin tool's DB
    prim calls forward to /api/prim). This is the whole seam: the loop
    itself is host-agnostic. *)
let loop_host (t : t) : Loop.host =
  {
    Loop.tool_order = (fun () -> t.tool_order);
    tool_entry = (fun name -> Hashtbl.find_opt t.tools name);
    fire = fire t;
    eval_tool_body = eval_tool_body t;
    call_prim =
      (fun name args ->
         Proxy_rim.run ~local_prims:t.local_prims ~transport:t.transport
           (fun () -> (Hashtbl.find t.local_prims name) args));
  }

(** Register the shared soup/ tree prims (lifted into pure/prims_soup.ml
    so both hosts register the same set). [Prims_soup.register_pure_prims]
    fills the prim table; we bind each soup/ name as a VPrim in env so
    Nopales resolves it, and add each to [pure_prims] so the placer
    treats it as local (soup prims are pure — they never cross the
    wire). Mirrors agent/runtime.ml register_soup_prims. *)
let register_soup_prims (t : t) : unit =
  Prims_soup.register_pure_prims t.local_prims;
  Hashtbl.iter
    (fun name _ ->
      if String.length name >= 5 && String.sub name 0 5 = "soup/" then begin
        Env.define t.env name (Ast.VPrim name);
        t.pure_prims <- name :: t.pure_prims
      end)
    t.local_prims

(** Evaluate all top-level forms in [src] into the shared env, under the
    proxy rim. Used to boot-load Nopales packages (libs/html, libs/hyper,
    the surface source) as SOURCE — the same env as the REPL, so the
    surface is redefinable live (borge/remote-eval.borg surfaces). *)
let load_source ~(rt : t) (src : string) : unit =
  Proxy_rim.run ~local_prims:rt.local_prims ~transport:rt.transport (fun () ->
      ignore (Eval.read_eval_all_direct ~prims:rt.local_prims rt.env src))

(** Set the lens record on the client (after [make]) so lens/list +
    lens/read surface prims browse the substrate. *)
let set_lens (t : t) (lens : Lens_store.lens) : unit =
  t.lens <- Some lens


let make ~(transport : transport) ?(remote_eval : (string -> value) option)
    ?(lens_lookup : (string -> string option) option) () : t =
  let t =
    {
      env = Env.create ();
      local_prims = Eval.make_prim_table ();
      transport;
      def_cache = Def_cache.create ~transport ();
      pure_prims = [];
      remote_eval;
      lens_lookup;
      lens = None;
      routes = Hashtbl.create 16;
      current_request = ref None;
      last_turn_parts = Ast.VNil;
      tools = Hashtbl.create 16;
      tool_order = [];
      handlers = Hashtbl.create 8;
    }
  in
  register_pure_prims t;
  register_soup_prims t;
  register_route_prims t;
  register_event_prims t;
  register_proxy_prims t;
  (* Minimal stdlib: the [let*] sequential-binding macro libs/hyper
     uses, without the heavy libs/std (which depends on image-only
     prims). Mirrors agent/runtime.ml boot_stdlib. *)
  load_source ~rt:t {|
(defmacro let* (bindings body)
  (if (null? bindings)
      (list (quote let) (list) body)
      (list (quote let)
            (list (list (car (car bindings)) (car (cdr (car bindings)))))
            (list (quote let*) (cdr bindings) body))))
|};
  t

(** [register_lens_reads t ~lens] overrides the [get] and [tree/get]
    proxy prims with LOCAL handlers that read from the lens substrate
    FIRST, proxying to the image on miss. This is the Phase-1
    local-first data path: working-set reads [(get 'type id)] and
    [(tree/get path)] resolve from the filesystem JSON the lens
    materialized, so they stay local offline. Indexed reads [(rows …)],
    [(tree/find-by …)] keep proxying (the server is the index —
    local-first.borg local-substrate).

    A transport FAILURE (the Eio HTTP transport raises [Eio.Io] on a
    refused connection) is caught and degrades to [VNil] ("not found")
    rather than crashing — offline grace. The lens is a WORKING COPY; a
    miss + offline is "not in the working set," not an error. *)
let register_lens_reads (t : t) ~(lens : Lens_store.lens) : unit =
  (* Mirror the server's [coerce_sym_args]: quoted symbols ('type) arrive
     as VSymbol; the server's CRUD handlers coerce to VStr, so the lens
     handlers must too or the [VStr] arg match would miss. *)
  let coerce = function Ast.VSymbol s -> Ast.VStr s | v -> v in
  let proxy_or lens_hit args =
    match lens_hit () with
    | Some v -> v
    | None ->
        (* miss: proxy. Catch transport exceptions (offline) → VNil. *)
        (match (try t.transport "get" args with _ -> Error (Failure "offline")) with
         | Ok v -> v
         | Error _ -> Ast.VNil)
  in
  (* (get 'type id) -> VStr fields-json | VNil. Reads the datum node
     envelope at [data.u.<user>.<type>.<id>] from the lens, extracts the
     body (the row's fields), returns it as VStr — matching [Crud.get]. *)
  add_prim t "get"
    (fun args ->
      match List.map coerce args with
      | [ Ast.VStr type_name; Ast.VStr id ] ->
          let path = Lens_store.datum_path ~user:lens.owner ~type_name ~id () in
          proxy_or
            (fun () ->
              match Lens_store.get ~lens path with
              | Some env ->
                  (match Lens_store.datum_body_of_envelope env with
                   | Some s -> Some (Ast.VStr s)
                   | None -> None)
              | None -> None)
            args
      | _ -> Ast.VNil)
  (* (tree/get path) -> VStr node-envelope-json | VNil. Returns the full
     envelope verbatim (matches [Crud.tree_get]). *)
  ; add_prim t "tree/get"
    (fun args ->
      match List.map coerce args with
      | [ Ast.VStr path ] ->
          proxy_or
            (fun () ->
              match Lens_store.get ~lens path with
              | Some env -> Some (Ast.VStr env)
              | None -> None)
            args
      | _ -> Ast.VNil)

(** ---- Stepping / trace debugger over one remote client ----

    (borge/remote-eval.borg stepping: set bp, step, inspect frame,
    continue.) The driver runs [Explain.trace] over THIS client's env +
    local pure prim table + transport, so local pure compute stays local
    and every other prim round trip crosses the same [transport] the
    fast eval would — the server is unaware a breakpoint fired; the
    data plane is untouched. *)

(** One-pass stepped run of [src] over this client. Returns the trace
    (local steps interleaved with remote round trips) plus a drivable
    stepper to set breakpoints / step / continue over it. *)
let stepper_of (t : t) (src : string) :
    Explain.trace_result * Explain.stepper =
  let tr =
    Explain.trace ~local_prims:t.local_prims ~transport:t.transport
      ~init_env:t.env ~src
  in
  (tr, Explain.stepper_of_trace tr)
