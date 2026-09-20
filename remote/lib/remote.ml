(** remote/lib/remote.ml (borge/local-first.borg client).

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
    server-asserted (whoami/current-author come back from the server).

    The host seam (request shape, route table + dispatch, template
    calls, event/tool registry, [Loop.host], boot stdlib macro) lives
    in hostkit/host_seam.ml — ONE home shared with the agent
    (i-1788671304-d). This module is the client's [run_in_rim] (the
    proxy rim) plus the client-only layers: proxy prim binding, the
    lazy def cache, hybrid placement, the lens substrate, stepping. *)

open Pricklypear_lisp
open Ast

type transport = Proxy_rim.transport

type t = {
  seam : Host_seam.t;
  transport : transport;
  (* definitions materialized locally, keyed (branch, generation): the
     def cache (caching subsection). Bounds server fetches on a miss and
     invalidates on generation change / branch switch. *)
  def_cache : Def_cache.t;
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
}

let value_to_json = Prims_pure.value_to_json
let json_to_value = Prims_pure.json_to_value

let str_arg = Host_seam.str_arg

let emit_error = Host_seam.emit_error

(** Register a prim BOTH in the dispatch table and as a VPrim binding
    in the env (Nopales resolves prims via the env, like the image
    session binds its catalog). The shared seam records the name as a
    LOCAL prim (the placer keeps it off the wire). *)
let add_prim (t : t) (name : string) (fn : value list -> value) : unit =
  Host_seam.add_prim t.seam name fn

(** The [Placer.cfg] for this client: the local prims registered
    through [add_prim] are the names the placer must NOT count as
    proxy-round-trips. The rest — proxyable prims and user-function
    references — walk as prim calls. *)
let placer_cfg (t : t) : Placer.cfg =
  Placer.default_cfg ~local_prims:(Host_seam.registered_local_prims t.seam)

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
  Env.define t.seam.env name (VPrim name)

(** Register the frozen pure prim surface from the ONE shared catalog
    (pure/prims_pure.ml [Prims_pure.catalog], i-1788670799-f — the
    per-host hand copies had already diverged: this one registered
    string-split twice and skipped the math/render towers). Same code
    as the image links (Prims_pure), so local semantics are identical —
    the proxy rim's local table is the "pure compute stays local" half
    of hybrid placement. add_prim also records each name in the seam's
    [registered] list so the placer keeps them OFF the wire. Options that
    keep a prim OUT of this table simply proxy it; nothing depends on
    the split being exhaustive. *)
let register_pure_prims (t : t) : unit =
  Host_seam.register_pure_prims t.seam

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
    ; "find-rows"; "find-rows-author"
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
    t.seam.run_in_rim (fun () ->
        Eval.read_eval_all_direct ~prims:t.seam.prims t.seam.env src)
  in
  (match Env.lookup t.seam.env name with
   | None -> Env.define t.seam.env name v
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
     List.iter (fun n -> Hashtbl.remove t.seam.env.bindings n) stale)
   else ());
  let seen = Hashtbl.create 4 in
  let rec go () =
    try
      t.seam.run_in_rim (fun () ->
          Eval.eval_string_direct ~prims:t.seam.prims t.seam.env
            ~budget:(Eval.make_budget ()) src)
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
          List.iter (fun n -> Hashtbl.remove t.seam.env.bindings n) stale;
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

(** Hybrid placement entry point (borge/local-first.borg placement).
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

(** ---- Lens-browsing surface prims (client-only; registered at make,
   after the shared route prims) ----

    Register the lens-browsing prims (lens/list, lens/read) so the
    Nopales-authored surface can browse the local lens. Both are LOCAL
    prims (they operate on the client's lens, never the wire), so they
    go through [add_prim] and the placer treats them as not-crossing.
    Registered at make even WITHOUT a lens: lens/list answers "[]" and
    lens/read VNil, so the /code surface page works lens-less. *)
let register_lens_prims (t : t) : unit =
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

(** Current working directory as seen by the read tool's path
    resolution. Immutable per process (pp-remote never chdir-s).
    Refuses ".." escapes — the same v0 sandbox posture as the agent.
    Shared logic in Host_seam.resolve_path. *)
let cwd = ref (Sys.getcwd ())

let resolve_path (p : string) : string = Host_seam.resolve_path ~cwd p

(** Evaluate all top-level forms in [src] into the shared env, under the
    proxy rim. Used to boot-load Nopales packages (libs/html, libs/hyper,
    the surface source) as SOURCE — the same env as the REPL, so the
    surface is redefinable live (borge/local-first.borg surfaces). *)
let load_source ~(rt : t) (src : string) : unit =
  ignore
    (rt.seam.run_in_rim (fun () ->
         ignore (Eval.read_eval_all_direct ~prims:rt.seam.prims rt.seam.env src);
         Ast.VNil))

(** Set the lens record on the client (after [make]) so lens/list +
    lens/read surface prims browse the substrate. *)
let set_lens (t : t) (lens : Lens_store.lens) : unit =
  t.lens <- Some lens

(** Apply a Nopales function value to host-side value args under the
    proxy rim — for OCaml callers OUTSIDE an active eval (the HTTP
    surface renders templates). Prims running inside an eval use
    [Host_seam.apply_value_raw] instead (the rim is already active).
    Shared implementation: Host_seam.apply_value. *)
let apply_value (t : t) (fn : Ast.value) (args : Ast.value list) : Ast.value =
  Host_seam.apply_value t.seam fn args

(* The route table + request-context prims (route / route/lookup /
   route/dispatch / query-param / request-method / hx-request? /
   session/last-turn-parts) live in Host_seam.register_route_prims —
   the agent-route-table host seam, ported to the client so the
   Nopales-authored surface runs unchanged. The only adaptation: the
   rim that services Prim_call inside a dispatched handler is the
   PROXY rim (forwarding to /api/prim), not the agent's local
   host-prim table — the [run_in_rim] hook. *)
let dispatch_route ~(rt : t) ~handler ~method_ ~path ~params ~headers
    ~body : Ast.value =
  Host_seam.dispatch_route ~rt:rt.seam ~handler ~method_ ~path ~params
    ~headers ~body

let lookup_route ~(rt : t) (path : string) : string option =
  Host_seam.lookup_route ~rt:rt.seam path

let call_template ~(rt : t) (name : string) (args : Ast.value list)
    : string option =
  Host_seam.call_template ~rt:rt.seam name args

(** Fire an event: run handlers in registration order, containing a
    raising handler into an {"error": …} value. Shared implementation:
    Host_seam.fire. *)
let fire (t : t) (ev : string) (payload : Ast.value list) : Ast.value list =
  Host_seam.fire t.seam ev payload

(** Run a plugin tool's closure with JSON args under the proxy rim,
    containing errors into a {"error": …} JSON string. Shared
    implementation: Host_seam.eval_tool_body. *)
let eval_tool_body (t : t) (name : string) (args_json : string) : Ast.value =
  Host_seam.eval_tool_body t.seam name args_json

(** Register a builtin host tool (read/bash): registry entry as a VPrim
    naming a LOCAL prim, so the loop dispatches builtins through the
    proxy rim's local table (no wire — builtin bodies are OCaml).
    Shared implementation: Host_seam.register_builtin_tool. *)
let register_builtin_tool (t : t) (name : string)
    ~(run : Yojson.Safe.t -> string) : unit =
  Host_seam.register_builtin_tool t.seam name ~run

(** Host adapter for the shared agent turn loop (llm/lib/loop.ml): the
    client's [Loop.host] — local tool registry, event fire, tool-body
    eval under the proxy rim, and builtin prim dispatch through the
    proxy rim (builtin bodies run as local prims; a plugin tool's DB
    prim calls forward to /api/prim). This is the whole seam: the loop
    itself is host-agnostic. *)
let loop_host (t : t) : Loop.host = Host_seam.loop_host t.seam

(** ---- Stepping / trace debugger over one remote client ----

    (borge/local-first.borg stepping: set bp, step, inspect frame,
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
    Explain.trace ~local_prims:t.seam.prims ~transport:t.transport
      ~init_env:t.seam.env ~src
  in
  (tr, Explain.stepper_of_trace tr)

(** Create a client bound to a [transport] against one image. No
    network is touched here (boot is lazy), so construction is cheap
    and pure. Pass [~remote_eval] (a whole-expression POST /api/eval) to
    enable hybrid placement; without it a Remote placement (e.g. a
    (with-transaction …)) raises rather than silently running locally. *)
let make ~(transport : transport) ?(remote_eval : (string -> value) option)
    ?(lens_lookup : (string -> string option) option) () : t =
  let prims = Eval.make_prim_table () in
  let run_in_rim f = Proxy_rim.run ~local_prims:prims ~transport f in
  let cwd = ref (Sys.getcwd ()) in
  let seam = Host_seam.make ~prims ~run_in_rim ~cwd () in
  let t =
    {
      seam;
      transport;
      def_cache = Def_cache.create ~transport ();
      remote_eval;
      lens_lookup;
      lens = None;
    }
  in
  register_pure_prims t;
  Host_seam.register_soup_prims t.seam;
  Host_seam.register_route_prims t.seam;
  register_lens_prims t;
  Host_seam.register_event_prims t.seam;
  register_proxy_prims t;
  (* Minimal stdlib: the [let*] sequential-binding macro libs/hyper
     uses, without the heavy libs/std (which depends on image-only
     prims). The shared host stdlib (Host_seam.boot_stdlib). *)
  load_source ~rt:t Host_seam.boot_stdlib;
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
