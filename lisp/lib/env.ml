(** Pricklypear Lisp — Lexical environment.

    Lexically scoped bindings. Each env has a parent (set at closure
    capture time) and a hashtable of name -> value. Lookup walks up the
    parent chain.

    Per spec [lisp-interpreter.eval]: the evaluator is pure w.r.t. I/O;
    the env is mutable (binding updates), which is fine — it's local to
    a session, not persisted. Persistence happens via [(define ...)]
    which, in the IMAGE layer, ALSO writes to the store. *)
open Ast

type env = Ast.env

(** Create a fresh env. [~parent] (default [None]) is the lexical parent
    captured at closure-creation time; lookups walk up this chain. *)
let create ?(parent = None) () : env =
  { bindings = Hashtbl.create 16; parent }

(** Create a child env whose parent is [e]. Bindings defined in the child
    shadow (not mutate) names in [e]. *)
let child (e : env) : env = create ~parent:(Some e) ()

(** Read-set tracking for dependency-tracked caching
    (image.product-theme). While armed, every successful [lookup]
    posts the resolved name to this hook. Armed ONLY by
    [Session.refresh_ui_shell] around the (ui/css)/(ui/nav) evals so
    a define can bust the shell cache iff the defined name was
    actually read — no TTL, no prefix guess. Global mutable, and NOT
    fiber-isolated (the kernel stays Eio-free): concurrent requests'
    lookups land in whichever collector is armed. Session arms the
    hook with capture-restore (i-1788670103-5) so an overlapping
    refresh can no longer drop another's collector; cross-fiber
    postings only INFLATE a read-set (superset ⇒ cache busts more
    often, never serves stale). [None] costs one ref-read per lookup. *)
let lookup_hook : (string -> unit) option ref = ref None

(** Force a [VThunk] cell (lisp-interpreter.lazy-env-thunks).
    Returns the memoized value on [Forced], replays the memoized
    exception on [Failed], raises Interp_error on a [Forcing] cycle,
    and on [Unforced] runs the payload's OWN evaluator (which is how
    the image forces under its prim rim without the pure kernel
    learning about I/O). Values other than VThunk pass through. *)
(** [retryable_thunk_exn exn] - failures that are functions of the
    CALLING CONTEXT, not of the body's own semantics (card 4c6d3e9f,
    generalizing the 2026-10-05 budget carve-out of d101502). These are
    memoization-poisoners: a body that would force fine under a fresh
    budget / a fully-loaded package / a seat with the right tier must
    not be pinned Failed by the request that happened to touch it
    first. Classes:
      - Budget_exceeded: the classic (see audit-2026-10-05).
      - Prim_error "denied": seat/delegation-tier denies (reseed,
        deftype, ws writes) - the same body forces fine as admin or
        after tier changes; memoizing pinned the filmstrip library for
        delegated seats (2026-10-14 load-library abort at ws-init!).
      - Unbound_var: a package registering forms in order leaves later
        names unbound while a mid-package thunk is first forced;
        memoizing that poisons the binding even after the load
        completes. Deterministic typos merely retry (cheap, and the
        error stays visible on every touch - which for a typo is what
        you want anyway). *)
let retryable_thunk_exn (exn : exn) : bool =
  match exn with
  | Interp_error Budget_exceeded -> true
  | Interp_error (Prim_error (_, _, "denied")) -> true
  | Interp_error (Unbound_var _) -> true
  | _ -> false

(* In-flight forces owned by THIS call stack (card 8660796a). A cell
   observed [Forcing] whose payload is physically one of ours is a genuine
   self-reference; one owned by a DIFFERENT force is an abandoned in-flight
   cell (the owner raised, or a define_thunk refreshed the binding
   mid-force) and is settled here instead of raising the historical
   "cyclic thunk force", which used to 500 every anonymous request through
   a redefined name until the session restarted. *)
let in_flight : thunk_payload list ref = ref []

let owns_force (payload : thunk_payload) : bool =
  List.exists (fun p -> p == payload) !in_flight

let release_force (payload : thunk_payload) : unit =
  let rec drop = function
    | [] -> []
    | p :: rest -> if p == payload then rest else p :: drop rest
  in
  in_flight := drop !in_flight

let rec force_value (v : value) : value =
  match v with
  | VThunk cell ->
      (match !cell with
       | Forced v -> v
       | Failed exn -> raise exn
       | Forcing owner ->
           if owns_force owner then
             raise
               (Interp_error
                  (User_error
                     ("cyclic thunk force (binding refers to itself)",
                      "cyclic-thunk")))
           else begin
             (* Another fiber's force abandoned this cell (it raised and
                left the memo behind) — settle it and retry here. *)
             cell := Unforced owner;
             force_value v
           end
       | Unforced payload ->
           cell := Forcing payload;
           in_flight := payload :: !in_flight;
           (* Only the OWNER settles the cell (physical-equality guard):
              a define_thunk refreshing the binding mid-force wins over the
              stale result computed by the superseded force. *)
           let settle_value (result : value) =
             release_force payload;
             (match !cell with
              | Forcing p when p == payload -> cell := Forced result
              | _ -> ());
             result
           in
           let settle_exn (exn : exn) =
             release_force payload;
             (* Memoize only DETERMINISTIC failures. Budget_exceeded is
                context-dependent — the same body forces fine under a
                fresh budget, so memoizing it poisons the binding for
                the whole session (image.audit-2026-10-05: one /notes
                render near budget end failed the force of notes/ui/*
                thunks and every later /notes request 500'd at full
                budget until rehydrate). Leave the cell Unforced so a
                later lookup retries; everything else replays. *)
             (match !cell with
              | Forcing p when p == payload ->
                  (match exn with
                   | _ when retryable_thunk_exn exn -> cell := Unforced payload
                   | _ -> cell := Failed exn)
              | _ -> ());
             raise exn
           in
           (match
              payload.thunk_force payload.thunk_env payload.thunk_body
            with
            | v ->
                (match force_value v with
                 | result -> settle_value result
                 | exception exn -> settle_exn exn)
            | exception exn -> settle_exn exn))
  | v -> v

(** Bind [name] in [e] to a not-yet-evaluated body (image load_defs).
    [force] is the evaluator used when the binding is first touched. *)
let define_thunk (e : env) (name : string)
    ~(force : env -> expr -> value) (body : expr) : unit =
  let payload = { thunk_body = body; thunk_env = e; thunk_force = force } in
  let cell =
    match Hashtbl.find_opt e.bindings name with
    | Some (VThunk cell) ->
        cell := Unforced payload;
        cell
    | Some _ | None -> ref (Unforced payload)
  in
  Hashtbl.replace e.bindings name (VThunk cell)

(** [lookup e name] returns [Some v] if [name] is bound in [e] or any
    ancestor, else [None]. Walks the parent chain to the root.
    VThunk bindings are forced (and memoized) here — the single
    observation point, so user code never receives a VThunk. *)
let rec lookup (e : env) (name : string) : value option =
  match Hashtbl.find_opt e.bindings name with
  | Some v ->
      (match !lookup_hook with Some f -> f name | None -> ());
      Some (force_value v)
  | None ->
      (match e.parent with
       | Some p -> lookup p name
       | None -> None)


(** [find_raw e name] returns the binding cell for [name] in [e] or any
    ancestor WITHOUT forcing VThunk cells. card b851a9a0: the dispatch-time
    unbound-binding heal must distinguish a genuinely missing binding from
    a bound thunk whose force raised — the latter surfaces, never re-binds.
    card 6b8edbe4: it also lets a caller holding one env COPY a binding into
    another (the per-request snapshot a handler evaluates in vs the
    session's live env). *)
let rec find_raw (e : env) (name : string) : value option =
  match Hashtbl.find_opt e.bindings name with
  | Some v -> Some v
  | None -> (match e.parent with Some p -> find_raw p name | None -> None)

let bound_raw (e : env) (name : string) : bool =
  match find_raw e name with Some _ -> true | None -> false
(** Bind [name] to [v] in [e], replacing any existing binding locally
    (does not touch parents). Mirrors the image-layer [(define ...)]
    which in the IMAGE also persists to the store. *)
let define (e : env) (name : string) (v : value) : unit =
  Hashtbl.replace e.bindings name v

(** Fold [f] over every binding in [e] and its ancestor chain, in
    child-to-parent order. Used by reflection primitives like
    [(list-bindings)]. *)
let rec fold (e : env) (f : string -> value -> 'a -> 'a) (acc : 'a) : 'a =
  let local = Hashtbl.fold f e.bindings acc in
  match e.parent with
  | Some p -> fold p f local
  | None -> local

