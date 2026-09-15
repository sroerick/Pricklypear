(** Session record and HTTP request types (shared by Session + Agent_tools). *)
type http_method = [ `GET | `POST ]

type request = {
  method_ : http_method;
  path : string;
  params : (string * string) list;  (** form fields win over query *)
  body : string;
  (** Lowercase header names → values (for HX-Request, etc.). *)
  headers : (string * string) list;
}

(** Impact of a successful define/install (image.inspector I2). *)
type impact_event = {
  name : string;
  at : float;
  branch : string;
  author : string;
  dependents : string list;
  routes : string list;
  status : string; (** pending | smoked | dismissed *)
}

type change_event = {
  op : string; (** define | route | delete *)
  name : string;
  at : float;
  branch : string;
  author : string;
}

(** image.product-theme: the product shell cache (nav HTML + theme CSS).
    Grouped sub-record (i-1788671109-1): these five fields are written
    from exactly one concern area — the refresh_ui_shell dependency-
    tracked cache and its suppress/retry windows. Mutate PER FIELD only
    (t.ui_shell.nav_html <- …); never replace the sub-record wholesale,
    so in-flight readers keep single-field semantics. *)
type ui_shell = {
  (** Cached HTML from ui/nav when defined; None = use OCaml default nav. *)
  mutable nav_html : string option;
  (** Cached CSS from (ui/css) when defined; None = emergency_css in http. *)
  mutable css : string option;
  (** image.product-theme: shell cache invalidation is DEPENDENCY-TRACKED.
      Each successful shell eval records the exact binding names it read
      (via Env.lookup_hook); a define busts the cache iff the defined name
      is in this set. No TTL, no name-prefix heuristic. Empty set + cached
      value = never busted by define. Reset on checkout (per-branch). *)
  mutable reads : string list;
  (** image.http-robustness invariant 3: refresh_ui_shell runs on every
      request. When a shell eval FAILS, do not re-attempt + re-warn per
      request: suppress retry for retry_at seconds (single warn per
      window). Reset to 0.0 on checkout and on successful refresh. *)
  mutable retry_at : float;
  (** When true, the eval_one define-interceptor suppresses its
      refresh_ui_shell call. Used during seed_stdlib so that defining
      ui/nav mid-seed (before notes/capture/nav is bound) does not
      trigger an eager refresh that fails with unbound: notes/capture/nav.
      The boot sequence calls refresh_ui_shell once AFTER seeding completes. *)
  mutable suppress_refresh_on_define : bool;
}

(** Interpreter-driver state: the env (with its swap-on-rehydrate
    invariant), the prim rim, and the load/rehydrate bookkeeping.
    Grouped sub-record (i-1788671109-1). Mutate PER FIELD only
    (t.eval_state.env <- …); never replace the sub-record wholesale —
    in particular the env swap below is an inner-field assignment so
    closures that captured the old env keep reading it (invariant 4a). *)
type eval_state = {
  (* image.http-robustness invariant 4a: env is mutable so rehydrate can
     SWAP it to a fresh record (new Hashtbl, same parent chain) instead
     of clearing the existing binding table in place. VLambda closures
     capture `env` by reference; clearing in place would
     corrupt an in-flight page's captured environment. Swapping leaves the old
     record intact for any closure that still holds it, so concurrent
     rehydrate cannot break a running page. *)
  mutable env : Pricklypear_lisp.Ast.env;
  (** image.per-identity-sessions: Store.visible_stamp captured at the
      last load_defs for the current branch. [ensure_fresh] compares
      the live stamp at attach time and rehydrates only on mismatch. *)
  mutable load_stamp : string;
  (** Sync prim table for [Control_rim.run] (H1.7). Interpreter is
      direct-style; I/O prims are serviced only at this rim. *)
  mutable prims : Primitives.t;
  (** lisp-interpreter.package-surface: while loading a lib.pp that opened
      with (package NAME (export …)), stamp new defines with this package
      name. None outside a package load / REPL package form. *)
  mutable current_package : string option;
  (** image.load-library-skip (2026-09-03): during interactive
      load-library / reseed, maps define-name -> fingerprint (pp_expr
      body + doc) of the branch's OWN stored defs; eval_form skips
      defines whose normalized body + doc are identical, so re-loading
      a big lib no longer re-evaluates + re-persists ~1000 unchanged
      defines (the boot seed got the same medicine via
      persist_if_absent). Kernel-prim + disabled-package names excluded;
      inherited names keep full eval (no-diff-no-pin unpin must stay
      reachable). None outside load-library; a nested load-string during
      the load window inherits it. *)
  mutable load_skip_names : (string, string) Hashtbl.t option;
  (** tmp/eval-fast-path incremental rehydrate: Def_journal cursor —
      the journal seq this session has consumed. ensure_fresh drains
      entries newer than the cursor and rebinds exactly those defs,
      instead of re-fetching + re-parsing the whole visible chain
      (~2800 bodies, measured ~280-520ms per rehydrate). *)
  mutable def_journal_seq : int;
  (** tmp/eval-fast-path: parsed body per bound def thunk (load_defs +
      incremental rebind). Lets an incremental rehydrate RE-THUNK carried
      defs from cached bodies — resetting memoization and re-capturing
      the fresh env — without re-fetching/re-parsing from the DB.
      Memory: retains def ASTs after first force (they would otherwise
      be dropped by the memoized cell); bounded by the def count. *)
  mutable thunk_bodies : (string, Pricklypear_lisp.Ast.expr) Hashtbl.t;
}

type t = {
  mutable manifest : Pricklypear_common.Manifest.t;
  store : Store.t;
  (* In-memory route table mirroring view_bindings for the branch.
     Populated by load_defs_async at session-make time; (route ...)
     updates also live-persist to view_bindings AND update this table.
     Value is (handler, visibility) where visibility is public|private.
     http.ml reads from here on every request (no DB round-trip). *)
  routes : (string, string * string) Hashtbl.t;
  mutable started_at : float;
  (** Kernel prim catalog for Functions browser: (name, summary) list. *)
  mutable kernel_catalog : (string * string) list;
  (** Product shell cache (nav/css) + its invalidation windows. *)
  mutable ui_shell : ui_shell;
  (** Env + rim + load/rehydrate bookkeeping (interpreter driver). *)
  mutable eval_state : eval_state;
  (** branching.seed-policy: when true, define/route persist is
      insert-if-absent (boot seed into system/main). Interactive
      load-library / define keep upsert (false). *)
  mutable seed_additive : bool;
  (** Active HTTP request for user-route dispatch; None in REPL. *)
  mutable current_request : request option;
  (** When true, (checkout)/(fork)/(merge) are blocked so the agent
      cannot leave the current branch mid-task. *)
  mutable agent_branch_lock : bool;
  (** i-1788669715 / i-1788669761: set while a PUBLIC route handler is
      being dispatched. The meta-eval prims (eval-string / load-string /
      load-library) deny when set — they are the capability-reset escape
      hatch and must not run inside public-handler execution. *)
  mutable public_dispatch : bool;
  (** spec_delegation q4 (2026-09-11): true when the session carries a
      scoped on-behalf-of token - manifest.username is the OWNER the
      session acts for; the carrying agent is the actor (recorded per
      write once the token resolve path lands; no producer yet, so every
      session is self today). A delegated session may load SEEDED
      packages only (Session.load_library) and NEVER persists code:
      define/route/package persistence is skipped while set -
      delegation is for data, never for code. Blocks reseed-package and
      deftype (schema is code-adjacent). Branch moves (fork/merge/
      checkout) are the token resolve path's job via agent_branch_lock. *)
  mutable delegated : bool;
  (** Admin flag from users.is_admin (auth identity). *)
  mutable is_admin : bool;
  (** Raw pp_session cookie for the active HTTP request, if any. *)
  mutable auth_token : string option;
  (** image.inspector I2: recent define impacts (newest first, max 32). *)
  mutable impact_queue : impact_event list;
  (** branching.change-set: recent define/route ops (newest first, max 64). *)
  mutable change_transcript : change_event list;
  (** single-image.net-prims: session-local stream table for tcp-connect.
      Maps handle-id (UUID string) -> stream handle. Populated by
      (tcp-connect ...); drained by (stream-close ...) and by the switch
      finishing. Quota enforced at insert. The stream type is defined
      here to avoid a module cycle (Net_prims references it). *)
  mutable net_streams : (string, Unix.file_descr) Hashtbl.t;
  }

(** Tenant tier (spec_town_profile 2026-09-13): delegated tokens always
    carry the data-only tier; in ANY named profile (PP_PROFILE set, e.g.
    town) every non-admin session is a tenant in P1 — one tier.
    Mainframe runs without PP_PROFILE and is unchanged: only delegated
    tokens restrict. Enforcement sites: define/route/package persist,
    load-library, reseed-package, deftype. The check reads only the env
    (cheap; no profile file I/O). *)
let restricted (t : t) : bool =
  t.delegated
  || (match Profile.name () with Some _ -> not t.is_admin | None -> false)

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
