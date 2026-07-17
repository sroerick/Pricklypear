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
  (** image.http-robustness invariant 3: refresh_ui_shell runs on every
      request. Without caching it re-evals ui/nav + ui/css per request
      (wasteful) and, when they fail, warns per request (log spam). These
      fields cache success (refreshed_at, TTL 60s) and suppress re-attempt
      + warn for 60s after a failure (retry_at). Reset to 0.0 on checkout. *)
  mutable ui_nav_refreshed_at : float;
  mutable ui_nav_retry_at : float;
  mutable ui_css_refreshed_at : float;
  mutable ui_css_retry_at : float;
  (** When true, the eval_one define-interceptor suppresses its
      refresh_ui_shell call. Used during seed_stdlib so that defining
      ui/nav mid-seed (before notes/capture/nav is bound) does not
      trigger an eager refresh that fails with unbound: notes/capture/nav.
      The boot sequence calls refresh_ui_shell once AFTER seeding completes. *)
  mutable suppress_refresh_on_define : bool;
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
