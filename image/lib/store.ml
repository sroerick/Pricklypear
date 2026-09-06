(** Pricklypear Image — Storage facade (i-1788671083-c split).

    The store was a ~2,700-line monolith mixing five domains AND the
    conn/tx substrate. It is now five family modules over one shared
    SQL-substrate module, composed here (surface unchanged for callers):

      Store_common    rel names + the ONE branch-chain CTE + staleness stamp
      Store_defs      def lookups / counts / fingerprints (opaque JSON bodies)
      Store_branch    branch status / persist / promote / merge / diff / fork
      Store_packages  package registry
      Store_history   code history / diff / restore
      Store_retention tree-time.gc retention worker

    EXPLICIT SUBSTRATE ACCESSORS (replacing the old wholesale
    substrate include): the domain module no longer re-exports the
    conn/tx substrate wholesale; the
    accessors external callers actually use are re-exported below, and
    the family modules call [Store_db] directly.

    OPAQUE BODIES (was Lisp-AST-typed): function bodies cross the store
    boundary as the raw jsonb text stored; parsing to [Ast.expr] happens
    at the interpreter boundary via [expr_of_body_json] on
    [Pricklypear_lisp].Ast — the store no longer depends on the
    interpreter.

    ERROR POLICY (i-1788671060-2): unchanged — a failed query is an
    ERROR, not an empty result. DB-error catch-arms log-and-PROPAGATE
    via [Store_db.reraise]; remaining swallows are genuine optional
    probes, each commented as such at the site. The no-DB (conn=None)
    arms are unchanged: "not configured" is a distinct, truthful
    degraded mode. *)

type t = Store_db.t

(* ---- explicit conn/tx substrate accessors (replacing the old
       wholesale substrate include) ---- *)

let make = Store_db.make
let config = Store_db.config
let config_from_env = Store_db.config_from_env
let conn_of = Store_db.conn_of
let current_conn = Store_db.current_conn
let in_transaction = Store_db.in_transaction
let connect = Store_db.connect
let health_check = Store_db.health_check
let exec = Store_db.exec
let with_transaction = Store_db.with_transaction
let with_transaction_low = Store_db.with_transaction_low
let with_dynamic_transaction = Store_db.with_dynamic_transaction
let with_pp_user = Store_db.with_pp_user
let current_pp_user = Store_db.current_pp_user
let reraise = Store_db.reraise
let is_unique_violation = Store_db.is_unique_violation
let is_serialization_failure = Store_db.is_serialization_failure

(** Staleness stamp (image.per-identity-sessions); lives in Store_common
    with its substrate peers, re-exported for session.ml. *)
let visible_stamp = Store_common.visible_stamp

include Store_defs
include Store_branch
include Store_packages
include Store_history
include Store_retention
