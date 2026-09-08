(** Pricklypear Image — Store connection, config, transactions (Eio floor).

    Domain-facing API returns plain ['a] (blocks the calling Eio fiber).
    Single backend: Io_eio.Pg (pure pgx over Eio.Net). Process always runs
    under Eio_main (image/bin/main.ml). *)

type config = {
  database : string;
  host : string;
  port : int;
  user : string;
  password : string;
}

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft

let config_from_env () : config =
  {
    database = env_or "PP_DB_NAME" (env_or "PGDATABASE" "pricklypear");
    host = env_or "PP_DB_HOST" (env_or "PGHOST" "127.0.0.1");
    port = int_of_string (env_or "PP_DB_PORT" (env_or "PGPORT" "5433"));
    user = env_or "PP_DB_USER" (env_or "PGUSER" "pricklypear_image");
    password = env_or "PP_DB_PASS" "";
  }

(** Connection handle + mutex: identity-monad pgx only locks per SQL op, so
    concurrent Httpun_eio fibers would interleave BEGIN/SELECT on one wire.
    All SQL (exec + whole transactions) takes this lock; same-fiber re-entry
    is allowed via [eio_lock_depth_key] so a tx body can exec. *)
type conn = Io_eio.Pg.t * Eio.Mutex.t

let eio_lock_depth_key : int Eio.Fiber.key = Eio.Fiber.create_key ()

let with_eio_conn_lock (mu : Eio.Mutex.t) (f : unit -> 'a) : 'a =
  match Eio.Fiber.get eio_lock_depth_key with
  | Some d when d > 0 ->
      Eio.Fiber.with_binding eio_lock_depth_key (d + 1) f
  | _ ->
      Eio.Mutex.lock mu;
      (match
         Eio.Fiber.with_binding eio_lock_depth_key 1 (fun () ->
             Eio.Cancel.protect f)
       with
       | v ->
           Eio.Mutex.unlock mu;
           v
       | exception ex ->
           Eio.Mutex.unlock mu;
           raise ex)

type t = {
  cfg : config;
  mutable conn : conn option;
  mutable branch_id : string;
  mutable current_tx : conn option;
  (* image.boot-seed-batch: when seed_additive boot seed is running, this
     holds the set of function/route names already present on the seed
     branch (system/main), so persist_function_if_absent /
     register_route_if_absent skip the per-name get_by_path SELECT. None
     outside the boot seed pass. See borge/image.borg boot-seed-batch. *)
  mutable seed_names : (string, unit) Hashtbl.t option;
}

let make ~cfg ~branch_id () : t =
  { cfg; conn = None; branch_id; current_tx = None; seed_names = None }

(** Expose the config so the session registry can open per-session
    connections with the same parameters (image.per-identity-sessions). *)
let config (t : t) : config = t.cfg

let conn_of (t : t) : conn option = t.conn

let current_conn (t : t) : conn option =
  match t.current_tx with Some tx -> Some tx | None -> t.conn

let in_transaction (t : t) : bool = t.current_tx <> None

(* PP_EVAL_TRACE profiling (tmp/eval-fast-path): per-request DB statement
   counters, bumped by exec when the env knob is set. Read by http.ml
   phase tracing to attribute DB time/hits to request phases. *)
let trace_on = Sys.getenv_opt "PP_EVAL_TRACE" <> None
let slow_trace_on = Sys.getenv_opt "PP_TRACE_DB" <> None
let trace_count = Atomic.make 0
let trace_ms = Atomic.make 0.0

let exec ((pg, mu) : conn) ?params sql : Pgx.row list =
  if not (trace_on || slow_trace_on) then
    (* fast path: no timing, no getenv after the one check *)
    with_eio_conn_lock mu (fun () ->
        match params with
        | None -> Io_eio.Pg.execute pg sql
        | Some params -> Io_eio.Pg.execute ~params pg sql)
  else begin
    let t0 = Unix.gettimeofday () in
    let r =
      with_eio_conn_lock mu (fun () ->
          match params with
          | None -> Io_eio.Pg.execute pg sql
          | Some params -> Io_eio.Pg.execute ~params pg sql)
    in
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    if trace_on then begin
      Atomic.incr trace_count;
      Atomic.set trace_ms (Atomic.get trace_ms +. ms)
    end;
    if slow_trace_on && ms > 20.0 then
      Printf.eprintf "[db] execute %.1f ms: %s\n%!" ms
        (String.sub sql 0 (min 60 (String.length sql)));
    r
  end

(** Fiber-local session username for SET LOCAL pp.user (RLS floor).
    Bound by Request_context.with_current so store_db need not depend on
    the later request_context module (dune module order). *)
let pp_user_key : string Eio.Fiber.key = Eio.Fiber.create_key ()

let pp_user_safe (s : string) : string =
  let s = String.lowercase_ascii (String.trim s) in
  let ok =
    s <> ""
    && String.for_all
         (function
           | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
           | _ -> false)
         s
  in
  if ok then s else "anonymous"

let with_pp_user (username : string) (f : unit -> 'a) : 'a =
  Eio.Fiber.with_binding pp_user_key (pp_user_safe username) f

let current_pp_user () : string =
  match Eio.Fiber.get pp_user_key with
  | Some u -> u
  | None -> "anonymous"

(** SET LOCAL pp.user inside the open transaction so RLS / tree_data_rows
    can confine. LOCAL scopes to the tx; safe even on a shared connection. *)
let set_local_pp_user (pg : Io_eio.Pg.t) : unit =
  let u = current_pp_user () in
  let _ =
    Io_eio.Pg.execute ~params:[ Pgx.Value.of_string u ]
      pg "SELECT set_config('pp.user', $1, true)"
  in
  ()

(** serialization_failure (SQLSTATE 40001): SSI aborted the transaction
    after concurrent commits invalidated its snapshot. Nothing committed,
    and the only correct response is to re-run the body. Retried
    unconditionally by with_transaction (i-1788670872-3: the Serializable
    default previously had zero retry anywhere). Other PG error classes
    (constraint / RLS violations, syntax) are caller errors — retrying
    would only repeat them. *)
let serialization_retries = 2

let is_serialization_failure (exn : exn) : bool =
  match exn with
  | Pgx.PostgreSQL_Error (_, fields) ->
      String.equal fields.Pgx.Error_response.code "40001"
  | _ -> false

(** unique_violation (SQLSTATE 23505): the one failure class a caller may
    recover from by re-reading (insert races). ensure_branch narrows its
    catch to this; every other class propagates. *)
let is_unique_violation (exn : exn) : bool =
  match exn with
  | Pgx.PostgreSQL_Error (_, fields) ->
      String.equal fields.Pgx.Error_response.code "23505"
  | _ -> false

(** i-1788671060-2 (swallow-and-default retirement): a FAILED query is an
    error, not an empty result. Store/auth_store DB-error catch-arms call
    this instead of eprintf + default, so an outage, missing migration, or
    bad row PROPAGATES to the session/HTTP boundary (which already
    converts: /api/eval error JSON, HTTP 500 + ops incident, REPL
    Lwt.catch) instead of presenting as no-functions / count-0 / no-user —
    degraded must be distinguishable from legitimately empty.

    Swallows that remain are genuine optional probes and say so in a
    comment at the site (visible_stamp fast path, begin_seed_names,
    best-effort post-success bookkeeping, log_auth). The no-DB
    (conn=None) arms are unchanged: "not configured" is a distinct,
    truthful degraded mode, not an error to mask.

    Greppable invariant: every remaining catch-arm in
    store.ml/auth_store.ml is either [with exn -> Store_db.reraise …]
    (log + propagate), an honest (Error msg) result contract, a
    comment-marked probe, or a per-row DECODE GUARD after a successful
    query (try <parse a cell> with _ -> skip-row — one malformed cell
    never masks a DB failure; the query-error class propagates). *)
let reraise ~(op : string) (exn : exn) : 'a =
  Printf.eprintf "[store] %s failed: %s\n%!" op (Printexc.to_string exn);
  raise exn

let with_transaction ?(isolation = Pgx.Isolation.Serializable) (t : t)
    (f : conn option -> 'a) : 'a =
  match t.conn with
  | None -> f None
  | Some (pg, mu) ->
      (* Nested-transaction ride (i-job-tx-wedge, 2026-09-07): when a
         transaction is already active on this store's connection — a Lisp
         (with-transaction …) body (with_dynamic_transaction), a
         session-control prim, or an outer with_transaction — JOIN it
         instead of opening a nested BEGIN. pgx begin_work raises
         "begin_work: cannot transact while in another transaction" on the
         second BEGIN, and the raised exception aborts the OUTER tx
         ("current transaction is aborted, commands ignored until end of
         transaction block") — so every rows / find-rows / get / create
         prim called from a (with-transaction …) body or a job body used to
         wedge with an opaque error while the outer work rolled back (the
         agent lobby bot went silent through this hole). Riding joins the
         AMBIENT tx: its isolation wins, the OPENER owns commit/rollback
         and pp.user (SET LOCAL armed by whoever opened it), and this
         layer's retry loop does not apply (the owner's retry/backoff
         governs). Same tradeoff Crud.with_tree_tx's ambient branch already
         made. *)
      (match t.current_tx with
       | Some conn -> f (Some conn)
       | None ->
           let attempt () =
             with_eio_conn_lock mu (fun () ->
                 Io_eio.Pg.with_transaction ~isolation pg (fun tx ->
                     t.current_tx <- Some (tx, mu);
                     set_local_pp_user tx;
                     Fun.protect
                       ~finally:(fun () -> t.current_tx <- None)
                       (fun () -> f (Some (tx, mu)))))
           in
           (* Bounded immediate retry: the body's DB effects rolled back with
              the aborted tx, so re-running it is safe for the DB; OCaml-side
              effects in f are the usual value-building closures. *)
           let rec loop n =
             match attempt () with
             | v -> v
             | exception ex when is_serialization_failure ex && n > 0 ->
                 loop (n - 1)
           in
           loop serialization_retries)

(** Low-isolation (READ COMMITTED) transaction for the datum-touching
    tree/edge prims (Crud.with_tree_tx). Before the RLS-floor fix those
    prims ran per-statement autocommit — i.e. READ COMMITTED — so the tx
    here exists to (a) arm the pp.user RLS floor via set_local_pp_user
    and (b) make multi-statement sequences (link, tree/move, delete +
    edge count) atomic, NOT to add SSI conflict surface that did not
    exist before. SERIALIZABLE remains the default for with_transaction. *)
let with_transaction_low (t : t) (f : conn option -> 'a) : 'a =
  with_transaction ~isolation:Pgx.Isolation.Read_committed t f

let with_dynamic_transaction ?(isolation = Pgx.Isolation.Serializable)
    (t : t) (f : unit -> 'a) : 'a =
  match t.conn with
  | None -> f ()
  | Some (pg, mu) ->
      with_eio_conn_lock mu (fun () ->
          (match t.current_tx with
           | Some _ ->
               (* Nested ride (i-job-tx-wedge): an active transaction owns
                  commit/rollback — join it instead of raising. The old
                  raise fired inside the OUTER tx's body, so the outer
                  rollback took the whole (with-transaction …) body down
                  with it. Riding keeps nested bodies composable: the
                  inner join commits (or rolls back) with the outer. *)
               f ()
           | None ->
               Io_eio.Pg.with_transaction ~isolation pg (fun tx_conn ->
                   t.current_tx <- Some (tx_conn, mu);
                   set_local_pp_user tx_conn;
                   Fun.protect
                     ~finally:(fun () -> t.current_tx <- None)
                     f)))

(* with_dynamic_transaction deliberately does NOT retry serialization
   failures: its body is arbitrary Lisp ((with-transaction …) prim) and
   may carry non-DB side effects (net prims, output), which a silent
   re-run would duplicate. A 40001 inside (with-transaction …) surfaces
   to the Lisp caller, as before. *)

let connect (t : t) : unit =
  match t.conn with
  | Some _ -> ()
  | None -> (
      try
        let c =
          Io_eio.Pg.connect ~host:t.cfg.host ~port:t.cfg.port
            ~user:t.cfg.user ~password:t.cfg.password
            ~database:t.cfg.database ()
        in
        let mu = Eio.Mutex.create () in
        t.conn <- Some (c, mu);
        Printf.eprintf "[store] connected (eio) to PG %s:%d/%s as %s\n%!"
          t.cfg.host t.cfg.port t.cfg.database t.cfg.user
      with exn ->
        Printf.eprintf "[store] PG connect failed (eio): %s\n%!"
          (Printexc.to_string exn))

let health_check (t : t) : (unit, string) result =
  match current_conn t with
  | None -> Error "no database connection"
  | Some conn -> (
      try
        match exec conn "SELECT 1" with
        | _ :: _ -> Ok ()
        | [] -> Error "SELECT 1 returned no rows"
      with exn -> Error (Printexc.to_string exn))

(** @deprecated compatibility alias — was Pg_lwt. *)
module Pg = Io_eio.Pg
