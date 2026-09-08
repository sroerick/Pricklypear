(** Pricklypear Image — function_stat write path (image.function-stat-ring).

    Every labeled eval used to call Crud.create synchronously — a
    SERIALIZABLE insert + fsync on the request path, and unbounded row
    growth (54k rows in weeks on prod). Same medicine as
    http-robustness's ops-incidents rule: request path pushes to an
    in-process ring (drop-oldest at capacity), a background fiber
    flushes at most once every [flush_secs]. Crash loss is fine —
    these are perf stats, not ledger. *)

type entry = {
  fn : string;
  fields_json : string;
  author : string;
}

let capacity = 512
let flush_secs = 30.0

(* Retention (log/issues "function_stat unbounded — 55,775 rows"): the
   ring only bounded the WRITE path; the TABLE grew forever and dragged
   every datum read (and /data, /branches) down with it. The only reader
   is the per-fn "last run" widget (Crud.find_row ~latest:true), so
   keep the newest [keep_rows] rows and drop the rest. Prune runs on the
   flush fiber, throttled to once per [prune_secs] (one DELETE over the
   datum rows of type function_stat; the deferred mirrors keep the
   scaffolding table in lockstep). Loss is fine — perf stats, not
   ledger. *)
let keep_rows = 1000
let prune_secs = 600.0

let buf : entry Queue.t = Queue.create ()
let mu : Eio.Mutex.t = Eio.Mutex.create ()

(** Request-path push: mutex + queue op only, zero DB ops. *)
let push ~(fn : string) ~(fields_json : string) ~(author : string) : unit =
  Eio.Mutex.use_rw ~protect:true mu (fun () ->
      if Queue.length buf >= capacity then ignore (Queue.pop buf);
      Queue.push { fn; fields_json; author } buf)

(** Drain the ring into data_rows. Best-effort: a row that fails to
    insert is logged and dropped (stats must never kill the flush loop). *)
let flush ~(store : Store.t) : unit =
  let pending =
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
        let xs = Queue.fold (fun acc e -> e :: acc) [] buf |> List.rev in
        Queue.clear buf;
        xs)
  in
  match pending with
  | [] -> ()
  | xs ->
      List.iter
        (fun e ->
          (try
             ignore
               (Crud.create store ~type_name:"function_stat"
                  ~fields_json:e.fields_json ~author:e.author)
           with exn ->
             Printf.eprintf "[function_stats] flush row failed: %s\n%!"
               (Printexc.to_string exn)))
        xs

(** Retention: keep only the newest [keep_rows] function_stat datum rows.
    DELETE via OFFSET on the newest-first index scan; the mirror triggers
    cascade to the data_rows scaffolding table. Best-effort: failures are
    logged and retried on the next prune (never kills the flush loop). *)
let prune ~(store : Store.t) : unit =
  let sql =
    "DELETE FROM tree_nodes t USING ( " ^
    "  SELECT ctid FROM tree_nodes " ^
    "  WHERE kind = 'datum' AND type_name = 'function_stat' " ^
    "  ORDER BY created_at DESC OFFSET $1 " ^
    ") victim WHERE t.ctid = victim.ctid"
  in
  (match Io_lwt.Db.current_conn store with
   | None -> ()
   | Some c ->
       (try
          ignore
            (Store_db.exec c
               ~params:[ Pgx.Value.of_int keep_rows ]
               sql)
        with exn ->
          Printf.eprintf "[function_stats] prune failed: %s\n%!"
            (Printexc.to_string exn)))

let last_prune = ref 0.0

(** True when at least [prune_secs] have elapsed since the last prune
    (or no prune has run yet). *)
let prune_due (now : float) : bool = now -. !last_prune >= prune_secs

(** Background flush loop; fork once at boot. Uses the Eio clock —
    NOT Lwt_eio.run_lwt: concurrent run_lwt across fibers (this loop vs
    the jobs runner) is unsafe and took the whole switch down on prod
    (watchdog restart loop, 2026-08-21). *)
let flush_loop ~(clock : _ Eio.Time.clock) ~(store : Store.t) : unit =
  let rec loop () =
    Eio.Time.sleep clock flush_secs;
    (try flush ~store
     with exn ->
       Printf.eprintf "[function_stats] flush failed: %s\n%!"
         (Printexc.to_string exn));
    (* Retention rides the flush cadence but at its own (slower) beat:
       ~10min. Runs unconditionally on the first pass so a deployed image
       prunes the accumulated backlog (~54k rows on prod, 2026-09-08)
       without waiting. *)
    let now = Eio.Time.now clock in
    if prune_due now then begin
      last_prune := now;
      (try prune ~store
       with exn ->
         Printf.eprintf "[function_stats] prune failed: %s\n%!"
           (Printexc.to_string exn))
    end;
    loop ()
  in
  loop ()
