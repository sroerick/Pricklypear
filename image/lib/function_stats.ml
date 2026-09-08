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
   ledger.

   CHUNKED (incident 2026-09-08): the first prod pass fired ONE delete
   of ~54k rows; per-row RLS (PLpgSQL tree_path_in_member_space) + the
   deferred mirror triggers turned it into a 25-minute transaction that
   held the store's per-connection mutex, starved every other DB fiber,
   hung the health probe, and took prod down (the rc.d watchdog probe
   had no timeout either, so nothing self-healed — fixed separately in
   scripts/rc.d/pricklypear). The prune now deletes at most
   [prune_chunk] rows per flush beat: each transaction is ~1-2s, the
   backlog drains at ~4k rows/min, steady-state is one small chunk
   every ~10min. Set PP_FUNCSTAT_PRUNE=0 to disable entirely. *)
let keep_rows = 1000
let prune_secs = 600.0
let prune_chunk = 2000

let enabled () = Sys.getenv_opt "PP_FUNCSTAT_PRUNE" <> Some "0"

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

(** Retention: delete at most [prune_chunk] function_stat datum rows
    beyond the newest [keep_rows] (OFFSET on the newest-first scan;
    mirror triggers cascade to the scaffolding table). Returns the
    deleted count; best-effort — failures are logged and retried on the
    next beat (never kills the flush loop). One chunk per call keeps
    each transaction ~1-2s so the store mutex is never held for long. *)
let prune_once ~(store : Store.t) : int =
  let sql =
    "DELETE FROM tree_nodes t USING ( " ^
    "  SELECT ctid FROM tree_nodes " ^
    "  WHERE kind = 'datum' AND type_name = 'function_stat' " ^
    "  ORDER BY created_at DESC OFFSET $1 LIMIT $2 " ^
    ") victim WHERE t.ctid = victim.ctid"
  in
  match Io_lwt.Db.current_conn store with
  | None -> 0
  | Some c ->
      (try
         match
           Store_db.exec c
             ~params:[ Pgx.Value.of_int keep_rows;
                       Pgx.Value.of_int prune_chunk ]
             sql
         with
         | [ [ cnt ] ] -> (match Pgx.Value.to_int cnt with Some n -> n | None -> 0)
         | _ -> 0
       with exn ->
         Printf.eprintf "[function_stats] prune failed: %s\n%!"
           (Printexc.to_string exn);
         0)

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
       ~10min, ONE bounded chunk per beat (see prune_once — the unchunked
       first pass took prod down, 2026-09-08). Runs on the first pass so
       a deployed image starts draining the accumulated backlog
       (~54k rows on prod) immediately, in ~2s-per-beat slices. *)
    let now = Eio.Time.now clock in
    if prune_due now then begin
      last_prune := now;
      if enabled () then begin
        let n = prune_once ~store in
        if n > 0 then
          Printf.eprintf "[function_stats] pruned %d function_stat rows\n%!" n
      end
    end;
    loop ()
  in
  loop ()
