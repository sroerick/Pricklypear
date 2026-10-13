(** Pricklypear Image — function_stat write path (image.function-stat-ring).

    Every labeled eval used to call Crud.create synchronously — a
    SERIALIZABLE insert + fsync on the request path, and unbounded row
    growth (54k rows in weeks on prod). Same medicine as
    http-robustness's ops-incidents rule: request path pushes to an
    in-process ring (drop-oldest at capacity), a background fiber
    flushes at most once every [flush_secs]. Crash loss is fine —
    these are perf stats, not ledger.

    STORAGE (migration 031, incident 2026-09-08): telemetry lives in the
    DEDICATED [function_stat] table — NOT tree datums. The datum path
    paid the full tree write per row (mirrors, RLS, tree_versions) and
    made every retention delete a 25-minute transaction at 76k rows that
    took prod down twice. Plain rows: flush = one INSERT per entry,
    retention = plain DELETE (milliseconds, no triggers). The only
    reader is the per-fn "last run" widget (Crud.function_stat_last). *)

type entry = {
  fn : string;
  fields_json : string;
  author : string;
}

let capacity = 512
let flush_secs = 30.0

(* Retention: keep the newest [keep_rows] rows. The delete is set-based
   over plain rows (no triggers) — one statement, milliseconds, bounded
   by the flush beat. PP_FUNCSTAT_PRUNE=0 disables (kept from the
   2026-09-08 incident as a kill switch). *)
let keep_rows = 1000
let prune_secs = 600.0

let enabled () = Sys.getenv_opt "PP_FUNCSTAT_PRUNE" <> Some "0"

let buf : entry Queue.t = Queue.create ()
let mu : Eio.Mutex.t = Eio.Mutex.create ()

(** Request-path push: mutex + queue op only, zero DB ops. *)
let push ~(fn : string) ~(fields_json : string) ~(author : string) : unit =
  Eio.Mutex.use_rw ~protect:true mu (fun () ->
      if Queue.length buf >= capacity then ignore (Queue.pop buf);
      Queue.push { fn; fields_json; author } buf)

(** Drain the ring into function_stat. Best-effort: a row that fails to
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
      let sql =
        "INSERT INTO function_stat (fn, fields, author) " ^
        "VALUES ($1, $2::jsonb, $3)"
      in
      match Io_db.Db.current_conn store with
      | None -> ()
      | Some c ->
          List.iter
            (fun e ->
              (try
                 ignore
                   (Store_db.exec c
                      ~params:[ Pgx.Value.of_string e.fn;
                                Pgx.Value.of_string e.fields_json;
                                Pgx.Value.of_string e.author ]
                      sql)
               with exn ->
                 Printf.eprintf "[function_stats] flush row failed: %s\n%!"
                   (Printexc.to_string exn)))
            xs

(** Retention: keep only the newest [keep_rows] rows. Set-based DELETE
    over plain rows — no mirrors, no RLS, no versions; returns the
    deleted count; best-effort (failures logged, retried next beat). *)
let prune_once ~(store : Store.t) : int =
  let sql =
    "DELETE FROM function_stat f USING ( " ^
    "  SELECT id FROM function_stat " ^
    "  ORDER BY id DESC OFFSET $1 " ^
    ") victim WHERE f.id = victim.id"
  in
  match Io_db.Db.current_conn store with
  | None -> 0
  | Some c ->
      (try
         match
           Store_db.exec c
             ~params:[ Pgx.Value.of_int keep_rows ]
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
    never route this through any run_lwt-style bridge: concurrent
    bridge calls across fibers (this loop vs the jobs runner) is
    unsafe and took the whole switch down on prod (watchdog restart
    loop, 2026-08-21). *)
let flush_loop ~(clock : _ Eio.Time.clock) ~(store : Store.t) : unit =
  let rec loop () =
    Eio.Time.sleep clock flush_secs;
    (try flush ~store
     with exn ->
       Printf.eprintf "[function_stats] flush failed: %s\n%!"
         (Printexc.to_string exn));
    (* Retention rides the flush cadence at its own (slower) beat:
       ~10min. Runs on the first pass so a deployed image trims any
       backlog immediately. *)
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
