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
    loop ()
  in
  loop ()
