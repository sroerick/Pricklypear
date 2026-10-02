(** Pricklypear Image — activity ledger write path
    (product-experience.borg §activity-ledger, security.borg
    §the-watched-machine).

    An SMF-shaped, identity-attributed, STRUCTURE-ONLY event stream:
    who did what class, to what target, with what outcome, at what cost.
    No content — target is a path (b.fn.* / data.* / route), never a
    body, never a payload.

    Same storage doctrine as function_stats (migration 031): TELEMETRY IS
    NOT CONTENT. Dedicated table (migration 035), in-process ring
    (drop-oldest at capacity), ~30s background flush with plain INSERTs,
    set-based retention DELETE over plain rows, plus forever-kept day
    rollups per (day, actor, class). The request path NEVER does
    synchronous DB work: [push] is a mutex + queue op only.

    Two read shapes:
      - raw rows  — recent detail, bounded retention (newest ~50k)
      - rollups   — chargeback / budgets-as-security substrate, kept
                    forever; accumulated from each flushed batch exactly
                    once, so raw pruning never loses rollup history. *)

type entry = {
  at : float;                          (* Unix epoch seconds at push time *)
  actor : string;
  session_id : string option;
  branch_id : string option;
  cls : string;                        (* validated class grammar *)
  target : string option;              (* a path, never content *)
  outcome : string;                    (* ok | denied | error | timed-out *)
  cost_json : string option;           (* {steps,prim_calls,db_hits,db_ms} *)
  source_ip : string option;           (* auth.* classes only *)
}

(* The class grammar is CLOSED (safe_sort_sql precedent): a free-text
   class would make the ledger unqueryable and the knock thresholds
   uncomputable. Adding a class is a spec change, not a caller's choice. *)
let valid_classes : string list =
  [
    "auth.login"; "auth.mint"; "auth.revoke"; "auth.deny"; "auth.delegated";
    "api.eval"; "route.hit"; "tree.write"; "data.write"; "rls.violation";
    "admin.content-read"; "admin.hold"; "admin.knock"; "admin.revoke";
    "admin.create"; "admin.delete"; "promote.attempt"; "token.mint";
    "agent.spawn"; "egress.http"; "quota.exceeded";
  ]

let valid_outcomes : string list = [ "ok"; "denied"; "error"; "timed-out" ]

let valid_class (s : string) : bool = List.mem s valid_classes
let valid_outcome (s : string) : bool = List.mem s valid_outcomes

let capacity = 512
let flush_secs = 30.0

(* Retention: keep the newest [keep_rows] raw rows. Set-based DELETE over
   plain rows (no triggers) — milliseconds, bounded by the flush beat.
   PP_ACTIVITY_PRUNE=0 disables (031 incident kill switch). *)
let keep_rows = 50_000
let prune_secs = 600.0

let enabled () = Sys.getenv_opt "PP_ACTIVITY_PRUNE" <> Some "0"

(** Lonely-habitat bit (security.borg §the-watched-machine): when there
    is exactly ONE human user, or PP_LONELY_HABITAT is truthy, deliberate
    reads of another identity's content are not a ceremony — the operator
    IS the only human, so `admin.content-read` is suppressed. [None] means
    "not yet determined"; the first caller to compute it memoizes the
    answer. PP_LONELY_HABITAT=1 forces true; =0 forces false. *)
let lonely_habitat : bool option ref = ref None

let env_lonely () : bool option =
  match Sys.getenv_opt "PP_LONELY_HABITAT" with
  | Some "1" | Some "true" | Some "yes" | Some "on" -> Some true
  | Some "0" | Some "false" | Some "no" | Some "off" -> Some false
  | _ -> None

(** True when this habitat has exactly one human user. A non-blocking
    in-memory memo: callers on the request path pass the count they have
    (or [None] to leave it undetermined), so no DB work is ever required
    at the emit seam. *)
let is_lonely_habitat ?(human_count : int option = None) () : bool =
  match !lonely_habitat with
  | Some b -> b
  | None -> (
      match env_lonely () with
      | Some b -> lonely_habitat := Some b; b
      | None -> (
          match human_count with
          | Some n ->
              let b = n <= 1 in
              lonely_habitat := Some b;
              b
          | None -> false))

(** Test/reset hook: clear the memo (used by tests). *)
let reset_lonely () : unit = lonely_habitat := None

let buf : entry Queue.t = Queue.create ()
let mu : Eio.Mutex.t = Eio.Mutex.create ()

(** Request-path push: mutex + queue op only, ZERO DB work. Returns
    [Error] (and enqueues nothing) for an unknown class or outcome — the
    closed grammar is enforced here, not at flush. *)
let push ~(actor : string) ~(cls : string) ~(target : string option)
    ~(outcome : string) ?(cost_json : string option) ?session_id ?branch_id
    ?source_ip () : (unit, string) result =
  if not (valid_class cls) then
    Error (Printf.sprintf "unknown activity class %S" cls)
  else if not (valid_outcome outcome) then
    Error (Printf.sprintf "unknown activity outcome %S" outcome)
  else begin
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
        if Queue.length buf >= capacity then ignore (Queue.pop buf);
        Queue.push
          { at = Unix.gettimeofday (); actor; session_id; branch_id; cls;
            target; outcome; cost_json; source_ip }
          buf);
    Ok ()
  end

(** Drain the ring under the mutex. *)
let drain () : entry list =
  Eio.Mutex.use_rw ~protect:true mu (fun () ->
      let xs = Queue.fold (fun acc e -> e :: acc) [] buf |> List.rev in
      Queue.clear buf;
      xs)

(** A datum path for a target was a path, never content. Callers pass the
    path explicitly; these helpers just keep the ledger's promise visible
    at the call site. No-op identity. *)
let path_target (s : string) : string option = Some s

(** Budget object → the ledger's cost JSON (steps, prim_calls, db_hits,
    db_ms). Same shape the /timing fiber slot reads; no content. *)
let budget_cost_json ~(steps : int) ~(prim_calls : int) ~(db_hits : int)
    ~(db_ms : float) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("steps", `Int steps); ("prim_calls", `Int prim_calls);
         ("db_hits", `Int db_hits); ("db_ms", `Int (int_of_float db_ms)) ])

(* —— day rollups ————————————————————————————————————————————————
   Accumulated from the flushed batch exactly once, grouped by
   (day, actor, class). Kept forever; raw pruning never touches them. *)
let rollup_day (at : float) : string =
  let tm = Unix.gmtime at in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

let cost_sum (cost_json : string option) : int * int * int * int =
  match cost_json with
  | None -> (0, 0, 0, 0)
  | Some s -> (
      match Yojson.Safe.from_string s with
      | `Assoc fields ->
          let get k =
            match List.assoc_opt k fields with
            | Some (`Int n) -> n
            | Some (`Float f) -> int_of_float f
            | _ -> 0
          in
          (get "steps", get "prim_calls", get "db_hits", get "db_ms")
      | _ -> (0, 0, 0, 0))
      | exception _ -> (0, 0, 0, 0)

(** Upsert one day-rollup increment per (day, actor, class) group. *)
let rollup_entries ~(store : Store.t) (xs : entry list) : unit =
  match Io_db.Db.current_conn store with
  | None -> ()
  | Some c ->
      let groups : (string * string * string, int * int * int * int * int) Hashtbl.t =
        Hashtbl.create 16
      in
      List.iter
        (fun e ->
          let key = (rollup_day e.at, e.actor, e.cls) in
          let ev, st, pc, dh, dm =
            match Hashtbl.find_opt groups key with
            | Some v -> v
            | None -> (0, 0, 0, 0, 0)
          in
          let s, p, h, m = cost_sum e.cost_json in
          Hashtbl.replace groups key (ev + 1, st + s, pc + p, dh + h, dm + m))
        xs;
      Hashtbl.iter
        (fun (day, actor, cls) (ev, st, pc, dh, dm) ->
          let sql =
            "INSERT INTO activity_rollup_day " ^
            "(day, actor, class, events, steps, prim_calls, db_hits, db_ms) " ^
            "VALUES ($1::date, $2, $3, $4, $5, $6, $7, $8) " ^
            "ON CONFLICT (day, actor, class) DO UPDATE SET " ^
            "  events = activity_rollup_day.events + EXCLUDED.events, " ^
            "  steps = activity_rollup_day.steps + EXCLUDED.steps, " ^
            "  prim_calls = activity_rollup_day.prim_calls + EXCLUDED.prim_calls, " ^
            "  db_hits = activity_rollup_day.db_hits + EXCLUDED.db_hits, " ^
            "  db_ms = activity_rollup_day.db_ms + EXCLUDED.db_ms"
          in
          try
            ignore
              (Store_db.exec c
                 ~params:
                   [ Pgx.Value.of_string day; Pgx.Value.of_string actor;
                     Pgx.Value.of_string cls; Pgx.Value.of_int ev;
                     Pgx.Value.of_int st; Pgx.Value.of_int pc;
                     Pgx.Value.of_int dh; Pgx.Value.of_int dm ]
                 sql)
          with exn ->
            Printf.eprintf "[activity_ledger] rollup row failed: %s\n%!"
              (Printexc.to_string exn))
        groups

(** Drain the ring into activity_ledger, then accumulate its day
    rollups. Best-effort: a row that fails to insert is logged and
    dropped (the ledger must never kill the flush loop). *)
let flush ~(store : Store.t) : unit =
  let pending = drain () in
  match pending with
  | [] -> ()
  | xs -> (
      let sql =
        "INSERT INTO activity_ledger " ^
        "(at, actor, session_id, branch_id, class, target, outcome, cost, source_ip) " ^
        "VALUES (to_timestamp($1), $2, $3, $4, $5, $6, $7, $8::jsonb, $9::inet)"
      in
      match Io_db.Db.current_conn store with
      | None -> ()
      | Some c ->
          List.iter
            (fun e ->
              let opt = function Some s -> Pgx.Value.of_string s | None -> Pgx.Value.null in
              try
                ignore
                  (Store_db.exec c
                     ~params:
                       [ Pgx.Value.of_float e.at; Pgx.Value.of_string e.actor;
                         opt e.session_id; opt e.branch_id;
                         Pgx.Value.of_string e.cls; opt e.target;
                         Pgx.Value.of_string e.outcome; opt e.cost_json;
                         opt e.source_ip ]
                     sql)
              with exn ->
                Printf.eprintf "[activity_ledger] flush row failed: %s\n%!"
                  (Printexc.to_string exn))
            xs;
          rollup_entries ~store xs)

(** Retention: keep only the newest [keep_rows] raw rows. Set-based
    DELETE over plain rows — no mirrors, no RLS, no versions; returns the
    deleted count; best-effort (failures logged, retried next beat). *)
let prune_once ~(store : Store.t) : int =
  let sql =
    "DELETE FROM activity_ledger a USING ( " ^
    "  SELECT id FROM activity_ledger " ^
    "  ORDER BY id DESC OFFSET $1 " ^
    ") victim WHERE a.id = victim.id"
  in
  match Io_db.Db.current_conn store with
  | None -> 0
  | Some c -> (
      try
        match Store_db.exec c ~params:[ Pgx.Value.of_int keep_rows ] sql with
        | [ [ cnt ] ] ->
            (match Pgx.Value.to_int cnt with Some n -> n | None -> 0)
        | _ -> 0
      with exn ->
        Printf.eprintf "[activity_ledger] prune failed: %s\n%!"
          (Printexc.to_string exn);
        0)

let last_prune = ref 0.0

(** True when at least [prune_secs] have elapsed since the last prune
    (or no prune has run yet). *)
let prune_due (now : float) : bool = now -. !last_prune >= prune_secs

(* —— read face —————————————————————————————————————————————————
   (ops/activity [actor] [since] [class]) — product-experience
   §activity-ledger READ. Read-only and safe for agent monitors.

   SECURITY: a non-admin caller is force-filtered to its own actor rows
   AT THE SQL LAYER (the WHERE clause carries `actor = $caller`), never
   post-filtered in Lisp. [~actor] is honored only when [~is_admin]. A
   watched machine shows its watch to the watched; it never leaks
   another identity's rows through a crafted argument. Port 033's admin
   disjunct precedent, but THIS table has no RLS, so the caller identity
   is the only gate and it must live in the query text. *)
let json_row (row : Pgx.row) : string =
  match row with
  | [ at; actor; cls; target; outcome; cost ] ->
      let str v = match Pgx.Value.to_string v with Some s -> s | None -> "" in
      let opt v =
        match Pgx.Value.to_string v with Some s -> `String s | None -> `Null
      in
      let cost_j =
        match Pgx.Value.to_string cost with
        | Some c -> (try Yojson.Safe.from_string c with _ -> `Null)
        | None -> `Null
      in
      Yojson.Safe.to_string
        (`Assoc
           [ ("at", `String (str at)); ("actor", `String (str actor));
             ("class", `String (str cls)); ("target", opt target);
             ("outcome", `String (str outcome)); ("cost", cost_j) ])
  | _ -> "{}"

let read_rows ~(store : Store.t) ~(caller : string) ~(is_admin : bool)
    ?(actor : string option)
    ?(since : [ `Epoch of float | `Iso of string ] option)
    ?(cls : string option) ?(limit : int = 100) () : string list =
  match Io_db.Db.current_conn store with
  | None -> []
  | Some conn ->
      let where = ref [] in
      let params = ref [] in
      let add v =
        params := !params @ [ v ];
        List.length !params
      in
      (* force-filter: a non-admin always filters to self, ignoring any
         supplied [actor] (SQL-layer gate, not a Lisp post-filter). *)
      let actor_filter = if is_admin then actor else Some caller in
      (match actor_filter with
       | Some a ->
           let i = add (Pgx.Value.of_string a) in
           where := !where @ [ Printf.sprintf "actor = $%d" i ]
       | None -> ());
      (match since with
       | Some (`Epoch t) ->
           let i = add (Pgx.Value.of_float t) in
           where := !where @ [ Printf.sprintf "at >= to_timestamp($%d)" i ]
       | Some (`Iso s) ->
           let i = add (Pgx.Value.of_string s) in
           where := !where @ [ Printf.sprintf "at >= $%d::timestamptz" i ]
       | None -> ());
      (match cls with
       | Some c ->
           let i = add (Pgx.Value.of_string c) in
           where := !where @ [ Printf.sprintf "class = $%d" i ]
       | None -> ());
      let limit_i = add (Pgx.Value.of_int (max 0 (min limit 1000))) in
      let conds = match !where with [] -> "" | xs -> "WHERE " ^ String.concat " AND " xs in
      let sql =
        Printf.sprintf
          "SELECT at::text, actor, class, target, outcome, cost::text FROM activity_ledger %s \
           ORDER BY at DESC, id DESC LIMIT $%d"
          conds limit_i
      in
      (try Store_db.exec conn ~params:!params sql |> List.map json_row
       with exn ->
         Printf.eprintf "[activity_ledger] read failed: %s\n%!"
           (Printexc.to_string exn);
         [])

(** Background flush loop; fork once at boot. Uses the Eio clock — never
    route this through any run_lwt-style bridge (function_stats comment:
    concurrent bridge calls across fibers took the switch down). *)
let flush_loop ~(clock : _ Eio.Time.clock) ~(store : Store.t) : unit =
  let rec loop () =
    Eio.Time.sleep clock flush_secs;
    (try flush ~store
     with exn ->
       Printf.eprintf "[activity_ledger] flush failed: %s\n%!"
         (Printexc.to_string exn));
    let now = Eio.Time.now clock in
    if prune_due now then begin
      last_prune := now;
      if enabled () then begin
        let n = prune_once ~store in
        if n > 0 then
          Printf.eprintf "[activity_ledger] pruned %d rows\n%!" n
      end
    end;
    loop ()
  in
  loop ()
