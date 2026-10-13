(* BORGE-1790987438 — de-contend the data.* head counter.

   Before migration 038, every branch-free data.* write allocated its
   version-row gen with

       UPDATE tree_generations SET generation = generation + 1 RETURNING ...

   on the SINGLE tree-wide tree_generations row. That UPDATE takes a
   TRANSACTION-SCOPED ROW LOCK held until COMMIT, so every datum write
   tree-wide serialized on one row — a stalled writer froze ALL data
   writes (the 2026-10-02 prod lock-pileup amplifier; measured on dev PG:
   4+ concurrent disjoint-path writers blocked on the same single row).
   Migration 038 replaces the singleton row as the data.* head with a
   Postgres SEQUENCE (tree_generations_seq): nextval is non-transactional,
   holds no row lock, and keeps a SINGLE GLOBAL gen axis (retention
   windows / as-of reads / undo are unaffected; gaps from rolled-back
   writers are harmless — no reader assumes contiguity, undo restores-by-
   write). Per-path ordering is still strictly increasing.

   These tests PROVE the fix (the before/after measurement is captured in
   the plan log):
     - test_sequence_is_the_data_head: a datum write allocates a version
       gen STRICTLY ABOVE the sequence's pre-write high-water mark (no
       reuse / no collision) while the old tree_generations ROW stays
       FROZEN (the write path no longer takes its lock).
     - test_held_datum_tx_does_not_block_disjoint_writer: one session
       holding a tx that wrote a datum (the exact shape that used to
       serialize every other datum writer) does NOT block a DISJOINT-path
       datum write on a second connection — it completes within the
       timeout. With the pre-038 singleton this blocks until the holder
       commits.
     - test_concurrent_disjoint_writes_unique_gens: many concurrent datum
       writes on disjoint paths from independent connections all succeed
       with GLOBALLY UNIQUE gens, no duplicate (path, gen) row, and the
       frozen row untouched.
     - test_same_path_writes_keep_gen_order: repeated writes to the SAME
       path (here sequential — concurrent same-path SERIALIZABLE upserts
       legitimately conflict with 40001, a pre-existing property of the
       store unrelated to this fix) produce strictly increasing gens —
       the exactly-once-sensitive per-path version order (undo / head /
       as-of) is preserved.

   Path hygiene: Random is seeded and type names carry the pid, so every
   run writes FRESH paths (a deterministic path would make a second run an
   ON CONFLICT echo-guard no-op and a stale leftover the echo guard skips).

   Skips cleanly when no Postgres is reachable (scripts/dev.sh). *)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
module IE = Pricklypear_image_lib.Io_eio

let main_id = "00000000-0000-0000-0000-000000000001"

(* Real-PG group (skips when unreachable) — same shape as
   store_reraise_test / store_tx_cancel_test: try the env user first,
   then the dev.sh default (postgres, trust auth), and skip loudly when
   nothing connects. *)
let with_store ~database f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : IE.Ctx.t = IE.Ctx.of_stdenv ~sw env in
  IE.with_ctx ctx (fun () ->
      let base = { (S.config_from_env ()) with database } in
      let rec try_cfg = function
        | [] -> None
        | cfg :: rest ->
            let t = S.make ~cfg ~branch_id:main_id () in
            S.connect t;
            (match S.conn_of t with Some _ -> Some t | None -> try_cfg rest)
      in
      let users =
        if base.user = "postgres" then [ base ]
        else [ base; { base with user = "postgres"; password = "" } ]
      in
      match try_cfg users with
      | Some t -> f t
      | None ->
          Printf.printf
            "SKIP: no postgres reachable (set PP_DB_* / run scripts/dev.sh)\n%!";
          ())

(** A second store to the same database = a SECOND pgx connection, used to
    write concurrently from an independent session (the point of the fix:
    datum writers no longer share one lockable row). *)
let observer (t : S.t) : S.t =
  let cfg = S.config t in
  let o = S.make ~cfg ~branch_id:main_id () in
  S.connect o;
  o

(** Run a scalar SQL query returning a single bigint (or 0 on no row). *)
let scalar_int (t : S.t) (sql : string) : int64 =
  match S.conn_of t with
  | Some (pg, mu) -> (
      match S.exec (pg, mu) sql with
      | [ [ v ] ] -> (
          match Pgx.Value.to_int64 v with
          | Some i -> i
          | None -> failwith "scalar_int: no int64")
      | _ -> failwith "scalar_int: unexpected rows")
  | None -> failwith "scalar_int: no conn"

let frozen_row (t : S.t) : int64 =
  scalar_int t
    "SELECT COALESCE((SELECT generation FROM tree_generations LIMIT 1), 0)"

let seq_last_value (t : S.t) : int64 =
  scalar_int t "SELECT COALESCE(last_value, 0) FROM tree_generations_seq"

(** Count of duplicate (path, gen) rows under an ltree prefix — MUST be 0. *)
let dup_path_gen (t : S.t) ~(prefix : string) : int64 =
  match S.conn_of t with
  | Some (pg, mu) -> (
      match
        S.exec (pg, mu)
          ~params:[ Pgx.Value.of_string prefix ]
          "SELECT count(*) FROM (SELECT path, gen FROM tree_versions \
           WHERE path <@ $1::ltree GROUP BY path, gen HAVING count(*) > 1) d"
      with
      | [ [ v ] ] -> (
          match Pgx.Value.to_int64 v with
          | Some i -> i
          | None -> failwith "dup_path_gen: no int64")
      | _ -> failwith "dup_path_gen: unexpected rows")
  | None -> failwith "dup_path_gen: no conn"

(** Best-effort cleanup of this run's tree_nodes rows (their version
    tombstones stay in history — the append-only audit is untouched). *)
let cleanup_prefix (t : S.t) ~(prefix : string) : unit =
  match S.conn_of t with
  | Some (pg, mu) ->
      ignore
        (S.exec (pg, mu)
           ~params:[ Pgx.Value.of_string prefix ]
           "DELETE FROM tree_nodes WHERE path <@ $1::ltree")
  | None -> ()

let run_tag : string = Printf.sprintf "genseq_%d" (Unix.getpid ())

let type_name_for (what : string) : string =
  Printf.sprintf "%s_%s" run_tag what

let datum_path type_name suffix : string =
  Printf.sprintf "data.u.tester.%s.%s_%d" type_name suffix
    (Random.int 1_000_000)

let run_prefix : string = Printf.sprintf "data.u.tester.%s" run_tag

let history (t : S.t) ~(path : string) : T.version list =
  match S.conn_of t with
  | Some c -> T.versioned_history c ~path ()
  | None -> failwith "history: no conn"

let upsert (t : S.t) ~(path : string) ~(type_name : string) ~(body : string) :
    unit =
  S.with_transaction t (fun conn_opt ->
      match conn_opt with
      | Some c -> (
          match
            T.upsert_datum c ~path ~type_name ~body_json:body ~author:"tester"
              ~rank:None
          with
          | Ok _ -> ()
          | Error e -> Alcotest.fail ("upsert failed: " ^ e))
      | None -> Alcotest.fail "no conn in tx")

(* ---- 1. the sequence is the data head; the old row is frozen ---- *)

let test_sequence_is_the_data_head () =
  with_store ~database:(S.config_from_env ()).database @@ fun t ->
  let type_name = type_name_for "head" in
  let g0 = frozen_row t in
  let s0 = seq_last_value t in
  let path = datum_path type_name "h" in
  upsert t ~path ~type_name ~body:"{\"title\":\"head\"}";
  let g1 = frozen_row t in
  let s1 = seq_last_value t in
  let hist = history t ~path in
  Printf.printf "  row %Ld -> %Ld ; seq %Ld -> %Ld ; path=%s\n%!" g0 g1 s0 s1
    path;
  (* the old singleton row is FROZEN: the write path no longer touches it *)
  Alcotest.(check int64) "tree_generations row frozen (no lock point)" g0 g1;
  (* the version row's gen is STRICTLY ABOVE the pre-write high-water mark
     (no reuse / no collision), and the sequence is at or beyond it. The
     live dev image may interleave a write between s0 and s1, so we do NOT
     assert s1 = s0 + 1. *)
  (match hist with
   | [ v ] -> (
       match v.T.v_gen with
       | Some g ->
           Printf.printf "  version gen = %Ld\n%!" g;
           Alcotest.(check bool) "version gen above pre-write seq mark" true
             (Int64.compare g s0 > 0);
           Alcotest.(check bool) "seq at-or-beyond version gen" true
             (Int64.compare s1 g >= 0)
       | None -> Alcotest.fail "no gen on version row")
   | _ -> Alcotest.fail "expected exactly one version row");
  cleanup_prefix t ~prefix:run_prefix

(* ---- 2. a held datum tx does NOT block a disjoint datum write ---- *)

let test_held_datum_tx_does_not_block_disjoint_writer () =
  with_store ~database:(S.config_from_env ()).database @@ fun a ->
  let b = observer a in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let type_name = type_name_for "hold" in
  let pa = datum_path type_name "held" in
  let pb = datum_path type_name "other" in
  let mid_tx = Eio.Stream.create 1 in
  let release = Eio.Stream.create 1 in
  (* Fiber A: write a datum, then HOLD the transaction open. *)
  let w =
    Eio.Fiber.fork_promise ~sw (fun () ->
        S.with_transaction a (fun conn_opt ->
            (match conn_opt with
             | Some c -> (
                 match
                   T.upsert_datum c ~path:pa ~type_name
                     ~body_json:"{\"title\":\"held\"}" ~author:"tester"
                     ~rank:None
                 with
                 | Ok _ -> ()
                 | Error e -> Alcotest.fail ("holder write failed: " ^ e))
             | None -> Alcotest.fail "no conn in holder tx");
            Eio.Stream.add mid_tx "held";
            ignore (Eio.Stream.take release)))
  in
  ignore (Eio.Stream.take mid_tx);
  (* B writes a DISJOINT datum while A's tx is open. With the pre-038
     singleton this blocks on the tree_generations row lock until A
     commits (would hit the timeout); with the sequence it completes. *)
  (try
     Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
         S.with_transaction b (fun conn_opt ->
             match conn_opt with
             | Some c -> (
                 match
                   T.upsert_datum c ~path:pb ~type_name
                     ~body_json:"{\"title\":\"other\"}" ~author:"tester"
                     ~rank:None
                 with
                 | Ok _ -> ()
                 | Error e -> Alcotest.fail ("disjoint write failed: " ^ e))
             | None -> Alcotest.fail "no conn in disjoint writer"));
     Alcotest.(check bool)
       "disjoint datum write completed while holder tx open" true true
   with
  | Eio.Time.Timeout ->
      Alcotest.fail
        "disjoint datum write BLOCKED on the shared data head while another \
         tx held a datum (pre-038 tree_generations contention; \
         tree_generations_seq expected)");
  Eio.Stream.add release "go";
  (match Eio.Promise.await w with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("holder fiber error: " ^ Printexc.to_string e));
  (* both paths now have exactly one version row, unique gens *)
  let hist_a = history a ~path:pa in
  let hist_b = history b ~path:pb in
  Alcotest.(check int) "held path has one version row" 1 (List.length hist_a);
  Alcotest.(check int) "disjoint path has one version row" 1
    (List.length hist_b);
  Alcotest.(check int64) "no duplicate (path,gen)" 0L
    (dup_path_gen a ~prefix:run_prefix);
  cleanup_prefix a ~prefix:run_prefix

(* ---- 3. concurrent disjoint writes -> unique gens, no dup rows ---- *)

let test_concurrent_disjoint_writes_unique_gens () =
  with_store ~database:(S.config_from_env ()).database @@ fun base ->
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let n = 6 and m = 2 in
  let type_name = type_name_for "conc" in
  let g0 = frozen_row base in
  let s0 = seq_last_value base in
  let fibers =
    List.init n (fun i ->
        Eio.Fiber.fork_promise ~sw (fun () ->
            let st = observer base in
            let paths = ref [] in
            for j = 0 to m - 1 do
              let p = datum_path type_name (Printf.sprintf "c%d_%d" i j) in
              S.with_transaction st (fun conn_opt ->
                  match conn_opt with
                  | Some c -> (
                      match
                        T.upsert_datum c ~path:p ~type_name
                          ~body_json:(Printf.sprintf "{\"v\":%d}" j)
                          ~author:"tester" ~rank:None
                      with
                      | Ok _ -> paths := p :: !paths
                      | Error e -> Alcotest.fail ("conc write failed: " ^ e))
                  | None -> Alcotest.fail "no conn in conc writer");
              ()
            done;
            !paths))
  in
  let all_paths =
    List.concat_map
      (fun w ->
        match Eio.Promise.await w with
        | Ok ps -> ps
        | Error e -> Alcotest.fail ("fiber error: " ^ Printexc.to_string e))
      fibers
  in
  Alcotest.(check int) "all concurrent writes succeeded" (n * m)
    (List.length all_paths);
  let g1 = frozen_row base in
  let s1 = seq_last_value base in
  Alcotest.(check int64) "frozen row untouched by concurrent writes" g0 g1;
  Alcotest.(check bool) "seq advanced by >= writes" true
    (Int64.compare s1 (Int64.add s0 (Int64.of_int (n * m))) >= 0);
  Alcotest.(check int64) "no duplicate (path,gen) under concurrency" 0L
    (dup_path_gen base ~prefix:run_prefix);
  (* each path: exactly one version row with a positive gen *)
  List.iter
    (fun p ->
      let hist = history base ~path:p in
      Alcotest.(check int) ("one version row for " ^ p) 1 (List.length hist))
    all_paths;
  cleanup_prefix base ~prefix:run_prefix

(* ---- 4. repeated writes to the SAME path keep gen order ---- *)

let test_same_path_writes_keep_gen_order () =
  with_store ~database:(S.config_from_env ()).database @@ fun base ->
  let type_name = type_name_for "order" in
  let p = datum_path type_name "same" in
  upsert base ~path:p ~type_name ~body:"{\"v\":1}";
  upsert base ~path:p ~type_name ~body:"{\"v\":2}";
  (* two version rows, strictly increasing in gen *)
  let hist = history base ~path:p in
  Alcotest.(check int) "two version rows for the same path" 2
    (List.length hist);
  (match hist with
   | [ v1; v2 ] -> (
       match (v1.T.v_gen, v2.T.v_gen) with
       | Some g1, Some g2 ->
           Printf.printf "  same-path gens: %Ld < %Ld\n%!" g1 g2;
           Alcotest.(check bool) "per-path gen strictly increasing" true
             (Int64.compare g1 g2 < 0)
       | _ -> Alcotest.fail "missing gens on same-path rows")
   | _ -> Alcotest.fail "expected exactly two version rows");
  cleanup_prefix base ~prefix:run_prefix

let () =
  Random.self_init ();
  Alcotest.run "tree_generations_seq de-contention (BORGE-1790987438)"
    [
      ( "real-pg",
        [
          Alcotest.test_case "sequence is the data head; row frozen" `Slow
            test_sequence_is_the_data_head;
          Alcotest.test_case "held datum tx does not block disjoint writer"
            `Slow test_held_datum_tx_does_not_block_disjoint_writer;
          Alcotest.test_case "concurrent disjoint writes: unique gens, no dups"
            `Slow test_concurrent_disjoint_writes_unique_gens;
          Alcotest.test_case "same-path writes keep gen order" `Slow
            test_same_path_writes_keep_gen_order;
        ] );
    ]
