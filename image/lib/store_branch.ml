(** Pricklypear Image — Store: branch lifecycle (i-1788671083-c split).
    Branch status/classification, persist + pin-hygiene, promote, merge,
    diff, fork/ensure, routes/types registration. *)
(* ---- branch status (branching.branch-status) ----

   Git-style per-name status of a branch against its parent chain, as ONE
   derived read of the tree-time substrate (tree_versions, migrations
   018-021) keyed on the base_gen merge-base coordinate (migration 024).
   Hash-first per tree-time: equal hash = prune; bodies are never fetched.

   Per name (fn | type | route), with values resolved closest-ancestor-wins
   (a tombstone or missing newest version on one branch means that branch
   is dead for the name and the walk continues up, matching the
   scaffolding/unbind semantics):
     b = branch value at own head
     p = parent-chain value at parent head
     a = parent-chain value at base_gen (the fork-time base)
     own = a live scaffolding row exists on the branch (the pin)

   HASH DOMAIN (unified 2026-09, migration 027 / one-tree.hash-domain):
   every writer — Tree_store upserts AND the 009 mirror triggers — now
   computes the hash DB-SIDE over PG's canonical jsonb text of the body
   (digest(body_jsonb::text)). One canonicalization, writer-independent.
   The 2026-08-28 cross-writer mismatch (OCaml compact-JSON digests vs
   mirror jsonb::text digests) is gone, the 027 backfill converted old
   rows, so status reads the STORED tree_versions.hash column directly
   (no more per-row query-time re-digest). The stored column is guarded
   by the cross-writer hash-equality test in tree_store_test.

   Tombstones are the reserved 019/020 marker (body->_tombstone). Deletion
   on a branch REVEALS the parent binding (unbind semantics), so there is
   no "locally deleted" status — a delete is a pin removal, not a divergent
   state.

   APPROXIMATION (documented in the spec): base_gen bounds ONLY the direct
   parent's versions; ancestors above the parent contribute their full
   head. Exact for the dominant topology (empty child of system/main);
   for deeper chains, post-fork writes on grandparents fold into the base.
   Accepted v1; revisit with per-ancestor bounds if it ever misleads. *)

type diff_kind = DiffFn | DiffType | DiffRoute

type st_status = SClean | SAheadNew | SAhead | SBehind | SDiverged | SSynced

let st_status_s = function
  | SClean -> "clean"
  | SAheadNew -> "ahead-new"
  | SAhead -> "ahead"
  | SBehind -> "behind"
  | SDiverged -> "diverged"
  | SSynced -> "synced"

type branch_status_row = {
  st_kind : diff_kind;              (* DiffFn | DiffType | DiffRoute *)
  st_name : string;                 (* fn/type name, or the route path *)
  st_status : st_status;
  st_own : bool;                    (* live pin on the branch *)
  st_branch_hash : string option;   (* b *)
  st_parent_hash : string option;   (* p *)
  st_base_hash : string option;     (* a *)
}

(* The spec's decision table, as code. Comparisons are hash equality with
   ABSENT (None) as its own value. DELEGATES to Branch_algebra.classify —
   the pure decision table lives there (formal-verification island 1, one
   table shared by the property tests and the store). *)
let classify_status ~(own : bool) ~(b : string option) ~(p : string option)
    ~(a : string option) : st_status =
  match Branch_algebra.classify ~own ~b ~p ~a with
  | Branch_algebra.Clean -> SClean
  | Branch_algebra.AheadNew -> SAheadNew
  | Branch_algebra.Ahead -> SAhead
  | Branch_algebra.Behind -> SBehind
  | Branch_algebra.Diverged -> SDiverged
  | Branch_algebra.Synced -> SSynced

(* One round trip per call: chain walk + status resolution for all three
   kinds (fn/type/route) + live-pin existence from the scaffolding tables.
   Read-only (plain Store_db.exec on the given conn — callers inside a transaction
   pass the tx conn so the read shares its snapshot).

   log/issues "/branches 21s" (2026-09-08): b_res and p_res were resolved
   from tree_versions — a full scan of EVERY gen of EVERY def on the
   chain, per branch, per page load. The live head of a code path IS the
   tree_nodes row (delete_fn physically removes the row; only the VERSION
   trail keeps a tombstone), so:
     - b_res (the body the branch RESOLVES to: own pin first, else the
       nearest ancestor) = closest-depth live row over own + ancestor
       prefixes \u2014 tombstone-free by construction (a deleted fn has
       no live row, exactly like the old tombstone-filtered DISTINCT ON
       over version heads).
     - p_res (closest ancestor's defs) = live rows under ancestor
       prefixes, closest depth wins — the same closest-wins the old
       heads/p_res computed, minus the history scan.
   a_res (base-resolved state) KEEPS the tree_versions walk: it must read
   historical gens (gen <= base_gen at depth 1) with the exact old
   tombstone-fallback semantics, and that scan is gen-bounded for the
   common (parent) case.
   keys reads b_res now: ancestor-only keys were joined and then dropped
   by the OCaml filter ((not own) && b = None), so excluding them from
   the key set changes nothing observable. $1 anchors the chain (uuid
   context); $2 is the same branch id in TEXT context for the live-path
   prefixes. *)
let branch_status_conn (conn : Store_db.conn) ~(branch_id : string)
  : branch_status_row list =
  (try
        let sql =
          Store_common.chain_sql (", own_live AS MATERIALIZED ( "^
          "  SELECT (string_to_array(path::text, '.'))[3] AS kind, "^
          "         (string_to_array(path::text, '.'))[4] AS key, hash "^
          "  FROM tree_nodes "^
          "  WHERE path <@ ('b.' || replace($2::text, '-', ''))::ltree "^
          "    AND (string_to_array(path::text, '.'))[3] IN ('fn','type','route') "^
          "), live_all AS MATERIALIZED ( "^
          "  SELECT 0 AS depth, kind, key, hash FROM own_live "^
          "  UNION ALL "^
          "  SELECT depth, kind, key, hash FROM anc_live "^
          "), b_res AS MATERIALIZED ( "^
          "  SELECT DISTINCT ON (kind, key) kind, key, hash AS b_hash "^
          "  FROM live_all ORDER BY kind, key, depth ASC "^
          "), anc_live AS ( "^
          "  SELECT c.depth, "^
          "         (string_to_array(tn.path::text, '.'))[3] AS kind, "^
          "         (string_to_array(tn.path::text, '.'))[4] AS key, tn.hash "^
          "  FROM chain c "^
          "  JOIN tree_nodes tn "^
          "    ON tn.path <@ ('b.' || replace(c.id::text, '-', ''))::ltree "^
          "  WHERE c.depth >= 1 "^
          "    AND (string_to_array(tn.path::text, '.'))[3] IN ('fn','type','route') "^
          "), p_res AS MATERIALIZED ( "^
          "  SELECT DISTINCT ON (kind, key) kind, key, hash AS p_hash "^
          "  FROM anc_live ORDER BY kind, key, depth ASC "^
          "), vers_base AS ( "^
          "  SELECT v.kind, "^
          "         (string_to_array(v.path::text, '.'))[4] AS key, "^
          "         c.depth, v.gen, v.hash, v.body "^
          "  FROM tree_versions v "^
          "  JOIN chain c ON c.id = v.branch_id "^
          "  WHERE v.kind IN ('fn','type','route') "^
          "    AND c.depth >= 1 "^
          "    AND (c.depth > 1 OR v.gen <= (SELECT COALESCE(base_gen, 0) "^
          "                                   FROM branches WHERE id = $1)) "^
          "), heads_base AS ( "^
          "  SELECT DISTINCT ON (kind, key, depth) kind, key, depth, "^
          "         hash AS chash, body "^
          "  FROM vers_base ORDER BY kind, key, depth, gen DESC "^
          "), a_res AS ( "^
          "  SELECT DISTINCT ON (kind, key) kind, key, chash AS a_hash "^
          "  FROM heads_base WHERE depth >= 1 AND (body->>'_tombstone') IS NULL "^
          "  ORDER BY kind, key, depth ASC "^
          "), own_fn AS ( "^
          "  SELECT tree_enc_label(name) AS key FROM function_defs "^
          "  WHERE branch_id = $1 "^
          "), own_ty AS ( "^
          "  SELECT tree_enc_label(name) AS key FROM type_defs "^
          "  WHERE branch_id = $1 "^
          "), own_rt AS ( "^
          "  SELECT tree_enc_label(route) AS key FROM view_bindings "^
          "  WHERE branch_id = $1 "^
          "), keys AS ( "^
          "  SELECT kind, key FROM live_all "^
          "  UNION SELECT 'fn', key FROM own_fn "^
          "  UNION SELECT 'type', key FROM own_ty "^
          "  UNION SELECT 'route', key FROM own_rt "^
          ") SELECT k.kind, tree_dec_label(k.key) AS name, "^
          "       b.b_hash, p.p_hash, a.a_hash, "^
          "       (o1.key IS NOT NULL) AS own_fn, "^
          "       (o2.key IS NOT NULL) AS own_ty, "^
          "       (o3.key IS NOT NULL) AS own_rt "^
          "FROM keys k "^
          "LEFT JOIN b_res b ON b.kind = k.kind AND b.key = k.key "^
          "LEFT JOIN p_res p ON p.kind = k.kind AND p.key = k.key "^
          "LEFT JOIN a_res a ON a.kind = k.kind AND a.key = k.key "^
          "LEFT JOIN own_fn o1 ON o1.key = k.key "^
          "LEFT JOIN own_ty o2 ON o2.key = k.key "^
          "LEFT JOIN own_rt o3 ON o3.key = k.key "^
          "ORDER BY k.kind, name")
        in
        let rows =
          Store_db.exec conn
            ~params:[
              Pgx.Value.of_string branch_id;
              Pgx.Value.of_string branch_id;
            ]
            sql
        in
        List.filter_map
          (fun row ->
            match row with
            | [ kind_c; name_c; bh_c; ph_c; ah_c; ofn_c; oty_c; ort_c ] ->
                let kind =
                  match Pgx.Value.to_string kind_c with
                  | Some "type" -> DiffType
                  | Some "route" -> DiffRoute
                  | _ -> DiffFn
                in
                let name = Pgx.Value.to_string name_c in
                let h c = Pgx.Value.to_string c in
                let b = h bh_c and p = h ph_c and a = h ah_c in
                let own =
                  match kind with
                  | DiffFn -> Pgx.Value.to_bool ofn_c = Some true
                  | DiffType -> Pgx.Value.to_bool oty_c = Some true
                  | DiffRoute -> Pgx.Value.to_bool ort_c = Some true
                in
                (match name with
                 | None -> None
                 | Some name ->
                     (* Nothing live anywhere (tombstoned on every head):
                        both sides agree it is gone - nothing to report. *)
                     if (not own) && b = None then None
                     else
                       Some
                         { st_kind = kind;
                           st_name = name;
                           st_status =
                             classify_status ~own ~b ~p ~a;
                           st_own = own;
                           st_branch_hash = b;
                           st_parent_hash = p;
                           st_base_hash = a })
            | _ -> None)
          rows
        with exn -> Store_db.reraise ~op:"branch_status" exn)

(* log/issues "/branches 21s" (2026-09-08): the page recomputes
   branch_status for EVERY branch per render — N chain-walk queries.
   A result is a pure function of (chain live defs, own pins, base_gens);
   all three are covered by the trigger-maintained monotonic def_gen
   stamps (migration 028; fn/type/route tree writes AND the reverse
   mirror that reaches tree_nodes from scaffolding-direct promote/merge
   writes) plus base_gen over the chain. So: stamp-keyed memo.
     - stamp query is one aggregate over the (tiny) branches join;
     - pre-migration (no def_gen column) or any stamp failure → "" →
       compute directly, no caching (visible_stamp's fallback shape);
     - inside a transaction (share-snapshot callers) → compute directly,
       never cached — a deferred-mirror window must read its own tx;
     - single-domain fibers: find/replace are non-yielding, so plain
       mutable state is fiber-atomic; a lost update just recomputes;
     - cap 256 branches, clear-on-overflow. *)
let bs_cache : (string, string * branch_status_row list) Hashtbl.t =
  Hashtbl.create 16

let bs_stamp_sql =
  (* SUM, not MAX: main's counter dwarfs a garden branch's, so a bump on
     the branch itself must move the aggregate. Sum over the chain moves
     on ANY member's bump; the counts catch tree_branches-row appear/
     disappear (fork/delete) and base_gen moves (merge advance). *)
  "WITH RECURSIVE chain AS ( "
  ^ "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $1::uuid "
  ^ "  UNION ALL "
  ^ "  SELECT b.id, b.parent_id, c.depth + 1 "
  ^ "  FROM branches b JOIN chain c ON b.id = c.parent_id "
  ^ ") SELECT COALESCE(sum(d.def_gen), -1)::text || '|' || "
  ^ "count(d.def_gen)::text || '|' || "
  ^ "COALESCE(sum(b.base_gen), -1)::text || '|' || count(*)::text "
  ^ "FROM chain c "
  ^ "JOIN branches b ON b.id = c.id "
  ^ "LEFT JOIN tree_branches d ON d.branch_id = c.id"

let branch_status (t : Store_db.t) ~(branch_id : string) : branch_status_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      if Store_db.in_transaction t then branch_status_conn conn ~branch_id
      else
        let stamp =
          match
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string branch_id ]
              bs_stamp_sql
          with
          | [ [ c ] ] -> (match Pgx.Value.to_string c with Some s -> s | None -> "")
          | _ -> ""
        in
        if stamp = "" || String.starts_with ~prefix:"|" stamp then
          branch_status_conn conn ~branch_id
        else
          (match Hashtbl.find_opt bs_cache branch_id with
           | Some (k, rows) when k = stamp -> rows
           | _ ->
               let rows = branch_status_conn conn ~branch_id in
               if Hashtbl.length bs_cache >= 256 then Hashtbl.reset bs_cache;
               Hashtbl.replace bs_cache branch_id (stamp, rows);
               rows)


(* Opaque-body contract (i-1788671083-c): returns the RAW jsonb body text;
   the caller parses via [Ast.expr_of_body_json]. *)
let lookup_function_on_branch (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : (string * string * string * string option) option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT body::text, author, updated_at::text, doc FROM function_defs " ^
            "WHERE branch_id = $1 AND name = $2" in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string branch_id;
                        Pgx.Value.of_string name ]
              sql
          in
          (match rows with
           | [ [ body_c; author_c; upd_c; doc_c ] ] ->
               (match Pgx.Value.to_string body_c,
                       Pgx.Value.to_string author_c,
                       Pgx.Value.to_string upd_c with
                | Some body_str, Some author, Some updated ->
                    let doc = Pgx.Value.to_string doc_c in
                    Some (body_str, author, updated, doc)
                | _ -> None)
           | _ -> None)
    with exn -> Store_db.reraise ~op:"lookup_function_on_branch" exn)

let delete_function_on_branch (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : bool =
  match t.conn with
  | None -> false
  | Some conn ->
      (try
        match Tree_store.delete_fn conn ~branch_id ~name with
        | Ok _ -> true
        | Error _ -> false
     with exn -> Store_db.reraise ~op:"delete_function_on_branch" exn)

(** Copy selected function names from source branch to target (upsert).
    ATOMIC (branching.promote): the whole promote — fn upserts, route
    upserts, head-hash refresh — runs in ONE SERIALIZABLE transaction;
    any failure rolls back everything, so a crash mid-promote can no
    longer leave a half-promoted target (merge_branch already had this
    shape; this closes the same gap for promote). *)
let promote_functions (t : Store_db.t) ~(source_id : string) ~(target_id : string)
    ~(names : string list) : int =
  match t.conn with
  | None -> 0
  | Some _ when names = [] -> 0
  | Some _ ->
      (try
        Store_db.with_transaction t (fun conn ->
        match conn with
        | None -> 0
        | Some conn ->
      let n =
            List.fold_left
              (fun acc name ->
                let src =
                  lookup_function_on_branch t ~branch_id:source_id ~name
                in
                match src with
                | None -> acc
                | Some _ ->
                    let sql =
                      "INSERT INTO function_defs " ^
                      "  (branch_id, name, body, author, doc, package) " ^
                      "SELECT $1, name, body, author, doc, package " ^
                      "FROM function_defs " ^
                      "WHERE branch_id = $2 AND name = $3 " ^
                      "ON CONFLICT (branch_id, name) DO UPDATE " ^
                      "SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
                      "    doc = EXCLUDED.doc, package = EXCLUDED.package, " ^
                      "    updated_at = now() RETURNING name" in
                    let rows =
                      Store_db.exec conn
                        ~params:[ Pgx.Value.of_string target_id;
                                  Pgx.Value.of_string source_id;
                                  Pgx.Value.of_string name ]
                        sql
                    in
                    (if rows = [] then acc else acc + 1))
              0 names
          in
          (* tmp/eval-fast-path: promote/fork-copy journal (scaffolding
             direct writes; the mirror reaches tree_nodes). *)
          let () =
            List.iter
              (fun name -> Def_journal.record_upsert ~branch_id:target_id ~name)
              names
          in
          (* branching.promote ROUTES TRAVEL WITH THEIR HANDLER:
             upsert source view_bindings whose handler is a promoted name;
             preserve visibility. Non-promoted handlers' routes stay put. *)
          let () =
            List.iter
              (fun name ->
                let sql =
                  "INSERT INTO view_bindings " ^
                  "  (branch_id, route, handler, visibility) " ^
                  "SELECT $1, route, handler, " ^
                  "       COALESCE(visibility, 'private') " ^
                  "FROM view_bindings " ^
                  "WHERE branch_id = $2 AND handler = $3 " ^
                  "ON CONFLICT (branch_id, route) DO UPDATE " ^
                  "SET handler = EXCLUDED.handler, " ^
                  "    visibility = EXCLUDED.visibility"
                in
                let _ =
                  Store_db.exec conn
                    ~params:
                      [ Pgx.Value.of_string target_id
                      ; Pgx.Value.of_string source_id
                      ; Pgx.Value.of_string name ]
                    sql
                in
                (* tmp/eval-fast-path: promoted routes dirty the target. *)
                Def_journal.record_routes ~branch_id:target_id;
                ())
              names
          in
          (* 009 mirror triggers are DEFERRED to COMMIT; force them to fire
             NOW inside the tx so the head-hash refresh sees the promoted
             nodes. Firing early is safe — same triggers, same order, just
             before commit instead of at it — and a mirror failure here
             rolls the WHOLE promote back, which is the point. *)
          let (_ : Pgx.row list) =
            Store_db.exec conn "SET CONSTRAINTS ALL IMMEDIATE"
          in
          let () = Tree_store.recompute_head_hash conn ~branch_id:target_id in
          n)
      with exn -> Store_db.reraise ~op:"promote_functions" exn)

(* [inherited_fn_body_matches conn ~branch_id ~name ~body_json]: walks
   the PARENT chain of [branch_id] (the branch itself is excluded, so
   system/main with no parent never matches), first own body closest to
   the branch wins, and is true iff that inherited body is canonically
   identical to [body_json]. Both sides are normalized by PG's
   jsonb::text (key order + spacing), so the comparison is content
   equality regardless of which write path produced the row — the same
   single-domain rule branch_status recorded 2026-08-28.
   branching.pin-hygiene: write-time suppression + unpin-on-resync. *)
let inherited_fn_body_matches (conn : Store_db.conn) ~(branch_id : string)
    ~(name : string) ~(body_json : string) : bool =
  try
    let sql =
      Store_common.chain_sql ~anchor:"(SELECT parent_id FROM branches WHERE id = $1::uuid)"
        ("SELECT fd.body::text = $3::jsonb::text " ^
      "FROM function_defs fd JOIN chain c ON fd.branch_id = c.id " ^
      "WHERE fd.name = $2 ORDER BY c.depth ASC LIMIT 1") in
    (match
       Store_db.exec conn
         ~params:[ Pgx.Value.of_string branch_id;
                   Pgx.Value.of_string name;
                   Pgx.Value.of_string body_json ]
         sql
     with
     | [ [ cell ] ] -> Pgx.Value.to_bool cell = Some true
     | _ -> false)
  with exn -> Store_db.reraise ~op:"inherited_fn_body_matches" exn

(* Persist a function def. INSERT ... ON CONFLICT (branch_id, name)
   DO UPDATE so redefining a name in the same branch upserts. The body
   is serialized to JSONB. [doc] writes function_defs.doc; None clears
   (NULL) so stale docs do not survive a docstring-free redefine.

   branching.pin-hygiene (2026-12-30): NO DIFF => NO PIN. Before writing,
   compare against the body the branch would INHERIT (parent chain,
   closest-wins). If identical: do not write, and if an own pin exists
   UNPIN it (delete_fn), revealing the parent. A contentless pin freezes
   today's parent value with no intent behind it; it must not exist.
   system/main never suppresses (empty parent chain); the boot-seed uses
   persist_function_if_absent, which bypasses this path. *)
(* [body_json] is the RAW jsonb body text (opaque store contract,
   i-1788671083-c): the caller serializes via [Ast.expr_to_body_json]. *)
let persist_function (t : Store_db.t) (name : string)
    (body_json : string)
    ?(doc : string option = None)
    ?(package : string option = None)
    (author : string)
  : unit =
  match t.conn with
  | None -> ()  (* no DB; env binding still happened in-memory *)
  | Some conn ->
      (try
          if inherited_fn_body_matches conn ~branch_id:t.branch_id ~name
               ~body_json
          then
            (* No diff => no pin: drop any own row, revealing the parent.
               Never reached on system/main (empty parent chain). *)
            (try
               let _ =
                 Tree_store.delete_fn conn ~branch_id:t.branch_id ~name
               in
               ()
             with exn ->
               (* PROBE (i-1788671060-2 keep): best-effort post-success
                  cleanup — the persist already landed; a failed unpin is
                  logged and retried on the next resync, never blocks the
                  write it trails. *)
               Printf.eprintf
                 "[store] persist_function unpin-on-sync error: %s\n"
                 (Printexc.to_string exn))
          else
          (* tree is truth; reverse trigger mirrors to table. Package
             provenance is scaffolding-only — stamped on function_defs
             directly since the tree node envelope has no package field. *)
            let _ =
              Tree_store.upsert_fn conn ~branch_id:t.branch_id ~name
                ~body_json ~author ~doc
            in
            (match package with
             | None -> ()
             | Some pkg ->
                 let sql =
                   "UPDATE function_defs SET package = $3 " ^
                   "WHERE branch_id = $1 AND name = $2" in
                 let _ =
                   Store_db.exec conn
                     ~params:[ Pgx.Value.of_string t.branch_id;
                               Pgx.Value.of_string name;
                               Pgx.Value.of_string pkg ]
                     sql in
                 ())
    with exn -> Store_db.reraise ~op:"persist_function" exn)

(** image.boot-seed-batch: preload the set of function names and route
    paths already present on the current branch (system/main during boot
    seed) so persist_function_if_absent / register_route_if_absent skip
    the per-name get_by_path SELECT. Call inside the seed with_branch on
    system/main; pair with [end_seed_names] in a finally. *)
let begin_seed_names (t : Store_db.t) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      let set = Hashtbl.create 1024 in
      (try
         let sql = "SELECT name FROM " ^ Store_common.fn_rel () ^ " WHERE branch_id = $1" in
         let rows = Store_db.exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql in
         List.iter (fun row ->
             match row with
             | [ name_c ] ->
                 (match Pgx.Value.to_string name_c with
                  | Some n -> Hashtbl.replace set n ()
                  | None -> ())
             | _ -> ()) rows
       with exn ->
         (* PROBE (i-1788671060-2 keep): boot-seed OPTIMIZATION — preload
            the already-present names so seed_one can skip per-name
            SELECTs; a failure just means no skipping (full seed pass
            proceeds). The boot-seed-batch doc above covers the shape. *)
         Printf.eprintf "[store] begin_seed_names (fns) error: %s\n"
           (Printexc.to_string exn));
      (try
         let sql = "SELECT route FROM " ^ Store_common.rt_rel () ^ " WHERE branch_id = $1" in
         let rows = Store_db.exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql in
         List.iter (fun row ->
             match row with
             | [ route_c ] ->
                 (match Pgx.Value.to_string route_c with
                  | Some r -> Hashtbl.replace set r ()
                  | None -> ())
             | _ -> ()) rows
       with exn ->
         (* PROBE — same boot-seed-skip optimization as the fn half above. *)
         Printf.eprintf "[store] begin_seed_names (routes) error: %s\n"
           (Printexc.to_string exn));
      t.seed_names <- Some set

let end_seed_names (t : Store_db.t) : unit =
  t.seed_names <- None

(** branching.seed-policy: insert only if (branch_id, name) absent.
    Existing main (or any branch) rows are left untouched. Used by boot
    seed so deploy cannot clobber live promote. *)
let persist_function_if_absent (t : Store_db.t) (name : string)
    (body_json : string)  (* RAW jsonb body text, i-1788671083-c *)
    ?(doc : string option = None)
    ?(package : string option = None)
    (author : string)
  : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (* image.boot-seed-batch: during the additive boot seed pass, consult
         the preloaded name set instead of issuing a get_by_path SELECT per
         define. Additive seed never overwrites existing rows, so an
         existing name is a full skip. A genuinely new name upserts and
         records itself so later duplicate defines skip too. *)
      (match t.seed_names with
       | Some set when Hashtbl.mem set name -> ()
       | _ ->
      (try
          (* seed-policy: never overwrite body; fill package provenance
             on the existing scaffolding row when untagged. *)
            let exists =
              Tree_store.get_by_path conn
                ~path:(Tree_store.fn_path ~branch_id:t.branch_id ~name) in
            let stamp_pkg_if_null () =
              match package with
              | None -> ()
              | Some pkg ->
                  let sql =
                    "UPDATE function_defs SET package = $3 " ^
                    "WHERE branch_id = $1 AND name = $2 AND package IS NULL" in
                  let _ =
                    Store_db.exec conn
                      ~params:[ Pgx.Value.of_string t.branch_id;
                                Pgx.Value.of_string name;
                                Pgx.Value.of_string pkg ]
                      sql in
                  ()
            in
            (match exists with
             | Some _ -> stamp_pkg_if_null ()
             | None ->
                 let _ =
                   Tree_store.upsert_fn conn ~branch_id:t.branch_id ~name
                     ~body_json ~author ~doc
                 in
                 stamp_pkg_if_null ());
            (match t.seed_names with
             | Some set -> Hashtbl.replace set name ()
             | None -> ())
      with exn -> Store_db.reraise ~op:"persist_function_if_absent" exn))

(** Visible docstring for [name], or None. *)
let lookup_function_doc (t : Store_db.t) (name : string) : string option =
  let found = Store_defs.lookup_function t name in
  match found with
  | Some (_, _, Some d) when String.trim d <> "" -> Some d
  | _ -> None

let list_branches (t : Store_db.t) : Pricklypear_common.Branch.t list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT id::text, COALESCE(parent_id::text, ''), " ^
            "       owner, name, created_at::text, " ^
            "       COALESCE(last_synced::text, ''), " ^
            "       COALESCE(base_gen, 0)::bigint " ^
            "FROM branches ORDER BY owner, name" in
          let rows = Store_db.exec conn sql in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ id_c; pid_c; owner_c; name_c; created_c; synced_c; bg_c ] ->
                     (match Pgx.Value.to_string id_c,
                              Pgx.Value.to_string owner_c,
                              Pgx.Value.to_string name_c,
                              Pgx.Value.to_string created_c with
                      | Some id, Some owner, Some name, Some created ->
                          let parent_id =
                            match Pgx.Value.to_string pid_c with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          let last_synced =
                            match Pgx.Value.to_string synced_c with
                            | Some "" | None -> None
                            | Some s -> Some s
                          in
                          let base_gen =
                            match Pgx.Value.to_int64 bg_c with
                            | Some g -> g
                            | None -> 0L
                          in
                          let base =
                            Pricklypear_common.Branch.make
                               ~id ~parent_id ~owner ~name
                               ~created_at:created ~base_gen ()
                          in
                          Some
                            (match last_synced with
                             | Some s -> { base with last_synced = Some s }
                             | None -> base)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn -> Store_db.reraise ~op:"list_branches" exn)

(* Look up a branch id by (owner, name). Used by (fork ...) to find
   the parent's UUID from its Lisp-level reference. Returns None if
   the branch isn't in the DB (or no DB). *)
let lookup_branch_id_by_name (t : Store_db.t) ~(owner : string) ~(name : string)
  : string option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
      let sql = "SELECT id::text FROM branches WHERE owner = $1 AND name = $2" in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string owner;
                        Pgx.Value.of_string name ]
              sql
          in
          (match rows with
           | [ [ id_cell ] ] ->
               (Pgx.Value.to_string id_cell)
           | _ -> None)
    with exn -> Store_db.reraise ~op:"lookup_branch_id" exn)

(* Switch the store's branch_id: subsequent lookups/persists/registers
   will target the new branch. Per spec [image.repl-experience]:
   "(checkout branch-id)" keeps REPL locals (env) but changes visibility
   (which (define)d functions are effective). We mutate the field.

   Re-loading the env is the caller's responsibility (session.eval_one
   does it after we return successfully). *)
let checkout_branch (t : Store_db.t) ~(new_branch_id : string) : unit =
  t.branch_id <- new_branch_id;
  ()

(* Get the branch_id the store is currently bound to. *)
let current_branch_id (t : Store_db.t) : string = t.branch_id

(** Current generation counter for [branch_id] (migration 017). The SQL
    helper [current_branch_generation] walks the branch's ancestry chain
    and returns the max generation across it, so a write to an ancestor
    (e.g. main) is automatically visible to every descendant. The remote
    client stores defs under a [(branch, generation)] key and does a
    single cheap round trip against this to decide whether its local
    def cache is stale. Returns None when the DB is unreachable / the
    helper is missing (pre-017), letting callers treat an unknown
    generation as "reload". *)
let current_generation (t : Store_db.t) ~(branch_id : string) : int64 option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
         let sql =
           "SELECT COALESCE(current_branch_generation($1::uuid), 0)::bigint"
         in
         let rows =
           Store_db.exec conn
             ~params:[ Pgx.Value.of_string branch_id ]
             sql
         in
         match rows with
         | [ [ cell ] ] ->
             Option.bind (Pgx.Value.to_int64 cell) (fun g -> Some g)
         | _ -> None
       with exn -> Store_db.reraise ~op:"current_generation" exn)

(** Canonical system/main UUID — seeded by migrations/001_branches.sql. *)
let system_main_id = "00000000-0000-0000-0000-000000000001"

(** Delete the (branch_id, name) row from function_defs. Used by
    (unbind name): removes the CURRENT branch's override so the name
    resolves via inheritance to the shadowed parent binding. Refuses
    to delete on system/main (the inheritance root). Returns true if
    a row was deleted, false if no branch-local row existed. *)
let delete_function (t : Store_db.t) (name : string) : bool =
  if t.branch_id = system_main_id then
    raise (Failure "unbind: cannot unbind on system/main")
  else
    match t.conn with
    | None -> false
    | Some conn ->
        (try
          (* tree is truth; reverse mirror deletes the function_defs row *)
          match Tree_store.delete_fn conn ~branch_id:t.branch_id ~name with
          | Ok _ -> true
          | Error _ -> false
        with exn -> Store_db.reraise ~op:"delete_function" exn)

(** branching.pin-hygiene: (prune-synced). Removes every SYNCED FN pin on
    the CURRENT branch — pins whose body is canonically identical to the
    parent-chain body (contentless pin noise: historical full-copy forks,
    personal load-library upserts, post-promote residue). ahead /
    ahead-new / diverged pins are untouched (they carry a real diff);
    types and routes are untouched (unbind is fn-only for v1). Deletion
    is unbind semantics: the parent binding is revealed, nothing is lost.
    system/main returns [] (a root pin can never be synced).

    The deletions run in ONE SERIALIZABLE transaction: deferred triggers
    (009 reverse mirror, 018-021 version shadow) flush once at commit,
    so a multi-thousand-pin sweep is a couple of SQL round trips, not a
    per-pin autocommit procession (prod sweep 2026-12-30: ~0.7s/pin
    one-shot -> sub-second for ~1.2k pins in-tx). Returns the names
    unpinned. *)
let prune_synced_fn_pins (t : Store_db.t) : string list =
  if t.branch_id = system_main_id then []
  else
    let rows = branch_status t ~branch_id:t.branch_id in
    let synced =
      List.filter_map
        (fun (r : branch_status_row) ->
          if r.st_kind = DiffFn && r.st_status = SSynced then
            Some r.st_name
          else None)
        rows
    in
    (match synced with
     | [] -> []
     | names ->
         (try
            Store_db.with_transaction t (fun conn ->
              match conn with
              | None -> []
              | Some conn ->
                  List.filter
                    (fun name ->
                      try
                        match
                          Tree_store.delete_fn conn ~branch_id:t.branch_id
                            ~name
                        with
                        | Ok _ -> true
                        | Error _ -> false
                      with exn ->
                        (* PROBE (i-1788671060-2 keep): per-pin batch
                           semantics — one un-prunable pin must not abort
                           the prune of the others; it stays a pin and the
                           failure is logged. *)
                        Printf.eprintf
                          "[store] prune_synced_fn_pins %s: %s\n" name
                          (Printexc.to_string exn);
                        false)
                    names)
          with exn -> Store_db.reraise ~op:"prune_synced_fn_pins" exn))

(** Temporarily rebind [t.branch_id] for the duration of [f], then restore.
    Used so seed_stdlib can write package defaults to system/main without
    polluting the user's personal branch. *)
let with_branch (t : Store_db.t) ~(branch_id : string) (f : unit -> 'a) : 'a =
  let old = t.branch_id in
  t.branch_id <- branch_id;
  Fun.protect ~finally:(fun () -> t.branch_id <- old) f

(** Ensure a branch row exists; create an EMPTY child (parent pointer only)
    if missing. Does NOT copy function_defs/types/routes — inheritance via
    the recursive CTE makes parent defs visible, and personal overrides are
    just rows on the child. This is the right shape for user/personal. *)
let ensure_branch (t : Store_db.t) ~(parent_id : string) ~(owner : string)
    ~(name : string) : string option =
  let existing = lookup_branch_id_by_name t ~owner ~name in
  match existing with
  | Some id -> (Some id)
  | None ->
      (match t.conn with
       | None -> None
       | Some conn ->
           (try
      let sql =
            "INSERT INTO branches (id, parent_id, owner, name, base_gen) " ^
            "VALUES (gen_random_uuid(), $1, $2, $3, " ^
            "        COALESCE((SELECT generation FROM branch_generations " ^
            "                  WHERE branch_id = $1), 0)) " ^
            "ON CONFLICT (owner, name) DO UPDATE SET name = EXCLUDED.name " ^
            "RETURNING id::text" in
               let rows =
                 Store_db.exec conn
                   ~params:[ Pgx.Value.of_string parent_id;
                             Pgx.Value.of_string owner;
                             Pgx.Value.of_string name ]
                   sql
               in
               (match rows with
                | [ [ id_cell ] ] -> (Pgx.Value.to_string id_cell)
                | _ ->
                    (* Race: another image created it; re-lookup. *)
                    lookup_branch_id_by_name t ~owner ~name)
    with exn ->
      (* Narrow probe-recovery (i-1788671060-2 keep): only a genuine
         unique race re-looks-up (the loser of a concurrent create).
         ON CONFLICT DO UPDATE above already absorbs the common insert
         race; every other failure is real and propagates — a failed
         create must not masquerade as "some other branch id". *)
      if Store_db.is_unique_violation exn then
        lookup_branch_id_by_name t ~owner ~name
      else Store_db.reraise ~op:"ensure_branch" exn))

(* Fork a branch: create an EMPTY lens with parent_id as its parent.
   one-tree P5 / branching.personal-workspace: NO copy of code rows.
   Inheritance is the recursive-CTE / path-prefix walk; child pins are
   rows written later on the child. Data stays shared (branching.data).
   Returns the new branch_id. Single SERIALIZABLE insert. *)
(* Persist a route binding: (route "/path" 'handler-fn-name).
   INSERT ... ON CONFLICT (branch_id, route) DO UPDATE — so re-defining
   a route overrides the existing binding. *)
let normalize_visibility (v : string) : string =
  match String.lowercase_ascii (String.trim v) with
  | "public" -> "public"
  | _ -> "private"

(** Persist a route. [visibility_opt]:
    - Some v → set visibility (public|private)
    - None → on INSERT default private; on UPDATE keep existing visibility
      (handler-only re-register does not clobber public). *)
let register_route (t : Store_db.t) ~(route : string) ~(handler : string)
    ?visibility ()
  : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let vis =
            match visibility with
            | Some v -> normalize_visibility v
            | None -> "private"
          in
          ignore
            (Tree_store.upsert_route conn ~branch_id:t.branch_id ~route
               ~handler ~visibility:vis)
    with exn -> Store_db.reraise ~op:"register_route" exn)

(** branching.seed-policy: insert route only if absent on this branch. *)
let register_route_if_absent (t : Store_db.t) ~(route : string) ~(handler : string)
    ?visibility ()
  : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (* image.boot-seed-batch: skip the per-route get_by_path SELECT during
         additive boot seed when the route is already in the preloaded set. *)
      (match t.seed_names with
       | Some set when Hashtbl.mem set route -> ()
       | _ ->
      (try
      let vis =
            match visibility with
            | Some v -> normalize_visibility v
            | None -> "private"
          in
          (* seed-policy: insert route only if the tree node is absent *)
            let exists =
              Tree_store.get_by_path conn
                ~path:(Tree_store.route_path ~branch_id:t.branch_id ~route) in
            (match exists with
             | Some _ -> ()
             | None ->
                 ignore
                   (Tree_store.upsert_route conn ~branch_id:t.branch_id
                      ~route ~handler ~visibility:vis));
            (match t.seed_names with
             | Some set -> Hashtbl.replace set route ()
             | None -> ())
      with exn -> Store_db.reraise ~op:"register_route_if_absent" exn))

(* List all route bindings visible to this branch (recursive-CTE walk,
   same as list_function_names — annotations from ancestors are visible
   via inheritance, child's definition OVERRIDES parent's). Returns
   (route, handler, visibility) ordered with closest ancestor first. *)
let list_routes (t : Store_db.t) : (string * string * string) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT vb.route, vb.handler, " ^
            "COALESCE(vb.visibility, 'private') " ^
            "FROM " ^ Store_common.rt_rel () ^ " vb " ^
            "JOIN chain ON vb.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC") in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ route_c; handler_c; vis_c ] ->
                     (match Pgx.Value.to_string route_c,
                             Pgx.Value.to_string handler_c with
                      | Some r, Some h ->
                          let vis =
                            match Pgx.Value.to_string vis_c with
                            | Some v -> normalize_visibility v
                            | None -> "private"
                          in
                          Some (r, h, vis)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn -> Store_db.reraise ~op:"list_routes" exn)

(* ---- type_defs ----

   Per spec [storage.types]: user declares a type with (deftype 'name
   field1 field2 ...). The declaration is INSERTed into type_defs with
   branch_id = current branch and fields = JSON array of strings (the
   field names, in declared order).

   The data DOESN'T live in a per-type table: packed in migration 003's
   note that generating one PG table per Lisp type would require
   runtime DDL breaking additive-migrations-only. Instead, all data
   rows are stored in the shared data_rows(type_name, fields jsonb)
   table, and type_defs is pure declaration metadata. (Type
   declarations MAY eventually guide field-level validation; for now
   they're documentation.)

   The recursive-CTE override walk applies to type_defs the same as
   function_defs / view_bindings: ancestors' types are visible, child's
   definition wins on name conflict. *)

let persist_type (t : Store_db.t) ~(name : string) ~(fields : string list)
    ~(author : string) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let fields_json =
            Yojson.Safe.to_string
              (`List (List.map (fun s -> `String s) fields)) in
          ignore
            (Tree_store.upsert_type conn ~branch_id:t.branch_id ~name
               ~fields_json ~author)
    with exn -> Store_db.reraise ~op:"persist_type" exn)

(* List all type declarations visible to this branch via the recursive
   CTE override walk. Returns (name, fields) pairs with closest
   ancestor first; caller-side Hashtbl first-occurrence dedup wins. *)
let list_types (t : Store_db.t) : (string * string list) list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT td.name, td.fields::text FROM " ^ Store_common.ty_rel () ^ " td " ^
            "JOIN chain ON td.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC") in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id ]
              sql
          in
          (List.filter_map
               (fun row ->
                 match row with
                 | [ name_cell; fields_cell ] ->
                     (match Pgx.Value.to_string name_cell,
                             Pgx.Value.to_string fields_cell with
                      | Some name, Some fs ->
                          (try
                             let fs' =
                               match Yojson.Safe.from_string fs with
                               | `List lst ->
                                   List.filter_map
                                     (fun j ->
                                       match j with
                                       | `String s -> Some s
                                       | _ -> None)
                                     lst
                               | _ -> []
                             in Some (name, fs')
                           with _ -> None)
                      | _ -> None)
                 | _ -> None)
               rows)
    with exn -> Store_db.reraise ~op:"list_types" exn)

(** Field names for a type visible on the current branch (CTE walk).
    None if the type is not declared. *)
let lookup_type_fields (t : Store_db.t) ~(name : string) : string list option =
  let types = list_types t in
  match List.find_opt (fun (n, _) -> n = name) types with
  | Some (_, fields) -> Some fields
  | None -> None

let fork_branch (t : Store_db.t) ~(parent_id : string) ~(owner : string)
    ~(child_name : string) : string option =
  (* one-tree P5: EMPTY LENS fork.
     Create a branch row with parent_id only — NO copy of function_defs /
     type_defs / view_bindings. Inheritance is the recursive-CTE / path-
     prefix walk (child shadows parent). Matches ensure_branch and
     branching.personal-workspace ("empty child of main"). Copy-on-fork
     froze inheritance and contradicted the lens model.
     tree_branches lens registry is maintained by the branches→tree
     deferred trigger (009). head_hash is recomputed (empty → null/empty).
     Single SERIALIZABLE tx; rollback-safe. *)
  (try
      Store_db.with_transaction t (fun conn ->
        match conn with
        | None -> None
        | Some conn ->
            let insert_branch_sql =
              "INSERT INTO branches (id, parent_id, owner, name, base_gen) " ^
              "VALUES (gen_random_uuid(), $1, $2, $3, " ^
              "        COALESCE((SELECT generation FROM branch_generations " ^
              "                  WHERE branch_id = $1), 0)) " ^
              "RETURNING id::text" in
            let rows =
              Store_db.exec conn
                ~params:[ Pgx.Value.of_string parent_id;
                          Pgx.Value.of_string owner;
                          Pgx.Value.of_string child_name ]
                insert_branch_sql
            in
            (match rows with
             | [ [ id_cell ] ] ->
                 (match Pgx.Value.to_string id_cell with
                  | Some new_branch_id ->
                      (* Refresh head_hash on the new empty lens (0 own nodes). *)
                      let () =
                        Tree_store.recompute_head_hash conn
                          ~branch_id:new_branch_id
                      in
                      (Some new_branch_id)
                  | None -> None)
             | _ -> None))
    with exn -> Store_db.reraise ~op:"fork_branch" exn)

(* Merge a source branch's function_defs into a target branch.

   Per spec [branching.merge]:
     (merge 'alice/experiment 'alice/main)
   -> copy all function_defs from source to target (upsert).
   Policy: admin-only merges to main; per-user can merge into their
   own branches. Policy is enforced at the Lisp intercept layer
   (session.ml); Store.merge_branch just does the SQL.

   branching.branch-status S3: status-driven merge. Into the source's
   PARENT: upsert ONLY names the source moved since base_gen that the
   parent did NOT also move (ahead-new / ahead); behind names would
   clobber the parent's newer work and are skipped; synced/clean names
   are already identical. DIVERGED names (both sides moved since the
   fork base) are NEVER auto-applied: they are returned in the conflict
   report and need an explicit human decision (obituary rule — no
   silent reversals; resolution is re-define + re-merge, not LWW).
   Any other target (no shared base): head-vs-head fallback — upsert
   source fns whose body differs from the target's own row, no report.
   Schema-merge for type_defs is deliberately not implemented: only
   main accepts schema changes, via special migration-governed merge.

   Wrapped in a SERIALIZABLE transaction via [Store_db.with_transaction] since
   it16 (previously best-effort atomic — carried-forward since it9). *)
type merge_result = {
  count : int;
  conflicts : string list;
}

(* ---- branch diff (branching.diff) ---- *)

type diff_status = DiffAdded | DiffRemoved | DiffModified

(** One divergent row across two branches' effective views. [from_body]/
    [to_body] are the RAW stored texts (fn: the jsonb AST body; types: the
    fields JSON; routes: the handler) — opaque-body contract,
    i-1788671083-c: pretty-printing + AST parsing happen at the rendering
    boundary via [Ast.expr_of_body_json] / [Reader.pp_expr].
    [from_updated]/[to_updated] are ISO timestamps (None when the side is
    absent).

    Scope = merge scope: function_defs + type_defs + view_bindings;
    NEVER data_rows (shared, branch-free). *)
type branch_diff_row = {
  diff_name : string;
  diff_kind : diff_kind;
  diff_status : diff_status;
  from_body : string option;
  to_body : string option;
  from_updated : string option;
  to_updated : string option;
}

(** Format merge result for the (merge ...) prim / UI. *)
let format_merge_result (r : merge_result) : string =
  match r.conflicts with
  | [] -> Printf.sprintf "merged %d rows" r.count
  | cs ->
      Printf.sprintf "merged %d rows (%d conflicts: %s)"
        r.count (List.length cs) (String.concat ", " cs)

let merge_branch (t : Store_db.t) ~(source_id : string) ~(target_id : string)
  : merge_result option =
  (* Is this a merge into the source's direct parent? Read BEFORE the tx
     (the value is stable); the base advance runs AFTER the tx commits,
     because the deferred gen-bump triggers (017/019) fire at COMMIT —
     reading branch_generations inside the tx sees the PRE-merge counter. *)
  let src_parent =
    match t.conn with
    | None -> None
    | Some conn -> (
        match
          Store_db.exec conn
            ~params:[ Pgx.Value.of_string source_id ]
            "SELECT parent_id::text FROM branches WHERE id = $1"
        with
        | [ [ c ] ] -> Pgx.Value.to_string c
        | _ -> None)
  in
  let is_parent_merge = src_parent = Some target_id in
  let merged =
    try
      Store_db.with_transaction t (fun conn ->
        match conn with
        | None -> None  (* no DB; can't merge *)
        | Some conn ->
            (* branching.branch-status S3: status-driven merge.
               [is_parent_merge] was resolved before the tx (see above). *)
            let names, conflicts =
              if is_parent_merge then begin
                let rows = branch_status_conn conn ~branch_id:source_id in
                let fns =
                  List.filter (fun r -> r.st_kind = DiffFn) rows in
                (* DIVERGED is deliberately NOT in the applied set:
                   both sides moved, so auto-applying the source would
                   silently reverse the parent's work. Diverged names
                   come back as conflicts; re-define + re-merge resolves
                   them (branching.merge S3, 2026-09 decision). *)
                let ups =
                  List.filter_map
                    (fun r ->
                      match r.st_status with
                      | SAheadNew | SAhead -> Some r.st_name
                      | _ -> None)
                    fns
                in
                let conf =
                  List.filter_map
                    (fun r ->
                      if r.st_status = SDiverged then Some r.st_name
                      else None)
                    fns
                in
                (ups, conf)
              end
              else begin
                let sql =
                  "SELECT s.name FROM function_defs s " ^
                  "WHERE s.branch_id = $1 AND NOT EXISTS ( " ^
                  "  SELECT 1 FROM function_defs x " ^
                  "  WHERE x.branch_id = $2 AND x.name = s.name " ^
                  "  AND x.body IS NOT DISTINCT FROM s.body) " ^
                  "ORDER BY s.name" in
                let rows =
                  Store_db.exec conn
                    ~params:[ Pgx.Value.of_string source_id;
                              Pgx.Value.of_string target_id ]
                    sql
                in
                (List.filter_map
                   (function [ c ] -> Pgx.Value.to_string c | _ -> None)
                   rows,
                 [] (* no shared base: no conflict report *))
              end
            in
            (* Upsert only the selected names (per-name loop keeps binding
               simple; merge sets are branch-scale, tens of rows). *)
            let count =
              List.fold_left
                (fun acc name ->
                  let sql =
                    "INSERT INTO function_defs " ^
                    "  (branch_id, name, body, author, doc, package, updated_at) " ^
                    "SELECT $1, name, body, author, doc, package, updated_at " ^
                    "FROM function_defs " ^
                    "WHERE branch_id = $2 AND name = $3 " ^
                    "ON CONFLICT (branch_id, name) DO UPDATE " ^
                    "  SET body = EXCLUDED.body, author = EXCLUDED.author, " ^
                    "      doc = EXCLUDED.doc, package = EXCLUDED.package, " ^
                    "      updated_at = EXCLUDED.updated_at " ^
                    "RETURNING name" in
                  let rows =
                    Store_db.exec conn
                      ~params:[ Pgx.Value.of_string target_id;
                                Pgx.Value.of_string source_id;
                                Pgx.Value.of_string name ]
                      sql
                  in
                  if rows = [] then acc else acc + 1)
                0 names
            in
            (* tmp/eval-fast-path: merge writes scaffolding directly (the
               deferred mirror reaches tree_nodes) — journal the names so
               sessions on the target chain rebind only these. *)
            List.iter (fun name -> Def_journal.record_upsert ~branch_id:target_id ~name) names;
            (* Advance the source's merge base AFTER commit (see above):
               post-commit, branch_generations carries the merge's own
               bump, so base_gen lands on the parent's true head — git's
               moving merge-base. Only meaningful for parent-merges;
               promote deliberately does NOT advance. *)
            let () = Tree_store.recompute_head_hash conn ~branch_id:target_id in
            (Some { count; conflicts }))
    with exn -> Store_db.reraise ~op:"merge_branch" exn
  in
  let () =
    if is_parent_merge then
      match merged with
      | Some _ -> (
          match t.conn with
          | None -> ()
          | Some conn ->
              (try
                 let _ =
                   Store_db.exec conn
                     ~params:[ Pgx.Value.of_string source_id;
                               Pgx.Value.of_string target_id ]
                     ("UPDATE branches SET base_gen = COALESCE( " ^
                      "  (SELECT generation FROM branch_generations " ^
                      "    WHERE branch_id = $2), 0) WHERE id = $1")
                 in
                 ()
               with exn ->
                 (* PROBE (i-1788671060-2 keep): best-effort post-merge
                    bookkeeping — the merge already landed; a stale merge
                    base only makes the NEXT merge re-diff (conservative). *)
                 Printf.eprintf "[store] merge base advance: %s\n"
                   (Printexc.to_string exn)))
      | None -> ()
  in
  merged

(** Branch diff (branching.diff). Compares the EFFECTIVE views of two
    branches (closest-ancestor-wins walk per branch, deduped) across
    function_defs + type_defs + view_bindings, and classifies each
    divergent name as added / removed / modified.

    Read-only (NO writes) — matches the read-path style of
    [list_function_names_meta] (plain [Store_db.exec], no transaction). Scope
    mirrors [merge_branch]: never data_rows. *) 
let diff_branches (t : Store_db.t) ~(from_id : string) ~(to_id : string)
  : branch_diff_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
          (* chain_from / chain_to: parent walks for each branch.  Each
             side's *effective* rows are deduped (row_number depth-ASC,
             keep first = closest wins), then FULL OUTER JOINed on the
             key so added/removed/modified fall out naturally.  The body
             payload differs per table (fn=body, type=fields, route=
             handler) so the query is parameterized per kind. *)
          let read_kind (kind : diff_kind) : branch_diff_row list =
            let rel, key_sql, body_expr, is_route =
              match kind with
              | DiffFn ->
                  (Store_common.fn_rel (), "name", "body", false)
              | DiffType ->
                  (Store_common.ty_rel (), "name", "fields", false)
              | DiffRoute ->
                  (Store_common.rt_rel (), "route", "handler", true)
            in
            let sql =
              String.concat ""
                [ Store_common.chain_sql ~cte:"chain_from" (", chain_to AS ( "
                 ^ "  SELECT id, parent_id, 0 AS depth FROM branches WHERE id = $2 "
                 ^ "  UNION ALL "
                 ^ "  SELECT b.id, b.parent_id, c.depth + 1 "
                 ^ "  FROM branches b JOIN chain_to c ON b.id = c.parent_id "
                 ^ "), fromd AS ( ")
                ; "  SELECT " ^ key_sql ^ " AS key, " ^ body_expr ^ " AS body, "
                  ^ "updated_at::text AS up, "
                ; "         row_number() OVER (PARTITION BY " ^ key_sql
                  ^ " ORDER BY chain_from.depth ASC) AS rn "
                ; "  FROM " ^ rel ^ " fd JOIN chain_from ON fd.branch_id = chain_from.id "
                ; "), tod AS ( "
                ; "  SELECT " ^ key_sql ^ " AS key, " ^ body_expr ^ " AS body, "
                  ^ "updated_at::text AS up, "
                ; "         row_number() OVER (PARTITION BY " ^ key_sql
                  ^ " ORDER BY chain_to.depth ASC) AS rn "
                ; "  FROM " ^ rel ^ " fd JOIN chain_to ON fd.branch_id = chain_to.id "
                ; ") SELECT f.key, f.body, f.up::text, t.key, t.body, t.up::text "
                ; "FROM (SELECT * FROM fromd WHERE rn = 1) f "
                ; "FULL OUTER JOIN (SELECT * FROM tod WHERE rn = 1) t ON t.key = f.key "
                ; "WHERE f.key IS NULL OR t.key IS NULL "
                ; "      OR f.body IS DISTINCT FROM t.body "
                ; "ORDER BY COALESCE(f.key, t.key)" ]
            in
            let rows =
              Store_db.exec conn
                ~params:
                  [ Pgx.Value.of_string from_id
                  ; Pgx.Value.of_string to_id ]
                sql
            in
            List.filter_map
              (fun row ->
                match row with
                | [ fk_c; fb_c; fu_c; tk_c; tb_c; tu_c ] ->
                    let from_key = Pgx.Value.to_string fk_c in
                    let to_key = Pgx.Value.to_string tk_c in
                    let name =
                      match from_key, to_key with
                      | Some k, _ | _, Some k -> k
                      | None, None -> ""
                    in
                    let has_from = from_key <> None in
                    let has_to = to_key <> None in
                    if name = "" then None
                    else
                      let status =
                        if has_from && not has_to then DiffRemoved
                        else if has_to && not has_from then DiffAdded
                        else DiffModified
                      in
                      let from_body = if has_from then Pgx.Value.to_string fb_c else None in
                      let to_body = if has_to then Pgx.Value.to_string tb_c else None in
                      let from_updated = if has_from then Pgx.Value.to_string fu_c else None in
                      let to_updated = if has_to then Pgx.Value.to_string tu_c else None in
                      (* Opaque bodies: rows carry the RAW stored text;
                         the /diff renderer pretty-prints + parses at the
                         boundary (i-1788671083-c). *)
                      Some {
                        diff_name = name;
                        diff_kind = kind;
                        diff_status = status;
                        from_body = from_body;
                        to_body = to_body;
                        from_updated;
                        to_updated;
                      }
                | _ -> None)
              rows
          in
          List.concat
            [ read_kind DiffFn
            ; read_kind DiffType
            ; read_kind DiffRoute ]
    with exn -> Store_db.reraise ~op:"diff_branches" exn)

