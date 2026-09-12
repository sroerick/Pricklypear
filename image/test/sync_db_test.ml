(** image/test/sync_db_test.ml — borge/local-first.borg sync (campaign 8.3),
    DB-BACKED half of the sync merge authority (image/lib/sync.ml).

    The PURE core (classify / plan_of / actionable / JSON) is covered by
    sync_test.ml (pure, no PG). This file exercises the DB-backed harness
    the server-side commit path uses:

      - server_manifest reads the server's subtree (path, hash) manifest
        straight from tree_nodes (the exchange's server-side offer).
      - commit applies a client push (theirs_add) node through the
        typed-boundary-validated datum write path, and SURFACES conflicts
        report-only (sync-conflicts v0: LWW keeps server, conflict reported,
        no silent loss).

    Needs scripts/dev.sh Postgres; skips cleanly when unreachable.
*)

module S = Pricklypear_image_lib.Store
module Sync = Pricklypear_image_lib.Sync

let main_id = "00000000-0000-0000-0000-000000000001"

let anon_enc = Pricklypear_image_lib.Tree_store.enc_label "anonymous"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let h n = Printf.sprintf "h%d" n

let test_server_manifest () =
  with_store (fun store ->
      let uniq = Printf.sprintf "syn%06d" (Random.int 1_000_000) in
      let path = "data.u." ^ anon_enc ^ ".sync_" ^ uniq ^ ".probe" in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           ignore
             (Pricklypear_image_lib.Tree_store.insert_datum conn
                ~type_name:("sync_" ^ uniq)
                ~fields_json:"{\"n\":1}" ~author:"anonymous"));
      let prefix = "data.u." ^ anon_enc in
      (match Sync.server_manifest store ~prefixes:[ prefix ] with
       | None -> Alcotest.fail "server_manifest None"
       | Some m ->
           Alcotest.(check bool) "probe row in manifest" true
             (List.exists (fun (p, hs) -> p = path && hs <> "") m);
           Alcotest.(check bool) "hash is 64-hex" true
             (List.exists (fun (p, hs) -> p = path && String.length hs = 64) m);
           (match S.conn_of store with
            | Some conn ->
                ignore
                  (S.exec conn
                     ~params:[ Pgx.Value.of_string path ]
                     "DELETE FROM tree_nodes WHERE path = $1::ltree")
            | None -> ())))

let test_commit_push () =
  with_store (fun store ->
      let uniq = Printf.sprintf "syn2%05d" (Random.int 1_000_000) in
      let ty = "syncpush_" ^ uniq in
      let path = "data.u." ^ anon_enc ^ "." ^ ty ^ ".one" in
      (* declare a type with one required field n *)
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn
                ~params:[ Pgx.Value.of_string ty ]
                "INSERT INTO type_defs (branch_id, name, fields) \
                 VALUES ($1::uuid, $2, '[{\"name\":\"n\"}]'::jsonb) \
                 ON CONFLICT (branch_id, name) DO NOTHING")
       | None -> ());
      let env : Yojson.Safe.t =
        `Assoc
          [ ("v", `Int 1)
          ; ("kind", `String "datum")
          ; ("path", `String path)
          ; ("type", `String ty)
          ; ("body", `Assoc [ ("n", `Int 42) ])
          ; ("author", `String "anonymous")
          ]
      in
      (* three-way: base empty, server empty, client added the node *)
      let plans = Sync.plan_of [] [] [ (path, "H") ] in
      let actionable =
        Sync.actionable plans
        |> List.filter (fun { Sync.path = p; _ } -> p = path)
      in
      Alcotest.(check int) "theirs_add actionable" 1 (List.length actionable);
      let r = Sync.commit store ~client_bodies:[ (path, env) ] plans in
      Alcotest.(check int) "applied 1" 1 r.applied;
      Alcotest.(check int) "no conflicts" 0 (List.length r.conflicts);
      (match S.conn_of store with
       | Some conn ->
           Alcotest.(check bool) "node persisted" true
             (Pricklypear_image_lib.Tree_store.get_by_path conn ~path <> None);
           ignore
             (S.exec conn ~params:[ Pgx.Value.of_string path ]
                "DELETE FROM tree_nodes WHERE path = $1::ltree");
           ignore
             (S.exec conn ~params:[ Pgx.Value.of_string ty ]
                "DELETE FROM type_defs WHERE name = $1")
       | None -> ()))

let test_commit_surfaces_conflict () =
  with_store (fun store ->
      (* A conflict is LWW-arbitrated per sync-conflicts v0 AND surfaced
         either way (the obituary rule: no silent sync loss). With no
         client envelope (no updated_at stamp), the server copy stays and
         the conflict is reported with the three hashes + the kept list. *)
      let plans =
        Sync.plan_of [ ("a", h 1) ] [ ("a", h 8) ] [ ("a", h 9) ]
      in
      let r = Sync.commit store ~client_bodies:[] plans in
      Alcotest.(check int) "nothing applied on conflict" 0 r.applied;
      Alcotest.(check int) "one conflict surfaced" 1 (List.length r.conflicts);
      Alcotest.(check int) "server kept (no client stamp)" 1
        (List.length r.lww_server_kept);
      Alcotest.(check int) "client won nothing" 0
        (List.length r.lww_client_won);
      (match r.conflicts with
       | [ (path, o, t) ] ->
           Alcotest.(check string) "conflict path" "a" path;
           Alcotest.(check string) "ours hash kept" (h 8) o;
           Alcotest.(check string) "theirs hash reported" (h 9) t
       | _ -> Alcotest.fail "unexpected conflicts"))

let test_commit_lww_client_newer_wins () =
  with_store (fun store ->
      (* LWW v0: the client envelope carries a STRICTLY newer updated_at
         (epoch) than the server node → the client copy is applied and
         the win surfaced. The server node must exist for the clock to
         arbitrate, so insert it first via the typed write surface. *)
      let uniq = Printf.sprintf "lww%05d" (Random.int 1_000_000) in
      let ty = "synclww_" ^ uniq in
      let path = "data.u." ^ anon_enc ^ "." ^ ty ^ ".one" in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn
                ~params:[ Pgx.Value.of_string ty ]
                "INSERT INTO type_defs (branch_id, name, fields) \
                 VALUES ($1::uuid, $2, '[{\"name\":\"n\"}]'::jsonb) \
                 ON CONFLICT (branch_id, name) DO NOTHING")
       | None -> ());
      let server_env : Yojson.Safe.t =
        `Assoc
          [ ("v", `Int 1); ("kind", `String "datum"); ("path", `String path);
            ("type", `String ty); ("body", `Assoc [ ("n", `Int 1) ]);
            ("author", `String "anonymous") ]
      in
      (match Sync.apply_client_node store ~env:server_env with
       | Ok () -> () | Error e -> Alcotest.fail ("seed node: " ^ e));
      (* the node exists with a fresh server updated_at; a client edit
         stamped in the FUTURE strictly wins *)
      let future = Unix.gettimeofday () +. 3600.0 in
      let client_env : Yojson.Safe.t =
        `Assoc
          [ ("v", `Int 1); ("kind", `String "datum"); ("path", `String path);
            ("type", `String ty); ("body", `Assoc [ ("n", `Int 2) ]);
            ("author", `String "anonymous");
            ("updated_at", `String (string_of_float future)) ]
      in
      let plans =
        Sync.plan_of [ (path, h 1) ] [ (path, h 8) ] [ (path, h 9) ]
      in
      let r =
        Sync.commit store ~client_bodies:[ (path, client_env) ] plans
      in
      Alcotest.(check int) "LWW applied client copy" 1 r.applied;
      Alcotest.(check int) "one conflict surfaced" 1 (List.length r.conflicts);
      Alcotest.(check int) "client won" 1 (List.length r.lww_client_won);
      Alcotest.(check int) "server kept nothing" 0
        (List.length r.lww_server_kept);
      (match S.conn_of store with
       | Some conn ->
           (* the applied body is the client's (n=2): assert via the server
              manifest hash change over the original body hash *)
           let m =
             Option.value
                (Sync.server_manifest store ~prefixes:[ "data.u." ^ anon_enc ])
               ~default:[]
           in
           Alcotest.(check bool) "node still present post-LWW" true
             (List.exists (fun (p, _) -> p = path) m);
           ignore (S.exec conn ~params:[ Pgx.Value.of_string path ]
                     "DELETE FROM tree_nodes WHERE path = $1::ltree");
           ignore (S.exec conn ~params:[ Pgx.Value.of_string ty ]
                     "DELETE FROM type_defs WHERE name = $1")
       | None -> ()))

let test_commit_theirs_del_and_ours_del () =
  with_store (fun store ->
      (* Client DELETION propagates: base+server hold the node, client
         manifest lacks it → Theirs_del → the server node is deleted.
         And server deletions come back as client_deletes (Ours_del): the
         client must mirror-delete its stale local copy. *)
      let uniq = Printf.sprintf "del%05d" (Random.int 1_000_000) in
      let ty = "syncdel_" ^ uniq in
      let path = "data.u." ^ anon_enc ^ "." ^ ty ^ ".one" in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn
                ~params:[ Pgx.Value.of_string ty ]
                "INSERT INTO type_defs (branch_id, name, fields) \
                 VALUES ($1::uuid, $2, '[{\"name\":\"n\"}]'::jsonb) \
                 ON CONFLICT (branch_id, name) DO NOTHING")
       | None -> ());
      let env : Yojson.Safe.t =
        `Assoc
          [ ("v", `Int 1); ("kind", `String "datum"); ("path", `String path);
            ("type", `String ty); ("body", `Assoc [ ("n", `Int 3) ]);
            ("author", `String "anonymous") ]
      in
      (match Sync.apply_client_node store ~env with
       | Ok () -> () | Error e -> Alcotest.fail ("seed node: " ^ e));
      (* theirs_del: base has it, ours (server) has it at base hash,
         theirs (client) deleted it *)
      let m =
        Option.value
            (Sync.server_manifest store ~prefixes:[ "data.u." ^ anon_enc ])
          ~default:[]
      in
      let srv_hash = List.assoc path m in
      let plans =
        Sync.plan_of [ (path, srv_hash) ] [ (path, srv_hash) ] []
      in
      let r = Sync.commit store ~client_bodies:[] plans in
      Alcotest.(check int) "deletion applied" 1 r.applied;
      (match S.conn_of store with
       | Some conn ->
           Alcotest.(check bool) "node deleted server-side" false
             (Pricklypear_image_lib.Tree_store.get_by_path conn ~path <> None)
       | None -> ());
      (* ours_del: base+client hold it, server (ours) deleted → the plan
         reports Ours_del and the commit surfaces it as client_deletes *)
      let plans2 =
        Sync.plan_of [ (path, srv_hash) ] [] [ (path, srv_hash) ]
      in
      let r2 = Sync.commit store ~client_bodies:[] plans2 in
      Alcotest.(check (list string)) "client mirror-deletes" [ path ]
        r2.client_deletes;
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn ~params:[ Pgx.Value.of_string ty ]
                "DELETE FROM type_defs WHERE name = $1")
       | None -> ()))

let test_two_way_push_applies_client_only () =
  (* The sync/push prim path: a TWO-WAY classify (no base) where the
     server lacks a node the client holds (ClientOnly) is applied through
     Sync.apply_client_node. This is the v1 local-first push-up mechanism
     (borge/local-first.borg sync; diverge is reported, not auto-merged). *)
  with_store (fun store ->
      let uniq = Printf.sprintf "sp3%05d" (Random.int 1_000_000) in
      let ty = "syncpush2_" ^ uniq in
      let path = "data.u." ^ anon_enc ^ "." ^ ty ^ ".one" in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn
                ~params:[ Pgx.Value.of_string ty ]
                "INSERT INTO type_defs (branch_id, name, fields) \
                 VALUES ($1::uuid, $2, '[{\"name\":\"n\"}]'::jsonb) \
                 ON CONFLICT (branch_id, name) DO NOTHING")
       | None -> ());
      let env : Yojson.Safe.t =
        `Assoc
          [ ("v", `Int 1)
          ; ("kind", `String "datum")
          ; ("path", `String path)
          ; ("type", `String ty)
          ; ("body", `Assoc [ ("n", `Int 7) ])
          ; ("author", `String "anonymous")
          ]
      in
      (* server manifest is EMPTY for this subtree; client offers the
         node → classify reports it ClientOnly *)
      let server =
        Sync.server_manifest store ~prefixes:[ "data.u." ^ anon_enc ] in
      let server = Option.value server ~default:[] in
      let r = Sync.classify server [ (path, "H") ] in
      Alcotest.(check int) "one client_only" 1 (List.length r.client_only);
      Alcotest.(check int) "no diverge" 0 (List.length r.diverge);
      (* apply the ClientOnly push through the same surface the prim uses *)
      let applied =
        List.fold_left
          (fun n (p, _) ->
            if List.mem_assoc p r.client_only then
              (match Sync.apply_client_node store ~env with
               | Ok () -> n + 1
               | Error _ -> n)
            else n)
          0 [ (path, "H") ]
      in
      Alcotest.(check int) "applied 1" 1 applied;
      (match S.conn_of store with
       | Some conn ->
           Alcotest.(check bool) "pushed node persisted" true
             (Pricklypear_image_lib.Tree_store.get_by_path conn ~path <> None);
           ignore
             (S.exec conn ~params:[ Pgx.Value.of_string path ]
                "DELETE FROM tree_nodes WHERE path = $1::ltree");
           ignore
             (S.exec conn ~params:[ Pgx.Value.of_string ty ]
                "DELETE FROM type_defs WHERE name = $1")
       | None -> ()))

let test_space_guard_rejects_client_writes () =
  with_store (fun store ->
      (* Space subtrees are NEVER in a lens's write scope (borge/
         local-first.borg sync): any client-driven write outcome on a
         data.s.* path must be REJECTED and surfaced via failed — never
         applied. A server-side deletion (Ours_del) is the authority
         speaking and still mirrors to the client. *)
      let path = "data.s.fencefans.fencetest.probe" in
      let env : Yojson.Safe.t =
        `Assoc
          [ ("v", `Int 1); ("kind", `String "datum"); ("path", `String path);
            ("type", `String "fencetest"); ("body", `Assoc [ ("n", `Int 5) ]);
            ("author", `String "anonymous") ]
      in
      (* theirs_add on a space path (client offers a node the server lacks) *)
      let plans = Sync.plan_of [] [ (path, "H0") ] [ (path, "H") ] in
      let r = Sync.commit store ~client_bodies:[ (path, env) ] plans in
      Alcotest.(check int) "theirs_add on a space path rejected" 0 r.applied;
      Alcotest.(check (list string)) "surfaced as failed" [ path ] r.failed;
      (match S.conn_of store with
       | Some conn ->
           Alcotest.(check bool) "nothing persisted" false
             (Pricklypear_image_lib.Tree_store.get_by_path conn ~path <> None)
       | None -> ());
      (* theirs_del on a space path is also rejected *)
      let plans2 = Sync.plan_of [ (path, "H0") ] [ (path, "H0") ] [] in
      let r2 = Sync.commit store ~client_bodies:[] plans2 in
      Alcotest.(check int) "theirs_del on a space path rejected" 0 r2.applied;
      Alcotest.(check (list string)) "surfaced as failed" [ path ] r2.failed;
      (* a server-side deletion (Ours_del) still mirrors to the client *)
      let plans3 = Sync.plan_of [ (path, "H0") ] [] [ (path, "H0") ] in
      let r3 = Sync.commit store ~client_bodies:[] plans3 in
      Alcotest.(check (list string)) "ours_del mirrors" [ path ]
        r3.client_deletes)

let test_member_space_prefixes_manifest () =
  with_store (fun store ->
      (* member_space_prefixes returns the encoded data.s.<space> prefix
         for every space the identity belongs to (migration 029), and
         server_manifest over the multi-prefix unit reads the space
         subtree — the replication unit's PULL side. *)
      let space = "fencefans" in
      let ty = "fencetest" in
      let path = "data.s." ^ space ^ "." ^ ty ^ ".probeL" in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn
                ~params:[ Pgx.Value.of_string space ]
                "INSERT INTO data_space_members (space, username) \
                 SELECT $1, 'anonymous' \
                 WHERE NOT EXISTS (SELECT 1 FROM data_space_members \
                                   WHERE space = $1 AND username = 'anonymous')");
           ignore
             (S.exec conn
                ~params:
                  [ Pgx.Value.of_string path; Pgx.Value.of_string ty ]
                "INSERT INTO tree_nodes (path, kind, type_name, payload, hash, author) \
                 VALUES ($1::ltree, 'datum', $2, \
                   jsonb_build_object('v',1,'kind','datum','path',$1::text,\
                     'type',$2,'body','{\"n\":9}'::jsonb,'author','anonymous',\
                     'hash','H'), 'H', 'anonymous')")
       | None -> ());
      let prefixes = Sync.member_space_prefixes store ~user:"anonymous" in
      let expected =
        "data.s." ^ Pricklypear_image_lib.Tree_store.enc_label space
      in
      Alcotest.(check bool) "member space prefix present" true
        (List.mem expected prefixes);
      let m =
        Option.value
          (Sync.server_manifest store ~prefixes:[ expected ])
          ~default:[]
      in
      Alcotest.(check bool) "space row in space manifest" true
        (List.exists (fun (p, _) -> p = path) m);
      (match S.conn_of store with
       | Some conn ->
           ignore
             (S.exec conn ~params:[ Pgx.Value.of_string path ]
                "DELETE FROM tree_nodes WHERE path = $1::ltree");
           ignore
             (S.exec conn
                ~params:
                  [ Pgx.Value.of_string space
                  ; Pgx.Value.of_string "anonymous" ]
                "DELETE FROM data_space_members \
                 WHERE space = $1 AND username = $2")
       | None -> ()))

let () =
  Random.self_init ();
  Alcotest.run "sync_db"
    [ ( "sync-db",
        [
          Alcotest.test_case "server_manifest reads subtree hashes" `Quick
            test_server_manifest;
          Alcotest.test_case "commit applies a client push" `Quick
            test_commit_push;
          Alcotest.test_case "commit surfaces conflicts (no silent loss)"
            `Quick test_commit_surfaces_conflict;
          Alcotest.test_case "commit LWW: strictly-newer client wins"
            `Quick test_commit_lww_client_newer_wins;
          Alcotest.test_case "commit theirs_del + ours_del"
            `Quick test_commit_theirs_del_and_ours_del;
          Alcotest.test_case "two-way push applies client-only node"
            `Quick test_two_way_push_applies_client_only;
          Alcotest.test_case "space guard rejects client writes on data.s paths"
            `Quick test_space_guard_rejects_client_writes;
          Alcotest.test_case "member spaces join the replication unit"
            `Quick test_member_space_prefixes_manifest;
          ] ) ]
