(** image/test/sync_db_test.ml — borge/local-first.borg sync (campaign 8.3),
    DB-BACKED half of the sync merge authority (image/lib/sync.ml).

    The PURE core (classify / plan_of / actionable / JSON) is covered by
    sync_test.ml (pure, no PG). This file exercises the DB-backed harness
    the server-side commit path uses:

      - server_manifest reads the server's subtree (path, hash) manifest
        straight from tree_nodes (the exchange's server-side offer).
      - commit applies a client push (theirs_add) node through the
        typed-boundary-validated datum write path, and SURFACES conflicts
        report-only (the-glochids v0: LWW keeps server, conflict reported,
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
      (match Sync.server_manifest store ~prefix with
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
      (* A conflict is REPORTED not silently resolved (the obituary rule:
         no silent sync loss). LWW v0 keeps ours (server); the conflict is
         surfaced in the report with the three hashes. *)
      let plans =
        Sync.plan_of [ ("a", h 1) ] [ ("a", h 8) ] [ ("a", h 9) ]
      in
      let r = Sync.commit store ~client_bodies:[] plans in
      Alcotest.(check int) "nothing applied on conflict" 0 r.applied;
      Alcotest.(check int) "one conflict surfaced" 1 (List.length r.conflicts);
      (match r.conflicts with
       | [ (path, o, t) ] ->
           Alcotest.(check string) "conflict path" "a" path;
           Alcotest.(check string) "ours hash kept" (h 8) o;
           Alcotest.(check string) "theirs hash reported" (h 9) t
       | _ -> Alcotest.fail "unexpected conflicts"))

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
        ] ) ]
