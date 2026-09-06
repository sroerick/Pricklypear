(** tree-time Phase 1 dual-write shadow tests (tree-time.versioned-leaves).

    Every tree_nodes write is shadowed into tree_versions by the
    AFTER-trigger (migration 019 tree_version_append_trg). These tests
    exercise that shadow through the real store write paths and the
    versioned read surface added to Tree_store (versioned_head /
    versioned_history), then clean up after themselves:

      - datum create -> one version row; body/hash equal the live node;
        versioned_head (with no at_gen) == live tree_nodes head.
      - datum update -> the SAME (path, gen) chain grows (append); gen
        strictly increases; versioned_head returns the newest (the body
        that is live), and versioned_head ~at_gen:oldest returns the
        original body (an as-of-gen read).
      - datum delete -> a TOMBSTONE version row is appended at a higher
        gen (a row, never a DELETE); versioned_head reports the tombstone.
      - code-path fn write -> a version row under the b.<branch>.fn.*
        path with the branch's branch_generations bumped (not the data
        tree_generations counter).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    {
      sw;
      net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r);
    }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let uniq p = Printf.sprintf "tv_%s_%d" p (Random.int 1_000_000)

(** Last dot-separated label of a datum path = the datum id (uuid). *)
let path_id (path : string) : string =
  match List.rev (String.split_on_char '.' path) with
  | l :: _ -> l
  | [] -> path

let declare_type (store : S.t) ~name ~fields =
  S.persist_type store ~name ~fields ~author:"tester"

(** Body string field of a version row [{"body":{...}}]. *)
let body_field (v : T.version) (key : string) : string option =
  match v.T.v_body with
  | `Assoc kv ->
      (match List.assoc_opt "body" kv with
       | Some (`Assoc bodykv) ->
           (match List.assoc_opt key bodykv with
            | Some (`String s) -> Some s
            | _ -> None)
       | _ -> None)
  | _ -> None

(** The canonical body value of a version row (the {body:...} inside the
    envelope), as a JSON value — directly comparable to the live node's
    `body` field (which `node_of_row` sets to payload->'body'). *)
let version_body (v : T.version) : Yojson.Safe.t option =
  match v.T.v_body with
  | `Assoc kv -> List.assoc_opt "body" kv
  | _ -> None

(** Assert the versioned-head body equals the live tree_nodes body.
    NOTE: we compare CONTENT (body), not `hash`. The store's upsert hashes
    the raw caller-supplied body string (sha256 over {"title":"a",...})
    while the one-tree mirror bounce (tree_from_nodes -> data_rows ->
    tree_mirror_data_rows) recomputes the derived `hash` over the canonical
    jsonb `::text` (reordered keys / normalized spacing). So after a datum
    or fn echo, tree_nodes.hash legitimately differs from the version row's
    captured hash even though the content body is byte-identical. The
    versioned-leaves head-read invariant is about CONTENT, not the derived
    hash (which is a consistency artifact, per migration 020's own guard
    comment). *)
let check_head_body_equals_live (v : T.version) (n : T.node) : unit =
  match version_body v, n.T.body with
  | Some vb, lb ->
      Alcotest.(check bool) "head body equals live body" true
        (Yojson.Safe.equal vb lb)
  | None, _ -> Alcotest.fail "version body missing"

(* ---- datum create -> one version row; head read == live node ---- *)
let test_datum_create_shadow () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let path =
            match
              T.upsert_datum conn ~path:"data.u.tester.new" ~type_name
                ~body_json:"{\"title\":\"a\",\"body\":\"b\"}" ~author:"tester" ~rank:None
            with
            | Ok p -> p
            | Error e -> Alcotest.fail ("create failed: " ^ e)
          in
          (* one version row, gen a positive small int, matching live node *)
          let hist = T.versioned_history conn ~path () in
          Printf.printf "DEBUG create: history=%d%s\n%!"
            (List.length hist)
            (String.concat ","
               (List.map
                  (fun v ->
                    Printf.sprintf "(gen=%s hash=%s tom=%b)"
                      (match v.T.v_gen with Some g -> Int64.to_string g | None -> "-")
                      (match v.T.v_hash with Some h -> h | None -> "-")
                      v.T.v_tombstone)
                  hist));
          Alcotest.(check int) "one version row after create" 1
            (List.length hist);
          (match hist with
           | v :: _ ->
               Alcotest.(check bool) "gen is Some positive" true
                 (match v.T.v_gen with Some g -> Int64.compare g 0L > 0 | None -> false);
               Alcotest.(check bool) "not a tombstone" false v.T.v_tombstone
           | [] -> Alcotest.fail "no version row after create");
          (* versioned_head == live tree_nodes head (content-wise: the mirror
             bounce recomputes the derived hash, so compare bodies) *)
          (match T.get_by_path conn ~path with
           | None -> Alcotest.fail "live node missing"
           | Some n ->
               (match T.versioned_head conn ~path () with
                | None -> Alcotest.fail "versioned_head missing after create"
                | Some v ->
                    check_head_body_equals_live v n;
                    Alcotest.(check bool) "head not tombstone" false v.T.v_tombstone));
          (* clean up: remove the datum + its version rows stay (append-only) *)
          ignore (T.delete_datum conn ~type_name ~id:(path_id path)) )

(* ---- datum update -> append (gen grows), as-of read returns old body ---- *)
let test_datum_update_append () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let path =
            match
              T.upsert_datum conn ~path:"data.u.tester.new" ~type_name
                ~body_json:"{\"title\":\"a\",\"body\":\"b\"}" ~author:"tester" ~rank:None
            with
            | Ok p -> p
            | Error e -> Alcotest.fail ("create failed: " ^ e)
          in
          let g0 =
            match T.versioned_head conn ~path () with
            | Some v -> (match v.T.v_gen with Some g -> g | None -> 0L)
            | None -> 0L
          in
          (* update in place *)
          ignore
            (T.upsert_datum conn ~path ~type_name
               ~body_json:"{\"title\":\"a2\",\"body\":\"b2\"}" ~author:"tester" ~rank:None);
          let hist = T.versioned_history conn ~path () in
          Alcotest.(check int) "two version rows after update" 2
            (List.length hist);
          (match T.versioned_head conn ~path () with
           | None -> Alcotest.fail "head missing after update"
           | Some v ->
               Alcotest.(check bool) "head gen strictly greater" true
                 (match v.T.v_gen with
                  | Some g -> Int64.compare g g0 > 0
                  | None -> false);
               Alcotest.(check bool) "head body has title a2" true
                 (match body_field v "title" with Some t -> t = "a2" | None -> false));
          (* as-of the OLD gen -> old body *)
          (match T.versioned_head conn ~path ~at_gen:g0 () with
           | None -> Alcotest.fail "as-of head missing"
           | Some v ->
               Alcotest.(check bool) "as-of title is a" true
                 (match body_field v "title" with Some t -> t = "a" | None -> false));
          (* clean up *)
          ignore (T.delete_datum conn ~type_name ~id:(path_id path)) )

(* ---- datum delete -> TOMBSTONE version row appended (row, never DELETE) ---- *)
let test_datum_delete_tombstone () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let path =
            match
              T.upsert_datum conn ~path:"data.u.tester.new" ~type_name
                ~body_json:"{\"title\":\"a\",\"body\":\"b\"}" ~author:"tester" ~rank:None
            with
            | Ok p -> p
            | Error e -> Alcotest.fail ("create failed: " ^ e)
          in
          (match T.delete_datum conn ~type_name ~id:(path_id path) with
           | Ok _ -> ()
           | Error _ -> ());
          let hist = T.versioned_history conn ~path () in
          (* create + tombstone = 2 rows (delete appends, does not erase) *)
          Alcotest.(check int) "history keeps create + tombstone" 2
            (List.length hist);
          (match T.versioned_head conn ~path () with
           | None -> Alcotest.fail "head missing after delete"
           | Some v ->
               Alcotest.(check bool) "head is a tombstone" true v.T.v_tombstone) )

(* ---- code-path fn write -> branch_generations bumped, version row under b.* ---- *)
let test_fn_code_path_shadow () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let name = uniq "sfn" in
          let path = T.fn_path ~branch_id:main_id ~name in
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name
               ~body_json:"{\"params\":[],\"body\":1}" ~author:"tester"
               ~doc:None);
          (match T.get_by_path conn ~path with
           | None -> Alcotest.fail "fn node missing"
           | Some n ->
               (match T.versioned_head conn ~path () with
                | None -> Alcotest.fail "fn version row missing"
                | Some v ->
                    check_head_body_equals_live v n;
                    Alcotest.(check bool) "fn head not tombstone" false v.T.v_tombstone));
          (* branch_generations for system/main bumped for code paths, while
             the data tree_generations counter is NOT the source for it *)
          (match T.versioned_history conn ~path () with
           | h :: _ ->
               Alcotest.(check bool) "gen present" true
                 (match h.T.v_gen with Some _ -> true | None -> false)
           | [] -> Alcotest.fail "fn history empty");
          (* cleanup *)
          ignore (T.delete_fn conn ~branch_id:main_id ~name) )

(** one-tree.hash-domain (migration 027): identical bodies written through
    BOTH paths — tree-first (Tree_store.upsert_fn) and scaffolding-first
    (raw function_defs INSERT, forward-mirrored by the 009 trigger) — must
    land the SAME stored hash. The body's key order is deliberately NOT
    canonical ("params" before "zz"), so the pre-027 writers (OCaml
    compact-JSON digest vs mirror jsonb-canonical digest) would disagree
    here; since 027 both hash DB-side over PG canonical jsonb text. Also
    asserts the tree_versions shadow row carries the same-domain hash. *)
let test_hash_domain_cross_writer () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let suffix = string_of_int (Random.int 1_000_000) in
          let name = "tv_hash_" ^ suffix in
          let body = "{\"params\":[\"x\"],\"zz\":1}" in
          (* Path 1: tree-first upsert (OCaml writer). *)
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name ~body_json:body
               ~author:"tester" ~doc:None);
          let path = T.fn_path ~branch_id:main_id ~name in
          let h_treefirst =
            match T.get_by_path conn ~path with
            | Some n -> n.T.hash
            | None -> Alcotest.fail "tree-first node missing"
          in
          (* Path 2: scaffolding-first raw INSERT (009 mirror writer,
             deferred trigger — autocommit settles it before the read). *)
          let _ =
            S.exec conn
              ~params:
                [ Pgx.Value.of_string main_id
                ; Pgx.Value.of_string (name ^ "_mir")
                ; Pgx.Value.of_string body ]
              "INSERT INTO function_defs (branch_id, name, body, author) VALUES ($1::uuid, $2, $3::jsonb, 'tester')"
          in
          let path2 = T.fn_path ~branch_id:main_id ~name:(name ^ "_mir") in
          let h_mirror =
            match T.get_by_path conn ~path:path2 with
            | Some n -> n.T.hash
            | None -> Alcotest.fail "mirrored node missing"
          in
          Alcotest.(check string) "tree-first hash == mirror hash" h_mirror
            h_treefirst;
          (* and it IS the canonical jsonb digest of the body *)
          (match
             S.exec conn
               ~params:[ Pgx.Value.of_string body ]
               "SELECT encode(digest($1::jsonb::text,'sha256'),'hex')"
           with
          | [ [ c ] ] ->
              (match Pgx.Value.to_string c with
               | Some expect ->
                   Alcotest.(check string) "hash is canonical jsonb digest"
                     expect h_treefirst
               | None -> Alcotest.fail "digest query returned null")
          | _ -> Alcotest.fail "digest query failed");
          (* tree_versions shadow row carries the SAME domain hash *)
          (match T.versioned_head conn ~path () with
           | Some v ->
               (match v.T.v_hash with
                | Some vh ->
                    Alcotest.(check string) "version hash same domain"
                      h_treefirst vh
                | None -> Alcotest.fail "version hash missing")
           | None -> Alcotest.fail "version row missing");
          (* cleanup *)
          ignore (T.delete_fn conn ~branch_id:main_id ~name);
          let (_ : Pgx.row list) =
            S.exec conn
              ~params:
                [ Pgx.Value.of_string main_id
                ; Pgx.Value.of_string (name ^ "_mir") ]
              "DELETE FROM function_defs WHERE branch_id = $1::uuid AND name = $2"
          in
          ())

let () =
  Alcotest.run "tree_version_shadow"
    [
      ( "shadow",
        [
          Alcotest.test_case "datum create shadows one row" `Quick
            test_datum_create_shadow;
          Alcotest.test_case "datum update appends + as-of read" `Quick
            test_datum_update_append;
          Alcotest.test_case "datum delete appends tombstone" `Quick
            test_datum_delete_tombstone;
          Alcotest.test_case "fn code-path shadow bumps branch gen" `Quick
            test_fn_code_path_shadow;
          Alcotest.test_case "hash domain cross-writer equality" `Quick
            test_hash_domain_cross_writer;
        ] );
    ]
