(** local-first prim tests (borge/local-first.borg lens-model +
    replication-units, campaign 8.1).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.

    - lens/fork names + creates a lens branch as a NAMED CHILD of the
      checked-out branch (the pinned <owner>/lens-<device>[-<date>]
      contract); the fork is an EMPTY-LENS child (parent pointer only,
      copy-on-fork) so it owns zero code rows and resolves through
      branch ancestry. The server branch stays the parent; single-writer
      per lens is preserved by construction (no server writes into it).
    - replication/unit materializes the exact authorized-read subtree the
      identity would replicate: data.u.<user>.* + data.shared.* + the
      code lens + branch ancestry; path-prefix RLS confinement is the
      replication boundary.
*)

module S = Pricklypear_image_lib.Store

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

(* ---- lens-model: lens/fork names + creates an empty-lens child ---- *)
let test_lens_fork () =
  with_store (fun store ->
      let uniq = Printf.sprintf "probe%d" (Random.int 1_000_000) in
      let owner =
        match
          List.find_opt
            (fun (b : Pricklypear_common.Branch.t) -> b.id = main_id)
            (S.list_branches store)
        with
        | Some b -> b.owner
        | None -> Alcotest.fail "current branch (system/main) not in registry"
      in
      let v =
        Pricklypear_image_lib.Crud.lens_fork store ~device:uniq ~date:None ()
      in
      let s =
        match v with
        | Pricklypear_lisp.Ast.VStr x -> x
        | _ -> Alcotest.fail "lens/fork must return a VStr"
      in
      (* "ok <owner>/lens-<device> (<uuid>)" *)
      Alcotest.(check bool) "ack prefix" true
        (String.starts_with ~prefix:("ok " ^ owner ^ "/lens-" ^ uniq) s);
      (* the lens branch now exists and is an empty-lens child of main *)
      let created =
        List.find_opt
          (fun (b : Pricklypear_common.Branch.t) ->
            b.name = "lens-" ^ uniq)
          (S.list_branches store)
      in
      (match created with
       | None -> Alcotest.fail "lens branch not created"
       | Some b ->
           Alcotest.(check (option string)) "parent is current branch (main)"
             (Some main_id) b.parent_id;
           Alcotest.(check string) "owner preserved from current branch"
             owner b.owner;
           (* empty-lens: zero own function rows on the lens *)
           let n = S.count_functions_on_branch store ~branch_id:b.id in
           Alcotest.(check int) "empty-lens fork owns no code rows" 0 n;
           (* cleanup: drop the lens branch row (it owns no rows) *)
           (match S.conn_of store with
            | None -> ()
            | Some conn ->
                ignore
                  (S.exec conn
                     ~params:[ Pgx.Value.of_string b.id ]
                     "DELETE FROM branches WHERE id = $1::uuid")));
      (* confirm cleanup *)
      let still = List.exists
          (fun (b : Pricklypear_common.Branch.t) -> b.name = "lens-" ^ uniq)
          (S.list_branches store)
      in
      Alcotest.(check bool) "lens cleaned up" false still)

(* ---- replication-units: the manifest materializes the owned unit ---- *)
let test_replication_unit () =
  with_store (fun store ->
      match Pricklypear_image_lib.Crud.replication_unit store () with
      | Pricklypear_lisp.Ast.VStr json ->
          (match Yojson.Safe.from_string json with
           | `Assoc kv ->
               let field key =
                 match List.assoc_opt key kv with
                 | Some (`String s) -> s
                 | _ -> "<missing>"
               in
               let owner, branch_id =
                 field "owner", field "branch_id"
               in
               Alcotest.(check bool) "owner present" (owner <> "")
                 (owner <> "<missing>");
               Alcotest.(check string) "branch_id is current branch"
                 main_id branch_id;
               (* ancestry is self-first and includes the branch *)
               (match List.assoc_opt "ancestry" kv with
                | Some (`List ids) ->
                    Alcotest.(check bool) "ancestry is a non-empty list"
                      true (ids <> [])
                | _ -> Alcotest.fail "ancestry missing");
               (match List.assoc_opt "data" kv with
                | Some (`Assoc dk) ->
                    let df key =
                      match List.assoc_opt key dk with
                      | Some (`String s) -> s
                      | _ -> "<missing>"
                    in
                    Alcotest.(check bool) "user data prefix present"
                      true
                      (String.starts_with ~prefix:"data.u." (df "user"));
                    Alcotest.(check string) "shared seed prefix"
                      "data.shared" (df "shared");
                    Alcotest.(check bool) "shared_count is an int"
                      true
                      (match List.assoc_opt "shared_count" dk with
                       | Some (`Int _) -> true | _ -> false)
                | _ -> Alcotest.fail "data absent");
               Alcotest.(check bool) "scope names the boundary"
                 true
                 (String.starts_with ~prefix:"data.u.<user>.*"
                    (field "scope"))
           | _ -> Alcotest.fail "replication/unit not an object")
      | _ -> Alcotest.fail "replication/unit must return a VStr")

let () =
  Random.self_init ();
  Alcotest.run "local_first_prims"
    [ ( "lens-model+replication",
        [
          Alcotest.test_case "lens/fork creates empty-lens named child"
            `Quick test_lens_fork;
          Alcotest.test_case "replication/unit materializes the owned unit"
            `Quick test_replication_unit;
        ] ) ]
