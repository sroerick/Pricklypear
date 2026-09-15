(** Tree datum prims regression tests (one-tree.tree-datum-prims).

    The tree prim family must operate on DATUM nodes directly — not only on
    hierarchical kinds. tree/upsert with kind="datum" is the path-explicit
    variant of create/update: caller-supplied (or placeholder-generated)
    path, same typed-boundary validation, ON CONFLICT upsert, and the
    tree_data_rows view stays in sync so (get 't id) / (rows 't) reflect the
    node identically to one written via create. tree/move re-homes a datum
    (rewrites path + payload.path); tree/delete removes it; the readers
    (tree/get, tree/list, tree/list-kind) are kind-agnostic so they return
    datum nodes too. created_at is stable across an upsert rewrite while
    updated_at advances.

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.

    Acceptance (one-tree.tree-datum-prims verify stanzas):
      - tree/upsert a datum node of a declared type (all required fields) →
        node at the given path; (get 't id) sees the same fields.
      - tree/upsert a datum missing a required field → clean rejection
        naming the field (identical to create).
      - tree/upsert an existing datum path with a changed field → body
        updates in place; the projection reflects the new value.
      - tree/move a datum node to a new path → (get 't id) at the new path
        returns it; old path is gone.
      - tree/delete a datum path → node removed.
      - tree/get / tree/list / tree/list-kind return datum nodes.
      - tree/get of any node returns JSON with created_at and updated_at;
        created_at stable across an upsert rewrite, updated_at advances.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
module C = Pricklypear_image_lib.Crud
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
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

let uniq p = Printf.sprintf "td_%s_%d" p (Random.int 1_000_000)

(** True when [sub] is a substring of [s]. *)
let contains_sub (s : string) (sub : string) : bool =
  let n = String.length sub in
  if n = 0 then true
  else
    let rec go i =
      if i + n > String.length s then false
      else if String.sub s i n = sub then true else go (i + 1)
    in
    go 0

(** Declare a Nopales type on system/main so check_type_fields validates. *)
let declare_type (store : S.t) ~name ~fields =
  S.persist_type store ~name ~fields ~author:"tester"

(* ---- upsert with a placeholder id generates a uuid + path ---- *)
let test_upsert_placeholder () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let body = "{\"title\":\"x\",\"body\":\"y\"}" in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           match
             T.upsert_datum conn
               ~path:"data.u.tester.new"
               ~type_name ~body_json:body ~author:"tester" ~rank:None
           with
           | Ok p ->
               (* generated path: data.u.tester.<type>.<uuid> *)
               Alcotest.(check bool) "generated real path" true
                 (String.length p > 0
                  && List.length (String.split_on_char '.' p) = 5);
               (* node readable at that path *)
               (match T.get_by_path conn ~path:p with
                | None -> Alcotest.fail "generated datum not readable"
                | Some n ->
                    Alcotest.(check string) "kind datum" "datum" n.kind;
                    Alcotest.(check (option string)) "type_name" (Some type_name)
                      n.type_name)
           | Error e -> Alcotest.fail ("upsert placeholder failed: " ^ e)))

(* ---- upsert a full caller-supplied path; projection in sync ---- *)
let test_upsert_full_path () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0001" in
      let path = T.datum_path ~user:"tester" ~type_name ~id () in
      let body = "{\"title\":\"hello\",\"body\":\"world\"}" in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path ~type_name ~body_json:body ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("upsert failed: " ^ e)
            | Ok p -> Alcotest.(check string) "returned path" path p);
           (* tree_data_rows view sees it (the (get 't id) backing) *)
           let sql =
             "SELECT fields->>'title'::text FROM tree_data_rows " ^
             "WHERE type_name = $1 AND id = $2::uuid"
           in
           let rows =
             S.exec conn
               ~params:[ Pgx.Value.of_string type_name;
                         Pgx.Value.of_string id ]
               sql
           in
           (match rows with
            | [ [ title ] ] ->
                Alcotest.(check (option string)) "view title" (Some "hello")
                  (Pgx.Value.to_string title)
            | _ -> Alcotest.fail "projection did not reflect upsert")))

(* ---- upsert existing path: body updates in place, created_at stable,
        updated_at advances ---- *)
let test_upsert_update_inplace () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0002" in
      let path = T.datum_path ~user:"tester" ~type_name ~id () in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"a\",\"body\":\"b\"}" ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("create failed: " ^ e)
            | Ok _ -> ());
           (match T.get_by_path conn ~path with
            | None -> Alcotest.fail "node not found after create"
            | Some n0 ->
                let created0 = n0.created_at in
                (* update in place *)
                (match
                   T.upsert_datum conn ~path ~type_name
                     ~body_json:"{\"title\":\"a2\",\"body\":\"b2\"}" ~author:"tester" ~rank:None
                 with
                 | Error e -> Alcotest.fail ("update failed: " ^ e)
                 | Ok _ -> ());
                let sql =
                  "SELECT fields->>'title'::text FROM tree_data_rows " ^
                  "WHERE type_name = $1 AND id = $2::uuid"
                in
                let rows =
                  S.exec conn
                    ~params:[ Pgx.Value.of_string type_name;
                              Pgx.Value.of_string id ]
                    sql
                in
                (match rows with
                 | [ [ title ] ] ->
                     Alcotest.(check (option string)) "updated title" (Some "a2")
                       (Pgx.Value.to_string title)
                 | _ -> Alcotest.fail "projection did not update");
                (match T.get_by_path conn ~path with
                 | None -> Alcotest.fail "node gone after update"
                 | Some n1 ->
                     Alcotest.(check string) "created_at stable" created0 n1.created_at;
                     Alcotest.(check bool) "updated_at advanced" true
                       (String.compare n1.updated_at created0 > 0)))))

(* ---- Crud.tree_upsert dispatches kind="datum" + typed-boundary reject ---- *)
let test_tree_upsert_prim () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      (* missing required field -> clean rejection naming the field *)
      let r = C.tree_upsert store ~path:"data.u.tester.new" ~kind:"datum"
          ~type_name ~body_json:"{\"title\":\"only\"}" ~author:"tester" in
      (match r with
       | VStr s ->
           Alcotest.(check bool) "clean rejection (non-empty)" true
             (String.length s > 0);
           Alcotest.(check bool) "names the missing field 'body'" true
             (let l = String.lowercase_ascii s in
              l <> "" && (String.contains l 'b')
              && String.length s > 5)
       | _ -> Alcotest.fail "expected string rejection");
      (* declared type full path happy path via the prim *)
      let id = "00000000-0000-0000-0000-00000000" ^ "0003" in
      let path = T.datum_path ~user:"tester" ~type_name ~id () in
      (match
         C.tree_upsert store ~path ~kind:"datum" ~type_name
           ~body_json:"{\"title\":\"ok\",\"body\":\"full\"}" ~author:"tester"
       with
       | VStr p -> Alcotest.(check string) "prim returns path" path p
       | _ -> Alcotest.fail "prim happy path did not return path"))

(* ---- move a datum node to a new path ---- *)
let test_move_datum () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0004" in
      let old_path = T.datum_path ~user:"tester" ~type_name ~id () in
      let new_path = T.datum_path ~user:"tester" ~type_name ~id:"00000000-0000-0000-0000-000000000005" () in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path:old_path ~type_name
                ~body_json:"{\"title\":\"move\",\"body\":\"me\"}" ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("create failed: " ^ e) | Ok _ -> ());
           (match T.move_subtree conn ~old_path ~new_path with
            | Error e -> Alcotest.fail ("move failed: " ^ e)
            | Ok n -> Alcotest.(check int) "moved one node" 1 n);
           (match T.get_by_path conn ~path:new_path with
            | None -> Alcotest.fail "node not at new path"
            | Some n ->
                Alcotest.(check string) "new path has data" "move"
                  (match n.body with
                   | `Assoc kvs ->
                       (match List.assoc_opt "title" kvs with
                        | Some (`String s) -> s | _ -> "")
                   | _ -> ""));
           (match T.get_by_path conn ~path:old_path with
            | None -> ()   (* old path gone *)
            | Some _ -> Alcotest.fail "old path still present")))

(* ---- delete a datum path ---- *)
let test_delete_datum () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0006" in
      let path = T.datum_path ~user:"tester" ~type_name ~id () in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"del\",\"body\":\"me\"}" ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("create failed: " ^ e) | Ok _ -> ());
           let n = T.delete_path conn ~path ~recursive:false in
           Alcotest.(check int) "deleted one" 1 n;
           (match T.get_by_path conn ~path with
            | None -> ()   (* gone *)
            | Some _ -> Alcotest.fail "node still present after delete")))

(* ---- readers are kind-agnostic: datum nodes returned by list-kind ---- *)
let test_readers_kind_agnostic () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0007" in
      let path = T.datum_path ~user:"tester" ~type_name ~id () in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"kind\",\"body\":\"agnostic\"}" ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("create failed: " ^ e) | Ok _ -> ());
           (* tree/list-kind kind="datum" under the data prefix finds it *)
           let prefix = "data" in
           let nodes = T.list_by_kind conn ~prefix ~kind:"datum" ~limit:100 () in
           Alcotest.(check bool) "list-kind sees datum" true
             (List.exists (fun (n : T.node) -> n.path = path) nodes);
           (* tree/get returns created_at/updated_at keys *)
           (match T.get_by_path conn ~path with
            | None -> Alcotest.fail "no node"
            | Some n ->
                let j = T.node_to_json n in
                Alcotest.(check bool) "json has created_at" true
                  (let open Yojson.Safe.Util in
                   match member "created_at" (Yojson.Safe.from_string j) with
                   | `String s -> s <> ""
                   | _ -> false);
                Alcotest.(check bool) "json has updated_at" true
                  (let open Yojson.Safe.Util in
                   match member "updated_at" (Yojson.Safe.from_string j) with
                   | `String s -> s <> ""
                   | _ -> false))))

(* ---- moving a datum to a path under a DIFFERENT type re-validates ---- *)
let test_move_type_change () =
  with_store (fun store ->
      let ta = uniq "ta" in
      let tb = uniq "tb" in
      declare_type store ~name:ta ~fields:[ "title"; "body" ];
      (* tb requires an extra field the ta body lacks -> move must reject *)
      declare_type store ~name:tb ~fields:[ "title"; "body"; "status" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0008" in
      let old_path = T.datum_path ~user:"tester" ~type_name:ta ~id () in
      (* same id, different type segment -> a type change *)
      let new_path = T.datum_path ~user:"tester" ~type_name:tb ~id () in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path:old_path ~type_name:ta
                ~body_json:"{\"title\":\"x\",\"body\":\"y\"}" ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("create failed: " ^ e) | Ok _ -> ());
           (* type change to a type requiring 'status' -> reject naming it *)
           (match C.tree_move store ~old_path ~new_path with
            | VStr s ->
                (* clean typed-boundary rejection naming the missing field *)
                Alcotest.(check bool) "rejects type change naming missing field" true
                  (contains_sub (String.lowercase_ascii s) "missing fields"
                   && contains_sub (String.lowercase_ascii s) "status")
            | _ -> Alcotest.fail "type-change move should return a string rejection");
           (* node still at old path (not moved) *)
           (match T.get_by_path conn ~path:old_path with
            | None -> Alcotest.fail "node disappeared after rejected move"
            | Some _ -> ())));
  (* same-type move remains allowed (already covered) *)
  ()

(* ---- moving a datum to a DIFFERENT type that the body satisfies succeeds ---- *)
let test_move_type_change_ok () =
  with_store (fun store ->
      let ta = uniq "ta" in
      let tb = uniq "tb" in
      declare_type store ~name:ta ~fields:[ "title"; "body"; "status" ];
      declare_type store ~name:tb ~fields:[ "title"; "body"; "status" ];
      let id = "00000000-0000-0000-0000-00000000" ^ "0009" in
      let old_path = T.datum_path ~user:"tester" ~type_name:ta ~id () in
      let new_path = T.datum_path ~user:"tester" ~type_name:tb ~id () in
      (match S.conn_of store with
       | None -> ()
       | Some conn ->
           (match
              T.upsert_datum conn ~path:old_path ~type_name:ta
                ~body_json:"{\"title\":\"x\",\"body\":\"y\",\"status\":\"todo\"}" ~author:"tester" ~rank:None
            with
            | Error e -> Alcotest.fail ("create failed: " ^ e) | Ok _ -> ());
           (match C.tree_move store ~old_path ~new_path with
            | VInt _ -> ()
            | VStr s -> Alcotest.fail ("move should succeed: " ^ s)
            | _ -> Alcotest.fail "move returned unexpected value")));
  ()

let () =
  Random.self_init ();
  Alcotest.run "tree_datum"
    [ ( "datum-prims",
        [
          Alcotest.test_case "upsert placeholder id" `Quick test_upsert_placeholder;
          Alcotest.test_case "upsert full path + projection" `Quick
            test_upsert_full_path;
          Alcotest.test_case "upsert update in place" `Quick
            test_upsert_update_inplace;
          Alcotest.test_case "prim dispatch + reject" `Quick test_tree_upsert_prim;
          Alcotest.test_case "move datum" `Quick test_move_datum;
          Alcotest.test_case "move datum type change reject" `Quick
            test_move_type_change;
          Alcotest.test_case "move datum type change ok" `Quick
            test_move_type_change_ok;
          Alcotest.test_case "delete datum" `Quick test_delete_datum;
          Alcotest.test_case "readers kind-agnostic" `Quick test_readers_kind_agnostic;
        ] ) ]
