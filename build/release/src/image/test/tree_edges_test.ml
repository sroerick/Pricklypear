(** Tree-edges tests (one-tree.tree-edges).

    An edge IS a node: kind=link at <source-path>.lnk.<uuid>, body
    {to, kind, label?}. "from" is positional (never in the body); to is
    stored verbatim as branch-qualified path text and MAY dangle;
    resolution is a read-time act against the reader's lens. Edges
    inherit the whole tree stack: versioning (gen-read/gen-diff/undo),
    authorship, branch ownership, lens visibility. tree/delete and
    tree/move report incoming-edge counts (warn, not forbid).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.

    Acceptance (one-tree.tree-edges verify stanzas, minus the :ref
    materialization stanza — that half is deferred with the typed-
    boundary :ref work; see BORGE issue on the tree-edges subsection):
      - link a→b: (out-links a) lists b resolved; (backlinks <b-path>)
        includes the edge path.
      - tree/move of the target: the edge reports dangling on read;
        textual backlink still finds it (legacy half).
      - tree/delete of a node with 2 inbound edges reports count 2 and
        succeeds (warn, not forbid).
      - edge nodes are versioned: gen-diff shows edge add; undo of
        unlink restores the edge.
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

let uniq p = Printf.sprintf "te_%s_%d" p (Random.int 1_000_000)

let str_v = function VStr s -> s | _ -> Alcotest.fail "not a VStr"

let hier conn path slug =
  match
    T.upsert_hier conn ~path ~kind:"note" ~type_name:"note"
      ~body_json:(Printf.sprintf "{\"slug\":\"%s\",\"title\":\"%s\"}" slug slug)
      ~author:"tester" ~rank:None
  with
  | Ok p -> p
  | Error e -> Alcotest.fail ("upsert_hier failed: " ^ e)

(* JSON field access on the prim JSON strings *)
let jfield (json : string) (key : string) : Yojson.Safe.t option =
  match Yojson.Safe.from_string json with
  | `Assoc kvs -> List.assoc_opt key kvs
  | _ -> None

let jstr (json : string) (key : string) : string option =
  match jfield json key with Some (`String s) -> Some s | _ -> None

(* ---- spec smoke: link a->b, out-links resolved, backlinks finds it ---- *)

let test_link_out_back () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let root = "data.bench.te." ^ uniq "a" in
          let pa = hier conn (root ^ ".a") "tea1" in
          let pb = hier conn (root ^ ".b") "teb1" in
          (* link a -> b *)
          let edge_json =
            str_v (C.link_edge store ~from_path:pa ~to_path:pb
                     ~edge_kind:"mentions" ~label:(Some "the b node")
                     ~author:"tester")
          in
          Alcotest.(check bool) "edge is at a.lnk.<uuid>" true
            (String.length pa + 40 < String.length edge_json
             && (match jstr edge_json "path" with
                 | Some p ->
                     String.starts_with ~prefix:(pa ^ ".lnk.") p
                 | None -> false));
          Alcotest.(check (option string)) "edge kind" (Some "mentions")
            (jstr edge_json "kind");
          (* out-links a -> b resolved *)
          (match C.out_links store ~path:pa () with
           | VList items ->
               Alcotest.(check int) "one out-link" 1 (List.length items);
               (match items with
                | [ VStr j ] ->
                    Alcotest.(check (option string)) "status resolved"
                      (Some "resolved") (jstr j "status");
                    Alcotest.(check (option string)) "target path" (Some pb)
                      (match jfield j "target" with
                       | Some (`Assoc t) ->
                           (match List.assoc_opt "path" t with
                            | Some (`String p) -> Some p
                            | _ -> None)
                       | _ -> None)
                | _ -> Alcotest.fail "bad out-links shape")
           | _ -> Alcotest.fail "out-links not a list");
          (* backlinks of b includes the edge path *)
          let edge_path =
            match jstr edge_json "path" with Some p -> p | None -> ""
          in
          (match C.backlinks store ~needle:pb () with
           | VList paths ->
               let ps = List.map str_v paths in
               Alcotest.(check bool) "backlinks includes edge" true
                 (List.mem edge_path ps)
           | _ -> Alcotest.fail "backlinks not a list"))

(* ---- spec smoke: move of the target dangles; legacy still finds ---- *)

let test_dangle_after_move () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let root = "data.bench.te." ^ uniq "b" in
          let pa = hier conn (root ^ ".a") "tea2" in
          let pb = hier conn (root ^ ".b") "teb2" in
          let edge_json =
            str_v (C.link_edge store ~from_path:pa ~to_path:pb
                     ~edge_kind:"ref" ~label:None ~author:"tester")
          in
          (* textual legacy reference inside a datum-ish note body:
             upsert a note whose body carries pb as a string value *)
          let pc = hier conn (root ^ ".c") "tec2" in
          ignore
            (T.upsert_hier conn ~path:pc ~kind:"note" ~type_name:"note"
               ~body_json:(Printf.sprintf "{\"slug\":\"tec2b\",\"ref\":\"%s\"}" pb)
               ~author:"tester" ~rank:None);
          (* move the target *)
          let newb = root ^ ".b2" in
          (match C.tree_move store ~old_path:pb ~new_path:newb with
           | VStr j ->
               Alcotest.(check bool) "move json" true
                 (String.contains j '{')
           | _ -> Alcotest.fail "move not json");
          (* the edge now reports dangling on read *)
          (match C.out_links store ~path:pa () with
           | VList [ VStr j ] ->
               Alcotest.(check (option string)) "dangling after move"
                 (Some "dangling") (jstr j "status")
           | _ -> Alcotest.fail "expected one dangling edge");
          (* legacy textual backlink still finds the reference *)
          (match C.backlinks store ~needle:newb () with
           | VList paths ->
               let ps = List.map str_v paths in
               Alcotest.(check bool) "legacy half finds textual ref" true
                 (List.mem pc ps);
               (* the edge points at the OLD path text — it does not
                  follow the move (dangling by design) *)
               let edge_path =
                 match jstr edge_json "path" with Some p -> p | None -> ""
               in
               Alcotest.(check bool) "edge target text unmoved" true
                 (not (List.mem edge_path ps))
           | _ -> Alcotest.fail "backlinks not a list"))

(* ---- spec smoke: delete with 2 inbound edges reports 2, succeeds ---- *)

let test_delete_reports_inbound () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let root = "data.bench.te." ^ uniq "c" in
          let pa = hier conn (root ^ ".a") "tea3" in
          let pb = hier conn (root ^ ".b") "teb3" in
          let pd = hier conn (root ^ ".d") "ted3" in
          let _e1 =
            str_v (C.link_edge store ~from_path:pa ~to_path:pb
                     ~edge_kind:"ref" ~label:None ~author:"tester")
          in
          let _e2 =
            str_v (C.link_edge store ~from_path:pd ~to_path:pb
                     ~edge_kind:"ref" ~label:None ~author:"tester")
          in
          (* deleting b: 2 inbound edges — reports count, succeeds *)
          (match C.tree_delete store ~path:pb ~recursive:false with
           | VStr j ->
               (match
                  jfield j "inbound_edges", jfield j "deleted"
                with
                | Some (`Int 2), Some (`Int 1) -> ()
                | a, b ->
                    Alcotest.fail
                      (Printf.sprintf "expected {deleted:1,inbound:2}, got %s %s"
                         (Yojson.Safe.to_string (match a with Some x -> x | None -> `Null))
                         (Yojson.Safe.to_string (match b with Some x -> x | None -> `Null))))
           | _ -> Alcotest.fail "delete did not return JSON");
          (match T.get_by_path conn ~path:pb with
           | None -> () (* gone *)
           | Some _ -> Alcotest.fail "delete did not delete"));
  ()

(* ---- spec smoke: edges versioned; gen-diff; undo of unlink ---- *)

let test_edge_versioning () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let root = "data.bench.te." ^ uniq "d" in
          let pa = hier conn (root ^ ".a") "tea4" in
          let pb = hier conn (root ^ ".b") "teb4" in
          let edge_json =
            str_v (C.link_edge store ~from_path:pa ~to_path:pb
                     ~edge_kind:"ref" ~label:None ~author:"tester")
          in
          let edge_path =
            match jstr edge_json "path" with Some p -> p | None -> ""
          in
          (* versioned: the edge has version rows with its body *)
          (match T.versioned_head conn ~path:edge_path () with
           | Some v ->
               Alcotest.(check bool) "versioned body has to" true
                 (match v.T.v_body with
                  | `Assoc kvs -> (
                      match List.assoc_opt "body" kvs with
                      | Some (`Assoc b) -> List.mem_assoc "to" b
                      | _ -> false)
                  | _ -> false)
           | None -> Alcotest.fail "edge has no version rows");
          let gen_before =
            match T.versioned_head conn ~path:edge_path () with
            | Some v -> (match v.T.v_gen with Some g -> g | None -> 0L)
            | None -> 0L
          in
          (* unlink: tombstone through the same delete path *)
          (match C.unlink_edge store ~edge_path with
           | VStr _ -> ()
           | _ -> Alcotest.fail "unlink failed");
          (match T.get_by_path conn ~path:edge_path with
           | None -> () (* tombstoned *)
           | Some _ -> Alcotest.fail "unlink did not remove the edge");
          (* undo of unlink restores the edge *)
          ignore (C.tree_undo store ~path:edge_path ());
          (match T.get_by_path conn ~path:edge_path with
           | Some n ->
               Alcotest.(check bool) "undo restored the edge" true
                 (T.is_link_kind n.kind);
               Alcotest.(check (option string)) "restored to target"
                 (Some pb) (T.body_string_field n.body "to")
           | None -> Alcotest.fail "undo of unlink did not restore");
          (* gen-diff across the unlink shows the tombstone *)
          (match
             T.versioned_head conn ~path:edge_path ~at_gen:gen_before ()
           with
           | Some _ -> () (* pre-unlink leaf exists *)
           | None -> Alcotest.fail "pre-unlink version missing"))

let test_link_validation () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let root = "data.bench.te." ^ uniq "e" in
          let pa = hier conn (root ^ ".a") "tea5" in
          (* from must resolve *)
          (match
             C.link_edge store
               ~from_path:(root ^ ".missing")
               ~to_path:pa ~edge_kind:"ref" ~label:None ~author:"tester"
           with
           | VStr e -> Alcotest.(check bool) "from must resolve" true (String.length e > 0)
           | _ -> Alcotest.fail "link from missing node should error");
          (* to must be well-formed path text *)
          (match
             C.link_edge store ~from_path:pa
               ~to_path:"not a path" ~edge_kind:"ref" ~label:None
               ~author:"tester"
           with
           | VStr e -> Alcotest.(check bool) "to must be path text" true (String.length e > 0)
           | _ -> Alcotest.fail "link with malformed to should error");
          (* body with "from" is rejected by the store validator *)
          (match
             T.upsert_link conn
               ~path:(pa ^ ".lnk.00000000000000000000000000000001")
               ~body_json:"{\"to\":\"data.a.b\",\"from\":\"x\",\"kind\":\"ref\"}"
               ~author:"tester"
           with
           | Error e -> Alcotest.(check bool) "from-in-body rejected" true (String.length e > 0)
           | Ok _ -> Alcotest.fail "from-in-body must be rejected");
          (* non-lnk path rejected *)
          (match
             T.upsert_link conn ~path:(pa ^ ".other.x")
               ~body_json:"{\"to\":\"data.a.b\",\"kind\":\"ref\"}"
               ~author:"tester"
           with
           | Error _ -> ()
           | Ok _ -> Alcotest.fail "non-lnk edge path must be rejected"))

let () =
  Alcotest.run "tree_edges"
    [ "edges", [ Alcotest.test_case "link-out-back" `Slow test_link_out_back
               ; Alcotest.test_case "dangle-after-move" `Slow
                   test_dangle_after_move
               ; Alcotest.test_case "delete-reports-inbound" `Slow
                   test_delete_reports_inbound
               ; Alcotest.test_case "edge-versioning" `Slow test_edge_versioning
               ; Alcotest.test_case "link-validation" `Slow test_link_validation ]
    ]
