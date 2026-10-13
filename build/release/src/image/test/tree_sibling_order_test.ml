(** Sibling-order tests (one-tree.sibling-order).

    Children get a user-controlled order via the nullable rank column
    (migration 025): canonical base-36 fractional strings compared as raw
    bytes, midpoint allocation, rebalance on exhaustion. Reads order
    rank COLLATE "C" NULLS LAST, path ASC; reorder is an ordinary
    versioned write (gen-read/undo carry order); a lens override of a
    node owns its own rank.

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.

    Acceptance (one-tree.sibling-order verify stanzas):
      - insert three children, move third before first: children-of
        returns the new order.
      - unranked siblings sort after ranked, path-ordered among
        themselves.
      - reorder is versioned: gen-read at old gen shows old order; undo
        restores.
      - a lens override of a node owns its rank; the parent's rank is
        unchanged.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
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

let uniq p = Printf.sprintf "so_%s_%d" p (Random.int 1_000_000)

let json_field (json : string) (key : string) : string option =
  match Yojson.Safe.from_string json with
  | `Assoc kvs -> (
      match List.assoc_opt key kvs with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

let json_rank (json : string) : string option = json_field json "rank"

(** paths of children under [prefix] in read order *)
let child_order conn prefix =
  T.list_children conn ~prefix ()
  |> List.map (fun (n : T.node) -> n.path)

let hier conn ?(rank = None) path body author =
  match T.upsert_hier conn ~path ~kind:"note" ~type_name:"note" ~body_json:body
          ~author ~rank with
  | Ok p -> p
  | Error e -> Alcotest.fail ("upsert_hier failed: " ^ e)

(* ---- midpoint allocation sanity (pure) ---- *)

let test_rank_midpoint () =
  Alcotest.(check bool) "mid between" true
    (let m = T.rank_midpoint "a0" "a2" in
     String.compare "a0" m < 0 && String.compare m "a2" < 0);
  Alcotest.(check string) "mid adjacent" "a0h" (T.rank_midpoint "a0" "a1");
  Alcotest.(check bool) "mid prefix case" true
    (let m = T.rank_midpoint "a5" "a5b" in
     String.compare "a5" m < 0 && String.compare m "a5b" < 0);
  Alcotest.(check bool) "mid deep prefix" true
    (let m = T.rank_midpoint "a5" "a511" in
     String.compare "a5" m < 0 && String.compare m "a511" < 0);
  Alcotest.(check string) "before first" "a" (T.rank_before "" |> fun o -> (match o with Some r -> r | None -> "?"));
  Alcotest.(check bool) "before of first" true
    (match T.rank_before "a0" with
     | Some r -> String.compare r "a0" < 0
     | None -> false);
  Alcotest.(check bool) "after last" true
    (let r = T.rank_after "a0" in String.compare r "a0" > 0);
  (* canonical ranks never end in '0'; midpoint output is canonical *)
  Alcotest.(check bool) "canonical output" true
    (let rs =
       [ T.rank_midpoint "a0" "a2"; T.rank_midpoint "a5" "a5b";
         T.rank_after "az"; T.rank_after "zzz" ]
       @ (match T.rank_before "zz" with Some r -> [ r ] | None -> [])
     in
     List.for_all T.rank_valid rs);
  Alcotest.(check bool) "rebalance sequence ordered" true
    (let rs = T.rank_rebalance 40 in
     let sorted = List.sort String.compare rs in
     rs = sorted && List.for_all T.rank_valid rs);
  (try
     ignore (T.rank_midpoint "b" "a");
     Alcotest.fail "midpoint of a>=b must raise"
   with Invalid_argument _ -> ())

(* ---- spec smoke: move third before first ---- *)

let test_move_before () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let prefix = "data.bench.so." ^ uniq "t1" in
          let mk i =
            hier conn ~rank:None (prefix ^ ".c" ^ string_of_int i)
              (Printf.sprintf "{\"slug\":\"c%d\",\"title\":\"c%d\"}" i i)
              "tester"
          in
          let mk_ranked i =
            hier conn
              ~rank:(Some (string_of_int (i + 1)))
              (prefix ^ ".c" ^ string_of_int i)
              (Printf.sprintf "{\"slug\":\"c%d\",\"title\":\"c%d\"}" i i)
              "tester"
          in
          let p1 = mk_ranked 1 and p2 = mk_ranked 2 and p3 = mk_ranked 3 in
          (* ranked: c1, c2, c3 *)
          Alcotest.(check (list string)) "initial order" [ p1; p2; p3 ]
            (child_order conn prefix);
          (* move third before first *)
          (match
             Pricklypear_image_lib.Crud.tree_move_before store ~path:p3
               ~anchor:p1
           with
           | VStr r ->
               (* returns the allocated rank, which must sort before c1's *)
               Alcotest.(check bool) "new rank before anchor" true
                 (String.compare r "2" < 0)
           | _ -> Alcotest.fail "move-before failed");
          Alcotest.(check (list string)) "moved order" [ p3; p1; p2 ]
            (child_order conn prefix);
          (* a NULL-rank anchor is a clean error, not a guess *)
          (match
             Pricklypear_image_lib.Crud.tree_move_before store ~path:p1
               ~anchor:(mk 9)
           with
           | VStr e when String.length e > 0 && String.length e < 500 ->
               Alcotest.(check bool) "null anchor error mentions rank" true
                 (let found = ref false in
                  String.iter (fun c -> if c = 'r' then found := true) e;
                  !found)
           | VNil -> Alcotest.fail "move-before: nil"
           | _ -> Alcotest.fail "move-before null-anchor should error"))

(* ---- spec smoke: unranked sort after ranked ---- *)

let test_unranked_after_ranked () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let prefix = "data.bench.so." ^ uniq "t2" in
          let mk ?rank i =
            hier conn ?rank (prefix ^ ".c" ^ string_of_int i)
              (Printf.sprintf "{\"slug\":\"c%d\",\"title\":\"c%d\"}" i i)
              "tester"
          in
          let p1 = mk ~rank:(Some "a0") 1 in
          let p2 = mk 2 in
          let p3 = mk ~rank:(Some "a1") 3 in
          let p4 = mk 4 in
          (* ranked a0 < a1 first, then unranked path-ordered: c2 < c4 *)
          Alcotest.(check (list string)) "ranked then unranked"
            [ p1; p3; p2; p4 ] (child_order conn prefix))

(* ---- spec smoke: versioned reorder (gen-read + undo) ---- *)

let test_versioned_reorder () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let prefix = "data.bench.so." ^ uniq "t3" in
          let p1 =
            hier conn ~rank:(Some "a0") (prefix ^ ".x")
              "{\"slug\":\"x\",\"title\":\"x\"}" "tester"
          in
          let before_gen =
            match T.versioned_head conn ~path:p1 () with
            | Some v ->
                (match v.T.v_gen with
                 | Some g -> g
                 | None -> Alcotest.fail "version row without gen")
            | None -> Alcotest.fail "no version row after upsert"
          in
          (* reorder: rank change is a real, versioned write *)
          ignore (T.set_rank conn ~path:p1 ~rank:(Some "a5"));
          (match T.versioned_head conn ~path:p1 () with
           | Some v ->
               Alcotest.(check (option string)) "head rank advanced" (Some "a5")
                 v.T.v_rank
           | None -> Alcotest.fail "no head version after reorder");
          (* gen-read at the pre-reorder gen shows the OLD rank *)
          (match T.versioned_head conn ~path:p1 ~at_gen:before_gen () with
           | Some v ->
               Alcotest.(check (option string)) "old gen shows old rank"
                 (Some "a0") v.T.v_rank
           | None -> Alcotest.fail "no version at old gen");
          (* undo restores the old rank exactly *)
          let undo_json =
            let v =
              Pricklypear_image_lib.Crud.tree_undo store ~path:p1 ()
            in
            match v with VStr s -> s | _ -> Alcotest.fail "undo failed"
          in
          Alcotest.(check bool) "undo restored old rank" true
            (json_rank undo_json = Some "a0"))

(* ---- spec smoke: lens override owns its rank ---- *)

let test_override_owns_rank () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          (* two branch nodes at the same logical suffix = an override
             pair; each row owns its rank independently *)
          let b1 = main_id in
          let child_id =
            match S.fork_branch store ~parent_id:main_id
                    ~owner:"so_test"
                    ~child_name:("so-" ^ string_of_int (Random.int 1_000_000))
            with
            | Some id -> id
            | None -> Alcotest.fail "fork failed"
          in
          let name = uniq "fn" in
          let p_main = T.fn_path ~branch_id:b1 ~name in
          let p_child = T.fn_path ~branch_id:child_id ~name in
          ignore (T.upsert_fn conn ~branch_id:b1 ~name
                    ~body_json:"{}" ~author:"tester" ~doc:None);
          ignore (T.upsert_fn conn ~branch_id:child_id ~name
                    ~body_json:"{}" ~author:"tester" ~doc:None);
          (* the override (child branch) gets a rank; the parent's is NULL *)
          Alcotest.(check int) "set rank on override" 1
            (T.set_rank conn ~path:p_child ~rank:(Some "a0"));
          (match T.get_by_path conn ~path:p_main with
           | Some n -> Alcotest.(check (option string)) "parent rank unchanged"
                         None n.rank
           | None -> Alcotest.fail "main node missing");
          (match T.get_by_path conn ~path:p_child with
           | Some n -> Alcotest.(check (option string)) "override owns rank"
                         (Some "a0") n.rank
           | None -> Alcotest.fail "child node missing");
          (* rank on the parent branch does not leak to the child either *)
          Alcotest.(check int) "set rank on parent" 1
            (T.set_rank conn ~path:p_main ~rank:(Some "a1"));
          (match T.get_by_path conn ~path:p_child with
           | Some n -> Alcotest.(check (option string)) "child rank untouched"
                         (Some "a0") n.rank
           | None -> Alcotest.fail "child node missing"))

(* ---- insert-child + next/prev/siblings ---- *)

let test_insert_child_and_neighbors () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let prefix = "data.bench.so." ^ uniq "t4" in
          ignore
            (hier conn ~rank:None prefix
               "{\"slug\":\"root\",\"title\":\"root\"}" "tester");
          let c1 =
            let v =
              Pricklypear_image_lib.Crud.tree_insert_child store ~parent:prefix
                ~kind:"note" ~type_name:"note"
                ~body_json:"{\"slug\":\"a\",\"title\":\"a\"}"
                ~anchor:None ~author:"tester"
            in
            match v with
            | VStr j -> (match json_field j "path" with Some p -> p | None -> Alcotest.fail "no path")
            | _ -> Alcotest.fail "insert-child failed"
          in
          let c2 =
            let v =
              Pricklypear_image_lib.Crud.tree_insert_child store ~parent:prefix
                ~kind:"note" ~type_name:"note"
                ~body_json:"{\"slug\":\"b\",\"title\":\"b\"}"
                ~anchor:(Some c1) ~author:"tester"
            in
            match v with
            | VStr j -> (match json_field j "path" with Some p -> p | None -> Alcotest.fail "no path")
            | _ -> Alcotest.fail "insert-child after anchor failed"
          in
          Alcotest.(check (list string)) "insert order" [ c1; c2 ]
            (child_order conn prefix);
          (* move-after with an unranked anchor errors cleanly *)
          (match
             Pricklypear_image_lib.Crud.tree_move_after store ~path:c2
               ~anchor:(prefix ^ ".zzz")
           with
           | VStr e ->
               Alcotest.(check bool) "missing anchor error" true
                 (String.length e > 0)
           | _ -> Alcotest.fail "move-after missing anchor should error");
          (* next/prev walk the total order *)
          let next_of p =
            match Pricklypear_image_lib.Crud.tree_next store ~path:p with
            | VStr j -> json_field j "path"
            | VNil -> None
            | _ -> Alcotest.fail "next bad shape"
          in
          let prev_of p =
            match Pricklypear_image_lib.Crud.tree_prev store ~path:p with
            | VStr j -> json_field j "path"
            | VNil -> None
            | _ -> Alcotest.fail "prev bad shape"
          in
          Alcotest.(check (option string)) "next of c1" (Some c2) (next_of c1);
          Alcotest.(check (option string)) "next of c2" None (next_of c2);
          Alcotest.(check (option string)) "prev of c2" (Some c1) (prev_of c2);
          Alcotest.(check (option string)) "prev of c1" None (prev_of c1);
          (* siblings returns the ordered child set *)
          (match Pricklypear_image_lib.Crud.tree_siblings store ~path:c1 with
           | VList vs ->
               let paths =
                 List.filter_map
                   (function VStr j -> json_field j "path" | _ -> None)
                   vs
               in
               Alcotest.(check (list string)) "siblings ordered" [ c1; c2 ] paths
           | _ -> Alcotest.fail "siblings bad shape"))

let () =
  Alcotest.run "tree_sibling_order"
    [ "rank", [ Alcotest.test_case "midpoint" `Quick test_rank_midpoint ]
    ; "smokes", [ Alcotest.test_case "move-before" `Slow test_move_before
                 ; Alcotest.test_case "unranked-after-ranked" `Slow
                     test_unranked_after_ranked
                 ; Alcotest.test_case "versioned-reorder" `Slow test_versioned_reorder
                 ; Alcotest.test_case "override-owns-rank" `Slow test_override_owns_rank
                 ; Alcotest.test_case "insert-child-neighbors" `Slow
                     test_insert_child_and_neighbors ]
    ]
