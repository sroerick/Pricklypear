(* op-transplant (branching.op-transplant, spec commit 3f4d6d1, 2026-10-16):
   branch/pick — named writes transplant from a source branch's gen stream
   onto a target branch as NEW WRITES, cherry-pick over the log.

   DB cases need Postgres via scripts/dev.sh and skip if unreachable.

   Verifies:
   - pick at a checkpoint lands exactly the named bodies verbatim as new
     writes on the target (version trail + origin=pick in branch/log);
     gen moves forward; target base_gen untouched;
   - unchanged target pins skip (pin hygiene); diverged target pins skip
     with diverged-target + both hashes and land with 'force (+ exactly one
     code_history before-image);
   - closure first: an unbound ref holes the pick WHOLE (target untouched);
     a dep defined on the source auto-includes and lands;
   - absent-at-coordinate refuses (unbind stays a separate explicit op). *)

module S = Pricklypear_image_lib.Store
module P = Pricklypear_image_lib.Branch_pick

let main_id = "00000000-0000-0000-0000-000000000001"

let lit n =
  Pricklypear_lisp.Ast.Lit (Pricklypear_lisp.Ast.VInt (Int64.of_int n))

let body_json e = Pricklypear_lisp.Ast.expr_to_body_json e

let app f args = Pricklypear_lisp.Ast.App (Pricklypear_lisp.Ast.Var f, args)

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
      Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env in
    Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
        let cfg = S.config_from_env () in
        let store = S.make ~cfg ~branch_id:main_id () in
        S.connect store;
        match S.conn_of store with
        | None ->
            Printf.printf
              "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
        | Some _ -> f store)

let cleanup_branch store ~branch_id =
  let conn = Option.get (S.conn_of store) in
  let del sql =
    ignore (S.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql) in
  del "DELETE FROM branch_checkpoints WHERE branch_id = $1";
  del "DELETE FROM view_bindings WHERE branch_id = $1";
  del "DELETE FROM type_defs WHERE branch_id = $1";
  del "DELETE FROM function_defs WHERE branch_id = $1";
  del "DELETE FROM branches WHERE id = $1"

let scalar1 store sql ~branch_id =
  match S.conn_of store with
  | Some conn -> (
      let rows = S.exec conn ~params:[ Pgx.Value.of_string branch_id ] sql in
      match rows with
      | [ [ g ] ] -> (match Pgx.Value.to_int64 g with Some v -> v | None -> 0L)
      | _ -> 0L)
  | None -> 0L

let branch_gen store ~branch_id =
  scalar1 store
    "SELECT COALESCE((SELECT generation FROM branch_generations WHERE branch_id = $1::uuid), 0)::bigint"
    ~branch_id

let base_gen store ~branch_id =
  scalar1 store "SELECT base_gen::bigint FROM branches WHERE id = $1::uuid"
    ~branch_id

let code_history_count store ~branch_id ~name =
  match S.conn_of store with
  | Some conn -> (
      let rows =
        S.exec conn
          ~params:[ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
          "SELECT count(*)::bigint FROM code_history WHERE branch_id = $1::uuid AND name = $2"
      in
      match rows with
      | [ [ c ] ] -> (match Pgx.Value.to_int64 c with Some v -> v | None -> 0L)
      | _ -> 0L)
  | None -> 0L

let own_body store ~branch_id ~name =
  match S.lookup_function_on_branch store ~branch_id ~name with
  | Some (b, _, _, _) -> Some b
  | None -> None

(* Semantic body compare: function_defs.body is a jsonb column, so the raw
   text is PG-normalized (spaced) — compare parsed exprs, never raw text
   (the opaque-body contract: parse at the eval boundary). *)
let body_expr store ~branch_id ~name =
  Option.bind (own_body store ~branch_id ~name)
    Pricklypear_lisp.Ast.expr_of_body_json

let landed_names = function
  | P.Pick_ok { landed; _ } ->
      List.map (fun (r : P.pick_row) -> r.P.pr_name) landed
  | _ -> []

let skipped_of = function
  | P.Pick_ok { skipped; _ } ->
      List.map (fun (s : P.skip_row) -> (s.P.sk_name, s.P.sk_reason)) skipped
  | _ -> []

let pick_result_json_str = function
  | P.Pick_ok { landed; auto; skipped } ->
      Printf.sprintf "ok landed=%s auto=%s skipped=%d"
        (String.concat "," (landed_names (P.Pick_ok { landed; auto; skipped })))
        (String.concat "," auto)
        (List.length skipped)
  | P.Pick_holes holes ->
      "holes: " ^ Pricklypear_image_lib.Branch_push.holes_msg holes
  | P.Pick_refused why -> "refused: " ^ why

(* fork src + dst off main; both cleaned up by the caller *)
let fork_pair store ~sfx =
  let src =
    S.fork_branch store ~parent_id:main_id
      ~owner:"pick_test" ~child_name:("pick_src_" ^ sfx) in
  let dst =
    S.fork_branch store ~parent_id:main_id
      ~owner:"pick_test" ~child_name:("pick_dst_" ^ sfx) in
  match (src, dst) with
  | Some src_id, Some dst_id -> Some (src_id, dst_id)
  | _ -> None

(* ---- 1. pick at a checkpoint: lands verbatim, origin=pick, base_gen safe ---- *)

let test_pick_lands_at_cp () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_pair store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some (src_id, dst_id) ->
               store.branch_id <- src_id;
               S.persist_function store "alpha" (body_json (lit 1)) "tester";
               S.persist_function store "beta" (body_json (lit 2)) "tester";
               S.persist_function store "gamma" (body_json (lit 3)) "tester";
               (match
                  S.create_checkpoint store ~branch_id:src_id ~name:"one"
                    ~message:None ~author:"tester"
                with
                | Error why -> Alcotest.fail ("cp failed: " ^ why)
                | Ok _ ->
                    (* post-cp source work that must NOT travel *)
                    S.persist_function store "delta" (body_json (lit 4))
                      "tester";
                    let bg0 = base_gen store ~branch_id:dst_id in
                    let gen0 = branch_gen store ~branch_id:dst_id in
                    (match
                       P.pick store ~source_id:src_id ~target_id:dst_id
                         ~names:[ "alpha"; "beta" ]
                         ~at:(P.Pick_cp "one") ~force:false ~kernel:[]
                         ~author:"picker" ~source_ref:"pick-test-src"
                     with
                     | P.Pick_ok { landed; auto; skipped } ->
                         Alcotest.(check bool) "landed exactly alpha+beta" true
                           (let ns =
                              List.map (fun (r : P.pick_row) -> r.P.pr_name) landed
                            in
                            List.sort compare ns = [ "alpha"; "beta" ]);
                         Alcotest.(check (list string)) "no auto deps" [] auto;
                         Alcotest.(check int) "no skips" 0 (List.length skipped);
                           Alcotest.(check bool) "alpha verbatim at cp" true
                             (body_expr store ~branch_id:dst_id ~name:"alpha"
                              = Some (lit 1));
                           Alcotest.(check bool) "beta verbatim at cp" true
                             (body_expr store ~branch_id:dst_id ~name:"beta"
                              = Some (lit 2));
                         Alcotest.(check bool) "gamma NOT picked" true
                           (own_body store ~branch_id:dst_id ~name:"gamma" = None);
                         Alcotest.(check bool) "delta NOT picked" true
                           (own_body store ~branch_id:dst_id ~name:"delta" = None);
                         Alcotest.(check bool) "target gen moved" true
                           (Int64.compare (branch_gen store ~branch_id:dst_id) gen0
                           > 0);
                         Alcotest.(check bool) "target base_gen untouched" true
                           (Int64.equal bg0 (base_gen store ~branch_id:dst_id));
                         (* branch/log records the picks with origin=pick *)
                         let log =
                           S.checkpoint_log store ~branch_id:dst_id
                             ~since_gen:None in
                         let picks =
                           List.filter
                             (fun (e : S.cp_log_row) ->
                               String.equal e.S.l_origin "pick")
                             log in
                         Alcotest.(check int) "two pick entries" 2
                           (List.length picks);
                         Alcotest.(check bool) "pick entries name alpha+beta" true
                           (let ns =
                              List.map (fun (e : S.cp_log_row) -> e.S.l_name) picks
                            in
                            List.mem "alpha" ns && List.mem "beta" ns);
                         Alcotest.(check bool) "origin_detail non-empty" true
                           (List.for_all
                              (fun (e : S.cp_log_row) ->
                                String.length e.S.l_origin_detail > 0)
                                picks)
                     | r -> Alcotest.fail ("pick failed: " ^ pick_result_json_str r)));
               cleanup_branch store ~branch_id:src_id;
               cleanup_branch store ~branch_id:dst_id))

(* ---- 2. clobber guard: unchanged skips, diverged skips, 'force lands ---- *)

let test_clobber_guard () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_pair store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some (src_id, dst_id) ->
               store.branch_id <- src_id;
               S.persist_function store "alpha" (body_json (lit 1)) "tester";
               (* target diverges alpha BEFORE the pick *)
               store.branch_id <- dst_id;
               S.persist_function store "alpha" (body_json (lit 9)) "tester";
               let ch_before =
                 code_history_count store ~branch_id:dst_id ~name:"alpha" in
               (* default: diverged-target skip, nothing lands *)
               (match
                  P.pick store ~source_id:src_id ~target_id:dst_id
                    ~names:[ "alpha" ] ~at:P.Pick_head ~force:false ~kernel:[]
                    ~author:"picker" ~source_ref:"pick-test-src"
                with
                | P.Pick_ok { landed; skipped; _ } ->
                    Alcotest.(check int) "diverged: nothing lands" 0
                      (List.length landed);
                    (match skipped_of (P.Pick_ok { landed = []; auto = []; skipped }) with
                     | [ (_, why) ] ->
                         Alcotest.(check bool) "reason names diverged-target" true
                             (let rec contains s sub i =
                                i + String.length sub <= String.length s
                                && (String.sub s i (String.length sub) = sub
                                    || contains s sub (i + 1)) in
                              contains why "diverged-target" 0)
                     | other ->
                         Alcotest.fail
                           ("expected one skip, got " ^ string_of_int (List.length other)));
                    Alcotest.(check bool) "target body untouched" true
                        (body_expr store ~branch_id:dst_id ~name:"alpha"
                           = Some (lit 9))
                | r -> Alcotest.fail ("pick failed: " ^ pick_result_json_str r));
               (* 'force lands; exactly one code_history before-image fires *)
               (match
                  P.pick store ~source_id:src_id ~target_id:dst_id
                    ~names:[ "alpha" ] ~at:P.Pick_head ~force:true ~kernel:[]
                    ~author:"picker" ~source_ref:"pick-test-src"
                with
                | P.Pick_ok { landed; _ } ->
                    Alcotest.(check (list string)) "force lands alpha"
                      [ "alpha" ] (landed_names (P.Pick_ok { landed; auto = []; skipped = [] }));
                    Alcotest.(check bool) "target body replaced" true
                        (body_expr store ~branch_id:dst_id ~name:"alpha"
                           = Some (lit 1));
                    Alcotest.(check int) "one before-image fired" 1
                      (Int64.to_int
                         (Int64.sub
                            (code_history_count store ~branch_id:dst_id
                               ~name:"alpha")
                            ch_before))
                | r -> Alcotest.fail ("force pick failed: " ^ pick_result_json_str r));
               (* re-pick of the now-identical pin: unchanged skip, no write *)
               (match
                  P.pick store ~source_id:src_id ~target_id:dst_id
                    ~names:[ "alpha" ] ~at:P.Pick_head ~force:false ~kernel:[]
                    ~author:"picker" ~source_ref:"pick-test-src"
                with
                | P.Pick_ok { landed; skipped; _ } ->
                    Alcotest.(check int) "re-pick lands nothing" 0
                      (List.length landed);
                    (match skipped with
                     | [ s ] ->
                         Alcotest.(check bool) "reason is unchanged" true
                           (String.equal s.P.sk_reason "unchanged on target")
                     | other ->
                         Alcotest.fail
                           ("expected one skip, got " ^ string_of_int (List.length other)))
                | r -> Alcotest.fail ("re-pick failed: " ^ pick_result_json_str r));
               cleanup_branch store ~branch_id:src_id;
               cleanup_branch store ~branch_id:dst_id))

(* ---- 3. closure first: holes refuse whole; source deps auto-include ---- *)

let test_closure () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_pair store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some (src_id, dst_id) ->
               store.branch_id <- src_id;
               (* caller references g, defined NOWHERE *)
               S.persist_function store "caller" (body_json (app "g" [ lit 1 ]))
                 "tester";
               (match
                  P.pick store ~source_id:src_id ~target_id:dst_id
                    ~names:[ "caller" ] ~at:P.Pick_head ~force:false ~kernel:[]
                    ~author:"picker" ~source_ref:"pick-test-src"
                with
                | P.Pick_holes holes ->
                    Alcotest.(check bool) "caller holes on g" true
                      (match holes with
                       | [ (n, missing) ] ->
                           String.equal n "caller" && missing = [ "g" ]
                       | _ -> false);
                    Alcotest.(check bool) "refused pick lands NOTHING" true
                      (own_body store ~branch_id:dst_id ~name:"caller" = None)
                | r -> Alcotest.fail ("expected holes, got " ^ pick_result_json_str r));
               (* define g on the source: it auto-includes and lands *)
               S.persist_function store "g" (body_json (lit 2)) "tester";
               (match
                  P.pick store ~source_id:src_id ~target_id:dst_id
                    ~names:[ "caller" ] ~at:P.Pick_head ~force:false ~kernel:[]
                    ~author:"picker" ~source_ref:"pick-test-src"
                with
                | P.Pick_ok { landed; auto; _ } ->
                    Alcotest.(check (list string)) "g auto-included" [ "g" ] auto;
                    Alcotest.(check int) "caller + g landed" 2
                      (List.length landed);
                    Alcotest.(check bool) "caller verbatim" true
                        (body_expr store ~branch_id:dst_id ~name:"caller"
                           = Some (app "g" [ lit 1 ]));
                    Alcotest.(check bool) "g verbatim" true
                        (body_expr store ~branch_id:dst_id ~name:"g"
                           = Some (lit 2))
                | r -> Alcotest.fail ("pick failed: " ^ pick_result_json_str r));
               cleanup_branch store ~branch_id:src_id;
               cleanup_branch store ~branch_id:dst_id))

(* ---- 4. refusals: absent-at-coordinate, unknown cp, same branch, empty ---- *)

let test_refusals () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_pair store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some (src_id, dst_id) ->
               store.branch_id <- src_id;
               S.persist_function store "alpha" (body_json (lit 1)) "tester";
               (match
                  S.create_checkpoint store ~branch_id:src_id ~name:"snap"
                    ~message:None ~author:"tester"
                with
                | Error why -> Alcotest.fail ("cp failed: " ^ why)
                | Ok _ ->
                    (* defined only AFTER the cp: ABSENT at that coordinate *)
                    S.persist_function store "late" (body_json (lit 5)) "tester";
                    let refuses why r =
                      match r with
                      | P.Pick_refused _ -> ()
                      | r2 ->
                          Alcotest.fail
                            (why ^ ": expected refusal, got "
                           ^ pick_result_json_str r2)
                    in
                    refuses "late-at-cp"
                      (P.pick store ~source_id:src_id ~target_id:dst_id
                         ~names:[ "late" ] ~at:(P.Pick_cp "snap") ~force:false
                         ~kernel:[] ~author:"picker" ~source_ref:"s");
                    refuses "unknown-name"
                      (P.pick store ~source_id:src_id ~target_id:dst_id
                         ~names:[ "nope" ] ~at:P.Pick_head ~force:false
                         ~kernel:[] ~author:"picker" ~source_ref:"s");
                    refuses "unknown-cp"
                      (P.pick store ~source_id:src_id ~target_id:dst_id
                         ~names:[ "alpha" ] ~at:(P.Pick_cp "nope") ~force:false
                         ~kernel:[] ~author:"picker" ~source_ref:"s");
                    refuses "empty-set"
                      (P.pick store ~source_id:src_id ~target_id:dst_id
                         ~names:[] ~at:P.Pick_head ~force:false ~kernel:[]
                         ~author:"picker" ~source_ref:"s");
                    refuses "same-branch"
                      (P.pick store ~source_id:src_id ~target_id:src_id
                         ~names:[ "alpha" ] ~at:P.Pick_head ~force:false
                         ~kernel:[] ~author:"picker" ~source_ref:"s"));
               cleanup_branch store ~branch_id:src_id;
               cleanup_branch store ~branch_id:dst_id))

let () =
  Random.self_init ();
  Alcotest.run "op-transplant"
    [
      ( "db"
      , [
          Alcotest.test_case "pick at cp lands verbatim + origin=pick + base_gen safe"
            `Quick test_pick_lands_at_cp;
          Alcotest.test_case "clobber guard: unchanged/diverged skip, 'force lands"
            `Quick test_clobber_guard;
          Alcotest.test_case "closure: holes refuse whole, deps auto-include"
            `Quick test_closure;
          Alcotest.test_case "refusals: absent-at-cp, unknown cp/name, empty, same"
            `Quick test_refusals;
        ] ) ]
