(* file-bridge P2 (branching.file-bridge, spec commit 3f4d6d1, 2026-10-16):
   branch/ingest — one server call lands a projection file's def-op batch.

   DB cases need Postgres via scripts/dev.sh and skip if unreachable.

   Verifies:
   - a define lands as a new write with origin=file in branch/log;
   - an unchanged body (jsonb-equal) skips "unchanged on target" with zero ops;
   - a diverged target (head hash differs from BOTH the incoming body and the
     base_hash) skips; 'force lands with a code_history before-image;
   - base_hash = target head hash -> clean redefine lands without force;
   - unbind deletes the pin (tombstone, origin=file);
   - a ref bound nowhere holes the ingest WHOLE; a dep defined by the
     incoming batch auto-includes;
   - base-gen ahead of the branch refuses. *)

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

(* Semantic body compare: function_defs.body is jsonb — PG-normalized text.
   Parse at the boundary, never compare raw text. *)
let body_expr store ~branch_id ~name =
  Option.bind (own_body store ~branch_id ~name)
    Pricklypear_lisp.Ast.expr_of_body_json

let pin_hash store ~branch_id ~name =
  match
    List.find_opt
      (fun (p : S.pin_row) -> String.equal p.S.p_name name)
      (S.pins_at_head store ~branch_id)
  with
  | Some p -> Some p.S.p_hash
  | None -> None

let io name body =
  {
    P.io_op = "define";
    P.io_name = name;
    P.io_body = body;
    P.io_doc = None;
    P.io_base_hash = None;
  }

let io_h name body h = { (io name body) with P.io_base_hash = Some h }

let iu name h =
  {
    P.io_op = "unbind";
    P.io_name = name;
    P.io_body = "";
    P.io_doc = None;
    P.io_base_hash = Some h;
  }

let ingest_str = function
  | P.Ingest_ok { i_landed; i_skipped } ->
      Printf.sprintf "ok landed=[%s] skipped=%d"
        (String.concat "," i_landed)
        (List.length i_skipped)
  | P.Ingest_holes holes ->
      "holes: " ^ Pricklypear_image_lib.Branch_push.holes_msg holes
  | P.Ingest_refused why -> "refused: " ^ why

let skipped_reasons = function
  | P.Ingest_ok { i_skipped; _ } ->
      List.map (fun (s : P.skip_row) -> s.P.sk_reason) i_skipped
  | _ -> []

let contains s sub =
  let ls = String.length s and lp = String.length sub in
  let rec go i = i + lp <= ls && (String.sub s i lp = sub || go (i + 1)) in
  go 0

let fork_one store ~sfx =
  S.fork_branch store ~parent_id:main_id
    ~owner:"file_bridge" ~child_name:("fb_" ^ sfx)

(* ---- 1. define lands with origin=file ---- *)

let test_ingest_define_lands () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io "alpha" (body_json (lit 3)) ]
                    ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; i_skipped } ->
                    Alcotest.(check (list string)) "alpha landed" [ "alpha" ]
                      i_landed;
                    Alcotest.(check int) "no skips" 0 (List.length i_skipped);
                    Alcotest.(check bool) "alpha verbatim" true
                      (body_expr store ~branch_id:dst_id ~name:"alpha"
                       = Some (lit 3));
                    let log =
                      S.checkpoint_log store ~branch_id:dst_id ~since_gen:None in
                    let file_rows =
                      List.filter
                        (fun (e : S.cp_log_row) ->
                          String.equal e.S.l_origin "file")
                        log in
                    Alcotest.(check bool) "log records origin=file" true
                      (List.exists
                         (fun (e : S.cp_log_row) ->
                           String.equal e.S.l_name "alpha"
                           && String.length e.S.l_origin_detail > 0)
                         file_rows)
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               cleanup_branch store ~branch_id:dst_id))

(* ---- 2. unchanged body skips with zero ops ---- *)

let test_unchanged_skips () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               let op = io "alpha" (body_json (lit 3)) in
               (match
                  P.ingest store ~target_id:dst_id ~ops:[ op ] ~base_gen:0L
                    ~force:false ~kernel:[] ~author:"filer" ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; _ } ->
                    Alcotest.(check (list string)) "first lands" [ "alpha" ]
                      i_landed
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               let g0 = branch_gen store ~branch_id:dst_id in
               (match
                  P.ingest store ~target_id:dst_id ~ops:[ op ] ~base_gen:0L
                    ~force:false ~kernel:[] ~author:"filer" ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; i_skipped } ->
                    Alcotest.(check int) "unchanged lands nothing" 0
                      (List.length i_landed);
                    Alcotest.(check bool) "reason is unchanged" true
                      (skipped_reasons
                         (P.Ingest_ok { i_landed = []; i_skipped })
                       = List.map
                           (fun s -> s.P.sk_reason)
                           i_skipped
                       && List.exists
                            (fun r -> contains r "unchanged on target")
                            (skipped_reasons
                               (P.Ingest_ok
                                  { i_landed = []; i_skipped })));
                    Alcotest.(check bool) "zero ops: gen unmoved" true
                      (Int64.equal g0 (branch_gen store ~branch_id:dst_id))
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               cleanup_branch store ~branch_id:dst_id))

(* ---- 3. diverged target: skip unless 'force; before-image fires ---- *)

let test_diverged_and_force () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               store.branch_id <- dst_id;
               S.persist_function store "alpha" (body_json (lit 9)) "tester";
               let ch_before =
                 code_history_count store ~branch_id:dst_id ~name:"alpha" in
               (* base_hash does NOT match the target head -> diverged *)
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io_h "alpha" (body_json (lit 3)) "deadbeef" ]
                    ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; i_skipped } ->
                    Alcotest.(check int) "diverged: nothing lands" 0
                      (List.length i_landed);
                    Alcotest.(check bool) "reason names diverged-target" true
                      (List.exists
                         (fun r -> contains r "diverged-target")
                         (skipped_reasons
                            (P.Ingest_ok { i_landed; i_skipped })));
                    Alcotest.(check bool) "target body untouched" true
                      (body_expr store ~branch_id:dst_id ~name:"alpha"
                       = Some (lit 9))
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               (* 'force lands; exactly one code_history before-image *)
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io_h "alpha" (body_json (lit 3)) "deadbeef" ]
                    ~base_gen:0L ~force:true ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; _ } ->
                    Alcotest.(check (list string)) "force lands" [ "alpha" ]
                      i_landed;
                    Alcotest.(check bool) "body replaced" true
                      (body_expr store ~branch_id:dst_id ~name:"alpha"
                       = Some (lit 3));
                    Alcotest.(check int) "one before-image fired" 1
                      (Int64.to_int
                         (Int64.sub
                            (code_history_count store ~branch_id:dst_id
                               ~name:"alpha")
                            ch_before))
                | r -> Alcotest.fail ("force ingest failed: " ^ ingest_str r));
               cleanup_branch store ~branch_id:dst_id))

(* ---- 4. base_hash = target head: clean redefine, no force ---- *)

let test_clean_redefine () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io "alpha" (body_json (lit 3)) ]
                    ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; _ } ->
                    Alcotest.(check (list string)) "create first" [ "alpha" ]
                      i_landed
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               (match pin_hash store ~branch_id:dst_id ~name:"alpha" with
                | None -> Alcotest.fail "pin hash missing"
                | Some h ->
                    (match
                       P.ingest store ~target_id:dst_id
                         ~ops:[ io_h "alpha" (body_json (lit 5)) h ]
                         ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                         ~file_name:"todos.pp"
                     with
                     | P.Ingest_ok { i_landed; i_skipped } ->
                         Alcotest.(check (list string)) "clean redefine lands"
                           [ "alpha" ] i_landed;
                         Alcotest.(check int) "no skips" 0
                           (List.length i_skipped);
                         Alcotest.(check bool) "body is lit 5" true
                           (body_expr store ~branch_id:dst_id ~name:"alpha"
                            = Some (lit 5))
                     | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r)));
               cleanup_branch store ~branch_id:dst_id))

(* ---- 5. unbind deletes the pin; tombstone origin=file ---- *)

let test_unbind () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io "alpha" (body_json (lit 3)) ]
                    ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; _ } ->
                    Alcotest.(check (list string)) "create first" [ "alpha" ]
                      i_landed
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               (match pin_hash store ~branch_id:dst_id ~name:"alpha" with
                | None -> Alcotest.fail "pin hash missing"
                | Some h ->
                    (match
                       P.ingest store ~target_id:dst_id
                         ~ops:[ iu "alpha" h ]
                         ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                         ~file_name:"todos.pp"
                     with
                     | P.Ingest_ok { i_landed; i_skipped } ->
                         Alcotest.(check (list string)) "unbind lands" [ "alpha" ]
                           i_landed;
                         Alcotest.(check int) "no skips" 0
                           (List.length i_skipped);
                         Alcotest.(check bool) "pin gone" true
                           (own_body store ~branch_id:dst_id ~name:"alpha"
                            = None);
                         let log =
                           S.checkpoint_log store ~branch_id:dst_id
                             ~since_gen:None in
                         Alcotest.(check bool)
                           "tombstone logged origin=file" true
                           (List.exists
                              (fun (e : S.cp_log_row) ->
                                String.equal e.S.l_op "delete"
                                && String.equal e.S.l_origin "file")
                              log)
                     | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r)));
               cleanup_branch store ~branch_id:dst_id))

(* ---- 6. closure: holes refuse whole; batch dep auto-includes ---- *)

let test_holes_and_auto () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io "caller" (body_json (app "g" [ lit 1 ])) ]
                    ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_holes holes ->
                    Alcotest.(check bool) "caller holes on g" true
                      (match holes with
                       | [ (n, missing) ] ->
                           String.equal n "caller" && missing = [ "g" ]
                       | _ -> false);
                    Alcotest.(check bool) "refused ingest lands NOTHING" true
                      (own_body store ~branch_id:dst_id ~name:"caller" = None)
                | r ->
                    Alcotest.fail
                      ("expected holes, got " ^ ingest_str r));
               (* g in the same batch: auto-included, both land *)
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[
                      io "caller" (body_json (app "g" [ lit 1 ]));
                      io "g" (body_json (lit 2));
                    ]
                    ~base_gen:0L ~force:false ~kernel:[] ~author:"filer"
                    ~file_name:"todos.pp"
                with
                | P.Ingest_ok { i_landed; _ } ->
                    Alcotest.(check bool) "caller + g landed" true
                      (List.sort compare i_landed = [ "caller"; "g" ]);
                    Alcotest.(check bool) "bodies verbatim" true
                      (body_expr store ~branch_id:dst_id ~name:"caller"
                       = Some (app "g" [ lit 1 ])
                       && body_expr store ~branch_id:dst_id ~name:"g"
                          = Some (lit 2))
                | r -> Alcotest.fail ("ingest failed: " ^ ingest_str r));
               cleanup_branch store ~branch_id:dst_id))

(* ---- 7. base-gen ahead of the branch refuses ---- *)

let test_base_ahead () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let sfx = string_of_int (Random.int 1_000_000) in
          (match fork_one store ~sfx with
           | None -> Alcotest.fail "fork failed"
           | Some dst_id ->
               (match
                  P.ingest store ~target_id:dst_id
                    ~ops:[ io "alpha" (body_json (lit 3)) ]
                    ~base_gen:999_999_999L ~force:false ~kernel:[]
                    ~author:"filer" ~file_name:"todos.pp"
                with
                | P.Ingest_refused _ -> ()
                | r ->
                    Alcotest.fail
                      ("expected refusal, got " ^ ingest_str r));
               cleanup_branch store ~branch_id:dst_id))

let () =
  Random.self_init ();
  Alcotest.run "file-bridge"
    [
      ( "db"
      , [
          Alcotest.test_case "define lands with origin=file" `Quick
            test_ingest_define_lands;
          Alcotest.test_case "unchanged body skips with zero ops" `Quick
            test_unchanged_skips;
          Alcotest.test_case "diverged target: skip unless 'force" `Quick
            test_diverged_and_force;
          Alcotest.test_case "base_hash = head: clean redefine" `Quick
            test_clean_redefine;
          Alcotest.test_case "unbind deletes with origin=file" `Quick
            test_unbind;
          Alcotest.test_case "closure: holes refuse; batch dep auto-includes"
            `Quick test_holes_and_auto;
          Alcotest.test_case "base-gen ahead refuses" `Quick test_base_ahead;
        ] )
    ]
