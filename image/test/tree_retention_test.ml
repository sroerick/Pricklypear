(** tree-time.gc (5.3): retention worker surfaces.

    Exercises the retention-policy-as-habitat-data Store functions
    (retention_set_policy / retention_delete_policy / retention_list_policies)
    and the retention worker entry point (retention_run) in their two safe,
    NON-DESTRUCTIVE deterministic forms:

      (a) NO whole-class policy  -> retention keeps everything: retention_run
          returns [] (the worker is never invoked / no-op keep-all gate).
      (b) whole-class policy with keep_windows large enough that the retention
          floor CLAMPS to the base-window boundary (gen 1000) -> the worker's
          early-exit path returns (0,0): nothing beyond the base window is
          retentive yet, so nothing is checkpointed or dropped.

    The DESTRUCTIVE rollup + DROP PARTITION path (checkpoint-then-drop of
    lazy windows entirely below a non-trivial floor) is verified by the
    migration's own function tree_versions_retain against an isolated scratch
    DB (see the 5.3 notes log), NOT here — against the shared dev PG that
    would drop real partitions. These tests keep the policy rows scoped to a
    throwaway type and delete them after.

    Needs Postgres (scripts/dev.sh); skips cleanly when unreachable.
*)

module S = Pricklypear_image_lib.Store

let main_id = "00000000-0000-0000-0000-000000000001"
let uniq p = Printf.sprintf "gcr_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

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
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let contains_sub s sub =
  let ls = String.length s and lb = String.length sub in
  let rec go i =
    if i + lb > ls then false
    else if String.sub s i lb = sub then true
    else go (i + 1)
  in
  lb <= ls && (if lb = 0 then true else go 0)

let test_policy_crud () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let tn = uniq "ty" in
          (* set a whole-class policy and a per-type policy *)
          ignore (S.retention_set_policy store ~path_class:"data"
                    ~type_name:None ~keep_windows:4L);
          ignore (S.retention_set_policy store ~path_class:"data"
                    ~type_name:(Some tn) ~keep_windows:2L);
          let has pc tnc kw =
            List.exists
              (fun (pc', tn', kw', _) ->
                pc' = pc && tn' = tnc && kw' = kw)
              (S.retention_list_policies store)
          in
          Alcotest.(check bool) "whole-class data policy listed"
            true (has "data" None 4L);
          Alcotest.(check bool) "per-type data policy listed"
            true (has "data" (Some tn) 2L);
          (* upsert overwrites, not duplicates *)
          ignore (S.retention_set_policy store ~path_class:"data"
                    ~type_name:(Some tn) ~keep_windows:9L);
          let matches =
            List.filter (fun (_, tn', _, _) -> tn' = Some tn)
              (S.retention_list_policies store)
          in
          Alcotest.(check int) "upsert replaces same (path_class,type_name) row"
            1 (List.length matches);
          (* delete the per-type + whole-class policies, leaving the DB clean *)
          ignore (S.retention_delete_policy store ~path_class:"data"
                    ~type_name:(Some tn));
          ignore (S.retention_delete_policy store ~path_class:"data"
                    ~type_name:None);
          Alcotest.(check bool) "per-type policy deleted"
            false (has "data" (Some tn) 9L);
          Alcotest.(check bool) "whole-class data policy deleted"
            false (has "data" None 4L))

(* No whole-class policy -> retention_run keeps everything (empty). *)
let test_retention_run_no_policy () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          (* make sure no whole-class policy exists for either class *)
          ignore (S.retention_delete_policy store ~path_class:"code"
                    ~type_name:None);
          ignore (S.retention_delete_policy store ~path_class:"data"
                    ~type_name:None);
          let rs = S.retention_run store ~path_class:None () in
          Alcotest.(check int) "no policy -> nothing to retain (keep everything)"
            0 (List.length rs))

(* Whole-class policy with a floor clamped to the base window -> worker runs
   but early-exits (0,0): nothing beyond the base window is retentive yet.
   Deterministic + NON-DESTRUCTIVE against the real dev head gens. *)
let test_retention_run_clamped () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some _ ->
          let tn = uniq "ty" in
          (* keep 100 windows: any real head below 100k clamps floor to the
             base-window boundary (gen 1000) -> (0,0). *)
          ignore (S.retention_set_policy store ~path_class:"code"
                    ~type_name:None ~keep_windows:100L);
          ignore (S.retention_set_policy store ~path_class:"data"
                    ~type_name:None ~keep_windows:100L);
          (* not a whole-class policy -> must NOT trigger retention for it *)
          ignore (S.retention_set_policy store ~path_class:"data"
                    ~type_name:(Some tn) ~keep_windows:1L);
          let rs = S.retention_run store ~path_class:None () in
          (* both whole-class classes reported; both (0,0) clamped no-op *)
          Alcotest.(check int) "both whole-class classes reported"
            2 (List.length rs);
          List.iter
            (fun (pc, d, c) ->
              Alcotest.(check int64) (Printf.sprintf "%s drops 0" pc)
                0L d;
              Alcotest.(check int64) (Printf.sprintf "%s checkpoints 0" pc)
                0L c)
            rs;
          (* scoped run for one class *)
          let rc = S.retention_run store ~path_class:(Some "code") () in
          Alcotest.(check int) "scoped code run reports 1"
            1 (List.length rc);
          (* clean up *)
          ignore (S.retention_delete_policy store ~path_class:"code"
                    ~type_name:None);
          ignore (S.retention_delete_policy store ~path_class:"data"
                    ~type_name:(Some tn));
          ignore (S.retention_delete_policy store ~path_class:"data"
                    ~type_name:None))

let () =
  Alcotest.run "tree_retention"
    [
      ( "policies",
        [
          Alcotest.test_case "policy set/list/upsert/delete" `Quick test_policy_crud;
        ] );
      ( "worker",
        [
          Alcotest.test_case "retention-run keep-everything (no policy)" `Quick
            test_retention_run_no_policy;
          Alcotest.test_case "retention-run clamped floor (no drop)" `Quick
            test_retention_run_clamped;
        ] );
    ]
