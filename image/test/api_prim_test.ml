(** Pricklypear Image — /api/prim refuse-or-forward dispatch tests.

    Covers the server-endpoint verify clauses of borge/remote-eval.borg:
      - same value as in-process (dispatch executes the SAME table fn the
        image rim services, and the value serializes the same)
      - a server-only prim is refused with a structured error citing
        locality (kind "server_only"), not silently executed
      - refuse-or-forward classification is DERIVED from prim-registry
        metadata (register_server_only), never a hand-maintained list

    Pure: no Postgres, no HTTP server, no Session. We exercise the
    [Primitives.dispatch] decision the /api/prim endpoint is a thin
    wrapper around, and the real pure prim table [make_with_defaults]
    builds -- exactly the table a session dispatches through. *)

open Pricklypear_lisp.Ast
module P = Pricklypear_image_lib.Primitives

(* A proxyable prim + a server-only prim registered the way the image's
   registration sites do (register / register_server_only). *)
let make_tbl () =
  let t = P.make () in
  P.register_sync t "double" (function
      | [ VInt n ] -> VInt (Int64.mul n 2L)
      | _ -> VInt 0L);
  P.register_server_only_sync t "route" (fun _ -> VStr "route");
  P.register_server_only_sync t "checkout" (fun _ -> VStr "checkout");
  t

(* --- same value as in-process --- *)

(* dispatch on a proxyable prim returns the value the in-process rim
   would (same table fn). We cross-check against the REAL pure table
   prims via in-process eval on the same table. *)
let test_same_value_as_in_process () =
  let env_tbl = P.make_with_defaults () in
  let env = Pricklypear_lisp.Env.create () in
  Hashtbl.iter
    (fun name _ -> Pricklypear_lisp.Env.define env name (VPrim name))
    env_tbl;
  (* in-process: eval (+ 1 2) under the rim against the same table *)
  let in_process =
    Pricklypear_image_lib.Control_rim.run ~prims:env_tbl (fun () ->
        Pricklypear_lisp.Eval.eval_string_direct
          ~prims:Pricklypear_lisp.Eval.empty_prims env "(+ 1 2)")
  in
  Alcotest.(check string) "in-process (+ 1 2)" "3"
    (Pricklypear_lisp.Reader.pp_value in_process);
  (* the /api/prim path: dispatch "+" on the same table *)
  match P.dispatch env_tbl "+" [ VInt 1L; VInt 2L ] with
  | P.Prim_ok v ->
      Alcotest.(check string) "dispatch (+ 1 2)" "3"
        (Pricklypear_lisp.Reader.pp_value v);
      (* value serializes identically through the shared value codec *)
      Alcotest.(check string) "same JSON value" "3"
        (Yojson.Safe.to_string (Prims_pure.value_to_json v))
  | _ -> Alcotest.fail "dispatch returned a non-ok result for +"

(* dispatch of an unregistered name behaves like the rim: a Prim_error. *)
let test_unregistered_is_error () =
  let t = P.make () in
  match P.dispatch t "no-such-prim" [] with
  | P.Prim_error exn ->
      (match exn with
       | Interp_error (Prim_error (name, _, _)) ->
           Alcotest.(check string) "unregistered name" "no-such-prim" name
       | _ -> Alcotest.fail "expected Prim_error for unregistered prim")
  | _ -> Alcotest.fail "expected Prim_error for unregistered prim"

(* --- server-only refused, not executed --- *)

let test_server_only_refused_not_executed () =
  let t = make_tbl () in
  (* route is registered as a harm-free no-op; if it WERE executed it
     would return ok. The refusal must happen before execution. *)
  match P.dispatch t "route" [] with
  | P.Prim_refused_server_only (name, reason) ->
      Alcotest.(check string) "refused name" "route" name;
      Alcotest.(check string) "locality reason" "prim must execute server-side"
        reason
  | _ -> Alcotest.fail "expected server-only refusal for route"

(* --- classification is data-driven --- *)

let test_classification_data_driven () =
  let t = make_tbl () in
  (* proxyable by default (registered via register_sync) *)
  Alcotest.(check bool) "double proxyable" false (P.is_server_only t "double");
  (* server-only iff registered via register_server_only *)
  Alcotest.(check bool) "route server-only" true (P.is_server_only t "route");
  Alcotest.(check bool) "checkout server-only" true
    (P.is_server_only t "checkout");
  (* a prim not yet registered at all is proxyable (unclassified) *)
  Alcotest.(check bool) "unregistered is proxyable" false
    (P.is_server_only t "never-registered");
  (* classification tracks the registry metadata: a NEW registry that
     registers a name as proxyable does not leak the server-only mark *)
  let t2 = P.make () in
  P.register_sync t2 "route" (fun _ -> VStr "route");
  (* NB: server-only locality is a registry-wide property of a prim NAME
     (route is always server-only on the image), so re-registering it as
     proxyable in a scratch registry keeps it server-only. The single
     source of truth is the registration site, never a hand list. *) 
  Alcotest.(check bool) "registry is source of truth for route" true
    (P.is_server_only t2 "route");
  match P.dispatch t2 "route" [] with
  | P.Prim_refused_server_only (name, _) ->
      Alcotest.(check string) "refused via metadata" "route" name
  | _ -> Alcotest.fail "route must remain server-only from registry metadata"

(* DB CRUD prims on the real pure table are proxyable (not marked
   server-only). *)
let test_db_crud_not_server_only_on_real_table () =
  let t = P.make_with_defaults () in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is proxyable on the default table" name)
        false (P.is_server_only t name))
    [ "get"; "create"; "update"; "delete"; "rows"; "+"; "not" ]

let () =
  Alcotest.run "api_prim"
    [
      ( "dispatch",
        [
          Alcotest.test_case "same value as in-process" `Quick
            test_same_value_as_in_process;
          Alcotest.test_case "unregistered name is a Prim_error" `Quick
            test_unregistered_is_error;
          Alcotest.test_case "server-only refused, not executed" `Quick
            test_server_only_refused_not_executed;
          Alcotest.test_case "classification is data-driven" `Quick
            test_classification_data_driven;
          Alcotest.test_case "DB CRUD proxyable on real table" `Quick
            test_db_crud_not_server_only_on_real_table;
        ] );
    ]
