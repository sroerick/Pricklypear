(** RLS pp.user floor for the datum prims + serialization-failure retry
    (i-1788670872-3).

    Before this fix the crud tree/edge/data-lens prims and the http_tree
    face read/wrote tree_nodes on the bare pool conn — where
    set_config('pp.user', …) is never armed, so migration 013's
    datum_user_isolation policy (and 023's extension) passed EVERYTHING:
    any session could read or write another identity's data.u.* datums
    through tree/get, tree/upsert, link, while the CRUD get/find-* prims
    (i-rls-nontx) confined. with_tree_tx now routes each prim through a
    READ COMMITTED transaction whose set_config('pp.user', …, true) arms
    the floor (fiber-bound via Request_context / Store_db.with_pp_user).

    Pinned here, against a real Postgres (scripts/dev.sh; skips when
    unreachable):
      - the floor: bob's tree/get of an alice datum is VNil; alice's is
        the node; an UNBOUND fiber (set_local_pp_user defaults to
        "anonymous") also sees VNil.
      - the WITH CHECK half: bob's tree/upsert onto alice's datum path
        raises (policy rejection), and the row is untouched; bob can
        still upsert his own datum path.
      - the retry: with_transaction re-runs the body on a REAL SQLSTATE
        40001 (deterministic SSI recipe: a SERIALIZABLE tx's snapshot
        read, a concurrent committed UPDATE by a second connection, then
        the first tx's UPDATE), and the retried attempt commits.
      - the classifier: is_serialization_failure matches 40001 only.

    READ COMMITTED for the tree prims is deliberate: their pre-fix
    behavior was per-statement autocommit (= READ COMMITTED), so the tx
    adds the floor + atomicity without a new SSI abort class.
*)

module S = Pricklypear_image_lib.Store
module C = Pricklypear_image_lib.Crud
module T = Pricklypear_image_lib.Tree_store
module Ast = Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

(** Two independent connections (needed for the SSI conflict recipe). *)
let with_two_stores f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let a = S.make ~cfg ~branch_id:main_id () in
      S.connect a;
      let b = S.make ~cfg ~branch_id:main_id () in
      S.connect b;
      match S.conn_of a, S.conn_of b with
      | Some _, Some _ -> f a b
      | _ ->
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!")

let probe_uuid tag =
  Printf.sprintf "0197a2b3-c0de-7000-8000-%012d"
    (Hashtbl.hash tag land 0xFFFF_FFFF)

let probe_path user uuid =
  T.datum_path ~user ~type_name:"rls-probe" ~id:uuid ()

let is_vnil = function Ast.VNil -> true | _ -> false

(* ---- the floor: reads confine; writes reject cross-identity ---- *)

let test_floor () =
  with_two_stores
  @@ fun store _other ->
  let path = probe_path "alice-floor" (probe_uuid "floor") in
  let bare = Option.get (S.conn_of store) in
  (* Seed alice's datum on the BARE conn — pp.user unset, policy open:
     exactly the hole the fix closes for the prim surface. *)
  Alcotest.(check (result Alcotest.string Alcotest.string))
    "seed alice datum (bare conn, floor off)"
    (T.upsert_datum bare ~path ~type_name:"rls-probe"
       ~body_json:"{\"note\":\"alice-secret\"}" ~author:"alice-floor"
       ~rank:None)
    (Ok path);
  (* bob: RLS floor armed inside with_tree_tx's tx → hidden. *)
  Alcotest.(check bool) "bob's tree/get is VNil" true
    (is_vnil (S.with_pp_user "bob-floor" (fun () -> C.tree_get store ~path ())));
  (* unbound fiber: set_local_pp_user falls back to "anonymous" → hidden. *)
  Alcotest.(check bool) "unbound fiber's tree/get is VNil" true
    (is_vnil (C.tree_get store ~path ()));
  (* alice: sees her own datum, valid node JSON with her body field. *)
  (match S.with_pp_user "alice-floor" (fun () -> C.tree_get store ~path ()) with
   | Ast.VStr s -> (
       match Yojson.Safe.from_string s with
       | `Assoc kv -> (
           match List.assoc_opt "body" kv with
           | Some (`Assoc _) -> ()
           | _ -> Alcotest.fail "alice's node JSON lacks the body object")
       | _ -> Alcotest.fail "alice's node JSON is not an object")
   | _ -> Alcotest.fail "alice's tree/get should return the node JSON");
  (* WITH CHECK: bob's tree/upsert onto alice's path raises (policy
     rejection), and the stored body is untouched. *)
  let raised =
    try
      ignore
        (S.with_pp_user "bob-floor" (fun () ->
             C.tree_upsert store ~path ~kind:"datum" ~type_name:"rls-probe"
               ~body_json:"{\"note\":\"bob-was-here\"}" ~author:"bob-floor"));
      false
    with _ -> true
  in
  Alcotest.(check bool) "bob's cross-identity tree/upsert raises" true raised;
  (match S.with_pp_user "alice-floor" (fun () -> C.tree_get store ~path ()) with
   | Ast.VStr s ->
       Alcotest.(check bool) "row body untouched by the rejected write" true
         (not (String.contains s 'z'))
   | _ -> Alcotest.fail "alice's datum vanished");
  (* bob CAN upsert his own path (the floor permits the owner). *)
  let bpath = probe_path "bob-floor" (probe_uuid "floor-bob") in
  (match
     S.with_pp_user "bob-floor" (fun () ->
         C.tree_upsert store ~path:bpath ~kind:"datum" ~type_name:"rls-probe"
           ~body_json:"{\"note\":\"bob-own\"}" ~author:"bob-floor")
   with
   | Ast.VStr p when p = bpath -> ()
   | _ -> Alcotest.fail "bob's own-datum upsert should succeed");
  (* cleanup *)
  let _ = T.delete_path bare ~path ~recursive:false in
  let _ = T.delete_path bare ~path:bpath ~recursive:false in
  ()

(* ---- retry on a REAL serialization failure (SQLSTATE 40001) ---- *)

let test_retry () =
  with_two_stores
  @@ fun store_a store_b ->
  let path = probe_path "alice-floor" (probe_uuid "retry") in
  let bare = Option.get (S.conn_of store_a) in
  let _ =
    T.upsert_datum bare ~path ~type_name:"rls-probe"
      ~body_json:"{\"note\":\"retry-seed\"}" ~author:"alice-floor" ~rank:None
  in
  let attempts = ref 0 in
  let b_fired = ref false in
  (* The whole SSI dance runs as alice so the RLS floor never muddies it. *)
  S.with_pp_user "alice-floor" (fun () ->
      S.with_transaction store_a (fun conn_a ->
          (* 1. snapshot read on the row (takes an SIREAD lock). *)
          (match conn_a with
           | Some c ->
               let _ =
                 S.exec c ~params:[ Pgx.Value.of_string path ]
                   "SELECT author FROM tree_nodes WHERE path = $1::text"
               in
               ()
           | None -> Alcotest.fail "no conn on store_a");
          incr attempts;
          (* 2. a SECOND connection updates the row and commits — while
             tx A holds its older snapshot. *)
          if not !b_fired then begin
            b_fired := true;
            S.with_transaction store_b (fun conn_b ->
                match conn_b with
                | Some c ->
                    let _ =
                      S.exec c ~params:[ Pgx.Value.of_string path ]
                        "UPDATE tree_nodes SET updated_at = now() \
                         WHERE path = $1::text"
                    in
                    ()
                | None -> Alcotest.fail "no conn on store_b")
          end;
          (* 3. A's UPDATE of the row B modified → SQLSTATE 40001 on the
             FIRST attempt; the retry (new snapshot, B already done)
             succeeds and commits. *)
          match conn_a with
          | Some c ->
              let _ =
                S.exec c ~params:[ Pgx.Value.of_string path ]
                  "UPDATE tree_nodes SET author = 'retry-probe' \
                   WHERE path = $1::text"
              in
              ()
          | None -> Alcotest.fail "no conn on store_a (retry)"));
  Alcotest.(check int) "body ran twice (one 40001 retry)" 2 !attempts;
  Alcotest.(check bool) "the conflicting B write fired once" true !b_fired;
  let rows =
    S.exec bare ~params:[ Pgx.Value.of_string path ]
      "SELECT author FROM tree_nodes WHERE path = $1::text"
  in
  (match rows with
   | [ [ a ] ] -> (
       match Pgx.Value.to_string a with
       | Some "retry-probe" -> ()
       | other ->
           Alcotest.fail
             ("retry commit lost; author = "
              ^ (Option.value other ~default:"<null>")))
   | _ -> Alcotest.fail "probe row missing after retry");
  let _ = T.delete_path bare ~path ~recursive:false in
  ()

(* ---- the classifier (no DB) ---- *)

let test_classifier () =
  let err code =
    let fields =
      { Pgx.Error_response.code; severity = "ERROR"; message = "x"; custom = [] }
    in
    Pgx.PostgreSQL_Error ("msg", fields)
  in
  Alcotest.(check bool) "40001 is a serialization failure" true
    (S.is_serialization_failure (err "40001"));
  Alcotest.(check bool) "unique violation is not" false
    (S.is_serialization_failure (err "23505"));
  Alcotest.(check bool) "RLS violation is not" false
    (S.is_serialization_failure (err "42501"));
  Alcotest.(check bool) "plain Failure is not" false
    (S.is_serialization_failure (Failure "boom"))

let () =
  Alcotest.run "rls-floor"
    [
      ( "floor"
      , [ Alcotest.test_case "reads confine + writes reject" `Quick test_floor
        ] )
    ; ( "retry"
      , [ Alcotest.test_case "real 40001 retried and committed" `Quick test_retry
        ] )
    ; ( "classifier"
      , [ Alcotest.test_case "40001 only" `Quick test_classifier ] )
    ]
