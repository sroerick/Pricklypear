(** image/test/data_grants_test.ml — borge/branching.borg sharing-grants,
    rollout step 2: data_grants + the read disjunct beside spaces.

    Covers:
      - a grant on one row's path makes exactly that row visible to the
        grantee; the sibling row stays invisible. The view
        (tree_data_rows), the RLS floor under an unprivileged NOBYPASSRLS
        role, and Crud.visible_read_sql agree.
      - grantee kinds: a username, 'authenticated' (signed-in only) and
        'anonymous' (everyone).
      - a grant without the read right confers no visibility.
      - a grant is read-only at the floor: the grantee still cannot write
        into the granted path (WITH CHECK unchanged), and Crud.visible_sql,
        which gates the write paths, does not include grants.
      - attenuation: a re-share may only narrow (sub-prefix, subset of
        rights) and needs the parent's reshare right; revoking a grant
        revokes its descendants and hides the row on the next read.

    Not covered here (later rollout steps / leak surfaces): cached subtree
    reads keyed on a grant generation, backlinks and search, agent owner
    defaults.

    Needs scripts/dev.sh Postgres; fails (UNVERIFIED) when unreachable. *)

module S = Pricklypear_image_lib.Store
module TS = Pricklypear_image_lib.Tree_store
module Db = Pricklypear_image_lib.Store_db
module Crud = Pricklypear_image_lib.Crud

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
          Alcotest.fail "UNVERIFIED: no postgres (set PP_DB_* / run scripts/dev.sh)"
      | Some _ -> f store)

let s = Pgx.Value.of_string

let exec conn sql =
  String.split_on_char ';' sql
  |> List.filter (fun stmt -> String.trim stmt <> "")
  |> List.iter (fun stmt -> ignore (Db.exec conn stmt))

let q_count conn ?(params = []) sql : int =
  match Db.exec conn ~params sql with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some n -> int_of_string n
      | None -> Alcotest.fail ("count query returned NULL: " ^ sql))
  | _ -> Alcotest.fail ("count query shape: " ^ sql)

let q_string conn ?(params = []) sql : string =
  match Db.exec conn ~params sql with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some v -> v
      | None -> Alcotest.fail ("scalar query returned NULL: " ^ sql))
  | _ -> Alcotest.fail ("scalar query shape: " ^ sql)

let set_pp_user conn who =
  ignore
    (Db.exec conn ~params:[ s who ] "SELECT set_config('pp.user', $1, false)")

let insert conn ~type_name ~author =
  match TS.insert_datum conn ~type_name ~fields_json:"{\"n\":1}" ~author with
  | Ok id -> id
  | Error e -> Alcotest.fail ("insert_datum failed: " ^ e)

let path_of conn ~id =
  q_string conn ~params:[ s id ]
    "SELECT path::text FROM tree_nodes WHERE kind='datum' \
     AND tree_datum_id(path) = $1::uuid"

(** Two private rows of one fresh type, authored by alice: [row] is the one
    a test grants, [sib] is its sibling. Grants by this run are removed by
    the type's path prefix. *)
let with_rows f =
  with_store (fun store ->
      let conn = Option.get (S.conn_of store) in
      let ty = Printf.sprintf "grt%06d" (Random.int 1_000_000) in
      Fun.protect
        ~finally:(fun () ->
          set_pp_user conn "";
          exec conn
            (Printf.sprintf
               "DELETE FROM tree_nodes WHERE kind='datum' AND type_name='%s'; \
                DELETE FROM data_rows WHERE type_name='%s'; \
                DELETE FROM data_grants WHERE granted_by = 'grt-test-%s'"
               ty ty ty))
        (fun () ->
          let row = insert conn ~type_name:ty ~author:"alice" in
          let sib = insert conn ~type_name:ty ~author:"alice" in
          f conn ~ty ~row:(path_of conn ~id:row) ~sib:(path_of conn ~id:sib)))

(** Insert a grant; returns its id. [parent] attenuates an existing grant. *)
let grant conn ~ty ?parent ~prefix ~grantee ~rights () : string =
  set_pp_user conn "";
  q_string conn
    ~params:
      [ s prefix; s grantee; s ("{" ^ String.concat "," rights ^ "}");
        s ("grt-test-" ^ ty);
        (match parent with Some p -> s p | None -> Pgx.Value.null) ]
    "INSERT INTO data_grants (prefix, grantee, rights, granted_by, parent) \
     VALUES ($1::ltree, $2, $3::text[], $4, $5::uuid) RETURNING id::text"

let revoke conn ~id =
  set_pp_user conn "";
  ignore
    (Db.exec conn ~params:[ s id ]
       "UPDATE data_grants SET revoked_at = now() WHERE id = $1::uuid")

(* --- the three mirrors, each counting the rows of [ty] [who] can read --- *)

let view_count conn ~ty who =
  set_pp_user conn who;
  let n =
    q_count conn ~params:[ s ty ]
      "SELECT COUNT(*)::int FROM tree_data_rows WHERE type_name = $1"
  in
  set_pp_user conn "";
  n

let view_sees conn ~path who =
  set_pp_user conn who;
  let n =
    q_count conn ~params:[ s path ]
      "SELECT COUNT(*)::int FROM tree_data_rows WHERE path = $1::ltree"
  in
  set_pp_user conn "";
  n

(* Crud's read-side confinement string, applied as the prim layer applies
   it ($1 = type, $2 = caller), with the view unconfined (pp.user unset). *)
let crud_read_count conn ~ty who =
  set_pp_user conn "";
  q_count conn ~params:[ s ty; s who ]
    ("SELECT COUNT(*)::int FROM tree_data_rows WHERE type_name = $1 AND "
    ^ Crud.visible_read_sql ~user_param:2)

let crud_write_count conn ~ty who =
  set_pp_user conn "";
  q_count conn ~params:[ s ty; s who ]
    ("SELECT COUNT(*)::int FROM tree_data_rows WHERE type_name = $1 AND "
    ^ Crud.visible_sql ~user_param:2)

(** Run [f] as the unprivileged NOBYPASSRLS probe role, as [who]. *)
let as_probe conn f =
  if q_count conn
       "SELECT COUNT(*)::int FROM pg_roles WHERE rolname = 'pp_grants_probe'" > 0
  then exec conn "DROP OWNED BY pp_grants_probe";
  exec conn "DROP ROLE IF EXISTS pp_grants_probe";
  exec conn
    "CREATE ROLE pp_grants_probe NOLOGIN NOSUPERUSER NOBYPASSRLS NOINHERIT";
  exec conn "GRANT SELECT, INSERT ON tree_nodes TO pp_grants_probe";
  exec conn
    "GRANT SELECT ON data_space_members, data_spaces, data_grants \
     TO pp_grants_probe";
  Fun.protect
    ~finally:(fun () ->
      exec conn "RESET ROLE";
      set_pp_user conn "";
      exec conn "DROP OWNED BY pp_grants_probe";
      exec conn "DROP ROLE IF EXISTS pp_grants_probe")
    (fun () ->
      exec conn "SET ROLE pp_grants_probe";
      f ())

let rls_sees conn ~path who =
  set_pp_user conn who;
  q_count conn ~params:[ s path ]
    "SELECT COUNT(*)::int FROM tree_nodes WHERE kind='datum' AND path = $1::ltree"

(* ---------------- tests ---------------- *)

let test_baseline_private () =
  with_rows (fun conn ~ty ~row:_ ~sib:_ ->
      Alcotest.(check int) "alice sees both" 2 (view_count conn ~ty "alice");
      Alcotest.(check int) "bob sees none without a grant" 0
        (view_count conn ~ty "bob");
      Alcotest.(check int) "crud read: bob none" 0
        (crud_read_count conn ~ty "bob"))

let test_row_grant_exact () =
  with_rows (fun conn ~ty ~row ~sib ->
      ignore (grant conn ~ty ~prefix:row ~grantee:"bob" ~rights:[ "read" ] ());
      Alcotest.(check int) "view: bob sees the granted row" 1
        (view_sees conn ~path:row "bob");
      Alcotest.(check int) "view: bob does not see the sibling" 0
        (view_sees conn ~path:sib "bob");
      Alcotest.(check int) "view: bob sees exactly one row of the type" 1
        (view_count conn ~ty "bob");
      Alcotest.(check int) "view: carol still sees none" 0
        (view_count conn ~ty "carol");
      Alcotest.(check int) "crud read mirror: bob exactly one" 1
        (crud_read_count conn ~ty "bob");
      Alcotest.(check int) "crud read mirror: carol none" 0
        (crud_read_count conn ~ty "carol");
      as_probe conn (fun () ->
          Alcotest.(check int) "RLS: bob reads the granted row" 1
            (rls_sees conn ~path:row "bob");
          Alcotest.(check int) "RLS: bob cannot read the sibling" 0
            (rls_sees conn ~path:sib "bob");
          Alcotest.(check int) "RLS: carol cannot read the granted row" 0
            (rls_sees conn ~path:row "carol")))

let test_grant_is_read_only () =
  with_rows (fun conn ~ty ~row ~sib:_ ->
      let parent = String.sub row 0 (String.rindex row '.') in
      ignore
        (grant conn ~ty ~prefix:parent ~grantee:"bob" ~rights:[ "read" ] ());
      Alcotest.(check int) "crud write confinement ignores grants" 0
        (crud_write_count conn ~ty "bob");
      as_probe conn (fun () ->
          set_pp_user conn "bob";
          let blocked =
            try
              exec conn
                (Printf.sprintf
                   "INSERT INTO tree_nodes \
                    (path, kind, type_name, payload, hash, author, updated_at) \
                    VALUES (('%s.' || replace(gen_random_uuid()::text,'-',''))::ltree, \
                            'datum', '%s', '{}'::jsonb, 'deadbeef', 'bob', now())"
                   parent ty);
              false
            with _ -> true
          in
          Alcotest.(check bool) "RLS WITH CHECK: grantee write denied" true
            blocked))

let test_rights_without_read () =
  with_rows (fun conn ~ty ~row ~sib:_ ->
      ignore
        (grant conn ~ty ~prefix:row ~grantee:"bob" ~rights:[ "comment" ] ());
      Alcotest.(check int) "view: comment-only grant confers no read" 0
        (view_sees conn ~path:row "bob");
      Alcotest.(check int) "crud read: comment-only grant confers no read" 0
        (crud_read_count conn ~ty "bob"))

let test_grantee_kinds () =
  with_rows (fun conn ~ty ~row ~sib ->
      ignore
        (grant conn ~ty ~prefix:row ~grantee:"authenticated" ~rights:[ "read" ] ());
      Alcotest.(check int) "authenticated: carol reads" 1
        (view_sees conn ~path:row "carol");
      Alcotest.(check int) "authenticated: anonymous does not" 0
        (view_sees conn ~path:row "anonymous");
      ignore
        (grant conn ~ty ~prefix:sib ~grantee:"anonymous" ~rights:[ "read" ] ());
      Alcotest.(check int) "anonymous grant: anonymous reads" 1
        (view_sees conn ~path:sib "anonymous");
      Alcotest.(check int) "anonymous grant: carol reads" 1
        (view_sees conn ~path:sib "carol");
      Alcotest.(check int) "crud read: anonymous sees exactly the public row" 1
        (crud_read_count conn ~ty "anonymous"))

let expect_insert_denied label f =
  match f () with
  | _ -> Alcotest.fail (label ^ ": expected the insert to be rejected")
  | exception Pgx.PostgreSQL_Error _ -> ()

let test_attenuation_narrows () =
  with_rows (fun conn ~ty ~row ~sib ->
      let parent_prefix = String.sub row 0 (String.rindex row '.') in
      let p =
        grant conn ~ty ~prefix:row ~grantee:"bob" ~rights:[ "read"; "reshare" ] ()
      in
      expect_insert_denied "re-share to a wider prefix" (fun () ->
          grant conn ~ty ~parent:p ~prefix:parent_prefix ~grantee:"carol"
            ~rights:[ "read" ] ());
      expect_insert_denied "re-share to a sibling prefix" (fun () ->
          grant conn ~ty ~parent:p ~prefix:sib ~grantee:"carol"
            ~rights:[ "read" ] ());
      expect_insert_denied "re-share adding a right" (fun () ->
          grant conn ~ty ~parent:p ~prefix:row ~grantee:"carol"
            ~rights:[ "read"; "write" ] ());
      let ro = grant conn ~ty ~prefix:row ~grantee:"dave" ~rights:[ "read" ] () in
      expect_insert_denied "re-share from a grant without reshare" (fun () ->
          grant conn ~ty ~parent:ro ~prefix:row ~grantee:"carol"
            ~rights:[ "read" ] ());
      ignore
        (grant conn ~ty ~parent:p ~prefix:row ~grantee:"carol"
           ~rights:[ "read" ] ());
      Alcotest.(check int) "a narrowing re-share is accepted and reads" 1
        (view_sees conn ~path:row "carol"))

let test_revoke_cascades () =
  with_rows (fun conn ~ty ~row ~sib:_ ->
      let p =
        grant conn ~ty ~prefix:row ~grantee:"bob" ~rights:[ "read"; "reshare" ] ()
      in
      let c =
        grant conn ~ty ~parent:p ~prefix:row ~grantee:"carol" ~rights:[ "read" ] ()
      in
      Alcotest.(check int) "carol reads through the re-share" 1
        (view_sees conn ~path:row "carol");
      revoke conn ~id:p;
      Alcotest.(check int) "revoking the parent hides the row from carol" 0
        (view_sees conn ~path:row "carol");
      Alcotest.(check int) "and from bob" 0 (view_sees conn ~path:row "bob");
      Alcotest.(check int) "the child grant is marked revoked" 1
        (q_count conn ~params:[ s c ]
           "SELECT COUNT(*)::int FROM data_grants \
            WHERE id = $1::uuid AND revoked_at IS NOT NULL");
      expect_insert_denied "re-share from a revoked grant" (fun () ->
          grant conn ~ty ~parent:p ~prefix:row ~grantee:"erin"
            ~rights:[ "read" ] ());
      as_probe conn (fun () ->
          Alcotest.(check int) "RLS: carol cannot read after revoke" 0
            (rls_sees conn ~path:row "carol")))

let () =
  Random.self_init ();
  Alcotest.run "sharing-grants"
    [ ( "step 2: data_grants + read disjunct"
      , [ Alcotest.test_case "baseline: rows are private" `Quick
            test_baseline_private
        ; Alcotest.test_case "row grant: exactly that row, all mirrors" `Quick
            test_row_grant_exact
        ; Alcotest.test_case "grant is read-only" `Quick test_grant_is_read_only
        ; Alcotest.test_case "rights without read" `Quick test_rights_without_read
        ; Alcotest.test_case "grantee kinds" `Quick test_grantee_kinds
        ; Alcotest.test_case "attenuation narrows" `Quick test_attenuation_narrows
        ; Alcotest.test_case "revoke cascades" `Quick test_revoke_cascades
        ] )
    ]
