(** image/test/data_spaces_test.ml — borge/branching.borg shared-data-spaces
    S1 make (storage floor): migrations/027_data_spaces.sql.

    Covers:
      - admitted-type routing: (create) of a space-admitted type lands at
        data.s.<space>.<type>.<id> (tree_datum_path consults
        data_space_types)
      - regression: non-admitted types still route to data.u.<author>.*
      - view visibility: tree_data_rows shows space rows to members
        (two DIFFERENT member identities), never to non-members
      - the crud.ml visible_sql mirror semantics (same predicate shape)
      - the RLS floor: under an unprivileged role the member-space
        disjunct applies for reads, and WITH CHECK rejects a non-member
        write into a space path

    Needs scripts/dev.sh Postgres; skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
module TS = Pricklypear_image_lib.Tree_store
module Db = Pricklypear_image_lib.Io_lwt.Db

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    { sw; net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r) }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let s = Pgx.Value.of_string

let exec conn sql = ignore (Db.execute conn sql)

let q_count conn ?(params = []) sql : int =
  match Db.execute conn ~params sql with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some n -> int_of_string n
      | None -> Alcotest.fail ("count query returned NULL: " ^ sql))
  | _ -> Alcotest.fail ("count query shape: " ^ sql)

let q_string conn ?(params = []) sql : string =
  match Db.execute conn ~params sql with
  | [ [ c ] ] -> (
      match Pgx.Value.to_string c with
      | Some v -> v
      | None -> Alcotest.fail ("scalar query returned NULL: " ^ sql))
  | _ -> Alcotest.fail ("scalar query shape: " ^ sql)

let set_pp_user conn who =
  exec conn (Printf.sprintf "SELECT set_config('pp.user', '%s', false)"
               (if who = "" then "" else who))

(** One space + admitted type per test, unique per run; tears itself down.
    Members: alice + bob; carol is the non-member control. *)
let with_fixture f =
  with_store (fun store ->
      let conn = Option.get (S.conn_of store) in
      let uniq = Printf.sprintf "sp%06d" (Random.int 1_000_000) in
      let sp = "sps" ^ uniq in
      let ty = "spt" ^ uniq in
      exec conn
        (Printf.sprintf
           "INSERT INTO data_spaces (name, owner) VALUES ('%s','alice'); \
            INSERT INTO data_space_members (space, username, role) VALUES \
            ('%s','alice','owner'), ('%s','bob','member'); \
            INSERT INTO data_space_types (type_name, space, admitted_by) \
            VALUES ('%s','%s','test')"
           sp sp sp ty sp);
      Fun.protect
        ~finally:(fun () ->
          exec conn
            (Printf.sprintf
               "DELETE FROM tree_nodes WHERE kind='datum' AND type_name LIKE 'spt%%'; \
                DELETE FROM data_rows WHERE type_name LIKE 'spt%%'; \
                DELETE FROM data_spaces WHERE name = '%s'"
               sp);
          set_pp_user conn "")
        (fun () -> f conn ~sp ~ty))

(** Insert a datum via the real write path; return its id. *)
let insert conn ~type_name ~author =
  match
    TS.insert_datum conn ~type_name ~fields_json:"{\"n\":1}" ~author
  with
  | Some id -> id
  | None -> Alcotest.fail "insert_datum returned no id"

let path_of conn ~id =
  q_string conn ~params:[ s id ]
    "SELECT path::text FROM tree_nodes WHERE kind='datum' \
     AND tree_datum_id(path) = $1::uuid"

(* S1: an admitted type routes to data.s.<space>.<type>.<id>. *)
let test_admitted_routing () =
  with_fixture (fun conn ~sp ~ty ->
      let id = insert conn ~type_name:ty ~author:"alice" in
      let want_prefix =
        Printf.sprintf "data.s.%s.%s." (TS.enc_label sp) (TS.enc_label ty)
      in
      let got = path_of conn ~id in
      Alcotest.(check bool)
        (Printf.sprintf "space path (got %s)" got)
        true
        (String.length got > String.length want_prefix
         && String.sub got 0 (String.length want_prefix) = want_prefix))

(* S1 regression: a NON-admitted type still routes per-user. *)
let test_unadmitted_regression () =
  with_fixture (fun conn ~sp:_ ~ty ->
      let free_ty = ty ^ "free" in
      let id = insert conn ~type_name:free_ty ~author:"alice" in
      let got = path_of conn ~id in
      let want_prefix = "data.u.alice." in
      Alcotest.(check bool)
        (Printf.sprintf "user path (got %s)" got)
        true
        (String.sub got 0 (String.length want_prefix) = want_prefix);
      exec conn
        (Printf.sprintf
           "DELETE FROM tree_nodes WHERE type_name='%s'; \
            DELETE FROM data_rows WHERE type_name='%s'"
           free_ty free_ty))

(* S1: the view shows space rows to both members, not to non-members. *)
let test_view_visibility () =
  with_fixture (fun conn ~sp:_ ~ty ->
      ignore (insert conn ~type_name:ty ~author:"alice");
      let count_as who =
        set_pp_user conn who;
        q_count conn ~params:[ s ty ]
          "SELECT COUNT(*)::int FROM tree_data_rows WHERE type_name = $1"
      in
      Alcotest.(check int) "alice (owner) sees" 1 (count_as "alice");
      Alcotest.(check int) "bob (member) sees" 1 (count_as "bob");
      Alcotest.(check int) "carol (non-member) sees" 0 (count_as "carol");
      Alcotest.(check int) "unset pp.user (scripts) sees" 1 (count_as "");
      set_pp_user conn "")

(* S1: the crud.ml visible_sql mirror semantics — same predicate shape the
   prim layer appends (author OR seed OR member-space). *)
let test_visible_sql_mirror () =
  with_fixture (fun conn ~sp:_ ~ty ->
      ignore (insert conn ~type_name:ty ~author:"alice");
      let n who =
        q_count conn
          ~params:[ s ty; s who ]
          "SELECT COUNT(*)::int FROM tree_data_rows v \
           WHERE v.type_name = $1 \
           AND (v.author = $2 OR tree_is_seed_type(v.type_name) \
                OR tree_path_in_member_space(v.path, $2))"
      in
      Alcotest.(check int) "carol confined" 0 (n "carol");
      Alcotest.(check int) "bob admitted" 1 (n "bob");
      Alcotest.(check int) "alice author" 1 (n "alice"))

(* S1: the RLS floor. Under an unprivileged role the member-space disjunct
   confines reads; WITH CHECK rejects a non-member write into the space. *)
let test_rls_floor () =
  with_fixture (fun conn ~sp ~ty ->
      ignore (insert conn ~type_name:ty ~author:"alice");
      exec conn "DROP ROLE IF EXISTS pp_spaces_probe";
      exec conn
        "CREATE ROLE pp_spaces_probe NOLOGIN NOSUPERUSER NOBYPASSRLS \
         NOINHERIT";
      exec conn "GRANT SELECT, INSERT ON tree_nodes TO pp_spaces_probe";
      Fun.protect
        ~finally:(fun () ->
          exec conn "RESET ROLE";
          exec conn "REVOKE SELECT, INSERT ON tree_nodes FROM pp_spaces_probe";
          exec conn "DROP ROLE IF EXISTS pp_spaces_probe")
        (fun () ->
          exec conn "SET ROLE pp_spaces_probe";
          let count_as who =
            set_pp_user conn who;
            q_count conn ~params:[ s ty ]
              "SELECT COUNT(*)::int FROM tree_nodes WHERE kind='datum' \
               AND type_name = $1"
          in
          Alcotest.(check int) "RLS: carol sees 0" 0 (count_as "carol");
          Alcotest.(check int) "RLS: bob sees 1" 1 (count_as "bob");
          (* WITH CHECK: carol cannot write into the space path. *)
          let blocked =
            try
              exec conn
                (Printf.sprintf
                   "INSERT INTO tree_nodes \
                    (path, kind, type_name, payload, hash, author, updated_at) \
                    VALUES (('data.s.' || tree_enc_label('%s') || '.' || \
                             tree_enc_label('%s') || '.' || \
                             replace(gen_random_uuid()::text,'-',''))::ltree, \
                            'datum', '%s', '{}'::jsonb, 'deadbeef', 'carol', now())"
                   sp ty ty);
              false
            with _ -> true
          in
          Alcotest.(check bool) "RLS WITH CHECK: carol write denied" true
            blocked))

let () =
  Alcotest.run "data_spaces"
    [
      ( "shared-data-spaces S1",
        [
          Alcotest.test_case "admitted type routes to data.s" `Quick
            test_admitted_routing;
          Alcotest.test_case "non-admitted stays data.u" `Quick
            test_unadmitted_regression;
          Alcotest.test_case "view: members see, non-members don't" `Quick
            test_view_visibility;
          Alcotest.test_case "visible_sql mirror semantics" `Quick
            test_visible_sql_mirror;
          Alcotest.test_case "RLS floor + write check" `Quick test_rls_floor;
        ] );
    ]
