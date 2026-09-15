(* image/test/admin_rls_test.ml — admin disjunct in the datum RLS floor
   (migrations/033_admin_rls.sql + the crud.ml visible_sql mirror).

   Covers:
     - tree_user_is_admin: true only for users.is_admin rows
     - view visibility: an admin sees another user's personal rows; a
       non-admin third party still sees nothing
     - the crud.ml visible_sql mirror with the admin disjunct
     - the RLS floor under an unprivileged role: admin reads pass
     - update as admin preserves data_rows.author (no attribution steal);
       update as a non-admin non-author stays a silent no-op

   Needs scripts/dev.sh Postgres; skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
module TS = Pricklypear_image_lib.Tree_store
module Db = Pricklypear_image_lib.Store_db
module Crud = Pricklypear_image_lib.Crud
module Ast = Pricklypear_lisp.Ast

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
          Printf.printf "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let s = Pgx.Value.of_string

(* pgx execute is single-result; split fixture SQL on ';' so multi-statement
   setup/teardown strings run statement by statement. *)
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

let set_pp_user conn who =
  exec conn (Printf.sprintf "SELECT set_config('pp.user', '%s', false)" who)

(** Unique type + three users: one admin, the datum's author, and a
    non-admin third party. One datum authored by the plain user. Tears
    itself down. *)
let with_fixture f =
  with_store (fun store ->
      let conn = Option.get (S.conn_of store) in
      let uniq = Printf.sprintf "%06d" (Random.int 1_000_000) in
      let ty = "admt" ^ uniq in
      let admin_u = "adma" ^ uniq in
      let plain_u = "admp" ^ uniq in
      let other_u = "admo" ^ uniq in
      exec conn
        (Printf.sprintf
           "INSERT INTO users (username, is_admin) VALUES ('%s', true), ('%s', false), ('%s', false)"
           admin_u plain_u other_u);
      let id =
        match TS.insert_datum conn ~type_name:ty ~fields_json:"{\"n\":1}" ~author:plain_u with
        | Ok id -> id
        | Error e -> Alcotest.fail ("insert_datum failed: " ^ e)
      in
      Fun.protect
        ~finally:(fun () ->
          exec conn
            (Printf.sprintf
               "DELETE FROM tree_nodes WHERE kind='datum' AND type_name='%s'; \
                DELETE FROM data_rows WHERE type_name='%s'; \
                DELETE FROM users WHERE username IN ('%s','%s','%s')"
               ty ty admin_u plain_u other_u);
          set_pp_user conn "")
        (fun () -> f store conn ~ty ~id ~admin_u ~plain_u ~other_u))

(* helper truth table *)
let test_helper () =
  with_fixture (fun _store conn ~ty:_ ~id:_ ~admin_u ~plain_u ~other_u ->
      let is_admin who =
        q_count conn ~params:[ s who ]
          "SELECT tree_user_is_admin($1)::int"
      in
      Alcotest.(check int) "admin user" 1 (is_admin admin_u);
      Alcotest.(check int) "plain user" 0 (is_admin plain_u);
      Alcotest.(check int) "other user" 0 (is_admin other_u);
      Alcotest.(check int) "unknown user" 0 (is_admin "nobody"))

(* the view: admin sees another user's row; third party still blind *)
let test_view_visibility () =
  with_fixture (fun _store conn ~ty ~id:_ ~admin_u ~plain_u ~other_u ->
      let count_as who =
        set_pp_user conn who;
        q_count conn ~params:[ s ty ]
          "SELECT COUNT(*)::int FROM tree_data_rows WHERE type_name = $1"
      in
      Alcotest.(check int) "admin sees other's row" 1 (count_as admin_u);
      Alcotest.(check int) "author sees own row" 1 (count_as plain_u);
      Alcotest.(check int) "non-admin third party blind" 0 (count_as other_u);
      set_pp_user conn "")

(* the crud.ml visible_sql mirror with the admin disjunct *)
let test_visible_sql_mirror () =
  with_fixture (fun _store conn ~ty ~id:_ ~admin_u ~plain_u ~other_u ->
      let n who =
        q_count conn
          ~params:[ s ty; s who ]
          "SELECT COUNT(*)::int FROM tree_data_rows v \
           WHERE v.type_name = $1 \
           AND (v.author = $2 OR tree_is_seed_type(v.type_name) \
                OR tree_path_in_member_space(v.path, $2) \
                OR tree_user_is_admin($2))"
      in
      Alcotest.(check int) "mirror: admin admitted" 1 (n admin_u);
      Alcotest.(check int) "mirror: author admitted" 1 (n plain_u);
      Alcotest.(check int) "mirror: third party confined" 0 (n other_u))

(* RLS floor under an unprivileged role: the admin disjunct applies.
   The probe role mirrors an app role: broad SELECT (the floor's helper
   functions read users, spaces tables, type defs), no write grants except
   the ones make_update_probe adds. The tree-era migrations grant no app
   role that surface today, so the test grants it here. *)
let make_probe conn =
  exec conn "DROP ROLE IF EXISTS pp_admin_probe";
  exec conn
    "CREATE ROLE pp_admin_probe NOLOGIN NOSUPERUSER NOBYPASSRLS NOINHERIT";
  exec conn "GRANT SELECT ON ALL TABLES IN SCHEMA public TO pp_admin_probe";
  exec conn "GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO pp_admin_probe"

let drop_probe conn =
  exec conn "RESET ROLE";
  exec conn
    "REVOKE ALL ON ALL TABLES IN SCHEMA public FROM pp_admin_probe";
  exec conn
    "REVOKE ALL ON ALL SEQUENCES IN SCHEMA public FROM pp_admin_probe";
  exec conn "DROP ROLE IF EXISTS pp_admin_probe"

let test_rls_floor () =
  with_fixture (fun _store conn ~ty ~id:_ ~admin_u ~plain_u ~other_u ->
      make_probe conn;
      Fun.protect
        ~finally:(fun () -> drop_probe conn)
        (fun () ->
           exec conn "SET ROLE pp_admin_probe";
             let count_as who =
               set_pp_user conn who;
               q_count conn ~params:[ s ty ]
                 "SELECT COUNT(*)::int FROM tree_nodes WHERE kind='datum' \
                  AND type_name = $1"
             in
             Alcotest.(check int) "RLS: admin sees 1" 1 (count_as admin_u);
             Alcotest.(check int) "RLS: third party sees 0" 0 (count_as other_u);
             Alcotest.(check int) "RLS: author sees 1" 1 (count_as plain_u);
             exec conn "RESET ROLE";
             set_pp_user conn ""))

(* update as admin keeps the row's author; non-admin third party no-op.
   Runs as the probe role so the RLS floor, not superuser rights, does the
   scoping; the write grants cover the update path's trigger targets
   (tree_versions, branch_generations, tree_generations, tree_branches). *)
let make_update_probe conn =
  make_probe conn;
  exec conn
    "GRANT SELECT, INSERT, UPDATE, DELETE ON tree_nodes, data_rows \
     TO pp_admin_probe";
  exec conn
    "GRANT INSERT, UPDATE ON tree_versions, branch_generations, \
     tree_generations, tree_branches TO pp_admin_probe"

let test_update_preserves_author () =
  with_fixture (fun store conn ~ty ~id ~admin_u ~plain_u ~other_u ->
      make_update_probe conn;
      Fun.protect
        ~finally:(fun () -> drop_probe conn)
        (fun () ->
           exec conn "SET ROLE pp_admin_probe";
           (* the app arms the floor per request via the fiber-local pp.user;
              the tx's SET LOCAL overwrites any session-level value, so bind
              it the way production does. *)
           S.with_pp_user admin_u (fun () ->
               let updated =
                 Crud.update_guarded store ~type_name:ty ~id
                   ~fields_json:"{\"n\":2}" ~caller:admin_u
               in
               Alcotest.(check bool) "admin update succeeds"
                 true (match updated with Ast.VStr _ -> true | _ -> false));
           exec conn "RESET ROLE";
           let author =
             match
               Db.exec conn ~params:[ s id ]
                 "SELECT author FROM data_rows WHERE id = $1::uuid"
             with
             | [ [ a ] ] ->
                 (match Pgx.Value.to_string a with Some v -> v | None -> "")
             | _ -> Alcotest.fail "author query shape"
           in
           Alcotest.(check string) "author preserved" plain_u author;
           exec conn "SET ROLE pp_admin_probe";
           S.with_pp_user other_u (fun () ->
               let blocked =
                 Crud.update_guarded store ~type_name:ty ~id
                   ~fields_json:"{\"n\":3}" ~caller:other_u
               in
               Alcotest.(check bool) "third-party update is silent no-op"
                 true (match blocked with Ast.VNil -> true | _ -> false));
           exec conn "RESET ROLE";
           set_pp_user conn ""))

let () =
  Alcotest.run "admin_rls"
    [
      ( "admin RLS disjunct",
        [
          Alcotest.test_case "tree_user_is_admin truth table" `Quick
            test_helper;
          Alcotest.test_case "view: admin sees, third party blind" `Quick
            test_view_visibility;
          Alcotest.test_case "visible_sql mirror with admin" `Quick
            test_visible_sql_mirror;
          Alcotest.test_case "RLS floor: admin reads pass" `Quick
            test_rls_floor;
          Alcotest.test_case "update preserves author" `Quick
            test_update_preserves_author;
        ] );
    ]
