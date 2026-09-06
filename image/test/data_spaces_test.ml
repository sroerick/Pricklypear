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

(* ---------------- S2: prim surface (Crud-level) ---------------- *)

module Crud = Pricklypear_image_lib.Crud
module Ast = Pricklypear_lisp.Ast

(** S2 fixture: unique names, nothing pre-seeded — the tests exercise the
    Crud space-management functions themselves. Tears down by name. *)
let with_s2 f =
  with_store (fun store ->
      let conn = Option.get (S.conn_of store) in
      let uniq = Printf.sprintf "s2%06d" (Random.int 1_000_000) in
      let sp = "sps" ^ uniq in
      let ty = "spt" ^ uniq in
      Fun.protect
        ~finally:(fun () ->
          exec conn
            (Printf.sprintf
               "DELETE FROM tree_nodes WHERE kind='datum' AND type_name='%s'; \
                DELETE FROM data_rows WHERE type_name='%s'; \
                DELETE FROM data_spaces WHERE name = '%s'"
               ty ty sp))
        (fun () -> f store ~sp ~ty))

let expect_denied label f =
  match f () with
  | _ -> Alcotest.fail (label ^ ": expected denial, got success")
  | exception Ast.Interp_error (Ast.User_error (msg, _)) ->
      Alcotest.(check bool)
        (label ^ " (msg: " ^ msg ^ ")")
        true (String.length msg > 0)
  | exception e -> Alcotest.fail (label ^ ": wrong exception " ^ Printexc.to_string e)

let vlist_strs = function
  | Ast.VList xs ->
      List.filter_map (function Ast.VStr s -> Some s | _ -> None) xs
  | _ -> Alcotest.fail "expected VList"

(* S2: lifecycle — create, list, add, members, owner-gate on add. *)
let test_lifecycle () =
  with_s2 (fun store ~sp ~ty:_ ->
      ignore (Crud.space_create store ~caller:"alice" ~name:sp);
      let as_alice =
        vlist_strs (Crud.space_list store ~caller:"alice" ~kernel_admin:false)
      in
      Alcotest.(check bool) "alice lists her space" true (List.mem sp as_alice);
      ignore
        (Crud.space_add_member store ~caller:"alice" ~kernel_admin:false
           ~space:sp ~username:"bob");
      let as_bob =
        vlist_strs (Crud.space_list store ~caller:"bob" ~kernel_admin:false)
      in
      Alcotest.(check bool) "bob lists after add" true (List.mem sp as_bob);
      let as_carol =
        vlist_strs (Crud.space_list store ~caller:"carol" ~kernel_admin:false)
      in
      Alcotest.(check bool) "carol sees nothing" false (List.mem sp as_carol);
      let members =
        vlist_strs
          (Crud.space_members store ~caller:"alice" ~kernel_admin:false ~space:sp)
      in
      Alcotest.(check int) "two members" 2 (List.length members);
      expect_denied "non-owner add denied" (fun () ->
          Crud.space_add_member store ~caller:"bob" ~kernel_admin:false
            ~space:sp ~username:"dave");
      expect_denied "duplicate create denied" (fun () ->
          Crud.space_create store ~caller:"bob" ~name:sp))

(* S2: admit routes create; non-member create/update/delete denied;
   removed member's create denied; node-path reports the space home. *)
let test_routing_and_guard () =
  with_s2 (fun store ~sp ~ty ->
      let conn = Option.get (S.conn_of store) in
      ignore (Crud.space_create store ~caller:"alice" ~name:sp);
      ignore
        (Crud.space_add_member store ~caller:"alice" ~kernel_admin:false
           ~space:sp ~username:"bob");
      ignore
        (Crud.space_admit store ~caller:"alice" ~kernel_admin:false ~space:sp
           ~type_name:ty);
      (* member create routes to the space *)
      let id =
        match Crud.create store ~type_name:ty ~fields_json:"{\"n\":7}" ~author:"bob" with
        | Ast.VStr id -> id
        | _ -> Alcotest.fail "member create did not return id"
      in
      let p = path_of conn ~id in
      Alcotest.(check bool)
        ("space path " ^ p)
        true
        (String.sub p 0 7 = "data.s.");
      (* node-path parity *)
      (match Crud.node_path store ~type_name:ty ~id with
       | Ast.VStr np ->
           Alcotest.(check bool) ("node-path " ^ np) true
             (String.sub np 0 7 = "data.s.")
       | _ -> Alcotest.fail "node-path not VStr");
      (* guard: non-member create/update/delete all raise *)
      expect_denied "carol create denied" (fun () ->
          Crud.create store ~type_name:ty ~fields_json:"{\"n\":1}" ~author:"carol");
      expect_denied "carol update denied" (fun () ->
          Crud.update_guarded store ~type_name:ty ~id ~fields_json:"{\"n\":2}"
            ~caller:"carol");
      expect_denied "carol delete denied" (fun () ->
          Crud.delete_guarded store ~type_name:ty ~id ~caller:"carol");
      (* member update/delete work *)
      (match Crud.update_guarded store ~type_name:ty ~id ~fields_json:"{\"n\":8}"
               ~caller:"alice" with
       | Ast.VStr _ -> ()
       | _ -> Alcotest.fail "member update failed");
      (* removed member errors, loudly *)
      ignore
        (Crud.space_remove_member store ~caller:"alice" ~kernel_admin:false
           ~space:sp ~username:"bob");
      expect_denied "removed member create denied" (fun () ->
          Crud.create store ~type_name:ty ~fields_json:"{\"n\":3}" ~author:"bob"))

(* ---------------- S3: the log consumer (data-history) ---------------- *)

(* S3: member edits produce per-author before-images, newest-first;
   non-member history reads return nothing (visibility holds). *)
let test_history () =
  with_s2 (fun store ~sp ~ty ->
      ignore (Crud.space_create store ~caller:"alice" ~name:sp);
      ignore
        (Crud.space_add_member store ~caller:"alice" ~kernel_admin:false
           ~space:sp ~username:"bob");
      ignore
        (Crud.space_admit store ~caller:"alice" ~kernel_admin:false ~space:sp
           ~type_name:ty);
      let id =
        match Crud.create store ~type_name:ty ~fields_json:"{\"n\":1}" ~author:"alice" with
        | Ast.VStr id -> id
        | _ -> Alcotest.fail "create did not return id"
      in
      ignore
        (Crud.update_guarded store ~type_name:ty ~id ~fields_json:"{\"n\":2}"
           ~caller:"alice");
      ignore
        (Crud.update_guarded store ~type_name:ty ~id ~fields_json:"{\"n\":3}"
           ~caller:"bob");
      let hist = Crud.data_history_json store ~viewer:"bob" ~type_name:ty ~id in
      Alcotest.(check int) "three versions" 3 (List.length hist);
      let authors =
        List.map
          (fun v ->
            match Yojson.Safe.Util.member "author" v with
            | `String a -> a
            | _ -> Alcotest.fail "version missing author")
          hist
      in
      Alcotest.(check (list string)) "newest-first authors"
        [ "bob"; "alice"; "alice" ] authors;
      let head_n =
        match Yojson.Safe.Util.member "body" (List.hd hist) with
        | `Assoc a -> (
            match List.assoc_opt "n" a with
            | Some (`Int n) -> n
            | Some (`Intlit s) -> int_of_string s
            | _ -> Alcotest.fail "head body missing n")
        | _ -> Alcotest.fail "head body not an object"
      in
      Alcotest.(check int) "head is bob's write" 3 head_n;
      let deletes =
        List.for_all
          (fun v ->
            match Yojson.Safe.Util.member "delete" v with
            | `Bool b -> b
            | _ -> false)
          hist
      in
      Alcotest.(check bool) "no tombstones yet" false deletes;
      (* prim shape + non-member confinement *)
      (match Crud.data_history store ~viewer:"carol" ~type_name:ty ~id with
       | Ast.VNil -> ()
       | _ -> Alcotest.fail "carol should see no history"))

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
      ( "shared-data-spaces S2",
        [
          Alcotest.test_case "space lifecycle + owner gate" `Quick
            test_lifecycle;
          Alcotest.test_case "admit routing + non-member guard" `Quick
            test_routing_and_guard;
        ] );
      ( "shared-data-spaces S3",
        [
          Alcotest.test_case "data-history: per-author before-images" `Quick
            test_history;
        ] );
    ]
