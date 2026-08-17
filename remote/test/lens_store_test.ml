(** Tests for borge/local-first.borg lens-model + replication-units +
    local-substrate (the remote/lib/lens_store.ml module). No network, no
    Postgres — pure filesystem + JSON, mirroring how the worker itself is
    pure.

    Verifies the pinned contracts from the campaign 8.0 plan pass:
      - fork_lens_name produces <owner>/lens-<device>[-<date>] (lens-model
        fork naming for the (fork ...)/Store.fork_branch child branch).
      - in_replication_unit confines replication to exactly the
        authorized-read subtree: data.u.<user>.* (full), data.shared.*
        (read-mostly seed), b.<lens>.* (the forked code lens) - and NOTHING
        outside it (another user's data.u.<other>.* never replicates, the
        one-tree path-prefix RLS floor is the replication boundary).
      - the substrate (local-substrate): materialize writes one node
        envelope file per node at its path under the lens root; get /
        subtree_list are the path-addressed reads (the filesystem IS the
        path index); put_body + node_hash are the local write path whose
        content hash is the sync/three-way-diff key. *)

open Pricklypear_lisp
open Lens_store

let tmpdir () : string =
  let base = Filename.get_temp_dir_name () in
  let dir = Filename.concat base ("lens-test-" ^ string_of_int (Unix.getpid ()) ^ "-" ^ string_of_int (Random.int 100000)) in
  Unix.mkdir dir 0o755;
  dir

let with_tmpdir f =
  let dir = tmpdir () in
  Fun.protect ~finally:(fun () -> (try Unix.rmdir dir with _ -> ()))
    (fun () -> f dir)

let lens_of ~root =
  { root; owner = "admin"; device = "laptop"; branch = "lens-laptop"; generation = 7L }

(* --- fork naming (lens-model) --- *)
let test_fork_lens_name () =
  Alcotest.(check string) "no date"
    "admin/lens-laptop" (fork_lens_name ~owner:"admin" ~device:"laptop" ());
  Alcotest.(check string) "with date"
    "admin/lens-laptop-2026-08-14"
    (fork_lens_name ~date:"2026-08-14" ~owner:"admin" ~device:"laptop" ())

(* --- replication unit (replication-units, path-prefix confinement) --- *)
let test_replication_unit () =
  let user = "admin" in
  let lens_prefix = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in  (* 32-char uuid *)
  (* own data: in *)
  Alcotest.(check bool) "own data.u.admin" true
    (in_replication_unit ~lens_prefix:lens_prefix ~user
       ("data.u." ^ enc_label "admin" ^ ".todo.abc"));
  (* another user: OUT — never replicates *)
  Alcotest.(check bool) "other user data.u.bob" false
    (in_replication_unit ~lens_prefix:lens_prefix ~user
       ("data.u." ^ enc_label "bob" ^ ".todo.xyz"));
  (* shared seed: in *)
  Alcotest.(check bool) "data.shared" true
    (in_replication_unit ~lens_prefix:lens_prefix ~user
       ("data.shared.bible-verse.abc"));
  (* the forked code lens: in *)
  Alcotest.(check bool) "forks lens code" true
    (in_replication_unit ~lens_prefix:lens_prefix ~user
       ("b." ^ lens_prefix ^ ".fn.todo-get"));
  (* a DIFFERENT lens / code branch: out *)
  Alcotest.(check bool) "other lens branch" false
    (in_replication_unit ~lens_prefix:lens_prefix ~user
       ("b.bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.fn.other"));
  (* non-data, non-lens root: out *)
  Alcotest.(check bool) "external root" false
    (in_replication_unit ~lens_prefix:lens_prefix ~user "sys.meta")

(* --- substrate path mapping --- *)
let test_path_to_file () =
  let root = "/tmp/lensroot" in
  Alcotest.(check string) "fn path" (root ^ "/b/aaaa/fn/todo-get.json")
    (path_to_file ~root "b.aaaa.fn.todo-get");
  Alcotest.(check string) "datum path"
    (root ^ "/data/u/admin/todo/id123.json")
    (path_to_file ~root "data.u.admin.todo.id123")

(* --- substrate read/write round trips --- *)
let test_materialize_and_get () =
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    let path = "data.u.abc.todo.id1" in
    let env =
      Yojson.Safe.to_string
        (`Assoc [ ("v", `Int 1); ("kind", `String "datum");
                  ("path", `String path); ("type", `String "todo");
                  ("body", `Assoc [("title", `String "hi")]);
                  ("author", `String "admin"); ("hash", `String "h1");
                  ("created_at", `String "t1"); ("updated_at", `String "t2") ])
    in
    let file = match materialize ~lens env with Some f -> f | None -> Alcotest.fail "materialize returned None" in
    Alcotest.(check bool) "file exists" true (Sys.file_exists file);
    (match get ~lens path with
     | Some j -> Alcotest.(check string) "round trip" env j
     | None -> Alcotest.fail "get returned None");
    Alcotest.(check string) "cleaned path" (root ^ "/data/u/abc/todo/id1.json") file)

let test_put_body_hash () =
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    let body = `Assoc [("title", `String "offline edit")] in
    put_body ~lens ~path:"data.u.abc.todo.id2" ~kind:"datum" ~type_:"todo"
      ~author:"admin" ~body;
    Alcotest.(check bool) "get present" true (get ~lens "data.u.abc.todo.id2" <> None);
    (* the envelope carries the content hash of the canonical body *)
    (match node_hash ~lens "data.u.abc.todo.id2" with
     | Some h ->
         let expected = content_hash (Yojson.Safe.to_string body) in
         Alcotest.(check string) "hash = content hash" expected h
     | None -> Alcotest.fail "node_hash None");
    (* content_hash is deterministic *)
    Alcotest.(check string) "deterministic hash"
      (content_hash "x") (content_hash "x"))

let test_subtree_list () =
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    List.iter
      (fun p ->
         let env = Yojson.Safe.to_string
             (`Assoc [ ("v", `Int 1); ("kind", `String "datum");
                       ("path", `String p); ("type", `String "todo");
                       ("body", `Assoc []); ("author", `String "admin");
                       ("hash", `String "h"); ("created_at", `String "t");
                       ("updated_at", `String "t") ]) in
         ignore (materialize ~lens env))
      [ "data.u.abc.todo.a"; "data.u.abc.todo.b"; "data.u.abc.note.c" ];
    let todos = subtree_list ~lens "data.u.abc.todo" in
    Alcotest.(check int) "two todo paths under prefix" 2 (List.length todos);
    Alcotest.(check bool) "contains a" true (List.mem "data.u.abc.todo.a" todos);
    Alcotest.(check bool) "contains b" true (List.mem "data.u.abc.todo.b" todos);
    (* the note path is NOT under the todo prefix *)
    Alcotest.(check bool) "note not under todo" false (List.mem "data.u.abc.note.c" todos))

let () =
  Random.self_init ();
  Alcotest.run "lens_store"
    [ ("lens-model", [ Alcotest.test_case "fork_lens_name" `Quick test_fork_lens_name ])
    ; ("replication-units", [ Alcotest.test_case "in_replication_unit" `Quick test_replication_unit ])
    ; ("local-substrate"
       , [ Alcotest.test_case "path_to_file" `Quick test_path_to_file
         ; Alcotest.test_case "materialize_and_get" `Quick test_materialize_and_get
         ; Alcotest.test_case "put_body_hash" `Quick test_put_body_hash
         ; Alcotest.test_case "subtree_list" `Quick test_subtree_list ])
    ]
