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
        path index); put_body is the local write path whose envelope
        carries NO client-computed hash — hash authority is server-side
        (one-tree.hash-domain digests DB-side); the client carries server
        hashes verbatim and a locally-written envelope converges to the
        server stamp via the sync round's convergence pull. *)

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
    (* the client fabricates NO hash: the OCaml-side hash domain is gone
       (one-tree.hash-domain computes DB-side), so a locally-built
       envelope carries "" — the server stamps the canonical hash when
       the pushed body is applied, and the sync round's convergence pull
       (sync_client) replaces the envelope with the server's. *)
    Alcotest.(check (option string)) "no client-computed hash" (Some "")
      (node_hash ~lens "data.u.abc.todo.id2");
    (* the body itself travels intact (the sync push transfers it) *)
    (match get ~lens "data.u.abc.todo.id2" with
     | Some env ->
         Alcotest.(check bool) "body intact for the push" true
           (match Yojson.Safe.from_string env with
            | `Assoc kv -> List.assoc_opt "body" kv = Some body
            | _ -> false)
     | None -> Alcotest.fail "get None"))

let test_sync_convergence_pull () =
  (* i-1788670970-5 end to end: a locally-written envelope (hash "")
     converges to the server stamp in the SAME round that pushes it, so
     an identical body never re-diverges — no permanent Diverge/LWW
     churn. Round 2 proves the convergence: the manifest now carries the
     server hash, the server classifies Same, and no second tree/get
     fires. *)
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    (* the manifest only carries the owner's replication prefix (encoded
       user label), so the locally-written node must live there *)
    let path = "data.u." ^ enc_label "admin" ^ ".todo.local1" in
    put_body ~lens ~path ~kind:"datum" ~type_:"todo" ~author:"admin"
      ~body:(`Assoc [("title", `String "offline edit")]);
    Alcotest.(check (option string)) "starts unstamped" (Some "")
      (node_hash ~lens path);
    (* the server's canonical envelope for the merged body (the hash is
       the DB-side one-tree.hash-domain digest the client can only learn
       by pulling) *)
    let server_env =
      Yojson.Safe.to_string
        (`Assoc [ ("v", `Int 1); ("kind", `String "datum");
                  ("path", `String path); ("type", `String "todo");
                  ("body", `Assoc [("title", `String "offline edit")]);
                  ("author", `String "admin"); ("hash", `String "Hsrv");
                  ("created_at", `String "t"); ("updated_at", `String "t") ])
    in
    let tree_gets = ref 0 in
    (* stateful fake server: it holds the node (Hsrv); a client manifest
       hash of "" is a diverge pair the LWW gives to the newer client
       edit; the converged hash classifies Same. *)
    let transport (prim : string) (args : Pricklypear_lisp.Ast.value list) :
        (Pricklypear_lisp.Ast.value, exn) Result.t =
      match prim, args with
      | "sync/push", [ cm; _ ] ->
          let client_hash =
            match cm with
            | Ast.VList rows ->
                (match
                   List.filter_map
                     (function
                       | Ast.VList [ Ast.VStr p; Ast.VStr h ] when p = path ->
                           Some h
                       | _ -> None)
                     rows
                 with h :: _ -> h | [] -> "")
            | _ -> ""
          in
          let diverge, won, applied =
            if client_hash = "" then ([path], [path], 1) else ([], [], 0)
          in
          let jlist xs = `List (List.map (fun s -> `String s) xs) in
          Ok
            (Ast.VStr
               (Yojson.Safe.to_string
                  (`Assoc
                    [ ("mode", `String "two-way")
                    ; ("applied", `Int applied)
                    ; ("client_only", `Int 0)
                    ; ("server_only", `Int 0)
                    ; ("diverge", `Int (List.length diverge))
                    ; ("server_only_paths", jlist [])
                    ; ("diverge_paths", jlist diverge)
                    ; ("lww_client_won", jlist won)
                    ; ("lww_server_kept", jlist [])
                    ; ("generation", `String "1") ])))
      | "tree/get", [ Ast.VStr p ] when p = path ->
          incr tree_gets;
          Ok (Ast.VStr server_env)
      | "current-branch", [] -> Ok (Ast.VStr "system/main")
      | _ -> Error (Failure ("unexpected transport call: " ^ prim))
    in
    let r1 = Sync_client.run_round ~transport ~lens ~quiet:true () in
    Alcotest.(check bool) "round 1 ok" true r1.ok;
    Alcotest.(check int) "round 1 converged (the push applied)" 1 r1.pushed;
    (* the local envelope now carries the SERVER hash *)
    Alcotest.(check (option string)) "converged to server hash" (Some "Hsrv")
      (node_hash ~lens path);
    let r2 = Sync_client.run_round ~transport ~lens ~quiet:true () in
    Alcotest.(check bool) "round 2 ok" true r2.ok;
    Alcotest.(check int) "round 2: no divergence (no churn)" 0 r2.diverge;
    Alcotest.(check int) "round 2: nothing applied" 0 r2.pushed;
    Alcotest.(check int) "round 2: nothing pulled" 0 r2.pulled;
    Alcotest.(check int) "exactly one tree/get across both rounds" 1 !tree_gets)

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

(* --- code-lens + ancestry materialization (local-substrate 8.2) --- *)

let test_code_kind_path () =
  Alcotest.(check (option string)) "fn path"
    (Some "b.admin.fn.todo-get")
    (code_kind_path ~branch:"admin" "fn" ~name:"todo-get");
  Alcotest.(check (option string)) "route path"
    (Some "b.admin.route./issues")
    (code_kind_path ~branch:"admin" "route" ~name:"/issues");
  Alcotest.(check (option string)) "non-code kind rejected" None
    (code_kind_path ~branch:"admin" "datum" ~name:"x")

let test_materialize_code_and_resolve () =
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    (* lens owns an override; main is ancestry -- different source *)
    let lens_file =
      materialize_code_node ~lens ~branch:"lens-laptop" ~kind:"fn"
        ~name:"todo-get" ~src:"(define (todo-get id) 'lens)" ~doc:"lens own"
        ~author:"admin"
      |> function Some f -> f | None -> Alcotest.fail "materialize_code None"
    in
    Alcotest.(check bool) "code file exists" true (Sys.file_exists lens_file);
    ignore
      (materialize_code_node ~lens ~branch:"main" ~kind:"fn"
         ~name:"todo-get" ~src:"(define (todo-get id) 'main)" ~doc:"main"
         ~author:"admin");
    (* record ancestry self-first: lens then main *)
    record_ancestry ~lens [ "lens-laptop"; "main" ];
    Alcotest.(check (list string)) "ancestry round trip"
      [ "lens-laptop"; "main" ] (ancestry ~lens);
    (* closest-wins: the lens's own override resolves first *)
    (match resolve_def ~lens ~kind:"fn" ~name:"todo-get" with
     | Some env ->
         Alcotest.(check (option string)) "closest = lens's src"
           (Some "(define (todo-get id) 'lens)")
           (code_src_of_envelope env)
     | None -> Alcotest.fail "resolve_def None for own override");
    (* a def only on ancestry resolves through inheritance *)
    ignore
      (materialize_code_node ~lens ~branch:"main" ~kind:"fn"
         ~name:"shared-only" ~src:"(define (shared-only) 42)" ~doc:"main only"
         ~author:"admin");
    (match resolve_def ~lens ~kind:"fn" ~name:"shared-only" with
     | Some env ->
         Alcotest.(check (option string)) "inherited from main"
           (Some "(define (shared-only) 42)")
           (code_src_of_envelope env)
     | None -> Alcotest.fail "resolve_def None for ancestry def");
    (* a def on NO branch does not resolve *)
    Alcotest.(check bool) "unknown def is None" true
      (resolve_def ~lens ~kind:"fn" ~name:"nope" = None))

let test_node_manifest () =
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    (* one datum + one code node *)
    ignore
      (materialize ~lens
         (Yojson.Safe.to_string
            (`Assoc [ ("v", `Int 1); ("kind", `String "datum");
                      ("path", `String "data.u.admin.todo.a");
                      ("type", `String "todo"); ("body", `Assoc []);
                      ("author", `String "admin"); ("hash", `String "h1");
                      ("created_at", `String "t"); ("updated_at", `String "t") ])));
    ignore
      (materialize_code_node ~lens ~branch:"lens-laptop" ~kind:"fn"
         ~name:"f" ~src:"(define (f) 1)" ~doc:"" ~author:"admin");
    let manifest = node_manifest ~lens in
    Alcotest.(check int) "two node entries" 2 (List.length manifest);
    Alcotest.(check bool) "datum path present" true
      (List.mem_assoc "data.u.admin.todo.a" manifest);
    Alcotest.(check bool) "code path present" true
      (List.mem_assoc "b.lens-laptop.fn.f" manifest);
    (* the ancestry metadata file is NOT in the manifest *)
    Alcotest.(check bool) "no ancestry in manifest" false
      (List.exists (fun (p, _) -> String.length p > 0 && p.[0] = '_') manifest))

let test_lens_fetch_first () =
  (* the offline-first fetch order: lens substrate before the wire. We assert
     that Remote.fetch_def consults the lens [lens_lookup] first by giving it
     a transport that would FAIL, so only a substrate hit can satisfy it. *)
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    ignore
      (materialize_code_node ~lens ~branch:"lens-laptop" ~kind:"fn"
         ~name:"off" ~src:"(define (off) 7)" ~doc:"" ~author:"admin");
    record_ancestry ~lens [ "lens-laptop" ];
    let lookup name =
      match resolve_def ~lens ~kind:"fn" ~name with
      | Some env -> code_src_of_envelope env
      | None -> None
    in
    let fail_transport (_ : string) (_ : Pricklypear_lisp.Ast.value list) :
        (Pricklypear_lisp.Ast.value, exn) result =
      Error (Failure "wire must not be touched")
    in
    let client = Remote.make ~transport:fail_transport ~lens_lookup:lookup () in
    (* offline-first: resolves from the substrate without touching the wire *)
    match Remote.fetch_def client "off" with
    | Some src -> Alcotest.(check string) "lens-first src" "(define (off) 7)" src
    | None -> Alcotest.fail "fetch_def None despite substrate hit")

let test_delete_node () =
  (* Ours_del mirror: the server deleted the node, the client's stale
     copy goes too. delete_node removes the envelope file, prunes empty
     parents, and is false on an absent path. *)
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    let path = "data.u.admin.todo.gone" in
    ignore
      (materialize ~lens
         (Yojson.Safe.to_string
            (`Assoc [ ("v", `Int 1); ("kind", `String "datum");
                      ("path", `String path);
                      ("type", `String "todo"); ("body", `Assoc []);
                      ("author", `String "admin"); ("hash", `String "h9");
                      ("created_at", `String "t"); ("updated_at", `String "t") ])));
    Alcotest.(check bool) "node present before delete" true
      (get ~lens path <> None);
    Alcotest.(check bool) "delete removes" true (delete_node ~lens path);
    Alcotest.(check bool) "node gone" true (get ~lens path = None);
    Alcotest.(check bool) "delete absent is false" false
      (delete_node ~lens path))

let test_base_manifest () =
  (* the sync common ancestor, client side: record the last-synced merged
     manifest, read it back as the next round's base; it must NOT appear
     in the node manifest (metadata file). *)
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    Alcotest.(check (list (pair string string))) "no base on a fresh lens" []
      (base_manifest ~lens);
    record_base_manifest ~lens
      [ ("data.u.admin.todo.a", "h1"); ("data.u.admin.todo.b", "h2") ];
    let base = base_manifest ~lens in
    Alcotest.(check int) "two base entries" 2 (List.length base);
    Alcotest.(check bool) "base entry round-trips" true
      (List.mem_assoc "data.u.admin.todo.a" base);
    (* a datum node coexisting with the base file still manifests; the
       base file itself never does *)
    ignore
      (materialize ~lens
         (Yojson.Safe.to_string
            (`Assoc [ ("v", `Int 1); ("kind", `String "datum");
                      ("path", `String "data.u.admin.todo.a");
                      ("type", `String "todo"); ("body", `Assoc []);
                      ("author", `String "admin"); ("hash", `String "h1");
                      ("created_at", `String "t"); ("updated_at", `String "t") ])));
    let manifest = node_manifest ~lens in
    Alcotest.(check (list string)) "manifest holds the datum only"
      [ "data.u.admin.todo.a" ] (List.map fst manifest))

(* --- client llm config (sync round metadata, auth.user-llm-keys 5b) ---
   __config.json is lens-root metadata: excluded from node_manifest,
   sync-provided key/base record, and a locally chosen model SURVIVES a
   later sync round (per-surface dial, never replicated). *)
let test_client_config () =
  with_tmpdir (fun root ->
    let lens = lens_of ~root in
    (* absent -> empty *)
    let c0 = config ~lens in
    Alcotest.(check bool) "no config yet" true
      (c0.api_key = None && c0.base = None && c0.model = None);
    (* a sync round records key + base *)
    record_sync_config ~lens ~api_key:"sk-test-ab12" ~base:"https://llm.example/v1";
    let c1 = config ~lens in
    Alcotest.(check (option string)) "key landed"
      (Some "sk-test-ab12") c1.api_key;
    Alcotest.(check (option string)) "base landed"
      (Some "https://llm.example/v1") c1.base;
    Alcotest.(check (option string)) "no model yet" None c1.model;
    (* the file must NOT appear in the sync manifest (metadata, not corpus) *)
    Alcotest.(check bool) "config not in manifest" false
      (List.exists (fun (p, _) -> p = "__config.json") (node_manifest ~lens));
    (* a local model choice survives the NEXT sync round *)
    write_config ~lens { c1 with model = Some "hf:local/dial" };
    record_sync_config ~lens ~api_key:"sk-new-cd34" ~base:"https://llm2.example/v1";
    let c2 = config ~lens in
    Alcotest.(check (option string)) "sync overwrote key"
      (Some "sk-new-cd34") c2.api_key;
    Alcotest.(check (option string)) "sync overwrote base"
      (Some "https://llm2.example/v1") c2.base;
    Alcotest.(check (option string)) "model preserved"
      (Some "hf:local/dial") c2.model)

let () =
  Random.self_init ();
  Alcotest.run "lens_store"
    [ ("lens-model", [ Alcotest.test_case "fork_lens_name" `Quick test_fork_lens_name ])
    ; ("replication-units", [ Alcotest.test_case "in_replication_unit" `Quick test_replication_unit ])
    ; ("local-substrate"
       , [ Alcotest.test_case "path_to_file" `Quick test_path_to_file
         ; Alcotest.test_case "materialize_and_get" `Quick test_materialize_and_get
         ; Alcotest.test_case "put_body_hash" `Quick test_put_body_hash
         ; Alcotest.test_case "sync_convergence_pull" `Quick test_sync_convergence_pull
         ; Alcotest.test_case "subtree_list" `Quick test_subtree_list
         ; Alcotest.test_case "code_kind_path" `Quick test_code_kind_path
         ; Alcotest.test_case "materialize_code_and_resolve" `Quick test_materialize_code_and_resolve
         ; Alcotest.test_case "node_manifest" `Quick test_node_manifest
         ; Alcotest.test_case "delete_node" `Quick test_delete_node
         ; Alcotest.test_case "base_manifest" `Quick test_base_manifest
         ; Alcotest.test_case "lens_fetch_first" `Quick test_lens_fetch_first ])
    ; ("client-config", [ Alcotest.test_case "record_and_preserve" `Quick test_client_config ])
    ]
