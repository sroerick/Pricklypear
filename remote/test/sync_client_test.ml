(* sync_client_test.ml — push_paths: bodies only for nodes that changed
   against the recorded base (borge/local-first.borg sync; the quiet
   round POSTs the manifest, not every envelope — nginx 413 at ~1MB). *)

open Sync_client

let test_no_base_sends_everything () =
  let m = [ ("data.u.a.x.1", "h1"); ("data.u.a.x.2", "h2") ] in
  Alcotest.(check (list (pair string string)))
    "no base: whole manifest" m (push_paths ~base:[] m)

let test_unchanged_nodes_skip_bodies () =
  let base = [ ("data.u.a.x.1", "h1"); ("data.u.a.x.2", "h2") ] in
  let m = [ ("data.u.a.x.1", "h1"); ("data.u.a.x.2", "h2") ] in
  Alcotest.(check (list (pair string string)))
    "quiet round: manifest matches base, nothing travels" []
    (push_paths ~base m)

let test_changed_and_new_travel () =
  let base = [ ("data.u.a.x.1", "h1"); ("data.u.a.x.2", "h2") ] in
  (* x1 edited locally (hash moved), x2 untouched, x3 written offline *)
  let m =
    [ ("data.u.a.x.1", "h1-local"); ("data.u.a.x.2", "h2"); ("data.u.a.x.3", "h3") ]
  in
  Alcotest.(check (list (pair string string)))
    "only the edit + the addition travel"
    [ ("data.u.a.x.1", "h1-local"); ("data.u.a.x.3", "h3") ]
    (push_paths ~base m)

let test_locally_written_unstamped_travel () =
  (* a locally-written envelope's manifest hash is "" until the
     convergence pull stamps it; base holds the server hash, so the
     pair differs and the body MUST travel (else the edit never lands) *)
  let base = [ ("data.u.a.x.1", "h1") ] in
  let m = [ ("data.u.a.x.1", "") ] in
  Alcotest.(check (list (pair string string)))
    "unstamped local write travels" [ ("data.u.a.x.1", "") ]
    (push_paths ~base m)

let test_server_side_delete_stays_quiet () =
  (* node deleted server-side since base, client copy untouched: the
     three-way plan mirrors the deletion from the BASE — no body needed *)
  let base = [ ("data.u.a.x.1", "h1"); ("data.u.a.x.2", "h2") ] in
  let m = [ ("data.u.a.x.1", "h1") ] in
  Alcotest.(check (list (pair string string)))
    "client-side-unchanged node needs no body" []
    (push_paths ~base m)

(* ---- delta_of: the client's delta against its recorded base ---- *)

let test_delta_empty () =
  let base = [ ("data.u.a.x.1", "h1") ] in
  let changes, deletes = delta_of ~base base in
  Alcotest.(check (list (pair string string)))
    "manifest == base: no changes" [] changes;
  Alcotest.(check (list string)) "no deletions" [] deletes

let test_delta_change_and_add () =
  let base = [ ("a", "h1"); ("b", "h2") ] in
  let manifest = [ ("a", "h1-new"); ("b", "h2"); ("c", "h3") ] in
  let changes, deletes = delta_of ~base manifest in
  Alcotest.(check (list (pair string string)))
    "edit + addition travel"
    [ ("a", "h1-new"); ("c", "h3") ]
    changes;
  Alcotest.(check (list string)) "no deletions" [] deletes

let test_delta_delete () =
  let base = [ ("a", "h1"); ("b", "h2") ] in
  let manifest = [ ("b", "h2") ] in
  let changes, deletes = delta_of ~base manifest in
  Alcotest.(check (list (pair string string))) "no changes" [] changes;
  Alcotest.(check (list string)) "dropped base path is a deletion" [ "a" ] deletes

(* ---- chunk_bodies: request-sized body chunks (edge-proxy ~1MB cap) ---- *)

let test_chunk_small_single () =
  let bodies = [ ("p1", "{v:1}"); ("p2", "{v:2}") ] in
  (match chunk_bodies ~max_bytes:100_000 bodies with
   | [ only ] ->
       Alcotest.(check int) "one chunk holds both" 2 (List.length only)
   | _ -> Alcotest.fail "expected a single chunk")

let test_chunk_respects_cap () =
  let env = String.make 5000 'x' in
  let bodies = List.init 40 (fun i -> (Printf.sprintf "p%d" i, env)) in
  let chunks = chunk_bodies ~max_bytes:60_000 bodies in
  (* 40 x ~5KB = ~200KB; 60KB cap => 4 chunks *)
  Alcotest.(check int) "splits into 4 chunks" 4 (List.length chunks);
  List.iter
    (fun c -> Alcotest.(check bool) "chunk nonempty" true (c <> []))
    chunks;
  let total =
    List.fold_left (fun acc c -> acc + List.length c) 0 chunks
  in
  Alcotest.(check int) "all bodies preserved across chunks" 40 total

let test_chunk_exact_one () =
  let bodies = [ ("p1", String.make 100 'x') ] in
  (match chunk_bodies ~max_bytes:50 bodies with
   | [ [ _ ] ] -> ()
   | _ -> Alcotest.fail "an oversized single body still travels (own chunk)")

(* ------ the space scope: PULL + PROPOSE, never client-pushed ------ *)

let test_pushable_paths_drop_space_rows () =
  (* space rows (the data.s prefix family) join the manifest for pulls and
     the base, but NEVER travel in a client push — the server is sole
     write authority for spaces (borge/local-first.borg sync, SPACE
     SUBTREES subsection) *)
  let m =
    [ ("data.u." ^ "anon5.testty.1", "h1")
    ; ("data.s.cactus5.memty.9", "h2")
    ; ("data.shared.seedty.4", "h3") ]
  in
  Alcotest.(check (list (pair string string)))
    "space rows dropped, everything else kept"
    [ ("data.u." ^ "anon5.testty.1", "h1"); ("data.shared.seedty.4", "h3") ]
    (pushable_paths m)

let () =
  let open Alcotest in
  run "sync_client"
    [ ( "delta"
      , [ test_case "empty" `Quick test_delta_empty
        ; test_case "change-and-add" `Quick test_delta_change_and_add
        ; test_case "delete" `Quick test_delta_delete
        ] )
    ; ( "chunk_bodies"
      , [ test_case "small-single" `Quick test_chunk_small_single
        ; test_case "respects-cap" `Quick test_chunk_respects_cap
        ; test_case "oversized-single" `Quick test_chunk_exact_one
        ] )
    ; ( "push_paths"
      , [ test_case "no-base sends everything" `Quick test_no_base_sends_everything
          ; test_case "unchanged nodes skip bodies" `Quick
              test_unchanged_nodes_skip_bodies
          ; test_case "changed + new travel" `Quick test_changed_and_new_travel
          ; test_case "unstamped local write travels" `Quick
              test_locally_written_unstamped_travel
          ; test_case "server-side delete stays quiet" `Quick
                test_server_side_delete_stays_quiet
          ] )
    ; ( "pushable_paths"
      , [ test_case "space rows are dropped, others kept" `Quick
              test_pushable_paths_drop_space_rows ] ) ]
