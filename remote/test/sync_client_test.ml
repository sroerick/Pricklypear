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

let () =
  let open Alcotest in
  run "sync_client"
    [ ( "push_paths"
      , [ test_case "no-base sends everything" `Quick test_no_base_sends_everything
          ; test_case "unchanged nodes skip bodies" `Quick
              test_unchanged_nodes_skip_bodies
          ; test_case "changed + new travel" `Quick test_changed_and_new_travel
          ; test_case "unstamped local write travels" `Quick
              test_locally_written_unstamped_travel
          ; test_case "server-side delete stays quiet" `Quick
              test_server_side_delete_stays_quiet
        ] ) ]
