(* remote/test/agent_memory_test.ml — the stage-1 memory verbs against a
   synthetic lens: unstamped put, re-put updates in place (ver bump, uat
   refresh, cat preserved), and a drain-compatible tombstone on delete. *)

open Agent_memory

let rec mkdir_p d =
  if d <> "/" && d <> "." && not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    match Unix.mkdir d 0o755 with
    | () -> ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let mk_lens () =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "pp-am-test-%d" (Unix.getpid ()))
  in
  mkdir_p root;
  { Lens_store.root; owner = "gregor"; device = "test"; branch = "lens-test";
    generation = 0L }

let body_of lens path =
  match Lens_store.get ~lens path with
  | None -> Alcotest.fail ("missing node " ^ path)
  | Some json -> (
      match Yojson.Safe.from_string json with
      | `Assoc kv -> (
          (match List.assoc_opt "hash" kv with
           | Some (`String "") -> ()
           | _ -> Alcotest.fail "put must write an UNSTAMPED envelope");
          match List.assoc_opt "body" kv with
          | Some (`Assoc b) -> b
          | _ -> Alcotest.fail "no body")
      | _ -> Alcotest.fail "not an envelope")

let getf b k =
  match List.assoc_opt k b with Some (`String s) -> s | _ -> ""

let () =
  Alcotest.run "agent_memory"
    [
      ( "verbs",
        [
          Alcotest.test_case "put creates an unstamped node" `Quick (fun () ->
              let lens = mk_lens () in
              let r =
                put ~lens ~mid:"mem-1" ~title:"t" ~content:"c"
                  ~tags:[ "a"; "b" ] ~scope:"workspace" ~ws:"/home/al"
                  ~src:"test"
              in
              Alcotest.(check bool) "created" true r.created;
              let b = body_of lens r.tree_path in
              Alcotest.(check string) "mid" "mem-1" (getf b "mid");
              Alcotest.(check string) "ver" "1" (getf b "ver");
              Alcotest.(check string) "content" "c" (getf b "content");
                Alcotest.(check string) "tags json" "[\"a\",\"b\"]"
                (getf b "tags"));
          Alcotest.test_case "re-put updates in place" `Quick (fun () ->
              let lens = mk_lens () in
              let r1 =
                put ~lens ~mid:"mem-2" ~title:"t1" ~content:"c1" ~tags:[]
                  ~scope:"workspace" ~ws:"/w" ~src:"test"
              in
              let cat1 = getf (body_of lens r1.tree_path) "cat" in
              let r2 =
                put ~lens ~mid:"mem-2" ~title:"t2" ~content:"c2" ~tags:[]
                  ~scope:"workspace" ~ws:"/w" ~src:"test"
              in
              Alcotest.(check bool) "updated" false r2.created;
              Alcotest.(check string) "same node" r1.node_id r2.node_id;
              let b = body_of lens r2.tree_path in
              Alcotest.(check string) "ver bumped" "2" (getf b "ver");
              Alcotest.(check string) "title replaced" "t2" (getf b "title");
              Alcotest.(check string) "cat preserved" cat1 (getf b "cat"));
          Alcotest.test_case
            "delete enqueues a drain-compatible tombstone" `Quick
            (fun () ->
              let lens = mk_lens () in
              let r =
                put ~lens ~mid:"mem-3" ~title:"t" ~content:"c" ~tags:[]
                  ~scope:"workspace" ~ws:"/w" ~src:"test"
              in
              let dir =
                Filename.concat (Filename.get_temp_dir_name ())
                  (Printf.sprintf "pp-am-tombs-%d" (Unix.getpid ()))
              in
              mkdir_p dir;
              let dashed = normalize_address r.node_id in
              let d =
                delete ~lens ~type_:"agent-memory" ~address:r.node_id
                  ~tombstone_dir:dir ()
              in
              Alcotest.(check bool) "file removed" true
                (Option.is_some d.removed_file);
              Alcotest.(check string) "tombstone name"
                ("agent-memory." ^ dashed ^ ".tombstone")
                (Filename.basename d.tombstone);
              Alcotest.(check bool) "node gone from lens" false
                (Sys.file_exists
                   (Lens_store.path_to_file ~root:lens.root r.tree_path)));
          Alcotest.test_case "normalize accepts dashed + dashless" `Quick
            (fun () ->
              let n = normalize_address "2cd5dd5e-8ad5-4f0e-9a4d-1e2b3c4d5e6f" in
              Alcotest.(check string) "dashed kept"
                "2cd5dd5e-8ad5-4f0e-9a4d-1e2b3c4d5e6f" n;
              Alcotest.(check string) "dashless re-dashed" n
                (normalize_address "2cd5dd5e8ad54f0e9a4d1e2b3c4d5e6f"));
        ] );
    ]
