(* remote/test/agent_context_test.ml — the agent-context boot verb's
   digest over a synthetic lens substrate. Mirrors the Autolith
   pp-board-context contract (card 1ce0929f): pinned list_id, columns from
   the todo-list node, DONE/PARKED as counts, bounded output. *)

open Agent_context

let rec mkdir_p d =
  if d <> "/" && d <> "." && not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    match Unix.mkdir d 0o755 with
    | () -> ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let seq = ref 0

let tmp_root () =
  incr seq;
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "pp-ac-test-%d-%d" (Unix.getpid ()) !seq)
  in
  mkdir_p dir;
  dir

let node root typ id body =
  let enc = String.concat "" (String.split_on_char '-' id) in
  let dir =
    Filename.concat root (Filename.concat "data.u.gregor" (Filename.concat "todo" typ))
  in
  mkdir_p dir;
  let file = Filename.concat dir (enc ^ ".json") in
  let oc = open_out file in
  output_string oc
    (Yojson.Safe.to_string
       (`Assoc [ ("id", `String id); ("type", `String typ); ("body", body) ]));
  close_out oc

let board_list = "0b21ffaa-3a67-4bed-a9ac-658c855b4704"

let item fields =
  `Assoc
    ([ ("list_id", `String board_list)
     ; ("state", `String (List.assoc "state" fields))
     ; ("title", `String (List.assoc "title" fields))
     ; ("due", `String (try List.assoc "due" fields with Not_found -> "")) ])

let seed dir =
  (* the board todo-list node: the FILENAME is the dash-stripped address *)
  node dir "todo-list" board_list
    (`Assoc
      [ ("title", `String "Inbox")
      ; ("columns", `List [ `String "SPEC"; `String "CODE"; `String "DONE"; `String "PARKED" ])
      ]);
  node dir "todo-item" "11111111-2222-4333-8444-555555555555"
    (item [ ("state", "SPEC"); ("title", "alpha card") ]);
  node dir "todo-item" "66666666-7777-4888-8999-aaaaaaaaaaaa"
    (item
       [ ("state", "CODE")
       ; ("title", "beta card long title that exceeds seventy characters for the abbrev check padding pad")
       ; ("due", "2026-09-30") ]);
  node dir "todo-item" "bbbbbbbb-cccc-4ddd-8eee-ffffffffffff"
    (item [ ("state", "DONE"); ("title", "finished") ]);
  (* other list -> excluded from the digest *)
  node dir "todo-item" "12121212-3434-4565-8787-909090909090"
    (`Assoc
      [ ("list_id", `String "other-list"); ("state", `String "SPEC")
      ; ("title", `String "stray card"); ("due", `String "") ]);
  (* malformed json -> skipped, not fatal *)
  let bad = Filename.concat dir "data.u.gregor/broken" in
  mkdir_p bad;
  let oc = open_out (Filename.concat bad "x.json") in
  output_string oc "{not json";
  close_out oc

let contains re s =
  try
    ignore (Str.search_forward (Str.regexp re) s 0);
    true
  with Not_found -> false

let () =
  Alcotest.run "agent_context"
    [
      ( "digest",
        [
          Alcotest.test_case "board digest from lens" `Quick (fun () ->
              let dir = tmp_root () in
              seed dir;
              match digest ~root:dir ~list_id:board_list ~max_chars:1900 with
              | None -> Alcotest.fail "expected a digest"
              | Some d ->
                  Alcotest.(check bool) "title" true (contains "board 'Inbox'" d);
                  Alcotest.(check bool) "counts" true (contains "3 card(s), 2 open" d);
                  Alcotest.(check bool) "due" true (contains "due 2026-09-30" d);
                  Alcotest.(check bool) "id8" true (contains "\\[66666666\\]" d);
                  Alcotest.(check bool) "quiet-done" true (contains "DONE (1)" d);
                    Alcotest.(check bool) "abbrev" true
                      (contains
                         "beta card long title that exceeds seventy characters for the abbrev"
                         d
                      && not (contains "padding pad" d));
                  Alcotest.(check bool) "excluded" true (not (contains "stray" d)));
          Alcotest.test_case "empty lens" `Quick (fun () ->
              let dir = tmp_root () in
              Alcotest.(check (option string)) "none" None
                (digest ~root:dir ~list_id:board_list ~max_chars:1900));
          Alcotest.test_case "bounded" `Quick (fun () ->
              let dir = tmp_root () in
              seed dir;
              match digest ~root:dir ~list_id:board_list ~max_chars:60 with
              | None -> Alcotest.fail "expected a digest"
              | Some d ->
                  Alcotest.(check bool) "bounded" true
                    (String.length d <= 63 && contains "\\.\\.\\.$" d));
        ] );
    ]
