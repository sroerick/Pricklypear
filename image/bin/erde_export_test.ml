(** erde export: journal -> training trajectories.

    One trajectory file per session that holds a user/assistant pair, the
    user line always written before the assistant line, and no file at all
    for a journal without a pair (so the out dir stays empty). *)

let with_tmp_dir f =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "erde-export-%d-%d" (Unix.getpid ()) (Random.int 100000))
  in
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> try Sys.remove (Filename.concat dir name) with _ -> ())
        (try Sys.readdir dir with _ -> [||]);
      try Unix.rmdir dir with _ -> ())
    (fun () -> f dir)

let write_file path lines =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> List.iter (fun l -> output_string oc l; output_char oc '\n') lines)

let entry session role content =
  Yojson.Safe.to_string
    (`Assoc
      [
        ("session", `String session);
        ("role", `String role);
        ("content", `String content);
      ])

let read_lines path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let rec go acc =
        match input_line ic with
        | line -> go (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      go [])

let test_no_pair_writes_nothing () =
  with_tmp_dir (fun dir ->
      let journal = Filename.concat dir "journal.jsonl" in
      let out = Filename.concat dir "out" in
      write_file journal
        [ entry "s1" "tool" "ls"; entry "s1" "assistant" "no question" ];
      Unix.mkdir out 0o755;
      let n = Erde_export.export ~journal ~out in
      Alcotest.(check int) "no trajectories written" 0 n;
      Alcotest.(check int) "out dir still empty" 0 (Array.length (Sys.readdir out)))

let test_pairs_write_user_first () =
  with_tmp_dir (fun dir ->
      let journal = Filename.concat dir "journal.jsonl" in
      let out = Filename.concat dir "out" in
      write_file journal
        [
          entry "s1" "user" "how do I sort?";
          entry "s1" "assistant" "with sort";
          entry "s2" "user" "unanswered";
        ];
      Unix.mkdir out 0o755;
      let n = Erde_export.export ~journal ~out in
      Alcotest.(check int) "one trajectory written" 1 n;
      let files = Array.to_list (Sys.readdir out) in
      Alcotest.(check int) "one file on disk" 1 (List.length files);
      let lines = read_lines (Filename.concat out (List.hd files)) in
      Alcotest.(check (list string))
        "user line before assistant line"
        [ "{\"role\":\"user\",\"content\":\"how do I sort?\"}";
          "{\"role\":\"assistant\",\"content\":\"with sort\"}" ]
        lines)

let () =
  Random.self_init ();
  Alcotest.run "erde_export"
    [
      ( "export",
        [
          Alcotest.test_case "no user/assistant pair" `Quick
            test_no_pair_writes_nothing;
          Alcotest.test_case "user before assistant" `Quick
            test_pairs_write_user_first;
        ] );
    ]
