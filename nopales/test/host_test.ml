open Pricklypear_lisp
open Ast

let show = Reader.pp_value

let failf fmt = Printf.ksprintf (fun s -> Alcotest.fail s) fmt

let ok h src =
  match Host.eval_string h src with
  | Ok v -> v
  | Error f -> failf "unexpected failure: %s" (Host.format_failure f)

let check_show name expected v = Alcotest.(check string) name expected (show v)

(* ---- temp dirs ---- *)

let counter = ref 0

let mkdtemp () =
  incr counter;
  let d =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "nopales-test-%d-%d" (Unix.getpid ()) !counter)
  in
  Unix.mkdir d 0o755;
  d

let rec rm_rf p =
  if Sys.is_directory p then begin
    Array.iter (fun e -> rm_rf (Filename.concat p e)) (Sys.readdir p);
    Unix.rmdir p
  end
  else Sys.remove p

let with_tmp f =
  let d = mkdtemp () in
  Fun.protect ~finally:(fun () -> rm_rf d) (fun () -> f d)

let write_file p s =
  let oc = open_out_bin p in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc s)

(* ---- tests ---- *)

let test_forms_in_order () =
  let h = Host.create () in
  check_show "last form wins" "42" (ok h "(define x 41)\n(+ x 1)")

let test_package_form_skipped () =
  let h = Host.create () in
  check_show "deploy metadata skipped" "42" (ok h
    "(package demo (doc \"a package\"))\n42")

let test_std_seed () =
  let h = Host.create () in
  (match Host.load_std h "std.pp" with
   | Ok () -> ()
   | Error f -> failf "std load failed: %s" (Host.format_failure f));
  Alcotest.(check bool) "std_loaded" true (Host.std_loaded h);
  check_show "let* comes from std" "6"
    (ok h "(let* ((a 1) (b (+ a 1))) (* b 3))");
  check_show "when comes from std" "5" (ok h "(when #t 5)")

let test_argv () =
  let h = Host.create ~argv:[ "a"; "b" ] () in
  (match ok h "(car (argv))" with
   | VStr "a" -> ()
   | v -> failf "argv car: %s" (show v));
  check_show "argv length" "2" (ok h "(count (argv))");
  (match ok h "(nth (argv) 1)" with
   | VStr "b" -> ()
   | v -> failf "argv nth: %s" (show v))

let test_print () =
  let b = Buffer.create 16 in
  let h = Host.create ~out:(Buffer.add_string b) () in
  ignore (ok h "(print \"a\" 1) (println \"b\" 2)");
  Alcotest.(check string) "print/println join with spaces" "a 1b 2\n"
    (Buffer.contents b)

let test_fs () =
  with_tmp (fun d ->
      let h = Host.create ~cwd:d () in
      ignore (ok h "(fs/write \"d/x.txt\" \"hi\")");
      (match ok h "(fs/read \"d/x.txt\")" with
       | VStr "hi" -> ()
       | v -> failf "fs/read: %s" (show v));
      (match ok h "(fs/exists? \"d/x.txt\")" with
       | VBool true -> ()
       | v -> failf "fs/exists?: %s" (show v)))

let test_load_relative () =
  with_tmp (fun d ->
      let sub = Filename.concat d "sub" in
      Unix.mkdir sub 0o755;
      write_file (Filename.concat d "a.pp") "(load \"sub/b.pp\")\n";
      write_file (Filename.concat sub "b.pp") "(define (answer) 42)\n";
      let h = Host.create ~cwd:d () in
      (match Host.eval_string h "(load \"a.pp\")" with
       | Ok _ -> ()
       | Error f -> failf "load failed: %s" (Host.format_failure f));
      check_show "definition from loaded file" "42" (ok h "(answer)"))

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then false
    else if String.sub hay i n = needle then true
    else go (i + 1)
  in
  go 0

let test_load_cycle () =
  with_tmp (fun d ->
      write_file (Filename.concat d "a.pp") "(load \"a.pp\")\n";
      let h = Host.create ~cwd:d () in
      match Host.eval_string h "(load \"a.pp\")" with
      | Error f ->
          Alcotest.(check bool) "mentions cycle" true
            (contains f.message "cycle")
      | Ok v -> failf "expected a cycle error, got %s" (show v))

let test_exit () =
  let h = Host.create () in
  match Host.eval_string h "(exit 7)" with
  | exception Host.Exit 7 -> ()
  | exception Host.Exit n -> failf "wrong exit code: %d" n
  | Ok v -> failf "expected exit, got %s" (show v)
  | Error f -> failf "expected exit, got failure: %s" (Host.format_failure f)

let test_error_location () =
  let h = Host.create () in
  match Host.eval_string h "\n  (nope 1)" with
  | Error f ->
      Alcotest.(check int) "line" 2 f.line;
      Alcotest.(check int) "col" 3 f.col;
      Alcotest.(check string) "form head" "nope" f.head;
      Alcotest.(check bool) "message names the unbound var" true
        (String.length f.message > 0)
  | Ok v -> failf "expected an error, got %s" (show v)

let test_parse_error_location () =
  let h = Host.create () in
  match Host.eval_string h "(+ 1" with
  | Error f ->
      Alcotest.(check int) "line" 1 f.line;
      Alcotest.(check bool) "parse error" true
        (String.starts_with ~prefix:"parse error:" f.message)
  | Ok v -> failf "expected a parse error, got %s" (show v)

let mkdir_p dir =
  let rec go d =
    if d = Filename.dirname d || Sys.file_exists d then ()
    else begin
      go (Filename.dirname d);
      Unix.mkdir d 0o755
    end
  in
  go dir

let write_datum root ~owner ~type_name ~id ~created ~fields =
  let path = Lens_store.datum_path ~user:owner ~type_name ~id () in
  let file = Lens_store.path_to_file ~root path in
  mkdir_p (Filename.dirname file);
  let env =
    `Assoc
      [
        ("v", `Int 1);
        ("kind", `String "datum");
        ("path", `String path);
        ("type", `String type_name);
        ("id", `String ("node-" ^ id));  (* tree node id, != row id *)
        ("body", `Assoc fields);
        ("author", `String owner);
        ("hash", `String "");
        ("created_at", `String created);
        ("updated_at", `String created);
      ]
  in
  write_file file (Yojson.Safe.to_string env)

let test_lens_reads () =
  with_tmp (fun d ->
      let owner = "admin" in
      let id1 = "11111111-1111-1111-1111-111111111111" in
      let id2 = "22222222-2222-2222-2222-222222222222" in
      write_datum d ~owner ~type_name:"chord-quality" ~id:id1
        ~created:"2026-01-01 00:00:00+00"
        ~fields:[ ("name", `String "maj7") ];
      write_datum d ~owner ~type_name:"chord-quality" ~id:id2
        ~created:"2026-01-02 00:00:00+00"
        ~fields:[ ("name", `String "min7") ];
      let h = Host.create ~lens_root:d ~cwd:d () in
      (match ok h (Printf.sprintf "(get (quote chord-quality) %S)" id1) with
       | VStr s ->
           Alcotest.(check string) "get body" "{\"name\":\"maj7\"}" s
       | v -> failf "get: %s" (show v));
      let path =
        Lens_store.datum_path ~user:owner ~type_name:"chord-quality" ~id:id1
          ()
      in
      (match ok h (Printf.sprintf "(tree/get %S)" path) with
       | VStr s ->
           Alcotest.(check bool) "tree/get envelope" true
             (contains s "\"kind\":\"datum\"")
       | v -> failf "tree/get: %s" (show v));
      (match ok h "(rows (quote chord-quality) 10)" with
       | VList [ VPair (VStr a, _); VPair (VStr b, _) ] ->
           Alcotest.(check string) "created_at ASC first" id1 a;
           Alcotest.(check string) "created_at ASC second" id2 b
       | v -> failf "rows order: %s" (show v));
      (match ok h "(rows (quote chord-quality) 10 \"created_at DESC\")" with
       | VList (VPair (VStr a, _) :: _) ->
           Alcotest.(check string) "created_at DESC first" id2 a
       | v -> failf "rows desc: %s" (show v));
      (match ok h "(rows (quote chord-quality) 10 \"id\")" with
       | VList (VPair (VStr a, _) :: _) ->
           Alcotest.(check string) "id ASC first" id1 a
       | v -> failf "rows id sort: %s" (show v));
      (match ok h "(rows (quote chord-quality) 1)" with
       | VList [ _ ] -> ()
       | v -> failf "rows limit: %s" (show v));
      (match ok h "(rows (quote no-such-type) 10)" with
       | VList [] -> ()
       | v -> failf "missing type: %s" (show v));
      (match ok h "(get (quote chord-quality) \"deadbeef\")" with
       | VNil -> ()
       | v -> failf "missing get: %s" (show v));
      (match ok h "(tree/get \"data.u.nope\")" with
       | VNil -> ()
       | v -> failf "missing tree/get: %s" (show v));
      match Host.eval_string h
              "(rows (quote chord-quality) 10 \"name\")" with
      | Error f ->
          Alcotest.(check bool) "unsupported sort" true
            (contains f.message "unsupported local sort")
      | Ok v -> failf "expected a sort error, got %s" (show v))

let test_no_lens_get () =
  let h = Host.create () in
  match Host.eval_string h "(get (quote todo) \"x\")" with
  | Error f ->
      Alcotest.(check bool) "get stays unbound without a lens" true
        (contains f.message "unbound: get")
  | Ok v -> failf "expected unbound get, got %s" (show v)

let () =
  Alcotest.run "nopales-host"
    [
      ( "eval",
        [
          Alcotest.test_case "forms in order" `Quick test_forms_in_order;
          Alcotest.test_case "package form skipped" `Quick
            test_package_form_skipped;
          Alcotest.test_case "error location" `Quick test_error_location;
          Alcotest.test_case "parse error location" `Quick
            test_parse_error_location;
        ] );
      ( "stdlib",
        [ Alcotest.test_case "std seed provides let*/when" `Quick test_std_seed ]
      );
      ( "process",
        [
          Alcotest.test_case "argv" `Quick test_argv;
          Alcotest.test_case "print/println" `Quick test_print;
          Alcotest.test_case "exit" `Quick test_exit;
        ] );
      ( "files",
        [
          Alcotest.test_case "fs read/write/exists" `Quick test_fs;
          Alcotest.test_case "relative load" `Quick test_load_relative;
          Alcotest.test_case "load cycle" `Quick test_load_cycle;
        ] );
      ( "lens",
        [
          Alcotest.test_case "get/tree-get/rows" `Quick test_lens_reads;
          Alcotest.test_case "no lens, no get" `Quick test_no_lens_get;
        ] );
    ]
