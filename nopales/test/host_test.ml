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
    ]
