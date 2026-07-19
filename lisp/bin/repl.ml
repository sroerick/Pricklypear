(** Pricklypear Lisp — standalone dev REPL.

   No image, no Postgres, no SSH. Reads s-expressions from stdin,
   evaluates them, prints results. Useful for testing the interpreter
   without booting the full image.

   Dev convenience only — not the image REPL. The live system uses
   browser /repl, POST /api/eval, and an optional loopback TCP line
   protocol on the image.

   H1.7: interpreter is direct-style via [Control.run_sync]. *)
open Pricklypear_lisp

let prompt = "lisp> "

let run_one env prims input : unit =
  (try
     let v = Eval.eval_string ~prims env input in
     print_endline (Reader.pp_value v)
   with
   | Reader.Reader_error (msg, ln, col) ->
     Printf.printf "Read error (line %d, col %d): %s\n" ln col msg
   | (Ast.Interp_error _ as exn) ->
     Printf.printf "Error: %s\n" (Printexc.to_string exn))

let rec loop env prims =
  print_string prompt;
  flush stdout;
  match input_line stdin with
  | exception End_of_file -> print_endline "bye"
  | line ->
    if String.trim line = "" then loop env prims
    else
      let () =
        if not (Reader.is_complete line) then
          let buf = Buffer.create 64 in
          Buffer.add_string buf line;
          let rec more () =
            print_string "  ... ";
            flush stdout;
            (match input_line stdin with
             | exception End_of_file -> print_endline "(aborted)"
             | l ->
                Buffer.add_char buf '\n';
                Buffer.add_string buf l;
                if Reader.is_complete (Buffer.contents buf) then ()
                else more ())
          in
          more ();
          run_one env prims (Buffer.contents buf)
        else
          run_one env prims line
      in
      loop env prims

let () =
  let env = Env.create () in
  let prims = Eval.make_prim_table () in
  Eval.register_prim prims "+"
    (fun args ->
      let n =
        List.fold_left (fun acc v ->
          match v with Ast.VInt i -> Int64.add acc i | _ -> acc) 0L args
      in
      Ast.VInt n);
  Eval.register_prim prims "println"
    (fun args ->
      List.iter (fun v -> print_endline (Reader.pp_value v)) args;
      Ast.VNil);
  print_endline "Pricklypear Lisp dev REPL. Ctrl-D to quit.";
  loop env prims
