(** pp-remote — a Nopales client process (borge/remote-eval.borg client).

    Embeds the pure lisp interpreter, lazily materializes function
    definitions from a remote branch, and proxies Prim_call effects to
    the image over HTTP. Pure compute stays local; everything else
    crosses the wire to POST /api/prim. The store stays the single
    source of truth — quit the client, lose nothing but REPL state.

      export PP_URL=http://127.0.0.1:18080
      export PP_USER=admin PP_PASS=admin
      pp-remote '(+ 1 2)'          # one expr, print, exit
      pp-remote                    # stdin REPL

    Flags:
      --image URL    base URL of the image (default: PP_URL / 127.0.0.1:18080)
      --user NAME    basic-auth user (default: PP_USER / admin)
      --pass PASS    basic-auth pass (default: PP_PASS / admin)
      -e EXPR        eval one expr and exit (repeatable)
      -p EXPR        alias of -e (pipe mode)
*)

open Pricklypear_lisp

let getenv name ~default =
  match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | Some _ | None -> default

let () =
  let image = ref None in
  let user = ref None in
  let pass = ref None in
  let exprs = ref [] in
  let anon _ = () in
  let speclist =
    Arg.align
      [
        ("--image", Arg.String (fun s -> image := Some s), "URL image base");
        ("--user", Arg.String (fun s -> user := Some s), "NAME basic-auth user");
        ("--pass", Arg.String (fun s -> pass := Some s), "PASS basic-auth pass");
        ("-e", Arg.String (fun s -> exprs := s :: !exprs), "EXPR eval and exit");
        ("-p", Arg.String (fun s -> exprs := s :: !exprs), "EXPR eval and exit");
      ]
  in
  Arg.parse speclist anon "pp-remote [--image URL] [--user U --pass P] [-e EXPR]";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let cfg : Http_transport.cfg =
    {
      base_url =
        Option.value !image ~default:(getenv "PP_URL" ~default:"http://127.0.0.1:18080");
      user = Option.value !user ~default:(getenv "PP_USER" ~default:"admin");
      password = Option.value !pass ~default:(getenv "PP_PASS" ~default:"admin");
    }
  in
  let transport = Http_transport.transport ~env ~sw ~cfg in
  (* Hybrid placement: Local evaluates under the proxy rim; Remote
     (prim-dense / transactional) is POSTed to /api/eval wholesale. *)
  let remote_eval = Http_transport.eval_transport ~env ~sw ~cfg in
  let t = Remote.make ~transport ~remote_eval () in
  let eval_print src =
    try
      let v = Remote.eval t src in
      print_endline (Reader.pp_value v)
    with
    | Ast.Interp_error ie ->
        Printf.eprintf "error: %s\n%!" (Ast.pp_interp_error ie); exit 1
    | Reader.Reader_error (msg, ln, col) ->
        Printf.eprintf "read error %d:%d %s\n%!" ln col msg; exit 1
  in
  match List.rev !exprs with
  | [] ->
      (* stdin REPL: same env, same prims, same proxy rim. *)
      Printf.printf "pp-remote — %s  user=%s\n%!" cfg.base_url cfg.user;
      print_endline "enter Nopales forms; :quit to exit";
      let rec loop () =
        output_string stdout "remote> ";
        flush stdout;
        (try
           match input_line stdin |> String.trim with
           | line when line = "" -> loop ()
           | line when line = ":quit" || line = ":q" -> ()
           | line ->
               (try
                  let v = Remote.eval t line in
                  print_endline (Reader.pp_value v)
                with
                | Ast.Interp_error ie ->
                    Printf.printf "error: %s\n%!" (Ast.pp_interp_error ie)
                | Reader.Reader_error (msg, ln, col) ->
                    Printf.printf "read error %d:%d %s\n%!" ln col msg);
               loop ()
         with End_of_file -> print_newline ())
      in
      loop ()
  | xs -> List.iter eval_print xs
