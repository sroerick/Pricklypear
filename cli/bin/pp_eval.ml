(** pp-eval — eval Lisp on a live Pricklypear image (OCaml CLI, no Python).

    The image is the API: POST /api/eval with HTTP basic auth.

      export PP_URL=https://pricklypear.rocks
      export PP_USER=admin PP_PASS=…
      pp-eval '(+ 1 2)'
      pp-eval -j '(whoami)'
      pp-eval -e '(load-library "todo")' -e '(count (todo/list))'
      pp-eval -i
*)

open Cmdliner

let env_url =
  Pp_client.getenv_default "PP_URL" "http://127.0.0.1:18080"

let env_user = Pp_client.getenv_default "PP_USER" "admin"

let env_timeout =
  match Pp_client.getenv "PP_EVAL_TIMEOUT" with
  | Some s -> (try float_of_string s with _ -> 120.)
  | None -> 120.

let print_result ~(as_json : bool) (r : Pp_client.eval_result) : int =
  if as_json then begin
    let j =
      match r.ok, r.value, r.error with
      | true, Some v, _ ->
          `Assoc [ ("ok", `Bool true); ("value", `String v) ]
      | true, None, _ -> `Assoc [ ("ok", `Bool true); ("value", `Null) ]
      | false, _, Some e ->
          `Assoc [ ("ok", `Bool false); ("error", `String e) ]
      | false, _, None ->
          `Assoc
            [
              ("ok", `Bool false);
              ("error", `String (if r.raw = "" then "unknown error" else r.raw));
            ]
    in
    print_endline (Yojson.Safe.to_string j);
    if r.ok then 0 else 1
  end else if r.ok then begin
    (match r.value with Some v -> print_endline v | None -> ());
    0
  end else begin
    let msg =
      match r.error with Some e -> e | None -> "unknown error"
    in
    prerr_endline msg;
    1
  end

(* bumpers M1 (borge/bumpers.borg notices-envelope): print notices the
   server attached, one per stderr line prefixed ";; notice: " so the
   value stream on stdout stays clean (-j JSON stays machine-parseable;
   notices are out-of-band on stderr). Rendered via the shared codec so
   pp-eval emits the SAME text as the lens and line REPL for a given
   notice. *)
let print_notices (r : Pp_client.eval_result) : unit =
  List.iter
    (fun j ->
      match Wire_codec.notice_of_json j with
      | Some n -> prerr_endline (";; notice: " ^ Wire_codec.notice_to_line n)
      | None -> prerr_endline (";; notice: " ^ Yojson.Safe.to_string j))
    r.notices

let run_one ~url ~user ~password ~timeout ~as_json ~quiet expr =
  let r =
    Pp_client.eval_expr ~base_url:url ~user ~password ~expr
      ~timeout_s:timeout ~quiet
  in
  let rc = print_result ~as_json r in
  if not quiet then print_notices r;
  rc

let interactive ~url ~user ~password ~timeout ~as_json ~quiet =
  prerr_endline (Printf.sprintf "[pp-eval] %s  user=%s" url user);
  prerr_endline "[pp-eval] enter Lisp forms; :quit to exit";
  let rec loop rc =
    try
      output_string stdout "pp> ";
      flush stdout;
      let line = input_line stdin |> String.trim in
      if line = "" then loop rc
      else if
        line = ":quit" || line = ":q" || line = "quit" || line = "exit"
      then rc
      else
        let code =
          run_one ~url ~user ~password ~timeout ~as_json ~quiet line
        in
        loop (if code <> 0 then code else rc)
    with End_of_file ->
      prerr_newline ();
      rc
  in
  loop 0

let read_stdin_exprs () =
  if Unix.isatty Unix.stdin then []
  else
    let rec go acc =
      try
        let line = input_line stdin |> String.trim in
        if line = "" || (String.length line > 0 && line.[0] = ';') then
          go acc
        else go (line :: acc)
      with End_of_file -> List.rev acc
    in
    go []

let main url user password password_file timeout as_json quiet interactive_flag
    evals expr_opt =
  let password =
    Pp_client.resolve_password ~password ~password_file
  in
  let token = Pp_client.getenv_default "PP_TOKEN" "" in
  if password = "" && String.trim token = "" then begin
    prerr_endline
      "pp-eval: set PP_PASS or PP_PASS_FILE or --password (or PP_TOKEN)";
    2
  end else if interactive_flag then
    interactive ~url ~user ~password ~timeout ~as_json ~quiet
  else
    let exprs = List.rev evals in
    let exprs =
      match expr_opt with Some e -> exprs @ [ e ] | None -> exprs
    in
    let exprs =
      if exprs = [] then read_stdin_exprs () else exprs
    in
    if exprs = [] then begin
      prerr_endline
        "pp-eval: pass an expression, -e EXPR, stdin, or -i";
      2
    end else
      List.fold_left
        (fun rc expr ->
          let code =
            run_one ~url ~user ~password ~timeout ~as_json ~quiet expr
          in
          if code <> 0 then code else rc)
        0 exprs

(* ---- cmdliner ---- *)

let url =
  let doc = "Image base URL (default PP_URL or http://127.0.0.1:18080)." in
  Arg.(value & opt string env_url & info [ "url" ] ~docv:"URL" ~doc)

let user =
  let doc = "HTTP basic user (default PP_USER or admin)." in
  Arg.(value & opt string env_user & info [ "user" ] ~docv:"USER" ~doc)

let password =
  let doc = "HTTP basic password (or PP_PASS / PP_PASS_FILE)." in
  Arg.(value & opt string "" & info [ "password"; "p" ] ~docv:"PASS" ~doc)

let password_file =
  let doc = "Read password from file." in
  Arg.(
    value & opt string ""
    & info [ "password-file" ] ~docv:"PATH" ~doc)

let timeout =
  let doc = "HTTP timeout seconds (default PP_EVAL_TIMEOUT or 120)." in
  Arg.(value & opt float env_timeout & info [ "timeout" ] ~docv:"SEC" ~doc)

let as_json =
  let doc = "Print full JSON result {ok,value|error} (agent-friendly)." in
  Arg.(value & flag & info [ "j"; "json" ] ~doc)

let quiet =
  let doc =
    "Quiet bumpers: send {\"quiet\": true} to the image (server \
     suppresses the notices field entirely) and skip notice printing \
     (borge/bumpers.borg notices-envelope M1)."
  in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let interactive_flag =
  let doc = "Interactive multi-turn session." in
  Arg.(value & flag & info [ "i"; "interactive" ] ~doc)

let evals =
  let doc = "Expression to eval (repeatable, in order)." in
  Arg.(
    value & opt_all string []
    & info [ "e"; "eval" ] ~docv:"EXPR" ~doc)

let expr_opt =
  let doc = "Single Lisp expression." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"EXPR" ~doc)

let term =
  Term.(
    const main $ url $ user $ password $ password_file $ timeout
    $ as_json $ quiet $ interactive_flag $ evals $ expr_opt)

let cmd =
  let doc = "Eval Lisp on a live Pricklypear image (no Python)." in
  let info = Cmd.info "pp-eval" ~doc in
  Cmd.v info term

let () = exit (Cmd.eval' cmd)
