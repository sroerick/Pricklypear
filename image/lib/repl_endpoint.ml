(** Pricklypear Image — optional TCP REPL endpoint.

    Loopback line-protocol REPL. Still Lwt_unix sockets; boot runs this
    under Lwt_eio.run_lwt from an Eio fiber. Primary interactive surfaces
    remain browser /repl and POST /api/eval. *)
open Lwt.Syntax

let default_port = 7878

let pp_error (e : Pricklypear_lisp.Ast.interp_error) : string =
  let open Pricklypear_lisp.Ast in
  match e with
  | Unbound_var s -> Printf.sprintf "unbound variable: %s" s
  | Not_a_function v ->
      Printf.sprintf "not a function: %s" (Pricklypear_lisp.Reader.pp_value v)
  | Arity_mismatch (n, e_doc, g) ->
      Printf.sprintf "arity mismatch in %s: expected %d, got %d" n e_doc g
  | Wrong_arg_count (n, e_doc, g) ->
      Printf.sprintf "wrong arg count in %s: expected %d, got %d" n e_doc g
  | Type_mismatch (s, _) -> Printf.sprintf "type mismatch: %s" s
  | Budget_exceeded -> "budget exceeded"
  | Prim_error (n, m) -> Printf.sprintf "primitive %s: %s" n m
  | Parse_error (msg, l, c) ->
      Printf.sprintf "parse error: %s (line %d, col %d)" msg l c
  | User_error m -> Printf.sprintf "user error: %s" m

let prompt (s : Session.t) : string =
  Printf.sprintf "%s> " (Session.branch_name s)

let repl_turn ~(session : Session.t) ~(ic : Lwt_io.input_channel)
    ~(oc : Lwt_io.output_channel) : bool Lwt.t =
  let* () = Lwt_io.write oc (prompt session) in
  let* () = Lwt_io.flush oc in
  let* line = Lwt_io.read_line_opt ic in
  match line with
  | None -> Lwt.return false
  | Some "" -> Lwt.return true
  | Some input -> (
      match input with
      | ":quit" | ":q" -> Lwt.return false
      | _ ->
          Lwt.catch
            (fun () ->
              let result = Session.eval_one session input in
              let output = Pricklypear_lisp.Reader.pp_value result in
              let* () = Lwt_io.write oc (output ^ "\n") in
              let* () = Lwt_io.flush oc in
              Lwt.return true)
            (fun exn ->
              let msg =
                match exn with
                | Pricklypear_lisp.Ast.Interp_error e -> pp_error e
                | _ -> Printexc.to_string exn
              in
              let* () = Lwt_io.write oc ("error: " ^ msg ^ "\n") in
              let* () = Lwt_io.flush oc in
              Lwt.return true))

let serve ~(session : Session.t) ?(port = default_port) () : unit Lwt.t =
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind sock addr in
  Lwt_unix.listen sock 5;
  Printf.eprintf "[repl] listening on 127.0.0.1:%d\n" port;
  flush stderr;
  let rec loop () =
    let* fd, _peer = Lwt_unix.accept sock in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    let* () =
      Lwt_io.write oc "Pricklypear REPL. Type :quit to exit.\n"
    in
    let* () = Lwt_io.flush oc in
    let rec turn_loop () =
      let* cont = repl_turn ~session ~ic ~oc in
      if cont then turn_loop () else Lwt.return_unit
    in
    let* () =
      Lwt.catch turn_loop (fun exn ->
          (match exn with
           | Unix.Unix_error (Unix.EBADF, _, _) ->
               Printf.eprintf "[repl] peer disconnected\n"
           | _ ->
               Printf.eprintf "[repl] connection error: %s\n"
                 (Printexc.to_string exn));
          flush stderr;
          Lwt.return_unit)
    in
    let* () = Lwt.catch (fun () -> Lwt_io.close ic) (fun _ -> Lwt.return_unit) in
    let* () = Lwt.catch (fun () -> Lwt_io.close oc) (fun _ -> Lwt.return_unit) in
    let* () =
      Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
    in
    loop ()
  in
  loop ()
