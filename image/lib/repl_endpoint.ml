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
  | Prim_error (n, m, _) -> Printf.sprintf "primitive %s: %s" n m
  | Parse_error (msg, l, c) ->
      Printf.sprintf "parse error: %s (line %d, col %d)" msg l c
  | User_error (m, _) -> Printf.sprintf "user error: %s" m

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
  (* promote-window task 7 (2026-09-04): report bind cost (the retry loop
     from task 6 can legally spend up to 10s here; the clean path is ~ms). *)
  let t0 = Unix.gettimeofday () in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
  (* promote-window task 6 (2026-09-04): old image may still hold the REPL
     port at exec (same teardown race as the HTTP bind). Retry on EADDRINUSE
     for 10s (0.5s cadence, log every ~2s); after the budget fail as before
     — boot.ml's fiber catch keeps REPL failure non-fatal (unchanged). *)
  let bind_retry_budget = 10.0 and bind_retry_interval = 0.5 in
  let deadline = Unix.gettimeofday () +. bind_retry_budget in
  let rec bind_retry attempts =
    Lwt.catch
      (fun () -> Lwt_unix.bind sock addr)
      (function
        | Unix.Unix_error (Unix.EADDRINUSE, _, _)
          when Unix.gettimeofday () < deadline ->
            let attempts = attempts + 1 in
            if attempts = 1 || attempts mod 4 = 0 then begin
              Printf.eprintf
                "[repl] :%d in use, bind retry %d (budget %.0fs)\n%!"
                port attempts bind_retry_budget;
              flush stderr
            end;
            let* () = Lwt_unix.sleep bind_retry_interval in
            bind_retry attempts
        | e -> Lwt.fail e)
  in
  let* () = bind_retry 0 in
  Lwt_unix.listen sock 5;
  Printf.eprintf "[repl] listening on 127.0.0.1:%d (bind %d ms)\n" port
    (int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0));
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
