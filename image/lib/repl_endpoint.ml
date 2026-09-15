(** Pricklypear Image — optional TCP REPL endpoint.

    Loopback line-protocol REPL, direct-style on Eio.Net (no runtime
    bridge). Primary interactive surfaces remain browser /repl and
    POST /api/eval. *)

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

let write_str oc (s : string) : unit =
  Eio.Flow.copy_string s oc

(* One client at a time, as before (a shared Session is not fiber-safe
   for REPL turns). Returns false on EOF / :quit. *)
let repl_turn ~(session : Session.t) ~(br : Eio.Buf_read.t)
    ~oc : bool =
  write_str oc (prompt session);
  match Eio.Buf_read.line br with
  | exception End_of_file -> false
  | exception Sys.Break -> false
  | line -> (
      match line with
      | "" -> true
      | ":quit" | ":q" -> false
      | input -> (
          (try
             let result = Session.eval_one session input in
             write_str oc (Pricklypear_lisp.Reader.pp_value result ^ "\n")
           with
           | Pricklypear_lisp.Ast.Interp_error e ->
               write_str oc ("error: " ^ pp_error e ^ "\n")
           | exn -> write_str oc ("error: " ^ Printexc.to_string exn ^ "\n"));
          true))

let serve ~(session : Session.t) ?(port = default_port) () : unit =
  (* promote-window task 7 (2026-09-04): report bind cost (the retry loop
     from task 6 can legally spend up to 10s here; the clean path is ~ms). *)
  let t0 = Unix.gettimeofday () in
  let ctx = Io_eio.Ctx.get () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  (* promote-window task 6 (2026-09-04): old image may still hold the REPL
     port at exec (same teardown race as the HTTP bind). Retry on EADDRINUSE
     for 10s (0.5s cadence, log every ~2s); after the budget fail as before
     — boot.ml's fiber catch keeps REPL failure non-fatal. *)
  let bind_retry_budget = 10.0 and bind_retry_interval = 0.5 in
  let deadline = Unix.gettimeofday () +. bind_retry_budget in
  let rec bind_retry attempts =
    match
      Eio.Net.listen ~sw:ctx.sw ~reuse_addr:true ~backlog:5 ctx.net addr
    with
    | sock -> sock
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _)
      when Unix.gettimeofday () < deadline ->
        let attempts = attempts + 1 in
        if attempts = 1 || attempts mod 4 = 0 then begin
          Printf.eprintf
            "[repl] :%d in use, bind retry %d (budget %.0fs)\n%!"
            port attempts bind_retry_budget;
          flush stderr
        end;
        Io_eio.sleep bind_retry_interval;
        bind_retry attempts
  in
  let sock = bind_retry 0 in
  Printf.eprintf "[repl] listening on 127.0.0.1:%d (bind %d ms)\n" port
    (int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0));
  flush stderr;
  let rec loop () =
    (* One connection at a time on a fresh (child) switch so the socket
       closes when its turn loop returns, BEFORE we recurse (no switch
       nesting across connections); the listener itself is attached to
       the boot switch and outlives connections. *)
    let () =
      Eio.Switch.run @@ fun conn_sw ->
      match Eio.Net.accept ~sw:conn_sw sock with
      | flow, _peer ->
          Eio.Flow.copy_string "Pricklypear REPL. Type :quit to exit.\n" flow;
          let br = Eio.Buf_read.of_flow ~max_size:65536 flow in
          let rec turn_loop () =
            if repl_turn ~session ~br ~oc:flow then turn_loop ()
          in
          (try turn_loop () with
           | Unix.Unix_error (Unix.EBADF, _, _) ->
               Printf.eprintf "[repl] peer disconnected\n"
           | End_of_file -> ()
           | exn ->
               Printf.eprintf "[repl] connection error: %s\n"
                 (Printexc.to_string exn));
          flush stderr
      | exception exn ->
          Printf.eprintf "[repl] accept failed: %s\n%!"
            (Printexc.to_string exn)
    in
    loop ()
  in
  loop ()
