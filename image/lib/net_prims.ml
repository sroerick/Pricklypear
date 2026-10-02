(** Pricklypear Image — Network primitives (single-image.net-prims).

    v1: EGRESS ONLY. (tcp-connect host port) opens an outbound TCP
    connection via the OCaml Unix module (synchronous); stream-read/
    write/close operate on an opaque handle stored in the session's
    [net_streams] table.

    Ingress (tcp-listen, spawn-service) and the Eio.Net-native path are
    deferred to v2 (BORGE-1785967056, BORGE-1785968883 — Eio switches
    are run-scoped, no standalone make, so persistent handles need a
    service-manager switch wired from boot). v1 uses Unix blocking
    sockets, matching how do_http_get shells out synchronously.

    Capability: anonymous sessions denied; quota on open streams
    (default 16) prevents resource exhaustion. *)

open Pricklypear_lisp.Ast
open Session_types

let max_open_streams = 16

let random_uuid () =
  (* i-1787971303: minted identifiers use the crypto RNG, never the
     MT19937 Random state. *)
  let b = Bytes.of_string (Mirage_crypto_rng.generate 16) in
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let hex i = Printf.sprintf "%02x" (Char.code (Bytes.get b i)) in
  String.concat ""
    [ hex 0; hex 1; hex 2; hex 3; "-"; hex 4; hex 5; "-"; hex 6; hex 7
    ; "-"; hex 8; hex 9; "-"; hex 10; hex 11; hex 12; hex 13; hex 14; hex 15 ]

let deny_anon t label =
  if t.manifest.username = "anonymous" then
    raise (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.Prim_error
                (label, "denied for anonymous", "denied")))
  else ()

let register (t : Session_types.t) (tbl : Primitives.t) : unit =
  (* (tcp-connect host port) -> handle-id VStr | error VStr *)
  Primitives.register tbl "tcp-connect"
    (fun args ->
      deny_anon t "tcp-connect";
        (match args with
         | [ VStr host; VStr port_s ] ->
             (try
                if Hashtbl.length t.net_streams >= max_open_streams then
                  raise (Pricklypear_lisp.Ast.Interp_error
                           (Pricklypear_lisp.Ast.Prim_error
                              ( "tcp-connect"
                              , Printf.sprintf "quota exceeded (max %d open streams)"
                                  max_open_streams
                              , "denied" )))
                else begin
                  let port =
                    try int_of_string port_s
                    with _ -> failwith "tcp-connect: port must be an integer"
                  in
                  (* Resolve host (IP literal or hostname) *)
                  let inet =
                    try Unix.inet_addr_of_string host
                    with _ ->
                      let he = Unix.gethostbyname host in
                      if Array.length he.Unix.h_addr_list = 0 then
                        failwith "tcp-connect: host not found"
                      else he.Unix.h_addr_list.(0)
                  in
                  (* Egress guard (2026-10 security pass; same envelope as
                     http-get, i-1787971188): raw TCP must not reach
                     loopback/private/link-local space. On prod,
                     127.0.0.1:<PP_REPL_PORT> is the UNAUTHENTICATED line
                     REPL serving the shared BOOT session (admin
                     identity), so a signed-in tenant eval'ing
                     (tcp-connect "127.0.0.1" "17878") would otherwise
                     become admin. No TOCTOU: we connect to the SAME
                     resolved address checked here (no re-resolve).
                     Opt-out for local providers/dev:
                     PP_NET_EGRESS_UNRESTRICTED=1. *)
                  if not (Egress.unrestricted ()) && Egress.addr_is_private inet then
                    raise (Pricklypear_lisp.Ast.Interp_error
                             (Pricklypear_lisp.Ast.Prim_error
                                ( "tcp-connect"
                                , "blocked: target host resolves to a private/loopback/link-local address (egress restricted)"
                                , "denied" )));
                  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
                  Unix.connect sock (Unix.ADDR_INET (inet, port));
                  let id = random_uuid () in
                  Hashtbl.add t.net_streams id sock;
                  (VStr id)
                end
              with exn ->
                (match exn with
                 | Pricklypear_lisp.Ast.Interp_error _ as e ->
                     (* Real denial (quota / egress block) — raise; never
                       funnel a DENIAL into an in-band result string. *)
                     raise e
                 | _ ->
                     (VStr (Printf.sprintf "tcp-connect: %s"
                              (Printexc.to_string exn)))))
         | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tcp-connect", "need host port", "bad-args"))))));

  (* (stream-write handle bytes) -> n-written VInt | error VStr *)
  Primitives.register tbl "stream-write"
    (fun args ->
      deny_anon t "stream-write";
        (match args with
         | [ VStr handle; VStr data ] ->
             (match Hashtbl.find_opt t.net_streams handle with
              | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("stream-write", "no such handle", "not-found")))
              | Some sock ->
                  (try
                     let n = Unix.write sock (Bytes.of_string data) 0
                               (String.length data) in
                     (VInt (Int64.of_int n))
                   with exn ->
                     (VStr (Printf.sprintf "stream-write: %s"
                              (Printexc.to_string exn)))))
         | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("stream-write", "need handle bytes", "bad-args"))))));

  (* (stream-read handle max-bytes) -> bytes VStr | error VStr *)
  Primitives.register tbl "stream-read"
    (fun args ->
      deny_anon t "stream-read";
        (match args with
         | [ VStr handle; VStr max_s ] ->
             (match Hashtbl.find_opt t.net_streams handle with
              | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("stream-read", "no such handle", "not-found")))
              | Some sock ->
                  let max_bytes =
                    try int_of_string max_s with _ -> 4096
                  in
                  let max_bytes = min max_bytes 65536 in
                  (try
                     let buf = Bytes.create max_bytes in
                     let n = Unix.read sock buf 0 max_bytes in
                     (VStr (Bytes.sub_string buf 0 n))
                   with exn ->
                     (VStr (Printf.sprintf "stream-read: %s"
                              (Printexc.to_string exn)))))
         | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("stream-read", "need handle max-bytes", "bad-args"))))));

  (* (stream-close handle) -> ok VStr *)
  Primitives.register tbl "stream-close"
    (fun args ->
      deny_anon t "stream-close";
        (match args with
         | [ VStr handle ] ->
             (match Hashtbl.find_opt t.net_streams handle with
              | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("stream-close", "no such handle", "not-found")))
              | Some sock ->
                  (try Unix.close sock with _ -> ());
                  Hashtbl.remove t.net_streams handle;
                  (VStr "closed"))
         | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("stream-close", "need handle", "bad-args"))))));

  (* (stream-list) -> VList of open handle ids *)
  Primitives.register tbl "stream-list"
    (fun _args ->
      deny_anon t "stream-list";
        let ids =
          Hashtbl.fold (fun k _ acc -> (VStr k) :: acc) t.net_streams []
        in
        (VList ids))
