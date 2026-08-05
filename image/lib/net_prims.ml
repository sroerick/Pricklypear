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
  let b = Bytes.create 16 in
  for i = 0 to 15 do Bytes.set b i (Char.chr (Random.int 256)) done
  ;
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let hex i = Printf.sprintf "%02x" (Char.code (Bytes.get b i)) in
  String.concat ""
    [ hex 0; hex 1; hex 2; hex 3; "-"; hex 4; hex 5; "-"; hex 6; hex 7
    ; "-"; hex 8; hex 9; "-"; hex 10; hex 11; hex 12; hex 13; hex 14; hex 15 ]

let deny_anon t label =
  if t.manifest.username = "anonymous" then
    Some (VStr (label ^ ": denied for anonymous"))
  else None

let register (t : Session_types.t) (tbl : Primitives.t) : unit =
  (* (tcp-connect host port) -> handle-id VStr | error VStr *)
  Primitives.register tbl "tcp-connect"
    (fun args ->
      match deny_anon t "tcp-connect" with
      | Some v -> v
      | None ->
        (match args with
         | [ VStr host; VStr port_s ] ->
             (try
                if Hashtbl.length t.net_streams >= max_open_streams then
                  (VStr (Printf.sprintf
                      "tcp-connect: quota exceeded (max %d open streams)"
                      max_open_streams))
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
                  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
                  Unix.connect sock (Unix.ADDR_INET (inet, port));
                  let id = random_uuid () in
                  Hashtbl.add t.net_streams id sock;
                  (VStr id)
                end
              with exn ->
                (VStr (Printf.sprintf "tcp-connect: %s"
                          (Printexc.to_string exn))))
         | _ -> (VStr "tcp-connect: need host port")));

  (* (stream-write handle bytes) -> n-written VInt | error VStr *)
  Primitives.register tbl "stream-write"
    (fun args ->
      match deny_anon t "stream-write" with
      | Some v -> v
      | None ->
        (match args with
         | [ VStr handle; VStr data ] ->
             (match Hashtbl.find_opt t.net_streams handle with
              | None -> (VStr "stream-write: no such handle")
              | Some sock ->
                  (try
                     let n = Unix.write sock (Bytes.of_string data) 0
                               (String.length data) in
                     (VInt (Int64.of_int n))
                   with exn ->
                     (VStr (Printf.sprintf "stream-write: %s"
                              (Printexc.to_string exn)))))
         | _ -> (VStr "stream-write: need handle bytes")));

  (* (stream-read handle max-bytes) -> bytes VStr | error VStr *)
  Primitives.register tbl "stream-read"
    (fun args ->
      match deny_anon t "stream-read" with
      | Some v -> v
      | None ->
        (match args with
         | [ VStr handle; VStr max_s ] ->
             (match Hashtbl.find_opt t.net_streams handle with
              | None -> (VStr "stream-read: no such handle")
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
         | _ -> (VStr "stream-read: need handle max-bytes")));

  (* (stream-close handle) -> ok VStr *)
  Primitives.register tbl "stream-close"
    (fun args ->
      match deny_anon t "stream-close" with
      | Some v -> v
      | None ->
        (match args with
         | [ VStr handle ] ->
             (match Hashtbl.find_opt t.net_streams handle with
              | None -> (VStr "stream-close: no such handle")
              | Some sock ->
                  (try Unix.close sock with _ -> ());
                  Hashtbl.remove t.net_streams handle;
                  (VStr "closed"))
         | _ -> (VStr "stream-close: need handle")));

  (* (stream-list) -> VList of open handle ids *)
  Primitives.register tbl "stream-list"
    (fun _args ->
      match deny_anon t "stream-list" with
      | Some v -> v
      | None ->
        let ids =
          Hashtbl.fold (fun k _ acc -> (VStr k) :: acc) t.net_streams []
        in
        (VList ids))
