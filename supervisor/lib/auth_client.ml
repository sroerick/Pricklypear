(** Pricklypear Supervisor — Auth client.

    Per spec [supervisor.ssh-interface] / [auth]: on SSH connect, the
    supervisor asks the auth unikernel to verify the SSH key. Auth is a
    separate unikernel (audit/trust boundary). The supervisor calls it
    via internal HTTP (no TLS — internal network only).

    This is the real implementation: opens a plain TCP connection to
    auth's RPC port, writes a hand-rolled HTTP POST request, reads
    the response, parses the JSON, returns Ok uid / Denied. On any
    error we return Denied (fail-closed: a flaky auth is treated as
    "no") and log the failure. *)
open Lwt.Syntax

type config = {
  host : string;
  port : int;
}

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft
let config_from_env () : config =
  { host = env_or "PP_AUTH_HOST" "127.0.0.1";
    port = int_of_string (env_or "PP_AUTH_PORT" "7879") }

(* Read up to [max_bytes] bytes off [fd] as one chunk with a soft
   timeout. We don't try to read until EOF — auth's response IS small
   (a few hundred bytes) and arrives in one TCP segment typically.
   Returns the bytes read so far when either we see `}` (end of body)
   or no more bytes arrive without blocking. *)
let read_response (fd : Lwt_unix.file_descr) : string Lwt.t =
  let buf = Buffer.create 256 in
  let tmp = Bytes.create 4096 in
  let rec loop attempts =
    if attempts > 50 then Lwt.return (Buffer.contents buf)
    else
      let* n =
        Lwt.catch
          (fun () -> Lwt_unix.read fd tmp 0 4096)
          (fun exn ->
            (* Read error — return what we have. *)
            Printf.eprintf "[supervisor.auth_client] read err: %s\n"
              (Printexc.to_string exn);
            flush stderr;
            Lwt.return 0)
      in
      if n = 0 then
        (* EOF (server closed after Connection: close) — return what we have. *)
        Lwt.return (Buffer.contents buf)
      else begin
        Buffer.add_subbytes buf tmp 0 n;
        let s = Buffer.contents buf in
        (* Heuristic: if the buffer contains a `}` after the last `\r\n`,
           the JSON body is likely complete. *)
        if String.contains s '}' then Lwt.return s
        else loop (attempts + 1)
      end
  in
  loop 0

(* Verify an SSH public key against auth. Makes a real HTTP POST to
   auth's /verify_ssh_key endpoint. Returns [Denied] on any error
   (fail-closed: a flaky auth is treated as "no"). *)
let verify_ssh_key
    (cfg : config)
    (req : Pricklypear_common.Auth_types.verify_ssh_key_req)
    : Pricklypear_common.Auth_types.verify_ssh_key_resp Lwt.t =
  let body =
    Yojson.Safe.to_string
      (`Assoc [ ("key", `String req.key) ])
  in
  let host_hdr = Printf.sprintf "%s:%d" cfg.host cfg.port in
  let request =
    Printf.sprintf
      "POST /verify_ssh_key HTTP/1.1\r\n\
       Host: %s\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      host_hdr (String.length body) body
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, cfg.port) in
  Lwt.catch
    (fun () ->
      let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      let* () = Lwt_unix.connect fd addr in
      (* Write the request synchronously. *)
      let req_bytes = Bytes.of_string request in
      let written = ref 0 in
      let rec write_all () =
        if !written >= Bytes.length req_bytes then Lwt.return_unit
        else
          let* n = Lwt_unix.write fd req_bytes !written
                     (Bytes.length req_bytes - !written) in
          if n = 0 then Lwt.fail (Failure "write returned 0")
          else begin
            written := !written + n;
            write_all ()
          end
      in
      let* () = write_all () in
      (* Now read the response. *)
      let* resp_str = read_response fd in
      let* () =
        Lwt.catch
          (fun () -> Lwt_unix.close fd)
          (fun _ -> Lwt.return_unit)
      in
      Printf.eprintf "[supervisor.auth_client] got %d-byte response\n"
        (String.length resp_str);
      flush stderr;
      (* Find the JSON body: first `{` to last `}`. *)
      let json_str =
        match (String.index_opt resp_str '{', String.rindex_opt resp_str '}') with
        | Some i, Some j when j > i -> String.sub resp_str i (j - i + 1)
        | _ -> resp_str  (* let json parser fail *)
      in
      (match Yojson.Safe.from_string json_str with
       | exception Yojson.Json_error e ->
           Printf.eprintf "[supervisor.auth_client] JSON parse error: %s\n  body was: %s\n"
             e resp_str;
           flush stderr;
           Lwt.return Pricklypear_common.Auth_types.Denied
       | j ->
           Lwt.return
             (Pricklypear_common.Auth_types.verify_ssh_key_resp_of_json j
              |> function
              | Ok r -> r
              | Error e ->
                  Printf.eprintf "[supervisor.auth_client] resp decode failed: %s\n" e;
                  flush stderr;
                  Pricklypear_common.Auth_types.Denied)))
    (fun exn ->
      Printf.eprintf "[supervisor.auth_client] verify_ssh_key failed (auth at %s:%d unreachable?): %s\n"
        cfg.host cfg.port (Printexc.to_string exn);
      flush stderr;
      Lwt.return Pricklypear_common.Auth_types.Denied)

(* List all (username, ssh_key) pairs for users with an ssh_key
   credential. Returns the parsed list, or [] on any error
   (fail-open: the supervisor boots with NO authorized users
   rather than refusing to start — admin can fix-and-restart).
   Used at supervisor boot to build the Awa.Auth.db for awa-ssh
   0.3.1's static-db auth API (see BORGE-1783439434). *)
let list_ssh_credentials
    (cfg : config) : (string * string) list Lwt.t =
  let host_hdr = Printf.sprintf "%s:%d" cfg.host cfg.port in
  let request =
    Printf.sprintf
      "GET /list_ssh_credentials HTTP/1.1\r\n\
       Host: %s\r\n\
       Connection: close\r\n\
       \r\n"
      host_hdr
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, cfg.port) in
  Lwt.catch
    (fun () ->
      let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      let* () = Lwt_unix.connect fd addr in
      let req_bytes = Bytes.of_string request in
      let written = ref 0 in
      let rec write_all () =
        if !written >= Bytes.length req_bytes then Lwt.return_unit
        else
          let* n = Lwt_unix.write fd req_bytes !written
                     (Bytes.length req_bytes - !written) in
          if n = 0 then Lwt.fail (Failure "write returned 0")
          else begin
            written := !written + n;
            write_all ()
          end
      in
      let* () = write_all () in
      let* resp_str = read_response fd in
      let* () =
        Lwt.catch
          (fun () -> Lwt_unix.close fd)
          (fun _ -> Lwt.return_unit)
      in
      Printf.eprintf "[supervisor.auth_client] list_ssh_credentials got %d-byte response\n"
        (String.length resp_str);
      flush stderr;
      let json_str =
        match (String.index_opt resp_str '[', String.rindex_opt resp_str ']') with
        | Some i, Some j when j > i -> String.sub resp_str i (j - i + 1)
        | _ -> "[]"
      in
      (match Yojson.Safe.from_string json_str with
       | exception Yojson.Json_error e ->
           Printf.eprintf "[supervisor.auth_client] list_ssh_credentials JSON parse error: %s\n  body was: %s\n"
             e resp_str;
           flush stderr;
           Lwt.return []
       | `List items ->
           let entries =
             List.filter_map
               (fun item ->
                 match item with
                 | `Assoc kvs ->
                     (match List.assoc_opt "username" kvs,
                             List.assoc_opt "ssh_key" kvs with
                      | Some (`String u), Some (`String k) -> Some (u, k)
                      | _ -> None)
                 | _ -> None)
               items
           in
           Printf.eprintf "[supervisor.auth_client] list_ssh_credentials: %d credential(s)\n"
             (List.length entries);
           flush stderr;
           Lwt.return entries
       | _ ->
           Printf.eprintf "[supervisor.auth_client] list_ssh_credentials: expected a JSON array\n";
           flush stderr;
           Lwt.return []))
    (fun exn ->
      Printf.eprintf "[supervisor.auth_client] list_ssh_credentials failed (auth at %s:%d unreachable?): %s\n"
        cfg.host cfg.port (Printexc.to_string exn);
      flush stderr;
      Lwt.return [])
