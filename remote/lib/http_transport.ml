(** remote/lib/http.ml (borge/prim-seam.borg client).

    The production [Proxy_rim.transport]: POST /api/prim to the image
    over the ONE shared HTTP/1.1 client (httpc/pp_http.ml, plain or
    TLS; i-1788671008-e — this module hosted the hand-rolled Eio-flow
    original the client was extracted from). No curl.

    Auth is HTTP Basic (user:pass base64) by default; a configured
    [token] (PP_TOKEN, the delegated agent identity) takes over as
    Bearer when no password is given explicitly. Same path /api/eval and
    /api/prim accept on the image. Identity is server-asserted: the
    client only presents credentials; whoami/current-author come back
    FROM the server. The transport returns (value, exn) Result so it
    slots directly into [Proxy_rim.run] — errors are re-raised by the
    rim via [Prims_pure.error_of_json] so the client sees identical
    exceptions to in-process eval. *)

open Pricklypear_lisp
open Ast

type cfg = {
  base_url : string;   (* e.g. http://127.0.0.1:18080 *)
  user : string;
  password : string;
  token : string option;  (* PP_TOKEN: Bearer, wins when no password set *)
}

let trim_url base =
  let base = String.trim base in
  if base = "" then "http://127.0.0.1:18080"
  else if base.[String.length base - 1] = '/' then
    String.sub base 0 (String.length base - 1)
  else base

(** Split a http(s)://host[:port] base into (host, port, tls).
    RETIRED (i-1788671008-e): Pp_http.split_base is the ONE URL split
    now; this copy died with the hand-rolled wire it fed. *)

let basic_auth_header ~user ~password =
  let cred = user ^ ":" ^ password in
  let b64 = Base64.encode_string ~pad:true cred in
  ("authorization", "Basic " ^ b64)

(** Bearer when a token is configured (the delegated identity has no
    Basic password); Basic otherwise. Mirrors cli/lib/pp_client.ml. *)
let auth_header ~user ~password ~token =
  match token with
  | Some t when String.trim t <> "" ->
      ("authorization", "Bearer " ^ String.trim t)
  | _ -> basic_auth_header ~user ~password

(** Generic one-POST: POST [path] with [payload] (a JSON value, encoded
    as the request body) and return (parsed-response-JSON | Error msg).
    The wire is the ONE shared client (httpc/pp_http.ml,
    i-1788671008-e) — this was the hand-rolled Eio-flow original the
    client was extracted from. [sw] is retained in the signature (the
    rim/bin seam passes its switch) but the wire runs on Pp_http's own
    internal switch; caller cancellation still unwinds the request (the
    body executes inline on the calling fiber). Connect/DNS/TLS failures
    that used to CRASH (List.hd on empty getaddrinfo, Result.get_ok on
    TLS auth/config) now degrade to Error strings. Non-2xx responses
    keep the exact ["HTTP %d: %s"] error shape; [~timeout_s] on
    Pp_http.request is deliberately NOT defaulted here — prim/eval
    calls can legitimately run long, so the old wait-forever shape is
    kept (the deadline capability + its tests live in httpc/test). *)
let post_json
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    ~(path : string) ~(payload : Yojson.Safe.t)
    : (Yojson.Safe.t, string) Result.t =
  ignore sw;
  match Pp_http.split_base (trim_url cfg.base_url) with
  | Error e -> Error e
  | Ok endpoint -> (
      match
        Pp_http.request ~env ~endpoint ~meth:"POST" ~path
            ~headers:
              [ auth_header ~user:cfg.user ~password:cfg.password
                  ~token:cfg.token ]
          ~body:(Yojson.Safe.to_string payload) ()
      with
      | Error e -> Error e
      | Ok r ->
          if r.Pp_http.status >= 200 && r.Pp_http.status < 300 then
            (try Ok (Yojson.Safe.from_string r.Pp_http.body)
             with Yojson.Json_error e -> Error ("bad json: " ^ e))
          else
            Error
              (Printf.sprintf "HTTP %d: %s" r.Pp_http.status r.Pp_http.body))

(** One POST to /api/prim with {name, args:[json…]}. Returns the parsed
    JSON response (or an Error message). *)
let post_prim
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    ~(name : string) ~(args_json : Yojson.Safe.t)
    : (Yojson.Safe.t, string) Result.t =
  post_json ~env ~sw ~cfg ~path:"/api/prim"
    ~payload:(`Assoc [ ("name", `String name); ("args", args_json) ]) 

(** One POST to /api/eval with {expr}. Returns the parsed JSON response
    (or an Error message). *)
let post_eval
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    ~(expr : string) : (Yojson.Safe.t, string) Result.t =
  post_json ~env ~sw ~cfg ~path:"/api/eval"
    ~payload:(`Assoc [ ("expr", `String expr) ])

(** Build a [Proxy_rim.transport] from an Eio env + [cfg]. The
    transport is the function the proxy rim calls per Prim_call: it
    serializes args, POSTs, and returns (Ok value | Error exn). A
    server-only refusal ({ok:false, kind:"server_only"}) is surfaced
    as an [Interp_error (Prim_error …)] so it behaves like any other
    prim failure to the client. *)
let transport
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    : Proxy_rim.transport =
  fun (name : string) (args : value list) ->
    let args_json = `List (List.map Prims_pure.value_to_json args) in
    match post_prim ~env ~sw ~cfg ~name ~args_json with
    | Error msg ->
        Error
          (Ast.Interp_error
             (Ast.Prim_error (name, "transport: " ^ msg, "transport")) : exn)
    | Ok j ->
        let open Yojson.Safe.Util in
        match j |> member "ok" with
        | `Bool true ->
            (try Ok (Prims_pure.json_to_value (j |> member "value"))
             with _ -> Error (Ast.Interp_error (Ast.Prim_error (name, "bad value json", "prim_error")) : exn))
        | _ ->
            let kind = j |> member "kind" |> to_string_option |> Option.value ~default:"error" in
            let msg = j |> member "error" |> to_string_option |> Option.value ~default:"prim failed" in
            (* A server-side error_to_json payload carries the structured
               Interp_error/Reader_error; reconstruct it so the client
               raises the SAME exn. Otherwise (server_only / generic)
               synthesize a Prim_error naming the prim. *)
            (match j |> member "kind" with
             | `String k when k = "interp_error" || k = "parse_error" ->
                 (try Error (Prims_pure.error_of_json j) with _ ->
                  Error (Ast.Interp_error (Ast.Prim_error (name, msg, kind)) : exn))
             | _ -> Error (Ast.Interp_error (Ast.Prim_error (name, msg, kind)) : exn))

(** Build the whole-expression remote eval hook ([Remote.set_remote_eval])
    from an Eio env + [cfg]. POSTs the whole source to /api/eval (the
    image's own eval path, so server-side /api/eval is authoritative
    for placement-Remote expressions) and returns the value, raising the
    SAME exception an in-process eval would (via the shared codec). This
    is the Remote half of hybrid placement: Local runs under the proxy
    rim, Remote (prim-dense / transactional) POSTs here wholesale. *)
let eval_transport
    ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) ~(cfg : cfg)
    : string -> Ast.value =
  fun (expr : string) ->
    match post_eval ~env ~sw ~cfg ~expr with
    | Error msg ->
        raise (Ast.Interp_error (Ast.Prim_error ("api/eval", msg, "transport")))
    | Ok j ->
        let open Yojson.Safe.Util in
        (match j |> member "ok" with
         | `Bool true ->
             (try Prims_pure.json_to_value (j |> member "value")
              with _ ->
                raise (Ast.Interp_error (Ast.Prim_error ("api/eval", "bad value json", "prim_error"))))
         | _ ->
             let kind =
               j |> member "kind" |> to_string_option
               |> Option.value ~default:"error"
             in
             let msg =
               j |> member "error" |> to_string_option
               |> Option.value ~default:"eval failed"
             in
             (match j |> member "kind" with
              | `String k when k = "interp_error" || k = "parse_error" ->
                  (try raise (Prims_pure.error_of_json j)
                   with
                   | Ast.Interp_error _ as e -> raise e
                   | Reader.Reader_error _ as e -> raise e
                   | _ -> raise (Ast.Interp_error (Ast.Prim_error ("api/eval", msg, kind))))
              | _ -> raise (Ast.Interp_error (Ast.Prim_error ("api/eval", msg, kind)))))
