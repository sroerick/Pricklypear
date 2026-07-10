(** Pricklypear Auth — Internal RPC HTTP server.

    Endpoints (internal / localhost-bound in v1):
      POST /verify_ssh_key
      POST /verify_password
      POST /mint_token
      POST /verify_token
      POST /admin/create_user
      POST /admin/add_credential
      GET  /admin/list_users
      GET  /list_ssh_credentials
      GET  /lookup/:user_id
      GET  /health

    Admin routes accept X-Admin: 1 (or true) header in v1; mTLS deferred. *)
open Lwt.Syntax
open Lwt.Infix

let default_port = 7879

let json_response (reqd : Httpaf.Reqd.t) ?(status = `OK) (j : Yojson.Safe.t) : unit =
  let s = Yojson.Safe.pretty_to_string j in
  let headers =
    Httpaf.Headers.of_list
      [ ("content-type", "application/json");
        ("content-length", string_of_int (String.length s)); ]
  in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp s

let error_response (reqd : Httpaf.Reqd.t) ~status ~msg : unit =
  json_response reqd ~status (`Assoc [("error", `String msg)])

let read_body (reqd : Httpaf.Reqd.t) : string Lwt.t =
  let body = Httpaf.Reqd.request_body reqd in
  let promise, wakeup = Lwt.task () in
  let buf = Buffer.create 1024 in
  let rec sched () =
    Httpaf.Body.schedule_read body
      ~on_eof:(fun () -> Lwt.wakeup_later wakeup (Buffer.contents buf))
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
        sched ())
  in
  sched ();
  promise

let json_field items name =
  match List.assoc_opt name items with
  | Some (`String s) -> Some s
  | Some (`Bool b) -> Some (string_of_bool b)
  | Some (`Int n) -> Some (string_of_int n)
  | _ -> None

let json_bool items name default =
  match List.assoc_opt name items with
  | Some (`Bool b) -> b
  | Some (`String "true") | Some (`String "1") -> true
  | Some (`String "false") | Some (`String "0") -> false
  | _ -> default

let is_admin_request (req : Httpaf.Request.t) : bool =
  match Httpaf.Headers.get req.headers "x-admin" with
  | Some "1" | Some "true" | Some "TRUE" | Some "yes" -> true
  | _ -> false

let with_json_body reqd (f : (string * Yojson.Safe.t) list -> unit Lwt.t) : unit Lwt.t =
  let* body_str = read_body reqd in
  try
    match Yojson.Safe.from_string body_str with
    | `Assoc items -> f items
    | _ -> error_response reqd ~status:`Bad_request ~msg:"expected JSON object"; Lwt.return_unit
  with Yojson.Json_error e ->
    error_response reqd ~status:`Bad_request ~msg:e; Lwt.return_unit

let request_handler
    ~(store : Store.t)
    (_sockaddr : Unix.sockaddr)
    (reqd : Httpaf.Reqd.t) : unit =
  let req = Httpaf.Reqd.request reqd in
  let target = req.Httpaf.Request.target in
  (* strip query string if present *)
  let path =
    match String.index_opt target '?' with
    | Some i -> String.sub target 0 i
    | None -> target
  in
  let meth = req.Httpaf.Request.meth in
  match meth, path with
  | `GET, "/health" ->
      json_response reqd (`Assoc [("ok", `Bool true); ("service", `String "auth")])

  | `GET, "/list_ssh_credentials" ->
      Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let* creds = Store.list_ssh_credentials store in
            let j =
              `List
                (List.map
                   (fun (u, k) ->
                     `Assoc [("username", `String u); ("ssh_key", `String k)])
                   creds)
            in
            json_response reqd j;
            Lwt.return_unit)
          (fun exn ->
            error_response reqd ~status:`Internal_server_error
              ~msg:(Printexc.to_string exn);
            Lwt.return_unit))

  | `GET, "/admin/list_users" ->
      Lwt.async (fun () ->
        if not (is_admin_request req) then begin
          error_response reqd ~status:`Unauthorized ~msg:"admin required";
          Lwt.return_unit
        end else
          let* users = Admin_api.list_users ~store in
          let j =
            `List
              (List.map
                 (fun (u : Admin_api.user_record) ->
                   `Assoc [ ("id", `String u.id)
                          ; ("username", `String u.username)
                          ; ("is_admin", `Bool u.is_admin) ])
                 users)
          in
          json_response reqd j;
          Lwt.return_unit)

  | `GET, path when String.length path > 8 && String.sub path 0 8 = "/lookup/" ->
      let uid = String.sub path 8 (String.length path - 8) in
      Lwt.async (fun () ->
        let* u = Admin_api.lookup ~store uid in
        match u with
        | None -> error_response reqd ~status:`Not_found ~msg:"user not found"; Lwt.return_unit
        | Some r ->
            json_response reqd
              (`Assoc [ ("id", `String r.id)
                      ; ("username", `String r.username)
                      ; ("is_admin", `Bool r.is_admin) ]);
            Lwt.return_unit)

  | `POST, "/verify_ssh_key" ->
      Lwt.async (fun () ->
        with_json_body reqd (fun items ->
          let key = Option.value (json_field items "key") ~default:"" in
          let req0 = { Pricklypear_common.Auth_types.key } in
          let* resp = Verify.verify_ssh_key ~store req0 in
          json_response reqd
            (Pricklypear_common.Auth_types.verify_ssh_key_resp_to_json resp);
          Lwt.return_unit))

  | `POST, "/verify_password" ->
      Lwt.async (fun () ->
        with_json_body reqd (fun items ->
          let username = Option.value (json_field items "username") ~default:"" in
          let password = Option.value (json_field items "password") ~default:"" in
          let* resp =
            Verify.verify_password ~store ~username ~password ()
          in
          json_response reqd
            (Pricklypear_common.Auth_types.verify_ssh_key_resp_to_json resp);
          Lwt.return_unit))

  | `POST, "/mint_token" ->
      Lwt.async (fun () ->
        with_json_body reqd (fun items ->
          match json_field items "user_id" with
          | None | Some "" ->
              error_response reqd ~status:`Bad_request ~msg:"user_id required";
              Lwt.return_unit
          | Some uid ->
              let* tok_opt = Verify.mint_token ~store uid in
              (match tok_opt with
               | None ->
                   error_response reqd ~status:`Internal_server_error
                     ~msg:"mint failed";
                   Lwt.return_unit
               | Some tok ->
                   json_response reqd
                     (`Assoc [ ("token", `String tok)
                             ; ("user_id", `String uid) ]);
                   Lwt.return_unit)))

  | `POST, "/verify_token" ->
      Lwt.async (fun () ->
        with_json_body reqd (fun items ->
          match json_field items "token" with
          | None | Some "" ->
              error_response reqd ~status:`Bad_request ~msg:"token required";
              Lwt.return_unit
          | Some tok ->
              let* uid_opt = Verify.verify_token ~store tok in
              (match uid_opt with
               | None ->
                   json_response reqd
                     (`Assoc [ ("ok", `Bool false) ]);
                   Lwt.return_unit
               | Some uid ->
                   json_response reqd
                     (`Assoc [ ("ok", `Bool true)
                             ; ("user_id", `String uid) ]);
                   Lwt.return_unit)))

  | `POST, "/admin/create_user" ->
      Lwt.async (fun () ->
        if not (is_admin_request req) then begin
          error_response reqd ~status:`Unauthorized ~msg:"admin required";
          Lwt.return_unit
        end else
          with_json_body reqd (fun items ->
            let username = Option.value (json_field items "username") ~default:"" in
            let is_admin = json_bool items "is_admin" false in
            let* res = Admin_api.create_user ~store ~username ~is_admin () in
            match res with
            | Ok id ->
                json_response reqd
                  (`Assoc [ ("ok", `Bool true)
                          ; ("user_id", `String id)
                          ; ("username", `String username) ]);
                Lwt.return_unit
            | Error msg ->
                error_response reqd ~status:`Bad_request ~msg;
                Lwt.return_unit))

  | `POST, "/admin/add_credential" ->
      Lwt.async (fun () ->
        if not (is_admin_request req) then begin
          error_response reqd ~status:`Unauthorized ~msg:"admin required";
          Lwt.return_unit
        end else
          with_json_body reqd (fun items ->
            let user_id = Option.value (json_field items "user_id") ~default:"" in
            let kind = Option.value (json_field items "kind") ~default:"" in
            let public_data =
              match json_field items "public_data" with
              | Some s -> Some s
              | None -> json_field items "ssh_key"
            in
            let secret_data =
              match json_field items "secret_data" with
              | Some s -> Some s
              | None -> json_field items "password"
            in
            let* res =
              Admin_api.add_credential ~store ~user_id ~kind
                ~public_data ~secret_data
            in
            match res with
            | Ok () ->
                json_response reqd (`Assoc [("ok", `Bool true)]);
                Lwt.return_unit
            | Error msg ->
                error_response reqd ~status:`Bad_request ~msg;
                Lwt.return_unit))

  | (`POST | `GET), _ ->
      error_response reqd ~status:`Not_found ~msg:("unknown route: " ^ path)
  | _ ->
      error_response reqd ~status:`Method_not_allowed ~msg:"method not allowed"

let error_handler
    (_sockaddr : Unix.sockaddr)
    ?(request : Httpaf.Request.t option)
    (err : Httpaf.Server_connection.error)
    (respond : Httpaf.Headers.t -> [> `write ] Httpaf.Body.t) : unit =
  let msg =
    match err with
    | `Bad_request -> "bad request"
    | `Bad_gateway -> "bad gateway"
    | `Internal_server_error -> "internal server error"
    | `Exn exn -> Printexc.to_string exn
  in
  let _ = request in
  let body_str =
    Yojson.Safe.pretty_to_string (`Assoc [("error", `String msg)])
  in
  let headers =
    Httpaf.Headers.of_list
      [ ("content-type", "application/json")
      ; ("content-length", string_of_int (String.length body_str)) ]
  in
  let body_w = respond headers in
  Httpaf.Body.write_string body_w body_str;
  Httpaf.Body.close_writer body_w

let serve ~(store : Store.t) ?(port = default_port) () : unit Lwt.t =
  let establisher =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler ~store)
      ~error_handler
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Printf.eprintf "[auth.http] RPC on 127.0.0.1:%d\n" port;
  let* _server = Lwt_io.establish_server_with_client_socket addr establisher in
  let waiter, _waker = Lwt.wait () in
  waiter
