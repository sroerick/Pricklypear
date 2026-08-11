(** Prim registration extracted from session.ml (Eio-floor Phase 4). *)
open Session_types

let do_http_get (t : t) (url : string)
    (req_hdrs : (string * string) list)
    : Pricklypear_lisp.Ast.value =
  let open Pricklypear_lisp.Ast in
  let shell_quote = Llm_client.shell_quote in
  let hdr_tmp = Filename.temp_file "pp_http_" ".hdr" in
  let body_tmp = Filename.temp_file "pp_http_" ".body" in
  let cleanup () =
    (try Sys.remove hdr_tmp with _ -> ());
    (try Sys.remove body_tmp with _ -> ())
  in
  let timeout =
    List.assoc_opt "_timeout_ms" req_hdrs
    |> Option.map int_of_string
    |> Option.value ~default:20000
  in
  let real_hdrs =
    List.filter (fun (k, _) -> k <> "_timeout_ms") req_hdrs
  in
  let hdr_args =
    String.concat " "
      (List.map
         (fun (k, v) ->
            shell_quote (k ^ ": " ^ v))
         real_hdrs)
  in
  let cmd =
    Printf.sprintf
      "curl -sS -L --max-redirs 5 -D %s -o %s --max-time %d %s %s 2>&1"
      (shell_quote hdr_tmp)
      (shell_quote body_tmp)
      (max 1 (timeout / 1000))
      hdr_args
      (shell_quote url)
  in
  (try
      let stderr =
        Lwt_eio.run_lwt (fun () ->
          Lwt_process.pread ~timeout:(float_of_int timeout /. 1000. +. 5.)
            (Lwt_process.shell cmd))
      in
      let body =
        try
          let ic = open_in body_tmp in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic; s
        with _ -> ""
      in
      let hdr_blob =
        try
          let ic = open_in hdr_tmp in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic; s
        with _ -> ""
      in
      (* status: parse the last HTTP/x.y NNN line (after redirects). *)
      let status =
        let lines = String.split_on_char '\n' hdr_blob in
        let rec last_status = function
          | [] -> 0
          | line :: rest ->
              let line = String.trim line in
              if String.length line >= 8
                 && String.sub line 0 5 = "HTTP/"
              then
                (match String.split_on_char ' ' line with
                 | _ :: n :: _ ->
                     (try int_of_string (String.trim n) with _ -> last_status rest)
                 | _ -> last_status rest)
              else last_status rest
        in
        last_status lines
      in
      let headers_json = Prims_pure.http_parse_headers hdr_blob in
      let err =
        if stderr = "" then ""
        else if String.length stderr > 5
                && (String.sub stderr 0 5 = "curl "
                    || String.sub stderr 0 5 = "curl:") then
          String.trim stderr
        else ""
      in
      let result_json =
        let open Yojson.Safe in
        `Assoc
          [ ("status", `Int status)
          ; ("headers",
             (try from_string headers_json with _ -> `Null))
          ; ("body", `String body)
          ; ("error", `String err) ]
      in
      cleanup ();
      (VStr (Yojson.Safe.to_string result_json))
    with exn ->
      cleanup ();
      let result_json =
        let open Yojson.Safe in
        `Assoc
          [ ("status", `Int 0)
          ; ("headers", `Null)
          ; ("body", `String "")
          ; ("error", `String ("http-get: " ^ Printexc.to_string exn)) ]
      in
      (VStr (Yojson.Safe.to_string result_json)))

let register (t : t) (tbl : Primitives.t) : unit =
  let deny_anon name =
    raise
      (Pricklypear_lisp.Ast.Interp_error
         (Pricklypear_lisp.Ast.User_error
            (name ^ ": denied for anonymous", "bad-args")))
  in
  (* (sleep seconds) -> VNil after delay. Host-owned concurrency surface
     (BORGE-1785211069). Caps at 60s per call so a runaway form cannot
     park the image forever. Uses Lwt_unix.sleep (yields; does not block
     the OS thread for the full duration under Lwt). *)
  Primitives.register tbl "sleep"
    (fun args ->
      let secs =
        match args with
        | [ Pricklypear_lisp.Ast.VInt n ] -> Int64.to_float n
        | [ Pricklypear_lisp.Ast.VFloat f ] -> f
        | [ Pricklypear_lisp.Ast.VStr s ] ->
            (try float_of_string (String.trim s) with _ -> -1.0)
        | _ -> -1.0
      in
      if secs < 0.0 then
        (Pricklypear_lisp.Ast.VStr "sleep: need non-negative seconds")
      else
        let secs = min secs 60.0 in
        let () = Lwt_eio.run_lwt (fun () -> Lwt_unix.sleep secs) in
        Pricklypear_lisp.Ast.VNil);
  let is_admin_or_agent () =
    let uname =
      match Request_context.get () with
      | Some c -> (Request_context.manifest c).username
      | None -> t.manifest.username
    in
    t.is_admin || String.ends_with ~suffix:"/agent" uname
  in
  Primitives.register tbl "http-get"
    (fun args ->
      let uname =
        match Request_context.get () with
        | Some c -> (Request_context.manifest c).username
        | None -> t.manifest.username
      in
      if uname = "anonymous" then deny_anon "http-get"
      else if not (is_admin_or_agent ()) then
        (Pricklypear_lisp.Ast.VStr
             "http-get: denied (admin/agent/human only)")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr url ] ->
            do_http_get t url []
        | [ Pricklypear_lisp.Ast.VStr url; Pricklypear_lisp.Ast.VStr hdrs_json ] ->
            let hdrs =
              (try
                 let j = Yojson.Safe.from_string hdrs_json in
                 (match j with
                  | `Assoc kvs ->
                      List.filter_map
                        (fun (k, v) ->
                          match v with
                          | `String s -> Some (k, s)
                          | `Int n -> Some (k, string_of_int n)
                          | _ -> None)
                        kvs
                  | _ -> [])
               with _ -> [])
            in
            do_http_get t url hdrs
        | _ ->
            (Pricklypear_lisp.Ast.VStr "http-get: need url [headers-json]"))

