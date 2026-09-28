(** Prim registration extracted from session.ml (Eio-floor Phase 4). *)
open Session_types

(* outbound-HTTP hardening (i-1787971188) ----------------------------- *)

(* Unique temp file created with perms 0o600 via Unix.openfile O_EXCL —
   never world-readable, no name/symlink race. Filename.temp_file is
   avoided on purpose: creation mode and perms stay explicit here. *)
let make_private_temp (prefix : string) (suffix : string) : string =
  let tmpdir = Filename.get_temp_dir_name () in
  let rec attempt n =
    let path =
      Filename.concat tmpdir
        (prefix ^ string_of_int (Unix.getpid ()) ^ "-" ^ string_of_int n
         ^ suffix)
    in
    (match
       (try
          Some
            (Unix.openfile path
               [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600)
        with Unix.Unix_error (Unix.EEXIST, _, _) -> None)
     with
     | Some fd -> Unix.close fd; path
     | None -> attempt (n + 1))
  in
  attempt 0

(* Host portion of a URL: strip the scheme ("//"), then path/query/
   fragment, userinfo (user:pass@host) and :port; un-bracket IPv6
   literals. Not a full RFC 3986 parser — anything still malformed
   fails later in curl as a transient. *)
let url_host (url : string) : string =
  let cut_at s c =
    match String.index_opt s c with
    | Some i -> String.sub s 0 i
    | None -> s
  in
  let rec after_scheme i =
    if i + 3 > String.length url then None
    else if url.[i] = ':' && url.[i + 1] = '/' && url.[i + 2] = '/' then
      Some (i + 3)
    else after_scheme (i + 1)
  in
  let rest =
    match after_scheme 0 with
    | Some i -> String.sub url i (String.length url - i)
    | None -> url
  in
  let authority = cut_at (cut_at (cut_at rest '/') '?') '#' in
  let authority =
    match String.rindex_opt authority '@' with
    | Some i -> String.sub authority (i + 1) (String.length authority - i - 1)
    | None -> authority
  in
  if String.length authority >= 2 && authority.[0] = '[' then
    (* bracketed IPv6 literal, optional :port after ']' *)
    (match String.index_opt authority ']' with
     | Some i when i > 1 -> String.sub authority 1 (i - 1)
     | _ -> authority)
  else cut_at authority ':'

(* Port portion of a URL for the --resolve pin: explicit :port after the
   host (after ']' for bracketed IPv6), else the scheme default (https
   443, anything else 80). Same parse discipline as url_host. *)
let url_port (url : string) : string option =
  let cut_at s c =
    match String.index_opt s c with
    | Some i -> String.sub s 0 i
    | None -> s
  in
  let rec after_scheme i =
    if i + 3 > String.length url then None
    else if url.[i] = ':' && url.[i + 1] = '/' && url.[i + 2] = '/' then
      Some (i + 3)
    else after_scheme (i + 1)
  in
  let rest =
    match after_scheme 0 with
    | Some i -> String.sub url i (String.length url - i)
    | None -> url
  in
  let authority = cut_at (cut_at (cut_at rest '/') '?') '#' in
  let authority =
    match String.rindex_opt authority '@' with
    | Some i -> String.sub authority (i + 1) (String.length authority - i - 1)
    | None -> authority
  in
  let explicit =
    if String.length authority >= 2 && authority.[0] = '[' then
      (* bracketed IPv6 literal, optional :port after ']' *)
      (match String.index_opt authority ']' with
       | Some i when i + 1 < String.length authority && authority.[i + 1] = ':' ->
           Some (String.sub authority (i + 2) (String.length authority - i - 2))
       | _ -> None)
    else
      (match String.index_opt authority ':' with
       | Some i -> Some (String.sub authority (i + 1) (String.length authority - i - 1))
       | None -> None)
  in
  match explicit with
  | Some p when p <> "" -> Some p
  | _ ->
      let s = String.lowercase_ascii url in
      if String.length s >= 8 && String.sub s 0 8 = "https://" then Some "443"
      else Some "80"

(* Loopback / private / link-local space, v4 and v6. *)
let private_subnets : Ipaddr.Prefix.t list =
  List.map Ipaddr.Prefix.of_string_exn
    [ "127.0.0.0/8"      (* v4 loopback *)
    ; "10.0.0.0/8"       (* RFC 1918 private *)
    ; "172.16.0.0/12"    (* RFC 1918 private *)
    ; "192.168.0.0/16"   (* RFC 1918 private *)
    ; "169.254.0.0/16"   (* v4 link-local (incl. cloud metadata) *)
    ; "::1/128"          (* v6 loopback *)
    ; "fc00::/7"         (* v6 unique-local *)
    ; "fe80::/10"        (* v6 link-local *)
    ]

let addr_is_private (addr : Unix.inet_addr) : bool =
  match Ipaddr.of_string (Unix.string_of_inet_addr addr) with
  | Ok ip -> List.exists (Ipaddr.Prefix.mem ip) private_subnets
  | Error _ -> true (* unparseable: fail closed *)

(** Validate the URL's host against the private-space blocklist and return
    the pin for curl: (host, ip, blocked). i-1788670187: the old shape was
    resolve-then-curl — the guard resolved the host, checked the addresses,
    and curl RE-RESOLVED independently at fetch time, so a rebinding DNS
    answer (TTL 0) walked straight past the blocklist. Now the guard returns
    the address it validated and the caller pins it into curl via
    --resolve host:port:ip, so the fetch uses the SAME address the check
    saw. [blocked] is true when ANY resolved address is private (fail
    closed on unparseable, as before). Unresolvable hosts yield
    blocked=false with no pin (""): curl surfaces resolution failure as
    its own transient, and there is nothing to pin. None when the URL has
    no host (relative/odd) — caller skips both gate and pin. *)
let egress_pin (url : string) : (string * string * bool) option =
  let host = url_host url in
  if host = "" then None
  else
    let addrs =
      (try Unix.getaddrinfo host "80" [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
       with _ -> [])
    in
    match addrs with
    | [] -> Some (host, "", false)
    | _ ->
        let parsed =
          List.filter_map
            (fun ai ->
              match ai.Unix.ai_addr with
              | Unix.ADDR_INET (a, _) ->
                  Some (Unix.string_of_inet_addr a, addr_is_private a)
              | Unix.ADDR_UNIX _ -> Some ("", true))
            addrs
        in
        let blocked = List.exists snd parsed in
        let ip = fst (List.hd parsed) in
        Some (host, ip, blocked)

(* Shared outbound-HTTP engine behind http-get (borge/image.borg
   primitives-layer) and http-post (borge/image.borg outbound-http).
   Same curl-backed shape, same payload-dict contract, same
   transient-vs-semantic discipline; [name] tags Prim_errors, [meth]
   is the HTTP verb, [body] is Some for send verbs (POST:
   --data-binary @file). Hardening (i-1787971188) covers both verbs:
   SSRF egress blocklist, 0600 O_EXCL temp files, Fun.protect
   cleanup. *)
let do_http (name : string)
    (meth : string)
    (body : string option)
    (url : string)
    (req_hdrs : (string * string) list)
    : Pricklypear_lisp.Ast.value =
  let open Pricklypear_lisp.Ast in
  let shell_quote = Llm_client.shell_quote in
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
  (* SSRF guard: refuse targets that resolve into loopback/private/
     link-local space so tenant lisp cannot reach internal services
     (DB, admin faces, cloud metadata). Opt-out: env
     PP_NET_EGRESS_UNRESTRICTED=1.

     TOCTOU CLOSED (i-1788670187): the validated address is PINNED into
     curl via --resolve host:port:ip, so the fetch cannot re-resolve to a
     different (private) address between the check and the connection.

     KNOWN RESIDUAL (documented, accepted): curl -L follows redirects
     itself — a public URL that 3xx-redirects to a NEW hostname re-resolves
     unpinned and can still bounce past this blocklist. The pin covers the
     initial request only. Closing the redirect path needs a local proxy
     or curl metadata hooks; unchanged scope. *)
  let resolve_pin =
    match Sys.getenv_opt "PP_NET_EGRESS_UNRESTRICTED" with
    | Some "1" -> None
    | _ ->
        (match egress_pin url with
         | None -> None
         | Some (host, ip, blocked) ->
             if blocked then
               raise
                 (Interp_error
                    (Prim_error
                       ( name
                       , "blocked: target host resolves to a private/loopback/link-local address (egress restricted)"
                       , "denied" )));
             (match ip with
              | "" -> None
              | ip ->
                  let port = match url_port url with Some p -> p | None -> "80" in
                  Some (Printf.sprintf "%s:%s:%s" host port ip)))
  in
  let hdr_tmp = ref "" and body_tmp = ref "" and req_tmp = ref "" in
  (* Fun.protect keeps hdr/body/req temp files off disk on every path,
     including exceptions from temp creation, curl and parsing. *)
  Fun.protect
    (fun () ->
      try
        hdr_tmp := make_private_temp "pp_http_" ".hdr";
        body_tmp := make_private_temp "pp_http_" ".body";
        (* Send-verb payload: write the body to a private temp file and
           hand curl --data-binary @file (never the command line). *)
        (match body with
         | Some b ->
             let req = make_private_temp "pp_http_" ".req" in
             req_tmp := req;
             let oc = open_out req in
             output_string oc b;
             close_out oc
         | None -> ());
        let meth_args =
          match body with
          | Some _ ->
              Printf.sprintf "-X %s --data-binary @%s" (shell_quote meth)
                (shell_quote !req_tmp)
          | None -> ""
        in
        let pin_arg =
          match resolve_pin with
          | Some pin -> "--resolve " ^ shell_quote pin
          | None -> ""
        in
        let cmd =
          Printf.sprintf
            "curl -sS -L --max-redirs 5 %s -D %s -o %s --max-time %d %s %s %s 2>&1"
            pin_arg
            (shell_quote !hdr_tmp)
            (shell_quote !body_tmp)
            (max 1 (timeout / 1000))
            meth_args
            hdr_args
            (shell_quote url)
        in
        let stderr =
          Io_eio.sh_capture
            ~timeout_s:(float_of_int timeout /. 1000. +. 5.)
            cmd
        in
        let resp_body =
          try
            let ic = open_in !body_tmp in
            let n = in_channel_length ic in
            let s = really_input_string ic n in
            close_in ic; s
          with _ -> ""
        in
        let hdr_blob =
          try
            let ic = open_in !hdr_tmp in
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
        (* D1: network-transient detection. curl writes errors to stderr
           (curl: (NN) ...) or returns non-zero. A status of 0 means no
           HTTP response was received → transient network failure. *)
        let curl_err =
          if stderr = "" then ""
          else if String.length stderr > 5
                  && (String.sub stderr 0 5 = "curl "
                      || String.sub stderr 0 5 = "curl:") then
            String.trim stderr
          else ""
        in
        if status = 0 && curl_err <> "" then
          (* Network transient: DNS, connection refused, timeout, etc.
             RAISE so guard handlers see a real error (design note §4b). *)
          raise
            (Interp_error
               (Prim_error
                  ( name
                  , "transient: " ^ curl_err
                  , "transient" )))
        else if status = 0 then
          (* No status AND no curl error — still a transient (no response). *)
          raise
            (Interp_error
               (Prim_error
                  ( name
                  , "transient: no HTTP response"
                  , "transient" )))
        else
          (* Semantic HTTP response (2xx/3xx/4xx/5xx). Return a payload
             dict with ok/err/kind so callers can branch without string
             parsing. 4xx/5xx are NOT raised — they're real responses the
             caller may want to inspect (design note §4b). *)
          let ok = status < 400 in
          let err = if ok then "" else Printf.sprintf "HTTP %d" status in
          let kind =
            if ok then "ok"
            else if status >= 500 then "http-server-error"
            else "http-client-error"
          in
          let result_json =
            let open Yojson.Safe in
            `Assoc
              [ ("ok", `Bool ok)
              ; ("status", `Int status)
              ; ("headers",
                 (try from_string headers_json with _ -> `Null))
              ; ("body", `String resp_body)
              ; ("err", `String err)
              ; ("kind", `String kind) ]
          in
          (VStr (Yojson.Safe.to_string result_json))
      with exn ->
        match exn with
        | Interp_error _ as e -> raise e
        | _ ->
            raise
              (Interp_error
                 (Prim_error
                    ( name
                    , "transient: " ^ Printexc.to_string exn
                    , "transient" ))))
    ~finally:(fun () ->
      (try if !hdr_tmp <> "" then Sys.remove !hdr_tmp with _ -> ());
      (try if !body_tmp <> "" then Sys.remove !body_tmp with _ -> ());
      (try if !req_tmp <> "" then Sys.remove !req_tmp with _ -> ()))

(* Compatibility alias: http-get engine under its pre-refactor name
   (session.ml binding + http_get_test.ml). *)
let do_http_get (url : string) (req_hdrs : (string * string) list) :
    Pricklypear_lisp.Ast.value =
  do_http "http-get" "GET" None url req_hdrs

let register (t : t) (tbl : Primitives.t) : unit =
  let deny_anon name =
    raise
      (Pricklypear_lisp.Ast.Interp_error
         (Pricklypear_lisp.Ast.User_error
            (name ^ ": denied for anonymous", "bad-args")))
  in
  (* (sleep seconds) -> VNil after delay. Host-owned concurrency surface
     (BORGE-1785211069). Caps at 60s per call so a runaway form cannot
     park the image forever. Uses Io_eio.sleep (yields the fiber; does
     not block the scheduler for the full duration). *)
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
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sleep", "need non-negative seconds", "bad-args"))))
      else
        let secs = min secs 60.0 in
        let () = Io_eio.sleep secs in
        Pricklypear_lisp.Ast.VNil);
  let is_admin_or_agent () =
    let uname =
      match Request_context.get () with
      | Some c -> (Request_context.manifest c).username
      | None -> t.manifest.username
    in
    if t.is_admin || String.ends_with ~suffix:"/agent" uname then true
    else
      (* auth 008: native agents are users with user_type='agent',
         whatever their username (M1 hand-created agent-prickle has no
         "/agent" suffix). One lookup per outbound-http call; the
         lookup is exception-safe (None on DB trouble -> deny closed). *)
      (match Auth_store.get_user_by_username t.store uname with
       | Some u -> u.user_type = "agent"
       | None -> false)
  in
  (* Identity gate shared by http-get / http-post (image.outbound-http):
     SSRF confinement is by CALLER, not by destination allowlist. *)
  let gate_http name =
    let uname =
      match Request_context.get () with
      | Some c -> (Request_context.manifest c).username
      | None -> t.manifest.username
    in
    if uname = "anonymous" then deny_anon name
    else if not (is_admin_or_agent ()) then
      (raise (Pricklypear_lisp.Ast.Interp_error
                (Pricklypear_lisp.Ast.Prim_error
                   (name, "denied (admin/agent/human only)", "denied"))))
  in
  (* headers-json -> (string * string) list; same Assoc format as the
     payload's "headers" field. Unknown shapes parse to []. *)
  (* Headers JSON is STRICT: an object mapping names to string (or int)
     values. Anything else — a non-object, a nested/bool/null value, or
     invalid JSON — is a loud prim error, NOT a silent drop
     (i-hdrs-silent-drop, 2026-09-07: `{"Authorization": {"token": "…"}}`
     used to vanish, the request went out unauthenticated, and the shim
     read its silent success as a kernel LIE — the agent retried a broken
     config forever). Rejection surfaces on the caller's error path (job
     log, /api/eval error JSON, REPL) where a config bug belongs. *)
  let hdrs_of_json ~(prim : string) (hdrs_json : string)
      : (string * string) list =
    let fail msg =
      raise
        (Pricklypear_lisp.Ast.Interp_error
           (Pricklypear_lisp.Ast.Prim_error (prim, msg, "bad-args")))
    in
    let j =
      try Yojson.Safe.from_string hdrs_json
      with e -> fail ("headers-json: invalid JSON: " ^ Printexc.to_string e)
    in
    match j with
    | `Assoc kvs ->
        List.map
          (fun (k, v) ->
             match v with
             | `String s -> (k, s)
             | `Int n -> (k, string_of_int n)
             | _ ->
                 fail
                   (Printf.sprintf
                      "headers-json: value for %S must be a string or int, got %s"
                      k (Yojson.Safe.to_string v)))
          kvs
    | other ->
        fail
          ("headers-json: need a JSON object of string values, got "
           ^ Yojson.Safe.to_string other)
  in
  Primitives.register tbl "http-get"
    (fun args ->
      gate_http "http-get";
      match args with
      | [ Pricklypear_lisp.Ast.VStr url ] ->
          do_http "http-get" "GET" None url []
      | [ Pricklypear_lisp.Ast.VStr url; Pricklypear_lisp.Ast.VStr hdrs_json ] ->
          do_http "http-get" "GET" None url (hdrs_of_json ~prim:"http-get" hdrs_json)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("http-get", "need url [headers-json]", "bad-args")))));
  (* (http-post url [headers-json [body]]) -> payload dict VStr
     {ok,status,headers,body,err,kind}. Same gate + contract as
     http-get (borge/image.borg outbound-http). 4xx/5xx RETURN; curl
     transport failure RAISES transient so jobs backoff sees it. *)
  Primitives.register tbl "http-post"
    (fun args ->
      gate_http "http-post";
      match args with
      | [ Pricklypear_lisp.Ast.VStr url ] ->
          do_http "http-post" "POST" (Some "") url []
      | [ Pricklypear_lisp.Ast.VStr url; Pricklypear_lisp.Ast.VStr hdrs_json ] ->
          do_http "http-post" "POST" (Some "") url (hdrs_of_json ~prim:"http-post" hdrs_json)
      | [ Pricklypear_lisp.Ast.VStr url; Pricklypear_lisp.Ast.VStr hdrs_json;
          Pricklypear_lisp.Ast.VStr body ] ->
          do_http "http-post" "POST" (Some body) url (hdrs_of_json ~prim:"http-post" hdrs_json)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("http-post", "need url [headers-json [body]]", "bad-args")))))
