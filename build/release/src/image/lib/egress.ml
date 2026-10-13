(** Pricklypear Image — the ONE server-side egress policy (leaf module).

    2026-10 security pass: factored verbatim out of prims_net so
    llm_client can enforce the SAME private-space envelope without the
    Prims_net -> Llm_client (shell_quote) -> Prims_net module cycle,
    and so the image holds exactly ONE private-space list. Consumers:
    Prims_net (Nopales http-get / http-post), Llm_client (the
    user-configurable provider base), Net_prims (tcp-connect).

    No Session/Http deps — the same leaf class as http_multipart. *)

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

(** The one opt-out switch for every server-side egress guard (local
    providers, dev environments): PP_NET_EGRESS_UNRESTRICTED=1. *)
let unrestricted () : bool =
  match Sys.getenv_opt "PP_NET_EGRESS_UNRESTRICTED" with
  | Some "1" -> true
  | _ -> false

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
