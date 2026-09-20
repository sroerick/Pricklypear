(** glochid habitat bridge — gregor's memory functions (memory-fns)
    + the bindings browser's read verbs (the function grammar).

    mem/* LOCAL prims delegating to the habitat's libs/memory package
    over POST /api/eval (HTTP Basic, the ONE Pp_http client). The
    remote contract is gregor's (borge/agent.borg memory-bridge):
    (mem/remember TYPE MID TITLE CONTENT TAGS) etc., rows keyed by a
    caller-chosen stable string id (mid), fields
    mid/ver/cat/uat/scope/ws/title/content/tags/src, author = the
    CALLER the habitat sees (our env credentials), shared store =
    type "gregor-memory" in space cactus-ops.

    Config (first non-empty wins): AGENT_HABITAT_URL > PP_URL;
    AGENT_HABITAT_USER > PP_USER; AGENT_HABITAT_PASS > PP_PASS.
    Model-facing TOOL "memory" wraps the four core verbs (remember /
    search / get / forget) with the default type. Fail-soft: a broken
    or missing habitat degrades to a Prim_error at CALL time — boot,
    chat turns, and sessions never depend on the bridge. *)

open Pricklypear_lisp
open Ast

let eval_timeout_s = 30.

let getenv ?(default = "") (n : string) : string =
  match Sys.getenv_opt n with Some s when s <> "" -> s | _ -> default

type creds = { url : string; user : string; pass : string }

(** Last-ditch fallback: the pp-standard local secrets file
    (~/.config/pricklypear/env, mode 600 — a SHELL file, values may
    be quoted/expanded). Sourced through /bin/sh exactly like
    pp-eval/curl consumers, so expansion semantics are the operator's
    own; only the three bridge keys are read, values never logged. *)
let env_file_cache : (string * string) list ref = ref []

let env_from_file (env : Eio_unix.Stdenv.base) (key : string) : string =
  (match List.assoc_opt key !env_file_cache with
   | Some v -> v
   | None ->
       let home = Sys.getenv_opt "HOME" |> Option.value ~default:"." in
       let path = Filename.concat home ".config/pricklypear/env" in
       let v =
         if not (Sys.file_exists path) then ""
         else
           let proc_mgr = Eio.Stdenv.process_mgr env in
           let cmd =
             Printf.sprintf "set -a; . %s >/dev/null 2>&1; printf '%%s' \"${%s-}\""
               (Filename.quote path) key
           in
           Eio.Switch.run (fun sw ->
               let out_r, out_w = Eio.Process.pipe ~sw proc_mgr in
               let _proc =
                 Eio.Process.spawn ~sw proc_mgr ~stdout:out_w
                   [ "/bin/sh"; "-c"; cmd ]
               in
               Eio.Flow.close out_w;
               let buf = Buffer.create 128 in
               let cs = Cstruct.create 8192 in
               (try
                  while true do
                    let n = Eio.Flow.single_read out_r cs in
                    Buffer.add_string buf (Cstruct.to_string (Cstruct.sub cs 0 n))
                  done
                with End_of_file -> ());
               String.trim (Buffer.contents buf))
       in
       env_file_cache := !env_file_cache @ [ (key, v) ];
       v)

let file_fallback ~env n =
  match Sys.getenv_opt n with
  | Some v when v <> "" -> v
  | _ -> env_from_file env n

(** Bridge env configuration; None = no habitat reachable from env. *)
let creds ~(env : Eio_unix.Stdenv.base) : creds option =
  let url =
    match getenv "AGENT_HABITAT_URL" with
    | "" -> file_fallback ~env "PP_URL"
    | u -> u
  in
  let user =
    match getenv "AGENT_HABITAT_USER" with
    | "" -> file_fallback ~env "PP_USER"
    | u -> u
  in
  let pass =
    match getenv "AGENT_HABITAT_PASS" with
    | "" -> file_fallback ~env "PP_PASS"
    | p -> p
  in
  if url = "" || user = "" || pass = "" then None
  else Some { url; user; pass }

let trim_url (base : string) : string =
  let base = String.trim base in
  let n = String.length base in
  if n = 0 then "http://127.0.0.1:18080"
  else if base.[n - 1] = '/' then String.sub base 0 (n - 1)
  else base

let basic_auth_header ~(user : string) ~(password : string) : string * string =
  let cred = user ^ ":" ^ password in
  let b64 = Base64.encode_string ~pad:true cred in
  ("authorization", "Basic " ^ b64)

(** POST {"expr": expr} to /api/eval; Ok carries the reply VALUE as
    JSON, Error the failure text. *)
let eval (env : Eio_unix.Stdenv.base) (expr : string)
    : (Yojson.Safe.t, string) Result.t =
  match creds ~env with
  | None ->
      Error
        "habitat bridge not configured: set AGENT_HABITAT_URL/USER/PASS \
         (or PP_URL/PP_USER/PP_PASS)"
  | Some { url; user; pass } -> (
      match Pp_http.split_base (trim_url url) with
      | Error e -> Error ("habitat: " ^ e)
      | Ok endpoint -> (
          let body =
            Yojson.Safe.to_string (`Assoc [ ("expr", `String expr) ])
          in
          match
            Pp_http.request ~timeout_s:eval_timeout_s ~env ~endpoint
              ~meth:"POST" ~path:"/api/eval"
              ~headers:[ basic_auth_header ~user ~password:pass ]
              ~body ()
          with
          | Error e -> Error ("habitat: " ^ e)
          | Ok r ->
              if r.Pp_http.status <> 200 then
                if r.Pp_http.status = 401 then
                  Error "habitat: 401 (bad credentials)"
                else
                  Error
                    (Printf.sprintf "habitat: http %d: %s" r.Pp_http.status
                       (String.sub r.Pp_http.body 0
                          (min (String.length r.Pp_http.body) 160)))
              else
                match Yojson.Safe.from_string r.Pp_http.body with
                | `Assoc fields -> (
                    match List.assoc_opt "ok" fields with
                    | Some (`Bool true) ->
                        Ok
                          (Option.value (List.assoc_opt "value" fields)
                             ~default:`Null)
                    | _ ->
                        Error
                          ("habitat: "
                           ^ Yojson.Safe.to_string
                               (Option.value (List.assoc_opt "error" fields)
                                  ~default:(`String "eval failed"))))
                | _ -> Error "habitat: unparsable reply"))

(** Render a remote JSON value as a local Nopales VStr (scalars plain,
    compounds as their JSON text — dict-get/json-parse consume both). *)
let json_to_vstr (j : Yojson.Safe.t) : Ast.value =
  match j with
  | `String s -> VStr s
  | `Int i -> VStr (string_of_int i)
  | `Intlit s -> VStr s
  | `Float f -> VStr (string_of_float f)
  | `Bool b -> VStr (if b then "true" else "false")
  | `Null -> VStr ""
  | other -> VStr (Yojson.Safe.to_string other)

(** Remote call + render; raises a Prim_error on failure (fail-soft:
    the error text names the bridge, contained wherever the prim ran). *)
let call ~(env : Eio_unix.Stdenv.base) (expr : string) : Ast.value =
  match eval env expr with
  | Ok v -> json_to_vstr v
  | Error e ->
      raise (Interp_error (Prim_error ("habitat", e, "habitat-unreachable")))

(* ---- remote expr builders (pure; unit-tested) ---- *)

(** The default shared store: type "gregor-memory" (AGENT_MEMORY_TYPE
    overrides). *)
let default_type () = getenv ~default:"gregor-memory" "AGENT_MEMORY_TYPE"

let remote (fn : string) (args : string list) : string =
  "(" ^ fn ^ " " ^ String.concat " " args ^ ")"

(* Quote a string for embedding in a remote expr; the Nopales reader
   accepts the named escapes backslash, quote, and newline. *)
let q (s : string) : string =
  "\"" ^ Pricklypear_lisp.Reader.escape_string s ^ "\""

let remember_expr ~(typ : string) ~(id : string) ~(title : string)
    ~(content : string) ~(tags : string) : string =
  remote "mem/remember"
    [ q typ; q id; q title; q content; q (if tags = "" then "[]" else tags) ]

let replace_expr ~(typ : string) ~(id : string) ~(title : string)
    ~(content : string) ~(tags : string) : string =
  remote "mem/replace"
    [ q typ; q id; q title; q content; q (if tags = "" then "[]" else tags) ]

let get_expr ~(typ : string) ~(id : string) : string =
  remote "as-str" [ remote "mem/get" [ q typ; q id ] ]

let search_expr ~(typ : string) ~(needle : string) : string =
  remote "json-emit" [ remote "mem/search" [ q typ; q needle ] ]

let forget_expr ~(typ : string) ~(id : string) : string =
  remote "as-str" [ remote "mem/forget" [ q typ; q id ] ]

let list_expr ~(typ : string) ~(n : int) : string =
  remote "json-emit" [ remote "mem/list" [ q typ; string_of_int n ] ]

(* ---- bindings browser verbs (the AST browser over the habitat's
   function grammar; borge/agent.borg filmstrip-shell cascade) ---- *)

(* (bind/packages) <= (json-emit (list-packages)) -> JSON array of
   package name strings (top-level prefixes on the bridge identity's
   branch chain). *)
let packages_expr () : string = "(json-emit (list-packages))"

(* (bind/list pkg) <= (json-emit (package-bindings pkg)) -> JSON array
   of full binding-name strings. *)
let bindings_expr ~(pkg : string) : string =
  remote "json-emit" [ remote "package-bindings" [ q pkg ] ]

(* (bind/get name) <= one row as a JSON object; function-doc /
   function-source VNil both emit json null, which json-parse reads as
   VNil and the surface prints as "". *)
let binding_expr ~(name : string) : string =
  "(json-emit (dict-set (dict-set \"{}\" \"doc\" (function-doc " ^ q name
  ^ ")) \"source\" (function-source " ^ q name ^ ")))"

let new_mid () : string =
  Printf.sprintf "m-%x-%04x" (int_of_float (Unix.gettimeofday ()))
    (Random.bits () land 0xFFFF)

(* ---- local prims + the model-facing tool ---- *)

let prim_err (name : string) (msg : string) =
  raise (Interp_error (Prim_error (name, msg, "bad-args")))

(** The glochid-side mem/* prims. Env-shaped (no server state); safe
    in every mode (serve, REPL, pipe, tests). Mirrors gregor's 5-arg
    contract exactly (TYPE first) so habits transfer; mem/search and
    mem/forget accept TYPE as a 1-arg short form against the default
    shared store. *)
let register ~(rt : Runtime.t) ~(env : Eio_unix.Stdenv.base) : unit =
  let strs (name : string) (args : Ast.value list) : string list =
    List.map
      (fun v -> match v with VStr s -> s | other -> Reader.pp_value other)
      args
  in
  Runtime.add_prim rt "mem/remember" (fun args ->
      match strs "mem/remember" args with
      | [ typ; id; title; content ] ->
          call ~env (remember_expr ~typ ~id ~title ~content ~tags:"")
      | [ typ; id; title; content; tags ] ->
          call ~env (remember_expr ~typ ~id ~title ~content ~tags)
      | _ ->
          prim_err "mem/remember"
            "usage: (mem/remember type id title content [tags])");
  Runtime.add_prim rt "mem/replace" (fun args ->
      match strs "mem/replace" args with
      | [ typ; id; title; content ] ->
          call ~env (replace_expr ~typ ~id ~title ~content ~tags:"")
      | [ typ; id; title; content; tags ] ->
          call ~env (replace_expr ~typ ~id ~title ~content ~tags)
      | _ ->
          prim_err "mem/replace"
            "usage: (mem/replace type id title content [tags])");
  Runtime.add_prim rt "mem/get" (fun args ->
      match strs "mem/get" args with
      | [ typ; id ] -> call ~env (get_expr ~typ ~id)
      | _ -> prim_err "mem/get" "usage: (mem/get type id)");
  Runtime.add_prim rt "mem/search" (fun args ->
      match strs "mem/search" args with
      | [ typ; needle ] -> call ~env (search_expr ~typ ~needle)
      | [ needle ] -> call ~env (search_expr ~typ:(default_type ()) ~needle)
      | _ ->
          prim_err "mem/search"
            "usage: (mem/search [type] needle)");
  Runtime.add_prim rt "mem/forget" (fun args ->
      match strs "mem/forget" args with
      | [ typ; id ] -> call ~env (forget_expr ~typ ~id)
      | [ id ] -> call ~env (forget_expr ~typ:(default_type ()) ~id)
      | _ -> prim_err "mem/forget" "usage: (mem/forget [type] id)");
  Runtime.add_prim rt "mem/list" (fun args ->
      match strs "mem/list" args with
      | [ typ ] -> call ~env (list_expr ~typ ~n:100)
      | [ typ; n ] ->
          (try call ~env (list_expr ~typ ~n:(int_of_string n))
           with _ -> prim_err "mem/list" "usage: (mem/list type [n])")
      | _ -> prim_err "mem/list" "usage: (mem/list type [n])");
  Runtime.add_prim rt "mem/new-id" (fun _ -> VStr (new_mid ()));
  (* The bindings browser's read verbs: the habitat's function grammar
     (list-packages / package-bindings / function-doc+source) over the
     same bridge. Same fail-soft rule — raise habitat-unreachable at
     CALL time; the Nopales surface and the filmstrip cascade guard
     (bind/safe) and keep rendering instead of erroring the shell. *)
  Runtime.add_prim rt "bind/packages" (fun args ->
      match args with
      | [] -> call ~env (packages_expr ())
      | _ -> prim_err "bind/packages" "usage: (bind/packages)");
  Runtime.add_prim rt "bind/list" (fun args ->
      match strs "bind/list" args with
      | [ pkg ] -> call ~env (bindings_expr ~pkg)
      | _ -> prim_err "bind/list" "usage: (bind/list pkg)");
  Runtime.add_prim rt "bind/get" (fun args ->
      match strs "bind/get" args with
      | [ name ] -> call ~env (binding_expr ~name)
      | _ -> prim_err "bind/get" "usage: (bind/get name)");
  (* Terminal ergonomics (glooide daily-drive): /remember saves one fact
     with an auto id/title; /memories prints the formatted recent
     list. Both ride the REPL command seam. *)
  let first_line (s : string) : string =
    let s = String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) (String.trim s) in
    let cut = 60 in
    if String.length s <= cut then s
    else String.sub s 0 cut ^ "…"
  in
  Runtime.add_prim rt "mem/save" (fun args ->
      match strs "mem/save" args with
      | [ content ] ->
          let id = new_mid () in
          ignore
            (call ~env
               (remember_expr ~typ:(default_type ()) ~id
                  ~title:(first_line content)
                  ~content ~tags:""));
          VStr id
      | _ -> prim_err "mem/save" "usage: (mem/save content)");
  Runtime.add_prim rt "mem/recent" (fun args ->
      let n =
        match strs "mem/recent" args with
        | [ n ] -> (try int_of_string n with _ -> 12)
        | _ -> 12
      in
      let typ = default_type () in
      match eval env (list_expr ~typ ~n) with
      | Error e -> raise (Interp_error (Prim_error ("habitat", e, "habitat-unreachable")))
      | Ok (`List entries) ->
          let fmt (entry : Yojson.Safe.t) : string =
            match entry with
            | `List [ id; fields ] ->
                let str k default =
                  match Yojson.Safe.Util.member k fields with
                  | `String s when s <> "" -> s
                  | _ -> default
                in
                Printf.sprintf "%-18s %s"
                  (str "mid" "?") (first_line (str "title" "(no title)"))
              | _ -> "(row)"
            in
          let lines = List.map fmt entries in
          if entries = [] then VStr "(no memories yet)"
          else VStr (String.concat "\n" lines)
      | Ok _ -> VStr "(mem/recent: unexpected store reply)");
  (* REPLACE commands (both ride the command seam; plugin
     register-command posts AFTER this and may override) *)
  Hashtbl.replace rt.Runtime.commands "remember"
    { Loop.doc = "save a durable fact (flags in the line)";
      run = Ast.VPrim "mem/save" };
  Hashtbl.replace rt.Runtime.commands "memories"
    { Loop.doc = "recent rows in the shared agent-memory store";
      run = Ast.VPrim "mem/recent" }

(** The MODEL-facing memory tool (Loop tool over the same verbs):
    {"action":"remember|search|get|forget", "id"?, "title"?,
     "content"?, "tags"?} — the type is pinned to the default shared
    store. Returns a JSON result string (or {"error": ...}). *)
let memory_tool ~(env : Eio_unix.Stdenv.base) (args : Yojson.Safe.t) : string
    =
  let field n default =
    match args with
    | `Assoc fields -> (
        match List.assoc_opt n fields with
        | Some (`String s) -> if s = "" then default else s
        | _ -> default)
    | _ -> default
  in
  let typ = default_type () in
  let id, expr =
    match field "action" "" with
    | "remember" ->
        let id =
          match field "id" "" with "" -> new_mid () | i -> i
        in
        ( id,
          remember_expr ~typ ~id
            ~title:(field "title" "untitled")
            ~content:(field "content" "")
            ~tags:(field "tags" "") )
    | "search" -> ("", search_expr ~typ ~needle:(field "query" (field "needle" "")))
    | "get" -> ("", get_expr ~typ ~id:(field "id" ""))
    | "forget" -> ("", forget_expr ~typ ~id:(field "id" ""))
    | other ->
        ( "", "{\"error\":" ^ q ("unknown action " ^ other) ^ "}" )
  in
  match eval env expr with
  | Ok `Null -> "{\"ok\":true" ^ (if id = "" then "" else ",\"id\":\"" ^ id ^ "\"") ^ "}"
  | Ok v ->
      let v =
        match v with `String "" -> `Null | s -> s
      in
      "{\"ok\":true,"
      ^ (if id = "" then "" else "\"id\":\"" ^ id ^ "\",")
      ^ "\"result\":"
      ^ (match v with
         | `String s when
             String.length s > 0
             && (String.unsafe_get s 0 = '{' || String.unsafe_get s 0 = '[')
           -> s
         | v -> Yojson.Safe.to_string v)
      ^ "}"
  | Error e -> "{\"error\":" ^ q e ^ "}"

(* ---- habitat/fwd: the habitat-pane forwarder (agent.borg
   habitat-panes stanza, 2026-09-20). Habitat app faces are ordinary
   htmx fragment routes already — glochid panes WEAR them: the gate
   forwards allowlisted prefixes (glochid/fwd-apps registers
   /todo /notes /calendar via habitat/fwd-prefix at boot) with the
   operator's habitat session and pipes the answer back: status,
   htmx relay headers, body. ONE auth tax per PROCESS, not per
   interaction: the jar's POST /login pays one argon2id verify and
   the pp_session cookie rides every forward (Basic-auth per request
   would re-verify argon2 on every click — a known habitat floor).
   Kernel paths (auth.route-visibility) can never be registered: the
   prefix prim refuses the hard-private set. Fragment/pass-through
   policy: the caller decides HX-Request (pane dispatch forces
   fragments; an address-bar GET pipes the habitat's full page
   through raw). ---- *)

let fwd_timeout_s = 30.

(* the session jar: one token per process; guarded so concurrent
   forwards settle into exactly one re-login on expiry *)
type fwd_jar = { fwd_mutex : Eio.Mutex.t; fwd_token : string option ref }

let fwd_jar = { fwd_mutex = Eio.Mutex.create (); fwd_token = ref None }

(* the NEVER-forward set: kernel paths stay hard-private on the
   habitat (the auth.route-visibility subset that could be mistaken
   for an app face, plus glochid's own surfaces) — the prefix prim
   refuses these *)
let fwd_refused =
  [ "/"
  ; "/admin"
  ; "/analytics"
  ; "/api"
  ; "/login"
  ; "/logout"
  ; "/health"
  ; "/assets"
  ; "/media"
  ; "/bindings"
  ; "/binding"
  ; "/repl"
  ; "/agent"
  ; "/filmstrip"
  ; "/chat"
  ; "/sessions"
  ; "/settings"
  ; "/memory"
  ]

let fwd_prefixes : (string, unit) Hashtbl.t = Hashtbl.create 8

let fwd_prefix_allowed (path : string) : bool =
  not
    (List.exists
       (fun p -> path = p || String.starts_with ~prefix:(p ^ "/") path)
       fwd_refused)

(** register one forwardable prefix (Nopales boot config). Answers
    false on a kernel-path refusal — the prim surfaces that. *)
let fwd_register_prefix (path : string) : bool =
  if fwd_prefix_allowed path then begin
    Hashtbl.replace fwd_prefixes path ();
    true
  end
  else false

let fwd_matches_prefix (path : string) : string option =
  Hashtbl.fold
    (fun k _ acc ->
       if String.starts_with ~prefix:k path then Some k else acc)
    fwd_prefixes None

(** minimal percent-encode for form fields (login user/pass; pane
    params already ride the Nopales side's url-enc) *)
let fwd_url_encode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
           Buffer.add_char buf c
       | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

(** first pp_session cookie in the (lowercased) reply headers, else
    None — login's 302 carries it *)
let fwd_take_token (headers : (string * string) list) : string option =
  List.find_map
    (fun (k, v) ->
       if k <> "set-cookie" then None
       else begin
         let prefix = "pp_session=" in
         let rec cut i =
           if i + String.length prefix > String.length v then None
           else if String.sub v i (String.length prefix) = prefix then begin
             let start = i + String.length prefix in
             let stop =
               match String.index_from_opt v start ';' with
               | Some j -> j
               | None -> String.length v
             in
             Some (String.sub v start (stop - start))
           end
           else cut (i + 1)
         in
         cut 0
       end)
    headers

(** Mint the session under the jar lock: skip (Ok token) if another
    fiber logged in while this one awaited. One argon2id verify per
    process, not per bounce. *)
let fwd_login ~(env : Eio_unix.Stdenv.base) (c : creds)
    : (string, string) Result.t =
  Eio.Mutex.use_rw ~protect:false fwd_jar.fwd_mutex (fun () ->
      match !(fwd_jar.fwd_token) with
      | Some t -> Ok t
      | None -> (
          match Pp_http.split_base (trim_url c.url) with
          | Error e -> Error ("habitat: " ^ e)
          | Ok endpoint -> (
              let body =
                "username=" ^ fwd_url_encode c.user
                ^ "&password=" ^ fwd_url_encode c.pass
                ^ "&next=%2F"
              in
              match
                Pp_http.request ~timeout_s:fwd_timeout_s ~env ~endpoint
                  ~meth:"POST" ~path:"/login"
                  ~content_type:(Some "application/x-www-form-urlencoded")
                  ~headers:[] ~body ()
              with
              | Error e -> Error ("habitat: " ^ e)
              | Ok r -> (
                  match fwd_take_token r.Pp_http.headers with
                  | Some token ->
                      fwd_jar.fwd_token := Some token;
                      Ok token
                  | None ->
                      Error
                        "habitat: login did not mint a pp_session cookie"))))

(** One forward hop: jar token if minted else anonymous (the bounce
    detection re-authenticates). [content-type] rides POSTs only. *)
let fwd_hop ~(env : Eio_unix.Stdenv.base) (c : creds) (meth : string)
    (path : string) (query : string) (body : string) ~(hx : bool)
    : (Pp_http.response, string) Result.t =
  match Pp_http.split_base (trim_url c.url) with
  | Error e -> Error ("habitat: " ^ e)
  | Ok endpoint -> (
      let target = if query = "" then path else path ^ "?" ^ query in
      let extra =
        match !(fwd_jar.fwd_token) with
        | Some t -> [ ("cookie", "pp_session=" ^ t) ]
        | None -> []
      in
      (* the caller's htmx-ness decides fragment vs full page: a boosted
         anchor or in-process pane dispatch answers fragments; an
         address-bar GET pipes the habitat's full page raw *)
      let extra = if hx then extra @ [ ("hx-request", "true") ] else extra in
      Pp_http.request ~timeout_s:fwd_timeout_s ~env ~endpoint ~meth
        ~path:target
        ?content_type:
          (if meth = "POST" then
           Some (Some "application/x-www-form-urlencoded")
           else Some None) ~headers:extra ~body ())

(** Is this reply an unauthenticated bounce (401, 302->/login, or the
    login page served on a 200)? A stale jar token lands there. *)
let fwd_auth_bounced (status : int) (headers : (string * string) list)
    (body : string) : bool =
  if status = 401 then true
  else if status >= 300 && status < 400 then
    (match List.assoc_opt "location" headers with
     | Some loc -> String.starts_with ~prefix:"/login" loc
     | None -> false)
  else String.starts_with ~prefix:"login" body

(** THE forward call: cookie'd hops; re-authenticate once on an auth
    bounce (the jar lock makes concurrent bounces settle into one
    login); walk relative redirects up to 3 hops so an action POST
    lands on its target — never cross-host (redirects to another
    server are surfaced as-is). The HX-* reply headers of the final
    hop ride back for the gate to relay. *)
let forward_response ~(env : Eio_unix.Stdenv.base) ~(meth : string)
    ~(path : string) ~(query : string) ~(body : string) ~(hx : bool)
    : (int * (string * string) list * string, string) Result.t =
  match creds ~env with
  | None ->
      Error
        "habitat bridge not configured: set AGENT_HABITAT_URL/USER/PASS \
         (or PP_URL/PP_USER/PP_PASS)"
  | Some c ->
      let rec walk path query redirects =
        match fwd_hop ~env c meth path query body ~hx with
        | Error e -> Error ("habitat: " ^ e)
        | Ok { Pp_http.status; Pp_http.headers = hs; Pp_http.body = b } ->
            if fwd_auth_bounced status hs b then
              if redirects <= 0 then
                Error "habitat: unauthenticated (session expired)"
              else
                match fwd_login ~env c with
                | Error e -> Error e
                | Ok _ -> walk path query (redirects - 1)
            else if status >= 300 && status < 400 && redirects > 0 then (
              match List.assoc_opt "location" hs with
              | Some loc when String.starts_with ~prefix:"/" loc ->
                  let p, q =
                    match String.index_opt loc '?' with
                    | Some i ->
                        ( String.sub loc 0 i,
                          String.sub loc (i + 1) (String.length loc - i - 1) )
                    | None -> (loc, "")
                  in
                  walk p q (redirects - 1)
              | _ -> Ok (status, hs, b))
            else Ok (status, hs, b)
      in
      walk path query 3

(** relay-worthy response headers for htmx: the HX-* family the
    habitat faces answer actions with (actions + htmx docs). *)
let fwd_relay_headers (headers : (string * string) list)
    : (string * string) list =
  List.filter (fun (k, _) -> String.starts_with ~prefix:"hx-" k) headers

(** The gate's prim face: (habitat/fwd meth path query body) -> VStr
    body (the gate hook relays status/headers itself; this prim is the
    IN-PROCESS dispatch path — filmstrip/open's pane content — where
    HX-Request semantics are already fragment-shaped). Fail-LOUD: the
    Nopales fwd face guards it into a muted card. *)
let fwd_register ~(rt : Runtime.t) ~(env : Eio_unix.Stdenv.base) : unit =
  Runtime.add_prim rt "habitat/fwd"
    (fun args ->
       match args with
       | [ Ast.VStr meth; Ast.VStr path; Ast.VStr query; Ast.VStr body ] ->
           (match forward_response ~env ~meth ~path ~query ~body ~hx:true with
            | Ok (_, _, b) -> Ast.VStr b
            | Error e ->
                raise
                  (Interp_error
                     (Prim_error ("habitat", e, "habitat-unreachable"))))
       | _ ->
           prim_err "habitat/fwd"
             "usage: (habitat/fwd meth path query body)");
  Runtime.add_prim rt "habitat/fwd-prefix"
    (fun args ->
       match args with
       | [ Ast.VStr path ] ->
           if fwd_register_prefix path then Ast.VBool true
           else
             raise
               (Interp_error
                  (Prim_error
                     ( "habitat/fwd-prefix",
                       "kernel paths cannot be forwarded (auth.route-visibility)",
                       "refused" )))
       | _ -> prim_err "habitat/fwd-prefix" "usage: (habitat/fwd-prefix path)")
