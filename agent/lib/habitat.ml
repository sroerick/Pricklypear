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
