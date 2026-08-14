(** Agent runtime: the shared Nopales env + rim host.

    One runtime per process. All evaluation is serialized through
    [eval_mutex] (per borge/agent.borg nopales-embed); host prims are
    synchronous and block the calling fiber under Eio. *)

open Pricklypear_lisp
open Ast

type registry_entry = { doc : string; run : Ast.value }

(** Thin request context bound during route/dispatch (agent-route-table,
    hypermedia-harness.request-runner AGENT HOST BACKING). Carries ONLY
    request-shape values: method, path, params, headers, body. DB/branch/
    auth request prims (current-author-as-DB-row etc.) are intentionally
    UNBOUND in the agent — the agent's request context provides only the
    request-shape values. *)
type request_shape = {
  method_ : string;
  path : string;
  params : (string * string) list;
  headers : (string * string) list;  (* lowercased keys *)
  body : string;
}

type t = {
  env : Ast.env;
  prims : Eval.prim_table;
  eval_mutex : Eio.Mutex.t;
  handlers : (string, (Ast.value list -> Ast.value) list ref) Hashtbl.t;
  tools : (string, registry_entry) Hashtbl.t;
  mutable tool_order : string list;
  commands : (string, registry_entry) Hashtbl.t;
  (* Agent route table: path -> handler-name (THE HOST SEAM,
     agent-route-table). Local-lens backing for the image's Postgres
     routes — identical Nopales signatures so libs/hyper runs unchanged. *)
  routes : (string, string) Hashtbl.t;
  (* Per-call request snapshot bound while a route/dispatch handler runs,
     so query-param / request-method / hx-request? resolve. None outside
     a dispatch. *)
  current_request : request_shape option ref;
  (* Captured multipart parts of the last consumed chat/agent turn, as a
     VList of part alists (("html" . frag) ("target" . sel) ("swap" .
     strat)) — the data shape [soup/apply-parts!] consumes. Set by the
     HTTP surface's multipart stream emitter so a consumed turn replays
     deterministically through [hyper/stream] (surfaces smoke). *)
  mutable last_turn_parts : Ast.value;
}

(* Current working directory as seen by path prims. Immutable per
   process: agent never chdir-s. *)
let cwd = ref (Sys.getcwd ())

let value_to_json (v : Ast.value) : Yojson.Safe.t =
  let rec go = function
    | Ast.VStr s -> `String s
    | VInt i -> `Intlit (Int64.to_string i)
    | VFloat f -> `Float f
    | VBool b -> `Bool b
    | VNil -> `Null
    | VSymbol s -> `String s
    | VList vs -> `List (List.map go vs)
    | VLambda _ -> `String "<lambda>"
    | VPrim n -> `String ("<prim " ^ n ^ ">")
    | other -> `String (Reader.pp_value other)
  in
  go v

(** Parse a plugin-produced value back to host JSON. VStr is treated
    as raw JSON first (dict prims produce JSON strings), else a plain
    string. The reverse of value_to_json for event payloads. *)
let rec json_of_value (v : Ast.value) : Yojson.Safe.t =
  match v with
  | Ast.VStr s -> (
      try Yojson.Safe.from_string s
      with Yojson.Json_error _ -> `String s)
  | VInt i -> `Intlit (Int64.to_string i)
  | VFloat f -> `Float f
  | VBool b -> `Bool b
  | VNil -> `Null
  | VSymbol s -> `String s
  | VList vs -> `List (List.map json_of_value vs)
  | VLambda _ -> `String "<lambda>"
  | VPrim n -> `String ("<prim " ^ n ^ ">")
  | other -> `String (Reader.pp_value other)

let rec value_of_json (j : Yojson.Safe.t) : Ast.value =
  match j with
  | `String s -> VStr s
  | `Int i -> VInt (Int64.of_int i)
  | `Intlit s -> (
      match Int64.of_string_opt s with Some i -> VInt i | None -> VStr s)
  | `Float f -> VFloat f
  | `Bool b -> VBool b
  | `Null -> VNil
  | `List js -> VList (List.map value_of_json js)
  | `Assoc _ -> VStr (Yojson.Safe.to_string j)

let str_arg name v =
  match v with
  | Ast.VStr s -> s
  | Ast.VSymbol s -> s
  | _ ->
      raise
        (Ast.Interp_error
           (Ast.Prim_error (name, "expected string/symbol argument", "bad-args")))

let emit_error name msg =
  raise (Ast.Interp_error (Ast.Prim_error (name, msg, "error")))

(** Register a host prim: dispatch table PLUS a VPrim binding in the
    shared env (Nopales resolves prims via env, like the image's
    auto-bind loop in session.ml). All registrations go through here. *)
let add_prim (t : t) (name : string) (fn : Ast.value list -> Ast.value)
    : unit =
  Eval.register_prim t.prims name fn;
  Env.define t.env name (Ast.VPrim name)

(** Path resolution: relative paths resolve under [cwd]; ".." escapes
    are refused (v0 sandbox posture: refuse, not normalize). *)
let resolve_path p =
  let p =
    if String.length p > 0 && p.[0] = '/' then p else Filename.concat !cwd p
  in
  let parts =
    String.split_on_char '/' p
    |> List.filter (fun s -> s <> "" && s <> ".")
  in
  if List.exists (fun s -> s = "..") parts then
    emit_error "fs/path" ("path escapes sandbox: " ^ p);
  "/" ^ String.concat "/" parts

(** Core eval path shared by user input and plugin callbacks. *)
(** Serialize Nopales evals. Manual lock/unlock (not use_rw):
    Interp_error is a normal outcome of eval and must not poison the
    mutex for later evals. *)
let with_eval_lock (t : t) (f : unit -> 'a) : 'a =
  Eio.Mutex.lock t.eval_mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.eval_mutex) f

let eval_string ~(rt : t) (src : string) : Ast.value =
  with_eval_lock rt (fun () ->
      Control.run_with_rim ~prims:rt.prims (fun () ->
          Eval.eval_string_direct ~prims:rt.prims rt.env src))

(** Evaluate all top-level forms in [src] into the shared env, under
    the eval lock + rim. Used to boot-load Nopales packages (libs/std,
    libs/html, libs/hyper) as SOURCE — the same env as plugins/REPL, so
    the surface is redefinable live (borge/agent.borg surfaces). *)
let load_source ~(rt : t) (src : string) : unit =
  with_eval_lock rt (fun () ->
      Control.run_with_rim ~prims:rt.prims (fun () ->
          ignore (Eval.read_eval_all_direct ~prims:rt.prims rt.env src)))

(** Load a .pp file from [path] into the shared env. *)
let load_file ~(rt : t) (path : string) : (unit, string) result =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    load_source ~rt s;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

(** [apply_value_raw] — apply a Nopales function value to host-side
    value args directly, assuming the caller ALREADY holds the eval lock
    AND has a rim active (direct_prims set). Used by prims that run
    inside an eval (route/dispatch invokes a handler from within the
    rim): re-locking the non-reentrant Eio.Mutex would deadlock, and the
    outer rim already services the handler's prim calls via
    [direct_prims]. Args NEVER go through the reader, so lambda values
    and arbitrary JSON strings are safe. *)
let apply_value_raw (t : t) (fn : Ast.value) (args : Ast.value list)
    : Ast.value =
  let fn_sym = "__host_apply_fn__" in
  Env.define t.env fn_sym fn;
  let arg_syms =
    List.mapi (fun i _ -> Printf.sprintf "__host_apply_arg_%d__" i) args
  in
  List.iter2 (Env.define t.env) arg_syms args;
  let src =
    "(" ^ fn_sym
    ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "" arg_syms
    ^ ")"
  in
  Eval.eval_string_direct ~prims:t.prims t.env src

(** Apply a Nopales function value to host-side value args. Acquires
    the eval lock and establishes the rim — for callers OUTSIDE an
    active eval (loop tool bodies, event handlers). Prims running
    inside an eval use [apply_value_raw] instead (no re-lock). *)
let apply_value (t : t) (fn : Ast.value) (args : Ast.value list) : Ast.value =
  with_eval_lock t (fun () ->
      Control.run_with_rim ~prims:t.prims (fun () ->
          apply_value_raw t fn args))

(* ---- Host prims ---- *)

let register_fs_prims (t : t) : unit =
  let read args =
    match args with
    | [ path ] -> (
        let p = resolve_path (str_arg "fs/read" path) in
        try
          let ic = open_in_bin p in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          VStr s
        with Sys_error e -> emit_error "fs/read" e)
    | _ -> emit_error "fs/read" "usage: (fs/read path)"
  in
  add_prim t "fs/read" read;
  let write args =
    match args with
    | [ path; content ] -> (
        let p = resolve_path (str_arg "fs/write" path) in
        let s = str_arg "fs/write" content in
        try
          let oc = open_out_bin p in
          output_string oc s;
          close_out oc;
          VBool true
        with Sys_error e -> emit_error "fs/write" e)
    | _ -> emit_error "fs/write" "usage: (fs/write path content)"
  in
  add_prim t "fs/write" write;
  let exists args =
    match args with
    | [ path ] ->
        VBool (Sys.file_exists (resolve_path (str_arg "fs/exists?" path)))
    | _ -> emit_error "fs/exists?" "usage: (fs/exists? path)"
  in
  add_prim t "fs/exists?" exists;
  (* (fs/ls path) -> JSON array of {"name","dir"} sorted by name *)
  let ls args =
    match args with
    | [ path ] -> (
        let p = resolve_path (str_arg "fs/ls" path) in
        try
          let entries =
            Sys.readdir p |> Array.to_list
            |> List.map (fun e ->
                   let full = Filename.concat p e in
                   `Assoc
                     [
                       ("name", `String e);
                       ("dir", `Bool (try Sys.is_directory full with _ -> false));
                     ])
          in
          VStr (Yojson.Safe.to_string (`List (List.sort compare entries)))
        with Sys_error e -> emit_error "fs/ls" e)
    | _ -> emit_error "fs/ls" "usage: (fs/ls path)"
  in
  add_prim t "fs/ls" ls;
  (* (fs/dir? path) -> VBool. True if path resolves to a directory. *)
  let is_dir args =
    match args with
    | [ path ] ->
        let p = resolve_path (str_arg "fs/dir?" path) in
        VBool (try Sys.is_directory p with Sys_error _ -> false)
    | _ -> emit_error "fs/dir?" "usage: (fs/dir? path)"
  in
  add_prim t "fs/dir?" is_dir

let register_shell_prims (t : t) : unit =
  (* (shell/run cmd) -> {"exit": n, "out": "...", "truncated": bool}.
     Fire-and-forget v0; the agent loop's real bash tool is in
     tools.ml with timeout/streaming. *)
  let run args =
    match args with
    | [ cmd ] -> (
        let cmd = str_arg "shell/run" cmd in
        let ic = Unix.open_process_in (cmd ^ " 2>&1") in
        let buf = Buffer.create 4096 in
        (try
           while true do
             Buffer.add_channel buf ic 4096
           done
         with End_of_file -> ());
        match Unix.close_process_in ic with
        | Unix.WEXITED code ->
            let out = Buffer.contents buf in
            let max_out = 50 * 1024 in
            let out, truncated =
              if String.length out > max_out then
                (String.sub out 0 max_out, true)
              else (out, false)
            in
            VStr
              (Yojson.Safe.to_string
                 (`Assoc
                   [
                     ("exit", `Int code);
                     ("out", `String out);
                     ("truncated", `Bool truncated);
                   ]))
        | _ -> emit_error "shell/run" "process did not exit normally")
    | _ -> emit_error "shell/run" "usage: (shell/run cmd)"
  in
  add_prim t "shell/run" run

let register_event_prims (t : t) : unit =
  (* (on "event" handler-fn) *)
  let on args =
    match args with
    | [ ev; fn ] ->
        let ev = str_arg "on" ev in
        let cell =
          match Hashtbl.find_opt t.handlers ev with
          | Some c -> c
          | None ->
              let c = ref [] in
              Hashtbl.replace t.handlers ev c;
              c
        in
        cell := !cell @ [ (fun vals -> apply_value t fn vals) ];
        VBool true
    | _ -> emit_error "on" "usage: (on \"event\" (lambda (ev) ...))"
  in
  add_prim t "on" on;
  (* (register-tool name params-alist fn). Params are validated but v0
     wire schemas are synthesized from param names only (loop.ml). *)
  let reg_tool args =
    match args with
    | [ name; _params; fn ] ->
        let name = str_arg "register-tool" name in
        Hashtbl.replace t.tools name { doc = ""; run = fn };
        if not (List.mem name t.tool_order) then
          t.tool_order <- t.tool_order @ [ name ];
        VBool true
    | _ -> emit_error "register-tool" "usage: (register-tool name params fn)"
  in
  add_prim t "register-tool" reg_tool;
  let reg_cmd args =
    match args with
    | [ name; doc; fn ] ->
        let name = str_arg "register-command" name in
        Hashtbl.replace t.commands name
          { doc = str_arg "register-command" doc; run = fn };
        VBool true
    | [ name; fn ] ->
        let name = str_arg "register-command" name in
        Hashtbl.replace t.commands name { doc = ""; run = fn };
        VBool true
    | _ ->
        emit_error "register-command"
          "usage: (register-command name [doc] fn)"
  in
  add_prim t "register-command" reg_cmd;
  let list_cmds _args =
    let names =
      Hashtbl.fold (fun k _ acc -> `String k :: acc) t.commands []
    in
    VStr (Yojson.Safe.to_string (`List (List.sort compare names)))
  in
  add_prim t "list-commands" list_cmds

let register_ui_prims (t : t) : unit =
  let notify args =
    match args with
    | [ msg ] ->
        Printf.printf "\027[2m[notify] %s\027[0m\n%!" (str_arg "ui/notify" msg);
        VNil
    | _ -> emit_error "ui/notify" "usage: (ui/notify msg)"
  in
  add_prim t "ui/notify" notify;
  let confirm args =
    match args with
    | [ msg ] ->
        Printf.printf "%s [y/N] %!" (str_arg "ui/confirm" msg);
        let line = try input_line stdin with End_of_file -> "n" in
        VBool
          (match String.lowercase_ascii (String.trim line) with
          | "y" | "yes" -> true
          | _ -> false)
    | _ -> emit_error "ui/confirm" "usage: (ui/confirm msg)"
  in
  add_prim t "ui/confirm" confirm

(* ---- Route table + request-context prims (agent-route-table,
   THE HOST SEAM; hypermedia-harness.request-runner AGENT HOST BACKING).

   Identical Nopales signatures to the image's route / route/lookup /
   route/dispatch, host-local backing (Runtime route table + a thin
   request context vs the image's Postgres routes + fat
   Request_context). libs/hyper runs unchanged on both. *)

(** Look up the active request shape, or error if none is bound (i.e. a
    request prim called outside a route/dispatch). Mirrors the image's
    prims_request require_request, but the agent's context is thinner. *)
let require_request (t : t) : request_shape =
  match !(t.current_request) with
  | Some r -> r
  | None ->
      emit_error "request" "no active request (call inside route/dispatch)"

(** [dispatch_route_raw] — the shared route/dispatch body. Assumes the
    caller holds the eval lock AND has a rim active (the route/dispatch
    prim runs inside an eval; the HTTP gate uses the locking
    [dispatch_route] wrapper). Returns the rendered raw-HTML VStr the
    browser would receive. *)
let dispatch_route_raw (t : t) ~handler ~method_ ~path ~params
    ~headers ~body : Ast.value =
  let req : request_shape =
    { method_ = String.uppercase_ascii method_; path; params; headers; body }
  in
  match Env.lookup t.env handler with
  | None -> VStr (Printf.sprintf "route/dispatch: unbound handler %s" handler)
  | Some fn ->
      t.current_request := Some req;
      Fun.protect
        ~finally:(fun () -> t.current_request := None)
        (fun () ->
          try
            let v = apply_value_raw t fn [] in
            VStr (Prims_soup.render_handler_value v)
          with Ast.Interp_error ie ->
            VStr
              (Printf.sprintf "route/dispatch: %s"
                 (Ast.pp_interp_error ie)))

(** [dispatch_route] — locking wrapper for OCaml callers OUTSIDE an
    active eval (the HTTP gate dispatches GET routes through the route
    table). Acquires the eval lock + establishes the rim. *)
let dispatch_route ~(rt : t) ~handler ~method_ ~path ~params ~headers
    ~body : Ast.value =
  with_eval_lock rt (fun () ->
      Control.run_with_rim ~prims:rt.prims (fun () ->
          dispatch_route_raw rt ~handler ~method_ ~path ~params ~headers ~body))

(** Resolve a URL path to its handler name via the agent route table. *)
let lookup_route ~(rt : t) (path : string) : string option =
  Hashtbl.find_opt rt.routes path

(** Call a Nopales template (zero-or-more-arg fn) by name and return the
    rendered raw-HTML string (raw-HTML-tagged VStrs unwrap). For OCaml
    callers outside an eval (the HTTP surface renders fragment templates).
    Returns [None] if the template is unbound or errors. *)
let call_template ~(rt : t) (name : string) (args : Ast.value list)
    : string option =
  match Env.lookup rt.env name with
  | None -> None
  | Some fn ->
      (try
         let v = apply_value rt fn args in
         Some (Prims_soup.render_handler_value v)
       with Ast.Interp_error _ -> None)

let register_route_prims (t : t) : unit =
  (* (route path handler) — bind a path to a Nopales render handler name
     on the agent route table. The agent's local-lens backing for the
     image's (route ...) over Postgres. *)
  let route args =
    match args with
    | [ path; handler ] ->
        let path = str_arg "route" path in
        let handler = str_arg "route" handler in
        Hashtbl.replace t.routes path handler;
        VBool true
    | _ -> emit_error "route" "usage: (route path handler)"
  in
  add_prim t "route" route;
  (* (route/lookup path) -> handler-name VStr | VNil. Resolve a URL path
     to its handler via the route table — exactly hyper/visit's
     resolver. *)
  let route_lookup args =
    match args with
    | [ path ] ->
        let path = str_arg "route/lookup" path in
        (match Hashtbl.find_opt t.routes path with
         | Some h -> VStr h
         | None -> VNil)
    | _ -> emit_error "route/lookup" "usage: (route/lookup path)"
  in
  add_prim t "route/lookup" route_lookup;
  (* (route/dispatch handler method path [params] [headers] [body])
     -> rendered raw-HTML VStr. Invoke the handler fn (looked up in env)
     through the rim (caller already holds the eval lock) with a THIN
     REQUEST CONTEXT bound (query-param / request-method / hx-request?
     resolve; DB/branch/auth request prims stay unbound), and return the
     rendered value the browser would receive. Reuses assoc_of +
     render_handler_value from the shared soup kernel. *)
  let route_dispatch args =
    match args with
    | VStr handler :: VStr meth :: VStr path :: rest ->
        let params =
          match rest with p :: _ -> Prims_soup.assoc_of p | _ -> []
        in
        let headers =
          match rest with
          | _ :: h :: _ ->
              List.map
                (fun (k, v) -> (String.lowercase_ascii k, v))
                (Prims_soup.assoc_of h)
          | _ -> []
        in
        let body =
          match rest with
          | _ :: _ :: VStr b :: _ -> b
          | _ -> ""
        in
        dispatch_route_raw t ~handler ~method_:meth ~path ~params ~headers ~body
    | _ -> emit_error "route/dispatch"
              "usage: (route/dispatch handler method path [params] [headers] [body])"
  in
  add_prim t "route/dispatch" route_dispatch;
  (* ---- request-shape prims (the thin context) ----
     query-param / request-method / hx-request? — they back
     route/dispatch's thin context and libs/hyper's hyper/do-swap
     HX-Request header path. DB/branch/auth request prims are NOT
     registered here (the agent's request context carries only
     request-shape values); calling them yields unbound-prim. *)
  let query_param args =
    match args with
    | [ key ] ->
        let r = require_request t in
        let key = str_arg "query-param" key in
        (match List.assoc_opt key r.params with
         | Some v -> VStr v
         | None -> VNil)
    | _ -> emit_error "query-param" "usage: (query-param key)"
  in
  add_prim t "query-param" query_param;
  let request_method _args =
    let r = require_request t in
    VStr r.method_
  in
  add_prim t "request-method" request_method;
  let hx_request_p _args =
    let r = require_request t in
    (* hyper/do-swap passes hx-request in the PARAMS alist; the HTTP
       gate passes it in HEADERS. Check both so libs/hyper works the same
       way it does on the image. *)
    let from_headers =
      match List.assoc_opt "hx-request" r.headers with
      | Some v when v <> "" && v <> "false" -> true
      | _ -> false
    in
    let from_params =
      match List.assoc_opt "hx-request" r.params with
      | Some v when v <> "" && v <> "false" -> true
      | _ -> false
    in
    VBool (from_headers || from_params)
  in
  add_prim t "hx-request?" hx_request_p;
  (* (session/last-turn-parts) -> VList of part alists captured from the
     last consumed chat/agent turn (the data shape soup/apply-parts! /
     hyper/stream consume). Lets a consumed turn replay deterministically
     through hyper/stream (surfaces smoke). Returns VNil if no turn ran. *)
  let last_turn_parts _args = t.last_turn_parts
  in
  add_prim t "session/last-turn-parts" last_turn_parts

(** Install the pricklypear-pure-prims surface (dict/json/string/
    list/math — the frozen core set per lisp-interpreter.core-freeze)
    into our table. The image's registry (image/lib/primitives.ml)
    enumerates the same set with image-specific additions; here we
    bind only what exists in pure/, no DB/auth/hx prims. *)
let register_pure_prims (t : t) : unit =
  let open Prims_pure in
  let r n f = add_prim t n f in
  r "+" prim_add;
  r "-" prim_sub;
  r "*" prim_mul;
  r "/" prim_div;
  r "div" prim_div;
  r "mod" prim_mod;
  r "quotient" prim_quotient;
  r "remainder" prim_remainder;
  r "abs" prim_abs;
  r "min" prim_min;
  r "max" prim_max;
  r "=" prim_num_eq;
  r "<" prim_lt;
  r ">" prim_gt;
  r "<=" prim_le;
  r ">=" prim_ge;
  r "not" prim_not;
  r "list" prim_list;
  r "car" prim_car;
  r "cdr" prim_cdr;
  r "cons" prim_cons;
  r "cadr" prim_cadr;
  r "caar" prim_caar;
  r "caddr" prim_caddr;
  r "cddr" prim_cddr;
  r "null?" prim_null_p;
  r "pair?" prim_pair_p;
  r "append" prim_append;
  r "reverse" prim_reverse;
  r "nth" prim_nth;
  r "count" prim_count;
  r "string-append" prim_string_append;
  r "string-length" prim_string_length;
  r "substring" prim_substring;
  r "string-contains?" prim_string_contains;
  r "string-trim" prim_string_trim;
  r "string-replace" prim_string_replace;
  r "string-downcase" prim_string_downcase;
  r "string-upcase" prim_string_upcase;
  r "string-starts-with?" prim_string_starts_with_p;
  r "string-ends-with?" prim_string_ends_with_p;
  r "string=?" prim_string_eq;
  r "string-eq" prim_string_eq;
  r "type-of" prim_type_of;
  r "symbol?" prim_symbol_p;
  r "string?" prim_string_p;
  r "symbol->string" prim_symbol_to_string;
  r "string->symbol" prim_string_to_symbol;
  r "str" prim_str;
  r "as-str" prim_as_str;
  r "assoc" prim_assoc;
  r "member" prim_member;
  r "json-parse" prim_json_parse;
  r "json-emit" prim_json_emit;
  r "dict-get" prim_dict_get;
  r "dict-set" prim_dict_set;
  r "dict-keys" prim_dict_keys;
  r "equal?" prim_equal_p;
  r "random-uuid" prim_random_uuid;
  r "render-html" prim_render_html;
  r "html-escape" prim_html_escape;
  r "string-split" prim_string_split

(** Register the SHARED soup/ tree prims (Phase 0: lifted into
    pure/prims_soup.ml so both hosts register the same tree prims).
    [Prims_soup.register_pure_prims] fills the prim table; we then
    bind each as a VPrim in env so Nopales resolves them by name
    (matching [add_prim]'s env-bind half, which the pure helper does
    not do). *)
let register_soup_prims (t : t) : unit =
  Prims_soup.register_pure_prims t.prims;
  Hashtbl.iter
    (fun name _ ->
      match name with
      | n when String.length n >= 5 && String.sub n 0 5 = "soup/" ->
          Env.define t.env name (Ast.VPrim name)
      | _ -> ())
    t.prims

(** Minimal agent stdlib — the host library layer (borge/agent.borg
    nopales-embed: "the agent adds HOST PRIMS and an optional host library
    layer in .pp"). Provides the [let*] sequential-binding macro that
    libs/hyper uses, without loading the heavy libs/std (which depends on
    image-only prims: css/*, menu/*, list-bindings, redirect, ...).
    The image gets [let*] from libs/std; the agent gets it here. Same
    expansion as the libs/std macro (written with car/cdr so it depends on
    no c*r beyond the registered set). *)
let boot_stdlib = {|
(defmacro let* (bindings body)
  (if (null? bindings)
      (list (quote let) (list) body)
      (list (quote let)
            (list (list (car (car bindings)) (car (cdr (car bindings)))))
            (list (quote let*) (cdr bindings) body))))
|}

let make () : t =
  let t =
    {
      env = Env.create ();
      prims = Eval.make_prim_table ();
      eval_mutex = Eio.Mutex.create ();
      handlers = Hashtbl.create 8;
      tools = Hashtbl.create 16;
      tool_order = [];
      commands = Hashtbl.create 8;
      routes = Hashtbl.create 16;
      current_request = ref None;
      last_turn_parts = Ast.VNil;
    }
  in
  register_pure_prims t;
  register_soup_prims t;
  register_fs_prims t;
  register_shell_prims t;
  register_event_prims t;
  register_ui_prims t;
  register_route_prims t;
  (* Host library layer: the minimal stdlib (let* macro) so libs/hyper
     and the Nopales surface run unchanged. *)
  load_source ~rt:t boot_stdlib;
  t

(** Fire an event: run handlers in registration order. Returns the
    last handler's value (or VNil). Callers that care about
    short-circuiting (tool_call block) inspect per-handler values
    themselves. *)
let fire (t : t) (ev : string) (payload : Ast.value list) : Ast.value list =
  match Hashtbl.find_opt t.handlers ev with
  | None -> []
  | Some hs ->
      List.map
        (fun h ->
          (* A handler raising must not kill the host turn; surface it
             as an {"error": ...} value the caller can inspect. *)
          try h payload
          with Ast.Interp_error ie ->
            Ast.VStr
              (Yojson.Safe.to_string
                 (`Assoc
                   [
                     ("error",
                      `String
                        (Printf.sprintf "%s handler: %s" ev
                           (Ast.pp_interp_error ie)));
                   ])))
        !hs

(** Run a plugin tool's closure with JSON args, containing errors into
    a {"error": ...} JSON string. *)
let eval_tool_body (t : t) (name : string) (args_json : string) : Ast.value =
  match Hashtbl.find_opt t.tools name with
  | None ->
      VStr
        (Yojson.Safe.to_string
           (`Assoc [ ("error", `String ("no tool named " ^ name)) ]))
  | Some e ->
      (try apply_value t e.run [ VStr args_json ]
       with Ast.Interp_error ie ->
         VStr
           (Yojson.Safe.to_string
              (`Assoc
                [ ("error", `String (Ast.pp_interp_error ie)) ])))

let register_builtin_tool (t : t) (name : string) ~(run : Yojson.Safe.t -> string) : unit =
  (* Builtin host tools bypass Nopales application for speed and to
     keep their bodies in OCaml; the registry shape is the same so
     plugin hooks (tool_call / tool_result events) still see them. *)
  Hashtbl.replace t.tools name
    { doc = "builtin"; run = VPrim ("__builtin__" ^ name) };
  if not (List.mem name t.tool_order) then
    t.tool_order <- t.tool_order @ [ name ];
  (* prim that the registry entry's VPrim names; loop.ml calls
     builtins through this prim table entry instead of applying a
     lambda. *)
  add_prim t ("__builtin__" ^ name)
    (fun args ->
      match args with
      | [ Ast.VStr args_json ] -> (
          try VStr (run (Yojson.Safe.from_string args_json))
          with
          | Yojson.Json_error e ->
              VStr
                (Yojson.Safe.to_string
                   (`Assoc [ ("error", `String ("bad json: " ^ e)) ]))
          | Ast.Interp_error ie ->
              VStr
                (Yojson.Safe.to_string
                   (`Assoc
                     [ ("error", `String (Ast.pp_interp_error ie)) ])))
      | _ -> emit_error "__builtin" "expected single JSON string arg")

let is_builtin (t : t) (name : string) : bool =
  match Hashtbl.find_opt t.tools name with
  | Some { run = VPrim p; _ } -> String.length p > 10 && String.sub p 0 11 = "__builtin__"
  | _ -> false
