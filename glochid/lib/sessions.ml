(** glochid persistent sessions (borge/glochid.borg sessions-persist).

    Every conversation is an APPEND-ONLY JSONL transcript under the
    local pp store (.agent/sessions/, one file per session), written at
    the LOOP boundary: one {"t":"msg",...} line per provider message the
    agent loop appends to the shared history (user, assistant-with-
    tool_calls, tool results). Resume = load the file back into a fresh
    history and keep appending to the same file. Local-first: the store
    is plain files in the working tree (no habitat round trip); the
    sync-up story is local-first.sync, out of scope here.

    Prims exposed to Nopales (borge/glochid.borg host-prim-namespace,
    session family): session/name session/list session/data session/new
    session/resume session/text — plus REPL commands /new /resume
    /sessions registered on the same seam. *)

type handle = {
  id : string;                (* session id, filename stem *)
  path : string;              (* full path of the JSONL transcript *)
  mutable count : int;        (* provider messages persisted so far *)
}

(** Root of the local session store. AGENT_SESSIONS_DIR overrides
    (tests / multi-checkout use). *)
let root () : string =
  match Sys.getenv_opt "AGENT_SESSIONS_DIR" with
  | Some d when d <> "" -> d
  | _ -> Filename.concat ".agent" "sessions"

(* ---- message <-> json (Provider.message is wire-shape JSON already:
        role + content + tool_call_id + tool_calls) ---- *)

let message_to_json (m : Provider.message) : Yojson.Safe.t =
  `Assoc
    [ ("role", `String m.role)
    ; ("content", m.content)
    ; ( "tool_call_id"
      , match m.tool_call_id with Some s -> `String s | None -> `Null )
    ; ("tool_calls", Option.value m.tool_calls ~default:`Null) ]

let message_of_json (j : Yojson.Safe.t) : Provider.message option =
  match j with
  | `Assoc fields -> (
      let role =
        match List.assoc_opt "role" fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let content = Option.value (List.assoc_opt "content" fields) ~default:`Null in
      let tool_call_id =
        match List.assoc_opt "tool_call_id" fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let tool_calls =
        match List.assoc_opt "tool_calls" fields with
        | Some (`List _ as tc) -> Some tc
        | _ -> None
      in
      (match role with
       | Some r -> Some { Provider.role = r; content; tool_call_id; tool_calls }
       | None -> None))
  | _ -> None

(* Display text for a message: the string content, or the first text
   part of a content-part array. None when content is null / a tool-call
   placeholder (assistant turns that ONLY made tool calls). *)
let content_text (c : Yojson.Safe.t) : string option =
  match c with
  | `String s -> Some s
  | `List parts -> (
      List.find_map
        (function
          | `Assoc fields -> (
              match
                ( List.assoc_opt "type" fields,
                  List.assoc_opt "text" fields )
              with
              | (Some (`String "text") | None), Some (`String s) -> Some s
              | _ -> None)
          | _ -> None)
        parts)
  | _ -> None

(** Human-facing single-line text for a transcript line (bubbles,
    preview). Very long tool output is truncated. *)
let display_text ?(limit = 400) (m : Provider.message) : string =
  let trunc s =
    if String.length s <= limit then s
    else String.sub s 0 limit ^ "…"
  in
  match m.role with
  | "tool" ->
      let out = Option.value (content_text m.content) ~default:"(no output)" in
      "tool -> " ^ trunc out
  | "assistant" when Option.is_some m.tool_calls -> (
      let label =
        match m.tool_calls with
        | Some (`List calls) ->
            String.concat ", "
              (List.filter_map
                 (function
                   | `Assoc fields -> (
                       match
                         List.assoc_opt "function" fields
                       with
                       | Some (`Assoc fn) -> (
                           match List.assoc_opt "name" fn with
                           | Some (`String n) -> Some n
                           | _ -> None)
                       | _ -> None)
                   | _ -> None)
                 calls)
        | _ -> ""
      in
      match (content_text m.content, label) with
      | Some t, _ -> trunc t
      | None, "" -> "(assistant turn)"
      | None, _ -> "(tool calls: " ^ label ^ ")")
  | _ -> Option.value (content_text m.content) ~default:""

let preview (msgs : Provider.message list) : string =
  let p =
    List.find_map
      (fun (m : Provider.message) ->
        match m.role with
        | "user" -> Some (display_text ~limit:80 m)
        | _ -> None)
      msgs
  in
  let clean s =
    String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) s
  in
  match p with Some s when s <> "" -> clean s | _ -> "(no prompt yet)"

(* ---- store layout ---- *)

let ensure_dir (d : string) : unit =
  try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let safe_id (id : string) : bool =
  id <> "" && String.for_all (fun c ->
      (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-' || c = '_')
      id

let file_path (id : string) : string =
  Filename.concat (root ()) (id ^ ".jsonl")

let new_session ~(model : string) : handle =
  ensure_dir (Filename.dirname (root ()));
  ensure_dir (root ());
  let id =
    Printf.sprintf "%x-%04x" (int_of_float (Unix.gettimeofday ()))
      (Random.bits () land 0xFFFF)
  in
  let path = file_path id in
  let h = { id; path; count = 0 } in
  let header =
    `Assoc
      [ ("t", `String "session")
      ; ("id", `String id)
      ; ("ts", `Float (Unix.gettimeofday ()))
      ; ("model", `String model) ]
  in
  let oc = open_out path in
  output_string oc (Yojson.Safe.to_string header);
  output_char oc '\n';
  close_out oc;
  h

(* Append one provider message (the LOOP boundary: called for every
   message the agent loop appends to the shared history). *)
let append_msg (h : handle) (m : Provider.message) : unit =
  let line =
    `Assoc
      [ ("t", `String "msg"); ("ts", `Float (Unix.gettimeofday ()))
      ; ("msg", message_to_json m) ]
  in
  let oc = open_out_gen [ Open_append; Open_creat ] 0o644 h.path in
  output_string oc (Yojson.Safe.to_string line);
  output_char oc '\n';
  close_out oc;
  h.count <- h.count + 1

(** A persistence SINK for the loop: appends to whatever session handle
    is CURRENT (ref'd, so /new rotation mid-run re-points the stream
    without changing the sink closure).

    LAZY TRIGGER (2026-09-16): with [create:true] the sink CREATES the
    first session on the first appended message — the first user
    prompt — instead of the host pre-creating one at boot. Boots,
    page opens, and harness restarts that never send a prompt leave
    NO transcript (no header-only litter in .agent/sessions/).
    [create:false] appends only when a session already exists
    (/resume, explicit /new) and stays store-silent otherwise —
    mock/pipe runs must never write the store. *)
let on_msg ?(create = false) ~(model : unit -> string)
    (sess : handle option ref) : Provider.message -> unit =
 fun m ->
  (match !sess with
   | Some h -> Some h
   | None ->
       if create then begin
         let h = new_session ~model:(model ()) in
         sess := Some h;
         Some h
       end
       else None)
  |> Option.iter (fun h -> append_msg h m)

(** Register an env-independent sink ref + history in one call; hosts
    pass the same refs everywhere. *)
let sink (sess : handle option ref)
    (history : Provider.message list ref) : Provider.message -> unit =
 fun m ->
  (match !sess with Some h -> append_msg h m | None -> ());
  history := !history @ [ m ]

(* ---- read side ---- *)

let load_file (path : string) : Provider.message list =
  (try
     List.filter_map
       (fun line ->
         match String.trim line with
         | "" -> None
         | trimmed -> (
             try
               match Yojson.Safe.from_string trimmed with
               | `Assoc fields -> (
                   match List.assoc_opt "msg" fields with
                   | Some j -> message_of_json j
                   | None -> None)
               | _ -> None
             with _ -> None))
       (String.split_on_char '\n' (In_channel.input_all (open_in_bin path)))
   with _ -> [])

let load (id : string) : Provider.message list = load_file (file_path id)

type entry = {
  e_id : string;
  e_ts : float;
  e_count : int;
  e_model : string;
  e_preview : string;
}

let entry_of_path (path : string) : entry option =
  (try
     let msgs = load_file path in
     let model =
       let first = String.split_on_char '\n' (In_channel.input_all (open_in_bin path)) in
       (match first with
        | line :: _ -> (
            try
              (match Yojson.Safe.from_string (String.trim line) with
               | `Assoc fields -> (
                   match List.assoc_opt "model" fields with
                   | Some (`String s) -> s
                   | _ -> "")
               | _ -> "")
              with _ -> "")
        | [] -> "")
     in
     let ts = (Unix.stat path).st_mtime in
     let id = Filename.remove_extension (Filename.basename path) in
     Some
       { e_id = id
       ; e_ts = ts
       ; e_count = List.length msgs
       ; e_model = model
       ; e_preview = preview msgs }
   with _ -> None)

(** Root id of the dir listing; largest file. *)
let entries_slurped () : entry list =
  (try
     let dir = root () in
     Sys.readdir dir
     |> Array.to_list
     |> List.filter (fun f -> Filename.check_suffix f ".jsonl")
     |> List.filter_map (fun f -> entry_of_path (Filename.concat dir f))
   with _ -> [])

let entries () : entry list =
  List.sort (fun a b -> compare b.e_ts a.e_ts) (entries_slurped ())

let utc_of_ts (ts : float) : string =
  let t = Unix.localtime ts in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min

let entries_json () : string =
  `List
    (List.map
       (fun (e : entry) ->
         `Assoc
           [ ("id", `String e.e_id)
           ; ("utc", `String (utc_of_ts e.e_ts))
           ; ("count", `String (string_of_int e.e_count))
           ; ("model", `String e.e_model)
           ; ("preview", `String e.e_preview) ])
       (entries ()))
  |> Yojson.Safe.to_string

(** Plain-text listing for the REPL /sessions command. *)
let entries_text () : string =
  match entries () with
  | [] -> "(no sessions yet)"
  | es ->
      String.concat "\n"
        (List.map
           (fun (e : entry) ->
             Printf.sprintf "%s  %s  %3d msgs  %s" e.e_id
               (utc_of_ts e.e_ts) e.e_count e.e_preview)
           es)

let list_json () : string = entries_json ()

(* resume: id -> (handle, messages) *)

let resume (id : string) : (handle * Provider.message list, string) Result.t
    =
  if not (safe_id id) then Error ("bad session id: " ^ id)
  else
    let path = file_path id in
    if not (Sys.file_exists path) then Error ("no such session: " ^ id)
    else
      let msgs = load_file path in
      Ok ({ id; path; count = List.length msgs }, msgs)

let data_json (id : string) : string =
  (match id with
   | "" -> Some []
   | id when safe_id id && Sys.file_exists (file_path id) ->
       Some (load id)
   | _ -> None)
  |> Option.value ~default:[]
  |> List.map (fun (m : Provider.message) ->
         `Assoc
           [ ("role", `String m.role)
           ; ("text", `String (display_text m)) ])
  |> fun ms -> `List ms |> Yojson.Safe.to_string

(* ---- host prims + REPL commands (live registrations; the surface
   registers inert defaults so pages can render in non-serve contexts) ---- *)

(** Live session prims over [history] + [sess]. Call AFTER the surface
    load (prim registration is last-write-wins). [model] is re-read per
    rotation so POST /settings model changes carry into new sessions. *)
let register ~(rt : Runtime.t) ~(history : Provider.message list ref)
    ~(sess : handle option ref) ~(model : unit -> string) : unit =
  let open Pricklypear_lisp in
  Runtime.add_prim rt "session/name" (fun _ ->
      match !sess with None -> Ast.VStr "" | Some h -> Ast.VStr h.id);
  Runtime.add_prim rt "session/list" (fun _ -> Ast.VStr (list_json ()));
  Runtime.add_prim rt "session/text" (fun _ -> Ast.VStr (entries_text ()));
  Runtime.add_prim rt "session/data" (fun args ->
      match args with
      | [ Ast.VStr id ] -> Ast.VStr (data_json id)
      | _ -> Ast.VStr "[]");
  Runtime.add_prim rt "session/new" (fun _ ->
      let h = new_session ~model:(model ()) in
      sess := Some h;
      history := [];
      Ast.VStr h.id);
  Runtime.add_prim rt "session/resume" (fun args ->
      match args with
      | [ Ast.VStr id ] -> (
          match resume id with
          | Error e -> Ast.VStr ("error: " ^ e)
          | Ok (h, msgs) ->
              sess := Some h;
              history := msgs;
              Ast.VInt (Int64.of_int h.count))
      | _ -> Ast.VStr "error: usage: (session/resume \"id\")");
  (* REPL commands: the dispatcher routes /name to rt.commands; run is
     a prim application (Runtime.apply_value handles VPrim). *)
  Hashtbl.replace rt.Runtime.commands "new"
    { Loop.doc = "start a new persistent session"; run = Ast.VPrim "session/new" };
  Hashtbl.replace rt.Runtime.commands "resume"
    { Loop.doc = "resume a stored session"; run = Ast.VPrim "session/resume" };
  Hashtbl.replace rt.Runtime.commands "sessions"
    { Loop.doc = "list stored sessions"; run = Ast.VPrim "session/text" }
