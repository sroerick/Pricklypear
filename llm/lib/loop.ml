(** Agent turn loop (borge/agent.borg agent-loop) — HOST-AGNOSTIC and
    INTERPRETER-FREE.

    The loop is shared by every host that embeds it: glochid's Runtime.t,
    pp-remote's Remote.t and the interpreter-free [Default_host] all link
    this library and supply a small [host] record (tool registry, event
    hooks, tool-body eval, prim dispatch). Every value that crosses that
    seam is Yojson, so this library links no interpreter: the Nopales
    hosts translate at [Host_seam.loop_host], and [Default_host] runs the
    builtin read/bash bodies with no Nopales at all.

    The provider (provider.ml) and the builtin read/bash tool bodies
    (tools.ml) are pure OCaml with zero host coupling — the API key is a
    value, the HTTP client is a library, so any client process can run the
    LLM turn locally while its DB prim calls cross whatever rim the host
    installs (in-process table on the agent, /api/prim proxy on pp-remote). *)

let max_turns_default = 40

(** Host-side registry entry: hosts keep their own [run] payload (a
    Nopales closure or a prim value) and hand the loop the dispatch shape
    below through [host.tool_entry]. *)
type 'run registry_entry = { doc : string; run : 'run }

(** What the loop needs to dispatch a tool call: a builtin prim dispatched
    through [host.call_prim], or a plugin body dispatched through
    [host.eval_tool_body]. *)
type tool_body = Builtin of string | Plugin

type turn_event =
  | Assistant_text of string
  | Reasoning_text of string  (* provider chain-of-thought chunk: rendered
                                 into the turn's "thinking + tools" zone,
                                 kept OUT of history *)
  | Tool_begin of string * string
  | Tool_end of string * string
  | Turn_error of string

(** The host seam: everything the loop needs from its embedding. Every
    value crossing it is Yojson; the loop never sees a host value type.

    - [tool_order] live tool-name order (register-tool appends);
    - [tool_entry] dispatch shape for one tool name ([Builtin prim] runs
      under [call_prim], [Plugin] under [eval_tool_body]);
    - [fire] run event hooks ("input" / "tool_call" / "tool_result"),
      returning their values (a {"block": true, "reason": s} hook result
      short-circuits a tool call);
    - [eval_tool_body] run a plugin tool's body with JSON args (the host
      contains errors as {"error": ...} values);
    - [call_prim] dispatch a builtin tool's prim under the host's rim +
      serialization (eval lock on the agent, proxy rim on pp-remote). The
      host contains its own dispatch errors as {"error": ...} values. *)
type host = {
  tool_order : unit -> string list;
  tool_entry : string -> tool_body option;
  fire : string -> Yojson.Safe.t list -> Yojson.Safe.t list;
  eval_tool_body : string -> Yojson.Safe.t -> Yojson.Safe.t;
  call_prim : string -> Yojson.Safe.t -> Yojson.Safe.t;
}

(** Tool defs visible to the provider: builtin schemas plus
    plugin-registered tools with synthesized permissive schemas
    (v0.0: plugins can't describe params on the wire yet — spec notes
    alist -> JSON schema mapping as follow-on). *)
let tool_defs (h : host) : Provider.tool_def list =
  List.filter_map
    (fun name ->
      match Tools.schema_of name with
      | Some schema -> Some { Provider.name; description = "builtin " ^ name; schema }
      | None ->
          (* plugin tool: permissive single-object schema *)
          Some
            {
              Provider.name;
              description = "plugin tool " ^ name;
              schema =
                `Assoc
                  [
                    ("type", `String "object");
                    ("additionalProperties", `Bool true);
                  ];
            })
    (h.tool_order ())

(** Execute one tool call: builtin through [call_prim], plugin through
    [eval_tool_body]. Routes through [tool_call] / [tool_result] events so
    hooks see builtins too. Returns the tool-result string that lands
    verbatim in the tool-role message. *)
let exec_tool (h : host) (call : Provider.tool_call) : string =
  let payload =
    `Assoc
      [
        ("id", `String call.id);
        ("name", `String call.name);
        ("input", call.input);
      ]
  in
  (* tool_call event: any handler returning {"block": true, "reason": s}
     short-circuits. *)
  let results = h.fire "tool_call" [ payload ] in
  let blocked =
    List.find_map
      (fun j ->
        match j with
        | `Assoc kvs -> (
            match List.assoc_opt "block" kvs with
            | Some (`Bool true) ->
                Some
                  (match List.assoc_opt "reason" kvs with
                  | Some (`String r) -> r
                  | _ -> "blocked by plugin")
            | _ -> None)
        | _ -> None)
      results
  in
  let out =
    match blocked with
    | Some reason ->
        Yojson.Safe.to_string
          (`Assoc [ ("error", `String ("blocked: " ^ reason)) ])
    | None -> (
        match h.tool_entry call.name with
        | None ->
            Yojson.Safe.to_string
              (`Assoc [ ("error", `String ("no tool named " ^ call.name)) ])
        | Some (Builtin prim) -> (
            match h.call_prim prim call.input with
            | `String s -> s
            | j -> Yojson.Safe.to_string j)
        | Some Plugin -> (
            match h.eval_tool_body call.name call.input with
            | `String s -> s
            | j -> Yojson.Safe.to_string j))
  in
  ignore
    (h.fire "tool_result"
       [
         `Assoc
           [
             ("id", `String call.id);
             ("name", `String call.name);
             ("result", `String out);
           ];
       ]);
  out

(** One user prompt -> full agent run. Appends user/assistant/tool
    messages into [history] (mutable stack). *)
let run ~(env : Eio_unix.Stdenv.base) ~sw ~(host : host)
    ~(cfg : Provider.cfg) ~(complete :
        env:Eio_unix.Stdenv.base ->
        sw:Eio.Switch.t ->
        cfg:Provider.cfg ->
        messages:Provider.message list ->
        tools:Provider.tool_def list ->
        on_delta:(Provider.delta -> unit) ->
        (Provider.reply, string) Result.t)
    ?(max_turns = max_turns_default)
    ?(on_usage : (Provider.usage -> unit) option)
    ?(on_msg : (Provider.message -> unit) option)
    ~(on_event : turn_event -> unit) (history : Provider.message list ref)
    (prompt : string) : (unit, string) Result.t =
  let on_msg = Option.value on_msg ~default:ignore in
  let on_usage = Option.value on_usage ~default:ignore in
  let user_msg =
    { Provider.role = "user"; content = `String prompt;
      tool_call_id = None; tool_calls = None }
  in
  on_msg user_msg;
  history := !history @ [ user_msg ];
  (* input + before_turn events *)
  ignore (host.fire "input" [ `String prompt ]);
  let rec go turn =
    if turn > max_turns then
        Error (Printf.sprintf "max turns exceeded (%d; raise with AGENT_MAX_TURNS)" max_turns)
    else
      match
        complete ~env ~sw ~cfg ~messages:!history ~tools:(tool_defs host)
          ~on_delta:(function
            | Provider.Reasoning s -> on_event (Reasoning_text s)
            | Provider.Text s -> on_event (Assistant_text s)
            | Provider.Done -> ())
      with
      | Error e -> Error e
      | Ok reply ->
            (* PROVIDER-STRUCTURED usage, per request: the loop hands the
               provider's own counts to the host instead of letting them
               evaporate (borge/run-primitive.borg budget-accounting). The host
               accumulates: one callback per provider request. *)
            (match reply.usage with
             | Some u -> on_usage u
             | None -> ());
          (* Replay the assistant turn in OPENAI MESSAGE SHAPE: content is the
             raw message's content field (never the whole message object —
             nesting it here produced provider 400s on turn 2), tool_calls
             replayed verbatim so following tool-role messages pair up. *)
          let raw_fields =
            match reply.raw with `Assoc fields -> fields | _ -> []
          in
          let content =
            match List.assoc_opt "content" raw_fields with
            | Some ((`String _ | `List _) as c) -> c
            | Some `Null when reply.text = "" -> `Null
            | Some `Null -> `String reply.text
            | None -> `String reply.text
            | Some _ -> `String reply.text
          in
          let tool_calls = List.assoc_opt "tool_calls" raw_fields in
          let assistant_msg =
            { Provider.role = "assistant"; content; tool_call_id = None;
              tool_calls }
          in
          on_msg assistant_msg;
          history := !history @ [ assistant_msg ];
          (match reply.calls with
           | [] -> Ok ()
           | calls ->
               List.iter
                 (fun (c : Provider.tool_call) ->
                   on_event (Tool_begin (c.name, Yojson.Safe.to_string c.input));
                   let out = exec_tool host c in
                   on_event (Tool_end (c.name, out));
                   (* OpenAI tool-result message: role=tool, tool_call_id
                      routes the result to the call that produced it,
                      content is the plain tool output string. *)
                   let tool_msg =
                     { Provider.role = "tool";
                       content = `String out;
                       tool_call_id = Some c.id;
                       tool_calls = None;
                     }
                   in
                   on_msg tool_msg;
                   history :=
                     !history
                     @ [ tool_msg ])
                 calls;
               go (turn + 1))
  in
  go 1
