(** Agent turn loop (borge/agent.borg agent-loop) — HOST-AGNOSTIC.

    The loop is shared by every host that embeds it: the agent's
    Runtime.t and pp-remote's Remote.t both link this library and supply
    a small [host] record (tool registry, event handlers, tool-body
    eval, serialized prim dispatch). The provider (provider.ml) and the
    builtin read/bash tool bodies (tools.ml) are pure OCaml with zero
    host coupling — the API key is a value, the HTTP client is a
    library, so any client process can run the LLM turn locally while
    its DB prim calls cross whatever rim the host installs (in-process
    table on the agent, /api/prim proxy on pp-remote). *)

open Pricklypear_lisp
open Ast

let max_turns_default = 40

(** Tool registry entry: a Nopales closure (plugin tool) or a VPrim
    naming a builtin's registered prim. Structural record — the hosts'
    own registry_entry definitions are compatible with it. *)
type registry_entry = { doc : string; run : Ast.value }

type turn_event =
  | Assistant_text of string
  | Reasoning_text of string  (* provider chain-of-thought chunk: rendered
                                 into the turn's "thinking + tools" zone,
                                 kept OUT of history *)
  | Tool_begin of string * string
  | Tool_end of string * string
  | Turn_error of string

(** Parse a host/plugin-produced value back to host JSON. VStr is
    treated as raw JSON first (dict prims produce JSON strings), else a
    plain string. Moved here from the agent's runtime.ml — pure, and the
    loop needs it to inspect plugin handler results. *)
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

(** The host seam: everything the loop needs from its embedding.

    - [tool_order] live tool-name order (register-tool appends);
    - [tool_entry] registry lookup by name;
    - [fire] run event handlers ("input" / "tool_call" /
      "tool_result"), returning their values (a {"block": true,
      "reason": s} handler result short-circuits a tool call);
    - [eval_tool_body] run a plugin tool's Nopales closure with JSON
      args (errors contained to {"error": ...} values);
    - [call_prim] dispatch a builtin tool's registered prim under the
      host's rim + serialization (eval lock on the agent, proxy rim on
      pp-remote). Raises [Interp_error]; the loop contains it. *)
type host = {
  tool_order : unit -> string list;
  tool_entry : string -> registry_entry option;
  fire : string -> Ast.value list -> Ast.value list;
  eval_tool_body : string -> string -> Ast.value;
  call_prim : string -> Ast.value list -> Ast.value;
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

(** Execute one tool call: plugin tool via Nopales closure, builtin via
    its registered VPrim. Routes through [tool_call] / [tool_result]
    events so hooks see builtins too. *)
let exec_tool (h : host) (call : Provider.tool_call) : string =
  let payload =
    Ast.VStr
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("id", `String call.id);
             ("name", `String call.name);
             ("input", call.input);
           ]))
  in
  (* tool_call event: any handler returning {"block": true, "reason": s}
     short-circuits. *)
  let results = h.fire "tool_call" [ payload ] in
  let blocked =
    List.find_map
      (fun v ->
        match json_of_value v with
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
    | None ->
        (match h.tool_entry call.name with
         | None ->
             Yojson.Safe.to_string
               (`Assoc [ ("error", `String ("no tool named " ^ call.name)) ])
         | Some entry -> (
             let args_json = Yojson.Safe.to_string call.input in
             match entry.run with
             | Ast.VPrim prim_name ->
                 (* builtin: call the prim directly through the host rim *)
                 (try
                    match h.call_prim prim_name [ Ast.VStr args_json ] with
                    | Ast.VStr s -> s
                    | other -> Reader.pp_value other
                  with Ast.Interp_error ie ->
                    Yojson.Safe.to_string
                      (`Assoc [ ("error", `String (Ast.pp_interp_error ie)) ]))
             | Ast.VLambda _ ->
                 (match h.eval_tool_body call.name args_json with
                  | Ast.VStr s -> s
                  | other -> Reader.pp_value other)
             | _ ->
                 Yojson.Safe.to_string
                   (`Assoc [ ("error", `String "malformed tool entry") ])))
  in
  ignore
    (h.fire "tool_result"
       [ Ast.VStr
           (Yojson.Safe.to_string
              (`Assoc
                [ ("id", `String call.id); ("name", `String call.name);
                  ("result", `String out) ])) ]);
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
    ?(on_msg : (Provider.message -> unit) option)
    ~(on_event : turn_event -> unit) (history : Provider.message list ref)
    (prompt : string) : (unit, string) Result.t =
  let on_msg = Option.value on_msg ~default:ignore in
  let user_msg =
    { Provider.role = "user"; content = `String prompt;
      tool_call_id = None; tool_calls = None }
  in
  on_msg user_msg;
  history := !history @ [ user_msg ];
  (* input + before_turn events *)
  ignore (host.fire "input" [ Ast.VStr prompt ]);
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
