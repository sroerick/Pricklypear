(** Agent turn loop (borge/agent.borg agent-loop). *)

open Pricklypear_lisp
open Ast

let max_turns_default = 8

type turn_event =
  | Assistant_text of string
  | Tool_begin of string * string
  | Tool_end of string * string

(** Tool defs visible to the provider: builtin schemas plus
    plugin-registered tools with synthesized permissive schemas
    (v0.0: plugins can't describe params on the wire yet — spec notes
    alist -> JSON schema mapping as follow-on). *)
let tool_defs (rt : Runtime.t) : Provider.tool_def list =
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
    rt.Runtime.tool_order

(** Execute one tool call: plugin tool via Nopales closure, builtin via
    its registered VPrim. Routes through [tool_call] / [tool_result]
    events so hooks see builtins too. *)
let exec_tool (rt : Runtime.t) (call : Provider.tool_call) : string =
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
  let results = Runtime.fire rt "tool_call" [ payload ] in
  let blocked =
    List.find_map
      (fun v ->
        match Runtime.json_of_value v with
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
        (match Hashtbl.find_opt rt.Runtime.tools call.name with
         | None ->
             Yojson.Safe.to_string
               (`Assoc [ ("error", `String ("no tool named " ^ call.name)) ])
         | Some entry -> (
             let args_json = Yojson.Safe.to_string call.input in
             match entry.run with
             | Ast.VPrim prim_name ->
                 (* builtin: call the prim directly through the rim *)
                 (try
                    match
                      Runtime.with_eval_lock rt
                        (fun () ->
                          Control.run_with_rim ~prims:rt.Runtime.prims
                            (fun () ->
                              (Hashtbl.find rt.Runtime.prims prim_name)
                                [ Ast.VStr args_json ]))
                    with
                    | Ast.VStr s -> s
                    | other -> Reader.pp_value other
                  with Ast.Interp_error ie ->
                    Yojson.Safe.to_string
                      (`Assoc [ ("error", `String (Ast.pp_interp_error ie)) ]))
             | Ast.VLambda _ ->
                 (match Runtime.eval_tool_body rt call.name args_json with
                  | Ast.VStr s -> s
                  | other -> Reader.pp_value other)
             | _ ->
                 Yojson.Safe.to_string
                   (`Assoc [ ("error", `String "malformed tool entry") ])))
  in
  ignore
    (Runtime.fire rt "tool_result"
       [ Ast.VStr
           (Yojson.Safe.to_string
              (`Assoc
                [ ("id", `String call.id); ("name", `String call.name);
                  ("result", `String out) ])) ]);
  out

(** One user prompt -> full agent run. Appends user/assistant/tool
    messages into [history] (mutable stack). *)
let run ~(env : Eio_unix.Stdenv.base) ~sw ~(rt : Runtime.t)
    ~(cfg : Provider.cfg) ~(complete :
        env:Eio_unix.Stdenv.base ->
        sw:Eio.Switch.t ->
        cfg:Provider.cfg ->
        messages:Provider.message list ->
        tools:Provider.tool_def list ->
        on_delta:(Provider.delta -> unit) ->
        (Provider.reply, string) Result.t)
    ?(max_turns = max_turns_default)
    ~(on_event : turn_event -> unit) (history : Provider.message list ref)
    (prompt : string) : (unit, string) Result.t =
  history := !history @ [ { Provider.role = "user"; content = `String prompt; tool_call_id = None } ];
  (* input + before_turn events *)
  ignore (Runtime.fire rt "input" [ Ast.VStr prompt ]);
  let rec go turn =
    if turn > max_turns then Error "max turns exceeded"
    else
      match
        complete ~env ~sw ~cfg ~messages:!history ~tools:(tool_defs rt)
          ~on_delta:(function
            | Provider.Text s -> on_event (Assistant_text s)
            | Provider.Done -> ())
      with
      | Error e -> Error e
      | Ok reply ->
          history := !history @ [ { role = "assistant"; content = reply.raw; tool_call_id = None } ];
          (match reply.calls with
           | [] -> Ok ()
           | calls ->
               List.iter
                 (fun (c : Provider.tool_call) ->
                   on_event (Tool_begin (c.name, Yojson.Safe.to_string c.input));
                   let out = exec_tool rt c in
                   on_event (Tool_end (c.name, out));
                   (* OpenAI tool-result message: role=tool, tool_call_id
                      routes the result to the call that produced it,
                      content is the plain tool output string. *)
                   history :=
                     !history
                     @ [
                         {
                           role = "tool";
                           content = `String out;
                           tool_call_id = Some c.id;
                         };
                       ])
                 calls;
               go (turn + 1))
  in
  go 1
