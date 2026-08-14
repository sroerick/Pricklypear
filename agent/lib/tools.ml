(** Builtin tools (borge/agent.borg agent-loop): read, bash.
    Shapes follow pi's baseline so prompts/habits transfer. Builtin
    bodies are OCaml; plugin hooks (tool_call / tool_result events)
    still fire around them, so plugins can block or rewrite. *)

open Yojson.Safe.Util

let max_read_bytes = 100_000 (* chars; no offset/limit in v0.0 *)

let tool_read ~(rt : Runtime.t) (args : Yojson.Safe.t) : string =
  ignore rt;
  let path = args |> member "path" |> to_string in
  let resolved = Runtime.resolve_path path in
  match
    (try
       let ic = open_in_bin resolved in
       let n = min (in_channel_length ic) max_read_bytes in
       let s = really_input_string ic n in
       close_in ic;
       Ok s
     with Sys_error e -> Error e)
  with
  | Error e -> Yojson.Safe.to_string (`Assoc [ ("error", `String e) ])
  | Ok s ->
      Yojson.Safe.to_string
        (`Assoc [ ("path", `String resolved); ("content", `String s) ])

let tool_bash ~(rt : Runtime.t) (args : Yojson.Safe.t) : string =
  ignore rt;
  let cmd = args |> member "command" |> to_string in
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  let code = match Unix.close_process_in ic with Unix.WEXITED c -> c | _ -> -1 in
  let out = Buffer.contents buf in
  let max_out = 50 * 1024 in
  let out, truncated =
    if String.length out > max_out then (String.sub out 0 max_out, true)
    else (out, false)
  in
  Yojson.Safe.to_string
    (`Assoc
      [ ("exit", `Int code); ("out", `String out); ("truncated", `Bool truncated) ])

(* JSON schemas in provider-neutral shape (JSON schema). *)
let read_schema : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "path",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Path, relative to the working directory");
                ] );
          ] );
      ("required", `List [ `String "path" ]);
    ]

let bash_schema : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "command",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Shell command to run");
                ] );
          ] );
      ("required", `List [ `String "command" ]);
    ]

let install ~(rt : Runtime.t) : unit =
  Runtime.register_builtin_tool rt "read" ~run:(tool_read ~rt);
  Runtime.register_builtin_tool rt "bash" ~run:(tool_bash ~rt)

let schema_of (_name : string) : Yojson.Safe.t option =
  match _name with
  | "read" -> Some read_schema
  | "bash" -> Some bash_schema
  | _ -> None
