(** Builtin tools (borge/agent.borg agent-loop): read, bash.
    Shapes follow pi's baseline so prompts/habits transfer. Builtin
    bodies are OCaml; plugin hooks (tool_call / tool_result events)
    still fire around them, so plugins can block or rewrite.

    HOST-AGNOSTIC: tool bodies take an explicit [resolve_path] (each
    host's path sandbox) and the host's Eio env (bash's process spawn
    + deadline live on it), and registration goes through an explicit
    [register] callback — no Runtime.t / Remote.t coupling, so the
    agent and pp-remote install the same builtin set. *)

open Yojson.Safe.Util

let max_read_bytes = 100_000 (* chars; no offset/limit in v0.0 *)

let tool_read ~(resolve_path : string -> string) (args : Yojson.Safe.t) : string =
  let path = args |> member "path" |> to_string in
  let resolved = resolve_path path in
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

(** Deadline for one bash invocation (seconds). Every builtin tool body
    runs as a prim while the host holds its eval lock (agent: eval
    mutex; pp-remote: proxy rim) — without a deadline one hung command
    (sleep 100000) wedged every later eval and chat turn. The deadline
    lives here, in the ONE bash tool both hosts install at their loop
    boundary (the agent's duplicate shell/run prim was retired with
    it). Override with PP_BASH_TIMEOUT_SECS. *)
let default_bash_timeout = 120.0

let bash_timeout_secs () =
  match Sys.getenv_opt "PP_BASH_TIMEOUT_SECS" with
  | None -> default_bash_timeout
  | Some s -> (
      match Float.of_string_opt s with
      | Some v when Float.is_finite v && v > 0.0 -> v
      | _ -> default_bash_timeout)

let max_bash_out = 50 * 1024 (* chars; excess truncated and flagged *)

(** Run [cmd] under [/bin/sh -c] (stderr merged — the old
    [open_process_in (cmd ^ " 2>&1")] shape) and capture output up to
    [max_bash_out] bytes; excess is truncated and flagged, never an
    error. Pipe + single_read drain: read to EOF, then reap. No
    deadline of its own — [tool_bash] wraps this in
    [Eio.Time.with_timeout], whose cancellation releases the switch and
    SIGKILLs the child (borge/agent.borg tool-registry: "bash runs via
    Eio.Process with timeout and output truncation"). *)
let run_bash_capture ~(env : Eio_unix.Stdenv.base) (cmd : string) :
    int * string * bool =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run (fun sw ->
      let out_r, out_w = Eio.Process.pipe ~sw proc_mgr in
      let proc =
        Eio.Process.spawn ~sw proc_mgr ~stdout:out_w
          [ "/bin/sh"; "-c"; cmd ^ " 2>&1" ]
      in
      (* The child holds its own dup of the write end; dropping ours is
         what lets the read below see EOF when the command exits. *)
      Eio.Flow.close out_w;
      let buf = Buffer.create 4096 in
      let cs = Cstruct.create 4096 in
      let truncated = ref false in
      (try
         while true do
           let n = Eio.Flow.single_read out_r cs in
           if Buffer.length buf < max_bash_out then begin
             let take = min n (max_bash_out - Buffer.length buf) in
             Buffer.add_string buf (Cstruct.to_string (Cstruct.sub cs 0 take));
             if take < n then truncated := true
           end
           else truncated := true
         done
       with End_of_file -> ());
      let code =
        match Eio.Process.await proc with
        | `Exited c -> c
        | `Signaled _ -> -1
      in
      (code, Buffer.contents buf, !truncated))

let tool_bash ~(env : Eio_unix.Stdenv.base) (args : Yojson.Safe.t) : string =
  let cmd = args |> member "command" |> to_string in
  let timeout = bash_timeout_secs () in
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) timeout (fun () ->
        Ok (run_bash_capture ~env cmd))
  with
  | Ok (code, out, truncated) ->
      Yojson.Safe.to_string
        (`Assoc
          [
            ("exit", `Int code);
            ("out", `String out);
            ("truncated", `Bool truncated);
          ])
  | Error `Timeout ->
      (* Deadline fired: cancellation released the switch, SIGKILLing
         the child. Report a timeout error instead of holding the eval
         lock until the command finishes on its own. *)
      Yojson.Safe.to_string
        (`Assoc
          [
            ("exit", `Int (-1));
            ("out", `String "");
            ("truncated", `Bool false);
            ( "error",
              `String
                (Printf.sprintf "timeout: command exceeded %gs and was killed"
                   timeout) );
          ])

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

let install ~(env : Eio_unix.Stdenv.base)
    ~(resolve_path : string -> string)
    ~(register : string -> (Yojson.Safe.t -> string) -> unit) : unit =
  (* [register] is the host's builtin-tool registration (agent:
     Runtime.register_builtin_tool; pp-remote: Remote.register_builtin_
     tool) — same registry shape on both, so plugin hooks see builtins
     identically. *)
  register "read" (tool_read ~resolve_path);
  register "bash" (tool_bash ~env)

let schema_of (_name : string) : Yojson.Safe.t option =
  match _name with
  | "read" -> Some read_schema
  | "bash" -> Some bash_schema
  | _ -> None
