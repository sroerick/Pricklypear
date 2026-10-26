(** The interpreter-free default host (borge/glochid.borg agent-loop).

    The smallest [Loop.host]: the builtin read/bash bodies from Tools plus
    in-memory event hooks. No Nopales env, no plugin registry, no prim
    table — this is the host pricklypear-llm's own tests drive, and the
    shape any embedder gets for a turn loop without the interpreter. *)

type handler = Yojson.Safe.t -> Yojson.Safe.t

type t = {
  env : Eio_unix.Stdenv.base;
  resolve_path : string -> string;
  mutable handlers : (string * handler) list;
}

let create ~(env : Eio_unix.Stdenv.base) ?(resolve_path = fun p -> p) () : t =
  { env; resolve_path; handlers = [] }

(** Register a hook for one event ("tool_call" / "tool_result" / "input").
    Hooks see the event payload as JSON and return JSON; the loop inspects a
    {"block": true, "reason": s} tool_call result. *)
let on (t : t) (ev : string) (h : handler) : unit =
  t.handlers <- t.handlers @ [ (ev, h) ]

let tool_order () = [ "read"; "bash" ]

let tool_entry = function
  | "read" -> Some (Loop.Builtin "read")
  | "bash" -> Some (Loop.Builtin "bash")
  | _ -> None

(** One handler result per (handler, payload) pair, in registration order. *)
let fire (t : t) (ev : string) (payloads : Yojson.Safe.t list) :
    Yojson.Safe.t list =
  let hs =
    List.filter_map (fun (e, h) -> if e = ev then Some h else None) t.handlers
  in
  List.concat_map (fun payload -> List.map (fun h -> h payload) hs) payloads

(** Builtin dispatch. Tool-body errors (bad args, unreadable file) are
    contained into {"error": ...} values, the contract the loop expects. *)
let call_prim (t : t) (name : string) (input : Yojson.Safe.t) : Yojson.Safe.t
    =
  try
    match name with
    | "read" -> `String (Tools.tool_read ~resolve_path:t.resolve_path input)
    | "bash" -> `String (Tools.tool_bash ~env:t.env input)
    | other -> `Assoc [ ("error", `String ("no builtin prim " ^ other)) ]
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      `Assoc [ ("error", `String ("bad tool args: " ^ msg)) ]
  | Sys_error msg -> `Assoc [ ("error", `String msg) ]

let host (t : t) : Loop.host =
  {
    Loop.tool_order;
    tool_entry;
    fire = fire t;
    eval_tool_body =
      (fun name _ ->
        `Assoc [ ("error", `String ("no plugin tool named " ^ name)) ]);
    call_prim = call_prim t;
  }