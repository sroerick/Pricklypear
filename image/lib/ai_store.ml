(** Durable agent conversation store.

    Spec [ai-authoring.agent-portal]: ai_sessions + ai_messages.
    No LLM calls here — maps rows <-> Llm_client.chat_message. *)

type session = {
  id : string;
  owner : string;
  title : string option;
  seed : string;
  branch_ref : string option;
  tools : bool;
  model : string option;
  created_at : string;
  updated_at : string;
}

let pg_conn (store : Store.t) : Io_db.Db.conn option = Io_db.Db.current_conn store

(* row-cell accessors live in Row (was a local copy here). *)

let row_to_session row : session option =
  match
    ( Row.cell_string row 0
    , Row.cell_string row 1
    , Row.cell_string row 2
    , Row.cell_string row 3
    , Row.cell_string row 4
    , Row.cell_string row 6
    , Row.cell_string row 7
    , Row.cell_string row 8 )
  with
  | ( Some id
    , Some owner
    , title
    , Some seed
    , branch_ref
    , model
    , Some created_at
    , Some updated_at ) ->
      Some
        { id
        ; owner
        ; title
        ; seed
        ; branch_ref
        ; tools = Row.cell_bool row 5
        ; model
        ; created_at
        ; updated_at
        }
  | _ -> None

let session_select_cols =
  "id::text, owner, title, seed, branch_ref, tools::text, model, \
   created_at::text, updated_at::text"

let create_session
    (store : Store.t)
    ~(owner : string)
    ~(seed : string)
    ?title
    ?branch_ref
    ?(tools = true)
    ?model
    () : (session, string) result =
  match pg_conn store with
  | None -> (Error "ai_store: no database")
  | Some conn ->
      (try
      let sql =
            "INSERT INTO ai_sessions \
             (owner, title, seed, branch_ref, tools, model) \
             VALUES ($1, $2, $3, $4, $5, $6) \
             RETURNING " ^ session_select_cols
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string owner
                ; (match title with
                   | Some t -> Pgx.Value.of_string t
                   | None -> Pgx.Value.null)
                ; Pgx.Value.of_string seed
                ; (match branch_ref with
                   | Some b -> Pgx.Value.of_string b
                   | None -> Pgx.Value.null)
                ; Pgx.Value.of_bool tools
                ; (match model with
                   | Some m -> Pgx.Value.of_string m
                   | None -> Pgx.Value.null)
                ]
              sql
          in
          match rows with
          | row :: _ ->
              (match row_to_session row with
               | Some s -> (Ok s)
               | None -> (Error "ai_store: create parse failed"))
          | [] -> (Error "ai_store: create returned no row")
    with exn ->
      (Error ("ai_store create: " ^ Printexc.to_string exn)))

let get_session (store : Store.t) ~(owner : string) ~(id : string)
    : session option =
  match pg_conn store with
  | None -> None
  | Some conn ->
      (try
      let sql =
            "SELECT " ^ session_select_cols ^
            " FROM ai_sessions WHERE id = $1::uuid AND owner = $2 LIMIT 1"
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string id; Pgx.Value.of_string owner ]
              sql
          in
          match rows with
          | row :: _ -> (row_to_session row)
          | [] -> None
    with exn ->
      Printf.eprintf "[ai_store] get_session: %s\n%!"
            (Printexc.to_string exn);
          None)

let list_sessions (store : Store.t) ~(owner : string) ?(limit = 50) ()
    : session list =
  match pg_conn store with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT " ^ session_select_cols ^
            " FROM ai_sessions WHERE owner = $1 \
             ORDER BY updated_at DESC LIMIT $2"
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string owner
                ; Pgx.Value.of_string (string_of_int limit)
                ]
              sql
          in
          (List.filter_map row_to_session rows)
    with exn ->
      Printf.eprintf "[ai_store] list_sessions: %s\n%!"
            (Printexc.to_string exn);
          [])

let delete_session (store : Store.t) ~(owner : string) ~(id : string)
    : bool =
  match pg_conn store with
  | None -> false
  | Some conn ->
      (try
      let sql =
            "DELETE FROM ai_sessions \
             WHERE id = $1::uuid AND owner = $2 \
             RETURNING id::text"
          in
          let rows =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string id; Pgx.Value.of_string owner ]
              sql
          in
          (rows <> [])
    with exn ->
      Printf.eprintf "[ai_store] delete_session: %s\n%!"
            (Printexc.to_string exn);
          false)

let touch_session
    (store : Store.t)
    ~(id : string)
    ?title
    ?branch_ref
    () : unit =
  match pg_conn store with
  | None -> ()
  | Some conn ->
      (try
      let sql =
            "UPDATE ai_sessions SET \
             updated_at = now(), \
             title = COALESCE($2, title), \
             branch_ref = COALESCE($3, branch_ref) \
             WHERE id = $1::uuid"
          in
          let _ =
            Store_db.exec conn
              ~params:
                [ Pgx.Value.of_string id
                ; (match title with
                   | Some t -> Pgx.Value.of_string t
                   | None -> Pgx.Value.null)
                ; (match branch_ref with
                   | Some b -> Pgx.Value.of_string b
                   | None -> Pgx.Value.null)
                ]
              sql
          in
          ()
    with exn ->
      Printf.eprintf "[ai_store] touch_session: %s\n%!"
            (Printexc.to_string exn);
          ())

(** Encode tool_calls list as JSON for jsonb column. *)
let tool_calls_to_json (tcs : Llm_client.tool_call list) : string =
  Yojson.Safe.to_string
    (`List
       (List.map
          (fun (tc : Llm_client.tool_call) ->
            `Assoc
              [ ("id", `String tc.id)
              ; ("name", `String tc.name)
              ; ("arguments", `String tc.arguments)
              ])
          tcs))

let tool_calls_of_json_string (s : string) : Llm_client.tool_call list =
  try
    match Yojson.Safe.from_string s with
    | `List xs ->
        List.filter_map
          (function
            | `Assoc items ->
                let id =
                  match List.assoc_opt "id" items with
                  | Some (`String x) -> Some x
                  | _ -> None
                in
                let name =
                  match List.assoc_opt "name" items with
                  | Some (`String x) -> Some x
                  | _ -> None
                in
                let arguments =
                  match List.assoc_opt "arguments" items with
                  | Some (`String x) -> x
                  | Some other -> Yojson.Safe.to_string other
                  | None -> "{}"
                in
                (match id, name with
                 | Some id, Some name ->
                     Some ({ id; name; arguments } : Llm_client.tool_call)
                 | _ -> None)
            | _ -> None)
          xs
    | _ -> []
  with _ -> []

let chat_message_to_row (m : Llm_client.chat_message)
    : (string * string option * string option * string option) option =
  match m with
  | Llm_client.Msg { role = Llm_client.System; _ } ->
      (* System is session.seed only — never stored. *)
      None
  | Llm_client.Msg { role = Llm_client.User; content } ->
      Some ("user", Some content, None, None)
  | Llm_client.Msg { role = Llm_client.Assistant; content } ->
      Some ("assistant", Some content, None, None)
  | Llm_client.Msg { role = Llm_client.Tool; content } ->
      (* Bare tool role without tool_call_id is incomplete; still store. *)
      Some ("tool", Some content, None, None)
  | Llm_client.Assistant_tools { content; tool_calls } ->
      Some
        ( "assistant"
        , content
        , Some (tool_calls_to_json tool_calls)
        , None )
  | Llm_client.Tool_result { tool_call_id; content } ->
      Some ("tool", Some content, None, Some tool_call_id)

let row_to_chat_message
    ~(role : string)
    ~(content : string option)
    ~(tool_calls : string option)
    ~(tool_call_id : string option) : Llm_client.chat_message option =
  match role with
  | "user" ->
      Some
        (Llm_client.Msg
           { role = Llm_client.User
           ; content = Option.value content ~default:""
           })
  | "assistant" ->
      (match tool_calls with
       | Some tc when String.trim tc <> "" && tc <> "null" ->
           let tcs = tool_calls_of_json_string tc in
           if tcs = [] then
             Some
               (Llm_client.Msg
                  { role = Llm_client.Assistant
                  ; content = Option.value content ~default:""
                  })
           else
             Some (Llm_client.Assistant_tools { content; tool_calls = tcs })
       | _ ->
           Some
             (Llm_client.Msg
                { role = Llm_client.Assistant
                ; content = Option.value content ~default:""
                }))
  | "tool" ->
      (match tool_call_id with
       | Some tid ->
           Some
             (Llm_client.Tool_result
                { tool_call_id = tid
                ; content = Option.value content ~default:""
                })
       | None ->
           Some
             (Llm_client.Msg
                { role = Llm_client.Tool
                ; content = Option.value content ~default:""
                }))
  | _ -> None

let next_seq (conn : Io_db.Db.conn) ~(session_id : string) : int =
  let sql =
    "SELECT COALESCE(MAX(seq), -1)::text FROM ai_messages \
     WHERE session_id = $1::uuid"
  in
  let rows =
    Store_db.exec conn ~params:[ Pgx.Value.of_string session_id ] sql
  in
  match rows with
  | row :: _ ->
      (match Row.cell_string row 0 with
       | Some s ->
           ((int_of_string_opt s |> Option.value ~default:(-1)) + 1)
       | None -> 0)
  | [] -> 0

let list_messages (store : Store.t) ~(session_id : string)
    : Llm_client.chat_message list =
  match pg_conn store with
  | None -> []
  | Some conn ->
      (try
      let sql =
            "SELECT role, content, tool_calls::text, tool_call_id \
             FROM ai_messages \
             WHERE session_id = $1::uuid \
             ORDER BY seq ASC"
          in
          let rows =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string session_id ]
              sql
          in
          let msgs =
            List.filter_map
              (fun row ->
                match Row.cell_string row 0 with
                | None -> None
                | Some role ->
                    row_to_chat_message
                      ~role
                      ~content:(Row.cell_string row 1)
                      ~tool_calls:(Row.cell_string row 2)
                      ~tool_call_id:(Row.cell_string row 3))
              rows
          in
          msgs
    with exn ->
      Printf.eprintf "[ai_store] list_messages: %s\n%!"
            (Printexc.to_string exn);
          [])

let append_messages
    (store : Store.t)
    ~(session_id : string)
    (msgs : Llm_client.chat_message list) : (unit, string) result =
  if msgs = [] then (Ok ())
  else
    match pg_conn store with
    | None -> (Error "ai_store: no database")
    | Some conn ->
        (try
      let start = next_seq conn ~session_id in
            let rec go i = function
              | [] -> (Ok ())
              | m :: rest ->
                  (match chat_message_to_row m with
                   | None -> go i rest (* skip system *)
                   | Some (role, content, tool_calls, tool_call_id) ->
                       let sql =
                         "INSERT INTO ai_messages \
                          (session_id, seq, role, content, tool_calls, tool_call_id) \
                          VALUES ($1::uuid, $2, $3, $4, $5::jsonb, $6)"
                       in
                       let _ =
                         Store_db.exec conn
                           ~params:
                             [ Pgx.Value.of_string session_id
                             ; Pgx.Value.of_string (string_of_int i)
                             ; Pgx.Value.of_string role
                             ; (match content with
                                | Some c -> Pgx.Value.of_string c
                                | None -> Pgx.Value.null)
                             ; (match tool_calls with
                                | Some t -> Pgx.Value.of_string t
                                | None -> Pgx.Value.null)
                             ; (match tool_call_id with
                                | Some t -> Pgx.Value.of_string t
                                | None -> Pgx.Value.null)
                             ]
                           sql
                       in
                       go (i + 1) rest)
            in
            let r = go start msgs in
            (match r with
             | Error _ as e -> e
             | Ok () ->
                 let () = touch_session store ~id:session_id () in
                 (Ok ()))
    with exn ->
      (Error ("ai_store append: " ^ Printexc.to_string exn)))

(** Build the full messages list for a provider call:
    system seed (not stored) + durable history. *)
let messages_for_api (sess : session) (history : Llm_client.chat_message list)
    : Llm_client.chat_message list =
  Llm_client.Msg { role = Llm_client.System; content = sess.seed }
  :: history

(** Pretty history for Lisp/HTTP (no tool_calls detail required). *)
let history_summaries (store : Store.t) ~(session_id : string)
    : (string * string) list =
  let msgs = list_messages store ~session_id in
  let rec go acc = function
    | [] -> (List.rev acc)
    | Llm_client.Msg { role; content } :: rest ->
        let r =
          match role with
          | Llm_client.User -> "user"
          | Llm_client.Assistant -> "assistant"
          | Llm_client.System -> "system"
          | Llm_client.Tool -> "tool"
        in
        go ((r, content) :: acc) rest
    | Llm_client.Assistant_tools { content; tool_calls } :: rest ->
        let names =
          String.concat ","
            (List.map (fun (t : Llm_client.tool_call) -> t.name) tool_calls)
        in
        let c =
          match content with
          | Some s when String.trim s <> "" -> s ^ " [tools: " ^ names ^ "]"
          | _ -> "[tools: " ^ names ^ "]"
        in
        go (("assistant", c) :: acc) rest
    | Llm_client.Tool_result { tool_call_id; content } :: rest ->
        go
          (("tool", tool_call_id ^ " → " ^ content) :: acc)
          rest
  in
  go [] msgs
