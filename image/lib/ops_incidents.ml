(** Error fingerprints → ops-incident rows + note/todo work items.

    Detection is deterministic. LLM enrichment is optional later.
    See product-experience.reliability-work-queue. *)
open Lwt.Syntax
open Pricklypear_lisp.Ast

let type_name = "ops-incident"

let iso_now () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let short_fp (fp : string) : string =
  let open Digestif.SHA256 in
  let hex = to_hex (digest_string fp) in
  String.sub hex 0 (min 10 (String.length hex))

let fields_of_json s =
  try
    match Yojson.Safe.from_string s with
    | `Assoc xs -> xs
    | _ -> []
  with _ -> []

let json_get fields key =
  match List.assoc_opt key fields with
  | Some (`String s) -> s
  | Some (`Int n) -> string_of_int n
  | Some (`Bool b) -> if b then "true" else "false"
  | Some j -> Yojson.Safe.to_string j
  | None -> ""

let json_get_int fields key default =
  match List.assoc_opt key fields with
  | Some (`Int n) -> n
  | Some (`String s) ->
      (match int_of_string_opt s with Some n -> n | None -> default)
  | _ -> default

let find_open_incident (store : Store.t) ~(fingerprint : string)
  : (string * (string * Yojson.Safe.t) list) option Lwt.t =
  let* listed =
    Crud.find_rows store ~type_name
      ~filters:[ ("fingerprint", fingerprint) ]
      ~limit:20 ()
  in
  match listed with
  | VList items ->
      let rec go = function
        | [] -> Lwt.return_none
        | VPair (VStr id, VStr fj) :: rest ->
            let fields = fields_of_json fj in
            let st = json_get fields "status" in
            if st = "open" || st = "" then Lwt.return_some (id, fields)
            else go rest
        | _ :: rest -> go rest
      in
      go items
  | _ -> Lwt.return_none

let escape_lisp_str (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let file_todo_and_note (session : Session_types.t) ~(title : string)
    ~(body : string) : string option Lwt.t =
  let title_e = escape_lisp_str title in
  let body_e = escape_lisp_str body in
  let expr =
    Printf.sprintf
      {|(let ((tid (todo/item/add (todo/list/ensure-inbox) "%s")))
          (seq
            (if (null? tid) () (update-field 'todo-item tid "notes" "%s"))
            (notes/capture/add "%s" "%s")
            tid))|}
      title_e body_e title_e body_e
  in
  Lwt.catch
    (fun () ->
      let* v =
        Control_rim.run_lwt ~prims:session.prims (fun () ->
            Pricklypear_lisp.Eval.eval_string_direct
              ~prims:Pricklypear_lisp.Eval.empty_prims session.env expr)
      in
      match v with
      | VStr s when String.trim s <> "" -> Lwt.return (Some s)
      | _ -> Lwt.return_none)
    (fun exn ->
      ignore
        (Ops_log.warn ~component:"ops"
           ("file_todo_and_note failed: " ^ Printexc.to_string exn));
      let fields_json =
        Yojson.Safe.to_string
          (`Assoc
             [ ("list_id", `String "")
             ; ("title", `String title)
             ; ("notes", `String body)
             ; ("state", `String "TODO")
             ; ("due", `String "")
             ; ("position", `String "0")
             ])
      in
      let* v =
        Crud.create session.store ~type_name:"todo-item" ~fields_json
          ~author:"ops"
      in
      match v with VStr s -> Lwt.return (Some s) | _ -> Lwt.return_none)

let record_event (session : Session_types.t) (ev : Ops_log.event)
  : unit Lwt.t =
  match ev.Ops_log.level with
  | Ops_log.Info | Ops_log.Warn -> Lwt.return_unit
  | Ops_log.Error | Ops_log.Fatal ->
      Ops_stats.note_error ~fatal:(ev.level = Ops_log.Fatal);
      let store = session.store in
      let now = iso_now () in
      let* existing = find_open_incident store ~fingerprint:ev.fingerprint in
      (match existing with
       | Some (id, fields) ->
           let count = json_get_int fields "count" 1 + 1 in
           let fields_json =
             Yojson.Safe.to_string
               (`Assoc
                  [ ("fingerprint", `String ev.fingerprint)
                  ; ("level", `String (Ops_log.level_to_string ev.level))
                  ; ("component", `String ev.component)
                  ; ("count", `Int count)
                  ; ("first_seen", `String (json_get fields "first_seen"))
                  ; ("last_seen", `String now)
                  ; ("status", `String "open")
                  ; ("note_slug", `String (json_get fields "note_slug"))
                  ; ("todo_id", `String (json_get fields "todo_id"))
                  ; ("sample_msg", `String ev.msg)
                  ; ("ai_enriched", `String (json_get fields "ai_enriched"))
                  ])
           in
           let* _ = Crud.update store ~type_name ~id ~fields_json in
           Lwt.return_unit
       | None ->
           let sfp = short_fp ev.fingerprint in
           let title =
             Printf.sprintf "[ops] %s: %s" ev.component
               (let m = ev.msg in
                if String.length m > 72 then String.sub m 0 72 ^ "…" else m)
           in
           let body =
             Printf.sprintf
               "Reliability incident (auto-filed)\n\n\
                fingerprint: %s\n\
                component: %s\n\
                level: %s\n\
                first_seen: %s\n\
                sample: %s\n\n\
                Filed as a TODO so it can be fixed. Deduped by fingerprint."
               ev.fingerprint ev.component
               (Ops_log.level_to_string ev.level)
               now ev.msg
           in
           let* todo_id = file_todo_and_note session ~title ~body in
           let fields_json =
             Yojson.Safe.to_string
               (`Assoc
                  [ ("fingerprint", `String ev.fingerprint)
                  ; ("level", `String (Ops_log.level_to_string ev.level))
                  ; ("component", `String ev.component)
                  ; ("count", `Int 1)
                  ; ("first_seen", `String now)
                  ; ("last_seen", `String now)
                  ; ("status", `String "open")
                  ; ("note_slug", `String ("ops-" ^ sfp))
                  ; ( "todo_id"
                    , `String (Option.value todo_id ~default:"") )
                  ; ("sample_msg", `String ev.msg)
                  ; ("ai_enriched", `String "false")
                  ])
           in
           let* _ =
             Crud.create store ~type_name ~fields_json ~author:"ops"
           in
           ignore
             (Ops_log.info ~component:"ops"
                ("filed incident " ^ sfp
                ^
                match todo_id with
                | Some t -> " todo=" ^ t
                | None -> ""));
           Lwt.return_unit)

(** Classify an exception as an *expected* runtime/user error that
    should be logged but NOT filed as a reliability incident.

    Route-handler eval can raise deterministic [Interp_error] variants
    that are the user's fault (bad input, wrong arity, runaway step
    budget), not a reliability fault in the image. These still get
    emitted to the ops log (stderr + ring) so they're observable, but
    [record_event] is skipped so they don't spawn a TODO/incident row
    for every user mistake.

    Genuine faults — [Not_a_function], [Prim_error], [Unbound_var],
    [Parse_error] (a seeded route shouldn't fail to parse at runtime),
    and any non-[Interp_error] exception — are still filed. *)
let is_expected_runtime_error (exn : exn) : bool =
  match exn with
  | Interp_error e ->
      (match e with
       | Budget_exceeded -> true
       | User_error _ -> true
       | Arity_mismatch _ -> true
       | Wrong_arg_count _ -> true
       | Type_mismatch _ -> true
       | _ -> false)
  | _ -> false

let emit_and_maybe_file (session : Session_types.t) ?(context = [])
    ?(file = true) ~(level : Ops_log.level) ~(component : string)
    (msg : string)
  : unit Lwt.t =
  let ev = Ops_log.emit ~context ~level ~component msg in
  if file then record_event session ev else Lwt.return_unit

let list_open_incidents (store : Store.t) : Yojson.Safe.t Lwt.t =
  let* listed = Crud.list store ~type_name ~limit:100 () in
  let items =
    match listed with
    | VList rows ->
        List.filter_map
          (function
            | VPair (VStr id, VStr fj) ->
                let fields = fields_of_json fj in
                let status = json_get fields "status" in
                if status = "resolved" then None
                else
                  Some
                    (`Assoc
                       [ ("id", `String id)
                       ; ( "fingerprint"
                         , `String (json_get fields "fingerprint") )
                       ; ("component", `String (json_get fields "component"))
                       ; ("count", `Int (json_get_int fields "count" 1))
                       ; ("level", `String (json_get fields "level"))
                       ; ("sample_msg", `String (json_get fields "sample_msg"))
                       ; ("todo_id", `String (json_get fields "todo_id"))
                       ; ("last_seen", `String (json_get fields "last_seen"))
                       ])
            | _ -> None)
          rows
    | _ -> []
  in
  Lwt.return (`List items)

let count_logins (store : Store.t) : (int * int) Lwt.t =
  match Store.current_conn store with
  | None -> Lwt.return (0, 0)
  | Some conn ->
      let day = Ops_stats.today_str () in
      let count ~success =
        let sql =
          "SELECT COUNT(*)::int FROM auth_log " ^
          "WHERE success = $1 AND kind IN ('password','mint') " ^
          "AND happened_at::date = $2::date"
        in
        Lwt.catch
          (fun () ->
            let* rows =
              Store.Pg.execute conn
                ~params:
                  [ Pgx.Value.of_bool success; Pgx.Value.of_string day ]
                sql
            in
            match rows with
            | [ cell ] :: _ ->
                (match Pgx.Value.to_int cell with
                 | Some n -> Lwt.return n
                 | None -> Lwt.return 0)
            | _ -> Lwt.return 0)
          (fun _ -> Lwt.return 0)
      in
      let* ok = count ~success:true in
      let* fail = count ~success:false in
      Lwt.return (ok, fail)
