(** erde export: training trajectories out of the turn journal.

    The journal is newline-delimited JSON, one object per turn event. Each
    object carries "session", "role" and "content"; "role" is "user",
    "assistant" or a tool role. A training trajectory is the ordered
    user/assistant exchanges of one session, written as its own JSONL file:
    in every turn the user line comes before the assistant line:

      {"role":"user","content":"…"}
      {"role":"assistant","content":"…"}

    A journal (or a session) with no user/assistant pair yields no file at
    all, so the caller's out dir stays exactly as it was. *)

type entry = { session : string; role : string; content : string }

let string_field json key =
  match json with
  | `Assoc items -> (
      match List.assoc_opt key items with
      | Some (`String s) -> s
      | Some (`Int i) -> string_of_int i
      | _ -> "")
  | _ -> ""

let parse_line line =
  let line = String.trim line in
  if line = "" then None
  else
    match Yojson.Safe.from_string line with
    | json ->
        let role = string_field json "role" in
        if role = "" then None
        else
          let session =
            match string_field json "session" with
            | "" -> "default"
            | s -> s
          in
          Some { session; role; content = string_field json "content" }
    | exception _ -> None

let read_journal path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let rec go acc =
        match input_line ic with
        | line ->
            go (match parse_line line with Some e -> e :: acc | None -> acc)
        | exception End_of_file -> List.rev acc
      in
      go [])

(* One trajectory turn = a user line immediately answered by the assistant. *)
let turns entries =
  let rec go pending acc = function
    | [] -> List.rev acc
    | e :: rest ->
        if e.role = "user" then go (Some e) acc rest
        else if e.role = "assistant" then
          match pending with
          | Some u -> go None ((u, e) :: acc) rest
          | None -> go None acc rest
        else go pending acc rest
  in
  go None [] entries

let sessions entries =
  let seen = Hashtbl.create 8 in
  let order = ref [] in
  List.iter
    (fun e ->
      if not (Hashtbl.mem seen e.session) then begin
        Hashtbl.add seen e.session e.session;
        order := e.session :: !order
      end)
    entries;
  List.rev !order

(* The two roles a trajectory row may carry, spelled out so the wire shape of
   a training file is visible here rather than implied by e.role. *)
let line_of_entry e =
  let content = Yojson.Safe.to_string (`String e.content) in
  if e.role = "user" then
    Printf.sprintf "{\"role\":\"user\",\"content\":%s}" content
  else Printf.sprintf "{\"role\":\"assistant\",\"content\":%s}" content

let write_trajectory dir n pairs =
  let path = Filename.concat dir (Printf.sprintf "trajectory-%03d.jsonl" n) in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      List.iter
        (fun (u, a) ->
          (* user line first, its assistant answer second *)
          output_string oc (line_of_entry u);
          output_char oc '\n';
          output_string oc (line_of_entry a);
          output_char oc '\n')
        pairs)

let export ~journal ~out : int =
  if not (Sys.file_exists out) then Unix.mkdir out 0o755;
  let entries = read_journal journal in
  let written = ref 0 in
  List.iter
    (fun session ->
      let pairs =
        turns (List.filter (fun e -> e.session = session) entries)
      in
      if pairs <> [] then begin
        incr written;
        write_trajectory out !written pairs
      end)
    (sessions entries);
  !written

(* CLI body: report the count and always exit 0 — a journal with no
   user/assistant pair is a successful run that wrote nothing. *)
let export_cli ~journal ~out : int =
  let n = export ~journal ~out in
  Printf.printf "trajectories written: %d\n" n;
  flush stdout;
  0
