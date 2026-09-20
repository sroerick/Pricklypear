(* remote/lib/agent_context.ml — the [agent-context] boot verb
   (borge/agent.borg run protocol; card 1ce0929f). A run boots, reads its
   assigned cards + shared memory, works, writes back; boot step (d)
   prints the board digest. This is the CLIENT-SIDE port of the Autolith
   pp-board-context contributor (a digest of the Inbox board read from the
   local lens), so the wiring leaves the agent harness's private image and
   every box that installs the client gets it.

   PURE + OFFLINE: reads the lens substrate (JSON node envelopes) only; no
   transport, no network. The board is the todo-list whose todo-item cards
   carry [list_id = the pinned board list]; columns come from that
   todo-list node itself (addressed by its dash-stripped id), so a second
   todo list in the one tree cannot hijack the digest. DONE and PARKED
   render as counts only; the digest is bounded (default 1900 chars) so it
   fits a bounded context-evidence slot. *)

let default_board_list_id = "0b21ffaa-3a67-4bed-a9ac-658c855b4704"

let default_max_chars = 1900

let abbrev (s : string) ~(max : int) : string =
  if String.length s <= max then s else String.sub s 0 (max - 1) ^ "..."

let id8 (id : string) : string = String.sub id 0 (min 8 (String.length id))

let read_file (file : string) : string =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

type card = { state : string; title : string; due : string; id : string }

(** One digest over the lens under [root]. Returns None when the lens
    holds no cards for [list_id] (nothing to report). *)
let digest ~(root : string) ~(list_id : string) ~(max_chars : int) :
    string option =
  let dashless = String.concat "" (String.split_on_char '-' list_id) in
  let cards = ref [] in
  let columns = ref [] in
  let list_title = ref "" in
  let str = function Some (`String s) -> s | _ -> "" in
  let handle file =
    match (try Yojson.Safe.from_string (read_file file) with _ -> `Null) with
    | `Assoc kv -> (
        let field name = List.assoc_opt name kv in
        match field "type" with
        | Some (`String "todo-item") -> (
            match field "body" with
            | Some (`Assoc b) ->
                let get name = str (List.assoc_opt name b) in
                if get "list_id" = list_id then
                  cards :=
                    {
                      state = get "state";
                      title = get "title";
                      due = get "due";
                      id = str (field "id");
                    }
                    :: !cards
            | _ -> ())
        | Some (`String "todo-list") ->
            if Filename.remove_extension (Filename.basename file) = dashless
            then (
              match field "body" with
              | Some (`Assoc b) ->
                  (match List.assoc_opt "columns" b with
                   | Some (`List cols) ->
                       columns :=
                         List.filter_map
                           (function `String c -> Some c | _ -> None)
                           cols
                   | _ -> ());
                  list_title := str (List.assoc_opt "title" b)
              | _ -> ())
        | _ -> ())
    | _ -> ()
  in
  let rec walk dir =
    (match Sys.readdir dir with
     | exception Sys_error _ -> ()
     | entries ->
         Array.sort compare entries;
         Array.iter
           (fun e ->
             let p = Filename.concat dir e in
             if Sys.is_directory p then walk p
            else if Filename.check_suffix e ".json" then (try handle p with _ -> ()))
           entries);
    ()
  in
  walk root;
  match !cards with
  | [] -> None
  | cards ->
        (* cards accumulate in reverse walk order (push); List.rev restores
           sorted walk order, so the per-column listing is deterministic.
           The incumbent Autolith digest's order was a push-order artifact,
           not a contract. *)
        let cards = List.rev cards in
      let default_order =
        [
          "SPEC"; "CODE"; "LOGIC-REVIEW"; "UI-REVIEW"; "HUMAN-REVIEW";
          "DONE"; "PARKED";
        ]
      in
      let order = if !columns = [] then default_order else !columns in
      let is_quiet col = col = "DONE" || col = "PARKED" in
      let open_count =
        List.length (List.filter (fun c -> not (is_quiet c.state)) cards)
      in
      let buf = Buffer.create 1024 in
      let title_part =
        if !list_title = "" then "" else Printf.sprintf " '%s'" !list_title
      in
      Printf.bprintf buf
        "Pricklypear board%s from the local lens: %d card(s), %d open.\n"
        title_part (List.length cards) open_count;
      List.iter
        (fun col ->
          let in_col = List.filter (fun c -> c.state = col) cards in
          match in_col with
          | [] -> ()
          | _ ->
              if is_quiet col then
                Printf.bprintf buf "%s (%d)\n" col (List.length in_col)
              else
                Printf.bprintf buf "%s (%d): %s\n" col (List.length in_col)
                  (String.concat "; "
                     (List.map
                        (fun c ->
                          if c.due = "" then
                            Printf.sprintf "%s [%s]" (abbrev c.title ~max:70)
                              (id8 c.id)
                          else
                            Printf.sprintf "%s due %s [%s]"
                              (abbrev c.title ~max:70) c.due (id8 c.id))
                        in_col)))
        order;
      let s = Buffer.contents buf in
      if String.length s <= max_chars then Some s
      else
        (* bound at the last complete line under the cap *)
        let cut = ref 0 in
        String.iteri
          (fun i ch -> if ch = '\n' && i < max_chars then cut := i + 1)
          s;
        Some (String.sub s 0 !cut ^ "...")
