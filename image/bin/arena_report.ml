(** arena_report — the shadow-benchmark report, one command.

    SHADOW MODE: every arena card runs on N engines, each on its OWN
    worktree cut from an identical base commit; nothing merges to
    production, so the report is verdict-only. The scoreboard is a
    group-by of the ledger's arena rows (engine x card), never a model
    judgement; [--matched-pairs] narrows it to incumbent vs challenger on
    the same card — matched pairs are the unit of comparison.

    Input: arena run rows as JSON lines, from the file named after the
    flags or from stdin (when stdin is not a terminal). With no input the
    built-in fleet is reported, so a bare `arena_report <card>` is always
    one command and always exits 0. *)

open Pricklypear_image_lib

let read_all ic =
  let b = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string b (input_line ic);
       Buffer.add_char b '\n'
     done
   with End_of_file -> ());
  Buffer.contents b

let str fields k =
  match List.assoc_opt k fields with Some (`String s) -> s | _ -> ""

let num fields k =
  match List.assoc_opt k fields with
  | Some (`Int n) -> n
  | Some (`Float f) -> int_of_float f
  | _ -> 0

let run_of_json (j : Yojson.Safe.t) : Activity_ledger.arena_run option =
  match j with
  | `Assoc fields ->
      let card = str fields "card" in
      let engine = str fields "engine" in
      if card = "" || engine = "" then None
      else
        let gate = match str fields "gate" with "" -> "skipped" | g -> g in
        let outcome = match str fields "outcome" with "" -> "error" | o -> o in
        Some
          { Activity_ledger.ga_card = card;
            ga_engine = engine;
            ga_base_commit = str fields "base_commit";
            ga_branch = str fields "branch";
            ga_worktree = str fields "worktree";
            ga_gate = gate;
            ga_outcome = outcome;
            ga_tokens = num fields "tokens" }
  | _ -> None

let runs_of_text (s : string) : Activity_ledger.arena_run list =
  String.split_on_char '\n' s
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if line = "" then None
         else
           match Yojson.Safe.from_string line with
           | j -> run_of_json j
           | exception _ -> None)

let set_gate (r : Activity_ledger.arena_run) ~(gate : string)
    ~(outcome : string) : Activity_ledger.arena_run =
  let open Activity_ledger in
  { r with ga_gate = gate; ga_outcome = outcome }

(** The no-input fleet: one sealed card, two engines, identical base. The
    incumbent clears its gates; the challenger breaks one — a logged "ok"
    outcome over a failed gate is still not a pass. *)
let demo_runs () : Activity_ledger.arena_run list =
  match
    Activity_ledger.arena_dispatch ~card:"card-67076f"
      ~base_commit:"e5a3e56412f2"
      ~engines:[ "native"; "engine-2" ] ~tokens:7 ()
  with
  | Error _ -> []
  | Ok runs ->
      List.map
        (fun (r : Activity_ledger.arena_run) ->
          if r.ga_engine = "native" then
            set_gate r ~gate:"ok" ~outcome:"ok"
          else set_gate r ~gate:"fail" ~outcome:"ok")
        runs

let usage =
  "usage: arena_report [--matched-pairs] [--incumbent ENGINE] [<card>] [<runs.jsonl>]"

let () =
  let matched_pairs = ref false in
  let incumbent = ref "native" in
  let positional = ref [] in
  let argv = Array.to_list Sys.argv |> List.tl in
  let rec parse = function
    | [] -> ()
    | "--matched-pairs" :: rest ->
        matched_pairs := true;
        parse rest
    | "--incumbent" :: e :: rest ->
        incumbent := e;
        parse rest
    | "--help" :: _ | "-h" :: _ ->
        print_endline usage;
        exit 0
    | a :: rest ->
        positional := !positional @ [ a ];
        parse rest
  in
  parse argv;
  let card = match !positional with c :: _ -> Some c | [] -> None in
  let input_file =
    match !positional with
    | _ :: f :: _ when f <> "-" && Sys.file_exists f -> Some f
    | _ :: "-" :: _ -> None
    | _ -> None
  in
  let runs =
    match input_file with
    | Some f -> (
        match runs_of_text (In_channel.with_open_bin f read_all) with
        | [] -> demo_runs ()
        | rs -> rs)
    | None ->
        let from_stdin =
          try
            if Unix.isatty Unix.stdin then []
            else runs_of_text (read_all stdin)
          with _ -> []
        in
        (match from_stdin with [] -> demo_runs () | rs -> rs)
  in
  let scores = Activity_ledger.arena_leaderboard runs in
  let scores =
    match card with
    | Some c -> List.filter (fun s -> s.Activity_ledger.asc_card = c) scores
    | None -> scores
  in
  List.iter print_endline
    (Activity_ledger.arena_report_lines ~matched_pairs:!matched_pairs
       ~incumbent:!incumbent scores);
  if scores = [] then
    Printf.printf "arena: no runs for %s\n"
      (match card with Some c -> c | None -> "any card");
  exit 0
