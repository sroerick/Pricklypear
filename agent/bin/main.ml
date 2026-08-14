(** agent — local Nopales-native coding agent (borge/agent.borg).

    v0.0 surface: stdin REPL. `agent` boots, loads ~/.config/agent/plugins/*.pp
    then ./.agent/plugins/*.pp, and enters a read/eval/print loop. Lines
    starting with `/` dispatch as plugin commands; lines starting with
    `(` are evaluated as Nopales; anything else is a prompt for the
    model.

    Flags:
      --mock-script FILE   canned provider replies (test/demo)
      --model NAME
      --base-url URL
      -p EXPR              eval one Nopales expr and exit (pipe mode)
*)

open Pricklypear_lisp

let getenv name ~default =
  match Sys.getenv_opt name with
  | Some s -> if s = "" then default else s
  | None -> default

(* Mock provider: reads a JSON script of turns. Each entry:
   {"text": "...", "calls": [{"id","name","input"}]} — entries are
   consumed in order; running out ends the turn loop. *)
let mock_complete ~(env : Eio_unix.Stdenv.base) ~sw ~cfg ~messages
    ~tools ~on_delta script_cell
    : (Provider.reply, string) Result.t =
  ignore env;
  ignore sw;
  ignore cfg;
  ignore tools;
  ignore on_delta;
  match !script_cell with
  | [] -> Ok { Provider.text = ""; calls = []; stop_reason = "end_turn"; raw = `Null }
  | r :: rest ->
      script_cell := rest;
      ignore messages;
      Ok r

let parse_mock_script script_path : Provider.reply list =
  let open Yojson.Safe.Util in
  let j = Yojson.Safe.from_file script_path in
  j |> to_list
  |> List.map (fun t ->
         let text = t |> member "text" |> to_string_option |> Option.value ~default:"" in
         let calls =
           match t |> member "calls" with
           | `List cs ->
               List.map
                 (fun c ->
                   {
                     Provider.id = c |> member "id" |> to_string;
                     name = c |> member "name" |> to_string;
                     input = c |> member "input";
                   })
                 cs
           | _ -> []
         in
         { Provider.text; calls; stop_reason = "stop"; raw = `Assoc [ ("role", `String "assistant"); ("content", `String text) ] })

let print_event = function
  | Loop.Assistant_text s ->
      if s <> "" then Printf.printf "\n\027[1massistant>\027[0m %s\n%!" s
  | Loop.Tool_begin (name, input) ->
      Printf.printf "\027[2m[tool %s %s]\027[0m\n%!" name input
  | Loop.Tool_end (name, out) ->
      let preview =
        if String.length out > 300 then String.sub out 0 300 ^ "…" else out
      in
      Printf.printf "\027[2m[tool %s -> %s]\027[0m\n%!" name preview

let () =
  let mock_script = ref None in
  let model = ref None in
  let base_url = ref None in
  let pipe_expr = ref None in
  let anon (_ : string) = () in
  let speclist =
    Arg.align
      [
        ("--mock-script", Arg.String (fun s -> mock_script := Some s), "FILE canned provider replies");
        ("--model", Arg.String (fun s -> model := Some s), "NAME");
        ("--base-url", Arg.String (fun s -> base_url := Some s), "URL");
        ("-p", Arg.String (fun s -> pipe_expr := Some s), "EXPR eval and exit");
      ]
  in
  Arg.parse speclist anon "agent [--mock-script F] [--provider N] [-p EXPR]";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let rt = Runtime.make () in
  Tools.install ~rt;
  Plugins.load_all rt;
  match !pipe_expr with
  | Some src ->
      (try
         let v = Runtime.eval_string ~rt src in
         print_endline (Reader.pp_value v)
       with Ast.Interp_error ie ->
         Printf.eprintf "error: %s\n%!" (Ast.pp_interp_error ie);
         exit 1)
  | None ->
      let cfg : Provider.cfg =
        {
          base_url =
            Option.value !base_url
              ~default:(getenv "AGENT_BASE_URL" ~default:"https://api.synthetic.new/openai/v1");
          api_key = getenv "AGENT_API_KEY" ~default:"";
          model = Option.value !model ~default:(getenv "AGENT_MODEL" ~default:"hf:zai-org/GLM-4.7-Flash");
          system =
            "You are a coding agent in the user's working directory. Prefer the read and bash tools. Be terse.";
        }
      in
      let script_cell = ref (Option.map parse_mock_script !mock_script |> Option.value ~default:[]) in
      let complete =
        match !mock_script with
        | Some _ -> (fun ~env ~sw ~cfg ~messages ~tools ~on_delta ->
            mock_complete ~env ~sw ~cfg ~messages ~tools ~on_delta script_cell)
        | None -> Provider.Openai.complete
      in
      let history : Provider.message list ref = ref [] in
      print_endline "agent v0.0 — Nopales plugins. (expr) evaluates; /cmd dispatches; other lines prompt the model.";
      let rec loop () =
        print_string "\027[1magent>\027[0m ";
        flush stdout;
        match input_line stdin with
        | exception End_of_file -> print_endline "bye"
        | line ->
            let line = String.trim line in
            if line = "" then loop ()
            else if String.length line > 0 && line.[0] = '/' then (
              let sp = try String.index line ' ' with Not_found -> String.length line in
              let name = String.sub line 1 (sp - 1) in
              let rest =
                if sp < String.length line then
                  String.trim (String.sub line sp (String.length line - sp))
                else ""
              in
              (match Hashtbl.find_opt rt.Runtime.commands name with
               | Some entry ->
                   (try
                      let v = Runtime.apply_value rt entry.run [ Ast.VStr rest ] in
                      if v <> Ast.VNil then print_endline (Reader.pp_value v)
                    with Ast.Interp_error ie ->
                      Printf.eprintf "cmd error: %s\n%!" (Ast.pp_interp_error ie))
               | None ->
                   Printf.printf "commands: %s\n%!"
                     (match Runtime.eval_string ~rt "(list-commands)" with
                      | Ast.VStr s -> s
                      | _ -> "?"));
              loop ())
            else if line.[0] = '(' || String.length line > 0 && line.[0] = '\'' then (
              (try
                 let v = Runtime.eval_string ~rt line in
                 print_endline (Reader.pp_value v)
               with
               | Ast.Interp_error ie ->
                   Printf.printf "error: %s\n%!" (Ast.pp_interp_error ie)
               | Reader.Reader_error (msg, ln, col) ->
                   Printf.printf "read error %d:%d %s\n%!" ln col msg);
              loop ())
            else (
              (match
                 Loop.run ~env ~sw ~rt ~cfg ~complete ~on_event:print_event
                   history line
               with
               | Ok () -> ()
               | Error e -> Printf.printf "\027[31m[loop error] %s\027[0m\n%!" e);
              loop ())
      in
      loop ()
