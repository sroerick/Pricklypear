(** glochid — local-first Nopales-native agent harness (borge/agent.borg).

    v0.0 surface: stdin REPL. `agent` boots, loads ~/.config/agent/plugins/*.pp
    then ./.agent/plugins/*.pp, and enters a read/eval/print loop. Lines
    starting with `/` dispatch as plugin commands; lines starting with
    `(` are evaluated as Nopales; anything else is a prompt for the
    model.

    Flags:
      --mock-script FILE   canned provider replies (test/demo)
      --model NAME
      --base-url URL
      --once               run ONE full agent turn from piped stdin and exit
      -p EXPR              eval one Nopales expr and exit (pipe mode)

    Piped-turn mode: with either --once or non-tty stdin, the whole of
    stdin is read as the prompt (one string), a single full agent turn
    (model call -> tool calls -> final reply) runs, the final assistant
    text is printed, and we exit 0. This is the surfaces smoke #2
    trigger: `echo "say hello" | agent --mock-script m.json`.
*)

open Pricklypear_lisp

(* Identity + harness-specific fact lines / conduct for the system
   prompt. The shared skeleton (facts block + assembly) lives in
   llm/lib/harness.ml; these segments are this binary's own narrative
   (i-1788671587-9). *)
let pre_facts =
  "You are pp-agent, the local Nopales-native coding agent of the \
   Pricklypear project. The human operator talks to you through this \
   binary's browser chat surface or line REPL. You run as ONE unix \
   process directly on the operator's machine -- no container, no \
   sandbox; your commands affect the operator's real system."

let fact_lines =
  "- tools: read, bash, plus any Nopales plugin tools loaded from \
   ~/.config/agent/plugins/*.pp and ./.agent/plugins/*.pp\n\
   - /code on this binary's web surface is a read-only file browser over cwd\n"

let post_facts =
  "Conduct:\n\
   - Prefer the read and bash tools. Be terse.\n\
   - For questions about the project, read AGENTS.md/docs in cwd -- do not \
   probe the OS for facts listed above.\n\
   - Never print secret VALUES (env vars ending in _API_KEY, _TOKEN, \
   _SECRET); reference them by name. Avoid full `env` dumps."

let harness_context ~(model : string) ~(base_url : string) : string =
  Harness.context ~pre_facts ~fact_lines ~post_facts ~model ~base_url

(* Streaming-aware print: accumulate text deltas into the current
   assistant line so token fragments don't each get their own header.
   [tool] traces still get their own dim lines. *)
let pending_assistant = ref false

let print_event = function
  | Loop.Reasoning_text s ->
      if s <> "" then begin
        if !pending_assistant then (Printf.printf "\n%!"; pending_assistant := false);
        Printf.printf "\027[2;3m%s\027[0m%!" s
      end
  | Loop.Assistant_text s ->
      if s <> "" then begin
        if not !pending_assistant then begin
          Printf.printf "\n\027[1massistant>\027[0m %!";
          pending_assistant := true
        end;
        Printf.printf "%s%!" s
      end
  | Loop.Tool_begin (name, input) ->
      if !pending_assistant then (Printf.printf "\n%!"; pending_assistant := false);
      Printf.printf "\027[2m[tool %s %s]\027[0m\n%!" name input
  | Loop.Tool_end (name, out) ->
      let preview =
        if String.length out > 300 then String.sub out 0 300 ^ "…" else out
      in
      Printf.printf "\027[2m[tool %s -> %s]\027[0m\n%!" name preview
  | Loop.Turn_error msg ->
      Printf.printf "\027[31m[turn error] %s\027[0m\n%!" msg

(* One-shot harness identity for the system prompt: the shared facts
   block + assembly (llm/lib/harness.ml) with this binary's own narrative
   segments. *)
let () =
  Envcfg.load_dotenv ();
  let mock_script = ref None in
  let model = ref None in
  let base_url = ref None in
  let lens_dir = ref None in
  let pipe_expr = ref None in
  let once = ref false in
  let serve = ref false in
  let open_browser = ref false in
  let port = ref 18090 in
  let anon (_ : string) = () in
  let speclist =
    Arg.align
      [
        ("--mock-script", Arg.String (fun s -> mock_script := Some s), "FILE canned provider replies");
        ("--model", Arg.String (fun s -> model := Some s), "NAME");
        ("--base-url", Arg.String (fun s -> base_url := Some s), "URL");
        ("--lens",
         Arg.String (fun s -> lens_dir := Some s),
         "DIR local-first lens root; its __config.json (sync-provided llm key+base) feeds provider config");
        ("--once", Arg.Set once, "run one full agent turn from piped stdin and exit");
        ("--serve", Arg.Set serve, "serve the HTML chat surface on 127.0.0.1 until Ctrl-C (default port 18090)");
        ("--port", Arg.Int (fun p -> port := p), "PORT fixed port for --serve (default 18090; 0 = random)");
        ("--open", Arg.Set open_browser, "with --serve: best-effort browser launch");
        ("-p", Arg.String (fun s -> pipe_expr := Some s), "EXPR eval and exit");
      ]
  in
  Arg.parse speclist anon "agent [--mock-script F] [--provider N] [--once] [--serve [--port P] [--open]] [-p EXPR]";
  (* Pipe mode is entered either explicitly (--once) or implicitly when
     stdin is not a tty (surfaces smoke #2: `echo ... | agent ...`). *)
  let pipe_mode = !once || not (Unix.isatty Unix.stdin) in
  (* TLS (tls-eio) needs a seeded RNG before any HTTPS provider call;
     mock-only runs never hit TLS, so the smokes didn't surface this.
     Mirage_crypto_rng_unix.use_default wires /dev/urandom + reseeding. *)
  Mirage_crypto_rng_unix.use_default ();
  (* Random.bits drives /chat stream ids + multipart boundaries; without
     self_init every process run repeats the same id/boundary sequence. *)
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let rt = Runtime.make () in
  Tools.install ~env ~resolve_path:Runtime.resolve_path
    ~register:(fun name run -> Runtime.register_builtin_tool rt name ~run);
  (* Boot-load the Nopales surface (libs/html + libs/hyper + the chat/code
     route-table handlers) so the HTML surface is Nopales-authored and
     redefinable live (borge/agent.borg surfaces). *)
  Surface.load ~rt;
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
      (* borge/agent.borg llm-config — layered provider config.
         Per field, first non-empty wins:
           --flag > env (the AGENT_ vars) > <lens>/__config.json
             (sync-provided) > ~/.config/agent/config.json (the
             /settings surface's store).
         The lens file is how the habitat's llm credential arrives
         INVISIBLY through the sync exchange (auth.user-llm-keys 5b) —
         no bespoke export, no tree datum. *)
      let file_cfg = Config.load () in
      let lens_cfg =
        match !lens_dir with
        | Some d -> Config.load_lens d
        | None -> Config.empty
      in
      let merged = Config.merge ~primary:lens_cfg ~fallback:file_cfg in
      let pick ~flag ~env ~fallback ~default =
        match flag with
        | Some v -> v
        | None ->
            if env <> "" then env
            else (match fallback with Some v -> v | None -> default)
      in
      let cfg_model =
        pick ~flag:!model ~env:(Envcfg.getenv "AGENT_MODEL" ~default:"")
          ~fallback:merged.model ~default:"hf:zai-org/GLM-4.7-Flash"
      in
      let cfg_base =
        pick ~flag:!base_url ~env:(Envcfg.getenv "AGENT_BASE_URL" ~default:"")
          ~fallback:merged.base_url
          ~default:"https://api.synthetic.new/openai/v1"
      in
      let cfg_key =
        pick ~flag:None ~env:(Envcfg.getenv "AGENT_API_KEY" ~default:"")
          ~fallback:merged.api_key ~default:""
      in
      let cfg : Provider.cfg =
        {
          base_url = cfg_base;
          api_key = cfg_key;
          model = cfg_model;
          system =
            Envcfg.getenv "AGENT_SYSTEM"
              ~default:(harness_context ~model:cfg_model ~base_url:cfg_base);
        }
      in
      let script_cell =
        ref (Option.map Mock.parse_mock_script !mock_script |> Option.value ~default:[])
      in
      let complete =
        match !mock_script with
        | Some _ -> Mock.complete ~script_cell
        | None -> Provider.Openai.complete
      in
      let history : Provider.message list ref = ref [] in
      (* Run one full agent turn (model call -> tool calls -> final
         reply) for the given prompt, streaming events via print_event.
         Returns Ok () / Error msg. Exit status is derived by callers. *)
      let run_turn prompt =
        Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete
          ~on_event:print_event history prompt
      in
      if !serve then
        (* HTML primary surface: serve until Ctrl-C. Blocks forever in
           the accept loop. wrap_serve passes the sink-only [print]
           used for URL output. --open is best-effort browser launch. *)
        Http_ui.serve ~env ~sw ~rt ~cfg ~complete ~port:!port
          ~open_browser:!open_browser print_endline
      else if pipe_mode then (
        (* Piped-turn smoke: read ALL of stdin as the prompt, run ONE
           full turn, print the final assistant text, exit 0 on Ok.
           Multi-line input is joined with newlines (trimmed). *)
        let buf = Buffer.create 256 in
        (try
           while true do Buffer.add_string buf (input_line stdin); Buffer.add_char buf '\n' done
         with End_of_file -> ());
        let prompt = String.trim (Buffer.contents buf) in
        match run_turn prompt with
        | Ok () -> () (* final assistant text already printed via print_event *)
        | Error e ->
            Printf.eprintf "agent turn error: %s\n%!" e;
            exit 1)
      else (
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
                 Loop.run ~env ~sw ~host:(Runtime.loop_host rt) ~cfg ~complete
                 ~on_event:print_event history line
               with
               | Ok () -> ()
               | Error e -> Printf.printf "\027[31m[loop error] %s\027[0m\n%!" e);
              loop ())
      in
      loop () )
