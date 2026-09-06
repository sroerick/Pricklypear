(* Mock provider for the standalone agent binaries' --mock-script flag:
   canned [Provider.reply] turns consumed in order. Verbatim shared home
   of the copies that lived in agent/bin/main.ml and remote/bin/main.ml
   (i-1788671587-9). *)

(* Mock provider: reads a JSON script of turns. Each entry:
   {"text": "...", "calls": [{"id","name","input"}]} — entries are
   consumed in order; running out ends the turn loop. *)
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

(* The [Loop.run] [complete] implementation over a canned script cell:
   pops the next reply (mirroring the real provider's streaming
   contract — the reply's text fires as an [on_delta] so the
   loop/print_event path is identical, canned or live), and returns an
   empty end_turn reply when the script runs out. *)
let complete ~(script_cell : Provider.reply list ref)
    ~(env : Eio_unix.Stdenv.base) ~sw ~(cfg : Provider.cfg)
    ~(messages : Provider.message list) ~(tools : Provider.tool_def list)
    ~(on_delta : Provider.delta -> unit)
    : (Provider.reply, string) Result.t =
  ignore env;
  ignore sw;
  ignore cfg;
  ignore tools;
  ignore messages;
  match !script_cell with
  | [] ->
      on_delta (Provider.Text "");
      on_delta Provider.Done;
      Ok { Provider.text = ""; calls = []; stop_reason = "end_turn"; raw = `Null }
  | r :: rest ->
      script_cell := rest;
      let r : Provider.reply = r in
      if r.Provider.text <> "" then on_delta (Provider.Text r.Provider.text);
      on_delta Provider.Done;
      Ok r
