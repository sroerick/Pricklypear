(** pp-remote — a Nopales client process (borge/remote-eval.borg client).

    Embeds the pure lisp interpreter, lazily materializes function
    definitions from a remote branch, and proxies Prim_call effects to
    the image over HTTP. Pure compute stays local; everything else
    crosses the wire to POST /api/prim. The store stays the single
    source of truth — quit the client, lose nothing but REPL state.

      export PP_URL=http://127.0.0.1:18080
      export PP_USER=admin PP_PASS=admin
      pp-remote '(+ 1 2)'          # one expr, print, exit
      pp-remote                    # stdin REPL

    Flags:
      --image URL    base URL of the image (default: PP_URL / 127.0.0.1:18080)
      --user NAME    basic-auth user (default: PP_USER / admin)
      --pass PASS    basic-auth pass (default: PP_PASS / admin)
      -e EXPR        eval one expr and exit (repeatable)
      -p EXPR        alias of -e (pipe mode)
      --model NAME   provider model (default: AGENT_MODEL / GLM-4.7-Flash)
      --base-url URL provider base URL (default: AGENT_BASE_URL / synthetic.new)
      --mock-script FILE  canned provider replies (test/demo of /chat)

    The LLM provider runs LOCALLY (borge/remote-eval.borg agent-loop):
    the API key is a value read from AGENT_API_KEY (same env the agent
    reads); only the data plane (DB prims inside tool bodies) crosses
    to the image. *)

open Pricklypear_lisp

let getenv name ~default =
  match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | Some _ | None -> default

(* Auto-load ./.env if present (KEY=VALUE or `export KEY=VALUE` lines),
   same as the agent binary — AGENT_API_KEY etc. land without sourcing.
   Failures are silent (file optional). *)
let load_dotenv () : unit =
  let path = ".env" in
  match open_in path with
  | exception _ -> ()
  | ic ->
    (try
       while true do
         let line = String.trim (input_line ic) in
         if line = "" || line.[0] = '#' then ()
         else begin
           let line =
             if String.length line > 7 && String.sub line 0 7 = "export "
             then String.trim (String.sub line 7 (String.length line - 7))
             else line
           in
           match String.index_opt line '=' with
           | Some i ->
               let k = String.trim (String.sub line 0 i) in
               let v_raw = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
               let v =
                 let n = String.length v_raw in
                 if n >= 2
                    && (v_raw.[0] = '"' && v_raw.[n - 1] = '"'
                        || v_raw.[0] = '\'' && v_raw.[n - 1] = '\'')
                 then String.sub v_raw 1 (n - 2)
                 else v_raw
               in
               Unix.putenv k v
           | None -> ()
         end
       done
     with End_of_file -> ());
    close_in ic

(* Mock provider: reads a JSON script of turns (same shape as the
   agent's --mock-script), consumed in order. Mirrors the real
   provider's streaming contract (fires the reply's text as an
   on_delta) so the loop path is identical, canned or live. *)
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
         {
           Provider.text;
           calls;
           stop_reason = "stop";
           raw = `Assoc [ ("role", `String "assistant"); ("content", `String text) ];
         })

let mock_complete ~(env : Eio_unix.Stdenv.base) ~sw ~cfg ~messages ~tools
    ~on_delta script_cell
    : (Provider.reply, string) Result.t =
  ignore env;
  ignore sw;
  ignore cfg;
  ignore tools;
  match !script_cell with
  | [] ->
      on_delta (Provider.Text "");
      on_delta Provider.Done;
      Ok { Provider.text = ""; calls = []; stop_reason = "end_turn"; raw = `Null }
  | r :: rest ->
      script_cell := rest;
      ignore messages;
      if r.Provider.text <> "" then on_delta (Provider.Text r.Provider.text);
      on_delta Provider.Done;
      Ok r

(* One-shot harness identity for the system prompt (pp-remote variant
   of the agent's harness_context): told up front so the model does not
   probe the OS or leak secrets discovering them. *)
let run_captured (cmd : string) : string =
  try
    let ic = Unix.open_process_in cmd in
    let out = String.trim (In_channel.input_all ic) in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> out
    | _ -> ""
  with _ -> ""

let harness_context ~(model : string) ~(base_url : string) ~(image : string)
    ~(user : string) : string =
  let host = try Unix.gethostname () with _ -> "?" in
  let uname = run_captured "uname -srm" in
  let cwd = Sys.getcwd () in
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let date =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min
  in
  Printf.sprintf
    "You are the pp-remote chat agent: the LLM turn runs LOCALLY on this \n\
     machine while Nopales evaluation runs against a remote Pricklypear \n\
     image over a proxy rim. The human operator talks to you through the \n\
     browser chat surface this process serves. You run as ONE unix process \n\
     directly on the operator's machine -- no container, no sandbox; your \n\
     commands affect the operator's real system.\n\
     \n\
     Harness facts (told to you here; do NOT probe the OS to discover them):\n\
     - host: %s (%s)\n\
     - cwd: %s -- the operator's working directory; treat as project root\n\
     - date: %s (operator local)\n\
     - model: %s via %s\n\
     - image: %s (basic-auth user %s) -- Nopales DB prims (get/create/\n\
       rows/tree/*...) proxy there per call; whoami/current-author are\n\
       server-asserted\n\
     - tools: read, bash, plus Nopales plugin tools registered with\n\
       (register-tool ...) -- a plugin tool's prim calls cross the proxy rim\n\
     \n\
     Conduct:\n\
     - Prefer the read and bash tools. Be terse.\n\
     - For questions about the project, read AGENTS.md/docs in cwd -- do not \n\
       probe the OS for facts listed above.\n\
     - Never print secret VALUES (env vars ending in _API_KEY, _TOKEN, \n\
       _SECRET); reference them by name. Avoid full `env` dumps."
    host (if uname = "" then "?" else uname) cwd date model base_url image user

(** ---- local-first lens (borge/local-first.borg Phase 1) ----

    [--lens DIR] wires [Remote.make ~lens_lookup] so function defs
    resolve from a filesystem JSON substrate BEFORE proxying
    [function-source] to the image — the offline-first def path. The
    lens root is a directory of node-envelope .json files (one file per
    node, at its tree path) produced by [Lens_store.materialize_*].

    [--lens-pull NAME] (repeatable) seeds the lens from the live image:
    each NAME is fetched via the [function-source] proxy (the resolved,
    closest-wins source) and written as a code node under the lens
    branch, so the substrate is populated and subsequent evals read
    from disk. This is the Phase-1 materialize step done simply —
    online pull, then offline eval. *)
let () =
  let image = ref None in
  let user = ref None in
  let pass = ref None in
  let exprs = ref [] in
  let lens_dir = ref None in
  let lens_owner = ref None in
  let lens_device = ref None in
  let lens_branch = ref None in
  let lens_pull = ref [] in
  let lens_pull_tree = ref [] in
  let lens_pull_data = ref [] in
  let sync_flag = ref false in
  let serve_flag = ref false in
  let open_browser = ref false in
  let model = ref None in
  let base_url = ref None in
  let mock_script = ref None in
  let anon _ = () in
  let speclist =
    Arg.align
      [
        ("--image", Arg.String (fun s -> image := Some s), "URL image base");
        ("--user", Arg.String (fun s -> user := Some s), "NAME basic-auth user");
        ("--pass", Arg.String (fun s -> pass := Some s), "PASS basic-auth pass");
        ("-e", Arg.String (fun s -> exprs := s :: !exprs), "EXPR eval and exit");
        ("-p", Arg.String (fun s -> exprs := s :: !exprs), "EXPR eval and exit");
        ("--lens", Arg.String (fun s -> lens_dir := Some s), "DIR local-first lens root (offline-first defs from JSON)");
        ("--lens-owner", Arg.String (fun s -> lens_owner := Some s), "OWNER lens owner (default: --user)");
        ("--lens-device", Arg.String (fun s -> lens_device := Some s), "DEVICE lens device tag (default: hostname)");
        ("--lens-branch", Arg.String (fun s -> lens_branch := Some s), "BRANCH lens branch id (default: lens-<device>)");
        ("--lens-pull", Arg.String (fun s -> lens_pull := s :: !lens_pull), "NAME fetch def from image into lens (repeatable)");
        ("--lens-pull-tree", Arg.String (fun s -> lens_pull_tree := s :: !lens_pull_tree), "NAME fetch def + transitive deps into lens (repeatable)");
        ("--lens-pull-data", Arg.String (fun s -> lens_pull_data := s :: !lens_pull_data), "PREFIX fetch data subtree into lens: 'user' | 'shared' | path (repeatable)");
        ("--sync", Arg.Unit (fun () -> sync_flag := true), "push local lens data up to the image (two-way, data-only v1) then pull server-only paths; records the new (branch, generation) snapshot");
        ("--serve", Arg.Set serve_flag, "serve the Nopales-authored HTML surface (chat + eval box + /code lens browser) on 127.0.0.1:random-port until Ctrl-C");
        ("--open", Arg.Set open_browser, "with --serve: best-effort browser launch");
        ("--model", Arg.String (fun s -> model := Some s), "NAME provider model (AGENT_MODEL)");
        ("--base-url", Arg.String (fun s -> base_url := Some s), "URL provider base URL (AGENT_BASE_URL)");
        ("--mock-script", Arg.String (fun s -> mock_script := Some s), "FILE canned provider replies (test/demo of /chat)");
      ]
  in
  Arg.parse speclist anon
    "pp-remote [--image URL] [--user U --pass P] [-e EXPR]\n\
     \                  [--lens DIR] [--lens-pull NAME] ... [--sync] [--serve [--open]]\n\
     \                  [--model NAME] [--base-url URL] [--mock-script FILE]";
  load_dotenv ();
  (* TLS (tls-eio) needs a seeded RNG before any HTTPS proxy call;
     mock transports in the tests never hit TLS, so this slipped.
     Mirage_crypto_rng_unix.use_default wires /dev/urandom + reseeding. *)
  Mirage_crypto_rng_unix.use_default ();
  (* Random.bits drives multipart boundaries (agent) + future stream ids;
     without self_init every process run repeats the same sequence. *)
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let cfg : Http_transport.cfg =
    {
      base_url =
        Option.value !image ~default:(getenv "PP_URL" ~default:"http://127.0.0.1:18080");
      user = Option.value !user ~default:(getenv "PP_USER" ~default:"admin");
      password = Option.value !pass ~default:(getenv "PP_PASS" ~default:"admin");
    }
  in
  let transport = Http_transport.transport ~env ~sw ~cfg in
  (* Hybrid placement: Local evaluates under the proxy rim; Remote
     (prim-dense / transactional) is POSTed to /api/eval wholesale. *)
  let remote_eval = Http_transport.eval_transport ~env ~sw ~cfg in
  (* ---- local-first lens wiring (Phase 1) ----
     If [--lens] is given, build a lens record and (optionally) pull
     defs from the image into the substrate, then pass [lens_lookup]
     to [Remote.make] so [fetch_def] consults the substrate first. *)
  let lens_opt =
    match !lens_dir with
    | None -> None
    | Some root ->
        let owner =
          Option.value !lens_owner ~default:cfg.user
        in
        let device =
          Option.value !lens_device
            ~default:(
              try
                let h = Unix.gethostname () in
                if h = "" then "laptop" else String.lowercase_ascii h
              with _ -> "laptop")
        in
        let branch =
          Option.value !lens_branch ~default:("lens-" ^ device)
        in
        if not (Sys.file_exists root) then begin
          let rec mkdir d =
            if d = "" || d = "." || d = Filename.dir_sep then ()
            else begin
              mkdir (Filename.dirname d);
              try Unix.mkdir d 0o755 with
              | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
            end
          in
          mkdir root;
          Printf.eprintf "[lens] created empty lens root %s\n%!" root
        end;
        Some
          { Lens_store.root; owner; device; branch;
            generation = 0L (* unused by resolve_def in read-only mode *) }
  in
  let () =
    (* [--lens-pull] / [--lens-pull-tree]: seed the lens from the live
       image. [pull_one] fetches a single def via the [function-source]
       proxy and writes it as a code node; returns true if the image had
       the def, false on a nil/empty miss (prim, special form, or genuinely
       unbound). [--lens-pull-tree] walks the transitive closure of each
       name's free variables, skipping nil misses silently (they are prims
       / special forms, not defs) and logging only successful pulls. *)
    let pull_one ~lens ~name =
      match transport "function-source" [ Ast.VStr name ] with
      | Ok (Ast.VStr src) when String.trim src <> "" ->
          let _ =
            Lens_store.materialize_code_node ~lens ~branch:lens.branch
              ~kind:"fn" ~name ~src ~doc:"" ~author:lens.owner
          in
          Some src
      | Ok _ -> None
      | Error e ->
          Printf.eprintf "[lens] pull %s: %s\n%!" name (Printexc.to_string e);
          None
    in
    let pull_tree ~lens ~root_name =
      (* record ancestry (self-first) so resolve_def walks the lens
         branch. If a chain was already recorded, leave it. *)
      (match Lens_store.ancestry ~lens with
       | [] -> Lens_store.record_ancestry ~lens [ lens.branch ]
       | _ -> ());
      let visited = Hashtbl.create 16 in
      let queue = Queue.create () in
      Queue.push root_name queue;
      let pulled = ref 0 in
      while not (Queue.is_empty queue) do
        let name = Queue.pop queue in
        if Hashtbl.mem visited name then ()
        else begin
          Hashtbl.replace visited name ();
          match pull_one ~lens ~name with
          | Some src ->
              incr pulled;
              Printf.eprintf "[lens] pulled %s\n%!" name;
              (* walk the def's free vars for transitive deps *)
              let deps =
                try
                  Free_vars.free_vars_of_source src
                with _ -> []
              in
              List.iter
                (fun d ->
                  if not (Hashtbl.mem visited d) then Queue.push d queue)
                deps
          | None -> ()
          (* a prim / special form / unbound — skip silently *)
        end
      done;
      Printf.eprintf "[lens] tree %s: %d def(s) pulled\n%!" root_name !pulled
    in
    (match lens_opt with
     | None -> ()
     | Some lens ->
         (* explicit pulls: log not-found (the user asked for these) *)
         if List.rev !lens_pull <> [] then
           (match Lens_store.ancestry ~lens with
            | [] -> Lens_store.record_ancestry ~lens [ lens.branch ]
            | _ -> ());
         List.iter
           (fun name ->
             match pull_one ~lens ~name with
             | Some _ -> Printf.eprintf "[lens] pulled %s\n%!" name
             | None -> Printf.eprintf "[lens] pull %s: not found on image\n%!" name)
           (List.rev !lens_pull);
         (* tree pulls: walk transitive closure silently skipping prims *)
         List.iter (fun n -> pull_tree ~lens ~root_name:n)
           (List.rev !lens_pull_tree);
         (* data pulls: materialize a datum subtree into the lens via
            [tree/subtree] (returns a VList of node envelopes, each the
            exact shape [Lens_store.materialize] writes). 'user' →
            data.u.<owner>, 'shared' → data.shared, else a raw path. *)
         List.iter
           (fun spec ->
             let prefix =
               match spec with
               | "user" -> "data.u." ^ Lens_store.enc_label lens.owner
               | "shared" -> "data.shared"
               | p -> p
             in
             match transport "tree/subtree" [ Ast.VStr prefix ] with
             | Ok (Ast.VList nodes) ->
                 let n = List.length nodes in
                 List.iter
                   (fun node ->
                     match node with
                     | Ast.VStr env ->
                         ignore (Lens_store.materialize ~lens env)
                     | _ -> ())
                   nodes;
                 Printf.eprintf "[lens] pulled data %s: %d node(s)\n%!"
                   prefix n
             | Ok _ ->
                 Printf.eprintf "[lens] pull-data %s: bad shape\n%!" prefix
             | Error e ->
                 Printf.eprintf "[lens] pull-data %s: %s\n%!" prefix
                   (Printexc.to_string e))
           (List.rev !lens_pull_data);
         (* pin the (branch, generation) snapshot — the merge base a
            later sync round's three-way diff keys on. Only when we
            actually pulled (code or data); a pure offline eval against
            an existing lens must not touch the wire. Catches transport
            exceptions (Eio raises, not Error) so an unreachable image
            degrades to an empty snapshot rather than crashing. *)
         if !lens_pull <> [] || !lens_pull_tree <> [] || !lens_pull_data <> [] then begin
           let branch =
             match (try transport "current-branch" [] with _ -> Error (Failure "offline")) with
             | Ok (Ast.VStr b) -> b | _ -> ""
           in
           let gen =
             match (try transport "gen-now" [] with _ -> Error (Failure "offline")) with
             | Ok (Ast.VInt g) -> g | _ -> 0L
           in
           Lens_store.record_snapshot ~lens ~branch ~generation:gen
         end)
  in
  let lens_lookup =
    match lens_opt with
    | None -> None
    | Some lens ->
        Some
          (fun name ->
            match Lens_store.resolve_def ~lens ~kind:"fn" ~name with
            | Some env -> Lens_store.code_src_of_envelope env
            | None -> None)
  in
  let t = Remote.make ~transport ~remote_eval ?lens_lookup () in
  (* Override [get] / [tree/get] with lens-local handlers so working-set
     reads resolve from the substrate (offline-first data path), and pin
     the lens record so lens/list + lens/read surface prims browse it. *)
  (match lens_opt with
   | None -> ()
   | Some lens ->
       Remote.register_lens_reads t ~lens;
       Remote.set_lens t lens);

  (* Boot-load the Nopales surface (libs/html + libs/hyper + the
     eval/code route-table handlers) so the HTML surface is
     Nopales-authored and redefinable live (borge/remote-eval.borg
     surfaces). The route + soup + pure prims are registered in
     [Remote.make]; [Surface.load] runs the embedded source under the
     proxy rim. *)
  Surface.load ~rt:t;

  (* Install the builtin agent tools (read/bash — llm/lib/tools.ml) into
     the client's tool registry so the chat loop has them. Path
     resolution is the client's sandbox (same refuse-".." posture as the
     agent). *)
  Tools.install ~resolve_path:Remote.resolve_path
    ~register:(fun name run -> Remote.register_builtin_tool t name ~run);

  (* ---- --serve: serve the Nopales-authored HTML surface (chat + eval
     box + /code lens browser) on 127.0.0.1:random until Ctrl-C. The
     chat turn runs the SHARED agent loop (llm/) over this client: the
     provider call is local (API key from AGENT_API_KEY / --mock-script),
     tool bodies eval under the proxy rim so DB prims cross to the
     image. Mutually exclusive with --sync / REPL / -e. *)
  (if !serve_flag then begin
    let model_name =
      Option.value !model ~default:(getenv "AGENT_MODEL" ~default:"hf:zai-org/GLM-4.7-Flash")
    in
    let provider_base =
      Option.value !base_url
        ~default:(getenv "AGENT_BASE_URL" ~default:"https://api.synthetic.new/openai/v1")
    in
    let pcfg : Provider.cfg =
      {
        base_url = provider_base;
        api_key = getenv "AGENT_API_KEY" ~default:"";
        model = model_name;
        system =
          getenv "AGENT_SYSTEM"
            ~default:(harness_context ~model:model_name ~base_url:provider_base
                        ~image:cfg.base_url ~user:cfg.user);
      }
    in
    (if pcfg.Provider.api_key = "" && !mock_script = None then
       Printf.eprintf
         "[chat] AGENT_API_KEY not set — /chat will fail provider calls; set \n\
          AGENT_API_KEY (or AGENT_BASE_URL/AGENT_MODEL) or pass --mock-script\n%!");
    let script_cell =
      ref (Option.map parse_mock_script !mock_script |> Option.value ~default:[])
    in
    let complete =
      match !mock_script with
      | Some _ ->
          (fun ~env ~sw ~cfg ~messages ~tools ~on_delta ->
             mock_complete ~env ~sw ~cfg ~messages ~tools ~on_delta script_cell)
      | None -> Provider.Openai.complete
    in
    Http_ui.serve ~env ~sw ~rt:t ~cfg:pcfg ~complete
      ~open_browser:!open_browser print_endline;
    exit 0
  end);

  (* ---- --sync: push local lens data up to the image (borge/local-first.borg
     sync, v1 two-way / data-only). Build the data manifest + bodies from
     the lens substrate, call sync/push, pull any server-only paths, and
     pin the new (branch, generation) snapshot as the next merge base. *)
  (if !sync_flag then begin
    match lens_opt with
    | None ->
        prerr_endline "--sync requires --lens DIR"; exit 2
    | Some lens ->
        let user_prefix = "data.u." ^ Lens_store.enc_label lens.owner in
        let plen = String.length user_prefix in
        let is_data p =
          String.length p >= plen && String.sub p 0 plen = user_prefix
        in
        let full = Lens_store.node_manifest ~lens in
        let data_manifest = List.filter (fun (p, _) -> is_data p) full in
        let manifest_v =
          Ast.VList
            (List.map
               (fun (p, h) -> Ast.VList [ Ast.VStr p; Ast.VStr h ])
               data_manifest)
        in
        let bodies_v =
          Ast.VList
            (List.filter_map
               (fun (p, _) ->
                 match Lens_store.get ~lens p with
                 | Some env -> Some (Ast.VList [ Ast.VStr p; Ast.VStr env ])
                 | None -> None)
               data_manifest)
        in
        Printf.eprintf "[sync] pushing %d data node(s) to %s\n%!"
          (List.length data_manifest) cfg.base_url;
        (match transport "sync/push" [ manifest_v; bodies_v ] with
         | Ok (Ast.VStr json) ->
             (match Yojson.Safe.from_string json with
              | `Assoc kv ->
                  let int k =
                    match List.assoc_opt k kv with
                    | Some (`Int n) -> n | _ -> 0
                  in
                  let str k =
                    match List.assoc_opt k kv with
                    | Some (`String s) -> s | _ -> ""
                  in
                  let strs k =
                    match List.assoc_opt k kv with
                    | Some (`List xs) ->
                        List.filter_map
                          (function `String s -> Some s | _ -> None) xs
                    | _ -> []
                  in
                  let applied = int "applied" in
                  let server_only = strs "server_only_paths" in
                  let diverge = strs "diverge_paths" in
                  let gen_str = str "generation" in
                  let gen =
                    try Int64.of_string gen_str with _ -> 0L
                  in
                  Printf.eprintf
                    "[sync] applied=%d  server_only=%d  diverge=%d  gen=%s\n%!"
                    applied (List.length server_only) (List.length diverge)
                    gen_str;
                  List.iter
                    (fun p -> prerr_endline ("[sync] DIVERGE: " ^ p))
                    diverge;
                  (* pull server-only paths so the lens catches up to the
                     server's side of the world in the same round *)
                  List.iter
                    (fun p ->
                      match transport "tree/get" [ Ast.VStr p ] with
                      | Ok (Ast.VStr env) ->
                          ignore (Lens_store.materialize ~lens env);
                          Printf.eprintf "[sync] pulled %s\n%!" p
                      | Ok _ -> ()
                      | Error e ->
                          Printf.eprintf "[sync] pull %s: %s\n%!"
                            p (Printexc.to_string e))
                    server_only;
                  (* pin the new (branch, generation) snapshot as the next
                     merge base — the coordinate a later sync keys on. *)
                  let branch =
                    match (try transport "current-branch" [] with _ -> Error (Failure "offline")) with
                    | Ok (Ast.VStr b) -> b | _ -> ""
                  in
                  Lens_store.record_snapshot ~lens ~branch ~generation:gen;
                  Printf.eprintf "[sync] done; snapshot pinned (branch=%s, gen=%s)\n%!"
                    branch gen_str
              | _ -> prerr_endline "[sync] bad response shape")
         | Ok _ -> prerr_endline "[sync] unexpected return value"
         | Error e ->
             Printf.eprintf "[sync] failed: %s\n%!" (Printexc.to_string e);
             exit 1)
  end);

  let eval_print src =
    try
      let v = Remote.eval t src in
      print_endline (Reader.pp_value v)
    with
    | Ast.Interp_error ie ->
        Printf.eprintf "error: %s\n%!" (Ast.pp_interp_error ie); exit 1
    | Reader.Reader_error (msg, ln, col) ->
        Printf.eprintf "read error %d:%d %s\n%!" ln col msg; exit 1
  in
  match List.rev !exprs with
  | [] ->
      (* stdin REPL: same env, same prims, same proxy rim. *)
      (match lens_opt with
       | None ->
           Printf.printf "pp-remote — %s  user=%s\n%!" cfg.base_url cfg.user
       | Some lens ->
           Printf.printf "pp-remote — %s  user=%s  [lens %s, branch %s]\n%!"
             cfg.base_url cfg.user lens.root lens.branch;
           if Lens_store.ancestry ~lens = [] then
             prerr_endline
               "[lens] no ancestry recorded — defs will proxy; use --lens-pull to seed");
      print_endline "enter Nopales forms; :quit to exit";
      let rec loop () =
        output_string stdout "remote> ";
        flush stdout;
        (try
           match input_line stdin |> String.trim with
           | line when line = "" -> loop ()
           | line when line = ":quit" || line = ":q" -> ()
           | line ->
               (try
                  let v = Remote.eval t line in
                  print_endline (Reader.pp_value v)
                with
                | Ast.Interp_error ie ->
                    Printf.printf "error: %s\n%!" (Ast.pp_interp_error ie)
                | Reader.Reader_error (msg, ln, col) ->
                    Printf.printf "read error %d:%d %s\n%!" ln col msg);
               loop ()
         with End_of_file -> print_newline ())
      in
      loop ()
  | xs -> List.iter eval_print xs
