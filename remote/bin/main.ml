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

(* Harness-specific prompt segments: identity, image/tools fact lines,
   conduct. The shared skeleton (facts block + assembly) lives in
   llm/lib/harness.ml (i-1788671587-9). NOTE: the trailing spaces inside
   these lines are intentional — byte-identical to the pre-refactor
   prompt. *)
let pre_facts =
  "You are the pp-remote chat agent: the LLM turn runs LOCALLY on this \n\
   machine while Nopales evaluation runs against a remote Pricklypear \n\
   image over a proxy rim. The human operator talks to you through the \n\
   browser chat surface this process serves. You run as ONE unix process \n\
   directly on the operator's machine -- no container, no sandbox; your \n\
   commands affect the operator's real system."

let fact_lines ~(image : string) ~(user : string) : string =
  Printf.sprintf
    "- image: %s (basic-auth user %s) -- Nopales DB prims (get/create/\n\
     rows/tree/*...) proxy there per call; whoami/current-author are\n\
     server-asserted\n\
     - tools: read, bash, plus Nopales plugin tools registered with\n\
     (register-tool ...) -- a plugin tool's prim calls cross the proxy rim\n"
    image user

let post_facts =
  "Conduct:\n\
   - Prefer the read and bash tools. Be terse.\n\
   - For questions about the project, read AGENTS.md/docs in cwd -- do not \n\
   probe the OS for facts listed above.\n\
   - Never print secret VALUES (env vars ending in _API_KEY, _TOKEN, \n\
   _SECRET); reference them by name. Avoid full `env` dumps."

(* One-shot harness identity for the system prompt (pp-remote variant
   of the agent's harness_context): told up front so the model does not
   probe the OS or leak secrets discovering them. *)
let harness_context ~(model : string) ~(base_url : string) ~(image : string)
    ~(user : string) : string =
  Harness.context ~pre_facts ~fact_lines:(fact_lines ~image ~user) ~post_facts
    ~model ~base_url

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
  let sync_loop_flag = ref false in
  let sync_interval = ref 30.0 in
  let serve_flag = ref false in
  let open_browser = ref false in
  let model = ref None in
  let base_url = ref None in
  let mock_script = ref None in
  let agent_context_flag = ref false in
  let board_list = ref None in
  let memory_put = ref None in
  let memory_content_file = ref None in
  let memory_tags = ref [] in
  let memory_scope = ref "workspace" in
  let memory_id = ref None in
  let memory_type = ref "agent-memory" in
  let memory_delete = ref None in
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
        ("--sync-loop", Arg.Unit (fun () -> sync_loop_flag := true), "run the sync round on a timer in the background (the invisible part; needs --lens)");
        ("--sync-interval", Arg.Float (fun f -> sync_interval := f), "SECONDS between background sync rounds (default 30)");
        ("--serve", Arg.Set serve_flag, "serve the Nopales-authored HTML surface (chat + eval box + /code lens browser) on 127.0.0.1:random-port until Ctrl-C");
        ("--open", Arg.Set open_browser, "with --serve: best-effort browser launch");
        ("--model", Arg.String (fun s -> model := Some s), "NAME provider model (AGENT_MODEL)");
        ("--base-url", Arg.String (fun s -> base_url := Some s), "URL provider base URL (AGENT_BASE_URL)");
        ("--mock-script", Arg.String (fun s -> mock_script := Some s), "FILE canned provider replies (test/demo of /chat)");
        ("--agent-context", Arg.Unit (fun () -> agent_context_flag := true), "print the issue-board digest from the local lens and exit (borge/agent.borg boot verb)");
        ("--board-list", Arg.String (fun s -> board_list := Some s), "UUID todo-list id for --agent-context (default: PP inbox)");
          ("--memory-put", Arg.String (fun s -> memory_put := Some s), "TITLE write an agent-memory node into the lens (offline; sync pushes it)");
          ("--memory-content-file", Arg.String (fun s -> memory_content_file := Some s), "FILE memory body content ('-' = stdin)");
          ("--memory-tags", Arg.String (fun s -> memory_tags := List.filter (fun t -> t <> "") (List.map String.trim (String.split_on_char ',' s))), "A,B,C comma-separated tags");
          ("--memory-scope", Arg.String (fun s -> memory_scope := s), "workspace|global (default workspace)");
          ("--memory-id", Arg.String (fun s -> memory_id := Some s), "MID stable memory id (default: fresh uuid; re-put updates in place)");
          ("--memory-type", Arg.String (fun s -> memory_type := s), "agent-memory|agent-agenda for --memory-delete (default agent-memory)");
          ("--memory-delete", Arg.String (fun s -> memory_delete := Some s), "ADDRESS node address (dashed or dashless) to delete; removes the lens node and enqueues a tombstone");
      ]
  in
    Arg.parse speclist anon
    "pp-remote [--image URL] [--user U --pass P] [-e EXPR]\n\
       \                  [--lens DIR] [--lens-pull NAME] ... [--sync] [--sync-loop] [--serve [--open]] [--agent-context] [--memory-put TITLE] [--memory-delete ADDR]\n\
     \                  [--model NAME] [--base-url URL] [--mock-script FILE]";
  Envcfg.load_dotenv ();
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
      (* PP_TOKEN: the delegated identity has no Basic password, so a
         configured token rides as Bearer. An explicit --pass or PP_PASS
         still wins, so local dev keeps admin:admin. *)
      let pass_given =
        match !pass with
        | Some _ -> true
        | None -> Envcfg.getenv "PP_PASS" ~default:"" <> ""
      in
      let token =
        let t = Envcfg.getenv "PP_TOKEN" ~default:"" in
        if (not pass_given) && String.trim t <> "" then Some (String.trim t)
        else None
      in
      {
        base_url =
          Option.value !image ~default:(Envcfg.getenv "PP_URL" ~default:"http://127.0.0.1:18080");
        user = Option.value !user ~default:(Envcfg.getenv "PP_USER" ~default:"admin");
        password = Option.value !pass ~default:(Envcfg.getenv "PP_PASS" ~default:"admin");
        token;
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
         let pulled_data = ref false in
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
                 if n > 0 then pulled_data := true;
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
         (* A batched data pull IS a last-synced state: the pulled
            envelopes carry server hashes verbatim, so record them as
            the first sync round's BASE manifest (__base.json). Without
            it the first round runs base-less and ships every envelope
            again — the 8.6MB POST nginx 413s on prod. *)
         if !pulled_data then
           Lens_store.record_base_manifest ~lens (Sync_client.data_manifest ~lens);
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

  (* ---- --agent-context: the borge/agent.borg boot verb (card 1ce0929f
     gap 2): print the issue-board digest from the local lens and exit.
     A pure lens read - no transport, works offline. The assigned-cards
     half of boot step (d) arrives when todo/for-user ships (card
     402516f4). ---- *)
  (if !agent_context_flag then begin
     match lens_opt with
     | None ->
         prerr_endline "--agent-context requires --lens DIR";
         exit 2
     | Some lens ->
         let list_id =
           Option.value !board_list ~default:Agent_context.default_board_list_id
         in
         (match
            Agent_context.digest ~root:lens.root ~list_id
              ~max_chars:Agent_context.default_max_chars
          with
          | Some digest ->
              print_endline digest;
              exit 0
          | None ->
              prerr_endline "agent-context: no board cards in the lens";
              exit 1)
   end);

  (* ---- --memory-put / --memory-delete: the borge/agent.borg stage-1
     memory verbs (card 1ce0929f, spec sboot (e)): memory writes go
     through the client into the one tree, offline-safe - unstamped lens
     nodes + the tombstone queue (the drain turns those into wire
     deletes). Pure lens operations, no transport calls. ---- *)
  (if !memory_put <> None && !memory_delete <> None then
     (prerr_endline "--memory-put and --memory-delete are mutually exclusive";
      exit 2)
   else ());
  (if !memory_put <> None || !memory_delete <> None then begin
     match lens_opt with
     | None ->
         prerr_endline "--memory-put/--memory-delete require --lens DIR";
         exit 2
     | Some lens ->
         let memory_run () =
           match (!memory_put, !memory_delete) with
           | Some title, _ ->
               let content =
                 match !memory_content_file with
                 | Some f -> Agent_memory.read_content f
                 | None ->
                     failwith
                       "--memory-put needs --memory-content-file FILE ('-' = stdin)"
               in
               let mid = Option.value !memory_id ~default:(Agent_memory.uuid4 ()) in
               let r =
                 Agent_memory.put ~lens ~mid ~title ~content ~tags:!memory_tags
                   ~scope:!memory_scope ~ws:(Sys.getcwd ())
                   ~src:(try Unix.gethostname () with _ -> "unknown")
               in
               Printf.printf "node %s %s\n" r.node_id
                 (if r.created then "created" else "updated");
               print_endline r.tree_path
           | None, Some addr ->
               if not (Agent_memory.is_memory_type !memory_type) then
                 failwith ("unknown --memory-type: " ^ !memory_type);
               let r =
                 Agent_memory.delete ~lens ~type_:!memory_type ~address:addr ()
               in
               (match r.removed_file with
                | Some f -> Printf.printf "removed %s\n" f
                | None -> print_endline "node not in lens; tombstone still enqueued");
               print_endline r.tombstone
           | None, None -> assert false
         in
         (match
            (try
               memory_run ();
               Ok ()
             with
             | Invalid_argument m -> Error m
             | Failure m -> Error m
             | Sys_error m -> Error m
             | e -> Error (Printexc.to_string e))
          with
          | Error m ->
              prerr_endline ("error: " ^ m);
              exit 1
          | Ok () -> exit 0)
   end);
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
  Tools.install ~env ~resolve_path:Remote.resolve_path
    ~register:(fun name run -> Remote.register_builtin_tool t name ~run);

  (* ---- --serve: serve the Nopales-authored HTML surface (chat + eval
     box + /code lens browser) on 127.0.0.1:random until Ctrl-C. The
     chat turn runs the SHARED agent loop (llm/) over this client: the
     provider call is local (API key from AGENT_API_KEY / --mock-script),
     tool bodies eval under the proxy rim so DB prims cross to the
     image. Mutually exclusive with --sync / REPL / -e. *)
  (if !serve_flag then begin
    let model_name =
      Option.value !model ~default:(Envcfg.getenv "AGENT_MODEL" ~default:"hf:zai-org/GLM-4.7-Flash")
    in
    let provider_base =
      Option.value !base_url
        ~default:(Envcfg.getenv "AGENT_BASE_URL" ~default:"https://api.synthetic.new/openai/v1")
    in
    let pcfg : Provider.cfg =
      {
        base_url = provider_base;
        api_key = Envcfg.getenv "AGENT_API_KEY" ~default:"";
        model = model_name;
        system =
          Envcfg.getenv "AGENT_SYSTEM"
            ~default:(harness_context ~model:model_name ~base_url:provider_base
                        ~image:cfg.base_url ~user:cfg.user);
      }
    in
    (if pcfg.Provider.api_key = "" && !mock_script = None then
       Printf.eprintf
         "[chat] AGENT_API_KEY not set — /chat will fail provider calls; set \n\
          AGENT_API_KEY (or AGENT_BASE_URL/AGENT_MODEL) or pass --mock-script\n%!");
    let script_cell =
      ref (Option.map Mock.parse_mock_script !mock_script |> Option.value ~default:[])
    in
    let complete =
      match !mock_script with
      | Some _ -> Mock.complete ~script_cell
      | None -> Provider.Openai.complete
    in
    Http_ui.serve ~env ~sw ~rt:t ~cfg:pcfg ~complete
      ~open_browser:!open_browser print_endline;
    exit 0
  end);

  (* ---- --sync: run ONE sync round via remote/lib/sync_client.ml
     (borge/local-first.borg sync): push local lens data up to the image,
     pull server copies, mirror server deletions, surface every LWW
     resolution, and pin the new (branch, generation) snapshot + merged
     manifest as the next round's base. *)
  (if !sync_flag then begin
    match lens_opt with
    | None ->
        prerr_endline "--sync requires --lens DIR"; exit 2
    | Some lens ->
        let r = Sync_client.run_round ~transport ~lens ~quiet:false () in
        if not r.Sync_client.ok then exit 1
  end);

  (* ---- --sync-loop: the BACKGROUND half of "sync invisibly in the
     background" (borge/local-first.borg sync — "'background' is client
     scheduling, not new machinery"). An Eio fiber runs the same round
     every --sync-interval seconds (quiet mode: activity and surfaced
     resolutions print, a clean round is silent); a failed round (image
     unreachable — offline work continues locally) retries on a short
     backoff so reconnect is automatic. Runs alongside --serve / the
     REPL. *)
  (if !sync_loop_flag then
     match lens_opt with
     | None ->
         prerr_endline "--sync-loop requires --lens DIR"; exit 2
     | Some lens ->
         let clock = Eio.Stdenv.clock env in
         Printf.eprintf "[sync-loop] every %.0fs against %s\n%!"
           !sync_interval cfg.base_url;
         Eio.Fiber.fork ~sw (fun () ->
             while true do
               let r =
                 try Sync_client.run_round ~transport ~lens ~quiet:true ()
                 with exn ->
                   Printf.eprintf "[sync-loop] round failed: %s\n%!"
                     (Printexc.to_string exn);
                   { (Sync_client.empty_report ()) with
                     Sync_client.ok = false;
                     error = Some (Printexc.to_string exn) }
               in
               let backoff =
                 if r.Sync_client.ok then !sync_interval
                 else Float.min !sync_interval 5.0
               in
               Eio.Time.sleep clock backoff
           done));

  let print_local_notices () =
    (* bumpers M2 (borge/bumpers.borg notices-envelope): lens local
       define notices, drained after the value. Out of band on stderr
       with the same ";; notice: " prefix as the image pp_eval, so the
       value stream on stdout stays clean. *)
    List.iter
      (fun n -> prerr_endline (";; notice: " ^ Wire_codec.notice_to_line n))
      (Remote.take_local_notices t)
  in
  let eval_print src =
    try
      let v = Remote.eval t src in
      print_endline (Reader.pp_value v);
      print_local_notices ()
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
                  print_endline (Reader.pp_value v);
                  print_local_notices ()
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
