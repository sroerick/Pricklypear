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
*)

open Pricklypear_lisp

let getenv name ~default =
  match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | Some _ | None -> default

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
      ]
  in
  Arg.parse speclist anon
    "pp-remote [--image URL] [--user U --pass P] [-e EXPR]\n\
     \                  [--lens DIR] [--lens-pull NAME] ... [--sync]";
  (* TLS (tls-eio) needs a seeded RNG before any HTTPS proxy call;
     mock transports in the tests never hit TLS, so this slipped.
     Mirage_crypto_rng_unix.use_default wires /dev/urandom + reseeding. *)
  Mirage_crypto_rng_unix.use_default ();
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
     reads resolve from the substrate (offline-first data path). *)
  (match lens_opt with
   | None -> ()
   | Some lens -> Remote.register_lens_reads t ~lens);

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
