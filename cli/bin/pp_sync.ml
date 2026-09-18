(** pp-sync — sync libs/*.pp with live function_defs (OCaml, no Python).

      pp-sync status [pkg]
      pp-sync pull <pkg>
      pp-sync push <pkg>
      pp-sync pull-all
      pp-sync reseed <pkg>          admin: upsert libs → system/main
      pp-sync snapshot [--branch R] [--out DIR]
      pp-sync land <pkg> [name ...] [--from owner/branch]
*)

open Cmdliner

let repo_root () =
  (* Prefer cwd if it looks like the repo; else dirname of executable. *)
  let cwd = Sys.getcwd () in
  if Sys.file_exists (Filename.concat cwd "libs") then cwd
  else
    let exe = Sys.executable_name in
    let dir = Filename.dirname exe in
    (* dune install: …/bin → …; dune exec: _build/default/cli/bin *)
    let candidates =
      [
        Filename.concat dir "../../..";
        Filename.concat dir "../../../..";
        cwd;
      ]
    in
    match
      List.find_opt
        (fun p -> Sys.file_exists (Filename.concat p "libs"))
        candidates
    with
    | Some p -> p
    | None -> cwd

let libs_dir () = Filename.concat (repo_root ()) "libs"

let pkg_path pkg =
  if pkg = "" || pkg = "(root)" || pkg = "root" then
    Filename.concat (Filename.concat (libs_dir ()) "_root") "lib.pp"
  else Filename.concat (Filename.concat (libs_dir ()) pkg) "lib.pp"

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let rec mkdir_p path =
  if path = "" || path = "." || path = "/" then ()
  else if Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path text =
  let dir = Filename.dirname path in
  mkdir_p dir;
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc text;
      if text = "" || text.[String.length text - 1] <> '\n' then
        output_char oc '\n')

let sanitize_ref ref_ =
  let buf = Buffer.create (String.length ref_) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' ->
          Buffer.add_char buf c
      | '/' | ' ' -> Buffer.add_char buf '-'
      | _ -> Buffer.add_char buf '_')
    ref_;
  let s = Buffer.contents buf in
  if s = "" then "branch" else s

(* Local define names, collected by the real reader (i-1788670358-d).

   The previous hand-rolled regex \(\s*define\s+([^\s()]+) could not
   match function-style (define (f x) ...) — the ( after define is
   excluded by [^\s()]+ — so vault diffs lied about packages that use
   define sugar (libs/rb: 124 of 141 defines invisible, reported as
   only-live). It also mis-fired inside string literals and quoted
   data, since its hand-rolled comment stripper kept string bodies.

   Reader.read_all handles comments, strings and both define styles;
   Eval.parse_top_define is the kernel's one definition of what counts
   as a top-level define, so this list cannot drift from what eval
   accepts. Top-level only: nested defines are local bindings, not
   package bindings (matching what the live package-bindings reports).
   A file that does not parse is loud on stderr and yields [] — an
   empty-with-warning diff beats a confidently wrong one. *)
let parse_local_defines text =
  match
    (try
       Ok
         (List.filter_map
            (fun e ->
              match Pricklypear_lisp.Eval.parse_top_define e with
              | Some { name; _ } -> Some name
              | None -> None)
            (Pricklypear_lisp.Reader.read_all text))
     with
    | Pricklypear_lisp.Reader.Reader_error (msg, line, col) ->
        Error (Printf.sprintf "line %d col %d: %s" line col msg)
    | Pricklypear_lisp.Ast.Interp_error e ->
        Error (Pricklypear_lisp.Ast.pp_interp_error e)
    | exn -> Error (Printexc.to_string exn))
  with
  | Ok names -> List.sort_uniq String.compare names
  | Error err ->
      Printf.eprintf
        "pp-sync: local file does not parse (%s); local define list \"only-live\" \
         counts unreliable\n%!"
        err;
      []

let unwrap_list_of_strings (printed : string) : string list =
  let re = Re.Perl.compile_pat {|"([^"]+)"|} in
  let quoted =
    Re.all re printed
    |> List.filter_map (fun g ->
           try Some (Re.Group.get g 1) with Not_found -> None)
  in
  if quoted <> [] then quoted
  else
    let inner = String.trim printed in
    let inner =
      if
        String.length inner >= 2
        && inner.[0] = '('
        && inner.[String.length inner - 1] = ')'
      then String.sub inner 1 (String.length inner - 2) |> String.trim
      else inner
    in
    if inner = "" then []
    else String.split_on_char ' ' inner |> List.filter (( <> ) "")

let live ~url ~user ~password ~timeout expr =
  Pp_client.eval_expr ~base_url:url ~user ~password ~expr
    ~timeout_s:timeout

let live_value r =
  match r with
  | { Pp_client.ok = true; value = Some v; _ } ->
      Ok (Pp_client.unwrap_lisp_string v)
  | { ok = false; error = Some e; _ } -> Error e
  | _ -> Error "empty result"

let discover_packages ~url ~user ~password ~timeout =
  (* vault-shape (i-1788671331-d): discovery lives in Pp_sync_vault —
     dirs + live binding names collapsed to FIRST path segment (one
     family, one file). The old rindex spelling inferred sub-packages
     (notes/capture from notes/capture/add) so pull-all exported stale
     nested dumps the boot seed never reads. *)
  let ld = libs_dir () in
  let dirs =
    if Sys.file_exists ld then
      Array.to_list (Sys.readdir ld)
      |> List.filter (fun name ->
             let p = Filename.concat ld name in
             Sys.is_directory p
             && name <> "_root"
             && Sys.file_exists (Filename.concat p "lib.pp"))
    else []
  in
  let bindings =
    match live ~url ~user ~password ~timeout "(list-bindings)" |> live_value with
    | Ok s -> unwrap_list_of_strings s
    | Error _ -> []
  in
  Pp_sync_vault.packages_of ~dirs ~bindings

let cmd_status ~url ~user ~password ~timeout ~verbose package =
  match (if package <> "" then Pp_sync_vault.vault_refusal package else None) with
  | Some msg ->
      prerr_endline ("status: " ^ msg);
      2
  | None ->
  let pkgs =
    if package <> "" then [ package ]
    else discover_packages ~url ~user ~password ~timeout
  in
  let rc = ref 0 in
  List.iter
    (fun pkg ->
      let path = pkg_path pkg in
      let local =
        if Sys.file_exists path then parse_local_defines (read_file path)
        else []
      in
      let local_set = List.fold_left (fun s x -> x :: s) [] local in
      match
        live ~url ~user ~password ~timeout
          (Printf.sprintf "(package-bindings %S)" pkg)
        |> live_value
      with
      | Error e ->
          Printf.eprintf "%s: live error: %s\n%!" pkg e;
          rc := 1
      | Ok s ->
          let live_names = unwrap_list_of_strings s in
          let only_local =
            List.filter (fun n -> not (List.mem n live_names)) local_set
            |> List.sort_uniq String.compare
          in
          let only_live =
            List.filter (fun n -> not (List.mem n local_set)) live_names
            |> List.sort_uniq String.compare
          in
          let both =
            List.filter (fun n -> List.mem n live_names) local_set
            |> List.sort_uniq String.compare
          in
          Printf.printf "== %s ==\n" (if pkg = "" then "(root)" else pkg);
          Printf.printf "  local file: %s\n"
            (if Sys.file_exists path then path else "(missing)");
          Printf.printf
            "  both: %d  only-local: %d  only-live: %d\n"
            (List.length both) (List.length only_local)
            (List.length only_live);
          if only_local <> [] then
            Printf.printf "  only-local: %s\n"
              (String.concat ", " only_local);
          if only_live <> [] then
            Printf.printf "  only-live: %s\n" (String.concat ", " only_live);
          if verbose && both <> [] then
            Printf.printf "  both: %s\n" (String.concat ", " both);
          if only_local <> [] || only_live <> [] then rc := 1)
    pkgs;
  !rc

let cmd_pull ~url ~user ~password ~timeout package =
  (* vault-hygiene (i-1788671623-1, i-1788671331-d): refusals are checked
     BEFORE any network call, so a refused name cannot write the vault.
     The (root) namespace is not a package (root-branch lens dumps belong
     in gitignored backups/ via `pp-sync snapshot --branch <ref>`);
     sub-prefix names (notes/capture, …) are not packages either — pull
     used to export them as stale nested libs/<pkg>/<sub>/lib.pp dumps
     that boot never seeds and that drifted against the family monolith. *)
  if package = "" then begin
    prerr_endline "pull: need package name";
    2
  end else
  match Pp_sync_vault.vault_refusal package with
  | Some msg ->
      prerr_endline ("pull: " ^ msg);
      2
  | None ->
    match
      live ~url ~user ~password ~timeout
        (Printf.sprintf "(export-package %S)" package)
      |> live_value
    with
    | Error e ->
        prerr_endline e;
        1
    | Ok text ->
        let path = pkg_path package in
        write_file path text;
        Printf.printf "wrote %s (%d bytes)\n" path (String.length text);
        0

let cmd_push ~url ~user ~password ~timeout package =
  if package = "" then begin
    prerr_endline "push: need package name";
    2
  end else
  match Pp_sync_vault.vault_refusal package with
  | Some msg ->
      prerr_endline ("push: " ^ msg);
      2
  | None ->
    let path = pkg_path package in
    if not (Sys.file_exists path) then begin
      Printf.eprintf "push: missing %s\n" path;
      1
    end else
      let src = read_file path in
      (* Build a LISP string literal with the reader's own escaper
         (i-1789409290-0252a259). Yojson's JSON escaping was consumed
         directly by the Lisp reader, whose escape grammar differs:
         JSON \f / \b arrived as literal f / b (the reader is forgiving
         for unknown backslash-X), silently mutating pushed source.
         escape_string keeps UTF-8 raw and emits \\ddd for other
         controls, which the reader decodes byte-exactly. The JSON
         layer still encodes the REQUEST envelope (eval_expr does that
         itself) — only the Lisp literal inside the expr changes. *)
      let lit =
        "\"" ^ Pricklypear_lisp.Reader.escape_string src ^ "\""
      in
      match
        live ~url ~user ~password ~timeout
          (Printf.sprintf "(load-string %s)" lit)
        |> live_value
      with
      | Error e ->
          prerr_endline e;
          1
      | Ok msg ->
          print_endline msg;
          Printf.printf "pushed %s → live via load-string\n" path;
          0

let cmd_pull_all ~url ~user ~password ~timeout =
  let pkgs = discover_packages ~url ~user ~password ~timeout in
  List.fold_left
    (fun rc pkg ->
      Printf.printf "--- pull %s ---\n" pkg;
      let code = cmd_pull ~url ~user ~password ~timeout pkg in
      if code <> 0 then code else rc)
    0 pkgs

let cmd_reseed ~url ~user ~password ~timeout package =
  if package = "" then begin
    prerr_endline "reseed: need package name";
    2
  end else
  match Pp_sync_vault.vault_refusal package with
  | Some msg ->
      prerr_endline ("reseed: " ^ msg);
      2
  | None ->
    match
      live ~url ~user ~password ~timeout
        (Printf.sprintf "(reseed-package %S)" package)
      |> live_value
    with
    | Error e ->
        prerr_endline e;
        1
    | Ok msg ->
        print_endline msg;
        0

let pkg_path_under root pkg =
  if pkg = "" || pkg = "(root)" || pkg = "root" then
    Filename.concat (Filename.concat root "_root") "lib.pp"
  else Filename.concat (Filename.concat root pkg) "lib.pp"

let cmd_snapshot ~url ~user ~password ~timeout ~branch ~out_dir =
  let prev_branch =
    match live ~url ~user ~password ~timeout "(current-branch)" |> live_value with
    | Ok s -> String.trim s
    | Error _ -> ""
  in
  let target =
    if branch <> "" then branch
    else if prev_branch <> "" then prev_branch
    else "system/main"
  in
  let out_root =
    if out_dir <> "" then
      if Filename.is_relative out_dir then
        Filename.concat (repo_root ()) out_dir
      else out_dir
    else if target = "system/main" || target = "main" then libs_dir ()
    else
      Filename.concat
        (Filename.concat (repo_root ()) "backups")
        (sanitize_ref target)
  in
  (* Optional checkout to target branch for export visibility. *)
  let checkout_rc =
    if target = prev_branch || target = "" then 0
    else
      match
        live ~url ~user ~password ~timeout
          (Printf.sprintf "(checkout %S)" target)
        |> live_value
      with
      | Error e ->
          prerr_endline ("snapshot checkout: " ^ e);
          1
      | Ok msg ->
          if String.trim msg = "checked out" then 0
          else begin
            prerr_endline ("snapshot checkout: " ^ msg);
            1
          end
  in
  if checkout_rc <> 0 then checkout_rc
  else begin
    Printf.printf "snapshot branch=%s → %s\n%!" target out_root;
    let pkgs = discover_packages ~url ~user ~password ~timeout in
    let rc =
      List.fold_left
        (fun rc pkg ->
          match
            live ~url ~user ~password ~timeout
              (Printf.sprintf "(export-package %S)" pkg)
            |> live_value
          with
          | Error e ->
              Printf.eprintf "%s: %s\n%!" pkg e;
              if rc <> 0 then rc else 1
          | Ok text ->
              if String.trim text = "" then rc
              else begin
                let path = pkg_path_under out_root pkg in
                write_file path text;
                Printf.printf "  wrote %s (%d bytes)\n%!" path
                  (String.length text);
                rc
              end)
        0 pkgs
    in
    (* Restore previous branch when we switched. *)
    (if prev_branch <> "" && prev_branch <> target then
       ignore
         (live ~url ~user ~password ~timeout
            (Printf.sprintf "(checkout %S)" prev_branch)));
    rc
  end


let git_run ~cwd args : (int * string) =
  let prev = Sys.getcwd () in
  let restore () = try Sys.chdir prev with _ -> () in
  try
    Sys.chdir cwd;
    let stdout_path = Filename.temp_file "pp_sync_git" ".out" in
    let q a =
      if
        String.exists
          (function ' ' | '"' | '\'' | '\\' -> true | _ -> false)
          a
      then Filename.quote a
      else a
    in
    let cmdline =
      String.concat " " (List.map q ("git" :: args))
      ^ " >"
      ^ Filename.quote stdout_path
      ^ " 2>&1"
    in
    let code = Sys.command cmdline in
    let out =
      if Sys.file_exists stdout_path then read_file stdout_path else ""
    in
    (try Sys.remove stdout_path with _ -> ());
    restore ();
    (code, String.trim out)
  with exn ->
    restore ();
    (127, Printexc.to_string exn)

let checkout_ref ~url ~user ~password ~timeout ref_ =
  match
    live ~url ~user ~password ~timeout (Printf.sprintf "(checkout %S)" ref_)
    |> live_value
  with
  | Error e -> Error e
  | Ok msg ->
      if String.trim msg = "checked out" then Ok () else Error msg

let current_branch ~url ~user ~password ~timeout =
  match live ~url ~user ~password ~timeout "(current-branch)" |> live_value with
  | Ok s -> String.trim s
  | Error _ -> ""

let starts_with prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

(** pp-sync land <pkg> [name...] [--from owner/branch]
    1 resolve names on source
    2 promote-to-main
    3 export-package from system/main → libs/<pkg>/lib.pp
    4 git add + commit
    5 report SHA
    Never pushes; never unoverrides. Fail-fast per step. *)
let cmd_land ~url ~user ~password ~timeout ~from_ref ~package ~names =
  if package = "" then begin
    prerr_endline "land: need package name";
    2
  end else
  match Pp_sync_vault.vault_refusal package with
  | Some msg ->
      prerr_endline ("land: " ^ msg);
      2
  | None ->
    let prev = current_branch ~url ~user ~password ~timeout in
    let source =
      if from_ref <> "" then from_ref
      else if prev <> "" then prev
      else "admin/personal"
    in
    let restore () =
      if prev <> "" then
        ignore (checkout_ref ~url ~user ~password ~timeout prev)
    in
    Printf.printf "land: pkg=%s from=%s\n%!" package source;
    match checkout_ref ~url ~user ~password ~timeout source with
    | Error e ->
        prerr_endline ("land step1 checkout source: " ^ e);
        1
    | Ok () ->
        let name_list_res =
          if names <> [] then Ok names
          else
            match
              live ~url ~user ~password ~timeout
                (Printf.sprintf "(package-bindings %S)" package)
              |> live_value
            with
            | Error e -> Error ("land step1 package-bindings: " ^ e)
            | Ok s ->
                let ns = unwrap_list_of_strings s in
                if ns = [] then
                  Error
                    (Printf.sprintf
                       "land step1: no bindings for package %S on %s" package
                       source)
                else Ok ns
        in
        match name_list_res with
        | Error e ->
            prerr_endline e;
            restore ();
            1
        | Ok name_list ->
            Printf.printf "land: promoting %d name(s): %s\n%!"
              (List.length name_list) (String.concat ", " name_list);
            let names_lit =
              name_list
              |> List.map (fun n -> Printf.sprintf "%S" n)
              |> String.concat " "
            in
            let promote_expr =
              Printf.sprintf "(promote-to-main %S %s)" source names_lit
            in
            match live ~url ~user ~password ~timeout promote_expr |> live_value with
            | Error e ->
                prerr_endline ("land step2 promote failed: " ^ e);
                restore ();
                1
            | Ok msg ->
                let msg = String.trim msg in
                if
                  starts_with "promote-to-main:" msg
                  || starts_with "promote:" msg
                  || not (starts_with "promoted " msg)
                then begin
                  prerr_endline ("land step2 promote denied/failed: " ^ msg);
                  restore ();
                  1
                end else begin
                  Printf.printf "land step2: %s\n%!" msg;
                  match
                    checkout_ref ~url ~user ~password ~timeout "system/main"
                  with
                  | Error e ->
                      prerr_endline ("land step3 checkout system/main: " ^ e);
                      restore ();
                      1
                  | Ok () ->
                      match
                        live ~url ~user ~password ~timeout
                          (Printf.sprintf "(export-package %S)" package)
                        |> live_value
                      with
                      | Error e ->
                          prerr_endline ("land step3 export: " ^ e);
                          restore ();
                          1
                      | Ok text ->
                          let path = pkg_path package in
                          write_file path text;
                          Printf.printf "land step3: wrote %s (%d bytes)\n%!"
                            path (String.length text);
                          let root = repo_root () in
                          let rel = "libs/" ^ package ^ "/lib.pp" in
                          let names_msg =
                            if List.length name_list <= 6 then
                              String.concat " " name_list
                            else
                              Printf.sprintf "%d names" (List.length name_list)
                          in
                          let commit_msg =
                            Printf.sprintf "vault(%s): land %s" package
                              names_msg
                          in
                          let add_code, add_out =
                            git_run ~cwd:root [ "add"; "--"; rel ]
                          in
                          if add_code <> 0 then begin
                            prerr_endline
                              ("land step4 git add: " ^ add_out);
                            prerr_endline
                              "land: main promoted; vault written but not \
                               committed";
                            restore ();
                            1
                          end else
                            let diff_code, _ =
                              git_run ~cwd:root
                                [ "diff"; "--cached"; "--quiet" ]
                            in
                            if diff_code = 0 then begin
                              let _, sha =
                                git_run ~cwd:root
                                  [ "rev-parse"; "--short"; "HEAD" ]
                              in
                              Printf.printf
                                "land OK: promoted [%s] → system/main; vault \
                                 %s; HEAD %s (no new commit)\n%!"
                                (String.concat ", " name_list)
                                rel
                                (if sha = "" then "?" else sha);
                              restore ();
                              0
                            end else
                              let c_code, c_out =
                                git_run ~cwd:root
                                  [ "commit"; "-m"; commit_msg ]
                              in
                              if c_code <> 0 then begin
                                prerr_endline
                                  ("land step4 git commit: " ^ c_out);
                                prerr_endline
                                  "land: promote+vault done; commit failed";
                                restore ();
                                1
                              end else
                                let _, sha =
                                  git_run ~cwd:root
                                    [ "rev-parse"; "--short"; "HEAD" ]
                                in
                                Printf.printf
                                  "land OK: promoted [%s] → system/main; \
                                   vault %s; commit %s\n%!"
                                  (String.concat ", " name_list)
                                  rel
                                  (if sha = "" then "?" else sha);
                                restore ();
                                0
                end

let env_url =
  Pp_client.getenv_default "PP_URL" "http://127.0.0.1:18080"

let env_user = Pp_client.getenv_default "PP_USER" "admin"

let env_timeout =
  match Pp_client.getenv "PP_EVAL_TIMEOUT" with
  | Some s -> (try float_of_string s with _ -> 180.)
  | None -> 180.

let main command package names verbose url user password password_file timeout
    branch out_dir from_ref =
  let password =
    Pp_client.resolve_password ~password ~password_file
  in
  let token = Pp_client.getenv_default "PP_TOKEN" "" in
  if password = "" && String.trim token = "" then begin
    prerr_endline
      "pp-sync: set PP_PASS or --password (or PP_TOKEN for Bearer auth)";
    2
  end else
    match command with
    | "status" ->
        cmd_status ~url ~user ~password ~timeout ~verbose package
    | "pull" -> cmd_pull ~url ~user ~password ~timeout package
    | "push" -> cmd_push ~url ~user ~password ~timeout package
    | "pull-all" -> cmd_pull_all ~url ~user ~password ~timeout
    | "reseed" -> cmd_reseed ~url ~user ~password ~timeout package
    | "snapshot" ->
        cmd_snapshot ~url ~user ~password ~timeout ~branch ~out_dir
    | "land" ->
        cmd_land ~url ~user ~password ~timeout ~from_ref ~package ~names
    | _ ->
        prerr_endline
          "unknown command            (status|pull|push|pull-all|reseed|snapshot|land)";
        2

let command =
  let doc = "status | pull | push | pull-all | reseed | snapshot | land" in
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"COMMAND" ~doc)

let package =
  let doc = "Package name (ui, todo, …)." in
  Arg.(value & pos 1 string "" & info [] ~docv:"PKG" ~doc)

let verbose =
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc:"Verbose status.")

let url =
  Arg.(value & opt string env_url & info [ "url" ] ~docv:"URL" ~doc:"Image base URL.")

let user =
  Arg.(value & opt string env_user & info [ "user" ] ~docv:"USER" ~doc:"HTTP basic user.")

let password =
  Arg.(value & opt string "" & info [ "password"; "p" ] ~docv:"PASS" ~doc:"Password.")

let password_file =
  Arg.(value & opt string "" & info [ "password-file" ] ~docv:"PATH" ~doc:"Password file.")

let timeout =
  Arg.(value & opt float env_timeout & info [ "timeout" ] ~docv:"SEC" ~doc:"Timeout seconds.")

let branch =
  let doc = "Branch ref for snapshot (default: current; e.g. system/main)." in
  Arg.(value & opt string "" & info [ "branch"; "b" ] ~docv:"REF" ~doc)

let out_dir =
  let doc =
    "Output dir for snapshot (default: libs/ for system/main, else backups/<ref>/)."
  in
  Arg.(value & opt string "" & info [ "out"; "o" ] ~docv:"DIR" ~doc)

let names =
  let doc = "Function names for land (default: all package-bindings on source)." in
  Arg.(value & pos_right 1 string [] & info [] ~docv:"NAME" ~doc)

let from_ref =
  let doc = "Source branch for land (default: current session branch)." in
  Arg.(value & opt string "" & info [ "from" ] ~docv:"OWNER/BRANCH" ~doc)

let term =
  Term.(
    const main $ command $ package $ names $ verbose $ url $ user $ password
    $ password_file $ timeout $ branch $ out_dir $ from_ref)

let cmd =
  let doc =
    "Sync / snapshot / reseed / land Pricklypear packages (live image ↔ repo vault)."
  in
  Cmd.v (Cmd.info "pp-sync" ~doc) term

let () = exit (Cmd.eval' cmd)
