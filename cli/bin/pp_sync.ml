(** pp-sync — sync libs/*.pp with live function_defs (OCaml, no Python).

      pp-sync status [pkg]
      pp-sync pull <pkg>
      pp-sync push <pkg>
      pp-sync pull-all
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

let write_file path text =
  let dir = Filename.dirname path in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc text;
      if text = "" || text.[String.length text - 1] <> '\n' then
        output_char oc '\n')

let parse_local_defines text =
  let strip_comments s =
    let buf = Buffer.create (String.length s) in
    let in_str = ref false in
    let i = ref 0 in
    let n = String.length s in
    while !i < n do
      let c = s.[!i] in
      if c = '"' && (!i = 0 || s.[!i - 1] <> '\\') then begin
        in_str := not !in_str;
        Buffer.add_char buf c;
        incr i
      end else if c = ';' && not !in_str then begin
        while !i < n && s.[!i] <> '\n' do
          incr i
        done
      end else begin
        Buffer.add_char buf c;
        incr i
      end
    done;
    Buffer.contents buf
  in
  let blob = strip_comments text in
  let re = Re.Perl.compile_pat {|\(\s*define\s+([^\s()]+)|} in
  Re.all re blob
  |> List.filter_map (fun g ->
         try Some (Re.Group.get g 1) with Not_found -> None)
  |> List.sort_uniq String.compare

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
  let pkgs = Hashtbl.create 32 in
  let ld = libs_dir () in
  if Sys.file_exists ld then
    Array.iter
      (fun name ->
        let p = Filename.concat ld name in
        if
          Sys.is_directory p
          && name <> "_root"
          && Sys.file_exists (Filename.concat p "lib.pp")
        then Hashtbl.replace pkgs name ())
      (Sys.readdir ld);
  (match live ~url ~user ~password ~timeout "(list-bindings)" |> live_value with
   | Ok s ->
       List.iter
         (fun name ->
           match String.rindex_opt name '/' with
           | Some i ->
               Hashtbl.replace pkgs (String.sub name 0 i) ()
           | None -> ())
         (unwrap_list_of_strings s)
   | Error _ -> ());
  Hashtbl.fold (fun k () acc -> k :: acc) pkgs [] |> List.sort String.compare

let cmd_status ~url ~user ~password ~timeout ~verbose package =
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
            Printf.printf "  both: %s\n" (String.concat ", " both))
    pkgs;
  !rc

let cmd_pull ~url ~user ~password ~timeout package =
  if package = "" then begin
    prerr_endline "pull: need package name";
    2
  end else
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
    let path = pkg_path package in
    if not (Sys.file_exists path) then begin
      Printf.eprintf "push: missing %s\n" path;
      1
    end else
      let src = read_file path in
      (* JSON-encode as Lisp string literal for load-string *)
      let lit = Yojson.Safe.to_string (`String src) in
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

let env_url =
  Pp_client.getenv_default "PP_URL" "http://127.0.0.1:18080"

let env_user = Pp_client.getenv_default "PP_USER" "admin"

let env_timeout =
  match Pp_client.getenv "PP_EVAL_TIMEOUT" with
  | Some s -> (try float_of_string s with _ -> 180.)
  | None -> 180.

let main command package verbose url user password password_file timeout =
  let password =
    Pp_client.resolve_password ~password ~password_file
  in
  if password = "" then begin
    prerr_endline "pp-sync: set PP_PASS or --password";
    2
  end else
    match command with
    | "status" ->
        cmd_status ~url ~user ~password ~timeout ~verbose package
    | "pull" -> cmd_pull ~url ~user ~password ~timeout package
    | "push" -> cmd_push ~url ~user ~password ~timeout package
    | "pull-all" -> cmd_pull_all ~url ~user ~password ~timeout
    | _ ->
        prerr_endline "unknown command";
        2

let command =
  let doc = "status | pull | push | pull-all" in
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

let term =
  Term.(
    const main $ command $ package $ verbose $ url $ user $ password
    $ password_file $ timeout)

let cmd =
  let doc = "Sync libs/*.pp with live Pricklypear image (no Python)." in
  Cmd.v (Cmd.info "pp-sync" ~doc) term

let () = exit (Cmd.eval' cmd)
