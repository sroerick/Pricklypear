(** pp-sync — sync libs/*.pp with live function_defs (OCaml, no Python).

      pp-sync status [pkg] [--branch R]   vault↔system/main drift (default R)
      pp-sync pull <pkg>                 guarded merge of live main → vault
      pp-sync push <pkg>                 load vault file onto the live
                                        CURRENT session branch (garden)
      pp-sync pull-all                   pull every vaulted package
      pp-sync reseed <pkg>               admin: upsert libs → system/main
                                        (refuses on live-only names OR
                                        body drift without --force)
      pp-sync snapshot [--branch R] [--out DIR] [--dry-run]
      pp-sync land <pkg> [name ...] [--from owner/branch]
                                        promote live → main, then export
                                        back into the vault (guarded merge)

    ONE write authority (borge/branching.borg vault-authority): the
    vault mirrors live system/main. pull / snapshot / land all funnel
    through Pp_sync_merge.plan_write — hand-authored refusal, gut
    refusal, comment-preserving merge — so the guards cannot differ
    per verb. status/pull/snapshot default to comparing branch
    system/main (the vault's mirror source), not whatever branch the
    operator's session happens to be on. *)

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

(* Local define bodies, keyed by name (card 8dff7c9e Option 1): the
   NAME-level diff in cmd_status cannot see a stale BODY — exactly the
   "git has the fix, live main does not" case. Parsing + canonical body
   comparison live in Pp_sync_diff (unit-tested, and shared with the
   live-export side). *)
let parse_local_bodies (text : string) : (string * string) list option =
  Pp_sync_diff.parse_bodies text

(* Bodies for a package on a branch, from its live export-package text. *)
let bodies_of_export (text : string) : (string * string) list =
  Pp_sync_diff.bodies_of_export text

let body_divergence = Pp_sync_diff.divergence
let pp_body_json = Pp_sync_diff.pp_body

let unwrap_list_of_strings (printed : string) : string list =
  Pp_sync_diff.unwrap_quoted_names printed

let live ~url ~user ~password ~timeout expr =
  Pp_client.eval_expr ~base_url:url ~user ~password ~expr
    ~timeout_s:timeout ~quiet:false

let live_value r =
  match r with
  | { Pp_client.ok = true; value = Some v; _ } ->
      Ok (Pp_client.unwrap_lisp_string v)
  | { ok = false; error = Some e; _ } -> Error e
  | _ -> Error "empty result"

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

let discover_packages ~url ~user ~password ~timeout =
  (* vault-shape (i-1788671331-d): discovery lives in Pp_sync_vault —
     dirs + live binding names collapsed to FIRST path segment (one
     family, one file). The old rindex spelling inferred sub-packages
     (notes/capture from notes/capture/add) so pull-all exported stale
     nested dumps the boot seed never reads.
     BANNED families (Pp_sync_vault.vault_banned) are dropped silently:
     their live rows are known duplicates whose vault file is
     deliberately gone (libs/list is std's old shape, deleted twice by
     hand) — an EXPLICIT pull/reseed/land still refuses loudly. *)
  let ld = libs_dir () in
  let dirs =
    if Sys.file_exists ld then
      Array.to_list (Sys.readdir ld)
      |> List.filter (fun name ->
             let p = Filename.concat ld name in
             Sys.is_directory p
             && name <> "_root"
             && Sys.file_exists (Filename.concat p "lib.pp"))
      |> List.filter (fun name -> Pp_sync_vault.vault_banned name = None)
    else []
  in
  let bindings =
    match live ~url ~user ~password ~timeout "(list-bindings)" |> live_value with
    | Ok s -> unwrap_list_of_strings s
    | Error _ -> []
  in
  Pp_sync_vault.packages_of ~dirs ~bindings
  |> List.filter (fun p -> Pp_sync_vault.vault_banned p = None)

let cmd_status ~url ~user ~password ~timeout ~verbose ~names_only ~branch package =
  match (if package <> "" then Pp_sync_vault.vault_refusal package else None) with
  | Some msg ->
      prerr_endline ("status: " ^ msg);
      2
  | None ->
  (* VAULT AUTHORITY (card 8dff7c9e): the vault mirrors system/main, so
     the drift view reads MAIN by default — not whatever branch the
     operator's session happens to be on (the 2026-10-02 review: the
     plain run compared against admin/personal and inflated drift).
     The comparison branch is checked out for the export pass and
     restored afterward — the snapshot pattern. An explicit --branch is
     still honored, labeled, and restored. *)
  let prev_branch =
    match current_branch ~url ~user ~password ~timeout with
    | s -> String.trim s
    | exception _ -> ""
  in
  let target = if branch <> "" then branch else "system/main" in
  let restore () =
    if prev_branch <> "" && prev_branch <> target then
      ignore (checkout_ref ~url ~user ~password ~timeout prev_branch)
  in
  if target <> prev_branch then
    match checkout_ref ~url ~user ~password ~timeout target with
    | Ok () -> ()
    | Error e ->
        prerr_endline ("status: checkout " ^ target ^ ": " ^ e);
        restore ();
        raise Exit
  else ();
  let pkgs =
    if package <> "" then [ package ]
    else discover_packages ~url ~user ~password ~timeout
  in
  if target <> prev_branch then
    Printf.printf "(comparing vault against %s — the vault mirrors \
                    system/main; other branches show garden drift)\n"
      target
  else Printf.printf "(comparing vault against %s)\n" target;
  let rc = ref 0 in
  (try
     List.iter
       (fun pkg ->
         let path = pkg_path pkg in
         let local =
           if Sys.file_exists path then parse_local_defines (read_file path)
           else []
         in
         let local_set = List.fold_left (fun s x -> x :: s) [] local in
         (* Option 1 (card 8dff7c9e): also diff BODIES. package-bindings
            only names a define, so a name whose git body differs from live
            main looks "both" and healthy while the fix is undeployed.
            Fetch the live export and compare canonical AST bodies. *)
         let live_bodies = ref None in
         (if not names_only then
            match
              live ~url ~user ~password ~timeout
                (Printf.sprintf "(export-package %S)" pkg)
              |> live_value
            with
            | Ok text -> live_bodies := Some (bodies_of_export text)
            | Error _ -> ());
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
             let body_changed, _body_only_local, _body_only_live =
               match !live_bodies with
               | None -> ([], [], [])
               | Some live_body_list ->
                   let local_bodies =
                     match parse_local_bodies (read_file path) with
                     | Some bs -> bs
                     | None -> []
                   in
                   body_divergence ~local:local_bodies ~live:live_body_list
             in
             Printf.printf "== %s ==\n" (if pkg = "" then "(root)" else pkg);
             Printf.printf "  local file: %s\n"
               (if Sys.file_exists path then path else "(missing)");
             Printf.printf
               "  both: %d  only-local: %d  only-live: %d"
               (List.length both) (List.length only_local)
               (List.length only_live);
             if !live_bodies <> None then
               Printf.printf "  body-stale: %d" (List.length body_changed);
             Printf.printf "\n";
             if only_local <> [] then
               Printf.printf "  only-local: %s\n"
                 (String.concat ", " only_local);
             if only_live <> [] then
               Printf.printf "  only-live: %s\n"
                 (String.concat ", " only_live);
             (* The load-bearing signal for Option 1: a name present both
                places whose LIVE body differs from the vault body. These
                are the fixes stuck in git that an additive boot seed never
                deploys. *)
             if body_changed <> [] then begin
               Printf.printf "  body-stale (vault differs from live): %s\n"
                 (String.concat ", "
                    (List.map (fun (n, _, _) -> n) body_changed));
               if verbose then
                 List.iter
                   (fun (n, lb, vb) ->
                     Printf.printf
                       "    %s:\n      vault: %s\n      live:  %s\n" n
                       (pp_body_json lb) (pp_body_json vb))
                   body_changed
             end;
             if verbose && both <> [] then
               Printf.printf "  both: %s\n" (String.concat ", " both);
             if only_local <> [] || only_live <> [] || body_changed <> [] then
               rc := 1)
       pkgs
   with Exit -> ());
  restore ();
  !rc

(* Fetch the union of binding names for the currently-checked-out branch
   as completely as the eval API allows: every package's bindings from
   the registry (list-packages → package-bindings each) plus the root
   set. Used as the LAST-RESORT accounting set for dropped names ("does
   live main bind this name anywhere?"), because (list-bindings) is the
   SESSION env (353-odd names incl. prims) — NOT the store's binding
   table, and package names are lazily bound. Slow (~one eval per
   package); only runs when a vault write is refused over unaccounted
   drops, never on the happy path. Package-binding "" is the ROOT set
   (bare names — REPL debris, unresolved seed blockers live there). *)
let sweep_live_names ~url ~user ~password ~timeout base =
  let add_from_expr expr acc =
    match live ~url ~user ~password ~timeout expr |> live_value with
    | Ok s -> List.rev_append (unwrap_list_of_strings s) acc
    | Error _ -> acc
  in
  let pkgs =
    match
      live ~url ~user ~password ~timeout "(list-packages)" |> live_value
    with
    | Ok s -> unwrap_list_of_strings s
    | Error _ -> []
  in
  List.fold_left
    (fun acc pkg ->
      add_from_expr (Printf.sprintf "(package-bindings %S)" pkg) acc)
    (add_from_expr "(package-bindings \"\")" base)
    pkgs
  |> List.sort_uniq String.compare

(* THE ONE VAULT WRITE (2026-10-02 review): every path that puts live
   export text into a vault file — pull, snapshot, land — funnels through
   [vault_write]: same refusals (hand-authored with no live content /
   gut / unaccounted drops / unparseable), same comment-preserving
   merge, same reporting. Before this, snapshot was guarded but
   pull/land wrote RAW generator output (pull overwrote 20 packages on
   2026-10-01/02 and only escaped comment loss because they happened to
   be comment-free). [~verb] labels messages; [~dry] computes and
   reports everything without touching disk; [~live_names] is the
   dropped-name accounting base; [~sweep] (optional) fetches a wider
   live-name set for one retry when a refusal is about unaccounted
   drops. Returns process exit code: 0 wrote (or would), 2 refusal —
   guards are load-bearing, never "best effort". *)
let vault_write ~(sweep : unit -> string list) ~verb ~path ~new_text ~live_names
    ~force ~dry : int =
  let old_text =
    if Sys.file_exists path then read_file path else ""
  in
  let rec attempt ~live_names =
    Pp_sync_merge.plan_write ~old_text ~new_text ~live_names ~force
  in
  let report outcome =
    match outcome with
    | Pp_sync_merge.Refused reason ->
        Printf.eprintf "  REFUSED %s %s: %s\n%!" verb path reason;
        2
    | Pp_sync_merge.Write wr ->
      if dry then
        Printf.printf "  DRY %s %s (%d bytes would be written): %s\n%!" verb
          path (String.length wr.text)
          (Pp_sync_merge.describe_write
             (Pp_sync_merge.Write wr))
      else begin
        write_file path wr.text;
        Printf.printf "  %s %s (%d bytes): %s\n%!" verb path
          (String.length wr.text)
          (Pp_sync_merge.describe_write (Pp_sync_merge.Write wr))
      end;
      let rep = wr.report in
      if wr.raw && old_text <> "" then
        (* a raw overwrite (only via --force) has no comment knowledge —
           warn loudly if the file it replaces has interior comments *)
        Printf.printf "    note: raw write replaces a file with %d \
                       comment lines — they will be LOST.\n%!"
          (Pp_sync_merge.count_comment_lines old_text);
      if rep.removed <> [] then
        Printf.printf "    note: %d name(s) removed from this file — live \
                       main still binds them (re-homed to another \
                       package or root) — the vault keeps ONE file per \
                       family: %s\n%!"
          (List.length rep.removed) (String.concat ", " rep.removed);
      if wr.hand then
        Printf.printf "    note: hand-authored file MERGED — custom header \
                       kept, unchanged forms kept verbatim.\n%!";
      (match (not wr.raw, rep.changed_with_comments) with
       | true, names when names <> [] ->
           Printf.printf "    %-26s %s the vault still has old comments to \
                          re-attach by hand: %s\n%!"
             "changed-with-comments:"
             (string_of_int (List.length names))
             (String.concat ", " names)
       | _ -> ());
      0
  in
  (* first attempt with the caller's base names; the sweep (pull/land
     only, and only when the base says a drop is unaccounted) fetches a
     WIDER live-name set once and retries — it can only widen, never
     narrow; a refusal after the sweep is what gets printed. The root
     set + session env in the base keep the common path cheap. *)
  (match attempt ~live_names with
   | Pp_sync_merge.Refused _ ->
       let wider = sweep () in
       attempt ~live_names:(live_names @ wider) |> report
   | outcome -> report outcome)

let cmd_pull ~url ~user ~password ~timeout ~force ~dry package =
  (* vault-hygiene (i-1788671623-1, i-1788671331-d): refusals are checked
     BEFORE any network call, so a refused name cannot write the vault.
     The (root) namespace is not a package (root-branch lens dumps belong
     in gitignored backups/ via `pp-sync snapshot --branch <ref>`);
     sub-prefix names (notes/capture, ...) are not packages either — pull
     used to export them as stale nested libs/<pkg>/<sub>/lib.pp dumps
     that boot never seeds and that drifted against the family monolith.
     2026-10-02: pull is a guarded MERGE (vault_write), no longer a raw
     overwrite — a capture verb must not strip interior comments it
     can preserve. --force restores the old raw clobber. *)
  if package = "" then begin
    prerr_endline "pull: need package name";
    2
  end else
  match Pp_sync_vault.vault_refusal package with
  | Some msg ->
      prerr_endline ("pull: " ^ msg);
      2
  | None ->
  (* VAULT AUTHORITY: the vault mirrors system/main, so a pull READS main
     (checkout + restore), not whatever garden branch the session is
     on — capturing a garden branch into libs/ would write personal
     state into the mirror (use `snapshot --branch <ref> --out
     backups/<ref>/` for that). *)
  let prev_branch =
    match current_branch ~url ~user ~password ~timeout with
    | s -> String.trim s
    | exception _ -> ""
  in
  let restore () =
    if prev_branch <> "" && prev_branch <> "system/main" then
      ignore (checkout_ref ~url ~user ~password ~timeout prev_branch)
  in
  let checkout_rc =
    if prev_branch = "system/main" then 0
    else
      match checkout_ref ~url ~user ~password ~timeout "system/main" with
      | Ok () -> 0
      | Error e -> prerr_endline ("pull: checkout system/main: " ^ e); 1
  in
  if checkout_rc <> 0 then begin restore (); 2 end
  else begin
    Printf.printf "(pulling %s from system/main — the vault mirrors main)\n%!" package;
    match
      live ~url ~user ~password ~timeout
        (Printf.sprintf "(export-package %S)" package)
      |> live_value
    with
    | Error e ->
        prerr_endline e;
        restore ();
        1
    | Ok text ->
        let path = pkg_path package in
        (* live_names base for dropped-name accounting: the names in THIS
           export + the root set (package-bindings "" — bare REPL/seed
           names live there) + the session env as harmless extra. A drop
         still unaccounted triggers one sweep (sweep_live_names) —
         (list-bindings) alone is NOT the store binding set (session env,
         lazily bound: 353 names incl. prims, not the ~1k function rows). *)
        let base =
          let of_text =
            List.map (fun (n, _) -> n) (bodies_of_export text)
          in
          let root =
            match
              live ~url ~user ~password ~timeout "(package-bindings \"\")"
              |> live_value
            with
            | Ok s -> unwrap_list_of_strings s
            | Error _ -> []
          in
          let session =
            match
              live ~url ~user ~password ~timeout "(list-bindings)" |> live_value
            with
            | Ok s -> unwrap_list_of_strings s
            | Error _ -> []
          in
          List.rev_append of_text (List.rev_append root session)
        in
        let code =
          vault_write
            ~sweep:(fun () -> sweep_live_names ~url ~user ~password ~timeout [])
            ~verb:"pull" ~path ~new_text:text ~live_names:base ~force ~dry
        in
        restore ();
        code
  end

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

let cmd_pull_all ~url ~user ~password ~timeout ~force ~dry =
  let pkgs = discover_packages ~url ~user ~password ~timeout in
  List.fold_left
    (fun rc pkg ->
      Printf.printf "--- pull %s ---\n" pkg;
      let code = cmd_pull ~url ~user ~password ~timeout ~force ~dry pkg in
      if code <> 0 then code else rc)
    0 pkgs

let pkg_path_under root pkg =
  if pkg = "" || pkg = "(root)" || pkg = "root" then
    Filename.concat (Filename.concat root "_root") "lib.pp"
  else Filename.concat (Filename.concat root pkg) "lib.pp"

let cmd_reseed ~url ~user ~password ~timeout ~force package =
  if package = "" then begin
    prerr_endline "reseed: need package name";
    2
  end else
  match Pp_sync_vault.vault_refusal package with
  | Some msg ->
      prerr_endline ("reseed: " ^ msg);
      2
  | None ->
    (* Vault-drift guard (card 8dff7c9e): reseed is an explicit clobber of
       system/main from libs/. TWO refusals, both keying the same
       hazard — reseed overwrites live with vault text on purpose, so
       the vault must be PROVEN not-stale in both directions:
         (a) live-only NAMES (bound on main, absent from the vault) —
             reseed would delete them;
         (b) same-name BODY drift (2026-10-02) — live bodies newer than
             vault bodies; reseed would silently roll them back.
       Read-only when refusing; --force is the deliberate override;
       natural recovery is `pp-sync pull %s` (merges live into the
       vault, guarded) then reseed. *)
    let path = pkg_path_under (libs_dir ()) package in
    let live_only =
      let local =
        if Sys.file_exists path then parse_local_defines (read_file path)
        else []
      in
      let local_set = List.fold_left (fun s x -> x :: s) [] local in
      match
        live ~url ~user ~password ~timeout
          (Printf.sprintf "(package-bindings %S)" package)
        |> live_value
      with
      | Error _ -> []
      | Ok s ->
          unwrap_list_of_strings s
          |> List.filter (fun n -> not (List.mem n local_set))
          |> List.sort_uniq String.compare
    in
    let proceed () =
      match
        live ~url ~user ~password ~timeout
          (Printf.sprintf "(reseed-package %S)" package)
        |> live_value
      with
      | Error e ->
          prerr_endline e;
          2
      | Ok msg ->
          print_endline msg;
          0
    in
    if live_only <> [] && not force then begin
      Printf.eprintf
        "reseed: REFUSED — %s has %d live-only define(s) absent from the vault.\n\
         Reseeding would DELETE them. Run `pp-sync pull %s` first to merge\n\
         them into libs/, or pass --force to clobber anyway.\n\
         live-only: %s\n%!"
        package (List.length live_only) package
        (String.concat ", " live_only);
      2
    end
    else if not force then begin
      (* BODY guard (2026-10-02 review): the live-only guard was half
         blind. When the SAME names sit on both sides with different
         CANONICAL bodies — the habitat-fewer fix clobber — reseed
         overwrites live main with the vault text, SILENTLY. Run the
         body drift view (canned by `pp-sync status <pkg>`); if live
         bodies differ, merge them into the vault first (`pull %s`) or
         review; reseed is then a no-op refresh the boot seed cannot
         give existing names. --force stands for "the vault is truly
         newer, clobber live on purpose". *)
      let body_stale =
        match
          live ~url ~user ~password ~timeout
            (Printf.sprintf "(export-package %S)" package)
          |> live_value
        with
        | Error _ -> []  (* a live error must not silently permit a
                            clobber — unknown ≠ clean; refuse to guess *)
        | Ok text ->
            let local_bodies =
              if Sys.file_exists path then
                match parse_local_bodies (read_file path) with
                | Some bs -> bs
                | None -> []
              else []
            in
            body_divergence ~local:local_bodies
              ~live:(bodies_of_export text)
            |> (fun (changed, _, _) -> changed)
      in
      (match body_stale with
       | (n, _, _) :: _ ->
           Printf.eprintf
             "reseed: REFUSED — %d same-name live body(ies) DIFFER from the\n\
              vault (e.g. %s). Live is usually NEWER (habitat-side fixes —\n\
              the vault-authority point); reseed would clobber them with\n\
              vault text. Run `pp-sync pull %s` to merge live into the\n\
              vault, review with `pp-sync status -v`, then reseed.\n\
              --force to clobber anyway.\n%!"
             (List.length body_stale) n package;
           2
       | [] -> proceed ())
    end
    else proceed ()

let cmd_snapshot ~url ~user ~password ~timeout ~branch ~out_dir ~force ~dry =
  let prev_branch =
    match live ~url ~user ~password ~timeout "(current-branch)" |> live_value with
    | Ok s -> String.trim s
    | Error _ -> ""
  in
  (* VAULT AUTHORITY: a snapshot writes the mirror of one branch.
     Default target = system/main (the vault's mirror source); a
     personal/dev ref dumps to backups/<ref>/ (never libs/). *)
  let target = if branch <> "" then branch else "system/main" in
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
      | Error e -> prerr_endline ("snapshot checkout: " ^ e); 1
      | Ok msg ->
          if String.trim msg = "checked out" then 0
          else begin prerr_endline ("snapshot checkout: " ^ msg); 1 end
  in
  if checkout_rc <> 0 then checkout_rc
  else begin
    Printf.printf "snapshot branch=%s → %s%s\n%!" target out_root
      (if dry then " (DRY RUN — nothing will be written)" else "");
    let pkgs = discover_packages ~url ~user ~password ~timeout in
    (* PASS 1 (exports first): every export text is collected AND its
       define names folded into the live-name accounting base. (list-bindings)
       is the lazily-bound session env (~353 names incl. prims) — NOT
       the store's binding table — so the export sweep plus the root set
       (package-bindings "" = bare REPL/seed names) is what makes
       dropped-name accounting complete: a re-homed name (ui/badge
       moving lookbook→ui) merges with its drop reported, a vault-only
       name refuses BY NAME. *)
    let root_and_session =
      let root =
        match
          live ~url ~user ~password ~timeout "(package-bindings \"\")"
          |> live_value
        with
        | Ok s -> unwrap_list_of_strings s
        | Error _ -> []
      in
      let session =
        match
          live ~url ~user ~password ~timeout "(list-bindings)" |> live_value
        with
        | Ok s -> unwrap_list_of_strings s
        | Error _ -> []
      in
      List.rev_append root session
    in
    let fetch_rc = ref 0 in
    let exports =
      List.fold_left
        (fun acc pkg ->
          match
            live ~url ~user ~password ~timeout
              (Printf.sprintf "(export-package %S)" pkg)
            |> live_value
          with
          | Error e ->
              Printf.eprintf "%s: %s\n%!" pkg e;
              if !fetch_rc = 0 then fetch_rc := 1;
              acc
          | Ok text -> (pkg, text) :: acc)
        [] pkgs
    in
    let live_names =
      List.fold_left
        (fun acc (_, text) ->
          List.fold_left
            (fun a (n, _) -> n :: a) acc (bodies_of_export text))
        root_and_session exports
      |> List.sort_uniq String.compare
    in
    (* PASS 2 (the writes): THE ONE WRITE PATH — snapshot / pull / land
       all decide through vault_write (hand-authored zero-live refusal,
       unaccounted-drop refusal, gut refusal, comment-preserving merge
       with a custom header KEPT — the std case). DRY RUN compares
       against the file at the same path a real run would touch, so its
       report IS the report you would get (an `--out /tmp` preview
       bypassed every guard: no old file there to compare). *)
    let write_rc = ref 0 in
    List.iter
      (fun (pkg, text) ->
        if String.trim text = "" then ()
        else begin
          let path = pkg_path_under out_root pkg in
          let code =
            vault_write ~sweep:(fun () -> []) ~verb:"snapshot" ~path
              ~new_text:text ~live_names ~force ~dry
          in
          if code <> 0 && !write_rc = 0 then write_rc := code
        end)
      (List.rev exports);
    (* Restore previous branch when we switched. *)
    (if prev_branch <> "" && prev_branch <> target then
       ignore
         (live ~url ~user ~password ~timeout
            (Printf.sprintf "(checkout %S)" prev_branch)));
    if !write_rc <> 0 then !write_rc else !fetch_rc
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
                          (* ONE WRITE PATH (2026-10-02 review): land used
                             to write RAW export output over the vault —
                             the blessed promote path stripped comments
                             itself (filmstrip would lose 144 of its 146
                             lines here). Same guarded merge as snapshot /
                             pull; a REFUSED vault file (e.g. live export
                             empty for a hand-authored package) leaves
                             main promoted and the vault untouched — say
                             so loudly rather than silently gut libs/. *)
                          let live_names =
                            (* full main binding set, post-promote: the
                               just-landed names must all be bound (they
                               were); every name accounts. [] on fetch
                               failure = conservative. *)
                            (match
                               live ~url ~user ~password ~timeout "(list-bindings)"
                               |> live_value
                             with
                             | Ok s -> unwrap_list_of_strings s
                             | Error _ -> [])
                          in
                          let code =
                            vault_write
                              ~sweep:(fun () -> sweep_live_names ~url ~user ~password ~timeout [])
                              ~verb:"land" ~path ~new_text:text ~live_names
                              ~force:false ~dry:false
                          in
                          if code <> 0 then begin
                            Printf.eprintf
                              "land: vault NOT refreshed (refused) — main IS \
                               promoted; run `pp-sync pull %s` after review\n%!"
                              package;
                            restore ();
                            1
                          end else begin
                          Printf.printf "land step3: wrote %s\n%!" path;
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
                end

let env_url =
  Pp_client.getenv_default "PP_URL" "http://127.0.0.1:18080"

let env_user = Pp_client.getenv_default "PP_USER" "admin"

let env_timeout =
  match Pp_client.getenv "PP_EVAL_TIMEOUT" with
  | Some s -> (try float_of_string s with _ -> 180.)
  | None -> 180.

let main command package names verbose names_only url user password password_file timeout
    branch out_dir from_ref force dry =
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
        cmd_status ~url ~user ~password ~timeout ~verbose ~names_only ~branch
          package
    | "pull" -> cmd_pull ~url ~user ~password ~timeout ~force ~dry package
    | "push" -> cmd_push ~url ~user ~password ~timeout package
    | "pull-all" -> cmd_pull_all ~url ~user ~password ~timeout ~force ~dry
    | "reseed" -> cmd_reseed ~url ~user ~password ~timeout ~force package
    | "snapshot" ->
        cmd_snapshot ~url ~user ~password ~timeout ~branch ~out_dir ~force ~dry
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

let names_only =
  let doc =
    "status: skip the body-level diff and compare names only (fast, but \
     blind to a stale body — the git-has-the-fix main-does-not case)."
  in
  Arg.(value & flag & info [ "names-only" ] ~doc)

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
  let doc =
    "Branch ref for snapshot / status (default: system/main — the vault \
     mirrors main; other refs show garden drift and dump to backups/). \
     status restores the session branch after the comparison."
  in
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

let force =
  let doc =
    "THE deliberate override of the vault guards (they exist because the \
     danger is silent data loss): pull/snapshot/land write RAW generator \
     output ignoring the hand-authored / gut refusals; reseed clobbers \
     main past live-only names and body drift. Always review the refusal \
     message first — --force means you read it and chose anyway."
  in
  Arg.(value & flag & info [ "force" ] ~doc)

let dry =
  let doc =
    "pull / pull-all / snapshot: compute every guard + merge report and \
     write NOTHING (a true dry run — the report is against the same paths \
     a real run would touch, unlike an --out /tmp preview, which bypassed \
     the guards)."
  in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let term =
  Term.(
    const main $ command $ package $ names $ verbose $ names_only $ url $ user
    $ password $ password_file $ timeout $ branch $ out_dir $ from_ref $ force
    $ dry)

let cmd =
  let doc =
    "Sync / snapshot / reseed / land Pricklypear packages (live image ↔ repo vault)."
  in
  Cmd.v (Cmd.info "pp-sync" ~doc) term

let () = exit (Cmd.eval' cmd)
