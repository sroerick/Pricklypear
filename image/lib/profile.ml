(** Instance profile — one repo, several deployment shapes.

    spec_town_profile (approved 2026-09-13): mainframe runs the FULL
    package set; the town (public multi-tenant) box runs a curated
    subset. The subset lives in a repo file — profiles/<name>, one
    package name per line, '#' starts a comment — so instances differ
    only by profile, never by a private fork.

    PP_PROFILE selects the profile by name; PP_PROFILE_DIR overrides the
    directory (default: beside libs/, i.e. the repo root / release src
    stage). Unset = mainframe full behavior, byte-identical.

    The profile list is BOTH the boot seed set (Boot_seed) AND the
    tenant load-library allowlist (Session). A profile that cannot be
    read FAILS CLOSED: nothing is seeded, tenant tiers allow nothing;
    admin sessions keep full prims and can recover. *)

let name () : string option =
  match Sys.getenv_opt "PP_PROFILE" with
  | Some s when s <> "" -> Some s
  | _ -> None

let dir () : string =
  match Sys.getenv_opt "PP_PROFILE_DIR" with
  | Some d when d <> "" -> d
  | _ ->
      let libs = Loader.libs_dir () in
      (Filename.dirname libs) ^ "/profiles"

(** Parse profile text: one package per line; strip '#' comments and
    whitespace; drop blanks. *)
let parse (src : string) : string list =
  String.split_on_char '\n' src
  |> List.map (fun line ->
         match String.index_opt line '#' with
         | Some i -> String.sub line 0 i
         | None -> line)
  |> List.map String.trim
  |> List.filter (fun l -> l <> "")

(** Read + validate the configured profile.
    Ok pkgs: the profile's package list (may be empty = seeds nothing).
    Error msg: PP_PROFILE is set but the file is missing/unreadable or
    contains an invalid package name — callers must fail closed. *)
let packages () : (string list, string) result =
  match name () with
  | None -> Ok []
  | Some n ->
      let path = Filename.concat (dir ()) n in
      (match Loader.read_file path with
       | Error e -> Error (Printf.sprintf "profile %S unreadable: %s" n e)
       | Ok src ->
           let pkgs = parse src in
           (match
              List.find_opt
                (fun p -> not (Loader.valid_library_name p))
                pkgs
            with
            | Some p ->
                Error
                  (Printf.sprintf
                     "profile %S has invalid package name %S (want \
                      [a-z0-9_-]{1,64})"
                     n p)
            | None -> Ok pkgs))
