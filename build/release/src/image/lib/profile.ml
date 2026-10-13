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
    admin sessions keep full prims and can recover.

    Tier limits (spec_town_profile P1) ride the same file as @limit
    directives (see Tier_limits); malformed directives fail the same
    closed way. Limit values are memoized per process: they are boot
    config, like the seed set — a restart picks up edits. *)

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
    whitespace; drop blanks. @limit directive lines are NOT packages —
    they are skipped here and parsed by parse_limit_directives. *)
let parse (src : string) : string list =
  String.split_on_char '\n' src
  |> List.map (fun line ->
         match String.index_opt line '#' with
         | Some i -> String.sub line 0 i
         | None -> line)
  |> List.map String.trim
  |> List.filter (fun l ->
         l <> "" && not (String.starts_with ~prefix:"@limit" l))

(* One @limit directive: scope, key, value (scope in {tenant,instance};
   keys validated per scope; value a positive int). *)
type directive = string * string * int

let parse_int_value (line : string) (key : string) (raw : string) :
    (int, string) result =
  match int_of_string_opt raw with
  | Some v when v > 0 -> Ok v
  | _ -> Error (Printf.sprintf "@limit %s wants a positive int, got %S" key raw)

let parse_directive (line : string) : (directive list, string) result =
  match String.split_on_char ' ' line |> List.filter (fun t -> t <> "") with
  | "@limit" :: scope :: pairs when
      scope = "tenant" || scope = "instance" ->
      if pairs = [] then Error (Printf.sprintf "@limit %s: need key=value" scope)
      else
        List.fold_left
          (fun acc pair ->
            match acc with
            | Error e -> Error e
            | Ok ds ->
                (match String.index_opt pair '=' with
                 | None ->
                     Error
                       (Printf.sprintf "@limit %s: %S is not key=value" scope
                          pair)
                 | Some i ->
                     let key = String.sub pair 0 i in
                     let raw = String.sub pair (i + 1) (String.length pair - i - 1) in
                     let known =
                       match (scope, key) with
                       | "tenant", ("writes-per-min" | "media-mb") -> true
                       | "instance", "max-users" -> true
                       | _ -> false
                     in
                     if not known then
                       Error (Printf.sprintf "@limit %s: unknown key %S" scope key)
                     else
                       match parse_int_value line key raw with
                       | Error e -> Error e
                       | Ok v -> Ok (ds @ [ (scope, key, v) ])))
          (Ok []) pairs
  | _ ->
      Error
        (Printf.sprintf
           "@limit wants '@limit <tenant|instance> key=value ...', got %S" line)

(** Extract @limit directives from profile text. Anything else that
    starts with '@' is invalid (fail closed at the caller). *)
let parse_limit_directives (src : string) :
    (directive list, string) result =
  String.split_on_char '\n' src
  |> List.map (fun line ->
         match String.index_opt line '#' with
         | Some i -> String.sub line 0 i
         | None -> line)
  |> List.map String.trim
  |> List.filter (fun l -> l <> "" && String.starts_with ~prefix:"@" l)
  |> List.fold_left
       (fun acc line ->
         match acc with
         | Error e -> Error e
          | Ok ds ->
              (match parse_directive line with
               | Error e -> Error e
               | Ok d -> Ok (ds @ d)))
       (Ok [])

(** Memoized limit config for the configured profile (boot config,
    like the seed set: a restart picks up edits).

    tenant = None  : no PP_PROFILE — no tier enforcement (mainframe).
    tenant = Some l: tenant limits folded over env defaults.
    Error msg      : profile unreadable or a bad directive (Boot_seed
                     surfaces this at boot; limit callers fall back
                     deny-lean to the built-in defaults). *)
type limit_config = {
  tenant : Tier_limits.limits option;
  max_users : int;  (* always active, every deployment *)
}

let compute_config () : (limit_config, string) result =
  match name () with
  | None -> Ok { tenant = None; max_users = Tier_limits.default_max_users () }
  | Some n ->
      let path = Filename.concat (dir ()) n in
      (match Loader.read_file path with
       | Error e ->
           Error (Printf.sprintf "profile %S unreadable: %s" n e)
       | Ok src ->
           (match parse_limit_directives src with
            | Error e -> Error (Printf.sprintf "profile %S limits: %s" n e)
            | Ok ds ->
                let max_users =
                  match
                    List.find_map
                      (fun (s, k, v) ->
                        if s = "instance" && k = "max-users" then Some v
                        else None)
                      ds
                  with
                  | Some v -> v
                  | None -> Tier_limits.default_max_users ()
                in
                Ok
                  { tenant =
                      Some
                        (Tier_limits.apply_directives
                           (Tier_limits.default_tenant ()) ds)
                  ; max_users }))

let config_memo = lazy (compute_config ())

let limits () : (Tier_limits.limits option, string) result =
  match Lazy.force config_memo with Ok c -> Ok c.tenant | Error e -> Error e

(** Tenant limits with fallback: unreadable profile -> built-in
    defaults (enforced, deny-lean); no profile -> None (caller skips). *)
let tenant_limits () : Tier_limits.limits option =
  match limits () with
  | Ok (Some l) -> Some l
  | Ok None -> None
  | Error _ -> Some (Tier_limits.default_tenant ())

(** Instance user cap (always active): profile @limit instance
    max-users > env PP_MAX_USERS > built-in default 100. An unreadable
    profile keeps the default (fail-safe, not fail-open). *)
let max_users () : int =
  match Lazy.force config_memo with
  | Ok c -> c.max_users
  | Error _ -> Tier_limits.default_max_users ()

(** Read + validate the configured profile.
    Ok pkgs: the profile's package list (may be empty = seeds nothing).
    Error msg: PP_PROFILE is set but the file is missing/unreadable,
    contains an invalid package name, or has a malformed @limit
    directive — callers must fail closed. *)
let packages () : (string list, string) result =
  match name () with
  | None -> Ok []
  | Some n ->
      let path = Filename.concat (dir ()) n in
      (match Loader.read_file path with
       | Error e -> Error (Printf.sprintf "profile %S unreadable: %s" n e)
       | Ok src ->
           (match parse_limit_directives src with
            | Error e ->
                Error (Printf.sprintf "profile %S: %s" n e)
            | Ok _ ->
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
                 | None -> Ok pkgs)))
