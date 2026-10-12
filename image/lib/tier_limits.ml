(** Pricklypear Image - tier limits (spec_town_profile P1 quotas).

    Three knobs, one module:

      - max users: instance cap on non-agent accounts, enforced in
        Auth_store.create_user. Active on EVERY deployment: default
        100, env override PP_MAX_USERS, profile directive
        @limit instance max-users=N wins when a profile sets it.

      - write rate limit: per-username fixed-window (60s) cap on
        mutating requests (POST/PUT/DELETE), enforced in Http.launch.
        Active ONLY under a named profile (PP_PROFILE set) and only
        for restricted sessions (Session.restricted: delegated tokens,
        or non-admin under a profile). The mainframe (no profile) and
        admin sessions are never throttled, so live agent traffic is
        unaffected until a profile opts in.

      - media storage quota: per-author cap on summed
        media_blobs.byte_size, enforced inside Media.put so the HTTP
        edge and the media-put / media-put-raw prims share one
        implementation. Active only under a named profile; admins
        exempt; unknown authors treated as tenants (deny-lean).

    Config precedence: env var > profile directive > built-in default.
    Profile directives (parsed by Profile, applied here):

      @limit tenant writes-per-min=120 media-mb=64
      @limit instance max-users=50

    A malformed directive FAILS CLOSED at boot: Profile.packages
    errors, nothing is seeded, and limits fall back to the built-in
    defaults (same recovery rule as the package seed).

    Pure module (auth_throttle style): no I/O; callers supply clock
    and limit values. The write counters are process-local (the image
    is one process over one Postgres) and safe under the single Eio
    domain; keyed by username, which the user cap bounds. *)

let env_positive_int ~default name =
  match Sys.getenv_opt name with
  | None -> default
  | Some s ->
      (match int_of_string_opt s with
       | Some n when n > 0 -> n
       | _ -> default)

(** Instance cap on non-agent accounts (default 100, env PP_MAX_USERS). *)
let default_max_users () : int = env_positive_int ~default:100 "PP_MAX_USERS"

type limits = {
  writes_per_min : int;  (* mutating requests per 60s window, per user *)
  media_bytes : int;     (* summed media_blobs.byte_size cap, per author *)
}

(** Built-in tenant defaults: 120 writes/min, 64 MiB media. *)
let default_tenant () : limits =
  { writes_per_min = env_positive_int ~default:120 "PP_TENANT_WRITES_PER_MIN"
  ; media_bytes =
      1024 * 1024
      * env_positive_int ~default:64 "PP_TENANT_MEDIA_MB" }

(** Apply parsed profile directives; later same-key directives win.
    (Profile owns the @limit grammar; this only folds values.) *)
let apply_directives (base : limits) (ds : (string * string * int) list) :
    limits =
  List.fold_left
    (fun acc (scope, key, value) ->
      match (scope, key) with
      | "tenant", "writes-per-min" -> { acc with writes_per_min = value }
      | "tenant", "media-mb" -> { acc with media_bytes = 1024 * 1024 * value }
      | "instance", "max-users" -> acc (* consumed by Profile.max_users *)
      | _ -> acc (* unreachable: Profile validates scope/key *) )
    base ds

(* ---- write rate limit (fixed 60s window, per username) ---- *)

let write_counters : (string, int * int) Hashtbl.t = Hashtbl.create 64

(** Allow one mutating request for [username] inside the current 60s
    window if the window total stays under [limit]. Admitted requests
    count; denied requests do not. limit <= 0 disables the cap. *)
let allow_write ~(username : string) ~(limit : int) ~(now : float) : bool =
  if limit <= 0 then true
  else begin
    if Hashtbl.length write_counters > 1024 then Hashtbl.reset write_counters;
    let bucket = int_of_float (now /. 60.0) in
    match Hashtbl.find_opt write_counters username with
    | Some (b, n) when b = bucket ->
        if n >= limit then false
        else begin
          Hashtbl.replace write_counters username (b, n + 1);
          true
        end
    | _ ->
        Hashtbl.replace write_counters username (bucket, 1);
        true
  end

(* ---- media storage quota decision ---- *)

(** Quota decision for one upload: [used] is the author's current
    summed blob bytes, [incoming] the new blob. Zero-byte writes are
    always allowed; at or over the cap denies with a readable reason. *)
let media_ok ~(used : int) ~(incoming : int) ~(limit : int) :
    (unit, int * int * int) result =
  if incoming <= 0 then Ok ()
  else if used + incoming > limit then
    Error (used, incoming, limit)
  else Ok ()

let quota_message (used, incoming, limit) : string =
  Printf.sprintf "media quota exceeded: %d stored + %d incoming > %d cap"
    used incoming limit
