(** Pricklypear Image — live session presence (product-experience.borg
    §activity-ledger READ, "The console ships as a filmstrip strip: live
    sessions from Session_registry").

    Session_registry owns one [Session.t] per attached identity, but the
    registry is not reachable from the prim layer (dependency cycle:
    session_registry -> session -> prims -> prims_ops). Presence is the
    small, side-effect-free bridge: the HTTP request funnel and the
    anonymous/identity attach path [touch] an in-process map keyed by
    identity, and the operator console reads it. It stores NO content —
    username, branch, admin flag, last-seen timestamp only — so it is
    telemetry, not a tree datum (031 rule).

    [touch] is a mutex + map write, ZERO DB work; it is safe on the
    request path. [snapshot] is read-only and meant for the admin
    console. *)

type who = {
  username : string;
  user_id : string;
  branch_id : string;
  branch_name : string;
  is_admin : bool;
  last_seen : float;
}

let mu : Mutex.t = Mutex.create ()
let table : (string, who) Hashtbl.t = Hashtbl.create 16

(** Record that [username] was active now. [identity] is the map key —
    the manifest username (delegated sessions key the OWNER, matching
    Session_registry's "<owner>@by:<actor>" slot only when actor matters;
    presence keeps it simple: one row per authenticated username). *)
let touch ~(username : string) ~(user_id : string) ~(branch_id : string)
    ~(branch_name : string) ~(is_admin : bool) () : unit =
  if username = "" || username = "anonymous" || username = "public" then ()
  else begin
    Mutex.lock mu;
    (try
       Hashtbl.replace table username
         { username; user_id; branch_id; branch_name; is_admin;
           last_seen = Unix.gettimeofday () }
     with ex -> Mutex.unlock mu; raise ex);
    Mutex.unlock mu
  end

(** Drop an identity from presence (logout / revoke-all is best-effort;
    presence also ages out stale rows on snapshot). *)
let forget ~(username : string) () : unit =
  Mutex.lock mu;
  (try Hashtbl.remove table username with ex -> Mutex.unlock mu; raise ex);
  Mutex.unlock mu

(** Rows seen within [window] seconds, newest first. Stale rows are
    evicted here so a crashed connection does not linger forever. *)
let snapshot ?(window = 900.0) () : who list =
  let now = Unix.gettimeofday () in
  Mutex.lock mu;
  let rows =
    try
      let stale = ref [] in
      Hashtbl.iter (fun k v -> if now -. v.last_seen > window then stale := k :: !stale) table;
      List.iter (Hashtbl.remove table) !stale;
      Hashtbl.fold (fun _ v acc -> v :: acc) table []
    with ex -> Mutex.unlock mu; raise ex
  in
  Mutex.unlock mu;
  List.sort (fun a b -> compare b.last_seen a.last_seen) rows

(** Test/reset hook. *)
let clear () : unit =
  Mutex.lock mu;
  (try Hashtbl.reset table with ex -> Mutex.unlock mu; raise ex);
  Mutex.unlock mu

(** Live API tokens for one identity (the HOLD verb's revoke-token target).
    Read-only; [Auth_store.list_tokens] is owner-scoped by the caller's
    user id. Best-effort (DB down -> []). *)
let tokens_of ~(store : Store.t) ~(user_id : string) : Yojson.Safe.t list =
  if user_id = "" then []
  else
    List.map
      (fun (m : Auth_store.token_meta) ->
        `Assoc
          [ ("id", `String m.id);
            ("label", `String (Option.value m.label ~default:""));
            ("last4", `String m.token_last4);
            ("created_at", `String m.created_at);
            ("last_used_at",
             `String (Option.value m.last_used_at ~default:"")) ])
      (Auth_store.list_tokens store ~user_id)

(** In-flight + recent jobs for one identity (the HOLD verb's cancel-job
    target). [Jobs.list] renders VStr JSON rows; parse them back.
    Best-effort. *)
let projects_of ~(store : Store.t) ~(username : string) ~(is_admin : bool)
    () : Yojson.Safe.t list =
  match
    Jobs.list ~store ~caller:username ~is_admin ~limit:25 ()
  with
  | Pricklypear_lisp.Ast.VList rows ->
      List.filter_map
        (function
          | Pricklypear_lisp.Ast.VStr s -> (
              try Some (Yojson.Safe.from_string s) with _ -> None)
          | _ -> None)
        rows
  | _ -> []

(** Auth provenance: the identity's last logins (auth_log read face, the
    operator fast-path's "when did each identity last authenticate, from
    where"). Read-only, admin-gated by the caller. Best-effort. *)
let last_auth_of ~(store : Store.t) ~(user_id : string)
    ?(limit = 5) () : Yojson.Safe.t list =
  match Io_db.Db.current_conn store with
  | None -> []
  | Some conn ->
      let sql =
        "SELECT kind, success, source_ip::text, happened_at::text \
         FROM auth_log WHERE user_id = $1::uuid \
         ORDER BY happened_at DESC LIMIT $2"
      in
      (try
         Store_db.exec conn
           ~params:[ Pgx.Value.of_string user_id; Pgx.Value.of_int limit ]
           sql
         |> List.filter_map (function
              | [ kind; success; ip; at ] ->
                  let s v =
                    match Pgx.Value.to_string v with Some x -> x | None -> ""
                  in
                  let ok =
                    match Pgx.Value.to_bool success with
                    | Some b -> b
                    | None -> false
                  in
                  Some
                    (`Assoc
                       [ ("kind", `String (s kind));
                         ("ok", `Bool ok);
                         ("source_ip", `String (s ip));
                         ("at", `String (s at)) ])
              | _ -> None)
       with exn ->
         Printf.eprintf "[presence] last_auth failed: %s\n%!"
           (Printexc.to_string exn);
         [])
