(** Pricklypear Image — per-identity session registry
    (image.per-identity-sessions).

    One [Session.t] per attached identity instead of one shared
    session that every request flips to its own branch:

    - the anonymous session serves all unauthenticated traffic, parked
      on system/main;
    - each authenticated username gets its own session, built lazily
      on first attach, parked on that identity's branch (personal for
      humans, <parent>/agent for agents), with its OWN Store.t (own
      pgx connection — checkout never moves another identity's branch);
    - attach = select (or create) + a cheap staleness stamp check.
      Attach never hydrates an unchanged session
      (image.boot-request-cost) and never mutates another identity's
      session.

    Sessions are never expired in v1 (user count is small; revisit
    with an LRU if public signup appears).

    BORGE-1788334666: [get_or_create] is SINGLE-FLIGHT — the fast
    path takes the mutex only for the tables lookup; session
    construction (pgx connect + rehydrate) happens OUTSIDE the
    mutex, and concurrent first-touch of one identity waits on a
    one-shot promise instead of duplicating a session (each session
    owns a pgx connection). The previous shape held the registry
    mutex across all of [Session.make] — which at the time also ran
    a full libs seed_stdlib — so every identity's first attach after
    a restart stalled ALL HTTP for the seed's duration. *)

module Manifest = Pricklypear_common.Manifest

type t = {
  cfg : Store_db.config;
  mu : Eio.Mutex.t;
  sessions : (string, Session.t) Hashtbl.t;
  creating : (string, unit Eio.Promise.t * unit Eio.Promise.u) Hashtbl.t;
  boot : Session.t;
}

let create ~(boot : Session.t) : t =
  {
    cfg = Store.config boot.Session.store;
    mu = Eio.Mutex.create ();
    sessions = Hashtbl.create 8;
    creating = Hashtbl.create 8;
    boot;
  }

(** The boot session keeps its jobs (REPL endpoint, seed); the auth
    lookups in resolve_identity also use its store. *)
let boot_session (t : t) : Session.t = t.boot

let fresh_store (t : t) ~(branch_id : string) : Store.t =
  let store = Store.make ~cfg:t.cfg ~branch_id () in
  Store.connect store;
  store

let make_anonymous_session (t : t) : Session.t =
  let manifest =
    Manifest.make ~user_id:"anonymous" ~username:"anonymous"
      ~branch_id:Store.system_main_id ~branch_name:"system/main"
      ~boot_mode:t.boot.Session.manifest.Manifest.boot_mode
      ~image_version:t.boot.Session.manifest.Manifest.image_version ()
  in
  let store = fresh_store t ~branch_id:Store.system_main_id in
  let s = Session.make ~manifest ~store () in
  Session.rehydrate s;
  s

let make_user_session (t : t) ~(actor : string option)
    (user : Auth_store.user) : Session.t =
  (* spec_delegation (auth.delegation-tokens): when [actor] is Some, this
     session is carried by a delegated on-behalf-of token — manifest
     username is still the OWNER (whoami = owner), but the session is
     data-only: delegated=true (session.ml gates: no code persistence,
     seeded-only load-library), is_admin forced false in attach_identity,
     agent_branch_lock set. The owner's OWN session (actor=None) keeps
     its full authorized surface; the two never share a registry slot
     (keyed user@by:actor vs user in [select]). *)
  let delegated = actor <> None in
  let manifest =
    Manifest.make ~user_id:user.Auth_store.id ~username:user.username
      ~branch_id:Store.system_main_id ~branch_name:"system/main"
      ~boot_mode:t.boot.Session.manifest.Manifest.boot_mode
      ~image_version:t.boot.Session.manifest.Manifest.image_version
      ~actor ()
  in
  let store = fresh_store t ~branch_id:Store.system_main_id in
  let s = Session.make ~manifest ~store () in
  (* Fresh session: force the home-branch checkout now (personal for
     humans, <parent>/agent for agents). attach_identity handles both. *)
  Session.attach_identity s ~user_id:user.Auth_store.id
    ~username:user.Auth_store.username ~is_admin:user.Auth_store.is_admin
    ~auth_token:None ~checkout_personal_now:true
    ~user_type:user.Auth_store.user_type
    ~parent_user_id:user.Auth_store.parent_user_id
    ~delegated ~actor ();
  s

(** Get-or-create the session for [key]. Lookup is mutex-guarded;
    construction is single-flight per key but OUTSIDE the mutex.
    A creator that raises drops its in-flight mark so the next waiter
    retries as the new creator (no livelock). *)
let get_or_create (t : t) (key : string) (make : unit -> Session.t)
  : Session.t =
  let rec loop () =
    let decision =
      Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
          match Hashtbl.find_opt t.sessions key with
          | Some s -> `Ready s
          | None ->
              (match Hashtbl.find_opt t.creating key with
               | Some (p, _) -> `Wait p
               | None ->
                   let p, r = Eio.Promise.create () in
                   Hashtbl.replace t.creating key (p, r);
                   `Create (p, r)))
    in
    match decision with
    | `Ready s -> s
    | `Wait p ->
        Eio.Promise.await p;
        loop ()
    | `Create (p, r) ->
        (match
           let s = make () in
           Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
               Hashtbl.replace t.sessions key s;
               Hashtbl.remove t.creating key);
           Eio.Promise.resolve r ();
           s
         with
         | s -> s
         | exception exn ->
             Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
                 Hashtbl.remove t.creating key);
             if not (Eio.Promise.is_resolved p) then
               Eio.Promise.resolve r ();
             raise exn)
  in
  loop ()

(** Select the session for this request's identity. Pure selection:
    no branch movement, no cross-identity mutation. [anon] picks the
    parked anonymous session; a resolved user picks their own — keyed
    by "<owner>@by:<actor>" when the identity was carried by a delegated
    token (auth.delegation-tokens) so the delegated data-only session
    NEVER collides with the owner's own full-privilege session. The
    caller still runs attach_from_user / attach_anonymous on the
    returned session (cheap no-op when the identity matches) and
    [Session.ensure_fresh] for the staleness stamp. *)
let select (t : t) ~(user : Auth_store.user option)
    ~(actor : string option) : Session.t =
  let key =
    match user, actor with
    | None, _ -> "anonymous"
    | Some u, None -> u.Auth_store.username
    | Some u, Some a -> Printf.sprintf "%s@by:%s" u.Auth_store.username a
  in
  get_or_create t key
    (fun () ->
       match user with
       | None -> make_anonymous_session t
       | Some u -> make_user_session t ~actor u)
