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
    with an LRU if public signup appears). *)

module Manifest = Pricklypear_common.Manifest

type t = {
  cfg : Store.config;
  mu : Eio.Mutex.t;
  sessions : (string, Session.t) Hashtbl.t;
  boot : Session.t;
}

let create ~(boot : Session.t) : t =
  {
    cfg = Store.config boot.Session.store;
    mu = Eio.Mutex.create ();
    sessions = Hashtbl.create 8;
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

let make_user_session (t : t) (user : Auth_store.user) : Session.t =
  (* Start on system/main; attach_identity with checkout_personal_now
     checks out the identity's home branch (personal / agent) and
     rehydrates onto it. *)
  let manifest =
    Manifest.make ~user_id:user.Auth_store.id ~username:user.username
      ~branch_id:Store.system_main_id ~branch_name:"system/main"
      ~boot_mode:t.boot.Session.manifest.Manifest.boot_mode
      ~image_version:t.boot.Session.manifest.Manifest.image_version ()
  in
  let store = fresh_store t ~branch_id:Store.system_main_id in
  let s = Session.make ~manifest ~store () in
  (* Fresh session: force the home-branch checkout now (personal for
     humans, <parent>/agent for agents). attach_identity handles both. *)
  Session.attach_identity s ~user_id:user.Auth_store.id
    ~username:user.Auth_store.username ~is_admin:user.Auth_store.is_admin
    ~auth_token:None ~checkout_personal_now:true
    ~user_type:user.Auth_store.user_type
    ~parent_user_id:user.Auth_store.parent_user_id ();
  s

(** Get-or-create the session for [key] (serialized — creation opens a
    pgx connection; rare path only). *)
let get_or_create (t : t) (key : string) (make : unit -> Session.t)
  : Session.t =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
      match Hashtbl.find_opt t.sessions key with
      | Some s -> s
      | None ->
          let s = make () in
          Hashtbl.replace t.sessions key s;
          s)

(** Select the session for this request's identity. Pure selection:
    no branch movement, no cross-identity mutation. [anon] picks the
    parked anonymous session; a resolved user picks their own. The
    caller still runs attach_from_user / attach_anonymous on the
    returned session (cheap no-op when the identity matches) and
    [Session.ensure_fresh] for the staleness stamp. *)
let select (t : t) ~(user : Auth_store.user option) : Session.t =
  match user with
  | None -> get_or_create t "anonymous" (fun () -> make_anonymous_session t)
  | Some u -> get_or_create t u.Auth_store.username (fun () -> make_user_session t u)
