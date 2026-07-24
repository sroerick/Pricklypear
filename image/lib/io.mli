(** I/O port for the image (H2).

    Domain code (session/crud/store queries eventually) should depend on
    this module type rather than [Lwt]/[pgx_lwt]/[Lwt_unix] directly.
    [Io_lwt] is the v1 implementation; a future [Io_eio] is H3.

    Start minimal — Db only. Grow from real call sites (do not
    speculative-abstract HTTP). See docs/runtime-effects-transition.md H2. *)

module type DB = sig
  (** Opaque connection handle (pgx connection under Io_lwt). *)
  type conn

  (** Store handle carrying pool conn + dynamic tx binding + branch_id. *)
  type store

  type config = {
    database : string;
    host : string;
    port : int;
    user : string;
    password : string;
  }

  val config_from_env : unit -> config
  val make : cfg:config -> branch_id:string -> unit -> store
  val connect : store -> unit Lwt.t
  val current_conn : store -> conn option
  (** True when inside (with-transaction ...). *)
  val in_transaction : store -> bool
  val branch_id : store -> string
  val set_branch_id : store -> string -> unit

  (** Execute SQL on [conn]. Params are pgx values. *)
  val execute :
    conn -> ?params:Pgx.row -> string -> Pgx.row list Lwt.t

  (** SERIALIZABLE (default) transaction; [f] sees upgraded conn. *)
  val with_transaction :
    ?isolation:Pgx.Isolation.t ->
    store ->
    (conn option -> 'a Lwt.t) ->
    'a Lwt.t

  (** Session-scoped dynamic tx for (with-transaction ...) Lisp prim. *)
  val with_dynamic_transaction :
    ?isolation:Pgx.Isolation.t ->
    store ->
    (unit -> 'a Lwt.t) ->
    'a Lwt.t
end

module type S = sig
  module Db : DB
end
