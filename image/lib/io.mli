(** I/O port for the image (H2/H3).

    Domain code depends on this module type rather than [Lwt]/[pgx_lwt]
    directly. After the Eio-floor flip, APIs are direct-style (plain ['a]).
    [Io_lwt.Db] remains the name domain code calls; it delegates to
    [Store_db] (dual backend). See docs/runtime-effects-transition.md. *)

module type DB = sig
  (** Opaque connection handle. *)
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
  val connect : store -> unit
  val current_conn : store -> conn option
  (** True when inside (with-transaction ...). *)
  val in_transaction : store -> bool
  val branch_id : store -> string
  val set_branch_id : store -> string -> unit

  (** Execute SQL on [conn]. Params are pgx values. Blocks the fiber. *)
  val execute : conn -> ?params:Pgx.row -> string -> Pgx.row list

  (** SERIALIZABLE (default) transaction; [f] sees upgraded conn. *)
  val with_transaction :
    ?isolation:Pgx.Isolation.t ->
    store ->
    (conn option -> 'a) ->
    'a

  (** Session-scoped dynamic tx for (with-transaction ...) Lisp prim. *)
  val with_dynamic_transaction :
    ?isolation:Pgx.Isolation.t ->
    store ->
    (unit -> 'a) ->
    'a
end

module type S = sig
  module Db : DB
end
