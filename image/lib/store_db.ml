(** Pricklypear Image — Store connection, config, transactions.
    Query/branch APIs live in [Store] (includes this module). *)
open Lwt.Syntax

(* A [Pgx_lwt.Io_intf.S] implementation backed by [Lwt_unix]. Pgx_lwt's
   Make functor requires this; it provides the low-level read/write/flush
   primitives over a TCP or Unix-domain socket. *)
module Io_unix = struct
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  type sockaddr =
    | Unix of string
    | Inet of string * int

  let output_char = Lwt_io.write_char
  let output_string oc s = Lwt_io.write oc s
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let really_input ic buf pos len = Lwt_io.read_into_exactly ic buf pos len
  let close_in = Lwt_io.close
  let getlogin () = Lwt.return "pricklypear"
  let open_connection = function
    | Unix path ->
        let sock =
          Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
        let* () = Lwt_unix.connect sock (Lwt_unix.ADDR_UNIX path) in
        Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Input sock,
                    Lwt_io.of_fd ~mode:Lwt_io.Output sock)
    | Inet (host, port) ->
        let sock =
          Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
        let* () = Lwt_unix.connect sock addr in
        Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Input sock,
                    Lwt_io.of_fd ~mode:Lwt_io.Output sock)
end

(* Concrete pgx connection module, built with our Lwt_unix Io. *)
module Pg = Pgx_lwt.Make (Io_unix)

type config = {
  database : string;
  host : string;
  port : int;
  user : string;
  password : string;
}

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft

let config_from_env () : config =
  {
    database = env_or "PP_DB_NAME" (env_or "PGDATABASE" "pricklypear");
    host = env_or "PP_DB_HOST" (env_or "PGHOST" "127.0.0.1");
    port = int_of_string (env_or "PP_DB_PORT" (env_or "PGPORT" "5433"));
    user = env_or "PP_DB_USER" (env_or "PGUSER" "pricklypear_image");
    password = env_or "PP_DB_PASS" "";
  }

type t = {
  cfg : config;
  mutable conn : Pg.t option;
  mutable branch_id : string;  (* (checkout 'branch) reassigns it *)
  (* Session-level dynamic transaction binding (option A from
     BORGE-1783438027, lands it29). Points to the transaction-scoped
     pgx connection while inside (with-transaction ...), so all DB
     prims called from within the body see the SAME connection (and
     thus the same transaction — including row locks acquired by
     select-for-update). None when not inside a transaction, in
     which case prims fall back to [conn_of] (the default pool conn). *)
  mutable current_tx : Pg.t option;
}

let make ~cfg ~branch_id () : t = { cfg; conn = None; branch_id; current_tx = None }

(* Expose the optional pool connection to sibling modules. [conn_of]
   always returns the long-lived pool connection; prims should prefer
     [current_conn t]
   which returns the in-transaction connection (if set) or falls
   back to the pool conn. *)
let conn_of (t : t) : Pg.t option = t.conn

(* Active connection for DB prims to use: in-transaction connection if
   we're inside a (with-transaction ...) body, else the default pool
   connection. Returns None when there's no DB at all. *)
let current_conn (t : t) : Pg.t option =
  match t.current_tx with
  | Some tx -> Some tx
  | None -> t.conn

(* Run [f] inside a Postgres transaction with SERIALIZABLE isolation
   (per AGENTS.md / spec [storage.transactions]: SERIALIZABLE by
   default for mutations). [f] receives the upgraded transactional
   connection (pgx returns a new `t` representing the in-transaction
   state); all execute calls against that connection participate in the
   transaction.

   If [f] raises, pgx automatically ROLLS BACK and propagates the
   exception to with_transaction's caller. Per pgx's mli it's an error
   to call commit/rollback manually inside [f].

   On no-DB (t.conn = None), still calls [f] with None so the caller
   can implement their no-DB fallback (rare case; stores without DB
   aren't writable).

   Resolves the carried-forward TODO on fork_branch and merge_branch.
   Both now run all their SQL statements as a SINGLE atomic
   transaction. *)
let with_transaction ?(isolation = Pgx.Isolation.Serializable)
    (t : t) (f : Pg.t option -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> f None
  | Some conn ->
      Pg.with_transaction ~isolation conn (fun tx_conn -> f (Some tx_conn))

(** Session-scoped dynamic transaction binding (option A from
    BORGE-1783438027, lands it29). Used by the (with-transaction ...)
    Lisp prim: sets [t.current_tx] to the transactional pgx connection
    BEFORE [f] runs, so DB prims called from within the body consult
    [current_conn] (which they already use) and see the SAME
    transaction — including row locks acquired via
    (select-for-update 'type id).

    The cleanup at the end is a [finally]-shaped try-with that clears
    [t.current_tx] whether [f] succeeds or raises. CRITICAL: the
    error handler retries the body's value-type raise AFTER restoring
    the [current_tx = None] invariant; pgx still sees the propagated
    exception via its own finally and ROLLS BACK the transaction.

    Nested transactions (a (with-transaction ...) inside another) are
    NOT supported — at the pgx level [Pg.with_transaction] on the
    in-transaction conn would error. We raise [Interp_error] if
    someone tries it. *)
let with_dynamic_transaction
    ?(isolation = Pgx.Isolation.Serializable) (t : t)
    (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> f ()
  | Some conn ->
      if t.current_tx <> None then
        Lwt.fail (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.User_error
                       "with-transaction: nested transactions not supported"))
      else
        Pg.with_transaction ~isolation conn
          (fun tx_conn ->
             t.current_tx <- Some tx_conn;
             Lwt.finalize f
               (fun () -> t.current_tx <- None; Lwt.return_unit))


let connect (t : t) : unit Lwt.t =
  match t.conn with
  | Some _ -> Lwt.return_unit
  | None ->
      Lwt.catch
        (fun () ->
          let* conn =
            Pg.connect
              ~host:t.cfg.host ~port:t.cfg.port
              ~user:t.cfg.user ~password:t.cfg.password
              ~database:t.cfg.database ()
          in
          t.conn <- Some conn;
          Printf.eprintf "[store] connected to PG %s:%d/%s as %s\n"
            t.cfg.host t.cfg.port t.cfg.database t.cfg.user;
          Lwt.return_unit)
        (fun exn ->
          Printf.eprintf "[store] PG connect failed: %s\n"
            (Printexc.to_string exn);
          Lwt.return_unit)

