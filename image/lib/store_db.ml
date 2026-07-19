(** Pricklypear Image — Store connection, config, transactions.

    H3 dual-run: [conn] is either pgx_lwt or Io_eio.Pg. [PP_IO=eio] selects
    the Eio backend at [make] time; default remains Lwt. Query APIs live in
    [Store] (includes this module). *)
open Lwt.Syntax

(* A [Pgx_lwt.Io_intf.S] implementation backed by [Lwt_unix]. *)
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

module Pg_lwt = Pgx_lwt.Make (Io_unix)

(** @deprecated Prefer [Pg_lwt] / dual [conn]. Kept so older references
    to [Store.Pg] still resolve for type errors that should migrate. *)
module Pg = Pg_lwt

type config = {
  database : string;
  host : string;
  port : int;
  user : string;
  password : string;
}

type backend = Lwt_backend | Eio_backend

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft

let backend_from_env () : backend =
  match Sys.getenv_opt "PP_IO" with
  | Some s ->
      (match String.lowercase_ascii (String.trim s) with
       | "eio" -> Eio_backend
       | _ -> Lwt_backend)
  | None -> Lwt_backend

let config_from_env () : config =
  {
    database = env_or "PP_DB_NAME" (env_or "PGDATABASE" "pricklypear");
    host = env_or "PP_DB_HOST" (env_or "PGHOST" "127.0.0.1");
    port = int_of_string (env_or "PP_DB_PORT" (env_or "PGPORT" "5433"));
    user = env_or "PP_DB_USER" (env_or "PGUSER" "pricklypear_image");
    password = env_or "PP_DB_PASS" "";
  }

(** Opaque dual connection handle. *)
type conn =
  | Lwt_conn of Pg_lwt.t
  | Eio_conn of Io_eio.Pg.t

type t = {
  cfg : config;
  backend : backend;
  mutable conn : conn option;
  mutable branch_id : string;
  mutable current_tx : conn option;
}

let make ~cfg ~branch_id () : t =
  {
    cfg;
    backend = backend_from_env ();
    conn = None;
    branch_id;
    current_tx = None;
  }

let backend (t : t) = t.backend

let conn_of (t : t) : conn option = t.conn

let current_conn (t : t) : conn option =
  match t.current_tx with
  | Some tx -> Some tx
  | None -> t.conn

(** Execute SQL on a dual [conn]. Shared by store/crud/auth. *)
let exec (c : conn) ?params sql : Pgx.row list Lwt.t =
  match c with
  | Lwt_conn pg ->
      (match params with
       | None -> Pg_lwt.execute pg sql
       | Some params -> Pg_lwt.execute pg ~params sql)
  | Eio_conn pg ->
      Lwt_eio.run_eio (fun () ->
          match params with
          | None -> Io_eio.Pg.execute pg sql
          | Some params -> Io_eio.Pg.execute ~params pg sql)

let with_transaction ?(isolation = Pgx.Isolation.Serializable)
    (t : t) (f : conn option -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> f None
  | Some (Lwt_conn pg) ->
      Pg_lwt.with_transaction ~isolation pg (fun tx ->
          f (Some (Lwt_conn tx)))
  | Some (Eio_conn pg) ->
      Lwt_eio.run_eio (fun () ->
          Io_eio.Pg.with_transaction ~isolation pg (fun tx ->
              Lwt_eio.run_lwt (fun () -> f (Some (Eio_conn tx)))))

let with_dynamic_transaction
    ?(isolation = Pgx.Isolation.Serializable) (t : t)
    (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  match t.conn with
  | None -> f ()
  | Some (Lwt_conn pg) ->
      if t.current_tx <> None then
        Lwt.fail
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.User_error
                "with-transaction: nested transactions not supported"))
      else
        Pg_lwt.with_transaction ~isolation pg (fun tx_conn ->
            t.current_tx <- Some (Lwt_conn tx_conn);
            Lwt.finalize f (fun () ->
                t.current_tx <- None;
                Lwt.return_unit))
  | Some (Eio_conn pg) ->
      if t.current_tx <> None then
        Lwt.fail
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.User_error
                "with-transaction: nested transactions not supported"))
      else
        Lwt_eio.run_eio (fun () ->
            Io_eio.Pg.with_transaction ~isolation pg (fun tx_conn ->
                t.current_tx <- Some (Eio_conn tx_conn);
                Fun.protect
                  ~finally:(fun () -> t.current_tx <- None)
                  (fun () -> Lwt_eio.run_lwt f)))

let connect (t : t) : unit Lwt.t =
  match t.conn with
  | Some _ -> Lwt.return_unit
  | None ->
      (match t.backend with
       | Lwt_backend ->
           Lwt.catch
             (fun () ->
               let* c =
                 Pg_lwt.connect
                   ~host:t.cfg.host ~port:t.cfg.port
                   ~user:t.cfg.user ~password:t.cfg.password
                   ~database:t.cfg.database ()
               in
               t.conn <- Some (Lwt_conn c);
               Printf.eprintf
                 "[store] connected (lwt) to PG %s:%d/%s as %s\n%!"
                 t.cfg.host t.cfg.port t.cfg.database t.cfg.user;
               Lwt.return_unit)
             (fun exn ->
               Printf.eprintf "[store] PG connect failed (lwt): %s\n%!"
                 (Printexc.to_string exn);
               Lwt.return_unit)
       | Eio_backend ->
           Lwt.catch
             (fun () ->
               Lwt_eio.run_eio (fun () ->
                   let c =
                     Io_eio.Pg.connect
                       ~host:t.cfg.host ~port:t.cfg.port
                       ~user:t.cfg.user ~password:t.cfg.password
                       ~database:t.cfg.database ()
                   in
                   t.conn <- Some (Eio_conn c);
                   Printf.eprintf
                     "[store] connected (eio) to PG %s:%d/%s as %s\n%!"
                     t.cfg.host t.cfg.port t.cfg.database t.cfg.user))
             (fun exn ->
               Printf.eprintf "[store] PG connect failed (eio): %s\n%!"
                 (Printexc.to_string exn);
               Lwt.return_unit))
