(** Pricklypear Image — Store connection, config, transactions (Eio floor).

    Domain-facing API returns plain ['a] (blocks the calling Eio fiber).
    Single backend: Io_eio.Pg (pure pgx over Eio.Net). Process always runs
    under Eio_main (image/bin/main.ml). *)

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

(** Connection handle + mutex: identity-monad pgx only locks per SQL op, so
    concurrent Httpun_eio fibers would interleave BEGIN/SELECT on one wire.
    All SQL (exec + whole transactions) takes this lock; same-fiber re-entry
    is allowed via [eio_lock_depth_key] so a tx body can exec. *)
type conn = Io_eio.Pg.t * Eio.Mutex.t

let eio_lock_depth_key : int Eio.Fiber.key = Eio.Fiber.create_key ()

let with_eio_conn_lock (mu : Eio.Mutex.t) (f : unit -> 'a) : 'a =
  match Eio.Fiber.get eio_lock_depth_key with
  | Some d when d > 0 ->
      Eio.Fiber.with_binding eio_lock_depth_key (d + 1) f
  | _ ->
      Eio.Mutex.lock mu;
      (match
         Eio.Fiber.with_binding eio_lock_depth_key 1 (fun () ->
             Eio.Cancel.protect f)
       with
       | v ->
           Eio.Mutex.unlock mu;
           v
       | exception ex ->
           Eio.Mutex.unlock mu;
           raise ex)

type t = {
  cfg : config;
  mutable conn : conn option;
  mutable branch_id : string;
  mutable current_tx : conn option;
}

let make ~cfg ~branch_id () : t =
  { cfg; conn = None; branch_id; current_tx = None }

let conn_of (t : t) : conn option = t.conn

let current_conn (t : t) : conn option =
  match t.current_tx with Some tx -> Some tx | None -> t.conn

let in_transaction (t : t) : bool = t.current_tx <> None

let exec ((pg, mu) : conn) ?params sql : Pgx.row list =
  with_eio_conn_lock mu (fun () ->
      match params with
      | None -> Io_eio.Pg.execute pg sql
      | Some params -> Io_eio.Pg.execute ~params pg sql)

let with_transaction ?(isolation = Pgx.Isolation.Serializable) (t : t)
    (f : conn option -> 'a) : 'a =
  match t.conn with
  | None -> f None
  | Some (pg, mu) ->
      with_eio_conn_lock mu (fun () ->
          Io_eio.Pg.with_transaction ~isolation pg (fun tx ->
              f (Some (tx, mu))))

let with_dynamic_transaction ?(isolation = Pgx.Isolation.Serializable)
    (t : t) (f : unit -> 'a) : 'a =
  match t.conn with
  | None -> f ()
  | Some (pg, mu) ->
      with_eio_conn_lock mu (fun () ->
          if t.current_tx <> None then
            raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.User_error
                    "with-transaction: nested transactions not supported"))
          else
            Io_eio.Pg.with_transaction ~isolation pg (fun tx_conn ->
                t.current_tx <- Some (tx_conn, mu);
                Fun.protect
                  ~finally:(fun () -> t.current_tx <- None)
                  f))

let connect (t : t) : unit =
  match t.conn with
  | Some _ -> ()
  | None -> (
      try
        let c =
          Io_eio.Pg.connect ~host:t.cfg.host ~port:t.cfg.port
            ~user:t.cfg.user ~password:t.cfg.password
            ~database:t.cfg.database ()
        in
        let mu = Eio.Mutex.create () in
        t.conn <- Some (c, mu);
        Printf.eprintf "[store] connected (eio) to PG %s:%d/%s as %s\n%!"
          t.cfg.host t.cfg.port t.cfg.database t.cfg.user
      with exn ->
        Printf.eprintf "[store] PG connect failed (eio): %s\n%!"
          (Printexc.to_string exn))

let health_check (t : t) : (unit, string) result =
  match current_conn t with
  | None -> Error "no database connection"
  | Some conn -> (
      try
        match exec conn "SELECT 1" with
        | _ :: _ -> Ok ()
        | [] -> Error "SELECT 1 returned no rows"
      with exn -> Error (Printexc.to_string exn))

(** @deprecated compatibility alias — was Pg_lwt. *)
module Pg = Io_eio.Pg
