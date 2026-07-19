(** Lwt + pgx_lwt implementation of the Io.Db port (H2.2).

    Allowed to call [Store_db]/[Pgx_lwt]/[Lwt_unix] directly.
    Domain modules migrate onto [Io_lwt.Db] in H2.3+ (crud/store/auth). *)

open Lwt.Syntax

module Db = struct
  type conn = Store_db.Pg.t
  type store = Store_db.t

  type config = Store_db.config = {
    database : string;
    host : string;
    port : int;
    user : string;
    password : string;
  }

  let config_from_env = Store_db.config_from_env
  let make = Store_db.make
  let connect = Store_db.connect
  let current_conn = Store_db.current_conn
  let branch_id (s : store) = s.Store_db.branch_id
  let set_branch_id (s : store) id = s.Store_db.branch_id <- id

  let execute (conn : conn) ?params sql =
    match params with
    | None -> Store_db.Pg.execute conn sql
    | Some params -> Store_db.Pg.execute conn ~params sql

  let with_transaction ?isolation store f =
    Store_db.with_transaction ?isolation store f

  let with_dynamic_transaction ?isolation store f =
    Store_db.with_dynamic_transaction ?isolation store f
end
