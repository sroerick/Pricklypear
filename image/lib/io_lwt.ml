(** Lwt-facing Db port (H2.2 / H3 dual-run).

    Delegates to [Store_db], which holds either a pgx_lwt or Io_eio
    connection depending on [PP_IO]. Domain code keeps calling
    [Io_lwt.Db.*]; backend selection is inside [Store_db]. *)

module Db = struct
  type conn = Store_db.conn
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
    Store_db.exec conn ?params sql

  let with_transaction ?isolation store f =
    Store_db.with_transaction ?isolation store f

  let with_dynamic_transaction ?isolation store f =
    Store_db.with_dynamic_transaction ?isolation store f
end
