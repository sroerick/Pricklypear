(** Db port name kept for call-site stability (H2.2).

    Direct-style: delegates to [Store_db]. Process runs under Eio_main;
    Lwt backend (if PP_IO=lwt) blocks via Lwt_eio inside Store_db. *)

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
  let in_transaction = Store_db.in_transaction
  let branch_id (s : store) = s.Store_db.branch_id
  let set_branch_id (s : store) id = s.Store_db.branch_id <- id

  let with_transaction ?isolation store f =
    Store_db.with_transaction ?isolation store f

  let with_transaction_low store f = Store_db.with_transaction_low store f

  let with_dynamic_transaction ?isolation store f =
    Store_db.with_dynamic_transaction ?isolation store f
end
