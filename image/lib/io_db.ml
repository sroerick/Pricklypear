(** The domain-side DB port (H2.2 name: was [Io_lwt]; renamed at
    eio-floor Phase 5 — the name no longer implies any backend).

    Direct-style: delegates to [Store_db] (the single Eio backend:
    pure pgx over Eio.Net via [Io_eio]). *)

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
