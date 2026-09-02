(** Eio + pure pgx implementation of the Io.Db port (H3).

    Identity monad over Eio.Net flows. Domain still speaks [Lwt.t] via
    the [Io.DB] interface: each call is scheduled with [Lwt_eio.run_eio]
    when the process is under [Lwt_eio.with_event_loop], or runs
    directly inside [Eio_main.run] for the smoke binary.

    Ambient [ctx] (switch + net) must be installed with [with_ctx] before
    [connect]. *)

module Ctx = struct
  type t = {
    sw : Eio.Switch.t;
    net : [ `Generic ] Eio.Net.ty Eio.Std.r;
  }

  let current : t option ref = ref None

  let get () =
    match !current with
    | Some c -> c
    | None ->
        failwith
          "Io_eio: no ambient Eio context (call Io_eio.with_ctx under Eio_main)"

  let with_ctx (c : t) (f : unit -> 'a) : 'a =
    let prev = !current in
    current := Some c;
    Fun.protect ~finally:(fun () -> current := prev) f

  (** True when [with_ctx] is active (Eio_main path). *)
  let installed () = !current <> None
end

let with_ctx = Ctx.with_ctx
let ctx_installed = Ctx.installed

(** Fiber-local: true while this fiber is inside [Lwt_eio.run_eio] / an
    ambient Eio SQL section. A process-global ref is wrong under Httpun_eio
    (many fibers): fiber A in a tx yields at [run_lwt], fiber B would see
    depth>0 and skip [run_eio], racing the shared PG wire. *)
let eio_enter_key : bool Eio.Fiber.key = Eio.Fiber.create_key ()

let in_eio_enter () =
  match Eio.Fiber.get eio_enter_key with Some true -> true | _ -> false

(** Run [f] via [Lwt_eio.run_eio], or directly if this fiber is already in one. *)
let on_eio (f : unit -> 'a) : 'a Lwt.t =
  if in_eio_enter () then Lwt.return (f ())
  else
    Lwt_eio.run_eio (fun () ->
        Eio.Fiber.with_binding eio_enter_key true f)

(** Same as [on_eio] but for call sites already on an Eio fiber that need to
    mark the enter-key for nested SQL (e.g. whole-tx critical sections). *)
let with_eio_enter (f : unit -> 'a) : 'a =
  if in_eio_enter () then f ()
  else Eio.Fiber.with_binding eio_enter_key true f

(** Pgx Io backend: direct-style ('a t = 'a) over one Eio two-way flow. *)
module Pgx_io = struct
  type 'a t = 'a

  let return x = x
  let ( >>= ) v f = f v
  let catch f h = try f () with e -> h e

  type flow =
    [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

  type in_channel = flow
  type out_channel = flow

  type sockaddr =
    | Unix of string
    | Inet of string * int

  let open_connection sockaddr : flow * flow =
    let { Ctx.sw; net } = Ctx.get () in
    let flow =
      match sockaddr with
      | Unix path -> (Eio.Net.connect ~sw net (`Unix path) :> flow)
      | Inet (host, port) ->
          let service = string_of_int port in
          (match Eio.Net.getaddrinfo_stream net host ~service with
           | [] ->
               failwith ("Io_eio: getaddrinfo empty for " ^ host ^ ":" ^ service)
           | addr :: _ -> (Eio.Net.connect ~sw net addr :> flow))
    in
    (* BORGE-1788334666 follow-up (perf): pgx writes each wire message
       (Parse/Bind/Execute/Sync) as a separate small send. Without
       TCP_NODELAY, Nagle holds every segment after the first until the
       server's delayed ACK (~40 ms each), so EVERY statement pays
       ~120+ ms client-side while the server itself runs in
       microseconds. libpq sets TCP_NODELAY for exactly this reason;
       the raw Eio flow does not. Set it once at connect (best-effort —
       AF_UNIX sockets / exotic backends have no such option). *)
    (match Eio_unix.Resource.fd_opt flow with
     | Some fd ->
         (try
            Eio_unix.Fd.use_exn "pgx_tcp_nodelay" fd
              (fun u -> Unix.setsockopt u Unix.TCP_NODELAY true)
          with _ -> ())
     | None -> ());
    (flow, flow)

  type ssl_config
  let upgrade_ssl = `Not_supported

  let output_char (oc : out_channel) c =
    let b = Bytes.make 1 c in
    Eio.Flow.copy_string (Bytes.unsafe_to_string b) oc

  let output_string (oc : out_channel) s = Eio.Flow.copy_string s oc

  let output_binary_int (oc : out_channel) n =
    let chr = Char.chr in
    output_char oc (chr (n lsr 24));
    output_char oc (chr ((n lsr 16) land 255));
    output_char oc (chr ((n lsr 8) land 255));
    output_char oc (chr (n land 255))

  let flush (_oc : out_channel) = ()

  let input_char (ic : in_channel) =
    let buf = Cstruct.create 1 in
    Eio.Flow.read_exact ic buf;
    Char.chr (Cstruct.get_uint8 buf 0)

  let really_input (ic : in_channel) (b : Bytes.t) pos len =
    if len < 0 || pos < 0 || pos + len > Bytes.length b then
      invalid_arg "Io_eio.really_input";
    (* Must blit into [b]: Cstruct.of_bytes does not write through. *)
    let cs = Cstruct.create len in
    Eio.Flow.read_exact ic cs;
    Cstruct.blit_to_bytes cs 0 b pos len

  let input_binary_int (ic : in_channel) =
    let b = Bytes.create 4 in
    really_input ic b 0 4;
    let code = Char.code in
    (code (Bytes.get b 0) lsl 24)
    lor (code (Bytes.get b 1) lsl 16)
    lor (code (Bytes.get b 2) lsl 8)
    lor code (Bytes.get b 3)

  let close_in (ic : in_channel) = Eio.Flow.close ic

  let getlogin () =
    try
      let uid = Unix.getuid () in
      (Unix.getpwuid uid).Unix.pw_name
    with _ -> "pricklypear"

  let debug s = prerr_endline ("[io_eio] " ^ s)

  let protect f ~finally =
    match f () with
    | v ->
        finally ();
        v
    | exception e ->
        finally ();
        raise e

  module Sequencer = struct
    type 'a monad = 'a t
    type 'a t = {
      v : 'a;
      m : Eio.Mutex.t;
    }

    let create v = { v; m = Eio.Mutex.create () }

    (** Serialize [f] on the connection.

        MUST NOT use [Eio.Mutex.use_rw]: that permanently poisons the mutex
        when [f] raises (any SQL/Pgx error). A poisoned sequencer kills every
        subsequent DB op (auth included) while the accept loop stays up — the
        2026-07-22 login outage. Unlock-on-exception (use_ro style) plus
        [Cancel.protect] keeps the wire section non-cancellable without
        destroying the lock. image.http-robustness invariant 6. *)
    let enqueue t f =
      Eio.Mutex.lock t.m;
      match Eio.Cancel.protect (fun () -> f t.v) with
      | v ->
          Eio.Mutex.unlock t.m;
          v
      | exception ex ->
          Eio.Mutex.unlock t.m;
          raise ex
  end
end

module Pg = Pgx.Make (Pgx_io)

module Db = struct
  type conn = Pg.t

  type config = {
    database : string;
    host : string;
    port : int;
    user : string;
    password : string;
  }

  type store = {
    cfg : config;
    mutable conn : conn option;
    mutable branch_id : string;
    mutable current_tx : conn option;
  }

  let env_or name dft =
    match Sys.getenv_opt name with Some s -> s | None -> dft

  let config_from_env () : config =
    {
      database = env_or "PP_DB_NAME" (env_or "PGDATABASE" "pricklypear");
      host = env_or "PP_DB_HOST" (env_or "PGHOST" "127.0.0.1");
      port = int_of_string (env_or "PP_DB_PORT" (env_or "PGPORT" "5433"));
      user = env_or "PP_DB_USER" (env_or "PGUSER" "pricklypear_image");
      password = env_or "PP_DB_PASS" "";
    }

  let make ~cfg ~branch_id () : store =
    { cfg; conn = None; branch_id; current_tx = None }

  let branch_id (s : store) = s.branch_id
  let set_branch_id (s : store) id = s.branch_id <- id
  let current_conn (s : store) =
    match s.current_tx with Some tx -> Some tx | None -> s.conn
  let in_transaction (s : store) = s.current_tx <> None

  (** Direct (non-Lwt) connect — call under Eio ctx. *)
  let connect_sync (s : store) : unit =
    match s.conn with
    | Some _ -> ()
    | None ->
        let c =
          Pg.connect
            ~host:s.cfg.host
            ~port:s.cfg.port
            ~user:s.cfg.user
            ~password:s.cfg.password
            ~database:s.cfg.database
            ()
        in
        s.conn <- Some c;
        Printf.eprintf "[io_eio] connected to PG %s:%d/%s as %s\n%!"
          s.cfg.host s.cfg.port s.cfg.database s.cfg.user

  let execute_sync (c : conn) ?params sql : Pgx.row list =
    match params with
    | None -> Pg.execute c sql
    | Some params -> Pg.execute ~params c sql

  let with_transaction_sync ?isolation (s : store) (f : conn option -> 'a) : 'a
      =
    match s.conn with
    | None -> f None
    | Some c ->
        Pg.with_transaction ?isolation c (fun tx -> f (Some tx))

  let with_dynamic_transaction_sync ?isolation (s : store) (f : unit -> 'a) : 'a
      =
    match s.conn with
    | None -> f ()
    | Some c ->
        if s.current_tx <> None then
          failwith "Io_eio: nested transactions not supported"
        else
          Pg.with_transaction ?isolation c (fun tx ->
              s.current_tx <- Some tx;
              Fun.protect
                ~finally:(fun () -> s.current_tx <- None)
                f)

  (** Io.DB-shaped Lwt wrappers (for dual-run under Lwt_eio). *)
  let connect (s : store) : unit Lwt.t =
    Lwt_eio.run_eio (fun () -> connect_sync s)

  let execute (c : conn) ?params sql : Pgx.row list Lwt.t =
    Lwt_eio.run_eio (fun () -> execute_sync c ?params sql)

  let with_transaction ?isolation (s : store) (f : conn option -> 'a Lwt.t)
      : 'a Lwt.t =
    Lwt_eio.run_eio (fun () ->
        with_transaction_sync ?isolation s (fun copt ->
            Lwt_eio.run_lwt (fun () -> f copt)))

  let with_dynamic_transaction ?isolation (s : store) (f : unit -> 'a Lwt.t)
      : 'a Lwt.t =
    Lwt_eio.run_eio (fun () ->
        with_dynamic_transaction_sync ?isolation s (fun () ->
            Lwt_eio.run_lwt f))

  (** Health: SELECT 1. Returns row count (expect 1). *)
  let health_query_sync (s : store) : int =
    connect_sync s;
    match s.conn with
    | None -> 0
    | Some c ->
        let rows = execute_sync c "SELECT 1" in
        List.length rows

  let simple_query_sync (s : store) (sql : string) : string list list =
    connect_sync s;
    match s.conn with
    | None -> []
    | Some c ->
        let rows = execute_sync c sql in
        List.map
          (fun row ->
            List.map
              (fun cell ->
                match Pgx.Value.to_string cell with
                | Some x -> x
                | None -> "")
              row)
          rows
end

(** Self-test for image.http-robustness invariant 6: after [enqueue] raises,
    a later [enqueue] must still run (mutex not poisoned). Call under Eio. *)
let sequencer_survives_raise () : bool =
  let s = Pgx_io.Sequencer.create 0 in
  let raised =
    try
      ignore
        (Pgx_io.Sequencer.enqueue s (fun _ -> failwith "io_eio sequencer probe"));
      false
    with Failure _ -> true
  in
  if not raised then false
  else
    match Pgx_io.Sequencer.enqueue s (fun n -> n + 1) with
    | 1 -> true
    | _ -> false
    | exception _ -> false

(** [PP_IO=lwt|eio] — default eio. Explicit [PP_IO=lwt] is rollback. *)
let io_mode_from_env () =
  match Sys.getenv_opt "PP_IO" with
  | Some s ->
      (match String.lowercase_ascii (String.trim s) with
       | "lwt" -> `Lwt
       | _ -> `Eio)
  | None -> `Eio
