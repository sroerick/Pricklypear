(** Pricklypear Supervisor — Fleet management.

    Per spec [supervisor.fleet]: the supervisor maintains a fleet table
      (user, branch, image_version, instance_id, boot_mode, health_status,
       started_at, last_seen)

    For the minimal supervisor:
      - In-memory hashtable (instance_id -> instance record).
      - boot(user, branch, image_version) -> spawn image process, register.
      - kill(instance_id) -> kill process, deregister.
      - list_fleet() -> all instances + health.

    Persists to Postgres [sessions] table on boot/kill (TODO — for now
    in-memory only; the migrations-table rows are seeded but not yet
    written by the supervisor). Marked stub. *)

open Lwt.Syntax

type instance_id = string  (* uuid *)

type instance = {
  id : instance_id;
  user_id : string;
  username : string;
  branch_id : string;
  branch_name : string;
  image_version : string;
  boot_mode : Pricklypear_common.Manifest.boot_mode;
  mutable health : Pricklypear_common.Protocol.health_status;
  started_at : float;
  mutable last_seen : float;
  mutable process : Process.t option;
  mutable http_port : int;
  mutable repl_port : int;
}

type t = {
  (* Keyed by instance_id *)
  table : (instance_id, instance) Hashtbl.t;
  (* Allocator for ports — picks the next free port. *)
  mutable next_http_port : int;
  mutable next_repl_port : int;
  mutable next_id : int;
}

let make
    ?(http_port_base = 18080)
    ?(repl_port_base = 17878)
    () : t =
  { table = Hashtbl.create 16;
    next_http_port = http_port_base;
    next_repl_port = repl_port_base;
    next_id = 1; }

let alloc_id (t : t) : instance_id =
  let n = t.next_id in
  t.next_id <- t.next_id + 1;
  Printf.sprintf "inst-%03d" n

(* Check whether a TCP port can be bound (i.e. is free for us to use).
   Used by alloc_ports to skip ports already taken by some other process
   (authd, the supervisor's own admin/ssh ports, a stale image from a
   prior run, an unrelated OS daemon, etc.). Without this probe,
   Fleet.spawn's purely-incremental allocator could collide with the
   authd listener (17879 in dev.sh) or any other service co-tenanted
   on the same host. Returns true if a fresh socket SO_REUSEADDR binds
   successfully on 127.0.0.1:<port> (for repl) or 0.0.0.0:<port>
   (for http, matching the image's bind address).

   NOTE: this is a best-effort race-free-ish probe. There's a TOCTOU
   window between this check and the image's actual bind (the supervisor
   allocates, returns the port, the supervisor's Process.boot spawns
   the image, the image binds). For our dev/loopback context this is
   fine: the only writers to ports >= 18000 in the same millisecond
   are the image unikernels we spawn ourselves, and they bind within
   ~50-150ms of spawn. Blind-allocation lived with the same race. *)
let port_is_free ~(loopback : bool) (port : int) : bool =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (* Set SO_REUSEADDR to match the image's bind behavior; otherwise
     a recently-freed port would still be in TIME_WAIT and report
     as busy even though it's bindable. *)
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  let bind_addr =
    if loopback then Unix.inet_addr_loopback else Unix.inet_addr_any
  in
  let free =
    try
      Unix.bind sock (Unix.ADDR_INET (bind_addr, port));
      true
    with Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      false
  in
  Unix.close sock;
  free

(* Allocate the next free pair of ports. Walks forward from the current
   next_http_port/next_repl_port counter until both ports are free.
   Caps the search at +1024 attempts to avoid infinite loops in the
   pathological case where a long stretch of ports is in use. *)
let alloc_ports (t : t) : int * int =
  let rec walk http_port repl_port attempts =
    if attempts > 1024 then
      (* Give up; fall back to the blind-allocation behavior. This is
         strictly worse than the probe but avoids an infinite loop. *)
      (http_port, repl_port)
    else if port_is_free ~loopback:false http_port
         && port_is_free ~loopback:true repl_port then
      begin
        t.next_http_port <- http_port + 1;
        t.next_repl_port <- repl_port + 1;
        (http_port, repl_port)
      end
    else
      walk (http_port + 1) (repl_port + 1) (attempts + 1)
  in
  walk t.next_http_port t.next_repl_port 0

(* Boot (or attach to) an image instance for the given user/branch.
   Uses Process.boot to posix_spawn the image unikernel.
   Returns the new instance record on success. *)
let boot
    (t : t)
    ~(manifest : Pricklypear_common.Manifest.t)
    : (instance, string) result Lwt.t =
  let id = alloc_id t in
  let (http_port, repl_port) = alloc_ports t in
  let* proc_res =
    Process.boot
      ~manifest ~http_port ~repl_port
  in
  match proc_res with
  | Error e -> Lwt.return_error e
  | Ok proc ->
      let now = Unix.gettimeofday () in
      let inst = {
        id;
        user_id = manifest.user_id;
        username = manifest.username;
        branch_id = manifest.branch_id;
        branch_name = manifest.branch_name;
        image_version = manifest.image_version;
        boot_mode = manifest.boot_mode;
        health = Pricklypear_common.Protocol.H_unknown;
        started_at = now;
        last_seen = now;
        process = Some proc;
        http_port;
        repl_port;
      } in
      Hashtbl.replace t.table id inst;
      Lwt.return_ok inst

let kill (t : t) (id : instance_id) : (unit, string) result Lwt.t =
  match Hashtbl.find_opt t.table id with
  | None -> Lwt.return_error "no such instance"
  | Some inst ->
      (match inst.process with
       | None -> Lwt.return_ok ()
       | Some proc ->
           let* () = Process.kill proc in
           Hashtbl.remove t.table id;
           Lwt.return_ok ())

(* List all instances. *)
let list (t : t) : instance list =
  Hashtbl.fold (fun _ v acc -> v :: acc) t.table []

(* Lookup by user_id — useful for "is alice's image already running?"
   when an SSH connect arrives. *)
let find_by_user (t : t) (user_id : string) : instance option =
  Hashtbl.fold
    (fun _ v acc ->
      match acc with
      | Some _ -> acc
      | None -> if v.user_id = user_id then Some v else None)
    t.table None

(* Mark an instance's health; updates last_seen time. *)
let mark_health (t : t) (id : instance_id)
    (h : Pricklypear_common.Protocol.health_status) : unit =
  match Hashtbl.find_opt t.table id with
  | None -> ()
  | Some inst ->
      inst.health <- h;
      inst.last_seen <- Unix.gettimeofday ()
