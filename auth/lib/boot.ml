(** Pricklypear Auth — Boot.

    Per spec:
      [auth.credentials-store] — fetch the session_signing key from
      system_keys on boot. Auth is the only thing that holds it.
      For first boot, seed a fresh random 32-byte key.

      [auth.bootstrap-admin] — if users table is empty AND the env var
      PP_BOOTSTRAP_ADMIN_KEY is set, INSERT into users (admin, is_admin=true)
      + credentials (kind=ssh_key, public_data={"key":...}, user_id=admin).
      Warn loudly in logs. The env var is ignored on subsequent boots.

    Then start the internal RPC HTTP server (default :7879). *)
open Lwt.Syntax

(* Per-step print helper that flushes so log output survives
   even if the process exits early. Uses eprintf + flush stderr so the
   message reaches the pipe / log file before any subsequent failure. *)
let pr fmt =
  Printf.ksprintf (fun s ->
    Printf.eprintf "%s" s;
    flush stderr)
  fmt

let env_or name dft = match Sys.getenv_opt name with Some s -> s | None -> dft
let int_env_or name dft = int_of_string (env_or name (string_of_int dft))

(* Seed system_keys if first boot. *)
let ensure_signing_key (store : Store.t) : Cstruct.t option Lwt.t =
  let* existing = Store.get_signing_key store in
  match existing with
  | Some k ->
      pr "[auth.boot] using existing signing key\n";
      Lwt.return (Some k)
  | None ->
      pr "[auth.boot] no signing key; seeding fresh 32B random key\n";
      let new_key = Cstruct.of_string (Mirage_crypto_rng.generate 32) in
      let* ok = Store.seed_signing_key_if_missing store new_key in
      pr "[auth.boot] signing key seeded: %b\n" ok;
      Lwt.return (Some new_key)

let start () : unit Lwt.t =
  pr "[auth.boot] pricklypear auth starting up...\n";
  (* mirage-crypto-rng 1.x API (poohstack/Dream-compatible). *)
  Mirage_crypto_rng_unix.use_default ();
  let cfg = Store.config_from_env () in
  let store = Store.make ~cfg () in
  let* connected = Store.connect store in
  if not connected then begin
    pr "[auth.boot] PG UNREACHABLE — auth cannot function.\nExiting.\n";
    flush_all ();
    Lwt.return_unit
  end else begin
    let* key_opt = ensure_signing_key store in
    (match key_opt with
     | None -> pr "[auth.boot] WARNING: no signing key available\n"
     | Some k -> Verify.set_signing_key k);
    let* () =
      match Sys.getenv_opt "PP_BOOTSTRAP_ADMIN_KEY" with
      | None -> Lwt.return_unit
      | Some key_str ->
          pr "[auth.boot] PP_BOOTSTRAP_ADMIN_KEY env var present; checking users table...\n";
          let* empty = Store.users_empty store in
          pr "[auth.boot] users_empty returned: %b\n" empty;
          if empty then begin
            pr "[auth.boot] BOOTSTRAP via PP_BOOTSTRAP_ADMIN_KEY\n";
            let* ok = Store.bootstrap_admin store
              ~username:"admin" ~ssh_key:key_str in
            pr "[auth.boot] bootstrap admin inserted: %b\n" ok;
            pr "[auth.boot] WARNING: clear env var after first boot.\n";
            Lwt.return_unit
          end else begin
            pr "[auth.boot] users table non-empty; bootstrap env var ignored.\n";
            Lwt.return_unit
          end
    in
    let port = int_env_or "PP_AUTH_PORT" 7879 in
    flush_all ();
    Http.serve ~store ~port ()
  end
