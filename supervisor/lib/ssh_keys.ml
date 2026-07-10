(** Pricklypear Supervisor — SSH hostkey + auth-db management.

    Per spec [supervisor.ssh-interface]: the supervisor's SSH front
    door needs a host key and an authorized-user set. We persist
    an Ed25519 hostkey seed in [system_keys] (mirroring authd's
    session_signing pattern from it5) so the same key is presented
    across supervisor restarts. We also build the [Awa.Auth.db]
    from authd's stored user→ssh-key pairs at startup so SSH
    pubkey auth against awa 0.3.1's static-db API works.

    See BORGE-1783439434 for the snapshot-refresh caveat (new keys
    added later require supervisor restart until awa-ssh 0.5.x
    upgrade lands). *)
open Lwt.Syntax

let pp = Printf.eprintf
let flush' () = flush stderr

(* The string id used in PG system_keys for our hostkey seed. *)
let hostkey_seed_id = "supervisor_ssh_hostkey_seed"

(* Load the hostkey seed from PG system_keys. Returns None if
   not present (first boot) or if PG is down. *)
let load_hostkey_seed (store : Store.t) : Cstruct.t option Lwt.t =
  let open Store in
  match store.conn with
  | None -> Lwt.return_none
  | Some conn ->
      Lwt.catch
        (fun () ->
          let+ rows =
            Pg.execute conn
              "SELECT key_data FROM system_keys WHERE id = $1"
              ~params:[ Pgx.Value.of_string hostkey_seed_id ]
          in
          match rows with
          | [ [ cell ] ] ->
              (match Pgx.Value.to_binary cell with
               | Some s -> Some (Cstruct.of_string s)
               | None -> None)
          | _ -> None)
        (fun exn ->
          pp "[supervisor.ssh_keys] load_hostkey_seed failed: %s\n"
            (Printexc.to_string exn);
          flush' ();
          Lwt.return_none)

(* Persist a 32-byte hostkey seed to PG system_keys. Idempotent:
   ON CONFLICT DO NOTHING so two supervisors racing to seed are
   both happy. *)
let seed_hostkey_if_missing (store : Store.t) (seed : Cstruct.t)
    : bool Lwt.t =
  let open Store in
  match store.conn with
  | None -> Lwt.return_false
  | Some conn ->
      Lwt.catch
        (fun () ->
          let+ _ =
            Pg.execute_unit conn
              "INSERT INTO system_keys (id, key_data) \
               VALUES ($1, $2) \
               ON CONFLICT (id) DO NOTHING"
              ~params:[
                Pgx.Value.of_string hostkey_seed_id;
                Pgx.Value.of_binary (Cstruct.to_string seed);
              ]
          in
          true)
        (fun exn ->
          pp "[supervisor.ssh_keys] seed_hostkey failed: %s\n"
            (Printexc.to_string exn);
          flush' ();
          Lwt.return_false)

(* Generate a fresh 32-byte Ed25519 seed via the Poohstack RNG.
   Mirrors auth/lib/boot.ml's Mirage_crypto_rng_unix.initialize
   pattern (it5). *)
let generate_seed () : Cstruct.t =
  (* RNG must have been initialized by the caller; in practice the
     supervisor boot path initializes Mirage_crypto_rng_unix. *)
  Mirage_crypto_rng.generate 32

(* Load or generate the hostkey. Returns the Awa.Hostkey.priv. *)
let hostkey (store : Store.t) : Awa.Hostkey.priv Lwt.t =
  let* existing = load_hostkey_seed store in
  let seed =
    match existing with
    | Some s -> s
    | None ->
        let s = generate_seed () in
        (* Best-effort persist; non-fatal if it fails (we'll regenerate
           on next boot — SSH clients will see a hostkey mismatch then,
           which they will need to acknowledge). *)
        let _ = Lwt.async (fun () ->
          let _ = seed_hostkey_if_missing store s in
          Lwt.return_unit) in
        s
  in
  (* Awa.Keys.of_seed generates a hostkey deterministically from a seed.
     Using Ed25519 keeps the wire handshake small + fast on tiny dev boxes. *)
  let hostkey_val = Awa.Keys.of_seed `Ed25519 (Cstruct.to_string seed) in
  Lwt.return hostkey_val

(* Parse an "ssh-ed25519 AAAA..." public key string into an
   Awa.Hostkey.pub. Returns None if the format is unrecognized
   (the row will be dropped from the auth db with a log line). *)
let parse_pubkey (s : string) : Awa.Hostkey.pub option =
  match String.split_on_char ' ' s with
  | "ssh-ed25519" :: b64 :: _ ->
      (* The wire blob for an ssh-ed25519 pubkey is the 4-byte length
         + "ssh-ed25519" + 4-byte length + 32-byte key point. Awa's
         pubkey_of_blob parser expects exactly this wire-form blob. *)
      (match Base64.decode ~pad:false b64 with
       | Ok bin ->
           let cs = Cstruct.of_string bin in
           (match Awa.Wire.pubkey_of_blob_error_as_string cs with
            | Ok pub -> Some pub
            | Error e ->
                pp "[supervisor.ssh_keys] parse_pubkey: awa rejected blob: %s\n" e;
                flush' ();
                None)
       | Error (`Msg m) ->
           pp "[supervisor.ssh_keys] parse_pubkey: b64 failed: %s\n" m;
           flush' ();
           None)
  | _ ->
      pp "[supervisor.ssh_keys] parse_pubkey: not ssh-ed25519: %s\n"
        (String.sub s 0 (min 30 (String.length s)));
      flush' ();
      None

(* Build an [Awa.Auth.db] from a list of (username, ssh_key) pairs.
   Drops any key whose format we can't parse; logs each drop.
   Password field is None for all entries (we are ssh-key only
   per spec [auth.credentials-store]). *)
let build_auth_db (entries : (string * string) list) : Awa.Auth.db =
  List.filter_map
    (fun (username, ssh_key) ->
      match parse_pubkey ssh_key with
      | Some pub -> Some (Awa.Auth.make_user username [pub])
      | None -> None)
    entries
