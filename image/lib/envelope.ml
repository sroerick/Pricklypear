(** Envelope encryption for recoverable secrets at rest (i-1788670058).

    LLM provider keys must be RECOVERABLE (they are sent to the
    provider), so they cannot be one-way hashed like session tokens.
    They are sealed with AES-256-GCM under a master key from
    PP_LLM_ENVELOPE_KEY: a 64-hex-char key is used directly; any other
    non-empty secret is stretched with SHA-256 to 32 bytes. The
    ciphertext is bound to the row identity as AAD ("llm_key:" ^
    user_id), so a row swapped between users fails the tag check.

    Blob shape stored in credentials.secret_data:
      {"v":1,"alg":"aes256-gcm","nonce":<b64>,"ct":<b64>}
    where ct is ciphertext ++ 16-byte tag, base64.

    Degraded mode: with no master key configured, encryption is
    unavailable. Callers must keep plaintext behavior VISIBLE (warn at
    boot and on write), never silently pretend the secret is sealed.
    Legacy plaintext {"api_key": ...} rows still read back (see
    [is_envelope_blob] / [decrypt_secret]), and boot re-seals them
    idempotently (Auth_store.migrate_llm_keys). *)

let env_var = "PP_LLM_ENVELOPE_KEY"

let master_key_present () =
  match Sys.getenv_opt env_var with
  | Some s -> String.trim s <> ""
  | None -> false

(* Hex-or-stretched 32-byte AES key from the configured secret.
   mirage-crypto 1.x AEAD is string-based; the key is the raw 32-byte
   string via AES.GCM.of_secret. *)
let key_of_secret (s : string) : string =
  let s = String.trim s in
  let is_hex =
    String.length s = 64
    && String.for_all
         (fun c ->
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F'))
         s
  in
  let hex_to_bytes s =
    let b = Bytes.create 32 in
    for i = 0 to 31 do
      let v =
        match int_of_string_opt ("0x" ^ String.sub s (2 * i) 2) with
        | Some v -> v
        | None -> 0
      in
      Bytes.set b i (Char.chr (v land 0xff))
    done;
    Bytes.to_string b
  in
  if is_hex then hex_to_bytes s
  else
    (* stretch non-hex secrets deterministically to 32 bytes *)
    hex_to_bytes (Digestif.SHA256.to_hex (Digestif.SHA256.digest_string s))

let master_key () : Mirage_crypto.AES.GCM.key option =
  match Sys.getenv_opt env_var with
  | Some s when String.trim s <> "" ->
      Some (Mirage_crypto.AES.GCM.of_secret (key_of_secret s))
  | _ -> None

let aad_for ~user_id = "llm_key:" ^ user_id

let b64_encode s = Base64.encode_string s
let b64_decode s = match Base64.decode s with Ok s' -> Some s' | _ -> None

let tag_len = 16

(* Seal [plain] for [user_id]. Ok = the JSON blob to store. *)
let encrypt ~user_id (plain : string) : (string, string) result =
  match master_key () with
  | None -> (Error "no envelope key configured")
  | Some key ->
      let nonce = Mirage_crypto_rng.generate 12 in
      let ct, tag =
        Mirage_crypto.AES.GCM.authenticate_encrypt_tag ~key ~nonce
          ~adata:(aad_for ~user_id) plain
      in
      let blob =
        `Assoc
          [ ("v", `Int 1)
          ; ("alg", `String "aes256-gcm")
          ; ("nonce", `String (b64_encode nonce))
          ; ("ct", `String (b64_encode (ct ^ tag)))
          ]
      in
      Ok (Yojson.Safe.to_string blob)

(* True when the blob is an envelope blob (vs legacy plaintext api_key). *)
let is_envelope_blob (js : string) : bool =
  try
    match Yojson.Safe.from_string js with
    | `Assoc a -> List.assoc_opt "ct" a <> None
    | _ -> false
  with _ -> false

(* Open an envelope blob for [user_id]. None = wrong identity (AAD),
   tampered blob, or not an envelope blob at all. Callers use
   [is_envelope_blob] to tell the legacy-plaintext case apart. *)
let decrypt ~user_id (js : string) : string option =
  if not (is_envelope_blob js) then None
  else
    try
      match Yojson.Safe.from_string js with
      | `Assoc a ->
          (match (List.assoc_opt "nonce" a, List.assoc_opt "ct" a) with
           | Some (`String n), Some (`String c) ->
               (match (b64_decode n, b64_decode c) with
                | Some nonce_s, Some ct_s when
                    String.length nonce_s = 12
                    && String.length ct_s > tag_len ->
                    let ct = String.sub ct_s 0 (String.length ct_s - tag_len) in
                    let tag =
                      String.sub ct_s (String.length ct_s - tag_len) tag_len
                    in
                    (match
                       Mirage_crypto.AES.GCM.authenticate_decrypt_tag
                         ~key:(Option.get (master_key ()))
                         ~nonce:nonce_s
                         ~tag
                         ~adata:(aad_for ~user_id)
                         ct
                     with
                     | Some plain -> Some plain
                     | None -> None)
                | _ -> None)
           | _ -> None)
      | _ -> None
    with _ -> None