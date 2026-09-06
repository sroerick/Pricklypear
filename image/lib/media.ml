(** Pricklypear Image — binary media blob store (storage.media).

    Table: media_blobs (migration 007). Shared across branches like
    data_rows; ownership is [author]. Bytes live in bytea; meta path
    never returns the payload over Lisp eval. *)

type meta = {
  id : string;
  author : string;
  content_type : string;
  filename : string option;
  byte_size : int;
  sha256 : string;
  meta_json : string;
  created_at : string;
}

type put_result = {
  id : string;
  content_type : string;
  filename : string option;
  byte_size : int;
  sha256 : string;
}

let max_http_bytes = 5 * 1024 * 1024
(* Audio (music files) routinely dwarfs the image/pdf ceiling. *)
let max_http_audio_bytes = 64 * 1024 * 1024
let max_eval_put_bytes = 256 * 1024

let allowed_content_types =
  [ "image/png"
  ; "image/jpeg"
  ; "image/gif"
  ; "image/webp"
  ; "image/svg+xml"
  ; "application/pdf"
  ; "audio/mpeg" (* .mp3 *)
  ; "audio/ogg" (* .ogg / vorbis / opus *)
  ; "audio/flac" (* .flac *)
  ; "audio/wav" (* .wav *)
  ; "audio/mp4" (* .m4a / aac in mp4 *)
  ; "audio/aac" (* raw ADTS .aac *)
  ; "audio/webm" (* opus in webm (browser recordings) *)
  ]

(* Friendly aliases normalized to a canonical whitelisted type on
   put, so clients guessing mime by extension still land. *)
let content_type_aliases =
  [ ("audio/mp3", "audio/mpeg")
  ; ("audio/x-mp3", "audio/mpeg")
  ; ("audio/m4a", "audio/mp4")
  ; ("audio/x-m4a", "audio/mp4")
  ; ("audio/x-wav", "audio/wav")
  ; ("audio/wave", "audio/wav")
  ; ("audio/x-flac", "audio/flac")
  ]

let normalize_content_type (ct : string) : string =
  let ct = String.trim (String.lowercase_ascii ct) in
  let base =
    match String.index_opt ct ';' with
    | None -> ct
    | Some i -> String.trim (String.sub ct 0 i)
  in
  match List.assoc_opt base content_type_aliases with
  | Some canon -> canon
  | None -> base

let content_type_allowed (ct : string) : bool =
  List.mem (normalize_content_type ct) allowed_content_types

let is_audio_content_type (ct : string) : bool =
  String.starts_with ~prefix:"audio/" (normalize_content_type ct)

(* Type-aware upload ceiling: music files get the big one. *)
let max_http_bytes_for (ct : string) : int =
  if is_audio_content_type ct then max_http_audio_bytes else max_http_bytes

let sanitize_filename (name : string option) : string option =
  match name with
  | None -> None
  | Some raw ->
      let s = String.trim raw in
      if s = "" then None
      else
        let base =
          match String.rindex_opt s '/' with
          | Some i -> String.sub s (i + 1) (String.length s - i - 1)
          | None ->
              (match String.rindex_opt s '\\' with
               | Some i -> String.sub s (i + 1) (String.length s - i - 1)
               | None -> s)
        in
        let base = String.trim base in
        if base = "" || base = "." || base = ".." then None
        else
          let capped =
            if String.length base > 200 then String.sub base 0 200 else base
          in
          Some capped

let sha256_hex (bytes : string) : string = Auth_password.sha256_hex bytes

let b64_decode_opt (s : string) : string option =
  match Base64.decode ~pad:true s with
  | Ok raw -> Some raw
  | Error _ ->
      (match Base64.decode ~pad:false s with
       | Ok raw -> Some raw
       | Error _ -> None)

let url_of_id (id : string) : string = "/media/" ^ id

let meta_to_yojson (m : meta) : Yojson.Safe.t =
  `Assoc
    [ ("id", `String m.id)
    ; ("author", `String m.author)
    ; ("content_type", `String m.content_type)
    ; ( "filename"
      , match m.filename with Some f -> `String f | None -> `Null )
    ; ("byte_size", `Int m.byte_size)
    ; ("sha256", `String m.sha256)
    ; ("url", `String (url_of_id m.id))
    ; ("created_at", `String m.created_at)
    ; ( "meta"
      , try Yojson.Safe.from_string m.meta_json with _ -> `Assoc [] )
    ]

let meta_to_json_string (m : meta) : string =
  Yojson.Safe.to_string (meta_to_yojson m)

let put_result_to_yojson (r : put_result) : Yojson.Safe.t =
  `Assoc
    [ ("ok", `Bool true)
    ; ("id", `String r.id)
    ; ("content_type", `String r.content_type)
    ; ( "filename"
      , match r.filename with Some f -> `String f | None -> `Null )
    ; ("byte_size", `Int r.byte_size)
    ; ("sha256", `String r.sha256)
    ; ("url", `String (url_of_id r.id))
    ]

let cell_string row i =
  match List.nth_opt row i with
  | Some c -> Pgx.Value.to_string c
  | None -> None

let cell_int row i =
  match cell_string row i with
  | Some s -> (try Some (int_of_string s) with _ -> None)
  | None -> None

let row_to_meta (row : Pgx.row) : meta option =
  match
    ( cell_string row 0
    , cell_string row 1
    , cell_string row 2
    , cell_string row 3
    , cell_int row 4
    , cell_string row 5
    , cell_string row 6
    , cell_string row 7 )
  with
  | ( Some id
    , Some author
    , Some content_type
    , filename_opt
    , Some byte_size
    , Some sha256
    , Some meta_json
    , Some created_at ) ->
      Some
        { id
        ; author
        ; content_type
        ; filename = filename_opt
        ; byte_size
        ; sha256
        ; meta_json
        ; created_at
        }
  | _ -> None

let put
    (store : Store.t)
    ~(author : string)
    ~(content_type : string)
    ~(data : string)
    ?(filename : string option = None)
    ?(meta_json : string = "{}")
    ?(max_bytes : int = max_http_bytes)
    () : (put_result, string) result =
  let ct = normalize_content_type content_type in
  if not (content_type_allowed ct) then
    (Error ("disallowed content_type: " ^ ct))
  else
    let byte_size = String.length data in
    if byte_size <= 0 then (Error "empty body")
    else if byte_size > max_bytes then
      (Error
           (Printf.sprintf "payload too large (%d > %d)" byte_size max_bytes))
    else
      match Io_lwt.Db.current_conn store with
      | None -> (Error "no db connection")
      | Some conn ->
          let fname = sanitize_filename filename in
          let digest = sha256_hex data in
          let meta =
            let m = String.trim meta_json in
            if m = "" then "{}"
            else
              try
                ignore (Yojson.Safe.from_string m);
                m
              with _ -> "{}"
          in
          let sql =
            "INSERT INTO media_blobs \
             (author, content_type, filename, byte_size, sha256, data, meta) \
             VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb) \
             RETURNING id::text"
          in
          (try
      let rows =
                Store_db.exec conn
                  ~params:
                    [ Pgx.Value.of_string author
                    ; Pgx.Value.of_string ct
                    ; (match fname with
                       | Some f -> Pgx.Value.of_string f
                       | None -> Pgx.Value.of_string "")
                    ; Pgx.Value.of_int byte_size
                    ; Pgx.Value.of_string digest
                    ; Pgx.Value.of_binary data
                    ; Pgx.Value.of_string meta
                    ]
                  sql
              in
              match rows with
              | row :: _ ->
                  (match cell_string row 0 with
                   | Some id ->
                       (* Normalize empty filename column to None. *)
                       let filename =
                         match fname with
                         | Some f when f <> "" -> Some f
                         | _ -> None
                       in
                       (Ok
                            { id
                            ; content_type = ct
                            ; filename
                            ; byte_size
                            ; sha256 = digest
                            })
                   | None -> (Error "insert returned no id"))
              | [] -> (Error "insert returned no row")
    with exn ->
      (Error ("store failed: " ^ Printexc.to_string exn)))

(** Visibility confinement (BORGE-1785695911): a blob readable by [actor]
    iff [actor] authored it or [is_admin]. [None] performs no confinement
    (kernel callers that resolved identity upstream). Applied at the SQL
    layer so list/get/delete agree on one predicate. *)
let visible_sql ~(is_admin : bool) ~(actor_param : int) : string =
  if is_admin then "TRUE"
  else Printf.sprintf "author = $%d" actor_param

let get_meta (store : Store.t) ~(id : string)
  ?(visible_to : (string * bool) option) () : meta option =
  match Io_lwt.Db.current_conn store with
  | None -> None
  | Some conn ->
      let sql, params =
        match visible_to with
        | Some (actor, is_admin) ->
            (* BORGE-1785695911 regression fix: the admin predicate is
               literal TRUE — no $2 placeholder — so binding the actor
               param unconditionally made pgx reject the statement
               ("supplies 2 parameters, requires 1") and the swallowed
               exception turned every admin GET-by-uuid into a 404. *)
            ( "SELECT id::text, author, content_type, \
               NULLIF(filename, ''), byte_size::text, sha256, meta::text, \
               created_at::text \
               FROM media_blobs WHERE id = $1::uuid AND "
              ^ visible_sql ~is_admin ~actor_param:2
            , if is_admin then [ Pgx.Value.of_string id ]
              else
                [ Pgx.Value.of_string id; Pgx.Value.of_string actor ] )
        | None ->
            ( "SELECT id::text, author, content_type, \
               NULLIF(filename, ''), byte_size::text, sha256, meta::text, \
               created_at::text \
               FROM media_blobs WHERE id = $1::uuid"
            , [ Pgx.Value.of_string id ] )
      in
      (try
      let rows =
            Store_db.exec conn
              ~params
              sql
          in
          match rows with
          | row :: _ -> (row_to_meta row)
          | [] -> None
    with _ ->
      None)

let get_bytes (store : Store.t) ~(id : string)
  ?(visible_to : (string * bool) option) () : (meta * string) option =
  match Io_lwt.Db.current_conn store with
  | None -> None
  | Some conn ->
      let sql, params =
        match visible_to with
        | Some (actor, is_admin) ->
            (* Same admin-predicate param discipline as get_meta. *)
            ( "SELECT id::text, author, content_type, \
               NULLIF(filename, ''), byte_size::text, sha256, meta::text, \
               created_at::text, data \
               FROM media_blobs WHERE id = $1::uuid AND "
              ^ visible_sql ~is_admin ~actor_param:2
            , if is_admin then [ Pgx.Value.of_string id ]
              else
                [ Pgx.Value.of_string id; Pgx.Value.of_string actor ] )
        | None ->
            ( "SELECT id::text, author, content_type, \
               NULLIF(filename, ''), byte_size::text, sha256, meta::text, \
               created_at::text, data \
               FROM media_blobs WHERE id = $1::uuid"
            , [ Pgx.Value.of_string id ] )
      in
      (try
      let rows =
            Store_db.exec conn
              ~params
              sql
          in
          match rows with
          | row :: _ ->
              (match row_to_meta row, List.nth_opt row 8 with
               | Some m, Some data_cell ->
                   (match Pgx.Value.to_binary data_cell with
                    | Some bytes -> (Some (m, bytes))
                    | None ->
                        (* Text-mode fallback if driver surfaces bytea as hex/escape. *)
                        (match Pgx.Value.to_string data_cell with
                         | Some s -> (Some (m, s))
                         | None -> None))
               | _ -> None)
          | [] -> None
    with _ ->
      None)

let list
    (store : Store.t)
    ?(author : string option = None)
    ?(limit : int = 50)
    () : meta list =
  match Io_lwt.Db.current_conn store with
  | None -> []
  | Some conn ->
      let limit = max 1 (min 200 limit) in
      let sql, params =
        match author with
        | Some a ->
            ( "SELECT id::text, author, content_type, \
               NULLIF(filename, ''), byte_size::text, sha256, meta::text, \
               created_at::text \
               FROM media_blobs WHERE author = $1 \
               ORDER BY created_at DESC LIMIT $2"
            , [ Pgx.Value.of_string a; Pgx.Value.of_int limit ] )
        | None ->
            ( "SELECT id::text, author, content_type, \
               NULLIF(filename, ''), byte_size::text, sha256, meta::text, \
               created_at::text \
               FROM media_blobs \
               ORDER BY created_at DESC LIMIT $1"
            , [ Pgx.Value.of_int limit ] )
      in
      (try
      let rows = Store_db.exec conn ~params sql in
          (List.filter_map row_to_meta rows)
    with _ ->
      [])

let delete
    (store : Store.t)
    ~(id : string)
    ~(actor : string)
    ~(is_admin : bool)
  : ([ `Ok of string | `Not_found | `Forbidden ], string) result =
  let meta = get_meta store ~id () in
  match meta with
  | None -> (Ok `Not_found)
  | Some m ->
      if (not is_admin) && m.author <> actor then
        (Ok `Forbidden)
      else
        match Io_lwt.Db.current_conn store with
        | None -> (Error "no db connection")
        | Some conn ->
            let sql = "DELETE FROM media_blobs WHERE id = $1::uuid" in
            (try
      let _ =
                  Store_db.exec conn
                    ~params:[ Pgx.Value.of_string id ]
                    sql
                in
                (Ok (`Ok id))
    with exn ->
      (Error ("delete failed: " ^ Printexc.to_string exn)))
