(** HTTP handlers for storage.media — agent image push + browser upload.

    POST   /api/media          raw | JSON b64 | multipart
    GET    /api/media          JSON meta list
    DELETE /api/media/<uuid>
    GET    /media              browser gallery + upload form
    POST   /media/upload       multipart browser form
    GET    /media/<uuid>       raw bytes
*)
open Pure_html
open Http_core

(* Type-aware HTTP body cap: THE Http_core.request_body_cap table
   (~kind:`Media) — was a drifted private copy (i-1788671135-a). Audio
   (music files) gets the big ceiling; images/pdf keep the v1 5 MiB;
   JSON media pushes carry base64 (4/3 inflation). The per-stored-type
   cap is enforced a second time inside Media.put (store_upload). *)

(** Read body with a hard byte ceiling; Error `Too_large if exceeded. *)
let read_body_limited (reqd : Httpun.Reqd.t) ~(max_bytes : int)
  : (string, [ `Too_large ]) result =
  let body = Httpun.Reqd.request_body reqd in
  let promise, resolver = Eio.Promise.create () in
  let buf = Buffer.create (min 4096 max_bytes) in
  let oversized = ref false in
  let rec sched () =
    Httpun.Body.Reader.schedule_read body
      ~on_eof:(fun () ->
        if !oversized then Eio.Promise.resolve resolver (Error `Too_large)
        else Eio.Promise.resolve resolver (Ok (Buffer.contents buf)))
      ~on_read:(fun bs ~off ~len ->
        if !oversized then sched ()
        else if Buffer.length buf + len > max_bytes then begin
          oversized := true;
          Buffer.reset buf;
          sched ()
        end else begin
          Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
          sched ()
        end)
  in
  sched ();
  Eio.Promise.await promise

let require_authed_user (session : Session.t) : (string * bool, unit) result =
  let uname = Session.username session in
  if uname = "" || uname = "anonymous" then Error ()
  else Ok (uname, Session.is_admin session)

let too_large (reqd : Httpun.Reqd.t) ~(max_bytes : int) : unit =
  json_response reqd ~status:`Payload_too_large
    (`Assoc
       [ ("ok", `Bool false)
       ; ( "error"
         , `String (Printf.sprintf "body exceeds %d bytes" max_bytes) )
       ])

let bad_request (reqd : Httpun.Reqd.t) (msg : string) : unit =
  json_response reqd ~status:`Bad_request
    (`Assoc [ ("ok", `Bool false); ("error", `String msg) ])

let forbidden (reqd : Httpun.Reqd.t) (msg : string) : unit =
  json_response reqd ~status:`Forbidden
    (`Assoc [ ("ok", `Bool false); ("error", `String msg) ])

let not_found_json (reqd : Httpun.Reqd.t) (msg : string) : unit =
  json_response reqd ~status:`Not_found
    (`Assoc [ ("ok", `Bool false); ("error", `String msg) ])

(* ---- multipart/form-data (browser upload) ----
   Pure parser lives in Http_multipart (leaf module): shared with the
   (request-multipart) prim without a Prims_request -> Session ->
   Http_media cycle. Aliases keep the http_media call sites and the
   _for_test exports stable. *)

let multipart_boundary = Http_multipart.multipart_boundary

let parse_multipart = Http_multipart.parse_multipart

let pick_upload_file = Http_multipart.pick_upload_file

let parse_json_upload (body : string)
  : (string * string * string option * string, string) result =
  try
    match Yojson.Safe.from_string body with
    | `Assoc items ->
        let content_type =
          match List.assoc_opt "content_type" items with
          | Some (`String s) -> Some (String.trim s)
          | _ -> None
        in
        let data_b64 =
          match List.assoc_opt "data_b64" items with
          | Some (`String s) -> Some s
          | _ -> None
        in
        let filename =
          match List.assoc_opt "filename" items with
          | Some (`String s) -> Some s
          | Some `Null | None -> None
          | _ -> None
        in
        let meta =
          match List.assoc_opt "meta" items with
          | Some (`Assoc _ as j) -> Yojson.Safe.to_string j
          | Some (`String s) -> s
          | _ -> "{}"
        in
        (match content_type, data_b64 with
         | Some ct, Some b64 ->
             (match Media.b64_decode_opt (String.trim b64) with
              | Some raw -> Ok (ct, raw, filename, meta)
              | None -> Error "invalid data_b64")
         | None, _ -> Error "content_type required"
         | _, None -> Error "data_b64 required")
    | _ -> Error "JSON object required"
  with _ -> Error "invalid JSON"

let parse_upload_body ~(content_type : string option)
    ~(filename_hdr : string option) (body : string)
  : (string * string * string option * string, string) result =
  match content_type with
  | None -> Error "Content-Type required"
  | Some ct when body = "" -> Error "empty body"
  | Some ct ->
      let lower = String.lowercase_ascii (String.trim ct) in
      if String.starts_with ~prefix:"application/json" lower then
        parse_json_upload body
      else
        match multipart_boundary ct with
        | Some boundary ->
            (match parse_multipart ~boundary body with
             | Error e -> Error e
             | Ok parts ->
                 (match pick_upload_file parts with
                  | None ->
                      Error
                        "multipart: no file field (expected name=file or \
                         name=media)"
                  | Some part ->
                      let part_ct =
                        match part.content_type with
                        | Some c when String.trim c <> "" -> c
                        | _ ->
                            (* Fall back to extension guess is not done
                               server-side; require explicit type. *)
                            ""
                      in
                      if part_ct = "" then
                        Error
                          "multipart file missing Content-Type; browser \
                           should send one"
                      else
                        Ok
                          ( part_ct
                          , part.data
                          , (match part.filename with
                             | Some _ as f -> f
                             | None -> filename_hdr)
                          , "{}" )))
        | None ->
            (* Raw binary body — Content-Type is the media type. *)
            Ok (ct, body, filename_hdr, "{}")

let store_upload
    (session : Session.t)
    ~(author : string)
    ~(content_type : string)
    ~(data : string)
    ~(filename : string option)
    ~(meta_json : string)
  : (Media.put_result, Media.put_error) result =
  Media.put (Session.store session) ~author ~content_type ~data ~filename
    ~meta_json ~max_bytes:(Media.max_http_bytes_for content_type) ()

(** ONE status mapping for Media.put failures (i-1788670909): exhaustive
    over the typed variant — a new Media error constructor is a compile
    error here instead of a silently-misclassified 500. *)
let status_of_put_error (e : Media.put_error) : Httpun.Status.t =
  match e with
  | Media.Disallowed_content_type _ | Media.Empty_body -> `Bad_request
  | Media.Too_large _ -> `Payload_too_large
  | Media.No_db | Media.Store_failed _ -> `Internal_server_error

let api_media_post (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  match require_authed_user session with
  | Error () ->
      unauthorized_api reqd;
      ()
  | Ok (author, _) ->
      let req = Httpun.Reqd.request reqd in
      let headers = header_list req in
      let ct_hdr =
        Option.map String.trim (Http_kit.header_ci headers "content-type")
      in
      let cap = request_body_cap ~kind:`Media ct_hdr in
      let body_res = read_body_limited reqd ~max_bytes:cap in
      (match body_res with
       | Error `Too_large ->
           too_large reqd ~max_bytes:cap;
           ()
       | Ok body ->
           let filename_hdr = Http_kit.header_ci headers "x-filename" in
           (match
              parse_upload_body ~content_type:ct_hdr ~filename_hdr body
            with
            | Error msg ->
                bad_request reqd msg;
                ()
            | Ok (content_type, data, filename, meta_json) ->
                let res =
                  store_upload session ~author ~content_type ~data
                    ~filename ~meta_json
                in
                (match res with
                 | Ok r ->
                     json_response reqd ~status:`Created
                       (Media.put_result_to_yojson r);
                     ()
                 | Error e ->
                     json_response reqd ~status:(status_of_put_error e)
                       (`Assoc
                          [ ("ok", `Bool false)
                          ; ("error", `String (Media.put_error_message e))
                          ]);
                     ())))

let api_media_list (session : Session.t) (reqd : Httpun.Reqd.t)
    ~(target : string) : unit =
  match require_authed_user session with
  | Error () ->
      unauthorized_api reqd;
      ()
  | Ok (author, is_admin) ->
      let limit =
        match query_param target "limit" with
        | Some s -> (try int_of_string s with _ -> 50)
        | None -> 50
      in
      let scope_all =
        match query_param target "all" with
        | Some ("1" | "true" | "yes") when is_admin -> true
        | _ -> false
      in
      let rows =
        Media.list (Session.store session)
          ~author:(if scope_all then None else Some author)
          ~limit ()
      in
      let items = List.map Media.meta_to_yojson rows in
      json_response reqd
        (`Assoc [ ("ok", `Bool true); ("items", `List items) ]);
      ()

(* files-lens dedup probe (storage.files-file-lens): GET
   /api/media/by-sha256/<hex> — 200 {ok,id,...} | 404 | 401. The hex is
   length-checked (64) before it touches a uuid-shaped query param
   (this lookup keys sha256, not id, so no uuid cast to dodge). *)
let api_media_by_sha (session : Session.t) (reqd : Httpun.Reqd.t)
    ~(sha256 : string) : unit =
  match require_authed_user session with
  | Error () ->
      unauthorized_api reqd;
      ()
  | Ok (actor, is_admin) ->
      let sha = String.lowercase_ascii (String.trim sha256) in
      let valid =
        String.length sha = 64
        && String.for_all
             (function
               | '0' .. '9' | 'a' .. 'f' -> true
               | _ -> false)
             sha
      in
      if not valid then
        not_found_json reqd "no blob with that sha256"
      else
        let got =
          Media.get_meta_by_sha256 (Session.store session) ~sha256:sha
            ~visible_to:(Some (actor, is_admin)) ()
        in
        (match got with
         | None -> not_found_json reqd "no blob with that sha256"
         | Some m ->
             json_response reqd
               (`Assoc
                 [ ("ok", `Bool true)
                 ; ("id", `String m.id)
                 ; ("content_type", `String m.content_type)
                 ; ("byte_size", `Int m.byte_size)
                 ; ("sha256", `String m.sha256)
                 ; ("url", `String (Media.url_of_id m.id))
                 ]);
             ())

let api_media_delete (session : Session.t) (reqd : Httpun.Reqd.t)
    ~(id : string) : unit =
  match require_authed_user session with
  | Error () ->
      unauthorized_api reqd;
      ()
  | Ok (actor, is_admin) ->
      let res =
        Media.delete (Session.store session) ~id ~actor ~is_admin
      in
      (match res with
       | Ok (`Ok id') ->
           json_response reqd
             (`Assoc [ ("ok", `Bool true); ("id", `String id') ]);
           ()
       | Ok `Not_found ->
           not_found_json reqd "media not found";
           ()
       | Ok `Forbidden ->
           forbidden reqd "not author or admin";
           ()
       | Error msg ->
           json_response reqd ~status:`Internal_server_error
             (`Assoc [ ("ok", `Bool false); ("error", `String msg) ]);
           ())

(* HTTP Range for audio seek (RFC 9110). Single byte range only;
   malformed or multi-range requests are IGNORED (full 200); an
   unsatisfiable start (>= total) yields 416. *)
type range_req = [ `Range of int * int | `Full | `Unsatisfiable ]

let parse_range ~(total : int) (hdr : string option) : range_req =
  match hdr with
  | None -> `Full
  | Some h ->
      let h = String.trim h in
      let unit, spec =
        match String.index_opt h '=' with
        | Some i ->
            ( String.trim (String.sub h 0 i)
            , String.sub h (i + 1) (String.length h - i - 1) )
        | None -> ("", h)
      in
      if String.lowercase_ascii unit <> "bytes" then `Full
      else if String.contains spec ',' then `Full (* multi-range: ignore *)
      else
        let spec = String.trim spec in
        match String.index_opt spec '-' with
        | None -> `Full
        | Some dash ->
            let first = String.trim (String.sub spec 0 dash) in
            let last =
              String.trim
                (String.sub spec (dash + 1) (String.length spec - dash - 1))
            in
            if first = "" then
              (* Suffix form: bytes=-N (last N bytes). Zero-length
                 suffix is syntactically valid but unsatisfiable. *)
              (match int_of_string_opt last with
               | Some 0 -> `Unsatisfiable
               | Some n when n > 0 ->
                   if total = 0 then `Unsatisfiable
                   else
                     let n = min n total in
                     `Range (total - n, total - 1)
               | _ -> `Full)
            else
              (match int_of_string_opt first with
               | Some s when s >= 0 -> (
                   if s >= total then `Unsatisfiable
                   else
                     match int_of_string_opt last with
                     | None -> `Range (s, total - 1) (* bytes=5- *)
                     | Some e when e >= s ->
                         `Range (s, min e (total - 1))
                     | Some _ -> `Full (* start > end: ignore *))
               | _ -> `Full)

let media_get (session : Session.t) (reqd : Httpun.Reqd.t) ~(id : string)
  : unit =
  match require_authed_user session with
  | Error () ->
      unauthorized_api reqd;
      ()
  | Ok (actor, is_admin) ->
      (* BORGE-1785695911: GET by uuid is visibility-confined — other users'
         blobs are "not found", same as data_rows post Phase 2b. *)
      let got =
        Media.get_bytes (Session.store session) ~id
          ~visible_to:(actor, is_admin) ()
      in
      (match got with
       | None ->
           text_response reqd ~status:`Not_found ~content_type:"text/plain"
             "not found";
           ()
       | Some (m, bytes) ->
           let disposition =
             (* Stored-XSS hardening: SVG can carry script, so serve it as a
                download. Galleries keep rendering it via <img> tags. *)
             let kind =
               if m.content_type = "image/svg+xml" then "attachment"
               else "inline"
             in
             match m.filename with
             | Some f ->
                 Printf.sprintf "%s; filename=\"%s\"" kind
                   (String.map
                      (function '"' | '\n' | '\r' -> '_' | c -> c)
                      f)
             | None -> kind
           in
           let total = String.length bytes in
           let base_headers extra =
             Httpun.Headers.of_list
               ([ ("content-type", m.content_type)
                ; ("accept-ranges", "bytes")
                ; ("cache-control", "private, max-age=3600")
                ; ("content-disposition", disposition)
                ]
               @ extra
               @ Http_core.security_headers ())
           in
           let req_headers = header_list (Httpun.Reqd.request reqd) in
           let range_hdr = Http_kit.header_ci req_headers "range" in
           (match parse_range ~total range_hdr with
            | `Unsatisfiable ->
                let headers =
                  base_headers
                    [ ("content-length", "0")
                    ; ( "content-range"
                      , Printf.sprintf "bytes */%d" total )
                    ]
                in
                let resp =
                  Httpun.Response.create ~headers `Range_not_satisfiable
                in
                safe_respond_with_string reqd resp "";
                ()
            | `Full ->
                let headers =
                  base_headers [ ("content-length", string_of_int total) ]
                in
                let resp = Httpun.Response.create ~headers `OK in
                safe_respond_with_string reqd resp bytes;
                ()
            | `Range (start, stop) ->
                let slice =
                  String.sub bytes start ((stop - start) + 1)
                in
                let headers =
                  base_headers
                    [ ( "content-length"
                      , string_of_int (String.length slice) )
                    ; ( "content-range"
                      , Printf.sprintf "bytes %d-%d/%d" start stop total )
                    ]
                in
                let resp =
                  Httpun.Response.create ~headers `Partial_content
                in
                safe_respond_with_string reqd resp slice;
                ()))

(* ---- Browser UI: GET /media + POST /media/upload ---- *)

let is_image_ct (ct : string) : bool =
  String.starts_with ~prefix:"image/" (String.lowercase_ascii ct)

let is_audio_ct (ct : string) : bool =
  String.starts_with ~prefix:"audio/" (String.lowercase_ascii ct)

let media_row_card (m : Media.meta) : Pure_html.node =
  let name =
    match m.filename with Some f -> f | None -> m.id
  in
  let thumb =
    if is_image_ct m.content_type then
      HTML.a [HTML.href "%s" (Media.url_of_id m.id)] [
        HTML.img
          [ HTML.src "%s" (Media.url_of_id m.id)
          ; HTML.alt "%s" name
          ; HTML.style_ "max-width:12rem;max-height:12rem;object-fit:contain;border:1px solid #ccc;background:#fafafa"
          ]
      ]
    else if is_audio_ct m.content_type then
      (* Inline player — /media doubles as the little music library. *)
      HTML.div [] [
        HTML.audio
          [ HTML.controls
          ; HTML.preload `none
          ; HTML.src "%s" (Media.url_of_id m.id)
          ; HTML.style_ "width:100%%;max-width:24rem;display:block"
          ]
          [];
        HTML.a [HTML.href "%s" (Media.url_of_id m.id)] [txt "download / open"];
      ]
    else
      HTML.p [] [
        HTML.a [HTML.href "%s" (Media.url_of_id m.id)] [txt "download / open"]
      ]
  in
  HTML.div [HTML.class_ "card"; HTML.style_ "margin-bottom:1rem"] [
    thumb;
    HTML.p [HTML.class_ "meta"] [
      txt "%s · %s · %d bytes" name m.content_type m.byte_size;
    ];
    HTML.p [HTML.class_ "meta"] [
      HTML.code [] [txt "%s" m.id];
      txt " · ";
      HTML.a [HTML.href "%s" (Media.url_of_id m.id)] [txt "raw"];
    ];
    HTML.form
      [ HTML.method_ `POST
      ; HTML.action "/media/%s/delete" m.id
      ; HTML.style_ "margin-top:0.5rem"
      ] [
      HTML.button
        [ HTML.type_ "submit"; HTML.class_ "secondary" ]
        [txt "Delete"];
    ];
  ]

let media_page (session : Session.t) (reqd : Httpun.Reqd.t)
    ~(target : string) : unit =
  match require_authed_user session with
  | Error () ->
      require_login_redirect reqd ~path:"/media" ~target;
      ()
  | Ok (author, is_admin) ->
      let msg = Option.value (query_param target "msg") ~default:"" in
      let err = Option.value (query_param target "err") ~default:"" in
      let rows =
        Media.list (Session.store session) ~author:(Some author) ~limit:100
          ()
      in
      let flash =
        if err <> "" then
          [ HTML.p [HTML.class_ "err"] [txt "%s" err] ]
        else if msg <> "" then
          [ HTML.p [HTML.class_ "ok"] [txt "%s" msg] ]
        else []
      in
      let allowed =
        String.concat ", " Media.allowed_content_types
      in
      let upload_card =
        HTML.div [HTML.class_ "card"] [
          HTML.h2 [] [txt "Upload"];
          HTML.p [HTML.class_ "meta"] [
            txt "Max 5 MiB for images/PDF; 64 MiB for audio and files. Allowed: %s." allowed;
          ];
          HTML.form
            [ HTML.method_ `POST
            ; HTML.action "/media/upload"
            ; HTML.enctype `formdata
            ] [
            HTML.p [] [
              HTML.label [HTML.for_ "file"] [txt "File"];
              txt " ";
              HTML.input
                [ HTML.type_ "file"
                ; HTML.name "file"
                ; HTML.id "file"
                ; HTML.required
                ; HTML.accept
                    "image/png,image/jpeg,image/gif,image/webp,image/svg+xml,application/pdf,audio/mpeg,audio/ogg,audio/flac,audio/wav,audio/mp4,.png,.jpg,.jpeg,.gif,.webp,.svg,.pdf,.mp3,.ogg,.flac,.wav,.m4a"
                ];
            ];
            HTML.button [HTML.type_ "submit"] [txt "Upload"];
          ];
          HTML.p [HTML.class_ "meta"] [
            txt "Agents: POST /api/media (raw or JSON), ";
            HTML.code [] [txt "scripts/pp_media.py put ./shot.png"];
            txt ", or whole-directory file sync: ";
            HTML.code [] [txt "scripts/pp_files.py push ~/ppfiles"];
            txt ".";
          ];
        ]
      in
      let gallery =
        match rows with
        | [] ->
            HTML.p [] [txt "(no media yet — upload above)"]
        | _ ->
            HTML.div [] (List.map media_row_card rows)
      in
      let admin_note =
        if is_admin then
          HTML.p [HTML.class_ "meta"] [
            txt "Admin: JSON list of all users via ";
            HTML.code [] [txt "GET /api/media?all=1"];
            txt ".";
          ]
        else HTML.null []
      in
      let body =
        chrome ~session ~title:"Media" (
          flash
          @ [ HTML.p [HTML.class_ "meta"] [
                txt
                  "Your binary blobs (images/PDF/audio). Auth required to view.";
              ]
            ; upload_card
            ; admin_note
            ; HTML.h2 [] [txt "Yours"]
            ; gallery
            ]
        )
      in
      html_response reqd ~session body;
      ()

let media_upload_post (session : Session.t) (reqd : Httpun.Reqd.t)
  : unit =
  match require_authed_user session with
  | Error () ->
      require_login_redirect reqd ~path:"/media" ~target:"/media";
      ()
  | Ok (author, _) ->
      let cap =
        request_body_cap ~kind:`Media
          (Option.map String.trim
             (Http_kit.header_ci (header_list (Httpun.Reqd.request reqd)) "content-type"))
      in
      let body_res = read_body_limited reqd ~max_bytes:cap in
      (match body_res with
       | Error `Too_large ->
           redirect_response reqd
             ("/media?err="
             ^ url_encode
                 (Printf.sprintf "file too large (max %d bytes)" cap));
           ()
       | Ok body ->
           let req = Httpun.Reqd.request reqd in
           let headers = header_list req in
           let ct_hdr =
             Option.map String.trim (Http_kit.header_ci headers "content-type")
           in
           (match
              parse_upload_body ~content_type:ct_hdr ~filename_hdr:None body
            with
            | Error msg ->
                redirect_response reqd
                  ("/media?err=" ^ url_encode msg);
                ()
            | Ok (content_type, data, filename, meta_json) ->
                let res =
                  store_upload session ~author ~content_type ~data
                    ~filename ~meta_json
                in
                (match res with
                 | Ok r ->
                     let msg =
                       Printf.sprintf "uploaded %s (%d bytes)" r.id
                         r.byte_size
                     in
                     redirect_response reqd
                       ("/media?msg=" ^ url_encode msg);
                     ()
                 | Error e ->
                     redirect_response reqd
                       ("/media?err=" ^ url_encode (Media.put_error_message e));
                     ())))

let media_delete_post (session : Session.t) (reqd : Httpun.Reqd.t)
    ~(id : string) : unit =
  match require_authed_user session with
  | Error () ->
      require_login_redirect reqd ~path:"/media" ~target:"/media";
      ()
  | Ok (actor, is_admin) ->
      (* Drain body if any (form POST). *)
      let _ = read_body_limited reqd ~max_bytes:(64 * 1024) in
      let res =
        Media.delete (Session.store session) ~id ~actor ~is_admin
      in
      (match res with
       | Ok (`Ok _) ->
           redirect_response reqd
             ("/media?msg=" ^ url_encode ("deleted " ^ id));
           ()
       | Ok `Not_found ->
           redirect_response reqd
             ("/media?err=" ^ url_encode "not found");
           ()
       | Ok `Forbidden ->
           redirect_response reqd
             ("/media?err=" ^ url_encode "not author or admin");
           ()
       | Error msg ->
           redirect_response reqd
             ("/media?err=" ^ url_encode msg);
           ())

(* Exposed for unit tests *)
let multipart_boundary_for_test = multipart_boundary
let parse_multipart_for_test = parse_multipart
let parse_range_for_test = parse_range
let pick_upload_file_for_test = pick_upload_file
