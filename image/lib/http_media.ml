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

(* Type-aware HTTP body cap. Audio (music files) gets the big
   ceiling; images/pdf keep the v1 5 MiB. Multipart may carry audio
   parts (framing overhead) and JSON data_b64 inflates bytes by 4/3,
   so both get slack over the audio ceiling. The per-stored-type cap
   is enforced a second time inside Media.put (store_upload). *)
let request_body_cap (content_type : string option) : int =
  match content_type with
  | None -> Media.max_http_bytes
  | Some ct ->
      let ct = String.lowercase_ascii ct in
      if String.starts_with ~prefix:"multipart/form-data" ct then
        Media.max_http_audio_bytes + 65536
      else if String.starts_with ~prefix:"application/json" ct then
        ((Media.max_http_audio_bytes * 4) / 3) + 4096
      else Media.max_http_bytes_for ct

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

let header_ci (headers : (string * string) list) (name : string)
  : string option =
  let lower = String.lowercase_ascii name in
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii k = lower then Some v else None)
    headers

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

(* ---- multipart/form-data (browser upload) ---- *)

let multipart_boundary (content_type : string) : string option =
  let ct = String.trim content_type in
  let lower = String.lowercase_ascii ct in
  if not (String.starts_with ~prefix:"multipart/form-data" lower) then None
  else
    (* boundary=VALUE — value may be quoted *)
    let rec find_boundary s =
      let sl = String.lowercase_ascii s in
      match String.index_opt sl 'b' with
      | None -> None
      | Some i ->
          if i + 9 <= String.length s
             && String.sub sl i 9 = "boundary="
          then
            let rest = String.sub s (i + 9) (String.length s - i - 9) in
            let rest = String.trim rest in
            if rest = "" then None
            else if rest.[0] = '"' then
              match String.index_from_opt rest 1 '"' with
              | Some j -> Some (String.sub rest 1 (j - 1))
              | None -> None
            else
              let end_at =
                match String.index_opt rest ';' with
                | Some j -> j
                | None -> String.length rest
              in
              let b = String.trim (String.sub rest 0 end_at) in
              if b = "" then None else Some b
          else find_boundary (String.sub s (i + 1) (String.length s - i - 1))
    in
    find_boundary ct

(** Split [s] on CRLF or LF; keep binary-safe (no decode). *)
let index_of_sub (hay : string) ~(pos : int) (needle : string) : int option =
  let nlen = String.length needle in
  let hlen = String.length hay in
  if nlen = 0 || pos < 0 || pos > hlen then None
  else
    let last = hlen - nlen in
    let rec loop i =
      if i > last then None
      else if String.sub hay i nlen = needle then Some i
      else loop (i + 1)
    in
    loop pos

let parse_part_headers (header_blob : string)
  : (string * string) list =
  let lines =
    (* Normalize CRLF → split on \n and strip trailing \r *)
    String.split_on_char '\n' header_blob
    |> List.map (fun line ->
           let line =
             if String.length line > 0 && line.[String.length line - 1] = '\r'
             then String.sub line 0 (String.length line - 1)
             else line
           in
           line)
    |> List.filter (fun l -> String.trim l <> "")
  in
  List.filter_map
    (fun line ->
      match String.index_opt line ':' with
      | None -> None
      | Some i ->
          let k =
            String.lowercase_ascii (String.trim (String.sub line 0 i))
          in
          let v =
            String.trim
              (String.sub line (i + 1) (String.length line - i - 1))
          in
          Some (k, v))
    lines

let content_disposition_params (cd : string) : (string * string) list =
  (* form-data; name="file"; filename="x.png" *)
  String.split_on_char ';' cd
  |> List.filter_map (fun part ->
         let part = String.trim part in
         match String.index_opt part '=' with
         | None -> None
         | Some i ->
             let k =
               String.lowercase_ascii (String.trim (String.sub part 0 i))
             in
             let raw =
               String.trim
                 (String.sub part (i + 1) (String.length part - i - 1))
             in
             let v =
               if String.length raw >= 2 && raw.[0] = '"'
                  && raw.[String.length raw - 1] = '"'
               then String.sub raw 1 (String.length raw - 2)
               else raw
             in
             Some (k, v))

type multipart_file = {
  field_name : string;
  filename : string option;
  content_type : string option;
  data : string;
}

(** Extract file parts from a multipart body. Prefers field name
    "file", then "media", then first part that has a filename. *)
let parse_multipart ~(boundary : string) (body : string)
  : (multipart_file list, string) result =
  if boundary = "" then Error "empty boundary"
  else
    let delim = "--" ^ boundary in
    let close_delim = delim ^ "--" in
    (* Walk parts between delimiters. *)
    let rec collect acc pos =
      match index_of_sub body ~pos delim with
      | None -> List.rev acc
      | Some i ->
          let after_delim = i + String.length delim in
          (* Skip optional leading CRLF after delimiter *)
          let after_delim =
            if after_delim + 1 < String.length body
               && body.[after_delim] = '\r'
               && body.[after_delim + 1] = '\n'
            then after_delim + 2
            else if after_delim < String.length body
                    && body.[after_delim] = '\n'
            then after_delim + 1
            else after_delim
          in
          (* Closing delimiter? *)
          if i >= 0
             && String.length body >= i + String.length close_delim
             && String.sub body i (String.length close_delim) = close_delim
          then List.rev acc
          else
            match index_of_sub body ~pos:after_delim delim with
            | None -> List.rev acc
            | Some next ->
                (* Part is [after_delim, next); strip trailing CRLF before next delim *)
                let part_end = next in
                let part_end =
                  if part_end >= 2
                     && body.[part_end - 2] = '\r'
                     && body.[part_end - 1] = '\n'
                  then part_end - 2
                  else if part_end >= 1 && body.[part_end - 1] = '\n'
                  then part_end - 1
                  else part_end
                in
                let part =
                  if part_end > after_delim then
                    String.sub body after_delim (part_end - after_delim)
                  else ""
                in
                (* Split headers / body on blank line *)
                let header_end =
                  match index_of_sub part ~pos:0 "\r\n\r\n" with
                  | Some j -> Some (j, 4)
                  | None ->
                      (match index_of_sub part ~pos:0 "\n\n" with
                       | Some j -> Some (j, 2)
                       | None -> None)
                in
                let acc =
                  match header_end with
                  | None -> acc
                  | Some (he, sep_len) ->
                      let headers =
                        parse_part_headers (String.sub part 0 he)
                      in
                      let data_start = he + sep_len in
                      let data =
                        if data_start <= String.length part then
                          String.sub part data_start
                            (String.length part - data_start)
                        else ""
                      in
                      let cd =
                        List.assoc_opt "content-disposition" headers
                      in
                      let params =
                        match cd with
                        | Some s -> content_disposition_params s
                        | None -> []
                      in
                      let field_name =
                        Option.value (List.assoc_opt "name" params)
                          ~default:""
                      in
                      let filename = List.assoc_opt "filename" params in
                      let content_type =
                        List.assoc_opt "content-type" headers
                      in
                      if field_name = "" && filename = None then acc
                      else
                        { field_name; filename; content_type; data } :: acc
                in
                collect acc next
    in
    try Ok (collect [] 0)
    with exn -> Error ("multipart parse failed: " ^ Printexc.to_string exn)

let pick_upload_file (parts : multipart_file list) : multipart_file option =
  let named n =
    List.find_opt (fun p -> p.field_name = n && p.data <> "") parts
  in
  match named "file" with
  | Some p -> Some p
  | None ->
      (match named "media" with
       | Some p -> Some p
       | None ->
           List.find_opt
             (fun p -> p.filename <> None && p.data <> "")
             parts)

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
  : (Media.put_result, string) result =
  Media.put (Session.store session) ~author ~content_type ~data ~filename
    ~meta_json ~max_bytes:(Media.max_http_bytes_for content_type) ()

let put_error_status (msg : string) : Httpun.Status.t =
  if String.starts_with ~prefix:"payload too large" msg then
    `Payload_too_large
  else if String.starts_with ~prefix:"disallowed" msg
          || msg = "empty body"
          || String.starts_with ~prefix:"multipart" msg
          || String.starts_with ~prefix:"Content-Type" msg
          || String.starts_with ~prefix:"invalid" msg
          || String.starts_with ~prefix:"content_type" msg
          || String.starts_with ~prefix:"data_b64" msg
          || String.starts_with ~prefix:"JSON" msg
  then `Bad_request
  else `Internal_server_error

let api_media_post (session : Session.t) (reqd : Httpun.Reqd.t) : unit =
  match require_authed_user session with
  | Error () ->
      unauthorized_api reqd;
      ()
  | Ok (author, _) ->
      let req = Httpun.Reqd.request reqd in
      let headers = header_list req in
      let ct_hdr =
        Option.map String.trim (header_ci headers "content-type")
      in
      let cap = request_body_cap ct_hdr in
      let body_res = read_body_limited reqd ~max_bytes:cap in
      (match body_res with
       | Error `Too_large ->
           too_large reqd ~max_bytes:cap;
           ()
       | Ok body ->
           let filename_hdr = header_ci headers "x-filename" in
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
                 | Error msg ->
                     json_response reqd ~status:(put_error_status msg)
                       (`Assoc
                          [ ("ok", `Bool false); ("error", `String msg) ]);
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
           let range_hdr = header_ci req_headers "range" in
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
            txt "Images and PDF, max 5 MiB. Allowed: %s." allowed;
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
            txt "Agents: POST /api/media (raw or JSON) or ";
            HTML.code [] [txt "scripts/pp_media.py put ./shot.png"];
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
        request_body_cap
          (Option.map String.trim
             (header_ci (header_list (Httpun.Reqd.request reqd)) "content-type"))
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
             Option.map String.trim (header_ci headers "content-type")
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
                 | Error msg ->
                     redirect_response reqd
                       ("/media?err=" ^ url_encode msg);
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
