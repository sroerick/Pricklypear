(** Pure multipart/form-data parser — leaf module (no Session/Http deps).

    Extracted from http_media.ml so both the media routes and the
    (request-multipart) prim in prims_request.ml can use it without a
    Prims_request -> Session -> Http_media -> Prims_request cycle.
    Binary-safe: byte offsets, no decoding. *)
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
