(** Http_kit — the HTTP plumbing shared by the two local surfaces
    (agent/lib/http_ui.ml + remote/lib/http_ui.ml, unified by
    i-1788671304-d): HTML escaping for status/error pages only (the
    surfaces themselves are Nopales), body reads, string/html
    responders, the closed-writer guard, query-string + form parsing,
    and vendored-asset serving. The image's Http_kit
    (image/lib/http_kit.ml, i-1788671505-a) is a NAME TWIN for its own
    row/header kit — the two are never linked into one binary. *)

(** HTML escaping (status/error pages only; surfaces are Nopales). *)
let esc (s : string) : string =
  let buf = Buffer.create (String.length s + 16) in
  String.iter
    (fun c ->
      match c with
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let status_page (title : string) (body : string) : string =
  Printf.sprintf
    {|<!doctype html><html><head><meta charset="utf-8"><title>%s</title>
<style>body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,monospace;padding:1.5rem}a{color:#7aa2f7}</style>
</head><body>%s</body></html>|}
    (esc title) body

let read_body (reqd : Httpun.Reqd.t) : string =
  let body = Httpun.Reqd.request_body reqd in
  let promise, resolver = Eio.Promise.create () in
  let buf = Buffer.create 1024 in
  let rec sched () =
    Httpun.Body.Reader.schedule_read body
      ~on_eof:(fun () -> Eio.Promise.resolve resolver (Buffer.contents buf))
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
        sched ())
  in
  sched ();
  Eio.Promise.await promise

let respond_text (reqd : Httpun.Reqd.t) ~(status : Httpun.Status.t)
    ~(content_type : string) ~(body : string)
    ~(relay : (string * string) list) : unit =
  let headers =
    Httpun.Headers.of_list
      (List.map (fun (k, v) -> (k, v)) relay
      @ [
        ("content-type", content_type);
        ("content-length", string_of_int (String.length body));
      ])
  in
  let _body_w =
    Httpun.Reqd.respond_with_string reqd
      (Httpun.Response.create ~headers status)
      body
  in
  ()

let respond_html (reqd : Httpun.Reqd.t) ~(status : Httpun.Status.t)
    ~(body : string) ~(relay : (string * string) list) : unit =
  respond_text reqd ~status ~content_type:"text/html; charset=utf-8" ~relay
    ~body

(** The httpun closed-writer guard: after a peer disconnect, write/flush
    on the Body.Writer raises Failure "cannot write to closed writer"
    (or a prefixed variant). ONE home for the string test — the image
    keeps its own copy in Http_core (never linked here). *)
let is_closed_writer_exn = function
  | Failure msg ->
      String.equal msg "cannot write to closed writer"
      || String.starts_with ~prefix:"cannot write to closed writer" msg
  | _ -> false

let close_writer_quiet (body : Httpun.Body.Writer.t) : unit =
  (try Httpun.Body.Writer.close body
   with exn when is_closed_writer_exn exn -> ())

let url_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec go i =
    if i >= len then Buffer.contents buf
    else if i + 2 < len && s.[i] = '%' then (
      (try
         Buffer.add_char buf
           (Char.chr (int_of_string ("0x" ^ String.sub s (i + 1) 2)))
       with _ -> ());
      go (i + 3))
    else (
      Buffer.add_char buf (if s.[i] = '+' then ' ' else s.[i]);
      go (i + 1))
  in
  go 0

(** Query-string parsing (route params for GET dispatch). *)
let parse_query (q : string) : (string * string) list =
  if q = "" then []
  else
    List.filter_map
      (fun kv ->
        match String.index_opt kv '=' with
        | Some i ->
            let k = String.sub kv 0 i in
            let v = String.sub kv (i + 1) (String.length kv - i - 1) in
            Some (url_decode k, url_decode v)
        | None -> Some (url_decode kv, ""))
      (String.split_on_char '&' q)

(** url-decode a form field value (htmx posts
    application/x-www-form-urlencoded). *)
let form_field (body : string) (name : string) : string =
  let prefix = name ^ "=" in
  let rec find i =
    if i >= String.length body then None
    else if i = 0 || body.[i - 1] = '&' then
      if String.length body - i >= String.length prefix
         && String.sub body i (String.length prefix) = prefix
      then
        let rest_start = i + String.length prefix in
        let rest_end =
          match String.index_from_opt body rest_start '&' with
          | Some j -> j | None -> String.length body
        in
        Some (String.sub body rest_start (rest_end - rest_start))
      else find (i + 1)
    else find (i + 1)
  in
  match find 0 with None -> "" | Some v -> url_decode v

(** Parse a url-encoded form body into a params alist (htmx posts
    application/x-www-form-urlencoded; hx-vals ride the POST body on
    non-GET requests). Parsed names + values are url-decoded, so the
    filmstrip POST routes read them as request params. *)
let parse_form_body (body : string) : (string * string) list =
  List.filter_map
    (fun kv ->
      if kv = "" then None
      else
        match String.index_opt kv '=' with
        | Some i ->
            Some
              ( url_decode (String.sub kv 0 i),
                url_decode (String.sub kv (i + 1) (String.length kv - i - 1)) )
        | None -> Some (url_decode kv, ""))
    (String.split_on_char '&' body)

(** Serve a vendored, baked-into-the-binary asset at /assets/*.js. The
    asset TABLE is host-provided (each host embeds its own Assets
    module): entries are (name, body, content_type). *)
let serve_asset (reqd : Httpun.Reqd.t)
    ~(assets : (string * string * string) list) (name : string) : unit =
  let body, ct =
    match List.find_opt (fun (n, _, _) -> n = name) assets with
    | Some (_, b, ct) -> (b, ct)
    | None -> ("", "application/octet-stream")
  in
  if body = "" then
    respond_html reqd ~status:`Not_found ~relay:[]
      ~body:(status_page "404" (Printf.sprintf "<p>no asset %s</p>" (esc name)))
  else respond_text reqd ~status:`OK ~content_type:ct ~body ~relay:[]
