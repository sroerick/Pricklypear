(** Public release: read-only source browser, CodeMirror assets,
    book.pdf + src.tgz from PP_PUBLIC_DIR.

    See product-experience.public-release. *)
let env_dir (name : string) (default : string) : string =
  match Sys.getenv_opt name with
  | Some d when String.trim d <> "" -> d
  | _ -> default

let source_root () : string =
  match Sys.getenv_opt "PP_SOURCE_ROOT" with
  | Some d when String.trim d <> "" -> d
  | _ ->
      (match Sys.getenv_opt "PP_ROOT" with
       | Some d when String.trim d <> "" -> d
       | _ ->
           if Sys.file_exists "pricklypear.borg" then "."
           else if Sys.file_exists "../pricklypear.borg" then ".."
           else ".")

let public_dir () : string =
  match Sys.getenv_opt "PP_PUBLIC_DIR" with
  | Some d when String.trim d <> "" -> d
  | _ ->
      let cand = Filename.concat (source_root ()) "public-artifacts" in
      if Sys.file_exists cand then cand else "/tmp/pp-public"

let static_root () : string =
  match Sys.getenv_opt "PP_STATIC_DIR" with
  | Some d when String.trim d <> "" -> d
  | _ ->
      let root = source_root () in
      let a = Filename.concat root "image/static" in
      if Sys.file_exists a then a
      else if Sys.file_exists "image/static" then "image/static"
      else "image/static"

(* Directory holding built browser assets (interp.js from client/ via
   js_of_ocaml). Falls back to the dune build tree under the source root.
   Spec: borge/structural-editing.borg client-eval. *)
let client_dir () : string =
  match Sys.getenv_opt "PP_CLIENT_DIR" with
  | Some d when String.trim d <> "" -> d
  | _ -> Filename.concat (source_root ()) "_build/default/client/bin"

let skip_dirs : string list =
  [ "_build"
  ; ".git"
  ; ".pi"
  ; ".ralph"
  ; "node_modules"
  ; "_opam"
  ; ".opamswitch"
  ; "spikes"
  ; "supervisor"
  ; ".borge-bugs"
  ; "public-artifacts"
  ; "_build_test"
  ; "backups"
  ; "log"
  ]

let allowed_ext (name : string) : bool =
  let lower = String.lowercase_ascii name in
  if lower = "dune" || lower = "dune-project" || lower = "pricklypear" then true
  else
    List.exists
      (fun suf -> Filename.check_suffix lower suf)
      [ ".borg"
      ; ".ml"
      ; ".mli"
      ; ".pp"
      ; ".md"
      ; ".sql"
      ; ".sh"
      ; ".ksh"
      ; ".py"
      ; ".dune"
      ; ".txt"
      ; ".opam"
      ; ".example"
      ]

let max_file_bytes = 2 * 1024 * 1024

(** Release artifacts (src.tgz ~2.25 MiB, book.pdf ~1.1 MiB) exceed the
    /code browser's 2 MiB cap. Give the public-artifact path its own
    larger bound so [/src.tgz] and [/book.pdf] actually serve instead
    of 404ing with "file too large". Still bounded to prevent abuse. *)
let max_public_artifact_bytes = 64 * 1024 * 1024

let html_escape (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let pct_encode (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' | '/') as c ->
          Buffer.add_char buf c
      | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

let hex_nibble (c : char) : int option =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let pct_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '%' && i + 2 < len then (
      match hex_nibble s.[i + 1], hex_nibble s.[i + 2] with
      | Some a, Some b ->
          Buffer.add_char buf (Char.chr ((a lsl 4) lor b));
          loop (i + 3)
      | _ ->
          Buffer.add_char buf s.[i];
          loop (i + 1))
    else if s.[i] = '+' then (
      Buffer.add_char buf ' ';
      loop (i + 1))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

let realpath_opt (path : string) : string option =
  try Some (Unix.realpath path) with
  | Unix.Unix_error _ | Sys_error _ -> None

(** Strip leading slashes; reject .. and empty segments.
    Callers should pct-decode first so %2e%2e cannot sneak through. *)
let normalize_rel (rel : string) : string option =
  let rel =
    let len = String.length rel in
    if len > 0 && rel.[0] = '/' then String.sub rel 1 (len - 1) else rel
  in
  if rel = "" then Some ""
  else
    let parts = String.split_on_char '/' rel in
    let ok =
      List.for_all
        (fun p ->
          p <> "" && p <> "." && p <> ".." && not (String.contains p '\000'))
        parts
    in
    if not ok then None else Some (String.concat "/" parts)

(** Resolve [rel] under [root]. Does not require the target to exist
    (realpath on missing files fails); safety is normalize_rel forbidding
    ".." plus realpath on the root only. *)
let resolve_under ~(root : string) (rel : string) : string option =
  match realpath_opt root with
  | None -> None
  | Some root_real ->
      (match normalize_rel rel with
       | None -> None
       | Some "" -> Some root_real
       | Some rel -> Some (Filename.concat root_real rel))

(** Typed read errors for the public file faces (/code/raw, /assets,
    /book.pdf, /src.tgz — all hard-public). The [Io_error] detail goes
    to the ops log only, never into a response body: unauthenticated
    clients must not see raw OS/exception text with resolved absolute
    paths (same sanitize invariant as http.ml's 500 handler and
    error_handler). i-1788670545-5347dabd. *)
type file_error =
  | Not_found
  | Too_large
  | Io_error of string

let read_file_within ~(max : int) (path : string) :
    (string, file_error) result =
  try
    let st = Unix.stat path in
    if st.Unix.st_kind <> Unix.S_REG then Error Not_found
    else if st.Unix.st_size > max then Error Too_large
    else
      let ic = open_in_bin path in
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      close_in ic;
      Ok s
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error Not_found
  | exn -> Error (Io_error (Printexc.to_string exn))

let read_file_limited (path : string) : (string, file_error) result =
  read_file_within ~max:max_file_bytes path

let mime_for_name (name : string) : string =
  let lower = String.lowercase_ascii name in
  if Filename.check_suffix lower ".css" then "text/css; charset=utf-8"
  else if Filename.check_suffix lower ".js" then
    "application/javascript; charset=utf-8"
  else if Filename.check_suffix lower ".pdf" then "application/pdf"
  else if Filename.check_suffix lower ".tgz"
       || Filename.check_suffix lower ".gz" then "application/gzip"
  else if Filename.check_suffix lower ".svg" then "image/svg+xml"
  else if Filename.check_suffix lower ".png" then "image/png"
  else if Filename.check_suffix lower ".jpg"
       || Filename.check_suffix lower ".jpeg" then "image/jpeg"
  else if Filename.check_suffix lower ".gif" then "image/gif"
  else if Filename.check_suffix lower ".webp" then "image/webp"
  else if Filename.check_suffix lower ".ico" then "image/x-icon"
  else "application/octet-stream"

let cm_mode_for (name : string) : string =
  let lower = String.lowercase_ascii name in
  if Filename.check_suffix lower ".ml"
     || Filename.check_suffix lower ".mli" then "text/x-ocaml"
  else if Filename.check_suffix lower ".md" then "text/x-markdown"
  else "text/plain"

type tree_node =
  | Dir of string * tree_node list
  | File of string * string (* name, rel_path *)

let rec walk_tree ~(root : string) ~(rel : string) : tree_node list =
  let full = if rel = "" then root else Filename.concat root rel in
  let entries =
    try Sys.readdir full |> Array.to_list with Sys_error _ -> []
  in
  let entries = List.sort String.compare entries in
  List.filter_map
    (fun name ->
      if List.mem name skip_dirs then None
      else if String.length name > 0 && name.[0] = '.' then None
      else
        let child_rel = if rel = "" then name else rel ^ "/" ^ name in
        let child_full = Filename.concat full name in
        try
          if Sys.is_directory child_full then
            let kids = walk_tree ~root ~rel:child_rel in
            if kids = [] then None else Some (Dir (name, kids))
          else if allowed_ext name then Some (File (name, child_rel))
          else None
        with Sys_error _ -> None)
    entries

(** Memo for the /code source tree: [walk_tree] is O(the whole source
    tree) and the /code page is hard-public, so every anonymous GET
    re-walked it. The tree is advisory navigation — a short TTL memo
    (keyed by root, ref-based; a concurrent-fiber race is harmless:
    worst case both walk and one result wins) bounds that to one walk
    per minute. Source edits surface within the TTL. *)
let walk_tree_ttl = 60.0

let walk_tree_cache : (string * float * tree_node list) option ref = ref None

let walk_tree_cached ~(root : string) ~(now : float) ~(ttl : float) :
    tree_node list =
  match !walk_tree_cache with
  | Some (r, at, tree) when String.equal r root && now -. at < ttl -> tree
  | _ ->
      let tree = walk_tree ~root ~rel:"" in
      walk_tree_cache := Some (root, now, tree);
      tree

let rec tree_to_html (nodes : tree_node list) ~(active : string) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "<ul class=\"code-tree\">";
  List.iter
    (function
      | Dir (name, kids) ->
          Buffer.add_string buf "<li class=\"code-dir\"><details open>";
          Buffer.add_string buf "<summary>";
          Buffer.add_string buf (html_escape name);
          Buffer.add_string buf "</summary>";
          Buffer.add_string buf (tree_to_html kids ~active);
          Buffer.add_string buf "</details></li>"
      | File (name, rel) ->
          let cls =
            if rel = active then "code-file active" else "code-file"
          in
          let href = "/code?f=" ^ pct_encode rel in
          Buffer.add_string buf (Printf.sprintf "<li class=\"%s\">" cls);
          Buffer.add_string buf
            (Printf.sprintf "<a href=\"%s\">%s</a>" href (html_escape name));
          Buffer.add_string buf "</li>")
    nodes;
  Buffer.add_string buf "</ul>";
  Buffer.contents buf

let code_css : string =
  "body{margin:0;font-family:ui-monospace,Menlo,monospace;background:#faf7f4;color:#1a1a1a}\n\
   header.code-hdr{display:flex;flex-wrap:wrap;gap:.75rem;align-items:center;\n\
   justify-content:space-between;padding:.5rem .75rem;border-bottom:2px solid #000;\n\
   background:#fff}\n\
   header.code-hdr a{color:#000080;font-weight:700}\n\
   .code-wrap{display:flex;min-height:calc(100vh - 3rem)}\n\
   .code-side{width:16rem;min-width:12rem;max-width:40vw;overflow:auto;\n\
   border-right:2px solid #000;background:#e8e2db;padding:.5rem;font-size:.85rem}\n\
   .code-main{flex:1;display:flex;flex-direction:column;min-width:0}\n\
   .code-meta{padding:.35rem .75rem;border-bottom:1px solid #808080;background:#fff;\n\
   font-size:.85rem}\n\
   .code-editor{flex:1;min-height:0}\n\
   .CodeMirror{height:100%;font-size:13px;line-height:1.4}\n\
   ul.code-tree{list-style:none;margin:0;padding-left:.85rem}\n\
   li.code-file a{text-decoration:none;color:#000}\n\
   li.code-file.active a{font-weight:700;background:#fff;padding:0 .2rem}\n\
   li.code-dir>details>summary{cursor:pointer;font-weight:700}\n\
   .code-err{color:#8b0000;padding:1rem}\n\
   @media(max-width:40rem){.code-wrap{flex-direction:column}\n\
   .code-side{width:auto;max-width:none;max-height:30vh;border-right:none;\n\
   border-bottom:2px solid #000}}\n"

let serve_bytes (reqd : Httpun.Reqd.t) ?(status = `OK) ~content_type
    (body : string) : unit =
  let headers =
    Httpun.Headers.of_list
      ([ ("content-type", content_type)
       ; ("content-length", string_of_int (String.length body))
       ; ("cache-control", "public, max-age=3600")
       ] @ Http_core.security_headers ())
  in
  let resp = Httpun.Response.create ~headers status in
  Http_core.safe_respond_with_string reqd resp body

(** One generic-body mapping for every typed read error; the raw detail
    of Io_error is correlated server-side via the ops log. *)
let serve_read_error (reqd : Httpun.Reqd.t) ~(path : string)
    (e : file_error) : unit =
  match e with
  | Not_found ->
      serve_bytes reqd ~status:`Not_found ~content_type:"text/plain; charset=utf-8"
        "not found"
  | Too_large ->
      serve_bytes reqd ~status:`Payload_too_large
        ~content_type:"text/plain; charset=utf-8" "file too large"
  | Io_error detail ->
      ignore
        (Ops_log.error ~component:"code" ~context:[ ("path", path) ]
           ("file read failed: " ^ detail));
      serve_bytes reqd ~status:`Internal_server_error
        ~content_type:"text/plain; charset=utf-8"
        "read error — incident logged"

let serve_file_path ?(max = max_file_bytes) (reqd : Httpun.Reqd.t) ~(path : string) : unit =
  match read_file_within ~max path with
  | Error e -> serve_read_error reqd ~path e
  | Ok body ->
      let ct = mime_for_name (Filename.basename path) in
      serve_bytes reqd ~content_type:ct body

let handle_asset (reqd : Httpun.Reqd.t) ~(rel : string) : unit =
  let rel = pct_decode rel in
  (* Search the committed static dir first, then the built client-asset dir
     (interp.js from client/ via js_of_ocaml). Spec: structural-editing.client-eval. *)
  let try_root root =
    match resolve_under ~root rel with
    | Some path when Sys.file_exists path && not (Sys.is_directory path) ->
        Some path
    | _ -> None
  in
  match try_root (static_root ()) with
  | Some path -> serve_file_path reqd ~path
  | None ->
      (match try_root (client_dir ()) with
       | Some path -> serve_file_path reqd ~path
       | None ->
           serve_bytes reqd ~status:`Not_found ~content_type:"text/plain; charset=utf-8"
             "not found")

let handle_public_artifact (reqd : Httpun.Reqd.t) ~(name : string) : unit =
  let root = public_dir () in
  match resolve_under ~root name with
  | None ->
      serve_bytes reqd ~status:`Not_found ~content_type:"text/html; charset=utf-8"
        ("<!doctype html><title>missing</title><p>Artifact <code>"
         ^ name
         ^ "</code> not installed. Run <code>scripts/public_artifacts.sh</code> \
            and set <code>PP_PUBLIC_DIR</code>.</p><p><a href=\"/welcome\">welcome</a></p>")
  | Some path ->
      if Sys.file_exists path && not (Sys.is_directory path) then
        serve_file_path reqd ~max:max_public_artifact_bytes ~path
      else
        serve_bytes reqd ~status:`Not_found ~content_type:"text/html; charset=utf-8"
          ("<!doctype html><title>missing</title><p>No <code>"
           ^ name
           ^ "</code> in <code>"
           ^ root
           ^ "</code>. Run <code>scripts/public_artifacts.sh</code>.</p>\
              <p><a href=\"/welcome\">welcome</a> · <a href=\"/code\">code</a></p>")

let handle_raw (reqd : Httpun.Reqd.t) ~(rel : string) : unit =
  let root = source_root () in
  let rel = pct_decode rel in
  match resolve_under ~root rel with
  | None ->
      serve_bytes reqd ~status:`Forbidden ~content_type:"text/plain; charset=utf-8"
        "forbidden path"
  | Some path ->
      if not (Sys.file_exists path) then
        serve_bytes reqd ~status:`Not_found ~content_type:"text/plain; charset=utf-8"
          "not found"
      else if Sys.is_directory path then
        serve_bytes reqd ~status:`Bad_request ~content_type:"text/plain; charset=utf-8"
          "is a directory"
      else if not (allowed_ext (Filename.basename path)) then
        serve_bytes reqd ~status:`Forbidden ~content_type:"text/plain; charset=utf-8"
          "extension not allowed"
      else (
        match read_file_limited path with
        | Error e -> serve_read_error reqd ~path e
        | Ok body ->
            serve_bytes reqd ~content_type:"text/plain; charset=utf-8" body)

let query_param (target : string) (key : string) : string option =
  match String.index_opt target '?' with
  | None -> None
  | Some i ->
      let q = String.sub target (i + 1) (String.length target - i - 1) in
      let pairs = String.split_on_char '&' q in
      List.find_map
        (fun pair ->
          match String.split_on_char '=' pair with
          | k :: rest when k = key ->
              let v = String.concat "=" rest in
              Some (pct_decode v)
          | _ -> None)
        pairs

let handle_code_page (reqd : Httpun.Reqd.t) ~(target : string) : unit =
  let root = source_root () in
  let active =
    match query_param target "f" with
    | Some f when f <> "" -> f
    | _ ->
        if Sys.file_exists (Filename.concat root "pricklypear.borg") then
          "pricklypear.borg"
        else ""
  in
  let tree =
    walk_tree_cached ~root ~now:(Unix.gettimeofday ()) ~ttl:walk_tree_ttl
  in
  let tree_html = tree_to_html tree ~active in
  let mode = cm_mode_for active in
  let title =
    if active = "" then "Source" else active
  in
  let boot_js =
    Printf.sprintf
      {|
var MODE=%s;
var FILE=%s;
var ta=document.getElementById('src');
var cm=CodeMirror.fromTextArea(ta,{
  lineNumbers:true,
  readOnly:true,
  mode:MODE,
  lineWrapping:true,
  viewportMargin:40
});
function loadFile(path){
  if(!path){cm.setValue('// pick a file from the tree');return;}
  fetch('/code/raw/'+path.split('/').map(encodeURIComponent).join('/'))
    .then(function(r){if(!r.ok)throw new Error(r.statusText);return r.text();})
    .then(function(t){
      var m='text/plain';
      if(/\.ml$|\.mli$/i.test(path))m='text/x-ocaml';
      else if(/\.md$/i.test(path))m='text/x-markdown';
      cm.setOption('mode',m);
      cm.setValue(t);
      document.getElementById('code-path').textContent=path;
    })
    .catch(function(e){cm.setValue('// error: '+e);});
}
loadFile(FILE);
|}
      (Yojson.Safe.to_string (`String mode))
      (Yojson.Safe.to_string (`String active))
  in
  let html =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>Pricklypear — %s</title>
<link rel="stylesheet" href="/assets/cm/codemirror.min.css"/>
<style>%s</style>
</head>
<body>
<header class="code-hdr">
  <div><strong>Pricklypear source</strong> · read-only</div>
  <nav>
    <a href="/welcome">Welcome</a> ·
    <a href="/book.pdf">Book PDF</a> ·
    <a href="/src.tgz">.tgz</a> ·
    <a href="/login">Log in</a>
  </nav>
</header>
<div class="code-wrap">
  <aside class="code-side">%s</aside>
  <div class="code-main">
    <div class="code-meta">file: <span id="code-path">%s</span> · root: <code>%s</code></div>
    <div class="code-editor"><textarea id="src"></textarea></div>
  </div>
</div>
<script src="/assets/cm/codemirror.min.js"></script>
<script src="/assets/cm/mode/mllike/mllike.min.js"></script>
<script src="/assets/cm/mode/markdown/markdown.min.js"></script>
<script>%s</script>
</body>
</html>|}
      (html_escape title) code_css tree_html (html_escape active)
      (html_escape root) boot_js
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8" html

(** Public Lisp pages: body HTML only (no product chrome / private nav). *)
let serve_public_body (reqd : Httpun.Reqd.t) ?(title = "Pricklypear")
    (body_html : string) : unit =
  let doc =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>%s</title>
<style>
body{margin:0;font-family:system-ui,-apple-system,sans-serif;background:#faf7f4;color:#1a1a1a;line-height:1.5}
a{color:#000080}
</style>
</head>
<body>
%s
</body>
</html>|}
      (html_escape title) body_html
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8" doc

let fallback_welcome (reqd : Httpun.Reqd.t) : unit =
  let body =
    {|<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"/>
<title>Pricklypear</title>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<style>
body{font-family:system-ui,sans-serif;max-width:40rem;margin:2rem auto;padding:0 1rem;line-height:1.5;background:#faf7f4}
a{color:#000080;font-weight:700} h1{letter-spacing:-.02em}
.actions a{display:inline-block;margin:.25rem .35rem .25rem 0;padding:.4rem .75rem;border:2px solid #000;text-decoration:none;color:#000;background:#fff}
</style></head><body>
<h1>Pricklypear</h1>
<p>A multi-user Lisp OS — live image, Postgres object store, htmx interface.</p>
<p class="actions">
<a href="/code">Explore the source</a>
<a href="/book.pdf">Book (PDF)</a>
<a href="/src.tgz">Source tarball</a>
<a href="/login">Log in</a>
</p>
<p>Lisp welcome page not loaded yet (seed layout + welcome packages).</p>
</body></html>|}
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8" body

let fallback_book (reqd : Httpun.Reqd.t) : unit =
  let body =
    {|<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"/>
<title>Pricklypear book</title>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<style>body{font-family:system-ui,sans-serif;max-width:40rem;margin:2rem auto;padding:0 1rem;line-height:1.5}
a{font-weight:700}</style></head><body>
<h1>The Pricklypear book</h1>
<p>Paginated literate rendering of the codebase via <code>borge book print</code>.</p>
<p><a href="/book.pdf">Download PDF</a> · <a href="/src.tgz">tarball</a> ·
<a href="/code">browse</a> · <a href="/welcome">welcome</a></p>
</body></html>|}
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8" body
