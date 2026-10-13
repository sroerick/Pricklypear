(** Public release: read-only source browser, CodeMirror assets,
    book.pdf + src.tgz from PP_PUBLIC_DIR.

    See product-experience.public-release. *)
open Pure_html
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
      if Sys.file_exists a then a else "image/static"

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

(* percent/query decoding lives in Http_kit (was pct_decode/query_param here,
   hand-rolled a second time). *)
let pct_decode = Http_kit.pct_decode
let query_param = Http_kit.query_param

let pct_encode (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' | '/') as c ->
          Buffer.add_char buf c
      | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

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

let rec tree_ul (nodes : tree_node list) ~(active : string) : Pure_html.node =
  (* Source-tree navigation as a pure-html tree (was a Buffer-assembled
     HTML string with hand escaping). *)
  HTML.ul [HTML.class_ "code-tree"]
    (List.filter_map
       (function
         | Dir (name, kids) ->
             Some
               (HTML.li [HTML.class_ "code-dir"]
                  [ HTML.details [HTML.open_]
                      [ HTML.summary [] [txt "%s" name]
                      ; tree_ul kids ~active ]])
         | File (name, rel) ->
             let cls =
               if rel = active then "code-file active" else "code-file"
             in
             Some
               (HTML.li [HTML.class_ "%s" cls]
                  [ HTML.a [HTML.href "/code?f=%s" (pct_encode rel)]
                      [txt "%s" name] ]))
       nodes)

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
  let missing_page (msg_nodes : Pure_html.node list) : unit =
    let doc =
      HTML.html []
        [ HTML.head [] [HTML.title [] "missing"]
        ; HTML.body [] msg_nodes
        ]
    in
    serve_bytes reqd ~status:`Not_found ~content_type:"text/html; charset=utf-8"
      (Pure_html.to_string doc)
  in
  match resolve_under ~root name with
  | None ->
      missing_page
        [ HTML.p []
            [ txt "Artifact "; HTML.code [] [txt "%s" name]
            ; txt " not installed. Run "
            ; HTML.code [] [txt "scripts/public_artifacts.sh"]
            ; txt " and set "; HTML.code [] [txt "PP_PUBLIC_DIR"]; txt "."
            ]
        ; HTML.p [] [HTML.a [HTML.href "/welcome"] [txt "welcome"]]
        ]
  | Some path ->
      if Sys.file_exists path && not (Sys.is_directory path) then
        serve_file_path reqd ~max:max_public_artifact_bytes ~path
      else
        missing_page
          [ HTML.p []
              [ txt "No "; HTML.code [] [txt "%s" name]
              ; txt " in "; HTML.code [] [txt "%s" root]; txt ". Run "
              ; HTML.code [] [txt "scripts/public_artifacts.sh"]; txt "."
              ]
          ; HTML.p []
              [ HTML.a [HTML.href "/welcome"] [txt "welcome"]
              ; txt " · "
              ; HTML.a [HTML.href "/code"] [txt "code"]
              ]
          ]

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
  (* The whole document as a pure-html tree (was one sprintf template
     with hand-escaped interpolations — a third document-assembly path
     beside html_response and the login page). *)
  let doc =
    HTML.html [HTML.lang "en"]
      [ HTML.head []
          [ HTML.meta [HTML.charset "utf-8"]
          ; HTML.meta [HTML.name "viewport";
                       HTML.content "width=device-width, initial-scale=1"]
          ; HTML.title [] "Pricklypear — %s" title
          ; HTML.link [HTML.rel "stylesheet";
                       HTML.href "/assets/cm/codemirror.min.css"]
          ; HTML.style [] "%s" code_css
          ]
      ; HTML.body []
          [ HTML.header [HTML.class_ "code-hdr"]
              [ HTML.div []
                  [ HTML.strong [] [txt "Pricklypear source"]
                  ; txt " · read-only" ]
              ; HTML.nav []
                  [ HTML.a [HTML.href "/welcome"] [txt "Welcome"]
                  ; txt " · "
                  ; HTML.a [HTML.href "/book.pdf"] [txt "Book PDF"]
                  ; txt " · "
                  ; HTML.a [HTML.href "/src.tgz"] [txt ".tgz"]
                  ; txt " · "
                  ; HTML.a [HTML.href "/login"] [txt "Log in"]
                  ]
              ]
          ; HTML.div [HTML.class_ "code-wrap"]
              [ HTML.aside [HTML.class_ "code-side"] [tree_ul tree ~active]
              ; HTML.div [HTML.class_ "code-main"]
                  [ HTML.div [HTML.class_ "code-meta"]
                      [ txt "file: "
                      ; HTML.span [HTML.id "code-path"] [txt "%s" active]
                      ; txt " · root: "
                      ; HTML.code [] [txt "%s" root]
                      ]
                  ; HTML.div [HTML.class_ "code-editor"]
                      [HTML.textarea [HTML.id "src"] ""]
                  ]
              ]
          ; HTML.script [HTML.src "/assets/cm/codemirror.min.js"] ""
          ; HTML.script [HTML.src "/assets/cm/mode/mllike/mllike.min.js"] ""
          ; HTML.script
              [HTML.src "/assets/cm/mode/markdown/markdown.min.js"] ""
          ; HTML.script [HTML.src "/assets/cm/mode/scheme.min.js"] ""
          ; HTML.script [] "%s" boot_js
          ]
      ]
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8"
    (Pure_html.to_string doc)

(** Public Lisp pages: body HTML only (no product chrome / private nav).
    [body_html] is raw HTML by contract — the Lisp faces return
    render-html-marked strings — so it is injected unescaped. *)
let serve_public_body (reqd : Httpun.Reqd.t) ?(title = "Pricklypear")
    (body_html : string) : unit =
  let doc =
    HTML.html [HTML.lang "en"]
      [ HTML.head []
          [ HTML.meta [HTML.charset "utf-8"]
          ; HTML.title [] "%s" title
          ; HTML.meta [HTML.name "viewport";
                       HTML.content "width=device-width, initial-scale=1"]
          ; HTML.style []
              {|body{margin:0;font-family:system-ui,-apple-system,sans-serif;background:#faf7f4;color:#1a1a1a;line-height:1.5}
a{color:#000080}
|}
          ]
      ; HTML.body [] [txt ~raw:true "%s" body_html]
      ]
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8"
    (Pure_html.to_string doc)

let fallback_welcome (reqd : Httpun.Reqd.t) : unit =
  let doc =
    HTML.html [HTML.lang "en"]
      [ HTML.head []
          [ HTML.meta [HTML.charset "utf-8"]
          ; HTML.title [] "Pricklypear"
          ; HTML.meta [HTML.name "viewport";
                       HTML.content "width=device-width, initial-scale=1"]
          ; HTML.style []
              {|body{font-family:system-ui,sans-serif;max-width:40rem;margin:2rem auto;padding:0 1rem;line-height:1.5;background:#faf7f4}
a{color:#000080;font-weight:700} h1{letter-spacing:-.02em}
.actions a{display:inline-block;margin:.25rem .35rem .25rem 0;padding:.4rem .75rem;border:2px solid #000;text-decoration:none;color:#000;background:#fff}
|}
          ]
      ; HTML.body []
          [ HTML.h1 [] [txt "Pricklypear"]
          ; HTML.p []
              [txt "A multi-user Lisp OS — live image, Postgres object store, htmx interface."]
          ; HTML.p [HTML.class_ "actions"]
              [ HTML.a [HTML.href "/code"] [txt "Explore the source"]
              ; HTML.a [HTML.href "/book.pdf"] [txt "Book (PDF)"]
              ; HTML.a [HTML.href "/src.tgz"] [txt "Source tarball"]
              ; HTML.a [HTML.href "/login"] [txt "Log in"]
              ]
          ; HTML.p []
              [txt "Lisp welcome page not loaded yet (seed layout + welcome packages)."]
          ]
      ]
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8"
    (Pure_html.to_string doc)

let fallback_book (reqd : Httpun.Reqd.t) : unit =
  let doc =
    HTML.html [HTML.lang "en"]
      [ HTML.head []
          [ HTML.meta [HTML.charset "utf-8"]
          ; HTML.title [] "Pricklypear book"
          ; HTML.meta [HTML.name "viewport";
                       HTML.content "width=device-width, initial-scale=1"]
          ; HTML.style []
              {|body{font-family:system-ui,sans-serif;max-width:40rem;margin:2rem auto;padding:0 1rem;line-height:1.5}
a{font-weight:700}|}
          ]
      ; HTML.body []
          [ HTML.h1 [] [txt "The Pricklypear book"]
          ; HTML.p []
              [ txt "Paginated literate rendering of the codebase via "
              ; HTML.code [] [txt "borge book print"]
              ]
          ; HTML.p []
              [ HTML.a [HTML.href "/book.pdf"] [txt "Download PDF"]
              ; txt " · "
              ; HTML.a [HTML.href "/src.tgz"] [txt "tarball"]
              ; txt " · "
              ; HTML.a [HTML.href "/code"] [txt "browse"]
              ; txt " · "
              ; HTML.a [HTML.href "/welcome"] [txt "welcome"]
              ]
          ]
      ]
  in
  serve_bytes reqd ~content_type:"text/html; charset=utf-8"
    (Pure_html.to_string doc)
