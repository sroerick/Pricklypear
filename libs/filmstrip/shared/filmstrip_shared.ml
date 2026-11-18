(* filmstrip-shared: the ONE strip renderer for Pricklypear and tuna.
   A launcher strip of app tiles; each tile = glyph + name + one-line
   summary + href. Server-rendered plain links (works with JS disabled),
   >= 44px tap targets, no page-level horizontal overflow. Every live
   number carries a bake/live stamp -- never a bare number.

   Consumers feed tiles in (PP's app registry is a tile provider); the
   rendering lives here so a refinement hits both products. *)

type tile = {
  glyph : string;
  name : string;
  summary : string;
  href : string;
  stamp : string; (* "baked" | "live" *)
}

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let css =
  ".fs-strip{display:flex;gap:8px;overflow-x:auto;overflow-y:hidden;\
   -webkit-overflow-scrolling:touch;padding:0;margin:0;list-style:none}\
   .fs-tile{flex:0 0 auto;min-width:44px;min-height:44px;\
   display:flex;flex-direction:column;justify-content:center}\
   .fs-glyph{font-size:1.25rem;line-height:1}\
   .fs-stamp{font-size:.75rem;opacity:.75}"

let tile_html (t : tile) =
  Printf.sprintf
    "<li class=\"fs-tile\"><a class=\"fs-link\" href=\"%s\">\
<span class=\"fs-glyph\" aria-hidden=\"true\">%s</span>\
<span class=\"fs-name\">%s</span><span class=\"fs-summary\">%s</span></a>\
<span class=\"pp-stamp-%s fs-stamp\" data-stamp=\"%s\">%s</span></li>"
    (esc t.href) (esc t.glyph) (esc t.name) (esc t.summary) (esc t.stamp)
    (esc t.stamp) (esc t.stamp)

(* Render the launcher strip. Tiles are plain anchors inside a horizontal
   scroller: at phone width the strip scrolls, the page does not. *)
let render_strip ?(id = "pp-strip") ?(class_ = "fs-strip-launcher") tiles =
  Printf.sprintf
    "<nav id=\"%s\" class=\"%s\" aria-label=\"apps\"><style>%s</style>\
<ul class=\"fs-strip\">%s</ul></nav>"
    (esc id) (esc class_) css
    (String.concat "" (List.map tile_html tiles))

(* ---- recent commits (the monorepo commit-history filmstrip) ----
   Read-only, public and NON-INTERACTIVE: every row is plain text
   (rev, author, date, subject) -- no anchors, no htmx, no forms. Same
   strip geometry as the launcher, so at phone width the strip scrolls
   and the page does not. Consumers feed the rows in (PP reads them from
   the bare repo's git log); the shape lives here so a refinement hits
   both products. *)
type commit = { rev : string; author : string; date : string; subject : string }

let commit_css =
  ".fs-commits{display:flex;gap:10px;overflow-x:auto;overflow-y:hidden;\
   -webkit-overflow-scrolling:touch;padding:0;margin:0;list-style:none}\
   .fs-commit{flex:0 0 auto;display:flex;flex-direction:column;gap:2px;\
   min-width:44px;padding:.25rem .5rem;border:1px solid var(--border,#333);\
   border-radius:6px}\
   .fs-commit-rev{font-size:.75rem;opacity:.75}\
   .fs-commit-subject{font-size:.9rem}\
   .fs-commit-meta{font-size:.7rem;opacity:.6}"

let commit_html (c : commit) =
  Printf.sprintf
    "<li class=\"fs-commit\"><span class=\"fs-commit-rev\">%s</span>\
<span class=\"fs-commit-subject\">%s</span>\
<span class=\"fs-commit-meta\">%s &middot; %s</span></li>"
    (esc c.rev) (esc c.subject) (esc c.author) (esc c.date)

(* Render the commit filmstrip. An empty list renders an empty strip --
   a missing repo is a quiet pane, never a page error. *)
let render_commits ?(id = "pp-commits") ?(class_ = "fs-strip-commits") commits =
  Printf.sprintf
    "<nav id=\"%s\" class=\"%s\" aria-label=\"recent commits\"><style>%s</style>\
<ul class=\"fs-commits\">%s</ul></nav>"
    (esc id) (esc class_) commit_css
    (String.concat "" (List.map commit_html commits))
