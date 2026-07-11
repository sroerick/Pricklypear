;; Editable UI shell — override these and reload with (load-library "ui")
;; or edit in Functions → package ui.

(define ui/nav
  (lambda ()
    (render-html
      (string-append
        "<a href=\"/\">home</a>"
        "<a href=\"/about\">about</a>"
        "<a href=\"/branches\">branches</a>"
        "<a href=\"/bindings\">functions</a>"
        "<a href=\"/types\">data</a>"
        "<a href=\"/pages\">pages</a>"
        "<a href=\"/repl\">repl</a>"
        "<a href=\"/todos\">todos</a>"))))

(define ui/home
  (lambda ()
    (render-html
      (string-append
        "<div class=\"card\">"
        "<h2>Workspace</h2>"
        "<p>This homepage is the Lisp function <code>ui/home</code>. "
        "Edit it under Functions → <strong>ui</strong>.</p>"
        "<ul>"
        "<li><a href=\"/about\">About / concepts</a></li>"
        "<li><a href=\"/bindings\">Functions</a> (browse kernel + packages)</li>"
        "<li><a href=\"/pages\">Pages</a></li>"
        "<li><a href=\"/todos\">Todos</a></li>"
        "<li><a href=\"/repl\">REPL</a></li>"
        "</ul>"
        "</div>"
        "<div class=\"card\">"
        "<h2>Customize</h2>"
        "<p><a href=\"/bindings?pkg=ui\">Edit ui package</a> · "
        "<a href=\"/edit/ui%2Fhome\">Edit homepage</a> · "
        "<a href=\"/edit/ui%2Fnav\">Edit navbar</a></p>"
        "<p class=\"muted\">System chrome (user/branch/uptime) stays fixed; "
        "nav links and home body are yours.</p>"
        "</div>"))))
