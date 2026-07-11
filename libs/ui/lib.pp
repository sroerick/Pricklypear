;; Editable UI shell — Functions package "ui"
;; (load-library "ui") or edit ui/nav and ui/home in the browser.

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
        "<a href=\"/agent\">agent</a>"
        "<a href=\"/todos\">todos</a>"))))

(define ui/home
  (lambda ()
    (render-html
      (string-append
        "<div class=\"card\">"
        "<h2>Workspace</h2>"
        "<p>Homepage is Lisp function <code>ui/home</code>. "
        "Edit under Functions / package <strong>ui</strong>.</p>"
        "<ul>"
        "<li><a href=\"/about\">About</a></li>"
        "<li><a href=\"/bindings\">Functions</a></li>"
        "<li><a href=\"/pages\">Pages</a></li>"
        "<li><a href=\"/todos\">Todos</a></li>"
        "<li><a href=\"/repl\">REPL</a></li>"
        "</ul></div>"
        "<div class=\"card\">"
        "<h2>Customize</h2>"
        "<p><a href=\"/bindings?pkg=ui\">ui package</a> · "
        "<a href=\"/edit/ui%2Fhome\">edit homepage</a> · "
        "<a href=\"/edit/ui%2Fnav\">edit navbar</a></p>"
        "<p class=\"muted\">Brand/user/branch chrome stays system; nav and home body are Lisp.</p>"
        "</div>"))))
