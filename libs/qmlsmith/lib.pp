;; package qmlsmith -- UI prims -> QML for desktop icons
;; Pipeline: ai/ask authors a tiny s-expr UI tree, qml/compile deterministically
;; emits QtQuick, qmlsmith/icon returns JSON {name,qml,launch}.
;; The model NEVER writes QML; the compiler owns the Qt.
;; Spike (2026-09-11), branch admin/personal; promote path: system/main seed.

;; ---------- prompt

(define qmlsmith/prompt
  (string-append
    "You author desktop-icon UI trees for a compiler. Reply with ONLY one "
    "s-expression, nothing else: no markdown, no fences, no prose, no reasoning. "
    "Grammar, one tree:\n"
    "(icon (name \"kebab-name\") (launch \"shell-command-or-empty\") (w 132) (h 132) (bg \"#hex-or-none\")\n"
    "  (rect (x 0) (y 0) (w 40) (h 20) (r 0) (color \"#2E8B57\"))\n"
    "  (circle (cx 20) (cy 20) (rad 8) (color \"white\"))\n"
    "  (text (x 10) (y 118) (s \"label\") (size 10) (color \"#ffffff\") (bold))\n"
    "  (group (x 0) (y 0) ...children...))\n"
    "Node tags: rect circle text group. Property tags: name launch bg w h x y r rad "
    "cx cy color s size bold opacity. Coordinates are pixels from 0,0 top-left of "
    "the icon; children of a group are relative to the group. Draw the described "
    "thing with 6 to 30 nodes; bold shapes, clean colors; include a small label "
    "unless told not to. Keep it compact."))

;; ---------- string helpers (kernel prims only)

(define qmlsmith/first-open
  (lambda (s i)
    "Index of first ( at/after i, or -1."
    (if (not (< i (string-length s)))
        -1
        (if (string=? (substring s i (+ i 1)) "(")
            i
            (qmlsmith/first-open s (+ i 1))))))

(define qmlsmith/scan-close
  (lambda (s i)
    "Index of last ) scanning left from i, or -1."
    (if (< i 0)
        -1
        (if (string=? (substring s i (+ i 1)) ")")
            i
            (qmlsmith/scan-close s (- i 1))))))

(define qmlsmith/trim
  (lambda (s)
    "Cut s down to its outermost (...) s-expression, tolerating prose."
    (let ((a (qmlsmith/first-open s 0)))
      (if (< a 0)
          s
          (let ((b (qmlsmith/scan-close s (- (string-length s) 1))))
            (if (< b a)
                s
                (substring s a (+ b 1))))))))

(define qmlsmith/jstr
  (lambda (s)
    "s as a JSON string literal (json-emit of a string)."
    (json-emit s)))

;; ---------- tree access

(define qmlsmith/prop?
  (lambda (k)
    "True if k is a property tag (pair car), false for node tags."
    (or
      (string=? k "name") (string=? k "launch") (string=? k "bg")
      (string=? k "w") (string=? k "h") (string=? k "x") (string=? k "y")
      (string=? k "r") (string=? k "rad") (string=? k "cx") (string=? k "cy")
      (string=? k "color") (string=? k "s") (string=? k "size")
      (string=? k "bold") (string=? k "opacity")
      (string=? k "eval") (string=? k "tcolor"))))

(define qmlsmith/prop-get
  (lambda (pairs key default)
    "Value of the first (key value) pair in pairs, else default."
    (if (null? pairs)
        default
        (if (and (not (null? (car pairs)))
                 (string=? (as-str (car (car pairs))) key))
            (car (cdr (car pairs)))
            (qmlsmith/prop-get (cdr pairs) key default)))))

(define qmlsmith/has?
  (lambda (pairs key)
    "True if pairs contains (key ...)."
    (if (null? pairs)
        #f
        (if (and (not (null? (car pairs)))
                 (string=? (as-str (car (car pairs))) key))
            #t
            (qmlsmith/has? (cdr pairs) key)))))

;; ---------- emitter

(define qmlsmith/num
  (lambda (pairs key default)
    "Numeric property, compiled (as-str of the raw value)."
    (as-str (qmlsmith/prop-get pairs key default))))

(define qmlsmith/emit-children
  (lambda (children ind)
    "Emit child nodes, each on its own indented line."
    (if (null? children)
        ""
        (string-append
          (qmlsmith/emit-node (car children) ind)
          (qmlsmith/emit-children (cdr children) ind)))))

(define qmlsmith/emit-node
  (lambda (node ind)
    "One node -> its QML block."
    (if (null? node)
        ""
        (let ((tag (as-str (car node))))
          (let ((body (cdr node)))
            (let ((props (qmlsmith/props-only body)))
              (let ((kids (qmlsmith/kids-only body)))
                (qmlsmith/emit-tag tag props kids ind))))))))

(define qmlsmith/props-only
  (lambda (body)
    "The property pairs of a node body."
    (if (null? body)
        (list)
        (if (and (not (null? (car body)))
                 (not (null? (car (car body))))
                 (qmlsmith/prop? (as-str (car (car body)))))
            (cons (car body) (qmlsmith/props-only (cdr body)))
            (qmlsmith/props-only (cdr body))))))

(define qmlsmith/kids-only
  (lambda (body)
    "The child nodes of a node body."
    (if (null? body)
        (list)
        (if (and (not (null? (car body)))
                 (not (null? (car (car body))))
                 (not (qmlsmith/prop? (as-str (car (car body))))))
            (cons (car body) (qmlsmith/kids-only (cdr body)))
            (qmlsmith/kids-only (cdr body))))))

(define qmlsmith/emit-tag*
  (lambda (tag props kids ind)
    "Dispatch one node to QML by tag."
    (if (string=? tag "rect")
        (string-append
          ind "Rectangle { x: " (qmlsmith/num props "x" 0)
          "; y: " (qmlsmith/num props "y" 0)
          "; width: " (qmlsmith/num props "w" 16)
          "; height: " (qmlsmith/num props "h" 16)
          "; radius: " (qmlsmith/num props "r" 0)
          "; color: " (json-emit (as-str (qmlsmith/prop-get props "color" "#888888")))
          " }\n")
        (if (string=? tag "circle")
            (string-append
              ind "Rectangle { x: " (as-str (- (qmlsmith/prop-get props "cx" 16) (qmlsmith/prop-get props "rad" 8)))
              "; y: " (as-str (- (qmlsmith/prop-get props "cy" 16) (qmlsmith/prop-get props "rad" 8)))
              "; width: " (as-str (* 2 (qmlsmith/prop-get props "rad" 8)))
              "; height: " (as-str (* 2 (qmlsmith/prop-get props "rad" 8)))
              "; radius: " (qmlsmith/num props "rad" 8)
              "; color: " (json-emit (as-str (qmlsmith/prop-get props "color" "#888888")))
              " }\n")
            (if (string=? tag "text")
                (string-append
                  ind "Text { x: " (qmlsmith/num props "x" 0)
                  "; y: " (qmlsmith/num props "y" 0)
                  "; text: " (json-emit (as-str (qmlsmith/prop-get props "s" "")))
                  "; color: " (json-emit (as-str (qmlsmith/prop-get props "color" "#eeeeee")))
                  "; font.pixelSize: " (qmlsmith/num props "size" 10)
                  (if (qmlsmith/has? props "bold") "; font.bold: true" "")
                  " }\n")
                (if (string=? tag "group")
                    (string-append
                      ind "Item {\n"
                      ind "  x: " (qmlsmith/num props "x" 0) "\n"
                      ind "  y: " (qmlsmith/num props "y" 0) "\n"
                      (qmlsmith/emit-children kids (string-append ind "  "))
                      ind "}\n")
                    (if (string=? tag "icon")
                        (string-append
                          ind "Rectangle {\n"
                          ind "  width: " (qmlsmith/num props "w" 132) "\n"
                          ind "  height: " (qmlsmith/num props "h" 132) "\n"
                          ind "  color: " (json-emit (as-str (qmlsmith/prop-get props "bg" "transparent"))) "\n"
                          (qmlsmith/emit-children kids (string-append ind "  "))
                          ind "}\n")
                        (error "qml/compile: unknown tag" tag))))))))

;; ---------- compiler + one-shot icon

(define qml/compile
  (lambda (tree)
    "UI prim tree -> QML source. tree is quoted data, head must be icon."
    (if (or (null? tree) (not (string=? (as-str (car tree)) "icon")))
        (error "qml/compile: tree must start with (icon ...)")
        (let ((props (qmlsmith/props-only (cdr tree))))
          (let ((kids (qmlsmith/kids-only (cdr tree))))
            (string-append
              "import QtQuick\n"
              "// name: " (as-str (qmlsmith/prop-get props "name" "icon")) "\n"
              "// launch: " (as-str (qmlsmith/prop-get props "launch" "")) "\n"
              (qmlsmith/emit-tag "icon" props kids "")))))))

(define qmlsmith/icon
  (lambda (desc)
    "Describe an icon -> JSON {name,qml,launch}: ai authors the tree, qml/compile emits QML."
    (let ((raw (ai/ask (string-append qmlsmith/prompt "\n\nDescription: " desc))))
      (let ((tree (read-string (qmlsmith/trim raw))))
        (let ((props (qmlsmith/props-only (cdr tree))))
          (let ((qml (qml/compile tree)))
              (string-append
                "{\"name\":"
                (qmlsmith/jstr (as-str (qmlsmith/prop-get props "name" "icon")))
                ",\"qml\":"
                (qmlsmith/jstr qml)
                ",\"launch\":"
                (qmlsmith/jstr (as-str (qmlsmith/prop-get props "launch" "")))
                "}")))))))

;; ---------- live rendering (prims render QML like they render HTML)

(define qmlsmith/emit-tag
  (lambda (tag props kids ind)
    "Dispatcher; button carries its eval in objectName for the host to wire."
    (if (string=? tag "button")
        (string-append
          ind "Rectangle { objectName: "
          (json-emit (string-append "act|" (as-str (qmlsmith/prop-get props "eval" ""))))
          "; x: " (qmlsmith/num props "x" 0)
          "; y: " (qmlsmith/num props "y" 0)
          "; width: " (qmlsmith/num props "w" 40)
          "; height: " (qmlsmith/num props "h" 20)
          "; radius: " (qmlsmith/num props "r" 6)
          "; color: " (json-emit (as-str (qmlsmith/prop-get props "color" "#7f5af0")))
          "\n"
          ind "  Text { anchors.centerIn: parent; text: "
          (json-emit (as-str (qmlsmith/prop-get props "s" "ok")))
          "; color: "
          (json-emit (as-str (qmlsmith/prop-get props "tcolor" "#ffffff")))
          "; font.pixelSize: " (qmlsmith/num props "size" 10)
          "; font.bold: true }\n"
          ind "}\n")
        (qmlsmith/emit-tag* tag props kids ind))))

(define qmlsmith/bump
  (lambda ()
    "Increment the shared qml-counter row; returns the new count."
    (let ((r (rows "qml-counter")))
      (if (null? r)
          (begin (create "qml-counter" "{\"count\": 1}") 1)
          (let ((id (as-str (car (car r)))))
            (let ((n (dict-get (cdr (car r)) "count")))
              (let ((n2 (+ (if (null? n) 0 (parse-int (as-str n))) 1)))
                (begin
                  (update "qml-counter" id (string-append "{\"count\": " (as-str n2) "}"))
                  n2))))))))

(define qmlsmith/render
  (lambda (fn)
    "Call a widget function (returns an icon tree) -> live QML JSON."
    (let ((tree (fn)))
      (let ((props (qmlsmith/props-only (cdr tree))))
        (let ((qml (qml/compile tree)))
          (string-append
            "{\"name\":"
            (qmlsmith/jstr (as-str (qmlsmith/prop-get props "name" "widget")))
            ",\"qml\":"
            (qmlsmith/jstr qml)
            ",\"actions\":[]"
            "}"))))))

(define qml/demo
  (lambda ()
    "Live demo widget: image identity + shared Postgres counter + bump button."
    (let ((r (rows "qml-counter")))
      (let ((n (if (null? r) 0 (dict-get (cdr (car r)) "count"))))
        (let ((n (if (null? n) 0 n)))
          (list
            (quote icon)
            (list (quote name) "demo")
            (list (quote w) 224)
            (list (quote h) 104)
            (list (quote bg) "transparent")
            (list (quote rect) (list (quote x) 0) (list (quote y) 0) (list (quote w) 224) (list (quote h) 104) (list (quote r) 14) (list (quote color) "#12161f"))
            (list (quote text) (list (quote x) 14) (list (quote y) 10) (list (quote s) "pricklypear") (list (quote size) 11) (list (quote color) "#e8e3d5") (list (quote bold)))
            (list (quote text) (list (quote x) 14) (list (quote y) 30) (list (quote s) (string-append (whoami) " @ " (as-str (current-branch)))) (list (quote size) 9) (list (quote color) "#8b93a7"))
            (list (quote text) (list (quote x) 14) (list (quote y) 50) (list (quote s) (string-append "bumps: " (as-str n))) (list (quote size) 10) (list (quote color) "#c9d1e0"))
            (list (quote button) (list (quote x) 168) (list (quote y) 62) (list (quote w) 42) (list (quote h) 24) (list (quote s) "+1") (list (quote eval) "(qmlsmith/bump)") (list (quote color) "#7f5af0"))
            (list (quote text) (list (quote x) 14) (list (quote y) 78) (list (quote s) "rendered by qml/compile prims") (list (quote size) 8) (list (quote color) "#6b7280"))))))))

;; ---------- backend 2: html from the same prims

(define qmlsmith/hchar
  (lambda (c)
    "HTML-escape one char where needed."
    (if (string=? c "&") "&amp;"
        (if (string=? c "<") "&lt;"
            (if (string=? c ">") "&gt;" c)))))

(define qmlsmith/hesc
  (lambda (s)
    "Escape a string for html text context."
    (if (<= (string-length s) 0)
        ""
        (string-append
          (qmlsmith/hchar (substring s 0 1))
          (qmlsmith/hesc (substring s 1 (string-length s)))))))

(define qmlsmith/html-children
  (lambda (kids)
    (if (null? kids)
        ""
        (string-append
          (qmlsmith/html-node (car kids))
          (qmlsmith/html-children (cdr kids))))))

(define qmlsmith/html-node
  (lambda (node)
    (if (null? node)
        ""
        (let ((tag (as-str (car node))))
          (let ((props (qmlsmith/props-only (cdr node))))
            (let ((kids (qmlsmith/kids-only (cdr node))))
              (qmlsmith/html-tag tag props kids)))))))

(define qmlsmith/html-tag
  (lambda (tag props kids)
    (if (string=? tag "rect")
        (string-append
          "<div style=\"position:absolute;"
          "left:" (qmlsmith/num props "x" 0) "px;top:" (qmlsmith/num props "y" 0) "px;"
          "width:" (qmlsmith/num props "w" 16) "px;height:" (qmlsmith/num props "h" 16) "px;"
          "border-radius:" (qmlsmith/num props "r" 0) "px;"
          "background:" (as-str (qmlsmith/prop-get props "color" "#888888")) ";\">"
          (qmlsmith/html-children kids)
          "</div>")
        (if (string=? tag "circle")
            (let ((rad (qmlsmith/prop-get props "rad" 8)))
              (string-append
                "<div style=\"position:absolute;"
                "left:" (as-str (- (qmlsmith/prop-get props "cx" 16) rad)) "px;top:" (as-str (- (qmlsmith/prop-get props "cy" 16) rad)) "px;"
                "width:" (as-str (* 2 rad)) "px;height:" (as-str (* 2 rad)) "px;"
                "border-radius:50%;"
                "background:" (as-str (qmlsmith/prop-get props "color" "#888888")) ";\">"
                (qmlsmith/html-children kids)
                "</div>"))
            (if (string=? tag "text")
                (string-append
                  "<span style=\"position:absolute;left:" (qmlsmith/num props "x" 0) "px;top:" (qmlsmith/num props "y" 0) "px;"
                  "font-size:" (qmlsmith/num props "size" 10) "px;"
                  (if (qmlsmith/has? props "bold") "font-weight:700;" "")
                  "color:" (as-str (qmlsmith/prop-get props "color" "#eeeeee")) ";\">"
                  (qmlsmith/hesc (as-str (qmlsmith/prop-get props "s" "")))
                  "</span>")
                (if (string=? tag "group")
                    (string-append
                      "<div style=\"position:absolute;left:" (qmlsmith/num props "x" 0) "px;top:" (qmlsmith/num props "y" 0) "px;\">"
                      (qmlsmith/html-children kids)
                      "</div>")
                    (if (string=? tag "button")
                        (string-append
                          "<button data-eval="
                          (json-emit (as-str (qmlsmith/prop-get props "eval" "")))
                          " style=\"position:absolute;left:" (qmlsmith/num props "x" 0) "px;top:" (qmlsmith/num props "y" 0) "px;"
                          "width:" (qmlsmith/num props "w" 40) "px;height:" (qmlsmith/num props "h" 20) "px;"
                          "border-radius:" (qmlsmith/num props "r" 6) "px;border:none;cursor:pointer;"
                          "background:" (as-str (qmlsmith/prop-get props "color" "#7f5af0")) ";"
                          "color:" (as-str (qmlsmith/prop-get props "tcolor" "#ffffff")) ";"
                          "font-size:" (qmlsmith/num props "size" 10) "px;font-weight:700;\">"
                          (qmlsmith/hesc (as-str (qmlsmith/prop-get props "s" "ok")))
                          "</button>")
                        (if (string=? tag "icon")
                            (string-append
                              "<div class=\"uix-view\" style=\"position:relative;width:" (qmlsmith/num props "w" 132) "px;height:" (qmlsmith/num props "h" 132) "px;"
                              "background:" (as-str (qmlsmith/prop-get props "bg" "transparent")) ";"
                              "font-family:system-ui,sans-serif;\">"
                              (qmlsmith/html-children kids)
                              "</div>")
                            (error "qml/html: unknown tag" tag)))))))))

(define qmlsmith/html
  (lambda (tree)
    "UI prim tree -> standalone html (backend 2)."
    (let ((props (qmlsmith/props-only (cdr tree))))
      (let ((kids (qmlsmith/kids-only (cdr tree))))
        (string-append
          "<!doctype html><meta charset=\"utf-8\">\n"
          "<title>" (qmlsmith/hesc (as-str (qmlsmith/prop-get props "name" "widget"))) "</title>\n"
          "<body style=\"margin:0;background:#0b0e14;display:grid;place-items:center;min-height:100vh\">\n"
          (qmlsmith/html-tag "icon" props kids)
          "\n</body>\n")))))

(define uix/html
  (lambda (fn)
    "Widget function -> html string."
    (qmlsmith/html (fn))))
