;; package lookbook -- UI VALUE declarations + the projections
;; (borge/filmstrip.borg lookbook stanza, 2026-09 operator decisions).
;;
;; DECLARED STATE ONLY: a lookbook ui prim DECLARES -- it never
;; renders. `(name params)` answers a UI VALUE: a dict
;;   {"t":"el","tag":T,"attrs":{..},"kids":[kid ...]}
;; (kids the JSON array of KID WRAPPERS; a kid is either
;; (ui/text s) == {"t":"text","v":s} or a nested el dict). HTML+htmx
;; is PROJECTION 1 (lookbook/html): ONE shared renderer over the
;; declared value; the indented outline is PROJECTION 2 (lookbook/text)
;; over the SAME value. Terminal (pads) and native renderers are
;; subsequent lowerings of the same declaration -- never a second
;; authoring surface. An element def containing markup strings or
;; render-html is a spec violation. hx-* verbs ride attrs as DECLARED
;; BEHAVIOR DATA: the html projection emits them verbatim; a foreign
;; projection maps or badges known verbs.
;;
;; EXAMPLES ARE THE DECLARATION: `<name>/examples` (an ordinary def)
;; answers the element's CARD:
;;   {"name","doc","formals":[...],"variants":[param dicts ...],
;;    "file","source"} -- self-contained like bind/get doc+source rows.
;; An element with a card inventories in the `ui` rail root; without
;; one it is invisible to the lookbook.
;;
;; GLOCHID EMBED NOTE: no (package ...) form -- the agent boot
;; evaluator has no pp-sync header intercept (see
;; llm/embed_rules.dune.inc for the filmstrip strip-out); habitat sync
;; is pass-two. The eval SEAM (lookbook/eval inside the agent surface,
;; example-eval on the habitat) is host-provided; this file stays pure.
;;
;; DIALECT: defined the filmstrip way -- (define name (lambda (args)
;; ...) "doc") -- so docstrings never become return values (the
;; (define (f x) body "doc") gotcha).

;; ---- flat (k v ...) pairs -> JSON object text. All values strings
;; (the filmstrip dict-set* discipline; the pure catalog has dict-set
;; only). Nested JSON strings stay STRUCTURED through dict-set, so
;; attrs and kids round-trip through json-emit/json-parse without
;; double encoding. ----
(define lb/obj
  (lambda (kvs)
    (if (null? kvs)
        "{}"
        (if (null? (cdr kvs))
            "{}"
            (dict-set (lb/obj (cdr (cdr kvs))) (car kvs) (car (cdr kvs))))))
  "JSON object text from flat (k v ...) pairs.")

;; ---- constructors ----
(define ui/text
  (lambda (s) (lb/obj (list "t" "text" "v" (as-str s))))
  "A declared text kid: {\"t\":\"text\",\"v\":s}.")

(define ui/elm
  (lambda (tag attrs kids)
    (lb/obj
      (list
        "t" "el"
        "tag" (as-str tag)
        "attrs" (lb/obj attrs)
        "kids" (json-emit kids))))
  "A declared element: {\"t\":\"el\",\"tag\":T,\"attrs\":{..},\n\"kids\":[kid ...]}. attrs rides flat (k v ...) pairs, kids a list of\nkid wrappers (ui/text or nested ui/elm dicts). Composite elements\ncall other elements and pass their returned dicts as kids.")

;; ---- projector helpers ----
;; json-values-alist: a parsed ui-value is an ALIST (nested objects are
;; native lists, not JSON texts). The helpers below accept BOTH shapes:
;; legacy VStr JSON (producers still store text) and native alists.
(define lb/get
  (lambda (d k)
    (let ((v (dict-get d k))) (if (null? v) "" (as-str v))))
  "Text at key k; null renders as \"\" (the bind/field pattern). ONLY
for string leaves (t/tag/v); structured fields go through lb/val.")

(define lb/val
  (lambda (d k)
    (let ((v (dict-get d k))) (if (null? v) "" v)))
  "Raw value at key k; null renders as \"\". Structured fields (attrs,
kids) arrive as alists under json-values-alist, as JSON text in legacy
rows.")

(define lb/field-list
  (lambda (d k)
    (let ((v (lb/val d k)))
      (if (null? v)
          (list)
          (if (string? v)
              (if (string-eq v "") (list) (json-parse v))
              v))))
  "List at key k: native VList (json-values-alist) returned as-is,
legacy JSON array text parsed; object items arrive as alists.")

(define lb/attr-text
  (lambda (d k)
    (let ((v (dict-get d k)))
      (if (null? v)
          ""
          (if (string? v) v (json-emit v)))))
  "Attr value as text: strings ride verbatim; structured values (an
hx-vals JSON object rides the wire structured through the emit
heuristic, so json-parse hands back an alist) re-emit as JSON text —
the exact text the legacy VStr path produced.")

(define lb/html-attrs
  (lambda (attrs)
    (if (null? attrs)
        ""
        (if (string? attrs)
            (if (string-eq attrs "") "" (lb/html-attrs (json-parse attrs)))
            (html/attrs
              (list/map
                (lambda (k) (html/attr k (lb/attr-text attrs k)))
                (dict-keys attrs))))))
  "PROJECTION helper: attrs (alist or legacy JSON object text) ->
rendered attrs string (keys ship in dict-keys order).")

(define lb/kid-html
  (lambda (kid)
    (if (string-eq (lb/get kid "t") "text")
        (html/text (lb/get kid "v"))
        (lookbook/html kid)))
  "One kid: text escapes, elements recurse.")

;; the ONE markup site
(define lookbook/html
  (lambda (w)
    (if (string-eq (lb/get w "t") "text")
        (html/text (lb/get w "v"))
        (let
          ((kids (lb/field-list w "kids")))
          (html/tag
            (lb/get w "tag")
            (lb/html-attrs (lb/val w "attrs"))
            (if (null? kids)
                ""
                (html/join (list/map lb/kid-html kids)))))))
  "PROJECTION 1 (html/htmx): one declared ui-value (el or text
wrapper) -> a fragment string. Every value escapes through html/*;
hx-* attrs emit verbatim. The ONLY place the shell renders element
markup.")

;; ---- PROJECTION 2 (plain text outline): the falsifiable portability
;; claim -- two renderers, one value, no second authoring path ----
(define lb/pad
  (lambda (n out)
    (if (< n 1)
        out
        (lb/pad (- n 1) (string-append out "  "))))
  "Two-space indent accumulator.")

(define lb/attr-sum
  (lambda (attrs)
    (if (null? attrs)
        ""
        (if (string? attrs)
            (if (string-eq attrs "") "" (lb/attr-sum (json-parse attrs)))
            (list/foldl
              (lambda (acc k)
                (string-append acc " " k "=\"" (lb/attr-text attrs k) "\""))
              ""
              (dict-keys attrs)))))
  "One-line attr summary: k=\"v\" per key (dict-keys order); alist or
legacy JSON object text.")

(define lookbook/text
  (lambda (w depth)
    (let*
      ((body
         (if (string-eq (lb/get w "t") "text")
             (string-append (lb/pad depth "") "text: " (lb/get w "v") "\n")
             (string-append
               (lb/pad depth "")
               (lb/get w "tag")
               (lb/attr-sum (lb/val w "attrs"))
               "\n"))))
      (if (string-eq (lb/get w "t") "text")
          body
          (lb/text-kids (lb/field-list w "kids") (+ depth 1) body))))
  "PROJECTION 2 (plain text): the declared value as an indented
outline, never markup.")

(define lb/text-kids
  (lambda (kids depth out)
    (if (null? kids)
        out
        (lb/text-walk kids depth out)))
  "Kid rows appended in order (the outline walk).")

(define lb/text-walk
  (lambda (kids depth out)
    (if (null? kids)
        out
        (lb/text-walk
          (cdr kids)
          depth
          (string-append out (lookbook/text (car kids) depth)))))
  "One kid row, then the rest.")

;; ---- seed elements (declared in the lib so both hosts share them;
;; hosts add their own surface elements by redefining
;; lookbook/elements) ----

(define ui/button
  (lambda (label kind)
    (ui/elm
      "button"
      (list
        "class" (string-append "lb-btn lb-btn-" kind)
        "type" "button")
      (list (ui/text label))))
  "One kind-preset button: label text, the kind rides the class.")

(define ui/badge
  (lambda (text tone)
    (ui/elm
      "span"
      (list "class" (string-append "lb-badge lb-badge-" tone))
      (list (ui/text text))))
  "One inline badge: text, tone class.")

(define ui/card
  (lambda (title badge-text body)
    (ui/elm
      "section"
      (list "class" "lb-card")
      (list
        (ui/elm
          "h3"
          (list)
          (list (ui/text title) (ui/badge badge-text "ok")))
        (ui/elm "p" (list) (list (ui/text body))))))
  "Card element: composes ui/badge INSIDE its title row -- reuse is a
def call, the composed value nests as kids (the reusability proof).")

;; ---- examples cards: the declaration (doc + formals + variants +
;; source; the card shape mirrors bind/get's doc+source row) ----

(define ui/button/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/button"
        "doc" "One kind-preset button (seed lookbook element)."
        "file" "libs/lookbook/lib.pp"
        "formals" (json-emit (list "label" "kind"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "label" "Save" "kind" "primary"))
                       (lb/obj (list "label" "Delete" "kind" "danger"))))
        "source"
        "(ui/button label kind) ->\n  (ui/elm \"button\"\n    (list \"class\" (string-append \"lb-btn lb-btn-\" kind)\n          \"type\" \"button\")\n    (list (ui/text label)))")))
  "The ui/button card: this def IS the element's declaration.")

(define ui/badge/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/badge"
        "doc" "One inline badge (seed lookbook element)."
        "file" "libs/lookbook/lib.pp"
        "formals" (json-emit (list "text" "tone"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "text" "stable" "tone" "ok"))
                       (lb/obj (list "text" "beta" "tone" "warn"))))
        "source"
        "(ui/badge text tone) ->\n  (ui/elm \"span\"\n    (list \"class\" (string-append \"lb-badge lb-badge-\" tone))\n    (list (ui/text text)))")))
  "The ui/badge card.")

(define ui/card/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/card"
        "doc"
        "Card with an in-title badge: composes ui/badge (reuse = a def call; the composed value nests as kids)."
        "file" "libs/lookbook/lib.pp"
        "formals" (json-emit (list "title" "badge-text" "body"))
        "variants"
        (json-emit
          (list
            (lb/obj
              (list
                "title" "Notes"
                "badge-text" "12 new"
                "body"
                "The register answers /issues and the spec notes docs."))))
        "source"
        "(ui/card title badge-text body) ->\n  (ui/elm \"section\" (list \"class\" \"lb-card\")\n    (list (ui/elm \"h3\" (list)\n            (list (ui/text title) (ui/badge badge-text \"ok\")))\n          (ui/elm \"p\" (list) (list (ui/text body)))))")))
  "The ui/card card -- the composition seed (nesting visible in both
projections).")

;; ---- inventory seam (like filmstrip/apps: host vocabulary) ----
(define lookbook/elements
  (lambda () (list "ui/button" "ui/badge" "ui/card"))
  "Names of declared lookbook ui elements. HOST SEAM: hosts redefine
to surface their own elements; an entry without a <name>/examples
card answers the pane's no-card message, never an error.")

;; ---- CARD validation (lib side stays guard-free and host-free: the
;; face fetches the card through its eval seam and validates here) ----
(define lookbook/card
  (lambda (text)
    (if (string-starts-with? text "{")
        (let ((name (lb/get text "name")))
          (if (string-eq name "") (list) (list text)))
        (list)))
  "Validate one fetched card: a JSON object carrying \"name\" answers
(card-text); anything else answers (). The face guards its eval seam
and feeds the result here, so a bad/missing <name>/examples def never
errors the pane.")
