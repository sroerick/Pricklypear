;; package tree -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define tree
  (outline/node*
    "root"
    "Move smoke A"
    ""
    ""
    (list
      (outline/node* "keep" "Keep me" "" "" (list))
      (outline/node*
        "child"
        "Child title"
        "body-x"
        "TODO"
        (list (outline/node* "grand" "Grand" "" "" (list)))))))

(define tree/as-int
  (lambda
    (x dft)
    "Parse x as an int, clamping negatives to 0; dft when blank/invalid."
    (if
      (tree/blank? x)
      dft
      (let
        ((n (parse-int (as-str x))))
        (if (null? n) dft (if (< n 0) 0 n))))))

(define tree/badge
  (lambda
    (n)
    "Render a child-count badge span for a node, or empty when leaf."
    (let
      ((c (tree/child-count n)))
      (if
        (<= c 0)
        ""
        (html/span
          (html/attr "class" "tb-badge")
          (html/text (string-append " (" (as-str c) ")")))))))

(define tree/blank?
  (lambda (x) "True when x is null or an empty string." (or (null? x) (string-eq (as-str x) ""))))

(define tree/breadcrumb
  (lambda
    (tree focus-id slug folders-only?)
    "Render a navigable breadcrumb to a focus node."
    (let
      ((path (tree/find-path tree (as-str focus-id))))
      (if
        (null? path)
        ""
        (html/div
          (html/attr "class" "tb-crumb")
          (tree/breadcrumb-parts path slug folders-only? #t))))))

(define tree/breadcrumb-parts
  (lambda
    (path slug folders-only? first?)
    "Recursively render breadcrumb links over a node path."
    (if
      (null? path)
      ""
      (let
        ((n (car path)) (rest (cdr path)))
        (string-append
          (if
            first?
            ""
            (html/span (html/attr "class" "sep") (html/text " / ")))
          (if
            (null? rest)
            (html/span
              (html/attr "class" "tb-crumb-here")
              (html/text (as-str (outline/title n))))
            (html/a
              (tree/page-url slug (outline/id n) 2 folders-only?)
              (html/text (as-str (outline/title n)))))
          (tree/breadcrumb-parts rest slug folders-only? #f))))))

(define tree/child-count
  (lambda (n) "Return the number of child nodes of n." (if (null? n) 0 (count (outline/kids n)))))

(define tree/default-depth
  (lambda () "Return the requested tree depth param, defaulting to 2." (tree/as-int (request-param "depth") 2)))

(define tree/default-slug
  (lambda
    ()
    "Return the requested slug param, defaulting to scripture_concepts."
    (let
      ((s (request-param "slug")))
      (if (tree/blank? s) "scripture_concepts" (as-str s)))))

(define tree/find-path
  (lambda
    (tree target)
    "Return the list of nodes from root to the target node."
    (tree/find-path-acc tree (as-str target) (list))))

(define tree/find-path-acc
  (lambda
    (node target acc)
    "Accumulator helper that walks a node looking for target id."
    (if
      (null? node)
      ()
      (let
        ((here (append acc (list node))))
        (if
          (string-eq (outline/id node) target)
          here
          (tree/find-path-in-kids (outline/kids node) target here))))))

(define tree/find-path-in-kids
  (lambda
    (kids target acc)
    "Search a kids list for target, returning its path or ()."
    (if
      (null? kids)
      ()
      (let
        ((hit (tree/find-path-acc (car kids) target acc)))
        (if
          (null? hit)
          (tree/find-path-in-kids (cdr kids) target acc)
          hit)))))

(define tree/folders-only-param
  (lambda
    ()
    "Return whether the folders-only query param is set."
    (tree/truthy? (request-param "folders-only"))))

(define tree/has-children?
  (lambda
    (n)
    "True when node n has at least one child."
    (if (null? n) #f (not (null? (outline/kids n))))))

(define tree/leaf-row
  (lambda
    (slug n folders-only?)
    "Render a leaf node row, or empty when folders-only."
    (if
      folders-only?
      ""
      (html/tag
        "li"
        (html/attr "class" "tb-node tb-leaf")
        (html/div
          (html/attr "class" "tb-row")
          (html/join
            (list
              (html/span (html/attr "class" "tb-twisty") ".")
              (tree/title-link slug n folders-only?))))))))

(define tree/load-doc
  (lambda
    (slug)
    "Load a document's outline by slug, or () when missing."
    (let
      ((doc (notes/doc/get (as-str slug))))
      (if (null? doc) () (notes/doc/outline doc)))))

(define tree/open-row
  (lambda
    (slug n depth folders-only?)
    "Render an expanded tree node with a child list."
    (html/tag
      "li"
      (html/attrs
        (list
          (html/attr "class" "tb-node tb-open")
          (html/attr "id" (string-append "tb-" (outline/id n)))))
      (html/join
        (list
          (html/div
            (html/attr "class" "tb-row")
            (html/join
              (list
                (html/span (html/attr "class" "tb-twisty") "-")
                (tree/title-link slug n folders-only?)
                (tree/badge n))))
          (html/tag
            "ul"
            (html/attr "class" "tb-kids")
            (tree/render-kids
              (outline/kids n)
              (- depth 1)
              folders-only?
              slug)))))))

(define tree/page
  (lambda () "Render the full tree browser page." (html/page (list (tree/page-body)))))

(define tree/page-body
  (lambda
    ()
    "Render the tree page body from current request params."
    (let
      ((slug (tree/default-slug))
        (rid (tree/root-param))
        (d (tree/default-depth))
        (fo (tree/folders-only-param)))
      (html/join
        (list
          (tree/style)
          (html/h1 "Tree")
          (html/p*
            (html/attr "class" "muted")
            "Lazy outline browser (image.tree-browser). Folder = non-leaf view bit.")
          (tree/toolbar slug rid d fo)
          (tree/render-doc slug rid d fo))))))

(define tree/page-url
  (lambda
    (slug root depth folders-only?)
    "Build a /tree URL for the given navigation args."
    (string-append
      "/tree?"
      (tree/qs slug root depth folders-only?))))

(define tree/partial
  (lambda () "Render the lazy-load tree partial page." (html/page (list (tree/partial-body)))))

(define tree/partial-body
  (lambda
    ()
    "Render a single focus node for lazy partial load."
    (let
      ((slug (tree/default-slug))
        (rid (tree/root-param))
        (d (tree/default-depth))
        (fo (tree/folders-only-param)))
      (let
        ((doc (tree/load-doc slug)))
        (if
          (null? doc)
          (html/p* (html/attr "class" "err") "missing doc")
          (let
            ((focus
              (if (string-eq rid "root") doc (outline/find doc rid))))
            (if
              (null? focus)
              (html/p* (html/attr "class" "err") "missing node")
              (tree/render-node focus (if (<= d 0) 2 d) fo slug))))))))

(define tree/partial-url
  (lambda
    (slug root depth folders-only?)
    "Build a /partials/tree URL for the given navigation args."
    (string-append
      "/partials/tree?"
      (tree/qs slug root depth folders-only?))))

(define tree/qs
  (lambda
    (slug root depth folders-only?)
    "Build the tree query string from navigation args."
    (string-append
      "slug="
      (as-str slug)
      "&root="
      (as-str root)
      "&depth="
      (as-str depth)
      (if folders-only? "&folders-only=1" ""))))

(define tree/render
  (lambda
    (root-id depth folders-only?)
    "Render the scripture_concepts document tree directly."
    (tree/render-doc
      "scripture_concepts"
      root-id
      depth
      folders-only?)))

(define tree/render-doc
  (lambda
    (slug root-id depth folders-only?)
    "Render a whole document tree for slug, focused on root-id."
    (let
      ((rid (if (tree/blank? root-id) "root" (as-str root-id)))
        (d (if (null? depth) 2 (if (< depth 0) 0 depth)))
        (fo (if (null? folders-only?) #f folders-only?)))
      (let
        ((doc (tree/load-doc slug)))
        (if
          (null? doc)
          (html/p*
            (html/attr "class" "tb-empty")
            (string-append "No outline for slug " (as-str slug)))
          (let
            ((focus
              (if (string-eq rid "root") doc (outline/find doc rid))))
            (if
              (null? focus)
              (html/p*
                (html/attr "class" "tb-empty")
                (string-append "No node " rid " in " (as-str slug)))
              (html/join
                (list
                  (tree/breadcrumb doc rid slug fo)
                  (html/tag
                    "ul"
                    (html/attrs
                      (list
                        (html/attr "class" "tb")
                        (html/attr "id" "tb-root")))
                    (tree/render-node focus d fo slug)))))))))))

(define tree/render-kids
  (lambda
    (kids depth folders-only? slug)
    "Concatenate rendered rows for every node in kids."
    (if
      (null? kids)
      ""
      (string-append
        (tree/render-node (car kids) depth folders-only? slug)
        (tree/render-kids (cdr kids) depth folders-only? slug)))))

(define tree/render-node
  (lambda
    (n depth folders-only? slug)
    "Render one node: leaf, open, or stub depending on depth."
    (if
      (null? n)
      ""
      (if
        (not (tree/has-children? n))
        (tree/leaf-row slug n folders-only?)
        (if
          (<= depth 0)
          (tree/stub slug n 2 folders-only?)
          (tree/open-row slug n depth folders-only?))))))

(define tree/root-param
  (lambda
    ()
    "Return the root/node-id param, defaulting to root."
    (let
      ((r (request-param "root")))
      (if
        (tree/blank? r)
        (let
          ((n (request-param "node-id")))
          (if (tree/blank? n) "root" (as-str n)))
        (as-str r)))))

(define tree/stub
  (lambda
    (slug n depth folders-only?)
    "Render a collapsed stub that lazy-loads its children."
    (let
      ((url
        (tree/partial-url
          slug
          (outline/id n)
          depth
          folders-only?)))
      (html/tag
        "li"
        (html/attrs
          (list
            (html/attr "class" "tb-stub")
            (html/attr "id" (string-append "tb-" (outline/id n)))))
        (html/tag
          "a"
          (html/attrs
            (list
              (html/attr "class" "tb-row")
              (html/attr "href" url)
              (html/attr "hx-get" url)
              (html/attr "hx-trigger" "click")
              (html/attr "hx-swap" "outerHTML")
              (html/attr "hx-target" "closest li")))
          (html/join
            (list
              (html/span (html/attr "class" "tb-twisty") "+")
              (html/span
                (html/attr "class" "tb-title")
                (html/text (as-str (outline/title n))))
              (tree/badge n))))))))

(define tree/style
  (lambda
    ()
    "Return the CSS style tag for the tree browser."
    (html/tag
      "style"
      ""
      (string-append
        ".tb{list-style:none;padding:0;margin:.5rem 0;}"
        ".tb ul{list-style:none;padding:0 0 0 1.1rem;margin:.15rem 0;border-left:1px solid #33cc33;}"
        ".tb-node,.tb-stub{margin:.1rem 0;}"
        ".tb-row{display:flex;align-items:center;gap:.35rem;padding:.15rem .25rem;}"
        ".tb-row:hover{background:#1a2a1a;}"
        ".tb-twisty{display:inline-block;width:1.1rem;color:#888;text-align:center;}"
        ".tb-title{color:#33ff33;text-decoration:none;font-weight:700;}"
        ".tb-title:hover{color:#66ff66;text-decoration:underline;}"
        ".tb-badge{color:#888;font-size:.85rem;}"
        ".tb-stub>a.tb-row{display:flex;cursor:pointer;color:inherit;text-decoration:none;}"
        ".tb-crumb{display:flex;flex-wrap:wrap;gap:.35rem;align-items:center;margin:.5rem 0 1rem;}"
        ".tb-crumb a{color:#33ff33;}"
        ".tb-crumb .sep{color:#888;}"
        ".tb-toolbar{display:flex;flex-wrap:wrap;gap:.5rem;align-items:center;margin:.5rem 0;}"
        ".tb-toolbar a,.tb-toolbar label{font-size:.9rem;}"
        ".tb-empty{color:#888;font-style:italic;}"))))

(define tree/title-link
  (lambda
    (slug n folders-only?)
    "Render a link to a node's page using its title."
    (html/tag
      "a"
      (html/attrs
        (list
          (html/attr
            "href"
            (tree/page-url slug (outline/id n) 2 folders-only?))
          (html/attr "class" "tb-title")))
      (html/text (as-str (outline/title n))))))

(define tree/toolbar
  (lambda
    (slug root depth folders-only?)
    "Render the tree navigation toolbar."
    (let
      ((flip (not folders-only?)))
      (html/div
        (html/attr "class" "tb-toolbar")
        (html/join
          (list
            (html/a
              (tree/page-url slug "root" depth #f)
              "full tree")
            (html/a
              (tree/page-url slug root depth flip)
              (if folders-only? "show leaves" "folders only"))
            (html/a
              (string-append "/notes?slug=" (as-str slug))
              "open in notes")
            (html/span
              (html/attr "class" "muted")
              (html/text
                (string-append
                  "slug="
                  (as-str slug)
                  " root="
                  (as-str root)
                  " depth="
                  (as-str depth))))))))))

(define tree/truthy?
  (lambda
    (x)
    "True when x is a truthy value: 1, true, yes, or on."
    (if
      (tree/blank? x)
      #f
      (let
        ((s (as-str x)))
        (or
          (string-eq s "1")
          (string-eq s "true")
          (string-eq s "yes")
          (string-eq s "on"))))))
