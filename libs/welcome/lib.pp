;; Public front door pages — uses layout/* components.
;; Registered public so anonymous visitors can read them.
;; Kernel also marks /welcome /book hard-public (belt + suspenders).

(define welcome/page
  (lambda ()
    (layout/page
      (list
        (layout/hero
          "Pricklypear"
          "What if WordPress was a Lisp Machine? (Nobody has asked this question)"
          (html/join
            (list
              (layout/cta "/code" "Explore the source")
              (layout/cta "/book.pdf" "Download the book (PDF)")
              (layout/cta "/src.tgz" "Source tarball")
              (layout/cta "/login" "Log in"))))
        (layout/section
          "What is this?"
          (html/join
            (list
              (html/p "Pricklypear is Smalltalk-like (live, inspectable, editable) without being Smalltalk. The image is the CPU; Postgres is the disk. You write Lisp functions that persist as data, branch code like git while sharing rows, and bind functions to URLs.")
              (html/p "This hosted instance is the release surface — not a separate marketing site. The PDF book is the literate reading of the codebase; the browser below is a convenience view of the same tree."))))
        (layout/cards
          (list
            (layout/card
              "Single image"
              "One unix process serves HTTP, REPL, auth, and eval. Multi-user means sessions and garden branches, not a process fleet.")
            (layout/card
              "Lisp + Postgres"
              "Functions, types, routes, and app data live in the store. (define …) persists; branches version code while data stays shared.")
            (layout/card
              "Literate book"
              "borge book print linearizes .borg specs and OCaml sources into a paginated PDF — the primary way to read the system on paper.")
            (layout/card
              "Live gardens"
              "system/main holds package defaults; each user gets username/personal as an inheriting branch. Edit, fork, promote.")))
        (layout/section
          "Get in"
          (html/join
            (list
              (html/p "Browse the source or download the book without an account. To run code in the image, log in — workspace home, Functions, Pages, Data, and the REPL stay private.")
              (html/div ""
                (html/join
                  (list
                    (layout/cta "/code" "Code browser")
                    " "
                    (layout/cta "/book" "About the book")
                    " "
                    (layout/cta "/login" "Sign in")))))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/text "Pricklypear · hosted release · ")
              (html/a "/about" (html/text "concepts (after login)")))))))))

(define welcome/book
  (lambda ()
    (layout/page
      (list
        (layout/hero
          "The Pricklypear book"
          "A paginated, linearly ordered rendering of the spec and code — literate programming without a WEB weave."
          (html/join
            (list
              (layout/cta "/book.pdf" "Download PDF")
              (layout/cta "/src.tgz" "Source tarball")
              (layout/cta "/code" "Browse online")
              (layout/cta "/welcome" "Back to welcome"))))
        (layout/section
          "How to read it"
          (html/join
            (list
              (html/p "The book is produced by borge book print. Order is four-tier: root .borg, chapter .borg files, .mli signatures, then .ml implementations (dependency-ranked). Line numbers and a wide margin are for hand notes.")
              (html/p "It is not a tangle/weave of prose-into-code. The .borg files remain the source of truth; the PDF is a read-only projection so you can leave the screen.")
              (html/p "If the PDF is missing on this host, the deploy step that runs scripts/public_artifacts.sh has not installed it yet — the code browser and tarball still cover the tree."))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/a "/welcome" (html/text "← welcome")))))))

(public-route "/welcome" "welcome/page")
(public-route "/book" "welcome/book")
