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
              (layout/cta "/login" "Log in"))))
        (layout/section
          "No, seriously, what is this?"
          (html/join
            (list
              (html/p "Honestly, I don't know. It's kind of like a Smalltalk. You, or your agent, can write functions. They get saved in the database and reloaded. Those functions can render web pages, so it's kind of a web framework.")
              (html/p "I gotta tell ya. Using it is beautiful. You don't need an API at all. Even your local agent can just use the REPL. I plugged in an LLM to it and it just... worked? It understood the harness and successfully started writing functions.")
              (html/p "Look, I don't even care if anybody uses this at all. I just want people to know about this design pattern. For small and medium business this is insanely powerful."))))
        (layout/cards
          (list
            (layout/card
              "Fully Interpreted Lisp in Postgres"
              "The AST is stored in Postgres. You can browse all the functions in the system. It's like the PHP interface of WordPress, except not terrifying.")
            (layout/card
              "htmx forward"
              "A lot of functions render htmx partials. This makes things pretty modular and composable. We have sexp wrappers for HTML and CSS.")
            (layout/card
              "The Great American Codebase"
              "I designed the codebase to be read like a book. I like to print codebases out to read them. There's a lot of AI slop in here, but I mark where I've inserted my own comments.")
            (layout/card
              "I live here now"
              "It's multiuser, and it's fun. I have bible study, todo, zettelkasten, flashcards, calendar. Writing apps is insanely easy. You aren't stuck with weird elisp or everything is a buffer rules.")))
        (layout/section
          "5-minute tour"
          (html/join
            (list
              (html/p "After you log in (or from scripts/repl.sh against a local image):")
              (html/ol ""
                (html/join
                  (list
                    (html/li "" (html/code "scripts/repl.sh"))
                    (html/li ""
                      (html/code
                        "(define hi-page (lambda () (render-html \"<h1>Hello</h1>\")))"))
                    (html/li ""
                      (html/code "(route \"/hello\" \"hi-page\")"))
                    (html/li "" "open /hello in the browser")
                    (html/li "" "edit at /edit/hi-page")
                    (html/li ""
                      (html/join
                        (list
                          "optional: "
                          (html/code
                            "(propose-function \"double\" \"lambda of int x -> (* x 2)\")"))))))))))
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
