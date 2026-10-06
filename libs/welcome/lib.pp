;; package welcome -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define welcome/background
  (lambda
    ()
    "Render the /background about page."
    (layout/page
      (list
        (layout/hero
          "Background"
          "A little about how Pricklypear happened."
          (html/join
            (list
              (layout/cta "/welcome" "Welcome")
              (layout/cta "/roadmap" "Roadmap")
              (layout/cta "/code" "Source")
              (layout/cta "/login" "Log in"))))
        (layout/section
          "A little about me"
          (html/join
            (list
              (html/p
                "I'm super excited about what has emerged with Pricklypear. Pricklypear is the result of a kind of long, meandering journey. I see a lot of these cactus on my hike. In the middle of the desert, they are common, they are prickly, and they bear delicious fruit. I've also been told you can use them to harvest water kefir yeast, but I've yet to confirm this.")
              (html/p
                "I quit my development job and now work at a dentist office and a restaurant. But I just can't seem to stop coding. For a while I was trying to make the ultimate \"CRUDmonkey\" platform. Basically, I spent a lot of time with half baked designs trying to reinvent web frameworks. Then, I became totally taken with the idea of a tree-based structure editor.")
              (html/p
                "My background is largely as a python CRUDmonkey, hence my interest in reinventing frameworks. I did a lot of data processing elbow grease work. I was CTO of a 13 person startup for a while. When I left that job I was pretty burnt out. I didn't think I enjoyed programming, but I still was obsessed with the idea of maybe I could make it easier.")
              (html/p
                "Then I found OCaml. Functional programming did it for me, man. It was like learning to program all over again. I started learning about all the things I had basically ignored as a python guy. I learned about compilers, low level languages. I took a freelance gig doing static analysis on IBM HLASM assembly language.")
              (html/p
                "The only thing I really wanted from code? I wanted to build my dream zettelkasten. I just wanted a good notes app for my bible study. First I built a bible mode in Obsidian with markdown files. But as soon as I tried to get Strong's Concordance in there, I absolutely hit the limits of what Obsidian could do. Then, I decided to switch over to elisp. LLMs were barely functional at this time, and they were really bad at lisp. This was the \"copy and paste from ChatGPT\" era. My emacs / org-roam zettelkasten worked amazingly. Honestly, I love emacs and I love org-roam. Absolutely beautiful pieces of software. I also love the sense of calm that I get with emacs software. I pulled up this bible app in emacs 4 years later and it just... worked. Amazingly. None of the constant churn of the JS ecosystem.")
              (html/p
                "OK, so then I got into OCaml, and I was very anti LLM. They didn't work, they made you dumb, they were a scam, yada yada yada. So I built myself a bible app, all by hand, in OCaml / Dream. I had a great time with this. I felt like I probably learned more about Dream than I did OCaml, but that's how I felt when I first learned Python also.")
              (html/p
                "Then I took a gig with some guys who were doing IBM mainframe LLM stuff. I started coding in TypeScript (which i had never really used), right around the time when the LLMs got really, really good. Like November of 2025? I saw a massive increase in their ability to compose and build code.")
              (html/p
                "Then I went deep. I started building harnesses, orchestrators, I did the full LLM psychosis thing. I eventually settled on my Borge architecture. Without going too deeply into it, it's a DSL for defining spec and a series of CLI tools to get agents to implement, review, and build out that spec. I'm not sure it's any more efficient but it seems to work super well for me.")
              (html/p
                "So then one day I had the brainwave. Can I make an interpreted lisp where the functions are stored in postgres? The agent assured me I was a genius yet again, but I had a good feeling about this one. I had the LLM draw out my Borge spec, and then started a ralph loop. I think it went for like, 25 iterations? It ran for 4 days and it crashed a couple times and I think I maxed out three different providers. It used a lot of tokens. I got an email from one of my LLM providers the next day letting me know they were tripling their costs.")
              (html/p
                "When I started playing with it, I actually couldn't figure out how to use it at first. I had the agent draw up a intro guide. Then I realized that not only did it work, I could write language functions to the database, but that also those functions were working as web endpoints. I realized I had a whole framework.")
              (html/p
                "It worked so well. I built a couple apps on top, it was effortless. I plugged an LLM into the system. Once I prompted it to realize Pricklypear was not a cactus in this scenario, it completely understood the harness it was in and it just... started writing functions. I couldn't believe it. Then I gave my local LLM access to the full REPL, and I realized I wouldn't ever have to build an API or CLI interface for agents ever again.")
              (html/p
                "I'm delighted by this. I have wanted a Smalltalk type environment since I learned about what that was. But by incredible, happy accident, I also got my brand spanking new web framework, and I also got my tree based editor. I honestly feel, I don't need any more software. I'd be good. If I had my druthers, I would just work on this in private and read Functional Programming papers and Alonzo Church in my hammock for the next 5 years while I dogfood this thing. But, you know, \"building in public\" and all that.")
              (html/p
                "I'm hoping some people are as thrilled and inspired by this as I am. Maybe the code is useful, or maybe just hearing about the concept is enough to inspire people. Or maybe, and this may be more likely, nobody will give a crap. I don't know where LLMs will go in the future, but I am really happy right here."))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/a "/roadmap" (html/text "roadmap"))
              (html/text " · ")
              (html/a "/welcome" (html/text "welcome")))))))));; package welcome -- exported from live branch system/main

(define welcome/blank?
  (lambda (s) "True if trimmed s is empty." (string-eq (string-trim (as-str s)) "")))

(define welcome/book
  (lambda
    ()
    "Render the /book explainer page for the literate code PDF."
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
              (html/p
                "The book is produced by borge book print. Order is four-tier: root .borg, chapter .borg files, .mli signatures, then .ml implementations (dependency-ranked). Line numbers and a wide margin are for hand notes.")
              (html/p
                "It is not a tangle/weave of prose-into-code. The .borg files remain the source of truth; the PDF is a read-only projection so you can leave the screen.")
              (html/p
                "If the PDF is missing on this host, the deploy step that runs scripts/public_artifacts.sh has not installed it yet — the code browser and tarball still cover the tree."))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/a "/welcome" (html/text "← welcome")))))))

(define welcome/guestbook
  (lambda
    ()
    "Route handler for /guestbook: record POSTs or show the form."
    (if
      (string-eq (request-method) "POST")
      (let
        ((name (request-param "name"))
          (site (request-param "site"))
          (message (request-param "message")))
        (if
          (or
            (welcome/blank? (string-trim (as-str name)))
            (welcome/blank? (string-trim (as-str message))))
          (welcome/guestbook-form "" "Need a name and a message.")
          (seq
            (with-public-write
              (lambda () (welcome/guestbook-record name site message)))
            (welcome/guestbook-form
              "Thanks for signing! 🌵"
              ""))))
      (welcome/guestbook-form "" ""))))

(define welcome/guestbook-entries
  (lambda
    ()
    "Render the list of guestbook entries as an HTML list."
    (let
      ((rs (rows (quote guestbook_entry) 200)))
      (if
        (null? rs)
        (html/p
          "No one has signed the guestbook yet. Be the first!")
        (html/ul
          (html/attr "class" "gb-list")
          (html/join (list/map welcome/guestbook-row-html rs)))))))

(define welcome/guestbook-fields
  (lambda
    (name site message)
    "Build the guestbook_entry fields JSON string from trimmed inputs."
    (dict-set*
      "{}"
      (list
        "name"
        (string-trim (as-str name))
        "site"
        (string-trim (as-str site))
        "message"
        (string-trim (as-str message))))))

(define welcome/guestbook-form
  (lambda
    (msg err)
    "Render the guestbook form page with msg/err notices and entries."
    (layout/page
      (list
        (layout/hero
          "Guestbook"
          "Sign my guestbook!! Leave a note. Tell me what you think. 🌵"
          (html/join
            (list
              (layout/cta "/welcome" "Welcome")
              (layout/cta "/code" "Source")
              (layout/cta "/login" "Log in"))))
        (if
          (welcome/blank? err)
          ""
          (html/p* (html/attr "class" "err") (html/text err)))
        (if
          (welcome/blank? msg)
          ""
          (html/p* (html/attr "class" "ok") (html/text msg)))
        (layout/section
          "Sign it"
          (html/form
            "POST"
            "/guestbook"
            (html/attr "class" "gb-form")
            (html/join
              (list
                (html/p "Your name")
                (html/text-input
                  "name"
                  (html/attrs
                    (list
                      (html/attr "required" "required")
                      (html/attr "placeholder" "anon")
                      (html/attr "maxlength" "60"))))
                (html/p "Your site (optional)")
                (html/text-input
                  "site"
                  (html/attrs
                    (list
                      (html/attr "placeholder" "https://your.site")
                      (html/attr "maxlength" "120"))))
                (html/p "Message")
                (html/textarea
                  "message"
                  (html/attrs
                    (list
                      (html/attr "rows" "3")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "say hi")
                      (html/attr "maxlength" "500")))
                  "")
                (html/hidden "source" "web")
                (html/div
                  ""
                  (html/button
                    (html/attrs (list (html/attr "type" "submit")))
                    "Sign it!"))))))
        (layout/section "Entries" (welcome/guestbook-entries))
        (html/div
          (html/attr "class" "layout-foot")
          (html/a "/welcome" (html/text "welcome")))))))

(define welcome/guestbook-record
  (lambda
    (name site message)
    "Create a new guestbook_entry row from name, site, and message."
    (create
      (quote guestbook_entry)
      (welcome/guestbook-fields name site message))))

(define welcome/guestbook-row-html
  (lambda
    (row)
    "Render a single guestbook entry row as an HTML <li>."
    (let
      ((f (cdr row)))
      (let
        ((name (as-str (dict-get f "name")))
          (site (as-str (dict-get f "site")))
          (message (as-str (dict-get f "message"))))
        (html/li
          (html/attr "class" "gb-entry")
          (html/join
            (list
              (html/tag
                "span"
                (html/attr "class" "gb-name")
                (html/text name))
              (if
                (welcome/blank? site)
                ""
                (html/join
                  (list
                    " "
                    (html/tag
                      "a"
                      (html/attrs
                        (list
                          (html/attr "class" "gb-site")
                          (html/attr "href" site)))
                      (html/text site)))))
              (html/tag
                "div"
                (html/attr "class" "gb-msg")
                (html/text message)))))))))

(define welcome/page
  (lambda
    ()
    "Render the /welcome landing page."
    (layout/page
      (list
        (layout/hero
          "Pricklypear"
          "What if WordPress was a Lisp Machine? (Nobody has asked this question)"
          (html/join
            (list
              (layout/cta "/code" "Explore the source")
              (layout/cta "/book.pdf" "Download the book (PDF)")
              (layout/cta "/agent.txt" "Agent instructions")
              (layout/cta "/background" "Background")
              (layout/cta "/roadmap" "Roadmap")
              (layout/cta "/signup" "Email signup")
              (layout/cta "/guestbook" "Sign guestbook")
              (layout/cta "/login" "Log in"))))
        (layout/section
          "What is Pricklypear?"
          (html/join
            (list
              (html/p
                "Pricklypear is a multi-user Lisp operating system. One unix process is the whole OS surface — HTTP, REPL, eval, auth. Postgres is the filesystem: every function, type, route, and data row lives there as rows. The image is ephemeral and rebuilds from the store.")
              (html/p
                "You don't deploy Nopales code. You save a row. Change a function and every route that calls it picks it up immediately — no compile step, no restart."))))
        (layout/section
          "What is Nopales?"
          (html/join
            (list
              (html/p
                "Nopales is the language. It's a small Scheme-family Lisp-1. It's homoiconic: the AST is the source, and function definitions persist in Postgres as JSONB. Editing the AST is editing the code.")
              (html/p
                "Here's a handler function. The browser hits /hello and this runs.")
              (html/pre
                (html/text
                  "(define hi-page\n  (lambda () (render-html \"<h1>Hello</h1>\")))"))
              (html/p
                "You don't need to restart to see this page. There's no translation layer. Postgres just stores the direct AST of your sexp, serialized as JSONB:")
              (html/tag
                "table"
                (html/attr "class" "ast-table")
                (html/join
                  (list
                    (html/tag
                      "tr"
                      ""
                      (html/join
                        (list
                          (html/tag "th" "" (html/text "column"))
                          (html/tag "th" "" (html/text "value")))))
                    (html/tag
                      "tr"
                      ""
                      (html/join
                        (list
                          (html/tag "td" "" (html/text "name"))
                          (html/tag "td" "" (html/code "hi-page")))))
                    (html/tag
                      "tr"
                      ""
                      (html/join
                        (list
                          (html/tag "td" "" (html/text "body"))
                          (html/tag
                            "td"
                            ""
                            (html/pre
                              (html/text
                                "{\"lambda\":[[],\n  {\"app\":[{\"var\":\"render-html\"},\n            {\"lit\":{\"str\":\"<h1>Hello</h1>\"}}]}]}"))))))
                    (html/tag
                      "tr"
                      ""
                      (html/join
                        (list
                          (html/tag "td" "" (html/text "doc"))
                          (html/tag "td" "" (html/text "Render the hello page.")))))
                    (html/tag
                      "tr"
                      ""
                      (html/join
                        (list
                          (html/tag "td" "" (html/text "author"))
                          (html/tag "td" "" (html/text "admin"))))))))
              (html/p
                "It has closures, lambda, let, quasiquote macros, pattern matching, and a frozen core. Dynamic typing, impure functional, effects-inferred. Two kinds of function: data builders you call from the REPL, and HTTP handlers you bind to a URL.")
              (html/p*
                ""
                (html/join
                  (list
                    "A handler is just "
                    (html/code "(route \"/hello\" \"hi-page\")")
                    " — where "
                    (html/code "hi-page")
                    " is a function you defined. The browser hits /hello; the REPL calls hi-page."))))))
        (layout/cards
          (list
            (layout/card
              "Interpreted runtime"
              "Agent skills and MCP are 2024 era coding dude. Here in Pricklypear we directly access the REPL. POST one Lisp expression to /api/eval and get a value back. Pure prims (+, car, string-append) run in the interpreter; DB prims (get, create, rows, route, fork, merge) read and write real Postgres rows. No build, no deploy, no second language. You can also mirror the whole language and run the interpreter locally.")
            (layout/card
              "HTML & CSS primitives"
              "Sexp wrappers for the whole tree. html/div, html/a, html/p, html/attr. You build pages in Lisp, not string templates. A tiny CSS DSL (css/rule, css/sheet, css/media) emits a real stylesheet. This page is styled that way.")
            (layout/card
              "htmx, server-evaluated"
              "Route handlers return htmx fragments. A server-side swap simulator (the hypermedia harness) lets agents can verify a flow without a browser. You don't have to do a goofy Chrome / screenshot workflow.")
            (layout/card
              "Local-first evaluation"
              "The same interpreter runs three ways: in the image against Postgres, over HTTP via /api/eval (local compute, remote data), and against a local lens offline that grafts back on reconnect. One language, one seam, three modes.")))
        (layout/section
          "3 hour tour"
          (html/join
            (list
              (html/p
                "After you log in (or from scripts/repl.sh against a local image):")
              (html/tag
                "ol"
                ""
                (html/join
                  (list
                    (html/li "" (html/code "scripts/repl.sh"))
                    (html/li
                      ""
                      (html/code
                        "(define hi-page (lambda () (render-html \"<h1>Hello</h1>\")))"))
                    (html/li
                      ""
                      (html/code "(route \"/hello\" \"hi-page\")"))
                    (html/li "" "open /hello in the browser")
                    (html/li "" "edit at /edit/hi-page")
                    (html/li
                      ""
                      (html/join
                        (list
                          "optional: "
                          (html/code
                            "(propose-function \"double\" \"lambda of int x -> (* x 2)\")"))))))))))
        (layout/section
          "Agents: no special API"
          (html/join
            (list
              (html/p
                "Remote agents (Agora, pi, curl on a phone) do not need a custom REST surface. Fetch the public bootstrap doc, take a username/password from a human once, then POST Lisp to /api/eval.")
              (html/p*
                ""
                (html/join
                  (list
                    "Machine contract: "
                    (html/a
                      "/.well-known/agent.json"
                      (html/code "/.well-known/agent.json"))
                    " · Prose: "
                    (html/a "/agent.txt" (html/code "/agent.txt"))
                    ". No secrets in either document.")))
              (html/div
                ""
                (html/join
                  (list
                    (layout/cta "/agent.txt" "Read agent.txt")
                    " "
                    (layout/cta "/.well-known/agent.json" "agent.json")))))))
        (layout/section
          "Harness, agent, habitat"
          (html/join
            (list
              (html/p
                "Has anybody noticed there's a third thing missing? I call it The Third Substrate if I'm trying to be fancy. Your harness writes code. Your agent manages tasks and processes. But there's nowhere to deploy your code. Now your poor OpenClaw is a TypeScript DevOps engineer")
              (html/p
                "Pricklypear is the third thing. It's an living app and a memory. If you want you can write all your agent memory to it.")
              (html/p
                "The agent calls it a habitat. I kind of think that name sucks, but oh well. Maybe its natural habitat is living inside its own Lisp. I think Cloudflare is trying to solve this problem with some insane, poorly designed abstraction. (like everything they touch)")
              (html/p
                "I kind of think a habitat has to be a Lisp. I think you could build it in CL or Clojure. But there are big advantages to having an interpreted Lisp. I'd actually like to see a CL or Clojure habitat. My agents say you could build one in less than 10000 LoC. That's about a quarter of the size of Pricklypear. But then I built an interpreter, too."))))
        (layout/section
          "Get in, Loser. We're a cactus now."
          (html/join
            (list
              (html/p
                "Browse the source or download the book without an account. To run code in the image, log in — workspace home, Functions, Pages, Data, and the REPL stay private.")
              (html/div
                ""
                (html/join
                  (list
                    (layout/cta "/code" "Code browser")
                    " "
                    (layout/cta "/book.pdf" "Book PDF")
                    " "
                    (layout/cta "/agent.txt" "Agent instructions")
                    " "
                    (layout/cta "/login" "Sign in")))))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/text
                "Pricklypear · hosted release · ")
              (html/a "/background" (html/text "background"))
              (html/text " · ")
              (html/a "/roadmap" (html/text "roadmap"))
              (html/text " · ")
              (html/a "/signup" (html/text "signup"))
              (html/text " · ")
              (html/a "/guestbook" (html/text "guestbook"))
              (html/text " · ")
              (html/a "/agent.txt" (html/text "agent instructions"))
              (html/text " · ")
              (html/a "/about" (html/text "concepts (after login)")))))))))

(define welcome/roadmap
  (lambda
    ()
    "Render the /roadmap page."
    (layout/page
      (list
        (layout/hero
          "Why this, and where next"
          "Lisp machines, agents, and a weird little platform for apps."
          (html/join
            (list
              (layout/cta "/welcome" "Welcome")
              (layout/cta "/background" "Background")
              (layout/cta "/book.pdf" "Book (PDF)")
              (layout/cta "/login" "Log in"))))
        (layout/section
          "The case for Pricklypear"
          (html/join
            (list
              (html/p
                "Alright - I'll talk a little bit about what I think Pricklypear is good at, and where I see it going")
              (html/p
                "A lot of people have talked about how incredible Lisp is. Personally, I'm not totally sold. Don't get me wrong, lisp is wonderful. But I think for larger projects you just need static typing. I didn't like static typing for a long time, I totally get it. But for medium or big projects, I just don't know how you do big refactors without it.")
              (html/p
                "That said, I think Lisp is like Ruby a lot, it's a beautiful and expressive language. So this is a lisp machine. And I think it's good for the same types of programs that Ruby is good at. So I did my best to clone Basecamp, hah.")
              (html/p
                "Having an agent work with a REPL is incredible. I mean, it's spectacular. They can define functions. They can read and execute individual functions. They can build functions inside out or bottom up. They can build them top down. They can SSH in remotely and access the REPL while they're developing. It's pretty tight.")
              (html/p
                "I don't think any other architecture can match Pricklypear for building web apps, and I honestly don't think it's even close. All of those LLM marketing idiots telling you about on-demand self healing software? Hey, this is it. It's right here. I don't think you can do this with any other stack? Maybe like erlang or something.")
              (html/p
                "The future I see for Pricklypear or something like it is enabling LLMs to build on demand software for small and medium businesses. I don't even think you need to have a Pricklypear guy on staff? I mean you could, it'd probably help. But I think literally users can ask for features and they will be deployed. I've had some trouble getting the built-in agent to build out like a whole app from start to finish, but it can certainly add features. And that loop is closing fast.")
              (html/p
                "I'm biased and deep in AI psychosis, but I'm telling you this is a much bigger leap than OpenClaw. Oh, you put your little crab agent inside a chat bot? Guess what? Mine lives inside a language which he is developing himself, and that language is also a fully baked web app. (Sorry, OpenClaw is brilliant, but I had to. You'll understand when you login the first time.)")
              (html/p
                "Let's talk roadmap. I'm going to dogfood this thing for a while. Maybe some other people will do something similar. I don't really feel any obligation to build anything for anybody else. Get your own! But, I would like to get this thing to the point where it's functioning as a self contained unit, and like, my dad could hop on it and he wouldn't have to learn anything about Lisp Machines in order to use it. I think this is feasible. It's just a weird little platform to build apps on.")
              (html/p
                "I'm hoping to get those apps fleshed out a bit. This is my new homelab center. I'm moving my life onto this thing. It's too good not to. So I'd like for those apps to feel really really good. Currently, they do not feel great. They are not cozy. I want calm, and I want cozy. Even though I can change these apps every day - I want to run them without changes for 5 years.")
              (html/p
                "I initially spec'd this out to run on MirageOS, but we've scrapped that for now. I would like to make this happen eventually. For now, I remain an OpenBSD guy. If you want Linux compatibility, make your agent do it. (I use Arch for development, btw). So I think self contained image is a priority.")
              (html/p
                "I would like to get some academic grounding on building this Lisp out. I don't know nothin about languages. Seriously. I just added floats to this thing. Also I think the language itself is called Nopales, but that isn't reflected anywhere right now. There is some sort of rudimentary effects system. I have no idea how it works. I don't really understand the subtleties of async, and the OCaml landscape is pretty confusing.")
              (html/p
                "On a similar note, I'd like to get some education on formal proofs in software. I think that it is absolutely work spending some time trying to add formal verification to the OCaml outershell of Pricklypear. Lisp gives a great surface for the LLM on top, and I'm perfectly happy with a little jank there, but the core really should be incredibly stable.")
              (html/p
                "I want to really polish up the node editor and the code editor. It should be a first class experience. Keyboard shortcuts, node navigation. I want it to feel as good as vim.")
              (html/p
                "I'd like to see local-first LLMs, too. You really don't need much here. And the way it's designed, it would plug right into a local agent. That local agent really wants to generate his own lisp functions using a recursive bottom up function generator call!")
              (html/p
                "OK, that's enough for now. This is about as much sharing as I do, generally speaking. I guess I could get on Twitter and try to shill this thing but I think I'd rather just build it instead."))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/a "/background" (html/text "background"))
              (html/text " · ")
              (html/a "/welcome" (html/text "welcome")))))))))
