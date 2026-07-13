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
              (layout/cta "/background" "Background")
              (layout/cta "/roadmap" "Roadmap")
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
              "I designed the codebase to be read like a book. I like to print codebases out to read them. There's a lot of AI slop in here, but I mark where I've inserted my own comments. My LaTeX pipeline is kind of weird rn so please consider this PDF incomplete and aspirational.")
            (layout/card
              "I live here now"
              "It's multiuser, and it's fun. I have bible study, todo, zettelkasten, flashcards, calendar. Writing apps is insanely easy. You aren't stuck with weird elisp or everything is a buffer rules.")))
        (layout/section
          "5-minute tour"
          (html/join
            (list
              (html/p "After you log in (or from scripts/repl.sh against a local image):")
              (html/tag "ol" ""
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
              (html/a "/background" (html/text "background"))
              (html/text " · ")
              (html/a "/roadmap" (html/text "roadmap"))
              (html/text " · ")
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

;; Author voice essays (from root pricklypear.borg roerick notes).

(define welcome/background
  (lambda ()
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
              (html/p "I'm super excited about what has emerged with Pricklypear. Pricklypear is the result of a kind of long, meandering journey. I see a lot of these cactus on my hike. In the middle of the desert, they are common, they are prickly, and they bear delicious fruit. I've also been told you can use them to harvest water kefir yeast, but I've yet to confirm this.")
              (html/p "I quit my development job and now work at a dentist office and a restaurant. But I just can't seem to stop coding. For a while I was trying to make the ultimate \"CRUDmonkey\" platform. Basically, I spent a lot of time with half baked designs trying to reinvent web frameworks. Then, I became totally taken with the idea of a tree-based structure editor.")
              (html/p "My background is largely as a python CRUDmonkey, hence my interest in reinventing frameworks. I did a lot of data processing elbow grease work. I was CTO of a 13 person startup for a while. When I left that job I was pretty burnt out. I didn't think I enjoyed programming, but I still was obsessed with the idea of maybe I could make it easier.")
              (html/p "Then I found OCaml. Functional programming did it for me, man. It was like learning to program all over again. I started learning about all the things I had basically ignored as a python guy. I learned about compilers, low level languages. I took a freelance gig doing static analysis on IBM HLASM assembly language.")
              (html/p "The only thing I really wanted from code? I wanted to build my dream zettelkasten. I just wanted a good notes app for my bible study. First I built a bible mode in Obsidian with markdown files. But as soon as I tried to get Strong's Concordance in there, I absolutely hit the limits of what Obsidian could do. Then, I decided to switch over to elisp. LLMs were barely functional at this time, and they were really bad at lisp. This was the \"copy and paste from ChatGPT\" era. My emacs / org-roam zettelkasten worked amazingly. Honestly, I love emacs and I love org-roam. Absolutely beautiful pieces of software. I also love the sense of calm that I get with emacs software. I pulled up this bible app in emacs 4 years later and it just... worked. Amazingly. None of the constant churn of the JS ecosystem.")
              (html/p "OK, so then I got into OCaml, and I was very anti LLM. They didn't work, they made you dumb, they were a scam, yada yada yada. So I built myself a bible app, all by hand, in OCaml / Dream. I had a great time with this. I felt like I probably learned more about Dream than I did OCaml, but that's how I felt when I first learned Python also.")
              (html/p "Then I took a gig with some guys who were doing IBM mainframe LLM stuff. I started coding in TypeScript (which i had never really used), right around the time when the LLMs got really, really good. Like November of 2025? I saw a massive increase in their ability to compose and build code.")
              (html/p "Then I went deep. I started building harnesses, orchestrators, I did the full LLM psychosis thing. I eventually settled on my Borge architecture. Without going too deeply into it, it's a DSL for defining spec and a series of CLI tools to get agents to implement, review, and build out that spec. I'm not sure it's any more efficient but it seems to work super well for me.")
              (html/p "So then one day I had the brainwave. Can I make an interpreted lisp where the functions are stored in postgres? The agent assured me I was a genius yet again, but I had a good feeling about this one. I had the LLM draw out my Borge spec, and then started a ralph loop. I think it went for like, 25 iterations? It ran for 4 days and it crashed a couple times and I think I maxed out three different providers. It used a lot of tokens. I got an email from one of my LLM providers the next day letting me know they were tripling their costs.")
              (html/p "When I started playing with it, I actually couldn't figure out how to use it at first. I had the agent draw up a intro guide. Then I realized that not only did it work, I could write language functions to the database, but that also those functions were working as web endpoints. I realized I had a whole framework.")
              (html/p "It worked so well. I built a couple apps on top, it was effortless. I plugged an LLM into the system. Once I prompted it to realize Pricklypear was not a cactus in this scenario, it completely understood the harness it was in and it just... started writing functions. I couldn't believe it. Then I gave my local LLM access to the full REPL, and I realized I wouldn't ever have to build an API or CLI interface for agents ever again.")
              (html/p "I'm delighted by this. I have wanted a Smalltalk type environment since I learned about what that was. But by incredible, happy accident, I also got my brand spanking new web framework, and I also got my tree based editor. I honestly feel, I don't need any more software. I'd be good. If I had my druthers, I would just work on this in private and read Functional Programming papers and Alonzo Church in my hammock for the next 5 years while I dogfood this thing. But, you know, \"building in public\" and all that.")
              (html/p "I'm hoping some people are as thrilled and inspired by this as I am. Maybe the code is useful, or maybe just hearing about the concept is enough to inspire people. Or maybe, and this may be more likely, nobody will give a crap. I don't know where LLMs will go in the future, but I am really happy right here."))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/a "/roadmap" (html/text "roadmap"))
              (html/text " · ")
              (html/a "/welcome" (html/text "welcome")))))))))

(define welcome/roadmap
  (lambda ()
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
              (html/p "Alright - I'll talk a little bit about what I think Pricklypear is good at, and where I see it going")
              (html/p "A lot of people have talked about how incredible Lisp is. Personally, I'm not totally sold. Don't get me wrong, lisp is wonderful. But I think for larger projects you just need static typing. I didn't like static typing for a long time, I totally get it. But for medium or big projects, I just don't know how you do big refactors without it.")
              (html/p "That said, I think Lisp is like Ruby a lot, it's a beautiful and expressive language. So this is a lisp machine. And I think it's good for the same types of programs that Ruby is good at. So I did my best to clone Basecamp, hah.")
              (html/p "Having an agent work with a REPL is incredible. I mean, it's spectacular. They can define functions. They can read and execute individual functions. They can build functions inside out or bottom up. They can build them top down. They can SSH in remotely and access the REPL while they're developing. It's pretty tight.")
              (html/p "I don't think any other architecture can match Pricklypear for building web apps, and I honestly don't think it's even close. All of those LLM marketing idiots telling you about on-demand self healing software? Hey, this is it. It's right here. I don't think you can do this with any other stack? Maybe like erlang or something.")
              (html/p "The future I see for Pricklypear or something like it is enabling LLMs to build on demand software for small and medium businesses. I don't even think you need to have a Pricklypear guy on staff? I mean you could, it'd probably help. But I think literally users can ask for features and they will be deployed. I've had some trouble getting the built-in agent to build out like a whole app from start to finish, but it can certainly add features. And that loop is closing fast.")
              (html/p "I'm biased and deep in AI psychosis, but I'm telling you this is a much bigger leap than OpenClaw. Oh, you put your little crab agent inside a chat bot? Guess what? Mine lives inside a language which he is developing himself, and that language is also a fully baked web app. (Sorry, OpenClaw is brilliant, but I had to. You'll understand when you login the first time.)")
              (html/p "Let's talk roadmap. I'm going to dogfood this thing for a while. Maybe some other people will do something similar. I don't really feel any obligation to build anything for anybody else. Get your own! But, I would like to get this thing to the point where it's functioning as a self contained unit, and like, my dad could hop on it and he wouldn't have to learn anything about Lisp Machines in order to use it. I think this is feasible. It's just a weird little platform to build apps on.")
              (html/p "I'm hoping to get those apps fleshed out a bit. This is my new homelab center. I'm moving my life onto this thing. It's too good not to. So I'd like for those apps to feel really really good. Currently, they do not feel great. They are not cozy. I want calm, and I want cozy. Even though I can change these apps every day - I want to run them without changes for 5 years.")
              (html/p "I initially spec'd this out to run on MirageOS, but we've scrapped that for now. I would like to make this happen eventually. For now, I remain an OpenBSD guy. If you want Linux compatibility, make your agent do it. (I use Arch for development, btw). So I think self contained image is a priority.")
              (html/p "I would like to get some academic grounding on building this Lisp out. I don't know nothin about languages. Seriously. I just added floats to this thing. Also I think the language itself is called Nopales, but that isn't reflected anywhere right now. There is some sort of rudimentary effects system. I have no idea how it works. I don't really understand the subtleties of async, and the OCaml landscape is pretty confusing.")
              (html/p "On a similar note, I'd like to get some education on formal proofs in software. I think that it is absolutely work spending some time trying to add formal verification to the OCaml outershell of Pricklypear. Lisp gives a great surface for the LLM on top, and I'm perfectly happy with a little jank there, but the core really should be incredibly stable.")
              (html/p "I want to really polish up the node editor and the code editor. It should be a first class experience. Keyboard shortcuts, node navigation. I want it to feel as good as vim.")
              (html/p "I'd like to see local-first LLMs, too. You really don't need much here. And the way it's designed, it would plug right into a local agent. That local agent really wants to generate his own lisp functions using a recursive bottom up function generator call!")
              (html/p "OK, that's enough for now. This is about as much sharing as I do, generally speaking. I guess I could get on Twitter and try to shill this thing but I think I'd rather just build it instead."))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/a "/background" (html/text "background"))
              (html/text " · ")
              (html/a "/welcome" (html/text "welcome")))))))))

(public-route "/background" "welcome/background")
(public-route "/roadmap" "welcome/roadmap")
