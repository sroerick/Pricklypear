;; package branch -- the branch-ops status face (borge/branching.borg
;; branching-ast-ergonomics branch-ops). A human-readable rendering of
;; the branch/status summary: counts, the diverged worklist (never
;; auto-applied), and the behind-pins count (the pull candidates).
;; Rendered FROM THE STORE via the prim — never the session snapshot
;; (the 2026-10-05/06 stale-counter incidents).

(package branch
  (export branch/status-page))

(define branch/status-page
  (lambda
    ()
    (let
      ((ref (or (request-param "ref") (current-branch))))
      (let
        ((rep (json-parse (branch/status ref "summary"))))
        (let
          ((counts (dict-get rep "counts"))
           (diverged (dict-get rep "diverged"))
           (omitted (dict-get rep "pins_omitted")))
          (if
            (null? counts)
            (html/page
              (list
                (html/h1 (html/text "Branch status"))
                (html/p*
                  (html/attr "class" "err")
                  (as-str rep))))
            (html/page
              (list
                (html/h1
                  (html/text (string-append "Branch status — " ref)))
                (html/p*
                  (html/attr "class" "muted")
                  "Rendered from the store via the branch/status summary mode — never the session snapshot. The full mode returns the complete JSON.")
                (html/p*
                  (html/attr "class" "muted")
                  (string-append
                    "clean " (as-str (dict-get counts "clean"))
                    " | ahead-new " (as-str (dict-get counts "ahead-new"))
                    " | ahead " (as-str (dict-get counts "ahead"))
                    " | behind " (as-str (dict-get counts "behind"))
                    " | diverged " (as-str (dict-get counts "diverged"))
                    " | synced " (as-str (dict-get counts "synced"))))
                (html/h2
                  (html/text "diverged (never auto-applied — obituary rule)"))
                (html/join
                  (list/map
                    (lambda
                      (d)
                      (html/p*
                        (html/attr "class" "muted")
                        (string-append
                          (as-str (dict-get d "kind"))
                          " "
                          (as-str (dict-get d "name")))))
                    diverged))
                (html/p*
                  (html/attr "class" "muted")
                  (string-append
                    "pins omitted in summary mode: "
                    (as-str omitted)
                    " — land via reseed-package for the full picture."))))))))
  "GET /branch-status?ref=BRANCH — the store-rendered status face over
   the branch/status summary mode. The diverged worklist is the
   never-auto-applied list (obituary rule); behind pins are the pull
   candidates via branch/pull. Defaults to the calling branch."))

(route "/branch-status" "branch/status-page" "private")
