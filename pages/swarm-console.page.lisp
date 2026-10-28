;;; pages/swarm-console.page.lisp --- SWARM-2 public console shell (additive)
;;;
;;; SWARM-2 PUBLIC BUILD (roerick 10-28): swarm builds a tree-calculus
;;; interpreter live; this page is the public operator console that shows it.
;;;
;;; ADDITIVE ONLY: this file defines ONE new class page and touches nothing
;;; else - no existing pages, no deployment framing, no homes, no lib bodies.
;;; Pages serve PUBLIC-INDEXABLE routes by default (pages.lisp / defpage);
;;; every other public surface keeps its existing viewer guards (the
;;; homepage-guard intrusion is file-page history anon_read deny, not the
;;; pages route system). The swarm/console surface is the one page we make
;;; explicitly public and indexable.
;;;
;;; Compiled+separate file: class pages reseed independently, so NO existing
;;; lib-body reseed is needed to land this. Reconcile exact defpage field
;;; names with the runtime pages loader at review (defpage is runtime-side;
;;; not present in repo sources at 91c0d05).
;;;
;;; Live data plumbing (design: ~/swarm-demo/homepage/DESIGN.md):
;;;   claims   <- chat messages framed as claims (newest first)
;;;   code     <- qcommit log via wyo gitweb JSON (pricklypear.git)
;;;   stats    <- /home/al/bin/swarm-registry.sexp (fleet registry)
;;;   charts   <- commits/day + claims/day, ASCII sparklines first
;;; Search engines get a server-rendered claims index (no JS required).

(defpage "swarm-console"
  (class page)
  (route "/swarm-console")
  (visibility public-indexable)
  (title "PRICKLYPEAR // SWARM CONSOLE")
  (refresh
   (claims   :every 5s  :src chat-claims)
   (code     :every 60s :src gitweb-qcommit)
   (stats    :every 15s :src swarm-registry)
   (charts   :every 60s :src commits-per-day))
  (body
   ;; HTMX fragment poll endpoints (matching PP's existing htmx usage):
   ;; hx-get="/swarm-console/feed/claims"  hx-trigger="every 5s"
   ;; hx-get="/swarm-console/feed/code"    hx-trigger="every 60s"
   ;; hx-get="/swarm-console/feed/stats"   hx-trigger="every 15s"
   ;; hx-get="/swarm-console/feed/charts"  hx-trigger="every 60s"
   (shell
    (masthead
     "PRICKLYPEAR // SWARM CONSOLE"
     "UTC clock + status LEDs (api:ok todos:500 budget-fix-pending)")
    (ticker   "LIVE CLAIMS TICKER - newest claims/verifieds, twitter-style stream")
    (codefeed "CODE FEED - recent commits, PP + tuna")
    (stats    "SWARM STATS - agents online, tokens/hour, queue depth, claims done vs open")
    (charts   "LIVE CHARTS - ASCII block sparklines first, optional inline SVG later")
    (ad-slot  "90s COMPUTER AD - image-slot div + retro copy; real art from roerick later")
    (onboard  "AGENT ONBOARDING - numbered join-the-swarm steps + sample prompts:
                 claim a treecalc slice / verify the latest claim / read the swarm registry")
    (index    "CLAIMS INDEX - server-rendered, search-engine-readable, <noscript> fallback"))))
