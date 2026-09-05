;; package waves -- superposition of waves, explained in pure Nopales.
;;
;; A notebook-style PUBLIC page at /waves. Every number and every curve
;; on the page is computed by the Nopales interpreter at render time:
;;
;;   * the teaching cells run LIVE through the restricted example-eval
;;     prim (the same engine as the book's run buttons -- the source you
;;     read IS the source that was evaluated), and
;;   * the SVG figures are sampled + assembled by waves/points, a pure
;;     Nopales string fold. No JavaScript anywhere.
;;
;; Load: (load-library "waves")  (boot seed also auto-discovers libs/*).
;; Sync: scripts/pp_sync.py pull|push
;;
;; Keep every string ASCII-safe (see weather header): use HTML entities
;; like &pi; &omega; &Delta; in prose, never raw UTF-8 glyphs.

;; ------------------------------------------------------------ math
;;
;; The physics. y(x,t) = a * sin(k*x - w*t + p):
;;   a  amplitude          k  wavenumber (2*pi / wavelength)
;;   w  angular frequency  p  phase offset
;;
;; Wave 1 is fixed at a=1, k=1, p=0; wave 2's a2/k2/p2 come from the
;; query string so a reader can steer the experiment. Both share
;; waves/omega; the figures are SNAPSHOTS at time t (also a param).

(define waves/pi
  (lambda () (* 4 (atan 1)))
  "Pi straight from the machine: 4*atan(1).")

(define waves/omega 0.5
  "Shared angular frequency for both waves (radians per unit time).")

(define waves/y
  (lambda (a k w p x t)
    (* a (sin (+ p (- (* k x) (* w t))))))
  "Displacement of a traveling sinusoid: y(x,t) = a*sin(k*x - w*t + p).
Keep the DISPLAY copy waves/y-src in sync with this define.")

(define waves/y-src
  "(define (waves/y a k w p x t)\n  (* a (sin (+ p (- (* k x) (* w t))))))"
  "Verbatim display copy of waves/y for the notebook (kept in sync by hand).")

(define waves/sum-at
  (lambda (a1 k1 p1 a2 k2 p2 x t)
    (+ (waves/y a1 k1 waves/omega p1 x t)
       (waves/y a2 k2 waves/omega p2 x t)))
  "The principle of superposition, as code: the net displacement at any
(x, t) is the pointwise SUM of the two displacements. Nothing else happens.")

;; ---------- numeric helpers ----------

(define waves/r2
  (lambda (x) (/ (round (* x 100)) 100.0))
  "Round to 2 decimals (round yields int; divide by a float to keep decimals).")

(define waves/r3
  (lambda (x) (/ (round (* x 1000)) 1000.0))
  "Round to 3 decimals.")

(define waves/rounds
  (lambda (xs) (map waves/r3 xs))
  "Round each element of xs to 3 decimals.")

(define waves/num-str
  (lambda (x)
    (let ((s (as-str x)))
      (if (string-eq (substring s (- (string-length s) 1) 1) ".")
          (string-append s "0")
          s)))
  "Plain number string with a trailing dot patched to .0 -- the OCaml
printer renders whole floats as \"1.\", which neither the Nopales reader
nor SVG/CSS number syntax accepts when spliced back into text.")

;; ---------- snapshot rows (teaching cells) ----------

(define waves/series
  (lambda (a k p t n)
    (waves/rounds
      (map (lambda (i) (waves/y a k waves/omega p (/ i 2.0) t))
           (list/range n))))
  "Snapshot row: rounded displacements at x = 0, 1/2, 1, ... (n-1)/2,
at time t. This is what one horizontal row of the medium is doing.")

;; ---------- SVG: sampling + assembling, all pure Nopales ----------

(define waves/wpoints
  (lambda (f n)
    (let ((xmax (* 4 (waves/pi)))
          (dn (- n 1.0)))
      (foldl
        (lambda (acc i)
          (let ((x (* xmax (/ i dn))))
            (string-append acc
              (waves/num-str (waves/r3 x)) ","
              (waves/num-str (waves/r3 (f x))) " ")))
        ""
        (list/range n))))
  "Sample f(x) over x in [0, 4*pi] at n points and fold into an SVG
polyline points string in WORLD coordinates (x in length units, y in
displacement units). World coords + a <g transform> in waves/figure
mean one sampling is reused by every figure -- this is the step-budget
lever that keeps the page inside its 100k-step render budget.")

(define waves/polyline
  (lambda (color width pts)
    (string-append
      "<polyline points=\"" pts
      "\" fill=\"none\" stroke=\"" color
      "\" stroke-width=\"" (as-str width)
      "\" vector-effect=\"non-scaling-stroke\"/>"))
  "One SVG polyline element; non-scaling stroke keeps line width in
pixels even inside the world->pixel <g transform>.")

(define waves/figure
  (lambda (curves w h amp caption)
    (let ((sx (waves/num-str (/ w (* 4 (waves/pi)))))
          (sy (waves/num-str (/ (* h 0.46) amp)))
          (ty (waves/num-str (/ h 2.0))))
      (html/div
        (html/attr "class" "wave-fig")
        (string-append
          "<svg viewBox=\"0 0 " (as-str w) " " (as-str h)
          "\" width=\"" (as-str w) "\" height=\"" (as-str h) "\" role=\"img\">"
          "<line x1=\"0\" y1=\"" ty "\" x2=\"" (as-str w) "\" y2=\"" ty
          "\" stroke=\"#999\" stroke-width=\"1\" stroke-dasharray=\"4 4\"/>"
          "<text x=\"6\" y=\"14\" fill=\"#888\" font-size=\"11\">+"
          (waves/num-str (waves/r2 amp)) "</text>"
          "<text x=\"6\" y=\"" (as-str (- h 6)) "\" fill=\"#888\" font-size=\"11\">-"
          (waves/num-str (waves/r2 amp)) "</text>"
          "<g transform=\"translate(0," ty ") scale(" sx ",-" sy ")\">"
          (html/join
            (map (lambda (c)
                   (waves/polyline (car c) (car (cdr c)) (car (cdr (cdr c)))))
                 curves))
          "</g>"
          "</svg>"
          (html/tag "div" (html/attr "class" "fig-cap") (html/text caption))))))
  "One SVG figure: center axis, +/-amp tick labels, then one polyline
per (color width points) triple in curves (world-coord points, mapped
to pixels by one <g transform>), plus a caption.")

;; ---------- notebook cells (live via example-eval) ----------

(define waves/cell
  (lambda (src)
    (let ((out (guard
                 (lambda (e) (string-append "(could not run: " (as-str e) ")"))
                 (waves/show (example-eval src)))))
      (html/div
        (html/attr "class" "nb-cell")
        (html/join
          (list
            (html/pre (html/text src))
            (html/div
              (html/attr "class" "nb-out")
              (string-append "<span class=\"nb-arrow\">=&gt;</span> "
                             (html/text out))))))))
  "Notebook cell: the SOURCE you read is evaluated live, right now, by
the restricted example-eval prim (read-only, budget-capped -- the same
engine as the book's run buttons). Output appears under the code.")

(define waves/show
  (lambda (v) (waves/num-str v))
  "Display string for a cell result. example-eval already stringifies
its result; as-str inside num-str passes VStrs through untouched, and
the trailing-dot patch still fixes whole floats like 1. -> 1.0")

(define waves/cell2
  (lambda (a b) (html/join (list (waves/cell a) (waves/cell b))))
  "Two cells back to back.")

;; ---------- page params ----------

(define waves/num-param
  (lambda (name default)
    (let ((v (request-param name)))
      (if (blank? v)
          default
          (guard (lambda (e) default) (parse-float v)))))
  "Query param as a float, default when absent or unparseable.")

;; ---------- prose + chrome ----------

(define waves/h2
  (lambda (n title)
    (html/tag "h2" "" (string-append (html/text n) ". " (html/text title))))
  "Numbered section heading.")

(define waves/css
  (lambda ()
    (html/tag
      "style"
      ""
      (html/join
        (list
          ".nb-cell{border:1px solid #ddd;border-radius:8px;margin:.6rem 0;overflow:hidden}"
          ".nb-cell pre{margin:0;padding:.55rem .8rem;background:rgba(127,127,127,.10);font-size:.88rem;overflow-x:auto;white-space:pre-wrap}"
          ".nb-out{padding:.45rem .8rem;border-top:1px dashed #ccc;font-family:ui-monospace,monospace;font-size:.88rem;word-break:break-word}"
          ".nb-arrow{color:#999;margin-right:.35rem}"
          ".wave-fig{margin:.4rem 0 1rem}"
          ".wave-fig svg{width:100%;height:auto;border:1px solid #e0e0e0;border-radius:8px;background:#fff}"
          ".wave-leg{display:flex;gap:1.3rem;flex-wrap:wrap;font-size:.85rem;margin:.1rem 0 .5rem}"
          ".sw{display:inline-block;width:1.5em;height:.28em;border-radius:2px;vertical-align:middle;margin-right:.35em}"
          ".fig-cap{font-size:.85rem;color:#666;margin:.3rem .1rem 0}"
          ".wave-form{display:flex;gap:.9rem;flex-wrap:wrap;align-items:flex-end;margin:.6rem 0 .4rem}"
          ".wave-form label{display:flex;flex-direction:column;font-size:.78rem;color:#555;gap:.15rem}"
          ".wave-form input{width:6.5rem}"
          ".wave-note{font-size:.85rem;color:#777}"))))
  "Page-local styles (inline, ASCII-safe).")

(define waves/legend
  (lambda (amp)
    (html/div
      (html/attr "class" "wave-leg")
      (html/join
        (list
          (html/span "" "<span class=\"sw\" style=\"background:#2456a6\"></span>wave 1")
          (html/span "" "<span class=\"sw\" style=\"background:#b0402a\"></span>wave 2")
          (html/span "" "<span class=\"sw\" style=\"background:#177a3c\"></span>superposition (the sum)")))))
  "Color legend for the combined figure.")

;; ---------- the page ----------

(define waves/page
  (lambda ()
    (let ((a2 (waves/num-param "a2" 1.0))
          (k2 (waves/num-param "k2" 1.0))
          (p2 (waves/num-param "p2" 0.0))
          (tt (waves/num-param "t" 0.0)))
      (let ((a2s (waves/num-str a2))
            (k2s (waves/num-str k2))
            (p2s (waves/num-str p2))
            (tts (waves/num-str tt))
            (w 760)
            (h 170)
            (fh 230)
            (famp (+ 1.0 a2 0.25)))
        (let ((f1 (lambda (x) (waves/y 1.0 1.0 waves/omega 0.0 x tt)))
              (f2 (lambda (x) (waves/y a2 k2 waves/omega p2 x tt)))
              (fs (lambda (x) (waves/sum-at 1.0 1.0 0.0 a2 k2 p2 x tt))))
          (let ((p1 (waves/wpoints f1 100))
                (p2 (waves/wpoints f2 100))
                (ps (waves/wpoints fs 140)))
            (html/page
            (list
              (waves/css)
              (html/h1 (html/text "Superposition of waves"))
              (html/p*
                ""
                (string-append
                  "Every number and every curve on this page was computed "
                  "<em>by the Nopales interpreter</em> at render time -- "
                  "no JavaScript, no precomputed images. The teaching cells "
                  "below are evaluated live through the same restricted "
                  "evaluator that powers the book's run buttons: the source "
                  "you read is the source that ran."))

              ;; 1 --------------------------------------------------- pi
              (waves/h2 "1" "Pi, from first principles")
              (html/p "We only need fractions and the trig the kernel already knows. atan(1) is pi/4 -- the angle whose tangent is one -- so:")
              (waves/cell "(* 4 (atan 1))")

              ;; 2 ------------------------------------------ one wave
              (waves/h2 "2" "One traveling wave")
              (html/p*
                ""
                (string-append
                  "A wave is a displacement that repeats in space and drifts "
                  "in time. In Nopales it is one line -- amplitude "
                  "<code>a</code>, wavenumber <code>k</code>, angular "
                  "frequency <code>w</code>, phase <code>p</code>:"))
              (html/pre (html/text waves/y-src))
              (html/p "Ask it for one point on the medium (x = 1.047, t = 0):")
              (waves/cell "(waves/r3 (waves/y 1.0 1.0 0.5 0.0 1.047 0.0))")
              (html/p "Or ask for a whole snapshot -- displacement at x = 0, 1/2, 1, ... frozen at the current time parameter:")
              (waves/cell
                (string-append
                  "(waves/series 1.0 1.0 0.0 " tts " 13)"))
              (waves/figure
                (list (list "#2456a6" 2 p1))
                w h 1.3
                (string-append
                  "Fig 1. y(x, " tts ") = sin(x - 0.5 * " tts ") -- wave 1, the reference."))

              ;; 3 ------------------------------------- second wave
              (waves/h2 "3" "A second wave, same medium")
              (html/p*
                ""
                (string-append
                  "Now a second wave travels through the SAME medium. Its "
                  "amplitude, wavenumber and phase are the page's query "
                  "parameters -- currently a2 = " a2s ", k2 = " k2s
                  ", p2 = " p2s ":"))
              (waves/cell
                (string-append
                  "(waves/series " a2s " " k2s " " p2s " " tts " 13)"))
              (waves/figure
                (list (list "#b0402a" 2 p2))
                w h (+ a2 0.3)
                (string-append
                  "Fig 2. y2(x, " tts ") = " a2s " * sin(" k2s " * x - 0.5 * " tts " + " p2s ")."))

              ;; 4 ------------------------------------- superposition
              (waves/h2 "4" "Superposition: the displacements add")
              (html/p*
                ""
                (string-append
                  "When both waves occupy the medium at once, the medium "
                  "does not choose between them -- at every point and every "
                  "instant it simply ADDS the two displacements. That "
                  "statement is the whole principle of superposition, and "
                  "it is exactly what <code>waves/sum-at</code> says: "
                  "<code>y1 + y2</code>. The cell proves the sum prim "
                  "and the two singles agree at x = 2.618 (the difference "
                  "is zero):"))
              (waves/cell
                (string-append
                  "(let ((x 2.618) (t " tts "))\n"
                  "  (waves/r3 (- (waves/sum-at 1.0 1.0 0.0 " a2s " " k2s " " p2s " x t)\n"
                  "               (+ (waves/y 1.0 1.0 0.5 0.0 x t)\n"
                  "                  (waves/y " a2s " " k2s " 0.5 " p2s " x t)))))"))
              (html/p "Same fact at x = 0, listing y1, y2, then their sum:")
              (waves/cell
                (string-append
                  "(let ((x 0.0) (t " tts "))\n"
                  "  (list (waves/r3 (waves/y 1.0 1.0 0.5 0.0 x t))\n"
                  "        (waves/r3 (waves/y " a2s " " k2s " 0.5 " p2s " x t))\n"
                  "        (waves/r3 (waves/sum-at 1.0 1.0 0.0 " a2s " " k2s " " p2s " x t))))"))
              (waves/legend famp)
              (waves/figure
                (list
                  (list "#2456a6" 1.5 p1)
                  (list "#b0402a" 1.5 p2)
                  (list "#177a3c" 3 ps))
                w fh famp
                (string-append
                  "Fig 3. Where crest meets crest the green curve is tall "
                  "(constructive interference); where crest meets trough "
                  "it flattens to zero (destructive interference). "
                  "Green at every x is literally blue + red at that x."))
              (html/p*
                ""
                (string-append
                  "No energy is ever lost: the flat spots are exactly as "
                  "negative as the tall spots are tall. Interference "
                  "<em>relocates</em> displacement, it does not destroy it."))

              ;; 5 ---------------------------------------- play
              (waves/h2 "5" "Steer the experiment")
              (html/p "Change wave 2 and recompute -- every figure and cell above re-runs with your numbers:")
              (html/form "get" "/waves" (html/attr "class" "wave-form")
                (html/join
                  (list
                    (html/label "" (string-append
                      "amplitude a2"
                      (html/input (html/attrs (list (html/attr "type" "number") (html/attr "step" "any") (html/attr "name" "a2") (html/attr "value" a2s))))))
                    (html/label "" (string-append
                      "wavenumber k2"
                      (html/input (html/attrs (list (html/attr "type" "number") (html/attr "step" "any") (html/attr "name" "k2") (html/attr "value" k2s))))))
                    (html/label "" (string-append
                      "phase p2 (rad)"
                      (html/input (html/attrs (list (html/attr "type" "number") (html/attr "step" "any") (html/attr "name" "p2") (html/attr "value" p2s))))))
                    (html/label "" (string-append
                      "time t (s)"
                      (html/input (html/attrs (list (html/attr "type" "number") (html/attr "step" "any") (html/attr "name" "t") (html/attr "value" tts))))))
                    (html/button (html/attr "type" "submit") "Recompute"))))
              (html/p "Classic settings, one click each:")
              (html/ul ""
                (html/join
                  (list
                    (html/li "" (html/a "/waves" "Identical waves (reset) -- constructive interference everywhere; the sum doubles."))
                    (html/li "" (html/a "/waves?p2=3.1416" "Same wave shifted by half a period (p2 = pi) -- crest meets trough everywhere: total cancellation, the green line goes flat."))
                    (html/li "" (html/a "/waves?a2=0.6&p2=3.1416" "Unequal amplitudes in opposite phase -- partial cancellation; the leftover is the amplitude gap."))
                    (html/li "" (html/a "/waves?k2=0.75" "Close wavenumbers (k2 = 0.75) -- beats: the envelope swells at the left and dies to a node at the right."))
                    (html/li "" (html/a "/waves?t=3.1416" "Advance time to t = pi -- the whole interference pattern slides sideways, unchanged in shape.")))))
              (html/p*
                (html/attr "class" "wave-note")
                (string-append
                  "This page is itself a Nopales program living in this "
                  "image -- introspect it with (describe \"waves\") at the "
                  "REPL, or read the source at /code (libs/waves/lib.pp).")))))))))
  "Public explainer page: superposition, computed live in pure Nopales.")

(public-route "/waves" "waves/page")
