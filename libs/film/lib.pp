;; package film -- the film-analysis MODEL (borge/film-analysis.borg,
;; subsection model): the film/scene/motif/beat types, integer-frame
;; timecodes, and the declared-vs-derived mirror split.
;;
;; Sync: scripts/pp-sync.sh status|pull|snapshot
;;
;; TYPES ARE GUARDED VALUE DEFINES. The seed lint allows only define/
;; defmacro/route/deftype/package/export/public-route at top level, so each
;; deftype rides an idempotent value define (the food / filmstrip-ws /
;; stretch pattern): the type is declared when it is absent and a re-seed
;; leaves an existing declaration alone. A delegated session no-ops by
;; design (the deftype denial lands as the value, not an error).
;;
;; TIME IS FRAMES. ord, tc_in, dur and a beat's tc are INTEGER FRAME
;; COUNTS. fps lives on the FILM row and nowhere else; seconds are a
;; RENDER concern (a renderer divides frames by the film's fps), so no type
;; here stores a float, a timestamp or a second count -- a ring report
;; never accumulates fps-conversion drift.
;;
;; MIRROR IS DECLARED, POSITION IS TRUTH. scene.mirror is a human CLAIM
;; (the mirror scene's slug, empty until curated). The derived mirror falls
;; out of ord: in a ring of n scenes, scene k mirrors scene n-k+1.
;; scene/derived-mirror-ord is pure ord arithmetic and never reads the
;; declared field; the linter reports where declaration and derivation
;; DISAGREE -- that disagreement is the analysis.

(package film
  (export
    film/create
    film/frames
    scene-create
    motif-create
    beat-create
    scene/declared-mirror
    scene/derived-mirror-ord
    scene/mirror-mismatch?
    film/scdet-times
    film/scdet-frames
    film/seconds->frames
    film/auto-cut
    film/ingest-scenes))

;; ---------- declared types ----------

(define film/model-film-declared
  (if
    (null? (type-fields (quote film)))
    (deftype
      (quote film)
      (quote slug)
      (quote title)
      (quote year)
      (quote fps)
      (quote notes))
    "film ok"))

;; ONE detected/curated scene in ONE cut. mirror is a DECLARATION; the
;; derivation comes from ord (see the derivation section below).
(define film/model-scene-declared
  (if
    (null? (type-fields (quote scene)))
    (deftype
      (quote scene)
      (quote film)
      (quote cut)
      (quote ord)
      (quote slug)
      (quote summary)
      (quote tc_in)
      (quote dur)
      (quote mirror))
    "scene ok"))

;; A tracked recurrence across the film.
(define film/model-motif-declared
  (if
    (null? (type-fields (quote motif)))
    (deftype
      (quote motif)
      (quote film)
      (quote name)
      (quote kind))
    "motif ok"))

;; A motif occurrence inside a scene: tc is an ABSOLUTE frame count.
(define film/model-beat-declared
  (if
    (null? (type-fields (quote beat)))
    (deftype
      (quote beat)
      (quote scene)
      (quote motif)
      (quote role)
      (quote tc))
    "beat ok"))

;; ---------- constructors ----------

(define film/frames
  (lambda (n)
    "A frame count as its canonical integer string. Every timecode this
library stores (ord, tc_in, dur, beat tc) is an INTEGER FRAME COUNT; the
conversion lives here so a caller can never smuggle a float or a second
count into a row. Seconds exist only where a renderer divides by the
film's fps."
    (as-str (parse-int (as-str n)))))

(define film/create
  (lambda (slug title year fps notes)
    "Create a film row. fps lives HERE, on the film, because scene, beat and
motif timecodes are all frame counts and need the rate only at render
time (seconds are render-only)."
    (create
      (quote film)
      (dict-set*
        "{}"
        (list
          "slug"
          (as-str slug)
          "title"
          (as-str title)
          "year"
          (as-str year)
          "fps"
          (as-str fps)
          "notes"
          (as-str notes))))))

(define scene-create
  (lambda (film-id cut ord slug summary tc-in dur mirror)
    "Create one scene in one cut: ord is the 1-based position in THIS cut,
tc-in and dur are frame integers, and mirror is the DECLARED mirror scene
slug (empty until curated) -- a claim to check, never the truth: the
derivation comes from ord via scene/derived-mirror-ord."
    (create
      (quote scene)
      (dict-set*
        "{}"
        (list
          "film"
          (as-str film-id)
          "cut"
          (as-str cut)
          "ord"
          (film/frames ord)
          "slug"
          (as-str slug)
          "summary"
          (as-str summary)
          "tc_in"
          (film/frames tc-in)
          "dur"
          (film/frames dur)
          "mirror"
          (as-str mirror))))))

(define motif-create
  (lambda (film-id name kind)
    "Create a motif row: a tracked recurrence of one film."
    (create
      (quote motif)
      (dict-set*
        "{}"
        (list
          "film"
          (as-str film-id)
          "name"
          (as-str name)
          "kind"
          (as-str kind))))))

(define beat-create
  (lambda (scene-id motif role tc)
    "Create a beat: one motif occurrence (plant | payoff | echo) inside a
scene. tc is an ABSOLUTE frame count in the film's frame space."
    (create
      (quote beat)
      (dict-set*
        "{}"
        (list
          "scene"
          (as-str scene-id)
          "motif"
          (as-str motif)
          "role"
          (as-str role)
          "tc"
          (film/frames tc))))))

;; ---------- derivation from ord (position is the truth) ----------

(define scene/declared-mirror
  (lambda (scene)
    "The mirror slug a scene DECLARES (empty when uncurated): a claim to
check against the position-derived mirror, not the answer."
    (let
      ((v (dict-get scene "mirror")))
      (if
        (null? v)
        ""
        (as-str v)))))

(define scene/derived-mirror-ord
  (lambda (n ord)
    "Derived mirror position: in a ring of n scenes, scene k mirrors scene
n-k+1 (1-based), so the mirror falls out of ord ALONE -- no stored mirror
field is consulted and no scene row is needed. Answers 0 when ord lies
outside 1..n."
    (let
      ((k (parse-int (as-str ord))))
      (if
        (or (< k 1) (> k n))
        0
        (- (+ n 1) k)))))

(define scene/mirror-mismatch?
  (lambda (declared derived)
    "True when a scene's DECLARED mirror slug disagrees with the DERIVED
mirror slug (the slug of the scene sitting at
scene/derived-mirror-ord). A mismatch is either a wrong annotation or a
deliberately broken ring -- broken symmetry is itself a finding."
    (not
      (string-eq
        (as-str declared)
        (as-str derived)))))

;; ---------- ingestion: scdet log → ordered scene-create forms ----------
;;
;; LAPTOP-SIDE BOUNDARY (borge/film-analysis.borg, subsection ingestion):
;; scene boundaries come from the media file ON THE LAPTOP, never from the
;; image. An ffmpeg/scdet run there produces a LOG; film/scdet-times pulls
;; the boundary seconds out of it, film/seconds->frames converts ONCE at
;; the film's fps, and film/ingest-scenes emits (scene-create ...) Nopales
;; source in ord order. The emitter hands pp_eval already-structured rows,
;; so the image never parses media.
;;
;; A RE-INGEST IS A NEW CUT LABEL. These functions are PURE: they create
;; no rows and write no branch, they only emit source. Running the
;; detector at a new threshold takes a new CUT label (e.g. auto-t12 ->
;; auto-t18) and the previous -auto branch is untouched; curation stays a
;; fork, ground truth stays pure, and two cuts diff cleanly.
;;
;; Detector limits are accepted, not hidden: scdet finds CUTS, not
;; narrative scenes (dissolves, fades and match cuts slip or merge), and
;; adjacent detections are merged by hand during curation.

;; The marker ffmpeg's scdet filter writes per detected cut.
(define film/scd/time-marker
  "lavfi.scd.time:")

(define film/scd/marker-pos
  (lambda (line)
    (string-find-pos line film/scd/time-marker 0 15))
  "Index of the scdet time marker in one log line, or -1.")

(define film/scdet-times
  (lambda (log)
    "Boundary timestamps of an scdet/ffmpeg log, in LOG ORDER, as the
detector's SECOND strings: every line carrying a lavfi.scd.time: marker,
trimmed to the number that follows it. This is the ONLY place a second
count exists -- rows are handed frames by film/seconds->frames."
    (list/filter
      (lambda (s) (not (string-eq s "")))
      (list/map
        (lambda (line)
          (let ((pos (film/scd/marker-pos line)))
            (if
              (< pos 0)
              ""
              (string-trim
                (car
                  (string-split
                    (substring
                      line
                      (+ pos 15)
                      (- (string-length line) (+ pos 15)))
                    ","))))))
        (string-split (as-str log) "\n")))))

(define film/seconds->frames
  (lambda (fps secs)
    "A detector SECOND count at the film's fps as an INTEGER frame count,
rounded once. The conversion lives here so a float never reaches a row:
tc_in and dur stay frame integers and seconds stay render-only."
    (round (* (parse-float (as-str fps)) (parse-float (as-str secs))))))

(define film/scd/dedupe
  (lambda (xs acc)
    "Ascending boundary frames with equal NEIGHBOURS collapsed (a detector
that fires on two adjacent frames is one boundary) -- order preserved,
nothing sorted away, so a non-monotonic log stays visible."
    (cond
      ((null? xs) (reverse acc))
      ((null? acc) (film/scd/dedupe (cdr xs) (cons (car xs) acc)))
      ((= (car xs) (car acc)) (film/scd/dedupe (cdr xs) acc))
      (else (film/scd/dedupe (cdr xs) (cons (car xs) acc))))))

(define film/scdet-frames
  (lambda (fps log)
    "Boundary FRAME counts of an scdet log at the film's fps: seconds
converted once, ascending, equal neighbours collapsed."
    (film/scd/dedupe
      (list/map
        (lambda (s) (film/seconds->frames fps s))
        (film/scdet-times log))
      (list))))

(define film/scd/starts
  (lambda (frames)
    "The tc_in of every scene: frame 0 first (scene 1 begins at the head of
the film) followed by each detected boundary that is not already 0."
    (if
      (null? frames)
      (list)
      (if (= (car frames) 0) frames (cons 0 frames)))))

(define film/scd/dur
  (lambda (starts)
    "dur of the scene starting at (car starts): the gap to the NEXT
boundary. The trailer scene answers 0 -- the log names no trailing
boundary, so its length stays open until curation supplies one."
    (if
      (null? (cdr starts))
      0
      (- (car (cdr starts)) (car starts)))))

(define film/scd/q
  (lambda (s)
    "A Nopales string literal for s: quoted, backslash first then quote, so
an emitted form survives a slug or a title with punctuation."
    (string-append
      "\""
      (string-replace (string-replace (as-str s) "\\" "\\\\") "\"" "\\\"")
      "\"")))

(define film/scd/form
  (lambda (film-id cut ord tc dur)
    "ONE (scene-create ...) source form: ord is the 1-based position in the
cut, the slug is CUT-sORD so a machine slug names its own cut, and mirror
is the EMPTY DECLARATION -- the ring is curated on a fork, never here."
    (string-append
      "(scene-create "
      (film/scd/q film-id)
      " "
      (film/scd/q cut)
      " "
      (as-str ord)
      " "
      (film/scd/q (string-append (as-str cut) "-s" (as-str ord)))
      " "
      (film/scd/q "")
      " "
      (film/frames tc)
      " "
      (film/frames dur)
      " "
      (film/scd/q "")
      ")")))

(define film/scd/forms
  (lambda (film-id cut starts ord)
    "Ord-ordered (scene-create ...) source, one form per start."
    (if
      (null? starts)
      (list)
      (cons
        (film/scd/form film-id cut ord (car starts) (film/scd/dur starts))
        (film/scd/forms film-id cut (cdr starts) (+ ord 1))))))

(define film/auto-cut
  (lambda (threshold)
    "The machine cut label a detector threshold lands on: auto-t<N>. A
re-ingest at a NEW threshold therefore takes a NEW label (auto-t12 ->
auto-t18) and the previous -auto branch is untouched -- re-runs are
diffable, curation stays a fork."
    (string-append "auto-t" (as-str (parse-int (as-str threshold))))))

(define film/ingest-scenes
  (lambda (film-id cut fps log)
    "Ingest an scdet log into ORDERED (scene-create ...) Nopales source:
the emitter a laptop pipes to pp_eval. CUT is the machine cut label the
rows land on (e.g. auto-t12); a re-ingest at a new threshold takes a NEW
label, so the previous -auto branch is untouched. PURE: nothing is
created and no branch is written here."
    (film/scd/forms
      film-id
      cut
      (film/scd/starts (film/scdet-frames fps log))
      1)))
