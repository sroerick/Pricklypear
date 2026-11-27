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
    scene/mirror-mismatch?))

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
