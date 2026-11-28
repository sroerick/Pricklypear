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
    chiasm/shape
    chiasm/pairs
    chiasm/center
    chiasm/tightness
    chiasm/declared
    chiasm/orphans
    film/report
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

;; ---------- chiasm: end-paired ring, center, report ----------
;;
;; CHIASM IS DERIVATION (borge/film-analysis.borg, subsection
;; chiastic-analysis): the pairing is positional, so nobody annotates a
;; pair. chiasm/pairs walks one cut's scene rows with TWO POINTERS -- the
;; first scene pairs the last, then the walk recurses inward on the rows
;; left between them. chiasm/center is that same walk's remainder shape:
;; one scene left over means the ring is ODD and that scene is the
;; CENTER; nothing left means the ring is EVEN and the center is the
;; SEAM between the middle pair. Both shapes are reported truthfully
;; rather than forced to match.
;;
;; film/report turns the walk into a dict/assoc tree: center, shape,
;; pairs (slugs, dur delta in frames, shared motifs), declared (mirror
;; claim vs position mismatches), orphans (plant beats with no
;; downstream payoff) and tightness (the sum of per-pair dur deltas).
;; Everything here is PURE: it reads rows it is handed and creates none.

(define chiasm/length
  (lambda (xs)
    "Number of rows in xs -- the report needs n for the derived mirror."
    (if
      (null? xs)
      0
      (+ 1 (chiasm/length (cdr xs))))))

(define chiasm/last
  (lambda (xs)
    "The LAST row of a non-empty list: the right pointer's first row."
    (if
      (null? (cdr xs))
      (car xs)
      (chiasm/last (cdr xs)))))

(define chiasm/init
  (lambda (xs acc)
    "Every row of a non-empty list but the LAST one: the rows between the
two pointers, i.e. what the pairing walk recurses on."
    (if
      (null? (cdr xs))
      (reverse acc)
      (chiasm/init (cdr xs) (cons (car xs) acc)))))

(define chiasm/abs
  (lambda (n)
    "Absolute value of an integer frame count (a dur DELTA is unsigned:
a pair is tight or loose, never negative)."
    (if (< n 0) (- 0 n) n)))

(define chiasm/scene-slug
  (lambda (scene)
    "A scene row's slug as a string (empty when the row is absent or the
slug is uncurated) -- the name every report row uses for it."
    (if
      (null? scene)
      ""
      (let ((v (dict-get scene "slug")))
        (if (null? v) "" (as-str v))))))

(define chiasm/scene-at
  (lambda (k scenes)
    "The scene row at 1-based position k, or '() when k lies outside the
list. Used to NAME the scene a derived mirror position points at."
    (cond
      ((< k 1) (list))
      ((null? scenes) (list))
      ((= k 1) (car scenes))
      (else (chiasm/scene-at (- k 1) (cdr scenes))))))

(define chiasm/frames-of
  (lambda (scene key)
    "The frame INTEGER a scene row stores under key (ord, dur), 0 when the
row carries none. Frames stay integers here, so a dur delta never picks
up a float or a second count."
    (let ((v (dict-get scene key)))
      (if (null? v) 0 (parse-int (as-str v))))))

(define chiasm/phrase
  (lambda (scene)
    "A scene as its report brief: slug, ord and summary -- enough to NAME
it in a report without handing the whole row back."
    (dict-set* "{}"
      (list
        "slug"
        (chiasm/scene-slug scene)
        "ord"
        (as-str (chiasm/frames-of scene "ord"))
        "summary"
        (let ((v (dict-get scene "summary")))
          (if (null? v) "" (as-str v)))))))

(define chiasm/beats-of-scene
  (lambda (beats slug)
    "The beats that sit in the scene with this curated slug: the beams a
pair can share. An uncurated (\"\") slug shares nothing."
    (list/filter
      (lambda (b) (string-eq (as-str (dict-get b "scene")) slug))
      beats)))

(define chiasm/names-of
  (lambda (beats acc)
    "The DISTINCT motif names a beat list carries, in first-seen order."
    (cond
      ((null? beats) (reverse acc))
      ((null?
         (list/filter
           (lambda (n)
             (string-eq n (as-str (dict-get (car beats) "motif"))))
           acc))
       (chiasm/names-of
         (cdr beats)
         (cons (as-str (dict-get (car beats) "motif")) acc)))
      (else (chiasm/names-of (cdr beats) acc)))))

(define chiasm/shared-acc
  (lambda (names others acc)
    "The names present in BOTH lists, accumulated as one comma-joined
string (so the report row stays a plain string field)."
    (cond
      ((null? names) acc)
      ((null?
         (list/filter
           (lambda (n) (string-eq n (car names)))
           others))
       (chiasm/shared-acc (cdr names) others acc))
      (else
        (chiasm/shared-acc (cdr names) others
          (if (string-eq acc "")
            (car names)
            (string-append acc "," (car names))))))))

(define chiasm/shared-motifs
  (lambda (beats left right)
    "The motifs a PAIR shares: the beats of both scenes' curated slugs,
intersected, comma-joined (\"\" when the pair shares nothing). A strong
overlap only PROPOSES a mirror; the pair itself is positional."
    (chiasm/shared-acc
      (chiasm/names-of (chiasm/beats-of-scene beats left) (list))
      (chiasm/names-of (chiasm/beats-of-scene beats right) (list))
      "")))

(define chiasm/pair
  (lambda (left right beats)
    "ONE end-pair of a ring: both scene slugs and ords, the dur DELTA in
frames (|left dur - right dur|) and the motifs the pair shares. The pair
carries no declared mirror -- position is the whole claim."
    (dict-set* "{}"
      (list
        "left"
        (chiasm/scene-slug left)
        "right"
        (chiasm/scene-slug right)
        "left_ord"
        (as-str (chiasm/frames-of left "ord"))
        "right_ord"
        (as-str (chiasm/frames-of right "ord"))
        "dur_delta"
        (as-str
          (chiasm/abs
            (-
              (chiasm/frames-of left "dur")
              (chiasm/frames-of right "dur"))))
        "shared_motifs"
        (chiasm/shared-motifs
          beats (chiasm/scene-slug left) (chiasm/scene-slug right))))))

(define chiasm/pairs-from
  (lambda (scenes beats acc)
    "chiasm/pairs's two-pointer recursion, outermost pair first: pair the
head with the last row, then recurse on chiasm/init of the tail. One row
left over (or none) is NOT a pair -- it is the center."
    (cond
      ((null? scenes) (reverse acc))
      ((null? (cdr scenes)) (reverse acc))
      (else
        (chiasm/pairs-from
          (chiasm/init (cdr scenes) (list))
          beats
          (cons
            (chiasm/pair (car scenes) (chiasm/last scenes) beats)
            acc))))))

(define chiasm/pairs
  (lambda (scenes)
    "END-PAIRED list of a ring's scene rows: the first scene pairs the
last, the next pairs the second-to-last, and the two pointers recurse
inward. Answers the pair dicts in outward-to-inward order; a ring of one
scene (or none) has no pair at all."
    (chiasm/pairs-from scenes (list) (list))))

(define chiasm/ring-pairs
  (lambda (scenes beats)
    "chiasm/pairs with the film's beats folded in, so each pair also
carries the motifs it shares."
    (chiasm/pairs-from scenes beats (list))))

(define chiasm/center-walk
  (lambda (slow fast)
    "The two-pointer walk behind chiasm/center: slow advances one scene per
TWO fast scenes, so the shape the fast pointer RETURNS names the answer
-- empty means every scene was consumed (EVEN ring, the center is the
seam), a singleton means one scene is left over (ODD ring, and that
scene is the center)."
    (cond
      ((null? fast) (list))
      ((null? (cdr fast)) (list (car slow)))
      (else (chiasm/center-walk (cdr slow) (cdr (cdr fast)))))))

(define chiasm/center
  (lambda (scenes)
    "The CENTER of a ring as a SINGLETON list (odd ring: the center scene)
or '() (even ring: the center is the SEAM between the middle pair). Same
two-pointer recursion as chiasm/pairs, so the center falls out of
structure alone -- no annotation and no length arithmetic."
    (chiasm/center-walk scenes scenes)))

(define chiasm/shape
  (lambda (center)
    "The ring's SHAPE as the report names it: odd when the walk left a
center scene, even when it left only a seam."
    (if (null? center) "even" "odd")))

(define chiasm/center-row
  (lambda (center)
    "The report's center: the center SCENE's brief on an odd ring, the
string \"seam\" on an even one -- so a reader always sees which shape
the film actually has."
    (if (null? center) "seam" (chiasm/phrase (car center)))))

(define chiasm/beat-brief
  (lambda (beat)
    "A beat as its report brief: the scene it sits in, the motif it plants
and the frame it plants it at."
    (dict-set* "{}"
      (list
        "scene"
        (as-str (dict-get beat "scene"))
        "motif"
        (as-str (dict-get beat "motif"))
        "tc"
        (as-str (parse-int (as-str (dict-get beat "tc"))))))))

(define chiasm/payoff-after?
  (lambda (beats motif tc)
    "True when a PAYOFF beat of motif sits at or after frame tc: the
downstream echo a plant was planted for."
    (not
      (null?
        (list/filter
          (lambda (b)
            (and
              (string-eq (as-str (dict-get b "motif")) motif)
              (string-eq (as-str (dict-get b "role")) "payoff")
              (not
                (< (parse-int (as-str (dict-get b "tc"))) tc))))
          beats)))))

(define chiasm/orphans
  (lambda (beats acc)
    "The ORPHAN PLANTS of a film: plant beats with no downstream payoff
beat of the same motif -- a plant the story never pays off. Beat order is
preserved, so the report reads in film order."
    (cond
      ((null? beats) (reverse acc))
      ((and
         (string-eq (as-str (dict-get (car beats) "role")) "plant")
         (not
           (chiasm/payoff-after?
             beats
             (as-str (dict-get (car beats) "motif"))
             (parse-int (as-str (dict-get (car beats) "tc"))))))
       (chiasm/orphans
         (cdr beats)
         (cons (chiasm/beat-brief (car beats)) acc)))
      (else (chiasm/orphans (cdr beats) acc)))))

(define chiasm/declared-one
  (lambda (scene scenes n k acc)
    "ONE declared-mirror report row, or acc unchanged when the scene makes
no claim or its claim matches the position-derived mirror."
    (let ((declared (scene/declared-mirror scene)))
      (if
        (string-eq declared "")
        acc
        (let
          ((derived
             (chiasm/scene-slug
               (chiasm/scene-at (scene/derived-mirror-ord n k) scenes))))
          (if
            (scene/mirror-mismatch? declared derived)
            (cons
              (dict-set* "{}"
                (list
                  "scene"
                  (chiasm/scene-slug scene)
                  "ord"
                  (as-str k)
                  "declared"
                  (as-str declared)
                  "derived"
                  (as-str derived)))
              acc)
            acc))))))

(define chiasm/declared-walk
  (lambda (scenes all n k acc)
    "Walk the ring in ord order collecting chiasm/declared-one's rows."
    (cond
      ((null? scenes) (reverse acc))
      (else
        (chiasm/declared-walk
          (cdr scenes)
          all
          n
          (+ k 1)
          (chiasm/declared-one (car scenes) all n k acc))))))

(define chiasm/declared
  (lambda (scenes)
    "The DECLARED-mirror mismatches of a ring: every curated scene whose
claimed mirror slug is not the scene sitting at the position-derived
mirror (n-k+1). A mismatch is either a wrong annotation or a ring broken
on purpose -- broken symmetry is itself a finding, so both are listed,
never silently repaired."
    (chiasm/declared-walk scenes scenes (chiasm/length scenes) 1 (list))))

(define chiasm/tightness
  (lambda (pairs)
    "The ring's TIGHTNESS: the SUM of per-pair dur deltas in frames, so a
lower number is a tighter ring and the metric moves whenever any
mirrored pair drifts."
    (if
      (null? pairs)
      0
      (+
        (parse-int (as-str (dict-get (car pairs) "dur_delta")))
        (chiasm/tightness (cdr pairs))))))

(define film/report
  (lambda (scenes beats)
    "A ring report over ONE cut's scene rows and the film's beats: a
dict/assoc tree carrying center (the center scene or the string seam),
shape (odd | even), pairs (slugs, dur delta, shared motifs), declared
(mirror claim vs position mismatches), orphans (plant beats with no
downstream payoff) and tightness (the sum of per-pair dur deltas). PURE:
it reads the rows it is handed and creates none."
    (let ((pairs (chiasm/ring-pairs scenes beats)))
      (let ((center (chiasm/center scenes)))
        (dict-set* "{}"
          (list
            "center"
            (chiasm/center-row center)
            "shape"
            (chiasm/shape center)
            "pairs"
            pairs
            "declared"
            (chiasm/declared scenes)
            "orphans"
            (chiasm/orphans beats (list))
            "tightness"
            (as-str (chiasm/tightness pairs))))))))

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
