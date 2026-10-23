;; package uke -- the ukulele theory mechanism (borge/ukulele.borg M1)
;;
;; Pure Nopales over songs L0. No DB, no request, no session, no clock:
;; every function takes values and returns values, so the whole core runs
;; under pp-local. No kernel prims and no data rows are added.
;;
;; Values are proper lists (the reader rejects dotted pairs).
;;   frets   = (f0 f1 f2 f3) in PHYSICAL string order G C E A
;;   shape   = (frets midis pcs lowest-pc span fretted)
;;   pc      = integer 0..11 in this file (songs uses (letter alter))
;;   entry   = (pcs root-midi root-str suffix order degrees symmetric?)
;;   record  = (name kind root-str suffix pcs omitted added symmetric?)
;;
;; M1 scope: tuning geometry, exact shape enumeration, reverse naming,
;; chord voicings. Moves, proximity, solo arrangement, and drills are M2+.

(define uke/gcea
  (list 67 60 64 69)
  "Standard tuning in physical string order G C E A (MIDI, reentrant).")

(define uke/tunings
  (list (list "gCEA" uke/gcea))
  "Tuning name -> four MIDI open strings. Data, not a code branch.")

(define uke/window
  12
  "Highest fret enumerated by default.")

(define uke/max-span
  4
  "Playability: max fretted fret minus min fretted fret.")

;; ---------- small list helpers ----------

(define uke/min-list
  (lambda (xs)
    "Smallest number in a non-empty list."
    (list/foldl (lambda (a x) (if (< x a) x a)) (car xs) xs)))

(define uke/max-list
  (lambda (xs)
    "Largest number in a non-empty list."
    (list/foldl (lambda (a x) (if (> x a) x a)) (car xs) xs)))

(define uke/insert
  (lambda (x xs)
    "Insert x into an ascending list."
    (cond
      ((null? xs) (list x))
      ((<= x (car xs)) (cons x xs))
      (else (cons (car xs) (uke/insert x (cdr xs)))))))

(define uke/sort
  (lambda (xs)
    "Ascending insertion sort (lists here are tiny)."
    (list/foldl (lambda (acc x) (uke/insert x acc)) (list) xs)))

(define uke/uniq
  (lambda (xs)
    "Drop adjacent duplicates from an ascending list."
    (if (null? xs)
      (list)
      (uke/uniq-acc (cdr xs) (list (car xs))))))

(define uke/uniq-acc
  (lambda (xs acc)
    "Accumulator for uke/uniq; acc is reversed."
    (cond
      ((null? xs) (reverse acc))
      ((= (car xs) (car acc)) (uke/uniq-acc (cdr xs) acc))
      (else (uke/uniq-acc (cdr xs) (cons (car xs) acc))))))

(define uke/diff
  (lambda (xs ys)
    "Elements of xs not in ys."
    (list/filter (lambda (x) (not (member x ys))) xs)))

(define uke/subset?
  (lambda (xs ys)
    "True when every element of xs is in ys."
    (cond
      ((null? xs) #t)
      ((member (car xs) ys) (uke/subset? (cdr xs) ys))
      (else #f))))

(define uke/index-of
  (lambda (x xs)
    "Index of x in xs, or 99 when absent."
    (uke/index-of-i x xs 0)))

(define uke/index-of-i
  (lambda (x xs i)
    "Index accumulator."
    (cond
      ((null? xs) 99)
      ((equal? x (car xs)) i)
      (else (uke/index-of-i x (cdr xs) (+ i 1))))))

;; ---------- frets and shapes ----------

(define uke/parse-frets
  (lambda (x)
    "Accept a four-digit string, a comma string, or a proper list."
    (cond
      ((string? x)
       (if (null? (songs/find-char x "," 0))
         (songs/frets-list x 0)
         (list/map parse-int (string-split x ","))))
      ((number? x) (list x))
      (else x))))

(define uke/midis
  (lambda (frets)
    "MIDI note per string for a four-fret vector."
    (uke/midis-i (uke/parse-frets frets) uke/gcea 0)))

(define uke/midis-i
  (lambda (frets base i)
    "MIDI accumulator."
    (if (null? frets)
      (list)
      (cons (+ (nth base i) (car frets))
            (uke/midis-i (cdr frets) base (+ i 1))))))

(define uke/pcs
  (lambda (midis)
    "Sorted distinct pitch classes of a MIDI list."
    (uke/uniq (uke/sort (list/map (lambda (m) (songs/fmod m 12)) midis)))))

(define uke/lowest
  (lambda (midis)
    "Lowest sounding MIDI note."
    (uke/min-list midis)))

(define uke/lowest-pc
  (lambda (frets)
    "Pitch class of the actual lowest sounding note."
    (songs/fmod (uke/lowest (uke/midis frets)) 12)))

(define uke/span
  (lambda (frets)
    "Max fretted fret minus min fretted fret; opens ignored."
    (let ((fs (list/filter (lambda (f) (> f 0)) (uke/parse-frets frets))))
      (if (null? fs) 0 (- (uke/max-list fs) (uke/min-list fs))))))

(define uke/fretted
  (lambda (frets)
    "Number of strings stopped by a finger."
    (count (list/filter (lambda (f) (> f 0)) (uke/parse-frets frets)))))

(define uke/shape
  (lambda (frets)
    "Derive the full shape value from a four-fret list."
    (let* ((midis (uke/midis frets))
           (pcs (uke/pcs midis))
           (low (uke/lowest midis))
           (span (uke/span frets))
           (fretted (uke/fretted frets)))
      (list frets midis pcs (songs/fmod low 12) span fretted))))

(define uke/frets-for-pcs
  (lambda (base pcs)
    "Frets on one string that sound any pitch class in pcs."
    (list/filter
      (lambda (f) (member (songs/fmod (+ base f) 12) pcs))
      (list/range (+ uke/window 1)))))

(define uke/prepend-each
  (lambda (x tails)
    "Cons x onto every list in tails."
    (list/map (lambda (t) (cons x t)) tails)))

(define uke/append-all
  (lambda (xss)
    "Flatten one level of a list of lists."
    (list/foldl (lambda (acc xs) (append acc xs)) (list) xss)))

(define uke/cross
  (lambda (choices)
    "Cartesian product of per-string fret choices."
    (if (null? choices)
      (list (list))
      (let ((tails (uke/cross (cdr choices))))
        (uke/append-all
          (list/map (lambda (f) (uke/prepend-each f tails)) (car choices)))))))

(define uke/shapes-for-pcs
  (lambda (pcs)
    "Every playable shape whose sounded set is exactly pcs.
Target-driven: each string only visits frets that sound a wanted pc,
so the search is a few hundred vectors, never the whole neck."
    (let* ((target (uke/uniq (uke/sort pcs)))
           (choices
             (list/map (lambda (base) (uke/frets-for-pcs base target)) uke/gcea))
           (vectors (uke/cross choices)))
      (list/filter
        (lambda (frets)
          (and (<= (uke/span frets) uke/max-span)
               (equal? (uke/pcs (uke/midis frets)) target)))
        vectors))))

;; ---------- the name index ----------

(define uke/quality-order
  (lambda (suffix)
    "Commonness order, taken from the songs quality table."
    (uke/index-of suffix songs/quality-names)))

(define uke/symmetric?
  (lambda (suffix)
    "True for the symmetric qualities (four names per dim7 shape)."
    (or (string-eq suffix "dim7") (string-eq suffix "aug"))))

(define uke/roots
  (lambda ()
    "The twelve roots once: (midi spelled-string)."
    (list/map
      (lambda (r) (list r (songs/pc-str (songs/best-spelling r))))
      (list/range 12))))

(define uke/qualities-table
  (lambda ()
    "Quality rows once: (suffix order degrees symmetric?)."
    (list/map
      (lambda (q)
        (list (car q) (uke/quality-order (car q)) (cdr q) (uke/symmetric? (car q))))
      songs/qualities)))

(define uke/entry*
  (lambda (root-midi root-str q)
    "One index entry from precomputed root and quality rows."
    (let* ((suffix (car q))
           (order (nth q 1))
           (degrees (nth q 2))
           (symmetric (nth q 3))
           (raw (cons root-midi
                      (list/map
                        (lambda (d) (songs/fmod (+ root-midi (nth d 1)) 12))
                        degrees)))
           (pcs (uke/uniq (uke/sort raw))))
      (list pcs root-midi root-str suffix order degrees symmetric))))

(define uke/entry
  (lambda (root-midi q)
    "One index entry for a root and raw quality row (test/debug path)."
    (uke/entry* root-midi
                (songs/pc-str (songs/best-spelling root-midi))
                (list (car q) (uke/quality-order (car q))
                      (cdr q) (uke/symmetric? (car q))))))

(define uke/build-name-index
  (lambda ()
    "Every quality on every root, as a flat entry list."
    (let ((roots (uke/roots))
          (qs (uke/qualities-table)))
      (list/foldl
        (lambda (acc q)
          (append
            acc
            (list/map
              (lambda (root) (uke/entry* (car root) (nth root 1) q))
              roots)))
        (list)
        qs))))

(define uke/name-index-cache
  (delay (uke/build-name-index))
  "Memoized exact-name index; forced by uke/name-index.")

(define uke/name-index
  (lambda ()
    "The exact-name index (built once per session)."
    (force uke/name-index-cache)))

(define uke/chord-value
  (lambda (entry)
    "Rebuild a songs chord value from an index entry (for fit/roman)."
    (list (songs/best-spelling (nth entry 1))
          ()
          (nth entry 3)
          (nth entry 5))))

(define uke/fit-score
  (lambda (entry key)
    "diatonic 3, borrowed 2, pivot 1, chromatic 0."
    (let ((fit (songs/fit (uke/chord-value entry) key)))
      (cond
        ((string-eq fit "diatonic") 3)
        ((string-eq fit "borrowed") 2)
        ((string-eq fit "pivot") 1)
        (else 0)))))

(define uke/exact-score
  (lambda (entry key lowest)
    "Canonical-name rank: key fit, tonic, actual bass, commonness."
    (let* ((root (nth entry 1))
           (fit (if (null? key) 0 (uke/fit-score entry key)))
           (tonic (if (and (not (null? key))
                           (= root (songs/pc-midi (car key))))
                    8
                    0))
           (bass (if (= root lowest) 2 0))
           (common (- 40 (nth entry 4))))
      (+ fit tonic bass common))))

(define uke/best-exact
  (lambda (entries key lowest)
    "The highest-scoring exact entry, or ()."
    (let ((best
            (list/foldl
              (lambda (acc e)
                (let ((s (uke/exact-score e key lowest)))
                  (if (or (null? acc) (> s (car acc)))
                    (list s e)
                    acc)))
              (list)
              entries)))
      (if (null? best) () (car (cdr best))))))

;; ---------- reverse naming ----------

(define uke/pc-list-str
  (lambda (pcs)
    "Spell a pitch-class list with simple accidentals."
    (string-join
      (list/map (lambda (pc) (songs/pc-str (songs/best-spelling pc))) pcs)
      " ")))

(define uke/name-str
  (lambda (entry kind omitted added)
    "Render one interpretation with its provenance."
    (let ((base (string-append (nth entry 2) (nth entry 3))))
      (cond
        ((string-eq kind "exact") base)
        ((string-eq kind "omit")
         (string-append base " (omit " (uke/pc-list-str omitted) ")"))
        ((string-eq kind "add")
         (string-append base " (add " (uke/pc-list-str added) ")"))
        (else
         (string-append base " ~ " (uke/pc-list-str added)))))))

(define uke/record
  (lambda (entry kind omitted added)
    "One result record."
    (list (uke/name-str entry kind omitted added)
          kind
          (nth entry 2)
          (nth entry 3)
          (nth entry 0)
          omitted
          added
          (nth entry 6))))

(define uke/exact-entries
  (lambda (pcs)
    "Index entries whose set equals pcs."
    (list/filter (lambda (e) (equal? pcs (nth e 0))) (uke/name-index))))

(define uke/classify
  (lambda (frets key)
    "All interpretations of a fret vector: exact, omit, add, then near."
    (let* ((midis (uke/midis frets))
           (pcs (uke/pcs midis))
           (lowest (songs/fmod (uke/lowest midis) 12))
           (idx (uke/name-index))
           (exact (list/filter (lambda (e) (equal? pcs (nth e 0))) idx))
           (subs
             (list/filter
               (lambda (e)
                 (and (< (count pcs) (count (nth e 0)))
                      (uke/subset? pcs (nth e 0))))
               idx))
           (sups
             (list/filter
               (lambda (e)
                 (and (> (count pcs) (count (nth e 0)))
                      (uke/subset? (nth e 0) pcs)))
               idx))
           (exact-records
             (list/map (lambda (e) (uke/record e "exact" () ())) exact))
           (sub-records
             (list/map
               (lambda (e) (uke/record e "omit" (uke/diff (nth e 0) pcs) ()))
               subs))
           (sup-records
             (list/map
               (lambda (e) (uke/record e "add" () (uke/diff pcs (nth e 0))))
               sups)))
      (cond
        ((not (null? exact)) exact-records)
        ((not (null? subs)) sub-records)
        ((not (null? sups)) sup-records)
        (else (uke/near-records pcs))))))

(define uke/near-records
  (lambda (pcs)
    "Nearest named sets, one pitch class away."
    (let ((near
            (list/filter
              (lambda (e)
                (<= (count (append (uke/diff pcs (nth e 0))
                                   (uke/diff (nth e 0) pcs)))
                    1))
              (uke/name-index))))
      (list/map
        (lambda (e)
          (uke/record e "near"
                      (uke/diff (nth e 0) pcs)
                      (append (uke/diff pcs (nth e 0))
                              (uke/diff (nth e 0) pcs))))
        near))))

(define uke/names
  (lambda (frets)
    "All interpretations of a fret vector, no key context."
    (uke/classify frets ())))

(define uke/names-in
  (lambda (frets key)
    "All interpretations, ranked with key context."
    (uke/classify frets key)))

(define uke/names-text
  (lambda (frets)
    "Just the names, in classification order."
    (list/map car (uke/names frets))))

(define uke/name-in
  (lambda (frets key)
    "The canonical name under key context, or the best available."
    (let* ((midis (uke/midis frets))
           (pcs (uke/pcs midis))
           (lowest (songs/fmod (uke/lowest midis) 12))
           (exact (uke/exact-entries pcs))
           (best (uke/best-exact exact key lowest)))
      (if (null? best)
        (let ((rs (uke/classify frets key)))
          (if (null? rs) "?" (car (car rs))))
        (uke/name-str best "exact" () ())))))

(define uke/name
  (lambda (frets)
    "The canonical name with no key context."
    (uke/name-in frets ())))

;; ---------- chord voicings ----------

(define uke/chord-pcs
  (lambda (chord)
    "Sorted distinct pitch classes of a songs chord value."
    (uke/uniq (uke/sort (songs/chord-tones chord)))))

(define uke/voicings
  (lambda (chord)
    "Every playable shape that sounds exactly the chord's pitch classes."
    (uke/shapes-for-pcs (uke/chord-pcs chord))))

(define uke/voicings-in
  (lambda (chord near)
    "Voicings ordered by distance from a reference fret vector."
    (uke/sort-frets (uke/voicings chord) (uke/parse-frets near))))

(define uke/sort-frets
  (lambda (frets ref)
    "Insertion sort by fret-distance from ref."
    (list/foldl
      (lambda (acc f) (uke/ins-fret f acc ref))
      (list)
      frets)))

(define uke/ins-fret
  (lambda (f xs ref)
    "Insert fret vector f into an ascending distance list."
    (cond
      ((null? xs) (list f))
      ((<= (uke/fret-distance f ref) (uke/fret-distance (car xs) ref)) (cons f xs))
      (else (cons (car xs) (uke/ins-fret f (cdr xs) ref))))))

(define uke/fret-distance
  (lambda (a b)
    "Sum of per-string fret differences."
    (uke/distance-i a b 0)))

(define uke/distance-i
  (lambda (a b i)
    "Distance accumulator."
    (if (or (null? a) (null? b))
      0
      (let ((d (- (car a) (car b))))
        (+ (if (< d 0) (- 0 d) d)
           (uke/distance-i (cdr a) (cdr b) (+ i 1)))))))

;; ---------- M2: the move grammar ----------
;;
;; A move is a pure operator over (chord, key). Every candidate carries
;; its move name and a one-line reason; ranking is a score, never a
;; hidden model. Proximity and style weights arrive in M3/M6.

(define uke/glyph
  (lambda (key pc quality)
    "Spell a root in the key and append the quality suffix."
    (string-append (songs/spell-tone key pc) quality)))

(define uke/spell-flat
  (lambda (pc)
    "Spell a pitch class with flats (tritone-sub convention)."
    (cond
      ((= pc 0) "C") ((= pc 1) "Db") ((= pc 2) "D") ((= pc 3) "Eb")
      ((= pc 4) "E") ((= pc 5) "F") ((= pc 6) "Gb") ((= pc 7) "G")
      ((= pc 8) "Ab") ((= pc 9) "A") ((= pc 10) "Bb") (else "B"))))

(define uke/chord-key-of
  (lambda (chord)
    "Sounding pitch-class set of a parsed chord, as a string key."
    (string-join
      (list/map as-str (uke/uniq (uke/sort (songs/chord-tones chord))))
      ",")))

(define uke/candidate-str
  (lambda (key root quality move why strength)
    "A parsed candidate record from an already-spelled root, or ()."
    (let* ((glyph (string-append root quality))
           (chord (songs/parse-chord glyph)))
      (if (null? chord)
        ()
        (list glyph move why strength
              (uke/chord-key-of chord)
              (songs/roman chord key))))))

(define uke/candidate
  (lambda (key pc quality move why strength)
    "A parsed candidate record, or () when the glyph is not a chord."
    (uke/candidate-str key (songs/spell-tone key pc) quality move why strength)))

(define uke/candidates
  (lambda (xs)
    "Drop empty candidate slots."
    (list/filter (lambda (x) (not (null? x))) xs)))

(define uke/major-succ
  (list
    (list (list 3 3) (list 4 3) (list 5 2))
    (list (list 4 3) (list 6 1))
    (list (list 5 2) (list 0 1))
    (list (list 0 3) (list 4 2) (list 1 1))
    (list (list 0 3) (list 5 2))
    (list (list 1 2) (list 3 2))
    (list (list 0 3)))
  "Major-key degree -> (degree weight) successors.")

(define uke/minor-succ
  (list
    (list (list 3 3) (list 4 3) (list 5 2))
    (list (list 4 2) (list 6 1))
    (list (list 5 2) (list 0 1))
    (list (list 0 3) (list 4 2) (list 1 1))
    (list (list 0 3) (list 5 2))
    (list (list 1 2) (list 3 2))
    (list (list 0 3)))
  "Natural-minor degree -> (degree weight) successors.")

(define uke/successors
  (lambda (key degree)
    "Successor list for a degree in the key's mode."
    (if (string-eq (car (cdr key)) "minor")
      (nth uke/minor-succ degree)
      (nth uke/major-succ degree))))

(define uke/quality-of-class
  (lambda (class)
    "Diatonic triad class -> glyph suffix."
    (cond
      ((string-eq class "minor") "m")
      ((string-eq class "dim") "dim")
      ((string-eq class "aug") "aug")
      ((string-eq class "sus") "sus4")
      (else ""))))

(define uke/diatonic-moves
  (lambda (chord key)
    "Weighted functional successors, plus every remaining diatonic chord."
    (let* ((d (songs/degree-of (car chord) key))
           (deg (if (null? d) 0 (car d)))
           (succ (uke/successors key deg))
           (scale (songs/scale key))
           (succ-records
             (uke/candidates
               (list/map
                 (lambda (s)
                   (let* ((target (car s))
                          (weight (nth s 1))
                          (pc (songs/pc-midi (nth scale target)))
                          (q (uke/quality-of-class (songs/diatonic-quality key target))))
                     (uke/candidate key pc q "diatonic"
                       (string-append "degree " (as-str (+ target 1)))
                       weight)))
                 succ)))
           (extra-records
             (uke/candidates
               (list/map
                 (lambda (target)
                   (let ((pc (songs/pc-midi (nth scale target)))
                         (q (uke/quality-of-class (songs/diatonic-quality key target))))
                     (uke/candidate key pc q "diatonic"
                       (string-append "diatonic degree " (as-str (+ target 1)))
                       1.0)))
                 (list/range 7)))))
      (append succ-records extra-records))))

(define uke/secondary-moves
  (lambda (key)
    "V7 of every scale degree."
    (let ((scale (songs/scale key)))
      (uke/candidates
        (list/map
          (lambda (i)
            (let* ((target (songs/pc-midi (nth scale i)))
                   (root (songs/fmod (+ target 7) 12))
                   (str (if (= i 0) 3 1.6)))
              (uke/candidate key root "7" "secondary-dominant"
                (string-append "V7 of degree " (as-str (+ i 1)))
                str)))
          (list/range 7))))))

(define uke/tritone-moves
  (lambda (key)
    "Tritone substitution for every secondary dominant."
    (let ((scale (songs/scale key)))
      (uke/candidates
        (list/map
          (lambda (i)
            (let* ((target (songs/pc-midi (nth scale i)))
                   (root (songs/fmod (+ target 1) 12)))
              (uke/candidate-str key (uke/spell-flat root) "7" "tritone-sub"
                (string-append "tritone sub for V7 of degree " (as-str (+ i 1)))
                1.8)))
          (list/range 7))))))

(define uke/dim-moves
  (lambda (key)
    "Leading-tone and upper dim7 approaches to every degree."
    (let ((scale (songs/scale key)))
      (uke/candidates
        (append
          (list/map
            (lambda (i)
              (let ((target (songs/pc-midi (nth scale i))))
                (uke/candidate key (songs/fmod (- target 1) 12) "dim7"
                  "dim-approach"
                  (string-append "leading-tone dim7 to degree " (as-str (+ i 1)))
                  2.2)))
            (list/range 7))
          (list/map
            (lambda (i)
              (let ((target (songs/pc-midi (nth scale i))))
                (uke/candidate key (songs/fmod (+ target 1) 12) "dim7"
                  "dim-approach"
                  (string-append "upper dim7 to degree " (as-str (+ i 1)))
                  1.6)))
            (list/range 7)))))))

(define uke/interchange-moves
  (lambda (key)
    "Same degree in the parallel mode (modal interchange)."
    (let* ((pkey (songs/parallel-key key))
           (scale (songs/scale pkey)))
      (uke/candidates
        (list/map
          (lambda (i)
            (let* ((pc (songs/pc-midi (nth scale i)))
                   (q (uke/quality-of-class (songs/diatonic-quality pkey i))))
              (uke/candidate key pc q "modal-interchange"
                (string-append "parallel-mode degree " (as-str (+ i 1)))
                1.4)))
          (list/range 7))))))

(define uke/shape-moves
  (lambda (chord key)
    "Chromatic mediants and planing: the same quality on a shifted root."
    (let* ((root (songs/pc-midi (car chord)))
           (q (nth chord 2)))
      (uke/candidates
        (append
          (list/map
            (lambda (s)
              (uke/candidate key (songs/fmod (+ root s) 12) q
                "chromatic-mediant"
                (string-append "same quality, root " (uke/signed-str s))
                1.3))
            (list -4 -3 3 4))
          (list/map
            (lambda (s)
              (uke/candidate key (songs/fmod (+ root s) 12) q
                "planing"
                (string-append "same shape slid " (uke/signed-str s))
                1.0))
            (list -2 -1 1 2)))))))

(define uke/signed-str
  (lambda (n)
    "Signed semitone interval for explanations."
    (if (< n 0)
      (string-append "-" (as-str (- 0 n)))
      (string-append "+" (as-str n)))))

(define uke/color-moves
  (lambda (chord key)
    "Quality recolors on the same root."
    (let* ((root (songs/pc-midi (car chord)))
           (q (nth chord 2))
           (class (songs/triad-class q))
           (options
             (cond
               ((string-eq q "7") (list "9" "7sus4" "7b9"))
               ((string-eq class "minor") (list "m7" "m6" "m9"))
               ((string-eq class "sus") (list "" "m"))
               ((string-eq class "dim") (list "m7b5" "dim7"))
               (else (list "sus4" "sus2" "add9" "maj7")))))
      (uke/candidates
        (list/map
          (lambda (oq)
            (uke/candidate key root oq "color"
              (string-append "recolor " q " -> " oq)
              1.4))
          options)))))

(define uke/cadence-move
  (lambda (history key)
    "A detected ii-V gets an explicit tonic resolution."
    (if (< (count history) 2)
      (list)
      (let* ((n (count history))
             (a (songs/degree-of (car (nth history (- n 2))) key))
             (b (songs/degree-of (car (nth history (- n 1))) key))
             (da (if (null? a) 99 (car a)))
             (db (if (null? b) 99 (car b))))
        (if (and (= da 1) (= db 4))
          (list (uke/candidate key (songs/pc-midi (car key)) ""
                  "cadence" "ii-V resolves to the tonic" 4.0))
          (list))))))

(define uke/ins-record
  (lambda (r rs)
    "Insert a candidate into a descending score list."
    (cond
      ((null? rs) (list r))
      ((>= (nth r 3) (nth (car rs) 3)) (cons r rs))
      (else (cons (car rs) (uke/ins-record r (cdr rs)))))))

(define uke/sort-records
  (lambda (rs)
    "Sort candidates by descending score."
    (list/foldl (lambda (acc r) (uke/ins-record r acc)) (list) rs)))

(define uke/record-key
  (lambda (r)
    "Identity of a candidate by its sounding pitch-class set."
    (nth r 4)))

(define uke/same-key?
  (lambda (k acc)
    "True when acc already holds a record with key k."
    (cond
      ((null? acc) #f)
      ((string-eq k (uke/record-key (car acc))) #t)
      (else (uke/same-key? k (cdr acc))))))

(define uke/dedupe-records
  (lambda (rs)
    "Keep the highest-scoring record per sounding set (input sorted)."
    (uke/dedupe-acc rs (list))))

(define uke/dedupe-acc
  (lambda (rs acc)
    "Dedupe accumulator."
    (cond
      ((null? rs) (reverse acc))
      ((uke/same-key? (uke/record-key (car rs)) acc)
       (uke/dedupe-acc (cdr rs) acc))
      (else (uke/dedupe-acc (cdr rs) (cons (car rs) acc))))))

(define uke/next
  (lambda (history key)
    "Ranked next-chord candidates from the current chord and key."
    (if (null? history)
      (list)
      (let* ((chord (nth history (- (count history) 1)))
             (all
               (append
                 (uke/diatonic-moves chord key)
                 (uke/secondary-moves key)
                 (uke/tritone-moves key)
                 (uke/dim-moves key)
                 (uke/interchange-moves key)
                 (uke/shape-moves chord key)
                 (uke/color-moves chord key)
                 (uke/cadence-move history key))))
        (uke/dedupe-records (uke/sort-records all))))))

(define uke/next-text
  (lambda (history key)
    "Candidate lines for the REPL."
    (string-join
      (list/map
        (lambda (r)
          (string-append (car r) "  [" (nth r 1) "]  " (nth r 5)
                         "  " (nth r 2) "  " (as-str (nth r 3))))
        (uke/next history key))
      "\n")))



;; ---------- diagnostics ----------

(define uke/describe
  (lambda (frets)
    "Human-readable shape + interpretations, for the REPL."
    (let* ((s (uke/shape (uke/parse-frets frets)))
           (midis (nth s 1))
           (pcs (nth s 2)))
      (string-append
        "frets " (uke/frets-str (nth s 0))
        " | midi " (uke/nums-str midis)
        " | pcs " (uke/pc-list-str pcs)
        " | lowest " (songs/pc-str (songs/best-spelling (nth s 3)))
        " | span " (as-str (nth s 4))
        " | names " (string-join (uke/names-text frets) ", ")))))

(define uke/frets-str
  (lambda (frets)
    "Comma-joined fret vector."
    (string-join (list/map as-str frets) ",")))

(define uke/nums-str
  (lambda (xs)
    "Space-joined numbers."
    (string-join (list/map as-str xs) " ")))
