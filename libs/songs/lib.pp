;; package songs -- chord charts and the layered music representation
;; (borge/songs.borg M1). Work branch admin/dev; promote to system/main
;; once the Kadosh acceptance test passes.
;;
;; L0 algebra: spelled pitch classes, the finite chord-quality table,
;; keys/modes, roman numerals, key-fit. L1: the chart DSL parser/printer.
;; Faces: /songs index + chart + transpose(?key=) + roman(?roman=1) +
;; uke diagrams(?inst=uke) + editor(?edit=1).
;;
;; Values are lists (no records). pitch-class = (letter-index alter).
;; chord = (root bass quality degrees), degrees = list of the proper
;; 2-list (degree semitones). Improper/dotted pairs are NOT used: the
;; reader rejects dotted notation, so the IR is proper lists only
;; (borge/songs.borg representation invariant 3).
;; key = (tonic mode) with mode "major"/"minor".
;; song AST = (meta sections); meta = (key meter tempo).
;;
;; Kernel semantics this package relies on (verified live, admin/dev):
;;   substring is (s start LENGTH) — NOT (start end);
;;   mod is truncated (negative stays negative) -> songs/fmod floors;
;;   only #f is falsy (0 "" () are all truthy);
;;   data rows need EVERY declared field on create AND update.

;; ---------- string helpers ----------

(define songs/blank?
  (lambda (s)
    "True when s is null or an empty string."
    (if (null? s) #t (string-eq (as-str s) ""))))

(define songs/char-at
  (lambda (s i)
    "One-character substring at index i."
    (substring s i 1)))

(define songs/find-char
  (lambda (s ch i)
    "Index of first ch at or after i, or ()."
    (cond
      ((>= i (string-length s)) ())
      ((string-eq (songs/char-at s i) ch) i)
      (else (songs/find-char s ch (+ i 1))))))

(define songs/split-first
  (lambda (s ch)
    "Split s on the first ch into (before after); after is () when absent."
    (let ((i (songs/find-char s ch 0)))
      (if (null? i)
        (list (as-str s) ())
        (list (substring s 0 i) (substring s (+ i 1) (- (string-length s) (+ i 1))))))))

(define songs/starts-with?
  (lambda (s p)
    "True when s begins with p."
    (let ((ls (string-length s)) (lp (string-length p)))
      (if (> lp ls) #f (string-eq (substring s 0 lp) p)))))

(define songs/last-char
  (lambda (s)
    "Final character of s (empty when s is empty)."
    (if (string-eq s "") "" (songs/char-at s (- (string-length s) 1)))))

(define songs/tokens
  (lambda (s)
    "Whitespace-separated non-empty tokens of s."
    (list/filter (lambda (t) (not (string-eq t ""))) (string-split (as-str s) " "))))

(define songs/take
  (lambda (xs n)
    "First n elements of xs."
    (if (or (<= n 0) (null? xs)) (list) (cons (car xs) (songs/take (cdr xs) (- n 1))))))

(define songs/index-of
  (lambda (xs x i)
    "Index of string x in xs at or after i, or ()."
    (cond
      ((null? xs) ())
      ((string-eq (car xs) x) i)
      (else (songs/index-of (cdr xs) x (+ i 1))))))

(define songs/all-digits?
  (lambda (s)
    "True when s is non-empty and all ASCII digits."
    (if (string-eq s "") #f (songs/all-digits-loop s 0))))

(define songs/all-digits-loop
  (lambda (s i)
    "Digit-scan helper."
    (if (>= i (string-length s))
      #t
      (if (string-contains? "0123456789" (songs/char-at s i))
        (songs/all-digits-loop s (+ i 1))
        #f))))

(define songs/safe-int
  (lambda (s)
    "Parse an integer string, or () when not all digits."
    (if (songs/all-digits? (as-str s)) (parse-int (as-str s)) ())))

(define songs/fmod
  (lambda (n m)
    "Floor modulo (mod is truncated): result always in 0..m-1."
    (let ((r (mod n m)))
      (if (< r 0) (+ r m) r))))

(define songs/simple-acc?
  (lambda (p)
    "True when a pitch class's alteration is at most one accidental."
    (let ((a (car (cdr p)))) (and (>= a -1) (<= a 1)))))

;; ---------- L0: spelled pitch algebra ----------

(define songs/letter-semis
  (list 0 2 4 5 7 9 11)
  "Semitone offset of letters C D E F G A B.")
(define songs/letters-lc
  (list "c" "d" "e" "f" "g" "a" "b")
  "Lowercase letter names C D E F G A B.")
(define songs/letters-uc
  (list "C" "D" "E" "F" "G" "A" "B")
  "Uppercase letter names C D E F G A B.")



(define songs/letter-index
  (lambda (s)
    "Index of a letter name in c..b, or ()."
    (songs/index-of songs/letters-lc (string-downcase (as-str s)) 0)))

(define songs/parse-acc
  (lambda (s)
    "Alteration semitones of an accidental suffix, or () when unknown."
    (cond
      ((string-eq s "") 0)
      ((string-eq s "#") 1)
      ((string-eq s "b") -1)
      ((string-eq s "##") 2)
      ((string-eq s "bb") -2)
      (else ()))))

(define songs/parse-pc
  (lambda (s)
    "Parse a pitch class (C, F#, Bb, Ab) into (letter-index alter), or ()."
    (let ((str (string-trim (as-str s))))
      (if (< (string-length str) 1)
        ()
        (let ((li (songs/letter-index (songs/char-at str 0))))
          (if (null? li)
            ()
            (let ((a (songs/parse-acc (substring str 1 (- (string-length str) 1)))))
              (if (null? a) () (list li a)))))))))

(define songs/acc-str
  (lambda (a)
    "Render an alteration as accidentals."
    (cond
      ((= a 0) "")
      ((= a 1) "#")
      ((= a -1) "b")
      ((= a 2) "##")
      ((= a -2) "bb")
      (else ""))))

(define songs/pc-str
  (lambda (pc)
    "Canonical string of a pitch class."
    (string-append (nth songs/letters-uc (car pc)) (songs/acc-str (car (cdr pc))))))

(define songs/pc-midi
  (lambda (pc)
    "Semitone 0-11 of a pitch class."
    (songs/fmod (+ (nth songs/letter-semis (car pc)) (car (cdr pc))) 12)))

(define songs/alter-for
  (lambda (base target)
    "Shortest alteration making base+alter = target mod 12."
    (let ((d (songs/fmod (- target base) 12)))
      (if (> d 6) (- d 12) d))))

;; ---------- L0: chords ----------

(define songs/qualities
  (list
    (list "" (list 3 4) (list 5 7))
    (list "m" (list 3 3) (list 5 7))
    (list "7" (list 3 4) (list 5 7) (list 7 10))
    (list "maj7" (list 3 4) (list 5 7) (list 7 11))
    (list "m7" (list 3 3) (list 5 7) (list 7 10))
    (list "6" (list 3 4) (list 5 7) (list 6 9))
    (list "9" (list 3 4) (list 5 7) (list 7 10) (list 9 14))
    (list "dim" (list 3 3) (list 5 6))
    (list "m7b5" (list 3 3) (list 5 6) (list 7 10))
    (list "aug" (list 3 4) (list 5 8))
    (list "sus2" (list 2 2) (list 5 7))
    (list "sus4" (list 4 5) (list 5 7))
    (list "sus" (list 4 5) (list 5 7))
    (list "add9" (list 3 4) (list 5 7) (list 9 14))
    (list "dim7" (list 3 3) (list 5 6) (list 7 9)))
  "The finite chord-quality table: suffix -> degree/semitone pairs.
Each pair is the proper 2-list (degree semitones) -- dots are not
supported by the reader (representation invariant 3).")
(define songs/quality-names
  (list "" "m" "7" "maj7" "m7" "6" "9" "dim" "m7b5" "aug" "sus2" "sus4" "sus" "add9" "dim7")
  "Accepted quality suffixes, for error messages.")

(define songs/split-root
  (lambda (head)
    "Split a chord head into (root-string quality-string)."
    (let ((n (string-length head)))
      (if (< n 1)
        ()
        (let ((acc (if (>= n 2)
                     (let ((c1 (songs/char-at head 1)))
                       (if (or (string-eq c1 "#") (string-eq c1 "b")) c1 ""))
                     "")))
          (list (string-append (songs/char-at head 0) acc)
                (substring head (+ 1 (string-length acc)) (- (string-length head) (+ 1 (string-length acc))))))))))

(define songs/parse-chord
  (lambda (glyph)
    "Parse a chord glyph (Esus/A, Dm, C#m7b5) into (root bass quality degrees), or ()."
    (let* ((parts (songs/split-first (string-trim (as-str glyph)) "/"))
           (head (string-trim (car parts)))
           (bass-s (car (cdr parts)))
           (rs (songs/split-root head)))
      (if (null? rs)
        ()
        (let ((root (songs/parse-pc (car rs))) (q (car (cdr rs))))
          (if (null? root)
            ()
            (let ((degs (assoc q songs/qualities)))
              (if (null? degs)
                ()
                (let ((bass (if (null? bass-s) () (songs/parse-pc (string-trim bass-s)))))
                  (if (and (not (null? bass-s)) (null? bass))
                    ()
                    (list root bass q (cdr degs))))))))))))

(define songs/chord-str
  (lambda (chord)
    "Canonical string of a parsed chord."
    (let ((root (car chord)) (bass (car (cdr chord))) (q (nth chord 2)))
      (string-append
        (songs/pc-str root)
        q
        (if (null? bass) "" (string-append "/" (songs/pc-str bass)))))))

(define songs/chord-key
  (lambda (chord)
    "Voicing/analysis key: root pc + quality, bass dropped."
    (string-append (songs/pc-str (car chord)) (nth chord 2))))

(define songs/chord-tones
  (lambda (chord)
    "Semitone pitch classes of all chord tones."
    (let ((root-midi (songs/pc-midi (car chord))) (degs (nth chord 3)))
      (cons root-midi
            (list/map (lambda (d) (songs/fmod (+ root-midi (nth d 1)) 12)) degs)))))

(define songs/triad-class
  (lambda (q)
    "Triad family of a quality suffix."
    (cond
      ((or (string-eq q "m") (string-eq q "m7")) "minor")
      ((or (string-eq q "dim") (string-eq q "m7b5") (string-eq q "dim7")) "dim")
      ((string-eq q "aug") "aug")
      ((or (string-eq q "sus") (string-eq q "sus2") (string-eq q "sus4")) "sus")
      (else "major"))))

(define songs/minorish?
  (lambda (chord)
    "True when the chord's triad is minor or diminished."
    (let ((tc (songs/triad-class (nth chord 2))))
      (or (string-eq tc "minor") (string-eq tc "dim")))))

;; ---------- L0: keys, scales, roman, fit ----------

(define songs/parse-key
  (lambda (s)
    "Parse a key (Am, C, F#m) into (tonic mode), or ()."
    (let ((str (string-trim (as-str s))))
      (if (< (string-length str) 1)
        ()
        (let ((mode (if (string-eq (songs/last-char str) "m") "minor" "major")))
          (let ((head (if (string-eq mode "minor")
                        (substring str 0 (- (string-length str) 1))
                        str)))
            (let ((pc (songs/parse-pc head)))
              (if (null? pc) () (list pc mode)))))))))

(define songs/scale
  (lambda (key)
    "Seven spelled pitch classes of the key's scale."
    (let ((tonic (car key)) (mode (car (cdr key))))
      (let ((semis (if (string-eq mode "minor")
                     (list 0 2 3 5 7 8 10)
                     (list 0 2 4 5 7 9 11))))
        (songs/scale-build tonic semis 0)))))

(define songs/scale-build
  (lambda (tonic semis i)
    "Scale builder."
    (if (>= i 7)
      (list)
      (let ((letter (songs/fmod (+ (car tonic) i) 7))
            (target (songs/fmod (+ (songs/pc-midi tonic) (nth semis i)) 12)))
        (cons (list letter (songs/alter-for (nth songs/letter-semis letter) target))
              (songs/scale-build tonic semis (+ i 1)))))))

(define songs/find-midi
  (lambda (scale midi i)
    "Index of first scale pc whose semitone = midi, or ()."
    (cond
      ((>= i 7) ())
      ((= (songs/pc-midi (nth scale i)) midi) i)
      (else (songs/find-midi scale midi (+ i 1))))))

(define songs/degree-of
  (lambda (pc key)
    "0-based scale degree + accidental prefix, matched by LETTER."
    (let* ((tonic (car key))
           (d (songs/fmod (- (car pc) (car tonic)) 7))
           (scale (songs/scale key))
           (dm (songs/pc-midi (nth scale d)))
           (pm (songs/pc-midi pc))
           (diff (songs/fmod (- pm dm) 12)))
      (list d (songs/degree-prefix diff)))))

(define songs/degree-prefix
  (lambda (diff)
    "Accidental prefix for a scale-degree offset in semitones."
    (cond
      ((= diff 0) "")
      ((= diff 1) "#")
      ((= diff 2) "##")
      ((= diff 11) "b")
      ((= diff 10) "bb")
      ((< diff 6) "#")
      (else "b"))))

(define songs/romans
  (list "I" "II" "III" "IV" "V" "VI" "VII")
  "Roman numeral degrees.")

(define songs/roman
  (lambda (chord key)
    "Roman numeral for a chord in a key (borrowed degrees get b/#)."
    (let ((d (songs/degree-of (car chord) key)))
      (if (null? d)
        (string-append "?" (songs/pc-str (car chord)))
        (let ((base (nth songs/romans (car d)))
              (prefix (car (cdr d)))
              (q (nth chord 2)))
          (string-append
            prefix
            (if (songs/minorish? chord) (string-downcase base) base)
            (songs/roman-detail q)))))))

(define songs/roman-detail
  (lambda (q)
    "Figured-bass/inversion-style suffix for a quality."
    (cond
      ((string-eq q "7") "7")
      ((string-eq q "maj7") "maj7")
      ((string-eq q "m7") "7")
      ((string-eq q "m7b5") "ø7")
      ((string-eq q "dim7") "°7")
      ((string-eq q "9") "9")
      ((string-eq q "6") "6")
      ((string-eq q "add9") "add9")
      ((string-eq q "sus") "sus")
      ((string-eq q "sus4") "sus4")
      ((string-eq q "sus2") "sus2")
      ((string-eq q "dim") "°")
      ((string-eq q "aug") "+")
      (else ""))))

(define songs/diatonic-quality
  (lambda (key degree)
    "Expected triad class on a scale degree (natural minor / major)."
    (let ((mode (car (cdr key))))
      (if (string-eq mode "minor")
        (nth (list "minor" "dim" "major" "minor" "minor" "major" "major") degree)
        (nth (list "major" "minor" "minor" "major" "major" "minor" "dim") degree)))))

(define songs/subset?
  (lambda (xs ys)
    "True when every element of xs is in ys."
    (cond
      ((null? xs) #t)
      ((member (car xs) ys) (songs/subset? (cdr xs) ys))
      (else #f))))

(define songs/parallel-key
  (lambda (key)
    "The parallel mode's key (major<->natural minor)."
    (list (car key) (if (string-eq (car (cdr key)) "minor") "major" "minor"))))

(define songs/parallel-scale
  (lambda (key)
    "Semitone set of the parallel mode (major<->natural minor)."
    (list/map songs/pc-midi (songs/scale (songs/parallel-key key)))))

(define songs/pivot?
  (lambda (chord key)
    "True when a non-diatonic chord's root is the parallel mode's tonic."
    (let ((pt (car (songs/parallel-key key))))
      (= (songs/pc-midi (car chord)) (songs/pc-midi pt)))))

(define songs/fit
  (lambda (chord key)
    "diatonic | borrowed | chromatic | pivot classification of a chord in a key."
    (let ((tones (songs/chord-tones chord))
          (set (list/map songs/pc-midi (songs/scale key))))
      (cond
        ((songs/subset? tones set)
         (let ((d (songs/degree-of (car chord) key)))
           (if (and (string-eq (car (cdr d)) "")
                    (string-eq (songs/triad-class (nth chord 2))
                               (songs/diatonic-quality key (car d))))
             "diatonic"
             "borrowed")))
        ((songs/pivot? chord key) "pivot")
        ((songs/subset? tones (songs/parallel-scale key)) "borrowed")
        (else "chromatic")))))

;; ---------- L0: transposition ----------

(define songs/key-interval
  (lambda (from to)
    "Ascending semitones from one key's tonic to another's."
    (songs/fmod (- (songs/pc-midi (car to)) (songs/pc-midi (car from))) 12)))

(define songs/spell-in-key
  (lambda (key midi)
    "Spell a semitone using the key's scale; () when absent."
    (let ((scale (songs/scale key)))
      (let ((i (songs/find-midi scale midi 0)))
        (if (null? i) () (nth scale i))))))

(define songs/best-spelling
  (lambda (midi)
    "Spell a semitone 0..11 with the simplest letter+accidental."
    (let ((best (songs/best-spelling-i midi 0 () 9)))
      (car (cdr best)))))

(define songs/best-spelling-i
  (lambda (midi i best bestcost)
    "Pick the letter/alteration with the fewest accidentals."
    (if (>= i 7)
      (list () best)
      (let* ((base (nth songs/letter-semis i))
             (a (songs/alter-for base midi))
             (cost (if (< a 0) (- 0 a) a)))
        (if (and (< cost bestcost) (>= a -2) (<= a 2))
          (songs/best-spelling-i midi (+ i 1) (list i a) cost)
          (songs/best-spelling-i midi (+ i 1) best bestcost))))))

(define songs/transpose-pc
  (lambda (pc from to)
    "Respell a pitch class into the target key, avoiding double accidentals."
    (let* ((delta (songs/key-interval from to))
           (midi (songs/fmod (+ (songs/pc-midi pc) delta) 12)))
      (let ((spelled (songs/spell-in-key to midi)))
        (if (and (not (null? spelled)) (songs/simple-acc? spelled))
          spelled
          (songs/best-spelling midi))))))

(define songs/transpose-chord
  (lambda (chord from to)
    "Transpose and respell a chord from one key into another."
    (let ((bass (car (cdr chord))))
      (list (songs/transpose-pc (car chord) from to)
            (if (null? bass) () (songs/transpose-pc bass from to))
            (nth chord 2)
            (nth chord 3)))))

;; ---------- L1: chart DSL ----------

(define songs/parse-meter
  (lambda (s)
    "Parse meter N/D into (N D), or ()."
    (let ((parts (list/filter (lambda (p) (not (string-eq p "")))
                              (string-split (string-trim (as-str s)) "/"))))
      (if (not (= (count parts) 2))
        ()
        (let ((a (songs/safe-int (car parts))) (b (songs/safe-int (nth parts 1))))
          (if (or (null? a) (null? b)) () (list a b)))))))

(define songs/bar-ticks
  (lambda (meter)
    "Ticks per bar (12 per beat)."
    (* (car meter) 12)))

(define songs/strip-pipes
  (lambda (s)
    "Inner text of a | ... | group."
    (let* ((n (string-length s))
           (start (if (and (> n 0) (string-eq (songs/char-at s 0) "|")) 1 0))
           (end (if (and (> n start) (string-eq (songs/char-at s (- n 1)) "|")) (- n 1) n)))
      (string-trim (substring s start (- end start))))))

(define songs/events-even
  (lambda (chords start part)
    "Events for chords sharing one bar, each part ticks."
    (if (null? chords)
      (list)
      (let ((c (songs/parse-chord (car chords))))
        (if (null? c)
          (songs/events-even (cdr chords) start part)
          (cons (list c start part)
                (songs/events-even (cdr chords) (+ start part) part)))))))

(define songs/events-bars
  (lambda (chords start ticks)
    "Events for chords each taking one full bar."
    (if (null? chords)
      (list)
      (let ((c (songs/parse-chord (car chords))))
        (if (null? c)
          (songs/events-bars (cdr chords) start ticks)
          (cons (list c start ticks)
                (songs/events-bars (cdr chords) (+ start ticks) ticks)))))))

(define songs/events-from-group
  (lambda (inner meter start)
    "Events for a | ... | group starting at tick start."
    (let ((toks (songs/tokens inner)))
      (if (songs/index-of toks "/" 0)
        (let ((chords (list/filter (lambda (t) (not (string-eq t "/"))) toks)))
          (let ((k (count chords)))
            (if (= k 0)
              (list)
              (songs/events-even chords start (quotient (songs/bar-ticks meter) k)))))
        (songs/events-bars toks start (songs/bar-ticks meter))))))

(define songs/bars-len
  (lambda (bars)
    "Total ticks spanned by a list of events (max end)."
    (songs/bars-len-acc bars 0)))

(define songs/bars-len-acc
  (lambda (bars acc)
    "Accumulator for bars-len."
    (if (null? bars)
      acc
      (let ((e (+ (nth (car bars) 1) (nth (car bars) 2))))
        (songs/bars-len-acc (cdr bars) (if (> e acc) e acc))))))

(define songs/parse-section-header
  (lambda (line key meter tempo)
    "Parse [Label] or [Label key X] -> (name key meter tempo)."
    (let* ((n (string-length line))
           (inner (substring line 1 (if (> n 1) (- n 2) 0)))
           (parts (songs/tokens inner)))
      (if (null? parts)
        (list "" key meter tempo)
        (let ((kw (songs/index-of parts "key" 0)))
          (if (null? kw)
            (list (string-join parts " ") key meter tempo)
            (let ((k (songs/parse-key (if (< (+ kw 1) (count parts)) (nth parts (+ kw 1)) ""))))
              (list (string-join (songs/take parts kw) " ")
                    (if (null? k) key k) meter tempo))))))))

(define songs/flush-section
  (lambda (sec-name key meter tempo bars texts sections)
    "Cons the current section onto the accumulator when non-empty."
    (if (and (songs/blank? sec-name) (null? bars) (null? texts))
      sections
      (cons (list sec-name key meter tempo bars (reverse texts)) sections))))

(define songs/parse-lines
  (lambda (lines key meter tempo sec-name bars texts sections n)
    "Fold chart lines into (meta sections)."
    (cond
      ((null? lines)
       (list (list key meter tempo)
             (reverse (songs/flush-section sec-name key meter tempo bars texts sections))))
      (else
        (let ((line (string-trim (car lines))))
          (cond
            ((string-eq line "")
             (songs/parse-lines (cdr lines) key meter tempo sec-name bars texts sections (+ n 1)))
            ((songs/starts-with? line ";")
             (songs/parse-lines (cdr lines) key meter tempo sec-name bars texts sections (+ n 1)))
            ((songs/starts-with? line "key ")
             (let ((k (songs/parse-key (substring line 4 (- (string-length line) 4)))))
               (songs/parse-lines (cdr lines) (if (null? k) key k) meter tempo sec-name bars texts sections (+ n 1))))
            ((songs/starts-with? line "meter ")
             (let ((m (songs/parse-meter (substring line 6 (- (string-length line) 6)))))
               (songs/parse-lines (cdr lines) key (if (null? m) meter m) tempo sec-name bars texts sections (+ n 1))))
            ((songs/starts-with? line "tempo ")
             (let ((t (songs/safe-int (substring line 6 (- (string-length line) 6)))))
               (songs/parse-lines (cdr lines) key meter (if (null? t) tempo t) sec-name bars texts sections (+ n 1))))
            ((songs/starts-with? line "[")
             (let* ((sections2 (songs/flush-section sec-name key meter tempo bars texts sections))
                    (sh (songs/parse-section-header line key meter tempo)))
               (songs/parse-lines (cdr lines) (nth sh 1) (nth sh 2) (nth sh 3)
                                  (nth sh 0) (list) (list) sections2 (+ n 1))))
            ((songs/starts-with? line "|")
             (let ((evs (songs/events-from-group (songs/strip-pipes line) meter (songs/bars-len bars))))
               (songs/parse-lines (cdr lines) key meter tempo sec-name (append bars evs) texts sections (+ n 1))))
            (else
              (songs/parse-lines (cdr lines) key meter tempo sec-name bars (cons line texts) sections (+ n 1)))))))))

(define songs/parse
  (lambda (body)
    "Parse chart DSL text into (meta sections)."
    (songs/parse-lines (string-split (as-str body) "\n")
                       (songs/parse-key "C") (list 4 4) 100
                       () (list) (list) (list) 1)))

(define songs/validate-lines
  (lambda (lines errs n)
    "Collect (line kind message) problems."
    (cond
      ((null? lines) (reverse errs))
      (else
        (let ((line (string-trim (car lines))))
          (cond
            ((songs/starts-with? line "key ")
             (let ((k (songs/parse-key (substring line 4 (- (string-length line) 4)))))
               (songs/validate-lines (cdr lines)
                 (if (null? k) (cons (list n "key" (string-append "unknown key: " (substring line 4 (- (string-length line) 4)))) errs) errs)
                 (+ n 1))))
            ((songs/starts-with? line "meter ")
             (let ((m (songs/parse-meter (substring line 6 (- (string-length line) 6)))))
               (songs/validate-lines (cdr lines)
                 (if (null? m) (cons (list n "meter" (string-append "bad meter: " (substring line 6 (- (string-length line) 6)))) errs) errs)
                 (+ n 1))))
            ((songs/starts-with? line "|")
             (songs/validate-lines (cdr lines) (songs/validate-bar (songs/tokens (songs/strip-pipes line)) errs n) (+ n 1)))
            (else (songs/validate-lines (cdr lines) errs (+ n 1)))))))))

(define songs/validate-bar
  (lambda (toks errs n)
    "Validate chord tokens on one bar line."
    (cond
      ((null? toks) errs)
      ((string-eq (car toks) "/") (songs/validate-bar (cdr toks) errs n))
      (else
        (let ((c (songs/parse-chord (car toks))))
          (songs/validate-bar (cdr toks)
            (if (null? c)
              (cons (list n "chord" (string-append "unknown chord: " (car toks))) errs)
              errs)
            n))))))

(define songs/validate
  (lambda (body)
    "List of (line kind message) chart parse problems."
    (songs/validate-lines (string-split (as-str body) "\n") (list) 1)))

;; ---------- program front end (program-authority, borge/songs.borg) ----------
;;
;; A composition body is SOURCE with a front-end directive: `form chart`
;; (default) parses the L1 DSL; `form program` evaluates Nopales source in
;; the read-only example-eval sandbox and decodes its canonical form.
;;
;; The score IR is PROPER LISTS ONLY (representation invariant 3), so the
;; native reader is the round-trip authority: (read-string (str score))
;; equals the score. The sandbox returns a printed value, so the bridge is
;; (example-eval BODY) -> (as-str) -> (json-parse); JSON is the WIRE
;; serialization used by engines/manifests, and canonicalizing both front
;; ends through it makes the honesty test parse(chart(p)) == eval(p)
;; decidable with (equal?). (2026-10-03: dots removed from the model, which
;; is what made the native reader able to round-trip a score at all.)

(define songs/directive-index
  (lambda (lines i)
    "Index of the first non-blank, non-comment line, or ()."
    (cond
      ((null? lines) ())
      (else
        (let ((l (string-trim (car lines))))
          (if (or (string-eq l "") (songs/starts-with? l ";"))
            (songs/directive-index (cdr lines) (+ i 1))
            i))))))

(define songs/drop-n
  (lambda (xs n)
    "Drop the first n elements."
    (if (or (<= n 0) (null? xs)) xs (songs/drop-n (cdr xs) (- n 1)))))

(define songs/form
  (lambda (body)
    "Front-end directive of a composition body: program | chart (default)."
    (let* ((lines (string-split (as-str body) "\n"))
           (i (songs/directive-index lines 0)))
      (if (null? i)
        "chart"
        (let* ((first (string-trim (nth lines i)))
               (toks (songs/tokens first)))
          (if (and (songs/starts-with? first "form ") (>= (count toks) 2))
            (nth toks 1)
            "chart"))))))

(define songs/nondet?
  (lambda (body)
    "True when the body's directive marks the program :nondet."
    (let* ((lines (string-split (as-str body) "\n"))
           (i (songs/directive-index lines 0)))
      (if (null? i)
        #f
        (string-contains? (string-trim (nth lines i)) ":nondet")))))

(define songs/program-source
  (lambda (body)
    "The body minus its leading form-directive line, when one is present."
    (let* ((lines (string-split (as-str body) "\n"))
           (i (songs/directive-index lines 0)))
      (if (or (null? i) (not (songs/starts-with? (string-trim (nth lines i)) "form ")))
        (as-str body)
        (string-join (songs/drop-n lines (+ i 1)) "\n")))))

(define songs/json-value
  (lambda (s)
    "Decode a JSON string, unwrapping quoted-string layers."
    (let ((v (json-parse (as-str s))))
      (if (string? v)
        (let ((sv (as-str v)))
          (if (and (> (string-length sv) 1)
                   (or (string-eq (songs/char-at sv 0) "[")
                       (string-eq (songs/char-at sv 0) "{")))
            (songs/json-value sv)
            v))
        v))))

(define songs/canonical
  (lambda (score)
    "Canonical score value: JSON-normalized (dotted pairs -> proper lists)."
    (json-parse (json-emit score))))

(define songs/eval-program
  (lambda (body)
    "Evaluate a form program body in the read-only sandbox -> score value."
    (songs/json-value
      (as-str (example-eval
                (string-append "(json-emit (begin "
                               (songs/program-source body)
                               "))"))))))

(define songs/score
  (lambda (body)
    "A composition body -> canonical score value.
chart (default) parses the DSL; program evaluates in the sandbox."
    (songs/canonical
      (if (string-eq (songs/form body) "program")
        (songs/eval-program body)
        (songs/parse body)))))

(define songs/score-or-error
  (lambda (body)
    "songs/score, or (\"error\" message) when a program is refused or fails."
    (guard
      (lambda (e) (list "error" (nth e 1)))
      (songs/score body))))

;; ---------- model ----------

(define songs/all
  (lambda ()
    "All chordsong rows, up to 200."
    (rows (quote chordsong) 200)))

(define songs/find-by-slug
  (lambda (rs s)
    "Row (id . fields) whose slug = s, or ()."
    (cond
      ((null? rs) ())
      ((string-eq (as-str (dict-get (cdr (car rs)) "slug")) s) (car rs))
      (else (songs/find-by-slug (cdr rs) s)))))

(define songs/by-slug
  (lambda (slug)
    "Row (id . fields) by slug, or ()."
    (songs/find-by-slug (songs/all) (as-str slug))))

(define songs/fields
  (lambda (slug title body)
    "Build chordsong fields JSON."
    (dict-set* "{}" (list "slug" (as-str slug) "title" (as-str title)
                          "body" (as-str body) "tags" "" "day" (date-today)))))

(define songs/save
  (lambda (slug title body)
    "Upsert a chordsong by slug; returns the row id."
    (let ((row (songs/by-slug slug)))
      (if (null? row)
        (create (quote chordsong) (songs/fields slug title body))
        (update (quote chordsong) (car row) (songs/fields slug title body))))))

;; ---------- voicings ----------

(define songs/uke-voicings
  (list
    (list "Am" "2000") (list "A" "2100") (list "A7" "0100")
    (list "Dm" "2210") (list "D" "2220") (list "D7" "2223")
    (list "Em" "0432") (list "E" "4442") (list "E7" "1202")
    (list "G" "0232") (list "G7" "0212") (list "C" "0003")
    (list "C7" "0001") (list "F" "2010") (list "Bb" "3211")
    (list "Bm" "4222") (list "B" "4322") (list "Gm" "0231")
    (list "Cm" "0333") (list "F#m" "2120") (list "Esus" "4400")
    (list "Esus4" "4400") (list "Asus4" "2200"))
  "Seed ukulele voicings (gCEA), chord-key -> 4 fret digits.
Proper-list pairs; dots unsupported (representation invariant 3).")

(define songs/uke-voicing
  (lambda (chord)
    "Frets string for a chord key, or ()."
    (let ((e (assoc (songs/chord-key chord) songs/uke-voicings)))
      (if (null? e) () (nth e 1)))))

(define songs/distinct-chords
  (lambda (sections)
    "Distinct chords across sections, first-seen order."
    (songs/distinct-acc sections (list))))

(define songs/distinct-acc
  (lambda (sections acc)
    "Accumulate distinct chords."
    (if (null? sections)
      (reverse acc)
      (songs/distinct-acc (cdr sections)
        (songs/distinct-bars (nth (car sections) 4) acc)))))

(define songs/distinct-bars
  (lambda (bars acc)
    "Accumulate distinct chords over one section's bars."
    (if (null? bars)
      acc
      (let ((c (car (car bars))))
        (if (songs/has-key? acc (songs/chord-key c))
          (songs/distinct-bars (cdr bars) acc)
          (songs/distinct-bars (cdr bars) (cons c acc)))))))

(define songs/has-key?
  (lambda (acc k)
    "True when a chord with key k is already in acc."
    (cond ((null? acc) #f)
          ((string-eq (songs/chord-key (car acc)) k) #t)
          (else (songs/has-key? (cdr acc) k)))))

;; ---------- rendering ----------

(define songs/css
  (lambda ()
    "The /songs stylesheet block."
    (html/tag "style" ""
      (html/join
        (list
          ".song-chart{margin:1.25rem 0}"
          ".song-sec{margin:1rem 0}"
          ".song-sec h3{margin:.2rem 0;font-size:1rem;letter-spacing:.02em}"
          ".song-bars{display:flex;flex-wrap:wrap;gap:.4rem;margin:.2rem 0}"
          ".song-cell{border:1px solid #cfc9bf;border-radius:6px;padding:.35rem .6rem;min-width:3.2rem;text-align:center;background:#fffdf8}"
          ".song-cell .glyph{font-weight:600;font-size:1.05rem}"
          ".song-cell .rom{display:block;font-size:.72rem;color:#7a6f60;margin-top:.15rem}"
          ".song-cell.fit-borrowed{border-color:#c9a227;background:#fdf6e3}"
          ".song-cell.fit-chromatic{border-color:#b4553f;background:#fbeeea}"
          ".song-cell.fit-pivot{border-color:#6b5ca5;background:#f1eefb}"
          ".song-lyrics{color:#6a6258;font-style:italic;margin:.3rem 0 .6rem;white-space:pre-wrap}"
          ".song-hdr{display:flex;flex-wrap:wrap;gap:.8rem;align-items:baseline;color:#6a6258}"
          ".song-keylinks a{margin-right:.4rem}"
          ".song-diagrams{display:flex;flex-wrap:wrap;gap:.8rem;margin:1rem 0}"
          ".song-diag{border:1px solid #cfc9bf;border-radius:6px;padding:.5rem .6rem;text-align:center;background:#fffdf8}"
          ".song-diag .name{font-weight:600}"
          ".song-diag pre{margin:.35rem 0 0;font-size:.78rem;line-height:1.25}"
          ".song-errors{color:#b4553f;margin:.6rem 0}")))))

(define songs/key-links
  (lambda (slug)
    "Circle-of-fifths transpose links for a song."
    (html/div (html/attr "class" "song-keylinks")
      (html/join
        (list/map
          (lambda (k)
            (html/a (string-append "/songs?slug=" slug "&key=" k) (html/text k)))
          (list "C" "G" "D" "A" "E" "B" "F#" "Db" "Ab" "Eb" "Bb" "F"))))))

(define songs/cell-html
  (lambda (ev shown roman? sec-key)
    "One chord cell; roman computed on the ORIGINAL chord."
    (let* ((chord (car ev))
           (num (if roman? (songs/roman chord sec-key) ""))
           (fit (if roman? (songs/fit chord sec-key) "")))
      (html/tag "span"
        (html/attrs
          (list (html/attr "class" (string-append "song-cell" (if (string-eq fit "") "" (string-append " fit-" fit))))))
        (html/join
          (list
            (html/tag "span" (html/attr "class" "glyph") (html/text (songs/chord-str shown)))
            (if (string-eq num "")
              ""
              (html/tag "span" (html/attr "class" "rom") (html/text num)))))))))

(define songs/lyrics-html
  (lambda (texts)
    "The annotation lines of a section, or the empty string."
    (if (null? texts)
      ""
      (html/div (html/attr "class" "song-lyrics") (html/text (string-join texts "\n"))))))

(define songs/group-bars
  (lambda (bars)
    "Group events by bar start tick (one group per bar)."
    (songs/group-bars-acc bars (list) (list) ())))

(define songs/group-bars-acc
  (lambda (bars cur cur-start groups)
    "Grouping accumulator."
    (cond
      ((null? bars)
       (reverse (if (null? cur) groups (cons (reverse cur) groups))))
      ((null? cur) (songs/group-bars-acc (cdr bars) (list (car bars)) (nth (car bars) 1) groups))
      ((= (nth (car bars) 1) cur-start) (songs/group-bars-acc (cdr bars) (cons (car bars) cur) cur-start groups))
      (else (songs/group-bars-acc (cdr bars) (list (car bars)) (nth (car bars) 1) (cons (reverse cur) groups))))))

(define songs/bar-group-html
  (lambda (group target roman? skey)
    "Render one bar-group (events sharing one bar) as cells."
    (html/div (html/attr "class" "song-bars")
      (html/join
        (list/map
          (lambda (ev)
            (let ((shown (if (null? target) (car ev) (songs/transpose-chord (car ev) skey target))))
              (songs/cell-html ev shown roman? skey)))
          group)))))

(define songs/bars-html
  (lambda (bars target roman? skey)
    "Render a section's bar list, one bar group per row."
    (if (null? bars)
      ""
      (html/div (html/attr "class" "song-chart")
        (html/join
          (list/map (lambda (bg) (songs/bar-group-html bg target roman? skey))
                    (songs/group-bars bars)))))))

(define songs/section-html
  (lambda (sec target roman?)
    "HTML for one section; transposes chord display when target key is set."
    (let* ((name (car sec)) (skey (nth sec 1)) (bars (nth sec 4)) (texts (nth sec 5)))
      (html/div (html/attr "class" "song-sec")
        (html/join
          (list
            (if (songs/blank? name)
              ""
              (html/h3 (html/text (string-append name (if (null? target) "" (string-append "  ·  in " (songs/key-str target)))))))
            (songs/bars-html bars target roman? skey)
            (songs/lyrics-html texts)))))))

(define songs/diagram-html
  (lambda (chord)
    "One ukulele diagram card."
    (let ((frets (songs/uke-voicing chord)))
      (html/div (html/attr "class" "song-diag")
        (html/join
          (list
            (html/div (html/attr "class" "name") (html/text (songs/chord-str chord)))
            (html/tag "pre" "" (html/text
              (if (null? frets)
                "no voicing"
                (songs/fret-table frets))))))))))

(define songs/diagrams-html
  (lambda (sections)
    "The ukulele diagram strip for a song."
    (html/div (html/attr "class" "song-diagrams")
      (html/join (list/map songs/diagram-html (songs/distinct-chords sections))))))

(define songs/frets-list
  (lambda (s i)
    "Digits of a frets string as ints."
    (if (>= i (string-length s))
      (list)
      (cons (songs/safe-int (songs/char-at s i)) (songs/frets-list s (+ i 1))))))

(define songs/mark-loop
  (lambda (i fret)
    "A fret marker: open ring, fretted dot, or empty."
    (if (> i 4)
      ""
      (string-append
        (if (= i fret) "● " (if (= fret 0) (if (= i 1) "○ " "· ") "· "))
        (songs/mark-loop (+ i 1) fret)))))

(define songs/fret-table
  (lambda (frets)
    "A plain-text uke grid for a 4-digit frets string."
    (let ((f (songs/frets-list frets 0)))
      (string-append
        "G " (songs/mark-loop 1 (nth f 0)) "\n"
        "C " (songs/mark-loop 1 (nth f 1)) "\n"
        "E " (songs/mark-loop 1 (nth f 2)) "\n"
        "A " (songs/mark-loop 1 (nth f 3))))))

;; ---------- pages ----------

(define songs/key-str
  (lambda (key)
    "Display string of a key."
    (string-append (songs/pc-str (car key)) (if (string-eq (car (cdr key)) "minor") "m" ""))))

(define songs/count-bars
  (lambda (sections)
    "Total bars across sections."
    (list/foldl (lambda (acc s) (+ acc (count (nth s 4)))) 0 sections)))

(define songs/index-row
  (lambda (row)
    "One index row."
    (let* ((f (cdr row))
           (slug (as-str (dict-get f "slug")))
           (title (as-str (dict-get f "title")))
           (parsed (songs/parse (as-str (dict-get f "body"))))
           (meta (car parsed)))
      (html/tag "tr" ""
        (html/join
          (list
            (html/tag "td" "" (html/a (string-append "/songs?slug=" slug) (html/text title)))
            (html/tag "td" "" (html/text (songs/key-str (car meta))))
            (html/tag "td" "" (html/text (songs/count-bars (car (cdr parsed)))))))))))

(define songs/index-table
  (lambda (rs)
    "Index table of songs."
    (if (null? rs)
      (html/p* (html/attr "class" "muted") (html/text "No songs yet."))
      (html/tag "table" (html/attr "class" "song-index")
        (html/join
          (list
            (html/tag "thead" ""
              (html/tag "tr" ""
                (html/join (list (html/tag "th" "" (html/text "Title"))
                                 (html/tag "th" "" (html/text "Key"))
                                 (html/tag "th" "" (html/text "Bars"))))))
            (html/tag "tbody" ""
              (html/join (list/map songs/index-row rs)))))))))

(define songs/index-page
  (lambda ()
    "GET /songs with no slug: the songbook index."
    (html/page
      (list
        (songs/css)
        (html/h1 "Songs")
        (html/p* (html/attr "class" "muted")
          (html/text "Chord charts on data_rows (type chordsong). One chart DSL per song; transpose, roman analysis, uke diagrams."))
        (songs/index-table (songs/all))))))

(define songs/editor-html
  (lambda (slug title body)
    "The chart editor form."
    (html/div (html/attr "class" "song-editor")
      (html/form "POST" "/songs"
        (html/attrs (list (html/attr "style" "display:flex;flex-direction:column;gap:.5rem;max-width:52rem")))
        (html/join
          (list
            (html/hidden "action" "save")
            (html/hidden "slug" slug)
            (html/label "" (html/join (list (html/text "Title ")
              (html/input (html/attrs (list (html/attr "type" "text") (html/attr "name" "title") (html/attr "value" title)))))))
            (html/tag "textarea" (html/attrs (list (html/attr "name" "body") (html/attr "rows" "18") (html/attr "style" "font-family:monospace;width:100%"))) (html/text body))
            (html/button (html/attr "type" "submit") "Save")))))))

(define songs/chart-body
  (lambda (sections target roman? errs)
    "The chart proper, plus any parse errors."
    (html/join
      (list
        (if (null? errs)
          ""
          (html/div (html/attr "class" "song-errors")
            (html/join
              (list/map (lambda (e) (html/p* "" (html/text (string-append "line " (as-str (car e)) ": " (nth e 2))))) errs))))
        (html/join (list/map (lambda (s) (songs/section-html s target roman?)) sections))))))

(define songs/chart-page
  (lambda (slug)
    "GET /songs?slug=... : the chart view with transpose/roman/uke faces."
    (let ((row (songs/by-slug slug)))
      (if (null? row)
        (html/page (list (html/h1 "Songs") (html/p* (html/attr "class" "muted") (html/text (string-append "No song: " slug)))))
        (let* ((f (cdr row))
               (body (as-str (dict-get f "body")))
               (title (as-str (dict-get f "title")))
               (parsed (songs/parse body))
               (sections (car (cdr parsed)))
               (errs (songs/validate body))
               (qkey (as-str (request-param "key")))
               (key-arg (if (songs/blank? qkey) () (songs/parse-key qkey)))
               (roman? (string-eq (as-str (request-param "roman")) "1"))
               (inst (as-str (request-param "inst")))
               (edit? (string-eq (as-str (request-param "edit")) "1")))
          (html/page
            (list
              (songs/css)
              (html/h1 (html/text title))
              (html/div (html/attr "class" "song-hdr")
                (html/join
                  (list
                    (html/span "" (html/text (string-append "slug " slug)))
                    (html/span "" (html/text (string-append "key " (songs/key-str (car (car parsed))))))
                    (html/a (string-append "/songs?slug=" slug "&edit=1") (html/text "edit"))
                    (html/a (string-append "/songs?slug=" slug "&roman=1") (html/text "roman"))
                    (html/a (string-append "/songs?slug=" slug "&inst=uke") (html/text "uke")))))
              (songs/key-links slug)
              (if edit?
                (songs/editor-html slug title body)
                (songs/chart-body sections key-arg roman? errs))
              (if (string-eq inst "uke")
                (songs/diagrams-html sections)
                ""))))))))

(define songs/handle-post
  (lambda ()
    "POST /songs: save or delete a chart."
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "delete")
         (let ((row (songs/by-slug (as-str (request-param "slug")))))
           (if (null? row)
             (redirect "/songs")
             (seq (delete (quote chordsong) (car row)) (redirect "/songs")))))
        (else
          (seq
            (songs/save (as-str (request-param "slug"))
                        (as-str (request-param "title"))
                        (as-str (request-param "body")))
            (redirect (string-append "/songs?slug=" (as-str (request-param "slug"))))))))))

(define songs/page
  (lambda ()
    "GET/POST /songs — chart index, chart view, editor."
    (if (string-eq (request-method) "POST")
      (songs/handle-post)
      (let ((slug (as-str (request-param "slug"))))
        (if (songs/blank? slug)
          (songs/index-page)
          (songs/chart-page slug))))))
