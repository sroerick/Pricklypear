;; package stretch -- the stretch encyclopedia (roerick 11-07).
;;
;; One question per row: can this stretch be traced to a historic yoga pose?
;; The provenance is stored (traced_pose, trace_source, trace_year,
;; trace_kind) and the judgement is DERIVED from trace_kind, never stored, so
;; better evidence moves the badge with it instead of leaving a stale claim.
;;
;; trace_kind:
;;   attested  - the pose is named in a pre-1800 hatha text
;;   codified  - it enters the canon through a 19th/20th c. text (the
;;               Sritattvanidhi c. 1830, Light on Yoga 1966) or a modern
;;               lineage's standing form
;;   invented  - no yoga lineage: gym, physio or calisthenics, usually with a
;;               Sanskrit name bolted on afterwards
;;
;; The row type is declared at load (stretch/init!), the todo-comment pattern:
;; a type declared by hand in a live image vanishes on the next reseed (the
;; food deftypes, 11-06).
;;
;; Data is seeded explicitly with (stretch/seed!), never at load: a reseed must
;; not create rows. Routes ride this file (MIRROR boot seed): /stretches and
;; /stretches/detail.
;; Sync: scripts/pp_sync.py pull|push

(define stretch/init!
  (lambda ()
    "Declare the stretch row type (guarded, idempotent; the filmstrip-ws and
     todo-comment pattern). The toplevel call below seeds the declaration at
     boot/reseed as admin; a delegated session no-ops with the denial as the
     value."
    (if
      (null? (type-fields (quote stretch)))
      (deftype
        (quote stretch)
        (quote name)
        (quote slug)
        (quote alt)
        (quote family)
        (quote region)
        (quote muscles)
        (quote setup)
        (quote cues)
        (quote dosage)
        (quote caution)
        (quote equipment)
        (quote traced_pose)
        (quote trace_source)
        (quote trace_year)
        (quote trace_kind)
        (quote trace_note))
      "stretch ok")))

(stretch/init!)

;; ---------------------------------------------------------------------------
;; The judgement, derived
;; ---------------------------------------------------------------------------
(define stretch/kinds
  (list "attested" "codified" "invented"))

(define stretch/badge-words
  (list
      (cons "attested" "faggy")
      (cons "codified" "faggy")
      (cons "invented" "straight"))
    "Display word per trace kind. roerick's direction, 11-07: the authentic
     hatha lineage is the faggy one, the gym and physio inventions read
     straight. This table is the only place the words live, and the honest kind
     is printed next to the badge, so a 15th-century attestation stays
     distinguishable from a 19th-century codification without opening the row.")

(define stretch/badge
  (lambda (kind)
    "The display word for a trace kind; the kind itself when unknown."
    (let ((p (assoc (as-str kind) stretch/badge-words)))
      (if (null? p) (as-str kind) (as-str (cdr p))))))

;; ---------------------------------------------------------------------------
;; Queries
;; ---------------------------------------------------------------------------
(define stretch/all
  (lambda ()
    (rows (quote stretch) 500)))

(define stretch/by-slug
  (lambda (slug)
    (find-row (quote stretch) "slug" (as-str slug))))

(define stretch/f
  (lambda (row key)
    "One field of a row pair as a string, empty when absent."
    (let ((v (dict-get (cdr row) (as-str key))))
      (if (null? v) "" (as-str v)))))

(define stretch/of-kind
  (lambda (kind)
    (if (string-eq (as-str kind) "")
      (stretch/all)
      (list/filter
        (lambda (r) (string-eq (stretch/f r "trace_kind") (as-str kind)))
        (stretch/all)))))

(define stretch/count
  (lambda (rs)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 rs)))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------
(define stretch/row-line
  (lambda (row)
    (html/p*
      (html/attr "class" "stretch-row")
      (html/join
        (list
          (html/a
            (string-append "/stretches/detail?slug=" (stretch/f row "slug"))
            (html/text (stretch/f row "name")))
          (html/span
            (html/attr "class" "badge")
            (html/text
              (string-append
                "  "
                (stretch/badge (stretch/f row "trace_kind"))
                " ("
                (stretch/f row "trace_kind")
                ")")))
          (html/span
            (html/attr "class" "muted")
            (html/text
              (string-append
                "  "
                (stretch/f row "region")
                " · "
                (stretch/f row "family")))))))))

(define stretch/page
  (lambda ()
    "Browse: all stretches, or one trace kind via ?kind=. The count line is the
     point of the app - how much of the canon is attested versus invented."
    (let*
      ((kind (request-param "kind"))
        (rs (stretch/of-kind kind))
        (total (stretch/count (stretch/all))))
      (html/page
        (list
          (html/h1 (html/text "Stretches, by provenance"))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/text
                  (string-append
                    (as-str (stretch/count rs))
                    " of "
                    (as-str total)
                    " shown. "))
                (html/a "/stretches" (html/text "all"))
                (html/text " · ")
                (html/a "/stretches?kind=attested" (html/text "attested"))
                (html/text " · ")
                (html/a "/stretches?kind=codified" (html/text "codified"))
                (html/text " · ")
                (html/a "/stretches?kind=invented" (html/text "invented")))))
          (html/join (list/map stretch/row-line rs)))))))

(define stretch/detail-page
  (lambda ()
    "Detail for one stretch via ?slug=. Provenance is shown in full: the pose
     name the trace rests on, the text and the year, and the judgement derived
     from the kind."
    (let*
      ((slug (request-param "slug"))
        (row (stretch/by-slug slug)))
      (if
        (null? row)
        (html/page
          (list
            (html/h1 (html/text "No such stretch"))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append "Nothing under slug " (as-str slug) ". ")))
            (html/a "/stretches" (html/text "back to the list"))))
        (html/page
          (list
            (html/h1 (html/text (stretch/f row "name")))
            (html/p*
              (html/attr "class" "badge")
              (html/text
                (string-append
                  (stretch/badge (stretch/f row "trace_kind"))
                  " · "
                  (stretch/f row "trace_kind"))))
            (html/p* (html/attr "class" "muted") (html/text (stretch/f row "alt")))
            (html/h2 (html/text "Where"))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append
                  (stretch/f row "region")
                  ": "
                  (stretch/f row "muscles")
                  " · "
                  (stretch/f row "family"))))
            (html/h2 (html/text "Setup"))
            (html/p (stretch/f row "setup"))
            (html/h2 (html/text "Cues"))
            (html/p (stretch/f row "cues"))
            (html/h2 (html/text "Dose and caution"))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append
                  (stretch/f row "dosage")
                  " · "
                  (stretch/f row "caution")
                  " · "
                  (stretch/f row "equipment"))))
            (html/h2 (html/text "Provenance"))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append
                  (stretch/f row "traced_pose")
                  " - "
                  (stretch/f row "trace_source")
                  " ("
                  (stretch/f row "trace_year")
                  ")")))
            (html/p (stretch/f row "trace_note"))
            (html/a "/stretches" (html/text "back to the list"))))))))

;; ---------------------------------------------------------------------------
;; Seed data. Fields in order: name slug alt family region muscles setup cues
;; dosage caution equipment traced_pose trace_source trace_year trace_kind
;; trace_note. Idempotent by slug.
;; ---------------------------------------------------------------------------
(define stretch/seed-row
  (lambda (s)
    (dict-set*
      "{}"
      (list
        "name" (nth s 0)
        "slug" (nth s 1)
        "alt" (nth s 2)
        "family" (nth s 3)
        "region" (nth s 4)
        "muscles" (nth s 5)
        "setup" (nth s 6)
        "cues" (nth s 7)
        "dosage" (nth s 8)
        "caution" (nth s 9)
        "equipment" (nth s 10)
        "traced_pose" (nth s 11)
        "trace_source" (nth s 12)
        "trace_year" (nth s 13)
        "trace_kind" (nth s 14)
        "trace_note" (nth s 15)))))

(define stretch/seeds
  (list
    (list
      "Padmasana" "padmasana" "Lotus" "seated static" "hips, knees, ankles"
      "hip rotators, adductors, ankle dorsiflexors"
      "Sit, right foot onto the left thigh, left foot onto the right thigh, hands on the knees."
      "Sit bones even and spine tall; the knees descend only as far as the hips allow."
      "3-10 min hold"
      "Knee pain or meniscus history: skip it, the knee takes torque here."
      "mat"
      "Padmasana" "Hathapradipika 1.44-49; Gheranda Samhita 2.8" "1450" "attested"
      "One of the seats the hatha corpus actually names; a meditation seat long before it was sold as a stretch.")
    (list
      "Siddhasana" "siddhasana" "Adept's seat, accomplished pose" "seated static" "hips, pelvis"
      "hip rotators, pelvic floor, adductors"
      "Heel to the perineum, the other heel above it, spine upright, hands on the knees."
      "The texts rank it above lotus; keep the pelvis neutral rather than tucked."
      "5-20 min" "Perineal pressure: skip in pregnancy." "mat, cushion"
      "Siddhasana" "Hathapradipika 1.35-43" "1450" "attested"
      "Named as the seat for pranayama, not as a stretch; the pressure is the point in the source.")
    (list
      "Gomukhasana" "gomukhasana" "Cow face" "seated static" "hips, shoulders"
      "glutes, hip rotators, lats, triceps"
      "Stack the knees, feet beside the hips, one hand up the back and one up from below, clasp or strap."
      "Knees in one line; the lower elbow points down, the upper one points up."
      "60-120 s per side" "Shoulder impingement: use a strap instead of clasping." "mat, strap"
      "Gomukhasana" "Hathapradipika 1.20" "1450" "attested"
      "Listed as a seat; the double arm clasp is the later accretion, so the hip half is old and the shoulder half is not.")
    (list
      "Kurmasana" "kurmasana" "Tortoise" "seated static" "back, hips, hamstrings"
      "hamstrings, adductors, spinal erectors"
      "Sit wide, slide the arms under the knees, walk the heels out, chest toward the floor."
      "Round the upper back on purpose and let the legs travel out, do not press them down."
      "30-60 s" "Hamstring origin pain: back off, this one loads the tendon end." "mat"
      "Kurmasana" "Hathapradipika 1.22" "1450" "attested"
      "A hatha asana that survives in modern classes, though with the arms out rather than under.")
    (list
      "Kukkutasana" "kukkutasana" "Rooster" "arm balance" "wrists, core, hips"
      "wrist flexors, abdominals, hip flexors"
      "From lotus, thread the arms between calves and thighs, press the palms down and lift."
      "Elbows tight to the ribs, lift from the belly rather than kicking up."
      "3-5 breaths" "Wrist or knee injury: skip it." "mat"
      "Kukkutasana" "Hathapradipika 1.23" "1450" "attested"
      "Old, and almost extinct outside demonstrations; a hatha asana nobody claims as a stretch.")
    (list
      "Dhanurasana" "dhanurasana" "Bow" "prone backbend" "spine, shoulders, quads"
      "spinal extensors, glutes, quadriceps, pectorals"
      "Prone, catch the ankles, lift the chest and the thighs off the floor."
      "Push the feet back into the hands instead of yanking the ankles up."
      "20-40 s" "Lumbar extension intolerance or pregnancy: skip." "mat"
      "Dhanurasana" "Hathapradipika 1.25" "1450" "attested"
      "In the hatha list, and in the modern canon unchanged in shape.")
    (list
      "Matsyendrasana" "matsyendrasana" "Lord of the fishes twist" "seated twist" "spine, hips, shoulders"
      "obliques, spinal erectors, hip rotators"
      "Seated, one foot outside the opposite knee, twist toward it, hand behind for support."
      "Lengthen up before turning, and let the neck follow last instead of leading."
      "30-60 s per side" "Disc pathology: gentle range only, no leverage from the arm." "mat"
      "Matsyendrasana" "Hathapradipika 1.26" "1450" "attested"
      "Named for Matsyendra, a hatha lineage figure; the full pose in the text is deeper than the half twist taught now.")
    (list
      "Mayurasana" "mayurasana" "Peacock" "arm balance" "wrists, core, elbows"
      "wrist extensors, abdominals, serratus anterior"
      "Kneel, elbows together under the navel, hands turned outward, straighten the legs and lift the body level."
      "Elbows stay together under the belly button, not out to the sides; the head stays up."
      "3-10 breaths" "Wrist, elbow or abdominal surgery: skip." "mat"
      "Mayurasana" "Hathapradipika 1.30-31; Gheranda Samhita 2.30" "1450" "attested"
      "The hatha texts credit it with curing disease, which is the text doing the selling rather than the pose doing the curing.")
    (list
      "Sarvangasana" "sarvangasana" "Shoulderstand, queen of asanas" "inversion" "shoulders, neck, spine"
      "shoulder girdle, abdominals, hip flexors"
      "Shoulder tips on a folded blanket, hips stacked over the ribs, legs vertical."
      "Weight rides on the shoulders, never on the cervical spine; keep the chin off the chest."
      "1-5 min" "Neck injury, glaucoma, uncontrolled hypertension: skip." "mat, blankets"
      "Sarvangasana" "Sritattvanidhi (c. 1830); popularised by Krishnamacharya and Iyengar, 20th c." "1830" "codified"
      "Absent from the hatha classics; it enters the canon through a nineteenth-century Mysore compilation and the twentieth-century lineages after it.")
    (list
      "Bhujangasana" "bhujangasana" "Cobra" "prone backbend" "spine, chest"
      "spinal extensors, pectorals, serratus anterior"
      "Prone, hands under the shoulders, press the chest up while the hips stay down."
      "Legs active and the pubic bone pressing down; do not crank the neck back."
      "15-30 s" "Lumbar compression or pregnancy: skip." "mat"
      "Bhujangasana" "Sritattvanidhi (c. 1830)" "1830" "codified"
      "A nineteenth-century compilation pose that became one of the two default backbends of modern classes.")
    (list
      "Halasana" "halasana" "Plough" "inversion" "spine, shoulders, hamstrings"
      "spinal erectors, hamstrings, shoulder girdle"
      "From shoulderstand, lower the feet to the floor overhead and walk them away."
      "Stay on the shoulders and keep the neck long; blanket under the shoulder tips."
      "30-60 s" "Neck injury or glaucoma: skip." "mat, blankets"
      "Halasana" "Sritattvanidhi (c. 1830)" "1830" "codified"
      "Follows shoulderstand into the canon; the hatha corpus does not name it.")
    (list
      "Trikonasana" "trikonasana" "Triangle" "standing" "hips, hamstrings, side body"
      "hamstrings, adductors, obliques, gluteus medius"
      "Feet wide, front foot turned out, hips stacked, side-bend with a long spine and the bottom hand supported."
      "Keep the top ribs from collapsing forward; the hips stay in one plane."
      "30-60 s per side" "Groin strain: shorten the stance and raise the hand." "mat, block"
      "Trikonasana" "Sritattvanidhi (c. 1830) names a triangle; the modern standing form is Krishnamacharya-line, 20th c." "1935" "codified"
      "The name is old, the shape most people practise is not; that gap is what this app is for.")
    (list
      "Couch stretch" "couch-stretch" "Quad and hip-flexor couch stretch" "static" "quads, hip flexors"
      "rectus femoris, iliopsoas, tensor fasciae latae"
      "Back foot up a wall or the seat of a couch, knee on the floor near the wall, hips forward, torso upright."
      "Tuck the pelvis under first; an arched low back hides the hip flexor."
      "60-120 s per side" "Knee pressure on the floor: pad it." "wall or couch, mat"
      "none" "Kelly Starrett, Becoming a Supple Leopard (2013); no earlier yoga source found" "2013" "invented"
      "Named for the furniture. The closest hatha relative is a kneeling hip opener, a different pose with a different intent.")
    (list
      "Figure-4 glute stretch" "figure-four" "Figure four, pretzel stretch" "static" "glutes, hips"
      "gluteus maximus, gluteus medius, piriformis"
      "Supine, ankle over the opposite knee, draw the supporting thigh toward the chest."
      "Press the crossed knee away from you to increase the stretch instead of pulling harder."
      "30-60 s per side" "Knee pain: lower the foot toward the floor." "mat"
      "none" "20th c. physical therapy and gym practice; no yoga lineage" "1980" "invented"
      "A physio drill with a shape adjacent to gomukhasana and none of its lineage.")
    (list
      "Doorway pec stretch" "doorway-pec" "Doorway chest stretch" "static" "chest, shoulders"
      "pectoralis major, pectoralis minor, anterior deltoid"
      "Forearm on the door frame, elbow at shoulder height, step through until the chest opens."
      "Keep the ribs down; leaning too far turns it into a shoulder crank."
      "30-60 s per side" "Shoulder instability: keep the elbow lower than the shoulder." "doorway"
      "none" "20th c. physiotherapy and rehab practice; no yoga lineage" "1970" "invented"
      "The nearest yoga pose is a wall chest opener, and even that is a twentieth-century teaching device.")))

(define stretch/seed!
  (lambda ()
    "Insert every seed whose slug is absent; returns the number inserted. Run it
     by hand after a deploy - loading the library must not create rows."
    (list/foldl
      (lambda (acc s)
        (if
          (null? (stretch/by-slug (nth s 1)))
          (seq (create (quote stretch) (stretch/seed-row s)) (+ acc 1))
          acc))
      0
      stretch/seeds)))

;; ---------------------------------------------------------------------------
;; Routes (vault-declared; ride the lib body via the MIRROR boot seed)
;; ---------------------------------------------------------------------------
(route "/stretches" "stretch/page" "private")
(route "/stretches/detail" "stretch/detail-page" "private")
