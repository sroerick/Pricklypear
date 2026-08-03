;; Film edit — studio library, cut graph, sound lines/cues, EDL export.
;; Prefix: edit/   Package dir: edit
;; Install: (load-library "edit") — discovered via libs/edit/lib.pp
;;
;; Types use edit- prefix to avoid colliding with generic names.
;; Spec chapter: borge/film-edit.borg (film-edit)
;;
;; v1: rows + expand + canonical hashes + text exports (MLT, Blender
;; VSE script, cue sheet, ffmpeg script). No gen API, no Blender
;; bridge, no automated bounce (those need kernel prims / v2).

(deftype 'edit-asset
  'project 'slug 'kind 'summary 'bible 'appearance 'negative
  'primary_hash 'primary_path 'ref_hashes 'voice_ref 'lora_ref
  'media_id 'meta)

(deftype 'edit-project
  'slug 'title 'fps 'resolution 'budget_cents 'notes)

(deftype 'edit-shot
  'project 'ord 'slug 'summary 'prompt 'dur_target)

(deftype 'edit-shot-cast
  'shot 'asset 'wardrobe 'role)

(deftype 'edit-shot-ref
  'shot 'asset 'purpose 'weight)

(deftype 'edit-take
  'shot 'n 'backend 'model 'seed 'cost_cents
  'media_hash 'media_path 'state 'rating
  'input_snapshot 'source_assets)

(deftype 'edit-edl-entry
  'project 'cut 'ord 'take 'tc_in 'dur 'transition 'audio)

(deftype 'edit-render-job
  'project 'cut 'edl_hash 'state 'out_hash 'out_path 'log)

(deftype 'edit-line
  'project 'shot 'ord 'character 'text 'notes 'take)

(deftype 'edit-cue
  'project 'cut 'ord 'mode 'asset 'tc_start 'dur 'gain_db 'loop)

(deftype 'edit-mix-job
  'project 'cut 'cue_hash 'profile 'state 'out_hash 'out_path 'log)

;; --- helpers ---

(define edit/blank?
  (lambda (v)
    (if (null? v)
        #t
        (string-eq (as-str v) "")))
  "True if value is null or empty string.")

(define edit/as-int
  (lambda (v)
    (if (null? v) 0 (parse-int (as-str v))))
  "Coerce value to int.")

(define edit/id
  (lambda (row)
    (if (pair? row)
        (as-str (car row))
        ""))
  "Row id string; empty if value is a bare fields dict from get.")

(define edit/fields
  (lambda (row)
    (if (pair? row)
        (cdr row)
        row))
  "Fields JSON from a rows pair or a bare get result.")

(define edit/f
  (lambda (row key)
    (as-str (dict-get (edit/fields row) key)))
  "Field string from rows pair or get result.")

(define edit/tr (lambda (body) (html/tag "tr" "" body)))
(define edit/td (lambda (body) (html/tag "td" "" body)))
(define edit/th (lambda (body) (html/tag "th" "" body)))
(define edit/table (lambda (body) (html/tag "table" "" body)))
(define edit/thead (lambda (body) (html/tag "thead" "" body)))
(define edit/tbody (lambda (body) (html/tag "tbody" "" body)))

(define edit/input
  (lambda (name value)
    (html/text-input name (html/attr "value" (as-str value))))
  "Text input with value= attr (edit/input takes attrs, not raw value).")

(define edit/escape-xml
  (lambda (s)
    (list/foldl
      (lambda (acc ch)
        (string-append acc
          (if (string-eq ch "&") "&amp;"
            (if (string-eq ch "<") "&lt;"
              (if (string-eq ch ">") "&gt;"
                (if (string-eq ch "\"") "&quot;" ch))))))
      ""
      (edit/chars (as-str s))))
  "Minimal XML escape.")

(define edit/chars
  (lambda (s)
    (edit/chars-acc s 0 (string-length s) (list)))
  "Split string into 1-char strings.")

(define edit/chars-acc
  (lambda (s i n acc)
    (if (>= i n)
        (reverse acc)
        (edit/chars-acc s (+ i 1) n
          (cons (substring s i 1) acc)))))

(define edit/join-lines
  (lambda (xs)
    (list/foldl
      (lambda (acc x)
        (if (string-eq acc "")
            (as-str x)
            (string-append acc "\n" (as-str x))))
      ""
      xs))
  "Join strings with newlines.")

(define edit/pad2
  (lambda (n)
    (let ((s (as-str n)))
      (if (< (edit/as-int n) 10)
          (string-append "0" s)
          s)))
  "Zero-pad small ints for timecode.")

(define edit/mod
  (lambda (n d)
    (if (<= (edit/as-int d) 0)
        0
        (- (edit/as-int n)
           (* (/ (edit/as-int n) (edit/as-int d)) (edit/as-int d)))))
  "Non-negative integer modulo via truncating division.")

(define edit/frames->tc
  (lambda (frames fps)
    (let ((f (edit/as-int frames))
          (r (if (<= (edit/as-int fps) 0) 24 (edit/as-int fps))))
      (let ((ff (edit/mod f r))
            (total-sec (/ f r)))
        (let ((ss (edit/mod total-sec 60))
              (total-min (/ total-sec 60)))
          (let ((mm (edit/mod total-min 60))
                (hh (/ total-min 60)))
            (string-append
              (edit/pad2 hh) ":"
              (edit/pad2 mm) ":"
              (edit/pad2 ss) ":"
              (edit/pad2 ff)))))))
  "Integer frames to HH:MM:SS:FF at fps.")

;; --- projects ---

(define edit/projects
  (lambda ()
    (rows 'edit-project 100))
  "List edit projects.")

(define edit/project-by-slug
  (lambda (slug)
    (let ((want (as-str slug)))
      (list/foldl
        (lambda (acc row)
          (if (null? acc)
              (if (string-eq (edit/f row "slug") want) row acc)
              acc))
        ()
        (edit/projects))))
  "Find project row by slug.")

(define edit/project-get
  (lambda (id) (get 'edit-project (as-str id)))
  "Get project by id.")

(define edit/project-upsert
  (lambda (slug title fps resolution budget-cents notes)
    (let ((existing (edit/project-by-slug slug)))
      (if (null? existing)
          (create 'edit-project
            (dict-set* "{}"
              (list
                "slug" (as-str slug)
                "title" (as-str title)
                "fps" (as-str fps)
                "resolution" (as-str resolution)
                "budget_cents" (as-str budget-cents)
                "notes" (as-str notes))))
          (seq
            (update 'edit-project (edit/id existing)
              (dict-set* (edit/fields existing)
                (list
                  "title" (as-str title)
                  "fps" (as-str fps)
                  "resolution" (as-str resolution)
                  "budget_cents" (as-str budget-cents)
                  "notes" (as-str notes))))
            (edit/id existing)))))
  "Create or update project by slug; returns id.")

;; --- assets (studio library) ---

(define edit/assets
  (lambda ()
    (rows 'edit-asset 200))
  "All assets on branch.")

(define edit/assets-for
  (lambda (project kind)
    (let ((p (as-str project))
          (k (as-str kind)))
      (list/filter
        (lambda (row)
          (let ((ok-p
                  (if (edit/blank? p)
                      #t
                      (or (string-eq (edit/f row "project") p)
                          (string-eq (edit/f row "project") ""))))
                (ok-k
                  (if (edit/blank? k)
                      #t
                      (string-eq (edit/f row "kind") k))))
            (and ok-p ok-k)))
        (edit/assets))))
  "Filter assets by project (incl studio-wide) and optional kind.")

(define edit/asset-by-slug
  (lambda (project slug)
    (let ((p (as-str project))
          (s (as-str slug)))
      (list/foldl
        (lambda (acc row)
          (if (null? acc)
              (if (and (string-eq (edit/f row "slug") s)
                       (or (string-eq (edit/f row "project") p)
                           (string-eq (edit/f row "project") "")))
                  row
                  acc)
              acc))
        ()
        (edit/assets))))
  "Lookup asset: prefer project scope then studio-wide (first match).")

(define edit/asset-get
  (lambda (id) (get 'edit-asset (as-str id)))
  "Get asset by id.")

(define edit/asset-upsert
  (lambda (project slug kind summary bible appearance negative
           primary-hash primary-path ref-hashes voice-ref lora-ref
           media-id meta)
    (let ((existing (edit/asset-by-slug project slug)))
      (let ((fields
              (dict-set* "{}"
                (list
                  "project" (as-str project)
                  "slug" (as-str slug)
                  "kind" (as-str kind)
                  "summary" (as-str summary)
                  "bible" (as-str bible)
                  "appearance" (as-str appearance)
                  "negative" (as-str negative)
                  "primary_hash" (as-str primary-hash)
                  "primary_path" (as-str primary-path)
                  "ref_hashes" (as-str ref-hashes)
                  "voice_ref" (as-str voice-ref)
                  "lora_ref" (as-str lora-ref)
                  "media_id" (as-str media-id)
                  "meta" (as-str meta)))))
        (if (null? existing)
            (create 'edit-asset fields)
            (seq
              (update 'edit-asset (edit/id existing)
                (dict-set* (edit/fields existing)
                  (list
                    "kind" (as-str kind)
                    "summary" (as-str summary)
                    "bible" (as-str bible)
                    "appearance" (as-str appearance)
                    "negative" (as-str negative)
                    "primary_hash" (as-str primary-hash)
                    "primary_path" (as-str primary-path)
                    "ref_hashes" (as-str ref-hashes)
                    "voice_ref" (as-str voice-ref)
                    "lora_ref" (as-str lora-ref)
                    "media_id" (as-str media-id)
                    "meta" (as-str meta))))
              (edit/id existing))))))
  "Upsert asset by project+slug; returns id.")

;; --- shots ---

(define edit/shots
  (lambda (project)
    (let ((p (as-str project)))
      (edit/sort-by-ord
        (list/filter
          (lambda (row) (string-eq (edit/f row "project") p))
          (rows 'edit-shot 300)))))
  "Shots for project slug, ordered.")

(define edit/shot-get
  (lambda (id) (get 'edit-shot (as-str id)))
  "Get shot by id.")

(define edit/shot-create
  (lambda (project ord slug summary prompt dur-target)
    (create 'edit-shot
      (dict-set* "{}"
        (list
          "project" (as-str project)
          "ord" (as-str ord)
          "slug" (as-str slug)
          "summary" (as-str summary)
          "prompt" (as-str prompt)
          "dur_target" (as-str dur-target)))))
  "Create shot; returns id.")

(define edit/sort-by-ord
  (lambda (rows-in)
    (edit/sort-rows rows-in "ord"))
  "Sort rows by integer ord field ascending.")

(define edit/sort-rows
  (lambda (xs key)
    (if (null? xs)
        (list)
        (edit/insert-sorted
          (car xs)
          (edit/sort-rows (cdr xs) key)
          key)))
  "Insertion sort by integer field.")

(define edit/insert-sorted
  (lambda (row xs key)
    (if (null? xs)
        (list row)
        (if (<= (edit/as-int (edit/f row key))
                (edit/as-int (edit/f (car xs) key)))
            (cons row xs)
            (cons (car xs) (edit/insert-sorted row (cdr xs) key)))))
  "Insert row into sorted list by key.")

;; --- cast / refs ---

(define edit/shot-casts
  (lambda (shot-id)
    (let ((s (as-str shot-id)))
      (list/filter
        (lambda (row) (string-eq (edit/f row "shot") s))
        (rows 'edit-shot-cast 200))))
  "Cast bindings for a shot.")

(define edit/shot-cast
  (lambda (shot-id asset-id role wardrobe-id)
    (create 'edit-shot-cast
      (dict-set* "{}"
        (list
          "shot" (as-str shot-id)
          "asset" (as-str asset-id)
          "wardrobe" (as-str wardrobe-id)
          "role" (as-str role)))))
  "Bind asset to shot as cast.")

(define edit/shot-cast-clear
  (lambda (shot-id)
    (list/map
      (lambda (row) (delete 'edit-shot-cast (edit/id row)))
      (edit/shot-casts shot-id)))
  "Remove all cast bindings for shot.")

(define edit/shot-refs
  (lambda (shot-id)
    (let ((s (as-str shot-id)))
      (list/filter
        (lambda (row) (string-eq (edit/f row "shot") s))
        (rows 'edit-shot-ref 200))))
  "Ref bindings for a shot.")

(define edit/shot-ref
  (lambda (shot-id asset-id purpose weight)
    (create 'edit-shot-ref
      (dict-set* "{}"
        (list
          "shot" (as-str shot-id)
          "asset" (as-str asset-id)
          "purpose" (as-str purpose)
          "weight" (as-str weight)))))
  "Bind non-cast ref asset to shot.")

(define edit/shots-using-asset
  (lambda (asset-id)
    (let ((a (as-str asset-id)))
      (let ((cast-shots
              (list/map
                (lambda (row) (edit/f row "shot"))
                (list/filter
                  (lambda (row)
                    (or (string-eq (edit/f row "asset") a)
                        (string-eq (edit/f row "wardrobe") a)))
                  (rows 'edit-shot-cast 500))))
            (ref-shots
              (list/map
                (lambda (row) (edit/f row "shot"))
                (list/filter
                  (lambda (row) (string-eq (edit/f row "asset") a))
                  (rows 'edit-shot-ref 500)))))
        (edit/unique-strs (edit/append cast-shots ref-shots)))))
  "Shot ids that bind this asset via cast or ref.")

(define edit/append
  (lambda (a b)
    (if (null? a) b (cons (car a) (edit/append (cdr a) b))))
  "Append two lists.")

(define edit/unique-strs
  (lambda (xs)
    (list/foldl
      (lambda (acc x)
        (if (edit/member-str? (as-str x) acc)
            acc
            (cons (as-str x) acc)))
      (list)
      xs))
  "Unique string list (order reversed ok).")

(define edit/member-str?
  (lambda (s xs)
    (if (null? xs)
        #f
        (if (string-eq s (as-str (car xs)))
            #t
            (edit/member-str? s (cdr xs)))))
  "String membership.")

;; --- takes ---

(define edit/takes
  (lambda (shot-id)
    (let ((s (as-str shot-id)))
      (edit/sort-rows
        (list/filter
          (lambda (row) (string-eq (edit/f row "shot") s))
          (rows 'edit-take 300))
        "n")))
  "Takes for shot, by take number.")

(define edit/take-get
  (lambda (id) (get 'edit-take (as-str id)))
  "Get take by id.")

(define edit/take-create
  (lambda (shot-id n backend model seed cost media-hash media-path
           state rating input-snapshot source-assets)
    (create 'edit-take
      (dict-set* "{}"
        (list
          "shot" (as-str shot-id)
          "n" (as-str n)
          "backend" (as-str backend)
          "model" (as-str model)
          "seed" (as-str seed)
          "cost_cents" (as-str cost)
          "media_hash" (as-str media-hash)
          "media_path" (as-str media-path)
          "state" (as-str state)
          "rating" (as-str rating)
          "input_snapshot" (as-str input-snapshot)
          "source_assets" (as-str source-assets)))))
  "Create take row; returns id.")

(define edit/take-set-state
  (lambda (take-id state)
    (update-field 'edit-take (as-str take-id) "state" (as-str state)))
  "Update take state.")

(define edit/choose
  (lambda (shot-id take-n)
    (let ((want (as-str take-n)))
      (list/map
        (lambda (row)
          (if (string-eq (edit/f row "n") want)
              (edit/take-set-state (edit/id row) "done")
              (if (string-eq (edit/f row "state") "done")
                  (edit/take-set-state (edit/id row) "rejected")
                  ())))
        (edit/takes shot-id))))
  "Mark take n chosen (done); demote prior done takes to rejected.")

(define edit/chosen-take
  (lambda (shot-id)
    (list/foldl
      (lambda (acc row)
        (if (null? acc)
            (if (string-eq (edit/f row "state") "done") row acc)
            acc))
      ()
      (edit/takes shot-id)))
  "First take with state=done for shot.")

(define edit/promote-take
  (lambda (take-id kind slug)
    (let ((trow (edit/take-get take-id)))
      (if (null? trow)
          ""
          (let ((shot (edit/shot-get (edit/f trow "shot"))))
            (let ((proj
                    (if (null? shot) "" (edit/f shot "project"))))
              (edit/asset-upsert
                proj
                slug
                kind
                (string-append "promoted from take " (as-str take-id))
                ""
                ""
                ""
                (edit/f trow "media_hash")
                (edit/f trow "media_path")
                "[]"
                ""
                ""
                ""
                "{}"))))))
  "Mint asset from landed take media; returns asset id.")

;; --- expand (pure preview of gen bundle) ---

(define edit/expand
  (lambda (shot-id)
    (let ((shot (edit/shot-get shot-id)))
      (if (null? shot)
          "{}"
          (let ((casts (edit/shot-casts shot-id))
                (refs (edit/shot-refs shot-id)))
            (let ((appearances
                    (edit/join-lines
                      (list/map edit/cast-appearance-line casts)))
                  (ref-bits
                    (edit/join-lines
                      (list/map edit/ref-appearance-line refs)))
                  (hashes
                    (edit/join-lines
                      (edit/unique-strs
                        (edit/append
                          (list/map edit/cast-hashes casts)
                          (list/map edit/ref-hashes-line refs)))))
                  (asset-ids
                    (edit/join-lines
                      (edit/unique-strs
                        (edit/append
                          (list/map
                            (lambda (r) (edit/f r "asset"))
                            casts)
                          (list/map
                            (lambda (r) (edit/f r "asset"))
                            refs))))))
              (dict-set* "{}"
                (list
                  "shot" (as-str shot-id)
                  "prompt" (edit/f shot "prompt")
                  "cast_appearance" appearances
                  "ref_appearance" ref-bits
                  "ref_hashes" hashes
                  "source_assets" asset-ids
                  "expanded_prompt"
                  (edit/join-lines
                    (list
                      (edit/f shot "prompt")
                      appearances
                      ref-bits)))))))))
  "Pure expansion of cast/refs into a gen input bundle (JSON string).")

(define edit/cast-appearance-line
  (lambda (crow)
    (let ((a (edit/asset-get (edit/f crow "asset")))
          (w-id (edit/f crow "wardrobe")))
      (let ((base
              (if (null? a)
                  ""
                  (string-append
                    (edit/f a "slug") ": "
                    (edit/f a "appearance"))))
            (ward
              (if (edit/blank? w-id)
                  ""
                  (let ((w (edit/asset-get w-id)))
                    (if (null? w)
                        ""
                        (string-append
                          " wardrobe "
                          (edit/f w "slug") ": "
                          (edit/f w "appearance")))))))
        (edit/join-lines
          (list/filter
            (lambda (s) (not (edit/blank? s)))
            (list base ward))))))
  "Appearance line for one cast binding.")

(define edit/ref-appearance-line
  (lambda (rrow)
    (let ((a (edit/asset-get (edit/f rrow "asset"))))
      (if (null? a)
          ""
          (string-append
            (edit/f rrow "purpose") " "
            (edit/f a "slug") ": "
            (edit/f a "appearance")))))
  "Appearance line for one ref binding.")

(define edit/cast-hashes
  (lambda (crow)
    (let ((a (edit/asset-get (edit/f crow "asset")))
          (w-id (edit/f crow "wardrobe")))
      (edit/join-lines
        (list/filter
          (lambda (s) (not (edit/blank? s)))
          (list
            (if (null? a) "" (edit/f a "primary_hash"))
            (if (null? a) "" (edit/f a "ref_hashes"))
            (if (edit/blank? w-id)
                ""
                (let ((w (edit/asset-get w-id)))
                  (if (null? w) "" (edit/f w "primary_hash")))))))))
  "Hash material from cast binding.")

(define edit/ref-hashes-line
  (lambda (rrow)
    (let ((a (edit/asset-get (edit/f rrow "asset"))))
      (if (null? a)
          ""
          (edit/join-lines
            (list
              (edit/f a "primary_hash")
              (edit/f a "ref_hashes"))))))
  "Hash material from ref binding.")

;; --- EDL ---

(define edit/edl
  (lambda (project cut)
    (let ((p (as-str project))
          (c (as-str cut)))
      (edit/sort-by-ord
        (list/filter
          (lambda (row)
            (and (string-eq (edit/f row "project") p)
                 (string-eq (edit/f row "cut") c)))
          (rows 'edit-edl-entry 500)))))
  "EDL entries for project+cut.")

(define edit/edl-add
  (lambda (project cut ord take-id tc-in dur transition audio)
    (create 'edit-edl-entry
      (dict-set* "{}"
        (list
          "project" (as-str project)
          "cut" (as-str cut)
          "ord" (as-str ord)
          "take" (as-str take-id)
          "tc_in" (as-str tc-in)
          "dur" (as-str dur)
          "transition" (as-str transition)
          "audio" (as-str audio)))))
  "Append EDL entry.")

(define edit/edl-rebuild-from-shots
  (lambda (project cut)
    (let ((shots (edit/shots project)))
      (seq
        (list/map
          (lambda (e) (delete 'edit-edl-entry (edit/id e)))
          (edit/edl project cut))
        (list/map
          (lambda (shot)
            (let ((ch (edit/chosen-take (edit/id shot))))
              (if (null? ch)
                  ()
                  (edit/edl-add
                    project
                    cut
                    (edit/f shot "ord")
                    (edit/id ch)
                    "0"
                    (edit/f shot "dur_target")
                    "cut"
                    (edit/line-audio-for-shot (edit/id shot))))))
          shots))))
  "Rebuild cut EDL from chosen takes on each shot.")

(define edit/edl-canonical
  (lambda (project cut)
    (edit/join-lines
      (list/map
        (lambda (e)
          (string-append
            (edit/f e "ord") "|"
            (edit/f e "take") "|"
            (edit/f e "tc_in") "|"
            (edit/f e "dur") "|"
            (edit/f e "transition") "|"
            (edit/f e "audio")))
        (edit/edl project cut))))
  "Canonical EDL print for hashing.")

(define edit/edl-hash
  (lambda (project cut)
    (sha1 (edit/edl-canonical project cut)))
  "Cache key for picture EDL.")

;; --- lines (dialogue) ---

(define edit/lines
  (lambda (shot-id)
    (let ((s (as-str shot-id)))
      (edit/sort-by-ord
        (list/filter
          (lambda (row) (string-eq (edit/f row "shot") s))
          (rows 'edit-line 300)))))
  "Dialogue lines for a shot.")

(define edit/lines-project
  (lambda (project)
    (let ((p (as-str project)))
      (edit/sort-by-ord
        (list/filter
          (lambda (row) (string-eq (edit/f row "project") p))
          (rows 'edit-line 500)))))
  "All lines for a project.")

(define edit/line-upsert
  (lambda (project shot ord character text notes)
    (let ((existing
            (list/foldl
              (lambda (acc row)
                (if (null? acc)
                    (if (and (string-eq (edit/f row "shot") (as-str shot))
                             (string-eq (edit/f row "ord") (as-str ord)))
                        row
                        acc)
                    acc))
              ()
              (edit/lines shot))))
      (if (null? existing)
          (create 'edit-line
            (dict-set* "{}"
              (list
                "project" (as-str project)
                "shot" (as-str shot)
                "ord" (as-str ord)
                "character" (as-str character)
                "text" (as-str text)
                "notes" (as-str notes)
                "take" "")))
          (seq
            (update 'edit-line (edit/id existing)
              (dict-set* (edit/fields existing)
                (list
                  "character" (as-str character)
                  "text" (as-str text)
                  "notes" (as-str notes))))
            (edit/id existing)))))
  "Create or update line by shot+ord.")

(define edit/line-choose
  (lambda (line-id take-id)
    (update-field 'edit-line (as-str line-id) "take" (as-str take-id)))
  "Set chosen audio take for a line.")

(define edit/line-audio-for-shot
  (lambda (shot-id)
    (list/foldl
      (lambda (acc row)
        (if (edit/blank? acc)
            (edit/f row "take")
            acc))
      ""
      (edit/lines shot-id)))
  "First non-empty chosen line take for EDL audio pointer.")

;; --- cues (beds + hits) ---

(define edit/cues
  (lambda (project cut)
    (let ((p (as-str project))
          (c (as-str cut)))
      (edit/sort-by-ord
        (list/filter
          (lambda (row)
            (and (string-eq (edit/f row "project") p)
                 (string-eq (edit/f row "cut") c)))
          (rows 'edit-cue 500)))))
  "Timeline cues for project+cut.")

(define edit/cue-add
  (lambda (project cut ord mode asset tc-start dur gain-db loop)
    (create 'edit-cue
      (dict-set* "{}"
        (list
          "project" (as-str project)
          "cut" (as-str cut)
          "ord" (as-str ord)
          "mode" (as-str mode)
          "asset" (as-str asset)
          "tc_start" (as-str tc-start)
          "dur" (as-str dur)
          "gain_db" (as-str gain-db)
          "loop" (as-str loop)))))
  "Add bed or hit cue.")

(define edit/cue-canonical
  (lambda (project cut)
    (let ((cue-part
            (edit/join-lines
              (list/map
                (lambda (c)
                  (string-append
                    (edit/f c "ord") "|"
                    (edit/f c "mode") "|"
                    (edit/f c "asset") "|"
                    (edit/f c "tc_start") "|"
                    (edit/f c "dur") "|"
                    (edit/f c "gain_db") "|"
                    (edit/f c "loop")))
                (edit/cues project cut))))
          (line-part
            (edit/join-lines
              (list/map
                (lambda (ln)
                  (string-append
                    (edit/f ln "shot") "|"
                    (edit/f ln "ord") "|"
                    (edit/f ln "take")))
                (edit/lines-project project)))))
      (string-append cue-part "\n--\n" line-part)))
  "Canonical audio graph print for hashing.")

(define edit/cue-hash
  (lambda (project cut profile)
    (sha1
      (string-append
        (edit/cue-canonical project cut)
        "\nprofile:"
        (as-str profile))))
  "Cache key for mix-job.")

;; --- exports (v1: strings; save to disk by hand or via UI) ---

(define edit/export-mlt
  (lambda (project cut)
    (let ((proj (edit/project-by-slug project))
          (entries (edit/edl project cut)))
      (let ((fps
              (if (null? proj) "24" (edit/f proj "fps")))
            (title
              (if (null? proj) project (edit/f proj "title"))))
        (string-append
          "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
          "<!-- pricklypear edit/export-mlt project="
          (edit/escape-xml project)
          " cut="
          (edit/escape-xml cut)
          " edl_hash="
          (edit/edl-hash project cut)
          " -->\n"
          "<mlt LC_NUMERIC=\"C\" version=\"7.0.0\" "
          "title=\""
          (edit/escape-xml title)
          "\" producer=\"main_bin\">\n"
          "  <profile frame_rate_num=\""
          (edit/escape-xml fps)
          "\" frame_rate_den=\"1\" width=\"1920\" height=\"1080\" "
          "display_aspect_num=\"16\" display_aspect_den=\"9\" "
          "sample_aspect_num=\"1\" sample_aspect_den=\"1\" "
          "colorspace=\"709\" progressive=\"1\"/>\n"
          "  <playlist id=\"main_bin\">\n"
          (edit/join-lines
            (list/map
              (lambda (e) (edit/mlt-entry-xml e))
              entries))
          "\n  </playlist>\n"
          "  <tractor id=\"tractor0\" title=\""
          (edit/escape-xml title)
          "\">\n"
          "    <track producer=\"main_bin\"/>\n"
          "  </tractor>\n"
          "</mlt>\n"))))
  "MLT XML string for Kdenlive/melt.")

(define edit/mlt-entry-xml
  (lambda (e)
    (let ((t (edit/take-get (edit/f e "take"))))
      (let ((path
              (if (null? t)
                  (string-append "missing-take-" (edit/f e "take"))
                  (let ((p (edit/f t "media_path")))
                    (if (edit/blank? p)
                        (string-append "hash:" (edit/f t "media_hash"))
                        p))))
            (in (edit/f e "tc_in"))
            (dur (edit/f e "dur")))
        (string-append
          "    <entry producer=\""
          (edit/escape-xml path)
          "\" in=\""
          (edit/escape-xml in)
          "\" out=\""
          (edit/escape-xml
            (as-str (+ (edit/as-int in) (edit/as-int dur) -1)))
          "\"/>"))))
  "One MLT playlist entry.")

(define edit/export-blender
  (lambda (project cut)
    (let ((proj (edit/project-by-slug project))
          (entries (edit/edl project cut)))
      (let ((fps
              (if (null? proj) "24" (edit/f proj "fps"))))
        (string-append
          "# pricklypear edit/export-blender\n"
          "# project=" project " cut=" cut
          " edl_hash=" (edit/edl-hash project cut) "\n"
          "# Load in Blender Text Editor and Run Script, or:\n"
          "#   blender -b -P this_script.py\n"
          "import bpy\n"
          "scene = bpy.context.scene\n"
          "scene.render.fps = " (as-str (edit/as-int fps)) "\n"
          "scene.sequence_editor_clear()\n"
          "se = scene.sequence_editor_create()\n"
          "frame = 1\n"
          (edit/join-lines
            (list/map
              (lambda (e) (edit/blender-strip-py e))
              entries))
          "\nprint('pricklypear EDL loaded')\n"))))
  "Blender VSE build script string.")

(define edit/blender-strip-py
  (lambda (e)
    (let ((t (edit/take-get (edit/f e "take"))))
      (let ((path
              (if (null? t)
                  "/missing.mp4"
                  (let ((p (edit/f t "media_path")))
                    (if (edit/blank? p) "/missing.mp4" p))))
            (dur (edit/as-int (edit/f e "dur")))
            (tc (edit/as-int (edit/f e "tc_in"))))
        (string-append
          "se.sequences.new_movie("
          "name='t" (edit/f e "ord") "', "
          "filepath='" path "', "
          "channel=1, frame_start=frame)\n"
          "strip = se.sequences_all['t" (edit/f e "ord") "']\n"
          "strip.frame_offset_start = " (as-str tc) "\n"
          "strip.frame_final_duration = " (as-str dur) "\n"
          "frame += " (as-str dur)))))
  "Python lines for one VSE strip.")

(define edit/export-cue-sheet
  (lambda (project cut)
    (string-append
      "# pricklypear cue sheet project=" project " cut=" cut "\n"
      "# cue_hash=" (edit/cue-hash project cut "dailies") "\n"
      "ord,mode,asset,tc_start,dur,gain_db,loop,path\n"
      (edit/join-lines
        (list/map
          (lambda (c)
            (let ((a (edit/asset-get (edit/f c "asset"))))
              (string-append
                (edit/f c "ord") ","
                (edit/f c "mode") ","
                (edit/f c "asset") ","
                (edit/f c "tc_start") ","
                (edit/f c "dur") ","
                (edit/f c "gain_db") ","
                (edit/f c "loop") ","
                (if (null? a) "" (edit/f a "primary_path")))))
          (edit/cues project cut)))
      "\n# lines\n"
      "shot,ord,character,text,take,path\n"
      (edit/join-lines
        (list/map
          (lambda (ln)
            (let ((t (if (edit/blank? (edit/f ln "take"))
                         ()
                         (edit/take-get (edit/f ln "take")))))
              (string-append
                (edit/f ln "shot") ","
                (edit/f ln "ord") ","
                (edit/f ln "character") ","
                (edit/escape-xml (edit/f ln "text")) ","
                (edit/f ln "take") ","
                (if (null? t) "" (edit/f t "media_path")))))
          (edit/lines-project project)))
      "\n"))
  "CSV-ish cue sheet for humans and agents.")

(define edit/export-ffmpeg
  (lambda (project cut)
    (let ((entries (edit/edl project cut))
          (cues (edit/cues project cut)))
      (string-append
        "#!/bin/sh\n"
        "# pricklypear edit/export-ffmpeg — dailies-style bounce sketch\n"
        "# project=" project " cut=" cut "\n"
        "# edl_hash=" (edit/edl-hash project cut) "\n"
        "# cue_hash=" (edit/cue-hash project cut "dailies") "\n"
        "# Review paths, then run under ksh/sh. Adjust -filter_complex as needed.\n"
        "set -e\n"
        "OUT=\"dailies-" project "-" cut ".mp4\"\n"
        (edit/ffmpeg-inputs entries)
        (edit/ffmpeg-filter entries)
        "ffmpeg -y \\\n"
        (edit/ffmpeg-input-flags entries)
        "  -filter_complex \"$FC\" \\\n"
        "  -map \"[vout]\" -map \"[aout]\" \\\n"
        "  -c:v libx264 -preset veryfast -crf 23 \\\n"
        "  -c:a aac -b:a 160k \"$OUT\"\n"
        "echo wrote \"$OUT\"\n"
        "# cues (apply in a second pass or DAW):\n"
        (edit/join-lines
          (list/map
            (lambda (c)
              (string-append
                "# cue " (edit/f c "mode")
                " asset=" (edit/f c "asset")
                " tc=" (edit/f c "tc_start")
                " gain=" (edit/f c "gain_db") "dB"))
            cues))
        "\n")))
  "Shell script sketch that concatenates chosen picture takes.")

(define edit/ffmpeg-inputs
  (lambda (entries)
    (edit/join-lines
      (list/map
        (lambda (e)
          (let ((t (edit/take-get (edit/f e "take"))))
            (let ((path
                    (if (null? t)
                        "/missing.mp4"
                        (let ((p (edit/f t "media_path")))
                          (if (edit/blank? p) "/missing.mp4" p)))))
              (string-append
                "# input ord=" (edit/f e "ord") " path=" path))))
        entries)))
  "Comments listing inputs.")

(define edit/ffmpeg-input-flags
  (lambda (entries)
    (edit/join-lines
      (list/map
        (lambda (e)
          (let ((t (edit/take-get (edit/f e "take"))))
            (let ((path
                    (if (null? t)
                        "/missing.mp4"
                        (let ((p (edit/f t "media_path")))
                          (if (edit/blank? p) "/missing.mp4" p)))))
              (string-append "  -i \"" path "\" \\"))))
        entries)))
  "ffmpeg -i flags.")

(define edit/ffmpeg-filter
  (lambda (entries)
    (if (null? entries)
        "FC=\"nullsrc=s=1920x1080:d=1[vout];anullsrc=r=48000:cl=stereo[aout]\"\n"
        (let ((n (edit/count entries)))
          (string-append
            "FC=\""
            (edit/ffmpeg-scale-chain entries 0)
            (edit/ffmpeg-concat n)
            "\"\n"))))
  "Build filter_complex assignment.")

(define edit/count
  (lambda (xs)
    (if (null? xs) 0 (+ 1 (edit/count (cdr xs)))))
  "List length.")

(define edit/ffmpeg-scale-chain
  (lambda (entries i)
    (if (null? entries)
        ""
        (string-append
          "[" (as-str i) ":v]scale=1920:1080:force_original_aspect_ratio=decrease,"
          "pad=1920:1080:(ow-iw)/2:(oh-ih)/2,setsar=1,fps=24[v" (as-str i) "];"
          "[" (as-str i) ":a]aformat=sample_fmts=fltp:sample_rates=48000:channel_layouts=stereo,"
          "volume=1.0[a" (as-str i) "];"
          (edit/ffmpeg-scale-chain (cdr entries) (+ i 1)))))
  "Per-input scale/aformat labels.")

(define edit/ffmpeg-concat
  (lambda (n)
    (string-append
      (edit/ffmpeg-concat-labels 0 n)
      "concat=n=" (as-str n) ":v=1:a=1[vout][aout]"))
  "concat filter tail.")

(define edit/ffmpeg-concat-labels
  (lambda (i n)
    (if (>= i n)
        ""
        (string-append
          "[v" (as-str i) "][a" (as-str i) "]"
          (edit/ffmpeg-concat-labels (+ i 1) n))))
  "v/a label stream for concat.")

;; --- spend ---

(define edit/spend-cents
  (lambda (project)
    (list/foldl
      (lambda (acc shot)
        (+ acc
           (list/foldl
             (lambda (a t)
               (+ a (edit/as-int (edit/f t "cost_cents"))))
             0
             (edit/takes (edit/id shot)))))
      0
      (edit/shots project)))
  "Sum take cost_cents for project.")

;; --- UI ---

(define edit/home-html
  (lambda ()
    (html/div
      (html/attr "class" "stack")
      (html/join
        (list
          (html/h1 "Studio")
          (html/p*
            (html/attr "class" "muted")
            "Film edit: library, shots, takes, lines, cues, EDL export. Film-as-data.")
          (html/div
            (html/attr "class" "card")
            (html/join
              (list
                (html/h2 "Projects")
                (edit/projects-table)
                (edit/project-create-form))))
          (html/div
            (html/attr "class" "card")
            (html/join
              (list
                (html/h2 "Studio assets")
                (edit/assets-table ""))))))))
  "Home list HTML.")

(define edit/projects-table
  (lambda ()
    (let ((ps (edit/projects)))
      (if (null? ps)
          (html/p* (html/attr "class" "muted") "No projects yet.")
          (edit/table
            (html/join
              (list
                (edit/thead
                  (edit/tr
                    (html/join
                      (list
                        (edit/th "slug")
                        (edit/th "title")
                        (edit/th "fps")
                        (edit/th "budget¢")
                        (edit/th "")))))
                (edit/tbody
                  (html/join
                    (list/map edit/project-row ps)))))))))
  "Projects table.")

(define edit/project-row
  (lambda (row)
    (edit/tr
      (html/join
        (list
          (edit/td (html/code (edit/f row "slug")))
          (edit/td (html/text (edit/f row "title")))
          (edit/td (html/text (edit/f row "fps")))
          (edit/td (html/text (edit/f row "budget_cents")))
          (edit/td
            (html/a
              (string-append "/studio/project?slug=" (edit/f row "slug"))
              "open"))))))
  "One project table row.")

(define edit/project-create-form
  (lambda ()
    (html/form
      "POST"
      "/studio"
      (html/attrs
        (list
          (html/attr "class" "stack")
          (html/attr "hx-post" "/studio")
          (html/attr "hx-target" "body")
          (html/attr "hx-swap" "outerHTML")))
      (html/join
        (list
          (html/hidden "action" "create-project")
          (html/label "" "slug")
          (edit/input "slug" "")
          (html/label "" "title")
          (edit/input "title" "")
          (html/label "" "fps")
          (edit/input "fps" "24")
          (html/label "" "resolution")
          (edit/input "resolution" "1920x1080")
          (html/label "" "budget_cents")
          (edit/input "budget_cents" "0")
          (html/button (html/attr "type" "submit") "Create project")))))
  "Create project form.")

(define edit/asset-thumb
  (lambda (row)
    (let ((mid (edit/f row "media_id"))
          (path (edit/f row "primary_path"))
          (slug (edit/f row "slug")))
      (let ((src
              (if (not (edit/blank? mid))
                  (string-append "/media/" mid)
                  (if (and (not (edit/blank? path))
                           (string-eq (substring path 0 1) "/"))
                      path
                      ""))))
        (if (edit/blank? src)
            (html/span (html/attr "class" "muted") "—")
            (html/a src
              (html/void
                "img"
                (html/attrs
                  (list
                    (html/attr "src" src)
                    (html/attr "alt" slug)
                    (html/attr "class" "edit-asset-thumb")
                    (html/attr "loading" "lazy")
                    (html/attr
                      "style"
                      "width:72px;height:48px;object-fit:cover;border:1px solid #000;vertical-align:middle;")))))))))
  "Thumb linked to full media when media_id or /media path set.")

(define edit/assets-table
  (lambda (project)
    (let ((xs (edit/assets-for project "")))
      (if (null? xs)
          (html/p* (html/attr "class" "muted") "No assets.")
          (edit/table
            (html/join
              (list
                (edit/thead
                  (edit/tr
                    (html/join
                      (list
                        (edit/th "")
                        (edit/th "slug")
                        (edit/th "kind")
                        (edit/th "project")
                        (edit/th "appearance")))))
                (edit/tbody
                  (html/join
                    (list/map edit/asset-row xs)))))))))
  "Assets table with optional media thumbs.")

(define edit/asset-row
  (lambda (row)
    (edit/tr
      (html/join
        (list
          (edit/td (edit/asset-thumb row))
          (edit/td (html/code (edit/f row "slug")))
          (edit/td (html/text (edit/f row "kind")))
          (edit/td
            (html/text
              (if (edit/blank? (edit/f row "project"))
                  "(studio)"
                  (edit/f row "project"))))
          (edit/td
            (html/text
              (let ((a (edit/f row "appearance")))
                (if (< (string-length a) 80)
                    a
                    (string-append (substring a 0 80) "…")))))))))
  "One asset row with thumb.")

(define edit/project-page-html
  (lambda (slug)
    (let ((proj (edit/project-by-slug slug)))
      (if (null? proj)
          (html/div ""
            (html/join
              (list
                (html/h1 "Missing project")
                (html/p* "" (html/a "/studio" "Back")))))
          (let ((p (edit/f proj "slug")))
            (html/div
              (html/attr "class" "stack")
              (html/join
                (list
                  (html/p* "" (html/a "/studio" "← Studio"))
                  (html/h1 (html/text (edit/f proj "title")))
                  (html/p*
                    (html/attr "class" "muted")
                    (string-append
                      "slug " p
                      " · fps " (edit/f proj "fps")
                      " · " (edit/f proj "resolution")
                      " · spend "
                      (as-str (edit/spend-cents p))
                      "¢ / budget "
                      (edit/f proj "budget_cents")
                      "¢ · edl "
                      (edit/edl-hash p "main")))
                  (html/div
                    (html/attr "class" "card")
                    (html/join
                      (list
                        (html/h2 "Cast & library")
                        (edit/assets-table p)
                        (edit/asset-create-form p))))
                  (html/div
                    (html/attr "class" "card")
                    (html/join
                      (list
                        (html/h2 "Shots")
                        (edit/shots-table p)
                        (edit/shot-create-form p))))
                  (html/div
                    (html/attr "class" "card")
                    (html/join
                      (list
                        (html/h2 "Cut tools")
                        (html/p*
                          (html/attr "class" "muted")
                          "Rebuild EDL from chosen takes, then export.")
                        (edit/cut-actions p)
                        (html/p*
                          ""
                          (html/a
                            (string-append
                              "/studio/export?slug=" p "&cut=main&kind=mlt")
                            "MLT XML"))
                        (html/p*
                          ""
                          (html/a
                            (string-append
                              "/studio/export?slug=" p "&cut=main&kind=blender")
                            "Blender VSE script"))
                        (html/p*
                          ""
                          (html/a
                            (string-append
                              "/studio/export?slug=" p "&cut=main&kind=cues")
                            "Cue sheet"))
                        (html/p*
                          ""
                          (html/a
                            (string-append
                              "/studio/export?slug=" p "&cut=main&kind=ffmpeg")
                            "ffmpeg dailies script"))))))))))))
  "Project detail HTML.")

(define edit/asset-create-form
  (lambda (project)
    (html/form
      "POST"
      (string-append "/studio/project?slug=" project)
      (html/attrs (list (html/attr "class" "stack")))
      (html/join
        (list
          (html/hidden "action" "create-asset")
          (html/hidden "project" project)
          (html/label "" "slug")
          (edit/input "slug" "")
          (html/label "" "kind (character|location|prop|style|voice|sfx|score|…)")
          (edit/input "kind" "character")
          (html/label "" "appearance (locked prompt fragment)")
          (edit/input "appearance" "")
          (html/label "" "summary")
          (edit/input "summary" "")
          (html/button (html/attr "type" "submit") "Add asset")))))
  "Quick asset form.")

(define edit/shots-table
  (lambda (project)
    (let ((xs (edit/shots project)))
      (if (null? xs)
          (html/p* (html/attr "class" "muted") "No shots.")
          (edit/table
            (html/join
              (list
                (edit/thead
                  (edit/tr
                    (html/join
                      (list
                        (edit/th "#")
                        (edit/th "slug")
                        (edit/th "summary")
                        (edit/th "cast")
                        (edit/th "take")
                        (edit/th "")))))
                (edit/tbody
                  (html/join
                    (list/map
                      (lambda (s) (edit/shot-row project s))
                      xs)))))))))
  "Shots table with cast + chosen take.")

(define edit/shot-row
  (lambda (project shot)
    (let ((sid (edit/id shot))
          (ch (edit/chosen-take (edit/id shot)))
          (casts (edit/shot-casts (edit/id shot))))
      (edit/tr
        (html/join
          (list
            (edit/td (html/text (edit/f shot "ord")))
            (edit/td (html/code (edit/f shot "slug")))
            (edit/td (html/text (edit/f shot "summary")))
            (edit/td
              (html/text
                (edit/join-lines
                  (list/map
                    (lambda (c)
                      (let ((a (edit/asset-get (edit/f c "asset"))))
                        (if (null? a)
                            (edit/f c "asset")
                            (edit/f a "slug"))))
                    casts))))
            (edit/td
              (html/text
                (if (null? ch)
                    "—"
                    (string-append
                      "#" (edit/f ch "n")
                      " "
                      (edit/f ch "state")))))
            (edit/td
              (html/a
                (string-append
                  "/studio/shot?id=" sid "&project=" project)
                "open")))))))
  "One shot row.")

(define edit/shot-create-form
  (lambda (project)
    (html/form
      "POST"
      (string-append "/studio/project?slug=" project)
      (html/attrs (list (html/attr "class" "stack")))
      (html/join
        (list
          (html/hidden "action" "create-shot")
          (html/hidden "project" project)
          (html/label "" "ord")
          (edit/input "ord" "1")
          (html/label "" "slug")
          (edit/input "slug" "")
          (html/label "" "summary")
          (edit/input "summary" "")
          (html/label "" "prompt (action only)")
          (edit/input "prompt" "")
          (html/label "" "dur_target (frames)")
          (edit/input "dur_target" "48")
          (html/button (html/attr "type" "submit") "Add shot")))))
  "Create shot form.")

(define edit/cut-actions
  (lambda (project)
    (html/form
      "POST"
      (string-append "/studio/project?slug=" project)
      ""
      (html/join
        (list
          (html/hidden "action" "rebuild-edl")
          (html/hidden "project" project)
          (html/hidden "cut" "main")
          (html/button
            (html/attr "type" "submit")
            "Rebuild EDL from chosen takes")))))
  "Rebuild EDL form.")

(define edit/shot-page-html
  (lambda (shot-id project)
    (let ((shot (edit/shot-get shot-id)))
      (if (null? shot)
          (html/div "" (html/h1 "Missing shot"))
          (html/div
            (html/attr "class" "stack")
            (html/join
              (list
                (html/p*
                  ""
                  (html/a
                    (string-append "/studio/project?slug=" project)
                    "← project"))
                (html/h1
                  (html/text
                    (string-append
                      (edit/f shot "ord") ". "
                      (edit/f shot "slug"))))
                (html/p (html/text (edit/f shot "summary")))
                (html/p*
                  (html/attr "class" "muted")
                  (edit/f shot "prompt"))
                (html/div
                  (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Expand preview")
                      (html/pre
                        (html/text (edit/expand shot-id))))))
                (html/div
                  (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Cast")
                      (edit/cast-list shot-id)
                      (edit/cast-form shot-id project))))
                (html/div
                  (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Refs")
                      (edit/ref-list shot-id)
                      (edit/ref-form shot-id project))))
                (html/div
                  (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Takes")
                      (edit/takes-table shot-id project)
                      (edit/take-form shot-id project))))
                (html/div
                  (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Lines")
                      (edit/lines-table shot-id)
                      (edit/line-form shot-id project))))))))))
  "Shot detail HTML.")

(define edit/cast-list
  (lambda (shot-id)
    (let ((xs (edit/shot-casts shot-id)))
      (if (null? xs)
          (html/p* (html/attr "class" "muted") "No cast.")
          (html/ul ""
            (html/join
              (list/map
                (lambda (c)
                  (let ((a (edit/asset-get (edit/f c "asset"))))
                    (html/li ""
                      (html/text
                        (string-append
                          (if (null? a) (edit/f c "asset") (edit/f a "slug"))
                          " ("
                          (edit/f c "role")
                          ")")))))
                xs))))))
  "Cast ul.")

(define edit/cast-form
  (lambda (shot-id project)
    (html/form
      "POST"
      (string-append "/studio/shot?id=" shot-id "&project=" project)
      (html/attrs (list (html/attr "class" "stack")))
      (html/join
        (list
          (html/hidden "action" "cast")
          (html/label "" "asset id")
          (edit/input "asset" "")
          (html/label "" "role")
          (edit/input "role" "hero")
          (html/label "" "wardrobe asset id (optional)")
          (edit/input "wardrobe" "")
          (html/button (html/attr "type" "submit") "Bind cast")))))
  "Cast bind form.")

(define edit/ref-list
  (lambda (shot-id)
    (let ((xs (edit/shot-refs shot-id)))
      (if (null? xs)
          (html/p* (html/attr "class" "muted") "No refs.")
          (html/ul ""
            (html/join
              (list/map
                (lambda (r)
                  (html/li ""
                    (html/text
                      (string-append
                        (edit/f r "purpose")
                        " → "
                        (edit/f r "asset")))))
                xs))))))
  "Refs ul.")

(define edit/ref-form
  (lambda (shot-id project)
    (html/form
      "POST"
      (string-append "/studio/shot?id=" shot-id "&project=" project)
      (html/attrs (list (html/attr "class" "stack")))
      (html/join
        (list
          (html/hidden "action" "ref")
          (html/label "" "asset id")
          (edit/input "asset" "")
          (html/label "" "purpose")
          (edit/input "purpose" "style")
          (html/label "" "weight")
          (edit/input "weight" "")
          (html/button (html/attr "type" "submit") "Bind ref")))))
  "Ref bind form.")

(define edit/takes-table
  (lambda (shot-id project)
    (let ((xs (edit/takes shot-id)))
      (if (null? xs)
          (html/p* (html/attr "class" "muted") "No takes.")
          (edit/table
            (html/join
              (list
                (edit/thead
                  (edit/tr
                    (html/join
                      (list
                        (edit/th "n")
                        (edit/th "backend")
                        (edit/th "state")
                        (edit/th "cost¢")
                        (edit/th "hash")
                        (edit/th "")))))
                (edit/tbody
                  (html/join
                    (list/map
                      (lambda (t)
                        (edit/take-row shot-id project t))
                      xs)))))))))
  "Takes table.")

(define edit/take-row
  (lambda (shot-id project t)
    (edit/tr
      (html/join
        (list
          (edit/td (html/text (edit/f t "n")))
          (edit/td (html/text (edit/f t "backend")))
          (edit/td (html/text (edit/f t "state")))
          (edit/td (html/text (edit/f t "cost_cents")))
          (edit/td
            (html/code
              (let ((h (edit/f t "media_hash")))
                (if (< (string-length h) 12)
                    h
                    (substring h 0 12)))))
          (edit/td
            (html/form
              "POST"
              (string-append
                "/studio/shot?id=" shot-id "&project=" project)
              ""
              (html/join
                (list
                  (html/hidden "action" "choose")
                  (html/hidden "n" (edit/f t "n"))
                  (html/button
                    (html/attr "type" "submit")
                    "choose")))))))))
  "Take row with choose.")

(define edit/take-form
  (lambda (shot-id project)
    (html/form
      "POST"
      (string-append "/studio/shot?id=" shot-id "&project=" project)
      (html/attrs (list (html/attr "class" "stack")))
      (html/join
        (list
          (html/hidden "action" "add-take")
          (html/label "" "n")
          (edit/input "n" "1")
          (html/label "" "backend")
          (edit/input "backend" "camera")
          (html/label "" "media_path")
          (edit/input "media_path" "")
          (html/label "" "media_hash")
          (edit/input "media_hash" "")
          (html/label "" "cost_cents")
          (edit/input "cost_cents" "0")
          (html/button (html/attr "type" "submit") "Add take")))))
  "Manual take form (camera or imported).")

(define edit/lines-table
  (lambda (shot-id)
    (let ((xs (edit/lines shot-id)))
      (if (null? xs)
          (html/p* (html/attr "class" "muted") "No lines.")
          (edit/table
            (html/join
              (list
                (edit/thead
                  (edit/tr
                    (html/join
                      (list
                        (edit/th "#")
                        (edit/th "character")
                        (edit/th "text")
                        (edit/th "take")))))
                (edit/tbody
                  (html/join
                    (list/map
                      (lambda (ln)
                        (edit/tr
                          (html/join
                            (list
                              (edit/td (html/text (edit/f ln "ord")))
                              (edit/td (html/code (edit/f ln "character")))
                              (edit/td (html/text (edit/f ln "text")))
                              (edit/td
                                (html/text
                                  (if (edit/blank? (edit/f ln "take"))
                                      "—"
                                      (edit/f ln "take"))))))))
                      xs)))))))))
  "Lines table.")

(define edit/line-form
  (lambda (shot-id project)
    (html/form
      "POST"
      (string-append "/studio/shot?id=" shot-id "&project=" project)
      (html/attrs (list (html/attr "class" "stack")))
      (html/join
        (list
          (html/hidden "action" "add-line")
          (html/hidden "project" project)
          (html/label "" "ord")
          (edit/input "ord" "1")
          (html/label "" "character asset id")
          (edit/input "character" "")
          (html/label "" "text")
          (edit/input "text" "")
          (html/label "" "notes")
          (edit/input "notes" "")
          (html/button (html/attr "type" "submit") "Add line")))))
  "Line form.")

(define edit/export-page-html
  (lambda (slug cut kind)
    (let ((body
            (if (string-eq kind "mlt")
                (edit/export-mlt slug cut)
                (if (string-eq kind "blender")
                    (edit/export-blender slug cut)
                    (if (string-eq kind "cues")
                        (edit/export-cue-sheet slug cut)
                        (if (string-eq kind "ffmpeg")
                            (edit/export-ffmpeg slug cut)
                            "unknown kind"))))))
      (html/div
        (html/attr "class" "stack")
        (html/join
          (list
            (html/p*
              ""
              (html/a
                (string-append "/studio/project?slug=" slug)
                "← project"))
            (html/h1
              (html/text
                (string-append "Export " kind " · " slug " / " cut)))
            (html/p*
              (html/attr "class" "muted")
              "v1 export is text — save this file yourself. Automated write lands with spawn prims.")
            (html/pre (html/text body)))))))
  "Export viewer.")

;; --- routes ---

(define edit/home-page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (let ((action (as-str (request-param "action"))))
            (if (string-eq action "create-project")
                (edit/project-upsert
                  (request-param "slug")
                  (request-param "title")
                  (request-param "fps")
                  (request-param "resolution")
                  (request-param "budget_cents")
                  "")
                ()))
          (redirect "/studio"))
        (html/page (list (edit/home-html)))))
  "GET/POST /studio")

(define edit/project-page
  (lambda ()
    (let ((slug (as-str (request-param "slug"))))
      (if (edit/blank? slug)
          (redirect "/studio")
          (if (string-eq (request-method) "POST")
              (seq
                (let ((action (as-str (request-param "action"))))
                  (cond
                    ((string-eq action "create-asset")
                     (edit/asset-upsert
                       (request-param "project")
                       (request-param "slug")
                       (request-param "kind")
                       (request-param "summary")
                       ""
                       (request-param "appearance")
                       ""
                       ""
                       ""
                       "[]"
                       ""
                       ""
                       ""
                       "{}"))
                    ((string-eq action "create-shot")
                     (edit/shot-create
                       (request-param "project")
                       (request-param "ord")
                       (request-param "slug")
                       (request-param "summary")
                       (request-param "prompt")
                       (request-param "dur_target")))
                    ((string-eq action "rebuild-edl")
                     (edit/edl-rebuild-from-shots
                       (request-param "project")
                       (request-param "cut")))
                    (else ())))
                (redirect (string-append "/studio/project?slug=" slug)))
              (html/page (list (edit/project-page-html slug)))))))
  "GET/POST /studio/project")

(define edit/shot-page
  (lambda ()
    (let ((id (as-str (request-param "id")))
          (project (as-str (request-param "project"))))
      (if (edit/blank? id)
          (redirect "/studio")
          (if (string-eq (request-method) "POST")
              (seq
                (let ((action (as-str (request-param "action"))))
                  (cond
                    ((string-eq action "cast")
                     (edit/shot-cast
                       id
                       (request-param "asset")
                       (request-param "role")
                       (request-param "wardrobe")))
                    ((string-eq action "ref")
                     (edit/shot-ref
                       id
                       (request-param "asset")
                       (request-param "purpose")
                       (request-param "weight")))
                    ((string-eq action "add-take")
                     (edit/take-create
                       id
                       (request-param "n")
                       (request-param "backend")
                       ""
                       ""
                       (request-param "cost_cents")
                       (request-param "media_hash")
                       (request-param "media_path")
                       "done"
                       ""
                       ""
                       "[]"))
                    ((string-eq action "choose")
                     (edit/choose id (request-param "n")))
                    ((string-eq action "add-line")
                     (edit/line-upsert
                       (request-param "project")
                       id
                       (request-param "ord")
                       (request-param "character")
                       (request-param "text")
                       (request-param "notes")))
                    (else ())))
                (redirect
                  (string-append
                    "/studio/shot?id=" id "&project=" project)))
              (html/page
                (list (edit/shot-page-html id project)))))))
  "GET/POST /studio/shot")

(define edit/export-page
  (lambda ()
    (let ((slug (as-str (request-param "slug")))
          (cut (as-str (request-param "cut")))
          (kind (as-str (request-param "kind"))))
      (let ((c (if (edit/blank? cut) "main" cut))
            (k (if (edit/blank? kind) "mlt" kind)))
        (if (edit/blank? slug)
            (redirect "/studio")
            (html/page
              (list (edit/export-page-html slug c k)))))))
  "GET /studio/export")

;; NOTE: /edit is the kernel function editor. Package UI is /studio
;; (borge issue filed; film-edit.ui smokes still say /edit until plan).
(route "/studio" "edit/home-page")
(route "/studio/project" "edit/project-page")
(route "/studio/shot" "edit/shot-page")
(route "/studio/export" "edit/export-page")
