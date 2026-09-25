;; package mem -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define mem/browse-type
  "gregor-memory"
  "Default row TYPE the /memories browser reads (?type= overrides).")

(define mem/browse-types
  (list "gregor-memory" "memory")
  "Row types offered as sibling links on /memories.")

(define mem/card
  (lambda
    (row type)
    (let*
      ((fields (cdr row))
        (mid (mem/field-or fields "mid" "?"))
        (title (mem/field-or fields "title" "(untitled)"))
        (content (mem/field-or fields "content" ""))
        (cat (mem/date (mem/field-or fields "cat" "0")))
        (uat (mem/date (mem/field-or fields "uat" "0")))
        (when (if (string-eq uat "") cat uat))
        (href
          (string-append "/memories/view?type=" type "&m=" mid))
        (snippet (mem/snippet content 240)))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h3 (html/a href (html/text title)))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append
                  when
                  (let
                    ((ws (mem/field-or fields "ws" "")))
                    (if (string-eq ws "") "" (string-append " · " ws)))
                  (let
                    ((v (mem/field-or fields "ver" "")))
                    (if (string-eq v "") "" (string-append " · ver " v))))))
            (html/p*
              (html/attr "style" "color:#888")
              (html/text snippet))
            (html/p*
              (html/attr "style" "margin-bottom:0")
              (html/join (mem/tag-chips (mem/tags fields)))))))))
  "One list card for a memory row (title link, date/ver line, content\nteaser, tag chips).")

(define mem/date
  (lambda
    (ts)
    (let
      ((n (parse-int (as-str ts))))
      (if
        (= n 0)
        ""
        (let*
          ((unix (if (> n 2500000000) (- n 2208988800) n))
            (z (+ (floor (/ unix 86400)) 719468))
            (era (/ z 146097))
            (doe (- z (* era 146097)))
            (yoe
              (/
                (-
                  (+ (- doe (/ doe 1460)) (/ doe 36524))
                  (/ doe 146096))
                365))
            (doy (- doe (+ (* 365 yoe) (- (/ yoe 4) (/ yoe 100)))))
            (mp (/ (+ (* 5 doy) 2) 153))
            (d01 (+ (- doy (/ (+ (* 153 mp) 2) 5)) 1))
            (mm (+ mp (if (< mp 10) 3 -9)))
            (yy (+ (* 400 era) yoe (if (< mm 3) 1 0)))
            (pad
              (lambda
                (x)
                (if (< x 10) (string-append "0" (as-str x)) (as-str x)))))
          (string-append (as-str yy) "-" (pad mm) "-" (pad d01))))))
  "Seconds -> YYYY-MM-DD (Hinnant civil-from-days). Accepts unix OR CL\nuniversal-time (>= 4e9: subtracts the 1900-01-01 offset); empty string\nfor 0/unparseable. parse-int of a bad string reads 0.")

(define mem/detail-page
  (lambda
    ()
    (let*
      ((type (mem/q-type))
        (mp (request-param "m"))
        (mid
          (if
            (or (null? mp) (string-eq (as-str mp) ""))
            ""
            (as-str mp)))
        (back (string-append "/memories?type=" type))
        (found
          (if
            (string-eq mid "")
            (list)
            (find-rows type "mid" mid))))
      (if
        (null? found)
        (html/page
          (list
            (html/h1 (html/text "Memory not found"))
            (html/p
              (string-append
                "No row with mid '"
                mid
                "' in '"
                type
                "'."))
            (html/a back "back to browse")))
        (let*
          ((fields (cdr (car found)))
            (title (mem/field-or fields "title" "(untitled)"))
            (content (mem/field-or fields "content" ""))
            (cat (mem/date (mem/field-or fields "cat" "0")))
            (uat (mem/date (mem/field-or fields "uat" "0")))
            (created (if (string-eq cat "") "?" cat))
            (updated (if (string-eq uat "") cat uat))
            (meta
              (string-append
                "created "
                created
                " · updated "
                updated
                (let
                  ((ws (mem/field-or fields "ws" "")))
                  (if (string-eq ws "") "" (string-append " · ws " ws)))
                (let
                  ((src (mem/field-or fields "src" "")))
                  (if
                    (string-eq src "")
                    ""
                    (string-append " · src " src)))
                (let
                  ((v (mem/field-or fields "ver" "")))
                  (if (string-eq v "") "" (string-append " · ver " v)))
                " · mid "
                mid)))
          (html/page
            (list
              (html/p*
                (html/attr "class" "muted")
                (html/a
                  back
                  (html/text (string-append "browse: " type))))
              (html/h1 (html/text title))
              (html/p* (html/attr "class" "muted") (html/text meta))
              (html/p*
                (html/attr "style" "margin-bottom:0")
                (html/join (mem/tag-chips (mem/tags fields))))
              (html/tag
                "pre"
                (html/attr
                  "style"
                  "white-space:pre-wrap;background:#fafafa;border:1px solid #eee;border-radius:6px;padding:12px;overflow-x:auto")
                (html/text content))))))))
  "GET /memories/view?m=<mid> — full detail: title, created/updated,\nws/src/ver, mid, tags, and the complete content in a pre-wrap block.")

(define mem/field-or
  (lambda
    (dict key default)
    (let
      ((v (dict-get dict key)))
      (if (or (null? v) (string-eq (as-str v) "")) default v)))
  "dict-get with a default for missing or empty values.")

(define mem/find-by
  (lambda (type field value) (find-rows type field value))
  "All rows of TYPE whose FIELD equals VALUE (exact match).")

(define mem/forget
  (lambda
    (type id)
    (let
      ((rs (find-rows type "mid" id)))
      (begin
        (map (lambda (row) (delete type (car row))) rs)
        (length rs))))
  "Delete every row whose mid = id. Returns the count deleted.")

(define mem/get
  (lambda
    (type id)
    (let
      ((rs (find-rows type "mid" id)))
      (if (null? rs) #f (cdr (car rs)))))
  "The fields dict of the row with mid = id, or #f when absent.")

(define mem/list
  (lambda (type n) (rows type n))
  "Up to N rows of TYPE (created-ascending), as (id fields) pairs.")

(define mem/page
  (lambda
    ()
    (let*
      ((type (mem/q-type))
        (qp (request-param "q"))
        (q
          (if
            (or (null? qp) (string-eq (as-str qp) ""))
            ""
            (as-str qp)))
        (rs
          (if
            (string-eq q "")
            (reverse (rows type 200))
            (mem/search-rows type q))))
      (html/page
        (list
          (html/h1 (html/text "Memories"))
          (html/p*
            (html/attr "class" "muted")
            (html/text
              (string-append
                (as-str (count rs))
                " of type '"
                type
                "'"
                (if
                  (string-eq q "")
                  ""
                  (string-append " matching '" q "'"))
                " · newest first")))
          (mem/type-links type)
          (mem/search-form q type)
          (html/join (map (lambda (row) (mem/card row type)) rs))))))
  "GET /memories — searchable newest-first browser over the memory\nrows (default store gregor-memory; ?type= switches stores).")

(define mem/q-type
  (lambda
    ()
    (let
      ((tp (request-param "type")))
      (if
        (or (null? tp) (string-eq (as-str tp) ""))
        mem/browse-type
        (as-str tp))))
  "The ?type= param of the active request (as a string), falling back\nto mem/browse-type.")

(define mem/remember
  (lambda
    (type id title content tags)
    (if
      (null? (find-rows type "mid" id))
      (seq
        (create
          type
          (mem/row-json
            id
            title
            content
            tags
            (as-str (now))
            "1"))
        id)
      (error
        "mem/remember: row with that mid already exists; use mem/replace")))
  "Create a new memory row. Errors if a row with that mid already\nexists (use mem/replace to overwrite). Returns id.")

(define mem/replace
  (lambda
    (type id title content tags)
    (let*
      ((rs (find-rows type "mid" id))
        (existing (if (null? rs) #f (car rs)))
        (cat
          (if
            existing
            (mem/field-or (cdr existing) "cat" (as-str (now)))
            (as-str (now))))
        (ver
          (if
            existing
            (mem/field-or (cdr existing) "ver" "1")
            "1")))
      (begin
        (if
          existing
          (update
            type
            (car existing)
            (mem/row-json
              id
              title
              content
              tags
              cat
              ver))
          (create
            type
            (mem/row-json
              id
              title
              content
              tags
              cat
              ver)))
        id)))
  "Create or overwrite the memory row with stable id MID. Preserves\nthe row's original created-at (cat) and ver on update. Returns id.")

(define mem/row-json
  (lambda
    (id title content tags cat ver)
    (string-append
      "{\"mid\":\""
      (json-escape id)
      "\",\"ver\":\""
      ver
      "\",\"cat\":\""
      cat
      "\",\"uat\":\""
      (as-str (now))
      "\",\"scope\":\"workspace\",\"ws\":\"\",\"title\":\""
      (json-escape title)
      "\",\"content\":\""
      (json-escape content)
      "\",\"tags\":"
      (if (string-eq (as-str tags) "") "[]" tags)
      ",\"src\":\"\"}"))
  "Fields JSON for one memory row (all-string fields).")

(define mem/search
  (lambda
    (type needle)
    (find-text type "content" needle 50))
  "Rows of TYPE whose content contains NEEDLE (case-insensitive\nsubstring), capped at 50.")

(define mem/search-form
  (lambda
    (q type)
    (html/form
      "GET"
      "/memories"
      (html/attrs (list))
      (html/join
        (list
          (html/hidden "type" type)
          (html/text-input
            "q"
            (html/attrs
              (list
                (html/attr "value" q)
                (html/attr "placeholder" "search content or title…")
                (html/attr "size" "40"))))
          (html/button
            (html/attr "type" "submit")
            (html/text "Search"))))))
  "GET search form for /memories (keeps the current type in a hidden\nfield).")

(define mem/search-rows
  (lambda
    (type q)
    (reverse
      (mem/union-by-id
        (list)
        (list)
        (append
          (find-text type "content" q 100)
          (find-text type "title" q 100)))))
  "Rows of TYPE whose content OR title contains the needle\n(case-insensitive substring); content hits come first, deduped.")

(define mem/snippet
  (lambda
    (s n)
    (if
      (<= (string-length s) n)
      s
      (string-append (substring s 0 n) "...")))
  "First n bytes of s with a trailing ... when truncated (list-card\nteaser; not utf8-boundary-aware, memory content is ASCII-mostly).")

(define mem/tag-chips
  (lambda
    (tags)
    (map
      (lambda
        (t)
        (html/span
          (html/attr
            "style"
            "display:inline-block;background:#eef0f2;border-radius:8px;padding:0 8px;margin:0 4px 4px 0;color:#555")
          (html/text (as-str t))))
      tags))
  "Tag chips for one memory row (inline style, no theme dependency).")

(define mem/tags
  (lambda
    (fields)
    (let
      ((raw (mem/field-or fields "tags" "")))
      (if
        (or
          (string-eq raw "")
          (not (string-starts-with? raw "["))
          (not (string-ends-with? raw "]")))
        (list)
        (map as-str (json-parse raw)))))
  "Tags of a memory row as a list of strings; [] when the field is not\na JSON array (graft rows are machine-written, so a [ ... ] shape is a\nreliable guard without exception handling).")

(define mem/type-links
  (lambda
    (cur)
    (html/p*
      (html/attr "class" "muted")
      (html/join
        (cons
          (html/text "types: ")
          (map
            (lambda
              (t)
              (if
                (string-eq t cur)
                (html/strong (html/text t))
                (html/a
                  (string-append "/memories?type=" t)
                  (html/text t))))
            mem/browse-types)))))
  "Sibling links for every browsable memory type; the current one is\nbold, unlinked text.")

(define mem/union-by-id
  (lambda
    (seen acc rs)
    (if
      (null? rs)
      acc
      (let
        ((id (as-str (car (car rs)))))
        (if
          (null? (assoc id seen))
          (mem/union-by-id
            (cons (cons id #t) seen)
            (cons (car rs) acc)
            (cdr rs))
          (mem/union-by-id seen acc (cdr rs))))))
  "Dedupe (id . fields) rows by string id: prepend never-seen rows of\nRS onto ACC (caller reverses if order matters).")

(define mem/upsert
  (lambda
    (type id fields-json)
    (let
      ((rs (find-rows type "mid" id)))
      (begin
        (if
          (null? rs)
          (create type fields-json)
          (update type (car (car rs)) fields-json))
        id)))
  "Create-or-replace a row by mid with caller-built fields JSON\n(graft-drain primitive: the caller owns the complete field set).\nReturns id.")

(route "/memories" "mem/page")
(route "/memories/view" "mem/detail-page")
