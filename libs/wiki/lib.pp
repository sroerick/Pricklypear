;; package wiki -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define wiki/apply-entities
  (lambda
    (ents source-slug touched)
    (if
      (null? ents)
      touched
      (wiki/apply-entities
        (cdr ents)
        source-slug
        (wiki/apply-entity (car ents) source-slug touched)))))
  "Apply each entity to the wiki, accumulating touched slugs."

(define wiki/apply-entity
  (lambda
    (ent source-slug touched)
    (let
      ((slug (as-str (dict-get ent "slug")))
        (title (as-str (dict-get ent "title")))
        (body (as-str (dict-get ent "body")))
        (kind (as-str (dict-get ent "kind"))))
      (let
        ((s (if (string-eq slug "") (wiki/slugify title) slug))
          (t (if (string-eq title "") s title))
          (b
            (if
              (string-eq body "")
              (string-append "From [[" source-slug "]].")
              (string-append
                body
                "\\n\\nSee also [["
                source-slug
                "]].")))
          (k
            (if
              (if
                (string-eq kind "entity")
                #t
                (string-eq kind "concept"))
              kind
              "entity")))
        (seq
          (wiki/upsert-page s t b k)
          (append touched (list s)))))))
  "Apply one entity to the wiki: upsert the page and add its slug to touched."

(define wiki/ask
  (lambda
    (prompt)
    (let
      ((sid
        (agent-session-new
          "You help maintain a personal knowledge wiki. Reply with plain text or JSON as asked. Do not call tools."
          #f)))
      (agent-say sid (as-str prompt)))))
  "Ask the wiki agent a prompt; returns its text reply."

(define wiki/corpus-index
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (cdr row)))
          (let
            ((slug (as-str (dict-get f "slug")))
              (title (as-str (dict-get f "title")))
              (kind (as-str (dict-get f "kind"))))
            (string-append
              acc
              "- ["
              kind
              "] "
              title
              " ([["
              slug
              "]])\\n"))))
      ""
      (notes/doc/index-list))))
  "Markdown bullet index of note titles/kinds with [[slug]] links."

(define wiki/default-schema
  (string-append
    "You maintain a personal knowledge wiki of outline notes.\\n"
    "Conventions:\\n"
    "- Pages are notes with kind in {source, entity, concept, query, synthesis, schema}.\\n"
    "- Cross-links use [[slug]] or [[slug|label]].\\n"
    "- Prefer updating existing entity/concept pages over creating duplicates.\\n"
    "- Keep titles short; put detail in outline bodies.\\n"
    "- When asked for JSON, reply with ONE JSON object only (no markdown fences).\\n"))

(define wiki/events
  (lambda () (rows (quote wiki-event) 100)))
  "Most recent wiki-event rows (up to 100)."

(define wiki/extract-json
  (lambda
    (s)
    (let
      ((t (as-str s)))
      (wiki/extract-json-scan t 0 () ()))))
  "Extract the first balanced {...} JSON object from a string; "{}" when none."

(define wiki/extract-json-scan
  (lambda
    (s i start end)
    (if
      (>= i (string-length s))
      (if
        (if (null? start) #t (null? end))
        "{}"
        (substring s start (+ (- end start) 1)))
      (let
        ((c (substring s i 1)))
        (if
          (string-eq c "{")
          (wiki/extract-json-scan
            s
            (+ i 1)
            (if (null? start) i start)
            end)
          (if
            (string-eq c "}")
            (wiki/extract-json-scan s (+ i 1) start i)
            (wiki/extract-json-scan s (+ i 1) start end)))))))
  "Scan helper for extract-json; tracks brace start/end indices."

(define wiki/ingest-from-source
  (lambda
    (source-text)
    (let
      ((src-slug
        (string-append "src-" (as-str (count (notes/doc/list))))))
      (seq
        (wiki/upsert-page
          src-slug
          (string-append "Source " src-slug)
          (as-str source-text)
          "source")
        (let
          ((raw
            (wiki/ask (wiki/ingest-prompt source-text src-slug))))
          (let
            ((plan (wiki/parse-plan raw)))
            (let
              ((summary (as-str (dict-get plan "summary")))
                (ents (wiki/plan-list plan "entities")))
              (seq
                (when
                  (not (string-eq summary ""))
                  (wiki/upsert-page
                    src-slug
                    (string-append "Source " src-slug)
                    (string-append
                      summary
                      "\\n\\n---\\n"
                      (as-str source-text))
                    "source"))
                (let
                  ((touched
                    (wiki/apply-entities ents src-slug (list src-slug))))
                  (seq
                    (wiki/log-event
                      "ingest"
                      "wiki/ingest-from-source"
                      src-slug
                      (string-append
                        "touched "
                        (as-str (count touched))
                        " pages"))
                    (list src-slug touched raw)))))))))))
  "Ingest a source text: create a source page, ask for a plan, apply entities; returns (source-slug touched raw)."

(define wiki/ingest-prompt
  (lambda
    (source-text source-slug)
    (string-append
      (wiki/schema)
      "\\n\\nIngest this source into the wiki.\\n"
      "Source slug: "
      source-slug
      "\\n"
      "Source text:\\n"
      (as-str source-text)
      "\\n\\n"
      "Reply with ONE JSON object only, shape:\\n"
      "{\"summary\":\"...\",\"entities\":[{\"slug\":\"...\",\"title\":\"...\","
      "\"body\":\"... with [[links]]\",\"kind\":\"entity|concept\"}]}\\n"
      "Create 1-5 entities/concepts. Bodies may include [[slug]] links.\\n")))
  "Build the ingest prompt asking the agent for a JSON plan."

(define wiki/lint
  (lambda
    ()
    (let
      ((report (wiki/ask (wiki/lint-prompt))))
      (seq
        (wiki/log-event "lint" "wiki/lint" "" "lint pass")
        report))))
  "Run a wiki lint pass via the agent; returns the report."

(define wiki/lint-prompt
  (lambda
    ()
    (string-append
      (wiki/schema)
      "\\n\\nCorpus index:\\n"
      (wiki/corpus-index)
      "\\n\\nLint this wiki. Report:\\n"
      "1. Orphan pages (no inbound links)\\n"
      "2. Missing pages (mentioned via [[slug]] but absent)\\n"
      "3. Contradictions or stale claims\\n"
      "4. Suggested new sources or questions\\n"
      "Be concise. Use bullet lists.\\n")))
  "Build the lint prompt over the corpus index."

(define wiki/log-event
  (lambda
    (kind op slug summary)
    (create
      (quote wiki-event)
      (dict-set*
        "{}"
        (list
          "kind"
          (as-str kind)
          "op"
          (as-str op)
          "slug"
          (as-str slug)
          "ts"
          (wiki/now)
          "summary"
          (as-str summary))))))
  "Create a wiki-event row recording kind, op, slug, timestamp and summary."

(define wiki/now
  (lambda () (as-str (date-today))))
  "Today's date as a string."

(define wiki/parse-plan
  (lambda (text) (json-parse (wiki/extract-json text))))
  "Parse the agent JSON plan text into a dict."

(define wiki/plan-list
  (lambda
    (plan key)
    (let
      ((v (dict-get plan key)))
      (if
        (null? v)
        (list)
        (let
          ((parsed (json-parse (as-str v))))
          (if
            (pair? parsed)
            parsed
            (if (null? parsed) (list) (list))))))))
  "List value for a plan key (parsed); () when absent or empty."

(define wiki/prune-events
  (lambda
    (days)
    (let
      ((cutoff
        (date-add-days
          (date-today)
          (- 0 (if (null? days) 30 (parse-int (as-str days)))))))
      (list/foldl
        (lambda
          (n row)
          (let
            ((f (cdr row)))
            (let
              ((ts (as-str (dict-get f "ts"))))
              (if
                (if
                  (string-eq ts "")
                  #f
                  (< (date-diff-days ts cutoff) 0))
                (seq (delete (quote wiki-event) (car row)) (+ n 1))
                n))))
        0
        (wiki/events)))))
  "Delete wiki-event rows older than days (default 30); returns count deleted."

(define wiki/query
  (lambda
    (question)
    (let
      ((answer (wiki/ask (wiki/query-prompt question))))
      (seq
        (wiki/log-event
          "query"
          "wiki/query"
          ""
          (substring
            (as-str question)
            0
            (if
              (< (string-length (as-str question)) 80)
              (string-length (as-str question))
              80)))
        answer))))
  "Ask the wiki a question; logs a query event and returns the answer."

(define wiki/query-prompt
  (lambda
    (question)
    (string-append
      (wiki/schema)
      "\\n\\nCorpus index:\\n"
      (wiki/corpus-index)
      "\\n\\nQuestion: "
      (as-str question)
      "\\n\\n"
      "Answer using the corpus. Cite pages as [[slug]]. "
      "If you create a synthesis, say so. Plain prose is fine.\\n")))
  "Build a question prompt over the corpus index."

(define wiki/schema
  (lambda
    ()
    (let
      ((n (notes/doc/get "wiki-schema")))
      (if
        (null? n)
        (seq
          (notes/doc/upsert-in
            "wiki-schema"
            "Wiki Schema"
            (outline/node*
              "root"
              "Wiki Schema"
              wiki/default-schema
              ""
              (list))
            "schema"
            "schema"
            ""
            "wiki")
          wiki/default-schema)
        (outline/body (notes/doc/outline n))))))
  "The wiki schema outline text; seeds the default schema note if missing."

(define wiki/slugify
  (lambda
    (title)
    (let
      ((t (string-trim (as-str title))))
      (if
        (string-eq t "")
        (string-append
          "wiki-"
          (as-str (count (notes/doc/list))))
        (string-append
          "w-"
          (substring
            t
            0
            (if (< (string-length t) 32) (string-length t) 32)))))))
  "Derive a wiki page slug from a title (truncated, w- prefix); unique fallback when empty."

(define wiki/upsert-page
  (lambda
    (slug title body kind)
    (let
      ((prev (notes/doc/get slug)))
      (let
        ((tree
            (if
              (null? prev)
              (outline/node* "root" title body "" (list))
              (outline/set-body
                (outline/set-title (notes/doc/outline prev) title)
                body)))
          (tags (if (null? prev) kind (notes/doc/tags prev)))
          (day (if (null? prev) "" (notes/doc/day prev)))
          (sc
            (if
              (null? prev)
              "1"
              (let
                ((raw
                  (dict-get (cdr (notes/doc/row slug)) "source_count")))
                (if
                  (null? raw)
                  "1"
                  (as-str (+ 1 (parse-int (as-str raw)))))))))
        (seq
          (notes/doc/upsert-in
            slug
            title
            tree
            tags
            kind
            day
            (if
              (null? prev)
              (if
                (string-eq kind "source")
                "wiki/sources"
                "wiki/entities")
              (notes/doc/folder prev)))
          (let
            ((row (notes/doc/row slug)))
            (if
              (null? row)
              slug
              (seq
                (update-field
                  (quote note)
                  (car row)
                  "updated_at"
                  (wiki/now))
                (update-field (quote note) (car row) "updated_by" "llm")
                (update-field (quote note) (car row) "source_count" sc)
                slug))))))))
  "Create or update a wiki page by slug; returns the slug."

