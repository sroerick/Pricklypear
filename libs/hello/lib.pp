;; package hello -- pads hello dual-render spike (plan:
;; docs/plans/pads-hello-dualrender.md). One card, two renders:
;; hello/page is the legacy string-pipeline ORACLE (libs/html);
;; hello/view is the SAME card as a pad tree (libs/pads); V1 is proven
;; when pads/render-html over hello/view is byte-identical with
;; hello/page. V3 lever: edit hello/greeting — both renders change with
;; zero shim/route edits.
;; Sync: scripts/pp_sync.py pull|push

(define hello/greeting
  "Hello, pads"
  "The greeting text — the single V3 edit point shared by BOTH renders.")

(define hello/subline
  "One authoring surface, two renders."
  "Subline text under the greeting (both renders).")

(define hello/box-a
  "Fixed box"
  "First text box: fixed-sized (flex:0 0 240px on web, fixed:240 in scene).")

(define hello/box-b
  "Growing box"
  "Second text box: growing (flex:1 on web, grow:1 in scene).")

;; ---------- V1 oracle: legacy string pipeline ----------

(define hello/page
  (lambda ()
    "GET /hello via the legacy libs/html string pipeline. This IS the
byte-identity oracle: hand-written html/ calls that pads/render-html
must match byte-for-byte over hello/view (V1). Byte-stable style strings
mirror pads/box-style / pads/size-flex canonical order."
    (render-html
      (html/tag
        "div"
        (html/attr "style" "display:flex;flex-direction:column;gap:12px;padding:16px")
        (html/join
          (list
            (html/h1 (html/text hello/greeting))
            (html/text hello/subline)
            (html/tag
              "div"
              (html/attr "style" "display:flex;flex-direction:row;gap:12px")
              (html/join
                (list
                  (html/tag
                    "div"
                    (html/attr "style" "flex:0 0 240px")
                    (html/text hello/box-a))
                  (html/tag
                    "div"
                    (html/attr "style" "flex:1")
                    (html/text hello/box-b)))))))))))

;; ---------- the one authoring surface: pad tree ----------

(define hello/view
  (lambda ()
    "The /hello card as a pad tree — the ONE authoring surface (V3).
Heading level 1 + para + one row of two text boxes (fixed/grow sized),
padding/gap set; the exact card hello/page hand-writes."
    (pad 'column
      (list (cons 'gap "12") (cons 'padding "16"))
      (pad 'heading (list (cons 'level 1)) hello/greeting)
      (pad 'text (list) hello/subline)
      (pad 'row (list (cons 'gap "12"))
        (pad 'text (list (cons 'size (list 'fixed 240))) hello/box-a)
        (pad 'text (list (cons 'size (list 'grow 1))) hello/box-b)))))

;; ---------- V2: native scene ----------

(define hello/scene
  (lambda ()
    "Scene JSON v0 for soil-hello (V2): (pads/render-scene (hello/view)).
Two consumption paths — --scene FILE and loopback eval — must yield the
same bytes for the same tree."
    (pads/render-scene (hello/view))))

(route "/hello" "hello/page")
