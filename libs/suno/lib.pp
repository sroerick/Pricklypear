;; package suno -- AI song generation via the sunoapi.org gateway
;;
;; Gateway docs: https://docs.sunoapi.org/suno-api/generate-music
;; Bearer auth; POST /api/v1/generate submits a task (2 songs per task);
;; GET /api/v1/generate/record-info?taskId=… polls status
;; (PENDING -> TEXT_SUCCESS -> FIRST_SUCCESS -> SUCCESS, or *_FAILED);
;; GET /api/v1/generate/credit shows remaining credits.
;;
;; Kernel deps (all shipped): http-post / http-get (admin/agent gated,
;; borge/image.borg outbound-http), json-parse, dict-get, sleep, rows,
;; find-rows, create, update, deftype, request-param/request-method,
;; redirect, hx-request?, render-html, now. std: json-escape, dict-set*,
;; html/*, html/page, list/*, map, filter, foldl.
;;
;; CONFIG: one data row of type suno-config (fields dict, via rows):
;;   api_key       bearer key from your gateway's API-key page
;;   base_url      gateway base (default "" = https://api.sunoapi.org;
;;                 e.g. "https://api.kie.ai" — same endpoint shape)
;;   callback_url  optional public callback URL (""). When blank the
;;                 lib relies on record-info polling only. The OpenAPI
;;                 marks callBackUrl required; if the gateway rejects a
;;                 generate without it, set this field to a public URL.
;;   model         default model (e.g. "V4_5ALL")
;; Create it (admin; type is declared below at load):
;;   (create 'suno-config "{\"api_key\":\"<key>\",\"base_url\":\"\",\"callback_url\":\"\",\"model\":\"V4_5ALL\"}")
;;
;; DATA: suno-task rows record submissions:
;;   task_id title style model instrumental prompt status tracks created
;;   tracks is the raw sunoData JSON array text from record-info
;;   (camelCase keys audioUrl / streamAudioUrl / imageUrl / title /
;;   tags / duration / id; suno/pick tolerates snake_case too).
;;
;; API surface (core):
;;   (suno/generate prompt)                              -> taskId
;;   (suno/generate/model prompt model instrumental?)    -> taskId
;;   (suno/generate/custom title style prompt instrumental? model) -> taskId
;;   (suno/record task-id)      -> full record-info data dict
;;   (suno/status task-id)      -> status string
;;   (suno/tracks task-id)      -> list of track dicts (JSON strings)
;;   (suno/wait task-id) / (suno/wait/with task-id attempts interval-s)
;;                              -> final data dict (REPL/jobs;
;;                                 the UI uses per-task refresh instead)
;;   (suno/credits)             -> remaining credits string
;; Rows/UI:
;;   (suno/submit title style prompt instrumental? model) -> row-id
;;   (suno/task-refresh row-id) -> updated status string
;;   route: /suno (private) — form + task cards with <audio> players.
;;   POST action=generate takes mode=simple (gateway writes lyrics from
;;   the prompt) | lyrics (prompt used VERBATIM as the lyrics; title +
;;   style required) | instrumental (style required, title defaults to
;;   "Untitled"); legacy posts without mode keep the old rule.
;;   (suno/task-refresh row-id) -> updated status string
;;   route: /suno (private) — form + task cards with <audio> players.
;;
;; Callbacks: sunoapi.org pushes POST {code,msg,data:{callbackType,task_id,data}}
;; to callback_url when set; this lib does not ingest callbacks — polling
;; record-info (suno/task-refresh) is the source of truth here. Files on
;; the gateway expire (14-15 days); download promptly if you want to keep
;; them ("download mp3" links point at audioUrl).

;; ---- types ----

(deftype 'suno-config 'api_key 'base_url 'callback_url 'model 'ace_url)
(deftype 'suno-task 'task_id 'title 'style 'model 'instrumental 'prompt 'status 'tracks 'created)

;; ---- small helpers ----

(define suno/blank?
  (lambda (s) "True when s is null or an empty string."
    (if (null? s) #t (string-eq (as-str s) ""))))

(define suno/nonblank
  (lambda (s fallback) "s when nonblank, else fallback."
    (if (suno/blank? s) (as-str fallback) (as-str s))))

(define suno/pick
  (lambda (obj k1 k2)
    "First non-null JSON field of obj between two key spellings
(camelCase record-info vs snake_case callback payloads)."
    (let ((v (dict-get obj k1)))
      (if (null? v)
          (let ((v2 (dict-get obj k2)))
            (if (null? v2) "" (as-str v2)))
          (as-str v)))))

(define suno/base
  (lambda ()
    "Gateway base url from config base_url; empty = sunoapi.org.
Compatible gateways (same /api/v1/generate + record-info shape):
https://api.kie.ai."
    (suno/nonblank (dict-get (suno/config) "base_url")
                   "https://api.sunoapi.org")))

;; ---- config ----

(define suno/config
  (lambda
    ()
    "Load the suno-config row (first row of the type — there is exactly one); User_error if absent."
    (let
      ((rs (rows 'suno-config 5)))
      (if
        (null? rs)
        (error "suno: no suno-config row — create it (admin): (create 'suno-config \"{\\\"api_key\\\":\\\"<key>\\\",\\\"callback_url\\\":\\\"\\\",\\\"model\\\":\\\"V4_5ALL\\\"}\")")
        (cdr (car rs))))))

(define suno/key
  (lambda () "Bearer api key from config; errors when blank."
    (let ((k (suno/nonblank (dict-get (suno/config) "api_key") "")))
      (if (string-eq k "")
          (error "suno: api_key is blank — set it on the suno-config row (/data/suno-config)")
          k))))

(define suno/default-model
  (lambda () "Configured default model (fallback V4_5ALL)."
    (suno/nonblank (dict-get (suno/config) "model") "V4_5ALL")))

(define suno/callback-url
  (lambda () "Configured callback_url, or \"\" when unset."
    (suno/nonblank (dict-get (suno/config) "callback_url") "")))

(define suno/headers
  (lambda () "Wire headers for gateway calls: bearer auth + JSON body."
    (string-append
      "{\"Authorization\": \"Bearer " (json-escape (suno/key)) "\""
      ", \"Content-Type\": \"application/json\"}")))

;; ---- transport ----

(define suno/check-call
  (lambda (tag r)
    "Shared code=200 discipline for gateway responses: returns the
parsed data dict, raises a descriptive error otherwise."
    (if (string-eq (as-str (dict-get r "ok")) "true")
        (let ((j (json-parse (dict-get r "body"))))
          (if (string-eq (as-str (dict-get j "code")) "200")
              (dict-get j "data")
              (error (string-append "suno " tag ": HTTP "
                      (as-str (dict-get r "status")) " — "
                      (as-str (dict-get j "code")) " "
                      (as-str (dict-get j "msg"))))))
        (error (string-append "suno " tag ": "
                (as-str (dict-get r "err")) " HTTP "
                (as-str (dict-get r "status")))))))

(define suno/post-call
  (lambda (path body)
    "POST one gateway call; returns the parsed data dict when code=200."
    (suno/retry 3
      (lambda ()
        (suno/check-call path
          (http-post (string-append (suno/base) path) (suno/headers) body))))))

(define suno/get-call
  (lambda (path+qs)
    "GET one gateway call; same code=200 discipline as suno/post-call."
    (suno/retry 3
      (lambda ()
        (suno/check-call path+qs
          (http-get (string-append (suno/base) path+qs) (suno/headers)))))))

;; ---- generate payloads ----
;;
;; Non-custom mode: prompt only (the gateway writes lyrics from the
;; description); custom mode: title+style, prompt holds lyrics when vocal.

(define suno/generate-body
  (lambda (prompt model instrumental? callback)
    "JSON body for a non-custom generate."
    (string-append
      "{\"prompt\":\"" (json-escape prompt)
      "\",\"customMode\":false"
      ",\"instrumental\":" (if instrumental? "true" "false")
      ",\"model\":\"" (json-escape model) "\""
      (if (string-eq callback "")
          ""
          (string-append ",\"callBackUrl\":\"" (json-escape callback) "\""))
      "}")))

(define suno/generate-custom-body
  (lambda (title style prompt model instrumental? callback)
    "JSON body for a custom-mode generate (title+style required)."
    (string-append
      "{\"customMode\":true"
      ",\"instrumental\":" (if instrumental? "true" "false")
      (if (suno/blank? prompt)
          ""
          (string-append ",\"prompt\":\"" (json-escape prompt) "\""))
      ",\"style\":\"" (json-escape style)
      "\",\"title\":\"" (json-escape title)
      "\",\"model\":\"" (json-escape model) "\""
      (if (string-eq callback "")
          ""
          (string-append ",\"callBackUrl\":\"" (json-escape callback) "\""))
      "}")))

;; ---- core api ----

(define suno/generate
  (lambda (prompt)
    "Submit a simple (customMode=false) generation; returns the taskId.
Model/default come from suno-config, instrumental #f. Prompt limit:
3000 chars. For control use suno/generate/model."
    (suno/generate/model prompt (suno/default-model) #f)))

(define suno/generate/model
  (lambda (prompt model instrumental?)
    "Submit a simple (customMode=false) generation with an explicit
model and instrumental flag; returns the taskId. Prompt limit: 3000
chars."
    (let ((d (suno/post-call "/api/v1/generate"
               (suno/generate-body prompt model instrumental? (suno/callback-url)))))
      (as-str (dict-get d "taskId")))))

(define suno/generate/custom
  (lambda (title style prompt instrumental? model)
    "Submit a custom-mode generation (title+style required; prompt is
the lyrics when instrumental? is #f). Returns the taskId. Limits:
prompt 5000 chars, style 1000, title 100 (V4_5+ models)."
    (if (string-eq (suno/nonblank style "") "")
        (error "suno/generate/custom: style is required in custom mode")
        (if (string-eq (suno/nonblank title "") "")
            (error "suno/generate/custom: title is required in custom mode")
            (if (and (not instrumental?) (string-eq (suno/nonblank prompt "") ""))
                (error "suno/generate/custom: prompt (lyrics) is required when instrumental is false")
                (let ((d (suno/post-call "/api/v1/generate"
                           (suno/generate-custom-body title style prompt model instrumental? (suno/callback-url)))))
                  (as-str (dict-get d "taskId"))))))))

(define suno/record
  (lambda (task-id)
    "Full record-info data dict for a task: taskId, status, param,
response.sunoData (track list) once generation advances."
    (suno/get-call (string-append "/api/v1/generate/record-info?taskId="
                     (json-escape task-id)))))

(define suno/record-status
  (lambda (data)
    "Status string of a record-info data dict (top-level, else
data.response.status, else PENDING)."
    (let ((s (suno/pick data "status" "status")))
      (if (string-eq s "")
          (suno/nonblank (dict-get (dict-get data "response") "status")
                         "PENDING")
          s))))

(define suno/status
  (lambda (task-id)
    "One-word task status: PENDING | TEXT_SUCCESS | FIRST_SUCCESS |
SUCCESS | CREATE_TASK_FAILED | GENERATE_AUDIO_FAILED | CALLBACK_EXCEPTION
| SENSITIVE_WORD_ERROR."
    (suno/record-status (suno/record task-id))))

(define suno/failed?
  (lambda (st)
    "True when st is a terminal failure status."
    (or (string-eq st "CREATE_TASK_FAILED")
        (string-eq st "GENERATE_AUDIO_FAILED")
        (string-eq st "CALLBACK_EXCEPTION")
        (string-eq st "SENSITIVE_WORD_ERROR")
        (string-eq st "FAILED"))))

(define suno/wait-loop
  (lambda (task-id n interval)
    "Bounded poll loop behind suno/wait."
    (let
      ((st
         (if
           (string-eq (suno/engine) "ace")
           (let ((elem (suno/ace/query task-id)))
             (if (null? elem) "PENDING" (suno/ace/status-of elem)))
           (suno/record-status (suno/record task-id)))))
      (cond
          ((string-eq st "SUCCESS") st)
          ((suno/failed? st)
           (error (string-append "suno/wait: task " task-id " failed: " st)))
          ((<= n 1)
           (error (string-append "suno/wait: out of attempts on " task-id
                                 " (status " st ")")))
          (else
            (sleep interval)
            (suno/wait-loop task-id (- n 1) interval))))))

(define suno/wait
  (lambda (task-id)
    "Poll record-info until SUCCESS or failure; returns the final data
dict with defaults 24 attempts x 10s (~4 min; downloadable URLs are
typically ready in 2-3 min). REPL/job helper: the /suno face uses
manual refresh instead of blocking."
    (suno/wait/with task-id 24 10)))

(define suno/wait/with
  (lambda (task-id attempts interval-s)
    "suno/wait with explicit attempts and interval seconds."
    (suno/wait-loop task-id attempts interval-s)))

(define suno/credits
  (lambda () "Remaining gateway credits (GET /api/v1/generate/credit)."
    (let ((d (suno/get-call "/api/v1/generate/credit")))
      (suno/nonblank (dict-get d "credits") "?"))))

;; ---- self-hosted engine (ACE-Step on your own GPU, e.g. RunPod) ----
;;
;; Deploy once: RunPod template uuc79b5j3c (ValyrianTech/ace-step-1.5)
;; bakes the API server + models; REST surfaces at
;; https://<POD-ID>-8000.proxy.runpod.net. Set ace_url on the
;; suno-config row (and optionally ACESTEP_API_KEY + ai_token later)
;; and the whole lib switches engines: suno-config.ace_url nonblank
;; => ACE-Step, blank => Suno gateway (kie/sunoapi.org dialect).
;;
;; ACE-Step API: POST /release_task {prompt, lyrics, audio_duration, ...}
;;   -> {task_id}; POST /query_result {task_id_list:[..]} -> data[] with
;;   {status: 0|1|2, result: "<json string>"} whose entries carry
;;   file: "/v1/audio?path=..."; GET that path for the audio.

(define suno/ace-url
  (lambda ()
    "Self-hosted ACE-Step API base (config ace_url), or \"\" for the
Suno gateway engine."
    (suno/nonblank (dict-get (suno/config) "ace_url") "")))

(define suno/engine
  (lambda ()
    "Active engine: ace | gateway."
    (if (string-eq (suno/ace-url) "") "gateway" "ace")))

(define suno/ace/check
  (lambda (tag r)
    "ACE-Step envelope discipline: {code:200,...}; returns data."
    (if (string-eq (as-str (dict-get r "ok")) "true")
        (let ((j (json-parse (dict-get r "body"))))
          (if (string-eq (as-str (dict-get j "code")) "200")
              (dict-get j "data")
              (error (string-append "suno/ace " tag ": HTTP "
                      (as-str (dict-get r "status")) " — "
                      (as-str (dict-get j "error"))))))
        (error (string-append "suno/ace " tag ": "
                (as-str (dict-get r "err")) " HTTP "
                (as-str (dict-get r "status")))))))

(define suno/retry
  (lambda (n f)
    "Call f (0-arg); on Lisp error retry up to n total attempts with a
3s gap; re-raises the last error. Prod POST bodies drop intermittently
(transport returns 200 + headers, empty body) in minutes-long windows —
8 x 3s absorbs most of them."
    (guard
      (lambda (e)
        (if (<= n 1)
            (error (car (cdr e)) (car (cdr (cdr e))))
            (begin (sleep 3) (suno/retry (- n 1) f))))
      (f))))

(define suno/ace/call
  (lambda (path body)
    "POST one ACE-Step call; returns the data member."
    (suno/retry 3
      (lambda ()
        (suno/ace/check path
          (http-post
            (string-append (suno/ace-url) path)
            "{\"Content-Type\": \"application/json\"}"
            body))))))

(define suno/ace/submit
  (lambda (caption lyrics)
    "Submit one ACE-Step text2music task (batch of 2); returns taskId.
caption describes the music; lyrics is the vocal text (\"\" =
no vocals). Duration is LM-autocompleted server-side."
    (let
      ((body
         (string-append
           "{\"prompt\":\"" (json-escape caption)
           (if (string-eq lyrics "")
               ""
               (string-append "\",\"lyrics\":\"" (json-escape lyrics)))
           "\",\"batch_size\":2}")))
      (let ((d (suno/ace/call "/release_task" body)))
        (as-str (dict-get d "task_id"))))))

(define suno/ace/query
  (lambda (task-id)
    "One query_result element for a task: {status:0|1|2,
result:\"<json string>\"}; result entries carry file paths."
    (let
      ((d (suno/ace/call
            "/query_result"
            (string-append "{\"task_id_list\":[\"" (json-escape task-id) "\"]}"))))
      (if (null? d)
          '()
          (car d)))))

(define suno/ace/absolutize
  (lambda (path)
    "ace file path -> proxied absolute url."
    (if (string-contains? path "http")
        path
        (string-append (suno/ace-url) path))))

(define suno/ace/status-of
  (lambda (elem)
    "ace int status (0|1|2) -> our status string."
    (let ((n (suno/nonblank (dict-get elem "status") "0")))
      (cond
        ((string-eq n "1") "SUCCESS")
        ((string-eq n "2") "FAILED")
        (else "PENDING")))))

(define suno/ace/tracks-json
  (lambda (elem)
    "Normalized track-array JSON from a query_result element:
absolute urls + our track-card keys."
    (let ((rj (dict-get elem "result")))
      (if (null? rj)
          "[]"
          (as-str
            (map
              (lambda (t)
                (let
                  ((url (suno/ace/absolutize (suno/pick t "file" "file"))))
                  (dict-set*
                    "{}"
                    (list
                      "audioUrl" url
                      "streamAudioUrl" url
                      "tags" (suno/pick t "prompt" "prompt")
                      "duration" (suno/pick
                                   (dict-get t "metas")
                                   "duration" "duration")
                      "id" (suno/pick t "task_id" "task_id")))))
              (if (null? (json-parse (as-str rj)))
                  '()
                  (json-parse (as-str rj)))))))))

;; ---- task rows ----

(define suno/task-fields
  (lambda (task-id title style model instrumental? prompt status tracks)
    "Fields JSON for a suno-task row."
    (dict-set*
      "{}"
      (list
        "task_id" (as-str task-id)
        "title" (as-str title)
        "style" (as-str style)
        "model" (as-str model)
        "instrumental" (if instrumental? "true" "false")
        "prompt" (as-str prompt)
        "status" (as-str status)
        "tracks" (as-str tracks)
        "created" (as-str (now))))))

(define suno/submit
  (lambda (title style prompt instrumental? model)
    "Submit a generation and record a suno-task row.
Gateway engine: custom mode when title or style is nonblank, simple
mode otherwise; the transport POST happens in-habitat (flaky: see
log/issues http-post). ACE engine (ace_url set): NO transport at all —
the row is created with status PENDING-LOCAL and the laptop loop
(scripts/pp-suno-loop.py) owns the pod round-trip. Returns row id."
    (let
      ((task-id
         (if
           (string-eq (suno/engine) "ace")
           (suno/nonblank title "local")
           (if
             (and
               (string-eq (suno/nonblank style "") "")
               (string-eq (suno/nonblank title "") ""))
             (suno/generate/model prompt model instrumental?)
             (suno/generate/custom
               (suno/nonblank title "Untitled")
               (suno/nonblank style "")
               prompt instrumental? model)))))
      (let
        ((status
           (if
             (string-eq (suno/engine) "ace")
             "PENDING-LOCAL"
             "PENDING")))
          (create 'suno-task
                  (suno/task-fields task-id title style model instrumental?
                                    prompt status "[]"))))))

(define suno/task-rows
  (lambda (n)
    "Most recent suno-task rows as (id . fields) pairs."
    (rows 'suno-task n)))

(define suno/tracks-json-of
  (lambda (fields)
    "Parsed sunoData track list from a task row's tracks column
(JSON array text); empty list when blank or empty array."
    (let ((j (suno/nonblank (dict-get fields "tracks") "")))
      (if
        (or (string-eq j "") (string-eq j "[]"))
        '()
        (json-parse j)))))

;; ---- ui (/suno) ----
;;
;; Generate form + task cards. On the ace engine the form does no
;; transport at all: rows land as PENDING-LOCAL and the laptop loop
;; (scripts/pp-suno-loop.py) fills them from the pod.
;;
;; Generate form + task cards. Post backs hit /suno; refresh is manual

(define suno/client-js
  (lambda ()
    "Browser-side engine (ace mode): submit + poll the pod straight from
the page and import finished tracks into the habitat. Backslash-free
JS (single quotes + fromCharCode(39)) so the Nopales string stays clean."
    (html/tag
      "script"
      ""
      (html/join
        (list
          "var SUNO_BASE = 'https://jzvxeopclkrhj5-8000.proxy.runpod.net';
function sunoVal(id) { return document.getElementById(id).value; }
async function sunoPost(url, obj) {
  var r = await fetch(url, {method: 'POST', headers: {'Content-Type': 'application/json'}, body: JSON.stringify(obj)});
  return r.json();
}
async function sunoSubmit(ev) {
  ev.preventDefault();
  var st = document.getElementById('suno-status');
  var mode = sunoVal('suno-mode'), title = sunoVal('suno-title'),
      style = sunoVal('suno-style'), prompt = sunoVal('suno-prompt');
  var payload;
  if (mode === 'lyrics') payload = {prompt: style || title || 'song', lyrics: prompt, batch_size: 2};
  else if (mode === 'instrumental') payload = {prompt: style || title || 'instrumental music', batch_size: 2};
  else payload = {prompt: prompt, batch_size: 2};
  try {
    st.textContent = 'submitting...';
    var j = await sunoPost(SUNO_BASE + '/release_task', payload);
    if (!j || j.code !== 200) { st.textContent = 'submit failed: ' + String((j && (j.error || j.msg)) || '?'); return false; }
    var taskId = j.data.task_id;
    st.textContent = 'queued ' + taskId + ' - polling (keep this tab open, ~1-3 min)';
    var tracks = null;
    while (true) {
      await new Promise(function(rs) { setTimeout(rs, 10000); });
      var q = await sunoPost(SUNO_BASE + '/query_result', {task_id_list: [taskId]});
      if (!q || q.code !== 200 || !q.data || !q.data.length) continue;
      var el = q.data[0];
      if (el.status === 2) { st.textContent = 'generation failed (sensitive words?)'; return false; }
      if (el.status !== 1) { st.textContent = 'still generating... ' + new Date().toLocaleTimeString(); continue; }
      tracks = JSON.parse(el.result || '[]'); break;
    }
    st.textContent = 'generated ' + tracks.length + ' track(s) - importing into the habitat...';
    for (var i = 0; i < tracks.length; i++) {
      var t = tracks[i];
      var audioUrl = t.file;
      if (audioUrl.indexOf('http') !== 0) audioUrl = SUNO_BASE + t.file;
      var bytes = await (await fetch(audioUrl)).blob();
      var m = await fetch('/api/media', {method: 'POST', headers: {'Content-Type': 'audio/mpeg', 'X-Filename': taskId.slice(0, 8) + '-' + (i + 1) + '.mp3'}, body: bytes});
      var mj = await m.json();
      if (!mj.ok) { st.textContent = 'media upload failed: ' + (mj.error || '?'); return false; }
      var label = title || payload.prompt;
      if (tracks.length > 1) label = label + ' (' + (i + 1) + ')';
      var fields = {title: label, artist: 'ace-step', album: '', media_id: mj.id, notes: payload.prompt};
      var q39 = String.fromCharCode(39);
      var expr = '(create ' + q39 + 'music-track ' + JSON.stringify(JSON.stringify(fields)) + ')';
      var e = await sunoPost('/api/eval', {expr: expr});
      if (!e.ok) { st.textContent = 'track row failed: ' + e.error; return false; }
      st.textContent = 'imported: ' + label;
    }
    st.textContent = 'all done - reloading...';
    setTimeout(function() { location.reload(); }, 1200);
  } catch (err) { st.textContent = 'error: ' + err; }
  return false;
}")))))

(define suno/style-html
  (lambda ()
    "Small style block for the generation page."
    (html/tag
      "style"
      ""
      (html/join
        (list
          ".suno-card{border:1px solid var(--border,#ddd);border-radius:.5rem;padding:.75rem;margin:.75rem 0}"
          ".suno-audio{width:100%;max-width:34rem;display:block;margin:.5rem 0}"
          ".suno-form input,.suno-form select,.suno-form textarea{margin:.15rem .35rem .15rem 0}"
          ".suno-muted{color:var(--muted,#777);font-size:.9em}"
          ".suno-status{font-weight:600}")))))

(define suno/action-form
  (lambda (pairs label)
    "One inline hx-post form from (action . extra-pairs) pairs."
    (html/form
      "POST"
      "/suno"
      (html/attrs
        (list
          (html/attr "style" "display:inline")
          (html/attr "hx-post" "/suno")
          (html/attr "hx-target" "#suno-main")
          (html/attr "hx-swap" "outerHTML")))
      (html/join
        (append
          (list (html/hidden "action" (as-str (car pairs))))
          (map
            (lambda (kv)
              (html/hidden (as-str (car kv)) (as-str (car (cdr kv)))))
            (cdr pairs))
          (list
            (html/button (html/attr "type" "submit") label)))))))

(define suno/track-card
  (lambda (t)
    "One generated track: tags/duration line, player, download link."
    (let
      ((stream (suno/pick t "streamAudioUrl" "stream_audio_url"))
        (audio (suno/pick t "audioUrl" "audio_url"))
        (tags (suno/pick t "tags" "tags"))
        (dur (suno/pick t "duration" "duration")))
      (html/div ""
        (html/join
          (list
            (html/span
              (html/attr "class" "suno-muted")
              (string-append
                (if (string-eq tags "") "" (string-append tags " — "))
                (if (string-eq dur "") "" (string-append dur "s"))))
            (if (string-eq stream "")
                ""
                (html/tag "audio"
                  (html/attr "class" "suno-audio")
                  (html/tag "source"
                    (html/attrs
                      (list
                        (html/attr "src" stream)
                        (html/attr "type" "audio/mpeg")))
                    "")))
            (if (string-eq audio "")
                ""
                (html/a audio "download mp3"))))))))

(define suno/task-html
  (lambda (row)
    "One task card: title, status, refresh, players per track."
    (let
      ((id (as-str (car row)))
        (f (cdr row)))
      (let
        ((task-id (suno/nonblank (dict-get f "task_id") ""))
          (title (suno/nonblank (dict-get f "title") "Untitled"))
          (st (suno/nonblank (dict-get f "status") "?")))
        (html/div
          (html/attr "class" "suno-card")
          (html/join
            (list
              (html/strong title)
              (html/text " ")
              (html/span
                (html/attr "class" "suno-status")
                st)
              (html/span
                (html/attr "class" "suno-muted")
                (string-append " — " task-id))
              "<br>"
              (suno/action-form
                (list "refresh" (list "id" id))
                "refresh")
              (html/join
                (map
                  suno/track-card
                  (suno/tracks-json-of f))))))))))

(define suno/text-input
  (lambda (name placeholder value)
    "One text input with placeholder and prefilled value."
    (html/text-input
      name
      (html/attrs
        (list
          (html/attr "placeholder" placeholder)
          (html/attr "value" value))))))

(define suno/model-select
  (lambda ()
    "Model dropdown (config default preselected)."
    (html/select
      "model"
      ""
      (html/join
        (map
          (lambda (m) (html/option m m (string-eq m (suno/default-model))))
          (list "V4_5ALL" "V4_5" "V4_5PLUS" "V5" "V5_5"))))))

(define suno/mode-select
  (lambda ()
    "Mode dropdown: where the words come from."
    (html/select
      "mode"
      (html/attr "id" "suno-mode")
      (html/join
        (list
          (html/option "simple" "Simple — gateway writes the lyrics" #t)
          (html/option "lyrics" "Lyrics — use my prompt verbatim" #f)
          (html/option "instrumental" "Instrumental — no lyrics" #f))))))

(define suno/generate-form
  (lambda (default-title default-style)
    "The generate form; mode select picks simple (free-text prompt),
lyrics (verbatim lyrics: title + style required) or instrumental
(style required, title defaults to Untitled)."
    (html/form
      "POST"
      "/suno"
      (html/attrs
        (list
          (html/attr "id" "suno-form")
          (html/attr "class" "suno-form")
          (html/attr "onsubmit" "return sunoSubmit(event)")))
      (html/join
        (list
          (html/hidden "action" "generate")
          (suno/mode-select)
          (suno/text-input "title" "Title (required for lyrics mode)" default-title)
          (suno/text-input "style" "Style, e.g. folk ballad, close harmonies" default-style)
          "<br>"
          (html/tag "textarea"
            (html/attrs
              (list
                (html/attr "id" "suno-prompt")
                (html/attr "name" "prompt")
                (html/attr "rows" "5")))
            "")
          (html/span
            (html/attr "id" "suno-status")
            "")
          "<br>"
          (if (string-eq (suno/engine) "ace")
              ""
              (html/join
                (list
                  (html/text "model ")
                  (suno/model-select)
                  "<br>")))
          (html/button (html/attr "type" "submit") "generate"))))))

(define suno/main-html
  (lambda (msg)
    "Main /suno column body: form, credits note, task cards."
    (html/div
      (html/attr "id" "suno-main")
      (html/join
        (list
          (if (string-eq msg "")
              ""
              (html/p (html/text msg)))
          (html/h2 "Generate")
          (if (string-eq (suno/engine) "ace")
              (suno/client-js)
              "")
          (suno/generate-form "" "")
          (html/p*
            (html/attr "class" "suno-muted")
            (html/text
              "Simple: describe the song, gateway writes + sings the lyrics. Lyrics: your prompt is the lyrics, word for word (title + style required). Instrumental: describe the music. Generation takes ~2-3 minutes — press refresh on a card. Gateway files expire after ~2 weeks."))
          (html/h2 "Tasks")
          (html/join
            (map
              suno/task-html
              (suno/task-rows 20))))))))

(define suno/dispatch
  (lambda ()
    "Dispatch a POST /suno action; returns a short status string.
Actions: generate (mode=simple | lyrics | instrumental; posts without
a mode keep the legacy rule), refresh (poll record-info + update)."
    (let ((action (as-str (request-param "action"))))
      (cond
        ((and (string-eq action "generate") (string-eq (suno/engine) "ace"))
         "client engine: the submit runs in your browser (keep the tab open); the track card appears when it finishes importing")
        ((string-eq action "generate")
         (let
           ((prompt (as-str (request-param "prompt")))
             (title (as-str (request-param "title")))
             (style (as-str (request-param "style")))
             (mode (as-str (request-param "mode")))
             (model (suno/nonblank (request-param "model")
                                  (suno/default-model))))
           (cond
             ((string-eq mode "lyrics")
              (cond
                ((string-eq prompt "")
                 "lyrics mode: paste your lyrics in the prompt box (used verbatim)")
                ((string-eq title "") "lyrics mode: title required")
                ((string-eq style "") "lyrics mode: style required")
                (else
                  (suno/submit-msg
                    (suno/submit title style prompt #f model)
                    "lyrics"))))
             ((string-eq mode "instrumental")
              (cond
                ((string-eq style "") "instrumental mode: style required")
                (else
                  (suno/submit-msg
                    (suno/submit title style prompt #t model)
                    "instrumental"))))
             ((and (string-eq title "") (string-eq style ""))
              (if (string-eq prompt "")
                  "prompt required"
                  (suno/submit-msg (suno/submit "" "" prompt #f model) "")))
             (else
               (cond
                 ((string-eq prompt "")
                  "custom mode: prompt (lyrics) required when not instrumental")
                 ((string-eq title "") "custom mode: title required")
                 ((string-eq style "") "custom mode: style required")
                 (else
                   (suno/submit-msg (suno/submit title style prompt #f model) "custom")))))))
        ((string-eq action "refresh")
         (let ((row-id (as-str (request-param "id"))))
           (if (string-eq row-id "")
               "refresh: no row id"
               (suno/task-refresh row-id))))
        (else (string-append "unknown action: " action))))))

(define suno/submit-msg
  (lambda (row-id label)
    "Standard submit status string; label may be blank."
    (string-append
      "submitted"
      (if (string-eq label "") "" (string-append " " label))
      " task (row " (as-str row-id)
      ") — refresh in ~2-3 minutes")))

(define suno/tracks-json
  (lambda (data)
    "sunoData array text from a record-info data dict (empty-array
marker while pending)."
    (let ((resp (dict-get data "response")))
      (if (null? resp)
          "[]"
          (let ((sd (dict-get resp "sunoData")))
            (if (null? sd) "[]" (as-str sd)))))))

(define suno/tracks
  (lambda (task-id)
    "sunoData track dicts (JSON strings) for a task; empty list when
still pending."
    (let ((j (suno/tracks-json (suno/record task-id))))
      (if (string-eq j "[]") '() (json-parse j)))))

(define suno/ace/refresh
  (lambda (row-id f task-id)
    "Poll a task row against the self-hosted ACE engine; update
status/tracks; returns a short status string."
    (let ((elem (suno/ace/query task-id)))
      (let
        ((st (if (null? elem) "PENDING" (suno/ace/status-of elem))))
        (update
          'suno-task
          row-id
          (dict-set*
            (as-str f)
            (list "status" "tracks")
            (list
              st
              (if (null? elem) "[]" (suno/ace/tracks-json elem)))))
        (if (suno/failed? st)
            (string-append "task failed: " st)
            (string-append "status: " st))))))

(define suno/task-refresh
  (lambda (row-id)
    "Poll record-info for a task row and update status/tracks; returns
a short status string."
    (let ((f (get 'suno-task row-id)))
      (let ((task-id (suno/nonblank (dict-get f "task_id") "")))
        (if (string-eq task-id "")
            "refresh: row has no task_id"
            (if (string-eq (suno/engine) "ace")
                (suno/ace/refresh row-id f task-id)
                (let ((data (suno/record task-id)))
                  (let ((st (suno/record-status data)))
                    (let
                      ((fields
                         (dict-set*
                           (as-str f)
                           (list "status" "tracks")
                           (list st (suno/tracks-json data)))))
                      (update 'suno-task row-id fields)
                      (if (suno/failed? st)
                          (string-append "task failed: " st)
                          (string-append "status: " st)))))))))))

;; ---- route ----

(define suno/page
  (lambda ()
    "GET /suno renders form + tasks; POST handles generate/refresh."
    (if (string-eq (request-method) "POST")
        (let
          ((status (suno/dispatch)))
          (if (hx-request?)
              (render-html (suno/main-html status))
              (redirect
                (string-append "/suno?msg=" (url-encode (as-str status))))))
        (html/page
          (list
            (suno/style-html)
            (html/h1 "Suno")
            (html/p*
              (html/attr "class" "suno-muted")
              (html/text
                "AI song generation via sunoapi.org — 2 tracks per task. Config: the suno-config data row (api_key, callback_url, model)."))
            (suno/main-html (as-str (request-param "msg"))))))))

(route "/suno" "suno/page")
