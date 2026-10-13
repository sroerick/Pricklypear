/* pp-cm.js — glochid /code file-arm boot: CodeMirror editor over the
   server-escaped <pre> (BORGE-1789581611). */
/* Contract (agent.borg file arm): the server keeps rendering
   `<pre data-pp-cm data-path="rel">escaped content</pre>` — escaping
   stays server-side; this script reads textContent, replaces the pre
   with a CodeMirror EDITOR (writable; Save posts /code/save), and
   re-runs on htmx swaps via MutationObserver so panes opened through
   /filmstrip/open boot too. Mode map mirrors image/lib/http_code.ml
   cm_mode_for (+ .pp/.scm -> text/x-scheme, BORGE-1789581611). */
(function () {
  "use strict";

  function modeFor(path) {
    var p = (path || "").toLowerCase();
    if (/\.ml$|\.mli$/.test(p)) return "text/x-ocaml";
    if (/\.md$/.test(p)) return "text/x-markdown";
    if (/\.pp$|\.scm$|\.ss$/.test(p)) return "text/x-scheme";
    return "text/plain";
  }

  function save(cm, path, status) {
    status.textContent = "saving…";
    fetch("/code/save", {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: new URLSearchParams({ path: path, content: cm.getValue() })
    })
      .then(function (r) {
        return r.text().then(function (t) { return { ok: r.ok, t: t }; });
      })
      .then(function (r) {
        if (r.ok) { status.textContent = "saved"; cm.__ppDirty = false; }
        else status.textContent = "save failed: " + r.t.slice(0, 200);
      })
      .catch(function (e) { status.textContent = "save failed: " + e; });
  }

  function bootOne(pre) {
    if (pre.getAttribute("data-pp-cm") === "done") return;
    var path = pre.getAttribute("data-path") || "";
    var host = document.createElement("div");
    host.className = "pp-cm-host";
    var bar = document.createElement("div");
    bar.className = "pp-cm-bar";
    var btn = document.createElement("button");
    btn.className = "pp-cm-save fs-btn";
    btn.textContent = "save";
    var status = document.createElement("span");
    status.className = "pp-cm-status muted";
    bar.appendChild(btn);
    bar.appendChild(status);
    var cm = CodeMirror(host, {
      value: pre.textContent,
      mode: modeFor(path),
      lineNumbers: true,
      lineWrapping: true,
      viewportMargin: 40
    });
    cm.__ppDirty = false;
    cm.on("change", function () {
      if (!cm.__ppDirty) { cm.__ppDirty = true; status.textContent = "edited — unsaved"; }
    });
    cm.on("keydown", function (_, e) {
      if ((e.ctrlKey || e.metaKey) && e.key === "s") {
        e.preventDefault();
        if (cm.__ppDirty) save(cm, path, status);
      }
    });
    btn.addEventListener("click", function () { if (cm.__ppDirty) save(cm, path, status); });
    pre.replaceWith(bar, host);
    // CodeMirror measures everything (line height, gutter, wrap points)
    // AT CONSTRUCTION. When the host boots inside a pane that is still
    // hidden / zero-size — filmstrip swap order, off-desktop panes,
    // cascade re-renders — every measurement is garbage and the editor
    // renders blank or with misaligned ghost lines until refreshed.
    // Refresh at boot (extra frames for settle), after webfonts land
    // (wrap points change), and whenever the host box changes size
    // (pane fade-in, filmstrip width edits). (BORGE-1789581611)
    var rere = function () { cm.refresh(); };
    requestAnimationFrame(rere);
    setTimeout(rere, 250);
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(rere);
    if (typeof ResizeObserver !== "undefined") new ResizeObserver(rere).observe(host);
  }

  function bootAll(root) {
    if (typeof CodeMirror === "undefined") return;
    var pres = root.querySelectorAll("pre[data-pp-cm]");
    for (var i = 0; i < pres.length; i++) bootOne(pres[i]);
  }

  bootAll(document);

  new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      var added = muts[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        var n = added[j];
        if (n.nodeType !== 1) continue;
        if (n.matches && n.matches("pre[data-pp-cm]")) bootOne(n);
        else if (n.querySelectorAll) bootAll(n);
      }
    }
  }).observe(document.body, { childList: true, subtree: true });
})();
