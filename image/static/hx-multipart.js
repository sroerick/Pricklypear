/**
 * hx-multipart — minimal htmx extension for multipart/mixed responses.
 *
 * Vendored for Pricklypear (no CDN). When an element has hx-ext="hx-multipart"
 * (or an ancestor does), responses with Content-Type: multipart/mixed are
 * parsed into parts; each part is swapped using its HX-Target / HX-Swap /
 * HX-Select headers (falling back to the request's hx-target / hx-swap and
 * envelope response headers). HX-Part-ID is recorded for reconnect cursors.
 *
 * Compatible with the server surface in borge/multipart.borg (mime-framing).
 */
(function () {
  "use strict";

  function headerMap(rawHeaders) {
    var map = {};
    if (!rawHeaders) return map;
    String(rawHeaders)
      .split(/\r?\n/)
      .forEach(function (line) {
        var i = line.indexOf(":");
        if (i > 0) {
          var k = line.slice(0, i).trim().toLowerCase();
          var v = line.slice(i + 1).trim();
          map[k] = v;
        }
      });
    return map;
  }

  function parseMultipart(body, contentType) {
    var m = /boundary=(?:"([^"]+)"|([^;\s]+))/i.exec(contentType || "");
    if (!m) return [];
    var boundary = m[1] || m[2];
    var parts = [];
    var chunks = String(body).split("--" + boundary);
    for (var i = 0; i < chunks.length; i++) {
      var chunk = chunks[i];
      if (!chunk || chunk === "--" || chunk === "--\r\n" || chunk === "--\n")
        continue;
      if (chunk.charAt(0) === "-") continue;
      if (chunk.indexOf("\r\n") === 0) chunk = chunk.slice(2);
      else if (chunk.indexOf("\n") === 0) chunk = chunk.slice(1);
      var split = chunk.indexOf("\r\n\r\n");
      var sepLen = 4;
      if (split < 0) {
        split = chunk.indexOf("\n\n");
        sepLen = 2;
      }
      if (split < 0) continue;
      var head = chunk.slice(0, split);
      var bodyPart = chunk.slice(split + sepLen);
      if (bodyPart.slice(-2) === "\r\n") bodyPart = bodyPart.slice(0, -2);
      else if (bodyPart.slice(-1) === "\n") bodyPart = bodyPart.slice(0, -1);
      parts.push({ headers: headerMap(head), body: bodyPart });
    }
    return parts;
  }

  function swapPart(htmx, elt, part, defaults) {
    var h = part.headers || {};
    var targetSel =
      h["hx-target"] || defaults.target || htmx.config.defaultTarget;
    var swap =
      h["hx-swap"] || defaults.swap || htmx.config.defaultSwapStyle || "innerHTML";
    var select = h["hx-select"] || defaults.select;
    var target = targetSel ? htmx.find(targetSel) : elt;
    if (!target) target = elt;
    var content = part.body;
    if (select) {
      var tmp = document.createElement("div");
      tmp.innerHTML = content;
      var selected = tmp.querySelector(select);
      content = selected ? selected.outerHTML : content;
    }
    htmx.swap(target, content, { swapStyle: swap });
    if (h["hx-part-id"] && elt && elt.setAttribute) {
      elt.setAttribute("data-hx-last-part-id", h["hx-part-id"]);
    }
  }

  htmx.defineExtension("hx-multipart", {
    onEvent: function (name, evt) {
      if (name !== "htmx:beforeOnLoad" && name !== "htmx:beforeSwap") return true;
      var xhr = evt.detail && (evt.detail.xhr || evt.detail.requestConfig && evt.detail.requestConfig.xhr);
      if (!xhr) return true;
      var ct = xhr.getResponseHeader("Content-Type") || "";
      if (ct.toLowerCase().indexOf("multipart/mixed") < 0) return true;

      var elt = evt.detail.elt;
      var defaults = {
        target:
          xhr.getResponseHeader("HX-Target") ||
          (elt && elt.getAttribute && elt.getAttribute("hx-target")),
        swap:
          xhr.getResponseHeader("HX-Swap") ||
          (elt && elt.getAttribute && elt.getAttribute("hx-swap")),
        select:
          xhr.getResponseHeader("HX-Select") ||
          (elt && elt.getAttribute && elt.getAttribute("hx-select")),
      };
      var parts = parseMultipart(xhr.responseText, ct);
      for (var i = 0; i < parts.length; i++) {
        swapPart(htmx, elt, parts[i], defaults);
      }
      if (evt.detail) {
        evt.detail.shouldSwap = false;
        evt.detail.isError = false;
      }
      if (typeof evt.preventDefault === "function") evt.preventDefault();
      return false;
    },
  });
})();
