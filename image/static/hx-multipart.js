/**
 * hx-multipart — htmx 4 extension for multipart/mixed responses.
 *
 * Vendored for Pricklypear (no CDN). Ported from the htmx 1.9 XHR-era
 * extension to the htmx 4 event-based API (registerExtension + hooks).
 *
 * When a response arrives with Content-Type: multipart/mixed, its parts are
 * parsed and each part is swapped using its own HX-Target / HX-Swap /
 * HX-Select headers (falling back to envelope response headers, then the
 * requesting element's hx-target / hx-swap / hx-select attributes).
 * HX-Part-ID is recorded on the request source element for reconnect cursors.
 * The raw multipart envelope itself is never swapped into the target.
 *
 * Wire protocol surface (unchanged): borge/multipart.borg (mime-framing).
 */
(() => {
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

  function reprocess(node) {
    // Re-wire hx-* attributes on freshly swapped-in content, if the public
    // process API is available (htmx 4 keeps it). Best effort only.
    if (window.htmx && typeof htmx.process === "function") {
      try { htmx.process(node); } catch (_e) {}
    }
  }

  function swapContent(target, content, style) {
    var s = String(style || "innerHTML").toLowerCase();
    switch (s) {
      case "innerhtml":
        target.innerHTML = content;
        reprocess(target);
        break;
      case "outerhtml": {
        var tpl = document.createElement("template");
        tpl.innerHTML = content.trim();
        var nodes = Array.prototype.slice.call(tpl.content.childNodes);
        if (!nodes.length) break;
        var parent = target.parentNode;
        target.replaceWith.apply(target, nodes);
        if (parent) reprocess(parent);
        break;
      }
      case "beforebegin":
        target.insertAdjacentHTML("beforebegin", content);
        if (target.parentNode) reprocess(target.parentNode);
        break;
      case "afterend":
        target.insertAdjacentHTML("afterend", content);
        if (target.parentNode) reprocess(target.parentNode);
        break;
      case "afterbegin":
      case "beforeend":
        target.insertAdjacentHTML(s, content);
        reprocess(target);
        break;
      case "delete":
        target.remove();
        break;
      default: // "none" and unknown styles: leave the part unswapped
        break;
    }
  }

  function swapPart(elt, part, defaults) {
    var h = part.headers || {};
    var targetSel = h["hx-target"] || defaults.target;
    var swap =
      h["hx-swap"] ||
      defaults.swap ||
      (window.htmx && (htmx.config.defaultSwap || htmx.config.defaultSwapStyle)) ||
      "innerHTML";
    var select = h["hx-select"] || defaults.select;
    var target = targetSel ? document.querySelector(targetSel) : null;
    if (!target) target = elt;
    if (!target) return;
    var content = part.body;
    if (select) {
      var tmp = document.createElement("div");
      tmp.innerHTML = content;
      var selected = tmp.querySelector(select);
      content = selected ? selected.outerHTML : content;
    }
    swapContent(target, content, swap);
    if (h["hx-part-id"] && elt && elt.setAttribute) {
      elt.setAttribute("data-hx-last-part-id", h["hx-part-id"]);
    }
  }

  htmx.registerExtension("hx-multipart", {
    // After the fetch completes: if the response is multipart/mixed, parse
    // it and stash the parts + defaults on the request context.
    htmx_after_request: function (elt, detail) {
      var ctx = detail && detail.ctx;
      if (!ctx || !ctx.response) return;
      var headers = ctx.response.headers;
      var ct =
        (headers && typeof headers.get === "function" && headers.get("content-type")) || "";
      if (ct.toLowerCase().indexOf("multipart/mixed") < 0) return;
      var hget = function (name) {
        return headers && typeof headers.get === "function" ? headers.get(name) : null;
      };
      var defaults = {
        target: hget("hx-target") || (elt.getAttribute && elt.getAttribute("hx-target")),
        swap: hget("hx-swap") || (elt.getAttribute && elt.getAttribute("hx-swap")),
        select: hget("hx-select") || (elt.getAttribute && elt.getAttribute("hx-select")),
      };
      ctx._hxMultipart = {
        elt: ctx.sourceElement || elt,
        defaults: defaults,
        parts: parseMultipart(ctx.text || "", ct),
      };
    },

    // Before the default swap: if this request produced multipart parts,
    // swap each part ourselves and cancel the default (the raw envelope
    // must not land in the target).
    htmx_before_swap: function (elt, detail) {
      var ctx = detail && detail.ctx;
      if (!ctx || !ctx._hxMultipart) return;
      var job = ctx._hxMultipart;
      for (var i = 0; i < job.parts.length; i++) {
        swapPart(job.elt, job.parts[i], job.defaults);
      }
      delete ctx._hxMultipart;
      detail.cancelled = true;
      return false;
    },
  });
})();
