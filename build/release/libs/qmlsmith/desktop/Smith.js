// QML Smith helpers. No I/O, no secrets land here.
.pragma library

// ~/.config/pricklypear/env, same variables scripts/pp-eval.sh uses.
// Only fills what is missing; real environment variables win.
function applyEnv(root, text) {
  var lines = String(text).split("\n")
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i].replace(/^export\s+/, "").trim()
    var eq = line.indexOf("=")
    if (eq <= 0) continue
    var key = line.slice(0, eq).trim()
    var value = line.slice(eq + 1).trim()
    if (value.length >= 2 &&
        ((value[0] === "\"" && value[value.length - 1] === "\"") ||
         (value[0] === "'" && value[value.length - 1] === "'")))
      value = value.slice(1, -1)
    if (key === "PP_URL" && root.ppUrl.length === 0) root.ppUrl = value
    else if (key === "PP_USER" && root.ppUser.length === 0) root.ppUser = value
    else if (key === "PP_PASS" && root.ppPass.length === 0) root.ppPass = value
  }
}

function slugify(name) {
  var s = String(name || "").toLowerCase().replace(/[^a-z0-9-]+/g, "-")
  s = s.replace(/^-+|-+$/g, "")
  return s.length > 0 ? s.slice(0, 32) : "icon"
}

// The image's compiler puts metadata in header comments.
function parseHeader(text) {
  var t = String(text || "")
  var name = ""
  var launch = ""
  var m1 = t.match(/^\/\/\s*name:\s*(.*)$/m)
  if (m1) name = m1[1].trim()
  var m2 = t.match(/^\/\/\s*launch:\s*(.*)$/m)
  if (m2) launch = m2[1].trim()
  return { name: name, launch: launch }
}

// Defensive: the image only ever emits QtQuick, but generated data is data.
function safeQml(text) {
  var t = String(text || "")
  return t.indexOf("Quickshell") === -1 && t.indexOf("XmlHttpRequest") === -1
      && t.indexOf("XMLHttpRequest") === -1 && t.indexOf("Process") === -1
}
