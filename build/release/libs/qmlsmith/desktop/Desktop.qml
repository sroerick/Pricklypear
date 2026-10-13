import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.Commons
import "Smith.js" as Smith

// QML Smith: live desktop widgets rendered by Pricklypear.
// Same shape as UI prims rendering html: a Nopales function returns a
// UI prim tree, qml/compile emits QtQuick, this host renders it on the
// desktop and polls for fresh renders. Buttons carry (eval "...") in
// their objectName; clicking evals them back on the image.
Item {
  id: root

  property var shell: null
  property var manifest: null

  property bool opened: false            // manage popup
  property string statusText: ""
  property string statusKind: "idle"     // idle | ok | error

  readonly property string home: Quickshell.env("HOME") || ""
  readonly property string dataDir: Quickshell.env("QMLSMITH_DIR")
      || (home + "/.config/omarchy/qmlsmith")

  property string ppUrl: Quickshell.env("PP_URL") || ""
  property string ppUser: Quickshell.env("PP_USER") || ""
  property string ppPass: Quickshell.env("PP_PASS") || ""

  property var widgets: []               // [{name, expr, refresh}]

  Component.onCompleted: {
    mkdirProc.running = true
  }

  FileView {
    id: envFile
    path: root.home + "/.config/pricklypear/env"
    printErrors: false
    onLoaded: Smith.applyEnv(root, text())
  }
  FileView {
    id: configFile
    path: root.dataDir + "/widgets.json"
    printErrors: false
    watchChanges: true
    onLoaded: root.applyConfig(text())
    onLoadFailed: root.applyConfig("[]")
    onFileChanged: configFile.reload()
  }

  Process { id: mkdirProc; command: ["mkdir", "-p", root.dataDir]; onExited: configFile.reload() }

  function applyConfig(text) {
    var list = []
    try {
      var parsed = JSON.parse(text)
      if (Array.isArray(parsed)) list = parsed
    } catch (e) {}
    if (JSON.stringify(list) !== JSON.stringify(root.widgets)) root.widgets = list
  }

  function saveConfig() {
    configFile.setText(JSON.stringify(root.widgets, null, 2))
  }

  // ---- shell lifecycle (manage popup)
  function open(payloadJson) {
    root.opened = true
    root.statusKind = "idle"
    root.statusText = "Widgets render from Pricklypear prims; edit exprs below."
    Qt.callLater(function() { addName.forceActiveFocus() })
  }
  function close() { root.opened = false }
  function dismiss() { root.opened = false }
  function toggle() {
    if (root.opened) root.dismiss()
    else root.open("{}")
  }

  function addWidget() {
    var expr = addExpr.text.trim()
    if (expr.length === 0) return
    var name = Smith.slugify(addName.text.trim() || Date.now().toString(36))
    var refresh = parseInt(addRefresh.text.trim(), 10)
    if (isNaN(refresh) || refresh < 3) refresh = 8
    for (var i = 0; i < root.widgets.length; i++)
      if (root.widgets[i].name === name) return
    var list = root.widgets.slice()
    list.push({ name: name, expr: expr, refresh: refresh })
    root.widgets = list
    saveConfig()
    addName.text = ""
    addExpr.text = ""
    root.statusKind = "ok"
    root.statusText = "Added " + name + ". Rendering on the desktop."
  }

  function removeWidget(name) {
    var list = []
    for (var i = 0; i < root.widgets.length; i++)
      if (root.widgets[i].name !== name) list.push(root.widgets[i])
    root.widgets = list
    saveConfig()
  }

  // ---- eval helper (shared)
  function doEval(expr, cb) {
    if (root.ppUrl.length === 0) {
      root.statusKind = "error"
      root.statusText = "No PP_URL in ~/.config/pricklypear/env."
      return
    }
    var xhr = new XMLHttpRequest()
    xhr.open("POST", root.ppUrl.replace(/\/+$/, "") + "/api/eval", true)
    xhr.setRequestHeader("Content-Type", "application/json")
    if (root.ppUser.length > 0)
      xhr.setRequestHeader("Authorization",
                           "Basic " + Qt.btoa(root.ppUser + ":" + root.ppPass))
    xhr.onreadystatechange = function() {
      if (xhr.readyState !== XMLHttpRequest.DONE) return
      if (xhr.status === 200) {
        var resp = null
        try { resp = JSON.parse(xhr.responseText) } catch (e) {}
        if (resp && resp.ok) {
          var value = resp.value
          if (typeof value === "string") {
            try { value = JSON.parse(value) } catch (e) {}
          }
          cb(value, null)
        } else {
          cb(null, (resp && resp.error) ? resp.error : "bad response")
        }
      } else {
        cb(null, "HTTP " + xhr.status + " from the image")
      }
    }
    xhr.send(JSON.stringify({ expr: expr }))
  }

  // =============================================================== desktop
  PanelWindow {
    id: desk
    visible: true
    anchors { top: true; bottom: true; left: true; right: true }
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.namespace: "omarchy-qmlsmith-desk"
    WlrLayershell.layer: WlrLayer.Bottom
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

    Flow {
      x: 44
      y: 40
      spacing: 28
      width: desk.width - 88

      Repeater {
        model: root.widgets

        delegate: LiveWidget {
          required property var modelData
          cfg: modelData
        }
      }
    }
  }

  // one live widget cell
  component LiveWidget: Item {
    id: cell
    property var cfg: null
    property string lastName: ""
    property int containerW: 240
    property int containerH: 110
    width: containerW + 4
    height: containerH + 22

    onCfgChanged: {
      if (cfg) {
        poller.interval = (cfg.refresh || 8) * 1000
        poller.restart()
        fetchNow()
      }
    }

    // poll the image for a fresh render
    Timer { id: poller; repeat: true; running: cfg !== null; onTriggered: cell.fetchNow() }

    function fetchNow() {
      if (!cfg) return
      root.doEval(cfg.expr, function(value, err) {
        if (err) { root.statusKind = "error"; root.statusText = cfg.name + ": " + err; return }
        if (!value || typeof value.qml !== "string") { root.statusKind = "error"; root.statusText = cfg.name + ": no qml"; return }
        root.statusKind = "ok"
        root.statusText = cfg.name + " rendered."
        cell.render(value.qml)
      })
    }

    function render(qmlText) {
      // replace children
      for (var i = stage.children.length - 1; i >= 0; i--) {
        var c = stage.children[i]
        if (c && c.objectName === "gen") { c.destroy(); }
      }
      try {
        var item = Qt.createQmlObject(qmlText, stage, "qwidget-" + (cfg ? cfg.name : "x"))
        item.objectName = "gen"
        cell.containerW = item.width || 240
        cell.containerH = item.height || 110
        wireActions(item, cfg)
      } catch (e) {
        root.statusKind = "error"
        root.statusText = (cfg ? cfg.name : "?") + ": " + e
      }
    }

    function wireActions(item, wcfg) {
      if (!item || !item.children) return
      var kids = item.children
      for (var i = 0; i < kids.length; i++) {
        var k = kids[i]
        if (k.objectName && k.objectName.indexOf("act|") === 0) {
          var evalExpr = k.objectName.slice(4)
          attachClick(k, wcfg, evalExpr)
        }
        cell.wireActions(k, wcfg)
      }
    }

    function attachClick(target, wcfg, evalExpr) {
      var ma = Qt.createQmlObject(
        "import QtQuick; MouseArea { anchors.fill: parent; cursorShape: Qt.PointingHandCursor }",
        target, "qsmith-click")
      ma.clicked.connect(function() {
        root.doEval(evalExpr, function(value, err) {
          if (err) { root.statusKind = "error"; root.statusText = "action: " + err; return }
          cell.fetchNow()
        })
      })
    }

    Rectangle {
      anchors.fill: parent
      radius: 16
      color: hover.containsMouse ? "#14ffffff" : "#0affffff"
    }
    MouseArea {
      id: hover
      anchors.fill: parent
      hoverEnabled: true
      z: -1
    }

    Item { id: stage; anchors.fill: parent }

    Text {
      visible: hover.containsMouse
      anchors.bottom: parent.bottom
      anchors.horizontalCenter: parent.horizontalCenter
      text: cell.cfg ? cell.cfg.name : ""
      color: "#ffffff"
      opacity: 0.65
      font.pixelSize: 11
      styleColor: "#80000000"
      style: Text.Outline
    }
  }

  // ============================================================ manage popup
  PanelWindow {
    id: manage
    visible: root.opened
    anchors { bottom: true; right: true }
    margins { bottom: 6; right: 14 }
    implicitWidth: 500
    implicitHeight: 420
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.namespace: "omarchy-qmlsmith"
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: (root.opened && addName.activeFocus)
                                 ? WlrKeyboardFocus.Exclusive
                                 : WlrKeyboardFocus.OnDemand

    Item {
      anchors.fill: parent

      Column {
        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.margins: 6
        spacing: 8

        Rectangle {
          width: 492
          height: colHeight()
          radius: 12
          color: root.statusKind === "error" ? "#5a1f1f" : Color.menu.background
          border.width: 1
          border.color: root.statusKind === "error" ? "#a63d3d" : Color.menu.border

          function colHeight() {
            var h = 24
            h += statusText.implicitHeight + 8
            if (root.widgets.length > 0) h += root.widgets.length * 30 + 8
            h += 130
            return Math.min(h, 340)
          }

          Column {
            id: mgrCol
            anchors.centerIn: parent
            width: parent.width - 28
            spacing: 8

            Text {
              id: statusText
              width: parent.width
              wrapMode: Text.Wrap
              font.pixelSize: 12
              color: root.statusKind === "error" ? "#f2cfcf" : Color.menu.text
              text: root.statusText
            }

            Repeater {
              model: root.widgets
              delegate: Row {
                required property var modelData
                spacing: 8
                Text {
                  width: 70
                  elide: Text.ElideRight
                  font.pixelSize: 12
                  color: Color.menu.text
                  text: modelData.name
                  anchors.verticalCenter: parent.verticalCenter
                }
                Text {
                  width: 320
                  elide: Text.ElideMiddle
                  font.pixelSize: 11
                  opacity: 0.6
                  color: Color.menu.text
                  text: modelData.expr
                  anchors.verticalCenter: parent.verticalCenter
                }
                LindButton {
                  label: "✕"
                  onClicked: root.removeWidget(wName())
                  function wName() { return modelData.name }
                }
              }
            }

            Rectangle { height: 1; color: Color.menu.border; width: parent.width }

            Text {
              font.pixelSize: 11
              opacity: 0.55
              color: Color.menu.text
              text: "add a widget: any expr that returns a widget, e.g."
            }
            Text {
              font.pixelSize: 11
              font.family: "monospace"
              opacity: 0.75
              color: Color.menu.text
              text: "(qmlsmith/render qml/demo)"
            }

            Row {
              spacing: 6
              LimeInput { id: addName; width: 90; placeholder: "name" }
              LimeInput { id: addRefresh; width: 60; placeholder: "sec" }
              LimeInput {
                id: addExpr
                width: mgrCol.width - 172
                placeholder: "expr (Enter adds)"
                onAccepted: root.addWidget()
              }
              LindButton { label: "Add"; onClicked: root.addWidget() }
            }
          }
        }

        Row {
          spacing: 6
          LindButton { label: "Rescan config"; onClicked: configFile.reload() }
          Item { width: 1; height: 1 }
          Text {
            anchors.verticalCenter: parent.verticalCenter
            font.pixelSize: 11
            color: Color.menu.text
            opacity: 0.55
            text: "Esc closes"
          }
        }
      }

      Keys.priority: Keys.BeforeItem
      Keys.onPressed: function(event) {
        if (event.key === Qt.Key_Escape) { root.dismiss(); event.accepted = true }
      }
      MouseArea {
        anchors.fill: parent
        z: -1
        onClicked: root.dismiss()
      }
    }
  }

  component LimeInput: Rectangle {
    id: li
    property string placeholder: ""
    signal accepted()
    height: 28
    radius: 6
    color: "transparent"
    border.width: 1
    border.color: tin.activeFocus ? Color.accent : Color.menu.border

    TextInput {
      id: tin
      anchors.fill: parent
      anchors.leftMargin: 7
      anchors.rightMargin: 7
      anchors.verticalCenter: parent.verticalCenter
      font.pixelSize: 12
      color: Color.menu.text
      clip: true
      onAccepted: li.accepted()
      Text {
        anchors.fill: parent
        anchors.leftMargin: 1
        verticalAlignment: Text.AlignVCenter
        visible: tin.text.length === 0 && !tin.activeFocus
        font.pixelSize: 12
        color: Color.menu.text
        opacity: 0.4
        text: li.placeholder
      }
      Keys.onPressed: function(event) {
        if (event.key === Qt.Key_Escape) { root.dismiss(); event.accepted = true }
        event.accepted = false
      }
    }
  }

  component LindButton: Rectangle {
    id: lb
    property string label: ""
    signal clicked()
    radius: 6
    width: label.length > 0 ? lbText.implicitWidth + 18 : 0
    height: visible ? 24 : 0
    color: lbArea.containsMouse ? Color.menu.selectedBackground : "transparent"
    border.width: 1
    border.color: Color.menu.border
    Text {
      id: lbText
      anchors.centerIn: parent
      font.pixelSize: 11
      color: Color.menu.text
      text: lb.label
    }
    MouseArea {
      id: lbArea
      anchors.fill: parent
      hoverEnabled: true
      onClicked: lb.clicked()
    }
  }
}
