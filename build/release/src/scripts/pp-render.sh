#!/bin/ksh
# pp-render — render a songs composition to audio on the laptop, off the image.
#
# The control plane (Nopales on the habitat) authors the manifest; this
# script is the data plane wiring: fetch the manifest from /songs?fmt=show,
# run the Zig engine (engine/pp-render) to synthesize a WAV, and push it
# back as a media blob. The image never synthesizes (borge/songs.borg
# section rendering: control plane in-image, data plane native).
#
#   export PP_URL=… PP_USER=… PP_PASS=…       (or ~/.config/pricklypear/env)
#   scripts/pp-render.sh kadosh [-o out.wav] [--upload] [--music-track TITLE]
#
#   kadosh              slug of a chordsong row on the current branch
#   -o FILE             write the WAV here (default /tmp/<slug>.wav)
#   --upload            push the WAV to the habitat as audio/wav media
#   --music-track TITLE also create a music-track row pointing at the blob
#
# Exit codes: 0 ok, 1 render/upload error, 2 usage/config.

set -e
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
ENGINE="$ROOT/engine/zig-out/bin/pp-render"

if [ ! -x "$ENGINE" ]; then
  echo "pp-render: engine not built; run: (cd '$ROOT/engine' && zig build)" >&2
  exit 2
fi

SLUG=""
OUT=""
UPLOAD=0
TRACK=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) OUT="$2"; shift 2 ;;
    --upload) UPLOAD=1; shift ;;
    --music-track) TRACK="$2"; shift 2 ;;
    -*) echo "pp-render: unknown option $1" >&2; exit 2 ;;
    *) SLUG="$1"; shift ;;
  esac
done

if [ -z "$SLUG" ]; then
  echo "usage: pp-render.sh SLUG [-o FILE] [--upload] [--music-track TITLE]" >&2
  exit 2
fi

for v in PP_URL PP_USER PP_PASS; do
  eval "val=\$$v"
  if [ -z "$val" ]; then
    echo "pp-render: $v is not set (source ~/.config/pricklypear/env)" >&2
    exit 2
  fi
done

[ -n "$OUT" ] || OUT="/tmp/$SLUG.wav"

MANIFEST="/tmp/pp-render-$SLUG.manifest.json"
echo "pp-render: fetching manifest for '$SLUG'…" >&2
curl -sS -u "$PP_USER:$PP_PASS" "$PP_URL/songs?slug=$SLUG&fmt=show" -o "$MANIFEST"

if head -c 40 "$MANIFEST" | grep -q '"ok":false'; then
  echo "pp-render: manifest refused: $(cat "$MANIFEST")" >&2
  exit 1
fi

echo "pp-render: synthesizing -> $OUT" >&2
"$ENGINE" < "$MANIFEST" > "$OUT"
echo "pp-render: wrote $(wc -c < "$OUT" | tr -d ' ') bytes" >&2

if [ "$UPLOAD" = 1 ]; then
  echo "pp-render: uploading media…" >&2
  MEDIA_ID="$(python3 "$ROOT/scripts/pp_media.py" put "$OUT" --type audio/wav 2>/dev/null | tail -1)"
  echo "pp-render: media id $MEDIA_ID" >&2
  if [ -n "$TRACK" ]; then
    python3 "$ROOT/scripts/pp_eval.py" -j "(music/create \"$TRACK\" \"songs engine\" \"songs\" \"$MEDIA_ID\")" >/dev/null
    echo "pp-render: music-track '$TRACK' created" >&2
  fi
fi

echo "$OUT"
