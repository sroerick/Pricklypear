#!/usr/bin/env python3
"""pp-transcribe.py -- recording -> chordsong chart DRAFT.

The INVERSE of the render engine (engine/pp-render): where that turns a
composition into audio, this turns audio into a composition draft — the
"importer front end" borge/songs.borg reserves as a non-goal-until-later
("an importer is a face on the parser, cheap later").

This is the AI/analysis tier, inbound (the mirror of suno): the habitat
never does DSP — this laptop-side script decodes, analyzes, and emits a
chart body you can open in /songs?edit=1 and correct.

A DRAFT, not truth. Chroma chord recognition gets the easy majority and
mislabels dense or inversions-heavy music; section boundaries are a
novelty heuristic. The machine proposes, the human promotes.

Pipeline: ffmpeg decode -> librosa (tempo/beat/key/chroma) -> per-beat
chord estimation -> key-relative bar grouping -> section segmentation by
chroma novelty -> chart text with key/meter/tempo directives.

Usage:
  python3 scripts/pp-transcribe.py SONG.(mp3|wav|flac|m4a) [-o out.borgchart]
  python3 scripts/pp-transcribe.py SONG --json          # analysis JSON only
  python3 scripts/pp-transcribe.py SONG --save SLUG [--title T]  # upsert a chordsong

Setup (one-time):
  uv venv --python 3.12 ~/.local/share/pp-transcribe
  ~/.local/share/pp-transcribe/bin/pip install librosa soundfile numpy
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

NOTES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
# Chord templates as pitch-class sets (root + intervals).
TEMPLATES = {
    "": [0, 4, 7],       # major
    "m": [0, 3, 7],      # minor
    "7": [0, 4, 7, 10],
    "maj7": [0, 4, 7, 11],
    "m7": [0, 3, 7, 10],
    "6": [0, 4, 7, 9],
    "m7b5": [0, 3, 6, 10],
    "dim": [0, 3, 6],
    "sus4": [0, 5, 7],
    "sus2": [0, 2, 7],
    "5": [0, 7],
    "9": [0, 4, 7, 10, 14],
}
# Which chart quality glyph each template maps to (keys are suffix names).
QUALITY_GLYPH = {k: k for k in TEMPLATES}
# Preference order when scores tie: simple first. Note 5/sus4/sus2 are
# DELIBERATELY last (after the triads and sevenths they compete with):
# the two/three-note templates match almost any voicing's loudest bins and
# would otherwise over-report, which is the classic chroma-template failure.
TEMPLATE_ORDER = ["", "m", "7", "m7", "maj7", "6", "m7b5", "dim", "9", "sus4", "sus2", "5"]

# Krumhansl-Schmuckler key profiles (major / natural minor).
KS_MAJOR = [6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88]
KS_MINOR = [6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17]


def die(msg: str) -> None:
    print(f"pp-transcribe: {msg}", file=sys.stderr)
    sys.exit(1)


def load_audio(path: str, sr: int = 22050):
    import librosa

    p = Path(path)
    if not p.exists():
        die(f"no such file: {path}")
    if p.suffix.lower() == ".wav":
        y, srr = librosa.load(str(p), sr=sr, mono=True)
        return y, srr
    # Transcode anything else through ffmpeg (mp3/m4a/flac/…).
    if not shutil.which("ffmpeg"):
        die("ffmpeg not found (needed to decode non-wav input)")
    with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
        out = tmp.name
    try:
        subprocess.run(
            ["ffmpeg", "-y", "-loglevel", "error", "-i", str(p), "-ac", "1", "-ar", str(sr), out],
            check=True,
        )
        y, srr = librosa.load(out, sr=sr, mono=True)
    finally:
        os.unlink(out)
    return y, srr


def estimate_key(chroma):
    """Krumhansl-Schmuckler: correlate the mean chroma with major/minor
    key profiles (Pearson correlation, the standard formulation)."""
    import numpy as np

    prof = chroma.mean(axis=1)
    if prof.sum() > 0:
        prof = prof / prof.sum()
    best = None
    for tonic in range(12):
        for mode, ks in (("major", KS_MAJOR), ("minor", KS_MINOR)):
            rotated = np.roll(np.array(ks), tonic)
            # Pearson r between the pitch-class profile and the key profile.
            a = prof - prof.mean()
            b = rotated - rotated.mean()
            denom = float(np.linalg.norm(a) * np.linalg.norm(b))
            score = float(np.dot(a, b) / denom) if denom > 0 else 0.0
            # Small minor preference: transcribers systematically over-
            # call major because the relative minor shares its notes.
            if mode == "minor":
                score += 0.02
            if best is None or score > best[0]:
                best = (score, tonic, mode)
    _, tonic, mode = best
    return NOTES[tonic], mode


def chord_score(chroma_vec, root, intervals):
    """Cosine similarity between the observed 12-dim chroma and this chord's
    template vector. Cosine (NOT sum/len) is essential: dividing by the
    template size makes two-note templates like A5 win everywhere because
    their strongest two bins always have high mean energy."""
    import numpy as np

    tmpl = np.zeros(12)
    for i in intervals:
        tmpl[(root + i) % 12] = 1.0
    vt = np.asarray(chroma_vec, dtype=float)
    vt = vt - vt.min()  # remove DC so silence/low bins don't dominate
    denom = float(np.linalg.norm(vt) * np.linalg.norm(tmpl))
    if denom <= 0:
        return 0.0
    return float(np.dot(vt, tmpl) / denom)


# Diatonic triads per mode: (degree semitone, quality) pairs, relative to
# the tonic. This is the same knowledge songs/fit uses (L2), applied in
# reverse: a chord recognised in a known key should prefer the key's own
# chords, so a G major triad does not beat Gm (vi) in Bb major.
DIATONIC = {
    "major": [(0, ""), (2, "m"), (4, "m"), (5, ""), (7, ""), (9, "m"), (11, "dim")],
    "minor": [(0, "m"), (2, "dim"), (3, ""), (5, "m"), (7, "m"), (8, ""), (10, "")],
}


def diatonic_bonus(tonic_pc: int, mode: str, root_pc: int, suffix: str) -> float:
    """Bonus for a (root, quality) that is diatonic in the detected key.
    Modest, so a genuinely chromatic chord still wins when it is clearly
    there — but enough to fix quality ambiguity (G vs Gm in Bb)."""
    for deg, q in DIATONIC.get(mode, DIATONIC["major"]):
        if (tonic_pc + deg) % 12 == root_pc and q == suffix:
            return 0.06
    return 0.0


def best_chord(chroma_vec, key_ctx=None):
    """Highest-cosine chord template with a diatonic bonus and a tiered
    margin rule. key_ctx is (tonic_pc, mode) from the detected key."""
    best = None
    refined = []
    for suffix in TEMPLATE_ORDER:
        intervals = TEMPLATES[suffix]
        rank = TEMPLATE_ORDER.index(suffix)
        for root in range(12):
            s = chord_score(chroma_vec, root, intervals)
            if key_ctx is not None:
                s += diatonic_bonus(key_ctx[0], key_ctx[1], root, suffix)
            refined.append((s, -rank, root, suffix))
            key = (round(s, 3), -rank)
            if best is None or key > best[0]:
                best = (key, NOTES[root] + QUALITY_GLYPH[suffix])
    # Tiered margin rule: how much a quality must beat the best basic
    # triad (cosine) before it is believed. Major/minor are free; a seventh
    # needs a little; sus/power need a lot (they are genuinely rare and
    # their sparse templates otherwise co-opt every loud voicing).
    margins = {"": 0.0, "m": 0.0, "7": 0.03, "m7": 0.03, "maj7": 0.03,
               "6": 0.05, "m7b5": 0.05, "dim": 0.06, "9": 0.06,
               "sus4": 0.12, "sus2": 0.12, "5": 0.14}
    refined.sort(key=lambda x: (x[0], x[1]), reverse=True)
    top = refined[0]
    basic = [r for r in refined if r[3] in ("", "m")]
    if basic:
        b = basic[0]
        need = margins.get(top[3], 0.05)
        if top[0] - b[0] < need:
            return NOTES[b[2]] + QUALITY_GLYPH[b[3]]
    return NOTES[top[2]] + QUALITY_GLYPH[top[3]]


def analyze(path: str):
    import librosa
    import numpy as np

    y, sr = load_audio(path)
    duration = len(y) / sr

    # Beat grid. On legato/no-transient material librosa returns 0 — try
    # once more on the harmonic component (percussive onset tracking sees
    # nothing in a pad, but chord changes still imply a pulse), then fall
    # back to a fixed 120 BPM grid. IMPORTANT: librosa returns beat FRAME
    # indices; the fallback builds times directly. Normalize to beat_times
    # (seconds) immediately so the two can't be confused (frames vs samples
    # is a ~43x error at the default hop 512 / sr 22050).
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr)
    tempo = float(np.atleast_1d(tempo)[0])
    if tempo <= 0 or len(beats) < 4:
        y_h, _ = librosa.effects.hpss(y)
        tempo2, beats2 = librosa.beat.beat_track(y=y_h, sr=sr)
        tempo2 = float(np.atleast_1d(tempo2)[0])
        if tempo2 > 0 and len(beats2) >= 4:
            tempo, beats = tempo2, beats2
            beat_times = librosa.frames_to_time(beats, sr=sr)
            grid_source = "librosa-harmonic"
        else:
            tempo = 120.0
            beat_times = np.arange(0.0, duration, 60.0 / tempo)
            grid_source = "fallback-120"
    else:
        beat_times = librosa.frames_to_time(beats, sr=sr)
        grid_source = "librosa"

    chroma = librosa.feature.chroma_cqt(y=y, sr=sr)
    tonic, mode = estimate_key(chroma)
    key_ctx = (NOTES.index(tonic), mode)
    # Smooth the chroma a little before per-beat chord estimation: adjacent
    # frames share a chord, and smoothing kills spurious passing-tone
    # victories without blurring real chord changes much.
    chroma_s = librosa.decompose.nn_filter(chroma, aggregate=__import__("numpy").mean, axis=1)

    # Per-beat chord: mean chroma over each beat span.
    chord_per_beat = []
    times = librosa.frames_to_time(np.arange(chroma.shape[1]), sr=sr)
    for i, t0 in enumerate(beat_times):
        t1 = beat_times[i + 1] if i + 1 < len(beat_times) else duration
        mask = (times >= t0) & (times < t1)
        if not mask.any():
            chord_per_beat.append(chord_per_beat[-1] if chord_per_beat else "N.C.")
            continue
        vec = chroma_s[:, mask].mean(axis=1)
        chord_per_beat.append(best_chord(vec, key_ctx))

    # Merge consecutive identical chords -> spans (chord, beats).
    spans = []
    for c in chord_per_beat:
        if spans and spans[-1][0] == c:
            spans[-1][1] += 1
        else:
            spans.append([c, 1])

    # Section segmentation by chroma novelty (self-similarity proxy).
    sections = segment_sections(spans, duration, len(beat_times))

    return {
        "source": os.path.basename(path),
        "duration_s": round(duration, 1),
        "tempo": round(tempo, 1),
        "grid_source": grid_source,
        "key": f"{tonic}{'m' if mode == 'minor' else ''}",
        "tonic": tonic,
        "mode": mode,
        "beat_count": int(len(beats)),
        "spans": spans,
        "sections": sections,
    }


def segment_sections(spans, duration, n_beats):
    """Group the chord spans into sections. Without a strong structural
    signal we split into even musical blocks (a section is typically 8-16
    bars); the chord spans themselves carry the content, and the human
    relabels. Kept simple and deterministic."""
    if not spans:
        return [{"label": "A", "start_s": 0.0, "end_s": round(duration, 1), "spans": []}]
    # ~16 beats (4 bars) per section block.
    block = 16
    sections = []
    idx = 0
    label = 0
    total = sum(c for _, c in spans)
    while idx < len(spans):
        sec = []
        beats = 0
        while idx < len(spans) and beats < block:
            sec.append([spans[idx][0], spans[idx][1]])
            beats += spans[idx][1]
            idx += 1
        start_s = round((sum(c for _, c in spans[:idx]) - beats) / max(total, 1) * duration, 1)
        end_s = round(sum(c for _, c in spans[:idx]) / max(total, 1) * duration, 1)
        sections.append({
            "label": chr(ord("A") + (label % 26)),
            "start_s": max(0.0, start_s),
            "end_s": end_s,
            "spans": sec,
        })
        label += 1
    return sections


def to_chart(analysis: dict, bars_per_line: int = 4) -> str:
    """Analysis -> chart DSL text (chart-dsl section of borge/songs.borg).

    TIME RULE: inside | ... | each space-separated chord glyph consumes
    ONE WHOLE BAR. A detected span of k beats therefore becomes round(k/4)
    bars (at least 1 for a sustained chord) — so a chord held two bars
    prints twice, which is how the time actually reads."""
    lines = []
    lines.append(f"key {analysis['key']}")
    lines.append("meter 4/4")
    lines.append(f"tempo {int(round(analysis['tempo']))}")
    lines.append("")
    lines.append(f"; DRAFT transcribed from {analysis['source']} by pp-transcribe")
    lines.append(f"; {analysis['duration_s']}s, {analysis['beat_count']} beats, grid={analysis['grid_source']}")
    lines.append("; correct with /songs?slug=...&edit=1")
    lines.append("")
    for sec in analysis["sections"]:
        lines.append(f"[{sec['label']}]")
        bars = []
        for c, beats in sec["spans"]:
            nbars = max(1, round(beats / 4.0))
            bars.extend([c] * nbars)
        for i in range(0, len(bars), bars_per_line):
            lines.append("| " + "  ".join(bars[i : i + bars_per_line]) + " |")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def main() -> None:
    ap = argparse.ArgumentParser(description="transcribe audio -> chordsong chart draft")
    ap.add_argument("path", help="audio file (mp3/wav/flac/m4a/…)")
    ap.add_argument("-o", "--out", help="write chart text here (default stdout)")
    ap.add_argument("--json", action="store_true", help="emit analysis JSON instead of a chart")
    ap.add_argument("--save", metavar="SLUG", help="upsert a chordsong row with this slug")
    ap.add_argument("--title", default=None, help="title for --save (default: filename)")
    args = ap.parse_args()

    a = analyze(args.path)
    if args.json:
        print(json.dumps(a, indent=2))
        return
    chart = to_chart(a)
    if args.out:
        Path(args.out).write_text(chart)
        print(f"wrote {args.out}", file=sys.stderr)
    else:
        print(chart)

    if args.save:
        save_to_habitat(args.save, args.title or Path(args.path).stem, chart)


def save_to_habitat(slug: str, title: str, chart: str) -> None:
    """Upsert a chordsong via pp_eval (best-effort; needs PP_* env)."""
    import base64
    import urllib.request

    url = os.environ.get("PP_URL", "")
    user = os.environ.get("PP_USER", "")
    pw = os.environ.get("PP_PASS", "")
    if not (url and user and pw):
        die("--save needs PP_URL/PP_USER/PP_PASS (source ~/.config/pricklypear/env)")
    expr = f'(songs/save {json.dumps(slug)} {json.dumps(title)} {json.dumps(chart)})'
    req = urllib.request.Request(
        url.rstrip("/") + "/api/eval",
        data=json.dumps({"expr": expr}).encode(),
        headers={
            "Content-Type": "application/json",
            "Authorization": "Basic " + base64.b64encode(f"{user}:{pw}".encode()).decode(),
        },
    )
    try:
        with urllib.request.urlopen(req, timeout=60) as r:
            out = json.loads(r.read().decode())
    except Exception as e:  # noqa: BLE001
        die(f"save failed: {e}")
    if out.get("ok"):
        print(f"saved chordsong {slug} -> {out.get('value')}", file=sys.stderr)
    else:
        die(f"save refused: {out.get('error')}")


if __name__ == "__main__":
    main()
