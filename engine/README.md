# engine/ — the Pricklypear audio data plane

Spec: [`borge/songs.borg`](../borge/songs.borg) § *rendering*.

Nopales owns the **control plane** (composition → score → performance →
manifest); this engine is the **data plane**. The image never synthesizes,
transports, or rasterizes — engines run outside it. SuperCollider shape:
Nopales is `sclang`, this engine is `scsynth`, and the manifest is the
protocol between them.

## One source, three hosts (only the first exists today)

| host | status | what it does |
|------|--------|--------------|
| native offline (this cut) | **shipped** | manifest on stdin → WAV on stdout |
| native live | planned | transport + loop-point hot-swap, projectors |
| browser WASM AudioWorklet | planned | the browser IS the audio device |

Zig was chosen for the target story: one engine source compiles to the
browser worklet (WASM), the native offline renderer, and a later native
live host. Oxcaml was considered and rejected on the WASM story alone.

## Build & test

```sh
cd engine
zig build test          # unit tests
zig build                # debug binary -> zig-out/bin/pp-render
zig build -Doptimize=ReleaseFast
```

Zig 0.17 (pinned in the repo toolchain). Build artifacts are gitignored.

## Contract

`pp-render` reads a manifest JSON (the `GET /songs?slug=…&fmt=show`
body) on **stdin** and writes a 16-bit mono 22050 Hz PCM **WAV** on
stdout. It interprets:

- `performance.tempo` — BPM (12 IR ticks per beat)
- `performance.sections[].events[]` — `start`/`dur` in ticks, `dyn`
  (0..1, continuous — not MIDI velocity), and `tones` (spelled note
  names; the engine maps them to pitch classes and mid-register octaves)

It composes nothing, analyzes nothing, and knows no keys. Patch programs,
bakes, transport, and the live hosts are the planned tail.

Errors exit non-zero with a one-line message on stderr
(`MalformedManifest`, `NoEvents`).

## Driving it from the habitat

[`scripts/pp-render.sh`](../scripts/pp-render.sh) does the full loop from
a laptop — fetch the manifest, render, and optionally land the audio:

```sh
export PP_URL=… PP_USER=… PP_PASS=…
scripts/pp-render.sh kadosh --upload --music-track "Kadosh (rendered)"
```

That produces a `media_blob` and a `music-track` row, so `/music` plays a
*rendered* composition exactly like a recording.
