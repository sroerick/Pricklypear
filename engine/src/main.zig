//! engine/pp-render — the Pricklypear audio data plane (toy, first cut).
//!
//! Contract (borge/songs.borg section rendering): a MANIFEST JSON on
//! stdin, a 16-bit mono PCM WAV on stdout. The engine composes nothing,
//! analyzes nothing, knows no keys — it interprets the manifest's
//! performance events (spelled tones, integer ticks, continuous
//! dynamics, strum models, slash bass) into samples. Nopales owns the
//! control plane; this owns the data plane.
//!
//! This is deliberately small: it closes the loop manifest -> bytes ->
//! media_blob -> /music. Real patches, bakes and the live/WASM hosts
//! come later.
const std = @import("std");

const sample_rate: u32 = 22050;
const ticks_per_beat: f64 = 12.0; // the IR's time grid (borge/songs.borg)

/// One sounding note, resolved to seconds.
const Note = struct {
    freq: f64,
    start_sec: f64,
    dur_sec: f64,
    dyn: f64,
    /// Pluck brightness: 0 = pure sine, 1 = more harmonic content. The
    /// bass stays rounder; upper chord tones pluck.
    bright: f64,
};

const EngineError = error{ MalformedManifest, NoEvents };

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;

    var in_buf: [64 * 1024]u8 = undefined;
    var reader = std.Io.File.stdin().reader(io, &in_buf);
    const input = try reader.interface.allocRemaining(gpa, .unlimited);
    defer gpa.free(input);

    const wav = render(gpa, input) catch |err| {
        std.debug.print("pp-render: {s}\n", .{@errorName(err)});
        std.process.exit(1);
    };
    defer gpa.free(wav);

    var out_buf: [64 * 1024]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &out_buf);
    try writer.interface.writeAll(wav);
    try writer.interface.flush();
}

/// Synthesize one sample at time `t` (seconds) from the notes.
fn sampleAt(notes: []const Note, t: f64) f64 {
    const attack = 0.008; // seconds
    const release = 0.06;
    const decay_tau = 1.1; // pluck decay time constant
    const sustain_floor = 0.22;
    var mix: f64 = 0.0;
    for (notes) |n| {
        if (t < n.start_sec or t >= n.start_sec + n.dur_sec) continue;
        const local = t - n.start_sec;
        var env: f64 = 1.0;
        if (local < attack) env = local / attack;
        const from_end = n.dur_sec - local;
        if (from_end < release) env = @min(env, from_end / release);
        if (env < 0.0) env = 0.0;
        // Pluck: exponential decay toward a sustain floor.
        env *= sustain_floor + (1.0 - sustain_floor) * std.math.exp(-local / decay_tau);

        // Fundamental plus a decaying harmonic stack for a plucked-string
        // feel (brighter for upper tones, rounder for bass).
        const w = 2.0 * std.math.pi * n.freq;
        var osc = std.math.sin(w * t);
        if (n.bright > 0.0) {
            const h2 = std.math.sin(2.0 * w * t) * 0.28 * n.bright;
            const h3 = std.math.sin(3.0 * w * t) * 0.12 * n.bright;
            osc += (h2 + h3) * std.math.exp(-local / (decay_tau * 0.5));
        }
        mix += osc * env * n.dyn;
    }
    return mix;
}

/// Manifest JSON -> WAV bytes.
pub fn render(gpa: std.mem.Allocator, manifest: []const u8) ![]u8 {
    const parsed = std.json.parseFromSlice(std.json.Value, gpa, manifest, .{}) catch
        return EngineError.MalformedManifest;
    defer parsed.deinit();
    const root = parsed.value;
    if (root != .object) return EngineError.MalformedManifest;

    const perf = root.object.get("performance") orelse return EngineError.MalformedManifest;
    if (perf != .object) return EngineError.MalformedManifest;
    const tempo = getNumber(perf.object.get("tempo")) orelse 100.0;
    const sec_per_tick = 60.0 / tempo / ticks_per_beat;

    var notes = std.ArrayList(Note).empty;
    defer notes.deinit(gpa);

    const sections = perf.object.get("sections") orelse return EngineError.NoEvents;
    if (sections != .array) return EngineError.MalformedManifest;
    for (sections.array.items) |section| {
        if (section != .object) continue;
        const events = section.object.get("events") orelse continue;
        if (events != .array) continue;
        for (events.array.items) |event| {
            if (event != .object) continue;
            try addEvent(gpa, &notes, event.object, sec_per_tick);
        }
    }

    if (notes.items.len == 0) return EngineError.NoEvents;

    var max_sec: f64 = 0.0;
    for (notes.items) |n| {
        const end = n.start_sec + n.dur_sec;
        if (end > max_sec) max_sec = end;
    }
    const total_samples: usize = @intFromFloat((max_sec + 0.2) * @as(f64, @floatFromInt(sample_rate)));
    if (total_samples == 0) return EngineError.NoEvents;

    // Pass 1: find peak so we can normalize without clipping.
    var peak: f64 = 0.0;
    for (0..total_samples) |idx| {
        const t = @as(f64, @floatFromInt(idx)) / @as(f64, @floatFromInt(sample_rate));
        const v = @abs(sampleAt(notes.items, t));
        if (v > peak) peak = v;
    }
    const gain: f64 = if (peak > 0.0) @min(0.9 / peak, 4.0) else 1.0;

    // Pass 2: render normalized PCM.
    const pcm = try gpa.alloc(i16, total_samples);
    defer gpa.free(pcm);
    for (pcm, 0..) |*sample, idx| {
        const t = @as(f64, @floatFromInt(idx)) / @as(f64, @floatFromInt(sample_rate));
        const clamped = std.math.clamp(sampleAt(notes.items, t) * gain, -1.0, 1.0);
        sample.* = @intFromFloat(clamped * 32767.0);
    }

    return encodeWav(gpa, pcm);
}

/// One performance event -> notes: the chord tones (optionally strummed)
/// plus, for a slash chord, the bass note an octave down.
fn addEvent(
    gpa: std.mem.Allocator,
    notes: *std.ArrayList(Note),
    event: std.json.ObjectMap,
    sec_per_tick: f64,
) !void {
    const start_sec = (getNumber(event.get("start")) orelse 0.0) * sec_per_tick;
    const dur_sec = (getNumber(event.get("dur")) orelse ticks_per_beat) * sec_per_tick;
    const dyn = getNumber(event.get("dyn")) orelse 0.8;

    // Strum model: {dir, spread_ms}. Absent/empty => block chord.
    var spread_sec: f64 = 0.0;
    var downward = true;
    if (event.get("strum")) |s| {
        if (s == .object) {
            if (getNumber(s.object.get("spread_ms"))) |ms| spread_sec = ms / 1000.0;
            if (s.object.get("dir")) |d| {
                if (d == .string and std.mem.eql(u8, d.string, "up")) downward = false;
            }
        }
    }

    const tones = event.get("tones") orelse return;
    if (tones != .array) return;

    var freqs = std.ArrayList(f64).empty;
    defer freqs.deinit(gpa);
    for (tones.array.items) |tone| {
        if (tone != .string) continue;
        const pc = notePitchClass(tone.string) orelse continue;
        try freqs.append(gpa, freqOf(60 + pc)); // mid register (C4..B4)
    }
    if (freqs.items.len == 0) return;

    // Down strum: low-to-high (order as given). Up strum: high-to-low.
    const n = freqs.items.len;
    for (freqs.items, 0..) |f, i| {
        const order_idx = if (downward) i else n - 1 - i;
        try notes.append(gpa, .{
            .freq = f,
            .start_sec = start_sec + @as(f64, @floatFromInt(order_idx)) * spread_sec,
            .dur_sec = dur_sec,
            .dyn = dyn * 0.9,
            .bright = 0.8,
        });
    }

    // Slash bass: sound the named bass an octave below, on the beat.
    if (event.get("bass")) |b| {
        if (b == .string and b.string.len > 0) {
            if (notePitchClass(b.string)) |pc| {
                try notes.append(gpa, .{
                    .freq = freqOf(48 + pc), // C3..B3
                    .start_sec = start_sec,
                    .dur_sec = dur_sec,
                    .dyn = dyn,
                    .bright = 0.35,
                });
            }
        }
    }
}

fn freqOf(midi: i32) f64 {
    return 440.0 * std.math.pow(f64, 2.0, (@as(f64, @floatFromInt(midi)) - 69.0) / 12.0);
}

fn getNumber(v: ?std.json.Value) ?f64 {
    const x = v orelse return null;
    return switch (x) {
        .integer => |i| @floatFromInt(i),
        .float => |f| f,
        else => null,
    };
}

/// Spelled note name (A, C, F#, Bb) -> pitch class 0..11 (C=0). Null when
/// the name is not a chromatic note (we ignore weird spellings in the toy).
fn notePitchClass(name: []const u8) ?i32 {
    if (name.len == 0) return null;
    const base: i32 = switch (std.ascii.toUpper(name[0])) {
        'C' => 0,
        'D' => 2,
        'E' => 4,
        'F' => 5,
        'G' => 7,
        'A' => 9,
        'B' => 11,
        else => return null,
    };
    var accidental: i32 = 0;
    var i: usize = 1;
    while (i < name.len) : (i += 1) {
        switch (name[i]) {
            '#' => accidental += 1,
            'b' => accidental -= 1,
            else => return null,
        }
    }
    const pc = @mod(base + accidental, 12);
    return pc;
}

/// 16-bit mono PCM -> a RIFF/WAVE byte buffer.
fn encodeWav(gpa: std.mem.Allocator, pcm: []const i16) ![]u8 {
    var buf = std.ArrayList(u8).empty;
    errdefer buf.deinit(gpa);

    const data_bytes: u32 = @intCast(pcm.len * 2);
    const byte_rate: u32 = sample_rate * 2; // mono * 16-bit
    const riff_size: u32 = 36 + data_bytes;

    try buf.appendSlice(gpa, "RIFF");
    try appendU32(&buf, gpa, riff_size);
    try buf.appendSlice(gpa, "WAVE");
    try buf.appendSlice(gpa, "fmt ");
    try appendU32(&buf, gpa, 16); // PCM chunk size
    try appendU16(&buf, gpa, 1); // audio format: PCM
    try appendU16(&buf, gpa, 1); // channels: mono
    try appendU32(&buf, gpa, sample_rate);
    try appendU32(&buf, gpa, byte_rate);
    try appendU16(&buf, gpa, 2); // block align
    try appendU16(&buf, gpa, 16); // bits per sample
    try buf.appendSlice(gpa, "data");
    try appendU32(&buf, gpa, data_bytes);
    for (pcm) |s| {
        const u: u16 = @bitCast(s);
        try appendU16(&buf, gpa, u);
    }
    return buf.toOwnedSlice(gpa);
}

fn appendU16(buf: *std.ArrayList(u8), gpa: std.mem.Allocator, v: u16) !void {
    try buf.append(gpa, @intCast(v & 0xff));
    try buf.append(gpa, @intCast((v >> 8) & 0xff));
}

fn appendU32(buf: *std.ArrayList(u8), gpa: std.mem.Allocator, v: u32) !void {
    try buf.append(gpa, @intCast(v & 0xff));
    try buf.append(gpa, @intCast((v >> 8) & 0xff));
    try buf.append(gpa, @intCast((v >> 16) & 0xff));
    try buf.append(gpa, @intCast((v >> 24) & 0xff));
}

test "notePitchClass" {
    try std.testing.expectEqual(@as(?i32, 9), notePitchClass("A"));
    try std.testing.expectEqual(@as(?i32, 6), notePitchClass("F#"));
    try std.testing.expectEqual(@as(?i32, 10), notePitchClass("Bb"));
    try std.testing.expectEqual(@as(?i32, 5), notePitchClass("F"));
    try std.testing.expectEqual(@as(?i32, null), notePitchClass("H"));
}

test "render a minimal manifest to a WAV" {
    const gpa = std.testing.allocator;
    const manifest =
        \\{"form":"chart","performance":{"key":"Am","tempo":70,"sections":[
        \\{"name":"V","events":[{"chord":"Am","start":0,"dur":48,
        \\"tones":["A","C","E"],"bass":"","dyn":0.8}]}]}}
    ;
    const wav = try render(gpa, manifest);
    defer gpa.free(wav);
    try std.testing.expect(wav.len > 44);
    try std.testing.expectEqualStrings("RIFF", wav[0..4]);
    try std.testing.expectEqualStrings("WAVE", wav[8..12]);
    try std.testing.expectEqualStrings("data", wav[36..40]);
}

test "slash bass adds a lower note; strum spreads onsets" {
    const gpa = std.testing.allocator;
    const manifest =
        \\{"performance":{"tempo":60,"sections":[
        \\{"events":[{"chord":"Esus/A","start":0,"dur":48,
        \\"tones":["E","A","B"],"bass":"A","dyn":0.8,
        \\"strum":{"dir":"down","spread_ms":18}}]}]}}
    ;
    const wav = try render(gpa, manifest);
    defer gpa.free(wav);
    try std.testing.expect(wav.len > 44);
}
