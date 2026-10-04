//! engine/pp-render — the Pricklypear audio data plane (toy, first cut).
//!
//! Contract (borge/songs.borg section rendering): a MANIFEST JSON on
//! stdin, a 16-bit mono PCM WAV on stdout. The engine composes nothing,
//! analyzes nothing, knows no keys — it interprets the manifest's
//! performance events (spelled tones, integer ticks, continuous
//! dynamics) into samples. Nopales owns the control plane.
//!
//! This is deliberately the smallest thing that closes the loop
//! manifest -> bytes -> media_blob -> /music. Real patches, bakes and
//! the live/WASM hosts come later.
const std = @import("std");

const sample_rate: u32 = 22050;
const ticks_per_beat: f64 = 12.0; // the IR's time grid (borge/songs.borg)

/// One performed note we collected from the manifest.
const Note = struct {
    freq: f64,
    start_tick: f64,
    dur_tick: f64,
    dyn: f64,
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
            const chord_obj = event.object.get("chord") orelse continue;
            _ = chord_obj;
            const start = getNumber(event.object.get("start")) orelse 0.0;
            const dur = getNumber(event.object.get("dur")) orelse ticks_per_beat;
            const dyn = getNumber(event.object.get("dyn")) orelse 0.8;
            const tones = event.object.get("tones") orelse continue;
            if (tones != .array) continue;
            for (tones.array.items) |tone| {
                if (tone != .string) continue;
                const pc = notePitchClass(tone.string) orelse continue;
                // Octave choice: mid register, C4..B4 (midi 60..71).
                const midi: i32 = 60 + pc;
                const freq = 440.0 * std.math.pow(f64, 2.0, (@as(f64, @floatFromInt(midi)) - 69.0) / 12.0);
                try notes.append(gpa, .{
                    .freq = freq,
                    .start_tick = start,
                    .dur_tick = dur,
                    .dyn = dyn,
                });
            }
        }
    }

    if (notes.items.len == 0) return EngineError.NoEvents;

    // seconds per tick from tempo (BPM); 12 ticks per beat.
    const sec_per_tick = 60.0 / tempo / ticks_per_beat;

    var max_tick: f64 = 0.0;
    for (notes.items) |n| {
        const end = n.start_tick + n.dur_tick;
        if (end > max_tick) max_tick = end;
    }
    const total_samples: usize = @intFromFloat(max_tick * sec_per_tick * @as(f64, @floatFromInt(sample_rate)));
    if (total_samples == 0) return EngineError.NoEvents;

    const pcm = try gpa.alloc(i16, total_samples);
    defer gpa.free(pcm);
    @memset(pcm, 0);

    const attack = 0.01; // seconds
    const release = 0.05;
    for (pcm, 0..) |*sample, idx| {
        const t = @as(f64, @floatFromInt(idx)) / @as(f64, @floatFromInt(sample_rate));
        const tick = t / sec_per_tick;
        var mix: f64 = 0.0;
        for (notes.items) |n| {
            if (tick < n.start_tick or tick >= n.start_tick + n.dur_tick) continue;
            const local = (tick - n.start_tick) * sec_per_tick; // seconds into the note
            const note_secs = n.dur_tick * sec_per_tick;
            var env: f64 = 1.0;
            if (local < attack) env = local / attack;
            const from_end = note_secs - local;
            if (from_end < release) env = @min(env, from_end / release);
            if (env < 0.0) env = 0.0;
            mix += std.math.sin(2.0 * std.math.pi * n.freq * t) * env * n.dyn;
        }
        // Headroom: divide by a nominal 3-tone chord and clamp.
        const scaled = mix / 3.0;
        const clamped = std.math.clamp(scaled, -1.0, 1.0);
        sample.* = @intFromFloat(clamped * 32767.0);
    }

    return encodeWav(gpa, pcm);
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

    try appendBytes(&buf, gpa, "RIFF");
    try appendU32(&buf, gpa, riff_size);
    try appendBytes(&buf, gpa, "WAVE");
    try appendBytes(&buf, gpa, "fmt ");
    try appendU32(&buf, gpa, 16); // PCM chunk size
    try appendU16(&buf, gpa, 1); // audio format: PCM
    try appendU16(&buf, gpa, 1); // channels: mono
    try appendU32(&buf, gpa, sample_rate);
    try appendU32(&buf, gpa, byte_rate);
    try appendU16(&buf, gpa, 2); // block align
    try appendU16(&buf, gpa, 16); // bits per sample
    try appendBytes(&buf, gpa, "data");
    try appendU32(&buf, gpa, data_bytes);
    for (pcm) |s| {
        const u: u16 = @bitCast(s);
        try appendU16(&buf, gpa, u);
    }
    return buf.toOwnedSlice(gpa);
}

fn appendBytes(buf: *std.ArrayList(u8), gpa: std.mem.Allocator, bytes: []const u8) !void {
    try buf.appendSlice(gpa, bytes);
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
