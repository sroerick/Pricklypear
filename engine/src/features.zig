//! pp-features — the aviz data plane's feature track. argv:
//! "pp-features <in.wav> <out.features.json>".
//!
//! Deterministic post-hoc analysis of a rendered WAV, so the player NEVER
//! analyzes audio at runtime (borge/songs.borg projection discipline):
//! - 60 fps frames, Hann-windowed 1024-point FFT
//! - band energies: bass 20-150 Hz, mid 150-2000, treble 2000-8000
//! - fast-attack / slow-decay envelopes per band (the punch lives here)
//! - spectral-flux onsets + a pulse envelope decaying from each onset
//!
//! Pure function of the WAV bytes: same WAV, same JSON, every run, so the
//! player stays deterministic-in-time and video export stays frame-exact.

const std = @import("std");
const io_util = @import("io_util.zig");

const fft_size: usize = 1024;
const fps: u32 = 60;

pub fn main() !void {
    const gpa = std.heap.c_allocator;

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);
    if (args.len != 3) {
        std.debug.print("usage: pp-features <in.wav> <out.features.json>\n", .{});
        std.process.exit(2);
    }

    const wav = try io_util.readFileAll(gpa, args[1]);
    defer gpa.free(wav);

    const parsed = parseWav(wav) orelse {
        std.debug.print("pp-features: not a PCM WAV\n", .{});
        std.process.exit(1);
    };
    const samples = parsed.samples;
    const sample_rate = parsed.sample_rate;
    if (samples.len <= fft_size) {
        std.debug.print("pp-features: wav too short\n", .{});
        std.process.exit(1);
    }

    const hop: usize = @max(1, sample_rate / fps);
    const frames: usize = (samples.len - fft_size) / hop + 1;
    const bins = fft_size / 2;

    var window: [fft_size]f32 = undefined;
    for (0..fft_size) |i| {
        const x = @as(f32, @floatFromInt(i)) / @as(f32, @floatFromInt(fft_size - 1));
        window[i] = 0.5 - 0.5 * @cos(2.0 * std.math.pi * x);
    }

    const bin_hz = @as(f32, @floatFromInt(sample_rate)) / @as(f32, @floatFromInt(fft_size));
    const bass_lo: usize = @intFromFloat(@max(1.0, 20.0 / bin_hz));
    const bass_hi: usize = @intFromFloat(@min(@as(f32, @floatFromInt(bins - 1)), 150.0 / bin_hz));
    const mid_hi: usize = @intFromFloat(@min(@as(f32, @floatFromInt(bins - 1)), 2000.0 / bin_hz));
    const treble_hi: usize = @intFromFloat(@min(@as(f32, @floatFromInt(bins - 1)), 8000.0 / bin_hz));

    const bass_raw = try gpa.alloc(f32, frames);
    defer gpa.free(bass_raw);
    const mid_raw = try gpa.alloc(f32, frames);
    defer gpa.free(mid_raw);
    const treble_raw = try gpa.alloc(f32, frames);
    defer gpa.free(treble_raw);
    const flux = try gpa.alloc(f32, frames);
    defer gpa.free(flux);

    var re: [fft_size]f32 = undefined;
    var im: [fft_size]f32 = undefined;
    var prev_mag: [bins]f32 = @splat(0.0);

    for (0..frames) |fi| {
        const start = fi * hop;
        for (0..fft_size) |k| {
            re[k] = @as(f32, @floatFromInt(samples[start + k])) / 32768.0 * window[k];
            im[k] = 0.0;
        }
        fftInPlace(&re, &im);

        var bass: f32 = 0.0;
        var mid: f32 = 0.0;
        var treble: f32 = 0.0;
        var fl: f32 = 0.0;
        for (1..bins) |k| {
            const mag = @sqrt(re[k] * re[k] + im[k] * im[k]);
            if (k >= bass_lo and k <= bass_hi) bass += mag * mag;
            if (k > bass_hi and k <= mid_hi) mid += mag * mag;
            if (k > mid_hi and k <= treble_hi) treble += mag * mag;
            const d = mag - prev_mag[k];
            if (d > 0.0) fl += d;
            prev_mag[k] = mag;
        }
        bass_raw[fi] = @sqrt(bass);
        mid_raw[fi] = @sqrt(mid);
        treble_raw[fi] = @sqrt(treble);
        flux[fi] = fl;
    }

    const bass_env = try envelope(gpa, bass_raw);
    defer gpa.free(bass_env);
    const mid_env = try envelope(gpa, mid_raw);
    defer gpa.free(mid_env);
    const treble_env = try envelope(gpa, treble_raw);
    defer gpa.free(treble_env);
    normalizeInPlace(bass_env);
    normalizeInPlace(mid_env);
    normalizeInPlace(treble_env);

    // Onsets: spectral flux above an adaptive local threshold, on a rising
    // edge, with a small refractory window.
    var onsets = std.ArrayList(f64).empty;
    defer onsets.deinit(gpa);
    var last_onset_frame: isize = -100;
    for (0..frames) |i| {
        var sum: f32 = 0.0;
        var count: f32 = 0.0;
        var w: isize = -20;
        while (w <= 10) : (w += 1) {
            const j = @as(isize, @intCast(i)) + w;
            if (j >= 0 and j < @as(isize, @intCast(frames))) {
                sum += flux[@intCast(j)];
                count += 1.0;
            }
        }
        const thr = (sum / count) * 1.6 + 1e-4;
        const rising = i == 0 or flux[i] >= flux[i - 1];
        if (flux[i] > thr and rising and @as(isize, @intCast(i)) - last_onset_frame > 6) {
            try onsets.append(gpa, @as(f64, @floatFromInt(i)) / @as(f64, @floatFromInt(fps)));
            last_onset_frame = @intCast(i);
        }
    }

    // Pulse: 1.0 at each onset, exponential decay between.
    const pulse = try gpa.alloc(f32, frames);
    defer gpa.free(pulse);
    var p: f32 = 0.0;
    var oi: usize = 0;
    for (0..frames) |i| {
        const t = @as(f64, @floatFromInt(i)) / @as(f64, @floatFromInt(fps));
        if (oi < onsets.items.len and t >= onsets.items[oi]) {
            p = 1.0;
            oi += 1;
        }
        p *= 0.90;
        pulse[i] = p;
    }

    var out = std.ArrayList(u8).empty;
    defer out.deinit(gpa);
    try appendFmt(gpa, &out, "{{\"sample_rate\":{d},\"fps\":{d},\"frames\":{d},\"duration_sec\":{d:.3},", .{
        sample_rate,
        fps,
        frames,
        @as(f64, @floatFromInt(samples.len)) / @as(f64, @floatFromInt(sample_rate)),
    });
    try appendFmt(gpa, &out, "\"onsets\":[", .{});
    for (onsets.items, 0..) |t, i| {
        if (i > 0) try out.appendSlice(gpa, ",");
        try appendFmt(gpa, &out, "{d:.3}", .{t});
    }
    try out.appendSlice(gpa, "],\"bands\":{\"bass\":[");
    try appendArray(gpa, &out, bass_env);
    try out.appendSlice(gpa, "],\"mid\":[");
    try appendArray(gpa, &out, mid_env);
    try out.appendSlice(gpa, "],\"treble\":[");
    try appendArray(gpa, &out, treble_env);
    try out.appendSlice(gpa, "],\"pulse\":[");
    try appendArray(gpa, &out, pulse);
    try out.appendSlice(gpa, "]}}\n");

    try io_util.writeFileAll(args[2], out.items);
}

fn envelope(gpa: std.mem.Allocator, raw: []const f32) ![]f32 {
    const out = try gpa.alloc(f32, raw.len);
    var env: f32 = 0.0;
    for (raw, 0..) |v, i| {
        const coef: f32 = if (v > env) 0.55 else 0.10;
        env += (v - env) * coef;
        out[i] = env;
    }
    return out;
}

fn normalizeInPlace(xs: []f32) void {
    var m: f32 = 0.0;
    for (xs) |v| {
        if (v > m) m = v;
    }
    if (m > 0.0) {
        for (xs) |*v| v.* /= m;
    }
}

fn fftInPlace(re: []f32, im: []f32) void {
    const n = re.len;
    var j: usize = 0;
    var i: usize = 1;
    while (i < n) : (i += 1) {
        var bit = n >> 1;
        while (j & bit != 0) {
            j ^= bit;
            bit >>= 1;
        }
        j |= bit;
        if (i < j) {
            std.mem.swap(f32, &re[i], &re[j]);
            std.mem.swap(f32, &im[i], &im[j]);
        }
    }
    var len: usize = 2;
    while (len <= n) : (len <<= 1) {
        const ang = -2.0 * std.math.pi / @as(f32, @floatFromInt(len));
        const wr = @cos(ang);
        const wi = @sin(ang);
        var start: usize = 0;
        while (start < n) : (start += len) {
            var cr: f32 = 1.0;
            var ci: f32 = 0.0;
            var k: usize = 0;
            while (k < len / 2) : (k += 1) {
                const a = start + k;
                const b = start + k + len / 2;
                const vr = re[b] * cr - im[b] * ci;
                const vi = re[b] * ci + im[b] * cr;
                const ur = re[a];
                const ui = im[a];
                re[a] = ur + vr;
                im[a] = ui + vi;
                re[b] = ur - vr;
                im[b] = ui - vi;
                const nr = cr * wr - ci * wi;
                ci = cr * wi + ci * wr;
                cr = nr;
            }
        }
    }
}

const ParsedWav = struct { samples: []const i16, sample_rate: u32 };

fn parseWav(data: []const u8) ?ParsedWav {
    if (data.len < 44 or !std.mem.eql(u8, data[0..4], "RIFF") or !std.mem.eql(u8, data[8..12], "WAVE")) return null;
    var off: usize = 12;
    var sample_rate: u32 = 22050;
    var data_off: ?usize = null;
    var data_len: usize = 0;
    while (off + 8 <= data.len) {
        const id = data[off .. off + 4];
        const sz = std.mem.readInt(u32, data[off + 4 ..][0..4], .little);
        const body = off + 8;
        if (body + sz > data.len) break;
        if (std.mem.eql(u8, id, "fmt ") and body + 16 <= data.len) {
            sample_rate = std.mem.readInt(u32, data[body + 4 ..][0..4], .little);
        } else if (std.mem.eql(u8, id, "data")) {
            data_off = body;
            data_len = sz;
        }
        off = body + sz + (sz & 1);
    }
    const doff = data_off orelse return null;
    if (data_len < 2 or doff + data_len > data.len) return null;
    const bytes = data[doff .. doff + data_len];
    const samples = std.mem.bytesAsSlice(i16, @as([]align(2) const u8, @alignCast(bytes)));
    return .{ .samples = samples, .sample_rate = sample_rate };
}

fn appendFmt(gpa: std.mem.Allocator, list: *std.ArrayList(u8), comptime fmt: []const u8, args: anytype) !void {
    var buf: [256]u8 = undefined;
    const s = try std.fmt.bufPrint(&buf, fmt, args);
    try list.appendSlice(gpa, s);
}

fn appendArray(gpa: std.mem.Allocator, list: *std.ArrayList(u8), xs: []const f32) !void {
    for (xs, 0..) |v, i| {
        if (i > 0) try list.appendSlice(gpa, ",");
        try appendFmt(gpa, list, "{d:.3}", .{v});
    }
}
