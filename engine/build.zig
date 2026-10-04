const std = @import("std");

// engine/ — the audio data plane (borge/songs.borg section rendering).
//
// Nopales owns the control plane (score -> performance -> manifest); this
// engine is the data plane. One source, three hosts someday: a native
// offline renderer now, a native live host later, and a WASM AudioWorklet
// for the browser. This first cut is deliberately tiny: read a manifest
// JSON on stdin, write a 16-bit mono PCM WAV on stdout. It composes
// nothing and knows no keys — it interprets the manifest's performance
// events (spelled tones, ticks, dynamics) into sound.

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "pp-render",
        .root_module = mod,
    });
    b.installArtifact(exe);

    const tests = b.addTest(.{ .root_module = mod });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run engine unit tests");
    test_step.dependOn(&run_tests.step);
}
