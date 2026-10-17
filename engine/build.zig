const std = @import("std");

// engine/ — the audio + feature data planes (borge/songs.borg section
// rendering).
//
// Nopales owns the control plane (score -> performance -> manifest); these
// binaries are the data plane:
//   pp-render    manifest.json -> 16-bit mono PCM WAV (argv paths)
//   pp-features  wav           -> features.json (60 fps bands/envelopes/onsets)
// One source, three hosts someday: a native offline renderer now, a native
// live host later, and a WASM AudioWorklet for the browser. cstdio file I/O
// on purpose: stable across the 0.15 std.Io churn on every host.

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const render_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const exe = b.addExecutable(.{
        .name = "pp-render",
        .root_module = render_mod,
    });
    b.installArtifact(exe);

    const features_mod = b.createModule(.{
        .root_source_file = b.path("src/features.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    const features = b.addExecutable(.{
        .name = "pp-features",
        .root_module = features_mod,
    });
    b.installArtifact(features);

    const tests = b.addTest(.{ .root_module = render_mod });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run engine unit tests");
    test_step.dependOn(&run_tests.step);
}
