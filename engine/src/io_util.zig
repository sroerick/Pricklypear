//! Small libc-stdio file helpers shared by the engine binaries. Deliberate:
//! cstdio is stable across Zig std churn (the 0.15 line reshaped std.Io
//! repeatedly) and the engine's I/O surface is trivial: whole-file in,
//! whole-file out.
const std = @import("std");

pub const c = @cImport({
    @cInclude("stdio.h");
});

pub fn readFileAll(gpa: std.mem.Allocator, path: []const u8) ![]u8 {
    var path_buf: [512]u8 = undefined;
    if (path.len >= path_buf.len) return error.PathTooLong;
    @memcpy(path_buf[0..path.len], path);
    path_buf[path.len] = 0;
    const f = c.fopen(&path_buf, "rb") orelse return error.CannotOpenInput;
    defer _ = c.fclose(f);
    _ = c.fseek(f, 0, c.SEEK_END);
    const size: usize = @intCast(@max(0, c.ftell(f)));
    _ = c.fseek(f, 0, c.SEEK_SET);
    const buf = try gpa.alloc(u8, size);
    errdefer gpa.free(buf);
    if (c.fread(buf.ptr, 1, size, f) != size) return error.ShortRead;
    return buf;
}

pub fn writeFileAll(path: []const u8, bytes: []const u8) !void {
    var path_buf: [512]u8 = undefined;
    if (path.len >= path_buf.len) return error.PathTooLong;
    @memcpy(path_buf[0..path.len], path);
    path_buf[path.len] = 0;
    const f = c.fopen(&path_buf, "wb") orelse return error.CannotOpenOutput;
    defer _ = c.fclose(f);
    if (bytes.len > 0 and c.fwrite(bytes.ptr, 1, bytes.len, f) != bytes.len) return error.ShortWrite;
}
