const std = @import("std");
const instructions = @import("instructions.zig");
const vm = @import("vm.zig");

const Mode = enum {
    interp,
    jit,
    disas,
};

pub fn main() !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    std.debug.assert(args.skip());
    const rom_path = args.next() orelse return error.NotEnoughArgs;
    const mode = if (args.next()) |mode|
        std.meta.stringToEnum(Mode, mode) orelse @panic("Invalid mode")
    else
        .interp;

    var rom_buf: [4096]u8 = undefined;
    const rom = try std.fs.cwd().readFile(rom_path, &rom_buf);

    if (mode == .disas) {
        try disassemble(rom);
        return;
    }

    try std.io.getStdOut().writeAll(term_setup);
    defer teardown();

    const act: std.os.Sigaction = .{
        .handler = .{ .handler = catchSignal },
        .mask = std.os.empty_sigset,
        .flags = 0,
    };
    try std.os.sigaction(std.os.SIG.INT, &act, null);
    try std.os.sigaction(std.os.SIG.HUP, &act, null);
    try std.os.sigaction(std.os.SIG.TERM, &act, null);
    try std.os.sigaction(std.os.SIG.TRAP, &act, null);
    try std.os.sigaction(std.os.SIG.SEGV, &act, null);

    try vm.exec(.{ .jit = mode == .jit }, rom);
}

fn disassemble(prog: []const u8) !void {
    var it: instructions.InsnIter = .{ .prog = prog };
    while (try it.next()) |match| {
        std.debug.print("{d: >4}: {s}", .{ it.offset - 2 + vm.Env.memory_base, @tagName(match) });
        switch (match) {
            inline else => |operands| inline for (operands) |operand| {
                switch (@TypeOf(operand)) {
                    vm.Register => std.debug.print(" {s}", .{@tagName(operand)}),
                    else => std.debug.print(" {}", .{operand}),
                }
            },
        }
        std.debug.print("\n", .{});
    }
}

fn catchSignal(sig: c_int) callconv(.C) void {
    teardown();
    std.os.exit(@intCast(128 + sig));
}
fn teardown() void {
    std.io.getStdOut().writeAll(term_teardown) catch {};
}

pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    teardown();
    std.builtin.default_panic(msg, error_return_trace, ret_addr orelse @returnAddress());
}

const term_setup =
    "\x1b[?1049h" ++ // Switch to alt buffer
    "\x1b[?25l" ++ // Hide cursor
    "\x1b[H" ++ // Move to top left
    "\x1b[2J" ++ // Clear screen
    "\x1b[3J" ++ // Clear scrollback
    "\x1b[97;40m" ++ // Set white fg, black bg
    (" " ** vm.screen_width ++ "\n") ** (vm.screen_height / 2); // Apply bg to entire screen

const term_teardown =
    "\x1b[?25h" ++ // Show cursor
    "\x1b[?1049l"; // Switch to main buffer

pub const std_options = struct {
    pub fn logFn(
        comptime level: std.log.Level,
        comptime scope: @Type(.EnumLiteral),
        comptime format: []const u8,
        args: anytype,
    ) void {
        // Switch to main buffer
        std.debug.print("\x1b[?1049l", .{});
        // Print log message
        std.log.defaultLog(level, scope, format, args);
        // Switch back to alt buffer
        std.debug.print("\x1b[?1049l", .{});
    }
};
