const std = @import("std");
const vm = @import("vm.zig");

var saved_termios: std.os.termios = undefined;

pub fn main() !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    std.debug.assert(args.skip());
    const rom_path = args.next() orelse return error.NotEnoughArgs;

    var rom_buf: [4096]u8 = undefined;
    const rom = try std.fs.cwd().readFile(rom_path, &rom_buf);

    saved_termios = try std.os.tcgetattr(std.os.STDIN_FILENO);
    var new_termios = saved_termios;
    // Disable echo and canonical mode
    new_termios.lflag &= ~(std.os.system.ECHO | std.os.system.ICANON);
    try std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, new_termios);
    defer teardown();

    try std.io.getStdOut().writeAll(term_setup);

    const act: std.os.Sigaction = .{
        .handler = .{ .handler = catchSignal },
        .mask = std.os.empty_sigset,
        .flags = 0,
    };
    try std.os.sigaction(std.os.SIG.INT, &act, null);
    try std.os.sigaction(std.os.SIG.HUP, &act, null);
    try std.os.sigaction(std.os.SIG.TERM, &act, null);

    try vm.exec(.{}, rom);
}

fn catchSignal(_: c_int) callconv(.C) void {
    teardown();
    std.os.exit(0);
}
fn teardown() void {
    std.os.tcsetattr(std.os.STDIN_FILENO, std.os.TCSA.NOW, saved_termios) catch {};
    std.io.getStdOut().writeAll(term_teardown) catch {};
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
