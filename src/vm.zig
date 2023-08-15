//! CHIP-8 interpreter + VM
const std = @import("std");
const copy_patch = @import("copy_patch.zig");
const instructions = @import("instructions.zig");

pub const Register = enum(u4) {
    v0,
    v1,
    v2,
    v3,
    v4,
    v5,
    v6,
    v7,
    v8,
    v9,
    va,
    vb,
    vc,
    vd,
    ve,
    vf,
};

pub fn exec(config: Config, prog: []const u8) !void {
    var seed: u64 = undefined;
    try std.os.getrandom(std.mem.asBytes(&seed));
    var rng = std.rand.DefaultPrng.init(0);

    var env = Env{
        .program_size = @intCast(prog.len),

        .rng = rng.random(),
        .timer = try std.time.Timer.start(),
        .out = std.io.getStdOut().writer(),
    };
    @memcpy(env.memory[Env.memory_base..][0..prog.len], prog);

    if (config.jit) {
        var ctx = CopyPatchContext{
            .env = env,
            .config = &config,
        };
        // TODO: free compiled blocks
        _ = try copy_patch.compileBlock(&ctx, Env.memory_base);
        const entry = ctx.program_map[0] orelse {
            return error.NoEntryPoint; // How even?
        };
        entry(&ctx);
        try ctx.trap.toErrorUnion();
    } else {
        try execInterp(.{
            .env = &env,
            .config = &config,
        });
    }
}

fn execInterp(ctx: InterpreterContext) !void {
    while (interpret(ctx)) |_| {
        //
    } else |err| switch (err) {
        error.Exit => {},
        else => |e| return e,
    }
}

pub const CompileError = error{
    InvalidInstruction,
} || std.os.MMapError || std.os.MProtectError;

pub const Trap = error{
    Exit,
    InvalidAddress,
    UnsupportedInstruction,
    CompileError,
    StackOverflow,
    CodeSegmentModified, // Only emitted if config.self_modification is disabled
    JumpToDataSegment, // Only emitted if config.self_modification is disabled
    InputOutput,
};

fn interpret(ctx: InterpreterContext) (Trap || CompileError)!void {
    const opcode_bytes = try ctx.getMem(ctx.env.pc, 2);
    ctx.env.pc += 2;

    const opcode = std.mem.readIntSliceBig(u16, opcode_bytes);
    const match = instructions.decode(opcode) catch |e| {
        std.log.err("Invalid instruction: {x:0>4}", .{opcode});
        return e;
    };
    switch (match) {
        inline else => |operands, name| {
            const func = @field(instructions.insns, @tagName(name));
            try @call(.auto, func, .{ctx} ++ operands);
        },
    }

    // Let's not murder the CPU
    // This is much more effective than std.Thread.yield, for whatever reason
    std.time.sleep(1);
}

pub const Config = struct {
    /// Allow programs to modify themselves
    /// Incompatible with jit
    self_modification: bool = false,

    /// Enable JIT compilation
    jit: bool = false,
};

pub const InterpreterContext = struct {
    env: *Env,
    config: *const Config,

    pub inline fn getMem(ctx: InterpreterContext, addr: u16, count: u4) ![]const u8 {
        return ctx.env.getMem(addr, count);
    }
    pub inline fn setMem(ctx: InterpreterContext, addr: u16, values: []const u8) !void {
        if (ctx.config.self_modification) {
            return ctx.env.setMem(addr, values);
        } else {
            return ctx.env.setMemNoSelfModify(addr, values);
        }
    }

    pub inline fn ret(ctx: InterpreterContext) !void {
        if (ctx.env.sp == 0) {
            return error.Exit;
        }
        ctx.env.pc = ctx.env.stack[ctx.env.sp];
        ctx.env.sp -= 1;
    }
    pub inline fn call(ctx: InterpreterContext, addr: u12) !void {
        if (ctx.env.sp >= ctx.env.stack.len - 1) {
            return error.StackOverflow;
        }
        ctx.env.sp += 1;
        ctx.env.stack[ctx.env.sp] = ctx.env.pc;
        try ctx.jump(addr);
    }

    pub inline fn jump(ctx: InterpreterContext, addr: u12) !void {
        if (!ctx.config.self_modification and addr >= Env.memory_base + ctx.env.program_size) {
            return error.JumpToDataSegment;
        }
        ctx.env.pc = addr;
    }
    pub inline fn skip(ctx: InterpreterContext) !void {
        const addr = std.math.add(u12, ctx.env.pc, 2) catch {
            return error.InvalidAddress;
        };
        try ctx.jump(addr);
    }

    pub inline fn blitScreen(ctx: InterpreterContext) !void {
        return ctx.env.blitScreen();
    }

    pub inline fn readTimer(ctx: InterpreterContext) u64 {
        return ctx.env.timer.read();
    }
};

pub const CopyPatchContext = struct {
    env: Env,
    /// Trap value - used to determine whether we're unwinding, and why
    trap: copy_patch.Trap = .success,
    /// Instruction address map
    program_map: [program_map_len]?*const fn (*CopyPatchContext) callconv(.C) void = .{null} ** program_map_len,

    config: *const Config,

    pub const program_map_len = Env.memory_len - Env.memory_base;

    pub inline fn getMem(ctx: *CopyPatchContext, addr: u16, count: u4) ![]const u8 {
        return ctx.env.getMem(addr, count);
    }
    pub inline fn setMem(ctx: *CopyPatchContext, addr: u16, values: []const u8) !void {
        const begin = try Env.addrOffset(addr);
        if (begin < ctx.env.program_size) {
            linked(.log_code_seg_modified, .{ begin, ctx.env.program_size });
            return error.CodeSegmentModified;
        }
        return ctx.env.setMem(addr, values);
    }

    pub inline fn ret(_: InterpreterContext) !void {
        @compileError("ret should be implemented in copy_patch_stub.zig");
    }
    pub inline fn call(ctx: *CopyPatchContext, addr: u12) !void {
        // TODO: stack overflow checking
        const compiled_addr = try ctx.compiledAddr(addr);
        compiled_addr(ctx);

        // Are we unwinding?
        if (ctx.trap != .success) {
            return error.Unwind;
        }
    }

    pub inline fn jump(ctx: *CopyPatchContext, addr: u12) !void {
        if (addr < Env.memory_base or addr >= Env.memory_len) {
            linked(.log_invalid_jump, .{addr});
            return error.InvalidAddress;
        }
        const compiled_addr = try ctx.compiledAddr(addr);
        @call(.always_tail, compiled_addr, .{ctx});
    }
    pub inline fn skip(ctx: *CopyPatchContext) !void {
        @call(.always_tail, next_next_instruction, .{ctx});
    }
    extern fn next_next_instruction(*CopyPatchContext) void;

    pub inline fn blitScreen(ctx: *CopyPatchContext) !void {
        const err = linked(.blitScreen, .{&ctx.env});
        return err.toErrorUnion();
    }

    pub inline fn readTimer(ctx: *CopyPatchContext) u64 {
        return linked(.readTimer, .{&ctx.env});
    }

    inline fn compiledAddr(ctx: *CopyPatchContext, addr: u12) !copy_patch.InsnFn {
        const offset = addr - Env.memory_base;
        if (ctx.program_map[offset]) |compiled| {
            return compiled;
        } else {
            const err = linked(.compileBlock, .{ ctx, addr });
            try err.toErrorUnion();
            return ctx.program_map[offset].?;
        }
    }

    inline fn linked(
        comptime func: std.meta.DeclEnum(linkable),
        args: anytype,
    ) @typeInfo(@TypeOf(@field(linkable, @tagName(func)))).Fn.return_type.? {
        const Fn = @TypeOf(@field(linkable, @tagName(func)));
        if (@typeInfo(Fn).Fn.calling_convention != .C) {
            @compileError("Expected function with C calling convention, got " ++ @typeName(Fn));
        }
        const f: *const Fn = @ptrCast(extern_link_table[@intFromEnum(func)]);
        return @call(.auto, f, args);
    }

    const linkable = struct {
        pub fn log_code_seg_modified(begin: u16, size: u16) callconv(.C) void {
            std.log.err("Attempted write to address 0x{x:0>4}, within code segment (ends 0x{x:0>4})", .{ begin, size });
        }

        pub fn log_invalid_jump(addr: u16) callconv(.C) void {
            std.log.err("Jump to invalid address 0x{x}", .{addr});
        }

        pub fn blitScreen(env: *Env) callconv(.C) copy_patch.Trap {
            return copy_patch.Trap.fromErrorUnion(env.blitScreen());
        }
        pub fn readTimer(env: *Env) callconv(.C) u64 {
            return env.timer.read();
        }

        pub fn compileBlock(ctx: *CopyPatchContext, addr: u16) callconv(.C) copy_patch.Trap {
            // TODO: free compiled blocks
            _ = copy_patch.compileBlock(ctx, @intCast(addr)) catch |err| {
                std.log.err("Compile error in block at {}: {s}", .{ addr, @errorName(err) });
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpStackTrace(trace.*);
                }
                return copy_patch.Trap.CompileError;
            };
            return .success;
        }
    };
    extern const extern_link_table: LinkTable;
    pub const LinkTable = [@typeInfo(linkable).Struct.decls.len]*const anyopaque;
    pub const link_table = a: {
        var ptrs: LinkTable = undefined;
        for (std.enums.values(std.meta.DeclEnum(linkable))) |func| {
            ptrs[@intFromEnum(func)] = @ptrCast(&@field(linkable, @tagName(func)));
        }
        break :a ptrs;
    };
};

// boot image for the "interpreter" region of memory
const interp_image = [_]u8{
    // 0x0000 - Digit font
    0xf0, 0x90, 0x90, 0x90, 0xf0,
    0x20, 0x60, 0x20, 0x20, 0x70,
    0xf0, 0x10, 0xf0, 0x80, 0xf0,
    0xf0, 0x10, 0xf0, 0x10, 0xf0,
    0x90, 0x90, 0xf0, 0x10, 0x10,
    0xf0, 0x80, 0xf0, 0x10, 0xf0,
    0xf0, 0x80, 0xf0, 0x90, 0xf0,
    0xf0, 0x10, 0x20, 0x40, 0x40,
    0xf0, 0x90, 0xf0, 0x90, 0xf0,
    0xf0, 0x90, 0xf0, 0x10, 0xf0,
    0xf0, 0x90, 0xf0, 0x90, 0x90,
    0xe0, 0x90, 0xe0, 0x90, 0xe0,
    0xf0, 0x80, 0x80, 0x80, 0xf0,
    0xe0, 0x90, 0x90, 0x90, 0xe0,
    0xf0, 0x80, 0xf0, 0x80, 0xf0,
    0xf0, 0x80, 0xf0, 0x80, 0x80,
};

/// The environment contains all runtime data used by a CHIP-8 program
pub const Env = struct {
    /// Memory buffer
    memory: [memory_len]u8 = interp_image ++ .{0} ** (memory_len - interp_image.len),

    /// 8-bit general purpose registers
    regs: [16]u8 = .{0} ** 16,
    /// 16-bit address register (I)
    addr: u16 = 0,

    /// Stack of up to 16 values; top indicated by sp
    stack: [16]u12 = undefined,
    /// Stack pointer
    sp: u5 = 0,

    /// Program counter
    pc: u12 = memory_base,

    /// Program size, used to trap self-modifying code
    program_size: u12,

    st: Timer = .{},
    dt: Timer = .{},

    screen: [screen_height][screen_width]u1 = std.mem.zeroes([screen_height][screen_width]u1),

    // System resources
    rng: std.rand.Random,
    timer: std.time.Timer,
    out: std.fs.File.Writer,

    pub const memory_base = 512;
    pub const memory_len = 4096;

    inline fn addrOffset(addr: u16) error{InvalidAddress}!u12 {
        return std.math.cast(u12, addr) orelse error.InvalidAddress;
    }

    pub inline fn getMem(env: *const Env, addr: u16, count: u4) ![]const u8 {
        const begin = try addrOffset(addr);
        const end = try addrOffset(addr + count);
        return env.memory[begin..end];
    }
    pub inline fn setMem(env: *Env, addr: u16, values: []const u8) !void {
        const begin = try addrOffset(addr);
        const end = try addrOffset(addr + @as(u4, @intCast(values.len)));
        for (env.memory[begin..end], values) |*d, s| {
            d.* = s;
        }
    }
    pub inline fn setMemNoSelfModify(env: *Env, addr: u16, values: []const u8) !void {
        const begin = try addrOffset(addr);
        const end = try addrOffset(addr + @as(u4, @intCast(values.len)));
        if (begin < env.program_size) {
            std.log.err("Attempted write to address 0x{x:0>4}, within code segment (ends 0x{x:0>4})", .{
                begin, env.program_size,
            });
            return error.CodeSegmentModified;
        }
        for (env.memory[begin..end], values) |*d, s| {
            d.* = s;
        }
    }

    pub inline fn getReg(env: *Env, r: Register) u8 {
        return env.regs[@intFromEnum(r)];
    }
    pub inline fn setReg(env: *Env, r: Register, value: u8) void {
        env.regs[@intFromEnum(r)] = value;
    }
    pub inline fn setFlag(env: *Env, value: bool) void {
        env.setReg(.vf, @intFromBool(value));
    }

    pub fn blitScreen(env: *const Env) !void {
        const pairs = [4][]const u8{
            " ",
            "\u{2584}",
            "\u{2580}",
            "\u{2588}",
        };
        const screen_max_size = (pairs[3].len * screen_width + 1) * screen_height / 2;

        const reset_cursor = "\x1b[H";
        var text: std.BoundedArray(u8, reset_cursor.len + screen_max_size) = .{};
        text.appendSliceAssumeCapacity(reset_cursor);

        for (0..screen_height / 2) |y| {
            for (0..screen_width) |x| {
                const px =
                    @as(u2, env.screen[y * 2][x]) << 1 |
                    env.screen[y * 2 + 1][x];
                text.appendSliceAssumeCapacity(pairs[px]);
            }
            text.appendAssumeCapacity('\n');
        }

        env.out.writeAll(text.slice()) catch |err| {
            std.log.err("IO error while blitting screen: {s}", .{@errorName(err)});
            return error.InputOutput;
        };
    }
};

pub const screen_width = 64;
pub const screen_height = 32;

const Timer = struct {
    t: u64 = 0,
    d: u8 = 0,
};
