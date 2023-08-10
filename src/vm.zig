//! CHIP-8 interpreter + VM
const std = @import("std");
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
    std.debug.assert(!config.jit);

    var seed: u64 = undefined;
    try std.os.getrandom(std.mem.asBytes(&seed));
    var rng = std.rand.DefaultPrng.init(0);

    var env = Env{
        .program_map = undefined,
        .program_size = @intCast(prog.len),

        .rng = rng.random(),
        .timer = try std.time.Timer.start(),
        .out = std.io.getStdOut().writer(),
    };
    @memcpy(env.memory[Env.memory_base..][0..prog.len], prog);

    var ctx = InterpreterContext{
        .env = &env,
        .config = &config,
    };

    while (interpret(ctx)) |_| {
        //
    } else |err| switch (err) {
        error.Exit => {},
        else => |e| return e,
    }
}

const Trap = error{
    Exit,
    InvalidInstruction,
    InvalidAddress,
    StackOverflow,
    CodeSegmentModified, // Only emitted if config.self_modification is disabled
    JumpToDataSegment, // Only emitted if config.self_modification is disabled
} || std.fs.File.WriteError;

fn interpret(ctx: InterpreterContext) Trap!void {
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
    pub inline fn jumpRel(ctx: InterpreterContext, off: u12) !void {
        const addr = std.math.add(u12, ctx.env.pc, off) catch {
            return error.InvalidAddress;
        };
        try ctx.jump(addr);
    }
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
const Env = struct {
    /// Memory buffer
    memory: [4096]u8 = interp_image ++ .{0} ** (4096 - interp_image.len),

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

    /// Instruction address map (only used by JIT)
    program_map: [4096 - memory_base]*anyopaque,
    /// Program size, used to trap self-modifying code
    program_size: u12,

    st: Timer = .{},
    dt: Timer = .{},

    screen: [screen_height][screen_width]u1 = std.mem.zeroes([screen_height][screen_width]u1),

    // System resources
    rng: std.rand.Random,
    timer: std.time.Timer,
    out: std.fs.File.Writer,

    pub const memory_base = 0x200;

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
        @memcpy(env.memory[begin..end], values);
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
        @memcpy(env.memory[begin..end], values);
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

        try env.out.writeAll(text.slice());
    }
};

pub const screen_width = 64;
pub const screen_height = 32;

const Timer = struct {
    t: u64 = 0,
    d: u8 = 0,
};
