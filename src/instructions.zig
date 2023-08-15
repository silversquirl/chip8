//! CHIP-8 instruction set
const std = @import("std");
const vm = @import("vm.zig");

pub fn decode(opcode: u16) error{InvalidInstruction}!Match {
    inline for (comptime std.meta.fieldNames(@TypeOf(table))) |name| {
        const pat = comptime pattern(@field(table, name));
        if (opcode & pat.match_mask == pat.match_cmp) {
            var args: Operands(pat) = undefined;
            comptime var i = 0;
            if (pat.x != 0) {
                args[i] = pat.extract(.x, opcode);
                i += 1;
            }
            if (pat.y != 0) {
                args[i] = pat.extract(.y, opcode);
                i += 1;
            }
            if (pat.n != 0) {
                args[i] = @intCast(pat.extract(.n, opcode));
                i += 1;
            }

            return @unionInit(Match, name, args);
        }
    }
    return error.InvalidInstruction;
}

pub const Match = a: {
    var opcodes = std.enums.values(Opcode);
    var fields: [opcodes.len]std.builtin.Type.UnionField = undefined;
    for (&fields, opcodes) |*field, op| {
        const name = @tagName(op);
        const pat = pattern(@field(table, name));
        const T = Operands(pat);

        field.* = .{
            .name = name,
            .type = T,
            .alignment = @alignOf(T),
        };
    }

    break :a @Type(.{ .Union = .{
        .layout = .Auto,
        .tag_type = Opcode,
        .fields = &fields,
        .decls = &.{},
    } });
};

pub const InsnIter = struct {
    prog: []const u8,
    offset: usize = 0,

    pub fn next(it: *InsnIter) !?Match {
        if (it.offset >= it.prog.len) return null;
        const opcode = std.mem.readIntBig(u16, it.prog[it.offset..][0..2]);
        const match = try decode(opcode);
        it.offset += 2;
        return match;
    }
};

fn pattern(desc: *const [4]u8) Pattern {
    var pat = Pattern{};
    for (desc) |c| {
        for (std.meta.fieldNames(Pattern)) |name| {
            @field(pat, name) <<= 4;
        }
        switch (c) {
            inline 'x', 'y', 'n' => |x| {
                @field(pat, &.{x}) |= 0xf;
            },
            else => {
                pat.match_mask |= 0xf;
                pat.match_cmp |= std.fmt.charToDigit(c, 16) catch unreachable;
            },
        }
    }
    return pat;
}
const Pattern = struct {
    // Matching masks
    match_mask: u16 = 0,
    match_cmp: u16 = 0,

    // Extraction masks
    x: u16 = 0,
    y: u16 = 0,
    n: u16 = 0,

    fn extract(
        pat: Pattern,
        comptime name: enum { x, y, n },
        opcode: u16,
    ) switch (name) {
        .x, .y => vm.Register,
        .n => u12,
    } {
        const mask = @field(pat, @tagName(name));
        const shift: u4 = @intCast(@ctz(mask));
        const val = (opcode & mask) >> shift;
        switch (name) {
            .x, .y => {
                const val4: u4 = @as(u4, @intCast(val));
                return @enumFromInt(val4);
            },
            .n => return @intCast(val),
        }
    }
};

fn Operands(comptime pat: Pattern) type {
    const n =
        @as(u2, @intFromBool(pat.x != 0)) +
        @intFromBool(pat.y != 0) +
        @intFromBool(pat.n != 0);
    var types: [n]type = undefined;
    var i = 0;

    if (pat.x != 0) {
        types[i] = vm.Register;
        i += 1;
    }
    if (pat.y != 0) {
        types[i] = vm.Register;
        i += 1;
    }
    if (pat.n != 0) {
        const count = @divExact(@popCount(pat.n), 4);
        types[i] = switch (count) {
            1 => u4,
            2 => u8,
            3 => u12,
            else => unreachable,
        };
        i += 1;
    }

    std.debug.assert(i == n);

    return std.meta.Tuple(&types);
}

pub const Opcode = std.meta.DeclEnum(insns);
const Table = std.enums.EnumFieldStruct(Opcode, *const [4]u8, null);
const table = Table{
    .cls = "00E0",
    .ret = "00EE",
    // .sys = "0nnn",
    .jp_imm = "1nnn",
    .call = "2nnn",
    .se_reg_imm = "3xnn",
    .sne_reg_imm = "4xnn",
    .se_reg_reg = "5xy0",
    .ld_reg_imm = "6xnn",
    .add_reg_imm = "7xnn",
    .ld_reg_reg = "8xy0",
    .@"or" = "8xy1",
    .@"and" = "8xy2",
    .xor = "8xy3",
    .add_reg_reg = "8xy4",
    .sub = "8xy5",
    .shr = "8xy6",
    .subn = "8xy7",
    .shl = "8xyE",
    .sne_reg_reg = "9xy0",
    .ld_i_imm = "Annn",
    .jp_reg0_off = "Bnnn",
    .rnd = "Cxnn",
    .drw = "Dxyn",
    .skp = "Ex9E",
    .sknp = "ExA1",
    .ld_reg_dt = "Fx07",
    .ld_reg_key = "Fx0A",
    .ld_dt_reg = "Fx15",
    .ld_st_reg = "Fx18",
    .add_i_reg = "Fx1E",
    .ld_i_sprite = "Fx29",
    .ld_mem_bcd = "Fx33",
    .ld_mem_reg = "Fx55",
    .ld_reg_mem = "Fx65",
};

const Context = std.meta.globalOption("InstructionContext", type) orelse vm.InterpreterContext;
pub const insns = struct {
    // Graphics
    pub fn cls(ctx: Context) !void {
        // Clear screen buffer
        // @memset(std.mem.asBytes(&ctx.env.screen), 0);
        for (&ctx.env.screen) |*row| {
            for (row) |*px| {
                px.* = 0;
            }
        }
        // Blit
        try ctx.blitScreen();
    }
    pub fn drw(ctx: Context, x: vm.Register, y: vm.Register, n: u4) !void {
        const xv = ctx.env.getReg(x);
        const yv = ctx.env.getReg(y);

        // Update screen buffer
        // TODO: consider moving this logic to a context function
        var collide = false;
        const sprite = try ctx.getMem(ctx.env.addr, n);
        for (sprite, 0..) |row, yoff| {
            const yc = (yv + yoff) % vm.screen_height;

            for (0..8) |i| {
                const bit: u1 = @truncate(row >> @intCast(7 - i));
                const px = &ctx.env.screen[yc][(xv + i) % vm.screen_width];
                if (px.* == bit) collide = true;
                px.* ^= bit;
            }
        }

        ctx.env.setFlag(collide);

        // Re-blit the whole screen
        // TODO: damage tracking
        try ctx.blitScreen();
    }
    pub fn ld_i_sprite(ctx: Context, x: vm.Register) !void {
        const digit = ctx.env.getReg(x) & 0xf;
        ctx.env.addr = 0x0000 + 5 * digit;
    }

    // Keyboard
    pub fn skp(_: Context, _: vm.Register) !void {
        return error.UnsupportedInstruction;
    }
    pub fn sknp(_: Context, _: vm.Register) !void {
        return error.UnsupportedInstruction;
    }
    pub fn ld_reg_key(_: Context, _: vm.Register) !void {
        return error.UnsupportedInstruction;
    }

    // Timers
    pub fn ld_reg_dt(ctx: Context, x: vm.Register) !void {
        const old_t = ctx.env.dt.t;
        const new_t = ctx.readTimer();
        const diff = new_t - old_t;
        ctx.env.dt.t = new_t + diff % 60;
        ctx.env.dt.d = @intCast(ctx.env.dt.d -| diff / 60);
        ctx.env.setReg(x, ctx.env.dt.d);
    }
    pub fn ld_dt_reg(ctx: Context, x: vm.Register) !void {
        ctx.env.dt.t = ctx.readTimer();
        ctx.env.dt.d = ctx.env.getReg(x);
    }
    pub fn ld_st_reg(ctx: Context, x: vm.Register) !void {
        ctx.env.st.t = ctx.readTimer();
        ctx.env.st.d = ctx.env.getReg(x);
    }

    // Control flow
    pub fn ret(ctx: Context) !void {
        try ctx.ret();
    }
    pub fn call(ctx: Context, addr: u12) !void {
        try ctx.call(addr);
    }

    pub fn jp_imm(ctx: Context, addr: u12) !void {
        // TODO(JIT): inline constant
        try ctx.jump(addr);
    }
    pub fn jp_reg0_off(ctx: Context, off: u12) !void {
        const addr = ctx.env.regs[0] + off;
        try ctx.jump(addr);
    }

    pub fn se_reg_imm(ctx: Context, x: vm.Register, n: u8) !void {
        if (ctx.env.getReg(x) == n) {
            try ctx.skip();
        }
    }
    pub fn se_reg_reg(ctx: Context, x: vm.Register, y: vm.Register) !void {
        if (ctx.env.getReg(x) == ctx.env.getReg(y)) {
            try ctx.skip();
        }
    }
    pub fn sne_reg_imm(ctx: Context, x: vm.Register, n: u8) !void {
        if (ctx.env.getReg(x) != n) {
            try ctx.skip();
        }
    }
    pub fn sne_reg_reg(ctx: Context, x: vm.Register, y: vm.Register) !void {
        if (ctx.env.getReg(x) != ctx.env.getReg(y)) {
            try ctx.skip();
        }
    }

    // Logistic
    pub fn ld_reg_imm(ctx: Context, x: vm.Register, n: u8) !void {
        ctx.env.setReg(x, n);
    }
    pub fn ld_reg_reg(ctx: Context, x: vm.Register, y: vm.Register) !void {
        ctx.env.setReg(x, ctx.env.getReg(y));
    }
    pub fn ld_i_imm(ctx: Context, n: u12) !void {
        ctx.env.addr = n;
    }
    pub fn ld_mem_reg(ctx: Context, x: vm.Register) !void {
        const xn = @intFromEnum(x) + 1;
        try ctx.setMem(ctx.env.addr, ctx.env.regs[0..xn]);
    }
    pub fn ld_reg_mem(ctx: Context, x: vm.Register) !void {
        const xn = @intFromEnum(x) + 1;
        const mem = try ctx.getMem(ctx.env.addr, xn);
        for (ctx.env.regs[0..xn], mem) |*d, s| d.* = s;
    }

    // Arithmetic
    pub fn add_reg_imm(ctx: Context, x: vm.Register, n: u8) !void {
        ctx.env.setReg(x, ctx.env.getReg(x) +% n);
    }
    pub fn add_i_reg(ctx: Context, x: vm.Register) !void {
        ctx.env.addr +%= ctx.env.getReg(x);
    }
    pub fn add_reg_reg(ctx: Context, x: vm.Register, y: vm.Register) !void {
        const res = @addWithOverflow(ctx.env.getReg(x), ctx.env.getReg(y));
        ctx.env.setReg(x, res[0]);
        ctx.env.setFlag(res[1] != 0);
    }

    pub fn sub(ctx: Context, x: vm.Register, y: vm.Register) !void {
        const res = @subWithOverflow(ctx.env.getReg(x), ctx.env.getReg(y));
        ctx.env.setReg(x, res[0]);
        ctx.env.setFlag(res[1] == 0);
    }
    pub fn subn(ctx: Context, x: vm.Register, y: vm.Register) !void {
        const res = @subWithOverflow(ctx.env.getReg(y), ctx.env.getReg(x));
        ctx.env.setReg(x, res[0]);
        ctx.env.setFlag(res[1] == 0);
    }

    pub fn @"or"(ctx: Context, x: vm.Register, y: vm.Register) !void {
        ctx.env.setReg(x, ctx.env.getReg(x) | ctx.env.getReg(y));
    }
    pub fn @"and"(ctx: Context, x: vm.Register, y: vm.Register) !void {
        ctx.env.setReg(x, ctx.env.getReg(x) & ctx.env.getReg(y));
    }
    pub fn xor(ctx: Context, x: vm.Register, y: vm.Register) !void {
        ctx.env.setReg(x, ctx.env.getReg(x) ^ ctx.env.getReg(y));
    }

    pub fn shr(ctx: Context, x: vm.Register, y: vm.Register) !void {
        _ = y; // Unused for some reason
        const xv = ctx.env.getReg(x);
        ctx.env.setReg(x, xv >> 1);
        ctx.env.setFlag(xv & 1 != 0);
    }
    pub fn shl(ctx: Context, x: vm.Register, y: vm.Register) !void {
        _ = y; // Unused for some reason
        const res = @shlWithOverflow(ctx.env.getReg(x), 1);
        ctx.env.setReg(x, res[0]);
        ctx.env.setFlag(res[1] != 0);
    }

    pub fn ld_mem_bcd(ctx: Context, x: vm.Register) !void {
        const xv = ctx.env.getReg(x);
        const h = xv / 100;
        const nh = xv % 100;
        const t = nh / 10;
        const u = nh % 10;
        try ctx.setMem(ctx.env.addr, &.{ h, t, u });
    }

    pub fn rnd(ctx: Context, x: vm.Register, n: u8) !void {
        const v = ctx.env.rng.int(u8);
        ctx.env.setReg(x, v & n);
    }
};
