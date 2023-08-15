//! Runtime code for copy-and-patch JIT
const std = @import("std");
const vm = @import("vm.zig");
const instructions = @import("instructions.zig");
const templates = @import("copy_patch_templates");

/// An ABI-compatible representation for VM traps
pub const Trap = enum(u8) {
    success,

    Exit,
    InvalidAddress,
    UnsupportedInstruction,
    CompileError,
    StackOverflow,
    CodeSegmentModified,
    JumpToDataSegment,
    InputOutput,

    pub inline fn toError(trap: Trap) vm.Trap {
        return switch (trap) {
            .success => unreachable,
            inline else => |t| @field(vm.Trap, @tagName(t)),
        };
    }
    pub inline fn fromError(err: vm.Trap) Trap {
        return switch (err) {
            inline else => |e| @field(Trap, @errorName(e)),
        };
    }

    pub inline fn toErrorUnion(trap: Trap) vm.Trap!void {
        return switch (trap) {
            .success => {},
            inline else => |t| @field(vm.Trap, @tagName(t)),
        };
    }
    pub inline fn fromErrorUnion(err: vm.Trap!void) Trap {
        return if (err) |_|
            .success
        else |e|
            fromError(e);
    }
};

pub fn compileBlock(ctx: *vm.CopyPatchContext, addr: u12) vm.CompileError!CompileResult {
    std.debug.assert(!ctx.config.self_modification);

    std.log.debug("compiling block at {}", .{addr});
    const result = try assembleBlock(ctx, addr);
    std.log.debug("finished assembly", .{});
    try linkBlock(ctx, addr, result.end, result.code);
    std.log.debug("finished linking", .{});

    try std.os.mprotect(
        result.code.ptr[0..std.mem.alignForward(
            usize,
            result.code.len,
            std.mem.page_size,
        )],
        std.os.PROT.READ | std.os.PROT.EXEC,
    );

    std.log.debug("compile successful", .{});
    return .{ .compiled_code = result.code };
}

fn assembleBlock(ctx: *vm.CopyPatchContext, addr: u12) !AssembleResult {
    // Compute assembled length
    var size: usize = 0;
    var just_skipped = false;
    var it: instructions.InsnIter = .{
        .prog = &ctx.env.memory,
        .offset = addr,
    };
    while (try it.next()) |match| {
        std.log.debug("counting {} {}", .{ it.offset - 2, match });
        const template = templates.templates[@intFromEnum(match)];
        size = std.mem.alignForward(usize, size + template.len, @alignOf(usize)) + immediates(match).size;

        switch (match) {
            .se_reg_imm,
            .se_reg_reg,
            .sne_reg_imm,
            .sne_reg_reg,
            .skp,
            .sknp,
            => just_skipped = true,

            // If we hit a jump or return and there's no way to skip over it, that's the end of the block
            // FIXME: theoretically, a skip could be pointless and never actually jump, meaning the code after it could be
            //        invalid, so we should probably handle that too. But that seems unlikely so let's ignore it! :)
            .jp_imm, .jp_reg0_off, .ret => if (just_skipped) {
                just_skipped = false;
            } else {
                break;
            },

            else => just_skipped = false,
        }
    }
    const end = it.offset;
    // Allocate extra space for the link table reference
    size = std.mem.alignForward(usize, size, @alignOf(usize)) + @sizeOf(usize);

    // Allocate memory
    const code = try std.os.mmap(
        null,
        size,
        std.os.PROT.READ | std.os.PROT.WRITE,
        std.os.MAP.PRIVATE | std.os.MAP.ANONYMOUS,
        -1,
        0,
    );

    // Assemble program from opcode templates, and update program map
    var offset: usize = 0;
    it.offset = addr; // Reset iterator
    while (it.offset < end) {
        const match = (try it.next()).?;
        std.log.debug("assembling {} {}", .{ it.offset - 2, match });
        ctx.program_map[it.offset - 2 - vm.Env.memory_base] = @ptrCast(&code[offset]);

        const template = templates.templates[@intFromEnum(match)];
        @memcpy(code[offset .. offset + template.len], template.code());
        offset = std.mem.alignForward(usize, offset + template.len, @alignOf(usize));

        var imm = immediates(match);
        if (imm.size != 0) {
            const imm_addr: *usize = @alignCast(@ptrCast(&code[offset]));
            imm_addr.* = @intFromPtr(&code[offset + @sizeOf(usize)]);
            offset += @sizeOf(usize);

            const count = imm.size - @sizeOf(usize);
            @memcpy(code[offset .. offset + count], std.mem.sliceAsBytes(&imm.values)[0..count]);
            offset += count;
        }
    }

    // Add link table reference
    offset = std.mem.alignForward(usize, offset, @alignOf(usize));
    std.debug.assert(offset + @sizeOf(usize) == code.len);
    std.mem.bytesAsValue(
        *const vm.CopyPatchContext.LinkTable,
        code[offset..][0..@sizeOf(usize)],
    ).* = &vm.CopyPatchContext.link_table;

    return .{ .code = code, .end = end };
}

fn immediates(match: instructions.Match) Immediates {
    var imm: Immediates = .{};
    switch (match) {
        inline else => |operands| {
            inline for (operands) |arg| {
                imm.values[@divExact(imm.size, 2)] = switch (@TypeOf(arg)) {
                    vm.Register => @intFromEnum(arg),
                    u4, u8, u12 => arg,
                    else => @compileError("Invalid operand type " ++ @typeName(@TypeOf(arg.*))),
                };
                imm.size += @sizeOf(u16);
            }
        },
    }
    if (imm.size != 0) {
        imm.size += @sizeOf(usize);
    }
    return imm;
}
const Immediates = struct {
    size: u4 = 0,
    values: [3]u16 = undefined,
};

fn linkBlock(ctx: *vm.CopyPatchContext, addr: u12, end: usize, code: []align(std.mem.page_size) u8) !void {
    var it: instructions.InsnIter = .{
        .prog = &ctx.env.memory,
        .offset = addr,
    };
    var offset: usize = 0;
    while (it.offset < end) {
        const match = (try it.next()).?;
        std.log.debug("linking {} {}", .{ it.offset - 2, match });
        const template = templates.templates[@intFromEnum(match)];
        const imm_offset = std.mem.alignForward(usize, offset + template.len, @sizeOf(usize));
        defer offset = imm_offset + immediates(match).size;

        var reloc_map = RelocationMap{
            .operands = @alignCast(std.mem.bytesAsValue(
                usize,
                code[imm_offset..][0..@sizeOf(usize)],
            )),

            .next_instruction = ctx.program_map[it.offset - vm.Env.memory_base],
            .next_next_instruction = ctx.program_map[it.offset + 2 - vm.Env.memory_base],

            .extern_link_table = @alignCast(@ptrCast(&code[code.len - @sizeOf(usize)])),
        };

        const relocs = template.relocations();
        for (relocs) |rel| {
            const symbol_value = switch (rel.symbol) {
                .undef => unreachable, // This is purely a sentinel

                inline else => |sym| a: {
                    const value = @field(reloc_map, @tagName(sym)) orelse {
                        // Depending on context, this could mean one of:
                        // - a "skip" instruction is used too close to the end of the program
                        // - the code ends with a non-jump, non-ret instruction
                        std.log.err("Relocation for {s} in {s} instruction has no value", .{
                            @tagName(sym),
                            @tagName(match),
                        });
                        return error.InvalidInstruction;
                    };

                    break :a @intFromPtr(value);
                },
            };

            const reloc_addr = @intFromPtr(code.ptr) + offset + rel.offset;
            const value: i32 = @intCast(@as(i65, symbol_value) - reloc_addr + rel.addend);
            std.mem.writeIntNative(
                u32,
                code[offset + rel.offset ..][0..@sizeOf(u32)],
                @as(u32, @bitCast(value)),
            );
        }
    }
}

pub const CompileResult = struct {
    compiled_code: []align(std.mem.page_size) u8,

    pub fn deinit(result: CompileResult) void {
        std.os.munmap(result.compiled_code);
    }
};

const AssembleResult = struct {
    code: []align(std.mem.page_size) u8,
    end: usize,
};

const RelocationMap = struct {
    operands: ?*const usize,

    next_instruction: ?InsnFn,
    next_next_instruction: ?InsnFn,

    extern_link_table: ?*const *const vm.CopyPatchContext.LinkTable,
};

pub const InsnFn = *const fn (*vm.CopyPatchContext) callconv(.C) void;
