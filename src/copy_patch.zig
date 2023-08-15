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

pub fn compile(config: vm.Config, prog: []const u8) vm.CompileError!CompileResult {
    std.debug.assert(!config.self_modification);

    const result = try assembleProgram(prog);
    try linkProgram(prog, result);
    try std.os.mprotect(
        result.compiled_code.ptr[0..std.mem.alignForward(
            usize,
            result.compiled_code.len,
            std.mem.page_size,
        )],
        std.os.PROT.READ | std.os.PROT.EXEC,
    );
    return result;
}

fn assembleProgram(prog: []const u8) !CompileResult {
    // Compute assembled program length
    var size: usize = 0;
    var it: instructions.InsnIter = .{ .prog = prog };
    while (try it.next()) |match| {
        const template = templates.templates[@intFromEnum(match)];
        size = std.mem.alignForward(usize, size + template.len, @alignOf(usize)) + immediates(match).size;
    }
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

    // Assemble program from opcode templates, and generate program map
    var map: vm.CopyPatchContext.ProgramMap = .{null} ** vm.CopyPatchContext.program_map_len;
    var offset: usize = 0;
    it = instructions.InsnIter{ .prog = prog };
    while (try it.next()) |match| {
        map[it.offset - 2] = @ptrCast(&code[offset]);

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
        *const @TypeOf(vm.CopyPatchContext.link_table),
        code[offset..][0..@sizeOf(usize)],
    ).* = &vm.CopyPatchContext.link_table;

    return .{
        .compiled_code = code,
        .program_map = map,
    };
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

fn linkProgram(prog: []const u8, result: CompileResult) !void {
    var it: instructions.InsnIter = .{ .prog = prog };
    var offset: usize = 0;
    while (try it.next()) |match| {
        const template = templates.templates[@intFromEnum(match)];
        const imm_offset = std.mem.alignForward(usize, offset + template.len, @sizeOf(usize));
        defer offset = imm_offset + immediates(match).size;

        var reloc_map = RelocationMap{
            .operands = @alignCast(std.mem.bytesAsValue(
                usize,
                result.compiled_code[imm_offset..][0..@sizeOf(usize)],
            )),

            .next_instruction = result.program_map[it.offset],
            .next_next_instruction = result.program_map[it.offset + 2],

            .extern_link_table = @alignCast(@ptrCast(&result.compiled_code[result.compiled_code.len - @sizeOf(usize)])),
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
                        return error.InvalidInstruction;
                    };

                    break :a @intFromPtr(value);
                },
            };

            const addr = @intFromPtr(result.compiled_code.ptr) + offset + rel.offset;
            const value: i32 = @intCast(@as(i65, symbol_value) - addr + rel.addend);
            std.mem.writeIntNative(
                u32,
                result.compiled_code[offset + rel.offset ..][0..@sizeOf(u32)],
                @as(u32, @bitCast(value)),
            );
        }
    }
}

pub const CompileResult = struct {
    compiled_code: []align(std.mem.page_size) u8,
    program_map: vm.CopyPatchContext.ProgramMap,

    pub fn deinit(result: CompileResult) void {
        std.os.munmap(result.compiled_code);
    }
};

const RelocationMap = struct {
    operands: ?*const usize,

    next_instruction: ?InsnFn,
    next_next_instruction: ?InsnFn,

    extern_link_table: ?*const *const @TypeOf(vm.CopyPatchContext.link_table),
};

const InsnFn = *const fn (*vm.CopyPatchContext) callconv(.C) void;
