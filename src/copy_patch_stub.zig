//! Stub for generating copy-and-patch templates
const std = @import("std");
const vm = @import("vm.zig");
const copy_patch = @import("copy_patch.zig");
const instructions = @import("instructions.zig");

pub const InstructionContext = *vm.CopyPatchContext;

extern const operands: [3]u16;
extern fn next_instruction(InstructionContext) void;

export const insn_table = a: {
    var table: [std.enums.directEnumArrayLen(instructions.Opcode, 0)]*const fn (InstructionContext) callconv(.C) void = undefined;
    for (std.enums.values(instructions.Opcode)) |opcode| {
        table[@intFromEnum(opcode)] = switch (opcode) {
            .ret => &returnHandler,
            else => &GenericHandler(opcode).handler,
        };
    }
    break :a table;
};

pub fn panic(msg: []const u8, trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    extern_panic(msg.ptr, msg.len, trace, ret_addr orelse @returnAddress());
}
extern fn extern_panic([*]const u8, usize, ?*std.builtin.StackTrace, usize) noreturn;

fn GenericHandler(comptime opcode: instructions.Opcode) type {
    return struct {
        const opcodeFn = @field(instructions.insns, @tagName(opcode));
        pub fn handler(ctx: InstructionContext) callconv(.C) void {
            var args: std.meta.ArgsTuple(@TypeOf(opcodeFn)) = undefined;
            args[0] = ctx;
            inline for (operands[0 .. args.len - 1], 1..) |operand, i| {
                args[i] = switch (@TypeOf(args[i])) {
                    vm.Register => @enumFromInt(operand),
                    u4, u8, u12 => @intCast(operand),
                    else => |t| @compileError("Invalid operand type " ++ @typeName(t)),
                };
            }

            @call(.always_inline, opcodeFn, args) catch |err| {
                switch (@as(vm.Trap || error{Unwind}, err)) {
                    error.Unwind => {}, // Continue unwinding
                    else => |e| ctx.trap = copy_patch.Trap.fromError(e),
                }
                return @call(.always_tail, returnHandler, .{ctx});
            };

            @call(.always_tail, next_instruction, .{ctx});
        }
    };
}

fn returnHandler(_: InstructionContext) callconv(.C) void {
    // Magic :)
}
