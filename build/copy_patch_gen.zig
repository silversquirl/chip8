//! Build-time tool for generating copy-and-patch instruction templates
const std = @import("std");

comptime {
    // So we can liberally throw around `unreachable`s without worrying about safety :)
    std.debug.assert(@import("builtin").mode == .Debug);
}

const Sections = struct {
    text: ?std.elf.Elf64_Shdr = null,
    @"rel.text": ?std.elf.Elf64_Shdr = null,
    @"rela.text": ?std.elf.Elf64_Shdr = null,

    @"data.rel.ro": ?std.elf.Elf64_Shdr = null,
    @"rel.data.rel.ro": ?std.elf.Elf64_Shdr = null,
    @"rela.data.rel.ro": ?std.elf.Elf64_Shdr = null,

    symtab: ?std.elf.Elf64_Shdr = null,
    strtab: ?std.elf.Elf64_Shdr = null,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    std.debug.assert(args.skip());
    const templates_o = args.next().?;
    const templates_bin = args.next().?;
    const templates_zig = args.next().?;

    const obj_file = try std.fs.cwd().openFile(templates_o, .{});
    const ehdr = try std.elf.Header.read(obj_file);

    var iter = ehdr.section_header_iterator(obj_file);
    iter.index = ehdr.shstrndx;
    const shstrtab = (try iter.next()).?;

    var sections: Sections = .{};
    var text_idx: usize = undefined;
    var section_name = std.ArrayList(u8).init(allocator);
    iter.index = 0;
    while (try iter.next()) |shdr| {
        try obj_file.seekTo(shstrtab.sh_offset + shdr.sh_name);
        try obj_file.reader().readUntilDelimiterArrayList(&section_name, '\x00', 128);
        defer section_name.clearRetainingCapacity();
        inline for (comptime std.meta.fieldNames(Sections)) |name| {
            if (std.mem.eql(u8, "." ++ name, section_name.items)) {
                if (comptime std.mem.eql(u8, name, "text")) {
                    text_idx = iter.index - 1;
                }
                @field(sections, name) = shdr;
                break;
            }
        }
    }

    const ptr_size: u4 = if (ehdr.is_64) 8 else @panic("cannot handle 32-bit elf");

    const relaFn = switch (ehdr.machine) {
        // .@"386" => &relaX86,
        .X86_64 => &relaX64,
        // .ARM => &relaArm32,
        // .AARCH64 => &relaArm64,
        else => @panic("unsupported architecture"),
    };

    // Read symbol table
    const symtab = try allocator.alloc(
        std.elf.Elf64_Sym,
        @divExact(sections.symtab.?.sh_size, sections.symtab.?.sh_entsize),
    );
    try obj_file.seekTo(sections.symtab.?.sh_offset);
    try obj_file.reader().readNoEof(std.mem.sliceAsBytes(symtab));
    if (@import("builtin").cpu.arch.endian() != ehdr.endian) {
        for (symtab) |*sym| {
            std.mem.byteSwapAllFields(std.elf.Elf64_Sym, sym);
        }
    }

    // Read text section
    const text_section = try allocator.alloc(u8, sections.text.?.sh_size);
    try obj_file.seekTo(sections.text.?.sh_offset);
    try obj_file.reader().readNoEof(text_section);

    // Process text relocations
    var symbol_names = std.StringArrayHashMap(void).init(allocator);
    var relocations = try allocator.alloc(std.ArrayListUnmanaged(Relocation), symtab.len);
    for (relocations) |*rels| rels.* = .{};
    var sym_name = std.ArrayList(u8).init(allocator);
    if (sections.@"rela.text") |shdr| {
        var relas = std.ArrayList(ElfRela).init(allocator);
        try relaFn(&relas, obj_file, shdr);
        for (relas.items) |rel| {
            const sym = symtab[rel.symbol];
            if (sym.st_shndx != std.elf.SHN_UNDEF) {
                std.debug.panic("Expected symbol to be undefined (symbol {})", .{rel.symbol});
            }

            try obj_file.seekTo(sections.strtab.?.sh_offset + sym.st_name);
            try obj_file.reader().readUntilDelimiterArrayList(&sym_name, '\x00', 1024);
            defer sym_name.clearRetainingCapacity();
            const name_result = try symbol_names.getOrPutValue(sym_name.items, {});
            name_result.key_ptr.* = try sym_name.toOwnedSlice();

            const func = for (symtab, 0..) |s, i| {
                if (containingFunc(text_idx, s, rel.offset)) {
                    break .{
                        .idx = i,
                        .start = s.st_value,
                    };
                }
            } else {
                std.debug.panic("Could not find containing function for address {x}\n", .{rel.offset});
            };

            try relocations[func.idx].append(allocator, .{
                .offset = @intCast(rel.offset - func.start),
                .symbol = @intCast(name_result.index),
                .addend = rel.addend,
            });
        }
    } else {
        const shdr = sections.@"rel.text".?;
        _ = shdr;
        @panic("TODO: rel");
    }

    // Allocate table based on size of data section
    const insn_table = try allocator.alloc(u32, @divExact(sections.@"data.rel.ro".?.sh_size, ptr_size));

    // Process data relocations
    if (sections.@"rela.data.rel.ro") |shdr| {
        var relas = std.ArrayList(ElfRela).init(allocator);
        try relaFn(&relas, obj_file, shdr);
        for (relas.items) |rel| {
            const sym = symtab[rel.symbol];
            if (sym.st_shndx != text_idx) {
                std.debug.panic(
                    "Expected symbol to reference text section ({}), got {} (symbol {})",
                    .{ text_idx, sym.st_shndx, rel.symbol },
                );
            }
            const sym_addr = symtab[rel.symbol].st_value;

            const i = @divExact(rel.offset, ptr_size);
            insn_table[i] = @intCast(@as(i65, sym_addr) + rel.addend);
        }
    } else {
        const shdr = sections.@"rel.data.rel.ro".?;
        _ = shdr;
        @panic("TODO: rel");
    }

    var zig_source = std.ArrayList(u8).init(allocator);
    const w = zig_source.writer();
    try w.writeAll(
        \\const std = @import("std");
        \\
        \\pub const Template = struct {
        \\    start: u32,
        \\    len: u32,
        \\    relocations_ptr: ?[*:Relocation.empty]const Relocation,
        \\
        \\    pub fn code(t: Template) []const u8 {
        \\        return text_data[t.start .. t.start + t.len];
        \\    }
        \\
        \\    pub fn relocations(t: Template) []const Relocation {
        \\        if (t.relocations_ptr) |ptr| {
        \\            var i: usize = 0;
        \\            while (ptr[i].symbol != .undef) {
        \\                i += 1;
        \\            }
        \\            return ptr[0..i];
        \\        } else {
        \\            return &.{};
        \\        }
        \\    }
        \\};
        \\
        \\pub const Relocation = struct {
        \\    offset: u32,
        \\    symbol: Symbol,
        \\    addend: i64,
        \\
        \\    pub const empty: Relocation = .{
        \\        .offset = 0,
        \\        .symbol = .undef,
        \\        .addend = 0,
        \\    };
        \\};
        \\
        \\pub const Symbol = enum {
        \\    undef,
        \\
    );
    for (symbol_names.keys()) |name| {
        try w.print("    {},\n", .{std.zig.fmtId(name)});
    }
    try w.writeAll("};\n\n");

    try w.writeAll("const text_data = @embedFile(\"templates.bin\");\n\n");

    try w.print("pub const templates: [{}]Template = .{{\n", .{insn_table.len});
    for (insn_table) |off| {
        const func = for (symtab, 0..) |sym, i| {
            if (containingFunc(text_idx, sym, off)) {
                std.debug.assert(sym.st_value == off);
                break .{ .idx = i, .len = sym.st_size };
            }
        } else {
            std.debug.panic("Cannot find containing function for address {x}\n", .{off});
        };
        try w.print(
            \\    .{{
            \\        .start = {},
            \\        .len = {},
            \\        .relocations_ptr =
        , .{ off, func.len });
        const rels = relocations[func.idx].items;
        if (rels.len == 0) {
            try w.writeAll(" null");
        } else {
            try w.writeAll(" &[_:Relocation.empty]Relocation{\n");
            for (rels) |rel| {
                try w.print(
                    \\            .{{
                    \\                .offset = {},
                    \\                .symbol = .{},
                    \\                .addend = {},
                    \\            }},
                    \\
                , .{
                    rel.offset,
                    std.zig.fmtId(symbol_names.keys()[rel.symbol]),
                    rel.addend,
                });
            }
            try w.writeAll("        }");
        }
        try w.writeAll(",\n    },\n");
    }
    try w.writeAll("};\n");

    try std.fs.cwd().writeFile(templates_zig, zig_source.items);
    try std.fs.cwd().writeFile(templates_bin, text_section);
}

fn containingFunc(text_idx: usize, sym: std.elf.Elf64_Sym, addr: u64) bool {
    return sym.st_type() == std.elf.STT_FUNC and
        sym.st_shndx == text_idx and
        addr >= sym.st_value and
        addr < sym.st_value + sym.st_size;
}

fn relaX64(array: *std.ArrayList(ElfRela), file: std.fs.File, shdr: std.elf.Elf64_Shdr) !void {
    try file.seekTo(shdr.sh_offset);

    for (0..shdr.sh_size / @sizeOf(std.elf.Elf64_Rela)) |_| {
        var rela: std.elf.Elf64_Rela = undefined;
        try file.reader().readNoEof(std.mem.asBytes(&rela));

        switch (rela.r_type()) {
            std.elf.R_X86_64_64,
            std.elf.R_X86_64_REX_GOTPCRELX,
            std.elf.R_X86_64_GOTPCREL,
            std.elf.R_X86_64_GOTPCRELX,
            std.elf.R_X86_64_PLT32,
            => {},
            else => std.debug.panic("Cannot handle relocation of type {} (offset 0x{x})\n", .{ rela.r_type(), rela.r_offset }),
        }

        try array.append(.{
            .offset = rela.r_offset,
            .symbol = rela.r_sym(),
            .addend = rela.r_addend,
        });
    }
}

const Relocation = struct {
    offset: u32, // Offset within function
    symbol: u32, // Index into symbol name table
    addend: i64,
};

const ElfRela = struct {
    offset: u64,
    symbol: u32,
    addend: i64,
};
