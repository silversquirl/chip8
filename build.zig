const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "chip8",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("copy_patch_templates", copyPatchTemplates(b, target));
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(unit_tests).step);
}

fn copyPatchTemplates(b: *std.Build, target: std.zig.CrossTarget) *std.Build.Module {
    const stub = b.addObject(.{
        .name = "copy_patch_stub",
        .root_source_file = .{ .path = "src/copy_patch_stub.zig" },
        .target = target,
        .optimize = .ReleaseSmall,
    });
    stub.force_pic = true;

    const gen = b.addExecutable(.{
        .name = "copy_patch_gen",
        .root_source_file = .{ .path = "build/copy_patch_gen.zig" },
    });

    const run = b.addRunArtifact(gen);
    run.addArtifactArg(stub);
    _ = run.addOutputFileArg("templates.bin");
    return b.createModule(.{
        .source_file = run.addOutputFileArg("templates.zig"),
    });
}
