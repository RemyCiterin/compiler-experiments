const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn foo(alloc: *const Allocator, pointer: []i32) void {
    alloc.free(pointer);
}

pub export fn _start() void {
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    const memory = allocator.alloc(i32, 100) catch unreachable;
    defer foo(&allocator, memory);

    // try expect(memory.len == 100);
    // try expect(@TypeOf(memory) == []u8);
}
