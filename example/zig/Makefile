
TARGET = -target riscv32-freestanding-none
OPT = -O ReleaseFast

.PHONY: all
all:
	zig build-exe test.zig -femit-llvm-bc $(TARGET) $(OPT)
	llvm-dis test.bc

.PHONY: spirv
spirv:
	zig build-exe test.zig -target spirv64-opencl-none
