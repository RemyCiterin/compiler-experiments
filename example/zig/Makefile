
TARGT = -target riscv32-freestanding-none
OPT = # -O ReleaseFast

.PHONY: all
all:
	zig build-exe test.zig -femit-llvm-bc $(TARGET) $(OPT)
	zig build-exe test.zig -femit-llvm-ir $(TARGET) $(OPT)

.PHONY: spirv
spirv:
	zig build-exe test.zig -target spirv64-opencl-none
