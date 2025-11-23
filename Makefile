generate:
	RUST_BACKTRACE=1 cargo build --release
	cp ./target/release/compiler-experiments ./compiler


example/%.s: example/%.lang
	RUST_BACKTRACE=1 RUST_BACKTRACE=full cargo run --release -- $(@:example/%.s=%) example build

OCL_FLAGS = \
					 -target spir -emit-llvm -O2 -cl-std=CL3.0

build/%.bc: stdlib/%.c
	clang $(OCL_FLAGS) -Istdlib -c $< -o $@

.PHONY: opencl
opencl: clean build/malloc.bc
	# Generate the llvm bytecode for each the input files
	clang $(OCL_FLAGS) -c test.c \
		-Istdlib -Lstdlib
	# Link the llvm bytecode of all the inputs
	llvm-link test.bc build/*.bc -o build/main.bc
	# Generate a spirv file from the llvm bytecode
	llvm-spirv build/main.bc -o test.spv
	# Diassemble spirv and llvm bytecodes
	spirv-dis test.spv > test.asm
	llvm-dis test.bc > test.ll
	# Run compiler
	make example/brainfuck.s

.PHONY: clean
clean:
	@rm -f ./test.bc
	@rm -f ./test.ll
	@rm -f ./test.asm
	@rm -f ./test.spv
	@rm -f ./build/*.bc
	@rm -f ./example/*.s
	@rm -f ./example/*.ir
	@rm -f ./example/*.rtl

.PHONY: test
test: example/*.s

.PHONY: all
all: example/brainfuck.s example/fibo.s example/mandelbrot.s example/eratosthene.s
