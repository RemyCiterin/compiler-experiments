generate:
	RUST_BACKTRACE=1 cargo build --release
	cp ./target/release/compiler-experiments ./compiler


example/%.s: example/%.lang
	RUST_BACKTRACE=1 RUST_BACKTRACE=full cargo run --release -- $(@:example/%.s=%) example build

.PHONY: opencl
opencl:
	clang -c -target spir test.c -emit-llvm -o test.bc -O2 -cl-std=CL3.0
	llvm-spirv test.bc -o test.spv
	spirv-dis test.spv > test.asm
	llvm-dis test.bc > test.ll
	rm test.bc
	make example/brainfuck.s

.PHONY: clean
clean:
	@rm -f ./example/*.s
	@rm -f ./example/*.ir
	@rm -f ./example/*.rtl

.PHONY: test
test: example/*.s

.PHONY: all
all: example/brainfuck.s example/fibo.s example/mandelbrot.s example/eratosthene.s
