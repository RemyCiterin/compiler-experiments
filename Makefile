generate:
	RUST_BACKTRACE=1 cargo run --release -- main.lang main


example/%.s: example/%.lang
	RUST_BACKTRACE=1 cargo run --release -- $(@:.s=)

.PHONY: clean
clean:
	@rm -f ./example/*.s
	@rm -f ./example/*.ir
	@rm -f ./example/*.rtl

.PHONY: test
test: example/*.s

.PHONY: all
all: example/brainfuck.s example/fibo.s example/mandelbrot.s example/eratosthene.s
