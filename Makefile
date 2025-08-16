generate:
	RUST_BACKTRACE=1 cargo run --release -- main.lang main.s

example/%.s: example/%.lang
	RUST_BACKTRACE=1 cargo run --release -- $^ $@

.PHONY: clean
clean:
	rm ./example/*.s

.PHONY: test
test: example/*.s
