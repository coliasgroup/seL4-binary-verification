.PHONY: none
none:

.PHONY: fmt
fmt:
	cargo fmt

.PHONY: check
check:
	cargo check --tests

.PHONY: test
test:
	cargo test --lib

.PHONY: doc
doc:
	cargo doc --document-private-items

.PHONY: odoc
odoc:
	cargo doc --document-private-items --open

.PHONY: scratch
scratch:
	RUST_LOG=trace cargo test -p bv --lib tests::scratch::dot::dot -- --ignored --exact --nocapture
