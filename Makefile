
.PHONY: none
none:

.PHONY: hpack
hpack:
	hpack

.PHONY: hpack-watch
hpack-watch:
	hpack
	set -eu -o pipefail; \
	inotifywait -e create,move,move_self,delete,delete_self -mr components | \
		while read _line; do \
			hpack; \
		done

.PHONY: clean
clean: hpack
	cabal clean
	rm -rf haddocks
	stack clean --full

.PHONY: clean-sel4-example
clean-sel4-example: hpack
	rm -rf examples/seL4/tmp

.PHONY: build
build: hpack
	cabal build --enable-tests

.PHONY: check
check: hpack
	cabal build --enable-tests

test_target_dirs := tmp/test-target-dirs

.PHONY: test
test: hpack
	cabal test

.PHONY: test-big
test-big: hpack
	cabal test \
		--jobs=1 \
		--test-option=--for-slow=$(test_target_dirs)/big

# TODO still seems to document some deps
haddock_cmd := cabal haddock-project

.PHONY: doc
doc: hpack
	$(haddock_cmd)

.PHONY: odoc
odoc: hpack
	$(haddock_cmd)
	xdg-open haddocks/index.html

.PHONY: fmt
fmt: hpack
	stylish-haskell -i $$(find components -name '*.hs')

.PHONY: hlint
hlint: hpack
	hlint components

.PHONY: nix
nix:
	nix-build -A sel4-bv

.PHONY: enumerate-transitive-deps
enumerate-transitive-deps: hpack
	false

.PHONY: i
i: hpack
	cabal repl --enable-multi-repl

# # #

.PHONY: test-stages
test-stages: hpack
	cabal test core-test \
		--test-option=--pattern=stages-with-reference

.PHONY: test-stages-big
test-stages-big: hpack
	cabal test core-test \
		--test-option=--for-slow=$(test_target_dirs)/big \
		--test-option=--pattern=stages-with-reference

.PHONY: test-stages-focused
test-stages-focused: hpack
	cabal test core-test \
		--test-option=--for-slow=$(test_target_dirs)/focused \
		--test-option=--pattern=stages-with-reference

.PHONY: test-inlining
test-inlining: hpack
	cabal test search-test \
		--test-option=--pattern=inlining

.PHONY: test-inlining-big
test-inlining-big: hpack
	cabal test search-test \
		--test-option=--for-slow=$(test_target_dirs)/big \
		--test-option=--pattern=inlining

.PHONY: test-inlining-focused
test-inlining-focused: hpack
	cabal test search-test \
		--test-option=--for-slow=$(test_target_dirs)/focused \
		--test-option=--pattern=inlining

.PHONY: test-stack-bounds
test-stack-bounds: hpack
	cabal test search-test \
		--test-option=--include-wip \
		--test-option=--pattern=stack-bounds
