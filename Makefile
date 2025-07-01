
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

.PHONY: build
build: hpack
	cabal build --enable-tests

.PHONY: check
check: hpack
	cabal build --enable-tests

.PHONY:
test: hpack
	cabal test

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

.PHONY: test-stages-with-reference
test-stages-with-reference: hpack
	cabal test core-test --test-option=--pattern=stages-with-reference

.PHONY: test-just-focused
test-just-focused: hpack
	cabal test core-test --test-option=--pattern=just-focused
