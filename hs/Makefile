.PHONY: none
none:

.PHONY: clean
clean:
	hpack
	cabal clean
	stack clean --full

.PHONY: hpack
hpack:
	hpack

.PHONY: build
build:
	hpack
	cabal build

.PHONY: check
check:
	hpack
	cabal build

.PHONY: test
test:
	cabal test

# TODO still seems to document some deps
haddock_cmd := cabal haddock-project

.PHONY: doc
doc:
	$(haddock_cmd)

.PHONY: odoc
odoc:
	$(haddock_cmd)
	xdg-open haddocks/index.html

.PHONY: fmt
fmt:
	stylish-haskell -i $$(find . -name '*.hs')

.PHONY: hlint
hlint:
	hlint src app test

.PHONY: nix
nix:
	nix-build -A package

.PHONY: enumerate-transitive-deps
enumerate-transitive-deps:
	stack dot --external --include-base | cut -d ' ' -f 1 | grep '"' | sort | uniq

.PHONY: i
i:
	cabal repl

.PHONY: it
it:
	cabal repl sel4-bv:test:bv-test
