
cabal_pkg_name := sel4-bv
cabal_pkg_desc := $(cabal_pkg_name).cabal

.PHONY: none
none:

$(cabal_pkg_desc): package.yaml
	hpack

.PHONY: clean
clean: $(cabal_pkg_desc)
	cabal clean
	rm -rf haddocks
	stack clean --full

.PHONY: build
build: $(cabal_pkg_desc)
	cabal build

.PHONY: check
check: $(cabal_pkg_desc)
	cabal build

.PHONY:
test: $(cabal_pkg_desc)
	cabal test

# TODO still seems to document some deps
haddock_cmd := cabal haddock-project

.PHONY: doc
doc: $(cabal_pkg_desc)
	$(haddock_cmd)

.PHONY: odoc
odoc: $(cabal_pkg_desc)
	$(haddock_cmd)
	xdg-open haddocks/index.html

.PHONY: fmt
fmt: $(cabal_pkg_desc)
	stylish-haskell -i $$(find . -name '*.hs')

.PHONY: hlint
hlint: $(cabal_pkg_desc)
	hlint components

.PHONY: nix
nix:
	nix-build -A package

.PHONY: enumerate-transitive-deps
enumerate-transitive-deps: $(cabal_pkg_desc)
	false

.PHONY: i
i: $(cabal_pkg_desc)
	cabal repl --enable-multi-repl

.PHONY: it
it: $(cabal_pkg_desc)
	cabal repl sel4-bv:test:test
