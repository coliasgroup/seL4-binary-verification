.PHONY: none
none:

.PHONY: build
build:
	stack build

.PHONY: check
check:
	# TODO
	stack build --fast

.PHONY: test
test:
	stack test

haddock_cmd := stack haddock --haddock-internal

.PHONY: doc
doc:
	$(haddock_cmd)

.PHONY: odoc
odoc:
	$(haddock_cmd) --open

.PHONY: fmt
fmt:
	stylish-haskell -i $$(find . -name '*.hs')

.PHONY: enumerate-transitive-deps
enumerate-transitive-deps:
	stack dot --external --include-base | cut -d ' ' -f 1 | grep '"' | sort | uniq
