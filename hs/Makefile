.PHONY: none
none:

.PHONY: clean
clean:
	stack clean --full

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

# TODO still seems to document some deps
haddock_cmd := stack haddock --haddock-internal --no-haddock-deps --only-locals bv

.PHONY: doc
doc:
	$(haddock_cmd)

.PHONY: odoc
odoc:
	$(haddock_cmd) --open

.PHONY: fmt
fmt:
	stylish-haskell -i $$(find . -name '*.hs')

.PHONY: hlint
hlint:
	hlint src app test

.PHONY: enumerate-transitive-deps
enumerate-transitive-deps:
	stack dot --external --include-base | cut -d ' ' -f 1 | grep '"' | sort | uniq

.PHONY: i
i:
	stack ghci

.PHONY: it
it:
	stack ghci bv:test:bv-test
