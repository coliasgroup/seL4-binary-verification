# Hacking

Building this project requires either [GHC](https://www.haskell.org/ghc/) (tested with 9.10.2) and
[Cabal](https://www.haskell.org/cabal/), or [Stack](https://docs.haskellstack.org).

Additional tools useful for development include:
- SMT solvers [Yices 2](https://yices.csl.sri.com/) and [Bitwuzla](https://bitwuzla.github.io/)
- [hpack](https://github.com/sol/hpack) for modifying `sel4-bv.cabal` via `package.yaml`
- [stylish-haskell](https://github.com/haskell/stylish-haskell) for formatting
- [Haskell Language Server](https://haskell-language-server.readthedocs.io) for IDE support

The easiest way to get started developing on this project is to have the [Nix package
manager](https://nixos.org/). With Nix installed, invoke `nix-shell` for an environment with all of
these dependencies installed.

If you are using VSCode, spawn it from within a `nix-shell` for an environment that includes a build
of the [Haskell Language Server](https://haskell-language-server.readthedocs.io) with support for
the right GHC version.
