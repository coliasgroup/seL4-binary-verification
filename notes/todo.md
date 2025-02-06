- parameterize Function with body container (Identity or Maybe)
- TypedIdent type for Argument, etc.
- PermeableOptic
- swap order of PairingOf
- use left/right instead of asm/c for PairingOf

- graph-refine flags:
    - hack-proofs-must-be-loaded
    - hack-inline-scripts-must-be-loaded

- https://hackage.haskell.org/package/safe-exceptions

- use for sexprs and beyond:
    - https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html

- Rename ProofCheck -> IntermediateProofCheck
         SMTProofCheck -> ProofCheck

- newtype ByPairing a = ByPairing { unwrap :: M.Map PairingId a }
  (or type alias)
