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

- add parallelism to stages
- use NFData1 with fake (a -> ()) arg to avoid eval'ing meta
- add test that just does stages without any checking reference

- pure json format for inline scripts, proofs(?), and stack bounds(?)
- rename SMTProofChecks, because that is all that is used in system (should have shorter name)

- doc fix in Data.Aeson.Decoding.Text:
  - "Lex (and parse) strict ByteString into Tokens stream." should be Text

- use function guards and MultiWayIf, including with e.g. "Just (exponent, mantissa) <- decomposeNumber str"

- pretty printing for sexprs

- use exceptions instead of die with throwString from safe-exceptions
