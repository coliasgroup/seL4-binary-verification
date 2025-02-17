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

- fail explicitly in situations where online solver is irrelevant and no offlines solvers are configured
  (or, in the case of justTheseChecks, no hyp offline solvers are configured)

- remove SeL4.hs. in general, provide expressivity required by target.py in CLI:
    --asm-symbol-prefix, --c-symbol-prefix
      (check to make these are distinct)
    --ignore-function
    (--include- and --ignore-function are for "true" name, with prefix on either side stripped)
    --include-function-with-deps
    --include-all-functions
      (use with --include-function to make sure that root functions are present)

- provide module in core with minimal exports, for system-core (concrete-syntax will need more)

- ensure that all functions in the binary are checked, by specifying entrypoints

- code defensively against sat solver bugs
  - don't just INSERT OR IGNORE in sqlite. instead, ensure that values match
  - add feature to require unsat from _multiple_ solvers, not just any solver, and(?) cache solver identities (witnesses) along with results

- printf instances for all pretty* functions
