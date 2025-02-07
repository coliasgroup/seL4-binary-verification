
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Core.Stages
    ( module BV.Core.Stages.BuildProblem
    , module BV.Core.Stages.CompileProofChecks
    , module BV.Core.Stages.EnumerateProofChecks
    , module BV.Core.Stages.Fixup
    , module BV.Core.Stages.FormulatePairing
    , module BV.Core.Stages.InlineAssembly
    , module BV.Core.Stages.PseudoCompile
    , PreparedSMTProofChecks
    , SMTProofCheckDescription
    , StagesInput (..)
    , StagesOutput (..)
    , stages
    ) where

import BV.Core.AdornProofScript
import BV.Core.Stages.BuildProblem
import BV.Core.Stages.CompileProofChecks
import BV.Core.Stages.EnumerateProofChecks
import BV.Core.Stages.Fixup
import BV.Core.Stages.FormulatePairing
import BV.Core.Stages.InlineAssembly
import BV.Core.Stages.PseudoCompile
import BV.Core.Types
import BV.Core.Types.Extras

import Control.DeepSeq (NFData, force)
import Control.Monad (guard, (>=>))
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Control.Parallel.Strategies
import Control.Monad.State (withState)

data StagesInput
  = StagesInput
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , proofs :: Proofs ()
      , asmFunctionFilter :: Ident -> Bool
        -- HACK
    --   , compatSMTProofChecks :: CompatSMTProofChecks
      }
  deriving (Generic, NFData)

data StagesOutput
  = StagesOutput
      { smtProofChecks :: PreparedSMTProofChecks
        -- report
      , unhandledInlineAssemblyFunctions :: [Ident]
      , unhandledInstructionFunctions :: [Ident]
        -- intermediate, for checking
      , functions :: Program
      , pairings :: Pairings
      , problems :: Problems
      , compatProofChecks :: CompatProofChecks
    --   , compatSMTProofChecks :: CompatSMTProofChecks
      --
      , x :: ProofChecks String
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type PreparedSMTProofChecks = FlattenedSMTProofChecks SMTProofCheckDescription

stages :: StagesInput -> StagesOutput
stages input = StagesOutput
    { smtProofChecks = preparedSMTProofChecks
    , unhandledInlineAssemblyFunctions = unhandledAsmFunctionNames.c
    , unhandledInstructionFunctions = unhandledAsmFunctionNames.asm
    , functions = collectedFunctions
    , pairings
    , problems
    , compatProofChecks
    -- , compatSMTProofChecks
    , x = proofChecks
    }

  where

    alteredPrograms = fixupProgram <$> PairingOf
        { asm = input.programs.asm & #functions %~ M.filterWithKey (\k _v -> input.asmFunctionFilter k)
        , c = pseudoCompile input.objDumpInfo input.programs.c
        }

    (inlineAsmPairings, alteredProgramsWithInlineAsm, unhandledAsmFunctionNames) =
        addInlineAssemblySpecs alteredPrograms

    finalPrograms = alteredProgramsWithInlineAsm

    collectedFunctions = programFromFunctions $
        M.unionWith (error "not disjoint") finalPrograms.c.functions finalPrograms.asm.functions

    normalFunctionPairingIds = do
        asm <- M.keys finalPrograms.asm.functions
        let c = asmFunNameToCFunName asm
        guard $ c `M.member` finalPrograms.c.functions
        return $ PairingOf { c = asmFunNameToCFunName asm, asm }

    normalPairings = M.fromList
        [ let stackBound = input.stackBounds.unwrap ! pairingId.asm
              cFun = finalPrograms.c.functions ! pairingId.c
              pairing = formulatePairing stackBound cFun.input cFun.output
           in (pairingId, pairing)
        | pairingId <- normalFunctionPairingIds
        ]

    pairings = Pairings $ normalPairings `M.union` inlineAsmPairings.unwrap

    lookupFunction (WithTag tag funName) = (pairingSide tag finalPrograms).functions ! funName

    problems' = Problems . M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns = (\funName prog -> Named funName (prog.functions ! funName)) <$> pairingId <*> finalPrograms
        guard $ isJust namedFuns.c.value.body
        guard $ isJust namedFuns.asm.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    problems = withStrategy problemsStrategy problems'
    -- problems = problems'

    provenProblems = problems & #unwrap %~ \m -> M.restrictKeys m (M.keysSet input.proofs.unwrap)

    proofChecks' = ProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        let pairing = pairings `atPairingId` pairingId
            proofScript = input.proofs `atPairingId` pairingId
            lookupOrigVarName quadrant mangledName =
                fromJust $ lookup mangledName (zip (map (.name) mangledArgs) (map (.name) origArgs))
              where
                fun = lookupFunction (pairingSideWithTag quadrant.tag pairingId)
                origArgs = case quadrant.direction of
                    PairingEqDirectionIn -> fun.input
                    PairingEqDirectionOut -> fun.output
                probSide = pairingSide quadrant.tag problem.sides
                mangledArgs = case quadrant.direction of
                    PairingEqDirectionIn -> probSide.input
                    PairingEqDirectionOut -> probSide.output
         in enumerateProofChecks lookupOrigVarName pairing problem proofScript

    -- proofChecks = force $ withStrategy proofChecksStrategy proofChecks'
    proofChecks = proofChecks'

    compatProofChecks' = toCompatProofChecks proofChecks

    -- compatProofChecks = force $ withStrategy compatProofChecksStrategy compatProofChecks'
    compatProofChecks = compatProofChecks'

    -- uncheckedSMTProofChecks'hack = liftCompatSMTProofChecks'hack input.compatSMTProofChecks

    uncheckedSMTProofChecks'nohack = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        compileProofChecks problem <$> (proofChecks `atPairingId` pairingId)

    -- uncheckedSMTProofChecks = uncheckedSMTProofChecks'hack
    uncheckedSMTProofChecks = uncheckedSMTProofChecks'nohack

    compatSMTProofChecks = toCompatSMTProofChecks (void uncheckedSMTProofChecks)

    groupsAreDistinctAsExpected = and
        [ length groups == S.size (S.fromList (map (.setup) groups))
        | groups <- toList compatSMTProofChecks.unwrap
        ]

    smtProofChecks =
        if groupsAreDistinctAsExpected
        then uncheckedSMTProofChecks
        else
            -- TODO
            -- error "SMT proof check groups should be distinct"
            uncheckedSMTProofChecks

    preparedSMTProofChecks' = flattenSMTProofChecks (adornSMTProofChecksWithDescriptions smtProofChecks)

    preparedSMTProofChecks = force $ withStrategy preparedSMTProofChecksStrategy preparedSMTProofChecks'


compatProofChecksStrategy :: Strategy CompatProofChecks
compatProofChecksStrategy = r0
-- compatProofChecksStrategy = traverseOf (#unwrap % traversed) $ \checks -> parEval $ do
--     -- traverse evalHyps checks
--     -- traverse evalHyps checks
--     rdeepseq checks
-- compatProofChecksStrategy = traverseOf (#unwrap % traversed) $ \checks -> parEval $ do
--     -- traverse evalHyps checks
--     -- traverse evalHyps checks
--     rdeepseq checks

proofChecksStrategy :: NFData a => Strategy (ProofChecks a)
-- proofChecksStrategy = r0
proofChecksStrategy = traverseOf traverseNodeChecks $ \nodeChecks -> parEval $ do
    rdeepseq nodeChecks
    -- traverse evalHyps nodeChecks
    -- traverse rdeepseq nodeChecks
  where
    traverseNodeChecks = #unwrap % traversed % traversed

-- evalHyps :: Strategy (ProofCheck a)
-- evalHyps =
--         traverseOf #hyp rdeepseq
--     >=> traverseOf (#hyps % traversed) rdeepseq

preparedSMTProofChecksStrategy :: Strategy PreparedSMTProofChecks
preparedSMTProofChecksStrategy = r0
-- preparedSMTProofChecksStrategy preparedSMTProofChecks = do
--     return preparedSMTProofChecks

problemsStrategy :: Strategy Problems
problemsStrategy = traverseOf (#unwrap % traversed) $ \problem -> parEval $ do
    rdeepseq problem
    -- traverse evalHyps nodeChecks
    -- traverse rdeepseq nodeChecks

asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)

--

liftCompatSMTProofChecks'hack :: CompatSMTProofChecks -> SMTProofChecks String
liftCompatSMTProofChecks'hack compatChecks = SMTProofChecks $ M.map f compatChecks.unwrap
  where
    f groups = ProofScript (ProofNodeWith (("" <$) <$> groups) ProofNodeLeaf)
