
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

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data StagesInput
  = StagesInput
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , proofs :: Proofs ()
      , asmFunctionFilter :: Ident -> Bool
      }
  deriving (Generic)

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
      , compatSMTProofChecks :: CompatSMTProofChecks
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
    , compatSMTProofChecks
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

    problems = Problems . M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns = (\funName prog -> Named funName (prog.functions ! funName)) <$> pairingId <*> finalPrograms
        guard $ isJust namedFuns.c.value.body
        guard $ isJust namedFuns.asm.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    provenProblems = problems & #unwrap %~ \m -> M.restrictKeys m (M.keysSet input.proofs.unwrap)

    proofChecks = ProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
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

    compatProofChecks = toCompatProofChecks proofChecks

    uncheckedSMTProofChecks = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        compileProofChecks problem <$> (proofChecks `atPairingId` pairingId)

    compatSMTProofChecks = toCompatSMTProofChecks (void uncheckedSMTProofChecks)

    groupsAreDistinctAsExpected = and
        [ length groups == S.size (S.fromList (map (.setup) groups))
        | groups <- toList compatSMTProofChecks.unwrap
        ]

    smtProofChecks =
        if groupsAreDistinctAsExpected
        then uncheckedSMTProofChecks
        else error "SMT proof check groups should be distinct"

    preparedSMTProofChecks = flattenSMTProofChecks (adornSMTProofChecksWithDescriptions smtProofChecks)


asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)
