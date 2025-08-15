
{-# LANGUAGE DeriveAnyClass #-}

-- TODO rename module and exports

module BV.Core.Glue
    ( IntermediateStagesOutput (..)
    , StagesInput (..)
    , StagesOutput (..)
    , stages
    ) where

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Foldable (for_)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data StagesInput
  = StagesInput
      { programs :: ByTag' Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , cFunctionPrefix :: String
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts'
      , proofScripts :: ProofScripts'
      , earlyAsmFunctionFilter :: FunctionFilter
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary StagesInput

data StagesOutput
  = StagesOutput
      { checks :: SMTProofChecks'
        -- report
      , unhandledInlineAssemblyFunctions :: [Ident]
      , unhandledInstructionFunctions :: [Ident]
        -- intermediate, for checking
      , intermediate :: IntermediateStagesOutput
      }
  deriving (Eq, Generic)

data IntermediateStagesOutput
  = IntermediateStagesOutput
      { functions :: Program
      , pairings :: Pairings'
      , problems :: Problems'
      , proofChecks :: ProofChecks'
      , compatProofChecks :: CompatProofChecks
      , compatSMTProofChecks :: CompatSMTProofChecks
      }
  deriving (Eq, Generic, NFData)

stages :: StagesInput -> StagesOutput
stages input = StagesOutput
    { checks = smtProofChecks
    , unhandledInlineAssemblyFunctions = unhandledAsmFunctionNames.c
    , unhandledInstructionFunctions = unhandledAsmFunctionNames.asm
    , intermediate = IntermediateStagesOutput
        { functions = collectedFunctions
        , pairings
        , problems
        , proofChecks
        , compatProofChecks = toCompatProofChecks proofChecks
        , compatSMTProofChecks = toCompatSMTProofChecks smtProofChecks
        }
    }

  where

    alterProgramByTag = byAsmRefineTag (ByAsmRefineTag
        { asm = applyFunctionFilter input.earlyAsmFunctionFilter
        , c = pseudoCompile input.objDumpInfo
        })

    alteredPrograms = alterProgramByTag <*> (fixupProgram <$> input.programs)

    (inlineAsmPairings, alteredProgramsWithInlineAsm, unhandledAsmFunctionNames) =
        addInlineAssemblySpecs alteredPrograms

    finalPrograms = alteredProgramsWithInlineAsm

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions ! funName

    functionSigs = signatureOfFunction . lookupFunction

    collectedFunctions = programFromFunctions $
        M.unionWith (error "not disjoint") finalPrograms.c.functions finalPrograms.asm.functions

    normalFunctionPairingIds = do
        asm <- M.keys finalPrograms.asm.functions
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
        guard $ c `M.member` finalPrograms.c.functions
        return $ byAsmRefineTag (ByAsmRefineTag { asm, c })

    normalPairings =
        let f pairingId = formulatePairing
                (input.stackBounds.unwrap ! pairingId.asm)
                (functionSigs (viewWithTag C pairingId))
         in M.fromSet f (S.fromList normalFunctionPairingIds)

    pairings = Pairings $ normalPairings `M.union` inlineAsmPairings.unwrap

    problems = Problems $ M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns =
                let f funName prog = Named funName (prog.functions ! funName)
                 in f <$> pairingId <*> finalPrograms
        for_ namedFuns $ \namedFun -> guard $ isJust namedFun.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    provenProblems = problems & #unwrap %~ (`M.restrictKeys` M.keysSet input.proofScripts.unwrap)

    lookupOrigVarNameFor problem = problemArgRenames problem $
        signatureOfFunction . lookupFunction <$> withTags (pairingIdOfProblem problem)

    proofChecks = ProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        let pairing = pairings.unwrap M.! pairingId
            proofScript = input.proofScripts.unwrap M.! pairingId
         in enumerateProofChecks (lookupOrigVarNameFor problem) pairing problem proofScript

    smtProofChecks = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        let repGraphInput = RepGraphBaseInput
                { structs = input.programs <&> (.structs)
                , rodata = input.rodata
                , problem
                }
         in compileProofChecks repGraphInput functionSigs pairings (lookupOrigVarNameFor problem)
                <$> (proofChecks.unwrap M.! pairingId)
