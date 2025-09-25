
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
        { functions = programFromFunctions $
                M.unionWith undefined
                    finalPrograms.c.functions
                    finalPrograms.asm.functions
        , pairings
        , problems
        , proofChecks
        , compatProofChecks = toCompatProofChecks proofChecks
        , compatSMTProofChecks = toCompatSMTProofChecks smtProofChecks
        }
    }

  where

    (inlineAsmPairings, finalPrograms, unhandledAsmFunctionNames) =
        addInlineAssemblySpecs
        . over (atTag C) (pseudoCompile input.objDumpInfo)
        . over (atTag Asm) (applyFunctionFilter input.earlyAsmFunctionFilter)
        . over mapped fixupProgram
        $ input.programs

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions ! funName

    lookupFunctionSig = signatureOfFunction . lookupFunction

    normalFunctionPairingIds = do
        asm <- M.keys finalPrograms.asm.functions
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
        guard $ c `M.member` finalPrograms.c.functions
        return $ byAsmRefineTag (ByAsmRefineTag { asm, c })

    normalPairings =
        let f pairingId = formulatePairing
                (input.stackBounds.unwrap ! pairingId.asm)
                (lookupFunctionSig (viewWithTag C pairingId))
         in Pairings $ M.fromSet f (S.fromList normalFunctionPairingIds)

    pairings = normalPairings <> inlineAsmPairings

    problems = Problems $ M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns =
                let f funName prog = Named funName (prog.functions ! funName)
                 in f <$> pairingId <*> finalPrograms
        for_ namedFuns $ \namedFun -> guard $ isJust namedFun.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    provenProblems = M.restrictKeys problems.unwrap (M.keysSet input.proofScripts.unwrap)

    proofChecks = ProofChecks $ flip M.mapWithKey provenProblems $ \pairingId problem ->
        let pairing = pairings.unwrap ! pairingId
            sigs = lookupFunctionSig <$> withTags pairingId
            proofScript = input.proofScripts.unwrap ! pairingId
         in enumerateProofChecks problem sigs pairing proofScript

    smtProofChecks = SMTProofChecks $ flip M.mapWithKey provenProblems $ \pairingId problem ->
        let repGraphInput = RepGraphBaseInput
                { structs = (.structs) <$> input.programs
                , rodata = input.rodata
                , problem
                }
         in compileProofChecks repGraphInput lookupFunctionSig pairings
                <$> (proofChecks.unwrap ! pairingId)
