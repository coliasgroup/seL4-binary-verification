{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Core.GluedStages
    ( GluedStagesInput (..)
    , IntermediateArtifact (..)
    , gluedStages
    ) where

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras

import Control.DeepSeq (NFData)
import Control.Monad (guard, unless)
import Control.Monad.Logger
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Data.String (fromString)
import GHC.Generics (Generic)
import Optics

data GluedStagesInput
  = GluedStagesInput
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , proofs :: Proofs ()
      , asmFunctionFilter :: Ident -> Bool
      }
  deriving (Generic)

data IntermediateArtifact
  = IntermediateArtifactFunctions Program
  | IntermediateArtifactPairings Pairings
  | IntermediateArtifactProblems Problems
  | IntermediateArtifactFlattenedProofChecks (FlattenedProofChecks String)
  | IntermediateArtifactFlattenedSMTProofChecks (FlattenedSMTProofChecks ())
  deriving (Eq, Generic, NFData, Ord, Show)

-- TODO leave logging to 'registerIntermediateArtifact' function?

gluedStages
    :: MonadLogger m
    => (IntermediateArtifact -> m ()) -> GluedStagesInput -> m (SMTProofChecks String)
gluedStages registerIntermediateArtifact input = do
    logWarnN . fromString $ "Unhandled inline assembly functions (C side): " ++ show (map (.unwrap) unhandledAsmFunctionNames.c)
    logWarnN . fromString $ "Unhandled instrcution functions (Asm side): " ++ show (map (.unwrap) unhandledAsmFunctionNames.asm)
    logInfoN "Registering functions"
    registerIntermediateArtifact $ IntermediateArtifactFunctions collectedFunctions
    logInfoN "Registering pairings"
    registerIntermediateArtifact $ IntermediateArtifactPairings pairings
    logInfoN "Registering problems"
    registerIntermediateArtifact $ IntermediateArtifactProblems problems
    logInfoN "Registering flattened proof checks"
    registerIntermediateArtifact $ IntermediateArtifactFlattenedProofChecks flattenedProofChecks
    logInfoN "Registering flattened SMT proof checks"
    registerIntermediateArtifact $ IntermediateArtifactFlattenedSMTProofChecks flattenedSMTProofChecks
    logInfoN "Registered all intermediate artifacts"
    unless groupsAreDistinctAsExpected $
        error "SMT proof check groups should be distinct"
    return smtProofChecks

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

    flattenedProofChecks = flattenProofChecks proofChecks

    smtProofChecks = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        compileProofChecks problem <$> (proofChecks `atPairingId` pairingId)

    flattenedSMTProofChecks = void $ flattenSMTProofChecks smtProofChecks

    groupsAreDistinctAsExpected = and
        [ length groups == S.size (S.fromList (map (.setup) groups))
        | groups <- toList flattenedSMTProofChecks.unwrap
        ]


asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)
