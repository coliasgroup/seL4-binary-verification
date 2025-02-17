
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
    , IntermediateStagesOutput (..)
    , StagesInput (..)
    , StagesOutput (..)
    , StagesOutputChecks (..)
    , stages
    ) where

import BV.Core.DecorateProofScript
import BV.Core.Stages.BuildProblem
import BV.Core.Stages.CompileProofChecks
import BV.Core.Stages.EnumerateProofChecks
import BV.Core.Stages.Fixup
import BV.Core.Stages.FormulatePairing
import BV.Core.Stages.InlineAssembly
import BV.Core.Stages.PseudoCompile
import BV.Core.Types
import BV.Core.Types.Extras

import Control.DeepSeq (NFData, liftRnf)
import Control.Monad (guard)
import Control.Parallel.Strategies (evalSeq, rdeepseq, rparWith, using)
import Data.Foldable (fold)
import Data.Function (applyWhen)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Optics
import Data.Foldable (toList)

data StagesInput
  = StagesInput
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , proofs :: Proofs ()
      , earlyAsmFunctionFilter :: Ident -> Bool
        -- HACK
      , compatSMTProofChecks :: CompatSMTProofChecks
      }
  deriving (Generic)

data StagesOutput
  = StagesOutput
      { checks :: StagesOutputChecks
        -- report
      , unhandledInlineAssemblyFunctions :: [Ident]
      , unhandledInstructionFunctions :: [Ident]
        -- intermediate, for checking
      , intermediate :: IntermediateStagesOutput
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype StagesOutputChecks
  = StagesOutputChecks { unwrap :: M.Map PairingId [(ProofScriptNodePath, SMTProofCheckGroup ProofCheckDescription)] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data IntermediateStagesOutput
  = IntermediateStagesOutput
      { functions :: Program
      , pairings :: Pairings
      , problems :: Problems
      , compatProofChecks :: CompatProofChecks
      , compatSMTProofChecks :: CompatSMTProofChecks
      }
  deriving (Eq, Generic, NFData, Ord, Show)

stages :: StagesInput -> StagesOutput
stages input = StagesOutput
    { checks = finalChecks
    , unhandledInlineAssemblyFunctions = unhandledAsmFunctionNames.c
    , unhandledInstructionFunctions = unhandledAsmFunctionNames.asm
    , intermediate = IntermediateStagesOutput
        { functions = collectedFunctions
        , pairings
        , problems
        , compatProofChecks
        , compatSMTProofChecks
        }
    }

  where

    alteredPrograms = fixupProgram <$> PairingOf
        { asm = input.programs.asm & #functions %~ M.filterWithKey (\k _v -> input.earlyAsmFunctionFilter k)
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

    -- TODO by doing this we lose laziness, and it's probably overkill anyways (reduces eval from ~8s -> ~4s)
    -- TODO parallelism probably overkill
    problems = using problems' $ traverseOf (#unwrap % traversed) (rparWith rdeepseq)
    -- problems = problems'

    problems' = Problems . M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns = (\funName prog -> Named funName (prog.functions ! funName)) <$> pairingId <*> finalPrograms
        guard $ isJust namedFuns.c.value.body
        guard $ isJust namedFuns.asm.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    provenProblems = problems & #unwrap %~ \m -> M.restrictKeys m (M.keysSet input.proofs.unwrap)

    -- TODO (see above)
    proofChecks = using proofChecks' $ traverseOf (#unwrap % traversed) (rparWith (evalSeq (liftRnf (const ()))))
    -- proofChecks = using proofChecks' $ traverseOf (#unwrap % traversed) (rparWith rdeepseq)
    -- proofChecks = proofChecks'

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

    -- checkAssumptionAboutProofCheckGroups =
    --     if False
    --     then error "!"
    --     else ()

    compatProofChecks = toCompatProofChecks proofChecks

    uncheckedSMTProofChecks'hack = liftCompatSMTProofChecks'hack input.compatSMTProofChecks

    uncheckedSMTProofChecks'nohack = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        compileProofChecks problem <$> (proofChecks `atPairingId` pairingId)

    uncheckedSMTProofChecks = uncheckedSMTProofChecks'hack
    -- uncheckedSMTProofChecks = uncheckedSMTProofChecks'nohack

    compatSMTProofChecks = toCompatSMTProofChecks (void uncheckedSMTProofChecks)

    groupsAreDistinctAsExpected = and
        [ let checksByNode = toList script
              groupKeysByNode = M.keysSet . proofCheckGroupsWithKeys <$> checksByNode
              l = sum (map length groupKeysByNode)
              r = S.size (fold groupKeysByNode)
              ok = l == r
           in applyWhen (not ok) (error (show (pairingId, l, r))) ok
        | (pairingId, script) <- M.toList proofChecks.unwrap
        ]

    smtProofChecks =
        if groupsAreDistinctAsExpected
        then uncheckedSMTProofChecks
        else
            -- TODO
            error "SMT proof check groups should be distinct"
            -- uncheckedSMTProofChecks

    finalChecks =
        let f = decorateProofScriptWithProofScriptNodePathsWith $ \path groups ->
                map (path,) groups
         in StagesOutputChecks $
                M.map (fold . f) smtProofChecks.unwrap


asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)

--

liftCompatSMTProofChecks'hack :: CompatSMTProofChecks -> SMTProofChecks String
liftCompatSMTProofChecks'hack compatChecks = SMTProofChecks $ M.map f compatChecks.unwrap
  where
    f groups = ProofScript (ProofNodeWith (("" <$) <$> groups) ProofNodeLeaf)
