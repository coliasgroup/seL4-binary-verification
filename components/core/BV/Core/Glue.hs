
{-# LANGUAGE DeriveAnyClass #-}

-- TODO rename module and exports

module BV.Core.Glue
    ( IntermediateStagesOutput (..)
    , ProofCheckMeta (..)
    , StagesInput (..)
    , StagesOutput (..)
    , StagesOutputChecks (..)
    , prettyProofCheckMeta
    , stages
    ) where

import BV.Core.DecorateProofScript
import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils.IncludeExcludeFilter

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Foldable (fold)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

-- import Control.DeepSeq (liftRnf)
-- import Control.Parallel.Strategies (evalSeq, rdeepseq, rparWith, using)

data StagesInput
  = StagesInput
      { programs :: ByTag' Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , cFunctionPrefix :: String
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts'
      , proofs :: Proofs' ()
      , earlyAsmFunctionFilter :: AsmFunctionFilter
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary StagesInput where

data StagesOutput
  = StagesOutput
      { checks :: StagesOutputChecks
        -- report
      , unhandledInlineAssemblyFunctions :: [Ident]
      , unhandledInstructionFunctions :: [Ident]
        -- intermediate, for checking
      , intermediate :: IntermediateStagesOutput
      }
  deriving (Eq, Generic, NFData)

newtype StagesOutputChecks
  = StagesOutputChecks { unwrap :: M.Map PairingId' [SMTProofCheckGroup ProofCheckMeta] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data ProofCheckMeta
  = ProofCheckMeta
      { path :: ProofScriptNodePath AsmRefineTag
      , desc :: ProofCheckDescription
      }
  deriving (Eq, Generic, NFData, Ord, Show)

prettyProofCheckMeta :: ProofCheckMeta -> String
prettyProofCheckMeta meta = prettyProofScriptNodePath meta.path ++ " >>> " ++ meta.desc

data IntermediateStagesOutput
  = IntermediateStagesOutput
      { functions :: Program
      , pairings :: Pairings'
      , problems :: Problems'
      , proofChecks :: ProofChecks' ProofCheckDescription
      , compatProofChecks :: CompatProofChecks
      , compatSMTProofChecks :: CompatSMTProofChecks
      }
  deriving (Eq, Generic, NFData)

stages :: StagesInput -> StagesOutput
stages input = StagesOutput
    { checks = finalChecks
    , unhandledInlineAssemblyFunctions = getC unhandledAsmFunctionNames
    , unhandledInstructionFunctions = getAsm unhandledAsmFunctionNames
    , intermediate = IntermediateStagesOutput
        { functions = collectedFunctions
        , pairings
        , problems
        , proofChecks
        , compatProofChecks
        , compatSMTProofChecks
        }
    }

  where

    alterProgramByTag = byAsmRefineTag (ByAsmRefineTag
        { asm = #functions %~ M.filterWithKey (\k _v ->
            applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)
        , c = pseudoCompile input.objDumpInfo
        })

    alteredPrograms = fixupProgram <$> (alterProgramByTag <*> input.programs)

    (inlineAsmPairings, alteredProgramsWithInlineAsm, unhandledAsmFunctionNames) =
        addInlineAssemblySpecs alteredPrograms

    finalPrograms = alteredProgramsWithInlineAsm

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions ! funName

    functionSigs = signatureOfFunction . lookupFunction

    collectedFunctions = programFromFunctions $
        M.unionWith (error "not disjoint") (getC finalPrograms).functions (getAsm finalPrograms).functions

    normalFunctionPairingIds = do
        asm <- M.keys (getAsm finalPrograms).functions
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
        guard $ c `M.member` (getC finalPrograms).functions
        return $ byAsmRefineTag (ByAsmRefineTag { asm, c })

    normalPairings =
        let f pairingId = formulatePairing
                (input.stackBounds.unwrap ! getAsm pairingId)
                (functionSigs (viewWithTag C pairingId))
         in M.fromSet f (S.fromList normalFunctionPairingIds)

    pairings = Pairings $ normalPairings `M.union` inlineAsmPairings.unwrap

    -- TODO
    -- By doing this we lose laziness, and it's probably overkill anyways (reduces eval from ~8s -> ~4s)
    -- Also, since implementing compileProofChecks, takes up too much memory
    -- problems = using problems' $ traverseOf (#unwrap % traversed) (rparWith rdeepseq)
    problems = problems'

    problems' = Problems $ M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns =
                let f funName prog = Named funName (prog.functions ! funName)
                 in f <$> pairingId <*> finalPrograms
        for namedFuns $ \namedFun -> guard $ isJust namedFun.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    provenProblems = problems & #unwrap %~ (`M.restrictKeys` M.keysSet input.proofs.unwrap)

    lookupOrigVarNameFor problem = argRenamesOf problem $
        signatureOfFunction . lookupFunction <$> withTags (pairingIdOfProblem problem)

    -- TODO (see above)
    -- proofChecks = using proofChecks' $ traverseOf (#unwrap % traversed) (rparWith (evalSeq (liftRnf (const ()))))
    -- proofChecks = using proofChecks' $ traverseOf (#unwrap % traversed) (rparWith rdeepseq)
    proofChecks = proofChecks'

    proofChecks' = ProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        let pairing = pairings.unwrap M.! pairingId
            proofScript = input.proofs.unwrap M.! pairingId
         in enumerateProofChecks (lookupOrigVarNameFor problem) pairing problem proofScript

    compatProofChecks = toCompatProofChecks proofChecks

    smtProofChecks = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        let repGraphInput = RepGraphBaseInput
                { structs = input.programs <&> (.structs)
                , rodata = input.rodata
                , problem
                }
         in compileProofChecks repGraphInput functionSigs pairings (lookupOrigVarNameFor problem)
                <$> (proofChecks.unwrap M.! pairingId)

    compatSMTProofChecks = toCompatSMTProofChecks (void smtProofChecks)

    finalChecks =
        let f = decorateProofScriptWithProofScriptNodePathsWith $ \path -> map (fmap (ProofCheckMeta path))
         in StagesOutputChecks $ M.map (fold . f) smtProofChecks.unwrap
