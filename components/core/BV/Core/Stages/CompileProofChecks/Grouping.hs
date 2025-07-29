module BV.Core.Stages.CompileProofChecks.Grouping
    ( ProofCheckGroup
    , proofCheckGroups
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type ProofCheckGroup t a = [ProofCheck t a]

proofCheckGroups :: Tag t => [ProofCheck t a] -> [ProofCheckGroup t a]
proofCheckGroups = toList . proofCheckGroupsWithKeys

proofCheckGroupsWithKeys :: Tag t => [ProofCheck t a] -> Map CheckGroupKey (ProofCheckGroup t a)
proofCheckGroupsWithKeys checks = M.unionsWith (<>)
    [ M.singleton (compatKey (groupKey check)) [check]
    | check <- checks
    ]

newtype CheckGroupKey
  = CheckGroupKey { unwrap :: [((String, [(Integer, ([Integer], [Integer]))]), String)] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

groupKey :: Tag t => ProofCheck t a -> Set (WithTag t Visit)
groupKey check = S.fromList (check ^.. checkVisits)

compatKey :: Tag t => Set (WithTag t Visit) -> CheckGroupKey
compatKey visits = CheckGroupKey $ sort
    [ ((prettyNodeId visit.nodeId, map compatRestr visit.restrs), prettyTag tag)
    | WithTag tag visit <- S.toList visits
    ]

compatRestr :: Restr -> (Integer, ([Integer], [Integer]))
compatRestr restr = (restr.nodeAddr.unwrap, compatVisitCount restr.visitCount)

compatVisitCount :: VisitCount -> ([Integer], [Integer])
compatVisitCount vc = (vc.numbers, vc.offsets)
